********************************************************************************
*
* LICENSE and NOTICE
*
* See include program ZSHRINKER_ABAPGIT_LICENSE
*
********************************************************************************

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lo_bsp            TYPE REF TO cl_o2_api_application,
          ls_attributes     TYPE o2applattr,
          lt_nodes          TYPE o2applnode_table,
          lt_navgraph       TYPE o2applgrap_table,
          lv_obj_name       TYPE string,
          lv_extra          TYPE string,
          lv_ext            TYPE string,
          lo_page           TYPE REF TO cl_o2_api_pages,
          lt_pages_info     TYPE ty_pages_tt,
          ls_pagekey        TYPE o2pagkey,
          ls_local_page     TYPE ty_page,
          lt_remote_content TYPE o2pageline_table,
          lt_local_content  TYPE o2pageline_table,
          lt_local_pages    TYPE o2pagelist.

    FIELD-SYMBOLS: <ls_remote_page> LIKE LINE OF lt_pages_info.

    io_xml->read( EXPORTING iv_name = 'ATTRIBUTES'
                  CHANGING cg_data = ls_attributes ).
    io_xml->read( EXPORTING iv_name = 'NAVGRAPH'
                  CHANGING cg_data = lt_navgraph ).
    io_xml->read( EXPORTING iv_name = 'PAGES'
                  CHANGING cg_data = lt_pages_info ).

    ls_attributes-devclass = iv_package.

    cl_o2_api_application=>load(
      EXPORTING
        p_application_name  = ls_attributes-applname    " Application Name
      IMPORTING
        p_application       = lo_bsp    " Instance Created
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3
        OTHERS              = 4 ).

    CASE sy-subrc.
      WHEN 0.

        cl_o2_api_pages=>get_all_pages(
          EXPORTING
            p_applname = ls_attributes-applname
            p_version  = c_active
          IMPORTING
            p_pages    = lt_local_pages ).

      WHEN 1.

        lo_bsp = create_new_application( is_attributes = ls_attributes
                                         it_nodes      = lt_nodes
                                         it_navgraph   = lt_navgraph ).

      WHEN OTHERS.

        Lcx_abapgit_exception=>raise( |Error { sy-subrc } from CL_O2_API_APPLICATION=>LOAD| ).

    ENDCASE.

    LOOP AT lt_pages_info ASSIGNING <ls_remote_page>.

      ls_pagekey-applname = <ls_remote_page>-attributes-applname.
      ls_pagekey-pagekey = <ls_remote_page>-attributes-pagekey.

      cl_o2_api_pages=>load(
        EXPORTING
          p_pagekey             = ls_pagekey
        IMPORTING
          p_page                = lo_page
        EXCEPTIONS
          object_not_existing   = 1
          version_not_existing  = 2
          OTHERS                = 3 ).

      CASE sy-subrc.
        WHEN 0.

          ls_local_page = read_page( is_page = <ls_remote_page>-attributes
                                     iv_no_files_add = abap_true ).

        WHEN 1.

          lo_page = create_new_page( <ls_remote_page>-attributes ).

        WHEN 2.

          " Do nothing...

        WHEN OTHERS.

          Lcx_abapgit_exception=>raise( |Error { sy-subrc } from CL_O2_API_PAGES=>LOAD| ).

      ENDCASE.

      SPLIT <ls_remote_page>-attributes-pagename AT '.' INTO lv_extra lv_ext.
      REPLACE ALL OCCURRENCES OF '/' IN lv_extra WITH '_-'.
      REPLACE ALL OCCURRENCES OF '/' IN lv_ext WITH '_-'.

      lt_remote_content = to_page_content( Lif_abapgit_object~mo_files->read_raw( iv_extra = lv_extra
                                                                                  iv_ext   = lv_ext ) ).
      lt_local_content = to_page_content( get_page_content( lo_page ) ).

      IF ls_local_page = <ls_remote_page> AND lt_local_content = lt_remote_content.
        " no changes -> nothing to do
        CONTINUE.
      ENDIF.

      IF <ls_remote_page>-attributes-pagetype <> so2_controller.

        lo_page->set_page( lt_remote_content ).

        lo_page->set_event_handlers( <ls_remote_page>-event_handlers ).
        lo_page->set_parameters( <ls_remote_page>-parameters ).
        lo_page->set_type_source( <ls_remote_page>-types ).

      ENDIF.

      lo_page->save( p_with_all_texts = abap_true ).

      lv_obj_name = cl_wb_object_type=>get_concatenated_key_from_id(
        p_key_component1 = <ls_remote_page>-attributes-applname
        p_key_component2 = <ls_remote_page>-attributes-pagekey
        p_external_id    = 'WG ' ).

      Lcl_abapgit_objects_activation=>add( iv_type = 'WAPP'
                                           iv_name = lv_obj_name ).

    ENDLOOP.

    delete_superfluous_pages( it_local_pages  = lt_local_pages
                              it_remote_pages = lt_pages_info ).

    Lcl_abapgit_sotr_handler=>create_sotr(
      iv_package = iv_package
      io_xml     = io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_name TYPE o2applname.


    lv_name = ms_item-obj_name.

    cl_o2_api_application=>load(
      EXPORTING
        p_application_name  = lv_name
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3 ).
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

    DATA: lv_name       TYPE o2applname,
          ls_attributes TYPE o2applattr,
          lt_navgraph   TYPE o2applgrap_table,
          lt_pages      TYPE o2pagelist,
          lt_pages_info TYPE ty_pages_tt,
          lo_bsp        TYPE REF TO cl_o2_api_application.

    FIELD-SYMBOLS: <ls_page> LIKE LINE OF lt_pages.

    lv_name = ms_item-obj_name.

    cl_o2_api_application=>load(
      EXPORTING
        p_application_name  = lv_name
      IMPORTING
        p_application       = lo_bsp
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lo_bsp->get_attributes(
      EXPORTING
        p_version    = c_active
      IMPORTING
        p_attributes = ls_attributes ).

    CLEAR: ls_attributes-author,
           ls_attributes-createdon,
           ls_attributes-changedby,
           ls_attributes-changedon,
           ls_attributes-devclass.

    io_xml->add( iv_name = 'ATTRIBUTES'
                 ig_data = ls_attributes ).

    lo_bsp->get_navgraph(
      EXPORTING
        p_version  = c_active
      IMPORTING
        p_navgraph = lt_navgraph ).

    io_xml->add( iv_name = 'NAVGRAPH'
                 ig_data = lt_navgraph ).

    cl_o2_api_pages=>get_all_pages(
      EXPORTING
        p_applname = lv_name
        p_version  = c_active
      IMPORTING
        p_pages    = lt_pages ).

    LOOP AT lt_pages ASSIGNING <ls_page>.
      APPEND read_page( <ls_page> ) TO lt_pages_info.
    ENDLOOP.

    io_xml->add( iv_name = 'PAGES'
                 ig_data = lt_pages_info ).

    Lcl_abapgit_sotr_handler=>read_sotr(
      iv_pgmid    = 'LIMU'
      iv_object   = 'WAPP'
      iv_obj_name = ms_item-obj_name
      io_i18n_params = mo_i18n_params
      io_xml      = io_xml ).

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
endclass. "ZCL_ABAPGIT_OBJECT_WAPA implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_WDYA <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_wdya=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_wdya=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_WDYA implementation.
*"* method's implementations
*include methods.
  METHOD read.

    DATA: li_app  TYPE REF TO if_wdy_md_application,
          li_map  TYPE REF TO if_object_map,
          lo_prop TYPE REF TO cl_wdy_md_application_property,
          ls_prop LIKE LINE OF et_properties,
          lv_name TYPE wdy_application_name.


    CLEAR es_app.
    CLEAR et_properties.

    lv_name = ms_item-obj_name.
    TRY.
        li_app = cl_wdy_md_application=>get_object_by_key(
                   name    = lv_name
                   version = 'A' ).
      CATCH cx_wdy_md_not_existing.
        RETURN.
      CATCH cx_wdy_md_permission_failure.
        Lcx_abapgit_exception=>raise( 'WDYA, permission failure' ).
    ENDTRY.

    li_app->if_wdy_md_object~get_definition( IMPORTING definition = es_app ).
    CLEAR: es_app-author,
           es_app-createdon,
           es_app-changedby,
           es_app-changedon.

    li_map = li_app->get_properties( ).
    DO li_map->size( ) TIMES.
      lo_prop ?= li_map->get_by_position( sy-index ).
      lo_prop->get_definition( IMPORTING definition = ls_prop ).
      APPEND ls_prop TO et_properties.
    ENDDO.

  ENDMETHOD.
  METHOD save.

    DATA: li_prop TYPE REF TO if_wdy_md_application_property,
          lo_app  TYPE REF TO cl_wdy_md_application.

    FIELD-SYMBOLS: <ls_property> LIKE LINE OF it_properties.


    TRY.
        CREATE OBJECT lo_app
          EXPORTING
            name       = is_app-application_name
            definition = is_app
            devclass   = iv_package.

        LOOP AT it_properties ASSIGNING <ls_property>.
          li_prop = lo_app->if_wdy_md_application~create_property( <ls_property>-name ).
          li_prop->set_value( <ls_property>-value ).
        ENDLOOP.

        tadir_insert( iv_package ).

        lo_app->if_wdy_md_lockable_object~save_to_database( ).
      CATCH cx_wdy_md_exception.
        Lcx_abapgit_exception=>raise( 'error saving WDYA' ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: li_app  TYPE REF TO if_wdy_md_application,
          ls_app  TYPE wdy_application,
          lv_name TYPE wdy_application_name.


    lv_name = ms_item-obj_name.
    TRY.
        li_app = cl_wdy_md_application=>get_object_by_key(
                   name    = lv_name
                   version = 'A' ).

        li_app->if_wdy_md_object~get_definition( IMPORTING definition = ls_app ).

        IF ls_app-changedby IS INITIAL.
          rv_user = ls_app-author.
        ELSE.
          rv_user = ls_app-changedby.
        ENDIF.
      CATCH cx_root.
        rv_user = c_user_unknown.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: li_app    TYPE REF TO if_wdy_md_application,
          lv_objkey TYPE wdy_wb_appl_name,
          lv_type   TYPE seu_type,
          lv_name   TYPE wdy_application_name.


    lv_name = ms_item-obj_name.
    TRY.
        li_app = cl_wdy_md_application=>get_object_by_key(
                   name    = lv_name
                   version = 'A' ).
        li_app->if_wdy_md_object~delete( ).
        li_app->if_wdy_md_lockable_object~save_to_database( ).

* method save_to_database calls function module TR_TADIR_INTERFACE
* with test mode = X, so it does not delete the TADIR entry.
* Instead the standard code uses RS_TREE_OBJECT_PLACEMENT to delete
* the TADIR entry
        lv_objkey = ms_item-obj_name.
        CONCATENATE 'O' swbm_c_type_wdy_application INTO lv_type.
        CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
          EXPORTING
            object    = lv_objkey
            type      = lv_type
            operation = 'DELETE'.

      CATCH cx_wdy_md_not_existing.
        RETURN.
      CATCH cx_wdy_md_exception.
        Lcx_abapgit_exception=>raise( 'WDYA, error deleting' ).
    ENDTRY.

    delete_longtexts( c_longtext_id_wdya ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_app        TYPE wdy_application,
          lt_properties TYPE wdy_app_property_table.

    io_xml->read( EXPORTING iv_name = 'APP'
                  CHANGING cg_data = ls_app ).
    io_xml->read( EXPORTING iv_name = 'PROPERTIES'
                  CHANGING cg_data = lt_properties ).

    save( is_app        = ls_app
          it_properties = lt_properties
          iv_package    = iv_package ).

    Lcl_abapgit_sotr_handler=>create_sotr(
      iv_package = iv_package
      io_xml     = io_xml ).

    deserialize_longtexts( ii_xml         = io_xml
                           iv_longtext_id = c_longtext_id_wdya ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_name TYPE wdy_application_name.


    lv_name = ms_item-obj_name.

    TRY.
        cl_wdy_md_application=>get_object_by_key(
          name    = lv_name
          version = 'A' ).
        rv_bool = abap_true.
      CATCH cx_wdy_md_not_existing.
        rv_bool = abap_false.
      CATCH cx_wdy_md_permission_failure.
        Lcx_abapgit_exception=>raise( 'WDYA, permission failure' ).
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

    DATA: ls_app        TYPE wdy_application,
          lt_properties TYPE wdy_app_property_table.

    read( IMPORTING es_app        = ls_app
                    et_properties = lt_properties ).

    io_xml->add( iv_name = 'APP'
                 ig_data = ls_app ).
    io_xml->add( iv_name = 'PROPERTIES'
                 ig_data = lt_properties ).

    Lcl_abapgit_sotr_handler=>read_sotr(
      iv_pgmid    = 'R3TR'
      iv_object   = ms_item-obj_type
      iv_obj_name = ms_item-obj_name
      io_i18n_params = mo_i18n_params
      io_xml      = io_xml ).

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_wdya ).

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
endclass. "ZCL_ABAPGIT_OBJECT_WDYA implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_CMOD <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_cmod=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_cmod=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_CMOD implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE anam FROM modattr INTO rv_user WHERE name = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA lv_name TYPE modact-name.

    lv_name = ms_item-obj_name.

    CALL FUNCTION 'MOD_KUN_ACTIVATE'
      EXPORTING
        activate           = abap_false
        deactivate         = abap_true
        modname            = lv_name
      EXCEPTIONS
        call_error         = 1
        generate_error     = 2
        modattr_status     = 3
        mod_active         = 4
        mod_enqueued       = 5
        not_activated      = 6
        no_modification    = 7
        permission_failure = 8
        OTHERS             = 9.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'MOD_KUN_DELETE'
      EXPORTING
        modname            = lv_name
        screen             = abap_false
      EXCEPTIONS
        attr_enqueued      = 1
        mod_active         = 2
        mod_enqueued       = 3
        text_enqueued      = 4
        permission_failure = 5
        OTHERS             = 6.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_name    TYPE modact-name,
          lt_modact  TYPE TABLE OF modact,
          lt_modtext TYPE TABLE OF modtext,
          lt_modattr TYPE TABLE OF modattr.

    lv_name = ms_item-obj_name.

    DELETE FROM modact WHERE name = lv_name.
    DELETE FROM modtext WHERE name = lv_name.
    DELETE FROM modattr WHERE name = lv_name.

    io_xml->read( EXPORTING iv_name = 'MODACT'
                  CHANGING  cg_data = lt_modact ).

    io_xml->read( EXPORTING iv_name = 'MODTEXT'
                  CHANGING  cg_data = lt_modtext ).

    io_xml->read( EXPORTING iv_name = 'MODATTR'
                  CHANGING  cg_data = lt_modattr ).

    INSERT modact FROM TABLE lt_modact.
    INSERT modtext FROM TABLE lt_modtext.
    INSERT modattr FROM TABLE lt_modattr.

    tadir_insert( iv_package ).

    CALL FUNCTION 'MOD_KUN_ACTIVATE'
      EXPORTING
        activate           = abap_true
        deactivate         = abap_false
        modname            = lv_name
      EXCEPTIONS
        call_error         = 1
        generate_error     = 2
        modattr_status     = 3
        mod_active         = 4
        mod_enqueued       = 5
        not_activated      = 6
        no_modification    = 7
        permission_failure = 8
        OTHERS             = 9.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_name TYPE modact-name.

    SELECT SINGLE name FROM modact INTO lv_name WHERE name = ms_item-obj_name.
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

    DATA: lt_modact  TYPE TABLE OF modact,
          lt_modtext TYPE TABLE OF modtext,
          lt_modattr TYPE TABLE OF modattr.

    FIELD-SYMBOLS: <ls_modattr> TYPE modattr.

    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    SELECT * FROM modact INTO TABLE lt_modact WHERE name = ms_item-obj_name
      ORDER BY PRIMARY KEY.
    IF sy-subrc = 0.
      io_xml->add( iv_name = 'MODACT'
                   ig_data = lt_modact ).
    ENDIF.

    SELECT * FROM modtext INTO TABLE lt_modtext WHERE name = ms_item-obj_name AND sprsl = mv_language
      ORDER BY PRIMARY KEY.
    IF sy-subrc = 0.
      io_xml->add( iv_name = 'MODTEXT'
                   ig_data = lt_modtext ).
    ENDIF.

    SELECT * FROM modattr INTO TABLE lt_modattr WHERE name = ms_item-obj_name
      ORDER BY PRIMARY KEY.
    IF sy-subrc = 0.
      LOOP AT lt_modattr ASSIGNING <ls_modattr>.
        CLEAR:
          <ls_modattr>-cnam, <ls_modattr>-cdat,
          <ls_modattr>-unam, <ls_modattr>-udat,
          <ls_modattr>-anam, <ls_modattr>-adat,
          <ls_modattr>-fnam, <ls_modattr>-fdat.
      ENDLOOP.

      io_xml->add( iv_name = 'MODATTR'
                   ig_data = lt_modattr ).
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
endclass. "ZCL_ABAPGIT_OBJECT_CMOD implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_WDYN <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_wdyn=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_wdyn=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_WDYN implementation.
*"* method's implementations
*include methods.
  METHOD add_fm_exception.

    DATA: ls_exception LIKE LINE OF ct_exception.

    ls_exception-name = iv_name.
    ls_exception-value = iv_value.

    INSERT ls_exception INTO TABLE ct_exception.

  ENDMETHOD.
  METHOD add_fm_param_exporting.

    DATA: ls_param LIKE LINE OF ct_param.

    ls_param-kind = abap_func_exporting.
    ls_param-name = iv_name.
    GET REFERENCE OF ig_value INTO ls_param-value.

    INSERT ls_param INTO TABLE ct_param.

  ENDMETHOD.
  METHOD add_fm_param_tables.

    DATA: ls_param LIKE LINE OF ct_param.

    ls_param-kind = abap_func_tables.
    ls_param-name = iv_name.
    GET REFERENCE OF ct_value INTO ls_param-value.

    INSERT ls_param INTO TABLE ct_param.

  ENDMETHOD.
  METHOD add_with_inactive_parts.

    DATA:
      lv_obj_name TYPE trobj_name,
      lv_object   TYPE trobjtype,
      lt_objects  TYPE dwinactiv_tab.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF lt_objects.

    lv_obj_name = ms_item-obj_name.
    lv_object = ms_item-obj_type.

    CALL FUNCTION 'RS_INACTIVE_OBJECTS_IN_OBJECT'
      EXPORTING
        obj_name         = lv_obj_name
        object           = lv_object
      TABLES
        inactive_objects = lt_objects
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    LOOP AT lt_objects ASSIGNING <ls_object>.
      Lcl_abapgit_objects_activation=>add( iv_type = <ls_object>-object
                                           iv_name = <ls_object>-obj_name ).
    ENDLOOP.

  ENDMETHOD.
  METHOD delta_controller.

    DATA: li_controller TYPE REF TO if_wdy_md_controller,
          lx_error      TYPE REF TO cx_wdy_md_exception,
          lv_found      TYPE abap_bool,
          ls_key        TYPE wdy_md_controller_key,
          ls_obj_new    TYPE svrs2_versionable_object,
          ls_obj_old    TYPE svrs2_versionable_object.

    FIELD-SYMBOLS: <ls_component>            LIKE LINE OF mt_components,
                   <ls_source>               LIKE LINE OF mt_sources,
                   <lt_ctrl_exceptions>      TYPE ANY TABLE,
                   <lt_ctrl_exception_texts> TYPE ANY TABLE,
                   <lt_excp>                 TYPE ANY TABLE,
                   <lt_excpt>                TYPE ANY TABLE.


    ls_key-component_name = is_controller-definition-component_name.
    ls_key-controller_name = is_controller-definition-controller_name.

    lv_found = cl_wdy_md_controller=>check_existency(
          component_name  = ls_key-component_name
          controller_name = ls_key-controller_name ).
    IF lv_found = abap_false.
      TRY.
          li_controller ?= cl_wdy_md_controller=>create_complete(
                component_name  = ls_key-component_name
                controller_name = ls_key-controller_name
                controller_type = is_controller-definition-controller_type ).
          li_controller->save_to_database( ).
          li_controller->unlock( ).
        CATCH cx_wdy_md_exception INTO lx_error.
          Lcx_abapgit_exception=>raise( |Error creating dummy controller: { lx_error->get_text( ) }| ).
      ENDTRY.
    ENDIF.

    ls_obj_new-objtype = wdyn_limu_component_controller.
    ls_obj_new-objname = ls_key.

    ls_obj_old-objtype = wdyn_limu_component_controller.
    ls_obj_old-objname = ls_key.

    APPEND is_controller-definition TO ls_obj_old-wdyc-defin.

    LOOP AT mt_components ASSIGNING <ls_component>
        WHERE component_name = ls_key-component_name
        AND controller_name = ls_key-controller_name.
      APPEND <ls_component> TO ls_obj_old-wdyc-ccomp.
    ENDLOOP.
    LOOP AT mt_sources ASSIGNING <ls_source>
        WHERE component_name = ls_key-component_name
        AND controller_name = ls_key-controller_name.
      APPEND <ls_source> TO ls_obj_old-wdyc-ccoms.
    ENDLOOP.

    ls_obj_old-wdyc-descr = is_controller-descriptions.
    ls_obj_old-wdyc-cusag = is_controller-controller_usages.
    ls_obj_old-wdyc-ccomt = is_controller-controller_component_texts.
    ls_obj_old-wdyc-cpara = is_controller-controller_parameters.
    ls_obj_old-wdyc-cpart = is_controller-controller_parameter_texts.
    ls_obj_old-wdyc-cnode = is_controller-context_nodes.
    ls_obj_old-wdyc-cattr = is_controller-context_attributes.
    ls_obj_old-wdyc-cmapp = is_controller-context_mappings.
*   Version 702 doesn't have these two attributes so we
*   use them dynamically for downward compatibility
    ASSIGN COMPONENT 'CONTROLLER_EXCEPTIONS' OF STRUCTURE is_controller
      TO <lt_ctrl_exceptions>.
    IF sy-subrc = 0.
      ASSIGN COMPONENT 'EXCP' OF STRUCTURE ls_obj_old-wdyc TO <lt_excp>.
      IF sy-subrc = 0.
        <lt_excp> = <lt_ctrl_exceptions>.
      ENDIF.
    ENDIF.
    ASSIGN COMPONENT 'CONTROLLER_EXCEPTIONS_TEXTS' OF STRUCTURE is_controller
      TO <lt_ctrl_exception_texts>.
    IF sy-subrc = 0.
      ASSIGN COMPONENT 'EXCPT' OF STRUCTURE ls_obj_old-wdyc TO <lt_excpt>.
      IF sy-subrc = 0.
        <lt_excpt> = <lt_ctrl_exception_texts>.
      ENDIF.
    ENDIF.
    ls_obj_old-wdyc-fgrps = is_controller-fieldgroups.

    CALL FUNCTION 'SVRS_MAKE_OBJECT_DELTA'
      EXPORTING
        obj_old              = ls_obj_new
        obj_new              = ls_obj_old
      CHANGING
        delta                = rs_delta
      EXCEPTIONS
        inconsistent_objects = 1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from SVRS_MAKE_OBJECT_DELTA' ).
    ENDIF.

  ENDMETHOD.
  METHOD delta_definition.

    DATA: ls_key       TYPE wdy_md_component_key,
          lv_found     TYPE abap_bool,
          ls_obj_new   TYPE svrs2_versionable_object,
          li_component TYPE REF TO if_wdy_md_component,
          lx_error     TYPE REF TO cx_wdy_md_exception,
          ls_obj_old   TYPE svrs2_versionable_object.


    ls_key-component_name = is_definition-definition-component_name.

    lv_found = cl_wdy_md_component=>check_existency( ls_key-component_name ).
    IF lv_found = abap_false.
      TRY.
          cl_wdy_md_component=>create_complete(
            EXPORTING
              name      = ls_key-component_name
            IMPORTING
              component = li_component
            CHANGING
              devclass  = iv_package ).
          li_component->save_to_database( ).
          li_component->unlock( ).
        CATCH cx_wdy_md_exception INTO lx_error.
          Lcx_abapgit_exception=>raise( |Error creating dummy component: { lx_error->get_text( ) }| ).
      ENDTRY.
    ENDIF.

    ls_obj_new-objtype = wdyn_limu_component_definition.
    ls_obj_new-objname = ls_key-component_name.

    ls_obj_old-objtype = wdyn_limu_component_definition.
    ls_obj_old-objname = ls_key-component_name.

    APPEND is_definition-definition TO ls_obj_old-wdyd-defin.
    ls_obj_old-wdyd-descr = is_definition-descriptions.
    ls_obj_old-wdyd-cusag = is_definition-component_usages.
    ls_obj_old-wdyd-intrf = is_definition-interface_implementings.
    ls_obj_old-wdyd-libra = is_definition-library_usages.
    ls_obj_old-wdyd-ctuse = is_definition-ext_ctlr_usages.
    ls_obj_old-wdyd-ctmap = is_definition-ext_ctx_mappings.

    CALL FUNCTION 'SVRS_MAKE_OBJECT_DELTA'
      EXPORTING
        obj_old              = ls_obj_new
        obj_new              = ls_obj_old
      CHANGING
        delta                = rs_delta
      EXCEPTIONS
        inconsistent_objects = 1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from SVRS_MAKE_OBJECT_DELTA' ).
    ENDIF.

  ENDMETHOD.
  METHOD delta_view.

    DATA: ls_key     TYPE wdy_md_view_key,
          ls_obj_new TYPE svrs2_versionable_object,
          ls_obj_old TYPE svrs2_versionable_object,
          lv_found   TYPE abap_bool,
          lx_error   TYPE REF TO cx_wdy_md_exception,
          li_view    TYPE REF TO if_wdy_md_abstract_view.

    FIELD-SYMBOLS: <ls_def> LIKE LINE OF ls_obj_old-wdyv-defin.


    ls_key-component_name = is_view-definition-component_name.
    ls_key-view_name      = is_view-definition-view_name.

    lv_found = cl_wdy_md_abstract_view=>check_existency(
                 component_name = ls_key-component_name
                 name           = ls_key-view_name ).
    IF lv_found = abap_false.
      TRY.
          li_view = cl_wdy_md_abstract_view=>create(
                      component_name = is_view-definition-component_name
                      view_name      = is_view-definition-view_name
                      type           = is_view-definition-type ).
          li_view->save_to_database( ).
          li_view->unlock( ).
        CATCH cx_wdy_md_exception INTO lx_error.
          Lcx_abapgit_exception=>raise( |Error creating dummy view: { lx_error->get_text( ) }| ).
      ENDTRY.
    ENDIF.

    ls_obj_new-objtype = wdyn_limu_component_view.
    ls_obj_new-objname = ls_key.

    ls_obj_old-objtype = wdyn_limu_component_view.
    ls_obj_old-objname = ls_key.

    APPEND INITIAL LINE TO ls_obj_old-wdyv-defin ASSIGNING <ls_def>.
    MOVE-CORRESPONDING is_view-definition TO <ls_def>.

    ls_obj_old-wdyv-descr = is_view-descriptions.
    ls_obj_old-wdyv-vcont = is_view-view_containers.
    ls_obj_old-wdyv-vcntt = is_view-view_container_texts.
    ls_obj_old-wdyv-ibplg = is_view-iobound_plugs.
    ls_obj_old-wdyv-ibplt = is_view-iobound_plug_texts.
    ls_obj_old-wdyv-plpar = is_view-plug_parameters.
    ls_obj_old-wdyv-plprt = is_view-plug_parameter_texts.
    ls_obj_old-wdyv-uiele = is_view-ui_elements.
    ls_obj_old-wdyv-uicon = is_view-ui_context_bindings.
    ls_obj_old-wdyv-uievt = is_view-ui_event_bindings.
    ls_obj_old-wdyv-uiddc = is_view-ui_ddic_bindings.
    ls_obj_old-wdyv-uiprp = is_view-ui_properties.
    ls_obj_old-wdyv-navil = is_view-navigation_links.
    ls_obj_old-wdyv-navit = is_view-navigation_target_refs.
    ls_obj_old-wdyv-vshno = is_view-vsh_nodes.
    ls_obj_old-wdyv-vshpl = is_view-vsh_placeholders.
    ls_obj_old-wdyv-views = is_view-viewset_properties.

    CALL FUNCTION 'SVRS_MAKE_OBJECT_DELTA'
      EXPORTING
        obj_old              = ls_obj_new
        obj_new              = ls_obj_old
      CHANGING
        delta                = rs_delta
      EXCEPTIONS
        inconsistent_objects = 1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from SVRS_MAKE_OBJECT_DELTA' ).
    ENDIF.

  ENDMETHOD.
  METHOD get_limu_objects.

    DATA: lv_name TYPE wdy_component_name.


    lv_name = ms_item-obj_name.
    CALL FUNCTION 'WDYN_GET_LIMU_OBJECTS'
      EXPORTING
        component_name = lv_name
      IMPORTING
        limu_objects   = rt_objects.

  ENDMETHOD.
  METHOD read.

    DATA: lt_objects        TYPE wdy_md_transport_keys,
          ls_controller_key TYPE wdy_md_controller_key,
          ls_component_key  TYPE wdy_md_component_key,
          ls_view_key       TYPE wdy_md_view_key.

    FIELD-SYMBOLS: <ls_object>               LIKE LINE OF lt_objects,
                   <ls_meta>                 LIKE LINE OF rs_component-ctlr_metadata,
                   <ls_view>                 LIKE LINE OF rs_component-view_metadata,
                   <lt_ctrl_exceptions>      TYPE ANY TABLE,
                   <lt_ctrl_exception_texts> TYPE ANY TABLE.

    CLEAR mt_components.
    CLEAR mt_sources.

    lt_objects = get_limu_objects( ).

    LOOP AT lt_objects ASSIGNING <ls_object>.
      CASE <ls_object>-sub_type.
        WHEN wdyn_limu_component_controller.
          ls_controller_key = <ls_object>-sub_name.
          APPEND read_controller( ls_controller_key ) TO rs_component-ctlr_metadata.
        WHEN wdyn_limu_component_definition.
          ls_component_key = <ls_object>-sub_name.
          rs_component-comp_metadata = read_definition( ls_component_key ).
        WHEN wdyn_limu_component_view.
          ls_view_key = <ls_object>-sub_name.
          APPEND read_view( ls_view_key ) TO rs_component-view_metadata.
        WHEN OTHERS.
          ASSERT 0 = 1.
      ENDCASE.
    ENDLOOP.

    SORT rs_component-ctlr_metadata BY
      definition-component_name ASCENDING
      definition-controller_name ASCENDING.

    LOOP AT rs_component-ctlr_metadata ASSIGNING <ls_meta>.
      SORT <ls_meta>-descriptions.
      SORT <ls_meta>-controller_usages.
      SORT <ls_meta>-controller_components.
      SORT <ls_meta>-controller_component_texts.
      SORT <ls_meta>-controller_parameters.
      SORT <ls_meta>-controller_parameter_texts.
      SORT <ls_meta>-context_nodes.
      SORT <ls_meta>-context_attributes.
      SORT <ls_meta>-context_mappings.
      SORT <ls_meta>-fieldgroups.
*     Version 702 doesn't have these two attributes so we
*     use them dynamically for downward compatibility
      ASSIGN COMPONENT 'CONTROLLER_EXCEPTIONS' OF STRUCTURE <ls_meta> TO <lt_ctrl_exceptions>.
      IF sy-subrc = 0.
        SORT <lt_ctrl_exceptions>.
      ENDIF.
      ASSIGN COMPONENT 'CONTROLLER_EXCEPTION_TEXTS' OF STRUCTURE <ls_meta> TO <lt_ctrl_exception_texts>.
      IF sy-subrc = 0.
        SORT <lt_ctrl_exception_texts>.
      ENDIF.
    ENDLOOP.

    SORT rs_component-view_metadata BY
      definition-component_name ASCENDING
      definition-view_name ASCENDING.

    LOOP AT rs_component-view_metadata ASSIGNING <ls_view>.
      SORT <ls_view>-descriptions.
      SORT <ls_view>-view_containers.
      SORT <ls_view>-view_container_texts.
      SORT <ls_view>-iobound_plugs.
      SORT <ls_view>-iobound_plug_texts.
      SORT <ls_view>-plug_parameters.
      SORT <ls_view>-plug_parameter_texts.
      SORT <ls_view>-ui_elements.
      SORT <ls_view>-ui_context_bindings.
      SORT <ls_view>-ui_event_bindings.
      SORT <ls_view>-ui_ddic_bindings.
      SORT <ls_view>-ui_properties.
      SORT <ls_view>-navigation_links.
      SORT <ls_view>-navigation_target_refs.
      SORT <ls_view>-vsh_nodes.
      SORT <ls_view>-vsh_placeholders.
      SORT <ls_view>-viewset_properties.
    ENDLOOP.

    SORT mt_components BY
      component_name ASCENDING
      controller_name ASCENDING
      cmpname ASCENDING.

    SORT mt_sources BY
      component_name ASCENDING
      controller_name ASCENDING
      cmpname ASCENDING
      line_number ASCENDING.

  ENDMETHOD.
  METHOD read_controller.

    DATA: lt_components   TYPE TABLE OF wdy_ctlr_compo_vrs,
          lt_sources      TYPE TABLE OF wdy_ctlr_compo_source_vrs,
          lt_definition   TYPE TABLE OF wdy_controller,
          lt_psmodilog    TYPE TABLE OF smodilog,
          lt_psmodisrc    TYPE TABLE OF smodisrc,
          lt_fm_param     TYPE abap_func_parmbind_tab,
          lt_fm_exception TYPE abap_func_excpbind_tab.

    FIELD-SYMBOLS: <lt_ctrl_exceptions>      TYPE ANY TABLE,
                   <lt_ctrl_exception_texts> TYPE ANY TABLE.

*   Calling FM dynamically because version 702 has less parameters

*   FM parameters
    add_fm_param_exporting( EXPORTING iv_name     = 'CONTROLLER_KEY'
                                      ig_value    = is_key
                            CHANGING  ct_param = lt_fm_param ).
    add_fm_param_exporting( EXPORTING iv_name     = 'GET_ALL_TRANSLATIONS'
                                      ig_value    = abap_false
                            CHANGING  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'DEFINITION'
                         CHANGING  ct_value = lt_definition
                                   ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'DESCRIPTIONS'
                         CHANGING ct_value = rs_controller-descriptions
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'CONTROLLER_USAGES'
                         CHANGING ct_value = rs_controller-controller_usages
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'CONTROLLER_COMPONENTS'
                         CHANGING ct_value = lt_components
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'CONTROLLER_COMPONENT_SOURCES'
                         CHANGING ct_value = lt_sources
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'CONTROLLER_COMPONENT_TEXTS'
                         CHANGING ct_value = rs_controller-controller_component_texts
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'CONTROLLER_PARAMETERS'
                         CHANGING ct_value = rs_controller-controller_parameters
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'CONTROLLER_PARAMETER_TEXTS'
                         CHANGING ct_value = rs_controller-controller_parameter_texts
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'CONTEXT_NODES'
                         CHANGING ct_value = rs_controller-context_nodes
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'CONTEXT_ATTRIBUTES'
                         CHANGING ct_value = rs_controller-context_attributes
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'CONTEXT_MAPPINGS'
                         CHANGING ct_value = rs_controller-context_mappings
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'FIELDGROUPS'
                         CHANGING ct_value = rs_controller-fieldgroups
                                  ct_param = lt_fm_param ).
*   Version 702 doesn't have these two attributes so we
*   use them dynamically for downward compatibility
    ASSIGN COMPONENT 'CONTROLLER_EXCEPTIONS' OF STRUCTURE rs_controller TO <lt_ctrl_exceptions>.
    IF sy-subrc = 0.
      add_fm_param_tables( EXPORTING iv_name = 'CONTROLLER_EXCEPTIONS'
                           CHANGING ct_value = <lt_ctrl_exceptions>
                                    ct_param = lt_fm_param ).
    ENDIF.
    ASSIGN COMPONENT 'CONTROLLER_EXCEPTION_TEXTS' OF STRUCTURE rs_controller TO <lt_ctrl_exception_texts>.
    IF sy-subrc = 0.
      add_fm_param_tables( EXPORTING iv_name = 'CONTROLLER_EXCEPTION_TEXTS'
                           CHANGING ct_value = <lt_ctrl_exception_texts>
                                    ct_param = lt_fm_param ).
    ENDIF.
    add_fm_param_tables( EXPORTING iv_name = 'PSMODILOG'
                         CHANGING ct_value = lt_psmodilog
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'PSMODISRC'
                         CHANGING ct_value = lt_psmodisrc
                                  ct_param = lt_fm_param ).

*   FM exceptions
    add_fm_exception( EXPORTING iv_name = 'NOT_EXISTING'
                                iv_value = 1
                      CHANGING ct_exception = lt_fm_exception ).
    add_fm_exception( EXPORTING iv_name = 'OTHERS'
                                iv_value = 2
                      CHANGING ct_exception = lt_fm_exception ).

    CALL FUNCTION 'WDYC_GET_OBJECT'
      PARAMETER-TABLE
      lt_fm_param
      EXCEPTION-TABLE
      lt_fm_exception.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from WDYC_GET_OBJECT' ).
    ENDIF.

    APPEND LINES OF lt_components TO mt_components.
    APPEND LINES OF lt_sources TO mt_sources.

    READ TABLE lt_definition INDEX 1 INTO rs_controller-definition.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'WDYC, definition not found' ).
    ENDIF.

    CLEAR: rs_controller-definition-author,
           rs_controller-definition-createdon,
           rs_controller-definition-changedby,
           rs_controller-definition-changedon.

  ENDMETHOD.
  METHOD read_definition.

    DATA: lt_definition TYPE TABLE OF wdy_component,
          lt_psmodilog  TYPE TABLE OF smodilog,
          lt_psmodisrc  TYPE TABLE OF smodisrc.


    CALL FUNCTION 'WDYD_GET_OBJECT'
      EXPORTING
        component_key           = is_key
        get_all_translations    = abap_false
      TABLES
        definition              = lt_definition
        descriptions            = rs_definition-descriptions
        component_usages        = rs_definition-component_usages
        interface_implementings = rs_definition-interface_implementings
        library_usages          = rs_definition-library_usages
        ext_ctlr_usages         = rs_definition-ext_ctlr_usages
        ext_ctx_mappings        = rs_definition-ext_ctx_mappings
        psmodilog               = lt_psmodilog " not optional in all versions
        psmodisrc               = lt_psmodisrc " not optional in all versions
      EXCEPTIONS
        not_existing            = 1
        OTHERS                  = 2.
    IF sy-subrc = 1.
      RETURN.
    ELSEIF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from WDYD_GET_OBJECT' ).
    ENDIF.

    READ TABLE lt_definition INDEX 1 INTO rs_definition-definition.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'WDYD, definition not found' ).
    ENDIF.

    CLEAR: rs_definition-definition-author,
           rs_definition-definition-createdon,
           rs_definition-definition-changedby,
           rs_definition-definition-changedon,
           rs_definition-definition-gendate,
           rs_definition-definition-gentime.

  ENDMETHOD.
  METHOD read_view.

    DATA: lt_definition TYPE TABLE OF wdy_view_vrs,
          lt_psmodilog  TYPE TABLE OF smodilog,
          lt_psmodisrc  TYPE TABLE OF smodisrc.

    FIELD-SYMBOLS: <ls_definition> LIKE LINE OF lt_definition.


    CALL FUNCTION 'WDYV_GET_OBJECT'
      EXPORTING
        view_key               = is_key
        get_all_translations   = abap_false
      TABLES
        definition             = lt_definition
        descriptions           = rs_view-descriptions
        view_containers        = rs_view-view_containers
        view_container_texts   = rs_view-view_container_texts
        iobound_plugs          = rs_view-iobound_plugs
        iobound_plug_texts     = rs_view-iobound_plug_texts
        plug_parameters        = rs_view-plug_parameters
        plug_parameter_texts   = rs_view-plug_parameter_texts
        ui_elements            = rs_view-ui_elements
        ui_context_bindings    = rs_view-ui_context_bindings
        ui_event_bindings      = rs_view-ui_event_bindings
        ui_ddic_bindings       = rs_view-ui_ddic_bindings
        ui_properties          = rs_view-ui_properties
        navigation_links       = rs_view-navigation_links
        navigation_target_refs = rs_view-navigation_target_refs
        vsh_nodes              = rs_view-vsh_nodes
        vsh_placeholders       = rs_view-vsh_placeholders
        viewset_properties     = rs_view-viewset_properties
        psmodilog              = lt_psmodilog
        psmodisrc              = lt_psmodisrc
      EXCEPTIONS
        not_existing           = 1
        OTHERS                 = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from WDYV_GET_OBJECT' ).
    ENDIF.

    READ TABLE lt_definition INDEX 1 ASSIGNING <ls_definition>.
    ASSERT sy-subrc = 0.
    MOVE-CORRESPONDING <ls_definition> TO rs_view-definition.

    CLEAR: rs_view-definition-author,
           rs_view-definition-createdon,
           rs_view-definition-changedby,
           rs_view-definition-changedon.

  ENDMETHOD.
  METHOD recover_controller.

    DATA: ls_key    TYPE wdy_controller_key,
          lv_corrnr TYPE trkorr,
          lx_error  TYPE REF TO cx_wdy_md_exception,
          ls_delta  TYPE svrs2_xversionable_object.


    ls_delta = delta_controller( is_controller ).
    ls_key-component_name  = is_controller-definition-component_name.
    ls_key-controller_name = is_controller-definition-controller_name.

    TRY.
        cl_wdy_md_controller=>recover_version(
          EXPORTING
            controller_key = ls_key
            delta          = ls_delta-wdyc
          CHANGING
            corrnr         = lv_corrnr ).
      CATCH cx_wdy_md_exception INTO lx_error.
        Lcx_abapgit_exception=>raise( |Error recovering version of controller: { lx_error->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.
  METHOD recover_definition.

    DATA: ls_key    TYPE wdy_md_component_key,
          lv_corrnr TYPE trkorr,
          lx_error  TYPE REF TO cx_wdy_md_exception,
          ls_delta  TYPE svrs2_xversionable_object.


    ls_delta = delta_definition(
      is_definition = is_definition
      iv_package    = iv_package ).

    ls_key-component_name = is_definition-definition-component_name.

    TRY.
        cl_wdy_md_component=>recover_version(
          EXPORTING
            component_key = ls_key
            delta         = ls_delta-wdyd
          CHANGING
            corrnr        = lv_corrnr ).
      CATCH cx_wdy_md_exception INTO lx_error.
        Lcx_abapgit_exception=>raise( |Error recovering version of component: { lx_error->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.
  METHOD recover_view.

    DATA: ls_key    TYPE wdy_md_view_key,
          lv_corrnr TYPE trkorr,
          lx_error  TYPE REF TO cx_wdy_md_exception,
          ls_delta  TYPE svrs2_xversionable_object.


    ls_delta = delta_view( is_view ).
    ls_key-component_name = is_view-definition-component_name.
    ls_key-view_name      = is_view-definition-view_name.

    TRY.
        cl_wdy_md_abstract_view=>recover_version(
          EXPORTING
            view_key = ls_key
            delta    = ls_delta-wdyv
          CHANGING
            corrnr   = lv_corrnr ).
      CATCH cx_wdy_md_exception INTO lx_error.
        Lcx_abapgit_exception=>raise( |Error recovering version of abstract view: { lx_error->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    SELECT SINGLE changedby FROM wdy_component INTO rv_user
      WHERE component_name = ms_item-obj_name AND version = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lo_component   TYPE REF TO cl_wdy_wb_component,
          lo_request     TYPE REF TO cl_wb_request,
          li_state       TYPE REF TO if_wb_program_state,
          lv_object_name TYPE seu_objkey.


    CREATE OBJECT lo_component.

    lv_object_name = ms_item-obj_name.
    CREATE OBJECT lo_request
      EXPORTING
        p_object_type = 'YC'
        p_object_name = lv_object_name
        p_operation   = swbm_c_op_delete_no_dialog.

    lo_component->if_wb_program~process_wb_request(
      p_wb_request       = lo_request
      p_wb_program_state = li_state ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_component   TYPE wdy_component_metadata,
          ls_description TYPE wdy_ext_ctx_map.

    FIELD-SYMBOLS: <ls_view>       LIKE LINE OF ls_component-view_metadata,
                   <ls_controller> LIKE LINE OF ls_component-ctlr_metadata.

    io_xml->read( EXPORTING iv_name = 'COMPONENT'
                  CHANGING cg_data = ls_component ).
    io_xml->read( EXPORTING iv_name  = 'COMPONENTS'
                  CHANGING cg_data = mt_components ).
    io_xml->read( EXPORTING iv_name  = 'SOURCES'
                  CHANGING cg_data = mt_sources ).

    ls_component-comp_metadata-definition-author = sy-uname.
    ls_component-comp_metadata-definition-createdon = sy-datum.
    recover_definition( is_definition = ls_component-comp_metadata
                        iv_package    = iv_package ).

    LOOP AT ls_component-ctlr_metadata ASSIGNING <ls_controller>.
      <ls_controller>-definition-author = sy-uname.
      <ls_controller>-definition-createdon = sy-datum.
      recover_controller( <ls_controller> ).
    ENDLOOP.
    LOOP AT ls_component-view_metadata ASSIGNING <ls_view>.
      <ls_view>-definition-author = sy-uname.
      <ls_view>-definition-createdon = sy-datum.
      recover_view( <ls_view> ).
    ENDLOOP.

    READ TABLE ls_component-comp_metadata-descriptions INTO ls_description INDEX 1.
    IF sy-subrc = 0.
      Lcl_abapgit_sotr_handler=>create_sotr(
        iv_package = iv_package
        io_xml     = io_xml ).
    ENDIF.

    add_with_inactive_parts( ).

    deserialize_longtexts(
      ii_xml         = io_xml
      iv_longtext_id = c_longtext_id_wd ).

    deserialize_longtexts(
      ii_xml           = io_xml
      iv_longtext_id   = c_longtext_id_wc
      iv_longtext_name = c_longtext_name_wc ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_component_name TYPE wdy_component-component_name.


    SELECT SINGLE component_name FROM wdy_component
      INTO lv_component_name
      WHERE component_name = ms_item-obj_name.          "#EC CI_GENBUFF
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

    DATA: ls_component   TYPE wdy_component_metadata,
          ls_comp        TYPE wdy_ctlr_compo_vrs,
          lv_object      TYPE dokil-object,
          lt_object      TYPE STANDARD TABLE OF dokil-object WITH DEFAULT KEY,
          lt_dokil       TYPE STANDARD TABLE OF dokil WITH DEFAULT KEY,
          ls_description TYPE wdy_ext_ctx_map.

    ls_component = read( ).

    io_xml->add( iv_name = 'COMPONENT'
                 ig_data = ls_component ).
    io_xml->add( ig_data = mt_components
                 iv_name = 'COMPONENTS' ).
    io_xml->add( ig_data = mt_sources
                 iv_name = 'SOURCES' ).

    READ TABLE ls_component-comp_metadata-descriptions INTO ls_description INDEX 1.
    IF sy-subrc = 0.
      Lcl_abapgit_sotr_handler=>read_sotr(
        iv_pgmid    = 'LIMU'
        iv_object   = 'WDYV'
        iv_obj_name = ms_item-obj_name
        io_i18n_params = mo_i18n_params
        io_xml      = io_xml ).
    ENDIF.

    serialize_longtexts(
      ii_xml         = io_xml
      iv_longtext_id = c_longtext_id_wd ).

    LOOP AT mt_components INTO ls_comp.
      lv_object    = ls_comp-component_name.
      lv_object+30 = ls_comp-controller_name.
      COLLECT lv_object INTO lt_object.
    ENDLOOP.

    IF lt_object IS NOT INITIAL.
      IF mo_i18n_params->ms_params-main_language_only = abap_true.
        SELECT * FROM dokil INTO TABLE lt_dokil
          FOR ALL ENTRIES IN lt_object
          WHERE id = c_longtext_id_wc AND object = lt_object-table_line AND masterlang = abap_true
          ORDER BY PRIMARY KEY.
      ELSE.
        SELECT * FROM dokil INTO TABLE lt_dokil
          FOR ALL ENTRIES IN lt_object
          WHERE id = c_longtext_id_wc AND object = lt_object-table_line
          ORDER BY PRIMARY KEY.
      ENDIF.

      serialize_longtexts(
        ii_xml           = io_xml
        it_dokil         = lt_dokil
        iv_longtext_id   = c_longtext_id_wc
        iv_longtext_name = c_longtext_name_wc ).
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
endclass. "ZCL_ABAPGIT_OBJECT_WDYN implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_WEBI <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_webi=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_webi=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_WEBI implementation.
*"* method's implementations
*include methods.
  METHOD handle_endpoint.

    DATA: ls_endpoint LIKE LINE OF is_webi-pvependpoint,
          li_endpoint TYPE REF TO if_ws_md_vif_endpoint_ref.

    FIELD-SYMBOLS: <ls_function> LIKE LINE OF is_webi-pvepfunction.


    READ TABLE is_webi-pvependpoint INDEX 1 INTO ls_endpoint.
    ASSERT sy-subrc = 0.

    IF mi_vi->has_endpoint_reference( sews_c_vif_version-all ) = abap_true.
      RETURN.
    ENDIF.

    li_endpoint = mi_vi->create_endpoint_reference(
      endpoint_type          = ls_endpoint-endpointtype
      service_def_startpoint = ls_endpoint-def_start_pt
      auto_generated         = ls_endpoint-auto_generated
      i_is_srvv              = ls_endpoint-is_srvv ).

    IF ls_endpoint-endpointtype = 'BAPI'.
* it looks like some special handling is needed when calling
* set_data, and looking at the cluster data LS_ENDPOINT-CLUSTD
      Lcx_abapgit_exception=>raise( 'todo, WEBI BAPI' ).
    ENDIF.

* field ls_endpoint-endpointname does not exist in 702
    READ TABLE is_webi-pvepfunction INDEX 1 ASSIGNING <ls_function>.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |WEBI { ms_item-obj_name }: couldn't detect endpoint name| ).
    ENDIF.

    li_endpoint->set_data(
      data_version = '1'
      data         = <ls_function>-function ).

  ENDMETHOD.
  METHOD handle_function.



    DATA: li_parameter TYPE REF TO if_ws_md_vif_param,
          li_soap      TYPE REF TO if_ws_md_soap_ext_func,
          li_fault     TYPE REF TO if_ws_md_vif_fault,
          li_function  TYPE REF TO if_ws_md_vif_func.

    FIELD-SYMBOLS: <ls_function>  LIKE LINE OF is_webi-pvepfunction,
                   <ls_soap>      LIKE LINE OF is_webi-pvepfuncsoapext,
                   <ls_fault>     LIKE LINE OF is_webi-pvepfault,
                   <ls_parameter> LIKE LINE OF is_webi-pvepparameter.


    LOOP AT is_webi-pvepfunction ASSIGNING <ls_function>.

      IF mi_vi->has_function( funcname = <ls_function>-function
                              version  = sews_c_vif_version-active ) = abap_true.
        CONTINUE.
      ENDIF.

      IF mi_vi->has_function( funcname = <ls_function>-function
                              version  = sews_c_vif_version-inactive ) = abap_true.

        li_function = mi_vi->get_function( funcname = <ls_function>-function
                                           version  = sews_c_vif_version-inactive ).

      ELSE.

        li_function = mi_vi->create_function( funcname    = <ls_function>-function
                                              mapped_name = <ls_function>-mappedname ).

      ENDIF.

      li_function->set_is_exposed( <ls_function>-is_exposed ).

      LOOP AT is_webi-pvepparameter ASSIGNING <ls_parameter>
          WHERE function = <ls_function>-function.

        li_parameter = handle_single_parameter( iv_name           = <ls_parameter>-vepparam
                                                ii_function       = li_function
                                                iv_parameter_type = <ls_parameter>-vepparamtype ).

        li_parameter->set_name_mapped_to( <ls_parameter>-mappedname ).
        li_parameter->set_is_exposed( <ls_parameter>-is_exposed ).
        li_parameter->set_is_optional( <ls_parameter>-is_optional ).
        li_parameter->set_default_value( <ls_parameter>-default_value ).
        li_parameter->set_initial( <ls_parameter>-is_initial ).
        li_parameter->set_type( <ls_parameter>-typename ).
      ENDLOOP.

      LOOP AT is_webi-pvepfuncsoapext ASSIGNING <ls_soap>
          WHERE function = <ls_function>-function.
        IF li_function->has_soap_extension_function( 'I' ) = abap_true.
          li_function->delete_soap_extension_function( ).
        ENDIF.
        li_soap = li_function->create_soap_extension_function( ).
        li_soap->set_soap_request_name( <ls_soap>-requestname ).
        li_soap->set_soap_response_name( <ls_soap>-responsename ).
        li_soap->set_namespace( <ls_soap>-namespace ).
      ENDLOOP.

      LOOP AT is_webi-pvepfault ASSIGNING <ls_fault>
          WHERE function = <ls_function>-function.
        li_fault = li_function->create_fault( <ls_fault>-fault ).
        li_fault->set_name_mapped_to( <ls_fault>-mappedname ).
        li_fault->set_detail( <ls_fault>-detail ).
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.
  METHOD handle_single_parameter.
    CONSTANTS:
      BEGIN OF lc_parameter_type,
        import TYPE vepparamtype VALUE 'I',
        export TYPE vepparamtype VALUE 'O',
      END OF lc_parameter_type.

    CASE iv_parameter_type.
      WHEN lc_parameter_type-import.
        ri_parameter = ii_function->get_incoming_parameter( parameter_name  = iv_name
                                                            version         = 'I' ).
        IF ri_parameter IS BOUND.
          ii_function->delete_incoming_parameter( ri_parameter ).
        ENDIF.
        ri_parameter = ii_function->create_incoming_parameter( iv_name ).

      WHEN lc_parameter_type-export.

        ri_parameter = ii_function->get_outgoing_parameter( parameter_name  = iv_name
                                                            version         = 'I' ).
        IF ri_parameter IS BOUND.
          ii_function->delete_outgoing_parameter( parameter = ri_parameter ).
        ENDIF.

        ri_parameter = ii_function->create_outgoing_parameter( iv_name ).

      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.
  METHOD handle_soap.

    DATA: li_soap TYPE REF TO if_ws_md_soap_ext_virtinfc,
          ls_soap LIKE LINE OF is_webi-pvepvisoapext.


    READ TABLE is_webi-pvepvisoapext INDEX 1 INTO ls_soap.
    ASSERT sy-subrc = 0.

    IF mi_vi->has_soap_extension_virtinfc( sews_c_vif_version-active ) = abap_true.
      RETURN.
    ENDIF.

    IF mi_vi->has_soap_extension_virtinfc( sews_c_vif_version-inactive ) = abap_true.
      li_soap = mi_vi->get_soap_extension_virtinfc( sews_c_vif_version-inactive ).
    ELSE.
      li_soap = mi_vi->create_soap_extension_virtinfc( ls_soap-soap_appl_uri ).
    ENDIF.

    li_soap->set_namespace( ls_soap-namespace ).

  ENDMETHOD.
  METHOD handle_types.

    DATA: lv_index TYPE i,
          li_soap  TYPE REF TO if_ws_md_soap_extension_type,
          li_struc TYPE REF TO if_ws_md_vif_struc_type,
          li_field TYPE REF TO if_ws_md_vif_field,
          li_table TYPE REF TO if_ws_md_vif_table_type,
          li_elem  TYPE REF TO if_ws_md_vif_elem_type.

    FIELD-SYMBOLS: <ls_elem>  LIKE LINE OF is_webi-pvepelemtype,
                   <ls_table> LIKE LINE OF is_webi-pveptabletype,
                   <ls_soap>  LIKE LINE OF is_webi-pveptypesoapext,
                   <ls_struc> LIKE LINE OF is_webi-pvepstrutype.


    LOOP AT is_webi-pvepelemtype ASSIGNING <ls_elem>.
      li_elem = mi_vi->create_type_as_elementary( <ls_elem>-typename ).
      li_elem->set_built_in_type( <ls_elem>-build_in_type ).
      li_elem->set_decimals( <ls_elem>-decimals ).
      li_elem->set_kind( <ls_elem>-kind ).
      li_elem->set_length( <ls_elem>-length ).
      li_elem->set_signed( <ls_elem>-signed ).
      li_elem->set_abaptype( <ls_elem>-abaptype ).

      IF li_elem->if_ws_md_vif_type~has_soap_extension_type( sews_c_vif_version-all ) = abap_false.
        READ TABLE is_webi-pveptypesoapext ASSIGNING <ls_soap>
          WITH KEY typename = <ls_elem>-typename.
        IF sy-subrc = 0.
          li_soap = li_elem->if_ws_md_vif_type~create_soap_extension_type( ).
          li_soap->set_namespace( <ls_soap>-namespace ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT is_webi-pvepstrutype ASSIGNING <ls_struc>.
      lv_index = sy-tabix.

      li_struc = mi_vi->create_type_as_structure( <ls_struc>-typename ).

      IF li_struc->has_field( field_pos = <ls_struc>-fieldpos
          version = sews_c_vif_version-active ) = abap_true.
        CONTINUE.
      ENDIF.

      li_field = li_struc->create_field(
        field_name = <ls_struc>-fieldname
        fieldpos = <ls_struc>-fieldpos ).
      li_field->set_type( mi_vi->get_type( typename = <ls_struc>-typeref
                                           version  = sews_c_vif_version-inactive ) ).

      IF lv_index = 1
          AND li_struc->if_ws_md_vif_type~has_soap_extension_type(
          sews_c_vif_version-all ) = abap_false.
        READ TABLE is_webi-pveptypesoapext ASSIGNING <ls_soap>
          WITH KEY typename = <ls_struc>-typename.
        IF sy-subrc = 0.
          li_soap = li_struc->if_ws_md_vif_type~create_soap_extension_type( ).
          li_soap->set_namespace( <ls_soap>-namespace ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT is_webi-pveptabletype ASSIGNING <ls_table>.
      li_table = mi_vi->create_type_as_table( <ls_table>-typename ).
      li_table->set_line_type( mi_vi->get_type( typename = <ls_table>-typeref
                                                version  = sews_c_vif_version-inactive ) ).

      IF li_table->if_ws_md_vif_type~has_soap_extension_type( sews_c_vif_version-all ) = abap_false.
        READ TABLE is_webi-pveptypesoapext ASSIGNING <ls_soap>
          WITH KEY typename = <ls_table>-typename.
        IF sy-subrc = 0.
          li_soap = li_table->if_ws_md_vif_type~create_soap_extension_type( ).
          li_soap->set_namespace( <ls_soap>-namespace ).
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD sort.
    SORT cs_webi-pvepheader BY vepname version.
    SORT cs_webi-pvepfunction BY vepname version function.
    SORT cs_webi-pvepfault BY vepname version function fault.
    SORT cs_webi-pvepparameter BY vepname version function vepparam vepparamtype.
    SORT cs_webi-pveptype BY vepname version typename.
    SORT cs_webi-pvepelemtype BY vepname version typename.
    SORT cs_webi-pveptabletype BY vepname version typename.
    SORT cs_webi-pvepstrutype BY vepname version typename fieldpos.
    SORT cs_webi-pveptypesoapext BY vepname version typename.
    SORT cs_webi-pvepeletypsoap BY vepname version typename assign_type assign_data1 assign_data2.
    SORT cs_webi-pveptabtypsoap BY vepname version typename.
    SORT cs_webi-pvepfuncsoapext BY vepname version function.
    SORT cs_webi-pvepfieldref BY vepname version function vepparam vepparamtype strucid fieldname.
    SORT cs_webi-pvependpoint BY relid vepname version sortfield.
    SORT cs_webi-pvepvisoapext BY vepname version.
    SORT cs_webi-pvepparasoapext BY vepname version function vepparam vepparamtype.
    SORT cs_webi-pwsheader BY wsname version.
    SORT cs_webi-pwssoapprop BY wsname version feature soapapp funcref propnum.
  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE changedby FROM vepheader INTO rv_user
      WHERE vepname = ms_item-obj_name AND version = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_name TYPE vepname,
          lo_vif  TYPE REF TO cl_ws_md_vif_root.


    lv_name = ms_item-obj_name.

    CREATE OBJECT lo_vif.
    TRY.
        lo_vif->if_ws_md_vif_root~delete_virtual_interface( lv_name ).
      CATCH cx_ws_md_exception.
        Lcx_abapgit_exception=>raise( 'error deleting WEBI' ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_webi     TYPE ty_webi,
          lv_name     TYPE vepname,
          ls_header   LIKE LINE OF ls_webi-pvepheader,
          lx_root     TYPE REF TO cx_root,
          lv_exists   TYPE abap_bool,
          li_root     TYPE REF TO if_ws_md_vif_root,
          ls_endpoint LIKE LINE OF ls_webi-pvependpoint.

    io_xml->read( EXPORTING iv_name = 'WEBI'
                  CHANGING  cg_data = ls_webi ).

    lv_name = ms_item-obj_name.

    READ TABLE ls_webi-pvependpoint INDEX 1 INTO ls_endpoint.
    ASSERT sy-subrc = 0.
    IF ls_endpoint-auto_generated = abap_true.
      " handled by SPRX.
      RETURN.
    ENDIF.

    READ TABLE ls_webi-pvepheader INDEX 1 INTO ls_header.
    ASSERT sy-subrc = 0.

    lv_exists = cl_ws_md_vif_root=>check_existence_by_vif_name(
      name      = lv_name
      i_version = sews_c_vif_version-all ).

    li_root = cl_ws_md_factory=>get_vif_root( ).
    TRY.
        IF lv_exists = abap_false.
          mi_vi = li_root->create_virtual_interface(
            name    = lv_name
            nameext = ls_header-vepnameext ).
        ELSE.
          mi_vi = li_root->get_virtual_interface( lv_name ).
          mi_vi->if_ws_md_lockable_object~lock( ).
        ENDIF.

        mi_vi->set_short_text( ls_webi-veptext ).

        handle_endpoint( ls_webi ).
        handle_types( ls_webi ).
        handle_function( ls_webi ).
        handle_soap( ls_webi ).

        tadir_insert( iv_package ).

        mi_vi->if_ws_md_lockable_object~save( ).
        mi_vi->if_ws_md_lockable_object~unlock( ).
      CATCH cx_ws_md_exception INTO lx_root.
        TRY.
            mi_vi->if_ws_md_lockable_object~unlock( ).
          CATCH cx_ws_md_exception ##NO_HANDLER.
        ENDTRY.
        Lcx_abapgit_exception=>raise_with_text( lx_root ).
    ENDTRY.

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

    Lcl_abapgit_sotr_handler=>create_sotr(
      iv_package = iv_package
      io_xml     = io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_name TYPE vepname.
    DATA lv_generated TYPE abap_bool.

    lv_name = ms_item-obj_name.

    " Check if service is generated by proxy
    SELECT SINGLE auto_generated FROM vependpoint INTO lv_generated
      WHERE vepname = lv_name AND version = sews_c_vif_version-active.
    IF sy-subrc = 0 AND lv_generated = abap_true.
      RETURN.
    ENDIF.

    rv_bool = cl_ws_md_vif_root=>check_existence_by_vif_name(
      name      = lv_name
      i_version = sews_c_vif_version-all ).

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

    DATA: ls_webi    TYPE ty_webi,
          lx_error   TYPE REF TO cx_ws_md_exception,
          lt_modilog TYPE STANDARD TABLE OF smodilog WITH DEFAULT KEY,
          li_vi      TYPE REF TO if_ws_md_vif,
          lv_name    TYPE vepname.

    FIELD-SYMBOLS: <ls_vepheader>   LIKE LINE OF ls_webi-pvepheader,
                   <ls_vependpoint> LIKE LINE OF ls_webi-pvependpoint,
                   <ls_wsheader>    TYPE wsheader.

    CALL FUNCTION 'WEBI_GET_OBJECT'
      EXPORTING
        webiname          = ms_item-obj_name
      TABLES
        psmodilog         = lt_modilog
        pvepheader        = ls_webi-pvepheader
        pvepfunction      = ls_webi-pvepfunction
        pvepfault         = ls_webi-pvepfault
        pvepparameter     = ls_webi-pvepparameter
        pveptype          = ls_webi-pveptype
        pvepelemtype      = ls_webi-pvepelemtype
        pveptabletype     = ls_webi-pveptabletype
        pvepstrutype      = ls_webi-pvepstrutype
        pveptypesoapext   = ls_webi-pveptypesoapext
        pvepeletypsoap    = ls_webi-pvepeletypsoap
        pveptabtypsoap    = ls_webi-pveptabtypsoap
        pvepfuncsoapext   = ls_webi-pvepfuncsoapext
        pvepfieldref      = ls_webi-pvepfieldref
        pvependpoint      = ls_webi-pvependpoint
        pvepvisoapext     = ls_webi-pvepvisoapext
        pvepparasoapext   = ls_webi-pvepparasoapext
        pwsheader         = ls_webi-pwsheader
        pwssoapprop       = ls_webi-pwssoapprop
      EXCEPTIONS
        version_not_found = 1
        webi_not_exist    = 2
        OTHERS            = 3.
    IF sy-subrc = 1.
      " no active version
      RETURN.
    ELSEIF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    sort( CHANGING cs_webi = ls_webi ).

    lv_name = ms_item-obj_name.
    TRY.
        li_vi = cl_ws_md_factory=>get_vif_root( )->get_virtual_interface( lv_name ).
        ls_webi-veptext = li_vi->get_short_text( sews_c_vif_version-active ).
      CATCH cx_ws_md_exception INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    LOOP AT ls_webi-pvepheader ASSIGNING <ls_vepheader>.
      CLEAR <ls_vepheader>-author.
      CLEAR <ls_vepheader>-createdon.
      CLEAR <ls_vepheader>-changedby.
      CLEAR <ls_vepheader>-changedon.
      CLEAR <ls_vepheader>-ctime.
      CLEAR <ls_vepheader>-text_id.
      CLEAR <ls_vepheader>-utime.
      CLEAR <ls_vepheader>-wsint_version.
    ENDLOOP.

    LOOP AT ls_webi-pvependpoint ASSIGNING <ls_vependpoint>.
      CLEAR <ls_vependpoint>-clustd.
    ENDLOOP.

    LOOP AT ls_webi-pwsheader ASSIGNING <ls_wsheader>.

      CLEAR:
        <ls_wsheader>-author,
        <ls_wsheader>-createdon,
        <ls_wsheader>-changedby,
        <ls_wsheader>-changedon,
        <ls_wsheader>-ctime,
        <ls_wsheader>-utime.

    ENDLOOP.

    io_xml->add( iv_name = 'WEBI'
                 ig_data = ls_webi ).

    Lcl_abapgit_sotr_handler=>read_sotr(
      iv_pgmid    = 'R3TR'
      iv_object   = ms_item-obj_type
      iv_obj_name = ms_item-obj_name
      io_i18n_params = mo_i18n_params
      io_xml      = io_xml ).

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
endclass. "ZCL_ABAPGIT_OBJECT_WEBI implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_DCLS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_dcls=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_dcls=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_DCLS implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.
    DATA: lr_data  TYPE REF TO data,
          lo_dcl   TYPE REF TO object,
          lx_error TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_data>  TYPE any,
                   <lg_field> TYPE any.

    CREATE DATA lr_data TYPE ('ACM_S_DCLSRC').
    ASSIGN lr_data->* TO <lg_data>.

    TRY.
        CALL METHOD ('CL_ACM_DCL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            ro_handler = lo_dcl.

        CALL METHOD lo_dcl->('READ')
          EXPORTING
            iv_dclname = ms_item-obj_name
          IMPORTING
            es_dclsrc  = <lg_data>.

        ASSIGN COMPONENT 'AS4USER' OF STRUCTURE <lg_data> TO <lg_field>.
        IF sy-subrc = 0.
          rv_user = <lg_field>.
        ELSE.
          rv_user = c_user_unknown.
        ENDIF.
      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lo_dcl   TYPE REF TO object,
          lx_error TYPE REF TO cx_root.

    TRY.
        CALL METHOD ('CL_ACM_DCL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            ro_handler = lo_dcl.

        CALL METHOD lo_dcl->('DELETE')
          EXPORTING
            iv_dclname = ms_item-obj_name.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lr_data  TYPE REF TO data,
          lo_dcl   TYPE REF TO object,
          lx_error TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_data>  TYPE any,
                   <lg_field> TYPE any.


    CREATE DATA lr_data TYPE ('ACM_S_DCLSRC').
    ASSIGN lr_data->* TO <lg_data>.

    io_xml->read(
      EXPORTING
        iv_name = 'DCLS'
      CHANGING
        cg_data = <lg_data> ).

    ASSIGN COMPONENT 'SOURCE' OF STRUCTURE <lg_data> TO <lg_field>.
    ASSERT sy-subrc = 0.
    <lg_field> = Lif_abapgit_object~mo_files->read_string( 'asdcls' ).

    TRY.
        tadir_insert( iv_package ).

        CALL METHOD ('CL_ACM_DCL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            ro_handler = lo_dcl.

        CALL METHOD lo_dcl->('SAVE')
          EXPORTING
            iv_dclname     = ms_item-obj_name
            iv_put_state   = 'I'
            is_dclsrc      = <lg_data>
            iv_devclass    = iv_package
            iv_access_mode = 'INSERT'.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lo_dcl   TYPE REF TO object,
          lx_error TYPE REF TO cx_root.

    TRY.
        CALL METHOD ('CL_ACM_DCL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            ro_handler = lo_dcl.

        CALL METHOD lo_dcl->('CHECK_EXISTENCE')
          EXPORTING
            iv_objectname = ms_item-obj_name
          RECEIVING
            rv_exists     = rv_bool.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
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

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'E_ACMDCLSRC'
                                            iv_argument    = |{ ms_item-obj_name }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_ADT_LINK=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lr_data  TYPE REF TO data,
          lo_dcl   TYPE REF TO object,
          lx_error TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_data>  TYPE any,
                   <lg_field> TYPE any.


    CREATE DATA lr_data TYPE ('ACM_S_DCLSRC').
    ASSIGN lr_data->* TO <lg_data>.

    TRY.
        CALL METHOD ('CL_ACM_DCL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            ro_handler = lo_dcl.

        CALL METHOD lo_dcl->('READ')
          EXPORTING
            iv_dclname = ms_item-obj_name
          IMPORTING
            es_dclsrc  = <lg_data>.

        ASSIGN COMPONENT 'AS4USER' OF STRUCTURE <lg_data> TO <lg_field>.
        ASSERT sy-subrc = 0.
        CLEAR <lg_field>.

        ASSIGN COMPONENT 'AS4DATE' OF STRUCTURE <lg_data> TO <lg_field>.
        ASSERT sy-subrc = 0.
        CLEAR <lg_field>.

        ASSIGN COMPONENT 'AS4TIME' OF STRUCTURE <lg_data> TO <lg_field>.
        ASSERT sy-subrc = 0.
        CLEAR <lg_field>.

        ASSIGN COMPONENT 'CREATED_BY' OF STRUCTURE <lg_data> TO <lg_field>.
        ASSERT sy-subrc = 0.
        CLEAR <lg_field>.

        ASSIGN COMPONENT 'CREATED_DATE' OF STRUCTURE <lg_data> TO <lg_field>.
        ASSERT sy-subrc = 0.
        CLEAR <lg_field>.

        ASSIGN COMPONENT 'AS4LOCAL' OF STRUCTURE <lg_data> TO <lg_field>.
        ASSERT sy-subrc = 0.
        CLEAR <lg_field>.

        ASSIGN COMPONENT 'ABAP_LANGUAGE_VERSION' OF STRUCTURE <lg_data> TO <lg_field>.
        IF sy-subrc = 0.
          CLEAR <lg_field>.
        ENDIF.

        ASSIGN COMPONENT 'SOURCE' OF STRUCTURE <lg_data> TO <lg_field>.
        ASSERT sy-subrc = 0.

        Lif_abapgit_object~mo_files->add_string(
          iv_ext    = 'asdcls'
          iv_string = <lg_field> ).

        CLEAR <lg_field>.

        io_xml->add( iv_name = 'DCLS'
                     ig_data = <lg_data> ).

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
endclass. "ZCL_ABAPGIT_OBJECT_DCLS implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_DDLS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ddls=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ddls=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_DDLS implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    DATA lo_ddl TYPE REF TO object.

    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).

    TRY.
        CALL METHOD ('CL_DD_DDL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            handler = lo_ddl.
      CATCH cx_root.
        Lcx_abapgit_exception=>raise( 'Object type DDLS is not supported by this system' ).
    ENDTRY.

  ENDMETHOD.
  METHOD format_source_before_serialize.

    DATA:
      lv_len       TYPE i,
      lv_lastchar1 TYPE c,
      lv_lastchar2 TYPE c.

    " New line included in 751+ by CL_DD_DDL_HANDLER=>ADD_BASEOBJS_INFO_TO_DDLS
    " Change for 750-

    lv_len = strlen( cv_string ) - 1.
    IF lv_len < 0.
      RETURN.
    ENDIF.
    lv_lastchar1 = cv_string+lv_len(1).

    lv_len = strlen( cv_string ) - 2.
    IF lv_len < 0.
      RETURN.
    ENDIF.
    lv_lastchar2 = cv_string+lv_len(1).

    " only add a line break, if the last character is unequal to cr_lf and newline !
    IF lv_lastchar1 <> cl_abap_char_utilities=>cr_lf AND lv_lastchar1 <> cl_abap_char_utilities=>newline AND
        lv_lastchar1 <> space OR
        ( lv_lastchar1 = space AND
          ( lv_lastchar2 <> cl_abap_char_utilities=>cr_lf AND lv_lastchar2 <> cl_abap_char_utilities=>newline ) ).
      cv_string = |{ cv_string }{ cl_abap_char_utilities=>cr_lf }|.
    ENDIF.

  ENDMETHOD.
  METHOD is_baseinfo_supported.

    DATA:
      lr_data_baseinfo TYPE REF TO data,
      lx_error         TYPE REF TO cx_root.

    TRY.
        CREATE DATA lr_data_baseinfo TYPE ('IF_DD_DDL_TYPES=>TY_S_BASEINFO_STRING_SAVE').
        rv_supported = abap_true.
      CATCH cx_root INTO lx_error.
        rv_supported = abap_false.
    ENDTRY.

  ENDMETHOD.
  METHOD open_adt_stob.

    DATA: lr_data  TYPE REF TO data,
          lo_ddl   TYPE REF TO object,
          lx_error TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lt_ddnames>     TYPE STANDARD TABLE.
    FIELD-SYMBOLS: <lt_entity_view> TYPE STANDARD TABLE.
    FIELD-SYMBOLS: <lg_ddnames>     TYPE any.
    FIELD-SYMBOLS: <lg_entity_view> TYPE any.
    FIELD-SYMBOLS: <lg_ddname>      TYPE any.
    FIELD-SYMBOLS: <lg_ddlname>     TYPE any.


    TRY.
        CREATE DATA lr_data TYPE ('IF_DD_DDL_TYPES=>TY_T_DDOBJ').
        ASSIGN lr_data->* TO <lt_ddnames>.

        CREATE DATA lr_data LIKE LINE OF <lt_ddnames>.
        ASSIGN lr_data->* TO <lg_ddnames>.

        CREATE DATA lr_data TYPE ('IF_DD_DDL_TYPES=>TY_T_ENTITY_OF_VIEW').
        ASSIGN lr_data->* TO <lt_entity_view>.

        CREATE DATA lr_data LIKE LINE OF <lt_entity_view>.
        ASSIGN lr_data->* TO <lg_entity_view>.

        CLEAR <lt_ddnames>.
        ASSIGN COMPONENT 'NAME' OF STRUCTURE <lg_ddnames> TO <lg_ddname>.
        <lg_ddname> = iv_ddls_name.
        INSERT <lg_ddnames> INTO TABLE <lt_ddnames>.

        CALL METHOD ('CL_DD_DDL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            handler = lo_ddl.

        CALL METHOD lo_ddl->('IF_DD_DDL_HANDLER~GET_VIEWNAME_FROM_ENTITYNAME')
          EXPORTING
            ddnames        = <lt_ddnames>
          IMPORTING
            view_of_entity = <lt_entity_view>.

        READ TABLE <lt_entity_view> ASSIGNING <lg_entity_view> INDEX 1.
        IF sy-subrc = 0.
          ASSIGN COMPONENT 'DDLNAME' OF STRUCTURE <lg_entity_view> TO <lg_ddlname>.

          Lcl_abapgit_adt_link=>jump( iv_obj_name = <lg_ddlname>
                                      iv_obj_type = 'DDLS' ).

        ENDIF.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD read_baseinfo.

    TRY.
        rv_baseinfo_string = Lif_abapgit_object~mo_files->read_string( 'baseinfo' ).

      CATCH Lcx_abapgit_exception.
        " File not found. That's ok, as the object could have been created in a
        " system where baseinfo wasn't supported.
        RETURN.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lo_ddl   TYPE REF TO object,
          lr_data  TYPE REF TO data,
          lx_error TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_data>  TYPE any,
                   <lg_field> TYPE any.


    TRY.
        CREATE DATA lr_data TYPE ('DDDDLSRCV').
        ASSIGN lr_data->* TO <lg_data>.

        CALL METHOD ('CL_DD_DDL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            handler = lo_ddl.

        CALL METHOD lo_ddl->('IF_DD_DDL_HANDLER~READ')
          EXPORTING
            name         = ms_item-obj_name
            get_state    = 'A'
          IMPORTING
            ddddlsrcv_wa = <lg_data>.

        ASSIGN COMPONENT 'AS4USER' OF STRUCTURE <lg_data> TO <lg_field>.
        IF sy-subrc = 0.
          rv_user = <lg_field>.
        ENDIF.
      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    IF rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA:
      lt_deltab TYPE TABLE OF dcdeltb,
      ls_deltab TYPE dcdeltb,
      lt_gentab TYPE TABLE OF dcgentb,
      lv_rc     TYPE sy-subrc.

    " CL_DD_DDL_HANDLER->DELETE does not work for CDS views that reference other views
    " To drop any views regardless of reference, we use delnoref = false
    ls_deltab-objtyp  = 'DDLS'.
    ls_deltab-objname = ms_item-obj_name.
    APPEND ls_deltab TO lt_deltab.

    CALL FUNCTION 'DD_MASS_ACT_C3'
      EXPORTING
        ddmode         = 'O'
        inactive       = abap_true
        write_log      = abap_false
        delall         = abap_true
        delnoref       = abap_false
        prid           = -1
      IMPORTING
        act_rc         = lv_rc
      TABLES
        gentab         = lt_gentab
        deltab         = lt_deltab
      EXCEPTIONS
        access_failure = 1
        no_objects     = 2
        locked         = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA:
      lo_ddl           TYPE REF TO object,
      lr_data          TYPE REF TO data,
      lr_data_baseinfo TYPE REF TO data,
      lx_error         TYPE REF TO cx_root.

    FIELD-SYMBOLS:
      <lg_data>             TYPE any,
      <lg_data_baseinfo>    TYPE any,
      <lg_source>           TYPE any,
      <lg_baseinfo_string>  TYPE any,
      <lg_baseinfo_ddlname> TYPE any.

    TRY.
        CREATE DATA lr_data TYPE ('DDDDLSRCV').
        ASSIGN lr_data->* TO <lg_data>.

        io_xml->read( EXPORTING iv_name = 'DDLS'
                      CHANGING cg_data  = <lg_data> ).

        ASSIGN COMPONENT 'SOURCE' OF STRUCTURE <lg_data> TO <lg_source>.
        ASSERT sy-subrc = 0.
        <lg_source> = Lif_abapgit_object~mo_files->read_string( 'asddls' ).

        CALL METHOD ('CL_DD_DDL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            handler = lo_ddl.

        IF is_baseinfo_supported( ) = abap_true.
          CREATE DATA lr_data_baseinfo TYPE ('IF_DD_DDL_TYPES=>TY_S_BASEINFO_STRING_SAVE').
          ASSIGN lr_data_baseinfo->* TO <lg_data_baseinfo>.

          ASSIGN COMPONENT 'BASEINFO_STRING' OF STRUCTURE <lg_data_baseinfo> TO <lg_baseinfo_string>.
          ASSERT sy-subrc = 0.

          <lg_baseinfo_string> = read_baseinfo( ).

          ASSIGN COMPONENT 'DDLNAME' OF STRUCTURE <lg_data_baseinfo> TO <lg_baseinfo_ddlname>.
          ASSERT sy-subrc = 0.
          <lg_baseinfo_ddlname> = ms_item-obj_name.

          CALL METHOD lo_ddl->('IF_DD_DDL_HANDLER~SAVE')
            EXPORTING
              name            = ms_item-obj_name
              put_state       = 'N'
              ddddlsrcv_wa    = <lg_data>
              baseinfo_string = <lg_data_baseinfo>.
        ELSE.
          CALL METHOD lo_ddl->('IF_DD_DDL_HANDLER~SAVE')
            EXPORTING
              name         = ms_item-obj_name
              put_state    = 'N'
              ddddlsrcv_wa = <lg_data>.
        ENDIF.

        CALL METHOD lo_ddl->('IF_DD_DDL_HANDLER~WRITE_TADIR')
          EXPORTING
            objectname = ms_item-obj_name
            devclass   = iv_package
            prid       = 0.

        corr_insert( iv_package ).

      CATCH cx_root INTO lx_error.
        IF lo_ddl IS NOT INITIAL.
          " Attempt clean-up but catch error if it doesn't work
          TRY.
              CALL METHOD lo_ddl->('IF_DD_DDL_HANDLER~DELETE')
                EXPORTING
                  name = ms_item-obj_name
                  prid = 0.
            CATCH cx_root ##NO_HANDLER.
          ENDTRY.
        ENDIF.

        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_state TYPE objstate,
          lo_ddl   TYPE REF TO object.

    TRY.
        CALL METHOD ('CL_DD_DDL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            handler = lo_ddl.

        CALL METHOD lo_ddl->('IF_DD_DDL_HANDLER~READ')
          EXPORTING
            name      = ms_item-obj_name
          IMPORTING
            got_state = lv_state.
        rv_bool = boolc( NOT lv_state IS INITIAL ).
      CATCH cx_root.
        rv_bool = abap_false.
    ENDTRY.

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

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESDICT'
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: lv_typename   TYPE typename.
    DATA: lv_ddtypekind TYPE ddtypekind.

    lv_typename = ms_item-obj_name.

    CALL FUNCTION 'DDIF_TYPEINFO_GET'
      EXPORTING
        typename = lv_typename
      IMPORTING
        typekind = lv_ddtypekind.

    IF lv_ddtypekind = 'STOB'.
      open_adt_stob( ms_item-obj_name ).
      rv_exit = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lo_ddl           TYPE REF TO object,
          lr_data          TYPE REF TO data,
          lr_data_baseinfo TYPE REF TO data,
          lt_clr_comps     TYPE STANDARD TABLE OF fieldname WITH DEFAULT KEY,
          lx_error         TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_data>          TYPE any,
                   <lg_field>         TYPE any,
                   <lv_comp>          LIKE LINE OF lt_clr_comps,
                   <lt_data_baseinfo> TYPE ANY TABLE,
                   <lg_data_baseinfo> TYPE any,
                   <lg_ddlname>       TYPE any,
                   <lg_as4local>      TYPE any.


    TRY.
        CREATE DATA lr_data TYPE ('DDDDLSRCV').
        ASSIGN lr_data->* TO <lg_data>.

        CALL METHOD ('CL_DD_DDL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            handler = lo_ddl.

        IF is_baseinfo_supported( ) = abap_true.
          CREATE DATA lr_data_baseinfo TYPE ('IF_DD_DDL_TYPES=>TY_T_BASEINFO_STRING').
          ASSIGN lr_data_baseinfo->* TO <lt_data_baseinfo>.
          ASSIGN lr_data_baseinfo->* TO <lg_data_baseinfo>.

          CALL METHOD lo_ddl->('IF_DD_DDL_HANDLER~READ')
            EXPORTING
              name            = ms_item-obj_name
              get_state       = 'A'
            IMPORTING
              ddddlsrcv_wa    = <lg_data>
              baseinfo_string = <lt_data_baseinfo>.

          LOOP AT <lt_data_baseinfo> ASSIGNING <lg_data_baseinfo>.
            ASSIGN COMPONENT 'DDLNAME' OF STRUCTURE <lg_data_baseinfo> TO <lg_ddlname>.
            ASSERT sy-subrc = 0.

            ASSIGN COMPONENT 'AS4LOCAL' OF STRUCTURE <lg_data_baseinfo> TO <lg_as4local>.
            ASSERT sy-subrc = 0.

            IF <lg_ddlname> = ms_item-obj_name AND <lg_as4local> = 'A'.
              ASSIGN COMPONENT 'BASEINFO_STRING' OF STRUCTURE <lg_data_baseinfo> TO <lg_field>.
              ASSERT sy-subrc = 0.
              Lif_abapgit_object~mo_files->add_string(
                iv_ext    = 'baseinfo'
                iv_string = <lg_field> ).
              EXIT.
            ENDIF.
          ENDLOOP.
        ELSE.
          CALL METHOD lo_ddl->('IF_DD_DDL_HANDLER~READ')
            EXPORTING
              name         = ms_item-obj_name
              get_state    = 'A'
            IMPORTING
              ddddlsrcv_wa = <lg_data>.
        ENDIF.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    APPEND 'AS4USER'               TO lt_clr_comps.
    APPEND 'AS4DATE'               TO lt_clr_comps.
    APPEND 'AS4TIME'               TO lt_clr_comps.
    APPEND 'ACTFLAG'               TO lt_clr_comps.
    APPEND 'CHGFLAG'               TO lt_clr_comps.
    APPEND 'ABAP_LANGUAGE_VERSION' TO lt_clr_comps.
    APPEND 'ABAP_LANGU_VERSION'    TO lt_clr_comps.

    LOOP AT lt_clr_comps ASSIGNING <lv_comp>.
      ASSIGN COMPONENT <lv_comp> OF STRUCTURE <lg_data> TO <lg_field>.
      IF sy-subrc = 0.
        CLEAR <lg_field>.
      ENDIF.
    ENDLOOP.

    ASSIGN COMPONENT 'SOURCE' OF STRUCTURE <lg_data> TO <lg_field>.
    ASSERT sy-subrc = 0.

    format_source_before_serialize( CHANGING cv_string = <lg_field> ).

    Lif_abapgit_object~mo_files->add_string(
      iv_ext    = 'asddls'
      iv_string = <lg_field> ).

    CLEAR <lg_field>.

    io_xml->add( iv_name = 'DDLS'
                 ig_data = <lg_data> ).

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
endclass. "ZCL_ABAPGIT_OBJECT_DDLS implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_DDLX <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ddlx=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ddlx=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_DDLX implementation.
*"* method's implementations
*include methods.
  METHOD clear_field.

    FIELD-SYMBOLS: <lg_field> TYPE data.

    ASSIGN COMPONENT iv_fieldname
           OF STRUCTURE cg_metadata
           TO <lg_field>.
    IF sy-subrc = 0.
      CLEAR: <lg_field>.
    ENDIF.

  ENDMETHOD.
  METHOD clear_fields.

    FIELD-SYMBOLS: <lg_metadata> TYPE any.

    ASSIGN COMPONENT 'METADATA'
           OF STRUCTURE cg_data
           TO <lg_metadata>.
    ASSERT sy-subrc = 0.

    clear_field( EXPORTING iv_fieldname = 'CHANGED_AT'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'CHANGED_BY'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'CREATED_AT'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'CREATED_BY'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'RESPONSIBLE'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'PACKAGE_REF-NAME'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'CONTAINER_REF-PACKAGE_NAME'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'VERSION'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'RESPONSIBLE'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'MASTER_SYSTEM'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'ABAP_LANGUAGE_VERSION'
                 CHANGING  cg_metadata  = <lg_metadata> ).
    clear_field( EXPORTING iv_fieldname = 'ABAP_LANGU_VERSION'
                 CHANGING  cg_metadata  = <lg_metadata> ).

  ENDMETHOD.
  METHOD get_persistence.

    DATA: lx_error TYPE REF TO cx_root.

    TRY.
        IF mi_persistence IS NOT BOUND.

          CREATE OBJECT mi_persistence
                 TYPE ('CL_DDLX_ADT_OBJECT_PERSIST').

        ENDIF.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    ri_persistence = mi_persistence.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA:
      lv_object_key  TYPE seu_objkey,
      li_data_model  TYPE REF TO if_wb_object_data_model,
      li_persistence TYPE REF TO if_wb_object_persist,
      lr_data        TYPE REF TO data.

    FIELD-SYMBOLS:
      <lg_data>       TYPE any,
      <lg_changed_by> TYPE data.

    lv_object_key = ms_item-obj_name.

    TRY.
        CREATE DATA lr_data
          TYPE ('CL_DDLX_WB_OBJECT_DATA=>TY_OBJECT_DATA').
        ASSIGN lr_data->* TO <lg_data>.

        CREATE OBJECT li_data_model
          TYPE ('CL_DDLX_WB_OBJECT_DATA').

        li_persistence = get_persistence( ).

        li_persistence->get(
          EXPORTING
            p_object_key  = lv_object_key
            p_version     = swbm_version_active
          CHANGING
            p_object_data = li_data_model ).
      CATCH cx_root.
        rv_user = c_user_unknown.
        RETURN.
    ENDTRY.

    li_data_model->get_data( IMPORTING p_data = <lg_data> ).

    ASSIGN COMPONENT 'METADATA-CHANGED_BY' OF STRUCTURE <lg_data> TO <lg_changed_by>.
    ASSERT sy-subrc = 0.
    rv_user = <lg_changed_by>.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_object_key TYPE seu_objkey,
          lx_error      TYPE REF TO cx_root.


    lv_object_key = ms_item-obj_name.

    TRY.

        get_persistence( )->delete( p_object_key = lv_object_key
                                    p_version    = swbm_version_active ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: li_data_model TYPE REF TO if_wb_object_data_model,
          lr_data       TYPE REF TO data,
          lx_error      TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_data>    TYPE any,
                   <lg_source>  TYPE data,
                   <lg_version> TYPE data,
                   <lg_package> TYPE data.

    TRY.
        CREATE DATA lr_data
          TYPE ('CL_DDLX_WB_OBJECT_DATA=>TY_OBJECT_DATA').
        ASSIGN lr_data->* TO <lg_data>.

        io_xml->read(
          EXPORTING
            iv_name = 'DDLX'
          CHANGING
            cg_data = <lg_data> ).

        ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <lg_data> TO <lg_source>.
        ASSERT sy-subrc = 0.

        TRY.
            " If the file doesn't exist that's ok, because previously
            " the source code was stored in the xml. We are downward compatible.
            <lg_source> = Lif_abapgit_object~mo_files->read_string( 'asddlxs' ).
          CATCH Lcx_abapgit_exception ##NO_HANDLER.
        ENDTRY.

        CREATE OBJECT li_data_model
          TYPE ('CL_DDLX_WB_OBJECT_DATA').

        ASSIGN COMPONENT 'METADATA-VERSION' OF STRUCTURE <lg_data> TO <lg_version>.
        ASSERT sy-subrc = 0.

        " We have to always save as inactive. Standard activation below activates then
        " and also creates transport request entry if necessary
        <lg_version> = 'inactive'.

        "package needed to be able to determine ABAP language version
        ASSIGN COMPONENT 'METADATA-PACKAGE_REF-NAME' OF STRUCTURE <lg_data> TO <lg_package>.
        IF <lg_package> IS ASSIGNED.
          <lg_package> = iv_package.
        ENDIF.

        li_data_model->set_data( <lg_data> ).

        get_persistence( )->save( li_data_model ).

        tadir_insert( iv_package ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_object_key TYPE seu_objkey.

    lv_object_key = ms_item-obj_name.

    rv_bool = abap_true.

    TRY.
        get_persistence( )->get( p_object_key           = lv_object_key
                                 p_version              = swbm_version_active
                                 p_existence_check_only = abap_true ).

      CATCH cx_swb_exception.
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
    " Covered by ZCL_ABAPGIT_ADT_LINK=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_object_key  TYPE seu_objkey,
          li_data_model  TYPE REF TO if_wb_object_data_model,
          li_persistence TYPE REF TO if_wb_object_persist,
          lr_data        TYPE REF TO data,
          lx_error       TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_data>  TYPE any,
                   <lg_field> TYPE data.

    lv_object_key = ms_item-obj_name.

    TRY.
        CREATE DATA lr_data
          TYPE ('CL_DDLX_WB_OBJECT_DATA=>TY_OBJECT_DATA').
        ASSIGN lr_data->* TO <lg_data>.

        CREATE OBJECT li_data_model
          TYPE ('CL_DDLX_WB_OBJECT_DATA').

        li_persistence = get_persistence( ).

        IF Lcl_abapgit_factory=>get_environment( )->compare_with_inactive( ) = abap_true.
          "Retrieve inactive version
          li_persistence->get(
            EXPORTING
              p_object_key  = lv_object_key
              p_version     = swbm_version_inactive
            CHANGING
              p_object_data = li_data_model ).
          IF li_data_model->get_object_name( ) IS INITIAL.
            "Fallback: retrieve active version
            li_persistence->get(
              EXPORTING
                p_object_key  = lv_object_key
                p_version     = swbm_version_active
              CHANGING
                p_object_data = li_data_model ).
          ENDIF.
        ELSE.
          "Retrieve active version
          li_persistence->get(
            EXPORTING
              p_object_key  = lv_object_key
              p_version     = swbm_version_active
            CHANGING
              p_object_data = li_data_model ).
        ENDIF.

        li_data_model->get_data( IMPORTING p_data = <lg_data> ).

        clear_fields( CHANGING cg_data = <lg_data> ).

        ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <lg_data> TO <lg_field>.
        ASSERT sy-subrc = 0.

        Lif_abapgit_object~mo_files->add_string(
          iv_ext    = 'asddlxs'
          iv_string = <lg_field> ).

        CLEAR <lg_field>.

        io_xml->add( iv_name = 'DDLX'
                     ig_data = <lg_data> ).

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
endclass. "ZCL_ABAPGIT_OBJECT_DDLX implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_EVTB <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_evtb=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_evtb=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_EVTB implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lv_user  TYPE string,
          lx_error TYPE REF TO cx_root.

    TRY.

        SELECT SINGLE changed_by INTO lv_user
            FROM (c_table_name)
            WHERE evtb_name = ms_item-obj_name AND version = 'I'.

        IF lv_user IS INITIAL.
          SELECT SINGLE changed_by INTO lv_user
            FROM (c_table_name)
            WHERE evtb_name = ms_item-obj_name AND version = 'A'.
        ENDIF.

        rv_user = lv_user.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_EVTB implementation

*>>>>>>> ZCL_ABAPGIT_POPUP_CODE_INSP <<<<<<<*

*"* macro definitions
*include zcl_abapgit_popup_code_insp===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_popup_code_insp===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_POPUP_CODE_INSP implementation.
*"* method's implementations
*include methods.
  METHOD create.
    CREATE OBJECT ri_popup TYPE Lcl_abapgit_popup_code_insp.
  ENDMETHOD.
  METHOD fetch_list.

    rt_list = Lcl_abapgit_factory=>get_code_inspector( '$TMP' )->list_global_variants( ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_render_item~render.

    FIELD-SYMBOLS <ls_item> TYPE LINE OF Lif_abapgit_code_inspector=>ty_variants.

    ASSIGN iv_item TO <ls_item>.
    ASSERT sy-subrc = 0.

    ri_html = Lcl_abapgit_html=>create( |<b>{ <ls_item>-name }</b> - { <ls_item>-description }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_html_popup~create_picklist.

    CREATE OBJECT ro_picklist
      EXPORTING
        iv_title         = 'Choose Variant'
        it_list          = fetch_list( )
        ii_item_renderer = me.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_POPUP_CODE_INSP implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_DTDC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_dtdc=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_dtdc=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_DTDC implementation.
*"* method's implementations
*include methods.
  METHOD clear_field.

    FIELD-SYMBOLS: <lv_value> TYPE data.

    ASSIGN COMPONENT iv_fieldname OF STRUCTURE cs_dynamic_cache
           TO <lv_value>.
    IF sy-subrc = 0.
      CLEAR: <lv_value>.
    ENDIF.

  ENDMETHOD.
  METHOD clear_fields.

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CREATED_AT'
      CHANGING
        cs_dynamic_cache = cs_dynamic_cache ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CREATED_BY'
      CHANGING
        cs_dynamic_cache = cs_dynamic_cache ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CHANGED_AT'
      CHANGING
        cs_dynamic_cache = cs_dynamic_cache ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CHANGED_BY'
      CHANGING
        cs_dynamic_cache = cs_dynamic_cache ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-MASTER_LANGUAGE'
      CHANGING
        cs_dynamic_cache = cs_dynamic_cache ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-RESPONSIBLE'
      CHANGING
        cs_dynamic_cache = cs_dynamic_cache ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-PACKAGE_REF'
      CHANGING
        cs_dynamic_cache = cs_dynamic_cache ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-ABAP_LANGUAGE_VERSION'
      CHANGING
        cs_dynamic_cache = cs_dynamic_cache ).
    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-ABAP_LANGU_VERSION'
      CHANGING
        cs_dynamic_cache = cs_dynamic_cache ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'CONTENT-SOURCE'
      CHANGING
        cs_dynamic_cache = cs_dynamic_cache ).

  ENDMETHOD.
  METHOD constructor.

    super->constructor(
        is_item     = is_item
        iv_language = iv_language ).

    mv_dynamic_cache_key = ms_item-obj_name.
    mv_has_own_wb_data_class = has_own_wb_data_class( ).

    TRY.
        IF mv_has_own_wb_data_class = abap_true.
          CREATE DATA mr_dynamic_cache TYPE ('CL_DTDC_WB_OBJECT_DATA=>TY_DTDC_OBJECT_DATA').
        ELSE.
          CREATE DATA mr_dynamic_cache TYPE ('CL_BLUE_SOURCE_OBJECT_DATA=>TY_OBJECT_DATA').
        ENDIF.
        CREATE OBJECT mi_persistence TYPE ('CL_DTDC_OBJECT_PERSIST').

      CATCH cx_sy_create_error.
        Lcx_abapgit_exception=>raise( |DTDC not supported by your NW release| ).
    ENDTRY.

  ENDMETHOD.
  METHOD fill_metadata_from_db.

    DATA:
      li_wb_object_operator TYPE REF TO object,
      lr_dynamic_cache_old  TYPE REF TO data.

    FIELD-SYMBOLS:
      <ls_dynamic_cache_old> TYPE any,
      <lv_created_at>        TYPE xsddatetime_z,
      <lv_created_by>        TYPE syuname,
      <lv_created_at_old>    TYPE xsddatetime_z,
      <lv_created_by_old>    TYPE syuname.

    li_wb_object_operator = get_wb_object_operator( ).

    IF mv_has_own_wb_data_class = abap_true.
      CREATE DATA lr_dynamic_cache_old TYPE ('CL_DTDC_WB_OBJECT_DATA=>TY_DTDC_OBJECT_DATA').
    ELSE.
      CREATE DATA lr_dynamic_cache_old TYPE ('CL_BLUE_SOURCE_OBJECT_DATA=>TY_OBJECT_DATA').
    ENDIF.
    ASSIGN lr_dynamic_cache_old->* TO <ls_dynamic_cache_old>.
    ASSERT sy-subrc = 0.

    CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
      IMPORTING
        data = <ls_dynamic_cache_old>.

    ASSIGN COMPONENT 'METADATA-CREATED_BY' OF STRUCTURE cs_dynamic_cache
           TO <lv_created_by>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'METADATA-CREATED_AT' OF STRUCTURE cs_dynamic_cache
           TO <lv_created_at>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'METADATA-CREATED_BY' OF STRUCTURE <ls_dynamic_cache_old>
           TO <lv_created_by_old>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'METADATA-CREATED_AT' OF STRUCTURE <ls_dynamic_cache_old>
           TO <lv_created_at_old>.
    ASSERT sy-subrc = 0.

    <lv_created_at> = <lv_created_at_old>.
    <lv_created_by> = <lv_created_by_old>.

  ENDMETHOD.
  METHOD get_wb_object_operator.

    DATA:
      ls_object_type TYPE wbobjtype,
      lx_error       TYPE REF TO cx_root.

    IF mi_wb_object_operator IS BOUND.
      ri_wb_object_operator = mi_wb_object_operator.
    ENDIF.

    ls_object_type-objtype_tr = 'DTDC'.
    ls_object_type-subtype_wb = 'DF'.

    TRY.
        CALL METHOD ('CL_WB_OBJECT_OPERATOR')=>('CREATE_INSTANCE')
          EXPORTING
            object_type = ls_object_type
            object_key  = mv_dynamic_cache_key
          RECEIVING
            result      = mi_wb_object_operator.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    ri_wb_object_operator = mi_wb_object_operator.

  ENDMETHOD.
  METHOD has_own_wb_data_class.

    DATA:
      lr_own_type TYPE REF TO data,
      lx_error    TYPE REF TO cx_root.

    TRY.
        CREATE DATA lr_own_type TYPE ('CL_DTDC_WB_OBJECT_DATA=>TY_DTDC_OBJECT_DATA').
        rv_supported = abap_true.
      CATCH cx_root INTO lx_error.
        rv_supported = abap_false.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA:
      li_object_data_model  TYPE REF TO if_wb_object_data_model,
      lx_error              TYPE REF TO cx_root,
      li_wb_object_operator TYPE REF TO object.

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

    TRY.
        li_wb_object_operator = get_wb_object_operator( ).

        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~DELETE')
          EXPORTING
            transport_request = iv_transport.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA:
      li_object_data_model  TYPE REF TO if_wb_object_data_model,
      li_wb_object_operator TYPE REF TO object,
      lx_error              TYPE REF TO cx_root.

    FIELD-SYMBOLS:
      <ls_dynamic_cache> TYPE any,
      <lv_source>        TYPE data.

    ASSIGN mr_dynamic_cache->* TO <ls_dynamic_cache>.
    ASSERT sy-subrc = 0.

    io_xml->read(
      EXPORTING
        iv_name = 'DTDC'
      CHANGING
        cg_data = <ls_dynamic_cache> ).

    li_wb_object_operator = get_wb_object_operator( ).

    TRY.
        IF mv_has_own_wb_data_class = abap_true.
          CREATE OBJECT li_object_data_model TYPE ('CL_DTDC_WB_OBJECT_DATA').
        ELSE.
          CREATE OBJECT li_object_data_model TYPE ('CL_BLUE_SOURCE_OBJECT_DATA').
        ENDIF.

        ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <ls_dynamic_cache>
               TO <lv_source>.
        ASSERT sy-subrc = 0.

        <lv_source> = Lif_abapgit_object~mo_files->read_string( 'asdtdc' ).

        tadir_insert( iv_package ).

        IF Lif_abapgit_object~exists( ) = abap_true.

          " We need to populate created_at, created_by, because otherwise update  is not possible
          fill_metadata_from_db( CHANGING cs_dynamic_cache = <ls_dynamic_cache> ).
          li_object_data_model->set_data( <ls_dynamic_cache> ).

          CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
            EXPORTING
              io_object_data    = li_object_data_model
              transport_request = iv_transport.

        ELSE.

          li_object_data_model->set_data( <ls_dynamic_cache> ).

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

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    TRY.
        mi_persistence->get(
            p_object_key           = mv_dynamic_cache_key
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

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESWB_EO'
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA:
      li_wb_object_operator TYPE REF TO object,
      li_object_data_model  TYPE REF TO if_wb_object_data_model,
      lx_error              TYPE REF TO cx_root,
      lv_source             TYPE string.

    FIELD-SYMBOLS:
      <ls_dynamic_cache> TYPE any,
      <lv_source>        TYPE string.

    ASSIGN mr_dynamic_cache->* TO <ls_dynamic_cache>.
    ASSERT sy-subrc = 0.

    TRY.
        li_wb_object_operator = get_wb_object_operator( ).

        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
          EXPORTING
            version        = 'A'
          IMPORTING
            data           = <ls_dynamic_cache>
            eo_object_data = li_object_data_model.

        ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <ls_dynamic_cache>
               TO <lv_source>.
        ASSERT sy-subrc = 0.

        lv_source = <lv_source>.

        clear_fields( CHANGING cs_dynamic_cache = <ls_dynamic_cache> ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    io_xml->add(
        iv_name = 'DTDC'
        ig_data = <ls_dynamic_cache> ).

    Lif_abapgit_object~mo_files->add_string(
        iv_ext    = 'asdtdc'
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
endclass. "ZCL_ABAPGIT_OBJECT_DTDC implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ENHO_CLASS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_enho_class=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_enho_class=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ENHO_CLASS implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    ms_item = is_item.
    mo_files = io_files.
  ENDMETHOD.
  METHOD deserialize_includes.

    DATA: lt_tab_methods TYPE enhnewmeth_tab,
          lv_editorder   TYPE n LENGTH 3,
          lv_methname    TYPE seocpdname,
          lt_abap        TYPE rswsourcet,
          lx_enh_root    TYPE REF TO cx_enh_root,
          lv_new_em      TYPE abap_bool,
          lt_files       TYPE Lif_abapgit_git_definitions=>ty_files_tt.

    FIELD-SYMBOLS: <ls_method> LIKE LINE OF lt_tab_methods,
                   <ls_file>   TYPE Lif_abapgit_git_definitions=>ty_file.

    ii_xml->read( EXPORTING iv_name = 'TAB_METHODS'
                  CHANGING cg_data = lt_tab_methods ).

    lv_new_em = abap_false.
    lt_files = mo_files->get_files( ).
    LOOP AT lt_files ASSIGNING <ls_file>
        WHERE filename CS 'enho.em_'.
      lv_new_em = abap_true.
      EXIT.
    ENDLOOP.

    SORT lt_tab_methods BY meth_header-editorder.
    LOOP AT lt_tab_methods ASSIGNING <ls_method>.

      lv_methname = <ls_method>-methkey-cmpname.
      IF lv_new_em = abap_true.
        lt_abap = mo_files->read_abap( iv_extra = 'em_' && lv_methname ).
      ELSE.
        " old way
        lv_editorder = <ls_method>-meth_header-editorder.
        lt_abap = mo_files->read_abap( iv_extra = 'em' && lv_editorder ).
      ENDIF.

      TRY.
          io_class->add_change_new_method_source(
              clsname    = <ls_method>-methkey-clsname
              methname   = lv_methname
              methsource = lt_abap ).
        CATCH cx_enh_root INTO lx_enh_root.
          Lcx_abapgit_exception=>raise_with_text( lx_enh_root ).
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.
  METHOD serialize_includes.

    DATA: lt_includes TYPE enhnewmeth_tabincl_plus_enha,
          lt_source   TYPE TABLE OF abaptxt255,
          lv_include  TYPE syrepid.

    FIELD-SYMBOLS: <ls_include> LIKE LINE OF lt_includes.


    lt_includes = io_class->get_enh_method_includes( ).
    LOOP AT lt_includes ASSIGNING <ls_include>.
      lv_include = io_class->if_enh_tool~get_name( ).
      TRANSLATE lv_include USING ' ='.
      lv_include+30 = 'EM'.
      lv_include+32(8) = <ls_include>-includenr.

      CALL FUNCTION 'RPY_PROGRAM_READ'
        EXPORTING
          program_name     = lv_include
          with_includelist = abap_false
          with_lowercase   = abap_true
        TABLES
          source_extended  = lt_source
        EXCEPTIONS
          cancelled        = 1
          not_found        = 2
          permission_error = 3
          OTHERS           = 4.
      IF sy-subrc = 0.
        mo_files->add_abap( iv_extra = |EM_{ <ls_include>-cpdname }|
                            it_abap  = lt_source ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_object_enho~deserialize.

    DATA: lo_enh_class TYPE REF TO cl_enh_tool_class,
          lt_owr       TYPE enhmeth_tabkeys,
          lt_pre       TYPE enhmeth_tabkeys,
          lt_post      TYPE enhmeth_tabkeys,
          lt_source    TYPE rswsourcet,
          li_tool      TYPE REF TO if_enh_tool,
          lv_shorttext TYPE string,
          lv_class     TYPE seoclsname,
          lv_enhname   TYPE enhname,
          lv_package   TYPE devclass,
          lx_enh_root  TYPE REF TO cx_enh_root.

    ii_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING cg_data  = lv_shorttext ).
    ii_xml->read( EXPORTING iv_name = 'OWR_METHODS'
                  CHANGING cg_data  = lt_owr ).
    ii_xml->read( EXPORTING iv_name = 'PRE_METHODS'
                  CHANGING cg_data  = lt_pre ).
    ii_xml->read( EXPORTING iv_name = 'POST_METHODS'
                  CHANGING cg_data  = lt_post ).
    ii_xml->read( EXPORTING iv_name = 'CLASS'
                  CHANGING cg_data  = lv_class ).
    lt_source = mo_files->read_abap( ).

    lv_enhname = ms_item-obj_name.
    lv_package = iv_package.
    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname     = lv_enhname
            enhtype     = ''
            enhtooltype = cl_enh_tool_class=>tooltype
          IMPORTING
            enhancement = li_tool
          CHANGING
            devclass    = lv_package ).
        lo_enh_class ?= li_tool.

        lo_enh_class->if_enh_object_docu~set_shorttext( lv_shorttext ).
        lo_enh_class->set_class( lv_class ).
        lo_enh_class->set_owr_methods( version     = 'I'
                                       owr_methods = lt_owr ).
        lo_enh_class->set_pre_methods( version     = 'I'
                                       pre_methods = lt_pre ).
        lo_enh_class->set_post_methods( version      = 'I'
                                        post_methods = lt_post ).
        lo_enh_class->set_eimp_include( version     = 'I'
                                        eimp_source = lt_source ).

        Lcl_abapgit_object_enho_clif=>deserialize(
          io_xml  = ii_xml
          io_clif = lo_enh_class ).

        deserialize_includes(
          ii_xml   = ii_xml
          io_class = lo_enh_class ).

        lo_enh_class->if_enh_object~save( run_dark = abap_true ).
        lo_enh_class->if_enh_object~unlock( ).
      CATCH cx_enh_root INTO lx_enh_root.
        TRY.
            lo_enh_class->if_enh_object~unlock( ).
          CATCH cx_sy_ref_is_initial cx_enh_mod_not_allowed ##NO_HANDLER.
        ENDTRY.
        Lcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object_enho~serialize.

    DATA: lo_enh_class TYPE REF TO cl_enh_tool_class,
          lt_owr       TYPE enhmeth_tabkeys,
          lt_pre       TYPE enhmeth_tabkeys,
          lt_post      TYPE enhmeth_tabkeys,
          lt_source    TYPE rswsourcet,
          lv_class     TYPE seoclsname,
          lv_shorttext TYPE string.


    lo_enh_class ?= ii_enh_tool.

    lv_shorttext = lo_enh_class->if_enh_object_docu~get_shorttext( ).
    lt_owr = lo_enh_class->get_owr_methods( ).
    lt_pre = lo_enh_class->get_pre_methods( ).
    lt_post = lo_enh_class->get_post_methods( ).
    lt_source = lo_enh_class->get_eimp_include( ).
    lo_enh_class->get_class( IMPORTING class_name = lv_class ).

    ii_xml->add( iv_name = 'TOOL'
                 ig_data = ii_enh_tool->get_tool( ) ).
    ii_xml->add( ig_data = lv_shorttext
                 iv_name = 'SHORTTEXT' ).
    ii_xml->add( iv_name = 'CLASS'
                 ig_data = lv_class ).
    ii_xml->add( iv_name = 'OWR_METHODS'
                 ig_data = lt_owr ).
    ii_xml->add( iv_name = 'PRE_METHODS'
                 ig_data = lt_pre ).
    ii_xml->add( iv_name = 'POST_METHODS'
                 ig_data = lt_post ).

    mo_files->add_abap( lt_source ).

    Lcl_abapgit_object_enho_clif=>serialize(
      io_xml  = ii_xml
      io_clif = lo_enh_class ).

    serialize_includes( lo_enh_class ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENHO_CLASS implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_NONT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_nont=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_nont=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_NONT implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.
    DATA: lv_user  TYPE string,
          lx_error TYPE REF TO cx_root.

    TRY.

        SELECT SINGLE changed_by INTO lv_user
            FROM (c_table_name)
            WHERE nont_name = ms_item-obj_name AND version = 'I'.

        IF lv_user IS INITIAL.
          SELECT SINGLE changed_by INTO lv_user
            FROM (c_table_name)
            WHERE nont_name = ms_item-obj_name AND version = 'A'.
        ENDIF.

        rv_user = lv_user.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_NONT implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ENQU <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_enqu=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_enqu=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ENQU implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE as4user FROM dd25l
      INTO rv_user
      WHERE viewname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers  = '0000'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    delete_ddic( 'L' ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_name  TYPE ddobjname,
          ls_dd25v TYPE dd25v,
          lt_dd26e TYPE TABLE OF dd26e,
          lt_dd27p TYPE ty_dd27p.


    io_xml->read( EXPORTING iv_name = 'DD25V'
                  CHANGING cg_data = ls_dd25v ).
    io_xml->read( EXPORTING iv_name = 'DD26E_TABLE'
                  CHANGING cg_data = lt_dd26e ).
    io_xml->read( EXPORTING iv_name = 'DD27P_TABLE'
                  CHANGING cg_data = lt_dd27p ).

    corr_insert( iv_package = iv_package
                 ig_object_class = 'DICT' ).

    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_ENQU_PUT'
      EXPORTING
        name              = lv_name
        dd25v_wa          = ls_dd25v
      TABLES
        dd26e_tab         = lt_dd26e
        dd27p_tab         = lt_dd27p
      EXCEPTIONS
        enqu_not_found    = 1
        name_inconsistent = 2
        enqu_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_viewname TYPE dd25l-viewname.

    SELECT SINGLE viewname FROM dd25l INTO lv_viewname
      WHERE viewname = ms_item-obj_name.
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

    rv_is_locked = abap_false.

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECT=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_name  TYPE ddobjname,
          lv_state TYPE ddgotstate,
          ls_dd25v TYPE dd25v,
          lt_dd26e TYPE TABLE OF dd26e,
          lt_dd27p TYPE ty_dd27p.

    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_ENQU_GET'
      EXPORTING
        name          = lv_name
        state         = 'A'
        langu         = mv_language
      IMPORTING
        gotstate      = lv_state
        dd25v_wa      = ls_dd25v
      TABLES
        dd26e_tab     = lt_dd26e
        dd27p_tab     = lt_dd27p
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF ls_dd25v IS INITIAL OR lv_state <> 'A'.
      RETURN.
    ENDIF.

    CLEAR: ls_dd25v-as4user,
           ls_dd25v-as4date,
           ls_dd25v-as4time,
           ls_dd25v-as4local,
           ls_dd25v-as4vers.

    _clear_dd27p_fields( CHANGING ct_dd27p = lt_dd27p ).

    io_xml->add( iv_name = 'DD25V'
                 ig_data = ls_dd25v ).
    io_xml->add( ig_data = lt_dd26e
                 iv_name = 'DD26E_TABLE' ).
    io_xml->add( ig_data = lt_dd27p
                 iv_name = 'DD27P_TABLE' ).

  ENDMETHOD.
  METHOD _clear_dd27p_fields.

    FIELD-SYMBOLS <ls_dd27p> TYPE dd27p.

    LOOP AT ct_dd27p ASSIGNING <ls_dd27p>.
      "taken from table
      CLEAR <ls_dd27p>-headlen.
      CLEAR <ls_dd27p>-scrlen1.
      CLEAR <ls_dd27p>-scrlen2.
      CLEAR <ls_dd27p>-scrlen3.
      CLEAR <ls_dd27p>-intlen.
      CLEAR <ls_dd27p>-outputlen.
      CLEAR <ls_dd27p>-flength.
      CLEAR <ls_dd27p>-ddtext.
      CLEAR <ls_dd27p>-reptext.
      CLEAR <ls_dd27p>-scrtext_s.
      CLEAR <ls_dd27p>-scrtext_m.
      CLEAR <ls_dd27p>-scrtext_l.
      CLEAR <ls_dd27p>-rollname.
      CLEAR <ls_dd27p>-rollnamevi.
      CLEAR <ls_dd27p>-entitytab.
      CLEAR <ls_dd27p>-datatype.
      CLEAR <ls_dd27p>-inttype.
      CLEAR <ls_dd27p>-ddlanguage.
      CLEAR <ls_dd27p>-domname.
      CLEAR <ls_dd27p>-signflag.
    ENDLOOP.

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
endclass. "ZCL_ABAPGIT_OBJECT_ENQU implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SAMC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_samc=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_samc=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SAMC implementation.
*"* method's implementations
*include methods.
  METHOD get_data_class_name.

    rv_data_class_name = 'CL_AMC_APPLICATION_OBJ_DATA'.

  ENDMETHOD.
  METHOD get_data_structure_name.

    rv_data_structure_name = 'AMC_APPLICATION_COMPLETE'.

  ENDMETHOD.
  METHOD get_persistence_class_name.

    rv_persistence_class_name = 'CL_AMC_APPLICATION_OBJ_PERS'.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SAMC implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SAPC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sapc=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sapc=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SAPC implementation.
*"* method's implementations
*include methods.
  METHOD get_data_class_name.

    rv_data_class_name = 'CL_APC_APPLICATION_OBJ_DATA'.

  ENDMETHOD.
  METHOD get_data_structure_name.

    rv_data_structure_name = 'APC_APPLICATION_COMPLETE'.

  ENDMETHOD.
  METHOD get_persistence_class_name.

    rv_persistence_class_name = 'CL_APC_APPLICATION_OBJ_PERS'.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SAPC implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_G4BA <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_g4ba=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_g4ba=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_G4BA implementation.
*"* method's implementations
*include methods.
  METHOD get_field_rules.

    ro_result = Lcl_abapgit_field_rules=>create( ).
    ro_result->add(
      iv_table     = '/IWBEP/I_V4_MSGR'
      iv_field     = 'CREATED_BY'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWBEP/I_V4_MSGR'
      iv_field     = 'CHANGED_BY'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWBEP/I_V4_MSGR'
      iv_field     = 'CREATED_TS'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-timestamp
    )->add(
      iv_table     = '/IWBEP/I_V4_MSGR'
      iv_field     = 'CHANGED_TS'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-timestamp
    )->add(
      iv_table     = '/IWBEP/I_V4_MSGA'
      iv_field     = 'CREATED_BY'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWBEP/I_V4_MSGA'
      iv_field     = 'CHANGED_BY'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWBEP/I_V4_MSGA'
      iv_field     = 'CREATED_TS'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-timestamp
    )->add(
      iv_table     = '/IWBEP/I_V4_MSGA'
      iv_field     = 'CHANGED_TS'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-timestamp ).

  ENDMETHOD.
  METHOD get_generic.

    CREATE OBJECT ro_generic
      EXPORTING
        io_field_rules = get_field_rules( )
        is_item        = ms_item
        iv_language    = mv_language.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    DATA lv_created TYPE sy-uname.
    DATA lv_changed TYPE sy-uname.

    SELECT SINGLE created_by changed_by INTO (lv_created, lv_changed) FROM ('/IWBEP/I_V4_MSGR')
      WHERE group_id = ms_item-obj_name.

    rv_user = lv_changed.
    IF lv_changed IS INITIAL.
      rv_user = lv_created.
    ENDIF.
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
endclass. "ZCL_ABAPGIT_OBJECT_G4BA implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_G4BS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_g4bs=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_g4bs=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_G4BS implementation.
*"* method's implementations
*include methods.
  METHOD get_field_rules.

    ro_result = Lcl_abapgit_field_rules=>create( ).
    ro_result->add(
      iv_table     = '/IWBEP/I_V4_MSRV'
      iv_field     = 'CREATED_BY'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWBEP/I_V4_MSRV'
      iv_field     = 'CHANGED_BY'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWBEP/I_V4_MSRV'
      iv_field     = 'CREATED_TS'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-timestamp
    )->add(
      iv_table     = '/IWBEP/I_V4_MSRV'
      iv_field     = 'CHANGED_TS'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-timestamp
    )->add(
      iv_table     = '/IWBEP/I_V4_MSRT'
      iv_field     = 'CREATED_BY'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWBEP/I_V4_MSRT'
      iv_field     = 'CHANGED_BY'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWBEP/I_V4_MSRT'
      iv_field     = 'CREATED_TS'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-timestamp
    )->add(
      iv_table     = '/IWBEP/I_V4_MSRT'
      iv_field     = 'CHANGED_TS'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-timestamp ).

  ENDMETHOD.
  METHOD get_generic.

    CREATE OBJECT ro_generic
      EXPORTING
        io_field_rules = get_field_rules( )
        is_item        = ms_item
        iv_language    = mv_language.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA lv_created TYPE sy-uname.
    DATA lv_changed TYPE sy-uname.

    " Get entry with highest version
    SELECT created_by changed_by INTO (lv_created, lv_changed) FROM ('/IWBEP/I_V4_MSRV')
      WHERE service_id = ms_item-obj_name ORDER BY PRIMARY KEY.
      rv_user = lv_changed.
      IF lv_changed IS INITIAL.
        rv_user = lv_created.
      ENDIF.
    ENDSELECT.

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
endclass. "ZCL_ABAPGIT_OBJECT_G4BS implementation

*>>>>>>> ZCL_ABAPGIT_UI_FACTORY <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ui_factory========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ui_factory========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_UI_FACTORY implementation.
*"* method's implementations
*include methods.
  METHOD get_asset_manager.

    DATA lo_buf TYPE REF TO Lcl_abapgit_string_buffer.
    DATA li_asset_man TYPE REF TO Lif_abapgit_gui_asset_manager.

    CREATE OBJECT lo_buf.

    li_asset_man = Lcl_abapgit_gui_asset_manager=>create( ).

lo_buf->add( '/*' ).
lo_buf->add( ' * ABAPGIT COMMON CSS' ).
lo_buf->add( ' */' ).
lo_buf->add( '' ).
lo_buf->add( '/* GLOBALS */' ).
lo_buf->add( '' ).
lo_buf->add( 'body {' ).
lo_buf->add( '  overflow-x: hidden;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'body.centered {' ).
lo_buf->add( '  max-width: 1280px;' ).
lo_buf->add( '  margin: 0 auto;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'body.full_width {' ).
lo_buf->add( '  width:100%;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'a, a:visited {' ).
lo_buf->add( '  text-decoration:  none;' ).
lo_buf->add( '}' ).
lo_buf->add( 'a:hover, a:active {' ).
lo_buf->add( '  cursor: pointer;' ).
lo_buf->add( '  text-decoration: underline;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'img {' ).
lo_buf->add( '  border-width: 0px;' ).
lo_buf->add( '  vertical-align: middle;' ).
lo_buf->add( '}' ).
lo_buf->add( 'table { border-collapse: collapse; }' ).
lo_buf->add( 'pre { display: inline; }' ).
lo_buf->add( 'sup {' ).
lo_buf->add( '  vertical-align: top;' ).
lo_buf->add( '  position: relative;' ).
lo_buf->add( '  top: -0.5em;' ).
lo_buf->add( '  font-size: 75%;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'input, textarea, select {' ).
lo_buf->add( '  padding: 3px 0.5em;' ).
lo_buf->add( '  border: 1px solid;' ).
lo_buf->add( '}' ).
lo_buf->add( 'input:focus, textarea:focus {' ).
lo_buf->add( '  border: 1px solid;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.cursor-pointer {' ).
lo_buf->add( '  cursor: pointer;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'span.separator {' ).
lo_buf->add( '  padding-left: 0.5em;' ).
lo_buf->add( '  padding-right: 0.5em;' ).
lo_buf->add( '  opacity: 0.25;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* MODIFIERS */' ).
lo_buf->add( '' ).
lo_buf->add( '.emphasis     { font-weight: bold !important; }' ).
lo_buf->add( '.crossout     { text-decoration: line-through !important; }' ).
lo_buf->add( '.right        { text-align:right; }' ).
lo_buf->add( '.center       { text-align:center; }' ).
lo_buf->add( '.paddings     { padding: 0.5em 0.5em; }' ).
lo_buf->add( '.pad-sides    { padding-left: 0.3em; padding-right: 0.3em; }' ).
lo_buf->add( '.pad-1em      { padding: 1em 1em; }' ).
lo_buf->add( '.margin-v5    { margin-top: 0.5em; margin-bottom: 0.5em; }' ).
lo_buf->add( '.margin-v1    { margin-top: 1em; margin-bottom: 1em; }' ).
lo_buf->add( '.indent5em    { padding-left: 0.5em; }' ).
lo_buf->add( '.pad4px       { padding: 4px; }' ).
lo_buf->add( '.w100         { width: 100%; }' ).
lo_buf->add( '.wmin         { width: 1%; }' ).
lo_buf->add( '.w40          { width: 40%; }' ).
lo_buf->add( '.float-right  { float: right; }' ).
lo_buf->add( '.pad-right    { padding-right: 6px; }' ).
lo_buf->add( '.inline       { display: inline; }' ).
lo_buf->add( '.hidden       { visibility: hidden; }' ).
lo_buf->add( '.nodisplay    { display: none }' ).
lo_buf->add( '.m-em5-sides  { margin-left: 0.5em; margin-right: 0.5em }' ).
lo_buf->add( '.w600px       { width: 600px }' ).
lo_buf->add( '.w800px       { width: 800px }' ).
lo_buf->add( '.w1000px      { width: 1000px }' ).
lo_buf->add( '.wmax600px    { max-width: 600px }' ).
lo_buf->add( '.auto-center  { /* use with max-width */' ).
lo_buf->add( '  width: 100%;' ).
lo_buf->add( '  margin-left: auto;' ).
lo_buf->add( '  margin-right: auto;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'span.boxed {' ).
lo_buf->add( '  border-radius: 3px;' ).
lo_buf->add( '  padding: 4px 7px;' ).
lo_buf->add( '  margin-left: 0.2em;' ).
lo_buf->add( '  margin-right: 0.2em;' ).
lo_buf->add( '  font-size: smaller;' ).
lo_buf->add( '}' ).
lo_buf->add( 'span.boxed i.icon {' ).
lo_buf->add( '  padding-right: 5px;' ).
lo_buf->add( '}' ).
lo_buf->add( '.red-filled-set {' ).
lo_buf->add( '  border-width: 0px;' ).
lo_buf->add( '  color: hsl(0, 78%, 93%);' ).
lo_buf->add( '  background-color: hsl(0, 78%, 65%);' ).
lo_buf->add( '}' ).
lo_buf->add( '.green-filled-set {' ).
lo_buf->add( '  border-width: 0px;' ).
lo_buf->add( '  color: hsl(120, 45%, 90%);' ).
lo_buf->add( '  background-color: hsl(120, 27%, 60%);' ).
lo_buf->add( '}' ).
lo_buf->add( '.yellow-filled-set {' ).
lo_buf->add( '  border-width: 0px;' ).
lo_buf->add( '  color: hsl(45, 99%, 90%);' ).
lo_buf->add( '  background-color: hsl(45, 100%, 46%);' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* PANELS */' ).
lo_buf->add( '' ).
lo_buf->add( 'div.panel {' ).
lo_buf->add( '  border-radius: 3px;' ).
lo_buf->add( '  padding: 0.5em 0.5em;' ).
lo_buf->add( '  margin: 0.5em 0.5em;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'div.dummydiv {' ).
lo_buf->add( '  padding:          0.5em 1em;' ).
lo_buf->add( '  text-align:       center;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'a.close-btn {' ).
lo_buf->add( '  text-decoration: none;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* STRUCTURE DIVS, HEADER & FOOTER */' ).
lo_buf->add( '' ).
lo_buf->add( 'div#header {' ).
lo_buf->add( '  padding:          0.5em 0.5em;' ).
lo_buf->add( '  border-bottom:    3px double;' ).
lo_buf->add( '}' ).
lo_buf->add( 'div#header > div { display: inline-block }' ).
lo_buf->add( '' ).
lo_buf->add( '.logo .icon { display: inline-block }' ).
lo_buf->add( '.logo .icon:before { width: auto }' ).
lo_buf->add( '' ).
lo_buf->add( '/* official logo colors, not vars, redefine in themes directly*/' ).
lo_buf->add( '.logo .icon.icon-git-alt { color: #f03c2e }' ).
lo_buf->add( '.logo .icon.icon-abapgit {' ).
lo_buf->add( '  color: #362701;' ).
lo_buf->add( '  vertical-align: bottom;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'div#header .logo { font-size: x-large }' ).
lo_buf->add( 'div#header .page-title { font-size: x-large }' ).
lo_buf->add( 'div#header span.spacer {' ).
lo_buf->add( '  display: inline-block;' ).
lo_buf->add( '  padding-right: 0.25em;' ).
lo_buf->add( '  padding-left: 0.25em;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'div#footer .sponsor a { font-size: smaller; }' ).
lo_buf->add( 'div#footer .logo { font-size: large }' ).
lo_buf->add( 'div#footer {' ).
lo_buf->add( '  padding:          0.5em 0.5em;' ).
lo_buf->add( '  border-top:       3px double;' ).
lo_buf->add( '}' ).
lo_buf->add( 'div#footer .version {' ).
lo_buf->add( '  margin-top: 0.5em;' ).
lo_buf->add( '  font-size: small;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '#debug-output {' ).
lo_buf->add( '  text-align: right;' ).
lo_buf->add( '  padding-right: 0.5em;' ).
lo_buf->add( '  font-size: smaller;' ).
lo_buf->add( '}' ).
lo_buf->add( '#debug-output p {' ).
lo_buf->add( '  margin-top: 0em;' ).
lo_buf->add( '  margin-bottom: 0em;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* ERROR LOG */' ).
lo_buf->add( '' ).
lo_buf->add( 'div.log {' ).
lo_buf->add( '  padding: 6px;' ).
lo_buf->add( '  margin: 4px;' ).
lo_buf->add( '  border: 1px  solid;' ).
lo_buf->add( '  border-radius: 4px;' ).
lo_buf->add( '}' ).
lo_buf->add( 'div.log > span   { display:block; }' ).
lo_buf->add( 'div.log .icon { padding-right: 6px; }' ).
lo_buf->add( '' ).
lo_buf->add( '/* REPOSITORY */' ).
lo_buf->add( '' ).
lo_buf->add( 'div.repo {' ).
lo_buf->add( '  padding: 0.5em 1em 0.5em 1em;' ).
lo_buf->add( '  position: relative;' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo_name span.name {' ).
lo_buf->add( '  font-weight: bold;' ).
lo_buf->add( '  font-size: 14pt;' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo_name a.url {' ).
lo_buf->add( '  font-size: 12pt;' ).
lo_buf->add( '  margin-left: 0.5em;' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo_name span.url {' ).
lo_buf->add( '  font-size: 12pt;' ).
lo_buf->add( '  margin-left: 0.5em;' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo_name .icon {' ).
lo_buf->add( '  padding-right: 4px;' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo_attr {' ).
lo_buf->add( '  font-size: 12pt;' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo_attr span {' ).
lo_buf->add( '  margin-left: 0.2em;' ).
lo_buf->add( '  margin-right: 0.5em;' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo_attr span.bg_marker {' ).
lo_buf->add( '  border: 1px solid;' ).
lo_buf->add( '  border-radius: 3px;' ).
lo_buf->add( '  font-size: 8pt;' ).
lo_buf->add( '  padding: 4px 2px 3px 2px;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* ABAPGIT OBJECTS */' ).
lo_buf->add( '' ).
lo_buf->add( 'span.branch,' ).
lo_buf->add( 'span.user-box,' ).
lo_buf->add( 'span.package-box,' ).
lo_buf->add( 'span.path-box,' ).
lo_buf->add( 'span.transport-box {' ).
lo_buf->add( '  padding: 2px 4px;' ).
lo_buf->add( '  border: 1px solid;' ).
lo_buf->add( '  border-radius: 4px;' ).
lo_buf->add( '  display: inline-block;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* MISC AND REFACTOR */' ).
lo_buf->add( '' ).
lo_buf->add( '.hidden-submit {' ).
lo_buf->add( '  border: 0 none;' ).
lo_buf->add( '  height: 0;' ).
lo_buf->add( '  width: 0;' ).
lo_buf->add( '  padding: 0;' ).
lo_buf->add( '  margin: 0;' ).
lo_buf->add( '  position: absolute;' ).
lo_buf->add( '  overflow: hidden;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* STATE BLOCK COMMON*/' ).
lo_buf->add( '' ).
lo_buf->add( 'span.state-block {' ).
lo_buf->add( '  margin-left: 1em;' ).
lo_buf->add( '  font-family: Consolas, "Lucida Console", Courier, monospace;' ).
lo_buf->add( '  font-size: x-small;' ).
lo_buf->add( '  vertical-align: 13%;' ).
lo_buf->add( '  display: inline-block;' ).
lo_buf->add( '  text-align: center;' ).
lo_buf->add( '  white-space: nowrap;' ).
lo_buf->add( '}' ).
lo_buf->add( 'span.state-block span {' ).
lo_buf->add( '  display: inline-block;' ).
lo_buf->add( '  padding: 0px 3px;' ).
lo_buf->add( '  border-width: 1px;' ).
lo_buf->add( '  border-style: solid;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* REPOSITORY TABLE*/' ).
lo_buf->add( '' ).
lo_buf->add( 'div.repo_container {' ).
lo_buf->add( '  position: relative;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'div.repo_banner {' ).
lo_buf->add( '  margin: 0em 1em 1em;' ).
lo_buf->add( '  padding: 0.5em 0.5em;' ).
lo_buf->add( '  text-align: center;' ).
lo_buf->add( '  font-size: 85%;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'table.repo_tab {' ).
lo_buf->add( '  border: 1px solid;' ).
lo_buf->add( '  border-radius: 3px;' ).
lo_buf->add( '  width: 100%;' ).
lo_buf->add( '  line-height: 1.2;' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo_tab th {' ).
lo_buf->add( '  text-align: left;' ).
lo_buf->add( '  padding: 0.5em;' ).
lo_buf->add( '  border-bottom: 1px solid;' ).
lo_buf->add( '  font-weight: normal;' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo_tab td {' ).
lo_buf->add( '  vertical-align: middle;' ).
lo_buf->add( '  padding-top: 2px;' ).
lo_buf->add( '  padding-bottom: 2px;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.repo_tab td.icon {' ).
lo_buf->add( '  width: 1px;' ).
lo_buf->add( '  text-align: center;' ).
lo_buf->add( '  padding-left: 8px;' ).
lo_buf->add( '  padding-right: 4px;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.repo_tab td.transport {' ).
lo_buf->add( '  width: 140px;' ).
lo_buf->add( '  text-align: left;' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo_tab td.type {' ).
lo_buf->add( '  width: 4em;' ).
lo_buf->add( '  padding-left: 0.5em;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.repo_tab td.filename{' ).
lo_buf->add( '  padding-left: 1em;' ).
lo_buf->add( '  word-break: break-all;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.repo_tab td.object {' ).
lo_buf->add( '  padding-left: 0.5em;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.repo_tab td.files {' ).
lo_buf->add( '  padding-left: 0.5em;' ).
lo_buf->add( '  line-height: 1.5;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.repo_tab tr.object_row{' ).
lo_buf->add( '  border-top: 1px solid;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.repo_tab td.cmd, .repo_tab th.cmd {' ).
lo_buf->add( '  text-align: right;' ).
lo_buf->add( '  padding-left: 0.5em;' ).
lo_buf->add( '  padding-right: 0.7em;' ).
lo_buf->add( '  min-width: 70px;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.repo_tab th.cmd .icon{' ).
lo_buf->add( '  padding-right: 8px;' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo_tab tr:first-child td { border-top: 0px; }' ).
lo_buf->add( '' ).
lo_buf->add( '.repo_tab tr:hover td {' ).
lo_buf->add( '  background-image: linear-gradient(rgba(0, 0, 0, 0.075), rgba(0, 0, 0, 0.075));' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* STAGE */' ).
lo_buf->add( '' ).
lo_buf->add( 'th.stage-status { width: 30px; }' ).
lo_buf->add( 'th.stage-objtype { width: 30px; }' ).
lo_buf->add( 'input.stage-filter { width: 18em; }' ).
lo_buf->add( '' ).
lo_buf->add( '.stage_tab {' ).
lo_buf->add( '  border: 1px solid;' ).
lo_buf->add( '  margin-top: 0.2em;' ).
lo_buf->add( '  line-height: 1.5;' ).
lo_buf->add( '}' ).
lo_buf->add( '.stage_tab td {' ).
lo_buf->add( '  border-top: 1px solid;' ).
lo_buf->add( '  vertical-align: middle;' ).
lo_buf->add( '  padding: 2px 0.5em;' ).
lo_buf->add( '}' ).
lo_buf->add( '.stage_tab th {' ).
lo_buf->add( '  text-align: left;' ).
lo_buf->add( '  font-weight: normal;' ).
lo_buf->add( '  padding: 4px 0.5em;' ).
lo_buf->add( '  border-bottom: 1px solid;' ).
lo_buf->add( '}' ).
lo_buf->add( '.stage_tab td.status {' ).
lo_buf->add( '  width: 2em;' ).
lo_buf->add( '  text-align: center;' ).
lo_buf->add( '}' ).
lo_buf->add( '.stage_tab td.highlight {' ).
lo_buf->add( '  font-weight: bold;' ).
lo_buf->add( '}' ).
lo_buf->add( '.stage_tab td.name {' ).
lo_buf->add( '  word-break: break-all;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.stage_tab tr:first-child td { border-top: 0px; }' ).
lo_buf->add( '' ).
lo_buf->add( '.stage_tab tr:hover td {' ).
lo_buf->add( '  background-image: linear-gradient(rgba(0, 0, 0, 0.075), rgba(0, 0, 0, 0.075));' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.stage_tab td.cmd {  cursor: pointer; }' ).
lo_buf->add( '.stage_tab td.cmd a { padding: 0px 4px; }' ).
lo_buf->add( '.stage_tab th.cmd a { padding: 0px 4px; }' ).
lo_buf->add( '.stage_tab tbody tr:first-child td { padding-top: 0.5em; }' ).
lo_buf->add( '.stage_tab tbody tr:last-child td { padding-bottom: 0.5em; }' ).
lo_buf->add( '' ).
lo_buf->add( '/* COMMIT */' ).
lo_buf->add( '' ).
lo_buf->add( 'div.form-container {' ).
lo_buf->add( '  padding: 1em 1em;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'form.aligned-form {' ).
lo_buf->add( '  display: table;' ).
lo_buf->add( '  border-spacing: 2px;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'form.aligned-form label {' ).
lo_buf->add( '  padding-right: 1em;' ).
lo_buf->add( '  vertical-align: middle;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'form.aligned-form select {' ).
lo_buf->add( '  padding-right: 1em;' ).
lo_buf->add( '  vertical-align: middle;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'form.aligned-form span.sub-title {' ).
lo_buf->add( '  font-size: smaller;' ).
lo_buf->add( '  padding-top: 8px;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'form.aligned-form div.row { display: table-row; }' ).
lo_buf->add( 'form.aligned-form label { display: table-cell; }' ).
lo_buf->add( 'form.aligned-form input { display: table-cell; }' ).
lo_buf->add( 'form.aligned-form input[type="text"] { width: 25em; }' ).
lo_buf->add( 'form.aligned-form span.cell { display: table-cell; }' ).
lo_buf->add( '' ).
lo_buf->add( '/* SETTINGS STYLES */' ).
lo_buf->add( '' ).
lo_buf->add( 'div.settings_container {' ).
lo_buf->add( '  padding: 0.5em 0.5em 1em;' ).
lo_buf->add( '  font-size: 10pt;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'div.settings_section {' ).
lo_buf->add( '  margin-left:50px' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'table.settings td:first-child {' ).
lo_buf->add( '  padding-left: 1em;' ).
lo_buf->add( '  padding-right: 1em;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* DIFF */' ).
lo_buf->add( '' ).
lo_buf->add( 'div.diff {' ).
lo_buf->add( '  padding: 0.7em' ).
lo_buf->add( '}' ).
lo_buf->add( 'div.diff_head {' ).
lo_buf->add( '  padding-bottom: 0.7em;' ).
lo_buf->add( '}' ).
lo_buf->add( 'span.diff_name {' ).
lo_buf->add( '  padding-left: 0.5em;' ).
lo_buf->add( '}' ).
lo_buf->add( 'span.diff_changed_by {' ).
lo_buf->add( '  float: right;' ).
lo_buf->add( '}' ).
lo_buf->add( 'span.diff_banner {' ).
lo_buf->add( '  border-style: solid;' ).
lo_buf->add( '  border-width: 1px;' ).
lo_buf->add( '  border-radius: 3px;' ).
lo_buf->add( '  padding-left: 0.3em;' ).
lo_buf->add( '  padding-right: 0.3em;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'div.diff_content {' ).
lo_buf->add( '  border-top: 1px solid;' ).
lo_buf->add( '  border-bottom: 1px solid;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'div.diff_content tbody tr td{' ).
lo_buf->add( '  width: 50%;' ).
lo_buf->add( '  vertical-align: top' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'div.diff_head span.state-block {' ).
lo_buf->add( '  margin-left: 0.5em;' ).
lo_buf->add( '  font-size: inherit;' ).
lo_buf->add( '  vertical-align: initial;' ).
lo_buf->add( '}' ).
lo_buf->add( 'div.diff_head span.state-block span {' ).
lo_buf->add( '  padding: 0px 4px;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* DIFF TABLE */' ).
lo_buf->add( '' ).
lo_buf->add( 'table.diff_tab {' ).
lo_buf->add( '  font-family: Consolas, Courier, monospace;' ).
lo_buf->add( '  font-size: 10pt;' ).
lo_buf->add( '  width: 100%;' ).
lo_buf->add( '}' ).
lo_buf->add( 'table.diff_tab td,' ).
lo_buf->add( 'table.diff_tab th {' ).
lo_buf->add( '  padding-left: 0.5em;' ).
lo_buf->add( '  padding-right: 0.5em;' ).
lo_buf->add( '}' ).
lo_buf->add( 'table.diff_tab th {' ).
lo_buf->add( '  text-align: left;' ).
lo_buf->add( '  font-weight: normal;' ).
lo_buf->add( '  padding-top: 3px;' ).
lo_buf->add( '  padding-bottom: 3px;' ).
lo_buf->add( '}' ).
lo_buf->add( 'table.diff_tab thead.header th {' ).
lo_buf->add( '  text-align: left;' ).
lo_buf->add( '  font-weight: bold;' ).
lo_buf->add( '  padding-left: 0.5em;' ).
lo_buf->add( '  font-size: 9pt;' ).
lo_buf->add( '}' ).
lo_buf->add( 'table.diff_tab td.num, th.num {' ).
lo_buf->add( '  width: 1%;' ).
lo_buf->add( '  min-width: 2em;' ).
lo_buf->add( '  padding-right: 8px;' ).
lo_buf->add( '  padding-left:  8px;' ).
lo_buf->add( '  text-align: right !important;' ).
lo_buf->add( '  border-left: 1px solid;' ).
lo_buf->add( '  border-right: 1px solid;' ).
lo_buf->add( '  -ms-user-select: none;' ).
lo_buf->add( '  user-select: none;' ).
lo_buf->add( '}' ).
lo_buf->add( 'table.diff_tab td.patch, th.patch {' ).
lo_buf->add( '  width: 1%;' ).
lo_buf->add( '  min-width: 1.5em;' ).
lo_buf->add( '  padding-right: 8px;' ).
lo_buf->add( '  padding-left:  8px;' ).
lo_buf->add( '  text-align: right !important;' ).
lo_buf->add( '  border-left: 1px solid;' ).
lo_buf->add( '  border-right: 1px solid;' ).
lo_buf->add( '  -ms-user-select: none;' ).
lo_buf->add( '  user-select: none;' ).
lo_buf->add( '  cursor: pointer;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'table.diff_tab tr.diff_line:hover td {' ).
lo_buf->add( '  background-image: linear-gradient(rgba(0, 0, 0, 0.075), rgba(0, 0, 0, 0.075));' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'table.diff_tab td.num::before {' ).
lo_buf->add( '  content: attr(line-num);' ).
lo_buf->add( '}' ).
lo_buf->add( 'table.diff_tab code {' ).
lo_buf->add( '  font-family: inherit;' ).
lo_buf->add( '  white-space: pre;' ).
lo_buf->add( '}' ).
lo_buf->add( 'table.diff_tab td.code {' ).
lo_buf->add( '  word-wrap: break-word;' ).
lo_buf->add( '  white-space: pre-wrap;' ).
lo_buf->add( '  overflow: visible;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'table.diff_tab tbody tr:first-child td { padding-top: 0.5em; }' ).
lo_buf->add( 'table.diff_tab tbody tr:last-child td { padding-bottom: 0.5em; }' ).
lo_buf->add( '' ).
lo_buf->add( 'table.diff_tab td.mark, th.mark {' ).
lo_buf->add( '  width: 0.1%;' ).
lo_buf->add( '  -ms-user-select: none;' ).
lo_buf->add( '  user-select: none;' ).
lo_buf->add( '  cursor: default;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.diff_select_left td.diff_right,' ).
lo_buf->add( '.diff_select_left td.diff_right *,' ).
lo_buf->add( '.diff_select_left th.diff_right,' ).
lo_buf->add( '.diff_select_left th.diff_right *,' ).
lo_buf->add( '.diff_select_right td.diff_left,' ).
lo_buf->add( '.diff_select_right td.diff_left *,' ).
lo_buf->add( '.diff_select_right th.diff_left,' ).
lo_buf->add( '.diff_select_right th.diff_left * {' ).
lo_buf->add( '  -ms-user-select: none;' ).
lo_buf->add( '  user-select: none;' ).
lo_buf->add( '  cursor: text;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.diff_select_left td.diff_left,' ).
lo_buf->add( '.diff_select_left td.diff_left *,' ).
lo_buf->add( '.diff_select_left th.diff_left,' ).
lo_buf->add( '.diff_select_left th.diff_left *,' ).
lo_buf->add( '.diff_select_right td.diff_right,' ).
lo_buf->add( '.diff_select_right td.diff_right *,' ).
lo_buf->add( '.diff_select_right th.diff_right,' ).
lo_buf->add( '.diff_select_right th.diff_right * {' ).
lo_buf->add( '  -ms-user-select: text;' ).
lo_buf->add( '  user-select: text;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'td.diff_others::selection,' ).
lo_buf->add( 'td.diff_others *::selection,' ).
lo_buf->add( 'th.diff_others::selection,' ).
lo_buf->add( 'th.diff_others *::selection {' ).
lo_buf->add( '  background-color: transparent;' ).
lo_buf->add( '  cursor: default;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.diff_select_left td.diff_right::selection,' ).
lo_buf->add( '.diff_select_left td.diff_right *::selection,' ).
lo_buf->add( '.diff_select_left th.diff_right::selection,' ).
lo_buf->add( '.diff_select_left th.diff_right *::selection,' ).
lo_buf->add( '.diff_select_right td.diff_left::selection,' ).
lo_buf->add( '.diff_select_right td.diff_left *::selection,' ).
lo_buf->add( '.diff_select_right th.diff_left::selection,' ).
lo_buf->add( '.diff_select_right th.diff_left *::selection {' ).
lo_buf->add( '  background-color: transparent;' ).
lo_buf->add( '  cursor: text;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* DEBUG INFO STYLES */' ).
lo_buf->add( '' ).
lo_buf->add( 'div.debug_container {' ).
lo_buf->add( '  padding: 0.5em;' ).
lo_buf->add( '  font-size: 10pt;' ).
lo_buf->add( '  font-family: Consolas, Courier, monospace;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'div.debug_container p {' ).
lo_buf->add( '  margin: 0px;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* *** */' ).
lo_buf->add( '' ).
lo_buf->add( 'li.action_link.enabled{' ).
lo_buf->add( '  visibility: visible;' ).
lo_buf->add( '  position: relative;' ).
lo_buf->add( '  display: block;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'li.action_link:not(enabled){' ).
lo_buf->add( '  visibility: hidden;' ).
lo_buf->add( '  position: fixed; /* so it does not take up space when hidden */' ).
lo_buf->add( '  display: none;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* TUTORIAL */' ).
lo_buf->add( '' ).
lo_buf->add( 'div.tutorial {' ).
lo_buf->add( '  margin-top:       3px;' ).
lo_buf->add( '  padding: 0.5em 1em 0.5em 1em;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'div.tutorial li { margin: 2px 0px }' ).
lo_buf->add( 'div.tutorial h1 { font-size: 18pt; }' ).
lo_buf->add( 'div.tutorial h2 { font-size: 14pt;}' ).
lo_buf->add( '' ).
lo_buf->add( '/* MENU */' ).
lo_buf->add( '' ).
lo_buf->add( '/* Special credits to example at https://codepen.io/philhoyt/pen/ujHzd */' ).
lo_buf->add( '/* container div, aligned left, but with .float-right modifier alignes right */' ).
lo_buf->add( '' ).
lo_buf->add( '.nav-container ul {' ).
lo_buf->add( '  list-style: none;' ).
lo_buf->add( '  position: relative;' ).
lo_buf->add( '  float: left;' ).
lo_buf->add( '  margin: 0;' ).
lo_buf->add( '  padding: 0;' ).
lo_buf->add( '  white-space: nowrap;' ).
lo_buf->add( '  text-align: left;' ).
lo_buf->add( '}' ).
lo_buf->add( '.nav-container.float-right ul { float: right; }' ).
lo_buf->add( '' ).
lo_buf->add( '.nav-container ul a {' ).
lo_buf->add( '  display: block;' ).
lo_buf->add( '  text-decoration: none;' ).
lo_buf->add( '  line-height: 30px;' ).
lo_buf->add( '  padding: 0 12px;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* clearfix https://css-tricks.com/snippets/css/clear-fix/ */' ).
lo_buf->add( '.nav-container:after {' ).
lo_buf->add( '  clear: both;' ).
lo_buf->add( '  display: block;' ).
lo_buf->add( '  content: "";' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* submenues align to left or right border of the active item' ).
lo_buf->add( '   depending on .float-right modifier */' ).
lo_buf->add( '.nav-container ul li {' ).
lo_buf->add( '  position: relative;' ).
lo_buf->add( '  float: left;' ).
lo_buf->add( '  margin: 0;' ).
lo_buf->add( '  padding: 0;' ).
lo_buf->add( '}' ).
lo_buf->add( '.nav-container.float-right ul ul { left: auto; right: 0; }' ).
lo_buf->add( '.nav-container ul li.current-menu-item { font-weight: 700; }' ).
lo_buf->add( '.nav-container ul li.force-nav-hover ul { display: block; }' ).
lo_buf->add( '.nav-container ul li:hover > ul { display: block; }' ).
lo_buf->add( '' ).
lo_buf->add( '/* special selection style for 1st level items (see also .corner below) */' ).
lo_buf->add( '' ).
lo_buf->add( '.nav-container ul ul {' ).
lo_buf->add( '  display: none;' ).
lo_buf->add( '  position: absolute;' ).
lo_buf->add( '  top: 100%;' ).
lo_buf->add( '  left: 0;' ).
lo_buf->add( '  z-index: 1;' ).
lo_buf->add( '  padding: 0;' ).
lo_buf->add( '  box-shadow: 1px 1px 3px 0px #bbb;' ).
lo_buf->add( '  max-height: 700px;' ).
lo_buf->add( '  overflow: auto;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.nav-container ul ul li {' ).
lo_buf->add( '  float: none;' ).
lo_buf->add( '  min-width: 160px;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.nav-container ul ul a {' ).
lo_buf->add( '  line-height: 120%;' ).
lo_buf->add( '  padding: 8px 15px;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.nav-container ul ul ul {' ).
lo_buf->add( '  top: 0;' ).
lo_buf->add( '  left: 100%;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.nav-container.float-right ul ul ul {' ).
lo_buf->add( '  left: auto;' ).
lo_buf->add( '  right: 100%;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* Minizone to extent hover area,' ).
lo_buf->add( '   aligned to the left or to the right of the selected item' ).
lo_buf->add( '   depending on .float-right modifier */' ).
lo_buf->add( '.nav-container > ul > li > div.minizone {' ).
lo_buf->add( '  display: none;' ).
lo_buf->add( '  z-index: 1;' ).
lo_buf->add( '  position: absolute;' ).
lo_buf->add( '  padding: 0px;' ).
lo_buf->add( '  width: 16px;' ).
lo_buf->add( '  height: 100%;' ).
lo_buf->add( '  bottom: 0px;' ).
lo_buf->add( '  left: 100%;' ).
lo_buf->add( '}' ).
lo_buf->add( '.nav-container > ul > li:hover div.minizone { display: block; }' ).
lo_buf->add( '.nav-container.float-right > ul > li > div.minizone {' ).
lo_buf->add( '  left: auto;' ).
lo_buf->add( '  right: 100%;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* icons - text-align strictly left - otherwise look ugly' ).
lo_buf->add( '   + bite a bit of left padding for nicer look' ).
lo_buf->add( '   + forbids item text wrapping (maybe can be done differently) */' ).
lo_buf->add( '.nav-container ul ul li a .icon {' ).
lo_buf->add( '  padding-right: 10px;' ).
lo_buf->add( '  margin-left: -3px;' ).
lo_buf->add( '}' ).
lo_buf->add( '.nav-container ul.with-icons li {' ).
lo_buf->add( '  text-align: left;' ).
lo_buf->add( '  white-space: nowrap;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* Special .corner modifier - hangs menu at the top right corner' ).
lo_buf->add( '   and cancels 1st level background coloring */' ).
lo_buf->add( '.nav-container.corner {' ).
lo_buf->add( '  position: absolute;' ).
lo_buf->add( '  right: 0px;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* Toolbar separator style */' ).
lo_buf->add( '.nav-container ul ul li.separator {' ).
lo_buf->add( '  font-size: x-small;' ).
lo_buf->add( '  text-align: center;' ).
lo_buf->add( '  padding: 4px 0;' ).
lo_buf->add( '  text-transform: uppercase;' ).
lo_buf->add( '  border-bottom: 1px solid;' ).
lo_buf->add( '  border-top: 1px solid;' ).
lo_buf->add( '}' ).
lo_buf->add( '.nav-container ul ul li.separator:first-child { border-top: none; }' ).
lo_buf->add( '' ).
lo_buf->add( '/* NEWS ANNOUNCEMENT */' ).
lo_buf->add( '' ).
lo_buf->add( 'div.info-panel {' ).
lo_buf->add( '  position: absolute;' ).
lo_buf->add( '  z-index: 99;' ).
lo_buf->add( '  top: 36px;' ).
lo_buf->add( '  left: 50%;' ).
lo_buf->add( '  width: 40em;' ).
lo_buf->add( '  margin-left: -20em;' ).
lo_buf->add( '  box-shadow: 1px 1px 3px 2px #dcdcdc;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'div.info-panel-fixed {' ).
lo_buf->add( '  position: fixed;' ).
lo_buf->add( '  top: 15%;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'div.info-panel div.info-hint {' ).
lo_buf->add( '  text-transform: uppercase;' ).
lo_buf->add( '  font-size: small;' ).
lo_buf->add( '  padding: 8px 6px 0px;' ).
lo_buf->add( '  text-align: center;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'div.info-panel div.info-title {' ).
lo_buf->add( '  text-transform: uppercase;' ).
lo_buf->add( '  font-size: small;' ).
lo_buf->add( '  padding: 6px;' ).
lo_buf->add( '  text-align: center;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'div.info-panel div.info-title a.close-btn {' ).
lo_buf->add( '  padding-left: 12px;' ).
lo_buf->add( '  padding-right: 2px;' ).
lo_buf->add( '  position: relative;' ).
lo_buf->add( '  bottom: 1px;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'div.info-panel div.info-list {' ).
lo_buf->add( '  padding: 0.8em 0.7em 1em;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'div.info-panel ul {' ).
lo_buf->add( '  padding-left: 10px;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'div.info-panel li {' ).
lo_buf->add( '  padding-left: 0px;' ).
lo_buf->add( '  list-style-type: none;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'div.info-panel h1:first-child { margin: auto; }' ).
lo_buf->add( 'div.info-panel h1 {' ).
lo_buf->add( '  font-size: inherit;' ).
lo_buf->add( '  padding: 6px 4px;' ).
lo_buf->add( '  margin: 4px auto auto;' ).
lo_buf->add( '  text-decoration: underline;' ).
lo_buf->add( '  font-weight: normal;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'div.info-panel .version-marker {' ).
lo_buf->add( '  display: inline-block;' ).
lo_buf->add( '  margin-left: 20px;' ).
lo_buf->add( '  border-radius: 3px;' ).
lo_buf->add( '  padding: 0px 6px;' ).
lo_buf->add( '  border:  1px solid;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'div.info-panel .update { border:  1px solid; }' ).
lo_buf->add( 'div.info-panel div.info-list td { padding-right: 1em }' ).
lo_buf->add( '' ).
lo_buf->add( '/* ERROR MESSAGE PANEL */' ).
lo_buf->add( '' ).
lo_buf->add( 'div.message-panel {' ).
lo_buf->add( '  z-index: 99;' ).
lo_buf->add( '  box-shadow: 2px 2px 4px 0px hsla(0, 0%, 0%, .1);' ).
lo_buf->add( '  padding: 0.5em 1em;' ).
lo_buf->add( '  position: fixed;' ).
lo_buf->add( '  bottom: 12px;' ).
lo_buf->add( '  width: 95%;' ).
lo_buf->add( '  margin: 0 auto;' ).
lo_buf->add( '  max-width: 1248px;' ).
lo_buf->add( '' ).
lo_buf->add( '  border: 1px solid;' ).
lo_buf->add( '  border-radius: 5px;' ).
lo_buf->add( '  border-color: hsl(0, 42%, 64%);' ).
lo_buf->add( '  background-color: hsla(0, 42%, 90%, 1);' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.message-panel-bar {' ).
lo_buf->add( '  position: absolute;' ).
lo_buf->add( '  bottom: 10px;' ).
lo_buf->add( '  right: 10px;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.message-panel-commands {' ).
lo_buf->add( '  display: none;' ).
lo_buf->add( '  margin-right: 2em;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.message-panel-commands a {' ).
lo_buf->add( '  padding: 0em 0.5em;' ).
lo_buf->add( '  border-left: 1px solid;' ).
lo_buf->add( '  border-left-color: #ccc;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.message-panel-commands a:first-child {' ).
lo_buf->add( '  padding-left: 0;' ).
lo_buf->add( '  border-left: none;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'div.message-panel:hover .message-panel-commands {' ).
lo_buf->add( '  display: block;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* TOOLTIP TEXT */' ).
lo_buf->add( '' ).
lo_buf->add( '.link-hint {' ).
lo_buf->add( '    line-height: 1em;' ).
lo_buf->add( '    text-align: center;' ).
lo_buf->add( '    padding: 5px 15px;' ).
lo_buf->add( '    border-radius: 4px;' ).
lo_buf->add( '    position: absolute;' ).
lo_buf->add( '    z-index: 1;' ).
lo_buf->add( '    margin-top: -30px;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.link-hint-a {' ).
lo_buf->add( '  margin-left: -60px;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.link-hint-input {' ).
lo_buf->add( '  margin-left: -30px;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.link-hint-i {' ).
lo_buf->add( '  margin-left: -30px;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.link-hint .pending { color: hsla(0, 0%, 0%, 0.2); }' ).
lo_buf->add( '' ).
lo_buf->add( '/* Tooltip arrow */' ).
lo_buf->add( '.link-hint::after {' ).
lo_buf->add( '    content: "";' ).
lo_buf->add( '    position: absolute;' ).
lo_buf->add( '    top: 100%;' ).
lo_buf->add( '    left: 50%;' ).
lo_buf->add( '    margin-left: -5px;' ).
lo_buf->add( '    border-width: 5px;' ).
lo_buf->add( '    border-style: solid;' ).
lo_buf->add( '    border-color: transparent;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* HOTKEYS */' ).
lo_buf->add( '' ).
lo_buf->add( 'ul.hotkeys {' ).
lo_buf->add( '  list-style-type: none;' ).
lo_buf->add( '  padding: 0;' ).
lo_buf->add( '  margin: 0;' ).
lo_buf->add( '  font-size: smaller;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'ul.hotkeys span.key-id {' ).
lo_buf->add( '  border: 1px solid;' ).
lo_buf->add( '  border-radius: 3px;' ).
lo_buf->add( '  padding: 1px 7px;' ).
lo_buf->add( '  width: 3em;' ).
lo_buf->add( '  display: inline-block;' ).
lo_buf->add( '  text-align: center;' ).
lo_buf->add( '  margin-top: 0.2em;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'ul.hotkeys span.key-descr {' ).
lo_buf->add( '  margin-left: 1.2em;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'div.corner-hint {' ).
lo_buf->add( '  position: fixed;' ).
lo_buf->add( '  bottom: 10px;' ).
lo_buf->add( '  right: 10px;' ).
lo_buf->add( '  border: 1px solid;' ).
lo_buf->add( '  border-radius: 3px;' ).
lo_buf->add( '  padding: 4px;' ).
lo_buf->add( '  font-size: smaller;' ).
lo_buf->add( '  opacity: 0.5;' ).
lo_buf->add( '  z-index: 99;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* Commit popup */' ).
lo_buf->add( 'table.commit tr .title {' ).
lo_buf->add( '  font-weight: bold;' ).
lo_buf->add( '  vertical-align: top;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* Repo overview */' ).
lo_buf->add( '.repo-overview {' ).
lo_buf->add( '  padding: 0.5em 0.7em;' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo-overview table {' ).
lo_buf->add( '  font-size: 90%;' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo-overview-toolbar {' ).
lo_buf->add( '  padding: 1em 1em;' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo-overview-toolbar label {' ).
lo_buf->add( '  margin-right: 0.5em;' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo-overview th {' ).
lo_buf->add( '  text-align: left;' ).
lo_buf->add( '  font-weight: normal;' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo-overview table {' ).
lo_buf->add( '  border: 1px solid;' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo-overview thead tr {' ).
lo_buf->add( '  border-bottom: 1px solid;' ).
lo_buf->add( '  line-height: 1.5;' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo-overview tfoot tr {' ).
lo_buf->add( '  border-top: 1px solid;' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo-overview tr.favorite .icon-star {' ).
lo_buf->add( '  color: #5e8dc9 !important;' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo-overview td,' ).
lo_buf->add( '.repo-overview th {' ).
lo_buf->add( '  padding: 6px 6px; /* maybe use height ? */' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo-overview .ro-detail { display: none; }' ).
lo_buf->add( '.repo-overview .ro-go a {' ).
lo_buf->add( '  padding: 0px 0.15em;' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo-overview .ro-go a:hover {' ).
lo_buf->add( '  color: #ff721e;' ).
lo_buf->add( '  text-decoration: none;' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo-overview td.labels {' ).
lo_buf->add( '  max-width: 18ch;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* REPO LABELS */' ).
lo_buf->add( '' ).
lo_buf->add( '.repo-label-catalog {' ).
lo_buf->add( '  padding: 1em 1em;' ).
lo_buf->add( '  margin-top: -1em;' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo-label-catalog label {' ).
lo_buf->add( '  margin-right: 0.5em;' ).
lo_buf->add( '}' ).
lo_buf->add( 'ul.repo-labels {' ).
lo_buf->add( '  display: inline-block;' ).
lo_buf->add( '  list-style-type: none;' ).
lo_buf->add( '  padding-inline-start: 0px;' ).
lo_buf->add( '  padding-left: 0px;' ).
lo_buf->add( '  margin-block-start: 0px;' ).
lo_buf->add( '  margin-block-end: 0px;' ).
lo_buf->add( '  margin-top: 0px;' ).
lo_buf->add( '  margin-bottom: 0px;' ).
lo_buf->add( '}' ).
lo_buf->add( 'ul.repo-labels li {' ).
lo_buf->add( '  display: inline-block;' ).
lo_buf->add( '  padding: 3px 5px;' ).
lo_buf->add( '  border-radius: 3px;' ).
lo_buf->add( '  border-style: solid;' ).
lo_buf->add( '  border-width: 1px;' ).
lo_buf->add( '  margin-bottom: 2px;' ).
lo_buf->add( '}' ).
lo_buf->add( 'ul.repo-labels li a {' ).
lo_buf->add( '  color: inherit;' ).
lo_buf->add( '}' ).
lo_buf->add( 'ul.repo-labels li:not(:last-child) { margin-right: 0.3em; }' ).
lo_buf->add( 'table ul.repo-labels li {' ).
lo_buf->add( '  font-size: 90%;' ).
lo_buf->add( '  padding: 2px 4px;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* LABEL COLORS */' ).
lo_buf->add( '' ).
lo_buf->add( '.rl-white {' ).
lo_buf->add( '  color: hsl(0, 0%, 30%);' ).
lo_buf->add( '  background-color: hsl(0, 0%, 100%);' ).
lo_buf->add( '  border-color: hsl(0, 0%, 80%);' ).
lo_buf->add( '}' ).
lo_buf->add( '.rl-white-b {' ).
lo_buf->add( '  color: hsl(214, 100%, 60%);' ).
lo_buf->add( '  background-color: hsl(0, 0%, 100%);' ).
lo_buf->add( '  border-color: hsl(214, 89%, 86%);' ).
lo_buf->add( '}' ).
lo_buf->add( '.rl-white-r {' ).
lo_buf->add( '  color: hsl(0, 100%, 41%);' ).
lo_buf->add( '  background-color: hsl(0, 0%, 100%);' ).
lo_buf->add( '  border-color: hsl(0, 100%, 85%);' ).
lo_buf->add( '}' ).
lo_buf->add( '.rl-grey {' ).
lo_buf->add( '  color: hsl(0, 0%, 100%);' ).
lo_buf->add( '  background-color: hsl(0, 0%, 70%);' ).
lo_buf->add( '  border-color: hsl(0, 0%, 60%);' ).
lo_buf->add( '}' ).
lo_buf->add( '.rl-dark-w {' ).
lo_buf->add( '  color: hsl(0, 0%, 100%);' ).
lo_buf->add( '  background-color: hsl(0, 0%, 25%);' ).
lo_buf->add( '  border-color: hsl(0, 0%, 25%);;' ).
lo_buf->add( '}' ).
lo_buf->add( '.rl-dark-y {' ).
lo_buf->add( '  color: hsl(43, 95%, 75%);' ).
lo_buf->add( '  background-color: hsl(0, 0%, 25%);' ).
lo_buf->add( '  border-color: hsl(0, 0%, 25%);' ).
lo_buf->add( '}' ).
lo_buf->add( '.rl-dark-r {' ).
lo_buf->add( '  color: hsl(0, 100%, 74%);' ).
lo_buf->add( '  background-color: hsl(0, 0%, 25%);' ).
lo_buf->add( '  border-color: hsl(0, 0%, 25%);' ).
lo_buf->add( '}' ).
lo_buf->add( '.rl-dark-b {' ).
lo_buf->add( '  color: hsl(227, 92%, 80%);' ).
lo_buf->add( '  background-color: hsl(0, 0%, 25%);' ).
lo_buf->add( '  border-color: hsl(0, 0%, 25%);' ).
lo_buf->add( '}' ).
lo_buf->add( '.rl-lightblue {' ).
lo_buf->add( '  color: hsl(217, 80%, 25%);' ).
lo_buf->add( '  background-color: hsl(216, 76%, 84%);' ).
lo_buf->add( '  border-color: hsl(216, 76%, 73%);' ).
lo_buf->add( '}' ).
lo_buf->add( '.rl-darkblue {' ).
lo_buf->add( '  color: hsl(218, 77%, 88%);' ).
lo_buf->add( '  background-color: hsl(217, 66%, 32%);' ).
lo_buf->add( '  border-color: hsl(217, 66%, 20%);' ).
lo_buf->add( '}' ).
lo_buf->add( '.rl-lightgreen {' ).
lo_buf->add( '  color: hsl(153, 76%, 18%);' ).
lo_buf->add( '  background-color: hsl(152, 65%, 82%);' ).
lo_buf->add( '  border-color: hsl(152, 65%, 65%);' ).
lo_buf->add( '}' ).
lo_buf->add( '.rl-darkgreen {' ).
lo_buf->add( '  color: hsl(0, 0%, 100%);' ).
lo_buf->add( '  background-color: hsl(153, 77%, 37%);' ).
lo_buf->add( '  border-color: hsl(153, 77%, 30%);' ).
lo_buf->add( '}' ).
lo_buf->add( '.rl-lightred {' ).
lo_buf->add( '  color: hsl(8, 86%, 29%);' ).
lo_buf->add( '  background-color: hsl(8, 74%, 80%);' ).
lo_buf->add( '  border-color: hsl(8, 74%, 70%);' ).
lo_buf->add( '}' ).
lo_buf->add( '.rl-darkred {' ).
lo_buf->add( '  color: hsl(7, 76%, 85%);' ).
lo_buf->add( '  background-color: hsl(8, 77%, 29%);' ).
lo_buf->add( '  border-color: hsl(8, 77%, 20%);' ).
lo_buf->add( '}' ).
lo_buf->add( '.rl-yellow {' ).
lo_buf->add( '  color: hsl(44, 87%, 22%);' ).
lo_buf->add( '  background-color: hsl(44, 94%, 87%);' ).
lo_buf->add( '  border-color: hsl(44, 94%, 70%);' ).
lo_buf->add( '}' ).
lo_buf->add( '.rl-darkyellow {' ).
lo_buf->add( '  color: hsl(49, 100%, 24%);' ).
lo_buf->add( '  background-color: hsl(49, 100%, 64%);' ).
lo_buf->add( '  border-color: hsl(49, 100%, 49%);' ).
lo_buf->add( '}' ).
lo_buf->add( '.rl-orange {' ).
lo_buf->add( '  color: hsl(0, 0%, 100%);' ).
lo_buf->add( '  background-color: hsl(19, 100%, 61%);' ).
lo_buf->add( '  border-color: hsl(19, 100%, 50%);' ).
lo_buf->add( '}' ).
lo_buf->add( '.rl-brown {' ).
lo_buf->add( '  color: hsl(33, 100%, 89%);' ).
lo_buf->add( '  background-color: hsl(33, 66%, 39%);' ).
lo_buf->add( '  border-color: hsl(33, 66%, 30%);' ).
lo_buf->add( '}' ).
lo_buf->add( '.rl-pink {' ).
lo_buf->add( '  color: hsl(340, 35%, 45%);' ).
lo_buf->add( '  background-color: hsl(340, 85%, 77%);' ).
lo_buf->add( '  border-color: hsl(340, 85%, 65%);' ).
lo_buf->add( '}' ).
lo_buf->add( '.rl-teal {' ).
lo_buf->add( '  color: hsl(0, 0%, 100%);' ).
lo_buf->add( '  background-color: hsl(191, 61%, 45%);' ).
lo_buf->add( '  border-color: hsl(191, 61%, 37%);' ).
lo_buf->add( '}' ).
lo_buf->add( '.rl-darkviolet {' ).
lo_buf->add( '  color: hsl(0, 0%, 100%);' ).
lo_buf->add( '  background-color: hsl(258, 100%, 80%);' ).
lo_buf->add( '  border-color: hsl(258, 100%, 72%);' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* FORM FIELD HELP TOOLTIP */' ).
lo_buf->add( '' ).
lo_buf->add( '.form-field-help-tooltip {' ).
lo_buf->add( '  position: relative;' ).
lo_buf->add( '  display: inline-block;' ).
lo_buf->add( '}' ).
lo_buf->add( '.form-field-help-tooltip .form-field-help-tooltip-text {' ).
lo_buf->add( '  visibility: hidden;' ).
lo_buf->add( '  width: 40ch;' ).
lo_buf->add( '  border-radius: 5px;' ).
lo_buf->add( '  position: absolute;' ).
lo_buf->add( '  z-index: 1;' ).
lo_buf->add( '  border: 1px solid;' ).
lo_buf->add( '  padding: 0.4em 0.6em;' ).
lo_buf->add( '}' ).
lo_buf->add( '.form-field-help-tooltip .form-field-help-tooltip-text p {' ).
lo_buf->add( '  margin: 0px;' ).
lo_buf->add( '}' ).
lo_buf->add( '.form-field-help-tooltip .form-field-help-tooltip-text {' ).
lo_buf->add( '  background-color: white;' ).
lo_buf->add( '  border-color: #888;' ).
lo_buf->add( '}' ).
lo_buf->add( '.form-field-help-tooltip:hover .form-field-help-tooltip-text {' ).
lo_buf->add( '  visibility: visible;' ).
lo_buf->add( '}' ).
lo_buf->add( '.form-field-help-tooltip code {' ).
lo_buf->add( '  border-radius: 5px;' ).
lo_buf->add( '  font-size: 90%;' ).
lo_buf->add( '  padding: 0.1em 0.4em;' ).
lo_buf->add( '  background-color: #e2e2e2;' ).
lo_buf->add( '  word-wrap: break-word;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* CODE INSPECTOR */' ).
lo_buf->add( '' ).
lo_buf->add( '.ci-head { padding: 0.5em 1em; }' ).
lo_buf->add( '.ci-head .package-name span { margin-left: 0.3em; }' ).
lo_buf->add( '.ci-variant { font-weight: bold; }' ).
lo_buf->add( '.ci-result {' ).
lo_buf->add( '  padding: 6px;' ).
lo_buf->add( '  margin-top: 4px;' ).
lo_buf->add( '}' ).
lo_buf->add( '.ci-result li {' ).
lo_buf->add( '  list-style-type: none;' ).
lo_buf->add( '  padding: 0.3em 0.8em;' ).
lo_buf->add( '  margin-top: 6px;' ).
lo_buf->add( '  border-left: 4px solid;' ).
lo_buf->add( '}' ).
lo_buf->add( '.ci-result li:first-child { margin-top: 0px; }' ).
lo_buf->add( '.ci-result li > span { display: block; }' ).
lo_buf->add( '' ).
lo_buf->add( '/* FLOATING BUTTONS */' ).
lo_buf->add( '' ).
lo_buf->add( '.floating-button {' ).
lo_buf->add( '  position: fixed;' ).
lo_buf->add( '  top: 8em;' ).
lo_buf->add( '  right: 2.8em;' ).
lo_buf->add( '  padding: 1em 1.8em;' ).
lo_buf->add( '  border-radius: 4px;' ).
lo_buf->add( '  border-width: 1px;' ).
lo_buf->add( '  border-style: solid;' ).
lo_buf->add( '  box-shadow: 2px 2px 6px 0px #ccc;' ).
lo_buf->add( '  cursor: pointer;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* COMMAND PALETTE */' ).
lo_buf->add( '' ).
lo_buf->add( '.cmd-palette {' ).
lo_buf->add( '  position: absolute;' ).
lo_buf->add( '  z-index: 99;' ).
lo_buf->add( '  top: 36px;' ).
lo_buf->add( '  left: 50%;' ).
lo_buf->add( '  width: 40em;' ).
lo_buf->add( '  margin-left: -20em;' ).
lo_buf->add( '  box-shadow: 1px 1px 3px 2px #dcdcdc;' ).
lo_buf->add( '  background-color: white;' ).
lo_buf->add( '  border: solid 2px;' ).
lo_buf->add( '  padding: 0px 1px;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.cmd-palette input {' ).
lo_buf->add( '  width: 100%;' ).
lo_buf->add( '  box-sizing: border-box;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.cmd-palette ul {' ).
lo_buf->add( '  max-height: 10em;' ).
lo_buf->add( '  overflow-y: scroll;' ).
lo_buf->add( '  margin: 4px 0;' ).
lo_buf->add( '  padding: 2px 4px;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.cmd-palette li {' ).
lo_buf->add( '  list-style-type: none;' ).
lo_buf->add( '  cursor: default;' ).
lo_buf->add( '  padding: 4px 6px;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.cmd-palette li .icon {' ).
lo_buf->add( '  margin-right: 10px;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.cmd-palette li .icon:before {' ).
lo_buf->add( '  width: 1.1em;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* SETTINGS */' ).
lo_buf->add( '' ).
lo_buf->add( 'table.settings_tab {' ).
lo_buf->add( '  border: 1px solid;' ).
lo_buf->add( '  max-width: 600px;' ).
lo_buf->add( '  line-height: 1.5;' ).
lo_buf->add( '}' ).
lo_buf->add( 'table.settings_tab th {' ).
lo_buf->add( '  text-align: left;' ).
lo_buf->add( '  padding: 0.5em;' ).
lo_buf->add( '  border-bottom: 1px solid;' ).
lo_buf->add( '}' ).
lo_buf->add( 'table.settings_tab td {' ).
lo_buf->add( '  text-align: left;' ).
lo_buf->add( '  padding: 0.3em 0.5em;' ).
lo_buf->add( '  border-top: 1px solid;' ).
lo_buf->add( '}' ).
lo_buf->add( 'table.settings_tab input {' ).
lo_buf->add( '  border: none;' ).
lo_buf->add( '  text-align: center;' ).
lo_buf->add( '}' ).
lo_buf->add( 'settings_tab tr:first-child td { border-top: 0px; }' ).
lo_buf->add( '' ).
lo_buf->add( '/* UNIT TESTS */' ).
lo_buf->add( '' ).
lo_buf->add( 'table.unit_tests {' ).
lo_buf->add( '  line-height: 1.5;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* DIALOGS */' ).
lo_buf->add( '' ).
lo_buf->add( '.dialog {' ).
lo_buf->add( '  margin: 0 auto;' ).
lo_buf->add( '  margin-top: 1em;' ).
lo_buf->add( '  margin-bottom: 1em;' ).
lo_buf->add( '  border: 1px solid;' ).
lo_buf->add( '  padding: 1em 1em;' ).
lo_buf->add( '  border-radius: 6px;' ).
lo_buf->add( '  text-align: left;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog-form {' ).
lo_buf->add( '  width: 600px;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog-form-center {' ).
lo_buf->add( '  margin: 1em auto 1em;' ).
lo_buf->add( '  max-width: 600px;' ).
lo_buf->add( '  width: 100%;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog ul {' ).
lo_buf->add( '  padding: 0;' ).
lo_buf->add( '  margin: 0;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog li {' ).
lo_buf->add( '  padding: 5px 10px;' ).
lo_buf->add( '  display: block;' ).
lo_buf->add( '  list-style: none;' ).
lo_buf->add( '  position: relative;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog li.dialog-commands {' ).
lo_buf->add( '  text-align: right;' ).
lo_buf->add( '  margin-top: 12px;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog li.dialog-commands a {' ).
lo_buf->add( '  border: 1px solid;' ).
lo_buf->add( '  cursor: pointer;' ).
lo_buf->add( '  text-decoration: none;' ).
lo_buf->add( '  padding: 6px 12px;' ).
lo_buf->add( '  border-radius: 3px;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog li.dialog-commands input[type="button"],' ).
lo_buf->add( '.dialog li.dialog-commands input[type="submit"] {' ).
lo_buf->add( '  border: 1px solid;' ).
lo_buf->add( '  padding: 6px 12px;' ).
lo_buf->add( '  border-radius: 3px;' ).
lo_buf->add( '  cursor: pointer;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog li.dialog-commands a.main,' ).
lo_buf->add( '.dialog li.dialog-commands input[type="submit"].main {' ).
lo_buf->add( '  border: 1px solid transparent;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog label {' ).
lo_buf->add( '  display: block;' ).
lo_buf->add( '  font-size: 90%;' ).
lo_buf->add( '  margin-top: 6px;' ).
lo_buf->add( '  margin-bottom: 6px;' ).
lo_buf->add( '  padding-left: 0.5em;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog li.error small {' ).
lo_buf->add( '  display: block;' ).
lo_buf->add( '  font-size: 75%;' ).
lo_buf->add( '  margin: 4px 0px;' ).
lo_buf->add( '  padding-left: 0.5em;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog li.hidden {' ).
lo_buf->add( '  padding: 0px 0px;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog .radio-container {' ).
lo_buf->add( '  border: 1px solid;' ).
lo_buf->add( '  display: inline-block;' ).
lo_buf->add( '  padding: 4px;' ).
lo_buf->add( '  border-radius: 3px;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog input[type="checkbox"] + label {' ).
lo_buf->add( '  display: inline-block;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog input[type="password"],' ).
lo_buf->add( '.dialog input[type="text"] {' ).
lo_buf->add( '  width: 100%;' ).
lo_buf->add( '  box-sizing: border-box;' ).
lo_buf->add( '  height: 2.5em;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog input[type="number"] {' ).
lo_buf->add( '  width: 25%;' ).
lo_buf->add( '  box-sizing: border-box;' ).
lo_buf->add( '  height: 2.5em;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog textarea {' ).
lo_buf->add( '  width: 100%;' ).
lo_buf->add( '  box-sizing: border-box;' ).
lo_buf->add( '  padding: 10px;' ).
lo_buf->add( '  font-family: Arial,Helvetica,sans-serif;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog .radio-container input[type="radio"] {' ).
lo_buf->add( '  visibility: hidden;' ).
lo_buf->add( '  display: none;' ).
lo_buf->add( '  height: 0px;' ).
lo_buf->add( '  width: 0px;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog .radio-container input[type="radio"] + label {' ).
lo_buf->add( '  border: 1px solid transparent;' ).
lo_buf->add( '  cursor: pointer;' ).
lo_buf->add( '  width: auto;' ).
lo_buf->add( '  margin: 0px;' ).
lo_buf->add( '  padding: 3px 8px;' ).
lo_buf->add( '  border-radius: 2px;' ).
lo_buf->add( '  display: inline-block;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog .radio-container input[type="radio"]:checked + label {' ).
lo_buf->add( '  border: 1px solid transparent;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog table {' ).
lo_buf->add( '  width: 100%;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog table thead td {' ).
lo_buf->add( '  font-size: 14px;' ).
lo_buf->add( '  height: 2.5em;' ).
lo_buf->add( '  background-color: #ddd;' ).
lo_buf->add( '  border: 1px solid #ccc;' ).
lo_buf->add( '  padding: 0px 10px;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog table tbody td {' ).
lo_buf->add( '  border: 1px solid #ccc;' ).
lo_buf->add( '  background-color: white;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog table td input {' ).
lo_buf->add( '  border: 0px;' ).
lo_buf->add( '  background: none;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog li.with-command div.input-container {' ).
lo_buf->add( '  display: table-cell;' ).
lo_buf->add( '  width: 100%;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog li.with-command div.command-container {' ).
lo_buf->add( '  display: table-cell;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog li.with-command input[type="button"],' ).
lo_buf->add( '.dialog li.with-command input[type="submit"] {' ).
lo_buf->add( '  height: 2.5em;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog fieldset {' ).
lo_buf->add( '  margin-top: 1em;' ).
lo_buf->add( '  border: 1px solid;' ).
lo_buf->add( '  border-right: none;' ).
lo_buf->add( '  border-left: none;' ).
lo_buf->add( '  border-bottom: none;' ).
lo_buf->add( '  border-radius: 6px; /* does not work in IE ? */' ).
lo_buf->add( '  padding-bottom: 1em;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog fieldset:first-child {' ).
lo_buf->add( '  margin-top: 0;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog fieldset legend {' ).
lo_buf->add( '  font-size: large;' ).
lo_buf->add( '  font-weight: bold;' ).
lo_buf->add( '  padding-left: 0.5em;' ).
lo_buf->add( '  padding-right: 0.5em;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog .dialog-help {' ).
lo_buf->add( '  float: left;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* STICKY HEADERS */' ).
lo_buf->add( '' ).
lo_buf->add( '/* https://www.w3schools.com/howto/howto_js_navbar_sticky.asp */' ).
lo_buf->add( '/* Note: We have to use JS since IE does not support CSS position:sticky */' ).
lo_buf->add( '' ).
lo_buf->add( '/* The sticky class is added to the navbar with JS when it reaches its scroll position */' ).
lo_buf->add( '.sticky {' ).
lo_buf->add( '  position: fixed;' ).
lo_buf->add( '  top: 0;' ).
lo_buf->add( '  z-index: 10;' ).
lo_buf->add( '  width: 100%;' ).
lo_buf->add( '  padding: 0.5em;' ).
lo_buf->add( '  margin-bottom: 3px;' ).
lo_buf->add( '  max-height: 47px;' ).
lo_buf->add( '  max-width: 1265px; /* if set to 1280px, then actual width will be 1296px (strange) */' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.sticky_full_width {' ).
lo_buf->add( '  position: fixed;' ).
lo_buf->add( '  top: 0;' ).
lo_buf->add( '  z-index: 10;' ).
lo_buf->add( '  width: 100%;' ).
lo_buf->add( '  padding: 0.5em 0.5em;' ).
lo_buf->add( '  margin-bottom: 3px;' ).
lo_buf->add( '  max-height: 47px;' ).
lo_buf->add( '}' ).
lo_buf->add( '.sticky_full_width .nav-container {' ).
lo_buf->add( '  margin-right: 18px;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* Add some top padding to the page content to prevent sudden quick movement' ).
lo_buf->add( '   as the navigation bar gets a new position at the top of the page */' ).
lo_buf->add( '.sticky + .not_sticky {' ).
lo_buf->add( '  padding-top: 50px;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.sticky_full_width + .not_sticky {' ).
lo_buf->add( '  padding-top: 50px;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* Light toolbar or blocks with separators */' ).
lo_buf->add( '.toolbar-light a {' ).
lo_buf->add( '  padding-left: 0.5em;' ).
lo_buf->add( '  padding-right: 0.5em;' ).
lo_buf->add( '  border-left: 1px solid;' ).
lo_buf->add( '  border-left-color: #ccc;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.toolbar-light a:first-child {' ).
lo_buf->add( '  padding-left: 0;' ).
lo_buf->add( '  border-left: none;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* Warning if wrong browser control is used */' ).
lo_buf->add( '.browser-control-warning {' ).
lo_buf->add( '  width: 100%;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* MODAL POPUP */' ).
lo_buf->add( '/* https://css-tricks.com/considerations-styling-modal/ */' ).
lo_buf->add( '' ).
lo_buf->add( '.modal {' ).
lo_buf->add( '    /* center on screen */' ).
lo_buf->add( '    position: fixed;' ).
lo_buf->add( '    top: 50%;' ).
lo_buf->add( '    left: 50%;' ).
lo_buf->add( '    transform: translate(-50%, -50%);' ).
lo_buf->add( '    /* size */' ).
lo_buf->add( '    max-width: 100%;' ).
lo_buf->add( '    max-height: 100%;' ).
lo_buf->add( '    /* infront of overlay */' ).
lo_buf->add( '    z-index: 1010;' ).
lo_buf->add( '    display: block;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.modal-guts {' ).
lo_buf->add( '    padding: 6px 6px;' ).
lo_buf->add( '    /* let it scroll */' ).
lo_buf->add( '    overflow: auto;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.modal-guts .dialog {' ).
lo_buf->add( '    box-shadow: 2px 2px 4px 1px rgba(0,0,0,0.3);' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.modal-overlay {' ).
lo_buf->add( '    /* darken and prevent interactions with background */' ).
lo_buf->add( '    z-index: 1000;' ).
lo_buf->add( '    position: fixed;' ).
lo_buf->add( '    top: 0;' ).
lo_buf->add( '    left: 0;' ).
lo_buf->add( '    width: 100%;' ).
lo_buf->add( '    height: 100%;' ).
lo_buf->add( '    background: rgba(0, 0, 0, 0.3);' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.modal .radio-container label {' ).
lo_buf->add( '  /* hacky, improve later, get rid of !important, hook it to a named style instead */' ).
lo_buf->add( '  border-radius: 3px !important;' ).
lo_buf->add( '  border: 1px solid rgba(0, 0, 0, 0.3) !important;' ).
lo_buf->add( '  margin-bottom: 2px !important;' ).
lo_buf->add( '}' ).
lo_buf->add( '.modal .radio-container label:hover {' ).
lo_buf->add( '  background-color: rgba(0, 0, 0, 0.1);' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
    li_asset_man->register_asset(
      iv_url       = 'css/common.css'
      iv_type      = 'text/css'
      iv_mime_name = 'ZABAPGIT_CSS_COMMON'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

lo_buf->add( '/*' ).
lo_buf->add( ' * ABAPGIT COLOR THEME CSS - DEFAULT' ).
lo_buf->add( ' */' ).
lo_buf->add( '' ).
lo_buf->add( ':root {' ).
lo_buf->add( '  --theme-background-color: #E8E8E8;' ).
lo_buf->add( '  --theme-container-background-color: #f2f2f2;' ).
lo_buf->add( '  --theme-container-border-color: lightgrey;' ).
lo_buf->add( '  --theme-table-background-color: white;' ).
lo_buf->add( '  --theme-table-head-background-color: #edf2f9;' ).
lo_buf->add( '  --theme-table-border-color: #ddd;' ).
lo_buf->add( '  --theme-table-cell-border-color: #eee;' ).
lo_buf->add( '' ).
lo_buf->add( '  --theme-primary-font: "72", Arial, Helvetica, sans-serif;' ).
lo_buf->add( '  --theme-primary-font-color: #333333;' ).
lo_buf->add( '  --theme-primary-font-color-reduced: #ccc;' ).
lo_buf->add( '  --theme-font-size: 12pt;' ).
lo_buf->add( '  --theme-link-color: #4078c0;' ).
lo_buf->add( '' ).
lo_buf->add( '  --theme-greyscale-dark: #808080;' ).
lo_buf->add( '  --theme-greyscale-medium: #b3b3b3;' ).
lo_buf->add( '  --theme-greyscale-light: #ccc;' ).
lo_buf->add( '  --theme-greyscale-lighter: lightgrey;' ).
lo_buf->add( '  --theme-linkhint-background: lightgreen;' ).
lo_buf->add( '  --theme-debug-color: #aaa;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* GLOBALS */' ).
lo_buf->add( '' ).
lo_buf->add( 'body {' ).
lo_buf->add( '  background-color: var(--theme-background-color);' ).
lo_buf->add( '  font-family: var(--theme-primary-font);' ).
lo_buf->add( '  color: var(--theme-primary-font-color);' ).
lo_buf->add( '  font-size: var(--theme-font-size);' ).
lo_buf->add( '}' ).
lo_buf->add( 'a, a:visited  { color: var(--theme-link-color); }' ).
lo_buf->add( '.link  {' ).
lo_buf->add( '  color: var(--theme-link-color);' ).
lo_buf->add( '  cursor: pointer;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'input, textarea, select     { border-color: #ddd; }' ).
lo_buf->add( 'input:focus, textarea:focus { border-color: #8cadd9; }' ).
lo_buf->add( '' ).
lo_buf->add( '/* COLOR PALETTE */' ).
lo_buf->add( '' ).
lo_buf->add( '.grey         { color: var(--theme-greyscale-lighter) !important; }' ).
lo_buf->add( '.grey70       { color: var(--theme-greyscale-medium)  !important; }' ).
lo_buf->add( '.grey80       { color: var(--theme-greyscale-light)   !important; }' ).
lo_buf->add( '.darkgrey     { color: var(--theme-greyscale-dark)    !important; }' ).
lo_buf->add( '.bgorange     { background-color: orange; }' ).
lo_buf->add( '.attention    { color: red        !important; }' ).
lo_buf->add( '.error        { color: #d41919    !important; }' ).
lo_buf->add( '.warning      { color: #efb301    !important; }' ).
lo_buf->add( '.success      { color: green      !important; }' ).
lo_buf->add( '.blue         { color: #5e8dc9    !important; }' ).
lo_buf->add( '.red          { color: red        !important; }' ).
lo_buf->add( '.white        { color: white      !important; }' ).
lo_buf->add( '.pink         { color: pink       !important; }' ).
lo_buf->add( '' ).
lo_buf->add( '/* FLOATING BUTTONS AND COLOR SETS */' ).
lo_buf->add( '' ).
lo_buf->add( '.blue-set {' ).
lo_buf->add( '  border-color: #abc3e3;' ).
lo_buf->add( '  color: #5e8dc9;' ).
lo_buf->add( '  background-color: #d9e4f2;' ).
lo_buf->add( '}' ).
lo_buf->add( '.grey-set {' ).
lo_buf->add( '  border-color: #c7c7c7;' ).
lo_buf->add( '  color: var(--theme-greyscale-dark);' ).
lo_buf->add( '  background-color: #e6e6e6;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* ABAPGIT OBJECTS */' ).
lo_buf->add( '' ).
lo_buf->add( 'span.user-box {' ).
lo_buf->add( '  border-color: #c2d4ea;' ).
lo_buf->add( '  background-color: #d9e4f2;' ).
lo_buf->add( '}' ).
lo_buf->add( 'span.package-box {' ).
lo_buf->add( '  border-color: #d3ccd2;' ).
lo_buf->add( '  background-color: #ebe3ea;' ).
lo_buf->add( '}' ).
lo_buf->add( 'span.path-box,' ).
lo_buf->add( 'span.transport-box {' ).
lo_buf->add( '  border-color: #a7e3cf;' ).
lo_buf->add( '  background-color: #dbf3eb;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* PANELS */' ).
lo_buf->add( '/* TODO: add warning and error colors */' ).
lo_buf->add( '' ).
lo_buf->add( 'div.panel.success {' ).
lo_buf->add( '  color: #589a58 !important;' ).
lo_buf->add( '  background-color: #c5eac5;' ).
lo_buf->add( '}' ).
lo_buf->add( 'div.panel.error {' ).
lo_buf->add( '  color: #d41919;' ).
lo_buf->add( '  background-color: #fad6d6;' ).
lo_buf->add( '}' ).
lo_buf->add( '#debug-output { color: var(--theme-debug-color); }' ).
lo_buf->add( 'div.dummydiv { background-color: var(--theme-container-background-color); }' ).
lo_buf->add( '' ).
lo_buf->add( '/* STRUCTURE DIVS, HEADER & FOOTER */' ).
lo_buf->add( '' ).
lo_buf->add( 'div#header {' ).
lo_buf->add( '  background-color: var(--theme-background-color);' ).
lo_buf->add( '  border-bottom-color: var(--theme-container-border-color);' ).
lo_buf->add( '}' ).
lo_buf->add( 'div#header .page-title { color: var(--theme-greyscale-medium); }' ).
lo_buf->add( 'div#footer .version { color: var(--theme-greyscale-medium); }' ).
lo_buf->add( 'div#footer { border-top-color: var(--theme-container-border-color); }' ).
lo_buf->add( '' ).
lo_buf->add( '/* ERROR LOG */' ).
lo_buf->add( '' ).
lo_buf->add( 'div.log {' ).
lo_buf->add( '  background-color: #fee6e6;' ).
lo_buf->add( '  border-color: #fdcece;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* REPOSITORY */' ).
lo_buf->add( '' ).
lo_buf->add( 'div.repo { background-color: var(--theme-container-background-color); }' ).
lo_buf->add( '.repo_name span.name { color: #333; }' ).
lo_buf->add( '.repo_name span.url  { color: var(--theme-primary-font-color-reduced); }' ).
lo_buf->add( '.repo_name a.url { color: var(--theme-primary-font-color-reduced); }' ).
lo_buf->add( '.repo_name a.url:hover { color: var(--theme-link-color); }' ).
lo_buf->add( '.repo_attr       { color: grey; }' ).
lo_buf->add( '' ).
lo_buf->add( '.repo_attr span.bg_marker {' ).
lo_buf->add( '  border-color: #d2d2d2;' ).
lo_buf->add( '  background-color: #d8d8d8;' ).
lo_buf->add( '  color: #fff;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.repo_attr span.branch_head {' ).
lo_buf->add( '  border-color: #d8dff3;' ).
lo_buf->add( '  background-color: #eceff9;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'span.branch {' ).
lo_buf->add( '  border-color: #d9d9d9;' ).
lo_buf->add( '  background-color: #e2e2e2;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'span.branch_branch {' ).
lo_buf->add( '  border-color: #e7d9b1;' ).
lo_buf->add( '  background-color: #f8f0d8;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* REPOSITORY TABLE*/' ).
lo_buf->add( '' ).
lo_buf->add( 'table.repo_tab {' ).
lo_buf->add( '  border-color: var(--theme-table-border-color);' ).
lo_buf->add( '  background-color: var(--theme-table-background-color);' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo_tab th {' ).
lo_buf->add( '  color: var(--theme-link-color);' ).
lo_buf->add( '  background-color: #edf2f9;' ).
lo_buf->add( '  border-bottom-color: var(--theme-table-border-color);' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo_tab td {' ).
lo_buf->add( '  color: var(--theme-primary-font-color);' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.repo_tab tr.object_row{' ).
lo_buf->add( '  border-top-color: var(--theme-table-cell-border-color);' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo_tab .inactive      { color: orange; }' ).
lo_buf->add( '.repo_tab tr.unsupported { color: var(--theme-greyscale-lighter); }' ).
lo_buf->add( '.repo_tab tr.modified    { background-color: #fbf7e9; }' ).
lo_buf->add( '.repo_tab td.current_dir { color: var(--theme-primary-font-color-reduced); }' ).
lo_buf->add( '' ).
lo_buf->add( '/* STAGE */' ).
lo_buf->add( '' ).
lo_buf->add( '.stage_tab {' ).
lo_buf->add( '  border-color: #ddd;' ).
lo_buf->add( '  background-color: #fff;' ).
lo_buf->add( '}' ).
lo_buf->add( '.stage_tab th {' ).
lo_buf->add( '  color: var(--theme-greyscale-dark);' ).
lo_buf->add( '  background-color: #edf2f9;' ).
lo_buf->add( '  border-bottom-color: #ddd;' ).
lo_buf->add( '}' ).
lo_buf->add( '.stage_tab td {' ).
lo_buf->add( '  color: var(--theme-greyscale-medium);' ).
lo_buf->add( '  border-top-color: var(--theme-table-cell-border-color);' ).
lo_buf->add( '}' ).
lo_buf->add( '.stage_tab td.status {' ).
lo_buf->add( '  color: var(--theme-primary-font-color-reduced);' ).
lo_buf->add( '  background-color: #fafafa;' ).
lo_buf->add( '}' ).
lo_buf->add( '.stage_tab td.highlight { color: #444 !important; }' ).
lo_buf->add( '.stage_tab td.method { font-weight: bold; }' ).
lo_buf->add( '.stage_tab mark {' ).
lo_buf->add( '  color: white;' ).
lo_buf->add( '  background-color: #79a0d2;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* COMMIT */' ).
lo_buf->add( '' ).
lo_buf->add( 'div.form-container { background-color: #F8F8F8; }' ).
lo_buf->add( 'form.aligned-form label { color: var(--theme-greyscale-medium); }' ).
lo_buf->add( 'form.aligned-form span.sub-title { color: var(--theme-greyscale-medium); }' ).
lo_buf->add( '' ).
lo_buf->add( '/* SETTINGS STYLES */' ).
lo_buf->add( '' ).
lo_buf->add( 'div.settings_container {' ).
lo_buf->add( '  color: #444;' ).
lo_buf->add( '  background-color: var(--theme-container-background-color);' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* DIFF */' ).
lo_buf->add( '' ).
lo_buf->add( 'div.diff { background-color: var(--theme-container-background-color); }' ).
lo_buf->add( 'span.diff_name { color: grey; }' ).
lo_buf->add( 'span.diff_name strong { color: #333; }' ).
lo_buf->add( 'span.diff_changed_by  { color: grey; }' ).
lo_buf->add( 'span.diff_changed_by span.user {' ).
lo_buf->add( '  border-color: #c2d4ea;' ).
lo_buf->add( '  background-color: #d9e4f2;' ).
lo_buf->add( '}' ).
lo_buf->add( '.diff_ins {' ).
lo_buf->add( '  border-color: #abf2ab;' ).
lo_buf->add( '  background-color: #e0ffe0;' ).
lo_buf->add( '}' ).
lo_buf->add( '.diff_del {' ).
lo_buf->add( '  border-color: #ff667d;' ).
lo_buf->add( '  background-color: #ffccd4;' ).
lo_buf->add( '}' ).
lo_buf->add( '.diff_upd {' ).
lo_buf->add( '  border-color: #dada00;' ).
lo_buf->add( '  background-color: #ffffcc;' ).
lo_buf->add( '}' ).
lo_buf->add( 'div.diff_content {' ).
lo_buf->add( '  background-color: #fff;' ).
lo_buf->add( '  border-top-color: #ddd;' ).
lo_buf->add( '  border-bottom-color: #ddd;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* STATE BLOCK COLORS */' ).
lo_buf->add( '' ).
lo_buf->add( 'span.state-block span.added {' ).
lo_buf->add( '  background-color: #69ad74;' ).
lo_buf->add( '  border-color: #579e64;' ).
lo_buf->add( '  color: white;' ).
lo_buf->add( '}' ).
lo_buf->add( 'span.state-block span.changed {' ).
lo_buf->add( '  background-color: #e0c150;' ).
lo_buf->add( '  border-color: #d4af25;' ).
lo_buf->add( '  color: white;' ).
lo_buf->add( '}' ).
lo_buf->add( 'span.state-block span.mixed {' ).
lo_buf->add( '  background-color: #e0c150;' ).
lo_buf->add( '  border-color: #579e64;' ).
lo_buf->add( '  color: #69ad74;' ).
lo_buf->add( '}' ).
lo_buf->add( 'span.state-block span.deleted {' ).
lo_buf->add( '  background-color: #c76861;' ).
lo_buf->add( '  border-color: #b8605a;' ).
lo_buf->add( '  color: white;' ).
lo_buf->add( '}' ).
lo_buf->add( 'span.state-block span.none {' ).
lo_buf->add( '  background-color: #e8e8e8;' ).
lo_buf->add( '  border-color: #dbdbdb;' ).
lo_buf->add( '  color: #c8c8c8;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* DIFF TABLE */' ).
lo_buf->add( '' ).
lo_buf->add( 'table.diff_tab td,' ).
lo_buf->add( 'table.diff_tab th {' ).
lo_buf->add( '  color: #444;' ).
lo_buf->add( '}' ).
lo_buf->add( 'table.diff_tab thead.header th {' ).
lo_buf->add( '  color: #eee;' ).
lo_buf->add( '  background-color: var(--theme-greyscale-medium);' ).
lo_buf->add( '}' ).
lo_buf->add( 'table.diff_tab thead.nav_line {' ).
lo_buf->add( '  background-color: #edf2f9;' ).
lo_buf->add( '}' ).
lo_buf->add( 'table.diff_tab thead.nav_line th {' ).
lo_buf->add( '  color: var(--theme-greyscale-medium);' ).
lo_buf->add( '}' ).
lo_buf->add( 'table.diff_tab td.num, th.num {' ).
lo_buf->add( '  color: var(--theme-primary-font-color-reduced);' ).
lo_buf->add( '  border-left-color: var(--theme-table-cell-border-color);' ).
lo_buf->add( '  border-right-color: var(--theme-table-cell-border-color);' ).
lo_buf->add( '}' ).
lo_buf->add( 'table.diff_tab td.patch, th.patch {' ).
lo_buf->add( '  color: var(--theme-primary-font-color-reduced);' ).
lo_buf->add( '  border-left-color: var(--theme-table-cell-border-color);' ).
lo_buf->add( '  border-right-color: var(--theme-table-cell-border-color);' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* STYLES FOR SYNTAX HIGHLIGHTING */' ).
lo_buf->add( '' ).
lo_buf->add( '/* ABAP */' ).
lo_buf->add( '.syntax-hl span.keyword  { color: #0a69ce; }' ).
lo_buf->add( '.syntax-hl span.text     { color: #48ce4f; }' ).
lo_buf->add( '.syntax-hl span.comment  { color: var(--theme-greyscale-dark); font-style: italic; }' ).
lo_buf->add( '/* XML+HTML */' ).
lo_buf->add( '.syntax-hl span.xml_tag  { color: #457ce3; }' ).
lo_buf->add( '.syntax-hl span.attr     { color: #b777fb; }' ).
lo_buf->add( '.syntax-hl span.attr_val { color: #7a02f9; }' ).
lo_buf->add( '/* CSS+JS */' ).
lo_buf->add( '.syntax-hl span.properties   { color:#0a69ce; }' ).
lo_buf->add( '.syntax-hl span.values       { color:blue; }' ).
lo_buf->add( '.syntax-hl span.units        { color:maroon; }' ).
lo_buf->add( '.syntax-hl span.selectors    { color:purple; }' ).
lo_buf->add( '.syntax-hl span.functions    { color:purple; }' ).
lo_buf->add( '.syntax-hl span.colors       { color:purple; }' ).
lo_buf->add( '.syntax-hl span.extensions   { color:darkblue; }' ).
lo_buf->add( '.syntax-hl span.at_rules     { color:darkblue; }' ).
lo_buf->add( '.syntax-hl span.html         { color:green; }' ).
lo_buf->add( '.syntax-hl span.variables    { color:purple; }' ).
lo_buf->add( '' ).
lo_buf->add( '/* DEBUG INFO STYLES */' ).
lo_buf->add( '' ).
lo_buf->add( 'div.debug_container {' ).
lo_buf->add( '  color: #444;' ).
lo_buf->add( '  background-color: var(--theme-container-background-color);' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* Repo overview */' ).
lo_buf->add( '' ).
lo_buf->add( '.repo-overview { background-color: var(--theme-container-background-color); }' ).
lo_buf->add( '.repo-overview table {' ).
lo_buf->add( '  background-color: var(--theme-table-background-color);' ).
lo_buf->add( '  border-color: var(--theme-table-border-color);' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo-overview th {' ).
lo_buf->add( '  color: var(--theme-link-color);' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo-overview thead tr {' ).
lo_buf->add( '  background-color: var(--theme-table-head-background-color);' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo-overview thead tr,' ).
lo_buf->add( '.repo-overview tfoot tr {' ).
lo_buf->add( '  border-color: var(--theme-table-border-color);' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo-overview a.remote_repo { color: var(--theme-primary-font-color-reduced); }' ).
lo_buf->add( '.repo-overview a.remote_repo:hover { color: var(--theme-link-color); }' ).
lo_buf->add( '.repo-overview tbody tr:hover td { background-color: hsla(214, 50%, 50%, 0.05); }' ).
lo_buf->add( '.repo-overview tbody tr.selected { background-color: hsla(214, 50%, 75%, 0.33); }' ).
lo_buf->add( '' ).
lo_buf->add( '/* TUTORIAL */' ).
lo_buf->add( '' ).
lo_buf->add( 'div.tutorial { background-color: var(--theme-container-background-color); }' ).
lo_buf->add( 'div.tutorial hr { border-color: var(--theme-greyscale-light); }' ).
lo_buf->add( 'div.tutorial h1, h2 { color: #404040; }' ).
lo_buf->add( '' ).
lo_buf->add( '/* MENU */' ).
lo_buf->add( '' ).
lo_buf->add( '.nav-container ul li:hover { background-color: #fff; }' ).
lo_buf->add( '.nav-container ul ul li:hover { background-color: #f6f6f6; }' ).
lo_buf->add( '.nav-container > ul > li:hover > a { background-color: #ffffff80; }' ).
lo_buf->add( '.nav-container ul ul { background-color: #fff; }' ).
lo_buf->add( '.nav-container.corner > ul > li:hover > a { background-color: inherit; }' ).
lo_buf->add( '' ).
lo_buf->add( '.nav-container ul ul li.separator {' ).
lo_buf->add( '  color: var(--theme-greyscale-medium);' ).
lo_buf->add( '  border-bottom-color: #eee;' ).
lo_buf->add( '  border-top-color: #eee;' ).
lo_buf->add( '}' ).
lo_buf->add( '.nav-container ul ul li.separator:hover { background-color: inherit; }' ).
lo_buf->add( '' ).
lo_buf->add( '/* NEWS ANNOUNCEMENT */' ).
lo_buf->add( '' ).
lo_buf->add( 'div.info-panel { background-color: white; }' ).
lo_buf->add( 'div.info-panel div.info-hint { color: var(--theme-greyscale-light); }' ).
lo_buf->add( 'div.info-panel div.info-title {' ).
lo_buf->add( '  color: #f8f8f8;' ).
lo_buf->add( '  background-color: #888;' ).
lo_buf->add( '}' ).
lo_buf->add( 'div.info-panel div.info-title a.close-btn { color: #d8d8d8; }' ).
lo_buf->add( 'div.info-panel div.info-list { color: #444; }' ).
lo_buf->add( 'div.info-panel .version-marker {' ).
lo_buf->add( '  color: white;' ).
lo_buf->add( '  border-color: #c0c0c0;' ).
lo_buf->add( '  background-color: var(--theme-greyscale-light);' ).
lo_buf->add( '}' ).
lo_buf->add( 'div.info-panel .update {' ).
lo_buf->add( '  border-color: #e8ba30;' ).
lo_buf->add( '  background-color: #f5c538;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* TOOLTIPS TEXT */' ).
lo_buf->add( '' ).
lo_buf->add( '.link-hint { color: var(--theme-primary-font-color); }' ).
lo_buf->add( '.link-hint { background-color: var(--theme-linkhint-background) }' ).
lo_buf->add( '.link-hint::after { border-top-color: var(--theme-linkhint-background) }' ).
lo_buf->add( '' ).
lo_buf->add( '/* HOTKEYS */' ).
lo_buf->add( '' ).
lo_buf->add( 'ul.hotkeys span.key-id {' ).
lo_buf->add( '  background-color: #f0f0f0;' ).
lo_buf->add( '  border-color: #dcdcdc;' ).
lo_buf->add( '}' ).
lo_buf->add( 'div.corner-hint {' ).
lo_buf->add( '  color: var(--theme-greyscale-medium);' ).
lo_buf->add( '  border-color: var(--theme-greyscale-light);' ).
lo_buf->add( '  background-color: #fff;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* CODE INSPECTOR */' ).
lo_buf->add( '' ).
lo_buf->add( '.ci-head { background-color: var(--theme-container-background-color); }' ).
lo_buf->add( '.ci-head .package-name span { color: grey; }' ).
lo_buf->add( '.ci-variant   { color: #444; }' ).
lo_buf->add( '.ci-result    { background-color: #f6f6f6; }' ).
lo_buf->add( '.ci-result li { color: #444; }' ).
lo_buf->add( '.ci-result li.ci-error   { border-left-color: #cd5353; }' ).
lo_buf->add( '.ci-result li.ci-warning { border-left-color: #ecd227; }' ).
lo_buf->add( '.ci-result li.ci-info    { border-left-color: #acacac; }' ).
lo_buf->add( '' ).
lo_buf->add( '/* COMMAND PALETTE */' ).
lo_buf->add( '' ).
lo_buf->add( '.cmd-palette {' ).
lo_buf->add( '  border-color: #ccc;' ).
lo_buf->add( '}' ).
lo_buf->add( '.cmd-palette li.selected {' ).
lo_buf->add( '  background-color: hsla(214, 50%, 90%, 1);' ).
lo_buf->add( '}' ).
lo_buf->add( '.cmd-palette mark {' ).
lo_buf->add( '  color: white;' ).
lo_buf->add( '  background-color: #79a0d2;' ).
lo_buf->add( '  /* todo merge with stage search */' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* SETTINGS */' ).
lo_buf->add( '' ).
lo_buf->add( 'table.settings_tab {' ).
lo_buf->add( '  background-color: #fff;' ).
lo_buf->add( '  border-color: #ddd;' ).
lo_buf->add( '}' ).
lo_buf->add( 'table.settings_tab th {' ).
lo_buf->add( '  color: #888888;' ).
lo_buf->add( '  border-bottom-color: #ddd;' ).
lo_buf->add( '}' ).
lo_buf->add( 'table.settings_tab td {' ).
lo_buf->add( '  color: #333;' ).
lo_buf->add( '  border-top-color: #eee;' ).
lo_buf->add( '}' ).
lo_buf->add( 'table.settings_tab input {' ).
lo_buf->add( '  background-color: #f8f8f8;' ).
lo_buf->add( '}' ).
lo_buf->add( 'table.settings_tab input:focus {' ).
lo_buf->add( '  background-color: #fff;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* HTML FORMS */' ).
lo_buf->add( '' ).
lo_buf->add( '.dialog input::placeholder { color: #ccc }' ).
lo_buf->add( '.dialog textarea::placeholder { color: #ccc }' ).
lo_buf->add( '.dialog input:-ms-input-placeholder { color: #ccc }' ).
lo_buf->add( '.dialog textarea:-ms-input-placeholder { color: #ccc }' ).
lo_buf->add( '.dialog {' ).
lo_buf->add( '  border-color: #ccc;' ).
lo_buf->add( '  background-color: #f0f0f0;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog li.dialog-commands a,' ).
lo_buf->add( '.dialog li.dialog-commands input[type="submit"] {' ).
lo_buf->add( '  border-color: #ccc;' ).
lo_buf->add( '  background-color: #ddd;' ).
lo_buf->add( '  color: #000;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog li.dialog-commands a.main,' ).
lo_buf->add( '.dialog li.dialog-commands input[type="submit"].main {' ).
lo_buf->add( '  background-color: #64a8ff;' ).
lo_buf->add( '  color: #fff;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog label {' ).
lo_buf->add( '  color: #444;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog label em {' ).
lo_buf->add( '  color: #64a8ff;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog li.error small {' ).
lo_buf->add( '  color: #ff5959;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog li.error input[type="number"],' ).
lo_buf->add( '.dialog li.error input[type="text"] {' ).
lo_buf->add( '  border-color: #ff5959;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog .radio-container {' ).
lo_buf->add( '  border-color: #ddd;' ).
lo_buf->add( '  background-color: #fff;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog .radio-container input[type="radio"] + label {' ).
lo_buf->add( '  color: #808080;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog .radio-container input[type="radio"]:checked + label {' ).
lo_buf->add( '  background-color: #64a8ff;' ).
lo_buf->add( '  color: #fff;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog li.with-command input[type="button"]:hover,' ).
lo_buf->add( '.dialog li.with-command input[type="submit"]:hover {' ).
lo_buf->add( '  background-color: #64a8ff;' ).
lo_buf->add( '  color: #fff;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog fieldset {' ).
lo_buf->add( '  border-color: #dfdfdf;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog fieldset legend {' ).
lo_buf->add( '  color: #444;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog input:read-only {' ).
lo_buf->add( '  background-color: #f4f4f4;' ).
lo_buf->add( '  color: var(--theme-greyscale-dark);' ).
lo_buf->add( '}' ).
lo_buf->add( '/* for IE */' ).
lo_buf->add( '.dialog input[readonly] {' ).
lo_buf->add( '  background-color: #f4f4f4;' ).
lo_buf->add( '  color: var(--theme-greyscale-dark);' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
    li_asset_man->register_asset(
      iv_url       = 'css/theme-default.css'
      iv_type      = 'text/css'
      iv_cachable  = abap_false
      iv_mime_name = 'ZABAPGIT_CSS_THEME_DEFAULT'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

lo_buf->add( '/*' ).
lo_buf->add( ' * ABAPGIT THEME CSS - DARK' ).
lo_buf->add( ' */' ).
lo_buf->add( '' ).
lo_buf->add( '/* https://experience.sap.com/fiori-design-web/colors/ */' ).
lo_buf->add( '' ).
lo_buf->add( ':root {' ).
lo_buf->add( '  --theme-background-color: #333333;' ).
lo_buf->add( '  --theme-container-background-color: #444444;' ).
lo_buf->add( '  --theme-primary-font: "72", Arial, Helvetica, sans-serif;' ).
lo_buf->add( '  --theme-primary-font-color: #cccccc;' ).
lo_buf->add( '  --theme-primary-font-color-reduced: #EEEEEE;' ).
lo_buf->add( '  --theme-font-size: 11pt;' ).
lo_buf->add( '  --theme-link-color: #d9ffff;' ).
lo_buf->add( '  --theme-link-color-hover: #f6f6f6;' ).
lo_buf->add( '  --theme-container-border-color: #D1E0EE;' ).
lo_buf->add( '  --theme-table-border-color: #E5E5E5; /* ALV border color */' ).
lo_buf->add( '  --theme-greyscale-dark: #666666;' ).
lo_buf->add( '  --theme-greyscale-medium: #999999;' ).
lo_buf->add( '  --theme-greyscale-light: #CCCCCC;' ).
lo_buf->add( '  --theme-greyscale-lighter: #E5E5E5;' ).
lo_buf->add( '  --theme-list-hover-background-color: black;' ).
lo_buf->add( '' ).
lo_buf->add( '  --theme-table-background-color: #333333;' ).
lo_buf->add( '  --theme-table-head-background-color: #202020;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* GLOBALS */' ).
lo_buf->add( '' ).
lo_buf->add( 'body {' ).
lo_buf->add( '  background-color: var(--theme-background-color);' ).
lo_buf->add( '  color: var(--theme-primary-font-color);' ).
lo_buf->add( '}' ).
lo_buf->add( 'select, input, textarea {' ).
lo_buf->add( '  color: var(--theme-primary-font-color);' ).
lo_buf->add( '  border-color: #ffffff;' ).
lo_buf->add( '  background-color: var(--theme-background-color);' ).
lo_buf->add( '}' ).
lo_buf->add( 'a:hover { color: var(--theme-link-color-hover); }' ).
lo_buf->add( '' ).
lo_buf->add( '/* HEADER */' ).
lo_buf->add( '' ).
lo_buf->add( '#header a, #header a:visited { color: var(--theme-link-color); }' ).
lo_buf->add( '' ).
lo_buf->add( '/* MENU */' ).
lo_buf->add( '' ).
lo_buf->add( 'div#toc .favorites a { opacity: 1; }' ).
lo_buf->add( '.nav-container ul a:hover { text-decoration: underline; }' ).
lo_buf->add( '.nav-container ul ul { background-color: #555555; }' ).
lo_buf->add( '.nav-container ul li:hover { background-color: #555555; }' ).
lo_buf->add( '.nav-container ul ul li:hover { background-color: var(--theme-list-hover-background-color); }' ).
lo_buf->add( 'table.repo_tab {' ).
lo_buf->add( '    border-color: var(--theme-container-background-color);' ).
lo_buf->add( '    background-color: var(--theme-background-color);' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* ABAPGIT OBJECTS */' ).
lo_buf->add( '' ).
lo_buf->add( 'span.user-box {' ).
lo_buf->add( '  background-color: #4c6782;' ).
lo_buf->add( '  border-color: #7491b2;' ).
lo_buf->add( '}' ).
lo_buf->add( 'span.package-box {' ).
lo_buf->add( '  background-color: #705a6d;' ).
lo_buf->add( '  border-color: #987095;' ).
lo_buf->add( '}' ).
lo_buf->add( 'span.path-box,' ).
lo_buf->add( 'span.transport-box {' ).
lo_buf->add( '  background-color: #456d5d;' ).
lo_buf->add( '  border-color: #60a087;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* PANELS */' ).
lo_buf->add( '' ).
lo_buf->add( '#debug-output { color: var(--theme-greyscale-dark); }' ).
lo_buf->add( '' ).
lo_buf->add( '/* abapGit logo in header and footer */' ).
lo_buf->add( '.logo .icon.icon-abapgit {' ).
lo_buf->add( '  color: var(--theme-primary-font-color);' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* TUTORIAL */' ).
lo_buf->add( '' ).
lo_buf->add( 'div.tutorial h1, h2 { color: var(--theme-primary-font-color); }' ).
lo_buf->add( '' ).
lo_buf->add( '/* REPOSITORY */' ).
lo_buf->add( '' ).
lo_buf->add( 'div.repo { background-color: var(--theme-container-background-color); }' ).
lo_buf->add( '.repo_name span.name { color: var(--theme-primary-font-color-reduced); }' ).
lo_buf->add( '.repo_name span.url  { color: var(--theme-greyscale-medium); }' ).
lo_buf->add( '.repo_name a.url { color: var(--theme-greyscale-medium); }' ).
lo_buf->add( '.repo_attr { color: var(--theme-primary-font-color); }' ).
lo_buf->add( '' ).
lo_buf->add( 'span.branch_branch {' ).
lo_buf->add( '  border-color: var(--theme-greyscale-medium);' ).
lo_buf->add( '  background-color: #777777;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* REPOSITORY TABLE */' ).
lo_buf->add( '' ).
lo_buf->add( '.repo_tab td { color: var(--theme-primary-font-color); }' ).
lo_buf->add( '.repo_tab tr.unsupported { background-color: #555; }' ).
lo_buf->add( '.repo_tab tr.modified { background-color: #555; }' ).
lo_buf->add( '.repo_tab tr:hover {background-color: var(--theme-list-hover-background-color) !important;}' ).
lo_buf->add( '' ).
lo_buf->add( '.repo_tab th {' ).
lo_buf->add( '  border-top-color: var(--theme-greyscale-dark);' ).
lo_buf->add( '  background-color: black;' ).
lo_buf->add( '}' ).
lo_buf->add( '.repo_tab td {' ).
lo_buf->add( '  border-top-color: var(--theme-greyscale-dark);' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* STAGE */' ).
lo_buf->add( '' ).
lo_buf->add( '.stage_tab {' ).
lo_buf->add( '  border-color: var(--theme-greyscale-dark);' ).
lo_buf->add( '  background-color: var(--theme-background-color);' ).
lo_buf->add( '}' ).
lo_buf->add( '.stage_tab th {' ).
lo_buf->add( '  border-top-color: var(--theme-greyscale-dark);' ).
lo_buf->add( '  background-color: black;' ).
lo_buf->add( '}' ).
lo_buf->add( '.stage_tab td {' ).
lo_buf->add( '  color: var(--theme-primary-font-color);' ).
lo_buf->add( '  border-top-color:  var(--theme-greyscale-dark);' ).
lo_buf->add( '}' ).
lo_buf->add( '.stage_tab td.status.highlight {' ).
lo_buf->add( '  color: var(--theme-primary-font-color) !important;' ).
lo_buf->add( '  background-color: var(--theme-background-color);' ).
lo_buf->add( '}' ).
lo_buf->add( '.stage_tab td.status {' ).
lo_buf->add( '  color: #777;' ).
lo_buf->add( '  background-color: var(--theme-background-color);' ).
lo_buf->add( '}' ).
lo_buf->add( '.stage_tab th { background-color: var(--theme-container-background-color); }' ).
lo_buf->add( '.stage_tab tr:hover {background-color: var(--theme-list-hover-background-color) !important;}' ).
lo_buf->add( '' ).
lo_buf->add( '/* COMMIT */' ).
lo_buf->add( '' ).
lo_buf->add( 'div.form-container { background-color: var(--theme-background-color); }' ).
lo_buf->add( '' ).
lo_buf->add( '/* SETTINGS STYLES */' ).
lo_buf->add( '' ).
lo_buf->add( 'div.settings_container { color: var(--theme-primary-font-color); }' ).
lo_buf->add( '' ).
lo_buf->add( '/* DIFF */' ).
lo_buf->add( '' ).
lo_buf->add( '.diff_ins { background-color: #352; }' ).
lo_buf->add( '.diff_del { background-color: #411; }' ).
lo_buf->add( '.diff_upd { background-color: #551; }' ).
lo_buf->add( 'div.diff_content { background-color: var(--theme-background-color); }' ).
lo_buf->add( '' ).
lo_buf->add( '/* DIFF TABLE */' ).
lo_buf->add( '' ).
lo_buf->add( 'table.diff_tab td,th { color: #fff; }' ).
lo_buf->add( 'table.diff_tab thead.nav_line { background-color: var(--theme-container-background-color); }' ).
lo_buf->add( '' ).
lo_buf->add( '/* STYLES FOR SYNTAX HIGHLIGHTING */' ).
lo_buf->add( '' ).
lo_buf->add( '/* ABAP */' ).
lo_buf->add( '.syntax-hl span.keyword  { color: #4af; }' ).
lo_buf->add( '.syntax-hl span.text     { color: #8f8; }' ).
lo_buf->add( '.syntax-hl span.comment  { color: #999; }' ).
lo_buf->add( '/* XML+HTML */' ).
lo_buf->add( '.syntax-hl span.xml_tag  { color: #659cff; }' ).
lo_buf->add( '.syntax-hl span.attr     { color: #bab2f9; }' ).
lo_buf->add( '.syntax-hl span.attr_val { color: #b777fb; }' ).
lo_buf->add( '/* CSS+JS */' ).
lo_buf->add( '.syntax-hl span.properties   { color:#0a69ce; }' ).
lo_buf->add( '.syntax-hl span.values       { color:blue; }' ).
lo_buf->add( '.syntax-hl span.units        { color:maroon; }' ).
lo_buf->add( '.syntax-hl span.selectors    { color:purple; }' ).
lo_buf->add( '.syntax-hl span.functions    { color:purple; }' ).
lo_buf->add( '.syntax-hl span.colors       { color:purple; }' ).
lo_buf->add( '.syntax-hl span.extensions   { color:lightblue; }' ).
lo_buf->add( '.syntax-hl span.at_rules     { color:lightblue; }' ).
lo_buf->add( '.syntax-hl span.html         { color:green; }' ).
lo_buf->add( '.syntax-hl span.variables    { color:purple; }' ).
lo_buf->add( '' ).
lo_buf->add( '/* DEBUG INFO STYLES */' ).
lo_buf->add( '' ).
lo_buf->add( 'div.debug_container#debug_info { color: var(--theme-primary-font-color); }' ).
lo_buf->add( '' ).
lo_buf->add( '/* DB ENTRIES */' ).
lo_buf->add( '' ).
lo_buf->add( 'div.db_list { background-color: var(--theme-container-background-color); }' ).
lo_buf->add( 'table.db_tab td      { color: var(--theme-primary-font-color); }' ).
lo_buf->add( 'table.db_tab td.data { opacity: 0.5; }' ).
lo_buf->add( 'table.db_tab tbody tr:hover, tr:active { background-color: var(--theme-list-hover-background-color); }' ).
lo_buf->add( 'table.db_tab th {' ).
lo_buf->add( '  color: var(--theme-primary-font-color);' ).
lo_buf->add( '  border-bottom-color: #333;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'table.db_tab tr.selected {' ).
lo_buf->add( '  background: rgba(92, 92, 92, 1) !important;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/* ERROR LOGS */' ).
lo_buf->add( '' ).
lo_buf->add( 'div.log { color: var(--theme-greyscale-dark); }' ).
lo_buf->add( '.close-btn, .message-panel, .message-panel-commands a { color: var(--theme-greyscale-dark); }' ).
lo_buf->add( '.message-panel-commands a:hover { color: var(--theme-greyscale-dark); }' ).
lo_buf->add( '' ).
lo_buf->add( '/* DIALOGS */' ).
lo_buf->add( '' ).
lo_buf->add( '.dialog {' ).
lo_buf->add( '  color: var(--theme-primary-font-color-reduced);' ).
lo_buf->add( '  background-color: var(--theme-container-background-color);' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog li.dialog-commands a,' ).
lo_buf->add( '.dialog li.dialog-commands input[type="submit"] {' ).
lo_buf->add( '  border-color: #ccc;' ).
lo_buf->add( '  background-color: var(--theme-greyscale-dark);' ).
lo_buf->add( '  color: #fff;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog li.dialog-commands a.main,' ).
lo_buf->add( '.dialog li.dialog-commands input[type="submit"].main {' ).
lo_buf->add( '  background-color: #64a8ff;' ).
lo_buf->add( '  color: #fff;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog label {' ).
lo_buf->add( '  color: var(--theme-primary-font-color);' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog label em {' ).
lo_buf->add( '  color: #64a8ff;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog li.error small {' ).
lo_buf->add( '  color: #ff5959;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog li.error input[type="number"],' ).
lo_buf->add( '.dialog li.error input[type="text"] {' ).
lo_buf->add( '  border-color: #ff5959;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog .radio-container {' ).
lo_buf->add( '  border-color: #ddd;' ).
lo_buf->add( '  background-color: var(--theme-greyscale-dark);' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog .radio-container input[type="radio"] + label {' ).
lo_buf->add( '  color: #fff;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog .radio-container input[type="radio"]:checked + label {' ).
lo_buf->add( '  background-color: #64a8ff;' ).
lo_buf->add( '  color: #fff;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog li.with-command input[type="button"]:hover,' ).
lo_buf->add( '.dialog li.with-command input[type="submit"]:hover {' ).
lo_buf->add( '  background-color: #64a8ff;' ).
lo_buf->add( '  color: #fff;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog fieldset {' ).
lo_buf->add( '  border-color: #dfdfdf;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog fieldset legend {' ).
lo_buf->add( '  color: var(--theme-primary-font-color);' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog input:read-only {' ).
lo_buf->add( '  background-color: var(--theme-greyscale-dark);' ).
lo_buf->add( '  color: var(--theme-greyscale-medium);' ).
lo_buf->add( '}' ).
lo_buf->add( '/* for IE */' ).
lo_buf->add( '.dialog input[readonly] {' ).
lo_buf->add( '  background-color: var(--theme-greyscale-dark);' ).
lo_buf->add( '  color: var(--theme-greyscale-medium);' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
    li_asset_man->register_asset(
      iv_url       = 'css/theme-dark.css'
      iv_type      = 'text/css'
      iv_cachable  = abap_false
      iv_mime_name = 'ZABAPGIT_CSS_THEME_DARK'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

lo_buf->add( '/*' ).
lo_buf->add( ' * ABAPGIT THEME CSS - BELIZE BLUE' ).
lo_buf->add( ' */' ).
lo_buf->add( '' ).
lo_buf->add( '/* https://experience.sap.com/fiori-design-web/colors/ */' ).
lo_buf->add( '' ).
lo_buf->add( ':root {' ).
lo_buf->add( '  --fiori-color-global-light-base: #EFF4F9; /* Background in SAP GUI */' ).
lo_buf->add( '  --fiori-color-gui-tab-background: #FCFDFE; /* Tabstrip background */' ).
lo_buf->add( '  --fiori-color-gui-container-border: #D1E0EE;' ).
lo_buf->add( '  --fiori-color-gui-uneditable-background: #F2F2F2; /* Textbox not editable */' ).
lo_buf->add( '  --fiori-color-gui-editable-background: #FFFFFF; /* Textbox editable */' ).
lo_buf->add( '  --fiori-color-font-primary: #333333; /* Grayscale 1 */' ).
lo_buf->add( '  --fiori-color-font-secondary: #666666; /* Grayscale 2 */' ).
lo_buf->add( '  --fiori-color-font-highlighted: #003D84;' ).
lo_buf->add( '  --fiori-color-message-box-background: #2F3C48; /* Bottom message container */' ).
lo_buf->add( '' ).
lo_buf->add( '  --theme-background-color: var(--fiori-color-global-light-base);' ).
lo_buf->add( '  --theme-container-background-color: var(--fiori-color-gui-tab-background);' ).
lo_buf->add( '  --theme-primary-font: "72", Arial, Helvetica, sans-serif;' ).
lo_buf->add( '  --theme-primary-font-color: var(--fiori-color-font-primary);' ).
lo_buf->add( '  --theme-primary-font-color-reduced: var(--fiori-color-font-secondary);' ).
lo_buf->add( '  --theme-font-size: 11pt;' ).
lo_buf->add( '  --theme-link-color: var(--fiori-color-font-highlighted);' ).
lo_buf->add( '  --theme-container-border-color: var(--fiori-color-gui-container-border);' ).
lo_buf->add( '  --theme-table-border-color: #E5E5E5; /* ALV border color */' ).
lo_buf->add( '  --theme-greyscale-dark: #666666;' ).
lo_buf->add( '  --theme-greyscale-medium: #BFBFBF;' ).
lo_buf->add( '  --theme-greyscale-light: #CCCCCC;' ).
lo_buf->add( '  --theme-greyscale-lighter: #E5E5E5;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '#header a, #header a:visited {' ).
lo_buf->add( '  color: #346187;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
    li_asset_man->register_asset(
      iv_url       = 'css/theme-belize-blue.css'
      iv_type      = 'text/css'
      iv_cachable  = abap_false
      iv_mime_name = 'ZABAPGIT_CSS_THEME_BELIZE_BLUE'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

lo_buf->add( '/**********************************************************' ).
lo_buf->add( ' * abapGit JavaScript Function Library' ).
lo_buf->add( ' **********************************************************/' ).
lo_buf->add( '' ).
lo_buf->add( '/**********************************************************' ).
lo_buf->add( '  Global variables used from outside' ).
lo_buf->add( ' **********************************************************/' ).
lo_buf->add( '' ).
lo_buf->add( '/* exported setInitialFocus */' ).
lo_buf->add( '/* exported setInitialFocusWithQuerySelector */' ).
lo_buf->add( '/* exported submitFormById */' ).
lo_buf->add( '/* exported errorStub */' ).
lo_buf->add( '/* exported confirmInitialized */' ).
lo_buf->add( '/* exported perfOut */' ).
lo_buf->add( '/* exported perfLog */' ).
lo_buf->add( '/* exported perfClear */' ).
lo_buf->add( '/* exported enableArrowListNavigation */' ).
lo_buf->add( '/* exported activateLinkHints */' ).
lo_buf->add( '/* exported setKeyBindings */' ).
lo_buf->add( '/* exported preparePatch */' ).
lo_buf->add( '/* exported registerStagePatch */' ).
lo_buf->add( '/* exported toggleRepoListDetail */' ).
lo_buf->add( '/* exported onTagTypeChange */' ).
lo_buf->add( '/* exported getIndocStyleSheet */' ).
lo_buf->add( '/* exported addMarginBottom */' ).
lo_buf->add( '/* exported enumerateJumpAllFiles */' ).
lo_buf->add( '/* exported createRepoCatalogEnumerator */' ).
lo_buf->add( '/* exported enumerateUiActions */' ).
lo_buf->add( '/* exported onDiffCollapse */' ).
lo_buf->add( '/* exported restoreScrollPosition */' ).
lo_buf->add( '/* exported toggleBrowserControlWarning */' ).
lo_buf->add( '/* exported displayBrowserControlFooter */' ).
lo_buf->add( '' ).
lo_buf->add( '/**********************************************************' ).
lo_buf->add( ' * Polyfills' ).
lo_buf->add( ' **********************************************************/' ).
lo_buf->add( '' ).
lo_buf->add( '// Bind polyfill (for IE7), taken from https://developer.mozilla.org/' ).
lo_buf->add( 'if (!Function.prototype.bind) {' ).
lo_buf->add( '  Function.prototype.bind = function(oThis) {' ).
lo_buf->add( '    if (typeof this !== "function") {' ).
lo_buf->add( '      throw new TypeError("Function.prototype.bind - subject is not callable");' ).
lo_buf->add( '    }' ).
lo_buf->add( '' ).
lo_buf->add( '    var aArgs   = Array.prototype.slice.call(arguments, 1);' ).
lo_buf->add( '    var fToBind = this;' ).
lo_buf->add( '    var fNOP    = function() { };' ).
lo_buf->add( '    var fBound  = function() {' ).
lo_buf->add( '      return fToBind.apply(' ).
lo_buf->add( '        this instanceof fNOP ? this : oThis,' ).
lo_buf->add( '        aArgs.concat(Array.prototype.slice.call(arguments))' ).
lo_buf->add( '      );' ).
lo_buf->add( '    };' ).
lo_buf->add( '' ).
lo_buf->add( '    if (this.prototype) {' ).
lo_buf->add( '      fNOP.prototype = this.prototype;' ).
lo_buf->add( '    }' ).
lo_buf->add( '    fBound.prototype = new fNOP();' ).
lo_buf->add( '' ).
lo_buf->add( '    return fBound;' ).
lo_buf->add( '  };' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '// String includes polyfill, taken from https://developer.mozilla.org' ).
lo_buf->add( 'if (!String.prototype.includes) {' ).
lo_buf->add( '  String.prototype.includes = function(search, start) {' ).
lo_buf->add( '    "use strict";' ).
lo_buf->add( '    if (typeof start !== "number") {' ).
lo_buf->add( '      start = 0;' ).
lo_buf->add( '    }' ).
lo_buf->add( '' ).
lo_buf->add( '    if (start + search.length > this.length) {' ).
lo_buf->add( '      return false;' ).
lo_buf->add( '    } else {' ).
lo_buf->add( '      return this.indexOf(search, start) !== -1;' ).
lo_buf->add( '    }' ).
lo_buf->add( '  };' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '// String startsWith polyfill, taken from https://developer.mozilla.org' ).
lo_buf->add( 'if (!String.prototype.startsWith) {' ).
lo_buf->add( '  Object.defineProperty(String.prototype, "startsWith", {' ).
lo_buf->add( '    value: function(search, pos) {' ).
lo_buf->add( '      pos = !pos || pos < 0 ? 0 : +pos;' ).
lo_buf->add( '' ).
lo_buf->add( '      return this.substring(pos, pos + search.length) === search;' ).
lo_buf->add( '    }' ).
lo_buf->add( '  });' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '// forEach polyfill, taken from https://developer.mozilla.org' ).
lo_buf->add( '// used for querySelectorAll results' ).
lo_buf->add( 'if (window.NodeList && !NodeList.prototype.forEach) {' ).
lo_buf->add( '  NodeList.prototype.forEach = Array.prototype.forEach;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/**********************************************************' ).
lo_buf->add( ' * Common functions' ).
lo_buf->add( ' **********************************************************/' ).
lo_buf->add( '' ).
lo_buf->add( '// Output text to the debug div' ).
lo_buf->add( 'function debugOutput(text, dstID) {' ).
lo_buf->add( '  var stdout  = document.getElementById(dstID || "debug-output");' ).
lo_buf->add( '  var wrapped = "<p>" + text + "</p>";' ).
lo_buf->add( '' ).
lo_buf->add( '  stdout.innerHTML = stdout.innerHTML + wrapped;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '// Use a supplied form, a pre-created form or create a hidden form' ).
lo_buf->add( '// and submit with sapevent' ).
lo_buf->add( 'function submitSapeventForm(params, action, method, form) {' ).
lo_buf->add( '' ).
lo_buf->add( '  function getSapeventPrefix() {' ).
lo_buf->add( '    // Depending on the used browser control and its version, different URL schemes' ).
lo_buf->add( '    // are used which we distinguish here' ).
lo_buf->add( '    if (document.querySelector(''a[href*="file:///SAPEVENT:"]'')) {' ).
lo_buf->add( '      // Prefix for old (SAPGUI <= 8.00 PL3) chromium based browser control' ).
lo_buf->add( '      return "file:///";' ).
lo_buf->add( '    } else if (document.querySelector(''a[href^="sap-cust"]'')) {' ).
lo_buf->add( '      // Prefix for new (SAPGUI >= 8.00 PL3 Hotfix 1) chromium based browser control' ).
lo_buf->add( '      return "sap-cust://sap-place-holder/";' ).
lo_buf->add( '    } else {' ).
lo_buf->add( '      return ""; // No prefix for old IE control' ).
lo_buf->add( '    }' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  var stub_form_id = "form_" + action;' ).
lo_buf->add( '' ).
lo_buf->add( '  form = form' ).
lo_buf->add( '    || document.getElementById(stub_form_id)' ).
lo_buf->add( '    || document.createElement("form");' ).
lo_buf->add( '' ).
lo_buf->add( '  form.setAttribute("method", method || "post");' ).
lo_buf->add( '  if (/sapevent/i.test(action)) {' ).
lo_buf->add( '    form.setAttribute("action", action);' ).
lo_buf->add( '  } else {' ).
lo_buf->add( '    form.setAttribute("action", getSapeventPrefix() + "SAPEVENT:" + action);' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  for (var key in params) {' ).
lo_buf->add( '    var hiddenField = document.createElement("input");' ).
lo_buf->add( '    hiddenField.setAttribute("type", "hidden");' ).
lo_buf->add( '    hiddenField.setAttribute("name", key);' ).
lo_buf->add( '    hiddenField.setAttribute("value", params[key]);' ).
lo_buf->add( '    form.appendChild(hiddenField);' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  var formExistsInDOM = form.id && Boolean(document.querySelector("#" + form.id));' ).
lo_buf->add( '' ).
lo_buf->add( '  if (form.id !== stub_form_id && !formExistsInDOM) {' ).
lo_buf->add( '    document.body.appendChild(form);' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  form.submit();' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '// Set focus to a control' ).
lo_buf->add( 'function setInitialFocus(id) {' ).
lo_buf->add( '  document.getElementById(id).focus();' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '// Set focus to a element with query selector' ).
lo_buf->add( 'function setInitialFocusWithQuerySelector(sSelector, bFocusParent) {' ).
lo_buf->add( '  var oSelected = document.querySelector(sSelector);' ).
lo_buf->add( '' ).
lo_buf->add( '  if (oSelected) {' ).
lo_buf->add( '    if (bFocusParent) {' ).
lo_buf->add( '      oSelected.parentElement.focus();' ).
lo_buf->add( '    } else {' ).
lo_buf->add( '      oSelected.focus();' ).
lo_buf->add( '    }' ).
lo_buf->add( '  }' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '// Submit an existing form' ).
lo_buf->add( 'function submitFormById(id) {' ).
lo_buf->add( '  document.getElementById(id).submit();' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '// JS error stub' ).
lo_buf->add( 'function errorStub(event) {' ).
lo_buf->add( '  var element    = event.target || event.srcElement;' ).
lo_buf->add( '  var targetName = element.id || element.name || "???";' ).
lo_buf->add( '  alert("JS Error, please log an issue (@" + targetName + ")");' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '// Confirm JS initilization' ).
lo_buf->add( 'function confirmInitialized() {' ).
lo_buf->add( '  var errorBanner = document.getElementById("js-error-banner");' ).
lo_buf->add( '  if (errorBanner) {' ).
lo_buf->add( '    errorBanner.style.display = "none";' ).
lo_buf->add( '  }' ).
lo_buf->add( '  debugOutput("js: OK"); // Final final confirmation :)' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/**********************************************************' ).
lo_buf->add( ' * Performance utils (for debugging)' ).
lo_buf->add( ' **********************************************************/' ).
lo_buf->add( '' ).
lo_buf->add( 'var gPerf = [];' ).
lo_buf->add( '' ).
lo_buf->add( 'function perfOut(prefix) {' ).
lo_buf->add( '  var totals = {};' ).
lo_buf->add( '  for (var i = gPerf.length - 1; i >= 0; i--) {' ).
lo_buf->add( '    if (!totals[gPerf[i].name]) totals[gPerf[i].name] = { count: 0, time: 0 };' ).
lo_buf->add( '' ).
lo_buf->add( '    totals[gPerf[i].name].time  += gPerf[i].time;' ).
lo_buf->add( '    totals[gPerf[i].name].count += 1;' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  var keys = Object.keys(totals);' ).
lo_buf->add( '  for (var j = keys.length - 1; j >= 0; j--) {' ).
lo_buf->add( '    console.log(prefix' ).
lo_buf->add( '      + " " + keys[j] + ": "' ).
lo_buf->add( '      + totals[keys[j]].time.toFixed(3) + "ms"' ).
lo_buf->add( '      + " (" + totals[keys[j]].count.toFixed() + ")");' ).
lo_buf->add( '  }' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'function perfLog(name, startTime) {' ).
lo_buf->add( '  gPerf.push({ name: name, time: window.performance.now() - startTime });' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'function perfClear() {' ).
lo_buf->add( '  gPerf = [];' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/**********************************************************' ).
lo_buf->add( ' * Repo Overview Logic' ).
lo_buf->add( ' **********************************************************/' ).
lo_buf->add( '' ).
lo_buf->add( 'function findStyleSheetByName(name) {' ).
lo_buf->add( '  for (var s = 0; s < document.styleSheets.length; s++) {' ).
lo_buf->add( '    var styleSheet = document.styleSheets[s];' ).
lo_buf->add( '    var classes    = styleSheet.cssRules || styleSheet.rules;' ).
lo_buf->add( '    for (var i = 0; i < classes.length; i++) {' ).
lo_buf->add( '      if (classes[i].selectorText === name) return classes[i];' ).
lo_buf->add( '    }' ).
lo_buf->add( '  }' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'function getIndocStyleSheet() {' ).
lo_buf->add( '  for (var s = 0; s < document.styleSheets.length; s++) {' ).
lo_buf->add( '    if (!document.styleSheets[s].href) return document.styleSheets[s]; // One with empty href' ).
lo_buf->add( '  }' ).
lo_buf->add( '  // None found ? create one' ).
lo_buf->add( '  var style = document.createElement("style");' ).
lo_buf->add( '  document.head.appendChild(style);' ).
lo_buf->add( '  return style.sheet;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'function RepoOverViewHelper(opts) {' ).
lo_buf->add( '  if (opts && opts.focusFilterKey) {' ).
lo_buf->add( '    this.focusFilterKey = opts.focusFilterKey;' ).
lo_buf->add( '  }' ).
lo_buf->add( '  this.setHooks();' ).
lo_buf->add( '  this.pageId                   = "RepoOverViewHelperState"; // constant is OK for this case' ).
lo_buf->add( '  this.isDetailsDisplayed       = false;' ).
lo_buf->add( '  this.isOnlyFavoritesDisplayed = false;' ).
lo_buf->add( '  this.detailCssClass           = findStyleSheetByName(".repo-overview .ro-detail");' ).
lo_buf->add( '' ).
lo_buf->add( '  var icon = document.getElementById("icon-filter-detail");' ).
lo_buf->add( '  this.toggleFilterIcon(icon, this.isDetailsDisplayed);' ).
lo_buf->add( '  this.registerRowSelection();' ).
lo_buf->add( '  this.registerKeyboardShortcuts();' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'RepoOverViewHelper.prototype.setHooks = function() {' ).
lo_buf->add( '  window.onload = this.onPageLoad.bind(this);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'RepoOverViewHelper.prototype.onPageLoad = function() {' ).
lo_buf->add( '  var data = window.localStorage && JSON.parse(window.localStorage.getItem(this.pageId));' ).
lo_buf->add( '  if (data) {' ).
lo_buf->add( '    if (data.isDetailsDisplayed) {' ).
lo_buf->add( '      this.toggleItemsDetail(true);' ).
lo_buf->add( '    }' ).
lo_buf->add( '    if (data.selectedRepoKey) {' ).
lo_buf->add( '      this.selectRowByRepoKey(data.selectedRepoKey);' ).
lo_buf->add( '    } else {' ).
lo_buf->add( '      this.selectRowByIndex(0);' ).
lo_buf->add( '    }' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'RepoOverViewHelper.prototype.registerKeyboardShortcuts = function() {' ).
lo_buf->add( '  var self = this;' ).
lo_buf->add( '  document.addEventListener("keypress", function(event) {' ).
lo_buf->add( '    if (document.activeElement.id === "filter") {' ).
lo_buf->add( '      return;' ).
lo_buf->add( '    }' ).
lo_buf->add( '    if (self.focusFilterKey && event.key === self.focusFilterKey && !CommandPalette.isVisible()) {' ).
lo_buf->add( '      var filterInput = document.getElementById("filter");' ).
lo_buf->add( '      if (filterInput) filterInput.focus();' ).
lo_buf->add( '      event.preventDefault();' ).
lo_buf->add( '      return;' ).
lo_buf->add( '    }' ).
lo_buf->add( '' ).
lo_buf->add( '    var keycode         = event.keyCode;' ).
lo_buf->add( '    var rows            = Array.prototype.slice.call(self.getVisibleRows());' ).
lo_buf->add( '    var selected        = document.querySelector(".repo-overview tr.selected");' ).
lo_buf->add( '    var indexOfSelected = rows.indexOf(selected);' ).
lo_buf->add( '    var lastRow         = rows.length - 1;' ).
lo_buf->add( '' ).
lo_buf->add( '    if (keycode == 13 && document.activeElement.tagName.toLowerCase() != "input") {' ).
lo_buf->add( '      // "enter" to open, unless command field has focus' ).
lo_buf->add( '      self.openSelectedRepo();' ).
lo_buf->add( '    } else if ((keycode == 52 || keycode == 56) && indexOfSelected > 0) {' ).
lo_buf->add( '      // "4,8" for previous, digits are the numlock keys' ).
lo_buf->add( '      // NB: numpad must be activated, keypress does not detect arrows' ).
lo_buf->add( '      //     if we need arrows it will be keydown. But then mind the keycodes, they may change !' ).
lo_buf->add( '      //     e.g. 100 is ''d'' with keypress (and conflicts with diff hotkey), and also it is arrow-left keydown' ).
lo_buf->add( '      self.selectRowByIndex(indexOfSelected - 1);' ).
lo_buf->add( '    } else if ((keycode == 54 || keycode == 50) && indexOfSelected < lastRow) {' ).
lo_buf->add( '      // "6,2" for next' ).
lo_buf->add( '      self.selectRowByIndex(indexOfSelected + 1);' ).
lo_buf->add( '    }' ).
lo_buf->add( '  });' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'RepoOverViewHelper.prototype.openSelectedRepo = function() {' ).
lo_buf->add( '  this.selectedRepoKey = document.querySelector(".repo-overview tr.selected").dataset.key;' ).
lo_buf->add( '  this.saveLocalStorage();' ).
lo_buf->add( '  document.querySelector(".repo-overview tr.selected td.ro-go a").click();' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'RepoOverViewHelper.prototype.selectRowByIndex = function(index) {' ).
lo_buf->add( '  var rows = this.getVisibleRows();' ).
lo_buf->add( '  if (rows.length >= index) {' ).
lo_buf->add( '    var selectedRow = rows[index];' ).
lo_buf->add( '    if (selectedRow.classList.contains("selected")) {' ).
lo_buf->add( '      return;' ).
lo_buf->add( '    }' ).
lo_buf->add( '' ).
lo_buf->add( '    this.deselectAllRows();' ).
lo_buf->add( '    rows[index].classList.add("selected");' ).
lo_buf->add( '    this.selectedRepoKey = selectedRow.dataset.key;' ).
lo_buf->add( '    this.updateActionLinks(selectedRow);' ).
lo_buf->add( '    this.saveLocalStorage();' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'RepoOverViewHelper.prototype.selectRowByRepoKey = function(key) {' ).
lo_buf->add( '  var attributeQuery = "[data-key=''" + key + "'']";' ).
lo_buf->add( '  var row            = document.querySelector(".repo-overview tbody tr" + attributeQuery);' ).
lo_buf->add( '  // navigation to already selected repo' ).
lo_buf->add( '  if (row.dataset.key === key && row.classList.contains("selected")) {' ).
lo_buf->add( '    return;' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  this.deselectAllRows();' ).
lo_buf->add( '  row.classList.add("selected");' ).
lo_buf->add( '  this.selectedRepoKey = key;' ).
lo_buf->add( '  this.updateActionLinks(row);' ).
lo_buf->add( '  this.saveLocalStorage();' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'RepoOverViewHelper.prototype.updateActionLinks = function(selectedRow) {' ).
lo_buf->add( '  // now we have a repo selected, determine which action buttons are relevant' ).
lo_buf->add( '  var selectedRepoKey       = selectedRow.dataset.key;' ).
lo_buf->add( '  var selectedRepoIsOffline = selectedRow.dataset.offline === "X";' ).
lo_buf->add( '' ).
lo_buf->add( '  var actionLinks = document.querySelectorAll("a.action_link");' ).
lo_buf->add( '  actionLinks.forEach(function(link) {' ).
lo_buf->add( '    // adjust repo key in urls' ).
lo_buf->add( '    link.href = link.href.replace(/\?key=(#|\d+)/, "?key=" + selectedRepoKey);' ).
lo_buf->add( '' ).
lo_buf->add( '    // toggle button visibility' ).
lo_buf->add( '    if (link.classList.contains("action_offline_repo")) {' ).
lo_buf->add( '      if (selectedRepoIsOffline) {' ).
lo_buf->add( '        link.parentElement.classList.add("enabled");' ).
lo_buf->add( '      } else {' ).
lo_buf->add( '        link.parentElement.classList.remove("enabled");' ).
lo_buf->add( '      }' ).
lo_buf->add( '    }' ).
lo_buf->add( '    else if (link.classList.contains("action_online_repo")) {' ).
lo_buf->add( '      if (!selectedRepoIsOffline) {' ).
lo_buf->add( '        link.parentElement.classList.add("enabled");' ).
lo_buf->add( '      } else {' ).
lo_buf->add( '        link.parentElement.classList.remove("enabled");' ).
lo_buf->add( '      }' ).
lo_buf->add( '    }' ).
lo_buf->add( '    else {' ).
lo_buf->add( '      // if the action is for both repository types, it will only have the .action_link class' ).
lo_buf->add( '      // it still needs to be toggled as we want to hide everything if no repo is selected' ).
lo_buf->add( '      link.parentElement.classList.add("enabled");' ).
lo_buf->add( '    }' ).
lo_buf->add( '  });' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'RepoOverViewHelper.prototype.deselectAllRows = function() {' ).
lo_buf->add( '  document.querySelectorAll(".repo-overview tbody tr").forEach(function(x) {' ).
lo_buf->add( '    x.classList.remove("selected");' ).
lo_buf->add( '  });' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'RepoOverViewHelper.prototype.getVisibleRows = function() {' ).
lo_buf->add( '  return document.querySelectorAll(".repo-overview tbody tr:not(.nodisplay)");' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'RepoOverViewHelper.prototype.registerRowSelection = function() {' ).
lo_buf->add( '  var self = this;' ).
lo_buf->add( '  document.querySelectorAll(".repo-overview tr td:not(.ro-go)").forEach(function(repoListRowCell) {' ).
lo_buf->add( '    repoListRowCell.addEventListener("click", function() {' ).
lo_buf->add( '      self.selectRowByRepoKey(this.parentElement.dataset.key);' ).
lo_buf->add( '    });' ).
lo_buf->add( '  });' ).
lo_buf->add( '' ).
lo_buf->add( '  document.querySelectorAll(".repo-overview tr td.ro-go").forEach(function(openRepoIcon) {' ).
lo_buf->add( '    openRepoIcon.addEventListener("click", function() {' ).
lo_buf->add( '      var selectedRow = this.parentElement;' ).
lo_buf->add( '      self.selectRowByRepoKey(selectedRow.dataset.key);' ).
lo_buf->add( '      self.openSelectedRepo();' ).
lo_buf->add( '    });' ).
lo_buf->add( '  });' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'RepoOverViewHelper.prototype.toggleRepoListDetail = function(forceDisplay) {' ).
lo_buf->add( '  if (this.detailCssClass) {' ).
lo_buf->add( '    this.toggleItemsDetail(forceDisplay);' ).
lo_buf->add( '    this.saveLocalStorage();' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'RepoOverViewHelper.prototype.toggleItemsDetail = function(forceDisplay) {' ).
lo_buf->add( '  if (this.detailCssClass) {' ).
lo_buf->add( '    this.isDetailsDisplayed = forceDisplay || !this.isDetailsDisplayed;' ).
lo_buf->add( '' ).
lo_buf->add( '    // change layout to wide if details are displayed' ).
lo_buf->add( '    if (this.isDetailsDisplayed) {' ).
lo_buf->add( '      document.body.classList.remove("centered");' ).
lo_buf->add( '      document.body.classList.add("full_width");' ).
lo_buf->add( '    } else {' ).
lo_buf->add( '      document.body.classList.add("centered");' ).
lo_buf->add( '      document.body.classList.remove("full_width");' ).
lo_buf->add( '    }' ).
lo_buf->add( '' ).
lo_buf->add( '    this.detailCssClass.style.display = this.isDetailsDisplayed ? "" : "none";' ).
lo_buf->add( '' ).
lo_buf->add( '    var icon = document.getElementById("icon-filter-detail");' ).
lo_buf->add( '    this.toggleFilterIcon(icon, this.isDetailsDisplayed);' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'RepoOverViewHelper.prototype.toggleFilterIcon = function(icon, isEnabled) {' ).
lo_buf->add( '  if (isEnabled) {' ).
lo_buf->add( '    icon.classList.remove("grey");' ).
lo_buf->add( '    icon.classList.add("blue");' ).
lo_buf->add( '  } else {' ).
lo_buf->add( '    icon.classList.remove("blue");' ).
lo_buf->add( '    icon.classList.add("grey");' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'RepoOverViewHelper.prototype.saveLocalStorage = function() {' ).
lo_buf->add( '  if (!window.localStorage) return;' ).
lo_buf->add( '  var data = {' ).
lo_buf->add( '    isDetailsDisplayed      : this.isDetailsDisplayed,' ).
lo_buf->add( '    isOnlyFavoritesDisplayed: this.isOnlyFavoritesDisplayed,' ).
lo_buf->add( '    selectedRepoKey         : this.selectedRepoKey,' ).
lo_buf->add( '  };' ).
lo_buf->add( '  window.localStorage.setItem(this.pageId, JSON.stringify(data));' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '/**********************************************************' ).
lo_buf->add( ' * Staging Logic' ).
lo_buf->add( ' **********************************************************/' ).
lo_buf->add( '' ).
lo_buf->add( '// Stage helper constructor' ).
lo_buf->add( 'function StageHelper(params) {' ).
lo_buf->add( '  this.pageSeed        = params.seed;' ).
lo_buf->add( '  this.formAction      = params.formAction;' ).
lo_buf->add( '  this.patchAction     = params.patchAction;' ).
lo_buf->add( '  this.user            = params.user;' ).
lo_buf->add( '  this.ids             = params.ids;' ).
lo_buf->add( '  this.selectedCount   = 0;' ).
lo_buf->add( '  this.filteredCount   = 0;' ).
lo_buf->add( '  this.lastFilterValue = "";' ).
lo_buf->add( '  this.focusFilterKey  = params.focusFilterKey;' ).
lo_buf->add( '' ).
lo_buf->add( '  // DOM nodes' ).
lo_buf->add( '  this.dom = {' ).
lo_buf->add( '    stageTab         : document.getElementById(params.ids.stageTab),' ).
lo_buf->add( '    commitAllBtn     : document.getElementById(params.ids.commitAllBtn),' ).
lo_buf->add( '    commitSelectedBtn: document.getElementById(params.ids.commitSelectedBtn),' ).
lo_buf->add( '    commitFilteredBtn: document.getElementById(params.ids.commitFilteredBtn),' ).
lo_buf->add( '    patchBtn         : document.getElementById(params.ids.patchBtn),' ).
lo_buf->add( '    objectSearch     : document.getElementById(params.ids.objectSearch),' ).
lo_buf->add( '    selectedCounter  : null,' ).
lo_buf->add( '    filteredCounter  : null,' ).
lo_buf->add( '  };' ).
lo_buf->add( '  this.findCounters();' ).
lo_buf->add( '' ).
lo_buf->add( '  // Table columns (autodetection)' ).
lo_buf->add( '  this.colIndex      = this.detectColumns();' ).
lo_buf->add( '  this.filterTargets = ["name", "user", "transport"];' ).
lo_buf->add( '' ).
lo_buf->add( '  // Constants' ).
lo_buf->add( '  this.HIGHLIGHT_STYLE = "highlight";' ).
lo_buf->add( '  this.STATUS          = {' ).
lo_buf->add( '    add    : "A",' ).
lo_buf->add( '    remove : "R",' ).
lo_buf->add( '    ignore : "I",' ).
lo_buf->add( '    reset  : "?",' ).
lo_buf->add( '    isValid: function(status) { return "ARI?".indexOf(status) == -1 }' ).
lo_buf->add( '  };' ).
lo_buf->add( '' ).
lo_buf->add( '  this.TEMPLATES = {' ).
lo_buf->add( '    cmdReset : "<a>reset</a>",' ).
lo_buf->add( '    cmdLocal : "<a>add</a>",' ).
lo_buf->add( '    cmdRemote: "<a>ignore</a><a>remove</a>"' ).
lo_buf->add( '  };' ).
lo_buf->add( '' ).
lo_buf->add( '  this.setHooks();' ).
lo_buf->add( '  if (this.user) this.injectFilterMe();' ).
lo_buf->add( '  Hotkeys.addHotkeyToHelpSheet("^Enter", "Commit");' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'StageHelper.prototype.findCounters = function() {' ).
lo_buf->add( '  this.dom.selectedCounter = this.dom.commitSelectedBtn.querySelector("span.counter");' ).
lo_buf->add( '  this.dom.filteredCounter = this.dom.commitFilteredBtn.querySelector("span.counter");' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'StageHelper.prototype.injectFilterMe = function() {' ).
lo_buf->add( '  var tabFirstHead = this.dom.stageTab.tHead.rows[0];' ).
lo_buf->add( '  if (!tabFirstHead || tabFirstHead.className !== "local") {' ).
lo_buf->add( '    return; // for the case only "remove part" is displayed' ).
lo_buf->add( '  }' ).
lo_buf->add( '  var changedByHead = tabFirstHead.cells[this.colIndex.user];' ).
lo_buf->add( '' ).
lo_buf->add( '  changedByHead.innerText = changedByHead.innerText + " (";' ).
lo_buf->add( '' ).
lo_buf->add( '  var a = document.createElement("A");' ).
lo_buf->add( '  a.appendChild(document.createTextNode("me"));' ).
lo_buf->add( '  a.onclick = this.onFilterMe.bind(this);' ).
lo_buf->add( '  a.href    = "#";' ).
lo_buf->add( '  changedByHead.appendChild(a);' ).
lo_buf->add( '  changedByHead.appendChild(document.createTextNode(")"));' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'StageHelper.prototype.onFilterMe = function() {' ).
lo_buf->add( '  this.dom.objectSearch.value = this.user;' ).
lo_buf->add( '  this.onFilter({ type: "keypress", which: 13, target: this.dom.objectSearch });' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '// Hook global click listener on table, load/unload actions' ).
lo_buf->add( 'StageHelper.prototype.setHooks = function() {' ).
lo_buf->add( '  window.onkeypress                  = this.onCtrlEnter.bind(this);' ).
lo_buf->add( '  this.dom.stageTab.onclick          = this.onTableClick.bind(this);' ).
lo_buf->add( '  this.dom.commitSelectedBtn.onclick = this.submit.bind(this);' ).
lo_buf->add( '  this.dom.commitFilteredBtn.onclick = this.submitVisible.bind(this);' ).
lo_buf->add( '  this.dom.patchBtn.onclick          = this.submitPatch.bind(this);' ).
lo_buf->add( '  this.dom.objectSearch.oninput      = this.onFilter.bind(this);' ).
lo_buf->add( '  this.dom.objectSearch.onkeypress   = this.onFilter.bind(this);' ).
lo_buf->add( '  window.onbeforeunload              = this.onPageUnload.bind(this);' ).
lo_buf->add( '  window.onload                      = this.onPageLoad.bind(this);' ).
lo_buf->add( '' ).
lo_buf->add( '  var self = this;' ).
lo_buf->add( '  document.addEventListener("keypress", function(event) {' ).
lo_buf->add( '    if (document.activeElement.id !== self.ids.objectSearch' ).
lo_buf->add( '      && self.focusFilterKey && event.key === self.focusFilterKey' ).
lo_buf->add( '      && !CommandPalette.isVisible()) {' ).
lo_buf->add( '' ).
lo_buf->add( '      self.dom.objectSearch.focus();' ).
lo_buf->add( '      event.preventDefault();' ).
lo_buf->add( '    }' ).
lo_buf->add( '  });' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '// Detect column index' ).
lo_buf->add( 'StageHelper.prototype.detectColumns = function() {' ).
lo_buf->add( '  var dataRow  = this.dom.stageTab.tBodies[0].rows[0];' ).
lo_buf->add( '  var colIndex = {};' ).
lo_buf->add( '' ).
lo_buf->add( '  for (var i = dataRow.cells.length - 1; i >= 0; i--) {' ).
lo_buf->add( '    if (dataRow.cells[i].className) colIndex[dataRow.cells[i].className] = i;' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  return colIndex;' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '// Store table state on leaving the page' ).
lo_buf->add( 'StageHelper.prototype.onPageUnload = function() {' ).
lo_buf->add( '  if (!window.sessionStorage) return;' ).
lo_buf->add( '' ).
lo_buf->add( '  var data = this.collectData();' ).
lo_buf->add( '  window.sessionStorage.setItem(this.pageSeed, JSON.stringify(data));' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '// Re-store table state on entering the page' ).
lo_buf->add( 'StageHelper.prototype.onPageLoad = function() {' ).
lo_buf->add( '  var data = window.sessionStorage && JSON.parse(window.sessionStorage.getItem(this.pageSeed));' ).
lo_buf->add( '' ).
lo_buf->add( '  this.iterateStageTab(true, function(row) {' ).
lo_buf->add( '    var status = data && data[row.cells[this.colIndex["name"]].innerText];' ).
lo_buf->add( '    this.updateRow(row, status || this.STATUS.reset);' ).
lo_buf->add( '  });' ).
lo_buf->add( '' ).
lo_buf->add( '  this.updateMenu();' ).
lo_buf->add( '  if (this.dom.objectSearch.value) {' ).
lo_buf->add( '    this.applyFilterValue(this.dom.objectSearch.value);' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '// Table event handler, change status' ).
lo_buf->add( 'StageHelper.prototype.onTableClick = function(event) {' ).
lo_buf->add( '  var target = event.target || event.srcElement;' ).
lo_buf->add( '  if (!target) return;' ).
lo_buf->add( '' ).
lo_buf->add( '  var td;' ).
lo_buf->add( '  if (target.tagName === "A") {' ).
lo_buf->add( '    td = target.parentNode;' ).
lo_buf->add( '  } else if (target.tagName === "TD") {' ).
lo_buf->add( '    td = target;' ).
lo_buf->add( '    if (td.children.length === 1 && td.children[0].tagName === "A") {' ).
lo_buf->add( '      target = td.children[0];' ).
lo_buf->add( '    } else return;' ).
lo_buf->add( '  } else return;' ).
lo_buf->add( '' ).
lo_buf->add( '  if (["TD", "TH"].indexOf(td.tagName) == -1 || td.className != "cmd") return;' ).
lo_buf->add( '' ).
lo_buf->add( '  var status    = this.STATUS[target.innerText]; // Convert anchor text to status' ).
lo_buf->add( '  var targetRow = td.parentNode;' ).
lo_buf->add( '' ).
lo_buf->add( '  if (td.tagName === "TD") {' ).
lo_buf->add( '    this.updateRow(targetRow, status);' ).
lo_buf->add( '  } else { // TH' ).
lo_buf->add( '    this.iterateStageTab(true, function(row) {' ).
lo_buf->add( '      if (row.style.display !== "none"           // Not filtered out' ).
lo_buf->add( '        && row.className === targetRow.className // Same context as header' ).
lo_buf->add( '      ) {' ).
lo_buf->add( '        this.updateRow(row, status);' ).
lo_buf->add( '      }' ).
lo_buf->add( '    });' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  this.updateMenu();' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'StageHelper.prototype.onCtrlEnter = function(e) {' ).
lo_buf->add( '  if (e.ctrlKey && (e.which === 10 || e.key === "Enter")) {' ).
lo_buf->add( '    var clickMap = {' ).
lo_buf->add( '      "default" : this.dom.commitAllBtn,' ).
lo_buf->add( '      "selected": this.dom.commitSelectedBtn,' ).
lo_buf->add( '      "filtered": this.dom.commitFilteredBtn' ).
lo_buf->add( '    };' ).
lo_buf->add( '    clickMap[this.calculateActiveCommitCommand()].click();' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '// Search object' ).
lo_buf->add( 'StageHelper.prototype.onFilter = function(e) {' ).
lo_buf->add( '  if ( // Enter hit or clear, IE SUCKS !' ).
lo_buf->add( '    e.type === "input" && !e.target.value && this.lastFilterValue' ).
lo_buf->add( '    || e.type === "keypress" && (e.which === 13 || e.key === "Enter") && !e.ctrlKey) {' ).
lo_buf->add( '' ).
lo_buf->add( '    this.applyFilterValue(e.target.value);' ).
lo_buf->add( '    submitSapeventForm({ filterValue: e.target.value }, "stage_filter", "post");' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'StageHelper.prototype.applyFilterValue = function(sFilterValue) {' ).
lo_buf->add( '  this.lastFilterValue = sFilterValue;' ).
lo_buf->add( '  this.filteredCount   = this.iterateStageTab(true, this.applyFilterToRow, sFilterValue);' ).
lo_buf->add( '  this.updateMenu();' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '// Apply filter to a single stage line - hide or show' ).
lo_buf->add( 'StageHelper.prototype.applyFilterToRow = function(row, filter) {' ).
lo_buf->add( '  // Collect data cells' ).
lo_buf->add( '  var targets = this.filterTargets.map(function(attr) {' ).
lo_buf->add( '    // Get the innermost tag with the text we want to filter' ).
lo_buf->add( '    // <td>text</td>: elem = td-tag' ).
lo_buf->add( '    // <td><span><i></i><a>text</a></span></td>: elem = a-tag' ).
lo_buf->add( '    var elem  = row.cells[this.colIndex[attr]];' ).
lo_buf->add( '    var elemA = elem.getElementsByTagName("A")[0];' ).
lo_buf->add( '' ).
lo_buf->add( '    if (elemA) elem = elemA;' ).
lo_buf->add( '    return {' ).
lo_buf->add( '      elem     : elem,' ).
lo_buf->add( '      plainText: elem.innerText.replace(/ /g, "\u00a0"), // without tags, with encoded spaces' ).
lo_buf->add( '      curHtml  : elem.innerHTML' ).
lo_buf->add( '    };' ).
lo_buf->add( '  }, this);' ).
lo_buf->add( '' ).
lo_buf->add( '  var isVisible = false;' ).
lo_buf->add( '' ).
lo_buf->add( '  // Apply filter to cells, mark filtered text' ).
lo_buf->add( '  for (var i = targets.length - 1; i >= 0; i--) {' ).
lo_buf->add( '    var target = targets[i];' ).
lo_buf->add( '    // Ignore case of filter' ).
lo_buf->add( '    var regFilter = new RegExp("(" + filter + ")", "gi");' ).
lo_buf->add( '' ).
lo_buf->add( '    target.newHtml = (filter)' ).
lo_buf->add( '      ? target.plainText.replace(regFilter, "<mark>$1</mark>")' ).
lo_buf->add( '      : target.plainText;' ).
lo_buf->add( '    target.isChanged = target.newHtml !== target.curHtml;' ).
lo_buf->add( '    isVisible        = isVisible || !filter || target.newHtml !== target.plainText;' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  // Update DOM' ).
lo_buf->add( '  row.style.display = isVisible ? "" : "none";' ).
lo_buf->add( '  for (var j = targets.length - 1; j >= 0; j--) {' ).
lo_buf->add( '    if (targets[j].isChanged) targets[j].elem.innerHTML = targets[j].newHtml;' ).
lo_buf->add( '  }' ).
lo_buf->add( '  return isVisible ? 1 : 0;' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '// Get how status should affect object counter' ).
lo_buf->add( 'StageHelper.prototype.getStatusImpact = function(status) {' ).
lo_buf->add( '  if (typeof status !== "string"' ).
lo_buf->add( '    || status.length !== 1' ).
lo_buf->add( '    || this.STATUS.isValid(status)) {' ).
lo_buf->add( '    alert("Unknown status");' ).
lo_buf->add( '  } else {' ).
lo_buf->add( '    return (status !== this.STATUS.reset) ? 1: 0;' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '// Update table line' ).
lo_buf->add( 'StageHelper.prototype.updateRow = function(row, newStatus) {' ).
lo_buf->add( '  var oldStatus = row.cells[this.colIndex["status"]].innerText;' ).
lo_buf->add( '' ).
lo_buf->add( '  if (oldStatus !== newStatus) {' ).
lo_buf->add( '    this.updateRowStatus(row, newStatus);' ).
lo_buf->add( '    this.updateRowCommand(row, newStatus);' ).
lo_buf->add( '  } else if (!row.cells[this.colIndex["cmd"]].children.length) {' ).
lo_buf->add( '    this.updateRowCommand(row, newStatus); // For initial run' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  this.selectedCount += this.getStatusImpact(newStatus) - this.getStatusImpact(oldStatus);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '// Update Status cell (render set of commands)' ).
lo_buf->add( 'StageHelper.prototype.updateRowStatus = function(row, status) {' ).
lo_buf->add( '  row.cells[this.colIndex["status"]].innerText = status;' ).
lo_buf->add( '  if (status === this.STATUS.reset) {' ).
lo_buf->add( '    row.cells[this.colIndex["status"]].classList.remove(this.HIGHLIGHT_STYLE);' ).
lo_buf->add( '  } else {' ).
lo_buf->add( '    row.cells[this.colIndex["status"]].classList.add(this.HIGHLIGHT_STYLE);' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '// Update Command cell (render set of commands)' ).
lo_buf->add( 'StageHelper.prototype.updateRowCommand = function(row, status) {' ).
lo_buf->add( '  var cell = row.cells[this.colIndex["cmd"]];' ).
lo_buf->add( '  if (status === this.STATUS.reset) {' ).
lo_buf->add( '    cell.innerHTML = (row.className == "local")' ).
lo_buf->add( '      ? this.TEMPLATES.cmdLocal' ).
lo_buf->add( '      :     this.TEMPLATES.cmdRemote;' ).
lo_buf->add( '  } else {' ).
lo_buf->add( '    cell.innerHTML = this.TEMPLATES.cmdReset;' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'StageHelper.prototype.calculateActiveCommitCommand = function() {' ).
lo_buf->add( '  var active;' ).
lo_buf->add( '  if (this.selectedCount > 0) {' ).
lo_buf->add( '    active = "selected";' ).
lo_buf->add( '  } else if (this.lastFilterValue) {' ).
lo_buf->add( '    active = "filtered";' ).
lo_buf->add( '  } else {' ).
lo_buf->add( '    active = "default";' ).
lo_buf->add( '  }' ).
lo_buf->add( '  return active;' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '// Update menu items visibility' ).
lo_buf->add( 'StageHelper.prototype.updateMenu = function() {' ).
lo_buf->add( '  var display = this.calculateActiveCommitCommand();' ).
lo_buf->add( '' ).
lo_buf->add( '  if (display === "selected") this.dom.selectedCounter.innerText = this.selectedCount.toString();' ).
lo_buf->add( '  if (display === "filtered") this.dom.filteredCounter.innerText = this.filteredCount.toString();' ).
lo_buf->add( '' ).
lo_buf->add( '  this.dom.commitAllBtn.style.display      = display === "default" ? "" : "none";' ).
lo_buf->add( '  this.dom.commitSelectedBtn.style.display = display === "selected" ? "" : "none";' ).
lo_buf->add( '  this.dom.commitFilteredBtn.style.display = display === "filtered" ? "" : "none";' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '// Submit stage state to the server' ).
lo_buf->add( 'StageHelper.prototype.submit = function() {' ).
lo_buf->add( '  submitSapeventForm(this.collectData(), this.formAction);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'StageHelper.prototype.submitVisible = function() {' ).
lo_buf->add( '  this.markVisiblesAsAdded();' ).
lo_buf->add( '  submitSapeventForm(this.collectData(), this.formAction);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'StageHelper.prototype.submitPatch = function() {' ).
lo_buf->add( '  submitSapeventForm(this.collectData(), this.patchAction);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '// Extract data from the table' ).
lo_buf->add( 'StageHelper.prototype.collectData = function() {' ).
lo_buf->add( '  var data = {};' ).
lo_buf->add( '  this.iterateStageTab(false, function(row) {' ).
lo_buf->add( '    data[row.cells[this.colIndex["name"]].innerText] = row.cells[this.colIndex["status"]].innerText;' ).
lo_buf->add( '  });' ).
lo_buf->add( '  return data;' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'StageHelper.prototype.markVisiblesAsAdded = function() {' ).
lo_buf->add( '  this.iterateStageTab(false, function(row) {' ).
lo_buf->add( '    // TODO refacotr, unify updateRow logic' ).
lo_buf->add( '    if (row.style.display === "" && row.className === "local") { // visible' ).
lo_buf->add( '      this.updateRow(row, this.STATUS.add);' ).
lo_buf->add( '    } else {' ).
lo_buf->add( '      this.updateRow(row, this.STATUS.reset);' ).
lo_buf->add( '    }' ).
lo_buf->add( '  });' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '// Table iteration helper' ).
lo_buf->add( 'StageHelper.prototype.iterateStageTab = function(changeMode, cb /*, ...*/) {' ).
lo_buf->add( '  var restArgs = Array.prototype.slice.call(arguments, 2);' ).
lo_buf->add( '  var table    = this.dom.stageTab;' ).
lo_buf->add( '  var retTotal = 0;' ).
lo_buf->add( '' ).
lo_buf->add( '  if (changeMode) {' ).
lo_buf->add( '    var scrollOffset = window.pageYOffset;' ).
lo_buf->add( '' ).
lo_buf->add( '    this.dom.stageTab.style.display = "none";' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  for (var b = 0, bN = table.tBodies.length; b < bN; b++) {' ).
lo_buf->add( '    var tbody = table.tBodies[b];' ).
lo_buf->add( '    for (var r = 0, rN = tbody.rows.length; r < rN; r++) {' ).
lo_buf->add( '      var args   = [tbody.rows[r]].concat(restArgs);' ).
lo_buf->add( '      var retVal = cb.apply(this, args); // callback' ).
lo_buf->add( '' ).
lo_buf->add( '      if (typeof retVal === "number") retTotal += retVal;' ).
lo_buf->add( '    }' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  if (changeMode) {' ).
lo_buf->add( '    this.dom.stageTab.style.display = "";' ).
lo_buf->add( '    window.scrollTo(0, scrollOffset);' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  return retTotal;' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '/**********************************************************' ).
lo_buf->add( ' * Check List Wrapper' ).
lo_buf->add( ' **********************************************************/' ).
lo_buf->add( '' ).
lo_buf->add( 'function CheckListWrapper(id, cbAction, cbActionOnlyMyChanges) {' ).
lo_buf->add( '  this.id                    = document.getElementById(id);' ).
lo_buf->add( '  this.cbAction              = cbAction;' ).
lo_buf->add( '  this.cbActionOnlyMyChanges = cbActionOnlyMyChanges;' ).
lo_buf->add( '  this.id.onclick            = this.onClick.bind(this);' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'CheckListWrapper.prototype.onClick = function(e) { // eslint-disable-line no-unused-vars' ).
lo_buf->add( '  // Get nodes' ).
lo_buf->add( '  var target = event.target || event.srcElement;' ).
lo_buf->add( '  if (!target) return;' ).
lo_buf->add( '  if (target.tagName !== "A") { target = target.parentNode } // icon clicked' ).
lo_buf->add( '  if (target.tagName !== "A") return;' ).
lo_buf->add( '  if (target.parentNode.tagName !== "LI") return;' ).
lo_buf->add( '' ).
lo_buf->add( '  var nodeA    = target;' ).
lo_buf->add( '  var nodeLi   = target.parentNode;' ).
lo_buf->add( '  var nodeIcon = target.children[0];' ).
lo_buf->add( '  if (!nodeIcon.classList.contains("icon")) return;' ).
lo_buf->add( '' ).
lo_buf->add( '  // Node updates' ).
lo_buf->add( '  var option   = nodeA.innerText;' ).
lo_buf->add( '  var oldState = nodeLi.getAttribute("data-check");' ).
lo_buf->add( '  if (oldState === null) return; // no data-check attribute - non-checkbox' ).
lo_buf->add( '  var newState = oldState === "X" ? false : true;' ).
lo_buf->add( '' ).
lo_buf->add( '  if (newState) {' ).
lo_buf->add( '    nodeIcon.classList.remove("grey");' ).
lo_buf->add( '    nodeIcon.classList.add("blue");' ).
lo_buf->add( '    nodeLi.setAttribute("data-check", "X");' ).
lo_buf->add( '  } else {' ).
lo_buf->add( '    nodeIcon.classList.remove("blue");' ).
lo_buf->add( '    nodeIcon.classList.add("grey");' ).
lo_buf->add( '    nodeLi.setAttribute("data-check", "");' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  // Action callback, special handling for "Only My Changes"' ).
lo_buf->add( '  if (option === "Only my changes") {' ).
lo_buf->add( '    this.cbActionOnlyMyChanges(nodeLi.getAttribute("data-aux"), newState);' ).
lo_buf->add( '' ).
lo_buf->add( '    // hide "Changed By" menu' ).
lo_buf->add( '  } else {' ).
lo_buf->add( '    this.cbAction(nodeLi.getAttribute("data-aux"), option, newState);' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '/**********************************************************' ).
lo_buf->add( ' * Diff Page Logic' ).
lo_buf->add( ' **********************************************************/' ).
lo_buf->add( '' ).
lo_buf->add( '// Diff helper constructor' ).
lo_buf->add( 'function DiffHelper(params) {' ).
lo_buf->add( '  this.pageSeed    = params.seed;' ).
lo_buf->add( '  this.counter     = 0;' ).
lo_buf->add( '  this.stageAction = params.stageAction;' ).
lo_buf->add( '' ).
lo_buf->add( '  // DOM nodes' ).
lo_buf->add( '  this.dom = {' ).
lo_buf->add( '    diffList   : document.getElementById(params.ids.diffList),' ).
lo_buf->add( '    stageButton: document.getElementById(params.ids.stageButton)' ).
lo_buf->add( '  };' ).
lo_buf->add( '' ).
lo_buf->add( '  this.repoKey = this.dom.diffList.getAttribute("data-repo-key");' ).
lo_buf->add( '  if (!this.repoKey) return; // Unexpected' ).
lo_buf->add( '' ).
lo_buf->add( '  this.dom.jump         = document.getElementById(params.ids.jump);' ).
lo_buf->add( '  this.dom.jump.onclick = this.onJump.bind(this);' ).
lo_buf->add( '' ).
lo_buf->add( '  // Checklist wrapper' ).
lo_buf->add( '  if (document.getElementById(params.ids.filterMenu)) {' ).
lo_buf->add( '    this.checkList        = new CheckListWrapper(params.ids.filterMenu, this.onFilter.bind(this), this.onFilterOnlyMyChanges.bind(this));' ).
lo_buf->add( '    this.dom.filterButton = document.getElementById(params.ids.filterMenu).parentNode;' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  // Hijack stage command' ).
lo_buf->add( '  if (this.dom.stageButton) {' ).
lo_buf->add( '    this.dom.stageButton.href    = "#";' ).
lo_buf->add( '    this.dom.stageButton.onclick = this.onStage.bind(this);' ).
lo_buf->add( '  }' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '// Action on jump click' ).
lo_buf->add( 'DiffHelper.prototype.onJump = function(e) {' ).
lo_buf->add( '  var text = ((e.target && e.target.text) || e);' ).
lo_buf->add( '  if (!text) return;' ).
lo_buf->add( '' ).
lo_buf->add( '  var elFile = document.querySelector("[data-file*=''" + text + "'']");' ).
lo_buf->add( '  if (!elFile) return;' ).
lo_buf->add( '' ).
lo_buf->add( '  setTimeout(function() {' ).
lo_buf->add( '    elFile.scrollIntoView();' ).
lo_buf->add( '  }, 100);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '// Action on filter click' ).
lo_buf->add( 'DiffHelper.prototype.onFilter = function(attr, target, state) {' ).
lo_buf->add( '  this.applyFilter(attr, target, state);' ).
lo_buf->add( '  this.highlightButton(state);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'DiffHelper.prototype.onFilterOnlyMyChanges = function(username, state) {' ).
lo_buf->add( '  this.applyOnlyMyChangesFilter(username, state);' ).
lo_buf->add( '  this.counter = 0;' ).
lo_buf->add( '' ).
lo_buf->add( '  if (state) {' ).
lo_buf->add( '    this.dom.filterButton.classList.add("bgorange");' ).
lo_buf->add( '  } else {' ).
lo_buf->add( '    this.dom.filterButton.classList.remove("bgorange");' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  // apply logic on Changed By list items' ).
lo_buf->add( '  var changedByListItems = Array.prototype.slice.call(document.querySelectorAll("[data-aux*=changed-by]"));' ).
lo_buf->add( '' ).
lo_buf->add( '  changedByListItems' ).
lo_buf->add( '    .map(function(item) {' ).
lo_buf->add( '      var nodeIcon = item.children[0].children[0];' ).
lo_buf->add( '' ).
lo_buf->add( '      if (state === true) {' ).
lo_buf->add( '        if (item.innerText === username) { // current user' ).
lo_buf->add( '          item.style.display = "";' ).
lo_buf->add( '          item.setAttribute("data-check", "X");' ).
lo_buf->add( '' ).
lo_buf->add( '          if (nodeIcon) {' ).
lo_buf->add( '            nodeIcon.classList.remove("grey");' ).
lo_buf->add( '            nodeIcon.classList.add("blue");' ).
lo_buf->add( '          }' ).
lo_buf->add( '        } else { // other users' ).
lo_buf->add( '          item.style.display = "none";' ).
lo_buf->add( '          item.setAttribute("data-check", "");' ).
lo_buf->add( '        }' ).
lo_buf->add( '      } else {' ).
lo_buf->add( '        item.style.display = "";' ).
lo_buf->add( '        item.setAttribute("data-check", "X");' ).
lo_buf->add( '' ).
lo_buf->add( '        if (nodeIcon) {' ).
lo_buf->add( '          nodeIcon.classList.remove("grey");' ).
lo_buf->add( '          nodeIcon.classList.add("blue");' ).
lo_buf->add( '        }' ).
lo_buf->add( '      }' ).
lo_buf->add( '    });' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'DiffHelper.prototype.applyOnlyMyChangesFilter = function(username, state) {' ).
lo_buf->add( '  var jumpListItems = Array.prototype.slice.call(document.querySelectorAll("[id*=li_jump]"));' ).
lo_buf->add( '' ).
lo_buf->add( '  this.iterateDiffList(function(div) {' ).
lo_buf->add( '    if (state === true) { // switching on "Only my changes" filter' ).
lo_buf->add( '      if (div.getAttribute("data-changed-by") === username) {' ).
lo_buf->add( '        div.style.display = state ? "" : "none";' ).
lo_buf->add( '      } else {' ).
lo_buf->add( '        div.style.display = state ? "none" : "";' ).
lo_buf->add( '      }' ).
lo_buf->add( '    } else { // disabling' ).
lo_buf->add( '      div.style.display = "";' ).
lo_buf->add( '    }' ).
lo_buf->add( '' ).
lo_buf->add( '    // hide the file in the jump list' ).
lo_buf->add( '    var dataFile = div.getAttribute("data-file");' ).
lo_buf->add( '    jumpListItems' ).
lo_buf->add( '      .filter(function(item) { return dataFile.includes(item.text) })' ).
lo_buf->add( '      .map(function(item) { item.style.display = div.style.display });' ).
lo_buf->add( '  });' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '// Hide/show diff based on params' ).
lo_buf->add( 'DiffHelper.prototype.applyFilter = function(attr, target, state) {' ).
lo_buf->add( '  var jumpListItems = Array.prototype.slice.call(document.querySelectorAll("[id*=li_jump]"));' ).
lo_buf->add( '' ).
lo_buf->add( '  this.iterateDiffList(function(div) {' ).
lo_buf->add( '    if (div.getAttribute("data-" + attr) === target) {' ).
lo_buf->add( '      div.style.display = state ? "" : "none";' ).
lo_buf->add( '' ).
lo_buf->add( '      // hide the file in the jump list' ).
lo_buf->add( '      var dataFile = div.getAttribute("data-file");' ).
lo_buf->add( '      jumpListItems' ).
lo_buf->add( '        .filter(function(item) { return dataFile.includes(item.text) })' ).
lo_buf->add( '        .map(function(item) { item.style.display = div.style.display });' ).
lo_buf->add( '    }' ).
lo_buf->add( '  });' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '// Action on stage -> save visible diffs as state for stage page' ).
lo_buf->add( 'DiffHelper.prototype.onStage = function(e) { // eslint-disable-line no-unused-vars' ).
lo_buf->add( '  if (window.sessionStorage) {' ).
lo_buf->add( '    var data = this.buildStageCache();' ).
lo_buf->add( '    window.sessionStorage.setItem(this.pageSeed, JSON.stringify(data));' ).
lo_buf->add( '  }' ).
lo_buf->add( '  var getParams = { key: this.repoKey, seed: this.pageSeed };' ).
lo_buf->add( '  submitSapeventForm(getParams, this.stageAction, "get");' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '// Collect visible diffs' ).
lo_buf->add( 'DiffHelper.prototype.buildStageCache = function() {' ).
lo_buf->add( '  var list = {};' ).
lo_buf->add( '  this.iterateDiffList(function(div) {' ).
lo_buf->add( '    var filename = div.getAttribute("data-file");' ).
lo_buf->add( '    if (!div.style.display && filename) { // No display override - visible !!' ).
lo_buf->add( '      list[filename] = "A"; // Add' ).
lo_buf->add( '    }' ).
lo_buf->add( '  });' ).
lo_buf->add( '  return list;' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '// Table iterator' ).
lo_buf->add( 'DiffHelper.prototype.iterateDiffList = function(cb /*, ...*/) {' ).
lo_buf->add( '  var restArgs = Array.prototype.slice.call(arguments, 1);' ).
lo_buf->add( '  var diffList = this.dom.diffList;' ).
lo_buf->add( '' ).
lo_buf->add( '  for (var i = 0, iN = diffList.children.length; i < iN; i++) {' ).
lo_buf->add( '    var div = diffList.children[i];' ).
lo_buf->add( '    if (div.className !== "diff") continue;' ).
lo_buf->add( '    var args = [div].concat(restArgs);' ).
lo_buf->add( '    cb.apply(this, args);// callback' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '// Highlight filter button if filter is activate' ).
lo_buf->add( 'DiffHelper.prototype.highlightButton = function(state) {' ).
lo_buf->add( '  this.counter += state ? -1 : 1;' ).
lo_buf->add( '  if (this.counter > 0) {' ).
lo_buf->add( '    this.dom.filterButton.classList.add("bgorange");' ).
lo_buf->add( '  } else {' ).
lo_buf->add( '    this.dom.filterButton.classList.remove("bgorange");' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '// Collapse or expand diffs' ).
lo_buf->add( 'function onDiffCollapse(event) {' ).
lo_buf->add( '  var source          = event.target || event.srcElement;' ).
lo_buf->add( '  var nextDiffContent = source.parentElement.nextElementSibling;' ).
lo_buf->add( '  var hide;' ).
lo_buf->add( '' ).
lo_buf->add( '  if (source.classList.contains("icon-chevron-down")) {' ).
lo_buf->add( '    source.classList.remove("icon-chevron-down");' ).
lo_buf->add( '    source.classList.add("icon-chevron-right");' ).
lo_buf->add( '    hide = true;' ).
lo_buf->add( '  } else {' ).
lo_buf->add( '    source.classList.remove("icon-chevron-right");' ).
lo_buf->add( '    source.classList.add("icon-chevron-down");' ).
lo_buf->add( '    hide = false;' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  hide ? nextDiffContent.classList.add("nodisplay"): nextDiffContent.classList.remove("nodisplay");' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '// Add bottom margin, so that we can scroll to the top of the last file' ).
lo_buf->add( 'function addMarginBottom() {' ).
lo_buf->add( '  document.getElementsByTagName("body")[0].style.marginBottom = screen.height + "px";' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/**********************************************************' ).
lo_buf->add( ' * Diff Page Column Selection' ).
lo_buf->add( ' **********************************************************/' ).
lo_buf->add( '' ).
lo_buf->add( 'function DiffColumnSelection() {' ).
lo_buf->add( '  this.selectedColumnIdx = -1;' ).
lo_buf->add( '  this.lineNumColumnIdx  = -1;' ).
lo_buf->add( '  //https://stackoverflow.com/questions/2749244/javascript-setinterval-and-this-solution' ).
lo_buf->add( '  document.addEventListener("mousedown", this.mousedownEventListener.bind(this));' ).
lo_buf->add( '  document.addEventListener("copy", this.copyEventListener.bind(this));' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'DiffColumnSelection.prototype.mousedownEventListener = function(e) {' ).
lo_buf->add( '  // Select text in a column of an HTML table and copy to clipboard (in DIFF view)' ).
lo_buf->add( '  // (https://stackoverflow.com/questions/6619805/select-text-in-a-column-of-an-html-table)' ).
lo_buf->add( '  // Process mousedown event for all TD elements -> apply CSS class at TABLE level.' ).
lo_buf->add( '  // (https://stackoverflow.com/questions/40956717/how-to-addeventlistener-to-multiple-elements-in-a-single-line)' ).
lo_buf->add( '  var unifiedLineNumColumnIdx    = 0;' ).
lo_buf->add( '  var unifiedCodeColumnIdx       = 3;' ).
lo_buf->add( '  var splitLineNumLeftColumnIdx  = 0;' ).
lo_buf->add( '  var splitCodeLeftColumnIdx     = 2;' ).
lo_buf->add( '  var splitLineNumRightColumnIdx = 3;' ).
lo_buf->add( '  var splitCodeRightColumnIdx    = 5;' ).
lo_buf->add( '' ).
lo_buf->add( '  if (e.button !== 0) return; // function is only valid for left button, not right button' ).
lo_buf->add( '' ).
lo_buf->add( '  var td = e.target;' ).
lo_buf->add( '' ).
lo_buf->add( '  while (td != undefined && td.tagName != "TD" && td.tagName != "TBODY") td = td.parentElement;' ).
lo_buf->add( '  if (td == undefined) return;' ).
lo_buf->add( '  var table = td.parentElement.parentElement;' ).
lo_buf->add( '' ).
lo_buf->add( '  var patchColumnCount = 0;' ).
lo_buf->add( '  if (td.parentElement.cells[0].classList.contains("patch")) {' ).
lo_buf->add( '    patchColumnCount = 1;' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  if (td.classList.contains("diff_left")) {' ).
lo_buf->add( '    table.classList.remove("diff_select_right");' ).
lo_buf->add( '    table.classList.add("diff_select_left");' ).
lo_buf->add( '    if (window.getSelection() && this.selectedColumnIdx != splitCodeLeftColumnIdx + patchColumnCount) {' ).
lo_buf->add( '      // De-select to avoid effect of dragging selection in case the right column was first selected' ).
lo_buf->add( '      if (document.body.createTextRange) { // All IE but Edge' ).
lo_buf->add( '        // document.getSelection().removeAllRanges() may trigger error' ).
lo_buf->add( '        // so use this code which is equivalent but does not fail' ).
lo_buf->add( '        // (https://stackoverflow.com/questions/22914075/javascript-error-800a025e-using-range-selector)' ).
lo_buf->add( '        range = document.body.createTextRange();' ).
lo_buf->add( '        range.collapse();' ).
lo_buf->add( '        range.select();' ).
lo_buf->add( '      } else {' ).
lo_buf->add( '        document.getSelection().removeAllRanges();' ).
lo_buf->add( '      }' ).
lo_buf->add( '    }' ).
lo_buf->add( '    this.selectedColumnIdx = splitCodeLeftColumnIdx + patchColumnCount;' ).
lo_buf->add( '    this.lineNumColumnIdx  = splitLineNumLeftColumnIdx + patchColumnCount;' ).
lo_buf->add( '' ).
lo_buf->add( '  } else if (td.classList.contains("diff_right")) {' ).
lo_buf->add( '    table.classList.remove("diff_select_left");' ).
lo_buf->add( '    table.classList.add("diff_select_right");' ).
lo_buf->add( '    if (window.getSelection() && this.selectedColumnIdx != splitCodeRightColumnIdx + patchColumnCount) {' ).
lo_buf->add( '      if (document.body.createTextRange) { // All IE but Edge' ).
lo_buf->add( '        // document.getSelection().removeAllRanges() may trigger error' ).
lo_buf->add( '        // so use this code which is equivalent but does not fail' ).
lo_buf->add( '        // (https://stackoverflow.com/questions/22914075/javascript-error-800a025e-using-range-selector)' ).
lo_buf->add( '        var range = document.body.createTextRange();' ).
lo_buf->add( '        range.collapse();' ).
lo_buf->add( '        range.select();' ).
lo_buf->add( '      } else {' ).
lo_buf->add( '        document.getSelection().removeAllRanges();' ).
lo_buf->add( '      }' ).
lo_buf->add( '    }' ).
lo_buf->add( '    this.selectedColumnIdx = splitCodeRightColumnIdx + patchColumnCount;' ).
lo_buf->add( '    this.lineNumColumnIdx  = splitLineNumRightColumnIdx + patchColumnCount;' ).
lo_buf->add( '' ).
lo_buf->add( '  } else if (td.classList.contains("diff_unified")) {' ).
lo_buf->add( '    this.selectedColumnIdx = unifiedCodeColumnIdx;' ).
lo_buf->add( '    this.lineNumColumnIdx  = unifiedLineNumColumnIdx;' ).
lo_buf->add( '' ).
lo_buf->add( '  } else {' ).
lo_buf->add( '    this.selectedColumnIdx = -1;' ).
lo_buf->add( '    this.lineNumColumnIdx  = -1;' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'DiffColumnSelection.prototype.copyEventListener = function(e) {' ).
lo_buf->add( '  // Select text in a column of an HTML table and copy to clipboard (in DIFF view)' ).
lo_buf->add( '  // (https://stackoverflow.com/questions/6619805/select-text-in-a-column-of-an-html-table)' ).
lo_buf->add( '  var td = e.target;' ).
lo_buf->add( '' ).
lo_buf->add( '  while (td != undefined && td.tagName != "TD" && td.tagName != "TBODY") td = td.parentElement;' ).
lo_buf->add( '  if (td != undefined) {' ).
lo_buf->add( '    // Use window.clipboardData instead of e.clipboardData' ).
lo_buf->add( '    // (https://stackoverflow.com/questions/23470958/ie-10-copy-paste-issue)' ).
lo_buf->add( '    var clipboardData = (e.clipboardData == undefined ? window.clipboardData : e.clipboardData);' ).
lo_buf->add( '    var text          = this.getSelectedText();' ).
lo_buf->add( '    clipboardData.setData("text", text);' ).
lo_buf->add( '    e.preventDefault();' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'DiffColumnSelection.prototype.getSelectedText = function() {' ).
lo_buf->add( '  // Select text in a column of an HTML table and copy to clipboard (in DIFF view)' ).
lo_buf->add( '  // (https://stackoverflow.com/questions/6619805/select-text-in-a-column-of-an-html-table)' ).
lo_buf->add( '  var sel   = window.getSelection();' ).
lo_buf->add( '  var range = sel.getRangeAt(0);' ).
lo_buf->add( '  var doc   = range.cloneContents();' ).
lo_buf->add( '  var nodes = doc.querySelectorAll("tr");' ).
lo_buf->add( '  var text  = "";' ).
lo_buf->add( '  if (nodes.length === 0) {' ).
lo_buf->add( '    text = doc.textContent;' ).
lo_buf->add( '  } else {' ).
lo_buf->add( '    var newline  = "";' ).
lo_buf->add( '    var realThis = this;' ).
lo_buf->add( '    var copySide = "";' ).
lo_buf->add( '    [].forEach.call(nodes, function(tr, i) {' ).
lo_buf->add( '      var cellIdx = (i == 0 ? 0 : realThis.selectedColumnIdx);' ).
lo_buf->add( '      if (tr.cells.length > cellIdx) {' ).
lo_buf->add( '        var tdSelected = tr.cells[cellIdx];' ).
lo_buf->add( '        // decide which side to copy based on first line of selection' ).
lo_buf->add( '        if (i == 0) {' ).
lo_buf->add( '          copySide = (tdSelected.classList.contains("new") ? "new" : "old" );' ).
lo_buf->add( '        }' ).
lo_buf->add( '        // copy is interesting only for one side of code, do not copy lines which exist on other side' ).
lo_buf->add( '        if (i == 0 || copySide == "new" && !tdSelected.classList.contains("old") || copySide == "old" && !tdSelected.classList.contains("new")) {' ).
lo_buf->add( '          text += newline + tdSelected.textContent;' ).
lo_buf->add( '          // special processing for TD tag which sometimes contains newline' ).
lo_buf->add( '          // (expl: /src/ui/zabapgit_js_common.w3mi.data.js) so do not add newline again in that case.' ).
lo_buf->add( '          var lastChar = tdSelected.textContent[tdSelected.textContent.length - 1];' ).
lo_buf->add( '          if (lastChar == "\n") newline = "";' ).
lo_buf->add( '          else newline = "\n";' ).
lo_buf->add( '        }' ).
lo_buf->add( '      }' ).
lo_buf->add( '    });' ).
lo_buf->add( '  }' ).
lo_buf->add( '  return text;' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '/**********************************************************' ).
lo_buf->add( ' * Display Helper' ).
lo_buf->add( ' **********************************************************/' ).
lo_buf->add( '' ).
lo_buf->add( '// Toggle display of changelog (news) and message popups' ).
lo_buf->add( 'function toggleDisplay(divId) {' ).
lo_buf->add( '  var div = document.getElementById(divId);' ).
lo_buf->add( '' ).
lo_buf->add( '  if (div) div.style.display = (div.style.display) ? "" : "none";' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/**********************************************************' ).
lo_buf->add( ' * Keyboard Navigation' ).
lo_buf->add( ' **********************************************************/' ).
lo_buf->add( '' ).
lo_buf->add( 'function KeyNavigation() { }' ).
lo_buf->add( '' ).
lo_buf->add( 'KeyNavigation.prototype.onkeydown = function(event) {' ).
lo_buf->add( '  if (event.defaultPrevented) return;' ).
lo_buf->add( '' ).
lo_buf->add( '  // navigate with arrows through list items and support pressing links with enter and space' ).
lo_buf->add( '  var isHandled = false;' ).
lo_buf->add( '  if (event.key === "Enter" || event.key === "") {' ).
lo_buf->add( '    isHandled = this.onEnterOrSpace();' ).
lo_buf->add( '  } else if (/Down$/.test(event.key)) {' ).
lo_buf->add( '    isHandled = this.onArrowDown();' ).
lo_buf->add( '  } else if (/Up$/.test(event.key)) {' ).
lo_buf->add( '    isHandled = this.onArrowUp();' ).
lo_buf->add( '  } else if (event.key === "Backspace") {' ).
lo_buf->add( '    isHandled = this.onBackspace();' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  if (isHandled) event.preventDefault();' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'KeyNavigation.prototype.onEnterOrSpace = function() {' ).
lo_buf->add( '  if (document.activeElement.nodeName !== "A") return;' ).
lo_buf->add( '  var anchor = document.activeElement;' ).
lo_buf->add( '' ).
lo_buf->add( '  if (anchor.href.replace(/#$/, "") === document.location.href.replace(/#$/, "")' ).
lo_buf->add( '    && !anchor.onclick' ).
lo_buf->add( '    && anchor.parentElement' ).
lo_buf->add( '    && anchor.parentElement.nodeName === "LI") {' ).
lo_buf->add( '    anchor.parentElement.classList.toggle("force-nav-hover");' ).
lo_buf->add( '  } else {' ).
lo_buf->add( '    anchor.click();' ).
lo_buf->add( '  }' ).
lo_buf->add( '  return true;' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'KeyNavigation.prototype.focusListItem = function(li) {' ).
lo_buf->add( '  var anchor = li.firstElementChild;' ).
lo_buf->add( '  if (!anchor || anchor.nodeName !== "A") return false;' ).
lo_buf->add( '  anchor.focus();' ).
lo_buf->add( '  return true;' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'KeyNavigation.prototype.closeDropdown = function(dropdownLi) {' ).
lo_buf->add( '  dropdownLi.classList.remove("force-nav-hover");' ).
lo_buf->add( '  if (dropdownLi.firstElementChild.nodeName === "A") dropdownLi.firstElementChild.focus();' ).
lo_buf->add( '  return true;' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'KeyNavigation.prototype.onBackspace = function() {' ).
lo_buf->add( '  var activeElement = document.activeElement;' ).
lo_buf->add( '' ).
lo_buf->add( '  // Detect opened subsequent dropdown' ).
lo_buf->add( '  if (activeElement.nodeName === "A"' ).
lo_buf->add( '    && activeElement.parentElement' ).
lo_buf->add( '    && activeElement.parentElement.nodeName === "LI"' ).
lo_buf->add( '    && activeElement.parentElement.classList.contains("force-nav-hover")) {' ).
lo_buf->add( '    return this.closeDropdown(activeElement.parentElement);' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  // Detect opened parent dropdown' ).
lo_buf->add( '  if (activeElement.nodeName === "A"' ).
lo_buf->add( '    && activeElement.parentElement' ).
lo_buf->add( '    && activeElement.parentElement.nodeName === "LI"' ).
lo_buf->add( '    && activeElement.parentElement.parentElement' ).
lo_buf->add( '    && activeElement.parentElement.parentElement.nodeName === "UL"' ).
lo_buf->add( '    && activeElement.parentElement.parentElement.parentElement' ).
lo_buf->add( '    && activeElement.parentElement.parentElement.parentElement.nodeName === "LI"' ).
lo_buf->add( '    && activeElement.parentElement.parentElement.parentElement.classList.contains("force-nav-hover")) {' ).
lo_buf->add( '    return this.closeDropdown(activeElement.parentElement.parentElement.parentElement);' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'KeyNavigation.prototype.onArrowDown = function() {' ).
lo_buf->add( '  var activeElement = document.activeElement;' ).
lo_buf->add( '' ).
lo_buf->add( '  // Start of dropdown list: LI > selected A :: UL > LI > A' ).
lo_buf->add( '  if (activeElement.nodeName === "A"' ).
lo_buf->add( '    && activeElement.parentElement' ).
lo_buf->add( '    && activeElement.parentElement.nodeName === "LI"' ).
lo_buf->add( '    && activeElement.parentElement.classList.contains("force-nav-hover") // opened dropdown' ).
lo_buf->add( '    && activeElement.nextElementSibling' ).
lo_buf->add( '    && activeElement.nextElementSibling.nodeName === "UL"' ).
lo_buf->add( '    && activeElement.nextElementSibling.firstElementChild' ).
lo_buf->add( '    && activeElement.nextElementSibling.firstElementChild.nodeName === "LI") {' ).
lo_buf->add( '    return this.focusListItem(activeElement.nextElementSibling.firstElementChild);' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  // Next item of dropdown list: ( LI > selected A ) :: LI > A' ).
lo_buf->add( '  if (activeElement.nodeName === "A"' ).
lo_buf->add( '    && activeElement.parentElement' ).
lo_buf->add( '    && activeElement.parentElement.nodeName === "LI"' ).
lo_buf->add( '    && activeElement.parentElement.nextElementSibling' ).
lo_buf->add( '    && activeElement.parentElement.nextElementSibling.nodeName === "LI") {' ).
lo_buf->add( '    return this.focusListItem(activeElement.parentElement.nextElementSibling);' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'KeyNavigation.prototype.onArrowUp = function() {' ).
lo_buf->add( '  var activeElement = document.activeElement;' ).
lo_buf->add( '' ).
lo_buf->add( '  // Prev item of dropdown list: ( LI > selected A ) <:: LI > A' ).
lo_buf->add( '  if (activeElement.nodeName === "A"' ).
lo_buf->add( '    && activeElement.parentElement' ).
lo_buf->add( '    && activeElement.parentElement.nodeName === "LI"' ).
lo_buf->add( '    && activeElement.parentElement.previousElementSibling' ).
lo_buf->add( '    && activeElement.parentElement.previousElementSibling.nodeName === "LI") {' ).
lo_buf->add( '    return this.focusListItem(activeElement.parentElement.previousElementSibling);' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'KeyNavigation.prototype.getHandler = function() {' ).
lo_buf->add( '  return this.onkeydown.bind(this);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '// this functions enables the navigation with arrows through list items (li)' ).
lo_buf->add( '// e.g. in dropdown menus' ).
lo_buf->add( 'function enableArrowListNavigation() {' ).
lo_buf->add( '  document.addEventListener("keydown", new KeyNavigation().getHandler());' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/**********************************************************' ).
lo_buf->add( ' * Link Hints (Vimium-like)' ).
lo_buf->add( ' **********************************************************/' ).
lo_buf->add( '' ).
lo_buf->add( 'function LinkHints(linkHintHotKey) {' ).
lo_buf->add( '  this.linkHintHotKey    = linkHintHotKey;' ).
lo_buf->add( '  this.areHintsDisplayed = false;' ).
lo_buf->add( '  this.pendingPath       = ""; // already typed code prefix' ).
lo_buf->add( '  this.hintsMap          = this.deployHintContainers();' ).
lo_buf->add( '  this.activatedDropdown = null;' ).
lo_buf->add( '  this.yankModeActive    = false;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'LinkHints.prototype.getHintStartValue = function(targetsCount) {' ).
lo_buf->add( '  // e.g. if we have 89 tooltips we start from 10' ).
lo_buf->add( '  //      if we have 90 tooltips we start from 100' ).
lo_buf->add( '  //      if we have 900 tooltips we start from 1000' ).
lo_buf->add( '  var' ).
lo_buf->add( '    baseLength          = Math.pow(10, targetsCount.toString().length - 1),' ).
lo_buf->add( '    maxHintStringLength = (targetsCount + baseLength).toString().length;' ).
lo_buf->add( '  return Math.pow(10, maxHintStringLength - 1);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'LinkHints.prototype.deployHintContainers = function() {' ).
lo_buf->add( '' ).
lo_buf->add( '  var hintTargets = document.querySelectorAll("a, input, textarea, i");' ).
lo_buf->add( '  var codeCounter = this.getHintStartValue(hintTargets.length);' ).
lo_buf->add( '  var hintsMap    = { first: codeCounter };' ).
lo_buf->add( '' ).
lo_buf->add( '  // <span class="link-hint" data-code="123">' ).
lo_buf->add( '  //   <span class="pending">12</span><span>3</span>' ).
lo_buf->add( '  // </span>' ).
lo_buf->add( '  for (var i = 0, N = hintTargets.length; i < N; i++) {' ).
lo_buf->add( '    // skip hidden fields' ).
lo_buf->add( '    if (hintTargets[i].type === "HIDDEN") {' ).
lo_buf->add( '      continue;' ).
lo_buf->add( '    }' ).
lo_buf->add( '' ).
lo_buf->add( '    var hint = {};' ).
lo_buf->add( '' ).
lo_buf->add( '    hint.container     = document.createElement("span");' ).
lo_buf->add( '    hint.pendingSpan   = document.createElement("span");' ).
lo_buf->add( '    hint.remainingSpan = document.createElement("span");' ).
lo_buf->add( '    hint.parent        = hintTargets[i];' ).
lo_buf->add( '    hint.code          = codeCounter.toString();' ).
lo_buf->add( '' ).
lo_buf->add( '    hint.container.appendChild(hint.pendingSpan);' ).
lo_buf->add( '    hint.container.appendChild(hint.remainingSpan);' ).
lo_buf->add( '' ).
lo_buf->add( '    hint.pendingSpan.classList.add("pending");' ).
lo_buf->add( '    hint.container.classList.add("link-hint");' ).
lo_buf->add( '    if (hint.parent.nodeName === "INPUT" || hint.parent.nodeName === "TEXTAREA") {' ).
lo_buf->add( '      hint.container.classList.add("link-hint-input");' ).
lo_buf->add( '    } else if (hint.parent.nodeName === "A") {' ).
lo_buf->add( '      hint.container.classList.add("link-hint-a");' ).
lo_buf->add( '    } else if (hint.parent.nodeName === "I" && hint.parent.classList.contains("cursor-pointer")) {' ).
lo_buf->add( '      hint.container.classList.add("link-hint-i");' ).
lo_buf->add( '    } else {' ).
lo_buf->add( '      continue;' ).
lo_buf->add( '    }' ).
lo_buf->add( '' ).
lo_buf->add( '    hint.container.classList.add("nodisplay"); // hide by default' ).
lo_buf->add( '    hint.container.dataset.code = codeCounter.toString(); // not really needed, more for debug' ).
lo_buf->add( '' ).
lo_buf->add( '    if (hintTargets[i].nodeName === "INPUT" || hintTargets[i].nodeName === "TEXTAREA") {' ).
lo_buf->add( '      // does not work if inside the input node' ).
lo_buf->add( '      if (hintTargets[i].type === "checkbox" || hintTargets[i].type === "radio") {' ).
lo_buf->add( '        if (hintTargets[i].nextElementSibling && hintTargets[i].nextElementSibling.nodeName === "LABEL") {' ).
lo_buf->add( '          // insert at end of label' ).
lo_buf->add( '          hintTargets[i].nextElementSibling.appendChild(hint.container);' ).
lo_buf->add( '        } else {' ).
lo_buf->add( '          // inserting right after' ).
lo_buf->add( '          hintTargets[i].insertAdjacentElement("afterend", hint.container);' ).
lo_buf->add( '        }' ).
lo_buf->add( '      } else {' ).
lo_buf->add( '        // inserting right after' ).
lo_buf->add( '        hintTargets[i].insertAdjacentElement("afterend", hint.container);' ).
lo_buf->add( '      }' ).
lo_buf->add( '    } else {' ).
lo_buf->add( '      hintTargets[i].appendChild(hint.container);' ).
lo_buf->add( '    }' ).
lo_buf->add( '    hintsMap[codeCounter++] = hint;' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  hintsMap.last = codeCounter - 1;' ).
lo_buf->add( '  return hintsMap;' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'LinkHints.prototype.getHandler = function() {' ).
lo_buf->add( '  return this.handleKey.bind(this);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'LinkHints.prototype.handleKey = function(event) {' ).
lo_buf->add( '  if (event.defaultPrevented) {' ).
lo_buf->add( '    return;' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  if (event.key === "y") {' ).
lo_buf->add( '    this.yankModeActive = !this.yankModeActive;' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  if (event.key === this.linkHintHotKey && Hotkeys.isHotkeyCallPossible()) {' ).
lo_buf->add( '' ).
lo_buf->add( '    // on user hide hints, close an opened dropdown too' ).
lo_buf->add( '    if (this.areHintsDisplayed && this.activatedDropdown) this.closeActivatedDropdown();' ).
lo_buf->add( '    if (this.areHintsDisplayed) this.yankModeActive = false;' ).
lo_buf->add( '' ).
lo_buf->add( '    this.pendingPath = "";' ).
lo_buf->add( '    this.displayHints(!this.areHintsDisplayed);' ).
lo_buf->add( '' ).
lo_buf->add( '  } else if (this.areHintsDisplayed) {' ).
lo_buf->add( '' ).
lo_buf->add( '    // the user tries to reach a hint' ).
lo_buf->add( '    this.pendingPath += event.key;' ).
lo_buf->add( '' ).
lo_buf->add( '    var hint = this.hintsMap[this.pendingPath];' ).
lo_buf->add( '' ).
lo_buf->add( '    if (hint) { // we are there, we have a fully specified tooltip. Let us activate or yank it' ).
lo_buf->add( '      this.displayHints(false);' ).
lo_buf->add( '      event.preventDefault();' ).
lo_buf->add( '      if (this.yankModeActive) {' ).
lo_buf->add( '        submitSapeventForm({ clipboard: hint.parent.firstChild.textContent }, "yank_to_clipboard");' ).
lo_buf->add( '        this.yankModeActive = false;' ).
lo_buf->add( '      } else {' ).
lo_buf->add( '        this.hintActivate(hint);' ).
lo_buf->add( '      }' ).
lo_buf->add( '    } else {' ).
lo_buf->add( '      // we are not there yet, but let us filter the link so that only' ).
lo_buf->add( '      // the partially matched are shown' ).
lo_buf->add( '      var visibleHints = this.filterHints();' ).
lo_buf->add( '      if (!visibleHints) {' ).
lo_buf->add( '        this.displayHints(false);' ).
lo_buf->add( '        if (this.activatedDropdown) this.closeActivatedDropdown();' ).
lo_buf->add( '      }' ).
lo_buf->add( '    }' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'LinkHints.prototype.closeActivatedDropdown = function() {' ).
lo_buf->add( '  if (!this.activatedDropdown) return;' ).
lo_buf->add( '  this.activatedDropdown.classList.remove("force-nav-hover");' ).
lo_buf->add( '  this.activatedDropdown = null;' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'LinkHints.prototype.displayHints = function(isActivate) {' ).
lo_buf->add( '  this.areHintsDisplayed = isActivate;' ).
lo_buf->add( '  for (var i = this.hintsMap.first; i <= this.hintsMap.last; i++) {' ).
lo_buf->add( '    var hint = this.hintsMap[i];' ).
lo_buf->add( '    if (isActivate) {' ).
lo_buf->add( '      hint.container.classList.remove("nodisplay");' ).
lo_buf->add( '      hint.pendingSpan.innerText   = "";' ).
lo_buf->add( '      hint.remainingSpan.innerText = hint.code;' ).
lo_buf->add( '    } else {' ).
lo_buf->add( '      hint.container.classList.add("nodisplay");' ).
lo_buf->add( '    }' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'LinkHints.prototype.hintActivate = function(hint) {' ).
lo_buf->add( '  if (hint.parent.nodeName === "A"' ).
lo_buf->add( '    // hint.parent.href doesn`t have a # at the end while accessing dropdowns the first time.' ).
lo_buf->add( '    // Seems like a idiosyncrasy of SAPGUI`s IE. So let`s ignore the last character.' ).
lo_buf->add( '    && (hint.parent.href.substr(0, hint.parent.href.length - 1) === document.location.href)// href is #' ).
lo_buf->add( '    && !hint.parent.onclick // no handler' ).
lo_buf->add( '    && hint.parent.parentElement && hint.parent.parentElement.nodeName === "LI") {' ).
lo_buf->add( '    // probably it is a dropdown ...' ).
lo_buf->add( '    this.activatedDropdown = hint.parent.parentElement;' ).
lo_buf->add( '    this.activatedDropdown.classList.toggle("force-nav-hover");' ).
lo_buf->add( '    hint.parent.focus();' ).
lo_buf->add( '  } else if (hint.parent.type === "checkbox") {' ).
lo_buf->add( '    this.toggleCheckbox(hint);' ).
lo_buf->add( '  } else if (hint.parent.type === "radio") {' ).
lo_buf->add( '    this.toggleRadioButton(hint);' ).
lo_buf->add( '  } else if (hint.parent.type === "submit") {' ).
lo_buf->add( '    hint.parent.click();' ).
lo_buf->add( '  } else if (hint.parent.nodeName === "INPUT" || hint.parent.nodeName === "TEXTAREA") {' ).
lo_buf->add( '    hint.parent.focus();' ).
lo_buf->add( '  } else {' ).
lo_buf->add( '    hint.parent.click();' ).
lo_buf->add( '    if (this.activatedDropdown) this.closeActivatedDropdown();' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'LinkHints.prototype.toggleCheckbox = function(hint) {' ).
lo_buf->add( '  var checked = hint.parent.checked;' ).
lo_buf->add( '  this.triggerClickHandler(hint.parent.parentElement);' ).
lo_buf->add( '  if (checked === hint.parent.checked) {' ).
lo_buf->add( '    // fallback if no handler is registered' ).
lo_buf->add( '    hint.parent.checked = !hint.parent.checked;' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'LinkHints.prototype.toggleRadioButton = function(hint) {' ).
lo_buf->add( '  this.triggerClickHandler(hint.parent);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'LinkHints.prototype.triggerClickHandler = function(el) {' ).
lo_buf->add( '  // ensures that onclick handler is executed' ).
lo_buf->add( '  // https://stackoverflow.com/questions/41981509/trigger-an-event-when-a-checkbox-is-changed-programmatically-via-javascript' ).
lo_buf->add( '  var event = document.createEvent("HTMLEvents");' ).
lo_buf->add( '  event.initEvent("click", false, true);' ).
lo_buf->add( '  el.dispatchEvent(event);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'LinkHints.prototype.filterHints = function() {' ).
lo_buf->add( '  var visibleHints = 0;' ).
lo_buf->add( '  for (var i = this.hintsMap.first; i <= this.hintsMap.last; i++) {' ).
lo_buf->add( '    var hint = this.hintsMap[i];' ).
lo_buf->add( '    if (i.toString().startsWith(this.pendingPath)) {' ).
lo_buf->add( '      hint.pendingSpan.innerText   = this.pendingPath;' ).
lo_buf->add( '      hint.remainingSpan.innerText = hint.code.substring(this.pendingPath.length);' ).
lo_buf->add( '      // hint.container.classList.remove("nodisplay"); // for backspace' ).
lo_buf->add( '      visibleHints++;' ).
lo_buf->add( '    } else {' ).
lo_buf->add( '      hint.container.classList.add("nodisplay");' ).
lo_buf->add( '    }' ).
lo_buf->add( '  }' ).
lo_buf->add( '  return visibleHints;' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'function activateLinkHints(linkHintHotKey) {' ).
lo_buf->add( '  if (!linkHintHotKey) return;' ).
lo_buf->add( '  var oLinkHint = new LinkHints(linkHintHotKey);' ).
lo_buf->add( '  document.addEventListener("keypress", oLinkHint.getHandler());' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/**********************************************************' ).
lo_buf->add( ' * Hotkeys' ).
lo_buf->add( ' **********************************************************/' ).
lo_buf->add( '' ).
lo_buf->add( 'function Hotkeys(oKeyMap) {' ).
lo_buf->add( '  this.oKeyMap = oKeyMap || {};' ).
lo_buf->add( '' ).
lo_buf->add( '  // these are the hotkeys provided by the backend' ).
lo_buf->add( '  Object.keys(this.oKeyMap).forEach(function(sKey) {' ).
lo_buf->add( '' ).
lo_buf->add( '    var action = this.oKeyMap[sKey];' ).
lo_buf->add( '' ).
lo_buf->add( '    // add a tooltip/title with the hotkey, currently only sapevents are supported' ).
lo_buf->add( '    this.getAllSapEventsForSapEventName(action).forEach(function(elAnchor) {' ).
lo_buf->add( '      elAnchor.title = elAnchor.title + " [" + sKey + "]";' ).
lo_buf->add( '    });' ).
lo_buf->add( '' ).
lo_buf->add( '    // We replace the actions with callback functions to unify' ).
lo_buf->add( '    // the hotkey execution' ).
lo_buf->add( '    this.oKeyMap[sKey] = function(oEvent) {' ).
lo_buf->add( '' ).
lo_buf->add( '      // gHelper is only valid for diff page' ).
lo_buf->add( '      var diffHelper = (window.gHelper || {});' ).
lo_buf->add( '' ).
lo_buf->add( '      // We have either a js function on this' ).
lo_buf->add( '      if (this[action]) {' ).
lo_buf->add( '        this[action].call(this);' ).
lo_buf->add( '        return;' ).
lo_buf->add( '      }' ).
lo_buf->add( '' ).
lo_buf->add( '      // Or a method of the helper object for the diff page' ).
lo_buf->add( '      if (diffHelper[action]) {' ).
lo_buf->add( '        diffHelper[action].call(diffHelper);' ).
lo_buf->add( '        return;' ).
lo_buf->add( '      }' ).
lo_buf->add( '' ).
lo_buf->add( '      // Or a global function' ).
lo_buf->add( '      if (window[action] && typeof (window[action]) === "function") {' ).
lo_buf->add( '        window[action].call(this);' ).
lo_buf->add( '        return;' ).
lo_buf->add( '      }' ).
lo_buf->add( '' ).
lo_buf->add( '      // Or a SAP event link' ).
lo_buf->add( '      var sUiSapEventHref = this.getSapEventHref(action);' ).
lo_buf->add( '      if (sUiSapEventHref) {' ).
lo_buf->add( '        submitSapeventForm({}, sUiSapEventHref, "post");' ).
lo_buf->add( '        oEvent.preventDefault();' ).
lo_buf->add( '        return;' ).
lo_buf->add( '      }' ).
lo_buf->add( '' ).
lo_buf->add( '      // Or a SAP event input' ).
lo_buf->add( '      var sUiSapEventInputAction = this.getSapEventInputAction(action);' ).
lo_buf->add( '      if (sUiSapEventInputAction) {' ).
lo_buf->add( '        submitSapeventForm({}, sUiSapEventInputAction, "post");' ).
lo_buf->add( '        oEvent.preventDefault();' ).
lo_buf->add( '        return;' ).
lo_buf->add( '      }' ).
lo_buf->add( '' ).
lo_buf->add( '      // Or a SAP event main form' ).
lo_buf->add( '      var elForm = this.getSapEventForm(action);' ).
lo_buf->add( '      if (elForm) {' ).
lo_buf->add( '        elForm.submit();' ).
lo_buf->add( '        oEvent.preventDefault();' ).
lo_buf->add( '        return;' ).
lo_buf->add( '      }' ).
lo_buf->add( '' ).
lo_buf->add( '    };' ).
lo_buf->add( '' ).
lo_buf->add( '  }.bind(this));' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'Hotkeys.prototype.showHotkeys = function() {' ).
lo_buf->add( '  var elHotkeys = document.querySelector("#hotkeys");' ).
lo_buf->add( '' ).
lo_buf->add( '  if (elHotkeys) {' ).
lo_buf->add( '    elHotkeys.style.display = (elHotkeys.style.display) ? "" : "none";' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Hotkeys.prototype.getAllSapEventsForSapEventName = function (sSapEvent) {' ).
lo_buf->add( '  if (/^#+$/.test(sSapEvent)){' ).
lo_buf->add( '    // sSapEvent contains only #. Nothing sensible can be done here' ).
lo_buf->add( '    return [];' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  var includesSapEvent = function(text){' ).
lo_buf->add( '    return (text.includes("sapevent") || text.includes("SAPEVENT"));' ).
lo_buf->add( '  };' ).
lo_buf->add( '' ).
lo_buf->add( '  return [].slice' ).
lo_buf->add( '    .call(document.querySelectorAll("a[href*="+ sSapEvent +"], input[formaction*="+ sSapEvent+"]"))' ).
lo_buf->add( '    .filter(function (elem) {' ).
lo_buf->add( '      return (elem.nodeName === "A" && includesSapEvent(elem.href)' ).
lo_buf->add( '          || (elem.nodeName === "INPUT" && includesSapEvent(elem.formAction)));' ).
lo_buf->add( '    });' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Hotkeys.prototype.getSapEventHref = function(sSapEvent) {' ).
lo_buf->add( '  return this.getAllSapEventsForSapEventName(sSapEvent)' ).
lo_buf->add( '    .filter(function(el) {' ).
lo_buf->add( '      // only anchors' ).
lo_buf->add( '      return (!!el.href);' ).
lo_buf->add( '    })' ).
lo_buf->add( '    .map(function(oSapEvent) {' ).
lo_buf->add( '      return oSapEvent.href;' ).
lo_buf->add( '    })' ).
lo_buf->add( '    .filter(this.eliminateSapEventFalsePositives(sSapEvent))' ).
lo_buf->add( '    .pop();' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Hotkeys.prototype.getSapEventInputAction = function(sSapEvent) {' ).
lo_buf->add( '  return this.getAllSapEventsForSapEventName(sSapEvent)' ).
lo_buf->add( '    .filter(function(el) {' ).
lo_buf->add( '      // input forms' ).
lo_buf->add( '      return (el.type === "submit");' ).
lo_buf->add( '    })' ).
lo_buf->add( '    .map(function(oSapEvent) {' ).
lo_buf->add( '      return oSapEvent.formAction;' ).
lo_buf->add( '    })' ).
lo_buf->add( '    .filter(this.eliminateSapEventFalsePositives(sSapEvent))' ).
lo_buf->add( '    .pop();' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Hotkeys.prototype.getSapEventForm = function(sSapEvent) {' ).
lo_buf->add( '  return this.getAllSapEventsForSapEventName(sSapEvent)' ).
lo_buf->add( '    .filter(function(el) {' ).
lo_buf->add( '      // forms' ).
lo_buf->add( '      var parentForm = el.parentNode.parentNode.parentNode;' ).
lo_buf->add( '      return (el.type === "submit" && parentForm.nodeName === "FORM");' ).
lo_buf->add( '    })' ).
lo_buf->add( '    .map(function(oSapEvent) {' ).
lo_buf->add( '      return oSapEvent.parentNode.parentNode.parentNode;' ).
lo_buf->add( '    })' ).
lo_buf->add( '    .pop();' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Hotkeys.prototype.eliminateSapEventFalsePositives = function(sapEvent) {' ).
lo_buf->add( '  return function(sapEventAttr) {' ).
lo_buf->add( '    return sapEventAttr.match(new RegExp("\\b" + sapEvent + "\\b"));' ).
lo_buf->add( '  };' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Hotkeys.prototype.onkeydown = function(oEvent) {' ).
lo_buf->add( '  if (oEvent.defaultPrevented) {' ).
lo_buf->add( '    return;' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  if (!Hotkeys.isHotkeyCallPossible()) {' ).
lo_buf->add( '    return;' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  var' ).
lo_buf->add( '    sKey     = oEvent.key || String.fromCharCode(oEvent.keyCode),' ).
lo_buf->add( '    fnHotkey = this.oKeyMap[sKey];' ).
lo_buf->add( '' ).
lo_buf->add( '  if (fnHotkey) {' ).
lo_buf->add( '    fnHotkey.call(this, oEvent);' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Hotkeys.isHotkeyCallPossible = function() {' ).
lo_buf->add( '  var activeElementType     = ((document.activeElement && document.activeElement.nodeName) || "");' ).
lo_buf->add( '  var activeElementReadOnly = ((document.activeElement && document.activeElement.readOnly) || false);' ).
lo_buf->add( '' ).
lo_buf->add( '  return (activeElementReadOnly || (activeElementType !== "INPUT" && activeElementType !== "TEXTAREA"));' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Hotkeys.addHotkeyToHelpSheet = function(key, description) {' ).
lo_buf->add( '  var hotkeysUl = document.querySelector("#hotkeys ul.hotkeys");' ).
lo_buf->add( '  if (!hotkeysUl) return;' ).
lo_buf->add( '' ).
lo_buf->add( '  var li        = document.createElement("li");' ).
lo_buf->add( '  var spanId    = document.createElement("span");' ).
lo_buf->add( '  var spanDescr = document.createElement("span");' ).
lo_buf->add( '' ).
lo_buf->add( '  spanId.className    = "key-id";' ).
lo_buf->add( '  spanId.innerText    = key;' ).
lo_buf->add( '  spanDescr.className = "key-descr";' ).
lo_buf->add( '  spanDescr.innerText = description;' ).
lo_buf->add( '  li.appendChild(spanId);' ).
lo_buf->add( '  li.appendChild(spanDescr);' ).
lo_buf->add( '' ).
lo_buf->add( '  hotkeysUl.appendChild(li);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'function setKeyBindings(oKeyMap) {' ).
lo_buf->add( '  var oHotkeys = new Hotkeys(oKeyMap);' ).
lo_buf->add( '' ).
lo_buf->add( '  document.addEventListener("keypress", oHotkeys.onkeydown.bind(oHotkeys));' ).
lo_buf->add( '  setTimeout(function() {' ).
lo_buf->add( '    var div                     = document.getElementById("hotkeys-hint");' ).
lo_buf->add( '    if  (div) div.style.opacity = 0.2;' ).
lo_buf->add( '  }, 4900);' ).
lo_buf->add( '  setTimeout(function() { toggleDisplay("hotkeys-hint") }, 5000);' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/**********************************************************' ).
lo_buf->add( ' * Patch Logic (git add -p)' ).
lo_buf->add( ' **********************************************************/' ).
lo_buf->add( '' ).
lo_buf->add( '/*' ).
lo_buf->add( '  We have three type of cascading checkboxes.' ).
lo_buf->add( '  Which means that by clicking a file or section checkbox all corresponding line checkboxes are checked.' ).
lo_buf->add( '' ).
lo_buf->add( '  The id of the checkbox indicates its semantics and its membership.' ).
lo_buf->add( '*/' ).
lo_buf->add( '' ).
lo_buf->add( '/*' ).
lo_buf->add( '  1) file links' ).
lo_buf->add( '' ).
lo_buf->add( '      example id of file link' ).
lo_buf->add( '' ).
lo_buf->add( '      patch_file_zcl_abapgit_user_exit.clas.abap' ).
lo_buf->add( '      \________/ \_____________________________/' ).
lo_buf->add( '          |                   |' ).
lo_buf->add( '          |                   |____ file name' ).
lo_buf->add( '          |' ).
lo_buf->add( '          |' ).
lo_buf->add( '          |' ).
lo_buf->add( '      constant prefix' ).
lo_buf->add( '*/' ).
lo_buf->add( '' ).
lo_buf->add( 'function PatchFile(sId) {' ).
lo_buf->add( '  var oRegex = new RegExp("(" + this.ID + ")_(.*$)");' ).
lo_buf->add( '  var oMatch = sId.match(oRegex);' ).
lo_buf->add( '' ).
lo_buf->add( '  this.id        = sId;' ).
lo_buf->add( '  this.prefix    = oMatch[1];' ).
lo_buf->add( '  this.file_name = oMatch[2];' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'PatchFile.prototype.ID = "patch_file";' ).
lo_buf->add( '' ).
lo_buf->add( '/*' ).
lo_buf->add( '  2) section links within a file' ).
lo_buf->add( '' ).
lo_buf->add( '      example id of section link' ).
lo_buf->add( '' ).
lo_buf->add( '      patch_section_zcl_abapgit_user_exit.clas.abap_1' ).
lo_buf->add( '      \___________/ \_____________________________/ ^' ).
lo_buf->add( '            |                   |                   |' ).
lo_buf->add( '            |               file name               |' ).
lo_buf->add( '            |                                       |' ).
lo_buf->add( '            |                                       ------ section' ).
lo_buf->add( '            |' ).
lo_buf->add( '      constant prefix' ).
lo_buf->add( '*/' ).
lo_buf->add( '' ).
lo_buf->add( 'function PatchSection(sId) {' ).
lo_buf->add( '  var oRegex = new RegExp("(" + this.ID + ")_(.*)_(\\d+$)");' ).
lo_buf->add( '  var oMatch = sId.match(oRegex);' ).
lo_buf->add( '' ).
lo_buf->add( '  this.id        = sId;' ).
lo_buf->add( '  this.prefix    = oMatch[1];' ).
lo_buf->add( '  this.file_name = oMatch[2];' ).
lo_buf->add( '  this.section   = oMatch[3];' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'PatchSection.prototype.ID = "patch_section";' ).
lo_buf->add( '' ).
lo_buf->add( '/*' ).
lo_buf->add( '  3) line links within a section' ).
lo_buf->add( '' ).
lo_buf->add( '      example id of line link' ).
lo_buf->add( '' ).
lo_buf->add( '      patch_line_zcl_abapgit_user_exit.clas.abap_1_25' ).
lo_buf->add( '      \________/ \_____________________________/ ^  ^' ).
lo_buf->add( '            ^                  ^                 |  |' ).
lo_buf->add( '            |                  |                 |  ------- line number' ).
lo_buf->add( '            |               file name            |' ).
lo_buf->add( '            |                                 section' ).
lo_buf->add( '            |' ).
lo_buf->add( '            |' ).
lo_buf->add( '      constant prefix' ).
lo_buf->add( '*/' ).
lo_buf->add( '' ).
lo_buf->add( 'function PatchLine() { }' ).
lo_buf->add( '' ).
lo_buf->add( 'PatchLine.prototype.ID = "patch_line";' ).
lo_buf->add( '' ).
lo_buf->add( 'function Patch() { }' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.ID = {' ).
lo_buf->add( '  STAGE: "stage"' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.ACTION = {' ).
lo_buf->add( '  PATCH_STAGE  : "patch_stage",' ).
lo_buf->add( '  REFRESH_LOCAL: "refresh_local",' ).
lo_buf->add( '  REFRESH_ALL  : "refresh_all"' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.escape = function(sFileName) {' ).
lo_buf->add( '  return sFileName' ).
lo_buf->add( '    .replace(/\./g, "\\.")' ).
lo_buf->add( '    .replace(/#/g, "\\#");' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.preparePatch = function() {' ).
lo_buf->add( '  this.registerClickHandlerForFiles();' ).
lo_buf->add( '  this.registerClickHandlerForSections();' ).
lo_buf->add( '  this.registerClickHandlerForLines();' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.buildSelectorInputStartsWithId = function(sId) {' ).
lo_buf->add( '  return "input[id^=''" + sId + "'']";' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.registerClickHandlerForFiles = function() {' ).
lo_buf->add( '  this.registerClickHandlerForSelectorParent(this.buildSelectorInputStartsWithId(PatchFile.prototype.ID), this.onClickFileCheckbox);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.registerClickHandlerForSections = function() {' ).
lo_buf->add( '  this.registerClickHandlerForSelectorParent(this.buildSelectorInputStartsWithId(PatchSection.prototype.ID), this.onClickSectionCheckbox);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.registerClickHandlerForLines = function() {' ).
lo_buf->add( '  this.registerClickHandlerForSelectorParent(this.buildSelectorInputStartsWithId(PatchLine.prototype.ID), this.onClickLineCheckbox);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.registerClickHandlerForSelectorParent = function(sSelector, fnCallback) {' ).
lo_buf->add( '  var elAll = document.querySelectorAll(sSelector);' ).
lo_buf->add( '' ).
lo_buf->add( '  [].forEach.call(elAll, function(elem) {' ).
lo_buf->add( '    elem.parentElement.addEventListener("click", fnCallback.bind(this));' ).
lo_buf->add( '  }.bind(this));' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.getAllLineCheckboxesForFile = function(oFile) {' ).
lo_buf->add( '  return this.getAllLineCheckboxesForId(oFile.id, PatchFile.prototype.ID);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.getAllSectionCheckboxesForFile = function(oFile) {' ).
lo_buf->add( '  return this.getAllSectionCheckboxesForId(oFile.id, PatchFile.prototype.ID);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.getAllLineCheckboxesForSection = function(oSection) {' ).
lo_buf->add( '  return this.getAllLineCheckboxesForId(oSection.id, PatchSection.prototype.ID);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.getAllLineCheckboxesForId = function(sId, sIdPrefix) {' ).
lo_buf->add( '  return this.getAllCheckboxesForId(sId, sIdPrefix, PatchLine.prototype.ID);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.getAllSectionCheckboxesForId = function(sId, sIdPrefix) {' ).
lo_buf->add( '  return this.getAllCheckboxesForId(sId, sIdPrefix, PatchSection.prototype.ID);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.getAllCheckboxesForId = function(sId, sIdPrefix, sNewIdPrefix) {' ).
lo_buf->add( '  var oRegex = new RegExp("^" + sIdPrefix);' ).
lo_buf->add( '' ).
lo_buf->add( '  sId = sId.replace(oRegex, sNewIdPrefix);' ).
lo_buf->add( '  return document.querySelectorAll(this.buildSelectorInputStartsWithId(this.escape(sId)));' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.getToggledCheckbox = function(oEvent) {' ).
lo_buf->add( '  var elCheckbox = null;' ).
lo_buf->add( '' ).
lo_buf->add( '  // We have either an input element or any element with input child' ).
lo_buf->add( '  // in the latter case we have to toggle the checkbox manually' ).
lo_buf->add( '  if (oEvent.srcElement.nodeName === "INPUT") {' ).
lo_buf->add( '    elCheckbox = oEvent.srcElement;' ).
lo_buf->add( '  } else {' ).
lo_buf->add( '    elCheckbox = this.toggleCheckbox(oEvent.srcElement.querySelector("INPUT"));' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  return elCheckbox;' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.toggleCheckbox = function(elCheckbox) {' ).
lo_buf->add( '  elCheckbox.checked = !elCheckbox.checked;' ).
lo_buf->add( '  return elCheckbox;' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.onClickFileCheckbox = function(oEvent) {' ).
lo_buf->add( '  var elCheckbox                   = this.getToggledCheckbox(oEvent);' ).
lo_buf->add( '  var oFile                        = new PatchFile(elCheckbox.id);' ).
lo_buf->add( '  var elAllLineCheckboxesOfFile    = this.getAllLineCheckboxesForFile(oFile);' ).
lo_buf->add( '  var elAllSectionCheckboxesOfFile = this.getAllSectionCheckboxesForFile(oFile);' ).
lo_buf->add( '' ).
lo_buf->add( '  [].forEach.call(elAllLineCheckboxesOfFile, function(elem) {' ).
lo_buf->add( '    elem.checked = elCheckbox.checked;' ).
lo_buf->add( '  }.bind(this));' ).
lo_buf->add( '' ).
lo_buf->add( '  [].forEach.call(elAllSectionCheckboxesOfFile, function(elem) {' ).
lo_buf->add( '    elem.checked = elCheckbox.checked;' ).
lo_buf->add( '  }.bind(this));' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.onClickSectionCheckbox = function(oEvent) {' ).
lo_buf->add( '  var elSrcElement = this.getToggledCheckbox(oEvent);' ).
lo_buf->add( '  var oSection     = new PatchSection(elSrcElement.id);' ).
lo_buf->add( '  this.clickAllLineCheckboxesInSection(oSection, elSrcElement.checked);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.onClickLineCheckbox = function(oEvent) {' ).
lo_buf->add( '  this.getToggledCheckbox(oEvent);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.clickAllLineCheckboxesInSection = function(oSection, bChecked) {' ).
lo_buf->add( '  var elAllLineCheckboxesOfSection = this.getAllLineCheckboxesForSection(oSection);' ).
lo_buf->add( '' ).
lo_buf->add( '  [].forEach.call(elAllLineCheckboxesOfSection, function(elem) {' ).
lo_buf->add( '    elem.checked = bChecked;' ).
lo_buf->add( '  }.bind(this));' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.registerStagePatch = function() {' ).
lo_buf->add( '  var elStage        = document.querySelector("#" + this.ID.STAGE);' ).
lo_buf->add( '  var REFRESH_PREFIX = "refresh";' ).
lo_buf->add( '' ).
lo_buf->add( '  elStage.addEventListener("click", this.submitPatch.bind(this, this.ACTION.PATCH_STAGE));' ).
lo_buf->add( '' ).
lo_buf->add( '  var aRefresh = document.querySelectorAll("[id*=" + REFRESH_PREFIX + "]");' ).
lo_buf->add( '  [].forEach.call(aRefresh, function(el) {' ).
lo_buf->add( '    el.addEventListener("click", memorizeScrollPosition(this.submitPatch.bind(this, el.id)).bind(this));' ).
lo_buf->add( '  }.bind(this));' ).
lo_buf->add( '' ).
lo_buf->add( '  // for hotkeys' ).
lo_buf->add( '  window.stagePatch = function() {' ).
lo_buf->add( '    this.submitPatch(this.ACTION.PATCH_STAGE);' ).
lo_buf->add( '  }.bind(this);' ).
lo_buf->add( '' ).
lo_buf->add( '  window.refreshLocal = memorizeScrollPosition(function() {' ).
lo_buf->add( '    this.submitPatch(this.ACTION.REFRESH_LOCAL);' ).
lo_buf->add( '  }.bind(this));' ).
lo_buf->add( '' ).
lo_buf->add( '  window.refreshAll = memorizeScrollPosition(function() {' ).
lo_buf->add( '    this.submitPatch(this.ACTION.REFRESH_ALL);' ).
lo_buf->add( '  }.bind(this));' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.submitPatch = function(action) {' ).
lo_buf->add( '  // Collect add and remove info and submit to backend' ).
lo_buf->add( '  var aAddPatch    = this.collectElementsForCheckboxId(PatchLine.prototype.ID, true);' ).
lo_buf->add( '  var aRemovePatch = this.collectElementsForCheckboxId(PatchLine.prototype.ID, false);' ).
lo_buf->add( '' ).
lo_buf->add( '  submitSapeventForm({ add: aAddPatch, remove: aRemovePatch }, action, "post");' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.collectElementsForCheckboxId = function(sId, bChecked) {' ).
lo_buf->add( '  var sSelector = this.buildSelectorInputStartsWithId(sId);' ).
lo_buf->add( '' ).
lo_buf->add( '  return [].slice.call(document.querySelectorAll(sSelector))' ).
lo_buf->add( '    .filter(function(elem) {' ).
lo_buf->add( '      return (elem.checked === bChecked);' ).
lo_buf->add( '    }).map(function(elem) {' ).
lo_buf->add( '      return elem.id;' ).
lo_buf->add( '    });' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'function preparePatch() {' ).
lo_buf->add( '  var oPatch = new Patch();' ).
lo_buf->add( '  oPatch.preparePatch();' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'function registerStagePatch() {' ).
lo_buf->add( '  var oPatch = new Patch();' ).
lo_buf->add( '  oPatch.registerStagePatch();' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/**********************************************************' ).
lo_buf->add( ' * Command Palette (Ctrl + P)' ).
lo_buf->add( ' **********************************************************/' ).
lo_buf->add( '' ).
lo_buf->add( '// fuzzy match helper' ).
lo_buf->add( '// return non empty marked string in case it fits the filter' ).
lo_buf->add( '// abc + b = a<mark>b</mark>c' ).
lo_buf->add( 'function fuzzyMatchAndMark(str, filter) {' ).
lo_buf->add( '  var markedStr   = "";' ).
lo_buf->add( '  var filterLower = filter.toLowerCase();' ).
lo_buf->add( '  var strLower    = str.toLowerCase();' ).
lo_buf->add( '  var cur         = 0;' ).
lo_buf->add( '' ).
lo_buf->add( '  for (var i = 0; i < filter.length; i++) {' ).
lo_buf->add( '    while (filterLower[i] !== strLower[cur] && cur < str.length) {' ).
lo_buf->add( '      markedStr += str[cur++];' ).
lo_buf->add( '    }' ).
lo_buf->add( '    if (cur === str.length) break;' ).
lo_buf->add( '    markedStr += "<mark>" + str[cur++] + "</mark>";' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  var matched = i === filter.length;' ).
lo_buf->add( '' ).
lo_buf->add( '  if (matched && cur < str.length) markedStr += str.substring(cur);' ).
lo_buf->add( '  return matched ? markedStr: null;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'function CommandPalette(commandEnumerator, opts) {' ).
lo_buf->add( '  if (typeof commandEnumerator !== "function") throw Error("commandEnumerator must be a function");' ).
lo_buf->add( '  if (typeof opts !== "object") throw Error("opts must be an object");' ).
lo_buf->add( '  if (typeof opts.toggleKey !== "string" || !opts.toggleKey) throw Error("toggleKey must be a string");' ).
lo_buf->add( '  this.commands = commandEnumerator();' ).
lo_buf->add( '  if (!this.commands) return;' ).
lo_buf->add( '  // this.commands = [{' ).
lo_buf->add( '  //   action:    "sap_event_action_code_with_params"' ).
lo_buf->add( '  //   iconClass: "icon icon_x ..."' ).
lo_buf->add( '  //   title:     "my command X"' ).
lo_buf->add( '  // }, ...];' ).
lo_buf->add( '' ).
lo_buf->add( '  if (opts.toggleKey[0] === "^") {' ).
lo_buf->add( '    this.toggleKeyCtrl = true;' ).
lo_buf->add( '    this.toggleKey     = opts.toggleKey.substring(1);' ).
lo_buf->add( '    if (!this.toggleKey) throw Error("Incorrect toggleKey");' ).
lo_buf->add( '  } else {' ).
lo_buf->add( '    this.toggleKeyCtrl = false;' ).
lo_buf->add( '    this.toggleKey     = opts.toggleKey;' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  this.hotkeyDescription = opts.hotkeyDescription;' ).
lo_buf->add( '  this.elements          = {' ).
lo_buf->add( '    palette: null,' ).
lo_buf->add( '    ul     : null,' ).
lo_buf->add( '    input  : null' ).
lo_buf->add( '  };' ).
lo_buf->add( '  this.selectIndex = -1; // not selected' ).
lo_buf->add( '  this.filter      = "";' ).
lo_buf->add( '  this.renderAndBindElements();' ).
lo_buf->add( '  this.hookEvents();' ).
lo_buf->add( '  Hotkeys.addHotkeyToHelpSheet(opts.toggleKey, opts.hotkeyDescription);' ).
lo_buf->add( '' ).
lo_buf->add( '  if (!CommandPalette.instances) {' ).
lo_buf->add( '    CommandPalette.instances = [];' ).
lo_buf->add( '  }' ).
lo_buf->add( '  CommandPalette.instances.push(this);' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.hookEvents = function() {' ).
lo_buf->add( '  document.addEventListener("keydown", this.handleToggleKey.bind(this));' ).
lo_buf->add( '  this.elements.input.addEventListener("keyup", this.handleInputKey.bind(this));' ).
lo_buf->add( '  this.elements.ul.addEventListener("click", this.handleUlClick.bind(this));' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.renderCommandItem = function(cmd) {' ).
lo_buf->add( '  var li = document.createElement("li");' ).
lo_buf->add( '  if (cmd.iconClass) {' ).
lo_buf->add( '    var icon = document.createElement("i");' ).
lo_buf->add( '' ).
lo_buf->add( '    icon.className = cmd.iconClass;' ).
lo_buf->add( '    li.appendChild(icon);' ).
lo_buf->add( '  }' ).
lo_buf->add( '  var titleSpan = document.createElement("span");' ).
lo_buf->add( '  li.appendChild(titleSpan);' ).
lo_buf->add( '  cmd.element   = li;' ).
lo_buf->add( '  cmd.titleSpan = titleSpan;' ).
lo_buf->add( '  return li;' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.renderAndBindElements = function() {' ).
lo_buf->add( '  var div   = document.createElement("div");' ).
lo_buf->add( '  var input = document.createElement("input");' ).
lo_buf->add( '  var ul    = document.createElement("ul");' ).
lo_buf->add( '' ).
lo_buf->add( '  div.className     = "cmd-palette";' ).
lo_buf->add( '  div.style.display = "none";' ).
lo_buf->add( '  input.placeholder = this.hotkeyDescription;' ).
lo_buf->add( '  for (var i = 0; i < this.commands.length; i++) ul.appendChild(this.renderCommandItem(this.commands[i]));' ).
lo_buf->add( '  div.appendChild(input);' ).
lo_buf->add( '  div.appendChild(ul);' ).
lo_buf->add( '' ).
lo_buf->add( '  this.elements.palette = div;' ).
lo_buf->add( '  this.elements.input   = input;' ).
lo_buf->add( '  this.elements.ul      = ul;' ).
lo_buf->add( '  document.body.appendChild(div);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.handleToggleKey = function(event) {' ).
lo_buf->add( '  if (event.key !== this.toggleKey) return;' ).
lo_buf->add( '  if (this.toggleKeyCtrl && !event.ctrlKey) return;' ).
lo_buf->add( '  this.toggleDisplay();' ).
lo_buf->add( '  event.preventDefault();' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.handleInputKey = function(event) {' ).
lo_buf->add( '  if (event.key === "ArrowUp" || event.key === "Up") {' ).
lo_buf->add( '    this.selectPrev();' ).
lo_buf->add( '  } else if (event.key === "ArrowDown" || event.key === "Down") {' ).
lo_buf->add( '    this.selectNext();' ).
lo_buf->add( '  } else if (event.key === "Enter") {' ).
lo_buf->add( '    this.exec(this.getSelected());' ).
lo_buf->add( '  } else if (event.key === "Backspace" && !this.filter) {' ).
lo_buf->add( '    this.toggleDisplay(false);' ).
lo_buf->add( '  } else if (this.filter !== this.elements.input.value) {' ).
lo_buf->add( '    this.filter = this.elements.input.value;' ).
lo_buf->add( '    this.applyFilter();' ).
lo_buf->add( '    this.selectFirst();' ).
lo_buf->add( '  }' ).
lo_buf->add( '  event.preventDefault();' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.applyFilter = function() {' ).
lo_buf->add( '  for (var i = 0; i < this.commands.length; i++) {' ).
lo_buf->add( '    var cmd = this.commands[i];' ).
lo_buf->add( '    if (!this.filter) {' ).
lo_buf->add( '      cmd.element.style.display = "";' ).
lo_buf->add( '      cmd.titleSpan.innerText   = cmd.title;' ).
lo_buf->add( '    } else {' ).
lo_buf->add( '      var matchedTitle = fuzzyMatchAndMark(cmd.title, this.filter);' ).
lo_buf->add( '      if (matchedTitle) {' ).
lo_buf->add( '        cmd.titleSpan.innerHTML   = matchedTitle;' ).
lo_buf->add( '        cmd.element.style.display = "";' ).
lo_buf->add( '      } else {' ).
lo_buf->add( '        cmd.element.style.display = "none";' ).
lo_buf->add( '      }' ).
lo_buf->add( '    }' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.applySelectIndex = function(newIndex) {' ).
lo_buf->add( '  if (newIndex !== this.selectIndex) {' ).
lo_buf->add( '    if (this.selectIndex >= 0) this.commands[this.selectIndex].element.classList.remove("selected");' ).
lo_buf->add( '    var newCmd = this.commands[newIndex];' ).
lo_buf->add( '    newCmd.element.classList.add("selected");' ).
lo_buf->add( '    this.selectIndex = newIndex;' ).
lo_buf->add( '    this.adjustScrollPosition(newCmd.element);' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.selectFirst = function() {' ).
lo_buf->add( '  for (var i = 0; i < this.commands.length; i++) {' ).
lo_buf->add( '    if (this.commands[i].element.style.display === "none") continue; // skip hidden' ).
lo_buf->add( '    this.applySelectIndex(i);' ).
lo_buf->add( '    break;' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.selectNext = function() {' ).
lo_buf->add( '  for (var i = this.selectIndex + 1; i < this.commands.length; i++) {' ).
lo_buf->add( '    if (this.commands[i].element.style.display === "none") continue; // skip hidden' ).
lo_buf->add( '    this.applySelectIndex(i);' ).
lo_buf->add( '    break;' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.selectPrev = function() {' ).
lo_buf->add( '  for (var i = this.selectIndex - 1; i >= 0; i--) {' ).
lo_buf->add( '    if (this.commands[i].element.style.display === "none") continue; // skip hidden' ).
lo_buf->add( '    this.applySelectIndex(i);' ).
lo_buf->add( '    break;' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.getSelected = function() {' ).
lo_buf->add( '  return this.commands[this.selectIndex];' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.adjustScrollPosition = function(itemElement) {' ).
lo_buf->add( '  var bItem      = itemElement.getBoundingClientRect();' ).
lo_buf->add( '  var bContainer = this.elements.ul.getBoundingClientRect();' ).
lo_buf->add( '' ).
lo_buf->add( '  bItem.top         = Math.round(bItem.top);' ).
lo_buf->add( '  bItem.bottom      = Math.round(bItem.bottom);' ).
lo_buf->add( '  bItem.height      = Math.round(bItem.height);' ).
lo_buf->add( '  bItem.mid         = Math.round(bItem.top + bItem.height / 2);' ).
lo_buf->add( '  bContainer.top    = Math.round(bContainer.top);' ).
lo_buf->add( '  bContainer.bottom = Math.round(bContainer.bottom);' ).
lo_buf->add( '' ).
lo_buf->add( '  if (bItem.mid > bContainer.bottom - 2) {' ).
lo_buf->add( '    this.elements.ul.scrollTop += bItem.bottom - bContainer.bottom;' ).
lo_buf->add( '  } else if (bItem.mid < bContainer.top + 2) {' ).
lo_buf->add( '    this.elements.ul.scrollTop += bItem.top - bContainer.top;' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.toggleDisplay = function(forceState) {' ).
lo_buf->add( '  var isDisplayed   = (this.elements.palette.style.display !== "none");' ).
lo_buf->add( '  var tobeDisplayed = (forceState !== undefined) ? forceState : !isDisplayed;' ).
lo_buf->add( '' ).
lo_buf->add( '  if (tobeDisplayed) {' ).
lo_buf->add( '    // auto close other command palettes' ).
lo_buf->add( '    CommandPalette.instances.forEach(function(instance) {' ).
lo_buf->add( '      instance.elements.palette.style.display = "none";' ).
lo_buf->add( '    });' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  this.elements.palette.style.display = tobeDisplayed ? "" : "none";' ).
lo_buf->add( '  if (tobeDisplayed) {' ).
lo_buf->add( '    this.elements.input.value = "";' ).
lo_buf->add( '    this.elements.input.focus();' ).
lo_buf->add( '    this.applyFilter();' ).
lo_buf->add( '    this.selectFirst();' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.getCommandByElement = function(element) {' ).
lo_buf->add( '  for (var i = 0; i < this.commands.length; i++) {' ).
lo_buf->add( '    if (this.commands[i].element === element) return this.commands[i];' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.handleUlClick = function(event) {' ).
lo_buf->add( '  var element = event.target || event.srcElement;' ).
lo_buf->add( '  if (!element) return;' ).
lo_buf->add( '  if (element.nodeName === "SPAN") element = element.parentNode;' ).
lo_buf->add( '' ).
lo_buf->add( '  if (element.nodeName === "I") element = element.parentNode;' ).
lo_buf->add( '' ).
lo_buf->add( '  if (element.nodeName !== "LI") return;' ).
lo_buf->add( '  this.exec(this.getCommandByElement(element));' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.exec = function(cmd) {' ).
lo_buf->add( '  if (!cmd) return;' ).
lo_buf->add( '  this.toggleDisplay(false);' ).
lo_buf->add( '  if (typeof cmd.action === "function") {' ).
lo_buf->add( '    cmd.action();' ).
lo_buf->add( '  } else {' ).
lo_buf->add( '    submitSapeventForm(null, cmd.action);' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '// Is any command palette visible?' ).
lo_buf->add( 'CommandPalette.isVisible = function() {' ).
lo_buf->add( '  return CommandPalette.instances.reduce(function(result, instance) { return result || instance.elements.palette.style.display !== "none" }, false);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '/**********************************************************' ).
lo_buf->add( ' * Command Enumerators' ).
lo_buf->add( ' **********************************************************/' ).
lo_buf->add( '' ).
lo_buf->add( 'function createRepoCatalogEnumerator(catalog, action) {' ).
lo_buf->add( '  // expecting [{ key, isOffline, displayName }]' ).
lo_buf->add( '  return function() {' ).
lo_buf->add( '    return catalog.map(function(i) {' ).
lo_buf->add( '      return {' ).
lo_buf->add( '        action   : action + "?key=" + i.key,' ).
lo_buf->add( '        iconClass: i.isOffline ? "icon icon-plug darkgrey" : "icon icon-cloud-upload-alt blue",' ).
lo_buf->add( '        title    : i.displayName' ).
lo_buf->add( '      };' ).
lo_buf->add( '    });' ).
lo_buf->add( '  };' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'function enumerateUiActions() {' ).
lo_buf->add( '  var items = [];' ).
lo_buf->add( '  function processUL(ulNode, prefix) {' ).
lo_buf->add( '    for (var i = 0; i < ulNode.children.length; i++) {' ).
lo_buf->add( '      var item = ulNode.children[i];' ).
lo_buf->add( '      if (item.nodeName !== "LI") continue; // unexpected node' ).
lo_buf->add( '      if (item.children.length >= 2 && item.children[1].nodeName === "UL") {' ).
lo_buf->add( '        // submenu detected' ).
lo_buf->add( '        var menutext = item.children[0].innerText;' ).
lo_buf->add( '        // special treatment for menus without text' ).
lo_buf->add( '        if (!menutext) {' ).
lo_buf->add( '          menutext = item.children[0].getAttribute("title");' ).
lo_buf->add( '        }' ).
lo_buf->add( '        processUL(item.children[1], menutext);' ).
lo_buf->add( '      } else if (item.firstElementChild && item.firstElementChild.nodeName === "A") {' ).
lo_buf->add( '        var anchor = item.firstElementChild;' ).
lo_buf->add( '        if (anchor.href && anchor.href !== "#") items.push([anchor, prefix]);' ).
lo_buf->add( '      }' ).
lo_buf->add( '    }' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  // toolbars' ).
lo_buf->add( '  [].slice.call(document.querySelectorAll("[id*=toolbar]"))' ).
lo_buf->add( '    .filter(function(toolbar) {' ).
lo_buf->add( '      return (toolbar && toolbar.nodeName === "UL");' ).
lo_buf->add( '    }).forEach(function(toolbar) {' ).
lo_buf->add( '      processUL(toolbar);' ).
lo_buf->add( '    });' ).
lo_buf->add( '' ).
lo_buf->add( '  items = items.map(function(item) {' ).
lo_buf->add( '    var action = "";' ).
lo_buf->add( '    var anchor = item[0];' ).
lo_buf->add( '    if (anchor.href.includes("#")) {' ).
lo_buf->add( '      action = function() {' ).
lo_buf->add( '        anchor.click();' ).
lo_buf->add( '      };' ).
lo_buf->add( '    } else {' ).
lo_buf->add( '      action = anchor.href.replace("sapevent:", "");' ).
lo_buf->add( '    }' ).
lo_buf->add( '    var prefix = item[1];' ).
lo_buf->add( '    return {' ).
lo_buf->add( '      action: action,' ).
lo_buf->add( '      title : (prefix ? prefix + ": " : "") + anchor.innerText.trim()' ).
lo_buf->add( '    };' ).
lo_buf->add( '  });' ).
lo_buf->add( '' ).
lo_buf->add( '  // forms' ).
lo_buf->add( '  [].slice.call(document.querySelectorAll("input[type=''submit'']"))' ).
lo_buf->add( '    .forEach(function(input) {' ).
lo_buf->add( '      items.push({' ).
lo_buf->add( '        action: function() {' ).
lo_buf->add( '          if (input.form.action.includes(input.formAction) || input.classList.contains("main")) {' ).
lo_buf->add( '            input.form.submit();' ).
lo_buf->add( '          } else {' ).
lo_buf->add( '            submitSapeventForm({}, input.formAction, "post", input.form);' ).
lo_buf->add( '          }' ).
lo_buf->add( '        },' ).
lo_buf->add( '        title: input.value + " " + input.title.replace(/\[.*\]/, "")' ).
lo_buf->add( '      });' ).
lo_buf->add( '    });' ).
lo_buf->add( '' ).
lo_buf->add( '  // radio buttons' ).
lo_buf->add( '  [].slice.call(document.querySelectorAll("input[type=''radio'']"))' ).
lo_buf->add( '    .forEach(function(input) {' ).
lo_buf->add( '      items.push({' ).
lo_buf->add( '        action: function() {' ).
lo_buf->add( '          input.click();' ).
lo_buf->add( '        },' ).
lo_buf->add( '        title: document.querySelector("label[for=''" + input.id + "'']").textContent' ).
lo_buf->add( '      });' ).
lo_buf->add( '    });' ).
lo_buf->add( '' ).
lo_buf->add( '  // others:' ).
lo_buf->add( '  // - links inside forms' ).
lo_buf->add( '  // - label links' ).
lo_buf->add( '  // - command links' ).
lo_buf->add( '  // - other header links' ).
lo_buf->add( '  [].slice.call(document.querySelectorAll("form a, a.command, #header ul:not([id*=''toolbar'']) a"))' ).
lo_buf->add( '    .filter(function(anchor) {' ).
lo_buf->add( '      return !!anchor.title || !!anchor.text;' ).
lo_buf->add( '    }).forEach(function(anchor) {' ).
lo_buf->add( '      items.push({' ).
lo_buf->add( '        action: function() {' ).
lo_buf->add( '          anchor.click();' ).
lo_buf->add( '        },' ).
lo_buf->add( '        title: (function() {' ).
lo_buf->add( '          var result = anchor.title + anchor.text;' ).
lo_buf->add( '          if (anchor.href.includes("label")) {' ).
lo_buf->add( '            result = "Label: " + result;' ).
lo_buf->add( '          }' ).
lo_buf->add( '          return result;' ).
lo_buf->add( '        })()' ).
lo_buf->add( '      });' ).
lo_buf->add( '    });' ).
lo_buf->add( '' ).
lo_buf->add( '  return items;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'function enumerateJumpAllFiles() {' ).
lo_buf->add( '  var root = document.getElementById("jump");' ).
lo_buf->add( '  if (!root || root.nodeName !== "UL") return null;' ).
lo_buf->add( '' ).
lo_buf->add( '  return Array' ).
lo_buf->add( '    .prototype.slice.call(root.children)' ).
lo_buf->add( '    .filter(function(elem) { return elem.nodeName === "LI" })' ).
lo_buf->add( '    .map(function(listItem) {' ).
lo_buf->add( '      var title = listItem.children[0].childNodes[0].textContent;' ).
lo_buf->add( '      return {' ).
lo_buf->add( '        action: root.onclick.bind(null, title),' ).
lo_buf->add( '        title : title' ).
lo_buf->add( '      };' ).
lo_buf->add( '    });' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/**********************************************************' ).
lo_buf->add( ' * Save Scroll Position' ).
lo_buf->add( ' **********************************************************/' ).
lo_buf->add( '' ).
lo_buf->add( 'function saveScrollPosition() {' ).
lo_buf->add( '  // Not supported by Java GUI' ).
lo_buf->add( '  try { if (!window.sessionStorage) { return } }' ).
lo_buf->add( '  catch (err) { return }' ).
lo_buf->add( '' ).
lo_buf->add( '  window.sessionStorage.setItem("scrollTop", document.querySelector("html").scrollTop);' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'function restoreScrollPosition() {' ).
lo_buf->add( '  // Not supported by Java GUI' ).
lo_buf->add( '  try { if (!window.sessionStorage) { return } }' ).
lo_buf->add( '  catch (err) { return }' ).
lo_buf->add( '' ).
lo_buf->add( '  var scrollTop = window.sessionStorage.getItem("scrollTop");' ).
lo_buf->add( '  if (scrollTop) {' ).
lo_buf->add( '    document.querySelector("html").scrollTop = scrollTop;' ).
lo_buf->add( '  }' ).
lo_buf->add( '  window.sessionStorage.setItem("scrollTop", 0);' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'function memorizeScrollPosition(fn) {' ).
lo_buf->add( '  return function() {' ).
lo_buf->add( '    saveScrollPosition();' ).
lo_buf->add( '    return fn.call(this, fn.args);' ).
lo_buf->add( '  }.bind(this);' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/**********************************************************' ).
lo_buf->add( ' * Sticky Header' ).
lo_buf->add( ' **********************************************************/' ).
lo_buf->add( '' ).
lo_buf->add( '/* https://www.w3schools.com/howto/howto_js_navbar_sticky.asp */' ).
lo_buf->add( '/* Note: We have to use JS since IE does not support CSS position:sticky */' ).
lo_buf->add( '' ).
lo_buf->add( '// When the user scrolls the page, execute toggleSticky' ).
lo_buf->add( 'window.onscroll = function() { toggleSticky() };' ).
lo_buf->add( '' ).
lo_buf->add( '// Add the sticky class to the navbar when you reach its scroll position.' ).
lo_buf->add( '// Remove "sticky" when you leave the scroll position' ).
lo_buf->add( 'function toggleSticky() {' ).
lo_buf->add( '  var body   = document.getElementsByTagName("body")[0];' ).
lo_buf->add( '  var header = document.getElementById("header");' ).
lo_buf->add( '  var sticky = header.offsetTop;' ).
lo_buf->add( '' ).
lo_buf->add( '  var stickyClass = "sticky";' ).
lo_buf->add( '  if (body.classList.contains("full_width")) {' ).
lo_buf->add( '    stickyClass = "sticky_full_width";' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  if (window.pageYOffset >= sticky) {' ).
lo_buf->add( '    header.classList.add(stickyClass);' ).
lo_buf->add( '  } else {' ).
lo_buf->add( '    header.classList.remove(stickyClass);' ).
lo_buf->add( '  }' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/**********************************************************' ).
lo_buf->add( ' * Browser Control' ).
lo_buf->add( ' **********************************************************/' ).
lo_buf->add( '' ).
lo_buf->add( '// Toggle display of warning message when using Edge (based on Chromium) browser control' ).
lo_buf->add( '// Todo: Remove once https://github.com/abapGit/abapGit/issues/4841 is fixed' ).
lo_buf->add( 'function toggleBrowserControlWarning(){' ).
lo_buf->add( '  if (!navigator.userAgent.includes("Edg")){' ).
lo_buf->add( '    var elBrowserControlWarning = document.getElementById("browser-control-warning");' ).
lo_buf->add( '    if (elBrowserControlWarning) {' ).
lo_buf->add( '      elBrowserControlWarning.style.display = "none";' ).
lo_buf->add( '    }' ).
lo_buf->add( '  }' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '// Output type of HTML control in the abapGit footer' ).
lo_buf->add( 'function displayBrowserControlFooter() {' ).
lo_buf->add( '  var out = document.getElementById("browser-control-footer");' ).
lo_buf->add( '  out.innerHTML = " - " + ( navigator.userAgent.includes("Edg") ? "Edge" : "IE"  );' ).
lo_buf->add( '}' ).
    li_asset_man->register_asset(
      iv_url       = 'js/common.js'
      iv_type      = 'text/javascript'
      iv_mime_name = 'ZABAPGIT_JS_COMMON'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

lo_buf->add( '@font-face {' ).
lo_buf->add( '    font-family: "ag-icons";' ).
lo_buf->add( '    font-weight: normal;' ).
lo_buf->add( '    font-style: normal;' ).
lo_buf->add( '    src: url("../font/ag-icons.woff") format("woff");' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.icon {' ).
lo_buf->add( '    line-height: 1;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.icon:before {' ).
lo_buf->add( '    font-family: ag-icons !important;' ).
lo_buf->add( '    font-style: normal;' ).
lo_buf->add( '    font-weight: normal !important;' ).
lo_buf->add( '' ).
lo_buf->add( '    display: inline-block;' ).
lo_buf->add( '    text-decoration: none;' ).
lo_buf->add( '    text-align: center;' ).
lo_buf->add( '    vertical-align: text-top;' ).
lo_buf->add( '    /* width: 1.1em; */' ).
lo_buf->add( '    /* padding-right: 0.2em */' ).
lo_buf->add( '' ).
lo_buf->add( '    /* For safety - reset parent styles, that can break glyph codes*/' ).
lo_buf->add( '    font-variant: normal;' ).
lo_buf->add( '    text-transform: none;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.icon.large { font-size: 200%; }' ).
lo_buf->add( '' ).
lo_buf->add( '.icon-abapgit:before { content: "\f101"; }' ).
lo_buf->add( '.icon-abaplint:before { content: "\f102"; }' ).
lo_buf->add( '.icon-arrow-circle-up:before { content: "\f103"; }' ).
lo_buf->add( '.icon-bars:before { content: "\f104"; }' ).
lo_buf->add( '.icon-bolt:before { content: "\f105"; }' ).
lo_buf->add( '.icon-box:before { content: "\f106"; }' ).
lo_buf->add( '.icon-briefcase:before { content: "\f107"; }' ).
lo_buf->add( '.icon-bug-solid:before { content: "\f108"; }' ).
lo_buf->add( '.icon-check:before { content: "\f109"; }' ).
lo_buf->add( '.icon-chevron-down:before { content: "\f10a"; }' ).
lo_buf->add( '.icon-chevron-left:before { content: "\f10b"; }' ).
lo_buf->add( '.icon-chevron-right:before { content: "\f10c"; }' ).
lo_buf->add( '.icon-chevron-up:before { content: "\f10d"; }' ).
lo_buf->add( '.icon-circle-dot-regular:before { content: "\f10e"; }' ).
lo_buf->add( '.icon-circle-play-regular:before { content: "\f10f"; }' ).
lo_buf->add( '.icon-circle-solid:before { content: "\f110"; }' ).
lo_buf->add( '.icon-cloud-commit:before { content: "\f111"; }' ).
lo_buf->add( '.icon-cloud-solid:before { content: "\f112"; }' ).
lo_buf->add( '.icon-cloud-upload-alt:before { content: "\f113"; }' ).
lo_buf->add( '.icon-code-branch:before { content: "\f114"; }' ).
lo_buf->add( '.icon-code-commit:before { content: "\f115"; }' ).
lo_buf->add( '.icon-code-pull-request-solid:before { content: "\f116"; }' ).
lo_buf->add( '.icon-code-solid:before { content: "\f117"; }' ).
lo_buf->add( '.icon-cog:before { content: "\f118"; }' ).
lo_buf->add( '.icon-copy-solid:before { content: "\f119"; }' ).
lo_buf->add( '.icon-download-solid:before { content: "\f11a"; }' ).
lo_buf->add( '.icon-edit-solid:before { content: "\f11b"; }' ).
lo_buf->add( '.icon-exclamation-circle:before { content: "\f11c"; }' ).
lo_buf->add( '.icon-exclamation-triangle:before { content: "\f11d"; }' ).
lo_buf->add( '.icon-file-alt:before { content: "\f11e"; }' ).
lo_buf->add( '.icon-file-code:before { content: "\f11f"; }' ).
lo_buf->add( '.icon-file-image:before { content: "\f120"; }' ).
lo_buf->add( '.icon-file:before { content: "\f121"; }' ).
lo_buf->add( '.icon-fire-alt:before { content: "\f122"; }' ).
lo_buf->add( '.icon-flow:before { content: "\f123"; }' ).
lo_buf->add( '.icon-folder:before { content: "\f124"; }' ).
lo_buf->add( '.icon-git-alt:before { content: "\f125"; }' ).
lo_buf->add( '.icon-github:before { content: "\f126"; }' ).
lo_buf->add( '.icon-heart-regular:before { content: "\f127"; }' ).
lo_buf->add( '.icon-info-circle-solid:before { content: "\f128"; }' ).
lo_buf->add( '.icon-language-solid:before { content: "\f129"; }' ).
lo_buf->add( '.icon-lock:before { content: "\f12a"; }' ).
lo_buf->add( '.icon-magnifying-glass-solid:before { content: "\f12b"; }' ).
lo_buf->add( '.icon-markdown:before { content: "\f12c"; }' ).
lo_buf->add( '.icon-paste-solid:before { content: "\f12d"; }' ).
lo_buf->add( '.icon-plug:before { content: "\f12e"; }' ).
lo_buf->add( '.icon-question-circle-solid:before { content: "\f12f"; }' ).
lo_buf->add( '.icon-redo-alt-solid:before { content: "\f130"; }' ).
lo_buf->add( '.icon-server-solid:before { content: "\f131"; }' ).
lo_buf->add( '.icon-sliders-h:before { content: "\f132"; }' ).
lo_buf->add( '.icon-snowflake:before { content: "\f133"; }' ).
lo_buf->add( '.icon-star:before { content: "\f134"; }' ).
lo_buf->add( '.icon-tag-solid:before { content: "\f135"; }' ).
lo_buf->add( '.icon-times-solid:before { content: "\f136"; }' ).
lo_buf->add( '.icon-tools-solid:before { content: "\f137"; }' ).
lo_buf->add( '.icon-truck-solid:before { content: "\f138"; }' ).
lo_buf->add( '.icon-upload-solid:before { content: "\f139"; }' ).
lo_buf->add( '.icon-user-cog-solid:before { content: "\f13a"; }' ).
lo_buf->add( '.icon-user-solid:before { content: "\f13b"; }' ).
lo_buf->add( '.icon-vial-solid:before { content: "\f13c"; }' ).
lo_buf->add( '' ).
    li_asset_man->register_asset(
      iv_url       = 'css/ag-icons.css'
      iv_type      = 'text/css'
      iv_mime_name = 'ZABAPGIT_ICON_FONT_CSS'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

    " @@abapmerge include-base64 zabapgit_icon_font.w3mi.data.woff > lo_buf->add( '$$' ).
    li_asset_man->register_asset(
      iv_url       = 'font/ag-icons.woff'
      iv_type      = 'font/woff'
      iv_mime_name = 'ZABAPGIT_ICON_FONT'
      iv_base64    = lo_buf->join_and_flush( ) ).

    ri_asset_man = li_asset_man.

  ENDMETHOD.
  METHOD get_frontend_services.

    IF gi_fe_services IS INITIAL.
      CREATE OBJECT gi_fe_services TYPE Lcl_abapgit_frontend_services.
    ENDIF.

    ri_fe_serv = gi_fe_services.

  ENDMETHOD.
  METHOD get_gui.

    DATA:
      li_hotkey_ctl TYPE REF TO Lif_abapgit_gui_hotkey_ctl,
      li_router     TYPE REF TO Lif_abapgit_gui_event_handler,
      li_asset_man  TYPE REF TO Lif_abapgit_gui_asset_manager.

    DATA lo_html_preprocessor TYPE REF TO Lcl_abapgit_gui_html_processor.

    IF go_gui IS INITIAL.
      li_asset_man = get_asset_manager( ).

      CREATE OBJECT lo_html_preprocessor EXPORTING ii_asset_man = li_asset_man.
      lo_html_preprocessor->preserve_css( 'css/ag-icons.css' ).
      lo_html_preprocessor->preserve_css( 'css/common.css' ).

      CREATE OBJECT li_router TYPE Lcl_abapgit_gui_router.
      CREATE OBJECT li_hotkey_ctl TYPE Lcl_abapgit_gui_hotkey_ctl.

      CREATE OBJECT go_gui
        EXPORTING
          io_component      = li_router
          ii_hotkey_ctl     = li_hotkey_ctl
          ii_html_processor = lo_html_preprocessor
          ii_asset_man      = li_asset_man.
    ENDIF.
    ro_gui = go_gui.

  ENDMETHOD.
  METHOD get_gui_services.
    IF gi_gui_services IS NOT BOUND.
      gi_gui_services ?= get_gui( ).
    ENDIF.
    ri_gui_services = gi_gui_services.
  ENDMETHOD.
  METHOD get_html_viewer.

    IF gi_html_viewer IS NOT BOUND.
      CREATE OBJECT gi_html_viewer TYPE Lcl_abapgit_html_viewer_gui
        EXPORTING
          io_container           = io_container
          iv_disable_query_table = iv_disable_query_table.
    ENDIF.

    ri_viewer = gi_html_viewer.

  ENDMETHOD.
  METHOD get_popups.

    IF gi_popups IS INITIAL.
      CREATE OBJECT gi_popups TYPE Lcl_abapgit_popups.
    ENDIF.

    ri_popups = gi_popups.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_UI_FACTORY implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_RONT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ront=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ront=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_RONT implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.
    DATA: lv_user  TYPE string,
          lx_error TYPE REF TO cx_root.

    TRY.

        SELECT SINGLE changed_by INTO lv_user
            FROM (c_table_name)
            WHERE ront_name = ms_item-obj_name AND version = 'I'.

        IF lv_user IS INITIAL.
          SELECT SINGLE changed_by INTO lv_user
            FROM (c_table_name)
            WHERE ront_name = ms_item-obj_name AND version = 'A'.
        ENDIF.

        rv_user = lv_user.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_RONT implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SFBF <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sfbf=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sfbf=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SFBF implementation.
*"* method's implementations
*include methods.
  METHOD activate.

    DATA: lt_bfuncts TYPE sfw_bftab,
          lt_msgtab  TYPE sprot_u_tab.

    IF Lif_abapgit_object~is_active( ) = abap_true.
      RETURN.
    ENDIF.

    APPEND mv_bf TO lt_bfuncts.

    cl_sfw_activate=>activate_sfbf(
      EXPORTING
        p_bfuncts = lt_bfuncts
        p_version = 'I'
      IMPORTING
        p_msgtab  = lt_msgtab ).

    READ TABLE lt_msgtab WITH KEY severity = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      Lcx_abapgit_exception=>raise( 'Error activating SFBF' ).
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).

    mv_bf = is_item-obj_name.

  ENDMETHOD.
  METHOD create.

    TRY.
        " make sure to clear cache
        ro_bf = cl_sfw_bf=>create_bf( mv_bf ).
        ro_bf->free( ).
        ro_bf = cl_sfw_bf=>create_bf( mv_bf ).
      CATCH cx_pak_invalid_data cx_pak_invalid_state cx_pak_not_authorized.
        Lcx_abapgit_exception=>raise( 'Error from CL_SFW_BF=>CREATE_BF' ).
    ENDTRY.

  ENDMETHOD.
  METHOD get.

    TRY.
        " make sure to clear cache, method GET_BF_FROM_DB does not exist in 702
        ro_bf = cl_sfw_bf=>get_bf( mv_bf ).
        ro_bf->free( ).
        ro_bf = cl_sfw_bf=>get_bf( mv_bf ).
      CATCH cx_pak_invalid_data cx_pak_invalid_state cx_pak_not_authorized.
        Lcx_abapgit_exception=>raise( 'Error from CL_SFW_BF=>GET_BF' ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: ls_data TYPE sfw_bf.

    ls_data = get( )->get_header_data( ).

    rv_user = ls_data-changedby.

    IF rv_user IS INITIAL.
      rv_user = ls_data-author.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lt_delete TYPE sfw_bftab,
          lt_msgtab TYPE sprot_u_tab.

    APPEND mv_bf TO lt_delete.

    cl_sfw_activate=>delete_sfbf( EXPORTING p_bfuncts = lt_delete
                                  IMPORTING p_msgtab = lt_msgtab ).

    READ TABLE lt_msgtab WITH KEY severity = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      Lcx_abapgit_exception=>raise( 'Error deleting SFBF' ).
    ENDIF.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lo_bf                TYPE REF TO cl_sfw_bf,
          ls_header            TYPE sfw_bf,
          lv_name_32           TYPE sfw_name32,
          lv_name_80           TYPE sfw_name80,
          lt_assigned_switches TYPE sfw_swbf_outtab,
          lt_dependancies      TYPE sfw_depend_outtab,
          ls_sfw_bfc_kw        TYPE sfw_bfc_kw,
          ls_sfw_bfc_tc        TYPE sfw_bfc_tc,
          ls_sfw_bfc_rn        TYPE sfw_bfc_rn,
          lt_parent_bfs        TYPE sfw_bs_bf_outtab.

    IF iv_step = Lif_abapgit_object=>gc_step_id-late.
      activate( ).
      RETURN.
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'HEADER'
                  CHANGING cg_data = ls_header ).
    io_xml->read( EXPORTING iv_name = 'NAME32'
                  CHANGING cg_data = lv_name_32 ).
    io_xml->read( EXPORTING iv_name = 'NAME80'
                  CHANGING cg_data = lv_name_80 ).

    io_xml->read( EXPORTING iv_name = 'ASSIGNED_SWITCHES'
                  CHANGING cg_data = lt_assigned_switches ).
    io_xml->read( EXPORTING iv_name = 'DEPENDANCIES'
                  CHANGING cg_data = lt_dependancies ).
    io_xml->read( EXPORTING iv_name = 'CONTENT_KW'
                  CHANGING cg_data = ls_sfw_bfc_kw ).
    io_xml->read( EXPORTING iv_name = 'CONTENT_TC'
                  CHANGING cg_data = ls_sfw_bfc_tc ).
    io_xml->read( EXPORTING iv_name = 'CONTENT_RN'
                  CHANGING cg_data = ls_sfw_bfc_rn ).
    io_xml->read( EXPORTING iv_name = 'PARENT_BFS'
                  CHANGING cg_data = lt_parent_bfs ).

    TRY.
        IF Lif_abapgit_object~exists( ) = abap_true.
          lo_bf = get( ).
        ELSE.
          lo_bf = create( ).
        ENDIF.
      CATCH cx_pak_not_authorized cx_pak_invalid_state cx_pak_invalid_data.
        Lcx_abapgit_exception=>raise( 'error in CL_SFW_BF=>CREATE_BF' ).
    ENDTRY.

    ls_header-author = sy-uname.
    ls_header-createdon = sy-datum.

    " Get component from package
    SELECT SINGLE dlvunit FROM tdevc INTO ls_header-component WHERE devclass = iv_package.

    lo_bf->set_header_data( ls_header ).

    lo_bf->set_texts( p_32 = lv_name_32
                      p_80 = lv_name_80 ).

    lo_bf->set_assigned_switches( lt_assigned_switches ).
    lo_bf->set_excluded_bf( lt_dependancies ).
    lo_bf->set_content_data(
        im_sfw_bfc_kw = ls_sfw_bfc_kw
        im_sfw_bfc_rn = ls_sfw_bfc_rn
        im_sfw_bfc_tc = ls_sfw_bfc_tc ).
    lo_bf->set_parent_bfs( lt_parent_bfs ).

    set_default_package( iv_package ).
    tadir_insert( iv_package ).

    lo_bf->save_all( ).

    unlock( ).

    deserialize_longtexts( ii_xml         = io_xml
                           iv_longtext_id = c_longtext_id_sfbf ).

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: ls_tadir TYPE tadir,
          lv_bf    TYPE sfw_bfunction.

    lv_bf = ms_item-obj_name.
    IF cl_sfw_bf=>check_existence( lv_bf ) = abap_false.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM tadir INTO ls_tadir
      WHERE pgmid = 'R3TR'
      AND object = ms_item-obj_type
      AND obj_name = ms_item-obj_name.
    IF ls_tadir IS INITIAL.
      RETURN.
    ENDIF.

    rv_bool = abap_true.
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
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'EEUDB'
                                            iv_argument    = ms_item-obj_name
                                            iv_prefix      = 'SF' ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lo_bf                TYPE REF TO cl_sfw_bf,
          ls_header            TYPE sfw_bf,
          lv_name_32           TYPE sfw_name32,
          lv_name_80           TYPE sfw_name80,
          lt_assigned_switches TYPE sfw_swbf_outtab,
          lt_dependancies      TYPE sfw_depend_outtab,
          ls_sfw_bfc_kw        TYPE sfw_bfc_kw,
          ls_sfw_bfc_tc        TYPE sfw_bfc_tc,
          ls_sfw_bfc_rn        TYPE sfw_bfc_rn,
          lt_parent_bfs        TYPE sfw_bs_bf_outtab.


    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lo_bf = get( ).

    ls_header = lo_bf->get_header_data( ).
    CLEAR: ls_header-author,
           ls_header-version,
           ls_header-component,
           ls_header-createdon,
           ls_header-changedby,
           ls_header-changedon,
           ls_header-timestamp.

    lo_bf->get_texts(
      IMPORTING
        p_32 = lv_name_32
        p_80 = lv_name_80 ).

    lt_assigned_switches = lo_bf->get_assigned_switches( ).
    lt_dependancies = lo_bf->get_excluded_bf( ).
    lo_bf->get_content_data(
      IMPORTING
        ex_sfw_bfc_kw = ls_sfw_bfc_kw
        ex_sfw_bfc_tc = ls_sfw_bfc_tc
        ex_sfw_bfc_rn = ls_sfw_bfc_rn ).
    lt_parent_bfs = lo_bf->get_parent_bfs( ).

    io_xml->add( ig_data = ls_header
                 iv_name = 'HEADER' ).
    io_xml->add( ig_data = lv_name_32
                 iv_name = 'NAME32' ).
    io_xml->add( ig_data = lv_name_80
                 iv_name = 'NAME80' ).

    io_xml->add( ig_data = lt_assigned_switches
                 iv_name = 'ASSIGNED_SWITCHES' ).
    io_xml->add( ig_data = lt_dependancies
                 iv_name = 'DEPENDANCIES' ).
    io_xml->add( ig_data = ls_sfw_bfc_kw
                 iv_name = 'CONTENT_KW' ).
    io_xml->add( ig_data = ls_sfw_bfc_tc
                 iv_name = 'CONTENT_TC' ).
    io_xml->add( ig_data = ls_sfw_bfc_rn
                 iv_name = 'CONTENT_RN' ).
    io_xml->add( ig_data = lt_parent_bfs
                 iv_name = 'PARENT_BFS' ).

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_sfbf ).

  ENDMETHOD.
  METHOD unlock.

    CALL FUNCTION 'DEQUEUE_EEUDB'
      EXPORTING
        relid     = 'SF'
        name      = ms_item-obj_name
        _synchron = 'X'
        _scope    = '1'
        mode_eudb = abap_true.

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
endclass. "ZCL_ABAPGIT_OBJECT_SFBF implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SFBS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sfbs=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sfbs=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SFBS implementation.
*"* method's implementations
*include methods.
  METHOD activate.

    DATA: lt_bfsets TYPE sfw_bstab,
          lt_msgtab TYPE sprot_u_tab.

    IF Lif_abapgit_object~is_active( ) = abap_true.
      RETURN.
    ENDIF.

    APPEND mv_bfset TO lt_bfsets.

    cl_sfw_activate=>activate_sfbs(
      EXPORTING
        p_bsets   = lt_bfsets
        p_version = 'I'
      IMPORTING
        p_msgtab  = lt_msgtab ).

    READ TABLE lt_msgtab WITH KEY severity = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      Lcx_abapgit_exception=>raise( 'Error activating SFBS' ).
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).

    mv_bfset = is_item-obj_name.

  ENDMETHOD.
  METHOD create.

    TRY.
        " make sure to clear cache
        ro_bfs = cl_sfw_bfs=>create_bfs( mv_bfset ).
        ro_bfs->free( ).
        ro_bfs = cl_sfw_bfs=>create_bfs( mv_bfset ).
      CATCH cx_pak_invalid_data cx_pak_invalid_state cx_pak_not_authorized.
        Lcx_abapgit_exception=>raise( 'Error from CL_SFW_BFS=>CREATE_BFS' ).
    ENDTRY.

  ENDMETHOD.
  METHOD get.

    TRY.
        " make sure to clear cache
        ro_bfs = cl_sfw_bfs=>get_bfs( mv_bfset ).
        ro_bfs->free( ).
        ro_bfs = cl_sfw_bfs=>get_bfs( mv_bfset ).
      CATCH cx_pak_invalid_data cx_pak_invalid_state cx_pak_not_authorized.
        Lcx_abapgit_exception=>raise( 'Error from CL_SFW_BFS=>GET_BFS' ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: ls_data TYPE sfw_bs.

    ls_data = get( )->get_header_data( ).

    rv_user = ls_data-changedby.

    IF rv_user IS INITIAL.
      rv_user = ls_data-author.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lt_delete TYPE sfw_bstab,
          lt_msgtab TYPE sprot_u_tab.

    APPEND mv_bfset TO lt_delete.

    cl_sfw_activate=>delete_sfbs( EXPORTING p_bsets = lt_delete
                                  IMPORTING p_msgtab = lt_msgtab ).

    READ TABLE lt_msgtab WITH KEY severity = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      Lcx_abapgit_exception=>raise( 'Error deleting SFBS' ).
    ENDIF.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lo_bfs         TYPE REF TO cl_sfw_bfs,
          ls_header      TYPE sfw_bs,
          lv_name_32     TYPE sfw_name32,
          lv_name_80     TYPE sfw_name80,
          lt_assigned_bf TYPE sfw_bfbs_outtab,
          lt_nested_bfs  TYPE sfw_bsbs_outtab,
          lt_parent_bfs  TYPE sfw_bs_bs_parent_outtab.

    IF iv_step = Lif_abapgit_object=>gc_step_id-late.
      activate( ).
      RETURN.
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'HEADER'
                  CHANGING cg_data = ls_header ).
    io_xml->read( EXPORTING iv_name = 'NAME32'
                  CHANGING cg_data = lv_name_32 ).
    io_xml->read( EXPORTING iv_name = 'NAME80'
                  CHANGING cg_data = lv_name_80 ).

    io_xml->read( EXPORTING iv_name = 'ASSIGNED_BF'
                  CHANGING cg_data = lt_assigned_bf ).
    io_xml->read( EXPORTING iv_name = 'NESTED_BFS'
                  CHANGING cg_data = lt_nested_bfs ).
    io_xml->read( EXPORTING iv_name = 'PARENT_BFS'
                  CHANGING cg_data = lt_parent_bfs ).

    TRY.
        IF Lif_abapgit_object~exists( ) = abap_true.
          lo_bfs = get( ).
        ELSE.
          lo_bfs = create( ).
        ENDIF.
      CATCH cx_pak_not_authorized cx_pak_invalid_state cx_pak_invalid_data.
        Lcx_abapgit_exception=>raise( 'error in CL_SFW_BFS=>CREATE_BFS' ).
    ENDTRY.

    ls_header-author = sy-uname.
    ls_header-createdon = sy-datum.
    lo_bfs->set_header_data( ls_header ).

    lo_bfs->set_texts( p_32 = lv_name_32
                       p_80 = lv_name_80 ).

    lo_bfs->set_assigned_bf( lt_assigned_bf ).
    lo_bfs->set_assigned_bfs( lt_nested_bfs ).
    lo_bfs->set_nested_parent( lt_parent_bfs ).

    set_default_package( iv_package ).
    tadir_insert( iv_package ).

    lo_bfs->save_all( ).

    unlock( ).

    deserialize_longtexts( ii_xml         = io_xml
                           iv_longtext_id = c_longtext_id_sfbs ).

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA ls_tadir TYPE tadir.

    IF cl_sfw_bfs=>check_existence( mv_bfset ) = abap_false.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM tadir INTO ls_tadir
      WHERE pgmid = 'R3TR'
      AND object = ms_item-obj_type
      AND obj_name = ms_item-obj_name.
    IF ls_tadir IS INITIAL.
      RETURN.
    ENDIF.

    rv_bool = abap_true.

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
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'EEUDB'
                                            iv_argument    = ms_item-obj_name
                                            iv_prefix      = 'SS' ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lo_bfs         TYPE REF TO cl_sfw_bfs,
          ls_header      TYPE sfw_bs,
          lv_name_32     TYPE sfw_name32,
          lv_name_80     TYPE sfw_name80,
          lt_assigned_bf TYPE sfw_bfbs_outtab,
          lt_nested_bfs  TYPE sfw_bsbs_outtab,
          lt_parent_bfs  TYPE sfw_bs_bs_parent_outtab.


    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lo_bfs = get( ).

    ls_header = lo_bfs->get_header_data( ).
    CLEAR: ls_header-author,
           ls_header-version,
           ls_header-createdon,
           ls_header-changedby,
           ls_header-changedon,
           ls_header-timestamp.

    lo_bfs->get_texts(
      IMPORTING
        p_32 = lv_name_32
        p_80 = lv_name_80 ).

    lt_assigned_bf = lo_bfs->get_assigned_bf( ).
    lt_nested_bfs = lo_bfs->get_nested_bfs( ).
    lt_parent_bfs = lo_bfs->get_nested_parent( ).

    io_xml->add( ig_data = ls_header
                 iv_name = 'HEADER' ).
    io_xml->add( ig_data = lv_name_32
                 iv_name = 'NAME32' ).
    io_xml->add( ig_data = lv_name_80
                 iv_name = 'NAME80' ).

    io_xml->add( ig_data = lt_assigned_bf
                 iv_name = 'ASSIGNED_BF' ).
    io_xml->add( ig_data = lt_nested_bfs
                 iv_name = 'NESTED_BFS' ).
    io_xml->add( ig_data = lt_parent_bfs
                 iv_name = 'PARENT_BFS' ).

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_sfbs ).

  ENDMETHOD.
  METHOD unlock.

    CALL FUNCTION 'DEQUEUE_EEUDB'
      EXPORTING
        relid     = 'SS'
        name      = ms_item-obj_name
        _synchron = 'X'
        _scope    = '1'
        mode_eudb = abap_true.

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
endclass. "ZCL_ABAPGIT_OBJECT_SFBS implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_OA2P <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_oa2p=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_oa2p=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_OA2P implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_profile = is_item-obj_name.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lo_persist     TYPE REF TO object,
          lr_wb          TYPE REF TO data,
          lo_profile     TYPE REF TO object,
          lv_profile_key TYPE seu_objkey.

    FIELD-SYMBOLS: <lo_wb> TYPE any.


    lv_profile_key = mv_profile.
    CREATE OBJECT lo_persist TYPE ('CL_OA2P_OBJECT_PERSIST').

    CREATE OBJECT lo_profile TYPE ('CL_OA2P_OBJECT_DATA').
    CREATE DATA lr_wb TYPE REF TO ('IF_WB_OBJECT_DATA_MODEL').
    ASSIGN lr_wb->* TO <lo_wb>.
    <lo_wb> ?= lo_profile.

    TRY.
        CALL METHOD lo_persist->('IF_WB_OBJECT_PERSIST~GET')
          EXPORTING
            p_object_key  = lv_profile_key    " Object Key
            p_version     = 'A'    " Version (Active/Inactive)
          CHANGING
            p_object_data = <lo_wb>.  " Object Data
      CATCH cx_swb_object_does_not_exist.
        Lcx_abapgit_exception=>raise( |OAuth2 Profile { lv_profile_key } doesn't exist.| ).
      CATCH cx_swb_exception.
        Lcx_abapgit_exception=>raise( |Error when geting details of OAuth2 Profile { lv_profile_key }.| ).
    ENDTRY.

    lo_profile = <lo_wb>.
    CALL METHOD lo_profile->('IF_WB_OBJECT_DATA_MODEL~GET_CHANGED_BY')
      RECEIVING
        p_user_name = rv_user.


  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    CONSTANTS: lc_actvt TYPE c LENGTH 2 VALUE `06`.

    DATA: lo_persist     TYPE REF TO object,
          lv_profile_key TYPE seu_objkey.

    "authority check
    AUTHORITY-CHECK OBJECT 'S_OA2C_ADM'
      ID 'ACTVT'     FIELD lc_actvt.
    IF sy-subrc <> 0.
      MESSAGE e463(01) WITH mv_profile INTO Lcx_abapgit_exception=>null.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    "delete profile
    lv_profile_key = mv_profile.
    CREATE OBJECT lo_persist TYPE ('CL_OA2P_OBJECT_PERSIST').

    TRY.
        CALL METHOD lo_persist->('IF_WB_OBJECT_PERSIST~DELETE')
          EXPORTING
            p_object_key = lv_profile_key.   " Object Key
      CATCH cx_swb_object_does_not_exist.
      CATCH cx_swb_exception.
        Lcx_abapgit_exception=>raise( |Error when deleting OAuth2 Profile { lv_profile_key }.| ).
    ENDTRY.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lo_persist      TYPE REF TO object,
          lo_profile      TYPE REF TO object,
          lr_wb           TYPE REF TO data,
          lr_profile_data TYPE REF TO data.
    FIELD-SYMBOLS: <ls_profile_data> TYPE data,
                   <lo_wb>           TYPE any.

    CREATE DATA lr_profile_data TYPE ('OA2C_SX_OA2P_OBJECT_DATA').
    ASSIGN lr_profile_data->* TO <ls_profile_data>.

    io_xml->read( EXPORTING iv_name = 'PROFILE'
                  CHANGING cg_data = <ls_profile_data> ).



    CREATE OBJECT lo_profile TYPE ('CL_OA2P_OBJECT_DATA').
    CREATE DATA lr_wb TYPE REF TO ('IF_WB_OBJECT_DATA_MODEL').
    ASSIGN lr_wb->* TO <lo_wb>.
    <lo_wb> ?= lo_profile.

    CALL METHOD lo_profile->('IF_WB_OBJECT_DATA_MODEL~SET_DATA')
      EXPORTING
        p_data = <ls_profile_data>.

    CREATE OBJECT lo_persist TYPE ('CL_OA2P_OBJECT_PERSIST').
    TRY.
        CALL METHOD lo_persist->('IF_WB_OBJECT_PERSIST~SAVE')
          EXPORTING
            p_object_data = <lo_wb>.   " Object Data
      CATCH cx_swb_exception.
        Lcx_abapgit_exception=>raise( |Error deserialize profile { mv_profile }.| ).
    ENDTRY.

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    CALL METHOD ('CL_OA2P_OBJECT_PERSIST')=>('CHECK_EXISTS_ON_DB')
      EXPORTING
        i_profile = mv_profile
      RECEIVING
        r_exists  = rv_bool.

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

    DATA: lv_profile_name TYPE eqegraarg,
          lv_lock_number  TYPE i,
          lt_locks        TYPE STANDARD TABLE OF seqg3.

    lv_profile_name = mv_profile.

    CALL FUNCTION 'ENQUEUE_READ'
      EXPORTING
        gclient = sy-mandt    " Client
        gname   = 'OA2C_PROFILES'    " Granularity name (-> table name)
        garg    = lv_profile_name    " Granularity value(->values of key fields)
      IMPORTING
        number  = lv_lock_number
      TABLES
        enq     = lt_locks.    " Number of chosen lock entries


    rv_is_locked = boolc( lv_lock_number > 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lo_persist      TYPE REF TO object,
          lo_profile      TYPE REF TO object,
          lv_profile_key  TYPE seu_objkey,
          lr_profile_data TYPE REF TO data,
          lr_wb           TYPE REF TO data.

    FIELD-SYMBOLS: <ls_profile_data> TYPE data,
                   <lo_specifics>    TYPE any,
                   <lo_wb>           TYPE any.

    CREATE DATA lr_profile_data TYPE ('OA2C_SX_OA2P_OBJECT_DATA').
    ASSIGN lr_profile_data->* TO <ls_profile_data>.


    lv_profile_key = mv_profile.
    CREATE OBJECT lo_persist TYPE ('CL_OA2P_OBJECT_PERSIST').
    CREATE OBJECT lo_profile TYPE ('CL_OA2P_OBJECT_DATA').
    CREATE DATA lr_wb TYPE REF TO ('IF_WB_OBJECT_DATA_MODEL').
    ASSIGN lr_wb->* TO <lo_wb>.
    <lo_wb> ?= lo_profile.


    TRY.
        CALL METHOD lo_persist->('IF_WB_OBJECT_PERSIST~GET')
          EXPORTING
            p_object_key  = lv_profile_key    " Object Key
            p_version     = 'A'    " Version (Active/Inactive)
          CHANGING
            p_object_data = <lo_wb>.  " Object Data
      CATCH cx_swb_object_does_not_exist.
        Lcx_abapgit_exception=>raise( |OAuth2 Profile { lv_profile_key } doesn't exist.| ).
      CATCH cx_swb_exception.
        Lcx_abapgit_exception=>raise( |Error when geting details of OAuth2 Profile { lv_profile_key }.| ).
    ENDTRY.

    "remove system specific information
    lo_profile = <lo_wb>.
    CALL METHOD lo_profile->('IF_WB_OBJECT_DATA_MODEL~SET_CHANGED_BY')
      EXPORTING
        p_user_name = ''.
    CALL METHOD lo_profile->('IF_WB_OBJECT_DATA_MODEL~SET_CHANGED_ON')
      EXPORTING
        p_date = '00000000'
        p_time = '000000'.
    CALL METHOD lo_profile->('IF_WB_OBJECT_DATA_MODEL~SET_CREATED_BY')
      EXPORTING
        p_user_name = ''.
    CALL METHOD lo_profile->('IF_WB_OBJECT_DATA_MODEL~SET_CREATED_ON')
      EXPORTING
        p_date = '00000000'
        p_time = '000000'.

    CALL METHOD lo_profile->('IF_WB_OBJECT_DATA_MODEL~GET_DATA')
      IMPORTING
        p_data = <ls_profile_data>.

    "remove runtime information
    ASSIGN COMPONENT 'O_SPECIFICS' OF STRUCTURE <ls_profile_data> TO <lo_specifics>.
    CLEAR <lo_specifics>.

    io_xml->add( iv_name = 'PROFILE'
                 ig_data = <ls_profile_data> ).


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
endclass. "ZCL_ABAPGIT_OBJECT_OA2P implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SFSW <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sfsw=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sfsw=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SFSW implementation.
*"* method's implementations
*include methods.
  METHOD activate.

    DATA: lt_switches TYPE sfw_switchtab,
          lt_msgtab   TYPE sprot_u_tab.

    IF Lif_abapgit_object~is_active( ) = abap_true.
      RETURN.
    ENDIF.

    APPEND mv_switch TO lt_switches.

    cl_sfw_activate=>activate_sfsw(
      EXPORTING
        p_switches = lt_switches
        p_version  = 'I'
      IMPORTING
        p_msgtab   = lt_msgtab ).

    READ TABLE lt_msgtab WITH KEY severity = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      Lcx_abapgit_exception=>raise( 'Error activating SFBS' ).
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).

    mv_switch = is_item-obj_name.

  ENDMETHOD.
  METHOD create.

    TRY.
        " make sure to clear cache
        ro_switch = cl_sfw_sw=>create_switch( mv_switch ).
        ro_switch->free( ).
        ro_switch = cl_sfw_sw=>create_switch( mv_switch ).
      CATCH cx_pak_invalid_data cx_pak_invalid_state cx_pak_not_authorized.
        Lcx_abapgit_exception=>raise( 'Error from CL_SFW_SW=>CREATE_SWITCH' ).
    ENDTRY.

  ENDMETHOD.
  METHOD get.

    TRY.
        " make sure to clear cache
        ro_switch = cl_sfw_sw=>get_switch( mv_switch ).
        ro_switch->free( ).
        ro_switch = cl_sfw_sw=>get_switch( mv_switch ).
      CATCH cx_pak_invalid_data cx_pak_invalid_state cx_pak_not_authorized.
        Lcx_abapgit_exception=>raise( 'Error from CL_SFW_SW=>GET_SWITCH' ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: ls_data TYPE sfw_switch.


    ls_data = get( )->get_header_data( ).

    rv_user = ls_data-changedby.
    IF rv_user IS INITIAL.
      rv_user = ls_data-author.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lt_delete TYPE sfw_switchtab,
          lt_msgtab TYPE sprot_u_tab.

    APPEND mv_switch TO lt_delete.

    cl_sfw_activate=>delete_sfsw( EXPORTING p_switches = lt_delete
                                  IMPORTING p_msgtab = lt_msgtab ).

    READ TABLE lt_msgtab WITH KEY severity = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      Lcx_abapgit_exception=>raise( 'Error deleting SFSW' ).
    ENDIF.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lo_switch    TYPE REF TO cl_sfw_sw,
          ls_header    TYPE sfw_switch,
          lv_name_32   TYPE sfw_name32,
          lv_name_80   TYPE sfw_name80,
          lt_parent_bf TYPE sfw_bf_sw_outtab,
          lt_conflicts TYPE sfw_confl_outtab.

    IF iv_step = Lif_abapgit_object=>gc_step_id-late.
      activate( ).
      RETURN.
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'HEADER'
                  CHANGING cg_data = ls_header ).
    io_xml->read( EXPORTING iv_name = 'NAME32'
                  CHANGING cg_data = lv_name_32 ).
    io_xml->read( EXPORTING iv_name = 'NAME80'
                  CHANGING cg_data = lv_name_80 ).

    io_xml->read( EXPORTING iv_name = 'PARENT_BF'
                  CHANGING cg_data = lt_parent_bf ).
    io_xml->read( EXPORTING iv_name = 'CONFLICTS'
                  CHANGING cg_data = lt_conflicts ).

    TRY.
        IF Lif_abapgit_object~exists( ) = abap_true.
          lo_switch = get( ).
        ELSE.
          lo_switch = create( ).
        ENDIF.
      CATCH cx_pak_not_authorized cx_pak_invalid_state cx_pak_invalid_data.
        Lcx_abapgit_exception=>raise( 'error in CL_SFW_SW=>CREATE_SWITCH' ).
    ENDTRY.

    ls_header-author = sy-uname.
    ls_header-createdon = sy-datum.
    lo_switch->set_header_data( ls_header ).

    lo_switch->set_texts( p_32 = lv_name_32
                          p_80 = lv_name_80 ).

    lo_switch->set_parent_bf( lt_parent_bf ).
    lo_switch->set_conflicts( lt_conflicts ).

    set_default_package( iv_package ).
    tadir_insert( iv_package ).

    lo_switch->save_all(
      EXCEPTIONS
        not_saved = 1
        OTHERS    = 2 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error in CL_SFW_SW->SAVE_ALL' ).
    ENDIF.

    unlock( ).

    deserialize_longtexts( ii_xml         = io_xml
                           iv_longtext_id = c_longtext_id_sfsw ).

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA ls_tadir TYPE tadir.

    IF cl_sfw_sw=>check_existence( mv_switch ) = abap_false.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM tadir INTO ls_tadir
      WHERE pgmid = 'R3TR'
      AND object = ms_item-obj_type
      AND obj_name = ms_item-obj_name.
    IF ls_tadir IS INITIAL.
      RETURN.
    ENDIF.

    rv_bool = abap_true.

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
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'EEUDB'
                                            iv_argument    = ms_item-obj_name
                                            iv_prefix      = 'SW' ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lo_switch    TYPE REF TO cl_sfw_sw,
          ls_header    TYPE sfw_switch,
          lv_name_32   TYPE sfw_name32,
          lv_name_80   TYPE sfw_name80,
          lt_parent_bf TYPE sfw_bf_sw_outtab,
          lt_conflicts TYPE sfw_confl_outtab.


    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lo_switch = get( ).

    ls_header = lo_switch->get_header_data( ).
    CLEAR: ls_header-author,
           ls_header-version,
           ls_header-createdon,
           ls_header-changedby,
           ls_header-changedon,
           ls_header-timestamp.

    lo_switch->get_texts(
      IMPORTING
        p_32 = lv_name_32
        p_80 = lv_name_80 ).

    lt_parent_bf = lo_switch->get_parent_bf( ).
    lt_conflicts = lo_switch->get_conflicts( ).

    io_xml->add( ig_data = ls_header
                 iv_name = 'HEADER' ).
    io_xml->add( ig_data = lv_name_32
                 iv_name = 'NAME32' ).
    io_xml->add( ig_data = lv_name_80
                 iv_name = 'NAME80' ).

    io_xml->add( ig_data = lt_parent_bf
                 iv_name = 'PARENT_BF' ).
    io_xml->add( ig_data = lt_conflicts
                 iv_name = 'CONFLICTS' ).

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_sfsw ).

  ENDMETHOD.
  METHOD unlock.

    CALL FUNCTION 'DEQUEUE_EEUDB'
      EXPORTING
        relid     = 'SW'
        name      = ms_item-obj_name
        _synchron = 'X'
        _scope    = '1'
        mode_eudb = abap_true.

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
endclass. "ZCL_ABAPGIT_OBJECT_SFSW implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SAXX_SUPER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_saxx_super=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_saxx_super=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SAXX_SUPER implementation.
*"* method's implementations
*include methods.
  METHOD create_channel_objects.

    get_names( ).

    TRY.
        IF mi_appl_obj_data IS NOT BOUND.
          CREATE OBJECT mi_appl_obj_data TYPE (mv_appl_obj_cls_name).
        ENDIF.

        IF mi_persistence IS NOT BOUND.
          CREATE OBJECT mi_persistence TYPE (mv_persistence_cls_name).
        ENDIF.

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |{ ms_item-obj_type } not supported| ).
    ENDTRY.

  ENDMETHOD.
  METHOD get_data.

    DATA: lv_object_key TYPE seu_objkey.

    lv_object_key = ms_item-obj_name.

    TRY.
        mi_persistence->get(
          EXPORTING
            p_object_key  = lv_object_key
            p_version     = 'A'
          CHANGING
            p_object_data = mi_appl_obj_data ).

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |{ ms_item-obj_type } not supported| ).
    ENDTRY.

    mi_appl_obj_data->get_data( IMPORTING p_data = eg_data ).

  ENDMETHOD.
  METHOD get_names.

    IF mv_data_structure_name IS INITIAL.
      mv_data_structure_name  = get_data_structure_name( ).
    ENDIF.

    IF mv_appl_obj_cls_name IS INITIAL.
      mv_appl_obj_cls_name    = get_data_class_name( ).
    ENDIF.

    IF mv_persistence_cls_name IS INITIAL.
      mv_persistence_cls_name = get_persistence_class_name( ).
    ENDIF.

  ENDMETHOD.
  METHOD lock.

    DATA: lv_objname    TYPE trobj_name,
          lv_object_key TYPE seu_objkey,
          lv_objtype    TYPE trobjtype.


    lv_objname    = ms_item-obj_name.
    lv_object_key = ms_item-obj_name.
    lv_objtype    = ms_item-obj_type.

    mi_persistence->lock(
      EXPORTING
        p_objname_tr   = lv_objname
        p_object_key   = lv_object_key
        p_objtype_tr   = lv_objtype
      EXCEPTIONS
        foreign_lock   = 1
        error_occurred = 2
        OTHERS         = 3 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error occured while locking { ms_item-obj_type } { lv_objname }| ).
    ENDIF.

  ENDMETHOD.
  METHOD unlock.

    DATA: lv_objname    TYPE trobj_name,
          lv_object_key TYPE seu_objkey,
          lv_objtype    TYPE trobjtype.

    lv_objname    = ms_item-obj_name.
    lv_object_key = ms_item-obj_name.
    lv_objtype    = ms_item-obj_type.

    mi_persistence->unlock( p_objname_tr = lv_objname
                            p_object_key = lv_object_key
                            p_objtype_tr = lv_objtype ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lr_data TYPE REF TO data.

    FIELD-SYMBOLS: <lg_data>       TYPE any,
                   <lg_header>     TYPE any,
                   <lg_changed_by> TYPE any.

    create_channel_objects( ).

    TRY.
        CREATE DATA lr_data TYPE (mv_data_structure_name).
        ASSIGN lr_data->* TO <lg_data>.

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |{ ms_item-obj_name } not supported| ).
    ENDTRY.

    get_data( IMPORTING eg_data = <lg_data> ).

    ASSIGN COMPONENT 'HEADER' OF STRUCTURE <lg_data> TO <lg_header>.
    ASSERT sy-subrc = 0.
    ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <lg_header> TO <lg_changed_by>.
    ASSERT sy-subrc = 0.

    IF <lg_changed_by> IS NOT INITIAL.
      rv_user = <lg_changed_by>.
    ELSE.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_object_key TYPE seu_objkey.

    create_channel_objects( ).

    lv_object_key = ms_item-obj_name.

    TRY.
        lock( ).

        mi_persistence->delete( lv_object_key ).

        unlock( ).

      CATCH cx_swb_exception.
        Lcx_abapgit_exception=>raise( |Error occured while deleting { ms_item-obj_type }| ).
    ENDTRY.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lr_data TYPE REF TO data.

    FIELD-SYMBOLS: <lg_data> TYPE any.

    create_channel_objects( ).

    TRY.
        CREATE DATA lr_data TYPE (mv_data_structure_name).
        ASSIGN lr_data->* TO <lg_data>.

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |{ ms_item-obj_type } not supported| ).
    ENDTRY.

    io_xml->read(
      EXPORTING
        iv_name = ms_item-obj_type
      CHANGING
        cg_data = <lg_data> ).

    IF Lif_abapgit_object~exists( ) = abap_true.
      Lif_abapgit_object~delete( iv_package   = iv_package
                                 iv_transport = iv_transport ).
    ENDIF.

    TRY.
        lock( ).

        corr_insert( iv_package ).

        mi_appl_obj_data->set_data( <lg_data> ).

        mi_persistence->save( mi_appl_obj_data ).

        unlock( ).

      CATCH cx_swb_exception.
        Lcx_abapgit_exception=>raise( |Error occured while creating { ms_item-obj_type }| ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_object_key TYPE seu_objkey.

    create_channel_objects( ).

    lv_object_key = ms_item-obj_name.

    TRY.
        mi_persistence->get( p_object_key           = lv_object_key
                             p_version              = 'A'
                             p_existence_check_only = abap_true ).

      CATCH cx_swb_object_does_not_exist cx_swb_exception.
        rv_bool = abap_false.
        RETURN.
    ENDTRY.

    rv_bool = abap_true.

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
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

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

    DATA: lr_data             TYPE REF TO data.

    FIELD-SYMBOLS: <lg_data>   TYPE any,
                   <lg_header> TYPE any,
                   <lg_field>  TYPE any.

    create_channel_objects( ).

    TRY.
        CREATE DATA lr_data TYPE (mv_data_structure_name).
        ASSIGN lr_data->* TO <lg_data>.

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |{ ms_item-obj_type } not supported| ).
    ENDTRY.

    get_data( IMPORTING eg_data = <lg_data> ).

    ASSIGN COMPONENT 'HEADER' OF STRUCTURE <lg_data> TO <lg_header>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'CHANGED_ON' OF STRUCTURE <lg_header> TO <lg_field>.
    ASSERT sy-subrc = 0.
    CLEAR <lg_field>.

    ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <lg_header> TO <lg_field>.
    ASSERT sy-subrc = 0.
    CLEAR <lg_field>.

    ASSIGN COMPONENT 'CHANGED_AT' OF STRUCTURE <lg_header> TO <lg_field>.
    ASSERT sy-subrc = 0.
    CLEAR <lg_field>.

    ASSIGN COMPONENT 'CHANGED_CLNT' OF STRUCTURE <lg_header> TO <lg_field>.
    ASSERT sy-subrc = 0.
    CLEAR <lg_field>.

    ASSIGN COMPONENT 'CREATED_ON' OF STRUCTURE <lg_header> TO <lg_field>.
    ASSERT sy-subrc = 0.
    CLEAR <lg_field>.

    ASSIGN COMPONENT 'CREATED_BY' OF STRUCTURE <lg_header> TO <lg_field>.
    ASSERT sy-subrc = 0.
    CLEAR <lg_field>.

    ASSIGN COMPONENT 'CREATED_AT' OF STRUCTURE <lg_header> TO <lg_field>.
    ASSERT sy-subrc = 0.
    CLEAR <lg_field>.

    ASSIGN COMPONENT 'CREATED_CLNT' OF STRUCTURE <lg_header> TO <lg_field>.
    ASSERT sy-subrc = 0.
    CLEAR <lg_field>.

    io_xml->add( iv_name = ms_item-obj_type
                 ig_data = <lg_data> ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SAXX_SUPER implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SCP1 <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_scp1=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_scp1=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SCP1 implementation.
*"* method's implementations
*include methods.
  METHOD adjust_inbound.

    FIELD-SYMBOLS: <ls_scprvals> TYPE scprvals,
                   <ls_scprreca> TYPE scprreca,
                   <ls_scprvall> TYPE scprvall.

* back to internal format
    LOOP AT cs_scp1-scprvals ASSIGNING <ls_scprvals>.
      SHIFT <ls_scprvals>-recnumber RIGHT DELETING TRAILING space.
    ENDLOOP.
    LOOP AT cs_scp1-scprreca ASSIGNING <ls_scprreca>.
      SHIFT <ls_scprreca>-recnumber RIGHT DELETING TRAILING space.
    ENDLOOP.
    LOOP AT cs_scp1-scprvall ASSIGNING <ls_scprvall>.
      SHIFT <ls_scprvall>-recnumber RIGHT DELETING TRAILING space.
    ENDLOOP.

  ENDMETHOD.
  METHOD adjust_outbound.

    FIELD-SYMBOLS: <ls_scprvals> TYPE scprvals,
                   <ls_scprreca> TYPE scprreca,
                   <ls_scprvall> TYPE scprvall.

* normalize the XML
    LOOP AT cs_scp1-scprvals ASSIGNING <ls_scprvals>.
      CONDENSE <ls_scprvals>-recnumber.
    ENDLOOP.
    LOOP AT cs_scp1-scprreca ASSIGNING <ls_scprreca>.
      CONDENSE <ls_scprreca>-recnumber.
    ENDLOOP.
    LOOP AT cs_scp1-scprvall ASSIGNING <ls_scprvall>.
      CONDENSE <ls_scprvall>-recnumber.
    ENDLOOP.

  ENDMETHOD.
  METHOD call_delete_fms.

    CONSTANTS:
      lc_version_new      TYPE c VALUE 'N', "Include SCPRINTCONST version_new
      lc_operation_delete TYPE c VALUE 'D'.

    DATA:
      lv_profile_type   TYPE scprattr-type,
      lt_fatherprofiles TYPE STANDARD TABLE OF scproprof WITH DEFAULT KEY,
      ls_fatherprofile  TYPE scproprof.


    CALL FUNCTION 'SCPR_DB_ATTR_GET_DETAIL'
      EXPORTING
        profid   = iv_profile_id
        version  = lc_version_new
      IMPORTING
        proftype = lv_profile_type
      EXCEPTIONS
        OTHERS   = 0.

    CALL FUNCTION 'SCPR_PRSET_DB_USED_IN'
      EXPORTING
        profid   = iv_profile_id
        version  = lc_version_new
      TABLES
        profiles = lt_fatherprofiles.

    ls_fatherprofile-id = iv_profile_id.
    APPEND ls_fatherprofile TO lt_fatherprofiles.
    CALL FUNCTION 'SCPR_CT_TRANSPORT_ENTRIES'
      TABLES
        profids                  = lt_fatherprofiles
      EXCEPTIONS
        error_in_transport_layer = 1
        user_abort               = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |error while deleting SCP1 - TRANSPORT, { sy-subrc }| ).
    ENDIF.

    CALL FUNCTION 'SCPR_PRSET_DB_DELETE_ALL'
      EXPORTING
        profid      = iv_profile_id
        proftype    = lv_profile_type
      TABLES
        fatherprofs = lt_fatherprofiles
      EXCEPTIONS
        user_abort  = 1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |error while deleting SCP1 - DB_DELETE, { sy-subrc }| ).
    ENDIF.

    CALL FUNCTION 'SCPR_MEM_SCPR_ACTIONS_ADD'
      EXPORTING
        bcset_id  = iv_profile_id
        operation = lc_operation_delete.

  ENDMETHOD.
  METHOD dequeue.

    DATA: lv_id TYPE scpr_id.


    lv_id = ms_item-obj_name.

    CALL FUNCTION 'SCPR_SV_DEQUEUE_BCSET'
      EXPORTING
        bcset_id = lv_id.

  ENDMETHOD.
  METHOD enqueue.

    DATA: lv_id TYPE scpr_id.

    lv_id = ms_item-obj_name.

    CALL FUNCTION 'SCPR_SV_ENQUEUE_BCSET'
      EXPORTING
        bcset_id          = lv_id
      EXCEPTIONS
        is_already_locked = 1
        system_failure    = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD load.

    CALL FUNCTION 'SCPR_TEMPL_DB_VALS_GET_DETAIL'
      EXPORTING
        profid   = cs_scp1-scprattr-id
        category = cs_scp1-scprattr-category
      TABLES
        values   = cs_scp1-scprvals
        valuesl  = cs_scp1-scprvall
        recattr  = cs_scp1-scprreca.

    CALL FUNCTION 'SCPR_TEMPL_DB_FLDTXTVAR_GET'
      EXPORTING
        bcset_id = cs_scp1-scprattr-id
        category = cs_scp1-scprattr-category
      TABLES
        it_fldv  = cs_scp1-scprfldv.

  ENDMETHOD.
  METHOD load_hier.

    CALL FUNCTION 'SCPR_PRSET_DB_SUBP_GET_DETAIL'
      EXPORTING
        profid   = cs_scp1-scprattr-id
        category = cs_scp1-scprattr-category
      TABLES
        subprofs = cs_scp1-subprofs.

  ENDMETHOD.
  METHOD save.

    DATA: ls_scp1 TYPE ty_scp1,
          ls_text TYPE scprtext.


* copy everything to local, the function module changes the values
    ls_scp1 = is_scp1.

    READ TABLE ls_scp1-scprtext INTO ls_text WITH KEY langu = mv_language. "#EC CI_SUBRC

    CALL FUNCTION 'SCPR_TEMPL_MN_TEMPLATE_SAVE'
      EXPORTING
        profid                    = ls_scp1-scprattr-id
        proftext                  = ls_text-text
        category                  = ls_scp1-scprattr-category
        cli_dep                   = ls_scp1-scprattr-cli_dep
        cli_cas                   = ls_scp1-scprattr-cli_cas
        reftype                   = ls_scp1-scprattr-reftype
        refname                   = ls_scp1-scprattr-refname
        orgid                     = ls_scp1-scprattr-orgid
        component                 = ls_scp1-scprattr-component
        minrelease                = ls_scp1-scprattr-minrelease
        maxrelease                = ls_scp1-scprattr-maxrelease
        act_info                  = ls_scp1-scprattr-act_info
        bcset_type                = ls_scp1-scprattr-type
        fldtxtvar_supplied        = 'YES'
        with_transp_insert        = abap_false
        with_progress_indicator   = abap_false
        remove_denied_data        = abap_true
        ask_for_cont_after_remove = abap_true
      TABLES
        values                    = ls_scp1-scprvals
        valuesl                   = ls_scp1-scprvall
        recattr                   = ls_scp1-scprreca
        it_fldv                   = ls_scp1-scprfldv
        texts                     = ls_scp1-scprtext
      EXCEPTIONS
        user_abort                = 1
        error_in_transport_layer  = 2
        inconsistent_data         = 3
        database_error            = 4
        OTHERS                    = 5.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD save_hier.

    DATA: ls_scp1  TYPE ty_scp1,
          ls_profs LIKE LINE OF ls_scp1-subprofs,
          lt_sub   TYPE STANDARD TABLE OF scproprof WITH DEFAULT KEY,
          ls_sub   LIKE LINE OF lt_sub,
          ls_text  TYPE scprtext.


* copy everything to local, the function module changes the values
    ls_scp1 = is_scp1.

    READ TABLE ls_scp1-scprtext INTO ls_text WITH KEY langu = mv_language. "#EC CI_SUBRC

* see fm SCPR_PRSET_DB_STORE, only this field and sequence is used
    LOOP AT ls_scp1-subprofs INTO ls_profs.
      ls_sub-id = ls_profs-subprofile.
      APPEND ls_sub TO lt_sub.
    ENDLOOP.

    CALL FUNCTION 'SCPR_PRSET_MN_BCSET_SAVE'
      EXPORTING
        profid                   = ls_scp1-scprattr-id
        proftext                 = ls_text-text
        category                 = ls_scp1-scprattr-category
        cli_dep                  = ls_scp1-scprattr-cli_dep
        cli_cas                  = ls_scp1-scprattr-cli_cas
        reftype                  = ls_scp1-scprattr-reftype
        refname                  = ls_scp1-scprattr-refname
        orgid                    = ls_scp1-scprattr-orgid
        component                = ls_scp1-scprattr-component
        minrelease               = ls_scp1-scprattr-minrelease
        maxrelease               = ls_scp1-scprattr-maxrelease
        act_info                 = ls_scp1-scprattr-act_info
        with_transp_insert       = abap_false
        with_progress_indicator  = abap_false
      TABLES
        subprofs                 = lt_sub
        texts                    = ls_scp1-scprtext
      EXCEPTIONS
        user_abort               = 1
        error_in_transport_layer = 2
        OTHERS                   = 3.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE modifier INTO rv_user FROM scprattr
      WHERE id = ms_item-obj_name
      AND version = 'N'.
    IF sy-subrc <> 0 OR rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_profile_id TYPE scpr_id.

    lv_profile_id = ms_item-obj_name.

    enqueue( ).
    call_delete_fms( lv_profile_id ).
    dequeue( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_scp1 TYPE ty_scp1.


    io_xml->read(
      EXPORTING iv_name = 'SCP1'
      CHANGING  cg_data = ls_scp1 ).

    adjust_inbound( CHANGING cs_scp1 = ls_scp1 ).

    IF ls_scp1-scprattr-type = 'TMP'.
      save_hier( ls_scp1 ).
    ELSE.
      save( ls_scp1 ).
    ENDIF.

    dequeue( ).

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_rc     TYPE sy-subrc,
          lv_profid TYPE scprattr-id.


    lv_profid = ms_item-obj_name.

    CALL FUNCTION 'SCPR_BCSET_EXISTS'
      EXPORTING
        profid = lv_profid
      IMPORTING
        rc     = lv_rc.
    rv_bool = boolc( lv_rc = 0 ).

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

    DATA: lv_display_only TYPE scpr_txt20,
          lv_bcset_id     TYPE scpr_id.

    lv_display_only = abap_false.
    lv_bcset_id     = ms_item-obj_name.

    EXPORT scpr3_display_only = lv_display_only
           scpr3_bcset_id     = lv_bcset_id
        TO MEMORY ID 'SCPR3_PARAMETER'.

    SUBMIT scpr3 AND RETURN.

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_scp1 TYPE ty_scp1.


    ls_scp1-scprattr-id = ms_item-obj_name.

    CALL FUNCTION 'SCPR_DB_ATTR_GET_DETAIL'
      EXPORTING
        profid     = ls_scp1-scprattr-id
      IMPORTING
        proftype   = ls_scp1-scprattr-type
        cli_dep    = ls_scp1-scprattr-cli_dep
        cli_cas    = ls_scp1-scprattr-cli_cas
        reftype    = ls_scp1-scprattr-reftype
        refname    = ls_scp1-scprattr-refname
        component  = ls_scp1-scprattr-component
        minrelease = ls_scp1-scprattr-minrelease
        maxrelease = ls_scp1-scprattr-maxrelease
        orgid      = ls_scp1-scprattr-orgid
        act_info   = ls_scp1-scprattr-act_info.

    CALL FUNCTION 'SCPR_TEXT_GET'
      EXPORTING
        profid        = ls_scp1-scprattr-id
        category      = ls_scp1-scprattr-category
      TABLES
        texts         = ls_scp1-scprtext
      EXCEPTIONS
        no_text_found = 1.

    IF ls_scp1-scprattr-type = 'TMP'.
      load_hier( CHANGING cs_scp1 = ls_scp1 ).
    ELSE.
      load( CHANGING cs_scp1 = ls_scp1 ).
    ENDIF.

    adjust_outbound( CHANGING cs_scp1 = ls_scp1 ).

    io_xml->add(
      iv_name = 'SCP1'
      ig_data  = ls_scp1 ).

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
endclass. "ZCL_ABAPGIT_OBJECT_SCP1 implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SRVD <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_srvd=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_srvd=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SRVD implementation.
*"* method's implementations
*include methods.
  METHOD clear_field.

    FIELD-SYMBOLS: <lv_value> TYPE data.

    ASSIGN COMPONENT iv_fieldname OF STRUCTURE cs_metadata TO <lv_value>.
    IF sy-subrc = 0.
      CLEAR: <lv_value>.
    ENDIF.

  ENDMETHOD.
  METHOD clear_fields.

    clear_field(
      EXPORTING
        iv_fieldname = 'VERSION'
      CHANGING
        cs_metadata  = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CREATED_AT'
      CHANGING
        cs_metadata  = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CREATED_BY'
      CHANGING
        cs_metadata  = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CHANGED_AT'
      CHANGING
        cs_metadata  = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CHANGED_BY'
      CHANGING
        cs_metadata  = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname = 'RESPONSIBLE'
      CHANGING
        cs_metadata  = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname = 'PACKAGE_REF'
      CHANGING
        cs_metadata  = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname = 'MASTER_SYSTEM'
      CHANGING
        cs_metadata  = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname = 'DT_UUID'
      CHANGING
        cs_metadata  = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname = 'ABAP_LANGUAGE_VERSION'
      CHANGING
        cs_metadata  = cs_metadata ).
    clear_field(
      EXPORTING
        iv_fieldname = 'ABAP_LANGU_VERSION'
      CHANGING
        cs_metadata  = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname = 'LINKS'
      CHANGING
        cs_metadata  = cs_metadata ).

  ENDMETHOD.
  METHOD constructor.
    super->constructor( is_item = is_item
                        iv_language = iv_language ).

    mv_service_definition_key = ms_item-obj_name.

    TRY.
        CREATE DATA mr_service_definition TYPE ('CL_SRVD_WB_OBJECT_DATA=>TY_SRVD_OBJECT_DATA').

      CATCH cx_sy_create_error.
        Lcx_abapgit_exception=>raise( |SRVD not supported by your NW release| ).
    ENDTRY.

  ENDMETHOD.
  METHOD get_object_data.

    DATA:
      lr_metadata TYPE REF TO data,
      lr_data     TYPE REF TO data.

    FIELD-SYMBOLS:
      <lv_metadata_node> TYPE any,
      <ls_metadata>      TYPE any,
      <lv_source>        TYPE any,
      <lg_data>          TYPE any.

    CREATE DATA lr_data TYPE ('CL_SRVD_WB_OBJECT_DATA=>TY_SRVD_OBJECT_DATA').
    ASSIGN lr_data->* TO <lg_data>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'METADATA' OF STRUCTURE <lg_data> TO <lv_metadata_node>.
    ASSERT sy-subrc = 0.

    CREATE DATA lr_metadata  TYPE ('CL_SRVD_WB_OBJECT_DATA=>TY_METADATA_EXTENDED').
    ASSIGN lr_metadata->* TO <ls_metadata>.
    ASSERT sy-subrc = 0.

    io_xml->read(
      EXPORTING
        iv_name = c_xml_parent_name
      CHANGING
        cg_data = <ls_metadata> ).

    <lv_metadata_node> = <ls_metadata>.

    ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <lg_data> TO <lv_source>.
    ASSERT sy-subrc = 0.

    <lv_source> = Lif_abapgit_object~mo_files->read_string( c_source_file ).
    IF <lv_source> IS INITIAL.
      <lv_source> = Lif_abapgit_object~mo_files->read_string( 'assrvd' ).
    ENDIF.

    CREATE OBJECT ro_object_data TYPE ('CL_SRVD_WB_OBJECT_DATA').
    ro_object_data->set_data( p_data = <lg_data> ).

  ENDMETHOD.
  METHOD get_wb_object_operator.

    DATA:
      ls_object_type TYPE wbobjtype,
      lx_error       TYPE REF TO cx_root.

    IF mo_object_operator IS BOUND.
      ro_object_operator = mo_object_operator.
    ENDIF.

    ls_object_type-objtype_tr = 'SRVD'.
    ls_object_type-subtype_wb = 'SRV'.

    TRY.
        CALL METHOD ('CL_WB_OBJECT_OPERATOR')=>('CREATE_INSTANCE')
          EXPORTING
            object_type = ls_object_type
            object_key  = mv_service_definition_key
          RECEIVING
            result      = mo_object_operator.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    ro_object_operator = mo_object_operator.

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

    CREATE OBJECT lo_object_data TYPE ('CL_SRVD_WB_OBJECT_DATA').
    lo_object_data = io_object_data.

    CREATE DATA lr_new TYPE ('CL_SRVD_WB_OBJECT_DATA=>TY_SRVD_OBJECT_DATA').
    ASSIGN lr_new->* TO <ls_new>.
    ASSERT sy-subrc = 0.

    CREATE DATA lr_old TYPE ('CL_SRVD_WB_OBJECT_DATA=>TY_SRVD_OBJECT_DATA').
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

    ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <ls_old> TO <lv_field_old>.
    ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <ls_new> TO <lv_field_new>.
    <lv_field_old> = <lv_field_new>.

    CREATE OBJECT ro_object_data_merged TYPE ('CL_SRVD_WB_OBJECT_DATA').

    CALL METHOD ro_object_data_merged->('SET_DATA')
      EXPORTING
        p_data = <ls_old>.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA:
      li_object_data_model  TYPE REF TO if_wb_object_data_model,
      li_wb_object_operator TYPE REF TO object,
      lx_error              TYPE REF TO cx_root.

    li_wb_object_operator = get_wb_object_operator( ).

    TRY.
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

    DATA:
      lo_object_data        TYPE REF TO if_wb_object_data_model,
      lx_error              TYPE REF TO cx_root,
      lo_wb_object_operator TYPE REF TO object,
      lo_merged_data_all    TYPE REF TO if_wb_object_data_model,
      lo_merged_data_prop   TYPE REF TO if_wb_object_data_model,
      lo_merged_data_cont   TYPE REF TO if_wb_object_data_model,
      lr_wbobjtype          TYPE REF TO data,
      lr_category           TYPE REF TO data.

    FIELD-SYMBOLS:
      <ls_wbobjtype> TYPE any,
      <lv_category>  TYPE any,
      <lv_field>     TYPE any.

    TRY.
        lo_object_data = get_object_data( io_xml ).
        lo_wb_object_operator = get_wb_object_operator( ).

        CREATE DATA lr_wbobjtype TYPE ('WBOBJTYPE').
        ASSIGN lr_wbobjtype->* TO <ls_wbobjtype>.
        ASSIGN COMPONENT 'OBJTYPE_TR' OF STRUCTURE <ls_wbobjtype> TO <lv_field>.
        <lv_field> = 'SRVD'.
        ASSIGN COMPONENT 'SUBTYPE_WB' OF STRUCTURE <ls_wbobjtype> TO <lv_field>.
        <lv_field> = 'SRV'.

        CREATE DATA lr_category TYPE ('WBADT_RESOURCE_CATEGORY').
        ASSIGN lr_category->* TO <lv_category>.

        CALL METHOD ('CL_BLUE_WB_UTILITY')=>('GET_RESOURCE_CATEGORY')
          EXPORTING
            is_object_type = <ls_wbobjtype>
          RECEIVING
            result         = <lv_category>.

        lo_wb_object_operator = get_wb_object_operator( ).

        tadir_insert( iv_package ).

        IF Lif_abapgit_object~exists( ) = abap_false.
          CASE <lv_category>.
            WHEN '1'. "if_wb_adt_plugin_resource_co=>co_sfs_res_category_atomic.
              CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~CREATE')
                EXPORTING
                  io_object_data    = lo_object_data
                  data_selection    = 'AL' "if_wb_object_data_selection_co=>c_all_data
                  version           = 'I' "swbm_version_inactive
                  package           = iv_package
                  transport_request = iv_transport.
            WHEN '2'. "if_wb_adt_plugin_resource_co=>co_sfs_res_category_compound_s.
              CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~CREATE')
                EXPORTING
                  io_object_data    = lo_object_data
                  data_selection    = 'P' "if_wb_object_data_selection_co=>c_properties
                  version           = 'I' "swbm_version_inactive
                  package           = iv_package
                  transport_request = iv_transport.
              CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
                EXPORTING
                  io_object_data    = lo_object_data
                  data_selection    = 'D' "if_wb_object_data_selection_co=>c_data_content
                  version           = 'I' "swbm_version_inactive
                  transport_request = iv_transport.
            WHEN OTHERS.
              Lcx_abapgit_exception=>raise( |Category '{ <lv_category> }' not supported| ).
          ENDCASE.
        ELSE.
          CASE <lv_category>.
            WHEN '1'. "if_wb_adt_plugin_resource_co=>co_sfs_res_category_atomic.
              lo_merged_data_all = merge_object_data( lo_object_data ).
              CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
                EXPORTING
                  io_object_data    = lo_merged_data_all
                  data_selection    = 'AL' "if_wb_object_data_selection_co=>c_all_data
                  version           = 'I' "swbm_version_inactive
                  transport_request = iv_transport.
            WHEN '2'. "if_wb_adt_plugin_resource_co=>co_sfs_res_category_compound_s.
              lo_merged_data_prop = merge_object_data( lo_object_data ).
              lo_merged_data_cont = merge_object_data( lo_object_data ).
              CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
                EXPORTING
                  io_object_data    = lo_merged_data_prop
                  data_selection    = 'P' "if_wb_object_data_selection_co=>c_properties
                  version           = 'I' "swbm_version_inactive
                  transport_request = iv_transport.
              CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
                EXPORTING
                  io_object_data    = lo_merged_data_cont
                  data_selection    = 'D' "if_wb_object_data_selection_co=>c_data_content
                  version           = 'I' "swbm_version_inactive
                  transport_request = iv_transport.
            WHEN OTHERS.
              Lcx_abapgit_exception=>raise( |Category '{ <lv_category> }' not supported| ).
          ENDCASE.
        ENDIF.

        corr_insert( iv_package ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA lo_object_data TYPE REF TO if_wb_object_data_model.
    DATA lo_wb_object_operator TYPE REF TO object.

    TRY.
        lo_wb_object_operator = get_wb_object_operator( ).
        CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
          EXPORTING
            data_selection = 'P'
          IMPORTING
            eo_object_data = lo_object_data.
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
      lx_error              TYPE REF TO cx_root,
      lv_source             TYPE string.

    FIELD-SYMBOLS:
      <ls_service_definition> TYPE any,
      <lv_metadata>           TYPE any,
      <lv_source>             TYPE string.

    ASSIGN mr_service_definition->* TO <ls_service_definition>.
    ASSERT sy-subrc = 0.

    TRY.
        li_wb_object_operator = get_wb_object_operator( ).

        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
          EXPORTING
            version        = 'A'
            data_selection = 'AL'
          IMPORTING
            "data           = <ls_service_definition>
            eo_object_data = li_object_data_model.

        CALL METHOD li_object_data_model->('GET_DATA')
          IMPORTING
            p_data = <ls_service_definition>.

        ASSIGN COMPONENT 'METADATA' OF STRUCTURE <ls_service_definition> TO <lv_metadata>.
        ASSERT sy-subrc = 0.
        clear_fields( CHANGING cs_metadata = <lv_metadata> ).

        ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <ls_service_definition> TO <lv_source>.
        ASSERT sy-subrc = 0.
        lv_source = <lv_source>.

        io_xml->add(
          iv_name = c_xml_parent_name
          ig_data = <lv_metadata> ).

        Lif_abapgit_object~mo_files->add_string(
          iv_ext    = c_source_file
          iv_string = lv_source ).

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
endclass. "ZCL_ABAPGIT_OBJECT_SRVD implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SFPF <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sfpf=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sfpf=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SFPF implementation.
*"* method's implementations
*include methods.
  METHOD fix_oref.

* During serialization of a SFPF / SFPI object the interface hierarchy
* is represented by attributes "id" and "href", where the id looks
* like "o<number>" and href like "#o<number>". Every run of
* serialization generates a new <number> in these  attributes, that
* leads to differences even by comparing of untouched forms.
* The purpose of this method is to renumber the id's consequentially
* and therefore to avoid fictive differences.

* NB: As the method iterator->get_next() works quite slowly,
*     it is better to collect all attributes in a cache table
*     instead of implementing of a nested loop using get_next().

    DATA:
      li_iterator TYPE REF TO if_ixml_node_iterator,
      li_elem     TYPE REF TO if_ixml_element,
      lv_new      TYPE string,
      lv_old      TYPE string,
      lv_count    TYPE i,
      BEGIN OF ls_attr_href,
        val  TYPE string,
        attr TYPE REF TO if_ixml_attribute,
      END OF ls_attr_href,
      lt_attr_href LIKE SORTED TABLE OF ls_attr_href WITH NON-UNIQUE KEY val.

    FIELD-SYMBOLS <ls_attr_href> LIKE LINE OF lt_attr_href.

*   Collect all attributes href='#o...' in the cache table
    li_iterator = ii_document->create_iterator_filtered(
      ii_document->create_filter_and(
        filter1 = ii_document->create_filter_node_type( if_ixml_node=>co_node_element )
        filter2 = ii_document->create_filter_attribute( 'href' ) ) ).
    li_elem ?= li_iterator->get_next( ).
    WHILE li_elem IS NOT INITIAL.
      ls_attr_href-attr = li_elem->get_attribute_node( 'href' ).
      ls_attr_href-val = ls_attr_href-attr->get_value( ).
      IF ls_attr_href-val CP '##o*'.
        INSERT ls_attr_href INTO TABLE lt_attr_href.
      ENDIF.
      li_elem ?= li_iterator->get_next( ).
    ENDWHILE.

*   Renumber id='o...' attributes
    li_iterator = ii_document->create_iterator_filtered(
      ii_document->create_filter_and(
        filter1 = ii_document->create_filter_node_type( if_ixml_node=>co_node_element )
        filter2 = ii_document->create_filter_attribute( 'id' ) ) ).
    li_elem ?= li_iterator->get_next( ).
    WHILE li_elem IS NOT INITIAL.
      lv_old = li_elem->get_attribute( 'id' ).
      IF lv_old CP 'o*'.
        lv_count = lv_count + 1.
        lv_new = |o{ lv_count }|.
*       Rewrite id
        IF li_elem->set_attribute( name = 'id'
                                   value = lv_new ) IS NOT INITIAL.
          Lcx_abapgit_exception=>raise( 'SFPF error, FIX_OREF' ).
        ENDIF.
*       Update references
        LOOP AT lt_attr_href ASSIGNING <ls_attr_href> WHERE val = '#' && lv_old.
          IF <ls_attr_href>-attr->set_value( '#' && lv_new ) IS NOT INITIAL.
            Lcx_abapgit_exception=>raise( 'SFPF error, FIX_OREF' ).
          ENDIF.
        ENDLOOP.
      ENDIF.
      li_elem ?= li_iterator->get_next( ).
    ENDWHILE.

  ENDMETHOD.
  METHOD form_to_xstring.

    CONSTANTS: lc_empty_data TYPE xstring VALUE ''.

    DATA: li_fp_form     TYPE REF TO if_fp_form,
          li_wb_form     TYPE REF TO if_fp_wb_form,
          li_fp_layout   TYPE REF TO if_fp_layout,
          lx_fp_err      TYPE REF TO cx_fp_api,
          lx_fp_conv_err TYPE REF TO cx_fp_api,
          lv_layout_data TYPE xstring.

    li_wb_form = load( ).
    li_fp_form ?= li_wb_form->get_object( ).
    li_fp_layout = li_fp_form->get_layout( ).
    lv_layout_data = li_fp_layout->get_layout_data( ).

    Lif_abapgit_object~mo_files->add_raw(
      iv_ext  = c_layout_file_ext
      iv_data = lv_layout_data ).

    TRY.
        li_fp_layout->set_layout_data( i_layout_data   = lc_empty_data
                                       i_set_xliff_ids = abap_false ).
      CATCH cx_fp_api INTO lx_fp_err.
        Lcx_abapgit_exception=>raise( |SFPF remove layout: { lx_fp_err->get_text( ) }| ).
    ENDTRY.

    TRY.
        rv_xstr = cl_fp_helper=>convert_form_to_xstring( li_fp_form ).
      CATCH cx_fp_api INTO lx_fp_conv_err.
        " Pass - the exception is handled below!
    ENDTRY.

    TRY.
        li_fp_layout->set_layout_data( i_layout_data   = lv_layout_data
                                       i_set_xliff_ids = abap_false ).
      CATCH cx_fp_api INTO lx_fp_err.
        " Be aware that there might be another exception
        " raised by cl_fp_helper=>convert_form_to_xstring( )
        Lcx_abapgit_exception=>raise( |SFPF recover layout: { lx_fp_err->get_text( ) }| ).
    ENDTRY.

    IF lx_fp_conv_err IS BOUND.
      " This statement handles the exception raised from cl_fp_helper=>convert_form_to_xstring( )
      Lcx_abapgit_exception=>raise( |SFPF convert_form_to_xstring: { lx_fp_conv_err->get_text( ) }| ).
    ENDIF.
  ENDMETHOD.
  METHOD load.

    DATA: lv_name TYPE fpname.


    lv_name = ms_item-obj_name.

    TRY.
        ri_wb_form = cl_fp_wb_form=>load( lv_name ).
      CATCH cx_fp_api.
        Lcx_abapgit_exception=>raise( 'SFPF error, load' ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE lastuser FROM fplayout
      INTO rv_user
      WHERE name = ms_item-obj_name
      AND state = 'A'.
    IF rv_user IS INITIAL.
      SELECT SINGLE firstuser FROM fplayout
        INTO rv_user
        WHERE name = ms_item-obj_name
        AND state = 'A'.
    ENDIF.
    IF rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_name TYPE fpname.

    lv_name = ms_item-obj_name.

    TRY.
        TRY.
            CALL METHOD cl_fp_wb_form=>('DELETE')
              EXPORTING
                i_name     = lv_name
                i_ordernum = iv_transport
                i_dark     = abap_true. " > 740
          CATCH cx_sy_dyn_call_error.
            cl_fp_wb_form=>delete(
              i_name     = lv_name
              i_ordernum = iv_transport ).
        ENDTRY.
      CATCH cx_fp_api.
        Lcx_abapgit_exception=>raise( 'SFPI error, delete' ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_xstr      TYPE xstring,
          lv_layout    TYPE xstring,
          lv_name      TYPE fpname,
          li_wb_object TYPE REF TO if_fp_wb_form,
          li_form      TYPE REF TO if_fp_form,
          lx_fp_err    TYPE REF TO cx_fp_api.


    lv_name = ms_item-obj_name.
    lv_xstr = cl_ixml_80_20=>render_to_xstring( io_xml->get_raw( ) ).

    TRY.
        li_form = cl_fp_helper=>convert_xstring_to_form( lv_xstr ).

        IF Lif_abapgit_object~mo_files->contains_file( c_layout_file_ext ) = abap_true.
          lv_layout = Lif_abapgit_object~mo_files->read_raw( c_layout_file_ext ).
          li_form->get_layout( )->set_layout_data( lv_layout ).
        ENDIF.

        IF Lif_abapgit_object~exists( ) = abap_true.
          TRY.
              CALL METHOD cl_fp_wb_form=>('DELETE')
                EXPORTING
                  i_name     = lv_name
                  i_ordernum = iv_transport
                  i_dark     = abap_true. " > 740
            CATCH cx_sy_dyn_call_error.
              cl_fp_wb_form=>delete(
                i_name     = lv_name
                i_ordernum = iv_transport ).
          ENDTRY.
        ENDIF.

        tadir_insert( iv_package ).

        TRY.
            CALL METHOD cl_fp_wb_form=>('CREATE')
              EXPORTING
                i_name     = lv_name
                i_form     = li_form
                i_ordernum = iv_transport
                i_dark     = abap_true " > 740
              RECEIVING
                r_wb_form  = li_wb_object.
          CATCH cx_sy_dyn_call_error.
            li_wb_object = cl_fp_wb_form=>create(
              i_name     = lv_name
              i_form     = li_form
              i_ordernum = iv_transport ).
        ENDTRY.

        li_wb_object->save( ).
        li_wb_object->free( ).
      CATCH cx_fp_api INTO lx_fp_err.
        Lcx_abapgit_exception=>raise( |SFPF deserialization error: { lx_fp_err->get_text( ) }| ).
    ENDTRY.

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_name TYPE fpname.

    SELECT SINGLE name FROM fplayout
      INTO lv_name
      WHERE name = ms_item-obj_name
      AND state = 'A'.
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

    DATA: lv_object TYPE seqg3-garg.

    lv_object = |{ ms_item-obj_name }|.
    OVERLAY lv_object WITH '                              '.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'EFPFORM'
                                            iv_argument    = lv_object ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_xstr            TYPE xstring,
          li_document        TYPE REF TO if_ixml_document,
          li_node_collection TYPE REF TO if_ixml_node_collection,
          li_node_iter       TYPE REF TO if_ixml_node_iterator,
          li_node            TYPE REF TO if_ixml_node,
          li_node_new        TYPE REF TO if_ixml_node,
          li_node_parent     TYPE REF TO if_ixml_node.

    lv_xstr = form_to_xstring( ).
    li_document = cl_ixml_80_20=>parse_to_document( stream_xstring = lv_xstr ).

*   Clear CACHE_INFO
    li_node_collection = li_document->get_elements_by_tag_name_ns( 'CACHE_INFO' ).
    IF li_node_collection IS NOT INITIAL.
      li_node_iter = li_node_collection->create_iterator( ).
      DO.
        li_node = li_node_iter->get_next( ).
        IF li_node IS INITIAL.
          EXIT.
        ENDIF.
        li_node_new = li_document->create_element_ns( 'CACHE_INFO' ).
        li_node_parent = li_node->get_parent( ).
        li_node_parent->replace_child( new_child = li_node_new
                                       old_child = li_node ).
      ENDDO.
    ENDIF.

    fix_oref( li_document ).
    io_xml->set_raw( li_document->get_root_element( ) ).

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
endclass. "ZCL_ABAPGIT_OBJECT_SFPF implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SFPI <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sfpi=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sfpi=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SFPI implementation.
*"* method's implementations
*include methods.
  METHOD interface_to_xstring.

    DATA: li_fp_interface TYPE REF TO if_fp_interface,
          li_wb_interface TYPE REF TO if_fp_wb_interface.


    TRY.
        li_wb_interface = load( ).
        li_fp_interface ?= li_wb_interface->get_object( ).
        rv_xstr = cl_fp_helper=>convert_interface_to_xstring( li_fp_interface ).
      CATCH cx_fp_api.
        Lcx_abapgit_exception=>raise( 'SFPI error, interface_to_xstring' ).
    ENDTRY.

  ENDMETHOD.
  METHOD load.

    DATA: lv_name TYPE fpname.


    lv_name = ms_item-obj_name.

    TRY.
        ri_wb_interface = cl_fp_wb_interface=>load( lv_name ).
      CATCH cx_fp_api.
        Lcx_abapgit_exception=>raise( 'SFPI error, load' ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE lastuser FROM fpinterface
      INTO rv_user
      WHERE name = ms_item-obj_name
      AND state = 'A'.
    IF rv_user IS INITIAL.
      SELECT SINGLE firstuser FROM fpinterface
        INTO rv_user
        WHERE name = ms_item-obj_name
        AND state = 'A'.
    ENDIF.
    IF rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_name         TYPE fpname,
          lo_wb_interface TYPE REF TO cl_fp_wb_interface.


    lo_wb_interface ?= load( ).

    lv_name = ms_item-obj_name.

    TRY.
        lo_wb_interface->delete( lv_name ).
      CATCH cx_fp_api.
        Lcx_abapgit_exception=>raise( 'SFPI error, delete' ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_xstr      TYPE xstring,
          lv_name      TYPE fpname,
          li_wb_object TYPE REF TO if_fp_wb_interface,
          li_interface TYPE REF TO if_fp_interface.


    lv_name = ms_item-obj_name.
    lv_xstr = cl_ixml_80_20=>render_to_xstring( io_xml->get_raw( ) ).

    IF Lif_abapgit_object~exists( ) = abap_true.
      Lif_abapgit_object~delete( iv_package   = iv_package
                                 iv_transport = iv_transport ).
    ENDIF.

    TRY.
        li_interface = cl_fp_helper=>convert_xstring_to_interface( lv_xstr ).
        tadir_insert( iv_package ).
        li_wb_object = cl_fp_wb_interface=>create( i_name      = lv_name
                                                   i_interface = li_interface ).
        li_wb_object->save( ).
        li_wb_object->free( ).
      CATCH cx_fp_api.
        Lcx_abapgit_exception=>raise( 'SFPI error, deserialize' ).
    ENDTRY.

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_name TYPE fpinterface-name.

    SELECT SINGLE name FROM fpinterface
      INTO lv_name
      WHERE name = ms_item-obj_name
      AND state = 'A'.
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

    DATA: lv_object TYPE seqg3-garg.

    lv_object = |{ ms_item-obj_name }|.
    OVERLAY lv_object WITH '                              '.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'EFPINTERFACE'
                                            iv_argument    = lv_object ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_xstr     TYPE xstring,
          li_document TYPE REF TO if_ixml_document.


    lv_xstr = interface_to_xstring( ).
    li_document = cl_ixml_80_20=>parse_to_document( stream_xstring = lv_xstr ).
    Lcl_abapgit_object_sfpf=>fix_oref( li_document ).
    io_xml->set_raw( li_document->get_root_element( ) ).

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
endclass. "ZCL_ABAPGIT_OBJECT_SFPI implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_W3HT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_w3ht=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_w3ht=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_W3HT implementation.
*"* method's implementations
*include methods.
  METHOD change_bdc_jump_data.

    DATA: ls_bdcdata LIKE LINE OF ct_bdcdata.

    ls_bdcdata-fnam = 'RADIO_HT'.
    ls_bdcdata-fval = 'X'.
    APPEND ls_bdcdata TO ct_bdcdata.

    CLEAR ls_bdcdata.
    ls_bdcdata-fnam = 'RADIO_MI'.
    ls_bdcdata-fval = ' '.
    APPEND ls_bdcdata TO ct_bdcdata.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_W3HT implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_W3MI <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_w3mi=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_w3mi=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_W3MI implementation.
*"* method's implementations
*include methods.
  METHOD change_bdc_jump_data.

    DATA: ls_bdcdata LIKE LINE OF ct_bdcdata.

    ls_bdcdata-fnam = 'RADIO_HT'.
    ls_bdcdata-fval = ' '.
    APPEND ls_bdcdata TO ct_bdcdata.

    CLEAR ls_bdcdata.
    ls_bdcdata-fnam = 'RADIO_MI'.
    ls_bdcdata-fval = 'X'.
    APPEND ls_bdcdata TO ct_bdcdata.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_W3MI implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SMIM <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_smim=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_smim=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SMIM implementation.
*"* method's implementations
*include methods.
  METHOD build_filename.

    CONCATENATE ms_item-obj_name ms_item-obj_type iv_filename
      INTO rv_filename SEPARATED BY '.'.
    TRANSLATE rv_filename TO LOWER CASE.

  ENDMETHOD.
  METHOD find_content.

    DATA: lv_filename TYPE string,
          lt_files    TYPE Lif_abapgit_git_definitions=>ty_files_tt.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF lt_files.


    lv_filename = get_filename( iv_url ).

    lv_filename = build_filename( lv_filename ).

    lt_files = Lif_abapgit_object~mo_files->get_files( ).

    READ TABLE lt_files ASSIGNING <ls_file>
        WITH KEY file
        COMPONENTS filename = lv_filename.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'SMIM, file not found' ).
    ENDIF.

    rv_content = <ls_file>-data.

  ENDMETHOD.
  METHOD get_filename.

    DATA: lv_lines   TYPE i,
          lt_strings TYPE TABLE OF string.


    SPLIT iv_url AT '/' INTO TABLE lt_strings.
    lv_lines = lines( lt_strings ).
    ASSERT lv_lines > 0.
    READ TABLE lt_strings INDEX lv_lines INTO rv_filename.
    ASSERT sy-subrc = 0.

  ENDMETHOD.
  METHOD get_url_for_io.

    DATA: ls_io       TYPE skwf_io,
          lv_url      TYPE skwf_url,
          ls_smimloio TYPE smimloio,
          lv_loio     TYPE sdok_docid.


    lv_loio = ms_item-obj_name.

    CLEAR ev_url.
    CLEAR ev_is_folder.

    SELECT SINGLE * FROM smimloio INTO ls_smimloio
      WHERE loio_id = lv_loio.                          "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE Lcx_abapgit_not_found.
    ENDIF.

    IF ls_smimloio-lo_class = wbmr_c_skwf_folder_class.
      ev_is_folder = abap_true.
      ls_io-objtype = skwfc_obtype_folder.
    ELSE.
      ls_io-objtype = skwfc_obtype_loio.
    ENDIF.
    ls_io-class = ls_smimloio-lo_class.
    ls_io-objid = ls_smimloio-loio_id.

    CALL FUNCTION 'SKWF_NMSPC_IO_ADDRESS_GET'
      EXPORTING
        io  = ls_io
      IMPORTING
        url = lv_url.

    ev_url = lv_url.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lv_loio TYPE sdok_docid.


    lv_loio = ms_item-obj_name.

    SELECT SINGLE chng_user FROM smimloio INTO rv_user
      WHERE loio_id = lv_loio.                          "#EC CI_GENBUFF
    IF sy-subrc <> 0 OR rv_user IS INITIAL.
      SELECT SINGLE chng_user FROM smimphio INTO rv_user
        WHERE loio_id = lv_loio.                        "#EC CI_GENBUFF
      IF sy-subrc <> 0 OR rv_user IS INITIAL.
        rv_user = c_user_unknown.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: li_api TYPE REF TO if_mr_api,
          lv_url TYPE string.


    TRY.
        get_url_for_io( IMPORTING ev_url  = lv_url ).
      CATCH Lcx_abapgit_not_found.
        " Deleted already (maybe by "folder with children") but record deletion in transport
        corr_insert( iv_package ).
        RETURN.
    ENDTRY.

    li_api = cl_mime_repository_api=>if_mr_api~get_api( ).
    li_api->delete(
      EXPORTING
        i_url              = lv_url
        i_delete_children  = abap_true
      EXCEPTIONS
        parameter_missing  = 1
        error_occured      = 2
        cancelled          = 3
        permission_failure = 4
        not_found          = 5
        OTHERS             = 6 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_url      TYPE string,
          lv_folder   TYPE abap_bool,
          lv_content  TYPE xstring,
          lv_filename TYPE skwf_filnm,
          lv_io       TYPE sdok_docid,
          lv_class    TYPE smimloio-lo_class,
          ls_skwf_io  TYPE skwf_io,
          li_api      TYPE REF TO if_mr_api.


    li_api = cl_mime_repository_api=>if_mr_api~get_api( ).
    lv_io = ms_item-obj_name.

    io_xml->read( EXPORTING iv_name = 'URL'
                  CHANGING cg_data = lv_url ).
    io_xml->read( EXPORTING iv_name = 'FOLDER'
                  CHANGING cg_data = lv_folder ).
    io_xml->read( EXPORTING iv_name = 'CLASS'
                  CHANGING cg_data = lv_class ).

    ls_skwf_io-objid = lv_io.

    IF lv_folder = abap_true.
      li_api->create_folder(
        EXPORTING
          i_url              = lv_url
          i_language         = mv_language
          i_dev_package      = iv_package
          i_folder_loio      = ls_skwf_io
        EXCEPTIONS
          parameter_missing  = 1
          error_occured      = 2
          cancelled          = 3
          permission_failure = 4
          folder_exists      = 5
          OTHERS             = 6 ).
      IF sy-subrc <> 5 AND sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ELSE.
      lv_filename = get_filename( lv_url ).
      ls_skwf_io-class = lv_class.
      IF ls_skwf_io-class IS INITIAL.
        cl_wb_mime_repository=>determine_io_class(
          EXPORTING
            filename = lv_filename
          IMPORTING
            io_class = ls_skwf_io-class ).
        CONCATENATE ls_skwf_io-class '_L' INTO ls_skwf_io-class.
      ENDIF.

      lv_content = find_content( lv_url ).

      li_api->put(
        EXPORTING
          i_url                   = lv_url
          i_content               = lv_content
          i_dev_package           = iv_package
          i_new_loio              = ls_skwf_io
        EXCEPTIONS
          parameter_missing       = 1
          error_occured           = 2
          cancelled               = 3
          permission_failure      = 4
          data_inconsistency      = 5
          new_loio_already_exists = 6
          is_folder               = 7
          OTHERS                  = 8 ).
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_loio TYPE sdok_docid.


    lv_loio = ms_item-obj_name.

    SELECT SINGLE loio_id FROM smimloio INTO lv_loio
      WHERE loio_id = lv_loio.                          "#EC CI_GENBUFF
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

    DATA: lv_url      TYPE string,
          lv_folder   TYPE abap_bool,
          lv_filename TYPE string,
          lv_class    TYPE smimloio-lo_class,
          ls_file     TYPE Lif_abapgit_git_definitions=>ty_file,
          lv_content  TYPE xstring,
          li_api      TYPE REF TO if_mr_api,
          lv_loio     TYPE sdok_docid.


    lv_loio = ms_item-obj_name.

    TRY.
        get_url_for_io(
          IMPORTING
            ev_url       = lv_url
            ev_is_folder = lv_folder ).
      CATCH Lcx_abapgit_not_found.
        RETURN.
    ENDTRY.

    IF lv_folder = abap_false.
      li_api = cl_mime_repository_api=>if_mr_api~get_api( ).
      li_api->get(
        EXPORTING
          i_url              = lv_url
        IMPORTING
          e_content          = lv_content
        EXCEPTIONS
          parameter_missing  = 1
          error_occured      = 2
          not_found          = 3
          permission_failure = 4
          OTHERS             = 5 ).
      IF sy-subrc <> 0 AND sy-subrc <> 2 AND sy-subrc <> 3.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      lv_filename = get_filename( lv_url ).
      CLEAR ls_file.
      ls_file-filename = build_filename( lv_filename ).
      ls_file-path     = '/'.
      ls_file-data     = lv_content.
      Lif_abapgit_object~mo_files->add( ls_file ).

      SELECT SINGLE lo_class FROM smimloio INTO lv_class
        WHERE loio_id = lv_loio.                        "#EC CI_GENBUFF
    ENDIF.

    io_xml->add( iv_name = 'URL'
                 ig_data = lv_url ).
    io_xml->add( iv_name = 'FOLDER'
                 ig_data = lv_folder ).
    io_xml->add( iv_name = 'CLASS'
                 ig_data = lv_class ).

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
endclass. "ZCL_ABAPGIT_OBJECT_SMIM implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SPLO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_splo=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_splo=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SPLO implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE chgname1 FROM tsp1d INTO rv_user
      WHERE papart = ms_item-obj_name.
    IF sy-subrc <> 0 OR rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DELETE FROM tsp1t WHERE papart = ms_item-obj_name. "#EC CI_NOFIRST "#EC CI_SUBRC
    DELETE FROM tsp1d WHERE papart = ms_item-obj_name.    "#EC CI_SUBRC
    DELETE FROM tsp0p WHERE pdpaper = ms_item-obj_name.   "#EC CI_SUBRC

    set_default_transport( iv_transport ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_tsp1t TYPE tsp1t,
          ls_tsp1d TYPE tsp1d,
          ls_tsp0p TYPE tsp0p.


    io_xml->read( EXPORTING iv_name = 'TSPLT'
                  CHANGING cg_data = ls_tsp1t ).
    io_xml->read( EXPORTING iv_name = 'TSPLD'
                  CHANGING cg_data = ls_tsp1d ).
    io_xml->read( EXPORTING iv_name = 'TSP0P'
                  CHANGING cg_data = ls_tsp0p ).

    MODIFY tsp1t FROM ls_tsp1t.                           "#EC CI_SUBRC
    MODIFY tsp1d FROM ls_tsp1d.                           "#EC CI_SUBRC
    MODIFY tsp0p FROM ls_tsp0p.                           "#EC CI_SUBRC

    set_default_transport( iv_transport ).

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_papart TYPE tsp1d-papart.


    SELECT SINGLE papart INTO lv_papart FROM tsp1d
      WHERE papart = ms_item-obj_name.
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

    DATA: ls_tsp1t TYPE tsp1t,
          ls_tsp1d TYPE tsp1d,
          ls_tsp0p TYPE tsp0p.


    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM tsp1t INTO ls_tsp1t
      WHERE papart = ms_item-obj_name
      AND spras = mv_language.            "#EC CI_GENBUFF "#EC CI_SUBRC
    SELECT SINGLE * FROM tsp1d INTO ls_tsp1d
      WHERE papart = ms_item-obj_name.                    "#EC CI_SUBRC
    SELECT SINGLE * FROM tsp0p INTO ls_tsp0p
      WHERE pdpaper = ms_item-obj_name.                   "#EC CI_SUBRC

    CLEAR: ls_tsp1d-chgname1,
           ls_tsp1d-chgtstmp1,
           ls_tsp1d-chgsaprel1,
           ls_tsp1d-chgsapsys1.

    io_xml->add( iv_name = 'TSPLT'
                 ig_data = ls_tsp1t ).
    io_xml->add( iv_name = 'TSPLD'
                 ig_data = ls_tsp1d ).
    io_xml->add( iv_name = 'TSP0P'
                 ig_data = ls_tsp0p ).

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
endclass. "ZCL_ABAPGIT_OBJECT_SPLO implementation

*>>>>>>> ZCL_ABAPGIT_OO_FACTORY <<<<<<<*

*"* macro definitions
*include zcl_abapgit_oo_factory========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_oo_factory========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OO_FACTORY implementation.
*"* method's implementations
*include methods.
  METHOD make.
    IF gi_object_oriented_object IS BOUND.
      ri_object_oriented_object = gi_object_oriented_object.
      RETURN.
    ENDIF.
    IF iv_object_type = 'CLAS'.
      CREATE OBJECT ri_object_oriented_object TYPE Lcl_abapgit_oo_class.
    ELSEIF iv_object_type = 'INTF'.
      CREATE OBJECT ri_object_oriented_object TYPE Lcl_abapgit_oo_interface.
    ENDIF.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OO_FACTORY implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SPRX <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sprx=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sprx=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SPRX implementation.
*"* method's implementations
*include methods.
  METHOD check_sprx_tadir.

    DATA: lt_abap_keys TYPE prx_abapobjects,
          ls_abap_key  LIKE LINE OF lt_abap_keys,
          lx_error     TYPE REF TO cx_proxy_gen_error.

    ls_abap_key-object   = mv_object.
    ls_abap_key-obj_name = mv_obj_name.
    APPEND ls_abap_key TO lt_abap_keys.

    TRY.
        cl_proxy_utils=>check_sprx_tadir(
            objects = lt_abap_keys
            repair  = abap_true ).

      CATCH cx_proxy_gen_error INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    get_object_and_name(
      IMPORTING
        ev_object   = mv_object
        ev_obj_name = mv_obj_name ).

  ENDMETHOD.
  METHOD delta_handling.

    DATA: lo_proxy   TYPE REF TO cl_proxy,
          lt_delta   TYPE sprx_t_delta,
          ls_db_data TYPE sprx_db_data.

    "add Delta-Handling to avoid that single objects created without the dependent objects.
    "Thereby the dependent objects will be deleted
    TRY.
        lo_proxy = cl_proxy_fact=>load_by_abap_name(
                       object   = mv_object
                       obj_name = mv_obj_name ).


        lt_delta = lo_proxy->get_delta_all( ).

        ls_db_data = cl_proxy_db=>serialize(
                         proxy    = lo_proxy
                         inactive = abap_false
                         delta    = lt_delta ).

        et_sproxhdr_new = ls_db_data-sproxhdr.
        et_sproxdat_new = ls_db_data-sproxdat.

      CATCH cx_proxy_gen_error.
        "No delta for this object -> create

        ii_xml->read(
          EXPORTING
            iv_name = c_proxy-header
          CHANGING
            cg_data = et_sproxhdr_new ).

        IF et_sproxhdr_new IS INITIAL.
          Lcx_abapgit_exception=>raise( |SPRX - error deserialize: { ms_item-obj_name }| ).
        ENDIF.

        ii_xml->read(
          EXPORTING
            iv_name = c_proxy-data
          CHANGING
            cg_data = et_sproxdat_new ).

    ENDTRY.

  ENDMETHOD.
  METHOD get_object_and_name.

    ev_object   = ms_item-obj_name(4).
    ev_obj_name = ms_item-obj_name+4.

  ENDMETHOD.
  METHOD load_db.

* method cl_proxy_db=>load_by_abap_name does not exist in lower releases

    DATA: lt_packages TYPE prx_t_namespace_package,
          ls_package  LIKE LINE OF lt_packages,
          ls_hdr      TYPE prx_s_proxy_hdr,
          lv_package  TYPE tadir-devclass,
          lt_ids      TYPE prx_ids.

    cl_proxy_query=>get_hdr_by_abap_name(
      EXPORTING
        object   = mv_object
        obj_name = mv_obj_name
      IMPORTING
        hdr      = ls_hdr ).
    APPEND ls_hdr-id TO lt_ids.

    IF ls_hdr-gen_appl = 'WEBSERVICES'.
      cl_proxy_utils=>get_package(
        EXPORTING
          object   = mv_object
          obj_name = mv_obj_name
        RECEIVING
          rval     = lv_package
        EXCEPTIONS
          OTHERS   = 0 ).

      ls_package-namespace = ls_hdr-esr_nspce.
      ls_package-prefix    = ls_hdr-prefix.
      ls_package-package   = lv_package.
      APPEND ls_package TO lt_packages.
    ENDIF.

    rs_data = cl_proxy_db=>load(
      inactive               = abap_false
      ids                    = lt_ids
      generating_application = ls_hdr-gen_appl
      packages               = lt_packages ).

  ENDMETHOD.
  METHOD save.

    DATA:
      lt_sproxhdr_old  TYPE sprx_hdr_t,
      lt_sproxdat_old  TYPE sprx_dat_t,
      lt_sproxsvar_old TYPE sprx_svar_t,
      lt_sproxintf_old TYPE sprx_matchintf_t,
      lt_sproxsvar_new TYPE sprx_svar_t,
      lt_sproxintf_new TYPE sprx_matchintf_t.

    cl_proxy_data=>db_save(
        sproxhdr_old  = lt_sproxhdr_old
        sproxdat_old  = lt_sproxdat_old
        sproxsvar_old = lt_sproxsvar_old
        sproxintf_old = lt_sproxintf_old
        sproxhdr_new  = it_sproxhdr_new
        sproxdat_new  = it_sproxdat_new
        sproxsvar_new = lt_sproxsvar_new
        sproxintf_new = lt_sproxintf_new ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA lv_changed_by TYPE sproxhdr-changed_by.

    rv_user = c_user_unknown.

    SELECT SINGLE changed_by
      FROM sproxhdr
      INTO lv_changed_by
      WHERE object = mv_object
      AND obj_name = mv_obj_name
      AND inactive = abap_false.

    IF sy-subrc = 0 AND lv_changed_by IS NOT INITIAL.
      rv_user = lv_changed_by.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA:
      lv_object      TYPE sproxhdr-object,
      lv_obj_name    TYPE sproxhdr-obj_name,
      lv_transp_flag TYPE abap_bool,
      lv_return_code TYPE i,
      lt_log         TYPE sprx_log_t.

    IF iv_package(1) <> '$'.
      lv_transp_flag = abap_true.
    ENDIF.

    get_object_and_name(
      IMPORTING
        ev_object   = lv_object
        ev_obj_name = lv_obj_name ).

    TRY.
        CALL METHOD ('CL_PROXY_DATA')=>('DELETE_SINGLE_PROXY')
          EXPORTING
            object           = lv_object
            obj_name         = lv_obj_name
            i_transport      = lv_transp_flag
            suppress_dialogs = abap_true
          CHANGING
            c_return_code    = lv_return_code
            ct_log           = lt_log.
      CATCH cx_root.
        cl_proxy_data=>delete_single_proxy(
           EXPORTING
             object           = lv_object
             obj_name         = lv_obj_name
             i_transport      = lv_transp_flag
           CHANGING
             c_return_code    = lv_return_code
             ct_log           = lt_log ).
    ENDTRY.
    IF lv_return_code <> 0.
      Lcx_abapgit_exception=>raise( 'SPRX: Error from DELETE_SINGLE_PROXY' ).
    ENDIF.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lt_sproxhdr_new TYPE sprx_hdr_t,
          lt_sproxdat_new TYPE sprx_dat_t.

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

    delta_handling(
      EXPORTING
        ii_xml = io_xml
      IMPORTING
        et_sproxhdr_new = lt_sproxhdr_new
        et_sproxdat_new = lt_sproxdat_new ).

    save(
      it_sproxhdr_new = lt_sproxhdr_new
      it_sproxdat_new = lt_sproxdat_new ).

    COMMIT WORK.

    check_sprx_tadir( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA:
      lv_status      TYPE prx_status,
      lv_status_text TYPE prx_status_t.

    cl_proxy_data=>db_get_status(
      EXPORTING
        object      = mv_object
        obj_name    = mv_obj_name
      IMPORTING
        status      = lv_status
        status_text = lv_status_text ).

    rv_bool = boolc( lv_status = if_proxy=>c_state_active ).

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
    rv_active = abap_true. "dummy implementation
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA:
      ls_sprx_db_data TYPE sprx_db_data.

    FIELD-SYMBOLS:
      <ls_sproxheader> LIKE LINE OF ls_sprx_db_data-sproxhdr,
      <ls_sproxdat>    LIKE LINE OF ls_sprx_db_data-sproxdat.


    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    ls_sprx_db_data = load_db( ).

    DELETE ls_sprx_db_data-sproxhdr WHERE object <> mv_object OR obj_name <> mv_obj_name.
    DELETE ls_sprx_db_data-sproxdat WHERE object <> mv_object OR obj_name <> mv_obj_name.
    DELETE ls_sprx_db_data-sproxsvar WHERE object <> mv_object OR obj_name <> mv_obj_name.
    DELETE ls_sprx_db_data-sproxpck WHERE object <> mv_object OR obj_name <> mv_obj_name.
    DELETE ls_sprx_db_data-sproxintf WHERE object <> mv_object OR obj_name <> mv_obj_name.

    IF lines( ls_sprx_db_data-sproxhdr ) <> 1.
      Lcx_abapgit_exception=>raise( |SPRX, no header found, { mv_object }, { mv_obj_name }| ).
    ENDIF.

    LOOP AT ls_sprx_db_data-sproxhdr ASSIGNING <ls_sproxheader>.

      CLEAR:
        <ls_sproxheader>-created_by,
        <ls_sproxheader>-created_on,
        <ls_sproxheader>-changed_by,
        <ls_sproxheader>-changed_on.

    ENDLOOP.

    LOOP AT ls_sprx_db_data-sproxdat ASSIGNING <ls_sproxdat>.

      CLEAR <ls_sproxdat>-warnings.

    ENDLOOP.

    io_xml->add(
        iv_name = c_proxy-header
        ig_data = ls_sprx_db_data-sproxhdr ).

    io_xml->add(
        iv_name = c_proxy-data
        ig_data = ls_sprx_db_data-sproxdat ).

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
endclass. "ZCL_ABAPGIT_OBJECT_SPRX implementation

*>>>>>>> ZCL_ABAPGIT_PATH <<<<<<<*

*"* macro definitions
*include zcl_abapgit_path==============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_path==============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_path==============ccau.




class LCL_ABAPGIT_PATH implementation.
*"* method's implementations
*include methods.
  METHOD change_dir.

    DATA: lv_last TYPE i,
          lv_temp TYPE string.

    lv_last = strlen( iv_cur_dir ) - 1.

    IF iv_cd = '' OR iv_cd = '.'. " No change
      rv_path = iv_cur_dir.
    ELSEIF iv_cd+0(1) = '/'.      " Absolute path
      rv_path = iv_cd.
    ELSEIF iv_cd = '..'.          " CD back
      IF iv_cur_dir = '/' OR iv_cur_dir = ''. " Back from root = root
        rv_path = iv_cur_dir.
      ELSE.
        lv_temp = reverse( iv_cur_dir ).
        IF lv_temp+0(1) = '/'.
          SHIFT lv_temp BY 1 PLACES LEFT.
        ENDIF.
        SHIFT lv_temp UP TO '/' LEFT.
        rv_path = reverse( lv_temp ).
      ENDIF.
    ELSEIF iv_cur_dir+lv_last(1) = '/'.  " Append cd to cur_dir separated by /
      rv_path = iv_cur_dir && iv_cd.
    ELSE.
      rv_path = iv_cur_dir && '/' && iv_cd.
    ENDIF.

    " TODO: improve logic and cases

  ENDMETHOD.
  METHOD get_filename_from_syspath.

    DATA: lv_split TYPE c LENGTH 1,
          lv_index TYPE i,
          lt_split TYPE TABLE OF string.

    " filename | c:\filename | /dir/filename | \\server\filename
    IF iv_path CA '/'.
      lv_split = '/'.
    ELSE.
      lv_split = '\'.
    ENDIF.

    SPLIT iv_path AT lv_split INTO TABLE lt_split.

    lv_index = lines( lt_split ).

    READ TABLE lt_split INDEX lv_index INTO rv_filename.

  ENDMETHOD.
  METHOD is_root.
    rv_yes = boolc( iv_path = '/' ).
  ENDMETHOD.
  METHOD is_subdir.

    DATA lv_len  TYPE i.
    DATA lv_last TYPE i.

    lv_len  = strlen( iv_parent ).
    lv_last = lv_len - 1.
    rv_yes  = boolc( strlen( iv_path ) > lv_len
                 AND iv_path+0(lv_len) = iv_parent
                 AND ( iv_parent+lv_last(1) = '/' OR iv_path+lv_len(1) = '/' ) ).

  ENDMETHOD.
  METHOD split_file_location.

    DATA: lv_cnt TYPE i,
          lv_len TYPE i.

    FIND FIRST OCCURRENCE OF REGEX '^/(.*/)?' IN iv_fullpath
      MATCH COUNT lv_cnt
      MATCH LENGTH lv_len.

    IF lv_cnt > 0.
      ev_path     = iv_fullpath+0(lv_len).
      ev_filename = iv_fullpath+lv_len.
    ELSE.
      CLEAR ev_path.
      ev_filename = iv_fullpath.
    ENDIF.

    ev_filename = cl_http_utility=>unescape_url( escaped = ev_filename ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_PATH implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SRFC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_srfc=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_srfc=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SRFC implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    DATA li_srfc_persist TYPE REF TO if_wb_object_persist.

    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).

    TRY.
        CREATE OBJECT li_srfc_persist TYPE ('CL_UCONRFC_OBJECT_PERSIST').
      CATCH cx_root.
        Lcx_abapgit_exception=>raise( 'Object type SRFC is not supported by this system' ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: li_object_data  TYPE REF TO if_wb_object_data_model,
          li_srfc_persist TYPE REF TO if_wb_object_persist,
          lr_srfc_data    TYPE REF TO data,
          lx_error        TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_srfc_data> TYPE any,
                   <lg_any>       TYPE any.

    TRY.
        CREATE DATA lr_srfc_data TYPE ('UCONRFCSERV_COMPLETE').
        ASSIGN lr_srfc_data->* TO <lg_srfc_data>.
        ASSERT sy-subrc = 0.

        CREATE OBJECT li_srfc_persist TYPE ('CL_UCONRFC_OBJECT_PERSIST').

        li_srfc_persist->get(
          EXPORTING
            p_object_key  = |{ ms_item-obj_name }|
            p_version     = 'A'
          CHANGING
            p_object_data = li_object_data ).

        li_object_data->get_data( IMPORTING p_data = <lg_srfc_data> ).

        ASSIGN COMPONENT 'HEADER-CHANGEDBY' OF STRUCTURE <lg_srfc_data> TO <lg_any>.
        IF sy-subrc = 0 AND <lg_any> IS NOT INITIAL.
          rv_user = <lg_any>.
        ELSE.
          rv_user = c_user_unknown.
        ENDIF.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: li_srfc_persist TYPE REF TO if_wb_object_persist,
          lx_error        TYPE REF TO cx_root.

    TRY.
        CREATE OBJECT li_srfc_persist TYPE ('CL_UCONRFC_OBJECT_PERSIST').

        li_srfc_persist->delete( p_object_key = |{ ms_item-obj_name }|
                                 p_version    = 'A' ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: li_srfc_persist TYPE REF TO if_wb_object_persist,
          li_object_data  TYPE REF TO if_wb_object_data_model,
          lr_srfc_data    TYPE REF TO data,
          lx_error        TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_srfc_data> TYPE any,
                   <lg_any>       TYPE any.

    TRY.
        CREATE DATA lr_srfc_data TYPE ('UCONRFCSERV_COMPLETE').
        ASSIGN lr_srfc_data->* TO <lg_srfc_data>.
        ASSERT sy-subrc = 0.

        ASSIGN COMPONENT 'HEADER-CREATEDBY' OF STRUCTURE <lg_srfc_data> TO <lg_any>.
        IF sy-subrc = 0.
          <lg_any> = sy-uname.
        ENDIF.

        ASSIGN COMPONENT 'HEADER-CREATEDON' OF STRUCTURE <lg_srfc_data> TO <lg_any>.
        IF sy-subrc = 0.
          <lg_any> = sy-datum.
        ENDIF.

        ASSIGN COMPONENT 'HEADER-CREATEDAT' OF STRUCTURE <lg_srfc_data> TO <lg_any>.
        IF sy-subrc = 0.
          <lg_any> = sy-uzeit.
        ENDIF.

        io_xml->read(
          EXPORTING
            iv_name = 'SRFC'
          CHANGING
            cg_data = <lg_srfc_data> ).

        CREATE OBJECT li_srfc_persist TYPE ('CL_UCONRFC_OBJECT_PERSIST').
        CREATE OBJECT li_object_data TYPE ('CL_UCONRFC_OBJECT_DATA').

        li_object_data->set_data( <lg_srfc_data> ).

        li_srfc_persist->save( li_object_data ).

        tadir_insert( iv_package ).

        corr_insert( iv_package ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: li_object_data  TYPE REF TO if_wb_object_data_model,
          li_srfc_persist TYPE REF TO if_wb_object_persist.

    TRY.
        CREATE OBJECT li_srfc_persist TYPE ('CL_UCONRFC_OBJECT_PERSIST').

        li_srfc_persist->get(
          EXPORTING
            p_object_key  = |{ ms_item-obj_name }|
            p_version     = 'A'
          CHANGING
            p_object_data = li_object_data ).

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

    DATA: li_object_data  TYPE REF TO if_wb_object_data_model,
          li_srfc_persist TYPE REF TO if_wb_object_persist,
          lr_srfc_data    TYPE REF TO data,
          lx_error        TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_srfc_data> TYPE any,
                   <lg_any>       TYPE any.

    TRY.
        CREATE DATA lr_srfc_data TYPE ('UCONRFCSERV_COMPLETE').
        ASSIGN lr_srfc_data->* TO <lg_srfc_data>.
        ASSERT sy-subrc = 0.

        CREATE OBJECT li_srfc_persist TYPE ('CL_UCONRFC_OBJECT_PERSIST').

        li_srfc_persist->get(
          EXPORTING
            p_object_key  = |{ ms_item-obj_name }|
            p_version     = 'A'
          CHANGING
            p_object_data = li_object_data ).

        li_object_data->get_data( IMPORTING p_data = <lg_srfc_data> ).

        ASSIGN COMPONENT 'HEADER-CREATEDBY' OF STRUCTURE <lg_srfc_data> TO <lg_any>.
        IF sy-subrc = 0.
          CLEAR <lg_any>.
        ENDIF.

        ASSIGN COMPONENT 'HEADER-CREATEDON' OF STRUCTURE <lg_srfc_data> TO <lg_any>.
        IF sy-subrc = 0.
          CLEAR <lg_any>.
        ENDIF.

        ASSIGN COMPONENT 'HEADER-CREATEDAT' OF STRUCTURE <lg_srfc_data> TO <lg_any>.
        IF sy-subrc = 0.
          CLEAR <lg_any>.
        ENDIF.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    io_xml->add( iv_name = 'SRFC'
                 ig_data = <lg_srfc_data> ).

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
endclass. "ZCL_ABAPGIT_OBJECT_SRFC implementation

*>>>>>>> ZCL_ABAPGIT_PERSIST_BACKGROUND <<<<<<<*

*"* macro definitions
*include zcl_abapgit_persist_backgroundccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_persist_backgroundccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_PERSIST_BACKGROUND implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    mo_db = Lcl_abapgit_persistence_db=>get_instance( ).
  ENDMETHOD.
  METHOD delete.

    TRY.
        mo_db->read( iv_type  = Lcl_abapgit_persistence_db=>c_type_background
                     iv_value = iv_key ).
      CATCH Lcx_abapgit_not_found.
        RETURN.
    ENDTRY.

    mo_db->delete( iv_type  = Lcl_abapgit_persistence_db=>c_type_background
                   iv_value = iv_key ).

  ENDMETHOD.
  METHOD exists.

    TRY.
        mo_db->read( iv_type  = Lcl_abapgit_persistence_db=>c_type_background
                     iv_value = iv_key ).
        rv_yes = abap_true.
      CATCH Lcx_abapgit_not_found.
        rv_yes = abap_false.
    ENDTRY.

  ENDMETHOD.
  METHOD from_xml.
    CALL TRANSFORMATION id
      OPTIONS value_handling = 'accept_data_loss'
      SOURCE XML iv_string
      RESULT data = rs_xml.
  ENDMETHOD.
  METHOD get_by_key.

    DATA: lt_list TYPE ty_background_keys.

    lt_list = list( ).

    READ TABLE lt_list WITH KEY key = iv_key INTO rs_data.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE Lcx_abapgit_not_found.
    ENDIF.

  ENDMETHOD.
  METHOD list.

    DATA: lt_list TYPE Lif_abapgit_persistence=>ty_contents,
          ls_xml  TYPE ty_xml.

    FIELD-SYMBOLS: <ls_list>   LIKE LINE OF lt_list,
                   <ls_output> LIKE LINE OF rt_list.


    lt_list = mo_db->list_by_type( Lcl_abapgit_persistence_db=>c_type_background ).

    LOOP AT lt_list ASSIGNING <ls_list>.
      ls_xml = from_xml( <ls_list>-data_str ).

      APPEND INITIAL LINE TO rt_list ASSIGNING <ls_output>.
      MOVE-CORRESPONDING ls_xml TO <ls_output>.
      <ls_output>-key = <ls_list>-value.
    ENDLOOP.

  ENDMETHOD.
  METHOD modify.

    ASSERT NOT is_data-key IS INITIAL.

    mo_db->modify(
      iv_type  = Lcl_abapgit_persistence_db=>c_type_background
      iv_value = is_data-key
      iv_data  = to_xml( is_data ) ).

  ENDMETHOD.
  METHOD to_xml.
    DATA: ls_xml TYPE ty_xml.

    MOVE-CORRESPONDING is_background TO ls_xml.

    CALL TRANSFORMATION id
      SOURCE data = ls_xml
      RESULT XML rv_string.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_PERSIST_BACKGROUND implementation

*>>>>>>> ZCL_ABAPGIT_PERSIST_PACKAGES <<<<<<<*

*"* macro definitions
*include zcl_abapgit_persist_packages==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_persist_packages==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_persist_packages==ccau.


class LCL_ABAPGIT_PERSIST_PACKAGES implementation.
*"* method's implementations
*include methods.
  METHOD from_xml.

    DATA lo_input TYPE REF TO Lif_abapgit_xml_input.

    CREATE OBJECT lo_input TYPE Lcl_abapgit_xml_input EXPORTING iv_xml = iv_xml.

    lo_input->read(
      EXPORTING
        iv_name = Lcl_abapgit_persistence_db=>c_type_packages
      CHANGING
        cg_data = rt_packages ).

  ENDMETHOD.
  METHOD get_instance.

    IF go_persist IS NOT BOUND.
      CREATE OBJECT go_persist.
    ENDIF.
    ro_persist = go_persist.

  ENDMETHOD.
  METHOD init.

    TRY.
        " Might have changed in another session so always get latest
        mt_packages = from_xml( Lcl_abapgit_persistence_db=>get_instance( )->read(
          iv_type  = Lcl_abapgit_persistence_db=>c_type_packages
          iv_value = '' ) ).
      CATCH Lcx_abapgit_exception Lcx_abapgit_not_found ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.
  METHOD modify.

    DATA ls_package LIKE LINE OF mt_packages.

    FIELD-SYMBOLS <ls_package> LIKE LINE OF mt_packages.

    init( ).

    IF iv_component IS INITIAL AND iv_comp_posid IS INITIAL.
      DELETE mt_packages WHERE devclass = iv_package.
    ELSE.
      READ TABLE mt_packages ASSIGNING <ls_package> WITH TABLE KEY devclass = iv_package.
      IF sy-subrc = 0.
        <ls_package>-component  = iv_component.
        <ls_package>-comp_posid = iv_comp_posid.
      ELSE.
        ls_package-devclass   = iv_package.
        ls_package-component  = iv_component.
        ls_package-comp_posid = iv_comp_posid.
        INSERT ls_package INTO TABLE mt_packages.
      ENDIF.
    ENDIF.

    Lcl_abapgit_persistence_db=>get_instance( )->modify(
      iv_type       = Lcl_abapgit_persistence_db=>c_type_packages
      iv_value      = ''
      iv_data       = to_xml( mt_packages ) ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.
  METHOD read.

    init( ).

    READ TABLE mt_packages INTO rs_package WITH TABLE KEY devclass = iv_package.
    IF sy-subrc <> 0.
      rs_package-devclass = iv_package. " no component
    ENDIF.

  ENDMETHOD.
  METHOD to_xml.

    DATA li_output TYPE REF TO Lif_abapgit_xml_output.

    CREATE OBJECT li_output TYPE Lcl_abapgit_xml_output.

    li_output->add(
      iv_name = Lcl_abapgit_persistence_db=>c_type_packages
      ig_data = it_packages ).

    rv_xml = li_output->render( ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_PERSIST_PACKAGES implementation

*>>>>>>> ZCL_ABAPGIT_PERSIST_SETTINGS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_persist_settings==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_persist_settings==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_PERSIST_SETTINGS implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_persist_settings~modify.

    DATA: lv_settings      TYPE string,
          ls_user_settings TYPE Lif_abapgit_definitions=>ty_s_user_settings.


    lv_settings = io_settings->get_settings_xml( ).

    Lcl_abapgit_persistence_db=>get_instance( )->modify(
      iv_type       = Lcl_abapgit_persistence_db=>c_type_settings
      iv_value      = ''
      iv_data       = lv_settings ).

    ls_user_settings = io_settings->get_user_settings( ).

    Lcl_abapgit_persistence_user=>get_instance( )->set_settings( ls_user_settings ).

    " Settings have been modified: Update Buffered Settings
    IF mo_settings IS BOUND.
      mo_settings->set_xml_settings( lv_settings ).
      mo_settings->set_user_settings( ls_user_settings ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_settings~read.

    IF mo_settings IS BOUND.
      " Return Buffered Settings
      ro_settings = mo_settings.
      RETURN.
    ENDIF.

    " Settings have changed or have not yet been loaded
    CREATE OBJECT ro_settings.

    TRY.

        ro_settings->set_xml_settings(
          Lcl_abapgit_persistence_db=>get_instance( )->read(
            iv_type  = Lcl_abapgit_persistence_db=>c_type_settings
            iv_value = '' ) ).

        ro_settings->set_user_settings( Lcl_abapgit_persistence_user=>get_instance( )->get_settings( ) ).

      CATCH Lcx_abapgit_not_found Lcx_abapgit_exception.

        ro_settings->set_defaults( ).

    ENDTRY.

    mo_settings = ro_settings.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_PERSIST_SETTINGS implementation

*>>>>>>> ZCL_ABAPGIT_PROXY_AUTH <<<<<<<*

*"* macro definitions
*include zcl_abapgit_proxy_auth========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_proxy_auth========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_PROXY_AUTH implementation.
*"* method's implementations
*include methods.
  METHOD enter.

    Lcl_abapgit_password_dialog=>popup(
      EXPORTING
        iv_repo_url = 'Proxy Authentication'
      CHANGING
        cv_user     = gv_username
        cv_pass     = gv_password ).

    IF gv_username IS INITIAL OR gv_password IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Proxy auth failed' ).
    ENDIF.

  ENDMETHOD.
  METHOD run.

    IF gv_username IS INITIAL OR gv_password IS INITIAL.
      enter( ).
    ENDIF.

    ii_client->authenticate(
      proxy_authentication = abap_true
      username             = gv_username
      password             = gv_password ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_PROXY_AUTH implementation

*>>>>>>> ZCL_ABAPGIT_PROXY_CONFIG <<<<<<<*

*"* macro definitions
*include zcl_abapgit_proxy_config======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_proxy_config======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_PROXY_CONFIG implementation.
*"* method's implementations
*include methods.
  METHOD bypass_proxy.

    DATA lt_proxy_bypass TYPE Lif_abapgit_definitions=>ty_range_proxy_bypass_url.

    lt_proxy_bypass = mo_settings->get_proxy_bypass( ).

    IF lt_proxy_bypass IS NOT INITIAL
    AND iv_repo_url IN lt_proxy_bypass.
      rv_bypass_proxy = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    mo_settings = Lcl_abapgit_persist_factory=>get_settings( )->read( ).

    mi_exit = Lcl_abapgit_exit=>get_instance( ).

  ENDMETHOD.
  METHOD get_proxy_authentication.

    IF bypass_proxy( iv_repo_url ) = abap_false.
      rv_auth = mo_settings->get_proxy_authentication( ).
    ENDIF.

    mi_exit->change_proxy_authentication(
      EXPORTING
        iv_repo_url            = iv_repo_url
      CHANGING
        cv_proxy_authentication = rv_auth ).

  ENDMETHOD.
  METHOD get_proxy_port.

    IF bypass_proxy( iv_repo_url ) = abap_false.
      rv_port = mo_settings->get_proxy_port( ).
    ENDIF.

    mi_exit->change_proxy_port(
      EXPORTING
        iv_repo_url  = iv_repo_url
      CHANGING
        cv_proxy_port = rv_port ).

    CONDENSE rv_port.

  ENDMETHOD.
  METHOD get_proxy_url.

    IF bypass_proxy( iv_repo_url ) = abap_false.
      rv_proxy_url = mo_settings->get_proxy_url( ).
    ENDIF.

    mi_exit->change_proxy_url(
      EXPORTING
        iv_repo_url = iv_repo_url
      CHANGING
        cv_proxy_url = rv_proxy_url ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_PROXY_CONFIG implementation

*>>>>>>> ZCL_ABAPGIT_PR_ENUMERATOR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_pr_enumerator=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_pr_enumerator=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_PR_ENUMERATOR implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    mv_repo_url = to_lower( iv_url ).
    TRY.
        mi_enum_provider = create_provider( mv_repo_url ).
      CATCH Lcx_abapgit_exception.
    ENDTRY.

  ENDMETHOD.
  METHOD create_provider.

    DATA li_agent TYPE REF TO Lif_abapgit_http_agent.
    DATA lv_user TYPE string.
    DATA lv_repo TYPE string.

    li_agent = Lcl_abapgit_factory=>get_http_agent( ).

    FIND ALL OCCURRENCES OF REGEX 'github\.com\/([^\/]+)\/([^\/]+)'
      IN iv_repo_url
      SUBMATCHES lv_user lv_repo.
    IF sy-subrc = 0.
      lv_repo = replace(
        val = lv_repo
        regex = '\.git$'
        with = '' ).
      CREATE OBJECT ri_provider TYPE Lcl_abapgit_pr_enum_github
        EXPORTING
          iv_user_and_repo = |{ lv_user }/{ lv_repo }|
          ii_http_agent    = li_agent.
    ELSE.
      Lcx_abapgit_exception=>raise( |PR enumeration is not supported for { iv_repo_url }| ).
    ENDIF.

    " TODO somewhen more providers

  ENDMETHOD.
  METHOD get_pulls.

    IF mi_enum_provider IS NOT BOUND.
      RETURN.
    ENDIF.

    rt_pulls = mi_enum_provider->list_pull_requests( ).

  ENDMETHOD.
  METHOD new.
    CREATE OBJECT ro_instance EXPORTING iv_url = iv_url.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_PR_ENUMERATOR implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_W3XX_SUPER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_w3xx_super=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_w3xx_super=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_W3XX_SUPER implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    super->constructor( is_item = is_item
                        iv_language = iv_language ).
    ms_key-relid = ms_item-obj_type+2(2).
    ms_key-objid = ms_item-obj_name.
  ENDMETHOD.
  METHOD find_param.

    FIELD-SYMBOLS <ls_param> LIKE LINE OF it_params.


    READ TABLE it_params ASSIGNING <ls_param> WITH KEY name = iv_name.
    IF sy-subrc > 0.
      Lcx_abapgit_exception=>raise( |W3xx: Cannot find { iv_name } for { ms_key-objid }| ).
    ENDIF.

    rv_value = <ls_param>-value.

  ENDMETHOD.
  METHOD get_ext.

    rv_ext = find_param( it_params = it_params
                         iv_name = c_param_names-fileext ).
    SHIFT rv_ext LEFT DELETING LEADING '.'.

  ENDMETHOD.
  METHOD normalize_params.

    FIELD-SYMBOLS <ls_param> LIKE LINE OF ct_params.

    " Ensure filesize param exists
    READ TABLE ct_params ASSIGNING <ls_param> WITH KEY name = c_param_names-filesize.
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO ct_params ASSIGNING <ls_param>.
      <ls_param>-name  = c_param_names-filesize.
    ENDIF.

    LOOP AT ct_params ASSIGNING <ls_param>.
      <ls_param>-relid = ms_key-relid. " Ensure param key = object key
      <ls_param>-objid = ms_key-objid.
      IF <ls_param>-name = c_param_names-filesize. " Patch filesize = real file size
        <ls_param>-value = iv_size.
        CONDENSE <ls_param>-value.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD strip_params.

    FIELD-SYMBOLS <ls_param> LIKE LINE OF ct_params.

    " Remove path from filename
    find_param( it_params = ct_params
                iv_name = c_param_names-filename ). " Check exists
    READ TABLE ct_params ASSIGNING <ls_param> WITH KEY name = c_param_names-filename.
    <ls_param>-value = Lcl_abapgit_path=>get_filename_from_syspath( |{ <ls_param>-value }| ).

    " Clear id and object name
    LOOP AT ct_params ASSIGNING <ls_param>.
      CLEAR: <ls_param>-relid, <ls_param>-objid.
    ENDLOOP.

    " Clear version & filesize
    DELETE ct_params WHERE name = c_param_names-version.
    DELETE ct_params WHERE name = c_param_names-filesize.

    " Avoid diffs due to different order
    SORT ct_params.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE chname INTO rv_user
      FROM wwwdata
      WHERE relid = ms_key-relid
      AND objid = ms_key-objid
      AND srtf2 = 0.

    IF sy-subrc IS NOT INITIAL OR rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    CALL FUNCTION 'WWWDATA_DELETE'
      EXPORTING
        key               = ms_key
      EXCEPTIONS
        wrong_object_type = 1
        delete_error      = 2.

    IF sy-subrc IS NOT INITIAL.
      Lcx_abapgit_exception=>raise( 'Cannot delete W3xx data' ).
    ENDIF.

    CALL FUNCTION 'WWWPARAMS_DELETE_ALL'
      EXPORTING
        key          = ms_key
      EXCEPTIONS
        delete_error = 1.

    IF sy-subrc IS NOT INITIAL.
      Lcx_abapgit_exception=>raise( 'Cannot delete W3xx params' ).
    ENDIF.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA lv_base64str TYPE string.
    DATA lt_w3params  TYPE STANDARD TABLE OF wwwparams.
    DATA lv_xstring   TYPE xstring.
    DATA lt_w3mime    TYPE STANDARD TABLE OF w3mime.
    DATA lt_w3html    TYPE STANDARD TABLE OF w3html.
    DATA lv_size      TYPE i.


    io_xml->read( EXPORTING iv_name = 'TEXT'
                  CHANGING  cg_data = ms_key-text ).

    io_xml->read( EXPORTING iv_name = 'PARAMS'
                  CHANGING  cg_data = lt_w3params ).

    CASE io_xml->get_metadata( )-version.
      WHEN 'v1.0.0'.
        io_xml->read( EXPORTING iv_name = 'DATA'
                      CHANGING  cg_data = lv_base64str ).
        lv_xstring = cl_http_utility=>decode_x_base64( lv_base64str ).
      WHEN 'v2.0.0'.
        lv_xstring = Lif_abapgit_object~mo_files->read_raw( iv_extra = 'data'
                                                    iv_ext   = get_ext( lt_w3params ) ).
      WHEN OTHERS.
        Lcx_abapgit_exception=>raise( 'W3xx: Unknown serializer version' ).
    ENDCASE.

    CASE ms_key-relid.
      WHEN 'MI'.
        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            buffer        = lv_xstring
          IMPORTING
            output_length = lv_size
          TABLES
            binary_tab    = lt_w3mime.
      WHEN 'HT'.
        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            buffer        = lv_xstring
          IMPORTING
            output_length = lv_size
          TABLES
            binary_tab    = lt_w3mime.

        CALL FUNCTION 'SCMS_BINARY_TO_TEXT'
          EXPORTING
            input_length  = lv_size
          IMPORTING
            output_length = lv_size
          TABLES
            binary_tab    = lt_w3mime
            text_tab      = lt_w3html
          EXCEPTIONS
            failed        = 1.
        IF sy-subrc IS NOT INITIAL.
          Lcx_abapgit_exception=>raise( 'Cannot update W3xx params' ).
        ENDIF.

        CLEAR lt_w3mime.
      WHEN OTHERS.
        Lcx_abapgit_exception=>raise( 'Wrong W3xx type' ).
    ENDCASE.

    " Update size of file based on actual data file size, prove param object name
    normalize_params( EXPORTING iv_size   = lv_size
                      CHANGING  ct_params = lt_w3params ).

    CALL FUNCTION 'WWWPARAMS_UPDATE'
      TABLES
        params       = lt_w3params
      EXCEPTIONS
        update_error = 1.

    IF sy-subrc IS NOT INITIAL.
      Lcx_abapgit_exception=>raise( 'Cannot update W3xx params' ).
    ENDIF.

    ms_key-tdate    = sy-datum.
    ms_key-ttime    = sy-uzeit.
    ms_key-chname   = sy-uname.
    ms_key-devclass = iv_package.

    CALL FUNCTION 'WWWDATA_EXPORT'
      EXPORTING
        key               = ms_key
      TABLES
        mime              = lt_w3mime
        html              = lt_w3html
      EXCEPTIONS
        wrong_object_type = 1
        export_error      = 2.

    IF sy-subrc IS NOT INITIAL.
      Lcx_abapgit_exception=>raise( 'Cannot upload W3xx data' ).
    ENDIF.

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    SELECT SINGLE objid INTO ms_key-objid
      FROM wwwdata
      WHERE relid = ms_key-relid
      AND objid = ms_key-objid
      AND srtf2 = 0.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    rv_bool = abap_true.

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
    rs_metadata         = get_metadata( ).
    rs_metadata-version = 'v2.0.0'. " Serialization v2, separate data file
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    DATA: lv_object TYPE eqegraarg.

    lv_object = |{ ms_item-obj_type+2(2) }{ ms_item-obj_name }|.
    OVERLAY lv_object WITH '                                          '.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'E_WWW_HTML'
                                            iv_argument    = lv_object ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: ls_bdcdata TYPE bdcdata,
          lt_bdcdata TYPE ty_bdcdata.

    ls_bdcdata-program  = 'SAPMWWW0'.
    ls_bdcdata-dynpro   = '0100'.
    ls_bdcdata-dynbegin = 'X'.
    APPEND ls_bdcdata TO lt_bdcdata.

    change_bdc_jump_data( CHANGING ct_bdcdata = lt_bdcdata ).

    CLEAR ls_bdcdata.
    ls_bdcdata-fnam = 'BDC_OKCODE'.
    ls_bdcdata-fval = '=CRO1'.
    APPEND ls_bdcdata TO lt_bdcdata.

    ls_bdcdata-program  = 'RSWWWSHW'.
    ls_bdcdata-dynpro   = '1000'.
    ls_bdcdata-dynbegin = 'X'.
    APPEND ls_bdcdata TO lt_bdcdata.

    CLEAR ls_bdcdata.
    ls_bdcdata-fnam     = 'SO_OBJID-LOW'.
    ls_bdcdata-fval     = ms_item-obj_name.
    APPEND ls_bdcdata TO lt_bdcdata.

    CLEAR ls_bdcdata.
    ls_bdcdata-fnam = 'BDC_OKCODE'.
    ls_bdcdata-fval = '=ONLI'.
    APPEND ls_bdcdata TO lt_bdcdata.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SMW0'
      it_bdcdata = lt_bdcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA lt_w3mime    TYPE STANDARD TABLE OF w3mime.
    DATA lt_w3html    TYPE STANDARD TABLE OF w3html.
    DATA lt_w3params  TYPE STANDARD TABLE OF wwwparams.
    DATA lv_xstring   TYPE xstring.
    DATA lv_size      TYPE i.

    SELECT SINGLE * INTO CORRESPONDING FIELDS OF ms_key
      FROM wwwdata
      WHERE relid = ms_key-relid
      AND objid = ms_key-objid
      AND srtf2 = 0.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key               = ms_key
      TABLES
        mime              = lt_w3mime
        html              = lt_w3html
      EXCEPTIONS
        wrong_object_type = 1
        import_error      = 2.

    IF sy-subrc IS NOT INITIAL.
      Lcx_abapgit_exception=>raise( 'Cannot read W3xx data' ).
    ENDIF.

    CALL FUNCTION 'WWWPARAMS_READ_ALL'
      EXPORTING
        type             = ms_key-relid
        objid            = ms_key-objid
      TABLES
        params           = lt_w3params
      EXCEPTIONS
        entry_not_exists = 1.

    IF sy-subrc IS NOT INITIAL.
      Lcx_abapgit_exception=>raise( 'Cannot read W3xx data' ).
    ENDIF.

    lv_size = find_param( it_params = lt_w3params
                          iv_name = c_param_names-filesize ).
    " Clean params (remove version, filesize & clear filename from path)
    strip_params( CHANGING  ct_params = lt_w3params ).

    CASE ms_key-relid.
      WHEN 'MI'.
        CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
          EXPORTING
            input_length = lv_size
          IMPORTING
            buffer       = lv_xstring
          TABLES
            binary_tab   = lt_w3mime
          EXCEPTIONS
            failed       = 1.
      WHEN 'HT'.
        CALL FUNCTION 'SCMS_TEXT_TO_XSTRING'
          IMPORTING
            buffer   = lv_xstring
          TABLES
            text_tab = lt_w3html
          EXCEPTIONS
            failed   = 1.
      WHEN OTHERS.
        Lcx_abapgit_exception=>raise( 'Wrong W3xx type' ).
    ENDCASE.

    IF sy-subrc IS NOT INITIAL.
      Lcx_abapgit_exception=>raise( 'Cannot convert W3xx to xstring' ).
    ENDIF.

    io_xml->add( iv_name = 'NAME'
                 ig_data = ms_key-objid ).

    io_xml->add( iv_name = 'TEXT'
                 ig_data = ms_key-text ).

    SORT lt_w3params.

    io_xml->add( iv_name = 'PARAMS'
                 ig_data = lt_w3params ).

    " Seriazation v2, separate data file. 'extra' added to prevent conflict with .xml
    Lif_abapgit_object~mo_files->add_raw( iv_data  = lv_xstring
                                  iv_extra = 'data'
                                  iv_ext   = get_ext( lt_w3params ) ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_W3XX_SUPER implementation

*>>>>>>> ZCL_ABAPGIT_REPO_CS_MIGRATION <<<<<<<*

*"* macro definitions
*include zcl_abapgit_repo_cs_migration=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_repo_cs_migration=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_REPO_CS_MIGRATION implementation.
*"* method's implementations
*include methods.
  METHOD clear_repo_metadata.

    DATA lo_repo_persistence TYPE REF TO Lcl_abapgit_persistence_repo.

    lo_repo_persistence ?= Lcl_abapgit_persist_factory=>get_repo( ).
    lo_repo_persistence->rewrite_repo_meta( iv_repo_key ).

  ENDMETHOD.
  METHOD convert_checksums.

    DATA lo_cs TYPE REF TO Lcl_abapgit_repo_checksums.
    DATA lv_xml TYPE Lif_abapgit_persistence=>ty_content-data_str.
    DATA:
      BEGIN OF ls_repo_extract,
        local_checksums TYPE Lif_abapgit_persistence=>ty_local_checksum_tt,
      END OF ls_repo_extract.

    lv_xml = Lcl_abapgit_persistence_db=>get_instance( )->read(
      iv_type  = Lcl_abapgit_persistence_db=>c_type_repo
      iv_value = iv_repo_key ).

    REPLACE ALL OCCURRENCES OF '<_--28C_TYPE_REPO_--29>' IN lv_xml WITH '<REPO>'.
    REPLACE ALL OCCURRENCES OF '</_--28C_TYPE_REPO_--29>' IN lv_xml WITH '</REPO>'.

    CALL TRANSFORMATION id
      OPTIONS value_handling = 'accept_data_loss'
      SOURCE XML lv_xml
      RESULT repo = ls_repo_extract.

    IF lines( ls_repo_extract-local_checksums ) = 0.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_cs EXPORTING iv_repo_key = iv_repo_key.
    lo_cs->force_write( ls_repo_extract-local_checksums ).

  ENDMETHOD.
  METHOD get_unconverted_repo_ids.

    DATA lt_cs_ids TYPE ty_repo_ids.
    DATA lv_repo_id LIKE LINE OF rt_repo_ids.
    DATA lv_index TYPE i.

    SELECT value FROM (Lcl_abapgit_persistence_db=>c_tabname)
      INTO TABLE rt_repo_ids
      WHERE type = Lcl_abapgit_persistence_db=>c_type_repo.
    SELECT value FROM (Lcl_abapgit_persistence_db=>c_tabname)
      INTO TABLE lt_cs_ids
      WHERE type = Lcl_abapgit_persistence_db=>c_type_repo_csum.

    LOOP AT rt_repo_ids INTO lv_repo_id.
      lv_index = sy-tabix.
      READ TABLE lt_cs_ids TRANSPORTING NO FIELDS WITH KEY table_line = lv_repo_id.
      IF sy-subrc = 0. " Already converted
        DELETE rt_repo_ids INDEX lv_index.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD run.

    DATA lt_repo_ids TYPE ty_repo_ids.
    DATA lv_repo_id LIKE LINE OF lt_repo_ids.

    lt_repo_ids = get_unconverted_repo_ids( ).

    LOOP AT lt_repo_ids INTO lv_repo_id.
      convert_checksums( lv_repo_id ).
      clear_repo_metadata( lv_repo_id ).
    ENDLOOP.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_REPO_CS_MIGRATION implementation

*>>>>>>> ZCL_ABAPGIT_REPO_FILTER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_repo_filter=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_repo_filter=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_REPO_FILTER implementation.
*"* method's implementations
*include methods.
  METHOD apply.

    DATA: lt_filter TYPE SORTED TABLE OF Lif_abapgit_definitions=>ty_tadir
                      WITH NON-UNIQUE KEY object obj_name,
          lv_index  TYPE i.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF ct_tadir.

    filter_generated_tadir( CHANGING ct_tadir = ct_tadir ).

    IF lines( it_filter ) = 0.
      RETURN.
    ENDIF.

    lt_filter = it_filter.

* this is another loop at TADIR, but typically the filter is blank
    LOOP AT ct_tadir ASSIGNING <ls_tadir>.
      lv_index = sy-tabix.
      READ TABLE lt_filter TRANSPORTING NO FIELDS WITH KEY object = <ls_tadir>-object
                                                           obj_name = <ls_tadir>-obj_name
                                                  BINARY SEARCH.
      IF sy-subrc <> 0.
        DELETE ct_tadir INDEX lv_index.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD apply_object_filter.
    DATA lr_file TYPE REF TO Lif_abapgit_git_definitions=>ty_file.
    DATA ls_item TYPE Lif_abapgit_definitions=>ty_item.
    DATA ls_tadir TYPE Lif_abapgit_definitions=>ty_tadir.
    DATA lt_tadir TYPE Lif_abapgit_definitions=>ty_tadir_tt.
    DATA lt_filter TYPE SORTED TABLE OF Lif_abapgit_definitions=>ty_tadir
                      WITH NON-UNIQUE KEY object obj_name.

    lt_filter = it_filter.

    LOOP AT ct_files REFERENCE INTO lr_file.
      IF lr_file->filename = Lif_abapgit_definitions=>c_dot_abapgit.
        CONTINUE.
      ENDIF.

      Lcl_abapgit_filename_logic=>file_to_object(
        EXPORTING
          iv_filename = lr_file->filename
          iv_path     = lr_file->path
          iv_devclass = iv_devclass
          io_dot      = io_dot
        IMPORTING
          es_item     = ls_item ).

      CLEAR lt_tadir.
      CLEAR ls_tadir.

      ls_tadir-object = ls_item-obj_type.
      ls_tadir-obj_name = ls_item-obj_name.
      ls_tadir-devclass = ls_item-devclass.

      INSERT ls_tadir INTO TABLE lt_tadir.

      filter_generated_tadir( CHANGING ct_tadir = lt_tadir ).

      IF lt_tadir IS INITIAL.
        DELETE ct_files.
        CONTINUE.
      ENDIF.

      READ TABLE lt_filter TRANSPORTING NO FIELDS
      WITH KEY object = ls_tadir-object
               obj_name = ls_tadir-obj_name
      BINARY SEARCH.

      IF sy-subrc <> 0.
        DELETE ct_files.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD filter_generated_tadir.

    DATA: ls_tadir     TYPE Lif_abapgit_definitions=>ty_tadir,
          ls_tadir_gen TYPE Lif_abapgit_definitions=>ty_tadir,
          lv_cd_object TYPE cdobjectcl,
          lt_cd_names  TYPE STANDARD TABLE OF cdnames,
          ls_cd_names  TYPE cdnames,
          lt_tcdrs     TYPE STANDARD TABLE OF tcdrs,
          ls_tcdrs     TYPE tcdrs.

    LOOP AT ct_tadir INTO ls_tadir WHERE pgmid = 'R3TR' AND object = 'CHDO'.
      CLEAR: lv_cd_object, lt_cd_names, ls_tadir_gen, lt_tcdrs, ls_tcdrs.

      lv_cd_object = ls_tadir-obj_name.

      CALL FUNCTION 'CDNAMES_GET'
        EXPORTING
          iv_object        = lv_cd_object
        TABLES
          it_names         = lt_cd_names
          it_tcdrs         = lt_tcdrs
        EXCEPTIONS
          object_space     = 1
          object_not_found = 2
          OTHERS           = 3.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      LOOP AT lt_cd_names INTO ls_cd_names.
        DELETE ct_tadir WHERE pgmid = 'R3TR'
                          AND ( ( object = 'PROG'
                              AND ( obj_name = ls_cd_names-repnamec
                                 OR obj_name = ls_cd_names-repnamet
                                 OR obj_name = ls_cd_names-repnamefix
                                 OR obj_name = ls_cd_names-repnamevar ) )
                               OR object = 'FUGR' AND obj_name = ls_cd_names-fgrp ).
      ENDLOOP.

      LOOP AT lt_tcdrs INTO ls_tcdrs.
        DELETE ct_tadir WHERE pgmid = 'R3TR' AND object = 'TABL' AND obj_name = ls_tcdrs-tabname.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_REPO_FILTER implementation

*>>>>>>> ZCL_ABAPGIT_REQUIREMENT_HELPER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_requirement_helperccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_requirement_helperccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_requirement_helperccau.
CLASS SHRITEFUH64VYIPO5I47WOOA54OASM DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS get_sap_basis_component
      RETURNING
        VALUE(rs_result) TYPE cvers_sdu
      RAISING
        Lcx_abapgit_exception.

ENDCLASS.


CLASS SHRITEFUH64VYIPO5I47WOOA54OASM IMPLEMENTATION.


  METHOD get_sap_basis_component.

    DATA:
      lt_installed TYPE STANDARD TABLE OF cvers_sdu.

    CALL FUNCTION 'DELIVERY_GET_INSTALLED_COMPS'
      TABLES
        tt_comptab       = lt_installed
      EXCEPTIONS
        no_release_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from DELIVERY_GET_INSTALLED_COMPS { sy-subrc }| ).
    ENDIF.

    READ TABLE lt_installed INTO rs_result
      WITH KEY component = `SAP_BASIS`.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Component SAP_BASIS not found| ).
    ENDIF.

  ENDMETHOD.


ENDCLASS.





















class LCL_ABAPGIT_REQUIREMENT_HELPER implementation.
*"* method's implementations
*include methods.
  METHOD get_requirement_met_status.

    DATA: lt_installed TYPE STANDARD TABLE OF cvers_sdu.

    FIELD-SYMBOLS: <ls_requirement>    TYPE Lif_abapgit_dot_abapgit=>ty_requirement,
                   <ls_status>         TYPE ty_requirement_status,
                   <ls_installed_comp> TYPE cvers_sdu.


    CALL FUNCTION 'DELIVERY_GET_INSTALLED_COMPS'
      TABLES
        tt_comptab       = lt_installed
      EXCEPTIONS
        no_release_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from DELIVERY_GET_INSTALLED_COMPS { sy-subrc }| ).
    ENDIF.

    LOOP AT it_requirements ASSIGNING <ls_requirement>.
      APPEND INITIAL LINE TO rt_status ASSIGNING <ls_status>.
      <ls_status>-component = <ls_requirement>-component.
      <ls_status>-required_release = <ls_requirement>-min_release.
      <ls_status>-required_patch = <ls_requirement>-min_patch.

      READ TABLE lt_installed WITH KEY component = <ls_requirement>-component
                              ASSIGNING <ls_installed_comp>.
      IF sy-subrc = 0.
        " Component is installed, requirement is met if the installed version is greater or equal
        " to the required one.
        <ls_status>-installed_release = <ls_installed_comp>-release.
        <ls_status>-installed_patch = <ls_installed_comp>-extrelease.
        <ls_status>-description = <ls_installed_comp>-desc_text.
        <ls_status>-met = is_version_greater_or_equal( <ls_status> ).
      ELSE.
        " Component is not installed at all
        <ls_status>-met = abap_false.
      ENDIF.

      UNASSIGN <ls_installed_comp>.
    ENDLOOP.

  ENDMETHOD.
  METHOD is_requirements_met.

    DATA: lt_met_status TYPE ty_requirement_status_tt.

    lt_met_status = get_requirement_met_status( it_requirements ).

    READ TABLE lt_met_status TRANSPORTING NO FIELDS WITH KEY met = abap_false.
    IF sy-subrc = 0.
      rv_status = Lif_abapgit_definitions=>c_no.
    ELSE.
      rv_status = Lif_abapgit_definitions=>c_yes.
    ENDIF.

  ENDMETHOD.
  METHOD is_version_greater_or_equal.

    DATA:
      lv_installed_release TYPE n LENGTH 4,
      lv_installed_patch   TYPE n LENGTH 4,
      lv_required_release  TYPE n LENGTH 4,
      lv_required_patch    TYPE n LENGTH 4.

    TRY.
        MOVE EXACT: is_status-installed_release TO lv_installed_release,
                    is_status-installed_patch   TO lv_installed_patch,
                    is_status-required_release  TO lv_required_release,
                    is_status-required_patch    TO lv_required_patch.
      CATCH cx_sy_conversion_error.
        " Cannot compare by number, assume requirement not fullfilled (user can force install
        " anyways if this was an error)
        rv_true = abap_false.
        RETURN.
    ENDTRY.

    " Versions are comparable by number, compare release and if necessary patch level
    IF lv_installed_release > lv_required_release
        OR ( lv_installed_release = lv_required_release
         AND ( lv_required_patch = 0
            OR lv_installed_patch >= lv_required_patch ) ).

      rv_true = abap_true.

    ENDIF.

  ENDMETHOD.
  METHOD requirements_popup.

    DATA: lt_met_status TYPE ty_requirement_status_tt,
          lv_answer     TYPE c LENGTH 1.


    lt_met_status = get_requirement_met_status( it_requirements ).

    show_requirement_popup( lt_met_status ).

    lv_answer = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
      iv_titlebar      = 'Warning'
      iv_text_question = 'The project has unmet requirements. Do you want to continue?' ).

    IF lv_answer <> '1'.
      Lcx_abapgit_exception=>raise( 'Cancelling because of unmet requirements.' ).
    ENDIF.

  ENDMETHOD.
  METHOD show_requirement_popup.

    TYPES: BEGIN OF ty_color_line,
             color TYPE lvc_t_scol.
             INCLUDE TYPE ty_requirement_status.
    TYPES: END OF ty_color_line.

    TYPES: ty_color_tab TYPE STANDARD TABLE OF ty_color_line WITH DEFAULT KEY.

    DATA: lo_alv            TYPE REF TO cl_salv_table,
          lo_column         TYPE REF TO cl_salv_column,
          lo_columns        TYPE REF TO cl_salv_columns_table,
          lt_color_table    TYPE ty_color_tab,
          lt_color_negative TYPE lvc_t_scol,
          lt_color_positive TYPE lvc_t_scol,
          ls_color          TYPE lvc_s_scol,
          ls_position       TYPE Lif_abapgit_popups=>ty_popup_position,
          lx_ex             TYPE REF TO cx_root.

    FIELD-SYMBOLS: <ls_line>        TYPE ty_color_line,
                   <ls_requirement> LIKE LINE OF it_requirements.


    ls_color-color-col = col_negative.
    APPEND ls_color TO lt_color_negative.

    ls_color-color-col = col_positive.
    APPEND ls_color TO lt_color_positive.

    CLEAR ls_color.

    LOOP AT it_requirements ASSIGNING <ls_requirement>.
      APPEND INITIAL LINE TO lt_color_table ASSIGNING <ls_line>.
      MOVE-CORRESPONDING <ls_requirement> TO <ls_line>.
    ENDLOOP.

    LOOP AT lt_color_table ASSIGNING <ls_line>.
      IF <ls_line>-met = abap_false.
        <ls_line>-color = lt_color_negative.
      ELSE.
        <ls_line>-color = lt_color_positive.
      ENDIF.
    ENDLOOP.
    UNASSIGN <ls_line>.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv
                                CHANGING t_table       = lt_color_table ).

        lo_columns = lo_alv->get_columns( ).
        lo_columns->get_column( 'MET' )->set_short_text( 'Met' ).
        lo_columns->set_color_column( 'COLOR' ).
        lo_columns->set_optimize( ).

        lo_column = lo_columns->get_column( 'REQUIRED_RELEASE' ).
        lo_column->set_short_text( 'Req. Rel.' ).

        lo_column = lo_columns->get_column( 'REQUIRED_PATCH' ).
        lo_column->set_short_text( 'Req. SP L.' ).

        ls_position = Lcl_abapgit_popups=>center(
          iv_width  = 70
          iv_height = 10 ).

        lo_alv->set_screen_popup( start_column = ls_position-start_column
                                  end_column   = ls_position-end_column
                                  start_line   = ls_position-start_row
                                  end_line     = ls_position-end_row ).

        lo_alv->get_display_settings( )->set_list_header( 'Requirements' ).
        lo_alv->display( ).

      CATCH cx_salv_msg cx_salv_not_found cx_salv_data_error INTO lx_ex.
        Lcx_abapgit_exception=>raise( lx_ex->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_REQUIREMENT_HELPER implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SUSH <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sush=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sush=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SUSH implementation.
*"* method's implementations
*include methods.
  METHOD clear_metadata.

    DATA:
      BEGIN OF ls_empty_metadata,
        modifier  TYPE c LENGTH 12, " usob_sm-modifier
        moddate   TYPE d, " usob_sm-moddate,
        modtime   TYPE t, " usob_sm-modtime,
        srcsystem TYPE tadir-srcsystem,
        author    TYPE tadir-author,
        devclass  TYPE tadir-devclass,
      END OF ls_empty_metadata.

    FIELD-SYMBOLS:
      <ls_usobx>     TYPE any,
      <ls_usbot>     TYPE any,
      <ls_usobt_ext> TYPE any,
      <ls_usobx_ext> TYPE any.

    MOVE-CORRESPONDING ls_empty_metadata TO cs_data_head.

    LOOP AT ct_usobx ASSIGNING <ls_usobx>.
      MOVE-CORRESPONDING ls_empty_metadata TO <ls_usobx>.
    ENDLOOP.

    LOOP AT ct_usobt ASSIGNING <ls_usbot>.
      MOVE-CORRESPONDING ls_empty_metadata TO <ls_usbot>.
    ENDLOOP.

    LOOP AT ct_usobt_ext ASSIGNING <ls_usobt_ext>.
      MOVE-CORRESPONDING ls_empty_metadata TO <ls_usobt_ext>.
    ENDLOOP.

    LOOP AT ct_usobx_ext ASSIGNING <ls_usobx_ext>.
      MOVE-CORRESPONDING ls_empty_metadata TO <ls_usobx_ext>.
    ENDLOOP.

  ENDMETHOD.
  METHOD constructor.

    DATA: lr_data_head TYPE REF TO data.

    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).

    TRY.
        CREATE DATA lr_data_head TYPE ('IF_SU22_ADT_OBJECT=>TS_SU2X_HEAD').

      CATCH cx_sy_create_data_error.
        Lcx_abapgit_exception=>raise( |SUSH is not supported in your release| ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    DATA ls_key TYPE usobkey.

    ls_key = ms_item-obj_name.

    SELECT SINGLE modifier FROM usob_sm INTO rv_user
      WHERE name = ls_key-name AND type = ls_key-type.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.
    DATA:
      lo_su22 TYPE REF TO object,
      ls_key  TYPE        usobkey,
      lx_err  TYPE REF TO cx_static_check.

    ASSERT NOT ms_item-obj_name IS INITIAL.

    ls_key = ms_item-obj_name.

    TRY.
        CREATE OBJECT lo_su22 TYPE ('CL_SU22_ADT_OBJECT').

        CALL METHOD lo_su22->('IF_SU22_ADT_OBJECT~DELETE')
          EXPORTING
            iv_key     = ls_key
            iv_cleanup = abap_true.
      CATCH cx_static_check INTO lx_err.
        Lcx_abapgit_exception=>raise_with_text( lx_err ).
    ENDTRY.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA:
      ls_key            TYPE usobkey,
      lo_su22           TYPE REF TO object,
      lo_appl           TYPE REF TO object,
      lt_usobx          TYPE usobx_t,
      lt_usobt          TYPE usobt_t,
      lr_appl_head      TYPE REF TO data,
      lr_data_head      TYPE REF TO data,
      lr_data_usobx_ext TYPE REF TO data,
      lr_data_usobt_ext TYPE REF TO data,
      lx_err            TYPE REF TO cx_static_check,
      lv_text           TYPE string.

    FIELD-SYMBOLS: <ls_data_head>      TYPE any,
                   <ls_appl_head>      TYPE any,
                   <lt_data_usobx_ext> TYPE ANY TABLE,
                   <lt_data_usobt_ext> TYPE ANY TABLE,
                   <ls_devclass>       TYPE any.

    ASSERT NOT ms_item-obj_name IS INITIAL.

    TRY.
        CREATE DATA lr_data_head TYPE ('IF_SU22_ADT_OBJECT=>TS_SU2X_HEAD').
        ASSIGN lr_data_head->* TO <ls_data_head>.

        CREATE DATA lr_data_usobx_ext TYPE ('IF_SU22_ADT_OBJECT=>TT_SU2X_X').
        ASSIGN lr_data_usobx_ext->* TO <lt_data_usobx_ext>.

        CREATE DATA lr_data_usobt_ext TYPE ('IF_SU22_ADT_OBJECT=>TT_SU2X_T').
        ASSIGN lr_data_usobt_ext->* TO <lt_data_usobt_ext>.

        "HEAD
        io_xml->read( EXPORTING iv_name = 'HEAD'
                      CHANGING  cg_data = <ls_data_head> ).

        "USOBX
        io_xml->read( EXPORTING iv_name = 'USOBX'
                      CHANGING  cg_data = lt_usobx ).

        "USOBT
        io_xml->read( EXPORTING iv_name = 'USOBT'
                      CHANGING  cg_data = lt_usobt ).

        "USOBX_EXT
        io_xml->read( EXPORTING iv_name = 'USOBX_EXT'
                      CHANGING  cg_data = <lt_data_usobx_ext> ).

        "USOBT_EXT
        io_xml->read( EXPORTING iv_name = 'USOBT_EXT'
                      CHANGING  cg_data = <lt_data_usobt_ext> ).

        CREATE OBJECT lo_su22
          TYPE ('CL_SU22_ADT_OBJECT').

        " check if lead application exists
        TRY.
            CALL METHOD lo_su22->('IF_SU22_ADT_OBJECT~CHECK')
              EXPORTING
                id_mode = '02'
              CHANGING
                cs_head = <ls_data_head>.
          CATCH cx_static_check INTO lx_err.
            lv_text = |Lead application of object { ms_item-obj_name } does not exist|.
            Lcx_abapgit_exception=>raise( lv_text ).
        ENDTRY.

        MOVE-CORRESPONDING <ls_data_head> TO ls_key.
        CREATE DATA lr_appl_head TYPE ('CL_SU2X=>TS_HEAD').
        ASSIGN lr_appl_head->* TO <ls_appl_head>.

        CREATE OBJECT lo_appl TYPE ('CL_SU22_APPL').

        CALL METHOD lo_appl->('GET_DATA')
          EXPORTING
            is_key  = ls_key
          IMPORTING
            es_head = <ls_appl_head>.

        ASSIGN COMPONENT 'DEVCLASS' OF STRUCTURE <ls_appl_head> TO <ls_devclass>.
        IF <ls_devclass> <> iv_package.
          lv_text =
          |Lead application of object { ms_item-obj_name } does not exist in package { <ls_devclass> }|.
          Lcx_abapgit_exception=>raise( lv_text ).
        ENDIF.

        TRY.
            CALL METHOD lo_su22->('IF_SU22_ADT_OBJECT~UPDATE')
              EXPORTING
                is_head  = <ls_data_head>
                it_usobx = lt_usobx
                it_usobt = lt_usobt.
          CATCH cx_static_check INTO lx_err.
            Lcx_abapgit_exception=>raise_with_text( lx_err ).
        ENDTRY.

        corr_insert( iv_package ).

      CATCH cx_static_check INTO lx_err.
        Lcx_abapgit_exception=>raise_with_text( lx_err ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.
    DATA: ls_usobhash TYPE usobhash.

    SELECT SINGLE * FROM usobhash INTO ls_usobhash "#EC CI_ALL_FIELDS_NEEDED
        WHERE name = ms_item-obj_name.                "#EC CI_SGLSELECT

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
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
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'E_USOBX'
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA:
      ls_key       TYPE usobkey,
      lo_su22      TYPE REF TO object,
      lt_usobx     TYPE usobx_t,
      lt_usobt     TYPE usobt_t,
      lr_head      TYPE REF TO data,
      lr_usobx_ext TYPE REF TO data,
      lr_usobt_ext TYPE REF TO data,
      lx_err       TYPE REF TO cx_static_check.


    FIELD-SYMBOLS: <ls_head>      TYPE any,
                   <lt_usobx_ext> TYPE ANY TABLE,
                   <lt_usobt_ext> TYPE ANY TABLE.

    ls_key = ms_item-obj_name.

    TRY.
        CREATE DATA lr_head TYPE ('IF_SU22_ADT_OBJECT=>TS_SU2X_HEAD').
        ASSIGN lr_head->* TO <ls_head>.

        CREATE DATA lr_usobx_ext TYPE ('IF_SU22_ADT_OBJECT=>TT_SU2X_X').
        ASSIGN lr_usobx_ext->* TO <lt_usobx_ext>.

        CREATE DATA lr_usobt_ext TYPE ('IF_SU22_ADT_OBJECT=>TT_SU2X_T').
        ASSIGN lr_usobt_ext->* TO <lt_usobt_ext>.

        CREATE OBJECT lo_su22
          TYPE ('CL_SU22_ADT_OBJECT').

        TRY.
            CALL METHOD lo_su22->('IF_SU22_ADT_OBJECT~SELECT')
              EXPORTING
                iv_key       = ls_key
              IMPORTING
                es_head      = <ls_head>
                et_usobx     = lt_usobx
                et_usobt     = lt_usobt
                et_usobx_ext = <lt_usobx_ext>
                et_usobt_ext = <lt_usobt_ext>.
          CATCH cx_static_check INTO lx_err.
            Lcx_abapgit_exception=>raise_with_text( lx_err ).
        ENDTRY.

        clear_metadata(
          CHANGING
            cs_data_head = <ls_head>
            ct_usobx     = lt_usobx
            ct_usobt     = lt_usobt
            ct_usobx_ext = <lt_usobx_ext>
            ct_usobt_ext = <lt_usobt_ext> ).

        "HEAD
        io_xml->add( iv_name = 'HEAD'
                     ig_data = <ls_head> ).

        "USOBX
        io_xml->add( iv_name = 'USOBX'
                     ig_data = lt_usobx ).

        "USOBT
        io_xml->add( iv_name = 'USOBT'
                     ig_data = lt_usobt ).

        "USOBX_EXT
        io_xml->add( iv_name = 'USOBX_EXT'
                     ig_data = <lt_usobx_ext> ).

        "USOBT_EXT
        io_xml->add( iv_name = 'USOBT_EXT'
                     ig_data = <lt_usobt_ext> ).

      CATCH cx_static_check INTO lx_err.
        Lcx_abapgit_exception=>raise_with_text( lx_err ).
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
endclass. "ZCL_ABAPGIT_OBJECT_SUSH implementation

*>>>>>>> ZCL_ABAPGIT_SYNTAX_ABAP <<<<<<<*

*"* macro definitions
*include zcl_abapgit_syntax_abap=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_syntax_abap=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_syntax_abap=======ccau.



*CLASS SHRITEFUH64VYIPO5I47WOOA55WASM DEFINITION DEFERRED.
*CLASS zcl_abapgit_syntax_abap DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA55WASM.

*----------------------------------------------------------------------*
*       CLASS SHRITEFUH64VYIPO5I47WOOA55WASM DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS ltcl_syntax_highlighter IMPLEMENTATION
*----------------------------------------------------------------------*

*CLASS SHRITEFUH64VYIPO5I47WOOA55YASM DEFINITION DEFERRED.
*CLASS zcl_abapgit_syntax_abap DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA55YASM.

*----------------------------------------------------------------------*
*       CLASS SHRITEFUH64VYIPO5I47WOOA55YASM definition
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS SHRITEFUH64VYIPO5I47WOOA55YASM IMPLEMENTATION
*----------------------------------------------------------------------*

class LCL_ABAPGIT_SYNTAX_ABAP implementation.
*"* method's implementations
*include methods.
  METHOD class_constructor.

    init_keywords( ).

  ENDMETHOD.
  METHOD constructor.

    super->constructor( ).

    " Initialize instances of regular expression
    add_rule( iv_regex = c_regex-keyword
              iv_token = c_token-keyword
              iv_style = c_css-keyword ).

    add_rule( iv_regex = c_regex-comment
              iv_token = c_token-comment
              iv_style = c_css-comment ).

    add_rule( iv_regex = c_regex-text
              iv_token = c_token-text
              iv_style = c_css-text ).

  ENDMETHOD.
  METHOD init_keywords.

    DATA: lv_keywords TYPE string,
          lt_keywords TYPE STANDARD TABLE OF string.

    lv_keywords =
     '&&|?TO|ABAP-SOURCE|ABBREVIATED|ABS|ABSTRACT|ACCEPT|ACCEPTING' &&
      '|ACCORDING|ACOS|ACTIVATION|ACTUAL|ADD|ADD-CORRESPONDING|ADJACENT|AFTER|ALIAS' &&
      '|ALIASES|ALIGN|ALL|ALLOCATE|ALPHA|ANALYSIS|ANALYZER|AND|ANY|APPEND|APPENDAGE' &&
      '|APPENDING|APPLICATION|ARCHIVE|AREA|ARITHMETIC|AS|ASCENDING|ASIN|ASPECT|ASSERT' &&
      '|ASSIGN|ASSIGNED|ASSIGNING|ASSOCIATION|ASYNCHRONOUS|AT|ATAN|ATTRIBUTES|AUTHORITY' &&
      '|AUTHORITY-CHECK|AVG|BACK|BACKGROUND|BACKUP|BACKWARD|BADI|BASE|BEFORE|BEGIN' &&
      '|BETWEEN|BIG|BINARY|BIT|BIT-AND|BIT-NOT|BIT-OR|BIT-XOR|BLACK|BLANK' &&
      '|BLANKS|BLOB|BLOCK|BLOCKS|BLUE|BOUND|BOUNDARIES|BOUNDS|BOXED|BREAK-POINT|BT' &&
      '|BUFFER|BY|BYPASSING|BYTE|BYTE-CA|BYTE-CN|BYTE-CO|BYTE-CS|BYTE-NA|BYTE-NS' &&
      '|BYTE-ORDER|C|CA|CALL|CALLING|CASE|CAST|CASTING|CATCH|CEIL|CENTER|CENTERED' &&
      '|CHAIN|CHAIN-INPUT|CHAIN-REQUEST|CHANGE|CHANGING|CHANNELS|CHARACTER|CHARLEN' &&
      '|CHAR-TO-HEX|CHECK|CHECKBOX|CI_|CIRCULAR|CLASS|CLASS-CODING|CLASS-DATA' &&
      '|CLASS-EVENTS|CLASS-METHODS|CLASS-POOL|CLEANUP|CLEAR|CLIENT|CLOB|CLOCK|CLOSE' &&
      '|CN|CO|COALESCE|CODE|CODING|COL_BACKGROUND|COL_GROUP|COL_HEADING|COL_KEY' &&
      '|COL_NEGATIVE|COL_NORMAL|COL_POSITIVE|COL_TOTAL|COLLECT|COLOR|COLUMN|COLUMNS' &&
      '|COMMENT|COMMENTS|COMMIT|COMMON|COMMUNICATION|COMPARING|COMPONENT|COMPONENTS' &&
      '|COMPRESSION|COMPUTE|CONCAT|CONCATENATE|COND|CONDENSE|CONDITION|CONNECT' &&
      '|CONNECTION|CONSTANTS|CONTEXT|CONTEXTS|CONTINUE|CONTROL|CONTROLS|CONV|CONVERSION' &&
      '|CONVERT|COPIES|COPY|CORRESPONDING|COS|COSH|COUNT|COUNTRY|COVER|CP|CPI|CREATE' &&
      '|CREATING|CRITICAL|CS|CURRENCY|CURRENCY_CONVERSION|CURRENT|CURSOR|CURSOR-SELECTION' &&
      '|CUSTOMER|CUSTOMER-FUNCTION|DANGEROUS|DATA|DATABASE|DATAINFO|DATASET|DATE' &&
      '|DAYLIGHT|DBMAXLEN|DD/MM/YY|DD/MM/YYYY|DDMMYY|DEALLOCATE|DECIMAL_SHIFT|DECIMALS' &&
      '|DECLARATIONS|DEEP|DEFAULT|DEFERRED|DEFINE|DEFINING|DEFINITION|DELETE|DELETING' &&
      '|DEMAND|DEPARTMENT|DESCENDING|DESCRIBE|DESTINATION|DETAIL|DIALOG|DIRECTORY' &&
      '|DISCONNECT|DISPLAY|DISPLAY-MODE|DISTANCE|DISTINCT|DIV|DIVIDE|DIVIDE-CORRESPONDING' &&
      '|DIVISION|DO|DUMMY|DUPLICATE|DUPLICATES|DURATION|DURING|DYNAMIC|DYNPRO' &&
      '|EDIT|EDITOR-CALL|ELSE|ELSEIF|EMPTY|ENABLED|ENABLING|ENCODING|END|ENDAT|ENDCASE' &&
      '|ENDCATCH|ENDCHAIN|ENDCLASS|ENDDO|ENDENHANCEMENT|END-ENHANCEMENT-SECTION' &&
      '|ENDEXEC|ENDFORM|ENDFUNCTION|ENDIAN|ENDIF|ENDING|ENDINTERFACE' &&
      '|END-LINES|ENDLOOP|ENDMETHOD|ENDMODULE|END-OF-DEFINITION|END-OF-FILE' &&
      '|END-OF-PAGE|END-OF-SELECTION|ENDON|ENDPROVIDE|ENDSELECT|ENDTRY|ENDWHILE' &&
      '|ENGINEERING|ENHANCEMENT|ENHANCEMENT-POINT|ENHANCEMENTS|ENHANCEMENT-SECTION' &&
      '|ENTRIES|ENTRY|ENVIRONMENT|EQ|EQUIV|ERRORMESSAGE|ERRORS|ESCAPE|ESCAPING' &&
      '|EVENT|EVENTS|EXACT|EXCEPT|EXCEPTION|EXCEPTIONS|EXCEPTION-TABLE|EXCLUDE|EXCLUDING' &&
      '|EXEC|EXECUTE|EXISTS|EXIT|EXIT-COMMAND|EXP|EXPAND|EXPANDING|EXPIRATION|EXPLICIT' &&
      '|EXPONENT|EXPORT|EXPORTING|EXTEND|EXTENDED|EXTENSION|EXTRACT|FAIL|FETCH|FIELD' &&
      '|FIELD-GROUPS|FIELDS|FIELD-SYMBOL|FIELD-SYMBOLS|FILE|FILTER|FILTERS|FILTER-TABLE' &&
      '|FINAL|FIND|FIRST|FIRST-LINE|FIXED-POINT|FKEQ|FKGE|FLOOR|FLUSH|FONT|FOR|FORM' &&
      '|FORMAT|FORWARD|FOUND|FRAC|FRAME|FRAMES|FREE|FRIENDS|FROM|FUNCTION|FUNCTIONALITY' &&
      '|FUNCTION-POOL|FURTHER|GAPS|GE|GENERATE|GET|GIVING|GKEQ|GKGE|GLOBAL|GRANT' &&
      '|GREEN|GROUP|GROUPS|GT|HANDLE|HANDLER|HARMLESS|HASHED|HAVING|HDB|HEADER|HEADERS' &&
      '|HEADING|HEAD-LINES|HELP-ID|HELP-REQUEST|HIDE|HIGH|HINT|HOLD|HOTSPOT|I|ICON|ID' &&
      '|IDENTIFICATION|IDENTIFIER|IDS|IF|IGNORE|IGNORING|IMMEDIATELY|IMPLEMENTATION' &&
      '|IMPLEMENTATIONS|IMPLEMENTED|IMPLICIT|IMPORT|IMPORTING|IN|INACTIVE|INCL|INCLUDE' &&
      '|INCLUDES|INCLUDING|INCREMENT|INDEX|INDEX-LINE|INFOTYPES|INHERITING|INIT|INITIAL' &&
      '|INITIALIZATION|INNER|INOUT|INPUT|INSERT|INSTANCES|INTENSIFIED|INTERFACE' &&
      '|INTERFACE-POOL|INTERFACES|INTERNAL|INTERVALS|INTO|INVERSE|INVERTED-DATE|IS' &&
      '|ISO|JOB|JOIN|KEEP|KEEPING|KERNEL|KEY|KEYS|KEYWORDS|KIND' &&
      '|LANGUAGE|LAST|LATE|LAYOUT|LE|LEADING|LEAVE|LEFT|LEFT-JUSTIFIED|LEFTPLUS' &&
      '|LEFTSPACE|LEGACY|LENGTH|LET|LEVEL|LEVELS|LIKE|LINE|LINE-COUNT|LINEFEED' &&
      '|LINES|LINE-SELECTION|LINE-SIZE|LIST|LISTBOX|LIST-PROCESSING|LITTLE|LLANG' &&
      '|LOAD|LOAD-OF-PROGRAM|LOB|LOCAL|LOCALE|LOCATOR|LOG|LOG10|LOGFILE|LOGICAL' &&
      '|LOG-POINT|LONG|LOOP|LOW|LOWER|LPAD|LPI|LT|M|MAIL|MAIN|MAJOR-ID|MAPPING|MARGIN' &&
      '|MARK|MASK|MATCH|MATCHCODE|MAX|MAXIMUM|MEDIUM|MEMBERS|MEMORY|MESH|MESSAGE' &&
      '|MESSAGE-ID|MESSAGES|MESSAGING|METHOD|METHODS|MIN|MINIMUM|MINOR-ID|MM/DD/YY' &&
      '|MM/DD/YYYY|MMDDYY|MOD|MODE|MODIF|MODIFIER|MODIFY|MODULE|MOVE|MOVE-CORRESPONDING' &&
      '|MULTIPLY|MULTIPLY-CORRESPONDING|NA|NAME|NAMETAB|NATIVE|NB|NE|NESTED|NESTING' &&
      '|NEW|NEW-LINE|NEW-PAGE|NEW-SECTION|NEXT|NO|NODE|NODES|NO-DISPLAY' &&
      '|NO-EXTENSION|NO-GAP|NO-GAPS|NO-GROUPING|NO-HEADING|NON-UNICODE|NON-UNIQUE' &&
      '|NO-SCROLLING|NO-SIGN|NOT|NO-TITLE|NO-TOPOFPAGE|NO-ZERO|NP|NS|NULL|NUMBER' &&
      '|NUMOFCHAR|O|OBJECT|OBJECTS|OBLIGATORY|OCCURRENCE|OCCURRENCES|OCCURS|OF|OFF' &&
      '|OFFSET|OLE|ON|ONLY|OPEN|OPTION|OPTIONAL|OPTIONS|OR|ORDER|OTHER|OTHERS|OUT' &&
      '|OUTER|OUTPUT|OUTPUT-LENGTH|OVERFLOW|OVERLAY|PACK|PACKAGE|PAD|PADDING|PAGE' &&
      '|PAGES|PARAMETER|PARAMETERS|PARAMETER-TABLE|PART|PARTIALLY|PATTERN|PERCENTAGE' &&
      '|PERFORM|PERFORMING|PERSON|PF1|PF2|PF3|PF4|PF5|PF6|PF7|PF8|PF9|PF10|PF11|PF12' &&
      '|PF13|PF14|PF15|PF-STATUS|PINK|PLACES|POOL|POS_HIGH|POS_LOW' &&
      '|POSITION|PRAGMAS|PRECOMPILED|PREFERRED|PRESERVING|PRIMARY|PRINT|PRINT-CONTROL' &&
      '|PRIORITY|PRIVATE|PROCEDURE|PROCESS|PROGRAM|PROPERTY|PROTECTED|PROVIDE|PUBLIC' &&
      '|PUSHBUTTON|PUT|QUEUE-ONLY|QUICKINFO|RADIOBUTTON|RAISE|RAISING|RANGE|RANGES' &&
      '|RAW|READ|READER|READ-ONLY|RECEIVE|RECEIVED|RECEIVER|RECEIVING|RED|REDEFINITION' &&
      '|REDUCE|REDUCED|REF|REFERENCE|REFRESH|REGEX|REJECT|REMOTE|RENAMING|REPLACE' &&
      '|REPLACEMENT|REPLACING|REPORT|REQUEST|REQUESTED|RESERVE|RESET|RESOLUTION' &&
      '|RESPECTING|RESPONSIBLE|RESULT|RESULTS|RESUMABLE|RESUME|RETRY|RETURN|RETURNCODE' &&
      '|RETURNING|RIGHT|RIGHT-JUSTIFIED|RIGHTPLUS|RIGHTSPACE|RISK|RMC_COMMUNICATION_FAILURE' &&
      '|RMC_INVALID_STATUS|RMC_SYSTEM_FAILURE|ROLE|ROLLBACK|ROUND|ROWS|RUN|SAP' &&
      '|SAP-SPOOL|SAVING|SCALE_PRESERVING|SCALE_PRESERVING_SCIENTIFIC|SCAN|SCIENTIFIC' &&
      '|SCIENTIFIC_WITH_LEADING_ZERO|SCREEN|SCROLL|SCROLL-BOUNDARY|SCROLLING|SEARCH' &&
      '|SECONDARY|SECONDS|SECTION|SELECT|SELECTION|SELECTIONS|SELECTION-SCREEN|SELECTION-SET' &&
      '|SELECTION-SETS|SELECTION-TABLE|SELECT-OPTIONS|SEND|SEPARATE|SEPARATED|SET' &&
      '|SHARED|SHIFT|SHORT|SHORTDUMP-ID|SIGN|SIGN_AS_POSTFIX|SIMPLE|SIN|SINGLE|SINH|SIZE' &&
      '|SKIP|SKIPPING|SMART|SOME|SORT|SORTABLE|SORTED|SOURCE|SPACE|SPECIFIED|SPLIT|SPOOL' &&
      '|SPOTS|SQL|SQLSCRIPT|SQRT|STABLE|STAMP|STANDARD|STARTING|START-OF-SELECTION|STATE' &&
      '|STATEMENT|STATEMENTS|STATIC|STATICS|STATUSINFO|STEP-LOOP|STOP|STRLEN|STRUCTURE' &&
      '|STRUCTURES|STYLE|SUBKEY|SUBMATCHES|SUBMIT|SUBROUTINE|SUBSCREEN|SUBSTRING|SUBTRACT' &&
      '|SUBTRACT-CORRESPONDING|SUFFIX|SUM|SUMMARY|SUMMING|SUPPLIED|SUPPLY|SUPPRESS|SWITCH' &&
      '|SWITCHSTATES|SYMBOL|SYNCPOINTS|SYNTAX|SYNTAX-CHECK|SYNTAX-TRACE' &&
      '|SYSTEM-CALL|SYSTEM-EXCEPTIONS|SYSTEM-EXIT|TAB|TABBED|TABLE|TABLES|TABLEVIEW|TABSTRIP' &&
      '|TAN|TANH|TARGET|TASK|TASKS|TEST|TESTING|TEXT|TEXTPOOL|THEN|THROW|TIME|TIMES|TIMESTAMP' &&
      '|TIMEZONE|TITLE|TITLEBAR|TITLE-LINES|TO|TOKENIZATION|TOKENS|TOP-LINES|TOP-OF-PAGE' &&
      '|TRACE-FILE|TRACE-TABLE|TRAILING|TRANSACTION|TRANSFER|TRANSFORMATION|TRANSLATE' &&
      '|TRANSPORTING|TRMAC|TRUNC|TRUNCATE|TRUNCATION|TRY|TYPE|TYPE-POOL|TYPE-POOLS|TYPES' &&
      '|ULINE|UNASSIGN|UNDER|UNICODE|UNION|UNIQUE|UNIT|UNIT_CONVERSION|UNIX|UNPACK|UNTIL' &&
      '|UNWIND|UP|UPDATE|UPPER|USER|USER-COMMAND|USING|UTF-8|VALID|VALUE|VALUE-REQUEST|VALUES' &&
      '|VARY|VARYING|VERIFICATION-MESSAGE|VERSION|VIA|VIEW|VISIBLE|WAIT|WARNING|WHEN|WHENEVER' &&
      '|WHERE|WHILE|WIDTH|WINDOW|WINDOWS|WITH|WITH-HEADING|WITHOUT|WITH-TITLE|WORD|WORK' &&
      '|WRITE|WRITER|X|XML|XSD|XSTRLEN|YELLOW|YES|YYMMDD|Z|ZERO|ZONE' &&
      '|BINTOHEX|CHAR|CLNT|CONCAT_WITH_SPACE|CURR|DATS|DATS_ADD_DAYS|DATS_ADD_MONTHS' &&
      '|DATS_DAYS_BETWEEN|DATS_IS_VALID|DEC|END-OF-EDITING|END-TEST-INJECTION|END-TEST-SEAM' &&
      '|ENDWITH|ENUM|HEXTOBIN|INSTANCE|INSTR|LANG|LTRIM|NUMC|PUSH' &&
      '|QUAN|RETURNS|RPAD|RTRIM|SSTRING|START-OF-EDITING|TEST-INJECTION|TEST-SEAM|TIMS' &&
      '|TIMS_IS_VALID|TSTMP_ADD_SECONDS|TSTMP_CURRENT_UTCTIMESTAMP|TSTMP_IS_VALID' &&
      '|TSTMP_SECONDS_BETWEEN|B|D|DECFLOAT16|DECFLOAT34|F|INT8|N|P|S|STRING|T|UTCLONG|XSTRING' &&
      '|ABAP_BOOL|ACCP|CUKY|DF16_DEC|DF16_RAW|DF34_DEC|DF34_RAW|FLTP' &&
      '|INT1|INT2|INT4|LCHR|LRAW|RAWSTRING|DF16_SCL|DF34_SCL' &&
      '|PREC|VARC|CLIKE|CSEQUENCE|DECFLOAT|NUMERIC|XSEQUENCE|ME|SYST|SY' &&
      '|BIT-SET|BOOLC|BOOLX|CHAR_OFF|CMAX|CMIN|CONCAT_LINES_OF|CONTAINS|CONTAINS_ANY_NOT_OF' &&
      '|CONTAINS_ANY_OF|COUNT_ANY_NOT_OF|COUNT_ANY_OF|FIND_ANY_NOT_OF|FIND_ANY_OF|FIND_END' &&
      '|FROM_MIXED|IPOW|LINE_EXISTS|LINE_INDEX|MATCHES|NMAX|NMIN|REPEAT|RESCALE|REVERSE' &&
      '|SEGMENT|SHIFT_LEFT|SHIFT_RIGHT|SUBSTRING_AFTER|SUBSTRING_BEFORE|SUBSTRING_FROM|SUBSTRING_TO' &&
      '|TO_LOWER|TO_MIXED|TO_UPPER|UTCLONG_ADD|UTCLONG_CURRENT|UTCLONG_DIFF|XSDBOOL'.


    SPLIT lv_keywords AT '|' INTO TABLE lt_keywords.
    " remove duplicates to avoid dumps when converting to a hash table
    SORT lt_keywords BY table_line ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_keywords.
    gt_keywords = lt_keywords. " Hash table

  ENDMETHOD.
  METHOD is_keyword.

    DATA lv_str TYPE string.

    lv_str = to_upper( iv_chunk ).
    READ TABLE gt_keywords WITH KEY table_line = lv_str TRANSPORTING NO FIELDS.
    rv_yes = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD order_matches.

    DATA:
      lv_index      TYPE sy-tabix,
      lv_line_len   TYPE i,
      lv_prev_token TYPE c.

    FIELD-SYMBOLS:
      <ls_prev>  TYPE ty_match,
      <ls_match> TYPE ty_match.

    SORT ct_matches BY offset.

    lv_line_len = strlen( iv_line ).

    LOOP AT ct_matches ASSIGNING <ls_match>.
      lv_index = sy-tabix.

      " Delete matches after open text match
      IF lv_prev_token = c_token-text AND <ls_match>-token <> c_token-text.
        DELETE ct_matches INDEX lv_index.
        CONTINUE.
      ENDIF.

      CASE <ls_match>-token.
        WHEN c_token-keyword.
          IF <ls_match>-offset > 0
              AND substring( val = iv_line
                             off = ( <ls_match>-offset - 1 )
                             len = 1 ) CA '-<'.
            " Delete match if keyword is part of structure or field symbol
            DELETE ct_matches INDEX lv_index.
            CONTINUE.
          ENDIF.

        WHEN c_token-comment.
          <ls_match>-length = lv_line_len - <ls_match>-offset.
          DELETE ct_matches FROM lv_index + 1.
          CONTINUE.

        WHEN c_token-text.
          <ls_match>-text_tag = substring( val = iv_line
                                        off = <ls_match>-offset
                                        len = <ls_match>-length ).
          IF lv_prev_token = c_token-text.
            IF <ls_match>-text_tag = <ls_prev>-text_tag.
              <ls_prev>-length = <ls_match>-offset + <ls_match>-length - <ls_prev>-offset.
              CLEAR lv_prev_token.
            ELSEIF <ls_prev>-text_tag = '}' AND <ls_match>-text_tag = '{'.
              <ls_prev>-length = <ls_match>-offset - <ls_prev>-offset - 1.  " Shift } out of scope
              <ls_prev>-offset = <ls_prev>-offset + 1.                   " Shift { out of scope
              CLEAR lv_prev_token.
            ELSEIF <ls_match>-text_tag = '{'.
              <ls_prev>-length = <ls_match>-offset - <ls_prev>-offset.
              CLEAR lv_prev_token.
            ELSEIF <ls_prev>-text_tag = '}'.
              <ls_prev>-length = <ls_match>-offset - <ls_prev>-offset.
              <ls_prev>-offset = <ls_prev>-offset + 1.                   " Shift } out of scope
              CLEAR lv_prev_token.
            ENDIF.
            DELETE ct_matches INDEX lv_index.
            CONTINUE.
          ENDIF.

      ENDCASE.

      lv_prev_token = <ls_match>-token.
      ASSIGN <ls_match> TO <ls_prev>.
    ENDLOOP.

  ENDMETHOD.
  METHOD parse_line. "REDEFINITION

    DATA lv_index TYPE i.

    FIELD-SYMBOLS <ls_match> LIKE LINE OF rt_matches.

    rt_matches = super->parse_line( iv_line ).

    " Remove non-keywords
    LOOP AT rt_matches ASSIGNING <ls_match> WHERE token = c_token-keyword.
      lv_index = sy-tabix.
      IF abap_false = is_keyword( substring( val = iv_line
                                             off = <ls_match>-offset
                                             len = <ls_match>-length ) ).
        DELETE rt_matches INDEX lv_index.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SYNTAX_ABAP implementation

*>>>>>>> ZCL_ABAPGIT_SYNTAX_CSS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_syntax_css========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_syntax_css========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_SYNTAX_CSS implementation.
*"* method's implementations
*include methods.
  METHOD class_constructor.

    init_keywords( ).

  ENDMETHOD.
  METHOD constructor.

    super->constructor( ).

    " Reset indicator for multi-line comments
    CLEAR gv_comment.

    " Initialize instances of regular expression
    add_rule( iv_regex = c_regex-keyword
              iv_token = c_token-keyword
              iv_style = c_css-keyword ).

    add_rule( iv_regex = c_regex-comment
              iv_token = c_token-comment
              iv_style = c_css-comment ).

    add_rule( iv_regex = c_regex-text
              iv_token = c_token-text
              iv_style = c_css-text ).

    add_rule( iv_regex = c_regex-selectors
              iv_token = c_token-selectors
              iv_style = c_css-selectors ).

    add_rule( iv_regex = c_regex-units
              iv_token = c_token-units
              iv_style = c_css-units ).

    " Styles for keywords
    add_rule( iv_regex = ''
              iv_token = c_token-html
              iv_style = c_css-html ).

    add_rule( iv_regex = ''
              iv_token = c_token-properties
              iv_style = c_css-properties ).

    add_rule( iv_regex = ''
              iv_token = c_token-values
              iv_style = c_css-values ).

    add_rule( iv_regex = ''
              iv_token = c_token-functions
              iv_style = c_css-functions ).

    add_rule( iv_regex = ''
              iv_token = c_token-colors
              iv_style = c_css-colors ).

    add_rule( iv_regex = ''
              iv_token = c_token-extensions
              iv_style = c_css-extensions ).

    add_rule( iv_regex = ''
              iv_token = c_token-at_rules
              iv_style = c_css-at_rules ).

  ENDMETHOD.
  METHOD init_keywords.

    DATA: lv_keywords TYPE string.

    CLEAR gt_keywords.

    " 1) CSS Properties
    lv_keywords =
    'align-content|align-items|align-self|animation|animation-delay|animation-direction|animation-duration|' &&
    'animation-fill-mode|animation-iteration-count|animation-name|animation-play-state|animation-timing-function|' &&
    'backface-visibility|background|background-attachment|background-blend-mode|background-clip|background-color|' &&
    'background-image|background-origin|background-position|background-repeat|background-size|border|' &&
    'border-bottom|border-bottom-color|border-bottom-left-radius|border-bottom-right-radius|border-bottom-style|' &&
    'border-bottom-width|border-collapse|border-color|border-image|border-image-outset|border-image-repeat|' &&
    'border-image-slice|border-image-source|border-image-width|border-left|border-left-color|border-left-style|' &&
    'border-left-width|border-radius|border-right|border-right-color|border-right-style|border-right-width|' &&
    'border-spacing|border-style|border-top|border-top-color|border-top-left-radius|border-top-right-radius|' &&
    'border-top-style|border-top-width|border-width|box-decoration-break|box-shadow|box-sizing|caption-side|' &&
    'caret-color|clear|clip|color|column-count|column-fill|column-gap|column-rule|column-rule-color|' &&
    'column-rule-style|column-rule-width|column-span|column-width|columns|content|counter-increment|' &&
    'counter-reset|cursor|direction|display|empty-cells|filter|flex|flex-basis|flex-direction|flex-flow|' &&
    'flex-grow|flex-shrink|flex-wrap|float|font|font-family|font-kerning|font-size|font-size-adjust|' &&
    'font-stretch|font-style|font-variant|font-weight|grid|grid-area|grid-auto-columns|grid-auto-flow|' &&
    'grid-auto-rows|grid-column|grid-column-end|grid-column-gap|grid-column-start|grid-gap|grid-row|' &&
    'grid-row-end|grid-row-gap|grid-row-start|grid-template|grid-template-areas|grid-template-columns|' &&
    'grid-template-rows|hanging-punctuation|height|hyphens|isolation|justify-content|' &&
    'letter-spacing|line-height|list-style|list-style-image|list-style-position|list-style-type|margin|' &&
    'margin-bottom|margin-left|margin-right|margin-top|max-height|max-width|media|min-height|min-width|' &&
    'mix-blend-mode|object-fit|object-position|opacity|order|outline|outline-color|outline-offset|' &&
    'outline-style|outline-width|overflow|overflow-x|overflow-y|padding|padding-bottom|padding-left|' &&
    'padding-right|padding-top|page-break-after|page-break-before|page-break-inside|perspective|' &&
    'perspective-origin|pointer-events|position|quotes|resize|scroll-behavior|tab-size|table-layout|' &&
    'text-align|text-align-last|text-decoration|text-decoration-color|text-decoration-line|' &&
    'text-decoration-style|text-indent|text-justify|text-overflow|text-rendering|text-shadow|text-transform|' &&
    'transform|transform-origin|transform-style|transition|transition-delay|transition-duration|' &&
    'transition-property|transition-timing-function|unicode-bidi|user-select|vertical-align|visibility|' &&
    'white-space|width|word-break|word-spacing|word-wrap|writing-mode|z-index'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-properties ).

    " 2) CSS Values
    lv_keywords =
    'absolute|all|auto|block|bold|border-box|both|bottom|center|counter|cover|dashed|fixed|hidden|important|' &&
    'inherit|initial|inline-block|italic|left|max-content|middle|min-content|no-repeat|none|normal|pointer|' &&
    'relative|rem|right|solid|table-cell|text|top|transparent|underline|url'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-values ).

    " 3) CSS Selectors
    lv_keywords =
    ':active|::after|::before|:checked|:disabled|:empty|:enabled|:first-child|::first-letter|::first-line|' &&
    ':first-of-type|:focus|:hover|:lang|:last-child|:last-of-type|:link|:not|:nth-child|:nth-last-child|' &&
    ':nth-last-of-type|:nth-of-type|:only-child|:only-of-type|:root|:target|:visited'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-selectors ).

    " 4) CSS Functions
    lv_keywords =
    'attr|calc|cubic-bezier|hsl|hsla|linear-gradient|radial-gradient|repeating-linear-gradient|' &&
    'repeating-radial-gradient|rgb|rgba|rotate|scale|translateX|translateY|var'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-functions ).

    " 5) CSS Colors
    lv_keywords =
    '#|aliceblue|antiquewhite|aqua|aquamarine|azure|beige|bisque|black|blanchedalmond|blue|blueviolet|brown|' &&
    'burlywood|cadetblue|chartreuse|chocolate|coral|cornflowerblue|cornsilk|crimson|cyan|darkblue|darkcyan|' &&
    'darkgoldenrod|darkgray|darkgreen|darkgrey|darkkhaki|darkmagenta|darkolivegreen|darkorange|darkorchid|' &&
    'darkred|darksalmon|darkseagreen|darkslateblue|darkslategray|darkslategrey|darkturquoise|darkviolet|' &&
    'deeppink|deepskyblue|dimgray|dimgrey|dodgerblue|firebrick|floralwhite|forestgreen|fuchsia|gainsboro|' &&
    'ghostwhite|gold|goldenrod|gray|green|greenyellow|grey|honeydew|hotpink|indianred|indigo|ivory|khaki|' &&
    'lavender|lavenderblush|lawngreen|lemonchiffon|lightblue|lightcoral|lightcyan|lightgoldenrodyellow|' &&
    'lightgray|lightgreen|lightgrey|lightpink|lightsalmon|lightseagreen|lightskyblue|lightslategray|' &&
    'lightslategrey|lightsteelblue|lightyellow|lime|limegreen|linen|magenta|maroon|mediumaquamarine|' &&
    'mediumblue|mediumorchid|mediumpurple|mediumseagreen|mediumslateblue|mediumspringgreen|mediumturquoise|' &&
    'mediumvioletred|midnightblue|mintcream|mistyrose|moccasin|navajowhite|navy|oldlace|olive|olivedrab|' &&
    'orange|orangered|orchid|palegoldenrod|palegreen|paleturquoise|palevioletred|papayawhip|peachpuff|' &&
    'peru|pink|plum|powderblue|purple|rebeccapurple|red|rosybrown|royalblue|saddlebrown|salmon|sandybrown|' &&
    'seagreen|seashell|sienna|silver|skyblue|slateblue|slategray|slategrey|snow|springgreen|steelblue|' &&
    'tan|teal|thistle|tomato|turquoise|violet|wheat|white|whitesmoke|yellow|yellowgreen'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-colors ).

    " 6) CSS Extensions
    lv_keywords =
    'moz|moz-binding|moz-border-bottom-colors|moz-border-left-colors|moz-border-right-colors|' &&
    'moz-border-top-colors|moz-box-align|moz-box-direction|moz-box-flex|moz-box-ordinal-group|' &&
    'moz-box-orient|moz-box-pack|moz-box-shadow|moz-context-properties|moz-float-edge|' &&
    'moz-force-broken-image-icon|moz-image-region|moz-orient|moz-osx-font-smoothing|' &&
    'moz-outline-radius|moz-outline-radius-bottomleft|moz-outline-radius-bottomright|' &&
    'moz-outline-radius-topleft|moz-outline-radius-topright|moz-stack-sizing|moz-system-metric|' &&
    'moz-transform|moz-transform-origin|moz-transition|moz-transition-delay|moz-user-focus|' &&
    'moz-user-input|moz-user-modify|moz-window-dragging|moz-window-shadow|ms|ms-accelerator|' &&
    'ms-block-progression|ms-content-zoom-chaining|ms-content-zoom-limit|' &&
    'ms-content-zoom-limit-max|ms-content-zoom-limit-min|ms-content-zoom-snap|' &&
    'ms-content-zoom-snap-points|ms-content-zoom-snap-type|ms-content-zooming|ms-filter|' &&
    'ms-flow-from|ms-flow-into|ms-high-contrast-adjust|ms-hyphenate-limit-chars|' &&
    'ms-hyphenate-limit-lines|ms-hyphenate-limit-zone|ms-ime-align|ms-overflow-style|' &&
    'ms-scroll-chaining|ms-scroll-limit|ms-scroll-limit-x-max|ms-scroll-limit-x-min|' &&
    'ms-scroll-limit-y-max|ms-scroll-limit-y-min|ms-scroll-rails|ms-scroll-snap-points-x|' &&
    'ms-scroll-snap-points-y|ms-scroll-snap-x|ms-scroll-snap-y|ms-scroll-translation|' &&
    'ms-scrollbar-3dlight-color|ms-scrollbar-arrow-color|ms-scrollbar-base-color|' &&
    'ms-scrollbar-darkshadow-color|ms-scrollbar-face-color|ms-scrollbar-highlight-color|' &&
    'ms-scrollbar-shadow-color|ms-scrollbar-track-color|ms-transform|ms-text-autospace|' &&
    'ms-touch-select|ms-wrap-flow|ms-wrap-margin|ms-wrap-through|o|o-transform|webkit|' &&
    'webkit-animation-trigger|webkit-app-region|webkit-appearance|webkit-aspect-ratio|' &&
    'webkit-backdrop-filter|webkit-background-composite|webkit-border-after|' &&
    'webkit-border-after-color|webkit-border-after-style|webkit-border-after-width|' &&
    'webkit-border-before|webkit-border-before-color|webkit-border-before-style|' &&
    'webkit-border-before-width|webkit-border-end|webkit-border-end-color|' &&
    'webkit-border-end-style|webkit-border-end-width|webkit-border-fit|' &&
    'webkit-border-horizontal-spacing|webkit-border-radius|webkit-border-start|' &&
    'webkit-border-start-color|webkit-border-start-style|webkit-border-start-width|' &&
    'webkit-border-vertical-spacing|webkit-box-align|webkit-box-direction|webkit-box-flex|' &&
    'webkit-box-flex-group|webkit-box-lines|webkit-box-ordinal-group|webkit-box-orient|' &&
    'webkit-box-pack|webkit-box-reflect|webkit-box-shadow|webkit-column-axis|' &&
    'webkit-column-break-after|webkit-column-break-before|webkit-column-break-inside|' &&
    'webkit-column-progression|webkit-cursor-visibility|webkit-dashboard-region|' &&
    'webkit-font-size-delta|webkit-font-smoothing|webkit-highlight|webkit-hyphenate-character|' &&
    'webkit-hyphenate-limit-after|webkit-hyphenate-limit-before|webkit-hyphenate-limit-lines|' &&
    'webkit-initial-letter|webkit-line-align|webkit-line-box-contain|webkit-line-clamp|' &&
    'webkit-line-grid|webkit-line-snap|webkit-locale|webkit-logical-height|' &&
    'webkit-logical-width|webkit-margin-after|webkit-margin-after-collapse|' &&
    'webkit-margin-before|webkit-margin-before-collapse|webkit-margin-bottom-collapse|' &&
    'webkit-margin-collapse|webkit-margin-end|webkit-margin-start|webkit-margin-top-collapse|' &&
    'webkit-marquee|webkit-marquee-direction|webkit-marquee-increment|' &&
    'webkit-marquee-repetition|webkit-marquee-speed|webkit-marquee-style|webkit-mask-box-image|' &&
    'webkit-mask-box-image-outset|webkit-mask-box-image-repeat|webkit-mask-box-image-slice|' &&
    'webkit-mask-box-image-source|webkit-mask-box-image-width|webkit-mask-repeat-x|' &&
    'webkit-mask-repeat-y|webkit-mask-source-type|webkit-max-logical-height|' &&
    'webkit-max-logical-width|webkit-min-logical-height|webkit-min-logical-width|' &&
    'webkit-nbsp-mode|webkit-padding-after|webkit-padding-before|webkit-padding-end|' &&
    'webkit-padding-start|webkit-perspective-origin-x|webkit-perspective-origin-y|' &&
    'webkit-print-color-adjust|webkit-rtl-ordering|webkit-svg-shadow|' &&
    'webkit-tap-highlight-color|webkit-text-combine|webkit-text-decoration-skip|' &&
    'webkit-text-decorations-in-effect|webkit-text-fill-color|webkit-text-security|' &&
    'webkit-text-stroke|webkit-text-stroke-color|webkit-text-stroke-width|webkit-text-zoom|' &&
    'webkit-transform|webkit-transform-origin|webkit-transform-origin-x|' &&
    'webkit-transform-origin-y|webkit-transform-origin-z|webkit-transition|' &&
    'webkit-transition-delay|webkit-user-drag|webkit-user-modify|overflow-clip-box|' &&
    'overflow-clip-box-block|overflow-clip-box-inline|zoom'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-extensions ).

    " 6) CSS At-Rules
    lv_keywords =
    '@|charset|counter-style|font-face|import|keyframes'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-at_rules ).

    " 7) HTML tage
    lv_keywords =
    'doctyype|a|abbr|acronym|address|applet|area|b|base|basefont|bdo|bgsound|big|blink|blockquote|' &&
    'body|br|button|caption|center|cite|code|col|colgroup|dd|del|dfn|dir|div|dl|dt|em|embed|fieldset|' &&
    'font|form|frame|frameset|h1|h2|h3|h4|h5|h6|head|hr|html|i|iframe|ilayer|img|input|ins|isindex|' &&
    'kbd|keygen|label|layer|legend|li|link|listing|map|menu|meta|multicol|nobr|noembed|noframes|' &&
    'nolayer|noscript|object|ol|optgroup|option|p|param|plaintext|pre|q|s|samp|script|select|server|' &&
    'small|sound|spacer|span|strike|strong|style|sub|sup|tbody|textarea|title|tt|u|ul|var|wbr|xmp|' &&
    'xsl|xml|accesskey|action|align|alink|alt|background|balance|behavior|bgcolor|bgproperties|' &&
    'border|bordercolor|bordercolordark|bordercolorlight|bottommargin|checked|class|classid|clear|' &&
    'code|codebase|codetype|color|cols|colspan|compact|content|controls|coords|data|datafld|' &&
    'dataformatas|datasrc|direction|disabled|dynsrc|enctype|event|face|for|frame|frameborder|' &&
    'framespacing|height|hidden|href|hspace|http-equiv|id|ismap|lang|language|leftmargin|link|loop|' &&
    'lowsrc|marginheight|marginwidth|maxlength|mayscript|method|methods|multiple|name|nohref|' &&
    'noresize|noshade|nowrap|palette|pluginspage|public|readonly|rel|rev|rightmargin|rows|rowspan|' &&
    'rules|scroll|scrollamount|scrolldelay|scrolling|selected|shape|size|span|src|start|style|' &&
    'tabindex|target|text|title|topmargin|truespeed|type|url|urn|usemap|valign|value|vlink|volume|' &&
    'vrml|vspace|width|wrap|apply-templates|attribute|choose|comment|define-template-set|' &&
    'entity-ref|eval|expr|for-each|if|match|no-entities|node-name|order-by|otherwise|select|' &&
    'stylesheet|template|test|value-of|version|when|xmlns|xsl|cellpadding|cellspacing|table|td|' &&
    'tfoot|th|thead|tr'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-html ).

  ENDMETHOD.
  METHOD insert_keywords.

    DATA: lt_keywords TYPE STANDARD TABLE OF string,
          ls_keyword  TYPE ty_keyword.

    FIELD-SYMBOLS: <lv_keyword> TYPE any.

    SPLIT iv_keywords AT '|' INTO TABLE lt_keywords.

    LOOP AT lt_keywords ASSIGNING <lv_keyword>.
      CLEAR ls_keyword.
      ls_keyword-keyword = <lv_keyword>.
      ls_keyword-token = iv_token.
      INSERT ls_keyword INTO TABLE gt_keywords.
    ENDLOOP.

  ENDMETHOD.
  METHOD is_keyword.

    DATA lv_str TYPE string.

    lv_str = to_lower( iv_chunk ).
    READ TABLE gt_keywords WITH TABLE KEY keyword = lv_str TRANSPORTING NO FIELDS.
    rv_yes = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD order_matches.

    DATA:
      lv_match      TYPE string,
      lv_line_len   TYPE i,
      lv_cmmt_end   TYPE i,
      lv_prev_end   TYPE i,
      lv_prev_token TYPE c.

    FIELD-SYMBOLS:
      <ls_prev>    TYPE ty_match,
      <ls_match>   TYPE ty_match,
      <ls_keyword> TYPE ty_keyword.

    " Longest matches
    SORT ct_matches BY offset length DESCENDING.

    lv_line_len = strlen( iv_line ).

    " Check if this is part of multi-line comment and mark it accordingly
    IF gv_comment = abap_true.
      READ TABLE ct_matches WITH KEY token = c_token-comment TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        CLEAR ct_matches.
        APPEND INITIAL LINE TO ct_matches ASSIGNING <ls_match>.
        <ls_match>-token = c_token-comment.
        <ls_match>-offset = 0.
        <ls_match>-length = lv_line_len.
        RETURN.
      ENDIF.
    ENDIF.

    LOOP AT ct_matches ASSIGNING <ls_match>.
      " Delete matches after open text match
      IF lv_prev_token = c_token-text AND <ls_match>-token <> c_token-text.
        CLEAR <ls_match>-token.
        CONTINUE.
      ENDIF.

      lv_match = substring( val = iv_line
                            off = <ls_match>-offset
                            len = <ls_match>-length ).

      CASE <ls_match>-token.
        WHEN c_token-keyword.
          " Skip keyword that's part of previous (longer) keyword
          IF <ls_match>-offset < lv_prev_end.
            CLEAR <ls_match>-token.
            CONTINUE.
          ENDIF.

          " Map generic keyword to specific CSS token
          lv_match = to_lower( lv_match ).
          READ TABLE gt_keywords ASSIGNING <ls_keyword> WITH TABLE KEY keyword = lv_match.
          IF sy-subrc = 0.
            <ls_match>-token = <ls_keyword>-token.
          ENDIF.

        WHEN c_token-comment.
          IF lv_match = '/*'.
            DELETE ct_matches WHERE offset > <ls_match>-offset.
            <ls_match>-length = lv_line_len - <ls_match>-offset.
            gv_comment = abap_true.
          ELSEIF lv_match = '*/'.
            DELETE ct_matches WHERE offset < <ls_match>-offset.
            <ls_match>-length = <ls_match>-offset + 2.
            <ls_match>-offset = 0.
            gv_comment = abap_false.
          ELSE.
            lv_cmmt_end = <ls_match>-offset + <ls_match>-length.
            DELETE ct_matches WHERE offset > <ls_match>-offset AND offset <= lv_cmmt_end.
          ENDIF.

        WHEN c_token-text.
          <ls_match>-text_tag = lv_match.
          IF lv_prev_token = c_token-text.
            IF <ls_match>-text_tag = <ls_prev>-text_tag.
              <ls_prev>-length = <ls_match>-offset + <ls_match>-length - <ls_prev>-offset.
              CLEAR lv_prev_token.
            ENDIF.
            CLEAR <ls_match>-token.
            CONTINUE.
          ENDIF.

      ENDCASE.

      lv_prev_token = <ls_match>-token.
      lv_prev_end   = <ls_match>-offset + <ls_match>-length.
      ASSIGN <ls_match> TO <ls_prev>.
    ENDLOOP.

    DELETE ct_matches WHERE token IS INITIAL.

  ENDMETHOD.
  METHOD parse_line. "REDEFINITION

    DATA lv_index TYPE i.

    FIELD-SYMBOLS <ls_match> LIKE LINE OF rt_matches.

    rt_matches = super->parse_line( iv_line ).

    " Remove non-keywords
    LOOP AT rt_matches ASSIGNING <ls_match> WHERE token = c_token-keyword.
      lv_index = sy-tabix.
      IF abap_false = is_keyword( substring( val = iv_line
                                             off = <ls_match>-offset
                                             len = <ls_match>-length ) ).
        CLEAR <ls_match>-token.
      ENDIF.
    ENDLOOP.

    DELETE rt_matches WHERE token IS INITIAL.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SYNTAX_CSS implementation

*>>>>>>> ZCL_ABAPGIT_SYNTAX_FACTORY <<<<<<<*

*"* macro definitions
*include zcl_abapgit_syntax_factory====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_syntax_factory====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_SYNTAX_FACTORY implementation.
*"* method's implementations
*include methods.
  METHOD create.

    " Create instance of highighter dynamically dependent on syntax type
    IF iv_filename CP '*.abap'.
      CREATE OBJECT ro_instance TYPE Lcl_abapgit_syntax_abap.
    ELSEIF iv_filename CP '*.xml' OR iv_filename CP '*.html'.
      CREATE OBJECT ro_instance TYPE Lcl_abapgit_syntax_xml.
    ELSEIF iv_filename CP '*.css'.
      CREATE OBJECT ro_instance TYPE Lcl_abapgit_syntax_css.
    ELSEIF iv_filename CP '*.js'.
      CREATE OBJECT ro_instance TYPE Lcl_abapgit_syntax_js.
    ELSEIF iv_filename CP '*.json' OR iv_filename CP '*.jsonc'.
      CREATE OBJECT ro_instance TYPE Lcl_abapgit_syntax_json.
    ELSEIF iv_filename CP '*.txt' OR iv_filename CP '*.ini'  OR iv_filename CP '*.text'.
      CREATE OBJECT ro_instance TYPE Lcl_abapgit_syntax_txt.
    ELSE.
      CLEAR ro_instance.
    ENDIF.

    IF ro_instance IS BOUND.
      ro_instance->set_hidden_chars( iv_hidden_chars ).
    ENDIF.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SYNTAX_FACTORY implementation

*>>>>>>> ZCL_ABAPGIT_SYNTAX_HIGHLIGHTER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_syntax_highlighterccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_syntax_highlighterccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_SYNTAX_HIGHLIGHTER implementation.
*"* method's implementations
*include methods.
  METHOD add_rule.

    DATA ls_rule LIKE LINE OF mt_rules.

    IF NOT iv_regex IS INITIAL.
      CREATE OBJECT ls_rule-regex
        EXPORTING
          pattern     = iv_regex
          ignore_case = abap_true.
    ENDIF.

    ls_rule-token         = iv_token.
    ls_rule-style         = iv_style.
    ls_rule-relevant_submatch = iv_submatch.
    APPEND ls_rule TO mt_rules.

  ENDMETHOD.
  METHOD apply_style.

    DATA lv_escaped TYPE string.

    lv_escaped = escape( val    = iv_line
                         format = cl_abap_format=>e_html_text ).

    lv_escaped = show_hidden_chars( lv_escaped ).

    IF iv_class IS NOT INITIAL.
      rv_line = |<span class="{ iv_class }">{ lv_escaped }</span>|.
    ELSE.
      rv_line = lv_escaped.
    ENDIF.

  ENDMETHOD.
  METHOD extend_matches.

    DATA: lv_line_len TYPE i,
          lv_last_pos TYPE i VALUE 0,
          lv_length   TYPE i,
          ls_match    TYPE ty_match.

    FIELD-SYMBOLS <ls_match> TYPE ty_match.

    lv_line_len = strlen( iv_line ).

    SORT ct_matches BY offset.

    " Add entries refering to parts of text that should not be formatted
    LOOP AT ct_matches ASSIGNING <ls_match>.
      IF <ls_match>-offset > lv_last_pos.
        lv_length = <ls_match>-offset - lv_last_pos.
        ls_match-token  = c_token_none.
        ls_match-offset = lv_last_pos.
        ls_match-length = lv_length.
        INSERT ls_match INTO ct_matches INDEX sy-tabix.
      ENDIF.
      lv_last_pos = <ls_match>-offset + <ls_match>-length.
    ENDLOOP.

    " Add remainder of the string
    IF lv_line_len > lv_last_pos.
      lv_length = lv_line_len - lv_last_pos.
      ls_match-token  = c_token_none.
      ls_match-offset = lv_last_pos.
      ls_match-length = lv_length.
      APPEND ls_match TO ct_matches.
    ENDIF.

  ENDMETHOD.
  METHOD format_line.

    DATA:
      lv_chunk TYPE string,
      ls_rule  LIKE LINE OF mt_rules.

    FIELD-SYMBOLS <ls_match> TYPE ty_match.

    LOOP AT it_matches ASSIGNING <ls_match>.
      lv_chunk = substring( val = iv_line
                            off = <ls_match>-offset
                            len = <ls_match>-length ).

      CLEAR ls_rule. " Failed read equals no style
      READ TABLE mt_rules INTO ls_rule WITH KEY token = <ls_match>-token.

      lv_chunk = apply_style( iv_line  = lv_chunk
                              iv_class = ls_rule-style ).

      rv_line = rv_line && lv_chunk.
    ENDLOOP.

  ENDMETHOD.
  METHOD is_whitespace.

    DATA: lv_whitespace TYPE string.

    "/^\s+$/
    lv_whitespace = ` ` && cl_abap_char_utilities=>horizontal_tab && cl_abap_char_utilities=>cr_lf.

    rv_result = boolc( iv_string CO lv_whitespace ).

  ENDMETHOD.
  METHOD order_matches.
  ENDMETHOD.
  METHOD parse_line.

    DATA:
      lo_regex   TYPE REF TO cl_abap_regex,
      lo_matcher TYPE REF TO cl_abap_matcher,
      lt_result  TYPE match_result_tab,
      ls_match   TYPE ty_match.

    FIELD-SYMBOLS:
      <ls_regex>    LIKE LINE OF mt_rules,
      <ls_result>   TYPE match_result,
      <ls_submatch> LIKE LINE OF <ls_result>-submatches.


    " Process syntax-dependent regex table and find all matches
    LOOP AT mt_rules ASSIGNING <ls_regex> WHERE regex IS BOUND.
      lo_regex   = <ls_regex>-regex.
      lo_matcher = lo_regex->create_matcher( text = iv_line ).
      lt_result  = lo_matcher->find_all( ).

      " Save matches into custom table with predefined tokens
      LOOP AT lt_result ASSIGNING <ls_result>.
        CLEAR: ls_match.
        IF <ls_regex>-relevant_submatch = 0.
          ls_match-token  = <ls_regex>-token.
          ls_match-offset = <ls_result>-offset.
          ls_match-length = <ls_result>-length.
          APPEND ls_match TO rt_matches.
        ELSE.
          READ TABLE <ls_result>-submatches ASSIGNING <ls_submatch> INDEX <ls_regex>-relevant_submatch.
          "submatch might be empty if only discarted parts matched
          IF sy-subrc = 0 AND <ls_submatch>-offset >= 0 AND <ls_submatch>-length > 0.
            ls_match-token  = <ls_regex>-token.
            ls_match-offset = <ls_submatch>-offset.
            ls_match-length = <ls_submatch>-length.
            APPEND ls_match TO rt_matches.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.
  METHOD process_line.

    DATA: lt_matches TYPE ty_match_tt.

    IF iv_line IS INITIAL OR is_whitespace( iv_line ) = abap_true.
      rv_line = show_hidden_chars( iv_line ).
      RETURN.
    ENDIF.

    lt_matches = parse_line( iv_line ).

    order_matches( EXPORTING iv_line    = iv_line
                   CHANGING  ct_matches = lt_matches ).

    extend_matches( EXPORTING iv_line    = iv_line
                    CHANGING  ct_matches = lt_matches ).

    rv_line = format_line( iv_line    = iv_line
                           it_matches = lt_matches ).

  ENDMETHOD.
  METHOD set_hidden_chars.
    mv_hidden_chars = iv_hidden_chars.
  ENDMETHOD.
  METHOD show_hidden_chars.

    DATA lv_bom TYPE x LENGTH 3.

    rv_line = iv_line.

    IF mv_hidden_chars = abap_true.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN rv_line WITH '&nbsp;&rarr;&nbsp;'.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf(1)       IN rv_line WITH '&para;'.
      REPLACE ALL OCCURRENCES OF ` `                                    IN rv_line WITH '&middot;'.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>form_feed IN rv_line
        WITH '<span class="red">&odash;</span>'.

      IF strlen( rv_line ) BETWEEN 1 AND 2.
        TRY.
            lv_bom = Lcl_abapgit_convert=>string_to_xstring( rv_line ).
          CATCH Lcx_abapgit_exception ##NO_HANDLER.
        ENDTRY.
        IF lv_bom(2) = cl_abap_char_utilities=>byte_order_mark_big.
          rv_line = '<span class="red">&squf;</span>'. " UTF-16 big-endian (FE FF)
        ENDIF.
        IF lv_bom(2) = cl_abap_char_utilities=>byte_order_mark_little.
          rv_line = '<span class="red">&compfn;</span>'. " UTF-16 little-endian (FF FE)
        ENDIF.
        IF lv_bom(3) = cl_abap_char_utilities=>byte_order_mark_utf8.
          rv_line = '<span class="red">&curren;</span>'. " UTF-8 (EF BB BF)
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SYNTAX_HIGHLIGHTER implementation

*>>>>>>> ZCL_ABAPGIT_SYNTAX_JS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_syntax_js=========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_syntax_js=========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_SYNTAX_JS implementation.
*"* method's implementations
*include methods.
  METHOD class_constructor.

    init_keywords( ).

  ENDMETHOD.
  METHOD constructor.

    super->constructor( ).

    " Reset indicator for multi-line comments
    CLEAR gv_comment.

    " Initialize instances of regular expression
    add_rule( iv_regex = c_regex-keyword
              iv_token = c_token-keyword
              iv_style = c_css-keyword ).

    add_rule( iv_regex = c_regex-comment
              iv_token = c_token-comment
              iv_style = c_css-comment ).

    add_rule( iv_regex = c_regex-text
              iv_token = c_token-text
              iv_style = c_css-text ).

    " Styles for keywords
    add_rule( iv_regex = ''
              iv_token = c_token-variables
              iv_style = c_css-variables ).

  ENDMETHOD.
  METHOD init_keywords.

    DATA: lv_keywords TYPE string.

    CLEAR gt_keywords.

    " 1) General keywords
    lv_keywords =
    'alert|all|body|break|bytetostring|case|continue|default|delete|do|document|else|event|export|for|function|if|' &&
    'import|in|innerhtml|isnan|item|mimetypes|navigator|new|onabort|onblur|onchange|onclick|ondblclick|ondragdrop|' &&
    'onerror|onfocus|onkeydown|onkeypress|onkeyup|onload|onmousedown|onmousemove|onmouseout|onmouseover|onmouseup|' &&
    'onmove|onreset|onselect|onsubmit|onunload|onresize|options|parsefloat|parseint|prototype|return|screen|switch|' &&
    'unit|var|void|while|window|with|anchor|applet|area|button|checkbox|fileupload|form|frame|hidden|link|mimetype|' &&
    'password|plugin|radio|reset|select|submit|text|textarea|abs|acos|alert|anchor|asin|atan|atan2|back|big|blink|' &&
    'blur|bold|captureevents|ceil|charat|charcodeat|clearinterval|cleartimeout|click|close|concat|confirm|cos|' &&
    'disableexternalcapture|enableexternalcapture|eval|exp|find|fixed|floor|focus|fontcolor|fontsize|forward|' &&
    'fromcharcode|getdate|getday|getelementbyid|gethours|getminutes|getmonth|getoptionvalue|getoptionvaluecount|' &&
    'getseconds|getselection|gettime|gettimezoneoffset|getyear|go|handleevent|home|indexof|italics|javaenabled|join|' &&
    'lastindexof|link|load|log|match|max|min|moveabove|movebelow|moveby|moveto|movetoabsolute|open|parse|plugins|' &&
    'pop|pow|preference|print|prompt|push|random|refresh|releaseevents|reload|replace|reset|resizeby|resizeto|' &&
    'reverse|round|routeevent|scroll|scrollby|scrollto|search|select|setdate|sethours|setinterval|setminutes|' &&
    'setmonth|setseconds|settime|settimeout|setyear|shift|sin|slice|small|sort|splice|split|sqrt|stop|strike|sub|' &&
    'submit|substr|substring|sup|taintenabled|tan|togmtstring|tolocalestring|tolowercase|tostring|touppercase|' &&
    'unshift|unwatch|utc|valueof|watch|write|writeln|e|ln10|ln2|log10e|log2e|max_value|min_value|negative_infinity|' &&
    'nan|pi|positive_infinity|url|above|action|alinkcolor|anchors|appcodename|appname|appversion|applets|arguments|' &&
    'arity|availheight|availwidth|background|backgroundcolor|below|bgcolor|border|bottom|caller|cancelbubble|' &&
    'checked|clientheight|clientwidth|clientx|clienty|clip|closed|color|colordepth|complete|constructor|cookie|' &&
    'count|current|defaultchecked|defaultselected|defaultstatus|defaultvalue|description|display|document|domain|' &&
    'elements|embeds|enabledplugin|encoding|false|fgcolor|filename|form|formname|forms|frames|hash|height|history|' &&
    'host|hostname|href|hspace|images|innerheight|innerwidth|language|lastmodified|layers|left|length|linkcolor|' &&
    'links|location|locationbar|lowsrc|menubar|method|mimetypes|name|next|null|offsetheight|offsetleft|offsetparent|' &&
    'offsetwidth|opener|outerheight|outerwidth|pagex|pagexoffset|pagey|pageyoffset|parent|parentlayer|pathname|' &&
    'personalbar|pixeldepth|platform|plugins|port|poswidth|previous|protocol|prototype|referrer|right|scrolltop|' &&
    'scrollbars|search|selected|selectedindex|self|siblingabove|siblingbelow|src|srcelement|status|statusbar|style|' &&
    'suffixes|tags|target|text|this|title|toolbar|top|true|type|useragent|value|visibility|vlinkcolor|vspace|width|' &&
    'window|zindex'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-keyword ).

    " 2) Variable types
    lv_keywords =
    'array|boolean|date|function|image|layer|math|number|object|option|regexp|string'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-variables ).

  ENDMETHOD.
  METHOD insert_keywords.

    DATA: lt_keywords TYPE STANDARD TABLE OF string,
          ls_keyword  TYPE ty_keyword.

    FIELD-SYMBOLS: <lv_keyword> TYPE any.

    SPLIT iv_keywords AT '|' INTO TABLE lt_keywords.

    LOOP AT lt_keywords ASSIGNING <lv_keyword>.
      CLEAR ls_keyword.
      ls_keyword-keyword = <lv_keyword>.
      ls_keyword-token = iv_token.
      INSERT ls_keyword INTO TABLE gt_keywords.
    ENDLOOP.

  ENDMETHOD.
  METHOD is_keyword.

    DATA lv_str TYPE string.

    lv_str = to_lower( iv_chunk ).
    READ TABLE gt_keywords WITH TABLE KEY keyword = lv_str TRANSPORTING NO FIELDS.
    rv_yes = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD order_matches.

    DATA:
      lv_match      TYPE string,
      lv_line_len   TYPE i,
      lv_cmmt_end   TYPE i,
      lv_prev_end   TYPE i,
      lv_prev_token TYPE c.

    FIELD-SYMBOLS:
      <ls_prev>    TYPE ty_match,
      <ls_match>   TYPE ty_match,
      <ls_keyword> TYPE ty_keyword.

    " Longest matches
    SORT ct_matches BY offset length DESCENDING.

    lv_line_len = strlen( iv_line ).

    " Check if this is part of multi-line comment and mark it accordingly
    IF gv_comment = abap_true.
      READ TABLE ct_matches WITH KEY token = c_token-comment TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        CLEAR ct_matches.
        APPEND INITIAL LINE TO ct_matches ASSIGNING <ls_match>.
        <ls_match>-token = c_token-comment.
        <ls_match>-offset = 0.
        <ls_match>-length = lv_line_len.
        RETURN.
      ENDIF.
    ENDIF.

    LOOP AT ct_matches ASSIGNING <ls_match>.
      " Delete matches after open text match
      IF lv_prev_token = c_token-text AND <ls_match>-token <> c_token-text.
        CLEAR <ls_match>-token.
        CONTINUE.
      ENDIF.

      lv_match = substring( val = iv_line
                            off = <ls_match>-offset
                            len = <ls_match>-length ).

      CASE <ls_match>-token.
        WHEN c_token-keyword.
          " Skip keyword that's part of previous (longer) keyword
          IF <ls_match>-offset < lv_prev_end.
            CLEAR <ls_match>-token.
            CONTINUE.
          ENDIF.

          " Map generic keyword to specific token
          lv_match = to_lower( lv_match ).
          READ TABLE gt_keywords ASSIGNING <ls_keyword> WITH TABLE KEY keyword = lv_match.
          IF sy-subrc = 0.
            <ls_match>-token = <ls_keyword>-token.
          ENDIF.

        WHEN c_token-comment.
          IF lv_match = '/*'.
            DELETE ct_matches WHERE offset > <ls_match>-offset.
            <ls_match>-length = lv_line_len - <ls_match>-offset.
            gv_comment = abap_true.
          ELSEIF lv_match = '//'.
            DELETE ct_matches WHERE offset > <ls_match>-offset.
            <ls_match>-length = lv_line_len - <ls_match>-offset.
          ELSEIF lv_match = '*/'.
            DELETE ct_matches WHERE offset < <ls_match>-offset.
            <ls_match>-length = <ls_match>-offset + 2.
            <ls_match>-offset = 0.
            gv_comment = abap_false.
          ELSE.
            lv_cmmt_end = <ls_match>-offset + <ls_match>-length.
            DELETE ct_matches WHERE offset > <ls_match>-offset AND offset <= lv_cmmt_end.
          ENDIF.

        WHEN c_token-text.
          <ls_match>-text_tag = lv_match.
          IF lv_prev_token = c_token-text.
            IF <ls_match>-text_tag = <ls_prev>-text_tag.
              <ls_prev>-length = <ls_match>-offset + <ls_match>-length - <ls_prev>-offset.
              CLEAR lv_prev_token.
            ENDIF.
            CLEAR <ls_match>-token.
            CONTINUE.
          ENDIF.

      ENDCASE.

      lv_prev_token = <ls_match>-token.
      lv_prev_end   = <ls_match>-offset + <ls_match>-length.
      ASSIGN <ls_match> TO <ls_prev>.
    ENDLOOP.

    DELETE ct_matches WHERE token IS INITIAL.

  ENDMETHOD.
  METHOD parse_line. "REDEFINITION

    DATA lv_index TYPE i.

    FIELD-SYMBOLS <ls_match> LIKE LINE OF rt_matches.

    rt_matches = super->parse_line( iv_line ).

    " Remove non-keywords
    LOOP AT rt_matches ASSIGNING <ls_match> WHERE token = c_token-keyword.
      lv_index = sy-tabix.
      IF abap_false = is_keyword( substring( val = iv_line
                                             off = <ls_match>-offset
                                             len = <ls_match>-length ) ).
        CLEAR <ls_match>-token.
      ENDIF.
    ENDLOOP.

    DELETE rt_matches WHERE token IS INITIAL.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SYNTAX_JS implementation

*>>>>>>> ZCL_ABAPGIT_SYNTAX_JSON <<<<<<<*

*"* macro definitions
*include zcl_abapgit_syntax_json=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_syntax_json=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_syntax_json=======ccau.



class LCL_ABAPGIT_SYNTAX_JSON implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( ).

    " Initialize instances of regular expression

    add_rule( iv_regex = c_regex-keyword
              iv_token = c_token-keyword
              iv_style = c_css-keyword ).

    " Style for keys
    add_rule( iv_regex = c_regex-text
              iv_token = c_token-text
              iv_style = c_css-text ).

    " Style for values
    add_rule( iv_regex = ''
              iv_token = c_token-values
              iv_style = c_css-values ).

    " JSONC comments
    add_rule( iv_regex = c_regex-comment
              iv_token = c_token-comment
              iv_style = c_css-comment ).

  ENDMETHOD.
  METHOD order_matches.

    DATA:
      lv_match      TYPE string,
      lv_count      TYPE i,
      lv_line_len   TYPE i,
      lv_prev_token TYPE c.

    FIELD-SYMBOLS:
      <ls_prev>  TYPE ty_match,
      <ls_match> TYPE ty_match.

    " Longest matches
    SORT ct_matches BY offset length DESCENDING.

    lv_line_len = strlen( iv_line ).

    LOOP AT ct_matches ASSIGNING <ls_match>.
      " Delete matches after open text match
      IF lv_prev_token = c_token-text AND <ls_match>-token <> c_token-text.
        CLEAR <ls_match>-token.
        CONTINUE.
      ENDIF.

      lv_match = substring( val = iv_line
                            off = <ls_match>-offset
                            len = <ls_match>-length ).

      IF <ls_match>-token = c_token-text.
        <ls_match>-text_tag = lv_match.
        IF lv_prev_token = c_token-text.
          IF <ls_match>-text_tag = <ls_prev>-text_tag.
            <ls_prev>-length = <ls_match>-offset + <ls_match>-length - <ls_prev>-offset.
            CLEAR lv_prev_token.
          ENDIF.
          CLEAR <ls_match>-token.
          CONTINUE.
        ENDIF.
      ENDIF.

      lv_prev_token = <ls_match>-token.
      ASSIGN <ls_match> TO <ls_prev>.
    ENDLOOP.

    DELETE ct_matches WHERE token IS INITIAL.

    " Switch style of second text match to values
    LOOP AT ct_matches ASSIGNING <ls_match> WHERE token = c_token-text.
      lv_count = lv_count + 1.
      IF lv_count >= 2.
        <ls_match>-token = c_token-values.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SYNTAX_JSON implementation

*>>>>>>> ZCL_ABAPGIT_SYNTAX_TXT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_syntax_txt========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_syntax_txt========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_SYNTAX_TXT implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( ).

    " No rules for plain text files

  ENDMETHOD.
  METHOD process_line.

    rv_line = apply_style(
      iv_line  = iv_line
      iv_class = '' ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SYNTAX_TXT implementation

*>>>>>>> ZCL_ABAPGIT_SYNTAX_XML <<<<<<<*

*"* macro definitions
*include zcl_abapgit_syntax_xml========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_syntax_xml========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_syntax_xml========ccau.



*CLASS SHRITEFUH64VYIPO5I47WOOA556ASM DEFINITION DEFERRED.
*CLASS zcl_abapgit_syntax_xml DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA556ASM.

*----------------------------------------------------------------------*
*       CLASS SHRITEFUH64VYIPO5I47WOOA556ASM definition
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS SHRITEFUH64VYIPO5I47WOOA556ASM IMPLEMENTATION
*----------------------------------------------------------------------*

class LCL_ABAPGIT_SYNTAX_XML implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( ).

    " Reset indicator for multi-line comments
    CLEAR gv_comment.

    " Initialize instances of regular expressions
    add_rule( iv_regex    = c_regex-xml_tag
              iv_token    = c_token-xml_tag
              iv_style    = c_css-xml_tag
              iv_submatch = 1 ).

    add_rule( iv_regex = c_regex-attr
              iv_token = c_token-attr
              iv_style = c_css-attr ).

    add_rule( iv_regex = c_regex-attr_val
              iv_token = c_token-attr_val
              iv_style = c_css-attr_val ).

    add_rule( iv_regex = c_regex-comment
              iv_token = c_token-comment
              iv_style = c_css-comment ).

  ENDMETHOD.
  METHOD order_matches.

    DATA:
      lv_match      TYPE string,
      lv_line_len   TYPE i,
      lv_cmmt_end   TYPE i,
      lv_index      TYPE sy-tabix,
      lv_prev_token TYPE c,
      lv_state      TYPE c VALUE 'O'. " O - for open tag; C - for closed tag;

    FIELD-SYMBOLS:
      <ls_prev>  TYPE ty_match,
      <ls_match> TYPE ty_match.

    SORT ct_matches BY offset.

    lv_line_len = strlen( iv_line ).

    " Check if this is part of multi-line comment and mark it accordingly
    IF gv_comment = abap_true.
      READ TABLE ct_matches WITH KEY token = c_token-comment TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        CLEAR ct_matches.
        APPEND INITIAL LINE TO ct_matches ASSIGNING <ls_match>.
        <ls_match>-token = c_token-comment.
        <ls_match>-offset = 0.
        <ls_match>-length = lv_line_len.
        RETURN.
      ENDIF.
    ENDIF.

    LOOP AT ct_matches ASSIGNING <ls_match>.
      lv_index = sy-tabix.

      lv_match = substring( val = iv_line
                            off = <ls_match>-offset
                            len = <ls_match>-length ).

      CASE <ls_match>-token.
        WHEN c_token-xml_tag.
          <ls_match>-text_tag = lv_match.

          " No other matches between two tags
          IF <ls_match>-text_tag = '>' AND lv_prev_token = c_token-xml_tag.
            lv_state = 'C'.
            <ls_prev>-length = <ls_match>-offset - <ls_prev>-offset + <ls_match>-length.
            DELETE ct_matches INDEX lv_index.
            CONTINUE.

            " Adjust length and offset of closing tag
          ELSEIF <ls_match>-text_tag = '>' AND lv_prev_token <> c_token-xml_tag.
            lv_state = 'C'.
            IF <ls_prev> IS ASSIGNED.
              <ls_match>-length = <ls_match>-offset - <ls_prev>-offset - <ls_prev>-length + <ls_match>-length.
              <ls_match>-offset = <ls_prev>-offset + <ls_prev>-length.
            ENDIF.
          ELSE.
            lv_state = 'O'.
          ENDIF.

        WHEN c_token-comment.
          IF lv_match = '<!--'.
            DELETE ct_matches WHERE offset > <ls_match>-offset.
            DELETE ct_matches WHERE offset = <ls_match>-offset AND token = c_token-xml_tag.
            <ls_match>-length = lv_line_len - <ls_match>-offset.
            gv_comment = abap_true.
          ELSEIF lv_match = '-->'.
            DELETE ct_matches WHERE offset < <ls_match>-offset.
            <ls_match>-length = <ls_match>-offset + 3.
            <ls_match>-offset = 0.
            gv_comment = abap_false.
          ELSE.
            lv_cmmt_end = <ls_match>-offset + <ls_match>-length.
            DELETE ct_matches WHERE offset > <ls_match>-offset AND offset <= lv_cmmt_end.
            DELETE ct_matches WHERE offset = <ls_match>-offset AND token = c_token-xml_tag.
          ENDIF.

        WHEN OTHERS.
          IF lv_prev_token = c_token-xml_tag.
            <ls_prev>-length = <ls_match>-offset - <ls_prev>-offset. " Extend length of the opening tag
          ENDIF.

          IF lv_state = 'C'.  " Delete all matches between tags
            DELETE ct_matches INDEX lv_index.
            CONTINUE.
          ENDIF.

      ENDCASE.

      lv_prev_token = <ls_match>-token.
      ASSIGN <ls_match> TO <ls_prev>.
    ENDLOOP.

    "if the last XML tag is not closed, extend it to the end of the tag
    IF lv_prev_token = c_token-xml_tag
        AND <ls_prev> IS ASSIGNED
        AND <ls_prev>-length  = 1
        AND <ls_prev>-text_tag = '<'.

      FIND REGEX '<\s*[^\s]*' IN iv_line+<ls_prev>-offset MATCH LENGTH <ls_prev>-length.
      IF sy-subrc <> 0.
        <ls_prev>-length = 1.
      ENDIF.

    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SYNTAX_XML implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SXCI <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sxci=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sxci=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SXCI implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE uname FROM sxc_attr INTO rv_user WHERE imp_name = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_implementation_name TYPE rsexscrn-imp_name.

    lv_implementation_name = ms_item-obj_name.

    CALL FUNCTION 'SXO_IMPL_DELETE'
      EXPORTING
        imp_name           = lv_implementation_name
        no_dialog          = abap_true
      EXCEPTIONS
        imp_not_existing   = 1
        action_canceled    = 2
        access_failure     = 3
        data_inconsistency = 4
        OTHERS             = 5.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_badi_definition             TYPE badi_data,
          lo_filter_object               TYPE REF TO cl_badi_flt_struct,
          lo_filter_values_object        TYPE REF TO cl_badi_flt_values_alv,
          lv_korrnum                     TYPE trkorr,
          lv_filter_type_enhanceability  TYPE rsexscrn-flt_ext,
          lv_package                     TYPE devclass,
          ls_classic_badi_implementation TYPE ty_classic_badi_implementation.

    io_xml->read(
      EXPORTING
        iv_name = 'SXCI'
      CHANGING
        cg_data = ls_classic_badi_implementation ).

    CALL FUNCTION 'SXO_BADI_READ'
      EXPORTING
        exit_name    = ls_classic_badi_implementation-implementation_data-exit_name
      IMPORTING
        badi         = ls_badi_definition
        filter_obj   = lo_filter_object
      EXCEPTIONS
        read_failure = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    lv_package = iv_package.

    CREATE OBJECT lo_filter_values_object
      EXPORTING
        filter_object = lo_filter_object
        filter_values = ls_classic_badi_implementation-filters.

    CALL FUNCTION 'SXO_IMPL_SAVE'
      EXPORTING
        impl             = ls_classic_badi_implementation-implementation_data
        flt_ext          = lv_filter_type_enhanceability
        filter_val_obj   = lo_filter_values_object
        genflag          = abap_true
        no_dialog        = abap_true
      TABLES
        fcodes_to_insert = ls_classic_badi_implementation-function_codes
        cocos_to_insert  = ls_classic_badi_implementation-control_composites
        intas_to_insert  = ls_classic_badi_implementation-customer_includes
        sscrs_to_insert  = ls_classic_badi_implementation-screens
      CHANGING
        korrnum          = lv_korrnum
        devclass         = lv_package
      EXCEPTIONS
        save_failure     = 1
        action_canceled  = 2
        OTHERS           = 3.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'SXO_IMPL_ACTIVE'
      EXPORTING
        imp_name                  = ls_classic_badi_implementation-implementation_data-imp_name
        no_dialog                 = abap_true
      EXCEPTIONS
        badi_not_existing         = 1
        imp_not_existing          = 2
        already_active            = 3
        data_inconsistency        = 4
        activation_not_admissable = 5
        action_canceled           = 6
        access_failure            = 7
        OTHERS                    = 8.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_implementation_name TYPE rsexscrn-imp_name.

    lv_implementation_name = ms_item-obj_name.

    CALL FUNCTION 'SXV_IMP_EXISTS'
      EXPORTING
        imp_name           = lv_implementation_name
      EXCEPTIONS
        not_existing       = 1
        data_inconsistency = 2
        OTHERS             = 3.

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

    "Note: SAP does not show inactive classic BAdIs as "Inactive objects" in SE80
    "Therefore, rv_active will always be true. The implementation state (runtime
    "behaviour of the BAdI) will be serialized as part of the XML
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_implementation_name         TYPE rsexscrn-imp_name,
          lv_exit_name                   TYPE rsexscrn-exit_name,
          lo_filter_object               TYPE REF TO cl_badi_flt_struct,
          ls_badi_definition             TYPE badi_data,
          lo_filter_values_object        TYPE REF TO cl_badi_flt_values_alv,
          lt_methods                     TYPE seex_mtd_table,
          ls_classic_badi_implementation TYPE ty_classic_badi_implementation.

    lv_implementation_name = ms_item-obj_name.

    CALL FUNCTION 'SXV_EXIT_FOR_IMP'
      EXPORTING
        imp_name           = lv_implementation_name
      IMPORTING
        exit_name          = lv_exit_name
      TABLES
        filters            = ls_classic_badi_implementation-filters
      EXCEPTIONS
        data_inconsistency = 1
        OTHERS             = 2.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'SXO_BADI_READ'
      EXPORTING
        exit_name    = lv_exit_name
      IMPORTING
        badi         = ls_badi_definition
        filter_obj   = lo_filter_object
      TABLES
        fcodes       = ls_classic_badi_implementation-function_codes
        cocos        = ls_classic_badi_implementation-control_composites
        intas        = ls_classic_badi_implementation-customer_includes
        scrns        = ls_classic_badi_implementation-screens
        methods      = lt_methods
      EXCEPTIONS
        read_failure = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'SXO_IMPL_FOR_BADI_READ'
      EXPORTING
        imp_name                    = lv_implementation_name
        exit_name                   = lv_exit_name
        inter_name                  = ls_badi_definition-inter_name
        filter_obj                  = lo_filter_object
        no_create_filter_values_obj = abap_true
      IMPORTING
        impl                        = ls_classic_badi_implementation-implementation_data
        filter_values_obj           = lo_filter_values_object
      TABLES
        fcodes                      = ls_classic_badi_implementation-function_codes
        cocos                       = ls_classic_badi_implementation-control_composites
        intas                       = ls_classic_badi_implementation-customer_includes
        scrns                       = ls_classic_badi_implementation-screens
      CHANGING
        methods                     = lt_methods
      EXCEPTIONS
        read_failure                = 1
        OTHERS                      = 2.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CLEAR: ls_classic_badi_implementation-implementation_data-aname,
           ls_classic_badi_implementation-implementation_data-adate,
           ls_classic_badi_implementation-implementation_data-atime,
           ls_classic_badi_implementation-implementation_data-uname,
           ls_classic_badi_implementation-implementation_data-udate,
           ls_classic_badi_implementation-implementation_data-utime.

    io_xml->add( iv_name = 'SXCI'
                 ig_data = ls_classic_badi_implementation ).

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
endclass. "ZCL_ABAPGIT_OBJECT_SXCI implementation

*>>>>>>> ZCL_ABAPGIT_TRANSPORT_OBJECTS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_transport_objects=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_transport_objects=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_transport_objects=ccau.




class LCL_ABAPGIT_TRANSPORT_OBJECTS implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    mt_transport_objects = it_transport_objects.
  ENDMETHOD.
  METHOD to_stage.
    DATA: ls_transport_object LIKE LINE OF mt_transport_objects,
          ls_local_file       TYPE Lif_abapgit_definitions=>ty_file_item,
          ls_object_status    TYPE Lif_abapgit_definitions=>ty_result.

    LOOP AT mt_transport_objects INTO ls_transport_object.
      LOOP AT it_object_statuses INTO ls_object_status
          WHERE obj_name = ls_transport_object-obj_name
          AND obj_type = ls_transport_object-object
          AND NOT lstate IS INITIAL.

        CASE ls_object_status-lstate.
          WHEN Lif_abapgit_definitions=>c_state-added OR Lif_abapgit_definitions=>c_state-modified.
            IF ls_transport_object-delflag = abap_true.
              Lcx_abapgit_exception=>raise( |Object { ls_transport_object-obj_name
                } should be added/modified, but has deletion flag in transport| ).
            ENDIF.

            READ TABLE is_stage_objects-local
                  INTO ls_local_file
              WITH KEY item-obj_name = ls_transport_object-obj_name
                       item-obj_type = ls_transport_object-object
                       file-filename = ls_object_status-filename.
            IF sy-subrc <> 0.
              Lcx_abapgit_exception=>raise( |Object { ls_transport_object-obj_name
                } not found in the local repository files| ).
            ELSE.
              io_stage->add(
                iv_path     = ls_local_file-file-path
                iv_filename = ls_local_file-file-filename
                iv_data     = ls_local_file-file-data ).
            ENDIF.
          WHEN Lif_abapgit_definitions=>c_state-deleted.
* SUSC, see https://github.com/abapGit/abapGit/issues/2772
            IF ls_transport_object-delflag = abap_false
                AND ls_transport_object-object <> 'SUSC'
                AND ls_transport_object-object <> 'IWOM'
                AND ls_transport_object-object <> 'IWMO'
                AND ls_transport_object-object <> 'IWSG'
                AND ls_transport_object-object <> 'IWSV'.
              Lcx_abapgit_exception=>raise( |Object { ls_transport_object-obj_name
                } should be removed, but has NO deletion flag in transport| ).
            ENDIF.
            io_stage->rm(
              iv_path     = ls_object_status-path
              iv_filename = ls_object_status-filename ).
          WHEN OTHERS.
            ASSERT 0 = 1. "Unexpected state
        ENDCASE.
      ENDLOOP.
      IF sy-subrc <> 0.
        " Since not all objects in a transport might be in the local repo
        " i.e generated SADL objects, we don't add these objects to
        " the stage.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_TRANSPORT_OBJECTS implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_TABL_COMPAR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_tabl_comparccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_tabl_comparccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_TABL_COMPAR implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    mi_local = ii_local.

  ENDMETHOD.
  METHOD get_where_used_recursive.

    DATA: lt_findstrings TYPE string_table,
          lt_founds      TYPE STANDARD TABLE OF rsfindlst,
          lt_scope       TYPE ty_seu_obj,
          lv_findstring  LIKE LINE OF lt_findstrings.

    FIELD-SYMBOLS: <ls_found> TYPE rsfindlst.

    IF iv_object_name IS INITIAL.
      RETURN.
    ENDIF.

    lt_scope = it_scope.

    lv_findstring = iv_object_name.
    INSERT lv_findstring INTO TABLE lt_findstrings.

    DO iv_depth TIMES.

      CLEAR: lt_founds.

      CALL FUNCTION 'RS_EU_CROSSREF'
        EXPORTING
          i_find_obj_cls           = iv_object_type
          no_dialog                = 'X'
        TABLES
          i_findstrings            = lt_findstrings
          o_founds                 = lt_founds
          i_scope_object_cls       = lt_scope
        EXCEPTIONS
          not_executed             = 1
          not_found                = 2
          illegal_object           = 3
          no_cross_for_this_object = 4
          batch                    = 5
          batchjob_error           = 6
          wrong_type               = 7
          object_not_exist         = 8
          OTHERS                   = 9.

      IF sy-subrc = 1 OR sy-subrc = 2 OR lines( lt_founds ) = 0.
        EXIT.
      ELSEIF sy-subrc > 2.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      INSERT LINES OF lt_founds INTO TABLE rt_founds_all.

      CLEAR: lt_findstrings.

      LOOP AT lt_founds ASSIGNING <ls_found>.

        lv_findstring = <ls_found>-object.
        INSERT lv_findstring INTO TABLE lt_findstrings.

      ENDLOOP.

    ENDDO.

  ENDMETHOD.
  METHOD is_structure_used_in_db_table.

    DATA: lt_scope  TYPE ty_seu_obj,
          lt_founds TYPE ty_founds.

    APPEND 'TABL' TO lt_scope.
    APPEND 'STRU' TO lt_scope.

    lt_founds = get_where_used_recursive( iv_object_name = iv_object_name
                                          iv_object_type = 'STRU'
                                          it_scope       = lt_scope
                                          iv_depth       = 5 ).

    DELETE lt_founds WHERE object_cls <> 'DT'.

    rv_is_structure_used_in_db_tab = boolc( lines( lt_founds ) > 0 ).

  ENDMETHOD.
  METHOD validate.

    DATA: lt_previous_table_fields TYPE TABLE OF dd03p,
          ls_previous_table_field  LIKE LINE OF lt_previous_table_fields,
          lt_current_table_fields  TYPE TABLE OF dd03p,
          ls_current_table_field   LIKE LINE OF lt_current_table_fields,
          ls_dd02v                 TYPE dd02v,
          ls_item                  TYPE Lif_abapgit_definitions=>ty_item,
          lv_inconsistent          TYPE abap_bool.

    FIELD-SYMBOLS <lv_is_gtt> TYPE abap_bool.

    ii_remote_version->read(
      EXPORTING
        iv_name = 'DD02V'
      CHANGING
        cg_data = ls_dd02v ).

    " We only want to compare transparent tables, or structures used in transparent tables
    IF ls_dd02v-tabclass <> 'TRANSP' AND is_structure_used_in_db_table( ls_dd02v-tabname ) = abap_false.
      RETURN.
    ENDIF.

    " No comparison for global temporary tables
    ASSIGN COMPONENT 'IS_GTT' OF STRUCTURE ls_dd02v TO <lv_is_gtt>.
    IF sy-subrc = 0 AND <lv_is_gtt> = abap_true.
      RETURN.
    ENDIF.

    ii_remote_version->read(
      EXPORTING
        iv_name       = 'DD03P_TABLE'
      CHANGING
        cg_data       = lt_previous_table_fields ).

    ii_local_version->read(
      EXPORTING
        iv_name       = 'DD03P_TABLE'
      CHANGING
        cg_data       = lt_current_table_fields ).

    ls_item-obj_name = ls_dd02v-tabname.
    ls_item-obj_type = 'TABL'.

    LOOP AT lt_previous_table_fields INTO ls_previous_table_field.
      READ TABLE lt_current_table_fields WITH KEY fieldname = ls_previous_table_field-fieldname
        INTO ls_current_table_field.
      IF sy-subrc = 0.
        IF ls_current_table_field-rollname <> ls_previous_table_field-rollname.
          IF ls_current_table_field-rollname IS NOT INITIAL AND ls_previous_table_field-rollname IS NOT INITIAL.
            ii_log->add_info(
              iv_msg  = |Field { ls_previous_table_field-fieldname }: | &
                        |Data element changed from { ls_previous_table_field-rollname } | &
                        |to { ls_current_table_field-rollname }|
              is_item = ls_item ).
          ELSEIF ls_current_table_field-rollname IS NOT INITIAL.
            ii_log->add_info(
              iv_msg  = |Field { ls_previous_table_field-fieldname }: | &
                        |Data type changed from internal type | &
                        |{ ls_previous_table_field-inttype }(length { ls_previous_table_field-intlen }) | &
                        |to data element { ls_current_table_field-rollname }|
              is_item = ls_item ).
          ELSEIF ls_previous_table_field-rollname IS NOT INITIAL.
            ii_log->add_info(
              iv_msg  = |Field { ls_previous_table_field-fieldname }: | &
                        |Data type changed from date element { ls_previous_table_field-rollname } | &
                        |to internal type | &
                        |{ ls_current_table_field-inttype }(length { ls_current_table_field-intlen })|
              is_item = ls_item ).
          ENDIF.
          "TODO: perform several other checks, e.g. field length truncated, ...
          lv_inconsistent = abap_true.
        ENDIF.
      ELSE.
        ii_log->add_info( iv_msg = |Field { ls_previous_table_field-fieldname } removed|
                          is_item = ls_item ).
        lv_inconsistent = abap_true.
      ENDIF.
    ENDLOOP.
    IF lv_inconsistent = abap_true.
      rv_message = |Database Table { ls_dd02v-tabname }: Fields were changed. This may lead to inconsistencies!|.
    ENDIF.

    IF NOT rv_message IS INITIAL.
      rv_message = |Database Table { ls_dd02v-tabname }: { rv_message }|.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_comparator~compare.

    rs_result-text = validate(
      ii_remote_version = ii_remote
      ii_local_version  = mi_local
      ii_log            = ii_log ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_TABL_COMPAR implementation

*>>>>>>> ZCL_ABAPGIT_ZIP <<<<<<<*

*"* macro definitions
*include zcl_abapgit_zip===============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_zip===============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ZIP implementation.
*"* method's implementations
*include methods.
  METHOD encode_files.

    DATA: lo_zip      TYPE REF TO cl_abap_zip,
          lv_filename TYPE string.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF it_files.


    CREATE OBJECT lo_zip.

    LOOP AT it_files ASSIGNING <ls_file>.
      CONCATENATE <ls_file>-file-path+1 <ls_file>-file-filename INTO lv_filename.
      lo_zip->add( name    = lv_filename
                   content = <ls_file>-file-data ).
    ENDLOOP.

    rv_xstr = lo_zip->save( ).

  ENDMETHOD.
  METHOD export.

    DATA li_log       TYPE REF TO Lif_abapgit_log.
    DATA lt_zip       TYPE Lif_abapgit_definitions=>ty_files_item_tt.
    DATA lo_serialize TYPE REF TO Lcl_abapgit_serialize.

    CREATE OBJECT li_log TYPE Lcl_abapgit_log.
    li_log->set_title( 'Zip Export Log' ).

    IF Lcl_abapgit_factory=>get_sap_package( iv_package )->exists( ) = abap_false.
      Lcx_abapgit_exception=>raise( |Package { iv_package } doesn't exist| ).
    ENDIF.

    CREATE OBJECT lo_serialize
      EXPORTING
        io_dot_abapgit    = io_dot_abapgit
        is_local_settings = is_local_settings.

    lt_zip = lo_serialize->files_local(
      iv_package = iv_package
      ii_log     = li_log
      it_filter  = it_filter ).

    FREE lo_serialize.

    IF li_log->count( ) > 0 AND iv_show_log = abap_true.
      Lcl_abapgit_log_viewer=>show_log( li_log ).
    ENDIF.

    rv_xstr = encode_files( lt_zip ).

  ENDMETHOD.
  METHOD export_object.

    DATA: ls_tadir         TYPE Lif_abapgit_definitions=>ty_tadir,
          lv_folder        TYPE string,
          lv_fullpath      TYPE string,
          lv_sep           TYPE c LENGTH 1,
          ls_files_item    TYPE Lif_abapgit_objects=>ty_serialization,
          lo_frontend_serv TYPE REF TO Lif_abapgit_frontend_services.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF ls_files_item-files.

    ls_tadir = Lcl_abapgit_factory=>get_tadir( )->read_single(
        iv_object   = iv_object_type
        iv_obj_name = iv_object_name ).

    IF ls_tadir IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Object could not be found' ).
    ENDIF.

    ls_files_item-item-obj_type = ls_tadir-object.
    ls_files_item-item-obj_name = ls_tadir-obj_name.

    ls_files_item = Lcl_abapgit_objects=>serialize(
      is_item        = ls_files_item-item
      io_i18n_params = Lcl_abapgit_i18n_params=>new(
        iv_main_language_only = iv_main_language_only
        iv_main_language      = sy-langu ) ).

    IF lines( ls_files_item-files ) = 0.
      Lcx_abapgit_exception=>raise( 'Empty' ).
    ENDIF.

    lo_frontend_serv = Lcl_abapgit_ui_factory=>get_frontend_services( ).
    lo_frontend_serv->directory_browse(
      EXPORTING
        iv_initial_folder  = gv_prev
      CHANGING
        cv_selected_folder = lv_folder ).
    IF lv_folder IS INITIAL.
      RAISE EXCEPTION TYPE Lcx_abapgit_cancel.
    ENDIF.

    gv_prev = lv_folder.
    lo_frontend_serv->get_file_separator( CHANGING cv_file_separator = lv_sep ).

    LOOP AT ls_files_item-files ASSIGNING <ls_file>.
      lv_fullpath = |{ lv_folder }{ lv_sep }{ <ls_file>-filename }|.
      save_binstring_to_localfile( iv_filename  = lv_fullpath
                                   iv_binstring = <ls_file>-data ).

    ENDLOOP.

  ENDMETHOD.
  METHOD export_package.

    DATA: ls_local_settings  TYPE Lif_abapgit_persistence=>ty_repo-local_settings,
          lo_dot_abapgit     TYPE REF TO Lcl_abapgit_dot_abapgit,
          lo_frontend_serv   TYPE REF TO Lif_abapgit_frontend_services,
          lv_default         TYPE string,
          lv_package_escaped TYPE string,
          lv_path            TYPE string,
          lv_zip_xstring     TYPE xstring.

    ls_local_settings-main_language_only = iv_main_lang_only.

    lo_dot_abapgit = Lcl_abapgit_dot_abapgit=>build_default( ).
    lo_dot_abapgit->set_folder_logic( iv_folder_logic ).

    lo_frontend_serv = Lcl_abapgit_ui_factory=>get_frontend_services( ).

    lv_package_escaped = iv_package.
    REPLACE ALL OCCURRENCES OF '/' IN lv_package_escaped WITH '#'.
    lv_default = |{ lv_package_escaped }_{ sy-datlo }_{ sy-timlo }|.

    lv_zip_xstring = export(
     is_local_settings = ls_local_settings
     iv_package        = iv_package
     io_dot_abapgit    = lo_dot_abapgit ).

    lv_path = lo_frontend_serv->show_file_save_dialog(
        iv_title            = 'Package Export'
        iv_extension        = 'zip'
        iv_default_filename = lv_default ).

    lo_frontend_serv->file_download(
        iv_path = lv_path
        iv_xstr = lv_zip_xstring ).
  ENDMETHOD.
  METHOD filename.

    IF iv_str CA '/'.
      FIND REGEX '(.*/)(.*)' IN iv_str
        SUBMATCHES ev_path ev_filename.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'Malformed path' ).
      ENDIF.
      IF ev_path <> '/'.
        CONCATENATE '/' ev_path INTO ev_path.
      ENDIF.
    ELSE.
      ev_path = '/'.
      ev_filename = iv_str.
    ENDIF.
    TRANSLATE ev_filename TO LOWER CASE.

  ENDMETHOD.
  METHOD load.

    rt_files = unzip_file( iv_xstr ).

  ENDMETHOD.
  METHOD normalize_path.
* removes first folder from path if needed

    DATA: lt_split  TYPE TABLE OF string,
          lv_needed TYPE abap_bool,
          lv_length TYPE i,
          lv_split  LIKE LINE OF lt_split.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF ct_files.


    READ TABLE ct_files INDEX 1 ASSIGNING <ls_file>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SPLIT <ls_file>-path AT '/' INTO TABLE lt_split.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    READ TABLE lt_split INDEX 2 INTO lv_split.
    IF sy-subrc <> 0 OR strlen( lv_split ) = 0.
      RETURN.
    ENDIF.

    CONCATENATE '/' lv_split '/*' INTO lv_split.

    lv_needed = abap_true.
    LOOP AT ct_files ASSIGNING <ls_file>.
      IF NOT <ls_file>-path CP lv_split.
        lv_needed = abap_false.
        EXIT. " current loop
      ENDIF.
    ENDLOOP.

    IF lv_needed = abap_true.
      lv_length = strlen( lv_split ) - 2.
      LOOP AT ct_files ASSIGNING <ls_file>.
        <ls_file>-path = <ls_file>-path+lv_length.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD save_binstring_to_localfile.

    Lcl_abapgit_ui_factory=>get_frontend_services( )->file_download(
      iv_path = iv_filename
      iv_xstr = iv_binstring ).

  ENDMETHOD.
  METHOD unzip_file.

    DATA: lo_zip  TYPE REF TO cl_abap_zip,
          lv_data TYPE xstring.

    FIELD-SYMBOLS: <ls_zipfile> LIKE LINE OF lo_zip->files,
                   <ls_file>    LIKE LINE OF rt_files.


    CREATE OBJECT lo_zip.
    lo_zip->load( EXPORTING
                    zip             = iv_xstr
                  EXCEPTIONS
                    zip_parse_error = 1
                    OTHERS          = 2 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from zip' ).
    ENDIF.

    LOOP AT lo_zip->files ASSIGNING <ls_zipfile>.

      lo_zip->get(
        EXPORTING
          name                    = <ls_zipfile>-name
        IMPORTING
          content                 = lv_data
        EXCEPTIONS
          zip_index_error         = 1
          zip_decompression_error = 2
          OTHERS                  = 3 ).
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'error from zip get' ).
      ENDIF.

      APPEND INITIAL LINE TO rt_files ASSIGNING <ls_file>.

      filename(
        EXPORTING
          iv_str      = <ls_zipfile>-name
        IMPORTING
          ev_path     = <ls_file>-path
          ev_filename = <ls_file>-filename ).

      <ls_file>-data = lv_data.

      <ls_file>-sha1 = Lcl_abapgit_hash=>sha1_blob( <ls_file>-data ).

    ENDLOOP.

    DELETE rt_files WHERE filename IS INITIAL.

    normalize_path( CHANGING ct_files = rt_files ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ZIP implementation

*>>>>>>> ZCL_ABAPGIT_STAGE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_stage=============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_stage=============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_STAGE implementation.
*"* method's implementations
*include methods.
  METHOD add.

    append( iv_path     = iv_path
            iv_filename = iv_filename
            iv_method   = Lif_abapgit_definitions=>c_method-add
            is_status   = is_status
            iv_data     = iv_data ).

  ENDMETHOD.
  METHOD append.

    DATA: ls_stage LIKE LINE OF mt_stage.

    FIELD-SYMBOLS: <ls_stage> LIKE LINE OF mt_stage.


    READ TABLE mt_stage WITH KEY
      file-path     = iv_path
      file-filename = iv_filename
      ASSIGNING <ls_stage>.
    IF sy-subrc = 0.
      <ls_stage>-file-data = iv_data.
      <ls_stage>-method    = iv_method.
    ELSE.
      ls_stage-file-path     = iv_path.
      ls_stage-file-filename = iv_filename.
      ls_stage-file-data     = iv_data.
      ls_stage-method        = iv_method.
      ls_stage-status        = is_status.
      INSERT ls_stage INTO TABLE mt_stage.
    ENDIF.

  ENDMETHOD.
  METHOD constructor.
    mv_merge_source = iv_merge_source.
  ENDMETHOD.
  METHOD count.
    rv_count = lines( mt_stage ).
  ENDMETHOD.
  METHOD get_all.
    rt_stage = mt_stage.
  ENDMETHOD.
  METHOD get_merge_source.
    rv_source = mv_merge_source.
  ENDMETHOD.
  METHOD ignore.
    append( iv_path     = iv_path
            iv_filename = iv_filename
            iv_method   = Lif_abapgit_definitions=>c_method-ignore ).
  ENDMETHOD.
  METHOD method_description.

    CASE iv_method.
      WHEN Lif_abapgit_definitions=>c_method-add.
        rv_description = 'add'.
      WHEN Lif_abapgit_definitions=>c_method-rm.
        rv_description = 'remove'.
      WHEN Lif_abapgit_definitions=>c_method-ignore.
        rv_description = 'ignore'.
      WHEN OTHERS.
        Lcx_abapgit_exception=>raise( 'unknown staging method type' ).
    ENDCASE.

  ENDMETHOD.
  METHOD reset.
    DELETE mt_stage WHERE file-path = iv_path AND file-filename = iv_filename.
    ASSERT sy-subrc = 0.
  ENDMETHOD.
  METHOD rm.
    append( iv_path     = iv_path
            iv_filename = iv_filename
            is_status   = is_status
            iv_method   = Lif_abapgit_definitions=>c_method-rm ).
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_STAGE implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SMBC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_smbc=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_smbc=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SMBC implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lo_handler      TYPE REF TO object,
          lo_db_api       TYPE REF TO object,
          lr_data         TYPE REF TO data,
          lv_technical_id TYPE c LENGTH 30.

    FIELD-SYMBOLS: <ls_smbc_config>     TYPE any,
                   <lv_smbc_changed_by> TYPE any.
    TRY.
        CREATE OBJECT lo_handler TYPE ('CL_SMBC_AFF_OBJECT_HANDLER').
        CREATE OBJECT lo_db_api TYPE ('CL_MBC_BUSINESS_CONFIG_DB').
        CREATE DATA lr_data TYPE ('SMBC_CONFIG').
        ASSIGN lr_data->* TO <ls_smbc_config>.
      CATCH cx_sy_create_object_error
            cx_sy_create_data_error.
        Lcx_abapgit_exception=>raise( 'SMBC not supported' ).
    ENDTRY.
    lv_technical_id = ms_item-obj_name.
    CALL METHOD lo_db_api->('IF_MBC_BUSINESS_CONFIG_DB~READ')
      EXPORTING
        iv_technical_id = lv_technical_id
        version         = 'I'
      RECEIVING
        rs_config       = <ls_smbc_config>.
    IF <ls_smbc_config> IS INITIAL.
      CALL METHOD lo_db_api->('IF_MBC_BUSINESS_CONFIG_DB~READ')
        EXPORTING
          iv_technical_id = lv_technical_id
          version         = 'A'
        RECEIVING
          rs_config       = <ls_smbc_config>.
    ENDIF.
    ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <ls_smbc_config> TO <lv_smbc_changed_by>.
    rv_user = <lv_smbc_changed_by>.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SMBC implementation

*>>>>>>> ZCL_ABAPGIT_ITEM_STATE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_item_state========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_item_state========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_item_state========ccau.


class LCL_ABAPGIT_ITEM_STATE implementation.
*"* method's implementations
*include methods.
  METHOD is_reassigned.
    rv_is_reassigned = mv_is_reassigned.
  ENDMETHOD.
  METHOD is_unchanged.
    rv_is_unchanged = boolc( mv_is_reassigned = abap_false
      AND mv_lstate = Lif_abapgit_definitions=>c_state-unchanged
      AND mv_rstate = Lif_abapgit_definitions=>c_state-unchanged ).
  ENDMETHOD.
  METHOD local.
    rv_state = mv_lstate.
  ENDMETHOD.
  METHOD reduce.

    rv_new = iv_prev.
    IF rv_new = iv_cur OR iv_cur IS INITIAL.
      RETURN. " No change
    ELSEIF rv_new IS INITIAL.
      rv_new = iv_cur.
    ELSE.
      rv_new = Lif_abapgit_definitions=>c_state-mixed.
    ENDIF.

  ENDMETHOD.
  METHOD remote.
    rv_state = mv_rstate.
  ENDMETHOD.
  METHOD sum_with_repo_item.

    mv_lstate = reduce(
      iv_prev = mv_lstate
      iv_cur  = is_repo_item-lstate ).
    mv_rstate = reduce(
      iv_prev = mv_rstate
      iv_cur  = is_repo_item-rstate ).
    mv_is_reassigned = boolc( mv_is_reassigned = abap_true OR is_repo_item-packmove = abap_true ).

  ENDMETHOD.
  METHOD sum_with_status_item.

    mv_lstate = reduce(
      iv_prev = mv_lstate
      iv_cur  = is_status_item-lstate ).
    mv_rstate = reduce(
      iv_prev = mv_rstate
      iv_cur  = is_status_item-rstate ).
    mv_is_reassigned = boolc( mv_is_reassigned = abap_true OR is_status_item-packmove = abap_true ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ITEM_STATE implementation

*>>>>>>> ZCL_ABAPGIT_NEWS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_news==============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_news==============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_news==============ccau.
**********************************************************************
* Helper classed

CLASS SHRITEFUH64VYIPO5I47WOOA5ZCASM DEFINITION FINAL.
  PUBLIC SECTION.
    DATA mt_buffer TYPE string_table.
    METHODS add
      IMPORTING
        iv_str TYPE string.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5I47WOOA5ZCASM IMPLEMENTATION.
  METHOD add.
    APPEND iv_str TO mt_buffer.
  ENDMETHOD.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5I47WOOA5ZEASM DEFINITION FINAL.
  PUBLIC SECTION.
    DATA mt_log_entries TYPE Lcl_abapgit_news=>ty_logs.
    METHODS add
      IMPORTING
        iv_str TYPE string.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5I47WOOA5ZEASM IMPLEMENTATION.
  METHOD add.
    DATA ls_log LIKE LINE OF mt_log_entries.
    DATA lv_pos_to_cur_str TYPE string.

    SPLIT iv_str AT '/' INTO
      ls_log-version
      ls_log-is_header
      ls_log-is_important
      lv_pos_to_cur_str
      ls_log-text.

    CONDENSE ls_log-version.
    CONDENSE ls_log-is_header.
    CONDENSE ls_log-is_important.
    CONDENSE ls_log-text.
    ls_log-pos_to_cur = lv_pos_to_cur_str.

    APPEND ls_log TO mt_log_entries.
  ENDMETHOD.
ENDCLASS.

**********************************************************************

*CLASS SHRITEFUH64VYIPO5I47WOOA5ZGASM DEFINITION DEFERRED.
*CLASS zcl_abapgit_news DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5ZGASM.

*----------------------------------------------------------------------*
*       CLASS SHRITEFUH64VYIPO5I47WOOA5ZGASM DEFINITION
*----------------------------------------------------------------------*
* Definition of test class for news announcement
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS SHRITEFUH64VYIPO5I47WOOA5ZGASM IMPLEMENTATION
*----------------------------------------------------------------------*
* Implementation of test class for news announcement
*----------------------------------------------------------------------*

class LCL_ABAPGIT_NEWS implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    DATA: lt_lines    TYPE string_table,
          lv_string   TYPE string,
          ls_log_line LIKE LINE OF mt_log.

    " Validate params
    mv_current_version  = Lcl_abapgit_version=>normalize( iv_current_version ).
    mv_lastseen_version = Lcl_abapgit_version=>normalize( iv_lastseen_version ).
    IF mv_current_version IS INITIAL.
      RETURN. " Internal format of program version is not correct -> abort parsing
    ENDIF.

    lv_string = Lcl_abapgit_convert=>xstring_to_string_utf8( iv_rawdata ).
    lt_lines  = Lcl_abapgit_convert=>split_string( lv_string ).
    mt_log    = parse( it_lines = lt_lines
                       iv_current_version = mv_current_version ).

    READ TABLE mt_log INTO ls_log_line INDEX 1.
    mv_latest_version = ls_log_line-version. " Empty if not found

  ENDMETHOD.
  METHOD create.

    CONSTANTS: " TODO refactor
      lc_log_path        TYPE string VALUE '/',
      lc_log_filename    TYPE string VALUE 'changelog*',
      lc_log_filename_up TYPE string VALUE 'CHANGELOG*'.

    DATA: lo_apack            TYPE REF TO Lcl_abapgit_apack_reader,
          lt_remote           TYPE Lif_abapgit_git_definitions=>ty_files_tt,
          lv_version          TYPE string,
          lv_last_seen        TYPE string,
          lv_url              TYPE string,
          lo_repo_online      TYPE REF TO Lcl_abapgit_repo_online,
          lv_version_constant TYPE Lif_abapgit_dot_abapgit=>ty_dot_abapgit-version_constant.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF lt_remote.


    IF io_repo->is_offline( ) = abap_true.
      RETURN.
    ENDIF.

    lo_repo_online ?= io_repo.
    lv_url          = lo_repo_online->get_url( ).

    lo_apack = io_repo->get_dot_apack( ).
    IF lo_apack IS BOUND.
      lv_version = lo_apack->get_manifest_descriptor( )-version.
    ENDIF.

    IF lv_version IS INITIAL.
      TRY.
          lv_version_constant = io_repo->get_dot_abapgit( )->get_version_constant( ).
          IF lv_version_constant IS NOT INITIAL.
            lv_version = Lcl_abapgit_version=>get_version_constant_value( lv_version_constant ).
          ENDIF.
        CATCH Lcx_abapgit_exception.
          CLEAR lv_version.
      ENDTRY.
    ENDIF.

    IF lv_version IS INITIAL.
      RETURN.
    ENDIF.

    lv_last_seen = Lcl_abapgit_persistence_user=>get_instance( )->get_repo_last_change_seen( lv_url ).

    TRY. " Find changelog
        lt_remote = io_repo->get_files_remote( ).
      CATCH Lcx_abapgit_exception.
        RETURN.
    ENDTRY.

    LOOP AT lt_remote ASSIGNING <ls_file> WHERE path = lc_log_path
                                            AND ( filename CP lc_log_filename OR filename CP lc_log_filename_up ).

      CREATE OBJECT ro_instance
        EXPORTING
          iv_rawdata          = <ls_file>-data
          iv_current_version  = lv_version
          iv_lastseen_version = Lcl_abapgit_version=>normalize( lv_last_seen ).

      EXIT.

    ENDLOOP.

    IF ro_instance IS BOUND AND lv_last_seen <> ro_instance->latest_version( ).
      Lcl_abapgit_persistence_user=>get_instance( )->set_repo_last_change_seen(
        iv_url     = lv_url
        iv_version = ro_instance->latest_version( ) ).
    ENDIF.

  ENDMETHOD.
  METHOD get_log.
    rt_log = mt_log.
  ENDMETHOD.
  METHOD has_important.
    READ TABLE mt_log WITH KEY is_important = abap_true TRANSPORTING NO FIELDS.
    rv_boolean = boolc( sy-subrc IS INITIAL ).
  ENDMETHOD.
  METHOD has_news.
    rv_boolean = boolc( lines( mt_log ) > 0 ).
  ENDMETHOD.
  METHOD has_unseen.
    rv_boolean = boolc( Lcl_abapgit_version=>compare(
      iv_a = mv_latest_version
      iv_b = mv_lastseen_version ) > 0 ).
  ENDMETHOD.
  METHOD has_updates.
    rv_boolean = boolc( Lcl_abapgit_version=>compare(
      iv_a = mv_latest_version
      iv_b = mv_current_version ) > 0 ).
  ENDMETHOD.
  METHOD latest_version.
    rv_version = mv_latest_version.
  ENDMETHOD.
  METHOD parse.

    DATA: lv_tail                TYPE i,
          lv_first_version_found TYPE abap_bool,
          lv_version             TYPE string,
          ls_log                 LIKE LINE OF rt_log.

    FIELD-SYMBOLS: <lv_line> LIKE LINE OF it_lines.


    LOOP AT it_lines ASSIGNING <lv_line>.
      ls_log = parse_line( iv_line = <lv_line>
                           iv_current_version = iv_current_version ).

      " Skip until first version head and Skip empty lines
      CHECK ls_log IS NOT INITIAL AND
            ( lv_first_version_found = abap_true OR ls_log-version IS NOT INITIAL ).

      IF lv_first_version_found = abap_false.
        lv_first_version_found = abap_true.
        IF Lcl_abapgit_version=>compare( iv_a = ls_log-version
                                         iv_b = iv_current_version ) <= 0.
          lv_tail = c_tail_length. " Display some last versions if no updates
        ENDIF.
      ENDIF.

      IF ls_log-is_header = abap_true.
        "Skip everything below current version or show tail news
        IF Lcl_abapgit_version=>compare( iv_a = ls_log-version
                                         iv_b = iv_current_version ) <= 0.
          IF lv_tail > 0.
            lv_tail = lv_tail - 1.
          ELSE.
            EXIT.
          ENDIF.
        ENDIF.
        lv_version = ls_log-version. " Save to fill news lines
      ELSE.
        ls_log-version = lv_version.
      ENDIF.

      APPEND ls_log TO rt_log.
    ENDLOOP.

  ENDMETHOD.
  METHOD parse_line.

    CONSTANTS: lc_header_pattern TYPE string
        VALUE '^\d{4}-\d{2}-\d{2}\s+v(\d{1,3}\.\d{1,3}\.\d{1,3})\s*$'.

    DATA: lv_version TYPE string.

    IF iv_line IS INITIAL OR iv_line CO ' -='.
      RETURN. " Skip empty and markup lines
    ENDIF.

    " Check if line is a header line
    FIND FIRST OCCURRENCE OF REGEX lc_header_pattern IN iv_line SUBMATCHES lv_version.
    IF sy-subrc IS INITIAL.
      lv_version        = Lcl_abapgit_version=>normalize( lv_version ).
      rs_log-version    = lv_version.
      rs_log-is_header  = abap_true.
      rs_log-pos_to_cur = Lcl_abapgit_version=>compare( iv_a = lv_version
                                                        iv_b = iv_current_version ).
    ELSE.
      FIND FIRST OCCURRENCE OF REGEX '^\s*!' IN iv_line.
      rs_log-is_important = boolc( sy-subrc IS INITIAL ). " Change is important
    ENDIF.

    rs_log-text = iv_line.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_NEWS implementation

*>>>>>>> ZCL_ABAPGIT_URL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_url===============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_url===============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_url===============ccau.



class LCL_ABAPGIT_URL implementation.
*"* method's implementations
*include methods.
  METHOD host.

    regex( EXPORTING iv_url = iv_url
           IMPORTING ev_host = rv_host ).

  ENDMETHOD.
  METHOD is_abapgit_repo.

    IF iv_url CS 'github.com' AND ( iv_url CP '*/abapGit' OR iv_url CP '*/abapGit.git' ).
      rv_abapgit = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD name.

    DATA: lv_path TYPE string.

    TRY.
        regex( EXPORTING iv_url = iv_url
               IMPORTING ev_name = rv_name
                         ev_path = lv_path ).

        IF rv_name IS INITIAL.
          FIND REGEX '([\w-]+)/$' IN lv_path SUBMATCHES rv_name.
          IF sy-subrc <> 0.
            Lcx_abapgit_exception=>raise( 'Malformed URL' ).
          ENDIF.
        ENDIF.

      CATCH Lcx_abapgit_exception.
        IF iv_validate = abap_true.
          Lcx_abapgit_exception=>raise( 'Malformed URL' ).
        ELSE.
          rv_name = 'URL error (fix repo with "Advanced > Change Remote")'.
        ENDIF.
    ENDTRY.

  ENDMETHOD.
  METHOD path_name.

    DATA: lv_host TYPE string ##NEEDED.

    FIND REGEX '(.*://[^/]*)(.*)' IN iv_url
      SUBMATCHES lv_host rv_path_name.

  ENDMETHOD.
  METHOD regex.

    FIND REGEX '^(https?://[^/]*)(.*/)(.*)\.git$' IN iv_url
      SUBMATCHES ev_host ev_path ev_name.
    IF sy-subrc <> 0.
      FIND REGEX '^(https?://[^/]*)(.*/)(.*)$' IN iv_url
        SUBMATCHES ev_host ev_path ev_name.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'Malformed URL' ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD url_address.

    DATA:
      lv_host TYPE string,
      lv_path TYPE string,
      lv_name TYPE string,
      lv_len  TYPE i.

    regex( EXPORTING iv_url  = iv_url
           IMPORTING ev_host = lv_host
                     ev_path = lv_path
                     ev_name = lv_name ).

    IF lv_path IS INITIAL AND lv_name IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Malformed URL' ).
    ELSEIF lv_name IS INITIAL.
      lv_len = strlen( lv_path ) - 1.
      IF lv_path+lv_len(1) = '/'.
        lv_path = lv_path(lv_len).
      ENDIF.
    ENDIF.

    rv_adress = |{ lv_host }{ lv_path }{ lv_name }|.

  ENDMETHOD.
  METHOD validate.

    name( iv_url      = iv_url
          iv_validate = abap_true ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_URL implementation

*>>>>>>> ZCL_ABAPGIT_VERSION <<<<<<<*

*"* macro definitions
*include zcl_abapgit_version===========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_version===========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_version===========ccau.
*CLASS SHRITEFUH64VYIPO5I47WOOA56WASM DEFINITION DEFERRED.
*CLASS zcl_abapgit_version DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA56WASM.



class LCL_ABAPGIT_VERSION implementation.
*"* method's implementations
*include methods.
  METHOD check_dependant_version.

    CONSTANTS: lc_message TYPE string VALUE 'Current version is older than required'.

    IF is_dependant-major > is_current-major.
      Lcx_abapgit_exception=>raise( lc_message ).
    ELSEIF is_dependant-major < is_current-major.
      RETURN.
    ENDIF.

    IF is_dependant-minor > is_current-minor.
      Lcx_abapgit_exception=>raise( lc_message ).
    ELSEIF is_dependant-minor < is_current-minor.
      RETURN.
    ENDIF.

    IF is_dependant-patch > is_current-patch.
      Lcx_abapgit_exception=>raise( lc_message ).
    ELSEIF is_dependant-patch < is_current-patch.
      RETURN.
    ENDIF.

    IF is_current-prerelase IS INITIAL.
      RETURN.
    ENDIF.

    CASE is_current-prerelase.
      WHEN 'rc'.
        IF is_dependant-prerelase = ''.
          Lcx_abapgit_exception=>raise( lc_message ).
        ENDIF.

      WHEN 'beta'.
        IF is_dependant-prerelase = '' OR is_dependant-prerelase = 'rc'.
          Lcx_abapgit_exception=>raise( lc_message ).
        ENDIF.

      WHEN 'alpha'.
        IF is_dependant-prerelase = '' OR is_dependant-prerelase = 'rc' OR is_dependant-prerelase = 'beta'.
          Lcx_abapgit_exception=>raise( lc_message ).
        ENDIF.

    ENDCASE.

    IF is_dependant-prerelase = is_current-prerelase AND is_dependant-prerelase_patch > is_current-prerelase_patch.
      Lcx_abapgit_exception=>raise( lc_message ).
    ENDIF.

  ENDMETHOD.
  METHOD compare.

    DATA: ls_version_a TYPE Lif_abapgit_definitions=>ty_version,
          ls_version_b TYPE Lif_abapgit_definitions=>ty_version.

    TRY.
        IF is_a IS NOT INITIAL.
          ls_version_a = is_a.
        ELSE.
          ls_version_a = conv_str_to_version( iv_a ).
        ENDIF.

        IF is_b IS NOT INITIAL.
          ls_version_b = is_b.
        ELSE.
          ls_version_b = conv_str_to_version( iv_b ).
        ENDIF.
      CATCH Lcx_abapgit_exception.
        rv_result = 0.
        RETURN.
    ENDTRY.

    IF ls_version_a = ls_version_b.
      rv_result = 0.
    ELSE.
      TRY.
          check_dependant_version( is_current   = ls_version_a
                                   is_dependant = ls_version_b ).
          rv_result = 1.
        CATCH Lcx_abapgit_exception.
          rv_result = -1.
          RETURN.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD conv_str_to_version.

    DATA: lt_segments TYPE STANDARD TABLE OF string,
          lt_parts    TYPE STANDARD TABLE OF string,
          lv_segment  TYPE string.

    SPLIT iv_version AT '-' INTO TABLE lt_segments.

    READ TABLE lt_segments INTO lv_segment INDEX 1. " Version
    IF sy-subrc <> 0.   " No version
      RETURN.
    ENDIF.

    SPLIT lv_segment AT '.' INTO TABLE lt_parts.

    LOOP AT lt_parts INTO lv_segment.

      TRY.
          CASE sy-tabix.
            WHEN 1.
              rs_version-major = lv_segment.
            WHEN 2.
              rs_version-minor = lv_segment.
            WHEN 3.
              rs_version-patch = lv_segment.
          ENDCASE.
        CATCH cx_sy_conversion_no_number.
          Lcx_abapgit_exception=>raise( 'Incorrect format for Semantic Version' ).
      ENDTRY.

    ENDLOOP.

    READ TABLE lt_segments INTO lv_segment INDEX 2. " Pre-release Version
    IF sy-subrc <> 0.   " No version
      RETURN.
    ENDIF.

    SPLIT lv_segment AT '.' INTO TABLE lt_parts.

    LOOP AT lt_parts INTO lv_segment.

      CASE sy-tabix.
        WHEN 1.
          rs_version-prerelase = lv_segment.
          TRANSLATE rs_version-prerelase TO LOWER CASE.
        WHEN 2.
          rs_version-prerelase_patch = lv_segment.
      ENDCASE.

    ENDLOOP.

    IF rs_version-prerelase <> 'rc' AND rs_version-prerelase <> 'beta' AND rs_version-prerelase <> 'alpha'.
      Lcx_abapgit_exception=>raise( 'Incorrect format for Semantic Version' ).
    ENDIF.

  ENDMETHOD.
  METHOD get_version_constant_value.
    DATA: lv_version_class     TYPE string,
          lv_version_component TYPE string.
    FIELD-SYMBOLS: <lv_version> TYPE string.

    IF iv_version_constant NP '*=>*'.
      Lcx_abapgit_exception=>raise( 'Version constant needs to use the format CLASS=>CONSTANT' ).
    ENDIF.

    SPLIT iv_version_constant AT '=>' INTO lv_version_class lv_version_component.
    IF sy-subrc <> 0 OR lv_version_class IS INITIAL OR lv_version_component IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Version constant cannot be parsed' ).
    ENDIF.

    ASSIGN (lv_version_class)=>(lv_version_component) TO <lv_version>.
    IF sy-subrc = 0.
      rv_version = <lv_version>.
    ELSE.
      Lcx_abapgit_exception=>raise( |Could not access version at class { lv_version_class } component | &&
                                    |{ lv_version_component }| ).
    ENDIF.
  ENDMETHOD.
  METHOD normalize.

    " Internal program version should be in format "XXX.XXX.XXX" or "vXXX.XXX.XXX"
    CONSTANTS:
      lc_version_pattern    TYPE string VALUE '^v?(\d{1,3}\.\d{1,3}\.\d{1,3})\s*$',
      lc_prerelease_pattern TYPE string VALUE '^((rc|beta|alpha)\.\d{1,3})\s*$'.

    DATA: lv_version      TYPE string,
          lv_prerelease   TYPE string,
          lv_version_n    TYPE string,
          lv_prerelease_n TYPE string.

    SPLIT iv_version AT '-' INTO lv_version lv_prerelease.

    FIND FIRST OCCURRENCE OF REGEX lc_version_pattern
      IN lv_version SUBMATCHES lv_version_n.

    IF lv_prerelease IS NOT INITIAL.

      FIND FIRST OCCURRENCE OF REGEX lc_prerelease_pattern
        IN lv_prerelease SUBMATCHES lv_prerelease_n.

    ENDIF.

    IF lv_version_n IS INITIAL.
      RETURN.
    ENDIF.

    rv_version = lv_version_n.

    IF lv_prerelease_n IS NOT INITIAL.
      CONCATENATE rv_version '-' lv_prerelease_n INTO rv_version.
    ENDIF.

  ENDMETHOD.
  METHOD version_to_numeric.

    DATA: lv_major   TYPE n LENGTH 4,
          lv_minor   TYPE n LENGTH 4,
          lv_release TYPE n LENGTH 4.

    SPLIT iv_version AT '.' INTO lv_major lv_minor lv_release.

    " Calculated value of version number, empty version will become 0 which is OK
    rv_version = lv_major * 1000000 + lv_minor * 1000 + lv_release.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_VERSION implementation

*>>>>>>> ZCL_ABAPGIT_PROGRESS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_progress==========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_progress==========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_PROGRESS implementation.
*"* method's implementations
*include methods.
  METHOD calc_pct.

    DATA: lv_f TYPE f.

    lv_f = ( iv_current / mv_total ) * 100.
    rv_pct = lv_f.

    IF rv_pct = 100.
      rv_pct = 99.
    ELSEIF rv_pct = 0.
      rv_pct = 1.
    ENDIF.

  ENDMETHOD.
  METHOD get_instance.

* max one progress indicator at a time is supported

    IF gi_progress IS INITIAL.
      CREATE OBJECT gi_progress TYPE Lcl_abapgit_progress.
    ENDIF.

    gi_progress->set_total( iv_total ).

    ri_progress = gi_progress.

  ENDMETHOD.
  METHOD set_instance.

    gi_progress = ii_progress.

  ENDMETHOD.
  METHOD Lif_abapgit_progress~off.

    " Clear the status bar
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'.

  ENDMETHOD.
  METHOD Lif_abapgit_progress~set_total.

    mv_total = iv_total.

    CLEAR mv_cv_time_next.
    CLEAR mv_cv_datum_next.

  ENDMETHOD.
  METHOD Lif_abapgit_progress~show.

    DATA: lv_pct  TYPE i,
          lv_time TYPE t.

    CONSTANTS: lc_wait_secs TYPE i VALUE 2.

    GET TIME.
    lv_time = sy-uzeit.
    IF mv_cv_time_next IS INITIAL AND mv_cv_datum_next IS INITIAL.
      mv_cv_time_next  = lv_time.
      mv_cv_datum_next = sy-datum.
    ENDIF.

    "We only do a progress indication if enough time has passed
    IF lv_time >= mv_cv_time_next
        AND sy-datum = mv_cv_datum_next
        OR sy-datum > mv_cv_datum_next.

      lv_pct = calc_pct( iv_current ).

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = lv_pct
          text       = iv_text.
      mv_cv_time_next = lv_time + lc_wait_secs.

    ENDIF.
    IF sy-datum > mv_cv_datum_next.
      mv_cv_datum_next = sy-datum.
    ENDIF.
    IF mv_cv_time_next < lv_time.
      mv_cv_datum_next = sy-datum + 1.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_PROGRESS implementation

*>>>>>>> ZCL_ABAPGIT_ZLIB_HUFFMAN <<<<<<<*

*"* macro definitions
*include zcl_abapgit_zlib_huffman======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_zlib_huffman======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_zlib_huffman======ccau.



class LCL_ABAPGIT_ZLIB_HUFFMAN implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    DATA: lv_index  TYPE i,
          lt_offset TYPE TABLE OF i,
          lv_length LIKE LINE OF it_lengths,
          lv_prev   TYPE i,
          lv_count  LIKE LINE OF mt_count.

    FIELD-SYMBOLS: <lv_offset> LIKE LINE OF lt_offset,
                   <lv_symbol> LIKE LINE OF mt_symbol,
                   <lv_i>      LIKE LINE OF it_lengths.


    DO c_maxbits TIMES.
      APPEND 0 TO mt_count.
    ENDDO.
    LOOP AT it_lengths INTO lv_index.
      IF lv_index = 0.
        CONTINUE.
      ENDIF.
      READ TABLE mt_count INDEX lv_index ASSIGNING <lv_i>.
      ASSERT sy-subrc = 0.
      <lv_i> = <lv_i> + 1.
    ENDLOOP.

************

    APPEND 0 TO lt_offset.
    DO c_maxbits - 1 TIMES.
      READ TABLE mt_count INDEX sy-index INTO lv_count.
      ASSERT sy-subrc = 0.
      lv_prev = lv_prev + lv_count.
      APPEND lv_prev TO lt_offset.
    ENDDO.

    DO lines( it_lengths ) TIMES.
      APPEND 0 TO mt_symbol.
    ENDDO.
    DO lines( it_lengths ) TIMES.
      lv_index = sy-index.
      READ TABLE it_lengths INDEX lv_index INTO lv_length.
      ASSERT sy-subrc = 0.
      IF lv_length = 0.
        CONTINUE.
      ENDIF.
      READ TABLE lt_offset INDEX lv_length ASSIGNING <lv_offset>.
      ASSERT sy-subrc = 0.
      READ TABLE mt_symbol INDEX <lv_offset> + 1 ASSIGNING <lv_symbol>.
      ASSERT sy-subrc = 0.
      <lv_symbol> = lv_index - 1.
      <lv_offset> = <lv_offset> + 1.
    ENDDO.

  ENDMETHOD.
  METHOD get_count.
    READ TABLE mt_count INDEX iv_index INTO rv_value.     "#EC CI_SUBRC
  ENDMETHOD.
  METHOD get_symbol.
    READ TABLE mt_symbol INDEX iv_index INTO rv_value.    "#EC CI_SUBRC
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ZLIB_HUFFMAN implementation

*>>>>>>> ZCL_ABAPGIT_ZLIB_STREAM <<<<<<<*

*"* macro definitions
*include zcl_abapgit_zlib_stream=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_zlib_stream=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_zlib_stream=======ccau.




class LCL_ABAPGIT_ZLIB_STREAM implementation.
*"* method's implementations
*include methods.
  METHOD clear_bits.
    CLEAR mv_bits.
  ENDMETHOD.
  METHOD constructor.

    mv_compressed = iv_data.

  ENDMETHOD.
  METHOD remaining.

    rv_length = xstrlen( mv_compressed ) + 1.

  ENDMETHOD.
  METHOD take_bits.

    DATA: lv_left  TYPE i,
          lv_index TYPE i,
          lv_x     TYPE x LENGTH 1.


    WHILE strlen( rv_bits ) < iv_length.
      IF mv_bits IS INITIAL.
        lv_x = mv_compressed(1).
        mv_bits = Lcl_abapgit_zlib_convert=>hex_to_bits( lv_x ).
        mv_compressed = mv_compressed+1.
      ENDIF.
      lv_left = iv_length - strlen( rv_bits ).
      IF lv_left >= strlen( mv_bits ).
        CONCATENATE mv_bits rv_bits INTO rv_bits.
        CLEAR mv_bits.
      ELSE.
        lv_index = strlen( mv_bits ) - lv_left.
        CONCATENATE mv_bits+lv_index(lv_left) rv_bits INTO rv_bits.
        mv_bits = mv_bits(lv_index).
      ENDIF.

    ENDWHILE.

  ENDMETHOD.
  METHOD take_bytes.

    rv_bytes = mv_compressed(iv_length).
    mv_compressed = mv_compressed+iv_length.

  ENDMETHOD.
  METHOD take_int.

    rv_int = Lcl_abapgit_zlib_convert=>bits_to_int( take_bits( iv_length ) ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ZLIB_STREAM implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_XINX <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_xinx=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_xinx=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_XINX implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    cl_wb_object_type=>get_key_components_from_id(
      EXPORTING
        p_key                   = |{ ms_item-obj_name }|
        p_external_id           = swbm_c_type_ddic_db_tabxinx
      IMPORTING
        p_key_component1        = mv_name
        p_key_component2        = mv_id
      EXCEPTIONS
        too_many_key_components = 1
        objecttype_not_existing = 2
        OTHERS                  = 3 ).

    ASSERT sy-subrc = 0.

  ENDMETHOD.
  METHOD xinx_delete_docu.

    DATA: lv_docuid  TYPE dokhl-id,
          lv_doctype TYPE dokhl-typ,
          lv_docname TYPE dokhl-object.

    lv_docname    = iv_objname.
    lv_docname+30 = iv_id.
    CALL FUNCTION 'INTERN_DD_DOCU_ID_MATCH'
      EXPORTING
        p_trobjtype  = c_objtype_extension_index
      IMPORTING
        p_docu_id    = lv_docuid
        p_doctype    = lv_doctype
      EXCEPTIONS
        illegal_type = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'DOKU_DELETE_ALL'
      EXPORTING
        doku_id            = lv_docuid
        doku_object        = lv_docname
        doku_typ           = lv_doctype
        suppress_authority = 'X'
        suppress_enqueue   = 'X'
        suppress_transport = 'X'
      EXCEPTIONS
        no_docu_found      = 1
        OTHERS             = 2.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    SELECT SINGLE as4user FROM dd12l INTO rv_user
      WHERE sqltab = mv_name AND indexname = mv_id.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    " Reimplement FM RS_DD_INDX_DELETE as it calls the UI

    DATA: ls_enqueue      TYPE ddenqs,
          lv_protname     TYPE tstrf01-file,
          lv_del_concname LIKE ls_enqueue-objname,
          lv_concname     TYPE rsdxx-objname,
          ls_transp_key   TYPE trkey,
          ls_e071         TYPE e071,
          lv_clm_corrnum  TYPE e070-trkorr.

    CONCATENATE mv_name '-' mv_id INTO lv_concname.
    ls_enqueue-objtype = c_objtype_extension_index.

    CALL FUNCTION 'INT_INDX_DEL_LOCK'
      EXPORTING
        i_trobjtype        = ls_enqueue-objtype
        i_tabname          = mv_name
        i_indexname        = mv_id
      EXCEPTIONS
        not_executed       = 1
        error_occured      = 2
        permission_failure = 3
        OTHERS             = 4.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    ls_enqueue-objname = mv_name.
    ls_enqueue-secname = mv_id.
    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object        = ls_enqueue
        object_class  = 'DICT'
        mode          = 'DELETE'
      IMPORTING
        transport_key = ls_transp_key
      EXCEPTIONS
        OTHERS        = 1.

    IF sy-subrc <> 0.
      " & was not deleted (correction entry not possible or canceled)
      MESSAGE s015(e2) WITH lv_concname INTO Lcx_abapgit_exception=>null.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'DD_LOGNPROT_NAME_GET'
      EXPORTING
        task        = 'DEL'
        obj_type    = ls_enqueue-objtype
        obj_name    = ls_enqueue-objname
        ind_name    = ls_enqueue-secname
      IMPORTING
        protname    = lv_protname
      EXCEPTIONS
        input_error = 0.

    PERFORM logdelete IN PROGRAM rddu0001 USING lv_protname.

    lv_del_concname = ls_enqueue-objname.
    lv_del_concname+16 = ls_enqueue-secname.

    CALL FUNCTION 'DD_OBJ_DEL'
      EXPORTING
        object_name = lv_del_concname
        object_type = ls_enqueue-objtype
        del_state   = 'M'
      EXCEPTIONS
        OTHERS      = 1.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'DD_DD_TO_E071'
      EXPORTING
        type          = ls_enqueue-objtype
        name          = ls_enqueue-objname
        id            = ls_enqueue-secname
      IMPORTING
        obj_name      = ls_e071-obj_name
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      " Internal error & in & (contact person in charge)
      MESSAGE i008(e2) WITH 'DD_DD_TO_E071' 'RS_DD_INDX_DELETE' INTO Lcx_abapgit_exception=>null.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    ls_e071-object = ls_enqueue-objtype.

    CALL FUNCTION 'RS_DELETE_FROM_WORKING_AREA'
      EXPORTING
        object                 = ls_e071-object
        obj_name               = ls_e071-obj_name
        immediate              = 'X'
        actualize_working_area = 'X'.

    xinx_delete_docu(
      iv_objname = mv_name
      iv_id      = mv_id ).

    CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
      EXPORTING
        object    = ls_e071-obj_name
        operation = 'DELETE'
        type      = c_objtype_extension_index.

    IF mv_id(1) CA 'YZ'.
      CALL FUNCTION 'CLM_INDX_MODIFICATION_DELETE'
        EXPORTING
          idxobj_name   = ls_enqueue-objname
          idx_type      = ls_enqueue-objtype
          idx_name      = mv_id
          transport_key = ls_transp_key
          corrnum       = lv_clm_corrnum.
    ENDIF.

    CALL FUNCTION 'RS_DD_DEQUEUE'
      EXPORTING
        objtype = ls_enqueue-objtype
        objname = ls_enqueue-objname
        secname = ls_enqueue-secname.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_extension_index TYPE ty_extension_index,
          lv_rc              TYPE sy-subrc.

    io_xml->read(
      EXPORTING
        iv_name = 'XINX'
      CHANGING
        cg_data = ls_extension_index ).

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

    CALL FUNCTION 'DDIF_INDX_PUT'
      EXPORTING
        name              = mv_name
        id                = mv_id
        dd12v_wa          = ls_extension_index-dd12v
      TABLES
        dd17v_tab         = ls_extension_index-t_dd17v
      EXCEPTIONS
        indx_not_found    = 1
        name_inconsistent = 2
        indx_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from DDIF_INDX_PUT { sy-subrc }| ).
    ENDIF.

    CALL FUNCTION 'DDIF_INDX_ACTIVATE'
      EXPORTING
        name        = mv_name
        id          = mv_id
      IMPORTING
        rc          = lv_rc
      EXCEPTIONS
        not_found   = 1
        put_failure = 2
        OTHERS      = 3.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from DDIF_INDX_ACTIVATE { sy-subrc }| ).
    ENDIF.

    IF lv_rc <> 0.
      Lcx_abapgit_exception=>raise( |Cannot activate extension index { mv_id } of table { mv_name }| ).
    ENDIF.

    deserialize_longtexts( ii_xml         = io_xml
                           iv_longtext_id = c_longtext_id_xinx ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: ls_dd12v TYPE dd12v.

    CALL FUNCTION 'DDIF_INDX_GET'
      EXPORTING
        name          = mv_name
        id            = mv_id
      IMPORTING
        dd12v_wa      = ls_dd12v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    rv_bool = boolc( ls_dd12v IS NOT INITIAL ).

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

    DATA: ls_extension_index TYPE ty_extension_index.

    CALL FUNCTION 'DDIF_INDX_GET'
      EXPORTING
        name          = mv_name
        id            = mv_id
        langu         = mv_language
      IMPORTING
        dd12v_wa      = ls_extension_index-dd12v
      TABLES
        dd17v_tab     = ls_extension_index-t_dd17v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from DDIF_INDX_GET { sy-subrc }| ).
    ENDIF.

    CLEAR: ls_extension_index-dd12v-as4user,
           ls_extension_index-dd12v-as4date,
           ls_extension_index-dd12v-as4time.

    io_xml->add( iv_name = 'XINX'
                 ig_data = ls_extension_index ).

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_xinx ).

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
endclass. "ZCL_ABAPGIT_OBJECT_XINX implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_XSLT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_xslt=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_xslt=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_XSLT implementation.
*"* method's implementations
*include methods.
  METHOD get.

    DATA: lv_name TYPE cxsltdesc.


    lv_name = ms_item-obj_name.

    cl_o2_api_xsltdesc=>load(
      EXPORTING
        p_xslt_desc        = lv_name
      IMPORTING
        p_obj              = ro_xslt
      EXCEPTIONS
        not_existing       = 1
        permission_failure = 2
        OTHERS             = 3 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from cl_o2_api_xsltdesc=>load' ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lo_xslt       TYPE REF TO cl_o2_api_xsltdesc,
          ls_attributes TYPE o2xsltattr.

    lo_xslt = get( ).
    lo_xslt->get_attributes(
      RECEIVING
        p_attributes     = ls_attributes
      EXCEPTIONS
        object_invalid   = 1
        xsltdesc_deleted = 2
        OTHERS           = 3 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    rv_user = ls_attributes-changedby.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lo_xslt TYPE REF TO cl_o2_api_xsltdesc,
          lv_name TYPE cxsltdesc.


    lv_name = ms_item-obj_name.

    cl_o2_api_xsltdesc=>load(
      EXPORTING
        p_xslt_desc        = lv_name
      IMPORTING
        p_obj              = lo_xslt
      EXCEPTIONS
        error_occured      = 1
        not_existing       = 2
        permission_failure = 3
        version_not_found  = 4
        OTHERS             = 5 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from cl_o2_api_xsltdesc=>load' ).
    ENDIF.

    lo_xslt->set_changeable( abap_true ).
    lo_xslt->delete( ).
    lo_xslt->save( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_source     TYPE string,
          lo_xslt       TYPE REF TO cl_o2_api_xsltdesc,
          lv_len        TYPE i,
          ls_attributes TYPE o2xsltattr.

    " Transformation might depend on other objects like a class
    " We attempt to activate it in late step
    IF iv_step = Lif_abapgit_object=>gc_step_id-late.
      IF Lif_abapgit_object~is_active( ) = abap_false.
        Lcl_abapgit_objects_activation=>add_item( ms_item ).
      ENDIF.
      RETURN.
    ENDIF.

    IF Lif_abapgit_object~exists( ) = abap_true.
      Lif_abapgit_object~delete( iv_package   = iv_package
                                 iv_transport = iv_transport ).
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'ATTRIBUTES'
                  CHANGING cg_data = ls_attributes ).

    ls_attributes-devclass = iv_package.

    lv_source = Lif_abapgit_object~mo_files->read_string(
      iv_extra = 'source'
      iv_ext   = 'xml' ).

* workaround: somewhere additional linefeeds are added
    lv_len = strlen( lv_source ) - 2.
    IF lv_source+lv_len(2) = cl_abap_char_utilities=>cr_lf.
      lv_source = lv_source(lv_len).
    ENDIF.

    cl_o2_api_xsltdesc=>create_new_from_string(
      EXPORTING
        p_source                = lv_source
        p_attr                  = ls_attributes
      IMPORTING
        p_obj                   = lo_xslt
      EXCEPTIONS
        action_cancelled        = 1
        error_occured           = 2
        not_authorized          = 3
        object_already_existing = 4
        undefined_name          = 5
        OTHERS                  = 6 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from XSLT new, { sy-subrc }| ).
    ENDIF.

    lo_xslt->save(
      EXCEPTIONS
        action_cancelled      = 1
        error_occured         = 2
        object_invalid        = 3
        object_not_changeable = 4
        permission_failure    = 5
        OTHERS                = 6 ).
    IF sy-subrc <> 0.
      lo_xslt->set_changeable( abap_false ). " unlock
      Lcx_abapgit_exception=>raise( |Error from XSLT save, { sy-subrc }| ).
    ENDIF.

    lo_xslt->set_changeable( abap_false ).

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_name TYPE cxsltdesc.

    lv_name = ms_item-obj_name.

    rv_bool = cl_o2_api_xsltdesc=>exists( lv_name ).
    rv_bool = boolc( rv_bool = '1' ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
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
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lo_xslt       TYPE REF TO cl_o2_api_xsltdesc,
          lv_source     TYPE string,
          ls_attributes TYPE o2xsltattr.


    lo_xslt = get( ).

    ls_attributes = lo_xslt->get_attributes( ).

    CLEAR: ls_attributes-author,
           ls_attributes-createdon,
           ls_attributes-changedby,
           ls_attributes-changedon,
           ls_attributes-devclass.

    io_xml->add( iv_name = 'ATTRIBUTES'
                 ig_data = ls_attributes ).

    lv_source = lo_xslt->get_source_string( ).

    Lif_abapgit_object~mo_files->add_string(
      iv_extra  = 'source'
      iv_ext    = 'xml'
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
endclass. "ZCL_ABAPGIT_OBJECT_XSLT implementation

*>>>>>>> ZCL_ABAPGIT_PERSISTENCE_REPO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_persistence_repo==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_persistence_repo==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_PERSISTENCE_REPO implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    DATA ls_dummy_meta_mask TYPE Lif_abapgit_persistence=>ty_repo_meta_mask.
    DATA ls_dummy_meta      TYPE Lif_abapgit_persistence=>ty_repo_xml.
    DATA lo_type_meta_mask  TYPE REF TO cl_abap_structdescr.
    DATA lo_type_meta       TYPE REF TO cl_abap_structdescr.
    FIELD-SYMBOLS <ls_comp> LIKE LINE OF lo_type_meta_mask->components.

    " Collect actual list of fields in repo meta data (used in update_meta)
    lo_type_meta_mask ?= cl_abap_structdescr=>describe_by_data( ls_dummy_meta_mask ).
    lo_type_meta      ?= cl_abap_structdescr=>describe_by_data( ls_dummy_meta ).
    LOOP AT lo_type_meta_mask->components ASSIGNING <ls_comp>.
      APPEND <ls_comp>-name TO mt_meta_fields.
    ENDLOOP.

    mo_db = Lcl_abapgit_persistence_db=>get_instance( ).

  ENDMETHOD.
  METHOD from_xml.

    DATA: lv_xml TYPE string.

    lv_xml = iv_repo_xml_string.

* fix downward compatibility
    REPLACE ALL OCCURRENCES OF '<_--28C_TYPE_REPO_--29>' IN lv_xml WITH '<REPO>'.
    REPLACE ALL OCCURRENCES OF '</_--28C_TYPE_REPO_--29>' IN lv_xml WITH '</REPO>'.

    CALL TRANSFORMATION id
      OPTIONS value_handling = 'accept_data_loss'
      SOURCE XML lv_xml
      RESULT repo = rs_repo.

* automatic migration of old fields
* todo, keep for transition period until 2022-12-31, then remove all of these
    FIND FIRST OCCURRENCE OF '</HEAD_BRANCH><WRITE_PROTECT>X</WRITE_PROTECT>' IN lv_xml.
    IF sy-subrc = 0.
      rs_repo-local_settings-write_protected = abap_true.
    ENDIF.
    FIND FIRST OCCURRENCE OF '<IGNORE_SUBPACKAGES>X</IGNORE_SUBPACKAGES></REPO>' IN lv_xml.
    IF sy-subrc = 0.
      rs_repo-local_settings-ignore_subpackages = abap_true.
    ENDIF.
    FIND FIRST OCCURRENCE OF '<SERIALIZE_MASTER_LANG_ONLY>X</SERIALIZE_MASTER_LANG_ONLY>' IN lv_xml.
    IF sy-subrc = 0.
      rs_repo-local_settings-main_language_only = abap_true.
    ENDIF.

    IF rs_repo IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Inconsistent repo metadata' ).
    ENDIF.

  ENDMETHOD.
  METHOD get_next_id.

* todo: Lock the complete persistence in order to prevent concurrent repo-creation
* however the current approach will most likely work in almost all cases

    DATA: lt_content TYPE Lif_abapgit_persistence=>ty_contents.

    FIELD-SYMBOLS: <ls_content> LIKE LINE OF lt_content.


    rv_next_repo_id = 1.

    lt_content = mo_db->list_by_type( Lcl_abapgit_persistence_db=>c_type_repo ).
    LOOP AT lt_content ASSIGNING <ls_content>.
      IF <ls_content>-value >= rv_next_repo_id.
        rv_next_repo_id = <ls_content>-value + 1.
      ENDIF.
    ENDLOOP.

    SHIFT rv_next_repo_id RIGHT DELETING TRAILING space.
    TRANSLATE rv_next_repo_id USING ' 0'.

  ENDMETHOD.
  METHOD get_repo_from_content.
    MOVE-CORRESPONDING from_xml( is_content-data_str ) TO rs_result.
    IF rs_result-local_settings-write_protected = abap_false AND
       Lcl_abapgit_factory=>get_environment( )->is_repo_object_changes_allowed( ) = abap_false.
      rs_result-local_settings-write_protected = abap_true.
    ENDIF.
    rs_result-key = is_content-value.
  ENDMETHOD.
  METHOD rewrite_repo_meta.

    DATA lv_old_blob TYPE string.
    DATA lv_new_blob TYPE string.
    DATA ls_repo_meta TYPE Lif_abapgit_persistence=>ty_repo.

    lv_old_blob = mo_db->read(
      iv_type  = Lcl_abapgit_persistence_db=>c_type_repo
      iv_value = iv_repo_key ).

    MOVE-CORRESPONDING from_xml( lv_old_blob ) TO ls_repo_meta.
    lv_new_blob = to_xml( ls_repo_meta ).

    mo_db->update(
      iv_type  = Lcl_abapgit_persistence_db=>c_type_repo
      iv_value = iv_repo_key
      iv_data  = lv_new_blob ).

  ENDMETHOD.
  METHOD to_xml.

    DATA: ls_xml TYPE Lif_abapgit_persistence=>ty_repo_xml.


    MOVE-CORRESPONDING is_repo TO ls_xml.

    CALL TRANSFORMATION id
      SOURCE repo = ls_xml
      RESULT XML rv_repo_xml_string.
  ENDMETHOD.
  METHOD Lif_abapgit_persist_repo_cs~delete.

    mo_db->delete(
      iv_type  = Lcl_abapgit_persistence_db=>c_type_repo_csum
      iv_value = iv_key ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_repo_cs~read.

    rv_cs_blob = mo_db->read(
      iv_type  = Lcl_abapgit_persistence_db=>c_type_repo_csum
      iv_value = iv_key ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_repo_cs~update.

    mo_db->modify(
      iv_type  = Lcl_abapgit_persistence_db=>c_type_repo_csum
      iv_value = iv_key
      iv_data  = iv_cs_blob ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_repo~add.

    DATA: ls_repo        TYPE Lif_abapgit_persistence=>ty_repo,
          lv_repo_as_xml TYPE string.


    ls_repo-url          = iv_url.
    ls_repo-branch_name  = iv_branch_name.
    ls_repo-package      = iv_package.
    ls_repo-offline      = iv_offline.
    ls_repo-created_by   = sy-uname.
    GET TIME STAMP FIELD ls_repo-created_at.
    ls_repo-dot_abapgit  = is_dot_abapgit.

    ls_repo-local_settings-display_name = iv_display_name.

    lv_repo_as_xml = to_xml( ls_repo ).

    rv_key = get_next_id( ).

    mo_db->add( iv_type  = Lcl_abapgit_persistence_db=>c_type_repo
                iv_value = rv_key
                iv_data  = lv_repo_as_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_repo~delete.

    DATA: lo_background TYPE REF TO Lcl_abapgit_persist_background.

    CREATE OBJECT lo_background.
    lo_background->delete( iv_key ).

    mo_db->delete( iv_type  = Lcl_abapgit_persistence_db=>c_type_repo
                   iv_value = iv_key ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_repo~exists.

    DATA lt_keys TYPE Lif_abapgit_persistence=>ty_repo_keys.
    DATA lt_content TYPE Lif_abapgit_persistence=>ty_contents.

    APPEND iv_key TO lt_keys.

    lt_content = mo_db->list_by_keys(
      it_keys = lt_keys
      iv_type = Lcl_abapgit_persistence_db=>c_type_repo ).

    rv_yes = boolc( lines( lt_content ) > 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_repo~list.

    DATA: lt_content TYPE Lif_abapgit_persistence=>ty_contents,
          ls_content LIKE LINE OF lt_content,
          ls_repo    LIKE LINE OF rt_repos.

    lt_content = mo_db->list_by_type( Lcl_abapgit_persistence_db=>c_type_repo ).

    LOOP AT lt_content INTO ls_content.
      ls_repo = get_repo_from_content( ls_content ).
      INSERT ls_repo INTO TABLE rt_repos.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_repo~list_by_keys.
    DATA: lt_content TYPE Lif_abapgit_persistence=>ty_contents,
          ls_content LIKE LINE OF lt_content,
          ls_repo    LIKE LINE OF rt_repos.

    lt_content = mo_db->list_by_keys(
      it_keys = it_keys
      iv_type = Lcl_abapgit_persistence_db=>c_type_repo ).

    LOOP AT lt_content INTO ls_content.
      ls_repo = get_repo_from_content( ls_content ).
      INSERT ls_repo INTO TABLE rt_repos.
    ENDLOOP.
  ENDMETHOD.
  METHOD Lif_abapgit_persist_repo~lock.

    mo_db->lock( iv_mode  = iv_mode
                 iv_type  = Lcl_abapgit_persistence_db=>c_type_repo
                 iv_value = iv_key ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_repo~read.

    DATA lt_repo TYPE Lif_abapgit_persistence=>ty_repos.

    lt_repo = Lif_abapgit_persist_repo~list( ).

    READ TABLE lt_repo INTO rs_repo WITH KEY key = iv_key.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE Lcx_abapgit_not_found.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_repo~update_metadata.

    DATA:
      lv_blob            TYPE Lif_abapgit_persistence=>ty_content-data_str,
      ls_persistent_meta TYPE Lif_abapgit_persistence=>ty_repo.

    FIELD-SYMBOLS <lv_field>   LIKE LINE OF mt_meta_fields.
    FIELD-SYMBOLS <lg_dst>     TYPE any.
    FIELD-SYMBOLS <lg_src>     TYPE any.
    FIELD-SYMBOLS <lv_changed> TYPE abap_bool.

    ASSERT NOT iv_key IS INITIAL.

    IF is_change_mask IS INITIAL.
      RETURN.
    ENDIF.

    " Validations
    IF is_change_mask-url = abap_true AND is_meta-url IS INITIAL.
      Lcx_abapgit_exception=>raise( 'update, url empty' ).
    ENDIF.

    ls_persistent_meta = Lcl_abapgit_repo_srv=>get_instance( )->get( iv_key )->ms_data.

    " Update
    LOOP AT mt_meta_fields ASSIGNING <lv_field>.
      ASSIGN COMPONENT <lv_field> OF STRUCTURE is_change_mask TO <lv_changed>.
      ASSERT sy-subrc = 0.
      CHECK <lv_changed> = abap_true.
      ASSIGN COMPONENT <lv_field> OF STRUCTURE ls_persistent_meta TO <lg_dst>.
      ASSERT sy-subrc = 0.
      ASSIGN COMPONENT <lv_field> OF STRUCTURE is_meta TO <lg_src>.
      ASSERT sy-subrc = 0.
      <lg_dst> = <lg_src>.
    ENDLOOP.

    lv_blob = to_xml( ls_persistent_meta ).

    mo_db->update( iv_type  = Lcl_abapgit_persistence_db=>c_type_repo
                   iv_value = iv_key
                   iv_data  = lv_blob ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_PERSISTENCE_REPO implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_AQSG <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_aqsg=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_aqsg=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_AQSG implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD get_field_rules.

    ro_result = Lcl_abapgit_field_rules=>create( ).

* add rules here if needed

  ENDMETHOD.
  METHOD get_generic.
    " transaction SQ02
    CREATE OBJECT ro_generic
      EXPORTING
        is_item        = ms_item
        io_field_rules = get_field_rules( )
        iv_language    = mv_language.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    rv_user = c_user_unknown.
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

    DATA lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS <ls_bdcdata> LIKE LINE OF lt_bdcdata.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPMS38O'.
    <ls_bdcdata>-dynpro   = '0050'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'RS38Q-NAME'.
    <ls_bdcdata>-fval = ms_item-obj_name.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode      = 'SQ02'
      it_bdcdata    = lt_bdcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.
    get_generic( )->serialize( io_xml ).
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_AQSG implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_CHAR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_char=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_char=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_CHAR implementation.
*"* method's implementations
*include methods.
  METHOD instantiate_char_and_lock.

    DATA: lv_new  TYPE abap_bool,
          lv_name TYPE cls_attribute_name.


    SELECT SINGLE name FROM cls_attribute INTO lv_name WHERE name = ms_item-obj_name.
    lv_new = boolc( sy-subrc <> 0 ).
    lv_name = ms_item-obj_name.

    TRY.
        CREATE OBJECT ro_char
          EXPORTING
            im_name             = lv_name
            im_type_group       = iv_type_group
            im_new              = lv_new
            im_activation_state = iv_activation_state.
      CATCH cx_pak_invalid_data
          cx_pak_not_authorized
          cx_pak_invalid_state
          cx_pak_wb_object_locked.
        Lcx_abapgit_exception=>raise( 'Error while instantiating CL_CLS_ATTRIBUTE' ).
    ENDTRY.

    IF lv_new = abap_false.
      TRY.
          ro_char->if_pak_wb_object~lock_and_refresh( ).
        CATCH cx_pak_invalid_data
            cx_pak_not_authorized
            cx_pak_invalid_state
            cx_pak_wb_object_locked.
          Lcx_abapgit_exception=>raise( |Could not aquire lock, CHAR { lv_name }| ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE changed_by FROM cls_attribute INTO rv_user
      WHERE name = ms_item-obj_name
      AND activation_state = 'A'.

    IF rv_user IS INITIAL.
      SELECT SINGLE created_by FROM cls_attribute INTO rv_user
        WHERE name = ms_item-obj_name
        AND activation_state = 'A'.
    ENDIF.

    IF rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lo_char       TYPE REF TO cl_cls_attribute,
          lv_type_group TYPE cls_attribute-type_group,
          lx_pak_error  TYPE REF TO cx_root,
          lv_text       TYPE string.


    SELECT SINGLE type_group FROM cls_attribute INTO lv_type_group
      WHERE name = ms_item-obj_name
      AND activation_state = 'A'.

    lo_char = instantiate_char_and_lock( iv_type_group       = lv_type_group
                                         iv_activation_state = cl_pak_wb_domains=>co_activation_state-active ).

    TRY.
        lo_char->if_pak_wb_object~delete( ).

        lo_char->if_pak_wb_object~save( ).

        lo_char->if_pak_wb_object_internal~unlock( ).

      CATCH cx_pak_invalid_state cx_pak_invalid_data cx_pak_not_authorized INTO lx_pak_error.
        lo_char->if_pak_wb_object_internal~unlock( ).
        lv_text = lx_pak_error->get_text( ).
        Lcx_abapgit_exception=>raise( lv_text ).
      CLEANUP.
        lo_char->if_pak_wb_object_internal~unlock( ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_char        TYPE ty_char,
          ls_description LIKE LINE OF ls_char-cls_attributet,
          lo_char        TYPE REF TO cl_cls_attribute,
          lx_pak_error   TYPE REF TO cx_root,
          lv_text        TYPE string.

    FIELD-SYMBOLS: <ls_value>  LIKE LINE OF ls_char-cls_attr_value,
                   <lg_any>    TYPE any,
                   <ls_valuet> LIKE LINE OF ls_char-cls_attr_valuet.


    io_xml->read( EXPORTING iv_name = 'CHAR'
                  CHANGING cg_data = ls_char ).

    tadir_insert( iv_package ).

    lo_char = instantiate_char_and_lock( iv_type_group       = ls_char-cls_attribute-type_group
                                         iv_activation_state = cl_pak_wb_domains=>co_activation_state-inactive ).

    TRY.
        lo_char->if_cls_attribute~set_kind( ls_char-cls_attribute-kind ).
        lo_char->if_cls_attribute~set_single_valued( ls_char-cls_attribute-is_single_valued ).
        lo_char->if_cls_attribute~set_aspect(
          im_aspect_for   = ls_char-cls_attribute-is_aspect_for
          im_aspect_value = ls_char-cls_attribute-aspect_value ).
        lo_char->if_cls_attribute~set_default_flag( ls_char-cls_attribute-default_flag ).
        lo_char->if_cls_attribute~set_default_value( ls_char-cls_attribute-default_value ).
        lo_char->if_cls_attribute~set_sub_object_treatment( ls_char-cls_attribute-sub_obj_treatm ).
        lo_char->if_cls_attribute~set_automatic_changes_allowed( ls_char-cls_attribute-automatic_change ).
        lo_char->if_cls_attribute~set_manual_changes_allowed( ls_char-cls_attribute-manu_chag_allow ).
        lo_char->if_cls_attribute~set_implicit_changes_allowed( ls_char-cls_attribute-implicit_change ).
        lo_char->if_cls_attribute~set_expl_values_dominate_links( ls_char-cls_attribute-weak_links ).
        lo_char->if_cls_attribute~set_assignment_package_rule( ls_char-cls_attribute-assignment_devc ).

* Method SET_HIDE_ICON does not exist in some releases, not present in 751
        ASSIGN COMPONENT 'HIDE_ICONS' OF STRUCTURE ls_char-cls_attribute TO <lg_any>.
        IF sy-subrc = 0.
          CALL METHOD lo_char->('IF_CLS_ATTRIBUTE~SET_HIDE_ICON')
            EXPORTING
              im_hide_icon = <lg_any>.
        ENDIF.

        lo_char->if_cls_attribute~set_hide_remark( ls_char-cls_attribute-hide_remark ).
        lo_char->if_cls_attribute~set_visible_in_customer_system( ls_char-cls_attribute-visible_for_cust ).
        lo_char->if_cls_attribute~set_value_table( ls_char-cls_attribute-value_table ).
        lo_char->if_cls_attribute~set_vtable_field( ls_char-cls_attribute-vtable_field ).
        lo_char->if_cls_attribute~set_vtable_icon_f( ls_char-cls_attribute-vtable_icon_f ).
        lo_char->if_cls_attribute~set_vtext_langu_f( ls_char-cls_attribute-vtext_langu_f ).
        lo_char->if_cls_attribute~set_vtext_table( ls_char-cls_attribute-vtext_table ).
        lo_char->if_cls_attribute~set_vtext_text_f( ls_char-cls_attribute-vtext_text_f ).
        lo_char->if_cls_attribute~set_vtext_value_f( ls_char-cls_attribute-vtext_value_f ).
        lo_char->if_cls_attribute~set_existing_objects_only( ls_char-cls_attribute-existing_objects ).
        lo_char->if_cls_attribute~set_objs_of_typegr( ls_char-cls_attribute-objs_of_typegr ).
        lo_char->if_cls_attribute~set_obj_values_have_subtypes( ls_char-cls_attribute-objs_w_subtype ).
        lo_char->if_cls_attribute~set_arbtry_val_type( ls_char-cls_attribute-arbtry_val_type ).

        READ TABLE ls_char-cls_attributet INTO ls_description WITH KEY langu = mv_language.
        IF sy-subrc <> 0.
          READ TABLE ls_char-cls_attributet INTO ls_description INDEX 1.
        ENDIF.
        lo_char->if_cls_attribute~set_description( ls_description-text ).

        LOOP AT ls_char-cls_attr_value ASSIGNING <ls_value>.
          <ls_value>-activation_state = 'I'.
        ENDLOOP.
        LOOP AT ls_char-cls_attr_valuet ASSIGNING <ls_valuet>.
          <ls_valuet>-activation_state = 'I'.
        ENDLOOP.

        lo_char->if_cls_attribute~set_values(
          im_values   = ls_char-cls_attr_value
          im_values_t = ls_char-cls_attr_valuet ).

        set_default_package( iv_package ).

        lo_char->if_pak_wb_object~save( ).

        lo_char->if_pak_wb_object~activate( ).

        lo_char->if_pak_wb_object_internal~unlock( ).

      CATCH cx_pak_invalid_state cx_pak_invalid_data cx_pak_not_authorized INTO lx_pak_error.
        lo_char->if_pak_wb_object_internal~unlock( ).
        lv_text = lx_pak_error->get_text( ).
        Lcx_abapgit_exception=>raise( lv_text ).
      CLEANUP.
        lo_char->if_pak_wb_object_internal~unlock( ).
    ENDTRY.

    deserialize_longtexts( ii_xml         = io_xml
                           iv_longtext_id = c_longtext_id_char ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    rv_bool = cl_cls_attribute=>exists_object_attribute( ms_item-obj_name ).

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

    DATA: ls_char TYPE ty_char.

    CONSTANTS: lc_active TYPE c LENGTH 1 VALUE 'A'.


    SELECT SINGLE * FROM cls_attribute INTO ls_char-cls_attribute
      WHERE name = ms_item-obj_name
      AND activation_state = lc_active.
* todo, ASSIGNMENT_DEVC?

    CLEAR: ls_char-cls_attribute-created_by,
           ls_char-cls_attribute-created_on,
           ls_char-cls_attribute-changed_by,
           ls_char-cls_attribute-changed_on.

    SELECT * FROM cls_attributet INTO TABLE ls_char-cls_attributet
      WHERE name = ms_item-obj_name
      AND activation_state = lc_active
      ORDER BY PRIMARY KEY.
    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      DELETE ls_char-cls_attributet WHERE langu <> mv_language.
    ENDIF.

    SELECT * FROM cls_attr_value INTO TABLE ls_char-cls_attr_value
      WHERE name = ms_item-obj_name
      AND activation_state = lc_active
      ORDER BY PRIMARY KEY.

    SELECT * FROM cls_attr_valuet INTO TABLE ls_char-cls_attr_valuet
      WHERE name = ms_item-obj_name
      AND activation_state = lc_active
      ORDER BY PRIMARY KEY.
    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      DELETE ls_char-cls_attr_valuet WHERE langu <> mv_language.
    ENDIF.

    io_xml->add( iv_name = 'CHAR'
                 ig_data = ls_char ).

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_char ).

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
endclass. "ZCL_ABAPGIT_OBJECT_CHAR implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_TTYP <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ttyp=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ttyp=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_TTYP implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE as4user FROM dd40l INTO rv_user
      WHERE typename = ms_item-obj_name
      AND as4local = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    delete_ddic( 'A' ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_name  TYPE ddobjname,
          lt_dd42v TYPE dd42v_tab,
          lt_dd43v TYPE dd43v_tab,
          ls_dd40v TYPE dd40v,
          lv_msg   TYPE string.

    io_xml->read( EXPORTING iv_name = 'DD40V'
                  CHANGING cg_data = ls_dd40v ).

    io_xml->read( EXPORTING iv_name = 'DD42V'
                  CHANGING cg_data = lt_dd42v ).
    io_xml->read( EXPORTING iv_name = 'DD43V'
                  CHANGING cg_data = lt_dd43v ).

    corr_insert( iv_package = iv_package
                 ig_object_class = 'DICT' ).

    lv_name = ms_item-obj_name. " type conversion

    CALL FUNCTION 'DDIF_TTYP_PUT'
      EXPORTING
        name              = lv_name
        dd40v_wa          = ls_dd40v
      TABLES
        dd42v_tab         = lt_dd42v
        dd43v_tab         = lt_dd43v
      EXCEPTIONS
        ttyp_not_found    = 1
        name_inconsistent = 2
        ttyp_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.

    IF sy-subrc <> 0.
      lv_msg = |Error in DDIF_TTYP_PUT on object { lv_name }|.

      CASE sy-subrc.
        WHEN 1.
          lv_msg = lv_msg && | (TTYP_NOT_FOUND)|.
        WHEN 2.
          lv_msg = lv_msg && | (NAME_INCONSISTENT)|.
        WHEN 3.
          lv_msg = lv_msg && | (TTYP_INCONSISTENT)|.
        WHEN 4.
          lv_msg = lv_msg && | (PUT_FAILURE)|.
        WHEN 5.
          lv_msg = lv_msg && | (PUT_REFUSED)|.
        WHEN OTHERS.
      ENDCASE.

      Lcx_abapgit_exception=>raise( lv_msg ).
    ENDIF.

    deserialize_longtexts( ii_xml         = io_xml
                           iv_longtext_id = c_longtext_id_ttyp ).

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_typename TYPE dd40l-typename.

    SELECT SINGLE typename FROM dd40l INTO lv_typename
      WHERE typename = ms_item-obj_name.
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

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESDICT'
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECT=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_name  TYPE ddobjname,
          lv_state TYPE ddgotstate,
          lt_dd42v TYPE dd42v_tab,
          lt_dd43v TYPE dd43v_tab,
          ls_dd40v TYPE dd40v.


    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_TTYP_GET'
      EXPORTING
        name          = lv_name
        state         = 'A'
        langu         = mv_language
      IMPORTING
        gotstate      = lv_state
        dd40v_wa      = ls_dd40v
      TABLES
        dd42v_tab     = lt_dd42v
        dd43v_tab     = lt_dd43v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF ls_dd40v IS INITIAL OR lv_state <> 'A'.
      RETURN.
    ENDIF.

    CLEAR: ls_dd40v-as4user,
           ls_dd40v-as4date,
           ls_dd40v-as4time.

    IF NOT ls_dd40v-rowkind IS INITIAL.
      CLEAR ls_dd40v-typelen.
    ENDIF.

    io_xml->add( iv_name = 'DD40V'
                 ig_data = ls_dd40v ).
    io_xml->add( iv_name = 'DD42V'
                 ig_data = lt_dd42v ).
    io_xml->add( iv_name = 'DD43V'
                 ig_data = lt_dd43v ).

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_ttyp ).

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
endclass. "ZCL_ABAPGIT_OBJECT_TTYP implementation

*>>>>>>> ZCL_ABAPGIT_PERSISTENCE_USER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_persistence_user==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_persistence_user==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_persistence_user==ccau.


class LCL_ABAPGIT_PERSISTENCE_USER implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    mv_user = iv_user.
    read( ).
  ENDMETHOD.
  METHOD from_xml.

    DATA: lv_xml TYPE string.

    lv_xml = iv_xml.

* fix downward compatibility
    REPLACE ALL OCCURRENCES OF '<_--28C_TYPE_USER_--29>' IN lv_xml WITH '<USER>'.
    REPLACE ALL OCCURRENCES OF '</_--28C_TYPE_USER_--29>' IN lv_xml WITH '</USER>'.

    CALL TRANSFORMATION id
      OPTIONS value_handling = 'accept_data_loss'
      SOURCE XML lv_xml
      RESULT user = rs_user.
  ENDMETHOD.
  METHOD get_instance.

    IF iv_user = sy-uname ##USER_OK.
      IF gi_current_user IS NOT BOUND.
        CREATE OBJECT gi_current_user TYPE Lcl_abapgit_persistence_user.
      ENDIF.
      ri_user = gi_current_user.
    ELSE.
      CREATE OBJECT ri_user TYPE Lcl_abapgit_persistence_user
        EXPORTING
          iv_user = iv_user.
    ENDIF.

  ENDMETHOD.
  METHOD read.

    DATA: lv_xml TYPE string.

    TRY.
        lv_xml = Lcl_abapgit_persistence_db=>get_instance( )->read(
          iv_type  = Lcl_abapgit_persistence_db=>c_type_user
          iv_value = mv_user ).
      CATCH Lcx_abapgit_not_found.
        RETURN.
    ENDTRY.

    ms_user = from_xml( lv_xml ).

  ENDMETHOD.
  METHOD read_repo_config.
    DATA lv_url TYPE string.
    lv_url = to_lower( iv_url ).
    READ TABLE ms_user-repo_config INTO rs_repo_config WITH KEY url = lv_url.
  ENDMETHOD.
  METHOD to_xml.
    CALL TRANSFORMATION id
      SOURCE user = is_user
      RESULT XML rv_xml.
  ENDMETHOD.
  METHOD update.

    DATA: lv_xml TYPE string.

    lv_xml = to_xml( ms_user ).

    Lcl_abapgit_persistence_db=>get_instance( )->modify(
      iv_type  = Lcl_abapgit_persistence_db=>c_type_user
      iv_value = mv_user
      iv_data  = lv_xml ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.
  METHOD update_repo_config.

    DATA: lv_key  TYPE string.

    FIELD-SYMBOLS <ls_repo_config> TYPE ty_repo_config.

    lv_key  = to_lower( iv_url ).

    READ TABLE ms_user-repo_config ASSIGNING <ls_repo_config> WITH KEY url = lv_key.
    IF sy-subrc IS NOT INITIAL.
      APPEND INITIAL LINE TO ms_user-repo_config ASSIGNING <ls_repo_config>.
    ENDIF.
    <ls_repo_config>     = is_repo_config.
    <ls_repo_config>-url = lv_key.

    update( ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_changes_only.

    rv_changes_only = ms_user-changes_only.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_default_git_user_email.

    rv_email = ms_user-default_git_user-email.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_default_git_user_name.

    rv_username = ms_user-default_git_user-name.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_diff_first.
    rv_diff_first = ms_user-diff_first.
  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_diff_unified.

    rv_diff_unified = ms_user-diff_unified.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_favorites.

    rt_favorites = ms_user-favorites.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_hide_files.

    rv_hide = ms_user-hide_files.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_order_by.
    rv_order_by = ms_user-order_by.
  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_order_descending.
    rv_order_descending = ms_user-order_descending.
  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_repo_git_user_email.

    rv_email = read_repo_config( iv_url )-git_user-email.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_repo_git_user_name.

    rv_username = read_repo_config( iv_url )-git_user-name.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_repo_last_change_seen.

    rv_version = read_repo_config( iv_url )-last_change_seen.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_repo_login.

    rv_login = read_repo_config( iv_url )-login.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_repo_show.

    rv_key = ms_user-repo_show.

    IF rv_key IS INITIAL.
      RETURN.
    ENDIF.

    " Check if repo exists
    TRY.
        Lcl_abapgit_persistence_db=>get_instance( )->read(
          iv_type  = Lcl_abapgit_persistence_db=>c_type_repo
          iv_value = rv_key ).
      CATCH Lcx_abapgit_not_found.
        " remove invalid key
        CLEAR rv_key.
        Lif_abapgit_persist_user~set_repo_show( rv_key ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_settings.

    rs_user_settings = ms_user-settings.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_show_folders.

    rv_folders = ms_user-show_folders.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~is_favorite_repo.

    READ TABLE ms_user-favorites TRANSPORTING NO FIELDS
      WITH KEY table_line = iv_repo_key.

    rv_yes = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~set_default_git_user_email.

    ms_user-default_git_user-email = iv_email.
    update( ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~set_default_git_user_name.

    ms_user-default_git_user-name = iv_username.
    update( ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~set_diff_first.
    ms_user-diff_first = iv_diff_first.
    update( ).
    rv_diff_first = ms_user-diff_first.
  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~set_order_by.
    ms_user-order_by = iv_order_by.
    update( ).
    rv_order_by = ms_user-order_by.
  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~set_order_descending.
    ms_user-order_descending = iv_order_descending.
    update( ).
    rv_order_descending = ms_user-order_descending.
  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~set_repo_git_user_email.

    DATA: ls_repo_config TYPE ty_repo_config.

    ls_repo_config                = read_repo_config( iv_url ).
    ls_repo_config-git_user-email = iv_email.
    update_repo_config( iv_url = iv_url
                        is_repo_config = ls_repo_config ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~set_repo_git_user_name.

    DATA: ls_repo_config TYPE ty_repo_config.

    ls_repo_config               = read_repo_config( iv_url ).
    ls_repo_config-git_user-name = iv_username.
    update_repo_config( iv_url = iv_url
                        is_repo_config = ls_repo_config ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~set_repo_last_change_seen.

    DATA: ls_repo_config TYPE ty_repo_config.

    ls_repo_config                  = read_repo_config( iv_url ).
    ls_repo_config-last_change_seen = iv_version.
    update_repo_config( iv_url = iv_url
                        is_repo_config = ls_repo_config ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~set_repo_login.

    DATA: ls_repo_config TYPE ty_repo_config.

    ls_repo_config       = read_repo_config( iv_url ).
    ls_repo_config-login = iv_login.
    update_repo_config( iv_url = iv_url
                        is_repo_config = ls_repo_config ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~set_repo_show.

    ms_user-repo_show = iv_key.
    update( ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~set_settings.

    ms_user-settings = is_user_settings.
    update( ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~toggle_changes_only.

    ms_user-changes_only = boolc( ms_user-changes_only = abap_false ).
    update( ).

    rv_changes_only = ms_user-changes_only.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~toggle_diff_unified.

    ms_user-diff_unified = boolc( ms_user-diff_unified = abap_false ).
    update( ).

    rv_diff_unified = ms_user-diff_unified.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~toggle_favorite.

    READ TABLE ms_user-favorites TRANSPORTING NO FIELDS
      WITH KEY table_line = iv_repo_key.

    IF sy-subrc = 0.
      DELETE ms_user-favorites INDEX sy-tabix.
    ELSE.
      APPEND iv_repo_key TO ms_user-favorites.
    ENDIF.

    update( ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~toggle_hide_files.

    ms_user-hide_files = boolc( ms_user-hide_files = abap_false ).
    update( ).

    rv_hide = ms_user-hide_files.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~toggle_show_folders.
    ms_user-show_folders = boolc( ms_user-show_folders = abap_false ).
    update( ).

    rv_folders = ms_user-show_folders.
  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_list_settings.

    rs_list_settings = ms_user-list_settings.

    IF rs_list_settings IS INITIAL.
      " for performance reasons, set "only favorites" as a default
      IF Lcl_abapgit_repo_srv=>get_instance( )->list_favorites( ) IS NOT INITIAL.
        rs_list_settings-only_favorites = abap_true.
      ENDIF.

      rs_list_settings-order_by = |NAME|.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~set_list_settings.
    ms_user-list_settings = is_list_settings.
    update( ).
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_PERSISTENCE_USER implementation

*>>>>>>> ZCL_ABAPGIT_TRANSPORT_2_BRANCH <<<<<<<*

*"* macro definitions
*include zcl_abapgit_transport_2_branchccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_transport_2_branchccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_TRANSPORT_2_BRANCH implementation.
*"* method's implementations
*include methods.
  METHOD create.
    DATA:
      lv_branch_name     TYPE string,
      ls_comment         TYPE Lif_abapgit_git_definitions=>ty_comment,
      lo_stage           TYPE REF TO Lcl_abapgit_stage,
      ls_stage_objects   TYPE Lif_abapgit_definitions=>ty_stage_files,
      lt_object_statuses TYPE Lif_abapgit_definitions=>ty_results_tt.

    lv_branch_name = Lcl_abapgit_git_branch_list=>complete_heads_branch_name(
        Lcl_abapgit_git_branch_list=>normalize_branch_name( is_transport_to_branch-branch_name ) ).

    io_repository->create_branch( lv_branch_name ).

    CREATE OBJECT lo_stage.

    ls_stage_objects = Lcl_abapgit_factory=>get_stage_logic( )->get( io_repository ).

    lt_object_statuses = Lcl_abapgit_repo_status=>calculate( io_repository ).

    stage_transport_objects(
       it_transport_objects = it_transport_objects
       io_stage             = lo_stage
       is_stage_objects     = ls_stage_objects
       it_object_statuses   = lt_object_statuses ).

    ls_comment = generate_commit_message( is_transport_to_branch ).

    io_repository->push( is_comment = ls_comment
                         io_stage   = lo_stage ).
  ENDMETHOD.
  METHOD generate_commit_message.
    rs_comment-committer-name  = sy-uname.
    rs_comment-committer-email = |{ rs_comment-committer-name }@localhost|.
    rs_comment-comment         = is_transport_to_branch-commit_text.
  ENDMETHOD.
  METHOD stage_transport_objects.
    DATA lo_transport_objects TYPE REF TO Lcl_abapgit_transport_objects.
    CREATE OBJECT lo_transport_objects
      EXPORTING
        it_transport_objects = it_transport_objects.

    lo_transport_objects->to_stage(
      io_stage           = io_stage
      is_stage_objects   = is_stage_objects
      it_object_statuses = it_object_statuses ).
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_TRANSPORT_2_BRANCH implementation

*>>>>>>> ZCX_ABAPGIT_AJSON_ERROR <<<<<<<*

*"* macro definitions
*include zcx_abapgit_ajson_error=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcx_abapgit_ajson_error=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcx_abapgit_ajson_error=======ccau.


class LCX_ABAPGIT_AJSON_ERROR implementation.
*"* method's implementations
*include methods.
method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->RC = RC .
me->MESSAGE = MESSAGE .
me->LOCATION = LOCATION .
me->A1 = A1 .
me->A2 = A2 .
me->A3 = A3 .
me->A4 = A4 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_AJSON_ERROR .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
endmethod.
method raise.

  data lx type ref to Lcx_abapgit_ajson_error.

  create object lx exporting message = iv_msg.
  lx->set_location(
    iv_location = iv_location
    is_node     = is_node ).
  raise exception lx.

endmethod.
method set_location.

  data ls_msg type ty_message_parts.
  data lv_location type string.
  data lv_tmp type string.
  field-symbols <path> type string.
  field-symbols <name> type string.

  if iv_location is not initial.
    lv_location = iv_location.
  elseif is_node is not initial.
    assign component 'PATH' of structure is_node to <path>.
    assign component 'NAME' of structure is_node to <name>.
    if <path> is assigned and <name> is assigned.
      lv_location = <path> && <name>.
    endif.
  endif.

  if lv_location is not initial.
    lv_tmp = message && | @{ lv_location }|.
  else.
    lv_tmp = message.
  endif.

  ls_msg = lv_tmp.

  location = lv_location.
  a1       = ls_msg-a1.
  a2       = ls_msg-a2.
  a3       = ls_msg-a3.
  a4       = ls_msg-a4.

endmethod.
endclass. "ZCX_ABAPGIT_AJSON_ERROR implementation

*>>>>>>> ZCL_ABAPGIT_ECATT_CONFIG_DOWNL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ecatt_config_downlccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ecatt_config_downlccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ECATT_CONFIG_DOWNL implementation.
*"* method's implementations
*include methods.
  METHOD download.

    " Downport

    DATA: lv_partyp TYPE string.

    load_help = im_load_help.
    typ = im_object_type.

    TRY.
        cl_apl_ecatt_object=>show_object(
          EXPORTING
            im_obj_type = im_object_type
            im_name     = im_object_name
            im_version  = im_object_version
          IMPORTING
            re_object   = ecatt_object ).
      CATCH cx_ecatt INTO ex_ecatt.
        RETURN.
    ENDTRY.

    lv_partyp = cl_apl_ecatt_const=>params_type_par.

    set_attributes_to_template( ).
    ecatt_config ?= ecatt_object.

    CALL METHOD ('SET_ECATT_OBJECTS_TO_TEMPLATE'). " doesn't exist in 702

* MS180406
    set_var_mode_to_dom( ).
* ENDMS180406
    get_general_params_data( im_params = ecatt_config->params
                             im_ptyp   = lv_partyp ).
    LOOP AT parm INTO wa_parm.
      set_general_params_data_to_dom( ).
      IF NOT wa_parm-val_type IS INITIAL.
        set_deep_stru_to_dom( ecatt_config->params ).
        set_deep_data_to_dom( im_params = ecatt_config->params
                              im_pindex = wa_parm-pindex ).
      ENDIF.
    ENDLOOP.

    set_variants_to_dom( ecatt_config->params ).

    download_data( ).

  ENDMETHOD.
  METHOD download_data.

    " Downport

    mv_xml_stream = Lcl_abapgit_ecatt_helper=>download_data( template_over_all ).

  ENDMETHOD.
  METHOD Lif_abapgit_ecatt_download~get_xml_stream.

    rv_xml_stream = mv_xml_stream.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ECATT_CONFIG_DOWNL implementation

*>>>>>>> ZCL_ABAPGIT_ECATT_DATA_DOWNL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ecatt_data_downl==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ecatt_data_downl==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ECATT_DATA_DOWNL implementation.
*"* method's implementations
*include methods.
  METHOD download.

    " Downport

    DATA: lv_partyp TYPE string.

    load_help = im_load_help.

    TRY.
        cl_apl_ecatt_object=>show_object(
          EXPORTING
            im_obj_type = im_object_type
            im_name     = im_object_name
            im_version  = im_object_version
          IMPORTING
            re_object   = ecatt_object ).
      CATCH cx_ecatt INTO ex_ecatt.
        RETURN.
    ENDTRY.

    typ = im_object_type.

    lv_partyp = cl_apl_ecatt_const=>params_type_par.

    ecatt_data ?= ecatt_object.
    set_attributes_to_template( ).
    get_general_params_data( im_params = ecatt_data->params
                             im_ptyp   = lv_partyp ).

    LOOP AT parm INTO wa_parm.
      set_general_params_data_to_dom( ).
      IF NOT wa_parm-val_type IS INITIAL.
        set_deep_stru_to_dom( ecatt_data->params ).
        set_deep_data_to_dom( im_params = ecatt_data->params
                              im_pindex = wa_parm-pindex ).
      ENDIF.
    ENDLOOP.

* MS180406
    set_var_mode_to_dom( ).
* ENDMS180406
    set_variants_to_dom( ecatt_data->params ).

    download_data( ).

  ENDMETHOD.
  METHOD download_data.

    " Downport

    mv_xml_stream = Lcl_abapgit_ecatt_helper=>download_data( template_over_all ).

  ENDMETHOD.
  METHOD Lif_abapgit_ecatt_download~get_xml_stream.

    rv_xml_stream = mv_xml_stream.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ECATT_DATA_DOWNL implementation

*>>>>>>> ZCL_ABAPGIT_ECATT_DATA_UPLOAD <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ecatt_data_upload=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ecatt_data_upload=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ECATT_DATA_UPLOAD implementation.
*"* method's implementations
*include methods.
  METHOD upload_data_from_stream.

    " Downport
    template_over_all = Lcl_abapgit_ecatt_helper=>upload_data_from_stream( mv_external_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_ecatt_upload~set_stream_for_upload.

    " donwnpoort from CL_ABAPGIT_ECATT_DATA_UPLOAD SET_STREAM_FOR_UPLOAD
    mv_external_xml = iv_xml.

  ENDMETHOD.
  METHOD on_ev_object_saved.
    DATA lo_ecatt_td TYPE REF TO cl_apl_ecatt_test_data.

    " Trickery to remove any local variants that do not exist on the remote on pull.

    SET HANDLER on_ev_object_saved FOR ALL INSTANCES ACTIVATION abap_false.

    TRY.
        IF ex_ecatt_object->object_type <> ms_current_object-s_obj_type OR
           ex_ecatt_object->object_name <> ms_current_object-d_obj_name OR
           ex_ecatt_object->object_version <> ms_current_object-d_obj_ver.
          CREATE OBJECT mx_ecatt_apl
            EXPORTING
              textid    = cx_ecatt_apl=>any_text
              free_text = 'Unexpected object in save sequence'.
          RETURN.
        ENDIF.

        lo_ecatt_td ?= ex_ecatt_object.
        lo_ecatt_td->params->delete_variants( '*' ).
        TRY.
            CALL METHOD ('GET_VARIANTS_FROM_DOM_NEW')
              EXPORTING
                im_params = lo_ecatt_td->params.
          CATCH cx_sy_dyn_call_error.
            get_variants_from_dom( lo_ecatt_td->params ).
        ENDTRY.
        lo_ecatt_td->save( ).
      CATCH cx_ecatt_apl INTO mx_ecatt_apl.
        RETURN.
    ENDTRY.
  ENDMETHOD.
  METHOD upload.
    SET HANDLER on_ev_object_saved FOR ALL INSTANCES.

    ms_current_object-s_obj_type = ch_object-s_obj_type.
    ms_current_object-d_obj_name = ch_object-d_obj_name.
    ms_current_object-d_obj_ver = ch_object-d_obj_ver.

    TRY.
        super->upload( CHANGING ch_object = ch_object ).
        SET HANDLER on_ev_object_saved FOR ALL INSTANCES ACTIVATION abap_false.
      CLEANUP.
        SET HANDLER on_ev_object_saved FOR ALL INSTANCES ACTIVATION abap_false.
    ENDTRY.

    IF mx_ecatt_apl IS BOUND.
      raise_upload_exception( previous = mx_ecatt_apl ).
    ENDIF.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ECATT_DATA_UPLOAD implementation

*>>>>>>> ZCL_ABAPGIT_ECATT_SCRIPT_DOWNL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ecatt_script_downlccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ecatt_script_downlccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ECATT_SCRIPT_DOWNL implementation.
*"* method's implementations
*include methods.
  METHOD download.

    " Downport

    load_help = im_load_help.
    typ = im_object_type.

    TRY.
        cl_apl_ecatt_object=>show_object(
          EXPORTING
            im_obj_type = im_object_type
            im_name     = im_object_name
            im_version  = im_object_version
          IMPORTING
            re_object   = ecatt_object ).
      CATCH cx_ecatt INTO ex_ecatt.
        RETURN.
    ENDTRY.

    toolname = ecatt_object->attrib->get_tool_name( ).
    set_attributes_to_template( ).

    IF toolname = cl_apl_ecatt_const=>toolname_ecatt.

      ecatt_script ?= ecatt_object.

      set_script_to_template( ).

      TRY.
          get_general_params_data( ecatt_script->params ).
        CATCH cx_ecatt_apl.                              "#EC NO_HANDLER
*         proceed with download and report errors later
      ENDTRY.

      LOOP AT parm INTO wa_parm.
        TRY.
            IF wa_parm-value = '<INITIAL>'.
              CLEAR wa_parm-value.
            ENDIF.
            set_general_params_data_to_dom( ).
            IF NOT wa_parm-pstruc_typ IS INITIAL.
              set_deep_stru_to_dom( ecatt_script->params ).
              set_deep_data_to_dom( ecatt_script->params ).
              IF wa_parm-xmlref_typ = cl_apl_ecatt_const=>ref_type_c_tcd.
                set_control_data_for_tcd( is_param  = wa_parm
                                          io_params = ecatt_script->params ).

              ENDIF.
            ENDIF.
          CATCH cx_ecatt_apl.                            "#EC NO_HANDLER
*         proceed with download and report errors later
        ENDTRY.
      ENDLOOP.

    ELSE.

      set_blob_to_template( ).
      set_artmp_to_template( ).

    ENDIF.

    download_data( ).

  ENDMETHOD.
  METHOD download_data.

    " Downport

    mv_xml_stream = Lcl_abapgit_ecatt_helper=>download_data( template_over_all ).

  ENDMETHOD.
  METHOD escape_control_data.

    " Downport

    DATA: li_iter     TYPE REF TO if_ixml_node_iterator,
          li_textit   TYPE REF TO if_ixml_node_iterator,
          li_abapctrl TYPE REF TO if_ixml_node_collection,
          li_text     TYPE REF TO if_ixml_text,
          li_filter   TYPE REF TO if_ixml_node_filter,
          li_list     TYPE REF TO if_ixml_node_list,
          lv_value    TYPE etdom_name,
          li_vars     TYPE REF TO if_ixml_element,
          li_elem     TYPE REF TO if_ixml_element.

    li_vars = ii_element->find_from_name_ns( iv_tabname ).
    li_filter = ii_element->create_filter_node_type( if_ixml_node=>co_node_text ).
    IF li_vars IS NOT INITIAL.
      li_abapctrl = ii_element->get_elements_by_tag_name_ns( iv_node ).

* just for debugging
      li_iter = li_abapctrl->create_iterator( ).
      li_elem ?= li_iter->get_next( ).
      WHILE li_elem IS NOT INITIAL.
        li_list = li_elem->get_children( ).

        li_textit = li_list->create_rev_iterator_filtered( li_filter ).
        li_text ?= li_textit->get_next( ).
        IF li_text IS NOT INITIAL.
          lv_value = li_text->get_data( ).
          IF lv_value(1) = cl_abap_char_utilities=>minchar.
            REPLACE SECTION OFFSET 0 LENGTH 1 OF lv_value WITH space.
            li_text->set_value( value = lv_value ).
          ENDIF.
        ENDIF.
        CLEAR: li_textit, li_list, li_elem, lv_value.
        li_elem ?= li_iter->get_next( ).
      ENDWHILE.
      CLEAR: li_abapctrl, li_elem, li_iter.

    ENDIF.

  ENDMETHOD.
  METHOD set_artmp_to_template.

    " Downport

    DATA: li_artmp_node   TYPE REF TO if_ixml_element,
          lv_rc           TYPE sy-subrc,
          lv_text         TYPE string,
          lv_rc_args_tmpl TYPE i,
          lv_errmsg       TYPE string.

    li_artmp_node = template_over_all->create_simple_element(
                      name   = 'ECET_ARTMP'
                      parent = root_node ).

    ecatt_extprog->get_args_tmpl(
      IMPORTING
        ex_xml_arg_tmpl = lv_text
        ex_rc           = lv_rc_args_tmpl
        ex_errmsg       = lv_errmsg ).

    IF li_artmp_node IS INITIAL OR lv_rc_args_tmpl > 0.
      raise_download_exception(
          textid        = cx_ecatt_apl_util=>download_processing
          previous      = ex_ecatt
          called_method = 'CL_APL_ECATT_SCRIPT_DOWNLOAD->SET_ARTMP_TO_TEMPLATE'
          free_text     = lv_errmsg ).
    ENDIF.

    lv_rc = li_artmp_node->set_value( lv_text ).
    IF lv_rc <> 0.
      raise_download_exception(
            textid        = cx_ecatt_apl_util=>download_processing
            previous      = ex_ecatt
            called_method = 'CL_APL_ECATT_SCRIPT_DOWNLOAD->SET_ARTMP_TO_TEMPLATE' ).
    ENDIF.

  ENDMETHOD.
  METHOD set_blob_to_template.

    " Downport

    DATA: li_blob_node TYPE REF TO if_ixml_element,
          lv_rc        TYPE sy-subrc,
          lv_text      TYPE string.

    li_blob_node = template_over_all->create_simple_element(
                  name   = 'ECET_BLOBS'
                  parent = root_node ).

    IF li_blob_node IS INITIAL.
      raise_download_exception(
            textid        = cx_ecatt_apl_util=>download_processing
            previous      = ex_ecatt
            called_method = 'CL_APL_ECATT_SCRIPT_DOWNLOAD->SET_BLOB_TO_TEMPLATE' ).
    ENDIF.

    ecatt_extprog->get_blob(
      EXPORTING
        im_whole_data = 1
      IMPORTING
        ex_xml_blob   = lv_text ).

    lv_rc = li_blob_node->set_value( lv_text ).
    IF lv_rc <> 0.
      raise_download_exception(
            textid        = cx_ecatt_apl_util=>download_processing
            previous      = ex_ecatt
            called_method = 'CL_APL_ECATT_SCRIPT_DOWNLOAD->SET_BLOB_TO_TEMPLATE' ).
    ENDIF.

  ENDMETHOD.
  METHOD set_control_data_for_tcd.

    " Downport

    DATA: lt_params TYPE ettcd_params_tabtype,
          lt_verbs  TYPE ettcd_verbs_tabtype,
          lt_vars   TYPE ettcd_vars_tabtype,
          lt_dp_tab TYPE ettcd_dp_tab_tabtype,
          lt_dp_for TYPE ettcd_dp_for_tabtype,
          lt_dp_pro TYPE ettcd_dp_pro_tabtype,
          lt_dp_fld TYPE ettcd_dp_fld_tabtype,
          lt_svars  TYPE ettcd_svars_tabtype.

    DATA: li_element   TYPE REF TO if_ixml_element,
          li_deep_tcd  TYPE REF TO if_ixml_element,
          lv_rc        TYPE sy-subrc,
          lv_name      TYPE string,
          lv_parname   TYPE string,
          lo_pval_xml  TYPE REF TO cl_apl_ecatt_xml_data,
          lo_ctrl_tabs TYPE REF TO cl_apl_ecatt_control_tables.

    FIELD-SYMBOLS: <lt_tab> TYPE STANDARD TABLE.

    IF is_param-xmlref_typ <> cl_apl_ecatt_const=>ref_type_c_tcd OR io_params IS INITIAL.
      RETURN.
    ENDIF.

    lv_parname = is_param-pname.

    io_params->get_param_value(     "TCD command interface
      EXPORTING
        im_var_id   = cl_apl_ecatt_const=>varid_default_val
        im_pname    = lv_parname
        im_pindex   = is_param-pindex
      IMPORTING
        ex_pval_xml = lo_pval_xml ).

    lo_ctrl_tabs = lo_pval_xml->get_control_tables_ref( ).
    IF lo_ctrl_tabs IS INITIAL.
      RETURN.
    ENDIF.

    lo_ctrl_tabs->get_control_tables(          "Read 8 control tables
      IMPORTING
        ex_params = lt_params
        ex_verbs  = lt_verbs
        ex_vars   = lt_vars
        ex_dp_tab = lt_dp_tab
        ex_dp_for = lt_dp_for
        ex_dp_pro = lt_dp_pro
        ex_dp_fld = lt_dp_fld
        ex_svars  = lt_svars ).

    IF lt_params IS INITIAL OR
       lt_verbs  IS INITIAL OR
       lt_vars   IS INITIAL OR
       lt_dp_tab IS INITIAL OR
       lt_dp_for IS INITIAL OR
       lt_dp_pro IS INITIAL OR
       lt_dp_fld IS INITIAL OR
       lt_svars  IS INITIAL.

      RETURN.
    ENDIF.

    li_deep_tcd = template_over_all->create_simple_element_ns(
                    name   = cl_apl_xml_const=>upl_tcd_node
                    parent = ap_current_param ).

    IF li_deep_tcd IS INITIAL.
      raise_download_exception(
            textid   = cx_ecatt_apl_util=>download_processing
            previous = ex_ecatt ).
    ENDIF.

    DO 8 TIMES.                                "Loop at 8 control tables
      CASE sy-index.
        WHEN 1.
          lv_name = 'ETTCD_PARAMS_TABTYPE'.
          ASSIGN lt_params TO <lt_tab>.
        WHEN 2.
          lv_name = 'ETTCD_VERBS_TABTYPE'.
          ASSIGN lt_verbs TO <lt_tab>.
        WHEN 3.
          lv_name = 'ETTCD_VARS_TABTYPE'.
          ASSIGN lt_vars TO <lt_tab>.
        WHEN 4.
          lv_name = 'ETTCD_DP_TAB_TABTYPE'.
          ASSIGN lt_dp_tab TO <lt_tab>.
        WHEN 5.
          lv_name = 'ETTCD_DP_FOR_TABTYPE'.
          ASSIGN lt_dp_for TO <lt_tab>.
        WHEN 6.
          lv_name = 'ETTCD_DP_PRO_TABTYPE'.
          ASSIGN lt_dp_pro TO <lt_tab>.
        WHEN 7.
          lv_name = 'ETTCD_DP_FLD_TABTYPE'.
          ASSIGN lt_dp_fld TO <lt_tab>.
        WHEN 8.
          lv_name = 'ETTCD_SVARS_TABTYPE'.
          ASSIGN lt_svars TO <lt_tab>.
      ENDCASE.

      CALL FUNCTION 'SDIXML_DATA_TO_DOM'       "Ast generieren lassen
        EXPORTING
          name         = lv_name
          dataobject   = <lt_tab>
        IMPORTING
          data_as_dom  = li_element
        EXCEPTIONS
          illegal_name = 1
          OTHERS       = 2.

      IF sy-subrc <> 0.
        raise_download_exception(
              textid   = cx_ecatt_apl_util=>download_processing
              previous = ex_ecatt ).
      ENDIF.

* Ast in Hauptbaum haengen
      lv_rc = li_deep_tcd->append_child( li_element ).

      IF lv_rc <> 0.
        raise_download_exception(
              textid   = cx_ecatt_apl_util=>download_processing
              previous = ex_ecatt ).
      ENDIF.
      FREE li_element.
      UNASSIGN <lt_tab>.
    ENDDO.

    escape_control_data( ii_element = li_deep_tcd
      iv_tabname = 'ETTCD_VARS_TABTYPE'
      iv_node    = 'CB_INDEX' ).

    escape_control_data(
      ii_element = li_deep_tcd
      iv_tabname = 'ETTCD_VERBS_TABTYPE'
      iv_node    = 'NAME' ).

    FREE: lt_dp_tab, lt_dp_for, lt_dp_fld, lt_svars,
          lt_params, lt_vars,   lt_dp_pro, lt_verbs.

  ENDMETHOD.
  METHOD set_script_to_template.

    " Downport

    DATA:
      lt_text    TYPE etxml_line_tabtype,
      li_element TYPE REF TO if_ixml_element,
      lv_rc      TYPE sy-subrc.

    ecatt_script->get_script_text( CHANGING scripttext = lt_text ).

    mi_script_node = template_over_all->create_simple_element(
                        name = 'SCRIPT'
                        parent = root_node ).

    IF mi_script_node IS INITIAL.
      raise_download_exception(
            textid        = cx_ecatt_apl_util=>download_processing
            previous      = ex_ecatt
            called_method = 'CL_APL_ECATT_SCRIPT_DOWNLOAD->SET_SCRIPT_TO_TEMPLATE' ).
    ENDIF.

    CALL FUNCTION 'SDIXML_DATA_TO_DOM'
      EXPORTING
        name         = 'ETXML_LINE_TABTYPE'
        dataobject   = lt_text
      IMPORTING
        data_as_dom  = li_element
      CHANGING
        document     = template_over_all
      EXCEPTIONS
        illegal_name = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      raise_download_exception(
            textid        = cx_ecatt_apl_util=>download_processing
            previous      = ex_ecatt
            called_method = 'CL_APL_ECATT_SCRIPT_DOWNLOAD->SET_SCRIPT_TO_TEMPLATE' ).

    ENDIF.

    lv_rc = mi_script_node->append_child( li_element ).
    IF lv_rc <> 0.
      raise_download_exception(
            textid        = cx_ecatt_apl_util=>download_processing
            previous      = ex_ecatt
            called_method = 'CL_APL_ECATT_SCRIPT_DOWNLOAD->SET_SCRIPT_TO_TEMPLATE' ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_ecatt_download~get_xml_stream.

    rv_xml_stream = mv_xml_stream.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ECATT_SCRIPT_DOWNL implementation

*>>>>>>> ZCL_ABAPGIT_ECATT_SP_DOWNLOAD <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ecatt_sp_download=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ecatt_sp_download=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ECATT_SP_DOWNLOAD implementation.
*"* method's implementations
*include methods.
  METHOD download.

    " We inherit from CL_APL_ECATT_DOWNLOAD because CL_APL_ECATT_SP_DOWNLOAD
    " doesn't exist in 702

    " Downport

    load_help = im_load_help.
    typ = im_object_type.

    TRY.
        cl_apl_ecatt_object=>show_object(
          EXPORTING
            im_obj_type = im_object_type
            im_name     = im_object_name
            im_version  = im_object_version
          IMPORTING
            re_object   = ecatt_object ).
      CATCH cx_ecatt INTO ex_ecatt.
        RETURN.
    ENDTRY.

    set_attributes_to_template( ).

    set_sp_data_to_template( ).

    download_data( ).

  ENDMETHOD.
  METHOD download_data.

    " Downport

    mv_xml_stream = Lcl_abapgit_ecatt_helper=>download_data( template_over_all ).

  ENDMETHOD.
  METHOD set_sp_data_to_template.

    " downport

    DATA: li_dom                     TYPE REF TO if_ixml_document,
          li_start_profile_data_node TYPE REF TO if_ixml_element,
          li_element                 TYPE REF TO if_ixml_element,
          lv_sp_xml                  TYPE etxml_line_str,
          lo_ecatt_sp                TYPE REF TO object.

    FIELD-SYMBOLS: <lg_ecatt_object> TYPE data.

    li_start_profile_data_node = template_over_all->create_simple_element(
                                   name = 'START_PROFILE'
                                   parent = root_node ).

    ASSIGN ('ECATT_OBJECT') TO <lg_ecatt_object>.
    ASSERT sy-subrc = 0.

    lo_ecatt_sp = <lg_ecatt_object>.

    TRY.
        CALL METHOD lo_ecatt_sp->('GET_SP_ATTRIBUTES')
          IMPORTING
            e_sp_xml = lv_sp_xml.
      CATCH cx_ecatt_apl.
    ENDTRY.

    CALL FUNCTION 'SDIXML_XML_TO_DOM'
      EXPORTING
        xml      = lv_sp_xml
      IMPORTING
        document = li_dom.

    li_element = li_dom->get_root_element( ).
    li_start_profile_data_node->append_child( li_element ).

  ENDMETHOD.
  METHOD Lif_abapgit_ecatt_download~get_xml_stream.

    rv_xml_stream = mv_xml_stream.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ECATT_SP_DOWNLOAD implementation

*>>>>>>> ZCL_ABAPGIT_ECATT_SP_UPLOAD <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ecatt_sp_upload===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ecatt_sp_upload===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ECATT_SP_UPLOAD implementation.
*"* method's implementations
*include methods.
  METHOD get_ecatt_sp.

    " downport

    DATA: li_ixml               TYPE REF TO if_ixml,
          li_section            TYPE REF TO if_ixml_element,
          li_dom                TYPE REF TO if_ixml_document,
          li_root               TYPE REF TO if_ixml_node,
          lv_start_profile      TYPE etxml_line_str,
          lv_exception_occurred TYPE etonoff,
          lo_ecatt_sp           TYPE REF TO object.

    FIELD-SYMBOLS: <lg_ecatt_object> TYPE any.

    TRY.
        li_section = template_over_all->find_from_name_ns( 'START_PROFILE' ).

        IF NOT li_section IS INITIAL.
          li_ixml = cl_ixml=>create( ).
          li_dom  = li_ixml->create_document( ).
          li_root ?= li_section->clone( ).
          li_dom->append_child( li_root ).
          CALL FUNCTION 'SDIXML_DOM_TO_XML'
            EXPORTING
              document      = li_dom
            IMPORTING
              xml_as_string = lv_start_profile.

          ASSIGN ('ECATT_OBJECT') TO <lg_ecatt_object>.
          ASSERT sy-subrc = 0.

          lo_ecatt_sp = <lg_ecatt_object>.

          CALL METHOD lo_ecatt_sp->('SET_SP_ATTRIBUTES')
            EXPORTING
              i_sp_xml = lv_start_profile.

        ENDIF.
      CATCH cx_ecatt_apl.
        lv_exception_occurred = 'X'.
    ENDTRY.

    IF lv_exception_occurred = 'X'.
      raise_upload_exception( previous = exception_to_raise ).
    ENDIF.
  ENDMETHOD.
  METHOD upload.

    " We inherit from CL_APL_ECATT_UPLOAD because CL_APL_ECATT_SP_UPLOAD
    " doesn't exist in 702

    " Downport

    "26.03.2013

    DATA: lx_ecatt    TYPE REF TO cx_ecatt_apl,
          lv_exists   TYPE etonoff,
          lv_exc_occ  TYPE etonoff,
          ls_tadir    TYPE tadir,
          lo_ecatt_sp TYPE REF TO object.

    FIELD-SYMBOLS: <lg_ecatt_sp> TYPE any,
                   <lg_d_akh>    TYPE data,
                   <lg_i_akh>    TYPE data.

    TRY.
        ch_object-i_devclass = ch_object-d_devclass.

        ASSIGN COMPONENT 'D_AKH' OF STRUCTURE ch_object
               TO <lg_d_akh>. " doesn't exist in 702
        ASSIGN COMPONENT 'I_AKH' OF STRUCTURE ch_object
               TO <lg_i_akh>. " doesn't exist in 702
        IF <lg_d_akh> IS ASSIGNED AND <lg_i_akh> IS ASSIGNED.
          <lg_i_akh> = <lg_d_akh>.
        ENDIF.

        super->upload( CHANGING ch_object = ch_object ).

        upload_data_from_stream( ch_object-filename ).

      CATCH cx_ecatt_apl INTO lx_ecatt.
        IF template_over_all IS INITIAL.
          RAISE EXCEPTION lx_ecatt.
        ELSE.
          lv_exc_occ = 'X'.
        ENDIF.
    ENDTRY.

    TRY.
        CALL METHOD ('GET_ATTRIBUTES_FROM_DOM_NEW') " doesn't exist in 720
          CHANGING
            ch_object = ch_object.
      CATCH cx_ecatt_apl INTO lx_ecatt.
        lv_exc_occ = 'X'.
    ENDTRY.

    ASSIGN ecatt_object TO <lg_ecatt_sp>.
    ASSERT sy-subrc = 0.

    lo_ecatt_sp = <lg_ecatt_sp>.

    TRY.
        get_ecatt_sp( ).
      CATCH cx_ecatt_apl INTO lx_ecatt.
        lv_exc_occ = 'X'.
    ENDTRY.

    TRY.
        lv_exists = cl_apl_ecatt_object=>existence_check_object(
                      im_name               = ch_object-d_obj_name
                      im_version            = ch_object-d_obj_ver
                      im_obj_type           = ch_object-s_obj_type
                      im_exists_any_version = 'X' ).

        IF lv_exists = space.
          CALL METHOD lo_ecatt_sp->('SET_TADIR_FOR_NEW_OBJECT')
            EXPORTING
              im_tadir_for_new_object = tadir_preset.
        ENDIF.
      CATCH cx_ecatt.
        CLEAR lv_exists.
    ENDTRY.

    TRY.
        CALL METHOD lo_ecatt_sp->('SAVE')
          EXPORTING
            im_do_commit = 'X'.
      CATCH cx_ecatt_apl INTO lx_ecatt.
        lv_exc_occ = 'X'.
    ENDTRY.
* Devesh,C5129871  18.07.2011  Releasing enqueu after uploading
*begin
    TRY.
        ecatt_object->close_object( im_suppress_events = 'X' ).
      CATCH cx_ecatt_apl INTO lx_ecatt.
    ENDTRY.
*end
*     get devclass from existing object
    TRY.
        cl_apl_ecatt_object=>get_tadir_entry(
          EXPORTING im_obj_name = ch_object-d_obj_name
                    im_obj_type = ch_object-s_obj_type
          IMPORTING ex_tadir = ls_tadir ).

        ch_object-d_devclass = ls_tadir-devclass.

      CATCH cx_ecatt.
        CLEAR ls_tadir.
    ENDTRY.
    IF lv_exc_occ = 'X'.
      raise_upload_exception( previous = lx_ecatt ).
    ENDIF.

  ENDMETHOD.
  METHOD upload_data_from_stream.

    " Downport
    template_over_all = Lcl_abapgit_ecatt_helper=>upload_data_from_stream( mv_external_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_ecatt_upload~set_stream_for_upload.

    " downport from CL_APL_ECATT_START_PROFIL SET_STREAM_FOR_UPLOAD
    mv_external_xml = iv_xml.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ECATT_SP_UPLOAD implementation

*>>>>>>> ZCL_ABAPGIT_ECATT_SYSTEM_DOWNL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ecatt_system_downlccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ecatt_system_downlccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ECATT_SYSTEM_DOWNL implementation.
*"* method's implementations
*include methods.
  METHOD download.

    " Downport

    load_help = im_load_help.
    typ = im_object_type.

    TRY.
        cl_apl_ecatt_object=>show_object(
          EXPORTING
            im_obj_type = im_object_type
            im_name     = im_object_name
            im_version  = im_object_version
          IMPORTING
            re_object   = ecatt_object ).
      CATCH cx_ecatt INTO ex_ecatt.
        RETURN.
    ENDTRY.

    set_attributes_to_template( ).
    set_systems_data_to_template( ).
    download_data( ).

  ENDMETHOD.
  METHOD download_data.

    " Downport

    mv_xml_stream = Lcl_abapgit_ecatt_helper=>download_data( template_over_all ).

  ENDMETHOD.
  METHOD set_systems_data_to_template.

    DATA: lo_ecatt_systems TYPE REF TO cl_apl_ecatt_system_data,
          lt_sys_data      TYPE etsys_def_tabtype,
          ls_sys_data      TYPE etsys_def,
          li_item          TYPE REF TO if_ixml_element,
          li_sysdata_node  TYPE REF TO if_ixml_element.

    lo_ecatt_systems ?= ecatt_object.
    lt_sys_data = lo_ecatt_systems->get_system_data( ).

    li_sysdata_node = template_over_all->create_simple_element(
                        name = 'SYSTEMS_DATA'
                        parent = root_node ).

    etpar_node = template_over_all->create_simple_element(
                   name = 'ETSYS_DEF'
                   parent = li_sysdata_node ).

    LOOP AT lt_sys_data INTO ls_sys_data.

      CLEAR: ls_sys_data-sys_desc, ls_sys_data-instance.

      CALL FUNCTION 'SDIXML_DATA_TO_DOM'
        EXPORTING
          name         = 'item'
          dataobject   = ls_sys_data
        IMPORTING
          data_as_dom  = li_item
        CHANGING
          document     = template_over_all
        EXCEPTIONS
          illegal_name = 1
          OTHERS       = 2.
      ASSERT sy-subrc = 0.

      etpar_node->append_child( li_item ).

    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_ecatt_download~get_xml_stream.

    rv_xml_stream = mv_xml_stream.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ECATT_SYSTEM_DOWNL implementation

*>>>>>>> ZCL_ABAPGIT_ECATT_SYSTEM_UPL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ecatt_system_upl==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ecatt_system_upl==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ECATT_SYSTEM_UPL implementation.
*"* method's implementations
*include methods.
  METHOD upload_data_from_stream.

    " Downport
    template_over_all = Lcl_abapgit_ecatt_helper=>upload_data_from_stream( mv_external_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_ecatt_upload~set_stream_for_upload.

    " downport from CL_APL_ECATT_SYSTEMS_UPLOAD SET_STREAM_FOR_UPLOAD
    mv_external_xml = iv_xml.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ECATT_SYSTEM_UPL implementation

*>>>>>>> ZCL_ABAPGIT_ECATT_VAL_OBJ_DOWN <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ecatt_val_obj_downccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ecatt_val_obj_downccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ECATT_VAL_OBJ_DOWN implementation.
*"* method's implementations
*include methods.
  METHOD download.

    " We inherit from CL_APL_ECATT_DOWNLOAD because CL_APL_ECATT_VO_DOWNLOAD
    " doesn't exist in 702

    " Downport

    DATA: lv_partyp   TYPE string,
          lo_ecatt_vo TYPE REF TO object.

    FIELD-SYMBOLS: <lg_ecatt_vo> TYPE any,
                   <lo_params>   TYPE REF TO cl_apl_ecatt_params.

    load_help = im_load_help.
    typ = im_object_type.

    TRY.
        cl_apl_ecatt_object=>show_object(
          EXPORTING
            im_obj_type = im_object_type
            im_name     = im_object_name
            im_version  = im_object_version
          IMPORTING
            re_object   = ecatt_object ).
      CATCH cx_ecatt INTO ex_ecatt.
        RETURN.
    ENDTRY.

    lv_partyp = cl_apl_ecatt_const=>params_type_par.


    ASSIGN ('ECATT_OBJECT') TO <lg_ecatt_vo>.
    ASSERT sy-subrc = 0.

    lo_ecatt_vo = <lg_ecatt_vo>.

    set_attributes_to_template( ).
    set_ecatt_impl_detail( ).
    set_ecatt_flags( ).
    set_business_msgs( ).

    ASSIGN lo_ecatt_vo->('PARAMS')
           TO <lo_params>.
    ASSERT sy-subrc = 0.

    get_general_params_data( im_params = <lo_params>
                             im_ptyp   = lv_partyp ).
    LOOP AT parm INTO wa_parm.
      set_general_params_data_to_dom( ).
      IF NOT wa_parm-val_type IS INITIAL.
        set_deep_stru_to_dom( <lo_params> ).
        set_deep_data_to_dom( im_params = <lo_params>
                              im_pindex = wa_parm-pindex ).
      ENDIF.
    ENDLOOP.

    set_variants_to_dom( <lo_params> ).

    download_data( ).

  ENDMETHOD.
  METHOD download_data.

    " Downport

    mv_xml_stream = Lcl_abapgit_ecatt_helper=>download_data( template_over_all ).

  ENDMETHOD.
  METHOD set_business_msgs.

    DATA:
      lt_buss_msg_ref   TYPE Lif_abapgit_ecatt=>ty_bus_msgs,
      li_element        TYPE REF TO if_ixml_element,
      li_insert_objects TYPE REF TO if_ixml_element,
      lo_ecatt_vo       TYPE REF TO object.

    FIELD-SYMBOLS: <lg_ecatt_vo> TYPE any.

    ASSIGN ('ECATT_OBJECT') TO <lg_ecatt_vo>.
    ASSERT sy-subrc = 0.

    lo_ecatt_vo = <lg_ecatt_vo>.

    mi_objects_node = template_over_all->create_simple_element(
                                           name   = 'BUSINESS_MESSAGES'
                                           parent = root_node ).

    CALL METHOD lo_ecatt_vo->('GET_BUSSINESS_MSG')
      IMPORTING
        ex_buss_msg_ref = lt_buss_msg_ref.

    CALL FUNCTION 'SDIXML_DATA_TO_DOM'
      EXPORTING
        name         = 'ETVO_MSG'
        dataobject   = lt_buss_msg_ref
      IMPORTING
        data_as_dom  = li_element
      CHANGING
        document     = template_over_all
      EXCEPTIONS
        illegal_name = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    li_insert_objects ?= template_over_all->find_from_name( 'BUSINESS_MESSAGES' ).

    li_insert_objects->append_child( li_element ).

  ENDMETHOD.
  METHOD set_ecatt_flags.

    DATA:
      lv_invert_validation TYPE Lif_abapgit_ecatt=>ty_invert_validation,
      lv_error_prio        TYPE Lif_abapgit_ecatt=>ty_error_prio,
      li_element           TYPE REF TO if_ixml_element,
      li_insert_objects    TYPE REF TO if_ixml_element,
      lo_ecatt_vo          TYPE REF TO object.

    FIELD-SYMBOLS: <lg_ecatt_vo> TYPE any.

    mi_objects_node = template_over_all->create_simple_element(
                                           name   = 'VO_FLAGS'
                                           parent = root_node ).

    ASSIGN ('ECATT_OBJECT') TO <lg_ecatt_vo>.
    ASSERT sy-subrc = 0.

    lo_ecatt_vo = <lg_ecatt_vo>.

    CALL METHOD lo_ecatt_vo->('GET_INVERT_VALIDATION_FLAG')
      RECEIVING
        re_invert_validation = lv_invert_validation.

    CALL FUNCTION 'SDIXML_DATA_TO_DOM'
      EXPORTING
        name         = 'INVERT_VALIDATION'
        dataobject   = lv_invert_validation
      IMPORTING
        data_as_dom  = li_element
      CHANGING
        document     = template_over_all
      EXCEPTIONS
        illegal_name = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    li_insert_objects ?= template_over_all->find_from_name( 'VO_FLAGS' ).

    li_insert_objects->append_child( li_element ).

    CALL METHOD lo_ecatt_vo->('GET_ERROR_PRIORITY')
      RECEIVING
        re_error_prio = lv_error_prio.

    CALL FUNCTION 'SDIXML_DATA_TO_DOM'
      EXPORTING
        name         = 'ERROR_PRIORITY'
        dataobject   = lv_error_prio
      IMPORTING
        data_as_dom  = li_element
      CHANGING
        document     = template_over_all
      EXCEPTIONS
        illegal_name = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    li_insert_objects = template_over_all->find_from_name( 'VO_FLAGS' ).

    li_insert_objects->append_child( li_element ).

  ENDMETHOD.
  METHOD set_ecatt_impl_detail.

    DATA:
      ls_impl_details   TYPE Lif_abapgit_ecatt=>ty_impl_det,
      li_element        TYPE REF TO if_ixml_element,
      li_insert_objects TYPE REF TO if_ixml_element,
      lo_ecatt_vo       TYPE REF TO object.

    FIELD-SYMBOLS: <lg_ecatt_vo> TYPE any.

    mi_objects_node = template_over_all->create_simple_element(
                                           name   = 'IMPL_DETAILS'
                                           parent = root_node ).

    ASSIGN ('ECATT_OBJECT') TO <lg_ecatt_vo>.
    ASSERT sy-subrc = 0.

    lo_ecatt_vo = <lg_ecatt_vo>.

    CALL METHOD lo_ecatt_vo->('GET_IMPL_DETAILS')
      RECEIVING
        re_impl_details = ls_impl_details.

    CALL FUNCTION 'SDIXML_DATA_TO_DOM'
      EXPORTING
        name         = 'IMPL_DET'
        dataobject   = ls_impl_details
      IMPORTING
        data_as_dom  = li_element
      CHANGING
        document     = template_over_all
      EXCEPTIONS
        illegal_name = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    li_insert_objects = template_over_all->find_from_name( 'IMPL_DETAILS' ).

    li_insert_objects->append_child( li_element ).

  ENDMETHOD.
  METHOD Lif_abapgit_ecatt_download~get_xml_stream.

    rv_xml_stream = mv_xml_stream.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ECATT_VAL_OBJ_DOWN implementation

*>>>>>>> ZCL_ABAPGIT_ECATT_VAL_OBJ_UPL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ecatt_val_obj_upl=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ecatt_val_obj_upl=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ECATT_VAL_OBJ_UPL implementation.
*"* method's implementations
*include methods.
  METHOD get_business_msgs_from_dom.

    " downport from CL_APL_ECATT_VO_UPLOAD

    DATA: li_section            TYPE REF TO if_ixml_element,
          lt_buss_msg_ref       TYPE Lif_abapgit_ecatt=>ty_bus_msgs,
          lv_exception_occurred TYPE etonoff,
          lo_ecatt_vo           TYPE REF TO object.

    FIELD-SYMBOLS: <lg_ecatt_vo> TYPE any.

    li_section = template_over_all->find_from_name_ns( 'ETVO_MSG' ).

    IF NOT li_section IS INITIAL.
      CALL FUNCTION 'SDIXML_DOM_TO_DATA'
        EXPORTING
          data_as_dom    = li_section
        IMPORTING
          dataobject     = lt_buss_msg_ref
        EXCEPTIONS
          illegal_object = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
        CLEAR lt_buss_msg_ref.
      ENDIF.
    ENDIF.

    ASSIGN ('ECATT_OBJECT') TO <lg_ecatt_vo>.
    ASSERT sy-subrc = 0.

    lo_ecatt_vo = <lg_ecatt_vo>.

    TRY.
        CALL METHOD lo_ecatt_vo->('SET_BUSSINESS_MSG')
          EXPORTING
            im_buss_msg_ref = lt_buss_msg_ref.
      CATCH cx_ecatt_apl INTO exception_to_raise.
        lv_exception_occurred = 'X'.
    ENDTRY.

    IF lv_exception_occurred = 'X'.
      raise_upload_exception( previous = exception_to_raise ).
    ENDIF.

  ENDMETHOD.
  METHOD get_impl_detail_from_dom.

    " downport from CL_APL_ECATT_VO_UPLOAD

    DATA: li_section            TYPE REF TO if_ixml_element,
          ls_impl_details       TYPE Lif_abapgit_ecatt=>ty_impl_det,
          lv_exception_occurred TYPE etonoff,
          lo_ecatt_vo           TYPE REF TO object.

    FIELD-SYMBOLS: <lg_ecatt_vo> TYPE any.

    li_section = template_over_all->find_from_name_ns( 'IMPL_DET' ).

    IF NOT li_section IS INITIAL.
      CALL FUNCTION 'SDIXML_DOM_TO_DATA'
        EXPORTING
          data_as_dom    = li_section
        IMPORTING
          dataobject     = ls_impl_details
        EXCEPTIONS
          illegal_object = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
        CLEAR ls_impl_details.
      ENDIF.
    ENDIF.

    ASSIGN ('ECATT_OBJECT') TO <lg_ecatt_vo>.
    ASSERT sy-subrc = 0.

    lo_ecatt_vo = <lg_ecatt_vo>.

    TRY.
        CALL METHOD lo_ecatt_vo->('SET_IMPL_DETAILS')
          EXPORTING
            im_impl_details = ls_impl_details.
      CATCH cx_ecatt_apl INTO exception_to_raise.
        lv_exception_occurred = 'X'.
    ENDTRY.

    IF lv_exception_occurred = 'X'.
      raise_upload_exception( previous = exception_to_raise ).
    ENDIF.

  ENDMETHOD.
  METHOD get_vo_flags_from_dom.

    " downport from CL_APL_ECATT_VO_UPLOAD

    DATA: li_section            TYPE REF TO if_ixml_element,
          lv_error_prio         TYPE Lif_abapgit_ecatt=>ty_error_prio,
          lv_invert_validation  TYPE Lif_abapgit_ecatt=>ty_invert_validation,
          lv_exception_occurred TYPE etonoff,
          lo_ecatt_vo           TYPE REF TO object.

    FIELD-SYMBOLS: <lg_ecatt_vo> TYPE any.

    li_section = template_over_all->find_from_name_ns( 'INVERT_VALIDATION' ).

    IF NOT li_section IS INITIAL.
      CALL FUNCTION 'SDIXML_DOM_TO_DATA'
        EXPORTING
          data_as_dom    = li_section
        IMPORTING
          dataobject     = lv_invert_validation
        EXCEPTIONS
          illegal_object = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
        CLEAR lv_invert_validation.
      ENDIF.
    ENDIF.

    ASSIGN ('ECATT_OBJECT') TO <lg_ecatt_vo>.
    ASSERT sy-subrc = 0.

    lo_ecatt_vo = <lg_ecatt_vo>.

    TRY.
        CALL METHOD lo_ecatt_vo->('SET_INVERT_VALIDATION_FLAG')
          EXPORTING
            im_invert_validation = lv_invert_validation.

      CATCH cx_ecatt_apl INTO exception_to_raise.
        lv_exception_occurred = 'X'.
    ENDTRY.

    li_section = template_over_all->find_from_name_ns( 'ERROR_PRIORITY' ).

    IF NOT li_section IS INITIAL.
      CALL FUNCTION 'SDIXML_DOM_TO_DATA'
        EXPORTING
          data_as_dom    = li_section
        IMPORTING
          dataobject     = lv_error_prio
        EXCEPTIONS
          illegal_object = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
        CLEAR lv_invert_validation.
      ENDIF.
    ENDIF.

    TRY.
        CALL METHOD lo_ecatt_vo->('SET_ERROR_PRIORITY')
          EXPORTING
            im_error_prio = lv_error_prio.
      CATCH cx_ecatt_apl INTO exception_to_raise.
        lv_exception_occurred = 'X'.
    ENDTRY.

    IF lv_exception_occurred = 'X'.
      raise_upload_exception( previous = exception_to_raise ).
    ENDIF.

  ENDMETHOD.
  METHOD upload.

    " We inherit from CL_APL_ECATT_UPLOAD because CL_APL_ECATT_VO_UPLOAD
    " doesn't exist in 702

    " downport from CL_APL_ECATT_VO_UPLOAD

    DATA: lx_ex       TYPE REF TO cx_ecatt_apl,
          lv_exists   TYPE etonoff,
          lv_exc_occ  TYPE etonoff,
          ls_tadir    TYPE tadir,
          lo_ecatt_vo TYPE REF TO object,
          lo_params   TYPE REF TO cl_apl_ecatt_params.

    FIELD-SYMBOLS: <lg_ecatt_vo> TYPE any,
                   <lg_params>   TYPE data,
                   <lg_d_akh>    TYPE data,
                   <lg_i_akh>    TYPE data.

    TRY.
        ch_object-i_devclass = ch_object-d_devclass.

        ASSIGN COMPONENT 'D_AKH' OF STRUCTURE ch_object
               TO <lg_d_akh>. " doesn't exist in 702
        ASSIGN COMPONENT 'I_AKH' OF STRUCTURE ch_object
               TO <lg_i_akh>. " doesn't exist in 702
        IF <lg_d_akh> IS ASSIGNED AND <lg_i_akh> IS ASSIGNED.
          <lg_i_akh> = <lg_d_akh>.
        ENDIF.

        super->upload( CHANGING ch_object = ch_object ).

        upload_data_from_stream( ch_object-filename ).
      CATCH cx_ecatt_apl INTO lx_ex.
        IF template_over_all IS INITIAL.
          RAISE EXCEPTION lx_ex.
        ELSE.
          lv_exc_occ = 'X'.
        ENDIF.
    ENDTRY.

    TRY.
        CALL METHOD ('GET_ATTRIBUTES_FROM_DOM_NEW') " doesn't exit in 702
          CHANGING
            ch_object = ch_object.
      CATCH cx_ecatt_apl INTO lx_ex.
        lv_exc_occ = 'X'.
    ENDTRY.

    ASSIGN ('ECATT_OBJECT') TO <lg_ecatt_vo>.
    ASSERT sy-subrc = 0.

    lo_ecatt_vo = <lg_ecatt_vo>.

    ASSIGN lo_ecatt_vo->('PARAMS') TO <lg_params>.
    ASSERT sy-subrc = 0.

    lo_params = <lg_params>.

    TRY.
        get_impl_detail_from_dom( ).
      CATCH cx_ecatt_apl INTO lx_ex.
        lv_exc_occ = 'X'.
    ENDTRY.

    TRY.
        get_vo_flags_from_dom( ).
      CATCH cx_ecatt_apl INTO lx_ex.
        lv_exc_occ = 'X'.
    ENDTRY.

    TRY.
        get_business_msgs_from_dom( ).
      CATCH cx_ecatt_apl INTO lx_ex.
        lv_exc_occ = 'X'.
    ENDTRY.

    TRY.
        CALL METHOD ('GET_PARAMS_FROM_DOM_NEW') " doesn't exist in 702
          EXPORTING
            im_params = lo_params.
      CATCH cx_ecatt_apl INTO lx_ex.
        lv_exc_occ = 'X'.
    ENDTRY.

    TRY.
        get_variants_from_dom( lo_params ).
      CATCH cx_ecatt_apl INTO lx_ex.
        lv_exc_occ = 'X'.
    ENDTRY.

    TRY.
        lv_exists = cl_apl_ecatt_object=>existence_check_object(
                im_name               = ch_object-d_obj_name
                im_version            = ch_object-d_obj_ver
                im_obj_type           = ch_object-s_obj_type
                im_exists_any_version = 'X' ).

        IF lv_exists = space.
          CALL METHOD lo_ecatt_vo->('SET_TADIR_FOR_NEW_OBJECT')
            EXPORTING
              im_tadir_for_new_object = tadir_preset.
        ENDIF.
      CATCH cx_ecatt.
        CLEAR lv_exists.
    ENDTRY.

    TRY.
        CALL METHOD lo_ecatt_vo->('SAVE')
          EXPORTING
            im_do_commit = 'X'.
      CATCH cx_ecatt_apl INTO lx_ex.
        lv_exc_occ = 'X'.
    ENDTRY.

*     get devclass from existing object
    TRY.
        cl_apl_ecatt_object=>get_tadir_entry(
          EXPORTING im_obj_name = ch_object-d_obj_name
                    im_obj_type = ch_object-s_obj_type
          IMPORTING ex_tadir = ls_tadir ).

        ch_object-d_devclass = ls_tadir-devclass.

      CATCH cx_ecatt.
        CLEAR ls_tadir.
    ENDTRY.
    IF lv_exc_occ = 'X'.
      raise_upload_exception( previous = lx_ex ).
    ENDIF.

  ENDMETHOD.
  METHOD upload_data_from_stream.

    " Downport
    template_over_all = Lcl_abapgit_ecatt_helper=>upload_data_from_stream( mv_external_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_ecatt_upload~set_stream_for_upload.

    " downport from CL_ABAPGIT_ECATT_DATA_UPLOAD SET_STREAM_FOR_UPLOAD
    mv_external_xml = iv_xml.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ECATT_VAL_OBJ_UPL implementation

*>>>>>>> ZCL_ABAPGIT_EXCEPTION_VIEWER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_exception_viewer==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_exception_viewer==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_EXCEPTION_VIEWER implementation.
*"* method's implementations
*include methods.
  METHOD add_row.

    DATA: lo_row TYPE REF TO cl_salv_form_layout_flow.

    lo_row = io_grid->add_row( ).

    lo_row->create_label( position = 1
                          text     = iv_col_1 ).

    lo_row->create_label( position = 2
                          text     = iv_col_2 ).

  ENDMETHOD.
  METHOD build_top_of_list.

    DATA: lo_grid TYPE REF TO cl_salv_form_layout_grid.

    CREATE OBJECT lo_grid
      EXPORTING
        columns = 2.

    add_row( io_grid  = lo_grid
             iv_col_1 = 'Main program:'
             iv_col_2 = is_top_of_stack-mainprogram ).

    add_row( io_grid  = lo_grid
             iv_col_1 = 'Include name:'
             iv_col_2 = is_top_of_stack-include ).

    add_row( io_grid  = lo_grid
             iv_col_1 = 'Source line'
             iv_col_2 = |{ is_top_of_stack-line }| ).

    ro_form = lo_grid.

  ENDMETHOD.
  METHOD constructor.

    mx_error = ix_error.
    mt_callstack = mx_error->mt_callstack.

  ENDMETHOD.
  METHOD extract_classname.

    rv_classname = substring_before( val   = iv_mainprogram
                                     regex = '=*CP$' ).

  ENDMETHOD.
  METHOD get_top_of_callstack.

    READ TABLE mt_callstack INDEX 1
                            INTO rs_top_of_callstack.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Callstack is empty| ).
    ENDIF.

  ENDMETHOD.
  METHOD goto_message.

    DATA: lt_bdcdata TYPE STANDARD TABLE OF bdcdata,
          ls_bdcdata LIKE LINE OF lt_bdcdata.

    ls_bdcdata-program  = 'SAPLWBMESSAGES'.
    ls_bdcdata-dynpro   = '0100'.
    ls_bdcdata-dynbegin = abap_true.
    INSERT ls_bdcdata INTO TABLE lt_bdcdata.

    CLEAR: ls_bdcdata.
    ls_bdcdata-fnam = 'RSDAG-ARBGB'.
    ls_bdcdata-fval = mx_error->if_t100_message~t100key-msgid.
    INSERT ls_bdcdata INTO TABLE lt_bdcdata.

    CLEAR: ls_bdcdata.
    ls_bdcdata-fnam = 'MSG_NUMMER'.
    ls_bdcdata-fval = mx_error->if_t100_message~t100key-msgno.
    INSERT ls_bdcdata INTO TABLE lt_bdcdata.

    CLEAR: ls_bdcdata.
    ls_bdcdata-fnam = 'RSDAG-MSGFLAG'.
    ls_bdcdata-fval = 'X'.
    INSERT ls_bdcdata INTO TABLE lt_bdcdata.

    CLEAR: ls_bdcdata.
    ls_bdcdata-fnam = 'BDC_OKCODE'.
    ls_bdcdata-fval = '=WB_DISPLAY'.
    INSERT ls_bdcdata INTO TABLE lt_bdcdata.

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      STARTING NEW TASK 'GIT'
      EXPORTING
        tcode                   = 'SE91'
        mode_val                = 'E'
      TABLES
        using_tab               = lt_bdcdata
      EXCEPTIONS
        call_transaction_denied = 1
        tcode_invalid           = 2
        OTHERS                  = 3.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD goto_source.

    goto_source_code( get_top_of_callstack( ) ).

  ENDMETHOD.
  METHOD goto_source_code.

    CONSTANTS:
      BEGIN OF lc_obj_type,
        class   TYPE trobjtype VALUE `CLAS`,
        program TYPE trobjtype VALUE `PROG`,
      END OF lc_obj_type.

    DATA:
      ls_item      TYPE Lif_abapgit_definitions=>ty_item,
      ls_sub_item  TYPE Lif_abapgit_definitions=>ty_item,
      lv_classname LIKE ls_item-obj_name.

    " you should remember that we distinct two cases
    " 1) we navigate to a global class
    " 2) we navigate to a program
    " the latter one is the default case

    lv_classname = extract_classname( is_callstack-mainprogram ).

    IF lv_classname IS NOT INITIAL.
      ls_item-obj_name = lv_classname.
      ls_item-obj_type = lc_obj_type-class.
    ELSE.
      ls_item-obj_name = is_callstack-mainprogram.
      ls_item-obj_type = lc_obj_type-program.
    ENDIF.

    ls_sub_item-obj_name = is_callstack-include.
    ls_sub_item-obj_type = lc_obj_type-program.

    Lcl_abapgit_objects=>jump(
      is_item        = ls_item
      is_sub_item    = ls_sub_item
      iv_line_number = is_callstack-line ).

  ENDMETHOD.
  METHOD on_double_click.

    DATA: lx_error TYPE REF TO Lcx_abapgit_exception.
    FIELD-SYMBOLS: <ls_callstack> TYPE abap_callstack_line.

    READ TABLE mt_callstack ASSIGNING <ls_callstack>
                            INDEX row.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    TRY.
        goto_source_code( <ls_callstack> ).

      CATCH Lcx_abapgit_exception INTO lx_error.
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.
  METHOD set_text.

    DATA: lo_column      TYPE REF TO cl_salv_column,
          lv_short_text  TYPE scrtext_s,
          lv_medium_text TYPE scrtext_m,
          lv_long_text   TYPE scrtext_l.

    lo_column = io_columns->get_column( iv_column ).

    lv_short_text  = iv_text.
    lv_medium_text = iv_text.
    lv_long_text   = iv_text.

    lo_column->set_short_text( lv_short_text ).
    lo_column->set_medium_text( lv_medium_text ).
    lo_column->set_long_text( lv_long_text ).

  ENDMETHOD.
  METHOD show_callstack.

    DATA: lx_error   TYPE REF TO cx_static_check,
          lo_event   TYPE REF TO cl_salv_events_table,
          lo_columns TYPE REF TO cl_salv_columns_table,
          lo_alv     TYPE REF TO cl_salv_table.
    DATA ls_position TYPE Lif_abapgit_popups=>ty_popup_position.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = mt_callstack ).

        lo_alv->get_columns( )->set_optimize( ).

        lo_alv->set_top_of_list( build_top_of_list( get_top_of_callstack( ) ) ).

        ls_position = Lcl_abapgit_popups=>center(
          iv_width  = 150
          iv_height = 25 ).

        lo_alv->set_screen_popup( start_column = ls_position-start_column
                                  end_column   = ls_position-end_column
                                  start_line   = ls_position-start_row
                                  end_line     = ls_position-end_row ).

        lo_event = lo_alv->get_event( ).

        lo_columns = lo_alv->get_columns( ).

        set_text( io_columns = lo_columns
                  iv_column  = |LINE|
                  iv_text    = |Line| ).

        set_text( io_columns = lo_columns
                  iv_column  = |LINE|
                  iv_text    = |Line| ).

        set_text( io_columns = lo_columns
                  iv_column  = |BLOCKTYPE|
                  iv_text    = |Event Type| ).

        set_text( io_columns = lo_columns
                  iv_column  = |BLOCKNAME|
                  iv_text    = |Event| ).

        set_text( io_columns = lo_columns
                  iv_column  = |FLAG_SYSTEM|
                  iv_text    = |System| ).

        SET HANDLER on_double_click FOR lo_event.

        lo_alv->display( ).

      CATCH cx_static_check INTO lx_error.
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_EXCEPTION_VIEWER implementation

*>>>>>>> ZCL_ABAPGIT_FUNCTION_MODULE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_function_module===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_function_module===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_FUNCTION_MODULE implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_function_module~function_exists.

    DATA: lv_function_module_name TYPE c LENGTH 30.

    lv_function_module_name = iv_function_module_name.

    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname           = lv_function_module_name
      EXCEPTIONS
        function_not_exist = 1
        OTHERS             = 2.
    rv_exists = boolc( sy-subrc = 0 ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_FUNCTION_MODULE implementation

*>>>>>>> ZCL_ABAPGIT_GIT_BRANCH_LIST <<<<<<<*

*"* macro definitions
*include zcl_abapgit_git_branch_list===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_git_branch_list===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_git_branch_list===ccau.
*CLASS SHRITEFUH64VYIPO5I47WOOA5VJASM DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_branch_list DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5VJASM.




class LCL_ABAPGIT_GIT_BRANCH_LIST implementation.
*"* method's implementations
*include methods.
  METHOD complete_heads_branch_name.
    IF iv_branch_name CP Lif_abapgit_git_definitions=>c_git_branch-heads.
      rv_name = iv_branch_name.
    ELSE.
      rv_name = Lif_abapgit_git_definitions=>c_git_branch-heads_prefix && iv_branch_name.
    ENDIF.
  ENDMETHOD.
  METHOD constructor.

    parse_branch_list(
      EXPORTING
        iv_data        = iv_data
      IMPORTING
        et_list        = mt_branches
        ev_head_symref = mv_head_symref ).

  ENDMETHOD.
  METHOD find_by_name.

    IF iv_branch_name IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Branch name empty' ).
    ENDIF.

    IF iv_branch_name CP Lif_abapgit_git_definitions=>c_git_branch-tags.
      rs_branch = find_tag_by_name( iv_branch_name ).
    ELSE.

      READ TABLE mt_branches INTO rs_branch
        WITH TABLE KEY name_key
        COMPONENTS name = iv_branch_name.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Branch { get_display_name( iv_branch_name )
          } not found. Use 'Branch' > 'Switch' to select a different branch| ).
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD find_tag_by_name.

    READ TABLE mt_branches INTO rs_branch
        WITH TABLE KEY name_key
        COMPONENTS name = Lcl_abapgit_git_tag=>add_peel( iv_branch_name ).
    IF sy-subrc <> 0.

      READ TABLE mt_branches INTO rs_branch
        WITH TABLE KEY name_key
        COMPONENTS name = iv_branch_name.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'Branch not found' ).
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD get_all.

    rt_branches = mt_branches.

  ENDMETHOD.
  METHOD get_branches_only.
