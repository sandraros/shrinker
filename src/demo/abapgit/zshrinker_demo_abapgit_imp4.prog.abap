********************************************************************************
*
* LICENSE and NOTICE
*
* See include program ZSHRINKER_DEMO_ABAPGIT_LICENSE
*
********************************************************************************

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
endclass. "ZCL_ABAPGIT_GUI_JUMPER implementation

*>>>>>>> ZCL_ABAPGIT_ADT_LINK <<<<<<<*

*"* macro definitions
*include zcl_abapgit_adt_link==========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_adt_link==========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ADT_LINK implementation.
*"* method's implementations
*include methods.
  METHOD generate.

    DATA: lv_adt_link       TYPE string.
    DATA: lo_adt_uri_mapper TYPE REF TO object.
    DATA: lo_adt_objref     TYPE REF TO object.
    DATA: lo_adt_sub_objref TYPE REF TO object.
    DATA: lv_program        TYPE progname.
    DATA: lv_include        TYPE progname.

    FIELD-SYMBOLS: <lv_uri> TYPE string.

    get_adt_objects_and_names(
      EXPORTING
        iv_obj_name       = iv_obj_name
        iv_obj_type       = iv_obj_type
      IMPORTING
        eo_adt_uri_mapper = lo_adt_uri_mapper
        eo_adt_objectref  = lo_adt_objref
        ev_program        = lv_program
        ev_include        = lv_include ).

    TRY.
        IF iv_sub_obj_name IS NOT INITIAL.

          IF ( lv_program <> iv_obj_name AND lv_include IS INITIAL ) OR
             ( lv_program = lv_include AND iv_sub_obj_name IS NOT INITIAL ).
            lv_include = iv_sub_obj_name.
          ENDIF.

          CALL METHOD lo_adt_uri_mapper->('IF_ADT_URI_MAPPER~MAP_INCLUDE_TO_OBJREF')
            EXPORTING
              program     = lv_program
              include     = lv_include
              line        = iv_line_number
              line_offset = 0
              end_line    = iv_line_number
              end_offset  = 1
            RECEIVING
              result      = lo_adt_sub_objref.
          IF lo_adt_sub_objref IS NOT INITIAL.
            lo_adt_objref = lo_adt_sub_objref.
          ENDIF.

        ENDIF.

        ASSIGN ('LO_ADT_OBJREF->REF_DATA-URI') TO <lv_uri>.
        ASSERT sy-subrc = 0.

        CONCATENATE 'adt://' sy-sysid <lv_uri> INTO lv_adt_link.

        rv_result = lv_adt_link.

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( 'ADT Jump Error' ).
    ENDTRY.

  ENDMETHOD.
  METHOD get_adt_objects_and_names.

    DATA lv_obj_type       TYPE trobjtype.
    DATA lv_obj_name       TYPE trobj_name.
    DATA lo_object         TYPE REF TO cl_wb_object.
    DATA lo_adt            TYPE REF TO object.

    FIELD-SYMBOLS <lv_uri> TYPE string.

    lv_obj_name = iv_obj_name.
    lv_obj_type = iv_obj_type.

    TRY.
        cl_wb_object=>create_from_transport_key(
          EXPORTING
            p_object    = lv_obj_type
            p_obj_name  = lv_obj_name
          RECEIVING
            p_wb_object = lo_object
          EXCEPTIONS
            OTHERS      = 1 ).
        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise( 'ADT Jump Error' ).
        ENDIF.

        CALL METHOD ('CL_ADT_TOOLS_CORE_FACTORY')=>('GET_INSTANCE')
          RECEIVING
            result = lo_adt.

        IF is_adt_jump_possible( io_object = lo_object
                                 io_adt    = lo_adt ) = abap_false.
          Lcx_abapgit_exception=>raise( 'ADT Jump Error' ).
        ENDIF.

        CALL METHOD lo_adt->('IF_ADT_TOOLS_CORE_FACTORY~GET_URI_MAPPER')
          RECEIVING
            result = eo_adt_uri_mapper.

        CALL METHOD eo_adt_uri_mapper->('IF_ADT_URI_MAPPER~MAP_WB_OBJECT_TO_OBJREF')
          EXPORTING
            wb_object = lo_object
          RECEIVING
            result    = eo_adt_objectref.

        ASSIGN ('EO_ADT_OBJECTREF->REF_DATA-URI') TO <lv_uri>.
        ASSERT sy-subrc = 0.

        CALL METHOD eo_adt_uri_mapper->('IF_ADT_URI_MAPPER~MAP_OBJREF_TO_INCLUDE')
          EXPORTING
            uri     = <lv_uri>
          IMPORTING
            program = ev_program
            include = ev_include.

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( 'ADT Jump Error' ).
    ENDTRY.

  ENDMETHOD.
  METHOD is_adt_jump_possible.

    DATA: lo_wb_request         TYPE REF TO cl_wb_request,
          lo_adt_uri_mapper_vit TYPE REF TO object,
          lv_vit_wb_request     TYPE abap_bool.

    cl_wb_request=>create_from_object_ref(
      EXPORTING
        p_wb_object       = io_object
      RECEIVING
        p_wb_request      = lo_wb_request
      EXCEPTIONS
        illegal_operation = 1
        cancelled         = 2
        OTHERS            = 3 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'ADT Jump Error' ).
    ENDIF.

    TRY.
        CALL METHOD io_adt->('IF_ADT_TOOLS_CORE_FACTORY~GET_URI_MAPPER_VIT')
          RECEIVING
            result = lo_adt_uri_mapper_vit.

        CALL METHOD lo_adt_uri_mapper_vit->('IF_ADT_URI_MAPPER_VIT~IS_VIT_WB_REQUEST')
          EXPORTING
            wb_request = lo_wb_request
          RECEIVING
            result     = lv_vit_wb_request.

        rv_is_adt_jump_possible = boolc( NOT lv_vit_wb_request = abap_true ).

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( 'ADT Jump Error' ).
    ENDTRY.

  ENDMETHOD.
  METHOD jump.

    DATA lv_adt_link TYPE string.

    TRY.
        lv_adt_link = generate(
          iv_obj_name     = iv_obj_name
          iv_obj_type     = iv_obj_type
          iv_sub_obj_name = iv_sub_obj_name
          iv_line_number  = iv_line_number ).

        Lcl_abapgit_ui_factory=>get_frontend_services( )->execute( iv_document = lv_adt_link ).

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( 'ADT Jump Error' ).
    ENDTRY.

  ENDMETHOD.
  METHOD link_transport.
* call to CL_CTS_ADT_TM_URI_BUILDER=>CREATE_ADT_URI replaced with logic that works on all systems,
    rv_link = |adt://{ sy-sysid }/sap/bc/adt/cts/transportrequests/{ iv_transport }|.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ADT_LINK implementation

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
*CLASS SHRIS5ZPAUXVKEPN5HWETLLAS54BTU DEFINITION DEFERRED.
*CLASS zcl_abapgit_oo_serializer DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLAS54BTU.




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

*>>>>>>> ZCL_ABAPGIT_FIELD_RULES <<<<<<<*

*"* macro definitions
*include zcl_abapgit_field_rules=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_field_rules=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_FIELD_RULES implementation.
*"* method's implementations
*include methods.
  METHOD create.
    CREATE OBJECT ro_result TYPE Lcl_abapgit_field_rules.
  ENDMETHOD.
  METHOD fill_value.
    CASE iv_rule.
      WHEN Lif_abapgit_field_rules=>c_fill_rule-date.
        cv_value = sy-datum.
      WHEN Lif_abapgit_field_rules=>c_fill_rule-time.
        cv_value = sy-uzeit.
      WHEN Lif_abapgit_field_rules=>c_fill_rule-timestamp.
        GET TIME STAMP FIELD cv_value.
      WHEN Lif_abapgit_field_rules=>c_fill_rule-user.
        cv_value = sy-uname.
      WHEN Lif_abapgit_field_rules=>c_fill_rule-client.
        cv_value = sy-mandt.
      WHEN Lif_abapgit_field_rules=>c_fill_rule-package.
        cv_value = iv_package.
    ENDCASE.
  ENDMETHOD.
  METHOD Lif_abapgit_field_rules~add.
    DATA ls_item TYPE ty_item.

    ls_item-tabname   = iv_table.
    ls_item-fieldname = iv_field.
    ls_item-fill_rule = iv_fill_rule.
    INSERT ls_item INTO TABLE mt_item.

    ro_self = me.
  ENDMETHOD.
  METHOD Lif_abapgit_field_rules~apply_clear_logic.
    DATA ls_item TYPE ty_item.

    FIELD-SYMBOLS <ls_data> TYPE any.
    FIELD-SYMBOLS <lv_value> TYPE any.

    IF mt_item IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT ct_data ASSIGNING <ls_data>.
      LOOP AT mt_item INTO ls_item WHERE tabname = iv_table.
        ASSIGN COMPONENT ls_item-fieldname OF STRUCTURE <ls_data> TO <lv_value>.
        IF sy-subrc = 0.
          CLEAR <lv_value>.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
  METHOD Lif_abapgit_field_rules~apply_fill_logic.
    DATA ls_item TYPE ty_item.

    FIELD-SYMBOLS <ls_data> TYPE any.
    FIELD-SYMBOLS <lv_value> TYPE any.

    IF mt_item IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT ct_data ASSIGNING <ls_data>.
      LOOP AT mt_item INTO ls_item WHERE tabname = iv_table.
        ASSIGN COMPONENT ls_item-fieldname OF STRUCTURE <ls_data> TO <lv_value>.
        IF sy-subrc = 0.
          fill_value(
            EXPORTING
              iv_rule    = ls_item-fill_rule
              iv_package = iv_package
            CHANGING
              cv_value   = <lv_value> ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_FIELD_RULES implementation

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

*>>>>>>> ZCL_ABAPGIT_SAP_PACKAGE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_sap_package=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_sap_package=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_SAP_PACKAGE implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    mv_package = iv_package.
  ENDMETHOD.
  METHOD Lif_abapgit_sap_package~are_changes_recorded_in_tr_req.

    DATA: li_package TYPE REF TO if_package.

    cl_package_factory=>load_package(
      EXPORTING
        i_package_name             = mv_package
      IMPORTING
        e_package                  = li_package
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        no_access                  = 4
        object_locked_and_modified = 5
        OTHERS                     = 6 ).

    CASE sy-subrc.
      WHEN 0.
        rv_are_changes_rec_in_tr_req = li_package->wbo_korr_flag.
      WHEN 1.
        " For new packages, derive from package name
        rv_are_changes_rec_in_tr_req = boolc( mv_package(1) <> '$' AND mv_package(1) <> 'T' ).
      WHEN OTHERS.
        Lcx_abapgit_exception=>raise_t100( ).
    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_sap_package~create.

    DATA: lv_err     TYPE string,
          li_package TYPE REF TO if_package,
          ls_package LIKE is_package.


    ASSERT NOT is_package-devclass IS INITIAL.

    cl_package_factory=>load_package(
      EXPORTING
        i_package_name             = is_package-devclass
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        no_access                  = 4
        object_locked_and_modified = 5 ).
    IF sy-subrc = 0.
      " Package already exists. We assume this is fine. Its properties might be changed later at
      " DEVC deserialization.
      RETURN.
    ENDIF.

    ls_package = is_package.

    " Set software component to 'HOME' if none is set at this point.
    " Otherwise SOFTWARE_COMPONENT_INVALID will be raised.
    IF ls_package-dlvunit IS INITIAL.
      ls_package-dlvunit = 'HOME'.
    ENDIF.

    " For transportable packages, get default transport and layer
    IF ls_package-devclass(1) <> '$' AND ls_package-pdevclass IS INITIAL.
      ls_package-pdevclass = Lif_abapgit_sap_package~get_transport_layer( ).
    ENDIF.

    cl_package_factory=>create_new_package(
      EXPORTING
        i_reuse_deleted_object     = abap_true
*        i_suppress_dialog          = abap_true " does not exist in 730
      IMPORTING
        e_package                  = li_package
      CHANGING
        c_package_data             = ls_package
      EXCEPTIONS
        object_already_existing    = 1
        object_just_created        = 2
        not_authorized             = 3
        wrong_name_prefix          = 4
        undefined_name             = 5
        reserved_local_name        = 6
        invalid_package_name       = 7
        short_text_missing         = 8
        software_component_invalid = 9
        layer_invalid              = 10
        author_not_existing        = 11
        component_not_existing     = 12
        component_missing          = 13
        prefix_in_use              = 14
        unexpected_error           = 15
        intern_err                 = 16
        no_access                  = 17
*        invalid_translation_depth  = 18
*        wrong_mainpack_value       = 19
*        superpackage_invalid       = 20
*        error_in_cts_checks        = 21
        OTHERS                     = 18 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    li_package->save(
*      EXPORTING
*        i_suppress_dialog     = abap_true    " Controls whether popups can be transmitted
      EXCEPTIONS
        object_invalid        = 1
        object_not_changeable = 2
        cancelled_in_corr     = 3
        permission_failure    = 4
        unexpected_error      = 5
        intern_err            = 6
        OTHERS                = 7 ).
    IF sy-subrc <> 0.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_err.

      " Here we have to delete the package,
      " otherwise it would remain in the memory
      " and cannot created again in this session.
      li_package->delete(
        EXCEPTIONS
          object_not_empty      = 1
          object_not_changeable = 2
          object_invalid        = 3
          intern_err            = 4
          OTHERS                = 5 ).

      Lcx_abapgit_exception=>raise( lv_err ).

    ENDIF.

    li_package->set_changeable( abap_false ).

  ENDMETHOD.
  METHOD Lif_abapgit_sap_package~create_child.

    DATA: li_parent TYPE REF TO if_package,
          ls_child  TYPE scompkdtln.


    cl_package_factory=>load_package(
      EXPORTING
        i_package_name             = mv_package
      IMPORTING
        e_package                  = li_parent
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        no_access                  = 4
        object_locked_and_modified = 5 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    ls_child-devclass  = iv_child.
    ls_child-dlvunit   = li_parent->software_component.
    ls_child-component = li_parent->application_component.
    ls_child-ctext     = iv_child.
    ls_child-parentcl  = mv_package.
    ls_child-pdevclass = li_parent->transport_layer.
    ls_child-as4user   = sy-uname.

    Lif_abapgit_sap_package~create( ls_child ).

  ENDMETHOD.
  METHOD Lif_abapgit_sap_package~create_local.

    DATA: ls_package TYPE scompkdtln.


    ls_package-devclass  = mv_package.
    ls_package-ctext     = mv_package.
    ls_package-parentcl  = '$TMP'.
    ls_package-dlvunit   = 'LOCAL'.
    ls_package-as4user   = sy-uname.

    Lif_abapgit_sap_package~create( ls_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_sap_package~exists.

    cl_package_factory=>load_package(
      EXPORTING
        i_package_name             = mv_package
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        no_access                  = 4
        object_locked_and_modified = 5 ).
    rv_bool = boolc( sy-subrc <> 1 ).

  ENDMETHOD.
  METHOD Lif_abapgit_sap_package~get_transport_layer.

    " Get default transport layer
    CALL FUNCTION 'TR_GET_TRANSPORT_TARGET'
      EXPORTING
        iv_use_default             = abap_true
        iv_get_layer_only          = abap_true
      IMPORTING
        ev_layer                   = rv_transport_layer
      EXCEPTIONS
        wrong_call                 = 1
        invalid_input              = 2
        cts_initialization_failure = 3
        OTHERS                     = 4.
    IF sy-subrc <> 0.
      " Return empty layer (i.e. "local workbench request" for the package)
      CLEAR rv_transport_layer.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_sap_package~get_transport_type.

    DATA:
      lv_pkg_name TYPE e071-obj_name,
      lv_obj_name TYPE tadir-obj_name,
      lv_role     TYPE trnrole.

    lv_pkg_name = mv_package.
    lv_obj_name = mv_package.

    CALL FUNCTION 'TR_GET_REQUEST_TYPE'
      EXPORTING
        iv_pgmid          = 'R3TR'
        iv_object         = 'DEVC'
        iv_obj_name       = lv_pkg_name
      IMPORTING
        ev_request_type   = rs_transport_type-request
        ev_task_type      = rs_transport_type-task
      EXCEPTIONS
        no_request_needed = 1
        invalid_object    = 2
        system_error      = 3
        OTHERS            = 4.

    CASE sy-subrc.
      WHEN 0 OR 1.
        RETURN.
      WHEN 2.
        " For new packages, set to workbench request
        rs_transport_type-request = 'K'.

        CALL FUNCTION 'TR_GET_NAMESPACE_AND_ROLE'
          EXPORTING
            iv_pgmid                   = 'R3TR'
            iv_object                  = 'DEVC'
            iv_objname                 = lv_obj_name
          IMPORTING
            ev_role                    = lv_role
          EXCEPTIONS
            namespace_not_existing     = 1
            invalid_object             = 2
            namespace_not_determinable = 3
            OTHERS                     = 4.
        IF sy-subrc = 0 AND lv_role = 'C'.
          " Namespace with repair license requires repair task
          rs_transport_type-task = 'R'.
        ELSE.
          " Otherweise use correction task
          rs_transport_type-task = 'S'.
        ENDIF.
      WHEN OTHERS.
        Lcx_abapgit_exception=>raise_t100( ).
    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_sap_package~list_subpackages.

    DATA: lt_list     LIKE rt_list.

    SELECT devclass FROM tdevc
      INTO TABLE lt_list
      WHERE parentcl = mv_package
      ORDER BY PRIMARY KEY.               "#EC CI_SUBRC "#EC CI_GENBUFF

    rt_list = lt_list.
    WHILE lines( lt_list ) > 0.

      SELECT devclass FROM tdevc
        INTO TABLE lt_list
        FOR ALL ENTRIES IN lt_list
        WHERE parentcl = lt_list-table_line
        ORDER BY PRIMARY KEY.             "#EC CI_SUBRC "#EC CI_GENBUFF
      APPEND LINES OF lt_list TO rt_list.

    ENDWHILE.

  ENDMETHOD.
  METHOD Lif_abapgit_sap_package~list_superpackages.

    DATA: lt_list   LIKE rt_list,
          lv_parent TYPE tdevc-parentcl.


    APPEND mv_package TO rt_list.

    lv_parent = Lif_abapgit_sap_package~read_parent( ).

    IF sy-subrc = 0 AND NOT lv_parent IS INITIAL.
      lt_list = Lcl_abapgit_factory=>get_sap_package( lv_parent )->list_superpackages( ).
      APPEND LINES OF lt_list TO rt_list.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_sap_package~read_description.

    SELECT SINGLE ctext FROM tdevct INTO rv_description
      WHERE devclass = mv_package AND spras = sy-langu ##SUBRC_OK.

  ENDMETHOD.
  METHOD Lif_abapgit_sap_package~read_parent.

    SELECT SINGLE parentcl FROM tdevc INTO rv_parentcl
      WHERE devclass = mv_package.                      "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Inconsistent package structure! Cannot find parent for { mv_package }| ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_sap_package~read_responsible.
    SELECT SINGLE as4user FROM tdevc
      INTO rv_responsible
      WHERE devclass = mv_package ##SUBRC_OK.           "#EC CI_GENBUFF
  ENDMETHOD.
  METHOD Lif_abapgit_sap_package~validate_name.

    IF mv_package IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Package name must not be empty' ).
    ENDIF.

    IF mv_package = '$TMP'.
      Lcx_abapgit_exception=>raise( 'It is not possible to use $TMP, use a different (local) package' ).
    ENDIF.

    " Check if package name is allowed
    cl_package_helper=>check_package_name(
      EXPORTING
        i_package_name       = mv_package
      EXCEPTIONS
        undefined_name       = 1
        wrong_name_prefix    = 2
        reserved_local_name  = 3
        invalid_package_name = 4 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Package name { mv_package } is not valid| ).
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SAP_PACKAGE implementation

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

*>>>>>>> ZCL_ABAPGIT_LONGTEXTS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_longtexts=========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_longtexts=========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_longtexts=========ccau.

*CLASS zcl_abapgit_longtexts DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLAS6ABTU.


class LCL_ABAPGIT_LONGTEXTS implementation.
*"* method's implementations
*include methods.
  METHOD escape_name.
    " Prepare name for SQL LIKE condition
    rv_object = iv_object_name.

    IF 'CA,CE,CO,CT,IA,IE,IO,WC,FU,FX,DI,IS,PS' CS iv_longtext_id.
      " Document types of objects with sub-objects
      rv_object+30 = '%'.
    ELSEIF 'OD' CS iv_longtext_id.
      rv_object+10 = '%'.
    ENDIF.

    rv_object = replace(
      val  = rv_object
      sub  = '_'
      with = '#_'
      occ  = 0 ).
  ENDMETHOD.
  METHOD read.

    DATA: ls_longtext TYPE Lif_abapgit_longtexts=>ty_longtext,
          lv_object   TYPE dokil-object,
          lt_dokil    TYPE Lif_abapgit_definitions=>ty_dokil_tt.

    FIELD-SYMBOLS: <ls_dokil> LIKE LINE OF lt_dokil.

    lv_object = escape_name(
      iv_longtext_id = iv_longtext_id
      iv_object_name = iv_object_name ).

    IF lines( it_dokil ) > 0.

      lt_dokil = it_dokil.

      IF iv_main_lang_only = abap_true.
        DELETE lt_dokil WHERE masterlang <> abap_true.
      ENDIF.

    ELSEIF iv_longtext_id IS NOT INITIAL.
      IF iv_main_lang_only = abap_true.
        SELECT * FROM dokil
                 INTO TABLE lt_dokil
                 WHERE id     = iv_longtext_id
                 AND object LIKE lv_object ESCAPE '#'
                 AND masterlang = abap_true
                 ORDER BY PRIMARY KEY.
      ELSE.
        SELECT * FROM dokil
                 INTO TABLE lt_dokil
                 WHERE id     = iv_longtext_id
                 AND object LIKE lv_object ESCAPE '#'
                 ORDER BY PRIMARY KEY.
      ENDIF.
    ELSE.

      Lcx_abapgit_exception=>raise( |serialize_longtexts parameter error| ).

    ENDIF.

    LOOP AT lt_dokil ASSIGNING <ls_dokil>
                     WHERE txtlines > 0.

      CLEAR: ls_longtext.

      ls_longtext-dokil = <ls_dokil>.

      CALL FUNCTION 'DOCU_READ'
        EXPORTING
          id      = <ls_dokil>-id
          langu   = <ls_dokil>-langu
          object  = <ls_dokil>-object
          typ     = <ls_dokil>-typ
          version = <ls_dokil>-version
        IMPORTING
          head    = ls_longtext-head
        TABLES
          line    = ls_longtext-lines.

      IF iv_clear_fields = abap_true.
        CLEAR: ls_longtext-head-tdfuser,
               ls_longtext-head-tdfreles,
               ls_longtext-head-tdfdate,
               ls_longtext-head-tdftime,
               ls_longtext-head-tdluser,
               ls_longtext-head-tdlreles,
               ls_longtext-head-tdldate,
               ls_longtext-head-tdltime.
      ENDIF.

      INSERT ls_longtext INTO TABLE rt_longtexts.

    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_longtexts~changed_by.

    DATA: lt_longtexts TYPE Lif_abapgit_longtexts=>ty_longtexts.
    FIELD-SYMBOLS: <ls_longtext> TYPE Lif_abapgit_longtexts=>ty_longtext.

    lt_longtexts = read( iv_object_name  = iv_object_name
                         iv_longtext_id  = iv_longtext_id
                         it_dokil        = it_dokil
                         iv_clear_fields = abap_false ).

    READ TABLE lt_longtexts INDEX 1 ASSIGNING <ls_longtext>.
    IF sy-subrc = 0.
      rv_user = <ls_longtext>-head-tdluser.
      IF rv_user IS INITIAL.
        rv_user = <ls_longtext>-head-tdfuser.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_longtexts~delete.

    DATA: lt_dokil  TYPE Lif_abapgit_definitions=>ty_dokil_tt,
          lv_object TYPE dokil-object.

    FIELD-SYMBOLS: <ls_dokil> TYPE dokil.

    lv_object = escape_name(
      iv_longtext_id = iv_longtext_id
      iv_object_name = iv_object_name ).

    SELECT * FROM dokil
      INTO TABLE lt_dokil
      WHERE id = iv_longtext_id AND object LIKE lv_object ESCAPE '#'.

    LOOP AT lt_dokil ASSIGNING <ls_dokil>.

      CALL FUNCTION 'DOCU_DEL'
        EXPORTING
          id       = <ls_dokil>-id
          langu    = <ls_dokil>-langu
          object   = <ls_dokil>-object
          typ      = <ls_dokil>-typ
        EXCEPTIONS
          ret_code = 1
          OTHERS   = 2.

      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_longtexts~deserialize.

    DATA: lt_longtexts    TYPE Lif_abapgit_longtexts=>ty_longtexts,
          lv_object       TYPE dokil-object,
          lt_dokil        TYPE Lif_abapgit_definitions=>ty_dokil_tt,
          lv_no_main_lang TYPE dokil-masterlang.

    FIELD-SYMBOLS: <ls_longtext> TYPE Lif_abapgit_longtexts=>ty_longtext,
                   <ls_dokil>    TYPE dokil.

    lv_object = escape_name(
      iv_longtext_id = iv_longtext_id
      iv_object_name = iv_object_name ).

    ii_xml->read(
      EXPORTING
        iv_name = iv_longtext_name
      CHANGING
        cg_data = lt_longtexts ).

    LOOP AT lt_longtexts ASSIGNING <ls_longtext>.

      lv_no_main_lang = boolc( iv_main_language <> <ls_longtext>-dokil-langu ).

      CALL FUNCTION 'DOCU_UPDATE'
        EXPORTING
          head          = <ls_longtext>-head
          state         = c_docu_state_active
          typ           = <ls_longtext>-dokil-typ
          version       = <ls_longtext>-dokil-version
          no_masterlang = lv_no_main_lang
        TABLES
          line          = <ls_longtext>-lines.

    ENDLOOP.

    " Read existing texts and check if they were deserialized above
    " If not, delete the texts
    SELECT * FROM dokil
      INTO TABLE lt_dokil
      WHERE id = iv_longtext_id AND object LIKE lv_object ESCAPE '#'.

    LOOP AT lt_dokil ASSIGNING <ls_dokil>.

      READ TABLE lt_longtexts TRANSPORTING NO FIELDS WITH KEY
        dokil-id     = <ls_dokil>-id
        dokil-langu  = <ls_dokil>-langu
        dokil-object = <ls_dokil>-object
        dokil-typ    = <ls_dokil>-typ.
      IF sy-subrc <> 0.
        CALL FUNCTION 'DOCU_DEL'
          EXPORTING
            id       = <ls_dokil>-id
            langu    = <ls_dokil>-langu
            object   = <ls_dokil>-object
            typ      = <ls_dokil>-typ
          EXCEPTIONS
            ret_code = 1
            OTHERS   = 2.

        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise_t100( ).
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_longtexts~serialize.

    rt_longtexts = read( iv_object_name    = iv_object_name
                         iv_longtext_id    = iv_longtext_id
                         it_dokil          = it_dokil
                         iv_main_lang_only = io_i18n_params->ms_params-main_language_only ).

    IF rt_longtexts IS SUPPLIED.
      RETURN.
    ENDIF.

    ii_xml->add( iv_name = iv_longtext_name
                 ig_data = rt_longtexts ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_LONGTEXTS implementation

*>>>>>>> ZCL_ABAPGIT_LXE_TEXTS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_lxe_texts=========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_lxe_texts=========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_lxe_texts=========ccau.

*CLASS zcl_abapgit_lxe_texts DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLAS6BBTU.


class LCL_ABAPGIT_LXE_TEXTS implementation.
*"* method's implementations
*include methods.
  METHOD check_langs_versus_installed.

    DATA lt_installed_hash TYPE HASHED TABLE OF laiso WITH UNIQUE KEY table_line.
    FIELD-SYMBOLS <lv_lang> LIKE LINE OF it_languages.

    CLEAR: et_intersection, et_missfits.
    lt_installed_hash = it_installed.

    LOOP AT it_languages ASSIGNING <lv_lang>.
      READ TABLE lt_installed_hash WITH KEY table_line = <lv_lang> TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        APPEND <lv_lang> TO et_intersection.
      ELSE.
        APPEND <lv_lang> TO et_missfits.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD convert_lang_string_to_table.

    DATA:
      lt_langs_str          TYPE string_table,
      lv_laiso              TYPE laiso,
      lv_skip_main_lang_iso TYPE laiso.

    FIELD-SYMBOLS:
      <lv_str>  LIKE LINE OF lt_langs_str.

    " Keep * as indicator for 'all installed languages'
    IF iv_langs = '*'.
      APPEND iv_langs TO rt_languages.
      RETURN.
    ENDIF.

    " Convert string of 2-letter ISO languages into table of sy-langu codes
    SPLIT iv_langs AT ',' INTO TABLE lt_langs_str.

    LOOP AT lt_langs_str ASSIGNING <lv_str>.
      lv_laiso = condense( to_upper( <lv_str> ) ).
      APPEND lv_laiso TO rt_languages.
    ENDLOOP.

    IF iv_skip_main_language IS NOT INITIAL.
      lv_skip_main_lang_iso = langu_to_laiso_safe( iv_skip_main_language ).
      DELETE rt_languages WHERE table_line = lv_skip_main_lang_iso.
    ENDIF.

    SORT rt_languages.
    DELETE ADJACENT DUPLICATES FROM rt_languages.

  ENDMETHOD.
  METHOD convert_table_to_lang_string.

    DATA:
      lt_langs_str TYPE string_table.

    FIELD-SYMBOLS:
      <lv_lang> LIKE LINE OF it_languages,
      <lv_str>  TYPE string.

    " Convert table of sy-langu codes into string of 2-letter ISO languages
    LOOP AT it_languages ASSIGNING <lv_lang>.
      " Keep * as indicator for 'all installed languages'
      IF <lv_lang> = '*'.
        CLEAR lt_langs_str.
        APPEND '*' TO lt_langs_str.
        EXIT.
      ENDIF.

      APPEND INITIAL LINE TO lt_langs_str ASSIGNING <lv_str>.
      <lv_str> = <lv_lang>.
    ENDLOOP.

    CONCATENATE LINES OF lt_langs_str INTO rv_langs SEPARATED BY ','.

  ENDMETHOD.
  METHOD detect_unsupported_languages.

    check_langs_versus_installed(
      EXPORTING
        it_languages = it_languages
        it_installed = get_installed_languages( )
      IMPORTING
        et_missfits = rt_unsupported_languages ).

  ENDMETHOD.
  METHOD get_installed_languages.

    DATA:
      lv_index               TYPE i,
      lv_langu               TYPE sy-langu,
      lv_laiso               TYPE laiso,
      lv_installed_languages TYPE string,
      lt_language_filter     TYPE Lif_abapgit_environment=>ty_system_language_filter.

    IF gt_installed_languages_cache IS INITIAL.
      CALL FUNCTION 'SYSTEM_INSTALLED_LANGUAGES'
        IMPORTING
          languages       = lv_installed_languages
        EXCEPTIONS
          sapgparam_error = 1                " Error requesting profile parameter
          OTHERS          = 2.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'Fail to get system SYSTEM_INSTALLED_LANGUAGES' ).
      ENDIF.

      lt_language_filter = Lcl_abapgit_factory=>get_environment( )->get_system_language_filter( ).

      DO strlen( lv_installed_languages ) TIMES.
        lv_index = sy-index - 1.
        lv_langu = lv_installed_languages+lv_index(1).

        IF lv_langu NOT IN lt_language_filter.
          CONTINUE.
        ENDIF.

        lv_laiso = langu_to_laiso_safe( lv_langu ).
        APPEND lv_laiso TO gt_installed_languages_cache.
      ENDDO.
    ENDIF.

    rt_languages = gt_installed_languages_cache.

  ENDMETHOD.
  METHOD get_lang_iso4.

    DATA lv_lang_iso639 TYPE laiso.
    DATA lv_country     TYPE land1.
    DATA lv_class       TYPE string.

    lv_class = 'CL_I18N_LANGUAGES'.

" cannot find a way to do this in Steampunk, so dynamic for now,
    CALL METHOD (lv_class)=>sap2_to_iso639_1
      EXPORTING
        im_lang_sap2   = iv_src
      IMPORTING
        ex_lang_iso639 = lv_lang_iso639
        ex_country     = lv_country
      EXCEPTIONS
        no_assignment  = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Failed to convert [{ iv_src }] lang to iso639| ).
    ENDIF.

    CONCATENATE lv_lang_iso639 lv_country INTO rv_iso4.

  ENDMETHOD.
  METHOD get_lxe_object_list.

    DATA lv_object_name TYPE trobj_name.

    lv_object_name = iv_object_name.

    CALL FUNCTION 'LXE_OBJ_EXPAND_TRANSPORT_OBJ'
      EXPORTING
        pgmid           = 'R3TR'
        object          = iv_object_type
        obj_name        = lv_object_name
      TABLES
        ex_colob        = rt_obj_list
      EXCEPTIONS
        unknown_object  = 1
        unknown_ta_type = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      RETURN. " Ignore error and return empty list
    ENDIF.

  ENDMETHOD.
  METHOD get_translation_languages.

    " Returns a list of translation languages for serialization
    " If the setting is initial, no translations shall be serialized
    " If the setting is `*`, all all installed system languages shall be serialized
    " Else, the setting shall contain all languages to be serialized

    DATA lv_main_lang_laiso TYPE laiso.

    IF it_i18n_languages IS NOT INITIAL.
      READ TABLE it_i18n_languages TRANSPORTING NO FIELDS WITH KEY table_line = '*'.
      IF sy-subrc = 0.
        rt_languages = get_installed_languages( ).
      ELSE.
        check_langs_versus_installed(
          EXPORTING
            it_languages = it_i18n_languages
            it_installed = get_installed_languages( )
          IMPORTING
            et_intersection = rt_languages ).
      ENDIF.
    ENDIF.

    " Remove main language from translation languages
    lv_main_lang_laiso = langu_to_laiso_safe( iv_main_language ).
    DELETE rt_languages WHERE table_line = lv_main_lang_laiso.

  ENDMETHOD.
  METHOD langu_to_laiso_safe.

    Lcl_abapgit_convert=>language_sap1_to_sap2(
      EXPORTING
        im_lang_sap1  = iv_langu
      RECEIVING
        re_lang_sap2  = rv_laiso
      EXCEPTIONS
        no_assignment = 1
        OTHERS        = 2 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Could not convert lang [{ iv_langu }] to ISO| ).
    ENDIF.

  ENDMETHOD.
  METHOD read_lxe_object_text_pair.

    DATA:
      lv_error TYPE lxestring.

    TRY.
        CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_READ'
          EXPORTING
            s_lang    = iv_s_lang
            t_lang    = iv_t_lang
            custmnr   = iv_custmnr
            objtype   = iv_objtype
            objname   = iv_objname
            read_only = iv_read_only
          IMPORTING
            err_msg   = lv_error  " doesn't exist in NW <= 750
          TABLES
            lt_pcx_s1 = rt_text_pairs_tmp.
        IF lv_error IS NOT INITIAL.
          Lcx_abapgit_exception=>raise( lv_error ).
        ENDIF.

      CATCH cx_sy_dyn_call_param_not_found.

        CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_READ'
          EXPORTING
            s_lang    = iv_s_lang
            t_lang    = iv_t_lang
            custmnr   = iv_custmnr
            objtype   = iv_objtype
            objname   = iv_objname
            read_only = iv_read_only
          TABLES
            lt_pcx_s1 = rt_text_pairs_tmp.

    ENDTRY.

  ENDMETHOD.
  METHOD write_lxe_object_text_pair.

    DATA:
      lv_error TYPE lxestring.

    TRY.
        CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_WRITE'
          EXPORTING
            s_lang    = iv_s_lang
            t_lang    = iv_t_lang
            custmnr   = iv_custmnr
            objtype   = iv_objtype
            objname   = iv_objname
          IMPORTING
            err_msg   = lv_error  " doesn't exist in NW <= 750
          TABLES
            lt_pcx_s1 = it_pcx_s1.
        IF lv_error IS NOT INITIAL.
          Lcx_abapgit_exception=>raise( lv_error ).
        ENDIF.

      CATCH cx_sy_dyn_call_param_not_found.

        CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_WRITE'
          EXPORTING
            s_lang    = iv_s_lang
            t_lang    = iv_t_lang
            custmnr   = iv_custmnr
            objtype   = iv_objtype
            objname   = iv_objname
          TABLES
            lt_pcx_s1 = it_pcx_s1.

    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_lxe_texts~deserialize.

    IF is_object_supported( iv_object_type ) = abap_false.
      RETURN.
    ENDIF.

    mo_i18n_params = io_i18n_params.
    mi_xml_in      = ii_xml.
    mo_files       = io_files.

    " MAYBE TODO: see comment in serialize

    IF 1 = 1.
      deserialize_from_po(
        iv_object_type = iv_object_type
        iv_object_name = iv_object_name ).
    ELSE.
      deserialize_xml(
        iv_object_type = iv_object_type
        iv_object_name = iv_object_name ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_lxe_texts~serialize.

    IF is_object_supported( iv_object_type ) = abap_false.
      RETURN.
    ENDIF.

    mo_i18n_params = io_i18n_params.
    mi_xml_out     = ii_xml.
    mo_files       = io_files.

    " MAYBE TODO
    " if other formats are needed, including the old in-XML approach
    " here is the place to implement it. Supposed architecture:
    " I18N_PARAMS should contain an option which format to use
    " The option should be originally maintained in dot_abapgit structures (e.g. `translation_storage_format`)
    " Consequently it comes here
    " The serialize method can read it and call a corresponding submethod,
    " e.g. serialize_xml or serialize_as_po or ...
    " both ii_xml and io_files are accessible intentionally to enable both XML based or file based formats
    " access to json can be easily added too,
    " or maybe (maybe) some kind of zif_ag_object_ctl with all DAO instead

    IF 1 = 1.
      serialize_as_po(
        iv_object_type = iv_object_type
        iv_object_name = iv_object_name ).
    ELSE.
      serialize_xml(
        iv_object_type = iv_object_type
        iv_object_name = iv_object_name ).
    ENDIF.

  ENDMETHOD.
  METHOD class_constructor.

    APPEND 'CLAS' TO gt_supported_obj_types.
    APPEND 'DOMA' TO gt_supported_obj_types.
    APPEND 'DTEL' TO gt_supported_obj_types.
    APPEND 'FUGR' TO gt_supported_obj_types.
    APPEND 'MSAG' TO gt_supported_obj_types.
    APPEND 'PARA' TO gt_supported_obj_types.
    APPEND 'PROG' TO gt_supported_obj_types.
    APPEND 'SHI3' TO gt_supported_obj_types.
    APPEND 'TABL' TO gt_supported_obj_types.
    APPEND 'TRAN' TO gt_supported_obj_types.
    APPEND 'VIEW' TO gt_supported_obj_types.

  ENDMETHOD.
  METHOD deserialize_from_po.

    DATA lv_lang LIKE LINE OF mo_i18n_params->ms_params-translation_languages.
    DATA lt_po_files TYPE Lif_abapgit_i18n_file=>ty_table_of.
    DATA li_po LIKE LINE OF lt_po_files.
    DATA lt_text_pairs_tmp TYPE ty_lxe_translation-text_pairs.
    DATA lt_obj_list TYPE lxe_tt_colob.
    DATA lv_main_lang TYPE lxeisolang.
    DATA lv_target_lang TYPE lxeisolang.

    FIELD-SYMBOLS <lv_lxe_object> LIKE LINE OF lt_obj_list.

    lt_obj_list = get_lxe_object_list(
      iv_object_name = iv_object_name
      iv_object_type = iv_object_type ).

    IF lt_obj_list IS INITIAL.
      RETURN.
    ENDIF.

    lt_po_files  = mo_files->read_i18n_files( ).
    lv_main_lang = get_lang_iso4( langu_to_laiso_safe( mo_i18n_params->ms_params-main_language ) ).

    LOOP AT mo_i18n_params->ms_params-translation_languages INTO lv_lang.
      lv_target_lang = get_lang_iso4( lv_lang ).

      LOOP AT lt_po_files INTO li_po.
        IF li_po->lang( ) = to_lower( lv_lang ). " Not quite efficient but the list is presumably very short
          EXIT.
        ELSE.
          CLEAR li_po.
        ENDIF.
      ENDLOOP.

      CHECK li_po IS BOUND. " Ignore missing files, missing translation is not a crime

      LOOP AT lt_obj_list ASSIGNING <lv_lxe_object>.

        lt_text_pairs_tmp = read_lxe_object_text_pair(
          iv_s_lang    = lv_main_lang
          iv_t_lang    = lv_target_lang
          iv_custmnr   = <lv_lxe_object>-custmnr
          iv_objtype   = <lv_lxe_object>-objtype
          iv_objname   = <lv_lxe_object>-objname
          iv_read_only = abap_false ).

        li_po->translate( CHANGING ct_text_pairs = lt_text_pairs_tmp ).
        " TODO maybe optimize, check if values have changed

        write_lxe_object_text_pair(
          iv_s_lang  = lv_main_lang
          iv_t_lang  = lv_target_lang
          iv_custmnr = <lv_lxe_object>-custmnr
          iv_objtype = <lv_lxe_object>-objtype
          iv_objname = <lv_lxe_object>-objname
          it_pcx_s1  = lt_text_pairs_tmp ).

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.
  METHOD deserialize_xml.

    DATA:
      lt_lxe_texts      TYPE ty_lxe_translations,
      ls_lxe_item       LIKE LINE OF lt_lxe_texts,
      lt_text_pairs_tmp LIKE ls_lxe_item-text_pairs.

    mi_xml_in->read(
      EXPORTING iv_name = iv_lxe_text_name
      CHANGING  cg_data = lt_lxe_texts ).

    LOOP AT lt_lxe_texts INTO ls_lxe_item.
      " Call Read first for buffer prefill

      lt_text_pairs_tmp = read_lxe_object_text_pair(
        iv_s_lang    = ls_lxe_item-source_lang
        iv_t_lang    = ls_lxe_item-target_lang
        iv_custmnr   = ls_lxe_item-custmnr
        iv_objtype   = ls_lxe_item-objtype
        iv_objname   = ls_lxe_item-objname
        iv_read_only = abap_false ).

      "Call actual Write FM
      write_lxe_object_text_pair(
        iv_s_lang  = ls_lxe_item-source_lang
        iv_t_lang  = ls_lxe_item-target_lang
        iv_custmnr = ls_lxe_item-custmnr
        iv_objtype = ls_lxe_item-objtype
        iv_objname = ls_lxe_item-objname
        it_pcx_s1  = ls_lxe_item-text_pairs ).

    ENDLOOP.

  ENDMETHOD.
  METHOD iso4_to_iso2.
    rv_laiso = iv_lxe_lang+0(2).
  ENDMETHOD.
  METHOD is_object_supported.
    READ TABLE gt_supported_obj_types TRANSPORTING NO FIELDS WITH KEY table_line = iv_object_type.
    rv_yes = boolc( sy-subrc = 0 ).
  ENDMETHOD.
  METHOD read_text_items.

    DATA:
      lt_obj_list      TYPE lxe_tt_colob,
      lv_main_lang     TYPE lxeisolang,
      ls_lxe_text_item LIKE LINE OF rt_text_items.

    FIELD-SYMBOLS:
      <lv_language>   LIKE LINE OF mo_i18n_params->ms_params-translation_languages,
      <lv_lxe_object> LIKE LINE OF lt_obj_list.

    lt_obj_list = get_lxe_object_list(
      iv_object_name = iv_object_name
      iv_object_type = iv_object_type ).

    IF lt_obj_list IS INITIAL.
      RETURN.
    ENDIF.

    " Get list of languages that need to be serialized (already resolves * and installed languages)
    lv_main_lang = get_lang_iso4( langu_to_laiso_safe( mo_i18n_params->ms_params-main_language ) ).

    LOOP AT lt_obj_list ASSIGNING <lv_lxe_object>.
      CLEAR ls_lxe_text_item.
      ls_lxe_text_item-custmnr = <lv_lxe_object>-custmnr.
      ls_lxe_text_item-objtype = <lv_lxe_object>-objtype.
      ls_lxe_text_item-objname = <lv_lxe_object>-objname.

      LOOP AT mo_i18n_params->ms_params-translation_languages ASSIGNING <lv_language>.
        ls_lxe_text_item-source_lang = lv_main_lang.
        ls_lxe_text_item-target_lang = get_lang_iso4( <lv_language> ).
        IF ls_lxe_text_item-source_lang = ls_lxe_text_item-target_lang.
          CONTINUE. " if source = target -> skip
        ENDIF.

        ls_lxe_text_item-text_pairs = read_lxe_object_text_pair(
          iv_s_lang    = ls_lxe_text_item-source_lang
          iv_t_lang    = ls_lxe_text_item-target_lang
          iv_custmnr   = ls_lxe_text_item-custmnr
          iv_objtype   = ls_lxe_text_item-objtype
          iv_objname   = ls_lxe_text_item-objname ).

        IF ls_lxe_text_item-text_pairs IS NOT INITIAL.
          APPEND ls_lxe_text_item TO rt_text_items.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.
  METHOD serialize_as_po.

    DATA lt_lxe_texts TYPE ty_lxe_translations.
    DATA lo_po_file TYPE REF TO Lcl_abapgit_po_file.
    DATA lv_lang LIKE LINE OF mo_i18n_params->ms_params-translation_languages.
    FIELD-SYMBOLS <ls_translation> LIKE LINE OF lt_lxe_texts.

    lt_lxe_texts = read_text_items(
      iv_object_name   = iv_object_name
      iv_object_type   = iv_object_type ).

    LOOP AT mo_i18n_params->ms_params-translation_languages INTO lv_lang.
      lv_lang = to_lower( lv_lang ).
      CREATE OBJECT lo_po_file
        EXPORTING
          iv_lang = lv_lang.
      LOOP AT lt_lxe_texts ASSIGNING <ls_translation>.
        IF iso4_to_iso2( <ls_translation>-target_lang ) = lv_lang.
          lo_po_file->push_text_pairs(
            iv_objtype    = <ls_translation>-objtype
            iv_objname    = <ls_translation>-objname
            it_text_pairs = <ls_translation>-text_pairs ).
        ENDIF.
      ENDLOOP.
      mo_files->add_i18n_file( lo_po_file ).
    ENDLOOP.

  ENDMETHOD.
  METHOD serialize_xml.

    DATA lt_lxe_texts TYPE ty_lxe_translations.

    lt_lxe_texts = read_text_items(
      iv_object_name   = iv_object_name
      iv_object_type   = iv_object_type ).

    IF lines( lt_lxe_texts ) > 0.
      mi_xml_out->add(
        iv_name = iv_lxe_text_name
        ig_data = lt_lxe_texts ).
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_LXE_TEXTS implementation

*>>>>>>> ZCL_ABAPGIT_SOTR_HANDLER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_sotr_handler======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_sotr_handler======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_sotr_handler======ccau.


class LCL_ABAPGIT_SOTR_HANDLER implementation.
*"* method's implementations
*include methods.
  METHOD create_sotr.

    DATA:
      lt_sotr     TYPE ty_sotr_tt,
      lt_sotr_use TYPE ty_sotr_use_tt.

    io_xml->read( EXPORTING iv_name = 'SOTR'
                  CHANGING cg_data = lt_sotr ).
    io_xml->read( EXPORTING iv_name = 'SOTR_USE'
                  CHANGING cg_data = lt_sotr_use ).

    create_sotr_from_data(
      iv_package  = iv_package
      it_sotr     = lt_sotr
      it_sotr_use = lt_sotr_use ).

  ENDMETHOD.
  METHOD create_sotr_from_data.

    DATA:
      lt_objects TYPE sotr_objects,
      ls_paket   TYPE sotr_pack,
      lv_alias   TYPE sotr_head-alias_name,
      lv_object  LIKE LINE OF lt_objects.

    FIELD-SYMBOLS: <ls_sotr> LIKE LINE OF it_sotr.

    LOOP AT it_sotr ASSIGNING <ls_sotr>.
      CALL FUNCTION 'SOTR_OBJECT_GET_OBJECTS'
        EXPORTING
          object_vector    = <ls_sotr>-header-objid_vec
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

      ls_paket-paket = iv_package.

      " Replace package in alias with new package
      lv_alias = <ls_sotr>-header-alias_name.
      IF lv_alias CS '/'.
        lv_alias = iv_package && lv_alias+sy-fdpos(*).
      ENDIF.

      CALL FUNCTION 'SOTR_CREATE_CONCEPT'
        EXPORTING
          paket                         = ls_paket
          crea_lan                      = <ls_sotr>-header-crea_lan
          alias_name                    = lv_alias
          object                        = lv_object
          entries                       = <ls_sotr>-entries
          concept_default               = <ls_sotr>-header-concept
        EXCEPTIONS
          package_missing               = 1
          crea_lan_missing              = 2
          object_missing                = 3
          paket_does_not_exist          = 4
          alias_already_exist           = 5
          object_type_not_found         = 6
          langu_missing                 = 7
          identical_context_not_allowed = 8
          text_too_long                 = 9
          error_in_update               = 10
          no_master_langu               = 11
          error_in_concept_id           = 12
          alias_not_allowed             = 13
          tadir_entry_creation_failed   = 14
          internal_error                = 15
          error_in_correction           = 16
          user_cancelled                = 17
          no_entry_found                = 18
          OTHERS                        = 19.
      IF sy-subrc <> 0 AND sy-subrc <> 5.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'SOTR_USAGE_MODIFY'
      EXPORTING
        sotr_usage = it_sotr_use.

  ENDMETHOD.
  METHOD delete_sotr.

    DATA lt_sotr_use TYPE ty_sotr_use_tt.

    FIELD-SYMBOLS <ls_sotr_use> LIKE LINE OF lt_sotr_use.

    lt_sotr_use = get_sotr_usage( iv_pgmid    = iv_pgmid
                                  iv_object   = iv_object
                                  iv_obj_name = iv_obj_name ).

    " Remove any usage to ensure deletion, see function module BTFR_CHECK
    DELETE sotr_use FROM TABLE lt_sotr_use ##SUBRC_OK.

    LOOP AT lt_sotr_use ASSIGNING <ls_sotr_use> WHERE concept IS NOT INITIAL.

      CALL FUNCTION 'SOTR_DELETE_CONCEPT'
        EXPORTING
          concept             = <ls_sotr_use>-concept
        EXCEPTIONS
          no_entry_found      = 1
          text_not_found      = 2
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
  METHOD delete_sotr_package.

    DATA lt_sotr_head TYPE STANDARD TABLE OF sotr_head WITH DEFAULT KEY.
    DATA lv_obj_name TYPE tadir-obj_name.

    FIELD-SYMBOLS <ls_sotr_head> LIKE LINE OF lt_sotr_head.

    SELECT * FROM sotr_head INTO TABLE lt_sotr_head WHERE paket = iv_package.

    LOOP AT lt_sotr_head ASSIGNING <ls_sotr_head> WHERE concept IS NOT INITIAL.

      CALL FUNCTION 'SOTR_DELETE_CONCEPT'
        EXPORTING
          concept             = <ls_sotr_head>-concept
        EXCEPTIONS
          no_entry_found      = 1
          text_not_found      = 2
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

    " Nothing left, then delete SOTR from TADIR
    SELECT * FROM sotr_head INTO TABLE lt_sotr_head WHERE paket = iv_package.
    IF sy-subrc <> 0.
      SELECT SINGLE obj_name FROM tadir INTO lv_obj_name
        WHERE pgmid = 'R3TR' AND object = 'SOTR' AND obj_name = iv_package.
      IF sy-subrc = 0.
        CALL FUNCTION 'TR_TADIR_INTERFACE'
          EXPORTING
            wi_delete_tadir_entry = abap_true
            wi_test_modus         = abap_false
            wi_tadir_pgmid        = 'R3TR'
            wi_tadir_object       = 'SOTR'
            wi_tadir_obj_name     = lv_obj_name
          EXCEPTIONS
            OTHERS                = 1 ##FM_SUBRC_OK.

        IF Lcl_abapgit_factory=>get_sap_package( iv_package )->are_changes_recorded_in_tr_req( ) = abap_true.

          Lcl_abapgit_factory=>get_cts_api( )->insert_transport_object(
            iv_object   = 'SOTR'
            iv_obj_name = lv_obj_name
            iv_package  = iv_package
            iv_mode     = Lif_abapgit_cts_api=>c_transport_mode-delete ).

        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD get_sotr_4_concept.

    DATA: ls_header  TYPE ty_sotr-header,
          lv_paket   LIKE ls_header-alias_name,
          lt_entries TYPE ty_sotr-entries.

    FIELD-SYMBOLS: <ls_entry> LIKE LINE OF lt_entries.

    CALL FUNCTION 'SOTR_GET_CONCEPT'
      EXPORTING
        concept        = iv_concept
      IMPORTING
        header         = ls_header
      TABLES
        entries        = lt_entries
      EXCEPTIONS
        no_entry_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " If alias contains package, remove it
    lv_paket = ls_header-paket && '/'.
    IF ls_header-alias_name CS lv_paket.
      ls_header-alias_name = replace(
        val  = ls_header-alias_name
        sub  = lv_paket
        with = '/'
        occ  = 1 ).
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

    rs_sotr-header  = ls_header.
    rs_sotr-entries = lt_entries.

  ENDMETHOD.
  METHOD get_sotr_usage.

    DATA: lv_obj_name TYPE trobj_name.

    lv_obj_name = iv_obj_name.

    " Objects with multiple components
    IF iv_pgmid = 'LIMU' AND ( iv_object CP 'WDY*' OR iv_object = 'WAPP' ).
      lv_obj_name+30 = '%'.
    ENDIF.

    CALL FUNCTION 'SOTR_USAGE_READ'
      EXPORTING
        pgmid          = iv_pgmid
        object         = iv_object
        obj_name       = lv_obj_name
      IMPORTING
        sotr_usage     = rt_sotr_use
      EXCEPTIONS
        no_entry_found = 1
        error_in_pgmid = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      SORT rt_sotr_use.
    ENDIF.

  ENDMETHOD.
  METHOD read_sotr.

    FIELD-SYMBOLS <ls_sotr_use> LIKE LINE OF et_sotr_use.

    DATA: lv_sotr            TYPE ty_sotr,
          lt_language_filter TYPE Lif_abapgit_environment=>ty_system_language_filter.

    " OTR short text usage: see TABLE BTFR_OBJ_IDS
    " LIMU: CPUB, WAPP, WDYC, WDYD, WDYV
    " R3TR: ENHC, ENHO, ENHS, ENSC, SCGR, SMIF, WDCA, WDCC, WEBI, WEBS

    et_sotr_use = get_sotr_usage( iv_pgmid    = iv_pgmid
                                  iv_object   = iv_object
                                  iv_obj_name = iv_obj_name ).

    LOOP AT et_sotr_use ASSIGNING <ls_sotr_use> WHERE concept IS NOT INITIAL.
      lv_sotr = get_sotr_4_concept( <ls_sotr_use>-concept ).

      IF io_xml IS BOUND AND io_i18n_params->ms_params-main_language_only = abap_true.
        DELETE lv_sotr-entries WHERE langu <> io_i18n_params->ms_params-main_language.
        CHECK lv_sotr-entries IS NOT INITIAL.
      ENDIF.
      lt_language_filter = io_i18n_params->build_language_filter( ).
      DELETE lv_sotr-entries WHERE NOT langu IN lt_language_filter
        AND langu <> io_i18n_params->ms_params-main_language.
      CHECK lv_sotr-entries IS NOT INITIAL.

      INSERT lv_sotr INTO TABLE et_sotr.
    ENDLOOP.

    IF io_xml IS BOUND.
      io_xml->add( iv_name = 'SOTR'
                   ig_data = et_sotr ).
      io_xml->add( iv_name = 'SOTR_USE'
                   ig_data = et_sotr_use ).
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SOTR_HANDLER implementation

*>>>>>>> ZCL_ABAPGIT_I18N_PARAMS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_i18n_params=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_i18n_params=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_i18n_params=======ccau.

*CLASS zcl_abapgit_i18n_params DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLAS6EBTU.


class LCL_ABAPGIT_I18N_PARAMS implementation.
*"* method's implementations
*include methods.
  METHOD build_language_filter.
    IF mt_language_filter IS INITIAL.
      " translation_languages are includes, system langs are excludes, so the do not interfere
      IF ms_params-translation_languages IS NOT INITIAL.
        mt_language_filter = iso_langs_to_lang_filter( ms_params-translation_languages ).
      ELSE.
        mt_language_filter = Lcl_abapgit_factory=>get_environment( )->get_system_language_filter( ).
      ENDIF.
    ENDIF.
    rt_language_filter = mt_language_filter.
  ENDMETHOD.
  METHOD constructor.
    IF is_params IS NOT INITIAL.
      ms_params = is_params.
    ELSE.
      ms_params-main_language         = iv_main_language.
      ms_params-main_language_only    = iv_main_language_only.
      ms_params-translation_languages = it_translation_langs.
      ms_params-use_lxe               = iv_use_lxe.
    ENDIF.
    ASSERT ms_params-main_language IS NOT INITIAL.
  ENDMETHOD.
  METHOD iso_langs_to_lang_filter.

    DATA lv_laiso LIKE LINE OF it_iso_filter.
    DATA lv_langu TYPE sy-langu.
    DATA ls_range LIKE LINE OF rt_language_filter.

    ls_range-sign = 'I'.
    ls_range-option = 'EQ'.

    LOOP AT it_iso_filter INTO lv_laiso.

      Lcl_abapgit_convert=>language_sap2_to_sap1(
        EXPORTING
          im_lang_sap2  = lv_laiso
        RECEIVING
          re_lang_sap1  = lv_langu
        EXCEPTIONS
          no_assignment = 1
          OTHERS        = 2 ).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      ls_range-low = lv_langu.
      APPEND ls_range TO rt_language_filter.

    ENDLOOP.

  ENDMETHOD.
  METHOD is_lxe_applicable.

    rv_yes = boolc( ms_params-main_language_only = abap_false AND
       ms_params-use_lxe = abap_true AND
       ms_params-translation_languages IS NOT INITIAL ).

  ENDMETHOD.
  METHOD new.
    CREATE OBJECT ro_instance
      EXPORTING
        iv_main_language      = iv_main_language
        iv_main_language_only = iv_main_language_only
        it_translation_langs  = it_translation_langs
        iv_use_lxe            = iv_use_lxe
        is_params             = is_params.
  ENDMETHOD.
  METHOD trim_saplang_keyed_table.

    DATA lv_laiso TYPE laiso.
    DATA lv_index TYPE i.

    FIELD-SYMBOLS <ls_i> TYPE any.
    FIELD-SYMBOLS <lv_langu> TYPE sy-langu.

    IF ms_params-translation_languages IS INITIAL OR iv_lang_field_name IS INITIAL.
      RETURN. " Nothing to filter
    ENDIF.

    LOOP AT ct_tab ASSIGNING <ls_i>.
      lv_index = sy-tabix.
      ASSIGN COMPONENT iv_lang_field_name OF STRUCTURE <ls_i> TO <lv_langu>.
      ASSERT sy-subrc = 0.

      IF iv_keep_master_lang = abap_true AND <lv_langu> = ms_params-main_language.
        CONTINUE. " Just keep it
      ENDIF.

      Lcl_abapgit_convert=>language_sap1_to_sap2(
        EXPORTING
          im_lang_sap1  = <lv_langu>
        RECEIVING
          re_lang_sap2  = lv_laiso
        EXCEPTIONS
          no_assignment = 1
          OTHERS        = 2 ).
      IF sy-subrc <> 0.
        DELETE ct_tab INDEX lv_index. " Not in the list anyway ...
        CONTINUE.
      ENDIF.

      " Not a sorted table, but presumably the list is small, so no significant performance flow
      READ TABLE ms_params-translation_languages TRANSPORTING NO FIELDS WITH KEY table_line = lv_laiso.
      IF sy-subrc <> 0.
        DELETE ct_tab INDEX lv_index.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD trim_saplang_list.

    DATA lv_langu TYPE sy-langu.
    DATA lv_laiso TYPE laiso.
    DATA lv_index TYPE i.

    IF ms_params-translation_languages IS INITIAL.
      RETURN. " Nothing to filter
    ENDIF.

    LOOP AT ct_sap_langs INTO lv_langu.
      lv_index = sy-tabix.

      Lcl_abapgit_convert=>language_sap1_to_sap2(
        EXPORTING
          im_lang_sap1  = lv_langu
        RECEIVING
          re_lang_sap2  = lv_laiso
        EXCEPTIONS
          no_assignment = 1
          OTHERS        = 2 ).
      IF sy-subrc <> 0.
        DELETE ct_sap_langs INDEX lv_index. " Not in the list anyway ...
        CONTINUE.
      ENDIF.

      " Not a sorted table, but presumably the list is small, so no significant performance flow
      READ TABLE ms_params-translation_languages TRANSPORTING NO FIELDS WITH KEY table_line = lv_laiso.
      IF sy-subrc <> 0.
        DELETE ct_sap_langs INDEX lv_index.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_I18N_PARAMS implementation

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

*CLASS zcl_abapgit_po_file DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLAS6FBTU.


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
  METHOD Lif_abapgit_persist_user~set_list_settings.
    ms_user-list_settings = is_list_settings.
    update( ).
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
endclass. "ZCL_ABAPGIT_PERSISTENCE_USER implementation

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



*>>>>>>> ZCL_ABAPGIT_PERSIST_MIGRATE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_persist_migrate===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_persist_migrate===ccimp.
CLASS SHRIS5ZPAUXVKEPN5HWETLLAS6IBTU DEFINITION INHERITING FROM Lcl_abapgit_objects_program FINAL.
  PUBLIC SECTION.
    CLASS-METHODS new
      RETURNING
        VALUE(ro_instance) TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLAS6IBTU.
    METHODS get_own_cua
      RETURNING
        VALUE(rs_cua) TYPE ty_cua
      RAISING
        Lcx_abapgit_exception.
    METHODS put_own_cua
      IMPORTING
        is_cua TYPE ty_cua
      RAISING
        Lcx_abapgit_exception.
ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLAS6IBTU IMPLEMENTATION.

  METHOD new.

    DATA ls_item TYPE Lif_abapgit_definitions=>ty_item.

    SELECT SINGLE devclass object obj_name INTO (ls_item-devclass, ls_item-obj_type, ls_item-obj_name)
      FROM tadir
      WHERE pgmid  = 'R3TR'
      AND object   = 'PROG'
      AND obj_name = sy-cprog.

    CREATE OBJECT ro_instance
      EXPORTING
        iv_language = 'E'
        is_item = ls_item.

  ENDMETHOD.

  METHOD get_own_cua.

    rs_cua = serialize_cua( sy-cprog ).

  ENDMETHOD.

  METHOD put_own_cua.

    DATA li_log TYPE REF TO Lif_abapgit_log.

    deserialize_cua(
      is_cua          = is_cua
      iv_program_name = ms_item-obj_name ).

    CREATE OBJECT li_log TYPE Lcl_abapgit_log.
    Lcl_abapgit_objects_activation=>activate( li_log ).
    Lcl_abapgit_objects_activation=>clear( ).

  ENDMETHOD.

ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLAS6KBTU DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS get
      RETURNING
        VALUE(rs_cua) TYPE Lcl_abapgit_objects_program=>ty_cua ##NEEDED.
ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLAS6KBTU IMPLEMENTATION.
  METHOD get.
    DATA ls_sta LIKE LINE OF rs_cua-sta.
    DATA ls_fun LIKE LINE OF rs_cua-fun.
    DATA ls_but LIKE LINE OF rs_cua-but.
    DATA ls_pfk LIKE LINE OF rs_cua-pfk.
    DATA ls_set LIKE LINE OF rs_cua-set.
    DATA ls_doc LIKE LINE OF rs_cua-doc.
    rs_cua-adm-pfkcode = '000001'.
    CLEAR ls_sta.
    ls_sta-code = 'DECIDE_DIALOG'.
    ls_sta-modal = 'P'.
    ls_sta-pfkcode = '000001'.
    ls_sta-butcode = '0001'.
    ls_sta-int_note = 'Object list decide dialog toolbar'.
    APPEND ls_sta TO rs_cua-sta.
    CLEAR ls_fun.
    ls_fun-code = '&ILD'.
    ls_fun-textno = '001'.
    ls_fun-text_type = 'S'.
    ls_fun-text_name = 'ICON_FILTER_UNDO'.
    ls_fun-icon_id = '@GD@'.
    ls_fun-fun_text = 'Reset filter'.
    ls_fun-code = '&ILT'.
    ls_fun-textno = '001'.
    ls_fun-text_type = 'S'.
    ls_fun-text_name = 'ICON_FILTER'.
    ls_fun-icon_id = '@4G@'.
    ls_fun-fun_text = 'Set Filter'.
    ls_fun-path = 'F'.
    ls_fun-code = '&ODN'.
    ls_fun-textno = '001'.
    ls_fun-text_type = 'S'.
    ls_fun-text_name = 'ICON_SORT_DOWN'.
    ls_fun-icon_id = '@3F@'.
    ls_fun-fun_text = 'Sort in Descending Order'.
    ls_fun-path = 'O'.
    ls_fun-code = '&OUP'.
    ls_fun-textno = '001'.
    ls_fun-text_type = 'S'.
    ls_fun-text_name = 'ICON_SORT_UP'.
    ls_fun-icon_id = '@3E@'.
    ls_fun-fun_text = 'Sort in Ascending Order'.
    ls_fun-path = 'I'.
    ls_fun-code = 'CANCEL'.
    ls_fun-textno = '001'.
    ls_fun-type = 'E'.
    ls_fun-text_type = 'S'.
    ls_fun-text_name = 'ICON_CANCEL'.
    ls_fun-icon_id = '@0W@'.
    ls_fun-fun_text = 'Cancel'.
    ls_fun-icon_text = 'Cancel'.
    ls_fun-path = 'A'.
    ls_fun-code = 'OK'.
    ls_fun-textno = '001'.
    ls_fun-text_type = 'S'.
    ls_fun-text_name = 'ICON_OKAY'.
    ls_fun-icon_id = '@0V@'.
    ls_fun-fun_text = 'Continue'.
    ls_fun-icon_text = 'Continue'.
    ls_fun-code = 'SEL_ALL'.
    ls_fun-textno = '001'.
    ls_fun-text_type = 'S'.
    ls_fun-text_name = 'ICON_SELECT_ALL'.
    ls_fun-icon_id = '@4B@'.
    ls_fun-fun_text = 'Select All Visible'.
    ls_fun-code = 'SEL_CAT'.
    ls_fun-textno = '001'.
    ls_fun-text_type = 'S'.
    ls_fun-text_name = 'ICON_WD_TOOLBAR'.
    ls_fun-icon_id = '@TF@'.
    ls_fun-fun_text = 'Select category'.
    ls_fun-code = 'SEL_DEL'.
    ls_fun-textno = '001'.
    ls_fun-text_type = 'S'.
    ls_fun-text_name = 'ICON_DESELECT_ALL'.
    ls_fun-icon_id = '@4D@'.
    ls_fun-fun_text = 'Deselect All Visible'.
    ls_fun-code = 'SEL_KEY'.
    ls_fun-textno = '001'.
    ls_fun-text_type = 'S'.
    ls_fun-text_name = 'ICON_SELECT_BLOCK'.
    ls_fun-icon_id = '@4C@'.
    ls_fun-fun_text = 'Mark/Toggle Selected'.
    APPEND ls_fun TO rs_cua-fun.
    CLEAR ls_but.
    ls_but-pfk_code = '000001'.
    ls_but-code = '0001'.
    ls_but-no = '01'.
    ls_but-pfno = '00'.
    ls_but-pfk_code = '000001'.
    ls_but-code = '0001'.
    ls_but-no = '02'.
    ls_but-pfno = 'S'.
    ls_but-pfk_code = '000001'.
    ls_but-code = '0001'.
    ls_but-no = '03'.
    ls_but-pfno = '13'.
    ls_but-pfk_code = '000001'.
    ls_but-code = '0001'.
    ls_but-no = '04'.
    ls_but-pfno = '17'.
    ls_but-pfk_code = '000001'.
    ls_but-code = '0001'.
    ls_but-no = '05'.
    ls_but-pfno = '14'.
    ls_but-pfk_code = '000001'.
    ls_but-code = '0001'.
    ls_but-no = '06'.
    ls_but-pfno = '16'.
    ls_but-pfk_code = '000001'.
    ls_but-code = '0001'.
    ls_but-no = '07'.
    ls_but-pfno = 'S'.
    ls_but-pfk_code = '000001'.
    ls_but-code = '0001'.
    ls_but-no = '08'.
    ls_but-pfno = '05'.
    ls_but-pfk_code = '000001'.
    ls_but-code = '0001'.
    ls_but-no = '09'.
    ls_but-pfno = '06'.
    ls_but-pfk_code = '000001'.
    ls_but-code = '0001'.
    ls_but-no = '10'.
    ls_but-pfno = '07'.
    ls_but-pfk_code = '000001'.
    ls_but-code = '0001'.
    ls_but-no = '11'.
    ls_but-pfno = '08'.
    ls_but-pfk_code = '000001'.
    ls_but-code = '0001'.
    ls_but-no = '12'.
    ls_but-pfno = 'S'.
    ls_but-pfk_code = '000001'.
    ls_but-code = '0001'.
    ls_but-no = '13'.
    ls_but-pfno = '12'.
    APPEND ls_but TO rs_cua-but.
    CLEAR ls_pfk.
    ls_pfk-code = '000001'.
    ls_pfk-pfno = '00'.
    ls_pfk-funcode = 'OK'.
    ls_pfk-funno = '001'.
    ls_pfk-code = '000001'.
    ls_pfk-pfno = '05'.
    ls_pfk-funcode = 'SEL_ALL'.
    ls_pfk-funno = '001'.
    ls_pfk-code = '000001'.
    ls_pfk-pfno = '06'.
    ls_pfk-funcode = 'SEL_DEL'.
    ls_pfk-funno = '001'.
    ls_pfk-code = '000001'.
    ls_pfk-pfno = '07'.
    ls_pfk-funcode = 'SEL_KEY'.
    ls_pfk-funno = '001'.
    ls_pfk-code = '000001'.
    ls_pfk-pfno = '08'.
    ls_pfk-funcode = 'SEL_CAT'.
    ls_pfk-funno = '001'.
    ls_pfk-code = '000001'.
    ls_pfk-pfno = '11'.
    ls_pfk-funcode = 'OK'.
    ls_pfk-funno = '001'.
    ls_pfk-code = '000001'.
    ls_pfk-pfno = '12'.
    ls_pfk-funcode = 'CANCEL'.
    ls_pfk-funno = '001'.
    ls_pfk-code = '000001'.
    ls_pfk-pfno = '13'.
    ls_pfk-funcode = '&ILT'.
    ls_pfk-funno = '001'.
    ls_pfk-code = '000001'.
    ls_pfk-pfno = '14'.
    ls_pfk-funcode = '&OUP'.
    ls_pfk-funno = '001'.
    ls_pfk-code = '000001'.
    ls_pfk-pfno = '16'.
    ls_pfk-funcode = '&ODN'.
    ls_pfk-funno = '001'.
    ls_pfk-code = '000001'.
    ls_pfk-pfno = '17'.
    ls_pfk-funcode = '&ILD'.
    ls_pfk-funno = '001'.
    APPEND ls_pfk TO rs_cua-pfk.
    CLEAR ls_set.
    ls_set-status = 'DECIDE_DIALOG'.
    ls_set-function = '&ILD'.
    ls_set-status = 'DECIDE_DIALOG'.
    ls_set-function = '&ILT'.
    ls_set-status = 'DECIDE_DIALOG'.
    ls_set-function = '&ODN'.
    ls_set-status = 'DECIDE_DIALOG'.
    ls_set-function = '&OUP'.
    ls_set-status = 'DECIDE_DIALOG'.
    ls_set-function = 'CANCEL'.
    ls_set-status = 'DECIDE_DIALOG'.
    ls_set-function = 'OK'.
    ls_set-status = 'DECIDE_DIALOG'.
    ls_set-function = 'SEL_ALL'.
    ls_set-status = 'DECIDE_DIALOG'.
    ls_set-function = 'SEL_CAT'.
    ls_set-status = 'DECIDE_DIALOG'.
    ls_set-function = 'SEL_DEL'.
    ls_set-status = 'DECIDE_DIALOG'.
    ls_set-function = 'SEL_KEY'.
    APPEND ls_set TO rs_cua-set.
    CLEAR ls_doc.
    ls_doc-obj_type = 'P'.
    ls_doc-obj_code = '000001'.
    ls_doc-modal = 'P'.
    ls_doc-int_note = 'Object list decide dialog FK settings'.
    ls_doc-obj_type = 'B'.
    ls_doc-obj_code = '000001'.
    ls_doc-sub_code = '0001'.
    ls_doc-modal = 'P'.
    ls_doc-int_note = 'Object list decide dialog PB settings'.
    APPEND ls_doc TO rs_cua-doc.
  ENDMETHOD.
ENDCLASS.

class LCL_ABAPGIT_PERSIST_MIGRATE implementation.
*"* method's implementations
*include methods.
  METHOD gui_status_create.

    DATA ls_cua TYPE Lcl_abapgit_objects_program=>ty_cua.

    IF Lcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_false.
      RETURN. " No autocreation for full version
    ENDIF.

    IF gui_status_exists( ) = abap_true.
      RETURN.
    ENDIF.

    ls_cua = SHRIS5ZPAUXVKEPN5HWETLLAS6KBTU=>get( ).

    IF ls_cua IS INITIAL. " Full version or something wrong with abapmerged version
      RETURN.
    ENDIF.

    TRY.
        SHRIS5ZPAUXVKEPN5HWETLLAS6IBTU=>new( )->put_own_cua( ls_cua ).
      CATCH Lcx_abapgit_exception.
    ENDTRY.

  ENDMETHOD.
  METHOD gui_status_exists.

    DATA ls_own_cua TYPE Lcl_abapgit_objects_program=>ty_cua.
    DATA ls_new_cua TYPE Lcl_abapgit_objects_program=>ty_cua.
    DATA lv_x_own TYPE xstring.
    DATA lv_x_new TYPE xstring.
    DATA lv_h_own TYPE Lif_abapgit_git_definitions=>ty_sha1.
    DATA lv_h_new TYPE Lif_abapgit_git_definitions=>ty_sha1.

    TRY.
        ls_own_cua = SHRIS5ZPAUXVKEPN5HWETLLAS6IBTU=>new( )->get_own_cua( ).
      CATCH Lcx_abapgit_exception.
    ENDTRY.

    IF ls_own_cua IS INITIAL.
      rv_exists = abap_false.
      RETURN.
    ENDIF.

    ls_new_cua = SHRIS5ZPAUXVKEPN5HWETLLAS6KBTU=>get( ).
    IF ls_new_cua IS INITIAL.
      rv_exists = abap_true. " own exists and new is not - nothing to compare with
      RETURN.
    ENDIF.

    EXPORT data = ls_own_cua TO DATA BUFFER lv_x_own.
    EXPORT data = ls_new_cua TO DATA BUFFER lv_x_new.

    TRY.
        lv_h_own = Lcl_abapgit_hash=>sha1_raw( lv_x_own ).
        lv_h_new = Lcl_abapgit_hash=>sha1_raw( lv_x_new ).
      CATCH Lcx_abapgit_exception.
        rv_exists = abap_true. " own exists and some issue with calculating hash ... assume own is OK
        RETURN.
    ENDTRY.

    " New exists and differs from own - then it is really new, needs to be installed
    rv_exists = boolc( lv_h_own = lv_h_new ).

  ENDMETHOD.
  METHOD lock_create.

    DATA: lv_obj_name TYPE tadir-obj_name,
          ls_dd25v    TYPE dd25v,
          lt_dd26e    TYPE STANDARD TABLE OF dd26e WITH DEFAULT KEY,
          lt_dd27p    TYPE STANDARD TABLE OF dd27p WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_dd26e> LIKE LINE OF lt_dd26e,
                   <ls_dd27p> LIKE LINE OF lt_dd27p.


    ls_dd25v-viewname   = Lcl_abapgit_persistence_db=>c_lock.
    ls_dd25v-aggtype    = 'E'.
    ls_dd25v-roottab    = Lcl_abapgit_persistence_db=>c_tabname.
    ls_dd25v-ddlanguage = Lif_abapgit_definitions=>c_english.
    ls_dd25v-ddtext     = c_text.

    APPEND INITIAL LINE TO lt_dd26e ASSIGNING <ls_dd26e>.
    <ls_dd26e>-viewname   = Lcl_abapgit_persistence_db=>c_lock.
    <ls_dd26e>-tabname    = Lcl_abapgit_persistence_db=>c_tabname.
    <ls_dd26e>-tabpos     = '0001'.
    <ls_dd26e>-fortabname = Lcl_abapgit_persistence_db=>c_tabname.
    <ls_dd26e>-enqmode    = 'E'.

    APPEND INITIAL LINE TO lt_dd27p ASSIGNING <ls_dd27p>.
    <ls_dd27p>-viewname  = Lcl_abapgit_persistence_db=>c_lock.
    <ls_dd27p>-objpos    = '0001'.
    <ls_dd27p>-viewfield = 'TYPE'.
    <ls_dd27p>-tabname   = Lcl_abapgit_persistence_db=>c_tabname.
    <ls_dd27p>-fieldname = 'TYPE'.
    <ls_dd27p>-keyflag   = abap_true.

    APPEND INITIAL LINE TO lt_dd27p ASSIGNING <ls_dd27p>.
    <ls_dd27p>-viewname  = Lcl_abapgit_persistence_db=>c_lock.
    <ls_dd27p>-objpos    = '0002'.
    <ls_dd27p>-viewfield = 'VALUE'.
    <ls_dd27p>-tabname   = Lcl_abapgit_persistence_db=>c_tabname.
    <ls_dd27p>-fieldname = 'VALUE'.
    <ls_dd27p>-keyflag   = abap_true.

    CALL FUNCTION 'DDIF_ENQU_PUT'
      EXPORTING
        name              = Lcl_abapgit_persistence_db=>c_lock
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

    lv_obj_name = Lcl_abapgit_persistence_db=>c_lock.
    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid    = 'R3TR'
        wi_tadir_object   = 'ENQU'
        wi_tadir_obj_name = lv_obj_name
        wi_set_genflag    = abap_true
        wi_test_modus     = abap_false
        wi_tadir_devclass = '$TMP'
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'DDIF_ENQU_ACTIVATE'
      EXPORTING
        name        = Lcl_abapgit_persistence_db=>c_lock
      EXCEPTIONS
        not_found   = 1
        put_failure = 2
        OTHERS      = 3.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'migrate, error from DDIF_ENQU_ACTIVATE' ).
    ENDIF.

  ENDMETHOD.
  METHOD lock_exists.

    DATA: lv_viewname TYPE dd25l-viewname.

    SELECT SINGLE viewname FROM dd25l INTO lv_viewname
      WHERE viewname = Lcl_abapgit_persistence_db=>c_lock.
    rv_exists = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD run.

    IF table_exists( ) = abap_false.
      table_create( ).
    ENDIF.

    IF lock_exists( ) = abap_false.
      lock_create( ).
    ENDIF.

    gui_status_create( ).

  ENDMETHOD.
  METHOD table_create.

    DATA: lv_rc       LIKE sy-subrc,
          lv_obj_name TYPE tadir-obj_name,
          ls_dd02v    TYPE dd02v,
          ls_dd09l    TYPE dd09l,
          lt_dd03p    TYPE STANDARD TABLE OF dd03p WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_dd03p> LIKE LINE OF lt_dd03p.

    ls_dd02v-tabname    = Lcl_abapgit_persistence_db=>c_tabname.
    ls_dd02v-ddlanguage = Lif_abapgit_definitions=>c_english.
    ls_dd02v-tabclass   = 'TRANSP'.
    ls_dd02v-ddtext     = c_text.
    ls_dd02v-contflag   = 'L'.
    ls_dd02v-exclass    = '1'.

    ls_dd09l-tabname  = Lcl_abapgit_persistence_db=>c_tabname.
    ls_dd09l-as4local = 'A'.
    ls_dd09l-tabkat   = '1'.
    ls_dd09l-tabart   = 'APPL1'.
    ls_dd09l-bufallow = 'N'.

    APPEND INITIAL LINE TO lt_dd03p ASSIGNING <ls_dd03p>.
    <ls_dd03p>-tabname   = Lcl_abapgit_persistence_db=>c_tabname.
    <ls_dd03p>-fieldname = 'TYPE'.
    <ls_dd03p>-position  = '0001'.
    <ls_dd03p>-keyflag   = 'X'.
    <ls_dd03p>-datatype  = 'CHAR'.
    <ls_dd03p>-leng      = '000012'.

    APPEND INITIAL LINE TO lt_dd03p ASSIGNING <ls_dd03p>.
    <ls_dd03p>-tabname   = Lcl_abapgit_persistence_db=>c_tabname.
    <ls_dd03p>-fieldname = 'VALUE'.
    <ls_dd03p>-position  = '0002'.
    <ls_dd03p>-keyflag   = 'X'.
    <ls_dd03p>-datatype  = 'CHAR'.
    <ls_dd03p>-leng      = '000012'.

    APPEND INITIAL LINE TO lt_dd03p ASSIGNING <ls_dd03p>.
    <ls_dd03p>-tabname   = Lcl_abapgit_persistence_db=>c_tabname.
    <ls_dd03p>-fieldname = 'DATA_STR'.
    <ls_dd03p>-position  = '0003'.
    <ls_dd03p>-datatype  = 'STRG'.

    CALL FUNCTION 'DDIF_TABL_PUT'
      EXPORTING
        name              = Lcl_abapgit_persistence_db=>c_tabname
        dd02v_wa          = ls_dd02v
        dd09l_wa          = ls_dd09l
      TABLES
        dd03p_tab         = lt_dd03p
      EXCEPTIONS
        tabl_not_found    = 1
        name_inconsistent = 2
        tabl_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    lv_obj_name = Lcl_abapgit_persistence_db=>c_tabname.
    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid    = 'R3TR'
        wi_tadir_object   = 'TABL'
        wi_tadir_obj_name = lv_obj_name
        wi_set_genflag    = abap_true
        wi_test_modus     = abap_false
        wi_tadir_devclass = '$TMP'
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'DDIF_TABL_ACTIVATE'
      EXPORTING
        name        = Lcl_abapgit_persistence_db=>c_tabname
        auth_chk    = abap_false
      IMPORTING
        rc          = lv_rc
      EXCEPTIONS
        not_found   = 1
        put_failure = 2
        OTHERS      = 3.
    IF sy-subrc <> 0 OR lv_rc <> 0.
      Lcx_abapgit_exception=>raise( 'migrate, error from DDIF_TABL_ACTIVATE' ).
    ENDIF.

  ENDMETHOD.
  METHOD table_exists.

    DATA: lv_tabname TYPE dd02l-tabname.

    SELECT SINGLE tabname FROM dd02l INTO lv_tabname
      WHERE tabname = Lcl_abapgit_persistence_db=>c_tabname.
    rv_exists = boolc( sy-subrc = 0 ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_PERSIST_MIGRATE implementation

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

*>>>>>>> ZCL_ABAPGIT_DOT_ABAPGIT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_dot_abapgit=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_dot_abapgit=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_dot_abapgit=======ccau.
*CLASS SHRIS5ZPAUXVKEPN5HWETLLAS6OBTU DEFINITION DEFERRED.
*CLASS zcl_abapgit_dot_abapgit DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLAS6OBTU.



class LCL_ABAPGIT_DOT_ABAPGIT implementation.
*"* method's implementations
*include methods.
  METHOD add_ignore.

    DATA: lv_name TYPE string.

    FIELD-SYMBOLS: <lv_ignore> LIKE LINE OF ms_data-ignore.


    lv_name = iv_path && iv_filename.

    READ TABLE ms_data-ignore FROM lv_name TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO ms_data-ignore ASSIGNING <lv_ignore>.
    <lv_ignore> = lv_name.

  ENDMETHOD.
  METHOD build_default.

    DATA: ls_data TYPE Lif_abapgit_dot_abapgit=>ty_dot_abapgit.


    ls_data-master_language = sy-langu.
    ls_data-starting_folder = '/src/'.
    ls_data-folder_logic    = Lif_abapgit_dot_abapgit=>c_folder_logic-prefix.

    CREATE OBJECT ro_dot_abapgit
      EXPORTING
        is_data = ls_data.

  ENDMETHOD.
  METHOD constructor.
    ms_data = is_data.
  ENDMETHOD.
  METHOD deserialize.

    DATA: lv_xml  TYPE string,
          ls_data TYPE Lif_abapgit_dot_abapgit=>ty_dot_abapgit.


    lv_xml = Lcl_abapgit_convert=>xstring_to_string_utf8( iv_xstr ).

    ls_data = from_xml( lv_xml ).

    CREATE OBJECT ro_dot_abapgit
      EXPORTING
        is_data = ls_data.

  ENDMETHOD.
  METHOD from_xml.

    DATA: lv_xml TYPE string.

    lv_xml = iv_xml.

    CALL TRANSFORMATION id
      OPTIONS value_handling = 'accept_data_loss'
      SOURCE XML lv_xml
      RESULT data = rs_data.

* downward compatibility
    IF rs_data-folder_logic IS INITIAL.
      rs_data-folder_logic = Lif_abapgit_dot_abapgit=>c_folder_logic-prefix.
    ENDIF.

  ENDMETHOD.
  METHOD get_abap_language_version.
    rv_abap_language_version = ms_data-abap_language_version.
  ENDMETHOD.
  METHOD get_data.
    rs_data = ms_data.
  ENDMETHOD.
  METHOD get_folder_logic.
    rv_logic = ms_data-folder_logic.
  ENDMETHOD.
  METHOD get_i18n_languages.
    rt_languages = ms_data-i18n_languages.
  ENDMETHOD.
  METHOD get_main_language.
    rv_language = ms_data-master_language.
  ENDMETHOD.
  METHOD get_requirements.
    rt_requirements = ms_data-requirements.
  ENDMETHOD.
  METHOD get_signature.

    rs_signature-path     = Lif_abapgit_definitions=>c_root_dir.
    rs_signature-filename = Lif_abapgit_definitions=>c_dot_abapgit.
    rs_signature-sha1     = Lcl_abapgit_hash=>sha1_blob( serialize( ) ).

  ENDMETHOD.
  METHOD get_starting_folder.
    rv_path = ms_data-starting_folder.
  ENDMETHOD.
  METHOD get_version_constant.
    rv_version_constant = ms_data-version_constant.
  ENDMETHOD.
  METHOD is_ignored.

    DATA: lv_name     TYPE string,
          lv_starting TYPE string,
          lv_dot      TYPE string,
          lv_ignore   TYPE string.


    lv_name = iv_path && iv_filename.

    CONCATENATE ms_data-starting_folder '*' INTO lv_starting.

    " Always allow .abapgit.xml and .apack-manifest.xml
    CONCATENATE '/' Lif_abapgit_definitions=>c_dot_abapgit INTO lv_dot.
    IF lv_name = lv_dot.
      RETURN.
    ENDIF.
    CONCATENATE '/' Lif_abapgit_apack_definitions=>c_dot_apack_manifest INTO lv_dot.
    IF lv_name = lv_dot.
      RETURN.
    ENDIF.

    " Ignore all files matching pattern in ignore list
    LOOP AT ms_data-ignore INTO lv_ignore.
      IF lv_name CP lv_ignore.
        rv_ignored = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Ignore all files outside of starting folder tree
    IF ms_data-starting_folder <> '/' AND NOT lv_name CP lv_starting.
      rv_ignored = abap_true.
    ENDIF.

    IF iv_path = Lif_abapgit_data_config=>c_default_path.
      rv_ignored = abap_false.
    ENDIF.

  ENDMETHOD.
  METHOD remove_ignore.

    DATA: lv_name TYPE string.


    lv_name = iv_path && iv_filename.

    DELETE TABLE ms_data-ignore FROM lv_name.

  ENDMETHOD.
  METHOD serialize.

    DATA lv_xml TYPE string.

    lv_xml = to_xml( ms_data ).

    rv_xstr = Lcl_abapgit_convert=>string_to_xstring_utf8_bom( lv_xml ).

  ENDMETHOD.
  METHOD set_abap_language_version.
    ms_data-abap_language_version = iv_abap_language_version.
  ENDMETHOD.
  METHOD set_folder_logic.
    ms_data-folder_logic = iv_logic.
  ENDMETHOD.
  METHOD set_i18n_languages.
    ms_data-i18n_languages = it_languages.
  ENDMETHOD.
  METHOD set_requirements.
    ms_data-requirements = it_requirements.
  ENDMETHOD.
  METHOD set_starting_folder.
    ms_data-starting_folder = iv_path.
  ENDMETHOD.
  METHOD set_version_constant.
    ms_data-version_constant = iv_version_constant.
  ENDMETHOD.
  METHOD to_file.
    rs_file-path     = Lif_abapgit_definitions=>c_root_dir.
    rs_file-filename = Lif_abapgit_definitions=>c_dot_abapgit.
    rs_file-data     = serialize( ).
    rs_file-sha1     = Lcl_abapgit_hash=>sha1_blob( rs_file-data ).
  ENDMETHOD.
  METHOD to_xml.

    CALL TRANSFORMATION id
      OPTIONS initial_components = 'suppress'
      SOURCE data = is_data
      RESULT XML rv_xml.

    rv_xml = Lcl_abapgit_xml_pretty=>print( rv_xml ).

    REPLACE FIRST OCCURRENCE
      OF REGEX '<\?xml version="1\.0" encoding="[\w-]+"\?>'
      IN rv_xml
      WITH '<?xml version="1.0" encoding="utf-8"?>'.
    ASSERT sy-subrc = 0.

  ENDMETHOD.
  METHOD use_lxe.

    IF iv_yes <> abap_undefined.
      ms_data-use_lxe = iv_yes.
    ENDIF.

    rv_yes = ms_data-use_lxe.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_DOT_ABAPGIT implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_FILTER_TRAN <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_filter_tranccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_filter_tranccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_object_filter_tranccau.
*"* use this source file for your ABAP unit test classes




class LCL_ABAPGIT_OBJECT_FILTER_TRAN implementation.
*"* method's implementations
*include methods.
  METHOD adjust_local_filter.

    DATA lt_e071_filter TYPE ty_e071_filter_tt.
    DATA lr_e071_filter TYPE REF TO ty_e071_filter.
    DATA ls_filter TYPE Lif_abapgit_definitions=>ty_tadir.
    DATA lv_trobj_name_new TYPE trobj_name.
    DATA lv_trobj_type_new TYPE tadir-object.
    DATA lt_filter TYPE Lif_abapgit_definitions=>ty_tadir_tt.
    DATA lr_cts_api TYPE REF TO Lif_abapgit_cts_api.

    lt_e071_filter = it_e071_filter.

    LOOP AT lt_e071_filter REFERENCE INTO lr_e071_filter.

      IF lr_e071_filter->pgmid = 'LIMU'.
        "Get Main Object from LIMU Object (Example the Class (R3TR) of a Method (LIMU))

        lr_cts_api = Lcl_abapgit_factory=>get_cts_api( ).

        TRY.
            lr_cts_api->get_r3tr_obj_for_limu_obj(
              EXPORTING
                iv_object   = lr_e071_filter->object
                iv_obj_name = lr_e071_filter->obj_name
              IMPORTING
                ev_object   = lv_trobj_type_new
                ev_obj_name = lv_trobj_name_new ).
          CATCH Lcx_abapgit_exception.
            CONTINUE.
        ENDTRY.

        CLEAR ls_filter.
        ls_filter-pgmid = 'R3TR'.
        ls_filter-object = lv_trobj_type_new.
        ls_filter-obj_name = lv_trobj_name_new.
      ELSE.
        ls_filter-pgmid = lr_e071_filter->pgmid.
        ls_filter-object = lr_e071_filter->object.
        ls_filter-obj_name = lr_e071_filter->obj_name.
      ENDIF.
      INSERT ls_filter INTO TABLE rt_filter.
    ENDLOOP.

    IF iv_package IS NOT INITIAL.
      ls_filter-pgmid = 'R3TR'.
      ls_filter-object = 'DEVC'.
      ls_filter-obj_name = iv_package.
      INSERT ls_filter INTO TABLE rt_filter.

      lt_filter = get_all_sub_packages( iv_package ).
      INSERT LINES OF lt_filter INTO TABLE rt_filter.

    ENDIF.

    SORT rt_filter.
    DELETE ADJACENT DUPLICATES FROM rt_filter.

    IF rt_filter IS INITIAL.

      Lcx_abapgit_exception=>raise( 'No objects found for transport filter' ).

    ENDIF.

  ENDMETHOD.
  METHOD generate_local_filter.
    DATA lt_e071_filter TYPE ty_e071_filter_tt.

    SELECT DISTINCT pgmid
                object
                obj_name
           INTO CORRESPONDING FIELDS OF TABLE lt_e071_filter
           FROM e071
      WHERE trkorr IN it_r_trkorr.
    IF sy-subrc <> 0.
      CLEAR lt_e071_filter.
    ENDIF.
    rt_filter = adjust_local_filter(
      iv_package     = iv_package
      it_e071_filter = lt_e071_filter ).
  ENDMETHOD.
  METHOD get_all_sub_packages.

    DATA li_package TYPE REF TO Lif_abapgit_sap_package.
    DATA lt_list TYPE Lif_abapgit_sap_package=>ty_devclass_tt.
    DATA lr_list TYPE REF TO devclass.
    DATA ls_filter TYPE Lif_abapgit_definitions=>ty_tadir.

    li_package = Lcl_abapgit_factory=>get_sap_package( iv_package ).
    lt_list = li_package->list_subpackages( ).
    LOOP AT lt_list REFERENCE INTO lr_list.
      ls_filter-pgmid = 'R3TR'.
      ls_filter-object = 'DEVC'.
      ls_filter-obj_name = lr_list->*.
      INSERT ls_filter INTO TABLE rt_filter.
    ENDLOOP.

  ENDMETHOD.
  METHOD get_filter_values.
    et_r_trkorr = mt_r_trkorr.
    ev_package = mv_package.
  ENDMETHOD.
  METHOD init.
    CLEAR mt_filter.
    CLEAR mt_r_trkorr.
    CLEAR mv_package.
  ENDMETHOD.
  METHOD set_filter_values.
    init( ).
    mt_r_trkorr = it_r_trkorr.
    mv_package = iv_package.
    IF it_r_trkorr IS NOT INITIAL.
      mt_filter = generate_local_filter(
        iv_package  = mv_package
        it_r_trkorr = mt_r_trkorr ).

    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object_filter~get_filter.
    rt_filter = mt_filter.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_FILTER_TRAN implementation

*>>>>>>> ZCL_ABAPGIT_REPO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_repo==============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_repo==============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_repo==============ccau.
*"* use this source file for your ABAP unit test classes


class LCL_ABAPGIT_REPO implementation.
*"* method's implementations
*include methods.
  METHOD bind_listener.
    mi_listener = ii_listener.
  ENDMETHOD.
  METHOD check_abap_language_version.

    DATA lo_abapgit_abap_language_vers TYPE REF TO Lcl_abapgit_abap_language_vers.
    DATA lv_text TYPE string.

    CREATE OBJECT lo_abapgit_abap_language_vers
      EXPORTING
        io_dot_abapgit = get_dot_abapgit( ).

    IF lo_abapgit_abap_language_vers->is_import_allowed( ms_data-package ) = abap_false.
      lv_text = |Repository cannot be imported. | &&
                |ABAP Language Version of linked package is not compatible with repository settings.|.
      Lcx_abapgit_exception=>raise( lv_text ).
    ENDIF.
  ENDMETHOD.
  METHOD check_language.

    DATA:
      lv_main_language  TYPE spras,
      lv_error_message  TYPE string,
      lv_error_longtext TYPE string.

    " for deserialize, assumes find_remote_dot_abapgit has been called before (or language won't be defined)
    lv_main_language = get_dot_abapgit( )->get_main_language( ).

    IF lv_main_language <> sy-langu.

      lv_error_message = |Current login language |
                      && |'{ Lcl_abapgit_convert=>conversion_exit_isola_output( sy-langu ) }'|
                      && | does not match main language |
                      && |'{ Lcl_abapgit_convert=>conversion_exit_isola_output( lv_main_language ) }'.|.

      " Feature open in main language only exists if abapGit tcode is present
      IF Lcl_abapgit_services_abapgit=>get_abapgit_tcode( ) IS INITIAL.
        lv_error_message = lv_error_message && | Please logon in main language and retry.|.
        lv_error_longtext = |For the Advanced menu option 'Open in Main Language' to be available a transaction code| &&
                            | must be assigned to report { sy-cprog }.|.
      ELSE.
        lv_error_message = lv_error_message && | Select 'Advanced' > 'Open in Main Language'|.
      ENDIF.

      Lcx_abapgit_exception=>raise( iv_text     = lv_error_message
                                    iv_longtext = lv_error_longtext ).

    ENDIF.

  ENDMETHOD.
  METHOD check_write_protect.

    IF get_local_settings( )-write_protected = abap_true.
      Lcx_abapgit_exception=>raise( 'Cannot deserialize. Local code is write-protected by repo config' ).
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    ASSERT NOT is_data-key IS INITIAL.

    ms_data = is_data.
    mv_request_remote_refresh = abap_true.

  ENDMETHOD.
  METHOD create_new_log.

    CREATE OBJECT mi_log TYPE Lcl_abapgit_log.
    mi_log->set_title( iv_title ).

    ri_log = mi_log.

  ENDMETHOD.
  METHOD delete_checks.

    DATA: li_package TYPE REF TO Lif_abapgit_sap_package.

    check_write_protect( ).
    check_language( ).

    li_package = Lcl_abapgit_factory=>get_sap_package( get_package( ) ).
    rs_checks-transport-required = li_package->are_changes_recorded_in_tr_req( ).

  ENDMETHOD.
  METHOD deserialize_data.

    DATA:
      lt_updated_files TYPE Lif_abapgit_git_definitions=>ty_file_signatures_tt,
      lt_result        TYPE Lif_abapgit_data_deserializer=>ty_results.

    "Deserialize data
    lt_result = Lcl_abapgit_data_factory=>get_deserializer( )->deserialize(
      ii_config  = get_data_config( )
      it_files   = get_files_remote( ) ).

    "Save deserialized data to DB and add entries to transport requests
    lt_updated_files = Lcl_abapgit_data_factory=>get_deserializer( )->actualize(
      it_result = lt_result
      is_checks = is_checks ).

    INSERT LINES OF lt_updated_files INTO TABLE ct_files.

  ENDMETHOD.
  METHOD deserialize_dot_abapgit.
    INSERT get_dot_abapgit( )->get_signature( ) INTO TABLE ct_files.
  ENDMETHOD.
  METHOD deserialize_objects.

    DATA:
      lt_updated_files TYPE Lif_abapgit_git_definitions=>ty_file_signatures_tt,
      lx_error         TYPE REF TO Lcx_abapgit_exception.

    TRY.
        lt_updated_files = Lcl_abapgit_objects=>deserialize(
          io_repo   = me
          is_checks = is_checks
          ii_log    = ii_log ).
      CATCH Lcx_abapgit_exception INTO lx_error.
        " Ensure to reset default transport request task
        Lcl_abapgit_default_transport=>get_instance( )->reset( ).
        refresh( iv_drop_log = abap_false ).
        RAISE EXCEPTION lx_error.
    ENDTRY.

    INSERT LINES OF lt_updated_files INTO TABLE ct_files.

  ENDMETHOD.
  METHOD find_remote_dot_abapgit.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF mt_remote.

    get_files_remote( ).

    READ TABLE mt_remote ASSIGNING <ls_remote>
      WITH KEY file_path
      COMPONENTS path     = Lif_abapgit_definitions=>c_root_dir
                 filename = Lif_abapgit_definitions=>c_dot_abapgit.
    IF sy-subrc = 0.
      ro_dot = Lcl_abapgit_dot_abapgit=>deserialize( <ls_remote>-data ).
      set_dot_abapgit( ro_dot ).
      COMMIT WORK AND WAIT. " to release lock
    ENDIF.

  ENDMETHOD.
  METHOD find_remote_dot_apack.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF mt_remote.

    get_files_remote( ).

    READ TABLE mt_remote ASSIGNING <ls_remote>
      WITH KEY file_path
      COMPONENTS path     = Lif_abapgit_definitions=>c_root_dir
                 filename = Lif_abapgit_apack_definitions=>c_dot_apack_manifest.
    IF sy-subrc = 0.
      ro_dot = Lcl_abapgit_apack_reader=>deserialize( iv_package_name = ms_data-package
                                                      iv_xstr         = <ls_remote>-data ).
      set_dot_apack( ro_dot ).
    ENDIF.

  ENDMETHOD.
  METHOD get_data_config.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF mt_remote.

    IF mi_data_config IS BOUND.
      ri_config = mi_data_config.
      RETURN.
    ENDIF.

    CREATE OBJECT ri_config TYPE Lcl_abapgit_data_config.
    mi_data_config = ri_config.

    " Assume remote data has been loaded already
    READ TABLE mt_remote ASSIGNING <ls_remote>
      WITH KEY file_path
      COMPONENTS path = Lif_abapgit_data_config=>c_default_path.
    IF sy-subrc = 0.
      ri_config->from_json( mt_remote ).
    ENDIF.

  ENDMETHOD.
  METHOD get_dot_apack.
    IF mo_apack_reader IS NOT BOUND.
      mo_apack_reader = Lcl_abapgit_apack_reader=>create_instance( ms_data-package ).
    ENDIF.

    ro_dot_apack = mo_apack_reader.

  ENDMETHOD.
  METHOD get_log.
    ri_log = mi_log.
  ENDMETHOD.
  METHOD get_unsupported_objects_local.

    DATA: lt_tadir           TYPE Lif_abapgit_definitions=>ty_tadir_tt,
          lt_supported_types TYPE Lcl_abapgit_objects=>ty_types_tt.

    FIELD-SYMBOLS: <ls_tadir>  LIKE LINE OF lt_tadir,
                   <ls_object> LIKE LINE OF rt_objects.

    lt_tadir = Lcl_abapgit_factory=>get_tadir( )->read(
                      iv_package            = ms_data-package
                      iv_ignore_subpackages = ms_data-local_settings-ignore_subpackages
                      iv_only_local_objects = ms_data-local_settings-only_local_objects
                      io_dot                = get_dot_abapgit( ) ).

    lt_supported_types = Lcl_abapgit_objects=>supported_list( ).
    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      READ TABLE lt_supported_types WITH KEY table_line = <ls_tadir>-object TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO rt_objects ASSIGNING <ls_object>.
        MOVE-CORRESPONDING <ls_tadir> TO <ls_object>.
        <ls_object>-obj_type = <ls_tadir>-object.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD normalize_local_settings.

    cs_local_settings-labels = Lcl_abapgit_repo_labels=>normalize( cs_local_settings-labels ).

    " TODO: more validation and normalization ?

  ENDMETHOD.
  METHOD notify_listener.

    DATA ls_meta_slug TYPE Lif_abapgit_persistence=>ty_repo_xml.

    IF mi_listener IS BOUND.
      MOVE-CORRESPONDING ms_data TO ls_meta_slug.
      mi_listener->on_meta_change(
        iv_key         = ms_data-key
        is_meta        = ls_meta_slug
        is_change_mask = is_change_mask ).
    ENDIF.

  ENDMETHOD.
  METHOD refresh_local_object.

    DATA:
      ls_tadir           TYPE Lif_abapgit_definitions=>ty_tadir,
      lt_tadir           TYPE Lif_abapgit_definitions=>ty_tadir_tt,
      lt_new_local_files TYPE Lif_abapgit_definitions=>ty_files_item_tt,
      lo_serialize       TYPE REF TO Lcl_abapgit_serialize.

    lt_tadir = Lcl_abapgit_factory=>get_tadir( )->read(
                   iv_package = ms_data-package
                   io_dot     = get_dot_abapgit( ) ).

    DELETE mt_local WHERE item-obj_type = iv_obj_type
                      AND item-obj_name = iv_obj_name.

    READ TABLE lt_tadir INTO ls_tadir
                        WITH KEY object   = iv_obj_type
                                 obj_name = iv_obj_name.
    IF sy-subrc <> 0 OR ls_tadir-delflag = abap_true.
      " object doesn't exist anymore, nothing todo here
      RETURN.
    ENDIF.

    CLEAR lt_tadir.
    INSERT ls_tadir INTO TABLE lt_tadir.

    CREATE OBJECT lo_serialize.
    lt_new_local_files = lo_serialize->serialize(
      iv_package = ms_data-package
      it_tadir   = lt_tadir ).

    INSERT LINES OF lt_new_local_files INTO TABLE mt_local.

  ENDMETHOD.
  METHOD refresh_local_objects.

    mv_request_local_refresh = abap_true.
    get_files_local( ).

  ENDMETHOD.
  METHOD remove_ignored_files.

    DATA lo_dot TYPE REF TO Lcl_abapgit_dot_abapgit.
    DATA lv_index TYPE sy-index.

    FIELD-SYMBOLS <ls_files> LIKE LINE OF ct_files.

    lo_dot = get_dot_abapgit( ).

    " Skip ignored files
    LOOP AT ct_files ASSIGNING <ls_files>.
      lv_index = sy-tabix.
      IF lo_dot->is_ignored( iv_path     = <ls_files>-path
                             iv_filename = <ls_files>-filename ) = abap_true.
        DELETE ct_files INDEX lv_index.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD reset_remote.
    CLEAR mt_remote.
    mv_request_remote_refresh = abap_true.
  ENDMETHOD.
  METHOD set.

* TODO: refactor, maybe use zcl_abapgit_string_map ?

    DATA: ls_mask TYPE Lif_abapgit_persistence=>ty_repo_meta_mask.


    ASSERT iv_url IS SUPPLIED
      OR iv_branch_name IS SUPPLIED
      OR iv_selected_commit IS SUPPLIED
      OR iv_head_branch IS SUPPLIED
      OR iv_offline IS SUPPLIED
      OR is_dot_abapgit IS SUPPLIED
      OR is_local_settings IS SUPPLIED
      OR iv_deserialized_by IS SUPPLIED
      OR iv_deserialized_at IS SUPPLIED
      OR iv_switched_origin IS SUPPLIED.


    IF iv_url IS SUPPLIED.
      ms_data-url = iv_url.
      ls_mask-url = abap_true.
    ENDIF.

    IF iv_branch_name IS SUPPLIED.
      ms_data-branch_name = iv_branch_name.
      ls_mask-branch_name = abap_true.
    ENDIF.

    IF iv_selected_commit IS SUPPLIED.
      ms_data-selected_commit = iv_selected_commit.
      ls_mask-selected_commit = abap_true.
    ENDIF.

    IF iv_head_branch IS SUPPLIED.
      ms_data-head_branch = iv_head_branch.
      ls_mask-head_branch = abap_true.
    ENDIF.

    IF iv_offline IS SUPPLIED.
      ms_data-offline = iv_offline.
      ls_mask-offline = abap_true.
    ENDIF.

    IF is_dot_abapgit IS SUPPLIED.
      ms_data-dot_abapgit = is_dot_abapgit.
      ls_mask-dot_abapgit = abap_true.
    ENDIF.

    IF is_local_settings IS SUPPLIED.
      ms_data-local_settings = is_local_settings.
      ls_mask-local_settings = abap_true.
      normalize_local_settings( CHANGING cs_local_settings = ms_data-local_settings ).
    ENDIF.

    IF iv_deserialized_at IS SUPPLIED OR iv_deserialized_by IS SUPPLIED.
      ms_data-deserialized_at = iv_deserialized_at.
      ms_data-deserialized_by = iv_deserialized_by.
      ls_mask-deserialized_at = abap_true.
      ls_mask-deserialized_by = abap_true.
    ENDIF.

    IF iv_switched_origin IS SUPPLIED.
      ms_data-switched_origin = iv_switched_origin.
      ls_mask-switched_origin = abap_true.
    ENDIF.

    notify_listener( ls_mask ).

  ENDMETHOD.
  METHOD set_dot_apack.
    get_dot_apack( ).
    mo_apack_reader->set_manifest_descriptor( io_dot_apack->get_manifest_descriptor( ) ).
  ENDMETHOD.
  METHOD set_files_remote.

    mt_remote = it_files.
    mv_request_remote_refresh = abap_false.

  ENDMETHOD.
  METHOD set_local_settings.

    set( is_local_settings = is_settings ).

  ENDMETHOD.
  METHOD switch_repo_type.

    IF iv_offline = ms_data-offline.
      Lcx_abapgit_exception=>raise( |Cannot switch_repo_type, offline already = "{ ms_data-offline }"| ).
    ENDIF.

    IF iv_offline = abap_true. " On-line -> OFFline
      set( iv_url             = Lcl_abapgit_url=>name( ms_data-url )
           iv_branch_name     = ''
           iv_selected_commit = ''
           iv_head_branch     = ''
           iv_offline         = abap_true ).
    ELSE. " OFFline -> On-line
      set( iv_offline = abap_false ).
    ENDIF.

  ENDMETHOD.
  METHOD update_last_deserialize.

    DATA: lv_deserialized_at TYPE Lif_abapgit_persistence=>ty_repo-deserialized_at,
          lv_deserialized_by TYPE Lif_abapgit_persistence=>ty_repo-deserialized_by.

    GET TIME STAMP FIELD lv_deserialized_at.
    lv_deserialized_by = sy-uname.

    set( iv_deserialized_at = lv_deserialized_at
         iv_deserialized_by = lv_deserialized_by ).

  ENDMETHOD.
  METHOD Lif_abapgit_repo~checksums.

    CREATE OBJECT ri_checksums TYPE Lcl_abapgit_repo_checksums
      EXPORTING
        iv_repo_key = ms_data-key.

  ENDMETHOD.
  METHOD Lif_abapgit_repo~deserialize.

    DATA lt_updated_files TYPE Lif_abapgit_git_definitions=>ty_file_signatures_tt.

    find_remote_dot_abapgit( ).
    find_remote_dot_apack( ).

    check_write_protect( ).
    check_language( ).

    IF is_checks-requirements-met = Lif_abapgit_definitions=>c_no AND is_checks-requirements-decision IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Requirements not met and undecided' ).
    ENDIF.

    IF is_checks-dependencies-met = Lif_abapgit_definitions=>c_no.
      Lcx_abapgit_exception=>raise( 'APACK dependencies not met' ).
    ENDIF.

    IF is_checks-transport-required = abap_true AND is_checks-transport-transport IS INITIAL.
      Lcx_abapgit_exception=>raise( |No transport request was supplied| ).
    ENDIF.

    deserialize_dot_abapgit( CHANGING ct_files = lt_updated_files ).

    deserialize_objects(
      EXPORTING
        is_checks = is_checks
        ii_log    = ii_log
      CHANGING
        ct_files  = lt_updated_files ).

    deserialize_data(
      EXPORTING
        is_checks = is_checks
      CHANGING
        ct_files  = lt_updated_files ).

    CLEAR mt_local. " Should be before CS update which uses NEW local

    Lif_abapgit_repo~checksums( )->update( lt_updated_files ).

    update_last_deserialize( ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.
  METHOD Lif_abapgit_repo~deserialize_checks.

    DATA: lt_requirements TYPE Lif_abapgit_dot_abapgit=>ty_requirement_tt,
          lt_dependencies TYPE Lif_abapgit_apack_definitions=>ty_dependencies.

    find_remote_dot_abapgit( ).
    find_remote_dot_apack( ).

    check_write_protect( ).
    check_language( ).
    check_abap_language_version( ).

    rs_checks = Lcl_abapgit_objects=>deserialize_checks( me ).

    lt_requirements = get_dot_abapgit( )->get_data( )-requirements.
    rs_checks-requirements-met = Lcl_abapgit_requirement_helper=>is_requirements_met( lt_requirements ).

    lt_dependencies = get_dot_apack( )->get_manifest_descriptor( )-dependencies.
    rs_checks-dependencies-met = Lcl_abapgit_apack_helper=>are_dependencies_met( lt_dependencies ).

    rs_checks-customizing = Lcl_abapgit_data_factory=>get_deserializer( )->deserialize_check(
      io_repo   = me
      ii_config = get_data_config( ) ).

  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_dot_abapgit.
    CREATE OBJECT ro_dot_abapgit
      EXPORTING
        is_data = ms_data-dot_abapgit.
  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_files_local.

    DATA lo_serialize TYPE REF TO Lcl_abapgit_serialize.

    " Serialization happened before and no refresh request
    IF lines( mt_local ) > 0 AND mv_request_local_refresh = abap_false.
      rt_files = mt_local.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_serialize
      EXPORTING
        io_dot_abapgit    = get_dot_abapgit( )
        is_local_settings = get_local_settings( ).

    rt_files = lo_serialize->files_local(
      iv_package     = get_package( )
      ii_data_config = get_data_config( )
      ii_log         = ii_log ).

    mt_local                 = rt_files.
    mv_request_local_refresh = abap_false. " Fulfill refresh

  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_files_remote.
    DATA lt_filter TYPE Lif_abapgit_definitions=>ty_tadir_tt.
    DATA lr_filter TYPE REF TO Lcl_abapgit_repo_filter.

    rt_files = mt_remote.
    IF ii_obj_filter IS NOT INITIAL.
      lt_filter = ii_obj_filter->get_filter( ).

      CREATE OBJECT lr_filter.
      lr_filter->apply_object_filter(
        EXPORTING
          it_filter   = lt_filter
          io_dot      = get_dot_abapgit( )
          iv_devclass = get_package( )
        CHANGING
          ct_files    = rt_files ).

    ENDIF.

    IF iv_ignore_files = abap_true.
      remove_ignored_files( CHANGING ct_files = rt_files ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_key.
    rv_key = ms_data-key.
  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_local_settings.

    rs_settings = ms_data-local_settings.

  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_name.

    rv_name = ms_data-local_settings-display_name.

  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_package.
    rv_package = ms_data-package.
  ENDMETHOD.
  METHOD Lif_abapgit_repo~is_offline.
    rv_offline = ms_data-offline.
  ENDMETHOD.
  METHOD Lif_abapgit_repo~refresh.

    mv_request_local_refresh = abap_true.
    reset_remote( ).

    IF iv_drop_log = abap_true.
      CLEAR mi_log.
    ENDIF.

    IF iv_drop_cache = abap_true.
      CLEAR mt_local.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_repo~set_dot_abapgit.
    set( is_dot_abapgit = io_dot_abapgit->get_data( ) ).
  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_files_local_filtered.

    DATA lo_serialize TYPE REF TO Lcl_abapgit_serialize.
    DATA lt_filter TYPE Lif_abapgit_definitions=>ty_tadir_tt.


    CREATE OBJECT lo_serialize
      EXPORTING
        io_dot_abapgit    = get_dot_abapgit( )
        is_local_settings = get_local_settings( ).

    lt_filter = ii_obj_filter->get_filter( ).

    rt_files = lo_serialize->files_local(
      iv_package     = get_package( )
      ii_data_config = get_data_config( )
      ii_log         = ii_log
      it_filter      = lt_filter ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_REPO implementation

*>>>>>>> ZCL_ABAPGIT_REPO_CHECKSUMS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_repo_checksums====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_repo_checksums====ccimp.
CLASS SHRIS5ZPAUXVKEPN5HWETLLAS6UBTU DEFINITION
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_splitter TYPE string VALUE `|`.
    CONSTANTS c_root TYPE string VALUE `@`.

    CLASS-METHODS serialize
      IMPORTING
        it_checksums     TYPE Lif_abapgit_persistence=>ty_local_checksum_tt
      RETURNING
        VALUE(rv_string) TYPE string.

    CLASS-METHODS deserialize
      IMPORTING
        iv_string           TYPE string
      RETURNING
        VALUE(rt_checksums) TYPE Lif_abapgit_persistence=>ty_local_checksum_tt.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS SHRIS5ZPAUXVKEPN5HWETLLAS6UBTU IMPLEMENTATION.


  METHOD deserialize.

    DATA lt_buf_tab TYPE string_table.
    DATA lv_buf TYPE string.
    DATA lt_checksums LIKE rt_checksums.

    FIELD-SYMBOLS <ls_cs> LIKE LINE OF lt_checksums.
    FIELD-SYMBOLS <ls_file> LIKE LINE OF <ls_cs>-files.

    SPLIT iv_string AT |\n| INTO TABLE lt_buf_tab.

    LOOP AT lt_buf_tab INTO lv_buf.
      CHECK lv_buf IS NOT INITIAL. " In fact this is a bug ... it cannot be empty, maybe raise

      IF lv_buf+0(1) = '/'.
        IF <ls_cs> IS NOT ASSIGNED.
          " Incorrect checksums structure, maybe raise, though it is not critical for execution
          RETURN.
        ENDIF.

        APPEND INITIAL LINE TO <ls_cs>-files ASSIGNING <ls_file>.
        SPLIT lv_buf AT c_splitter INTO <ls_file>-path <ls_file>-filename <ls_file>-sha1.

        IF <ls_file>-path IS INITIAL OR <ls_file>-filename IS INITIAL OR <ls_file>-sha1 IS INITIAL.
          " Incorrect checksums struture, maybe raise, though it is not critical for execution
          RETURN.
        ENDIF.
      ELSEIF lv_buf = c_root. " Root
        APPEND INITIAL LINE TO lt_checksums ASSIGNING <ls_cs>. " Empty item
      ELSE.
        APPEND INITIAL LINE TO lt_checksums ASSIGNING <ls_cs>.
        SPLIT lv_buf AT c_splitter INTO <ls_cs>-item-obj_type <ls_cs>-item-obj_name <ls_cs>-item-devclass.

        IF <ls_cs>-item-obj_type IS INITIAL OR <ls_cs>-item-obj_name IS INITIAL OR <ls_cs>-item-devclass IS INITIAL.
          " Incorrect checksums structure, maybe raise, though it is not critical for execution
          RETURN.
        ENDIF.

      ENDIF.
    ENDLOOP.

    rt_checksums = lt_checksums.

  ENDMETHOD.


  METHOD serialize.

    DATA lt_buf_tab TYPE string_table.
    DATA lv_buf TYPE string.
    DATA lt_checksums_sorted TYPE Lif_abapgit_persistence=>ty_local_checksum_by_item_tt.

    FIELD-SYMBOLS <ls_cs> LIKE LINE OF it_checksums.
    FIELD-SYMBOLS <ls_file> LIKE LINE OF <ls_cs>-files.

    lt_checksums_sorted = it_checksums.

    LOOP AT lt_checksums_sorted ASSIGNING <ls_cs>.

      IF lines( <ls_cs>-files ) = 0.
        CONTINUE.
      ENDIF.

      IF <ls_cs>-item-obj_type IS NOT INITIAL.
        CONCATENATE <ls_cs>-item-obj_type <ls_cs>-item-obj_name <ls_cs>-item-devclass
          INTO lv_buf
          SEPARATED BY c_splitter.
      ELSE.
        lv_buf = c_root.
      ENDIF.
      APPEND lv_buf TO lt_buf_tab.

      LOOP AT <ls_cs>-files ASSIGNING <ls_file>.

        CONCATENATE <ls_file>-path <ls_file>-filename <ls_file>-sha1
          INTO lv_buf
          SEPARATED BY c_splitter.
        APPEND lv_buf TO lt_buf_tab.

      ENDLOOP.

    ENDLOOP.

    rv_string = concat_lines_of(
      table = lt_buf_tab
      sep   = |\n| ).

  ENDMETHOD.
ENDCLASS.

**********************************************************************
* UPDATE CALCULATOR
**********************************************************************

CLASS SHRIS5ZPAUXVKEPN5HWETLLAS6WBTU DEFINITION
  FINAL
  CREATE PUBLIC.
  PUBLIC SECTION.

    CLASS-METHODS calculate_updated
      IMPORTING
        it_updated_files TYPE Lif_abapgit_git_definitions=>ty_file_signatures_tt
        it_current_checksums TYPE Lif_abapgit_persistence=>ty_local_checksum_tt
        it_local_files TYPE Lif_abapgit_definitions=>ty_files_item_tt
      RETURNING
        VALUE(rt_checksums) TYPE Lif_abapgit_persistence=>ty_local_checksum_tt.

  PRIVATE SECTION.

    CLASS-METHODS process_updated_files
      CHANGING
        ct_update_index TYPE Lif_abapgit_git_definitions=>ty_file_signatures_ts
        ct_checksums    TYPE Lif_abapgit_persistence=>ty_local_checksum_by_item_tt.

    CLASS-METHODS add_new_files
      IMPORTING
        it_local        TYPE Lif_abapgit_definitions=>ty_files_item_tt
        it_update_index TYPE Lif_abapgit_git_definitions=>ty_file_signatures_ts
      CHANGING
        ct_checksums    TYPE Lif_abapgit_persistence=>ty_local_checksum_by_item_tt.

ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLAS6WBTU IMPLEMENTATION.

  METHOD calculate_updated.

    DATA lt_update_index TYPE Lif_abapgit_git_definitions=>ty_file_signatures_ts.
    DATA lt_checksums_sorted TYPE Lif_abapgit_persistence=>ty_local_checksum_by_item_tt.

    lt_checksums_sorted = it_current_checksums.
    lt_update_index     = it_updated_files.

    process_updated_files(
      CHANGING
        ct_update_index = lt_update_index
        ct_checksums    = lt_checksums_sorted ).

    add_new_files(
      EXPORTING
        it_update_index = lt_update_index
        it_local        = it_local_files
      CHANGING
        ct_checksums    = lt_checksums_sorted ).

    rt_checksums = lt_checksums_sorted.

  ENDMETHOD.

  METHOD process_updated_files.

    DATA lv_cs_row  TYPE i.
    DATA lv_file_row  TYPE i.

    FIELD-SYMBOLS <ls_checksum>  LIKE LINE OF ct_checksums.
    FIELD-SYMBOLS <ls_file>      LIKE LINE OF <ls_checksum>-files.
    FIELD-SYMBOLS <ls_new_state> LIKE LINE OF ct_update_index.

    " Loop through current checksum state, update sha1 for common files

    LOOP AT ct_checksums ASSIGNING <ls_checksum>.
      lv_cs_row = sy-tabix.

      LOOP AT <ls_checksum>-files ASSIGNING <ls_file>.
        lv_file_row = sy-tabix.

        READ TABLE ct_update_index ASSIGNING <ls_new_state>
          WITH KEY
            path     = <ls_file>-path
            filename = <ls_file>-filename.
        IF sy-subrc <> 0.
          CONTINUE. " Missing in updated files -> nothing to update, skip
        ENDIF.

        IF <ls_new_state>-sha1 IS INITIAL. " Empty input sha1 is a deletion marker
          DELETE <ls_checksum>-files INDEX lv_file_row.
        ELSE.
          <ls_file>-sha1 = <ls_new_state>-sha1.  " Update sha1
          CLEAR <ls_new_state>-sha1.             " Mark as processed
        ENDIF.
      ENDLOOP.

      IF lines( <ls_checksum>-files ) = 0. " Remove empty objects
        DELETE ct_checksums INDEX lv_cs_row.
      ENDIF.
    ENDLOOP.

    DELETE ct_update_index WHERE sha1 IS INITIAL. " Remove processed

  ENDMETHOD.

  METHOD add_new_files.

    DATA lt_local_sorted TYPE Lif_abapgit_definitions=>ty_files_item_by_file_tt.
    DATA ls_checksum LIKE LINE OF ct_checksums.
    FIELD-SYMBOLS <ls_checksum> LIKE LINE OF ct_checksums.
    FIELD-SYMBOLS <ls_new_file> LIKE LINE OF it_update_index.
    FIELD-SYMBOLS <ls_local>    LIKE LINE OF lt_local_sorted.

    lt_local_sorted = it_local.

    " Add new files - not deleted and not marked as processed
    LOOP AT it_update_index ASSIGNING <ls_new_file>.

      READ TABLE lt_local_sorted ASSIGNING <ls_local>
        WITH KEY
          file-path     = <ls_new_file>-path
          file-filename = <ls_new_file>-filename.
      IF sy-subrc <> 0.
        " The file should be in locals, however:
        " if the deserialization fails, the local file might not be there
        " in this case no new CS added, and the file will appear to be remote+new
        CONTINUE.
      ENDIF.

      READ TABLE ct_checksums ASSIGNING <ls_checksum>
        WITH KEY
          item-obj_type = <ls_local>-item-obj_type
          item-obj_name = <ls_local>-item-obj_name.
      IF sy-subrc <> 0.
        MOVE-CORRESPONDING <ls_local>-item TO ls_checksum-item.
        INSERT ls_checksum INTO TABLE ct_checksums ASSIGNING <ls_checksum>.
      ENDIF.

      APPEND <ls_new_file> TO <ls_checksum>-files.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

*"* test class
*include zcl_abapgit_repo_checksums====ccau.
**********************************************************************
* SERIALIZER
**********************************************************************



**********************************************************************
* CHECKSUMS
**********************************************************************


**********************************************************************
* HELPERS
**********************************************************************

CLASS SHRIS5ZPAUXVKEPN5HWETLLAS63BTU DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_repo.
    INTERFACES Lif_abapgit_repo_srv.
    DATA mt_local_files TYPE Lif_abapgit_definitions=>ty_files_item_tt.
    DATA mt_remote_files TYPE Lif_abapgit_git_definitions=>ty_files_tt.
ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLAS63BTU IMPLEMENTATION.

  METHOD Lif_abapgit_repo_srv~get.
    IF iv_key = '1'.
      ri_repo = me.
    ENDIF.
  ENDMETHOD.

  METHOD Lif_abapgit_repo~get_files_local_filtered.
  ENDMETHOD.

  METHOD Lif_abapgit_repo~get_files_local.
    rt_files = mt_local_files.
  ENDMETHOD.

  METHOD Lif_abapgit_repo~get_files_remote.
    rt_files = mt_remote_files.
  ENDMETHOD.

  METHOD Lif_abapgit_repo~get_key.
    rv_key = '1'.
  ENDMETHOD.

  METHOD Lif_abapgit_repo~get_name.
    rv_name = 'test'.
  ENDMETHOD.
  METHOD Lif_abapgit_repo~checksums.
  ENDMETHOD.

  METHOD Lif_abapgit_repo_srv~init.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~delete.
  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_local_settings.
  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_package.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~get_repo_from_package.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~get_repo_from_url.
  ENDMETHOD.
  METHOD Lif_abapgit_repo~is_offline.
  ENDMETHOD.
  METHOD Lif_abapgit_repo~deserialize.
  ENDMETHOD.
  METHOD Lif_abapgit_repo~deserialize_checks.
  ENDMETHOD.
  METHOD Lif_abapgit_repo~set_dot_abapgit.
  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_dot_abapgit.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~is_repo_installed.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~list.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~list_favorites.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~new_offline.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~new_online.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~purge.
  ENDMETHOD.
  METHOD Lif_abapgit_repo~refresh.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~validate_package.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~validate_url.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~get_label_list.
  ENDMETHOD.

ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLAS65BTU DEFINITION FINAL.
  PUBLIC SECTION.
    DATA mt_tab TYPE Lif_abapgit_definitions=>ty_files_item_tt.
    METHODS add IMPORTING iv_str TYPE string.
ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLAS65BTU IMPLEMENTATION.
  METHOD add.
    DATA ls_item LIKE LINE OF mt_tab.
    DATA lv_tmp TYPE string.
    lv_tmp = iv_str.
    CONDENSE lv_tmp.
    SPLIT lv_tmp AT space INTO
      ls_item-item-devclass
      ls_item-item-obj_type
      ls_item-item-obj_name
      ls_item-file-path
      ls_item-file-filename
      ls_item-file-sha1.
    IF ls_item-item-devclass = '@'. " Root items
      CLEAR: ls_item-item-devclass, ls_item-item-obj_type, ls_item-item-obj_name.
    ENDIF.
    APPEND ls_item TO mt_tab.
  ENDMETHOD.
ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLAS67BTU DEFINITION FINAL.
  PUBLIC SECTION.
    DATA mt_tab TYPE Lif_abapgit_git_definitions=>ty_files_tt.
    METHODS add IMPORTING iv_str TYPE string.
ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLAS67BTU IMPLEMENTATION.
  METHOD add.
    DATA ls_item LIKE LINE OF mt_tab.
    DATA lv_tmp TYPE string.
    lv_tmp = iv_str.
    CONDENSE lv_tmp.
    SPLIT lv_tmp AT space INTO
      ls_item-path
      ls_item-filename
      ls_item-sha1.
    APPEND ls_item TO mt_tab.
  ENDMETHOD.
ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLAS7BBTU DEFINITION FINAL.
  PUBLIC SECTION.
    DATA mt_tab TYPE Lif_abapgit_git_definitions=>ty_file_signatures_tt.
    METHODS add IMPORTING iv_str TYPE string.
ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLAS7BBTU IMPLEMENTATION.
  METHOD add.
    DATA ls_item LIKE LINE OF mt_tab.
    DATA lv_tmp TYPE string.
    lv_tmp = iv_str.
    CONDENSE lv_tmp.
    SPLIT lv_tmp AT space INTO
      ls_item-path
      ls_item-filename
      ls_item-sha1.
    APPEND ls_item TO mt_tab.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
* CHECKSUMS UT
**********************************************************************


**********************************************************************
* UPDATE CALCULATOR
**********************************************************************



class LCL_ABAPGIT_REPO_CHECKSUMS implementation.
*"* method's implementations
*include methods.
  METHOD add_meta.

    DATA lv_meta_str TYPE string.

    lv_meta_str = |#repo_name#{ mi_repo->get_name( ) }|.

    cv_cs_blob = lv_meta_str && |\n| && cv_cs_blob.

  ENDMETHOD.
  METHOD build_checksums_from_files.

    DATA ls_last_item TYPE Lif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS:
      <ls_checksum>    LIKE LINE OF rt_checksums,
      <ls_local>       LIKE LINE OF it_local,
      <ls_cs_file_sig> LIKE LINE OF <ls_checksum>-files.

    " This methods is run at repo creation moment or manually by user
    " In the first case it assumes that the local state is the CURRENT state
    " Thus the idea is to copy local state to checksums
    " The second case is an exception, when we acknoledge that the state is unknown
    " Thus copying the local to checksums is the "best guess"

    LOOP AT it_local ASSIGNING <ls_local>.
      IF ls_last_item <> <ls_local>-item OR sy-tabix = 1. " First or New item reached ?
        APPEND INITIAL LINE TO rt_checksums ASSIGNING <ls_checksum>.
        MOVE-CORRESPONDING <ls_local>-item TO <ls_checksum>-item.
        ls_last_item       = <ls_local>-item.
      ENDIF.

      APPEND INITIAL LINE TO <ls_checksum>-files ASSIGNING <ls_cs_file_sig>.
      MOVE-CORRESPONDING <ls_local>-file TO <ls_cs_file_sig>.

    ENDLOOP.

  ENDMETHOD.
  METHOD constructor.
    ASSERT iv_repo_key IS NOT INITIAL.
    mv_repo_key = iv_repo_key.
    mi_repo = Lcl_abapgit_repo_srv=>get_instance( )->get( mv_repo_key ).
    " Should be safe as repo_srv is supposed to be single source of repo instances
  ENDMETHOD.
  METHOD extract_meta.

    DATA lv_meta_str TYPE string.

    IF cv_cs_blob+0(1) <> '#'.
      RETURN. " No meta ? just ignore it
    ENDIF.

    SPLIT cv_cs_blob AT |\n| INTO lv_meta_str cv_cs_blob.
    " Just remove the header meta string - this is OK for now.
    " There is just repo name for the moment - needed to for DB util and potential debug

  ENDMETHOD.
  METHOD force_write.

    " for migration only for the moment

    save_checksums( it_checksums ).

  ENDMETHOD.
  METHOD remove_non_code_related_files.

    DELETE ct_local_files
      WHERE item IS INITIAL
      AND NOT ( file-path = Lif_abapgit_definitions=>c_root_dir
      AND file-filename = Lif_abapgit_definitions=>c_dot_abapgit ).

  ENDMETHOD.
  METHOD save_checksums.

    DATA lv_cs_blob TYPE string.

    lv_cs_blob = SHRIS5ZPAUXVKEPN5HWETLLAS6UBTU=>serialize( it_checksums ).
    add_meta( CHANGING cv_cs_blob = lv_cs_blob ).
    Lcl_abapgit_persist_factory=>get_repo_cs( )->update(
      iv_key     = mv_repo_key
      iv_cs_blob = lv_cs_blob ).

  ENDMETHOD.
  METHOD Lif_abapgit_repo_checksums~get.

    DATA lv_cs_blob TYPE string.

    TRY.
        lv_cs_blob = Lcl_abapgit_persist_factory=>get_repo_cs( )->read( mv_repo_key ).
      CATCH Lcx_abapgit_exception Lcx_abapgit_not_found.
        " Ignore currently, it's not critical for execution, just return empty
        RETURN.
    ENDTRY.

    IF lv_cs_blob IS NOT INITIAL.
      extract_meta( CHANGING cv_cs_blob = lv_cs_blob ).
      rt_checksums = SHRIS5ZPAUXVKEPN5HWETLLAS6UBTU=>deserialize( lv_cs_blob ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_checksums~get_checksums_per_file.

    DATA lt_checksums   TYPE Lif_abapgit_persistence=>ty_local_checksum_tt.
    FIELD-SYMBOLS <ls_object> LIKE LINE OF lt_checksums.

    lt_checksums = Lif_abapgit_repo_checksums~get( ).

    LOOP AT lt_checksums ASSIGNING <ls_object>.
      APPEND LINES OF <ls_object>-files TO rt_checksums.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_checksums~rebuild.

    DATA lt_local     TYPE ty_local_files_by_item_tt.
    DATA lt_checksums TYPE Lif_abapgit_persistence=>ty_local_checksum_tt.

    lt_local  = mi_repo->get_files_local( ).
    remove_non_code_related_files( CHANGING ct_local_files = lt_local ).

    lt_checksums = build_checksums_from_files( lt_local ).
    save_checksums( lt_checksums ).

  ENDMETHOD.
  METHOD Lif_abapgit_repo_checksums~update.

    DATA lt_checksums   TYPE Lif_abapgit_persistence=>ty_local_checksum_tt.
    DATA lt_local_files TYPE Lif_abapgit_definitions=>ty_files_item_tt.

    lt_checksums   = Lif_abapgit_repo_checksums~get( ).
    lt_local_files = mi_repo->get_files_local( ).

    lt_checksums = SHRIS5ZPAUXVKEPN5HWETLLAS6WBTU=>calculate_updated(
      it_current_checksums = lt_checksums
      it_local_files       = lt_local_files
      it_updated_files     = it_updated_files ).

    save_checksums( lt_checksums ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_REPO_CHECKSUMS implementation

*>>>>>>> ZCL_ABAPGIT_REPO_CONTENT_LIST <<<<<<<*

*"* macro definitions
*include zcl_abapgit_repo_content_list=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_repo_content_list=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_REPO_CONTENT_LIST implementation.
*"* method's implementations
*include methods.
  METHOD build_folders.

    DATA: lv_index    TYPE i,
          lt_subitems LIKE ct_repo_items,
          ls_subitem  LIKE LINE OF ct_repo_items,
          ls_folder   LIKE LINE OF ct_repo_items.

    DATA lo_state TYPE REF TO Lcl_abapgit_item_state.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF ct_repo_items.


    LOOP AT ct_repo_items ASSIGNING <ls_item>.
      lv_index = sy-tabix.
      CHECK <ls_item>-path <> iv_cur_dir. " files in target dir - just leave them be

      IF Lcl_abapgit_path=>is_subdir( iv_path = <ls_item>-path
                                      iv_parent = iv_cur_dir ) = abap_true.
        ls_subitem-changes = <ls_item>-changes.
        ls_subitem-path    = <ls_item>-path.
        ls_subitem-lstate  = <ls_item>-lstate.
        ls_subitem-rstate  = <ls_item>-rstate.
        APPEND ls_subitem TO lt_subitems.
      ENDIF.

      DELETE ct_repo_items INDEX lv_index.
    ENDLOOP.

    SORT lt_subitems BY path ASCENDING.

    LOOP AT lt_subitems ASSIGNING <ls_item>.
      AT NEW path.
        CLEAR ls_folder.
        ls_folder-path    = <ls_item>-path.
        ls_folder-sortkey = c_sortkey-dir. " Directory
        ls_folder-is_dir  = abap_true.
        CREATE OBJECT lo_state.
      ENDAT.

      ls_folder-changes = ls_folder-changes + <ls_item>-changes.
      lo_state->sum_with_repo_item( <ls_item> ).

      AT END OF path.
        ls_folder-lstate = lo_state->local( ).
        ls_folder-rstate = lo_state->remote( ).
        APPEND ls_folder TO ct_repo_items.
      ENDAT.
    ENDLOOP.

  ENDMETHOD.
  METHOD build_repo_items_local_only.

    DATA: lt_tadir TYPE Lif_abapgit_definitions=>ty_tadir_tt,
          ls_item  TYPE Lif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS: <ls_repo_item> LIKE LINE OF rt_repo_items,
                   <ls_tadir>     LIKE LINE OF lt_tadir.


    lt_tadir = Lcl_abapgit_factory=>get_tadir( )->read(
      iv_package            = mo_repo->get_package( )
      iv_ignore_subpackages = mo_repo->get_local_settings( )-ignore_subpackages
      iv_only_local_objects = mo_repo->get_local_settings( )-only_local_objects
      io_dot                = mo_repo->get_dot_abapgit( ) ).

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      APPEND INITIAL LINE TO rt_repo_items ASSIGNING <ls_repo_item>.
      <ls_repo_item>-obj_type  = <ls_tadir>-object.
      <ls_repo_item>-obj_name  = <ls_tadir>-obj_name.
      <ls_repo_item>-path      = <ls_tadir>-path.
      <ls_repo_item>-srcsystem = <ls_tadir>-srcsystem.
      MOVE-CORRESPONDING <ls_repo_item> TO ls_item.
      <ls_repo_item>-inactive = boolc( Lcl_abapgit_objects=>is_active( ls_item ) = abap_false ).
      IF <ls_repo_item>-inactive = abap_true.
        <ls_repo_item>-sortkey = c_sortkey-inactive.
      ELSE.
        <ls_repo_item>-sortkey = c_sortkey-default.      " Default sort key
      ENDIF.

      IF <ls_repo_item>-obj_type IS NOT INITIAL.
        MOVE-CORRESPONDING <ls_repo_item> TO ls_item.
        IF Lcl_abapgit_objects=>exists( ls_item ) = abap_true.
          <ls_repo_item>-changed_by = Lcl_abapgit_objects=>changed_by( ls_item ).
        ENDIF.
        CLEAR ls_item.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD build_repo_items_with_remote.

    DATA:
      lo_state      TYPE REF TO Lcl_abapgit_item_state,
      ls_file       TYPE Lif_abapgit_definitions=>ty_repo_file,
      lt_status     TYPE Lif_abapgit_definitions=>ty_results_tt,
      ls_item       TYPE Lif_abapgit_definitions=>ty_item,
      ls_previous   LIKE ls_item,
      lv_changed_by TYPE string.

    FIELD-SYMBOLS: <ls_status>    LIKE LINE OF lt_status,
                   <ls_repo_item> LIKE LINE OF rt_repo_items.


    lt_status = Lcl_abapgit_repo_status=>calculate(
      io_repo = mo_repo
      ii_log  = mi_log ).

    LOOP AT lt_status ASSIGNING <ls_status>.
      AT NEW obj_name. "obj_type + obj_name
        APPEND INITIAL LINE TO rt_repo_items ASSIGNING <ls_repo_item>.
        <ls_repo_item>-obj_type  = <ls_status>-obj_type.
        <ls_repo_item>-obj_name  = <ls_status>-obj_name.
        <ls_repo_item>-inactive  = <ls_status>-inactive.
        <ls_repo_item>-sortkey   = c_sortkey-default. " Default sort key
        <ls_repo_item>-changes   = 0.
        <ls_repo_item>-path      = <ls_status>-path.
        <ls_repo_item>-srcsystem = <ls_status>-srcsystem.
        CREATE OBJECT lo_state.
      ENDAT.

      IF <ls_status>-filename IS NOT INITIAL.
        MOVE-CORRESPONDING <ls_status> TO ls_file.
        ls_file-is_changed = boolc( <ls_status>-match = abap_false ). " TODO refactor
        APPEND ls_file TO <ls_repo_item>-files.

        IF <ls_status>-inactive = abap_true AND <ls_repo_item>-sortkey > c_sortkey-changed.
          <ls_repo_item>-sortkey = c_sortkey-inactive.
        ENDIF.

        IF ls_file-is_changed = abap_true.
          <ls_repo_item>-sortkey = c_sortkey-changed. " Changed files
          <ls_repo_item>-changes = <ls_repo_item>-changes + 1.
          lo_state->sum_with_status_item( <ls_status> ).
        ENDIF.
      ENDIF.

      IF <ls_repo_item>-changes > 0 AND <ls_repo_item>-obj_type IS NOT INITIAL.
        MOVE-CORRESPONDING <ls_repo_item> TO ls_item.
        IF ls_previous = ls_item.
          <ls_repo_item>-changed_by = lv_changed_by.
        ELSEIF Lcl_abapgit_objects=>exists( ls_item ) = abap_true.
          <ls_repo_item>-changed_by = Lcl_abapgit_objects=>changed_by( ls_item ).
          ls_previous = ls_item.
          lv_changed_by = <ls_repo_item>-changed_by.
        ENDIF.
        CLEAR ls_item.
      ENDIF.

      AT END OF obj_name. "obj_type + obj_name
        IF <ls_repo_item>-obj_type IS INITIAL.
          <ls_repo_item>-sortkey = c_sortkey-orphan. "Virtual objects
        ENDIF.
        <ls_repo_item>-lstate = lo_state->local( ).
        <ls_repo_item>-rstate = lo_state->remote( ).
        <ls_repo_item>-packmove = lo_state->is_reassigned( ).
      ENDAT.
    ENDLOOP.

  ENDMETHOD.
  METHOD check_repo_size.

    CONSTANTS lc_new_repo_size TYPE i VALUE 10.

    DATA lt_remote TYPE Lif_abapgit_git_definitions=>ty_files_tt.

    lt_remote = mo_repo->get_files_remote( ).

    IF lines( lt_remote ) > lc_new_repo_size.
      " Less files means it's a new repo (with just readme and license, for example) which is ok
      READ TABLE lt_remote TRANSPORTING NO FIELDS
        WITH KEY file_path
        COMPONENTS path     = Lif_abapgit_definitions=>c_root_dir
                   filename = Lif_abapgit_definitions=>c_dot_abapgit.
      IF sy-subrc <> 0.
        mi_log->add_warning( |Cannot find .abapgit.xml - Is this an abapGit repository?| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD constructor.
    mo_repo = io_repo.
    CREATE OBJECT mi_log TYPE Lcl_abapgit_log.
  ENDMETHOD.
  METHOD determine_transports.

    DATA ls_item TYPE Lif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF ct_repo_items.

    LOOP AT ct_repo_items ASSIGNING <ls_item>.
      ls_item-obj_type = <ls_item>-obj_type.
      ls_item-obj_name = <ls_item>-obj_name.
      TRY.
          <ls_item>-transport = Lcl_abapgit_factory=>get_cts_api( )->get_transport_for_object( ls_item ).
        CATCH Lcx_abapgit_exception ##NO_HANDLER.
          " Ignore errors related to object check when trying to get transport
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.
  METHOD filter_changes.

    FIELD-SYMBOLS: <ls_item> TYPE Lif_abapgit_definitions=>ty_repo_item.

    DELETE ct_repo_items WHERE changes = 0 AND inactive = abap_false.
    LOOP AT ct_repo_items ASSIGNING <ls_item> WHERE inactive = abap_false.
      DELETE <ls_item>-files WHERE is_changed = abap_false.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_log.
    DATA li_repo_log TYPE REF TO Lif_abapgit_log.
    DATA lt_repo_msg TYPE Lif_abapgit_log=>ty_log_outs.
    DATA lr_repo_msg TYPE REF TO Lif_abapgit_log=>ty_log_out.

    ri_log = mi_log.

    "add warning and error messages from repo log
    li_repo_log = mo_repo->get_log( ).
    IF li_repo_log IS BOUND.
      lt_repo_msg = li_repo_log->get_messages( ).
      LOOP AT lt_repo_msg REFERENCE INTO lr_repo_msg WHERE type CA 'EW'.
        CASE lr_repo_msg->type.
          WHEN 'E'.
            ri_log->add_error( lr_repo_msg->text ).
          WHEN 'W'.
            ri_log->add_warning( lr_repo_msg->text ).
          WHEN OTHERS.
            CONTINUE.
        ENDCASE.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD list.

    mi_log->clear( ).

    IF mo_repo->has_remote_source( ) = abap_true.
      rt_repo_items = build_repo_items_with_remote( ).
      check_repo_size( ).
    ELSE.
      rt_repo_items = build_repo_items_local_only( ).
    ENDIF.

    IF iv_by_folders = abap_true.
      build_folders(
        EXPORTING iv_cur_dir    = iv_path
        CHANGING  ct_repo_items = rt_repo_items ).
    ENDIF.

    IF iv_changes_only = abap_true.
      " There are never changes for offline repositories
      filter_changes( CHANGING ct_repo_items = rt_repo_items ).
    ENDIF.

    IF iv_transports = abap_true.
      determine_transports( CHANGING ct_repo_items = rt_repo_items ).
    ENDIF.

    SORT rt_repo_items BY
      sortkey ASCENDING
      path ASCENDING
      obj_name ASCENDING.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_REPO_CONTENT_LIST implementation

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

*>>>>>>> ZCL_ABAPGIT_REPO_OFFLINE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_repo_offline======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_repo_offline======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_REPO_OFFLINE implementation.
*"* method's implementations
*include methods.
  METHOD has_remote_source.
    rv_yes = boolc( lines( mt_remote ) > 0 ).
  ENDMETHOD.
  METHOD reset_remote.

    DATA lt_backup LIKE mt_remote.

    " online repo has online source to renew data from, offline does not
    " so offline repo preserves the remote
    " in case of partial pull failure the user will immediately see the new difference
    " UI will detect "pullable" content based on mt_status
    " in the uniform way both for online and offline repos
    " for more details see discussion in 2096 and 1953

    lt_backup = mt_remote.
    super->reset_remote( ).
    set_files_remote( lt_backup ).

  ENDMETHOD.
  METHOD set_name.
    set( iv_url = iv_url ).
  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_name.
    rv_name = super->get_name( ).

    IF rv_name IS INITIAL.
      rv_name = ms_data-url.
    ENDIF.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_REPO_OFFLINE implementation

*>>>>>>> ZCL_ABAPGIT_REPO_ONLINE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_repo_online=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_repo_online=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_REPO_ONLINE implementation.
*"* method's implementations
*include methods.
  METHOD fetch_remote.

    DATA: li_progress TYPE REF TO Lif_abapgit_progress,
          ls_pull     TYPE Lcl_abapgit_git_porcelain=>ty_pull_result.

    IF mv_request_remote_refresh = abap_false.
      RETURN.
    ENDIF.

    li_progress = Lcl_abapgit_progress=>get_instance( 1 ).

    li_progress->show( iv_current = 1
                       iv_text    = 'Fetch remote files' ).

    IF get_selected_commit( ) IS INITIAL.
      ls_pull = Lcl_abapgit_git_porcelain=>pull_by_branch( iv_url         = get_url( )
                                                           iv_branch_name = get_selected_branch( ) ).
    ELSE.
      ls_pull = Lcl_abapgit_git_porcelain=>pull_by_commit( iv_url         = get_url( )
                                                           iv_commit_hash = get_selected_commit( ) ).
    ENDIF.

    set_files_remote( ls_pull-files ).
    set_objects( ls_pull-objects ).
    mv_current_commit = ls_pull-commit.

  ENDMETHOD.
  METHOD get_objects.
    fetch_remote( ).
    rt_objects = mt_objects.
  ENDMETHOD.
  METHOD handle_stage_ignore.

    DATA: lv_add         TYPE abap_bool,
          lo_dot_abapgit TYPE REF TO Lcl_abapgit_dot_abapgit,
          lt_stage       TYPE Lif_abapgit_definitions=>ty_stage_tt.

    FIELD-SYMBOLS: <ls_stage> LIKE LINE OF lt_stage.


    lo_dot_abapgit = get_dot_abapgit( ).
    lt_stage = io_stage->get_all( ).
    LOOP AT lt_stage ASSIGNING <ls_stage> WHERE method = Lif_abapgit_definitions=>c_method-ignore.

      lo_dot_abapgit->add_ignore(
        iv_path     = <ls_stage>-file-path
        iv_filename = <ls_stage>-file-filename ).

      " remove it from the staging object, as the action is handled here
      io_stage->reset( iv_path     = <ls_stage>-file-path
                       iv_filename = <ls_stage>-file-filename ).

      lv_add = abap_true.

    ENDLOOP.

    IF lv_add = abap_true.
      io_stage->add(
        iv_path     = Lif_abapgit_definitions=>c_root_dir
        iv_filename = Lif_abapgit_definitions=>c_dot_abapgit
        iv_data     = lo_dot_abapgit->serialize( ) ).

      set_dot_abapgit( lo_dot_abapgit ).
    ENDIF.

  ENDMETHOD.
  METHOD has_remote_source.
    rv_yes = abap_true.
  ENDMETHOD.
  METHOD raise_error_if_branch_exists.

    DATA:
      lt_branches     TYPE Lif_abapgit_git_definitions=>ty_git_branch_list_tt,
      lv_display_name TYPE string.

    lt_branches = Lcl_abapgit_git_transport=>branches( get_url( ) )->get_branches_only( ).

    READ TABLE lt_branches WITH TABLE KEY name_key
                           COMPONENTS name = iv_name
                           TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      lv_display_name = Lcl_abapgit_git_branch_list=>get_display_name( iv_name ).
      Lcx_abapgit_exception=>raise( |Branch '{ lv_display_name }' already exists| ).
    ENDIF.

  ENDMETHOD.
  METHOD set_objects.
    mt_objects = it_objects.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_online~check_for_valid_branch.

    DATA:
      lo_branch_list TYPE REF TO Lcl_abapgit_git_branch_list,
      lv_branch      TYPE string,
      lv_head        TYPE string,
      lv_msg         TYPE string.

    lv_branch = get_selected_branch( ).

    IF lv_branch IS NOT INITIAL.
      lo_branch_list = Lcl_abapgit_git_transport=>branches( get_url( ) ).

      TRY.
          lo_branch_list->find_by_name( lv_branch ).
        CATCH Lcx_abapgit_exception.
          " branch does not exist, fallback to head
          lv_head = lo_branch_list->get_head_symref( ).
          IF lo_branch_list->get_type( lv_branch ) = Lif_abapgit_git_definitions=>c_git_branch_type-branch.
            lv_msg = 'Branch'.
          ELSE.
            lv_msg = 'Tag'.
          ENDIF.
          lv_msg = |{ lv_msg } { lo_branch_list->get_display_name( lv_branch ) } does not exist.|
                && | Switched to { lo_branch_list->get_display_name( lv_head ) }|.
          MESSAGE lv_msg TYPE 'S'.
          select_branch( lv_head ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_online~create_branch.

    DATA: lv_sha1 TYPE Lif_abapgit_git_definitions=>ty_sha1.

    ASSERT iv_name CP Lif_abapgit_git_definitions=>c_git_branch-heads.

    IF iv_from IS INITIAL.
      lv_sha1 = get_current_remote( ).
    ELSE.
      lv_sha1 = iv_from.
    ENDIF.

    raise_error_if_branch_exists( iv_name ).

    Lcl_abapgit_git_porcelain=>create_branch(
      iv_url  = get_url( )
      iv_name = iv_name
      iv_from = lv_sha1 ).

    " automatically switch to new branch
    select_branch( iv_name ).

  ENDMETHOD.
  METHOD Lif_abapgit_repo_online~get_current_remote.
    fetch_remote( ).
    rv_sha1 = mv_current_commit.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_online~get_selected_branch.
    rv_name = ms_data-branch_name.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_online~get_selected_commit.
    rv_selected_commit = ms_data-selected_commit.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_online~get_switched_origin.
    rv_switched_origin = ms_data-switched_origin.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_online~get_url.
    rv_url = ms_data-url.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_online~push.

* assumption: PUSH is done on top of the currently selected branch

    DATA: ls_push   TYPE Lcl_abapgit_git_porcelain=>ty_push_result,
          lv_text   TYPE string,
          lv_parent TYPE Lif_abapgit_git_definitions=>ty_sha1.


    IF ms_data-branch_name CP Lif_abapgit_git_definitions=>c_git_branch-tags.
      lv_text = |You're working on a tag. Currently it's not |
             && |possible to push on tags. Consider creating a branch instead|.
      Lcx_abapgit_exception=>raise( lv_text ).
    ENDIF.

    IF ms_data-selected_commit IS NOT INITIAL.
      lv_text = 'You are currently checked out in a commit.'.
      lv_text = |{ lv_text } You must be on a branch to push|.
      Lcx_abapgit_exception=>raise( lv_text ).
    ENDIF.

    IF ms_data-local_settings-block_commit = abap_true
        AND Lcl_abapgit_factory=>get_code_inspector( get_package( )
          )->is_successful( ) = abap_false.
      Lcx_abapgit_exception=>raise( |A successful code inspection is required| ).
    ENDIF.

    handle_stage_ignore( io_stage ).

    IF get_selected_commit( ) IS INITIAL.
      lv_parent = get_current_remote( ).
    ELSE.
      lv_parent = get_selected_commit( ).
    ENDIF.

    ls_push = Lcl_abapgit_git_porcelain=>push(
      is_comment     = is_comment
      io_stage       = io_stage
      iv_branch_name = get_selected_branch( )
      iv_url         = get_url( )
      iv_parent      = lv_parent
      it_old_objects = get_objects( ) ).

    set_objects( ls_push-new_objects ).
    set_files_remote( ls_push-new_files ).

    mv_current_commit = ls_push-branch.

    Lif_abapgit_repo~checksums( )->update( ls_push-updated_files ).

  ENDMETHOD.
  METHOD Lif_abapgit_repo_online~select_branch.

    reset_remote( ).
    set( iv_branch_name     = iv_branch_name
         iv_selected_commit = space ).

  ENDMETHOD.
  METHOD Lif_abapgit_repo_online~select_commit.

    reset_remote( ).
    set( iv_selected_commit = iv_selected_commit ).

  ENDMETHOD.
  METHOD Lif_abapgit_repo_online~set_url.

    reset_remote( ).
    set( iv_url = iv_url ).

  ENDMETHOD.
  METHOD Lif_abapgit_repo_online~switch_origin.

    DATA lv_offs TYPE i.

    " For repo settings page
    IF iv_overwrite = abap_true.
      set( iv_switched_origin = iv_url ).
      RETURN.
    ENDIF.

    IF iv_url IS INITIAL.
      IF ms_data-switched_origin IS INITIAL.
        RETURN.
      ELSE.
        lv_offs = find(
          val = reverse( ms_data-switched_origin )
          sub = '@' ).
        IF lv_offs = -1.
          Lcx_abapgit_exception=>raise( 'Incorrect format of switched origin' ).
        ENDIF.
        lv_offs = strlen( ms_data-switched_origin ) - lv_offs - 1.
        set_url( substring(
          val = ms_data-switched_origin
          len = lv_offs ) ).
        select_branch( substring(
          val = ms_data-switched_origin
          off = lv_offs + 1 ) ).
        set( iv_switched_origin = '' ).
      ENDIF.
    ELSEIF ms_data-switched_origin IS INITIAL.
      set( iv_switched_origin = ms_data-url && '@' && ms_data-branch_name ).
      set_url( iv_url ).
      select_branch( iv_branch ).
    ELSE.
      Lcx_abapgit_exception=>raise( 'Cannot switch origin twice' ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_files_remote.
    fetch_remote( ).
    rt_files = super->get_files_remote(
      ii_obj_filter   = ii_obj_filter
      iv_ignore_files = iv_ignore_files ).
  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_name.
    rv_name = super->get_name( ).
    IF rv_name IS INITIAL.
      TRY.
          rv_name = Lcl_abapgit_url=>name( ms_data-url ).
          rv_name = cl_http_utility=>unescape_url( rv_name ).
        CATCH Lcx_abapgit_exception.
          rv_name = 'New online repo'. "unlikely fallback
      ENDTRY.
    ENDIF.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_REPO_ONLINE implementation

*>>>>>>> ZCL_ABAPGIT_REPO_SRV <<<<<<<*

*"* macro definitions
*include zcl_abapgit_repo_srv==========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_repo_srv==========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_REPO_SRV implementation.
*"* method's implementations
*include methods.
  METHOD add.

    DATA li_repo LIKE LINE OF mt_list.
    DATA lo_repo TYPE REF TO Lcl_abapgit_repo.

    LOOP AT mt_list INTO li_repo.
      IF li_repo->ms_data-key = ii_repo->ms_data-key.
        IF li_repo = ii_repo.
          RETURN.
        ENDIF.
        Lcx_abapgit_exception=>raise( 'identical keys' ).
      ENDIF.
    ENDLOOP.

    lo_repo ?= ii_repo. " TODO, refactor later
    lo_repo->bind_listener( me ).
    APPEND ii_repo TO mt_list.

  ENDMETHOD.
  METHOD determine_branch_name.

    DATA lo_branch_list TYPE REF TO Lcl_abapgit_git_branch_list.

    rv_name = iv_name.
    IF rv_name IS INITIAL.
      ASSERT NOT iv_url IS INITIAL.
      lo_branch_list = Lcl_abapgit_git_transport=>branches( iv_url ).
      rv_name = lo_branch_list->get_head_symref( ).
    ELSEIF -1 = find(
        val = rv_name
        sub = Lif_abapgit_git_definitions=>c_git_branch-heads_prefix ).
      " Assume short branch name was received
      rv_name = Lif_abapgit_git_definitions=>c_git_branch-heads_prefix && rv_name.
    ENDIF.

  ENDMETHOD.
  METHOD get_instance.
    IF gi_ref IS INITIAL.
      CREATE OBJECT gi_ref TYPE Lcl_abapgit_repo_srv.
    ENDIF.
    ri_srv = gi_ref.
  ENDMETHOD.
  METHOD inject_instance.
    gi_ref = ii_srv.
  ENDMETHOD.
  METHOD instantiate_and_add.

    IF is_repo_meta-offline = abap_false.
      CREATE OBJECT ri_repo TYPE Lcl_abapgit_repo_online
        EXPORTING
          is_data = is_repo_meta.
    ELSE.
      CREATE OBJECT ri_repo TYPE Lcl_abapgit_repo_offline
        EXPORTING
          is_data = is_repo_meta.
    ENDIF.
    add( ri_repo ).

  ENDMETHOD.
  METHOD refresh_all.

    DATA: lt_list TYPE Lif_abapgit_persistence=>ty_repos.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF lt_list.

    CLEAR mt_list.

    lt_list = Lcl_abapgit_persist_factory=>get_repo( )->list( ).
    LOOP AT lt_list ASSIGNING <ls_list>.
      instantiate_and_add( <ls_list> ).
    ENDLOOP.

    mv_init = abap_true.
    mv_only_favorites = abap_false.

  ENDMETHOD.
  METHOD refresh_favorites.

    DATA: lt_list           TYPE Lif_abapgit_persistence=>ty_repos,
          lt_user_favorites TYPE Lif_abapgit_persist_user=>ty_favorites.

    DATA li_repo TYPE REF TO Lif_abapgit_repo.
    DATA lv_repo_index TYPE i.
    DATA lo_repo_db TYPE REF TO Lif_abapgit_persist_repo.

    FIELD-SYMBOLS: <ls_repo_record> LIKE LINE OF lt_list.

    lo_repo_db        = Lcl_abapgit_persist_factory=>get_repo( ).
    lt_user_favorites = Lcl_abapgit_persistence_user=>get_instance( )->get_favorites( ).
    lt_list           = lo_repo_db->list_by_keys( lt_user_favorites ).

    SORT lt_list BY package.

    LOOP AT mt_list INTO li_repo.
      lv_repo_index = sy-tabix.

      READ TABLE lt_list TRANSPORTING NO FIELDS WITH KEY package = li_repo->get_package( ).
      IF sy-subrc = 0.
        DELETE lt_list INDEX sy-tabix.
        CONTINUE. " Leave the repo be
      ELSEIF lo_repo_db->exists( li_repo->get_key( ) ) = abap_false.
        " Not a fav, and also does not exist, probably uninstalled
        DELETE mt_list INDEX lv_repo_index.
      ENDIF.

    ENDLOOP.

    " Create remaining (new) favs
    LOOP AT lt_list ASSIGNING <ls_repo_record>.
      instantiate_and_add( <ls_repo_record> ).
    ENDLOOP.

    mv_init = abap_true.
    mv_only_favorites = abap_true.

  ENDMETHOD.
  METHOD reinstantiate_repo.

    DATA li_repo      TYPE REF TO Lif_abapgit_repo.
    DATA ls_full_meta TYPE Lif_abapgit_persistence=>ty_repo.

    li_repo = Lif_abapgit_repo_srv~get( iv_key ).
    DELETE TABLE mt_list FROM li_repo.
    ASSERT sy-subrc IS INITIAL.

    MOVE-CORRESPONDING is_meta TO ls_full_meta.
    ls_full_meta-key = iv_key.

    instantiate_and_add( ls_full_meta ).

  ENDMETHOD.
  METHOD validate_sub_super_packages.

    DATA:
      ls_repo     LIKE LINE OF it_repos,
      li_package  TYPE REF TO Lif_abapgit_sap_package,
      lt_packages TYPE Lif_abapgit_sap_package=>ty_devclass_tt,
      li_repo     TYPE REF TO Lif_abapgit_repo.

    LOOP AT it_repos INTO ls_repo.
      li_repo = Lif_abapgit_repo_srv~get( ls_repo-key ).

      li_package = Lcl_abapgit_factory=>get_sap_package( ls_repo-package ).
      IF li_package->exists( ) = abap_false.
        " Skip dangling repository
        CONTINUE.
      ENDIF.

      CLEAR lt_packages.
      IF li_repo->get_local_settings( )-ignore_subpackages = abap_false.
        APPEND LINES OF li_package->list_subpackages( ) TO lt_packages.
        READ TABLE lt_packages TRANSPORTING NO FIELDS
          WITH KEY table_line = iv_package.
        IF sy-subrc = 0.
          ei_repo = li_repo.
          ev_reason = |Repository { li_repo->get_name( ) } already contains { iv_package } |.
          RETURN.
        ENDIF.
      ENDIF.

      IF iv_ign_subpkg = abap_false.
        APPEND LINES OF li_package->list_superpackages( ) TO lt_packages.
        READ TABLE lt_packages TRANSPORTING NO FIELDS
          WITH KEY table_line = iv_package.
        IF sy-subrc = 0.
          ei_repo = li_repo.
          ev_reason = |Repository { li_repo->get_name( ) } already contains subpackage of { iv_package } |.
          RETURN.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_listener~on_meta_change.

    DATA li_persistence TYPE REF TO Lif_abapgit_persist_repo.

    li_persistence = Lcl_abapgit_persist_factory=>get_repo( ).
    li_persistence->update_metadata(
      iv_key         = iv_key
      is_meta        = is_meta
      is_change_mask = is_change_mask ).


    " Recreate repo instance if type changed
    " Instances in mt_list are of *_online and *_offline type
    " If type is changed object should be recreated from the proper class
    " TODO refactor, e.g. unify repo logic in one class
    IF is_change_mask-offline = abap_true.
      reinstantiate_repo(
        iv_key  = iv_key
        is_meta = is_meta ).

    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~delete.

    Lcl_abapgit_persist_factory=>get_repo( )->delete( ii_repo->get_key( ) ).
    Lcl_abapgit_persist_factory=>get_repo_cs( )->delete( ii_repo->get_key( ) ).

    " If favorite, remove it
    IF Lcl_abapgit_persistence_user=>get_instance( )->is_favorite_repo( ii_repo->get_key( ) ) = abap_true.
      Lcl_abapgit_persistence_user=>get_instance( )->toggle_favorite( ii_repo->get_key( ) ).
    ENDIF.

    DELETE TABLE mt_list FROM ii_repo.
    ASSERT sy-subrc = 0.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~get.

    FIELD-SYMBOLS: <li_repo> LIKE LINE OF mt_list.

    ASSERT iv_key IS NOT INITIAL.

    IF mv_init = abap_false.
      refresh_all( ).
    ENDIF.

    DO 2 TIMES.
      " Repo might have been created in another session. Try again after refresh
      IF sy-index = 2.
        refresh_all( ).
      ENDIF.
      LOOP AT mt_list ASSIGNING <li_repo>.
        IF <li_repo>->ms_data-key = iv_key.
          ri_repo = <li_repo>.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDDO.

    Lcx_abapgit_exception=>raise( |Repository not found in database. Key: REPO, { iv_key }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~get_label_list.

    DATA:
      lt_repo           TYPE Lif_abapgit_repo_srv=>ty_repo_list,
      ls_local_settings TYPE Lif_abapgit_persistence=>ty_repo-local_settings,
      lt_labels         TYPE string_table,
      ls_label          LIKE LINE OF rt_labels.

    FIELD-SYMBOLS:
      <ls_repo>  TYPE REF TO Lif_abapgit_repo,
      <lv_label> TYPE LINE OF string_table.

    lt_repo = Lif_abapgit_repo_srv~list( ).

    LOOP AT lt_repo ASSIGNING <ls_repo>.

      ls_local_settings = <ls_repo>->get_local_settings( ).
      lt_labels = Lcl_abapgit_repo_labels=>split( ls_local_settings-labels ).

      LOOP AT lt_labels ASSIGNING <lv_label>.
        ls_label-label = <lv_label>.
        INSERT ls_label INTO TABLE rt_labels.
      ENDLOOP.

    ENDLOOP.

    SORT rt_labels.
    DELETE ADJACENT DUPLICATES FROM rt_labels.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~get_repo_from_package.

    DATA:
      lt_repos TYPE Lif_abapgit_persistence=>ty_repos,
      lv_name  TYPE Lif_abapgit_persistence=>ty_local_settings-display_name,
      lv_owner TYPE Lif_abapgit_persistence=>ty_local_settings-display_name.

    FIELD-SYMBOLS:
      <ls_repo> LIKE LINE OF lt_repos.

    " check if package is already in use for a different repository
    lt_repos = Lcl_abapgit_persist_factory=>get_repo( )->list( ).
    READ TABLE lt_repos WITH KEY package = iv_package ASSIGNING <ls_repo>.
    IF sy-subrc = 0.
      ei_repo = get_instance( )->get( <ls_repo>-key ).
      lv_name = ei_repo->get_name( ).
      lv_owner = <ls_repo>-created_by.
      ev_reason = |Package { iv_package } already versioned as { lv_name } by { lv_owner }|.
    ELSE.
      " check if package is include as sub-package in a different repo
      validate_sub_super_packages(
        EXPORTING
          iv_package    = iv_package
          it_repos      = lt_repos
          iv_ign_subpkg = iv_ign_subpkg
        IMPORTING
          ei_repo       = ei_repo
          ev_reason     = ev_reason ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~get_repo_from_url.

    DATA:
      lt_repos                TYPE Lif_abapgit_persistence=>ty_repos,
      lv_current_repo_address TYPE string,
      lv_check_repo_address   TYPE string,
      lv_repo_path            TYPE string,
      lv_name                 TYPE Lif_abapgit_persistence=>ty_local_settings-display_name,
      lv_owner                TYPE Lif_abapgit_persistence=>ty_local_settings-display_name.

    FIELD-SYMBOLS:
      <ls_repo> LIKE LINE OF lt_repos.

    CLEAR:
      ei_repo, ev_reason.

    lv_current_repo_address = Lcl_abapgit_url=>url_address( iv_url ).

    " check if url is already in use for a different package
    lt_repos = Lcl_abapgit_persist_factory=>get_repo( )->list( ).
    LOOP AT lt_repos ASSIGNING <ls_repo> WHERE offline = abap_false.

      lv_check_repo_address = Lcl_abapgit_url=>url_address( <ls_repo>-url ).

      IF lv_current_repo_address = lv_check_repo_address.
        ei_repo      = get_instance( )->get( <ls_repo>-key ).
        lv_repo_path = Lcl_abapgit_url=>path_name( iv_url ).
        lv_name      = ei_repo->get_name( ).
        lv_owner     = <ls_repo>-created_by.
        ev_reason    = |Repository { lv_repo_path } already versioned as { lv_name } by { lv_owner }|.
        RETURN.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~init.
    CLEAR mv_init.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~is_repo_installed.

    DATA: lt_repo        TYPE Lif_abapgit_repo_srv=>ty_repo_list,
          li_repo        TYPE REF TO Lif_abapgit_repo,
          lv_url         TYPE string,
          lv_package     TYPE devclass,
          lo_repo_online TYPE REF TO Lcl_abapgit_repo_online,
          lv_err         TYPE string.

    lt_repo = Lif_abapgit_repo_srv~list( ).

    LOOP AT lt_repo INTO li_repo.
      CHECK li_repo->is_offline( ) = abap_false.
      lo_repo_online ?= li_repo.

      lv_url     = lo_repo_online->get_url( ).
      lv_package = lo_repo_online->get_package( ).
      CHECK to_upper( lv_url ) = to_upper( iv_url ).

      " Validate bindings
      "TODO refactor: move this message out of this method
      IF iv_target_package IS NOT INITIAL AND iv_target_package <> lv_package.
        lv_err = |Installation to package { lv_package } detected. |
              && |Cancelling installation|.
        Lcx_abapgit_exception=>raise( lv_err ).
      ENDIF.

      rv_installed = abap_true.
      EXIT.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~list.

    DATA li_repo TYPE REF TO Lif_abapgit_repo.

    IF mv_init = abap_false OR mv_only_favorites = abap_true.
      refresh_all( ).
    ENDIF.

    LOOP AT mt_list INTO li_repo.
      IF iv_offline = abap_undefined OR li_repo->is_offline( ) = iv_offline.
        INSERT li_repo INTO TABLE rt_list.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~list_favorites.

    DATA lt_user_favorites TYPE Lif_abapgit_persist_user=>ty_favorites.
    DATA li_repo TYPE REF TO Lif_abapgit_repo.

    lt_user_favorites = Lcl_abapgit_persistence_user=>get_instance( )->get_favorites( ).
    SORT lt_user_favorites BY table_line.

    IF mv_init = abap_false OR mv_only_favorites = abap_false.
      refresh_favorites( ).
    ENDIF.

    LOOP AT mt_list INTO li_repo.
      READ TABLE lt_user_favorites
        TRANSPORTING NO FIELDS
        WITH KEY table_line = li_repo->get_key( ).
      IF sy-subrc = 0 AND ( iv_offline = abap_undefined OR li_repo->is_offline( ) = iv_offline ).
        APPEND li_repo TO rt_list.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~new_offline.

    DATA: ls_repo        TYPE Lif_abapgit_persistence=>ty_repo,
          lv_key         TYPE Lif_abapgit_persistence=>ty_repo-key,
          lo_repo        TYPE REF TO Lcl_abapgit_repo_offline,
          lo_dot_abapgit TYPE REF TO Lcl_abapgit_dot_abapgit.


    IF Lcl_abapgit_auth=>is_allowed( Lif_abapgit_auth=>c_authorization-create_repo ) = abap_false.
      Lcx_abapgit_exception=>raise( 'Not authorized' ).
    ENDIF.

    Lif_abapgit_repo_srv~validate_package(
      iv_package    = iv_package
      iv_ign_subpkg = iv_ign_subpkg ).

    IF iv_url IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Missing display name for repo' ).
    ENDIF.

    " Repo Settings
    lo_dot_abapgit = Lcl_abapgit_dot_abapgit=>build_default( ).
    lo_dot_abapgit->set_folder_logic( iv_folder_logic ).
    lo_dot_abapgit->set_abap_language_version( iv_abap_lang_vers ).

    lv_key = Lcl_abapgit_persist_factory=>get_repo( )->add(
      iv_url          = iv_url
      iv_branch_name  = ''
      iv_package      = iv_package
      iv_offline      = abap_true
      is_dot_abapgit  = lo_dot_abapgit->get_data( ) ).

    TRY.
        ls_repo = Lcl_abapgit_persist_factory=>get_repo( )->read( lv_key ).
      CATCH Lcx_abapgit_not_found.
        Lcx_abapgit_exception=>raise( 'new_offline not found' ).
    ENDTRY.

    lo_repo ?= instantiate_and_add( ls_repo ).

    " Local Settings
    IF ls_repo-local_settings-ignore_subpackages <> iv_ign_subpkg.
      ls_repo-local_settings-ignore_subpackages = iv_ign_subpkg.
    ENDIF.
    ls_repo-local_settings-main_language_only = iv_main_lang_only.
    ls_repo-local_settings-labels = iv_labels.

    lo_repo->set_local_settings( ls_repo-local_settings ).

    ri_repo = lo_repo.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~new_online.

    DATA: ls_repo        TYPE Lif_abapgit_persistence=>ty_repo,
          lo_repo        TYPE REF TO Lcl_abapgit_repo_online,
          lv_branch_name LIKE iv_branch_name,
          lv_key         TYPE Lif_abapgit_persistence=>ty_repo-key,
          lo_dot_abapgit TYPE REF TO Lcl_abapgit_dot_abapgit,
          lv_url         TYPE string.


    ASSERT NOT iv_url IS INITIAL
      AND NOT iv_package IS INITIAL.

    lv_url = condense( iv_url ).

    IF Lcl_abapgit_auth=>is_allowed( Lif_abapgit_auth=>c_authorization-create_repo ) = abap_false.
      Lcx_abapgit_exception=>raise( 'Not authorized' ).
    ENDIF.

    Lif_abapgit_repo_srv~validate_package(
      iv_package    = iv_package
      iv_ign_subpkg = iv_ign_subpkg ).

    Lif_abapgit_repo_srv~validate_url( lv_url ).

    lv_branch_name = determine_branch_name(
      iv_name = iv_branch_name
      iv_url  = lv_url ).

    " Repo Settings
    lo_dot_abapgit = Lcl_abapgit_dot_abapgit=>build_default( ).
    lo_dot_abapgit->set_folder_logic( iv_folder_logic ).
    lo_dot_abapgit->set_abap_language_version( iv_abap_lang_vers ).

    lv_key = Lcl_abapgit_persist_factory=>get_repo( )->add(
      iv_url          = lv_url
      iv_branch_name  = lv_branch_name " local !
      iv_display_name = iv_display_name
      iv_package      = iv_package
      iv_offline      = abap_false
      is_dot_abapgit  = lo_dot_abapgit->get_data( ) ).

    TRY.
        ls_repo = Lcl_abapgit_persist_factory=>get_repo( )->read( lv_key ).
      CATCH Lcx_abapgit_not_found.
        Lcx_abapgit_exception=>raise( 'new_online not found' ).
    ENDTRY.

    lo_repo ?= instantiate_and_add( ls_repo ).

    " Local Settings
    IF ls_repo-local_settings-ignore_subpackages <> iv_ign_subpkg.
      ls_repo-local_settings-ignore_subpackages = iv_ign_subpkg.
    ENDIF.
    ls_repo-local_settings-main_language_only = iv_main_lang_only.
    ls_repo-local_settings-labels = iv_labels.

    lo_repo->set_local_settings( ls_repo-local_settings ).

    lo_repo->refresh( ).
    lo_repo->find_remote_dot_abapgit( ).

    ri_repo = lo_repo.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~purge.

* uninstalls all objects, no UI or popups in this class

* todo, this should be a method on the repo instead?

    DATA: lt_tadir TYPE Lif_abapgit_definitions=>ty_tadir_tt.
    DATA: lx_error TYPE REF TO Lcx_abapgit_exception.
    DATA lo_repo TYPE REF TO Lcl_abapgit_repo.

    lo_repo ?= ii_repo. " TODO, remove later
    ri_log = lo_repo->create_new_log( 'Uninstall Log' ).

    IF ii_repo->get_local_settings( )-write_protected = abap_true.
      Lcx_abapgit_exception=>raise( 'Cannot purge. Local code is write-protected by repo config' ).
    ELSEIF Lcl_abapgit_auth=>is_allowed( Lif_abapgit_auth=>c_authorization-uninstall ) = abap_false.
      Lcx_abapgit_exception=>raise( 'Not authorized' ).
    ENDIF.

    lt_tadir = Lcl_abapgit_factory=>get_tadir( )->read( ii_repo->get_package( ) ).

    TRY.
        Lcl_abapgit_objects=>delete( it_tadir  = lt_tadir
                                     is_checks = is_checks
                                     ii_log    = ri_log ).
      CATCH Lcx_abapgit_exception INTO lx_error.
        " If uninstall fails, repo needs a refresh to show which objects where deleted and which not
        ii_repo->refresh( iv_drop_log = abap_false ).
        RAISE EXCEPTION lx_error.
    ENDTRY.

    IF iv_keep_repo = abap_true.
      ii_repo->refresh( ).
      ii_repo->checksums( )->rebuild( ).
    ELSE.
      Lif_abapgit_repo_srv~delete( ii_repo ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~validate_package.

    DATA: lv_as4user TYPE usnam,
          li_repo    TYPE REF TO Lif_abapgit_repo,
          lv_reason  TYPE string.

    Lcl_abapgit_factory=>get_sap_package( iv_package )->validate_name( ).

    " Check if package owned by SAP is allowed (new packages are ok, since they are created automatically)
    lv_as4user = Lcl_abapgit_factory=>get_sap_package( iv_package )->read_responsible( ).

    IF sy-subrc = 0 AND lv_as4user = 'SAP' AND
      Lcl_abapgit_factory=>get_environment( )->is_sap_object_allowed( ) = abap_false.
      Lcx_abapgit_exception=>raise( |Package { iv_package } not allowed, responsible user = 'SAP'| ).
    ENDIF.

    " Check if package is already used in another repo
    IF iv_chk_exists = abap_true.
      Lif_abapgit_repo_srv~get_repo_from_package(
        EXPORTING
          iv_package    = iv_package
          iv_ign_subpkg = iv_ign_subpkg
        IMPORTING
          ei_repo       = li_repo
          ev_reason     = lv_reason ).

      IF li_repo IS BOUND.
        Lcx_abapgit_exception=>raise( lv_reason ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~validate_url.

    DATA:
      li_repo   TYPE REF TO Lif_abapgit_repo,
      lv_reason TYPE string.

    Lcl_abapgit_url=>validate( iv_url ).

    IF iv_chk_exists = abap_true.
      Lif_abapgit_repo_srv~get_repo_from_url(
        EXPORTING
          iv_url    = iv_url
        IMPORTING
          ei_repo   = li_repo
          ev_reason = lv_reason ).
      IF li_repo IS BOUND.
        Lcx_abapgit_exception=>raise( lv_reason ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_REPO_SRV implementation

*>>>>>>> ZCL_ABAPGIT_REPO_STATUS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_repo_status=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_repo_status=======ccimp.
CLASS SHRIS5ZPAUXVKEPN5HWETLLAS7GBTU DEFINITION FINAL.
  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        iv_root_package TYPE devclass
        io_dot          TYPE REF TO Lcl_abapgit_dot_abapgit.

    METHODS run_checks
      IMPORTING
        it_results    TYPE Lif_abapgit_definitions=>ty_results_tt
      RETURNING
        VALUE(ri_log) TYPE REF TO Lif_abapgit_log
      RAISING
        Lcx_abapgit_exception .

  PRIVATE SECTION.

    DATA mv_root_package TYPE devclass.
    DATA mo_dot          TYPE REF TO Lcl_abapgit_dot_abapgit.
    DATA mi_log          TYPE REF TO Lif_abapgit_log.

    METHODS check_package_move
      IMPORTING
        !it_results TYPE Lif_abapgit_definitions=>ty_results_tt
      RAISING
        Lcx_abapgit_exception .
    METHODS check_files_folder
      IMPORTING
        !it_results TYPE Lif_abapgit_definitions=>ty_results_tt
      RAISING
        Lcx_abapgit_exception .
    METHODS check_package_sub_package
      IMPORTING
        !it_results TYPE Lif_abapgit_definitions=>ty_results_tt
        !iv_top     TYPE devclass
      RAISING
        Lcx_abapgit_exception .
    METHODS check_package_folder
      IMPORTING
        !it_results TYPE Lif_abapgit_definitions=>ty_results_tt
        !io_dot     TYPE REF TO Lcl_abapgit_dot_abapgit
        !iv_top     TYPE devclass
      RAISING
        Lcx_abapgit_exception .
    METHODS check_multiple_files
      IMPORTING
        !it_results TYPE Lif_abapgit_definitions=>ty_results_tt
      RAISING
        Lcx_abapgit_exception .
    METHODS check_namespace
      IMPORTING
        !it_results      TYPE Lif_abapgit_definitions=>ty_results_tt
      RAISING
        Lcx_abapgit_exception .

ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLAS7GBTU IMPLEMENTATION.

  METHOD constructor.
    mv_root_package = iv_root_package.
    mo_dot          = io_dot.
  ENDMETHOD.

  METHOD run_checks.

    CREATE OBJECT mi_log TYPE Lcl_abapgit_log.

    " Find all objects which were assigned to a different package
    check_package_move( it_results ).

    " Check files for one object is in the same folder
    check_files_folder( it_results ).

    " Check that sub packages are included in the package hierarchy
    check_package_sub_package(
      it_results = it_results
      iv_top     = mv_root_package ).

    " Check that objects are created in package corresponding to folder
    check_package_folder(
      it_results = it_results
      io_dot     = mo_dot
      iv_top     = mv_root_package ).

    " Check for multiple files with same filename
    check_multiple_files( it_results ).

    " Check if namespaces exist already
    check_namespace( it_results ).

    ri_log = mi_log.

  ENDMETHOD.

  METHOD check_files_folder.

    DATA:
      ls_item     TYPE Lif_abapgit_definitions=>ty_item,
      lt_res_sort LIKE it_results,
      lt_item_idx LIKE it_results.

    FIELD-SYMBOLS:
      <ls_result>     LIKE LINE OF it_results,
      <ls_result_idx> LIKE LINE OF it_results.

    " TODO optimize ?
    " sort by obj, path
    " loop, and compare to first object record

    " Collect object index
    lt_res_sort = it_results.
    SORT lt_res_sort BY obj_type ASCENDING obj_name ASCENDING.

    LOOP AT it_results ASSIGNING <ls_result> WHERE NOT obj_type IS INITIAL AND packmove = abap_false.

      IF NOT ( <ls_result>-obj_type = ls_item-obj_type
          AND <ls_result>-obj_name = ls_item-obj_name ).
        APPEND INITIAL LINE TO lt_item_idx ASSIGNING <ls_result_idx>.
        <ls_result_idx>-obj_type = <ls_result>-obj_type.
        <ls_result_idx>-obj_name = <ls_result>-obj_name.
        <ls_result_idx>-path     = <ls_result>-path.
        MOVE-CORRESPONDING <ls_result> TO ls_item.
      ENDIF.

    ENDLOOP.

    LOOP AT it_results ASSIGNING <ls_result>
      WHERE NOT obj_type IS INITIAL AND obj_type <> 'DEVC' AND packmove = abap_false.

      READ TABLE lt_item_idx ASSIGNING <ls_result_idx>
        WITH KEY obj_type = <ls_result>-obj_type obj_name = <ls_result>-obj_name
        BINARY SEARCH. " Sorted above

      IF sy-subrc <> 0 OR <ls_result>-path <> <ls_result_idx>-path. " All paths are same
        mi_log->add_warning( |Files for object { <ls_result>-obj_type } { <ls_result>-obj_name }|
         && | are not placed in the same folder| ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD check_multiple_files.

    DATA:
      lt_res_sort LIKE it_results,
      ls_file     TYPE Lif_abapgit_git_definitions=>ty_file_signature.

    FIELD-SYMBOLS <ls_result> LIKE LINE OF it_results.

    lt_res_sort = it_results.
    SORT lt_res_sort BY filename ASCENDING.

    LOOP AT lt_res_sort ASSIGNING <ls_result> WHERE obj_type <> 'DEVC' AND packmove = abap_false.
      IF <ls_result>-filename IS NOT INITIAL AND <ls_result>-filename = ls_file-filename.
        mi_log->add_warning( |Multiple files with same filename, { <ls_result>-filename }| ).
      ENDIF.

      IF <ls_result>-filename IS INITIAL.
        mi_log->add_warning( |Filename is empty for object { <ls_result>-obj_type } { <ls_result>-obj_name }| ).
      ENDIF.

      MOVE-CORRESPONDING <ls_result> TO ls_file.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_namespace.

    DATA:
      li_namespace TYPE REF TO Lif_abapgit_sap_namespace,
      lv_namespace TYPE namespace,
      lt_namespace TYPE TABLE OF namespace.

    FIELD-SYMBOLS <ls_result> LIKE LINE OF it_results.

    " Collect all namespaces based on name of xml- and json-files
    LOOP AT it_results ASSIGNING <ls_result>.
      FIND REGEX '^#([a-zA-Z0-9]+)#.*\..*\.xml$' IN <ls_result>-filename SUBMATCHES lv_namespace.
      IF sy-subrc = 0.
        lv_namespace = '/' && to_upper( lv_namespace ) && '/'.
        COLLECT lv_namespace INTO lt_namespace.
      ENDIF.
      FIND REGEX '^\(([a-zA-Z0-9]+)\).*\..*\.json$' IN <ls_result>-filename SUBMATCHES lv_namespace.
      IF sy-subrc = 0.
        lv_namespace = '/' && to_upper( lv_namespace ) && '/'.
        COLLECT lv_namespace INTO lt_namespace.
      ENDIF.
    ENDLOOP.

    li_namespace = Lcl_abapgit_factory=>get_sap_namespace( ).

    LOOP AT lt_namespace INTO lv_namespace.
      IF li_namespace->exists( lv_namespace ) = abap_false.
        mi_log->add_warning( |Namespace { lv_namespace } does not exist.|
          && | Pull it first (or create it in transaction SE03)| ).
      ELSEIF li_namespace->is_editable( lv_namespace ) = abap_false.
        mi_log->add_warning( |Namespace { lv_namespace } is not modifiable. Check it in transaction SE03| ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_package_folder.

    DATA:
      lv_path         TYPE string,
      lv_object       TYPE string,
      lo_folder_logic TYPE REF TO Lcl_abapgit_folder_logic.

    FIELD-SYMBOLS <ls_result> LIKE LINE OF it_results.

    lo_folder_logic = Lcl_abapgit_folder_logic=>get_instance( ).

    LOOP AT it_results ASSIGNING <ls_result>
      WHERE NOT package IS INITIAL AND NOT path IS INITIAL AND packmove = abap_false.

      lv_path = lo_folder_logic->package_to_path(
        iv_top     = iv_top
        io_dot     = io_dot
        iv_package = <ls_result>-package ).

      lv_object = |{ <ls_result>-obj_type } { <ls_result>-obj_name }|.

      IF lv_path IS INITIAL.
        mi_log->add_error( |{ lv_object } already exists outside of { iv_top } package hierarchy| ).
      ELSEIF lv_path <> <ls_result>-path.
        mi_log->add_warning( |Package and path do not match for object { lv_object }| ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD check_package_move.

    DATA lt_move_idx LIKE it_results.

    FIELD-SYMBOLS:
      <ls_result>      LIKE LINE OF it_results,
      <ls_result_move> LIKE LINE OF it_results.

    " TODO: optimize ?
    " delete where packmove = false, delete adj duplicates and fire messages ?
    LOOP AT it_results ASSIGNING <ls_result>
      WHERE lstate = Lif_abapgit_definitions=>c_state-added AND packmove = abap_true.

      READ TABLE lt_move_idx TRANSPORTING NO FIELDS
        WITH KEY
          obj_type = <ls_result>-obj_type
          obj_name = <ls_result>-obj_name
        BINARY SEARCH. " Sorted since it_result is sorted
      IF sy-subrc <> 0.
        mi_log->add_warning( |Changed package assignment for object|
          && | { <ls_result>-obj_type } { <ls_result>-obj_name }| ).
        APPEND INITIAL LINE TO lt_move_idx ASSIGNING <ls_result_move>.
        <ls_result_move>-obj_type = <ls_result>-obj_type.
        <ls_result_move>-obj_name = <ls_result>-obj_name.
        <ls_result_move>-path     = <ls_result>-path.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD check_package_sub_package.

    DATA lv_msg TYPE string.

    FIELD-SYMBOLS <ls_result> LIKE LINE OF it_results.

    LOOP AT it_results ASSIGNING <ls_result> USING KEY sec_key
                       WHERE package IS INITIAL AND obj_type = 'DEVC'.

      IF Lcl_abapgit_factory=>get_sap_package( |{ <ls_result>-obj_name }| )->exists( ) = abap_true.
        " If package already exist but is not included in the package hierarchy of
        " the package assigned to the repository, then a manual change of the package
        " is required i.e. setting a parent package to the repo package (or one of its
        " subpackages). We don't do this automatically since it's not clear where in the
        " hierarchy the new package should be located or whether the sub package shall be
        " removed from the repo.
        lv_msg = |Package { <ls_result>-obj_name } already exists but is not a sub-package of { iv_top }. |
              && |Check your package and folder logic, and either assign { <ls_result>-obj_name } |
              && |to the package hierarchy of { iv_top } or remove package { <ls_result>-obj_name } |
              && |from the repository.|.
        mi_log->add_warning( lv_msg ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

*"* test class
*include zcl_abapgit_repo_status=======ccau.
*CLASS SHRIS5ZPAUXVKEPN5HWETLLAS7IBTU DEFINITION DEFERRED.
*CLASS zcl_abapgit_repo_status DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLAS7IBTU.





CLASS SHRIS5ZPAUXVKEPN5HWETLLAS7MBTU DEFINITION.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          it_results TYPE Lif_abapgit_definitions=>ty_results_tt,
      get_line
        IMPORTING
          iv_line        TYPE i
        RETURNING
          VALUE(rs_data) TYPE Lif_abapgit_definitions=>ty_result,
      assert_lines
        IMPORTING
          iv_lines TYPE i
          iv_msg   TYPE csequence OPTIONAL.

  PRIVATE SECTION.
    DATA: mt_results TYPE Lif_abapgit_definitions=>ty_results_tt.

ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLAS7MBTU IMPLEMENTATION.

  METHOD constructor.

    mt_results = it_results.
    SORT mt_results BY path filename.

  ENDMETHOD.

  METHOD get_line.

    READ TABLE mt_results INDEX iv_line INTO rs_data.
    cl_abap_unit_assert=>assert_subrc( ).

  ENDMETHOD.

  METHOD assert_lines.

    cl_abap_unit_assert=>assert_equals(
      act = lines( mt_results )
      exp = iv_lines
      msg = iv_msg ).

  ENDMETHOD.

ENDCLASS.

*CLASS SHRIS5ZPAUXVKEPN5HWETLLAS7OBTU DEFINITION DEFERRED.
*CLASS zcl_abapgit_repo_status DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLAS7OBTU.



*CLASS SHRIS5ZPAUXVKEPN5HWETLLAS7QBTU DEFINITION DEFERRED.
*CLASS zcl_abapgit_repo_status DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLAS7QBTU.



class LCL_ABAPGIT_REPO_STATUS implementation.
*"* method's implementations
*include methods.
  METHOD build_existing.

    DATA ls_file_sig LIKE LINE OF it_state.

    " Item
    rs_result-obj_type  = is_local-item-obj_type.
    rs_result-obj_name  = is_local-item-obj_name.
    rs_result-package   = is_local-item-devclass.
    rs_result-srcsystem = is_local-item-srcsystem.
    rs_result-origlang  = is_local-item-origlang.
    rs_result-inactive  = is_local-item-inactive.

    " File
    rs_result-path     = is_local-file-path.
    rs_result-filename = is_local-file-filename.

    rs_result-match    = boolc( is_local-file-sha1 = is_remote-sha1 ).
    IF rs_result-match = abap_true.
      RETURN.
    ENDIF.

    " Match against current state
    READ TABLE it_state INTO ls_file_sig
      WITH KEY
        path     = is_local-file-path
        filename = is_local-file-filename
      BINARY SEARCH.

    IF sy-subrc = 0.
      IF ls_file_sig-sha1 <> is_local-file-sha1.
        rs_result-lstate = Lif_abapgit_definitions=>c_state-modified.
      ENDIF.
      IF ls_file_sig-sha1 <> is_remote-sha1.
        rs_result-rstate = Lif_abapgit_definitions=>c_state-modified.
      ENDIF.
    ELSE.
      " This is a strange situation. As both local and remote exist
      " the state should also be present. Maybe this is a first run of the code.
      " In this case just compare hashes directly and mark both changed
      " the user will presumably decide what to do after checking the actual diff
      rs_result-lstate = Lif_abapgit_definitions=>c_state-modified.
      rs_result-rstate = Lif_abapgit_definitions=>c_state-modified.
    ENDIF.

  ENDMETHOD.
  METHOD build_new_local.

    " Item
    rs_result-obj_type  = is_local-item-obj_type.
    rs_result-obj_name  = is_local-item-obj_name.
    rs_result-package   = is_local-item-devclass.
    rs_result-srcsystem = is_local-item-srcsystem.
    rs_result-origlang  = is_local-item-origlang.
    rs_result-inactive  = is_local-item-inactive.

    " File
    rs_result-path     = is_local-file-path.
    rs_result-filename = is_local-file-filename.

    " Match
    rs_result-match    = abap_false.
    rs_result-lstate   = Lif_abapgit_definitions=>c_state-added.

  ENDMETHOD.
  METHOD build_new_remote.

    DATA ls_item     LIKE LINE OF it_items_idx.
    DATA ls_file_sig LIKE LINE OF it_state_idx.

    " Common and default part
    rs_result-path     = is_remote-path.
    rs_result-filename = is_remote-filename.
    rs_result-match    = abap_false.
    rs_result-rstate   = Lif_abapgit_definitions=>c_state-added.

    Lcl_abapgit_filename_logic=>file_to_object(
      EXPORTING
        iv_filename = is_remote-filename
        iv_path     = is_remote-path
        iv_devclass = mv_root_package
        io_dot      = mo_dot
      IMPORTING
        es_item     = ls_item ).

    " Check if in item index + get package
    READ TABLE it_items_idx INTO ls_item
      WITH KEY
        obj_type = ls_item-obj_type
        obj_name = ls_item-obj_name.

    IF sy-subrc = 0.

      " Completely new (xml, abap) and new file in an existing object
      rs_result-obj_type  = ls_item-obj_type.
      rs_result-obj_name  = ls_item-obj_name.
      rs_result-package   = ls_item-devclass.
      rs_result-srcsystem = sy-sysid.
      rs_result-origlang  = sy-langu.

      READ TABLE it_state_idx INTO ls_file_sig
        WITH KEY
          path     = is_remote-path
          filename = is_remote-filename.

      " Existing file but from another package
      " was not added during local file proc as was not in tadir for repo package
      IF sy-subrc = 0.
        IF ls_file_sig-sha1 = is_remote-sha1.
          rs_result-match = abap_true.
          CLEAR rs_result-rstate.
        ELSE.
          rs_result-rstate = Lif_abapgit_definitions=>c_state-modified.
        ENDIF.

        " Item is in state and in cache but with no package - it was deleted
        " OR devclass is the same as repo package (see #532)
        IF ls_item-devclass IS INITIAL OR ls_item-devclass = mv_root_package.
          rs_result-match  = abap_false.
          rs_result-lstate = Lif_abapgit_definitions=>c_state-deleted.
        ENDIF.
      ENDIF.

    ELSE. " Completely unknown file, probably non-abapgit
      ASSERT 1 = 1. " No action, just follow defaults
    ENDIF.

  ENDMETHOD.
  METHOD calculate.

    DATA lt_local TYPE Lif_abapgit_definitions=>ty_files_item_tt.
    DATA lt_remote TYPE Lif_abapgit_git_definitions=>ty_files_tt.
    DATA li_exit TYPE REF TO Lif_abapgit_exit.
    DATA lo_instance TYPE REF TO Lcl_abapgit_repo_status.
    DATA lo_consistency_checks TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLAS7GBTU.

    IF ii_obj_filter IS INITIAL.
      lt_local = io_repo->get_files_local( ii_log ).
    ELSE.
      lt_local = io_repo->get_files_local_filtered(
        ii_log        = ii_log
        ii_obj_filter = ii_obj_filter ).
    ENDIF.

    IF lines( lt_local ) <= 2.
      " Less equal two means that we have only the .abapgit.xml and the package in
      " our local repository. In this case we have to update our local .abapgit.xml
      " from the remote one. Otherwise we get errors when e.g. the folder starting
      " folder is different.
      io_repo->find_remote_dot_abapgit( ).
    ENDIF.

    lt_remote = io_repo->get_files_remote( ii_obj_filter = ii_obj_filter
                                           iv_ignore_files = abap_true ).

    li_exit = Lcl_abapgit_exit=>get_instance( ).
    li_exit->pre_calculate_repo_status(
      EXPORTING
        is_repo_meta = io_repo->ms_data
      CHANGING
        ct_local  = lt_local
        ct_remote = lt_remote ).

    CREATE OBJECT lo_instance
      EXPORTING
        iv_root_package = io_repo->get_package( )
        io_dot          = io_repo->get_dot_abapgit( ).

    rt_results = lo_instance->calculate_status(
      it_local     = lt_local
      it_remote    = lt_remote
      it_cur_state = io_repo->Lif_abapgit_repo~checksums( )->get_checksums_per_file( ) ).

    IF ii_log IS BOUND.
      " This method just adds messages to the log. No log, nothing to do here
      CREATE OBJECT lo_consistency_checks
        EXPORTING
          iv_root_package = io_repo->get_package( )
          io_dot          = io_repo->get_dot_abapgit( ).
      ii_log->merge_with( lo_consistency_checks->run_checks( rt_results ) ).
    ENDIF.

  ENDMETHOD.
  METHOD calculate_status.

    DATA:
      lt_remote        LIKE it_remote,
      lt_items         TYPE Lif_abapgit_definitions=>ty_items_tt,
      lt_items_by_obj  TYPE Lif_abapgit_definitions=>ty_items_ts, " Sorted by obj_type+obj_name
      lt_state_by_file TYPE Lif_abapgit_git_definitions=>ty_file_signatures_ts. " Sorted by path+filename

    lt_state_by_file = ensure_state( " Index by file
      it_cur_state = it_cur_state
      it_local     = it_local ).
    lt_remote        = it_remote.

    " Process local files and new local files
    process_local(
      EXPORTING
        it_local     = it_local
        it_state_idx = lt_state_by_file
      CHANGING
        ct_remote    = lt_remote
        ct_items     = lt_items
        ct_results   = rt_results ).

    " Remove processed remotes (with cleared SHA1)
    DELETE lt_remote WHERE sha1 IS INITIAL.

    " Complete item index for unmarked remote files
    process_items( " TODO: rename ?
      EXPORTING
        it_unprocessed_remote = lt_remote
      CHANGING
        ct_items              = lt_items ).

    " The item list was not unique by now, just collected as "mention" list
    SORT lt_items DESCENDING. " Default key - type, name, pkg, ...
    DELETE ADJACENT DUPLICATES FROM lt_items COMPARING obj_type obj_name.
    lt_items_by_obj = lt_items.

    " Process new remote files (marked above with empty SHA1)
    process_remote(
      EXPORTING
        it_local              = it_local
        it_unprocessed_remote = lt_remote
        it_state_idx          = lt_state_by_file
        it_items_idx          = lt_items_by_obj
      CHANGING
        ct_results            = rt_results ).

    SORT rt_results BY
      obj_type ASCENDING
      obj_name ASCENDING
      filename ASCENDING
      path ASCENDING.

  ENDMETHOD.
  METHOD check_local_remote_consistency.
    IF is_remote-sha1 IS INITIAL.
      IF is_local-file-filename = Lcl_abapgit_filename_logic=>c_package_file.
        Lcx_abapgit_exception=>raise(
          |Package name conflict { is_local-item-obj_type } { is_local-item-obj_name }. | &&
          |Rename package or use FULL folder logic| ).
      ELSE.
        Lcx_abapgit_exception=>raise(
          |Checksum conflict { is_local-item-obj_type } { is_local-item-obj_name }. | &&
          |Please create an issue on Github| ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD constructor.
    mv_root_package = iv_root_package.
    mo_dot          = io_dot.
  ENDMETHOD.
  METHOD ensure_state.

    FIELD-SYMBOLS <ls_state> LIKE LINE OF rt_state.
    FIELD-SYMBOLS <ls_local> LIKE LINE OF it_local.

    IF lines( it_cur_state ) = 0.
      " Empty state is usually not expected. Maybe for new repos.
      " In this case suppose the local state is unchanged
      LOOP AT it_local ASSIGNING <ls_local>.
        APPEND INITIAL LINE TO rt_state ASSIGNING <ls_state>.
        MOVE-CORRESPONDING <ls_local>-file TO <ls_state>.
      ENDLOOP.
    ELSE.
      rt_state = it_cur_state.
    ENDIF.

  ENDMETHOD.
  METHOD get_object_package.
    DATA: lv_name    TYPE devclass,
          li_package TYPE REF TO Lif_abapgit_sap_package.

    rv_devclass = Lcl_abapgit_factory=>get_tadir( )->get_object_package(
      iv_object   = iv_object
      iv_obj_name = iv_obj_name ).
    IF rv_devclass IS INITIAL AND iv_object = 'DEVC' AND iv_obj_name(1) = '$'.
      " local packages usually have no tadir entry
      lv_name = iv_obj_name.
      li_package = Lcl_abapgit_factory=>get_sap_package( lv_name ).
      IF li_package->exists( ) = abap_true.
        rv_devclass = lv_name.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD process_items.

    DATA:
      ls_item         LIKE LINE OF ct_items,
      lv_is_xml       TYPE abap_bool,
      lv_is_json      TYPE abap_bool,
      lv_sub_fetched  TYPE abap_bool,
      lt_sub_packages TYPE SORTED TABLE OF devclass WITH UNIQUE KEY table_line.

    FIELD-SYMBOLS <ls_remote> LIKE LINE OF it_unprocessed_remote.

    LOOP AT it_unprocessed_remote ASSIGNING <ls_remote>.

      Lcl_abapgit_filename_logic=>file_to_object(
        EXPORTING
          iv_filename = <ls_remote>-filename
          iv_path     = <ls_remote>-path
          io_dot      = mo_dot
          iv_devclass = mv_root_package
        IMPORTING
          es_item     = ls_item
          ev_is_xml   = lv_is_xml
          ev_is_json  = lv_is_json ).

      CHECK lv_is_xml = abap_true OR lv_is_json = abap_true. " only object definitions

      ls_item-devclass = get_object_package(
        iv_object   = ls_item-obj_type
        iv_obj_name = ls_item-obj_name ).

      IF ls_item-devclass IS NOT INITIAL AND mv_root_package <> ls_item-devclass.
        IF lv_sub_fetched = abap_false.
          lt_sub_packages = Lcl_abapgit_factory=>get_sap_package( mv_root_package )->list_subpackages( ).
          lv_sub_fetched  = abap_true.
        ENDIF.

        " Make sure the package is under the repo main package
        READ TABLE lt_sub_packages TRANSPORTING NO FIELDS
          WITH KEY table_line = ls_item-devclass.
        IF sy-subrc <> 0 AND ls_item-obj_type = 'DEVC'.
          CLEAR ls_item-devclass.
        ENDIF.
      ENDIF.

      APPEND ls_item TO ct_items.
    ENDLOOP.

  ENDMETHOD.
  METHOD process_local.

    FIELD-SYMBOLS:
      <ls_remote> LIKE LINE OF ct_remote,
      <ls_result> LIKE LINE OF ct_results,
      <ls_state>  LIKE LINE OF it_state_idx,
      <ls_local>  LIKE LINE OF it_local.

    LOOP AT it_local ASSIGNING <ls_local>.
      " Skip ignored files
      CHECK mo_dot->is_ignored(
        iv_path     = <ls_local>-file-path
        iv_filename = <ls_local>-file-filename ) = abap_false.

      IF <ls_local>-item IS NOT INITIAL
        AND Lcl_abapgit_filename_logic=>is_obj_definition_file( <ls_local>-file-filename ) = abap_true.
        " Collect for item index
        APPEND <ls_local>-item TO ct_items.
      ENDIF.

      APPEND INITIAL LINE TO ct_results ASSIGNING <ls_result>.

      " Find a match in remote
      READ TABLE ct_remote ASSIGNING <ls_remote>
        WITH KEY file_path
        COMPONENTS
          path     = <ls_local>-file-path
          filename = <ls_local>-file-filename.
      IF sy-subrc = 0.  " Both local and remote exist
        check_local_remote_consistency(
          is_local  = <ls_local>
          is_remote = <ls_remote> ).
        <ls_result> = build_existing(
          is_local  = <ls_local>
          is_remote = <ls_remote>
          it_state  = it_state_idx ).
        CLEAR <ls_remote>-sha1. " Mark as processed
      ELSE. " Only local exists
        <ls_result> = build_new_local( <ls_local> ).
        " Check if same file exists in different location
        READ TABLE ct_remote ASSIGNING <ls_remote>
          WITH KEY file
          COMPONENTS filename = <ls_local>-file-filename.
        IF sy-subrc = 0 AND <ls_local>-file-sha1 = <ls_remote>-sha1.
          " If yes, then it was probably moved
          <ls_result>-packmove = abap_true.
        ELSEIF sy-subrc = 4.
          " Check if file existed before and was deleted remotely
          READ TABLE it_state_idx ASSIGNING <ls_state>
            WITH KEY
              path     = <ls_local>-file-path
              filename = <ls_local>-file-filename.
          IF sy-subrc = 0.
            IF <ls_local>-file-sha1 = <ls_state>-sha1.
              <ls_result>-lstate = Lif_abapgit_definitions=>c_state-unchanged.
            ELSE.
              <ls_result>-lstate = Lif_abapgit_definitions=>c_state-modified.
            ENDIF.
            <ls_result>-rstate = Lif_abapgit_definitions=>c_state-deleted. " ??
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD process_remote.

    FIELD-SYMBOLS:
      <ls_remote> LIKE LINE OF it_unprocessed_remote,
      <ls_result> LIKE LINE OF ct_results,
      <ls_local>  LIKE LINE OF it_local.

    LOOP AT it_unprocessed_remote ASSIGNING <ls_remote>.

      APPEND INITIAL LINE TO ct_results ASSIGNING <ls_result>.

      <ls_result> = build_new_remote(
        is_remote   = <ls_remote>
        it_items_idx = it_items_idx
        it_state_idx = it_state_idx ).

      " Check if same file exists in different location (not for generic package files)
      READ TABLE it_local ASSIGNING <ls_local>
        WITH KEY file-filename = <ls_remote>-filename.
      IF sy-subrc = 0 AND <ls_remote>-filename <> Lcl_abapgit_filename_logic=>c_package_file.
        <ls_result>-match = abap_false.
        <ls_result>-lstate = Lif_abapgit_definitions=>c_state-deleted.
        <ls_result>-rstate = Lif_abapgit_definitions=>c_state-unchanged.
        IF <ls_local>-file-sha1 = <ls_remote>-sha1.
          <ls_result>-packmove = abap_true.
        ENDIF.
      ELSE.
        " Check if file existed before and was deleted locally
        READ TABLE it_state_idx TRANSPORTING NO FIELDS
          WITH KEY
            path     = <ls_remote>-path
            filename = <ls_remote>-filename.
        IF sy-subrc = 0.
          <ls_result>-match  = abap_false.
          <ls_result>-lstate = Lif_abapgit_definitions=>c_state-deleted.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_REPO_STATUS implementation

*>>>>>>> ZCL_ABAPGIT_MERGE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_merge=============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_merge=============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_MERGE implementation.
*"* method's implementations
*include methods.
  METHOD all_files.

    APPEND LINES OF ms_merge-stree TO rt_files.
    APPEND LINES OF ms_merge-ttree TO rt_files.
    APPEND LINES OF ms_merge-ctree TO rt_files.
    SORT rt_files BY path DESCENDING name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rt_files COMPARING path name.

  ENDMETHOD.
  METHOD calculate_result.

    DATA: lt_files        TYPE Lif_abapgit_git_definitions=>ty_expanded_tt,
          lv_found_source TYPE abap_bool,
          lv_found_target TYPE abap_bool,
          lv_found_common TYPE abap_bool.

    FIELD-SYMBOLS: <ls_source>   LIKE LINE OF lt_files,
                   <ls_target>   LIKE LINE OF lt_files,
                   <ls_common>   LIKE LINE OF lt_files,
                   <ls_file>     LIKE LINE OF lt_files,
                   <ls_result>   LIKE LINE OF ms_merge-result,
                   <ls_object>   LIKE LINE OF mt_objects,
                   <ls_conflict> LIKE LINE OF mt_conflicts.

    lt_files = all_files( ).

    CREATE OBJECT ms_merge-stage
      EXPORTING
        iv_merge_source = ms_merge-source-sha1.

    LOOP AT lt_files ASSIGNING <ls_file>.

      UNASSIGN <ls_source>.
      UNASSIGN <ls_target>.
      UNASSIGN <ls_common>.

      READ TABLE ms_merge-stree ASSIGNING <ls_source>
        WITH KEY path_name
        COMPONENTS path = <ls_file>-path name = <ls_file>-name. "#EC CI_SUBRC
      READ TABLE ms_merge-ttree ASSIGNING <ls_target>
        WITH KEY path_name
        COMPONENTS path = <ls_file>-path name = <ls_file>-name. "#EC CI_SUBRC
      READ TABLE ms_merge-ctree ASSIGNING <ls_common>
        WITH KEY path_name
        COMPONENTS path = <ls_file>-path name = <ls_file>-name. "#EC CI_SUBRC

      lv_found_source = boolc( <ls_source> IS ASSIGNED ).
      lv_found_target = boolc( <ls_target> IS ASSIGNED ).
      lv_found_common = boolc( <ls_common> IS ASSIGNED ).

      IF lv_found_source = abap_false
          AND lv_found_target = abap_false.
* deleted in source and target, skip
        CONTINUE.
      ELSEIF lv_found_source = abap_false
          AND lv_found_common = abap_true
          AND <ls_target>-sha1 = <ls_common>-sha1.
* deleted in source, skip
        ms_merge-stage->rm( iv_path     = <ls_file>-path
                            iv_filename = <ls_file>-name ).
        CONTINUE.
      ELSEIF lv_found_target = abap_false
          AND lv_found_common = abap_true
          AND <ls_source>-sha1 = <ls_common>-sha1.
* deleted in target, skip
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO ms_merge-result ASSIGNING <ls_result>.
      <ls_result>-path = <ls_file>-path.
      <ls_result>-name = <ls_file>-name.

      IF lv_found_target = abap_false.
* added in source
        READ TABLE mt_objects ASSIGNING <ls_object>
          WITH KEY type COMPONENTS
            type = Lif_abapgit_git_definitions=>c_type-blob
            sha1 = <ls_source>-sha1.
        ASSERT sy-subrc = 0.

        ms_merge-stage->add( iv_path     = <ls_file>-path
                             iv_filename = <ls_file>-name
                             iv_data     = <ls_object>-data ).
        <ls_result>-sha1 = <ls_source>-sha1.
        CONTINUE.
      ELSEIF lv_found_source = abap_false.
* added in target
        <ls_result>-sha1 = <ls_target>-sha1.
      ELSEIF lv_found_common = abap_false
          AND <ls_target>-sha1 = <ls_source>-sha1.
* added in source and target
        <ls_result>-sha1 = <ls_source>-sha1.
      ELSEIF lv_found_common = abap_false
          AND <ls_target>-sha1 <> <ls_source>-sha1.

        INSERT INITIAL LINE INTO TABLE mt_conflicts ASSIGNING <ls_conflict>.
        <ls_conflict>-path = <ls_file>-path.
        <ls_conflict>-filename = <ls_file>-name.
        <ls_conflict>-source_sha1 = <ls_source>-sha1.
        READ TABLE mt_objects ASSIGNING <ls_object>
          WITH KEY type COMPONENTS
            type = Lif_abapgit_git_definitions=>c_type-blob
            sha1 = <ls_source>-sha1.
        <ls_conflict>-source_data = <ls_object>-data.

        <ls_conflict>-target_sha1 = <ls_target>-sha1.
        READ TABLE mt_objects ASSIGNING <ls_object>
          WITH KEY type COMPONENTS
            type = Lif_abapgit_git_definitions=>c_type-blob
            sha1 = <ls_target>-sha1.
        <ls_conflict>-target_data = <ls_object>-data.

* added in source and target, but different, merge conflict must be resolved
        ms_merge-conflict = |{ <ls_file>-name } merge conflict|.
        CONTINUE.
      ENDIF.

      IF lv_found_source = abap_false
          OR lv_found_target = abap_false
          OR lv_found_common = abap_false.
        ms_merge-conflict = |{ <ls_file>-name } merge conflict, not found anywhere|.
        CONTINUE.
      ENDIF.

      IF <ls_target>-sha1 = <ls_source>-sha1.
* target and source match
        <ls_result>-sha1 = <ls_source>-sha1.
      ELSEIF <ls_target>-sha1 = <ls_common>-sha1.
* changed in source
        READ TABLE mt_objects ASSIGNING <ls_object>
          WITH KEY type COMPONENTS
            type = Lif_abapgit_git_definitions=>c_type-blob
            sha1 = <ls_source>-sha1.
        ASSERT sy-subrc = 0.

        ms_merge-stage->add( iv_path     = <ls_file>-path
                             iv_filename = <ls_file>-name
                             iv_data     = <ls_object>-data ).
        <ls_result>-sha1 = <ls_source>-sha1.
      ELSEIF <ls_source>-sha1 = <ls_common>-sha1.
* changed in target
        <ls_result>-sha1 = <ls_target>-sha1.
      ELSE.
* changed in source and target, conflict
* conflict must be resolved before merge
        INSERT INITIAL LINE INTO TABLE mt_conflicts ASSIGNING <ls_conflict>.
        <ls_conflict>-path = <ls_file>-path.
        <ls_conflict>-filename = <ls_file>-name.
        <ls_conflict>-source_sha1 = <ls_source>-sha1.
        READ TABLE mt_objects ASSIGNING <ls_object>
          WITH KEY type COMPONENTS
            type = Lif_abapgit_git_definitions=>c_type-blob
            sha1 = <ls_source>-sha1.
        <ls_conflict>-source_data = <ls_object>-data.

        <ls_conflict>-target_sha1 = <ls_target>-sha1.
        READ TABLE mt_objects ASSIGNING <ls_object>
          WITH KEY type COMPONENTS
            type = Lif_abapgit_git_definitions=>c_type-blob
            sha1 = <ls_target>-sha1.
        <ls_conflict>-target_data = <ls_object>-data.

        ms_merge-conflict = |{ <ls_file>-name } merge conflict, changed in source and target branch|.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD constructor.

    IF iv_source_branch = io_repo->get_selected_branch( ).
      Lcx_abapgit_exception=>raise( 'source = target' ).
    ENDIF.

    mo_repo = io_repo.
    mv_source_branch = iv_source_branch.

  ENDMETHOD.
  METHOD fetch_git.

    DATA: lo_branch_list TYPE REF TO Lcl_abapgit_git_branch_list,
          lt_upload      TYPE Lif_abapgit_git_definitions=>ty_git_branch_list_tt.

    lo_branch_list = Lcl_abapgit_git_transport=>branches( ms_merge-repo->get_url( ) ).

    ms_merge-source = lo_branch_list->find_by_name(
      Lcl_abapgit_git_branch_list=>complete_heads_branch_name( mv_source_branch ) ).

    ms_merge-target = lo_branch_list->find_by_name(
      Lcl_abapgit_git_branch_list=>complete_heads_branch_name( mo_repo->get_selected_branch( ) ) ).

    APPEND ms_merge-source TO lt_upload.
    APPEND ms_merge-target TO lt_upload.

    Lcl_abapgit_git_transport=>upload_pack_by_branch(
      EXPORTING
        iv_url          = ms_merge-repo->get_url( )
        iv_branch_name  = ms_merge-repo->get_selected_branch( )
        iv_deepen_level = 0
        it_branches     = lt_upload
      IMPORTING
        et_objects      = rt_objects ).

  ENDMETHOD.
  METHOD find_ancestors.

    DATA: ls_commit TYPE Lcl_abapgit_git_pack=>ty_commit,
          lt_visit  TYPE ty_visit_tt,
          lv_commit LIKE LINE OF lt_visit.

    FIELD-SYMBOLS: <ls_ancestor> LIKE LINE OF rt_ancestors,
                   <ls_object>   LIKE LINE OF mt_objects.


    APPEND iv_commit TO lt_visit.

    LOOP AT lt_visit INTO lv_commit.
      READ TABLE mt_objects ASSIGNING <ls_object>
        WITH KEY type COMPONENTS
          type = Lif_abapgit_git_definitions=>c_type-commit
          sha1 = lv_commit.
      ASSERT sy-subrc = 0.

      ls_commit = Lcl_abapgit_git_pack=>decode_commit( <ls_object>-data ).

      visit( EXPORTING iv_parent = ls_commit-parent
             CHANGING ct_visit   = lt_visit ).
      visit( EXPORTING iv_parent = ls_commit-parent2
             CHANGING ct_visit   = lt_visit ).

      APPEND INITIAL LINE TO rt_ancestors ASSIGNING <ls_ancestor>.
      <ls_ancestor>-commit = lv_commit.
      <ls_ancestor>-tree = ls_commit-tree.
      <ls_ancestor>-body = ls_commit-body.
      <ls_ancestor>-time = ls_commit-author.

      "Strip Author entry of all but the time component
      REPLACE ALL OCCURRENCES OF REGEX '[a-zA-Z<>@.-]*' IN <ls_ancestor>-time WITH ''.
      CONDENSE <ls_ancestor>-time.
    ENDLOOP.

    SORT rt_ancestors BY time DESCENDING.

  ENDMETHOD.
  METHOD find_first_common.

    FIELD-SYMBOLS: <ls_list1> LIKE LINE OF it_list1,
                   <ls_list2> LIKE LINE OF it_list2.

    LOOP AT it_list1 ASSIGNING <ls_list1>.
      LOOP AT it_list2 ASSIGNING <ls_list2>.
        IF <ls_list1>-tree = <ls_list2>-tree.
          rs_common = <ls_list1>.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    Lcx_abapgit_exception=>raise( 'error finding common ancestor' ).

  ENDMETHOD.
  METHOD visit.

    IF NOT iv_parent IS INITIAL.
      READ TABLE ct_visit FROM iv_parent TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND iv_parent TO ct_visit.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_merge~get_conflicts.

    rt_conflicts = mt_conflicts.

  ENDMETHOD.
  METHOD Lif_abapgit_merge~get_result.

    rs_merge = ms_merge.

  ENDMETHOD.
  METHOD Lif_abapgit_merge~get_source_branch.

    rv_source_branch = mv_source_branch.

  ENDMETHOD.
  METHOD Lif_abapgit_merge~has_conflicts.

    rv_conflicts_exists = boolc( lines( mt_conflicts ) > 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_merge~resolve_conflict.

    FIELD-SYMBOLS: <ls_conflict> TYPE Lif_abapgit_merge=>ty_merge_conflict,
                   <ls_result>   LIKE LINE OF ms_merge-result.

    IF is_conflict-result_sha1 IS NOT INITIAL
        AND is_conflict-result_data IS NOT INITIAL.
      READ TABLE mt_conflicts ASSIGNING <ls_conflict> WITH KEY path = is_conflict-path
                                                               filename = is_conflict-filename.
      IF sy-subrc = 0.
        READ TABLE ms_merge-result ASSIGNING <ls_result>
          WITH KEY path_name
          COMPONENTS path = is_conflict-path name = is_conflict-filename.
        IF sy-subrc = 0.
          <ls_result>-sha1 = is_conflict-result_sha1.

          ms_merge-stage->add( iv_path     = <ls_conflict>-path
                               iv_filename = <ls_conflict>-filename
                               iv_data     = is_conflict-result_data ).

          DELETE mt_conflicts WHERE path     = is_conflict-path
                                AND filename = is_conflict-filename.
        ENDIF.

        READ TABLE ms_merge-result ASSIGNING <ls_result> WITH KEY sha1 = space.
        IF sy-subrc = 0.
          ms_merge-conflict = |{ <ls_result>-name } merge conflict, changed in source and target branch|.
        ELSE.
          CLEAR ms_merge-conflict.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_merge~run.

    DATA: lt_asource TYPE ty_ancestor_tt,
          lt_atarget TYPE ty_ancestor_tt.

    CLEAR: ms_merge, mt_objects, mt_conflicts.

    ms_merge-repo = mo_repo.
    mt_objects = fetch_git( ).

    lt_asource = find_ancestors( ms_merge-source-sha1 ).
    lt_atarget = find_ancestors( ms_merge-target-sha1 ).

    ms_merge-common = find_first_common( it_list1 = lt_asource
                                         it_list2 = lt_atarget ).

    ms_merge-stree = Lcl_abapgit_git_porcelain=>full_tree(
      it_objects = mt_objects
      iv_parent  = ms_merge-source-sha1 ).
    ms_merge-ttree = Lcl_abapgit_git_porcelain=>full_tree(
      it_objects = mt_objects
      iv_parent  = ms_merge-target-sha1 ).
    ms_merge-ctree = Lcl_abapgit_git_porcelain=>full_tree(
      it_objects = mt_objects
      iv_parent  = ms_merge-common-commit ).

    calculate_result( ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_MERGE implementation

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

*>>>>>>> ZCL_ABAPGIT_STAGE_LOGIC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_stage_logic=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_stage_logic=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_STAGE_LOGIC implementation.
*"* method's implementations
*include methods.
  METHOD remove_identical.

    DATA: lv_index  TYPE i,
          ls_remote LIKE LINE OF cs_files-remote.

    FIELD-SYMBOLS: <ls_local> LIKE LINE OF cs_files-local.

    SORT cs_files-remote BY path filename.

    LOOP AT cs_files-local ASSIGNING <ls_local>.
      lv_index = sy-tabix.

      READ TABLE cs_files-remote INTO ls_remote
        WITH KEY path = <ls_local>-file-path filename = <ls_local>-file-filename
        BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE cs_files-remote INDEX sy-tabix.
        IF ls_remote-sha1 = <ls_local>-file-sha1.
          DELETE cs_files-local INDEX lv_index.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD remove_ignored.

    DATA: lv_index TYPE i.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF cs_files-remote,
                   <ls_local>  LIKE LINE OF cs_files-local.


    LOOP AT cs_files-remote ASSIGNING <ls_remote>.
      lv_index = sy-tabix.

      IF io_repo->get_dot_abapgit( )->is_ignored(
          iv_path     = <ls_remote>-path
          iv_filename = <ls_remote>-filename ) = abap_true.
        DELETE cs_files-remote INDEX lv_index.
      ELSEIF <ls_remote>-path = Lif_abapgit_definitions=>c_root_dir
          AND <ls_remote>-filename = Lif_abapgit_definitions=>c_dot_abapgit.
        " Remove .abapgit from remotes - it cannot be removed or ignored
        DELETE cs_files-remote INDEX lv_index.
      ENDIF.

    ENDLOOP.

    LOOP AT cs_files-local ASSIGNING <ls_local>.
      lv_index = sy-tabix.

      IF io_repo->get_dot_abapgit( )->is_ignored(
          iv_path     = <ls_local>-file-path
          iv_filename = <ls_local>-file-filename ) = abap_true.
        DELETE cs_files-local INDEX lv_index.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_stage_logic~get.

    IF ii_obj_filter IS INITIAL.
      rs_files-local  = io_repo->get_files_local( ).
    ELSE.
      rs_files-local  = io_repo->get_files_local_filtered( ii_obj_filter ).
    ENDIF.

    rs_files-remote = io_repo->get_files_remote( ii_obj_filter ).
    rs_files-status = Lcl_abapgit_repo_status=>calculate( io_repo = io_repo
                                                          ii_obj_filter = ii_obj_filter ).

    remove_identical( CHANGING cs_files = rs_files ).
    remove_ignored( EXPORTING io_repo  = io_repo
                    CHANGING  cs_files = rs_files ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_STAGE_LOGIC implementation

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



*CLASS SHRIS5ZPAUXVKEPN5HWETLLAS7UBTU DEFINITION DEFERRED.
*CLASS zcl_abapgit_syntax_abap DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLAS7UBTU.

*----------------------------------------------------------------------*
*       CLASS SHRIS5ZPAUXVKEPN5HWETLLAS7UBTU DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS ltcl_syntax_highlighter IMPLEMENTATION
*----------------------------------------------------------------------*

*CLASS SHRIS5ZPAUXVKEPN5HWETLLAS7WBTU DEFINITION DEFERRED.
*CLASS zcl_abapgit_syntax_abap DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLAS7WBTU.

*----------------------------------------------------------------------*
*       CLASS SHRIS5ZPAUXVKEPN5HWETLLAS7WBTU definition
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS SHRIS5ZPAUXVKEPN5HWETLLAS7WBTU IMPLEMENTATION
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



*CLASS SHRIS5ZPAUXVKEPN5HWETLLAS74BTU DEFINITION DEFERRED.
*CLASS zcl_abapgit_syntax_xml DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLAS74BTU.

*----------------------------------------------------------------------*
*       CLASS SHRIS5ZPAUXVKEPN5HWETLLAS74BTU definition
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS SHRIS5ZPAUXVKEPN5HWETLLAS74BTU IMPLEMENTATION
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

*>>>>>>> ZCL_ABAPGIT_OBJECTS_CI_TESTS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_objects_ci_tests==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_objects_ci_tests==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations



*>>>>>>> ZCL_ABAPGIT_FRONTEND_SERVICES <<<<<<<*

*"* macro definitions
*include zcl_abapgit_frontend_services=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_frontend_services=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_FRONTEND_SERVICES implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_frontend_services~clipboard_export.

    DATA lv_rc TYPE i.

    " Note: do not use a string table for 'it_data'!

    TRY.
        CALL METHOD cl_gui_frontend_services=>('CLIPBOARD_EXPORT')
          EXPORTING
            no_auth_check        = iv_no_auth_check " >= 740
          IMPORTING
            data                 = it_data
          CHANGING
            rc                   = lv_rc
          EXCEPTIONS
            cntl_error           = 1
            error_no_gui         = 2
            not_supported_by_gui = 3
            no_authority         = 4
            OTHERS               = 5.
        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise_t100( ).
        ENDIF.

      CATCH cx_sy_dyn_call_param_missing.

        cl_gui_frontend_services=>clipboard_export(
          IMPORTING
            data                 = it_data
          CHANGING
            rc                   = lv_rc
          EXCEPTIONS
            cntl_error           = 1
            error_no_gui         = 2
            not_supported_by_gui = 3
            no_authority         = 4
          OTHERS               = 5 ).
        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise_t100( ).
        ENDIF.

    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~directory_browse.

    cl_gui_frontend_services=>directory_browse(
      EXPORTING
        window_title         = iv_window_title
        initial_folder       = iv_initial_folder
      CHANGING
        selected_folder      = cv_selected_folder
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~directory_create.

    cl_gui_frontend_services=>directory_create(
      EXPORTING
        directory                = iv_directory
      CHANGING
        rc                       = cv_rc
      EXCEPTIONS
        directory_create_failed  = 1
        cntl_error               = 2
        error_no_gui             = 3
        directory_access_denied  = 4
        directory_already_exists = 5
        path_not_found           = 6
        unknown_error            = 7
        not_supported_by_gui     = 8
        wrong_parameter          = 9
        OTHERS                   = 10 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~directory_exist.

    cl_gui_frontend_services=>directory_exist(
      EXPORTING
        directory            = iv_directory
      RECEIVING
        result               = rv_exists
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~execute.

    cl_gui_frontend_services=>execute(
      EXPORTING
        document               = iv_document
        application            = iv_application
        parameter              = iv_parameter
        default_directory      = iv_default_directory
        maximized              = iv_maximized
        minimized              = iv_minimized
        synchronous            = iv_synchronous
        operation              = iv_operation
      EXCEPTIONS
        cntl_error             = 1
        error_no_gui           = 2
        bad_parameter          = 3
        file_not_found         = 4
        path_not_found         = 5
        file_extension_unknown = 6
        error_execute_failed   = 7
        synchronous_failed     = 8
        not_supported_by_gui   = 9
        OTHERS                 = 10 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~file_download.

    TYPES ty_hex TYPE x LENGTH 200.
    DATA lt_rawdata TYPE STANDARD TABLE OF ty_hex WITH DEFAULT KEY.

    Lcl_abapgit_convert=>xstring_to_bintab(
      EXPORTING iv_xstr   = iv_xstr
      IMPORTING et_bintab = lt_rawdata ).

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = xstrlen( iv_xstr )
        filename                  = iv_path
        filetype                  = 'BIN'
      CHANGING
        data_tab                  = lt_rawdata
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~file_upload.

    TYPES: ty_hex TYPE x LENGTH 255.

    DATA: lt_data   TYPE TABLE OF ty_hex WITH DEFAULT KEY,
          lv_length TYPE i.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = iv_path
        filetype                = 'BIN'
      IMPORTING
        filelength              = lv_length
      CHANGING
        data_tab                = lt_data
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CONCATENATE LINES OF lt_data INTO rv_xstr IN BYTE MODE.
    rv_xstr = rv_xstr(lv_length).

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~get_file_separator.

    cl_gui_frontend_services=>get_file_separator(
      CHANGING
        file_separator       = cv_file_separator
      EXCEPTIONS
        not_supported_by_gui = 1
        error_no_gui         = 2
        cntl_error           = 3
        OTHERS               = 4 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~get_gui_version.

    DATA:
      lt_version_table TYPE filetable,
      lv_rc            TYPE i,
      ls_version       LIKE LINE OF lt_version_table.

    cl_gui_frontend_services=>get_gui_version(
      CHANGING
        version_table            = lt_version_table
        rc                       = lv_rc
      EXCEPTIONS
        get_gui_version_failed   = 1
        cant_write_version_table = 2
        gui_no_version           = 3
        cntl_error               = 4
        error_no_gui             = 5
        not_supported_by_gui     = 6
        OTHERS                   = 7 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    READ TABLE lt_version_table INTO ls_version INDEX 1. " gui release
    ev_gui_release = ls_version-filename.
    READ TABLE lt_version_table INTO ls_version INDEX 2. " gui sp
    ev_gui_sp = ls_version-filename.
    READ TABLE lt_version_table INTO ls_version INDEX 3. " gui patch
    ev_gui_patch = ls_version-filename.

    ev_gui_version_string = |{ ev_gui_release }.{ condense( ev_gui_sp ) }.{ condense( ev_gui_patch ) }|.

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~get_system_directory.

    cl_gui_frontend_services=>get_system_directory(
      CHANGING
        system_directory     = cv_system_directory
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~gui_is_available.

    CALL FUNCTION 'GUI_IS_AVAILABLE'
      IMPORTING
        return = rv_gui_is_available.

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~is_sapgui_for_java.

    CALL FUNCTION 'GUI_HAS_JAVABEANS'
      IMPORTING
        return = rv_result.

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~is_sapgui_for_windows.

    CALL FUNCTION 'GUI_HAS_ACTIVEX'
      IMPORTING
        return = rv_result.

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~is_webgui.

    CALL FUNCTION 'GUI_IS_ITS'
      IMPORTING
        return = rv_is_webgui.

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~open_ie_devtools.

    DATA: lv_system_directory TYPE string,
          lv_exe_full_path    TYPE string.

    IF Lif_abapgit_frontend_services~is_sapgui_for_windows( ) = abap_false.
      Lcx_abapgit_exception=>raise( |IE DevTools not supported on frontend OS| ).
    ENDIF.

    Lif_abapgit_frontend_services~get_system_directory( CHANGING cv_system_directory = lv_system_directory ).

    cl_gui_cfw=>flush( ).

    lv_exe_full_path = lv_system_directory && `\F12\IEChooser.exe`.

    Lif_abapgit_frontend_services~execute( iv_application = lv_exe_full_path ).

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~show_file_open_dialog.

    DATA:
      lt_file_table TYPE filetable,
      ls_file_table LIKE LINE OF lt_file_table,
      lv_filter     TYPE string,
      lv_action     TYPE i,
      lv_rc         TYPE i.

    IF iv_extension = 'zip'.
      lv_filter = 'ZIP Files (*.zip)|*.zip|' && cl_gui_frontend_services=>filetype_all.
    ENDIF.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        window_title            = iv_title
        default_filename        = iv_default_filename
        file_filter             = lv_filter
      CHANGING
        file_table              = lt_file_table
        rc                      = lv_rc
        user_action             = lv_action
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.
    IF lv_action = cl_gui_frontend_services=>action_cancel.
      Lcx_abapgit_exception=>raise( 'Cancelled' ).
    ENDIF.

    READ TABLE lt_file_table INDEX 1 INTO ls_file_table.
    ASSERT sy-subrc = 0.
    rv_path = ls_file_table-filename.

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~show_file_save_dialog.

    DATA:
      lv_action   TYPE i,
      lv_filter   TYPE string,
      lv_filename TYPE string,
      lv_path     TYPE string.

    IF iv_extension = 'zip'.
      lv_filter = 'ZIP Files (*.zip)|*.zip|' && cl_gui_frontend_services=>filetype_all.
    ENDIF.

    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        window_title         = iv_title
        default_extension    = iv_extension
        default_file_name    = iv_default_filename
        file_filter          = lv_filter
      CHANGING
        filename             = lv_filename
        path                 = lv_path
        fullpath             = rv_path
        user_action          = lv_action
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.
    IF lv_action = cl_gui_frontend_services=>action_cancel.
      Lcx_abapgit_exception=>raise( 'Cancelled' ).
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_FRONTEND_SERVICES implementation

*>>>>>>> ZCL_ABAPGIT_PASSWORD_DIALOG <<<<<<<*

*"* macro definitions
*include zcl_abapgit_password_dialog===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_password_dialog===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_PASSWORD_DIALOG implementation.
*"* method's implementations
*include methods.
  METHOD popup.

    DATA: lx_error TYPE REF TO cx_sy_dyn_call_illegal_form.

    IF Lcl_abapgit_ui_factory=>get_frontend_services( )->gui_is_available( ) = abap_true.

      TRY.
          PERFORM password_popup
            IN PROGRAM (sy-cprog)
            USING iv_repo_url
            CHANGING cv_user cv_pass.
        CATCH cx_sy_dyn_call_illegal_form INTO lx_error.
          " abapGit was called via API and either wrong or no username/password
          " was supplied. It's not possible to call abapGit password popup in
          " this case.
          " See https://docs.abapgit.org/development/api.html#online-repository
          " on how to provide username/password
          Lcx_abapgit_exception=>raise_with_text( lx_error ).
      ENDTRY.
    ELSE.
      "Extract user credentials from the environment...
      "Class ZCL_ABAPGIT_DEFAULT_AUTH_INFO is part of https://github.com/abapGit/ADT_Backend.
      "It stores the credentials of a private repository as long as the session exists.
      "Usually this class should belong to abapGit core and a refactoring is recommended.
      "As a temporary solution - and to avoid a DYNPRO_SEND_IN_BACKGROUND dump - a generic
      "call of the getter methods for username and password is implemented by PR#2635.
      TRY.
          CALL METHOD ('LCL_ABAPGIT_DEFAULT_AUTH_INFO')=>('GET_USER')
            RECEIVING
              rv_user = cv_user.
        CATCH cx_root.
          RETURN.
      ENDTRY.
      TRY.
          CALL METHOD ('LCL_ABAPGIT_DEFAULT_AUTH_INFO')=>('GET_PASSWORD')
            RECEIVING
              rv_password = cv_pass.
        CATCH cx_root.
          "check if old version with typo in method name exists
          TRY.
              CALL METHOD ('LCL_ABAPGIT_DEFAULT_AUTH_INFO')=>('GET_PASSOWORD')
                RECEIVING
                  rv_password = cv_pass.
            CATCH cx_root.
              RETURN.
          ENDTRY.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_PASSWORD_DIALOG implementation

*>>>>>>> ZCL_ABAPGIT_POPUPS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_popups============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_popups============ccimp.
CLASS SHRIS5ZPAUXVKEPN5HWETLLAS76BTU DEFINITION FINAL.
  PUBLIC SECTION.

    CONSTANTS c_default_column     TYPE abap_componentdescr-name VALUE 'DEFAULT_COLUMN'.
    CONSTANTS c_fieldname_selected TYPE abap_componentdescr-name VALUE 'SELECTED'.
    CONSTANTS c_answer_cancel      TYPE c LENGTH 1 VALUE 'A'.
    CONSTANTS c_fieldname_obj_type TYPE abap_componentdescr-name VALUE 'OBJ_TYPE'.
    CONSTANTS c_own_pfstatus       TYPE sy-pfkey VALUE 'DECIDE_DIALOG'.

    METHODS constructor
      IMPORTING
        !it_list               TYPE STANDARD TABLE
        !iv_title              TYPE lvc_title DEFAULT space
        !iv_header_text        TYPE csequence DEFAULT space
        !is_position           TYPE Lif_abapgit_popups=>ty_popup_position
        !iv_striped_pattern    TYPE abap_bool DEFAULT abap_false
        !iv_optimize_col_width TYPE abap_bool DEFAULT abap_true
        !iv_selection_mode     TYPE salv_de_constant DEFAULT if_salv_c_selection_mode=>multiple
        !iv_select_column_text TYPE csequence DEFAULT space
        !it_columns_to_display TYPE Lif_abapgit_popups=>ty_alv_column_tt
        !it_preselected_rows   TYPE Lif_abapgit_popups=>ty_rows OPTIONAL
      RAISING
        Lcx_abapgit_exception.

    METHODS display
      RAISING
        Lcx_abapgit_exception.
    METHODS get_selected
      EXPORTING
        VALUE(et_list) TYPE STANDARD TABLE.

  PRIVATE SECTION.

    DATA mr_table TYPE REF TO data.
    DATA mo_table_descr TYPE REF TO cl_abap_tabledescr.
    DATA mo_alv TYPE REF TO cl_salv_table.
    DATA mv_cancel TYPE abap_bool.
    DATA ms_position TYPE Lif_abapgit_popups=>ty_popup_position.

    " Events
    METHODS on_select_list_link_click
      FOR EVENT link_click OF cl_salv_events_table
      IMPORTING
        !row
        !column.
    METHODS on_select_list_function_click
      FOR EVENT added_function OF cl_salv_events_table
      IMPORTING
        !e_salv_function.
    METHODS on_double_click
      FOR EVENT double_click OF cl_salv_events_table
      IMPORTING
        !row
        !column.

    " Methods
    METHODS create_new_selectable_table
      IMPORTING
        it_list TYPE STANDARD TABLE.
    METHODS preselect
      IMPORTING
        it_preselected_rows TYPE Lif_abapgit_popups=>ty_rows OPTIONAL
      RAISING
        Lcx_abapgit_exception.
    METHODS create_alv
      RETURNING
        VALUE(ro_alv) TYPE REF TO cl_salv_table
      RAISING
        cx_salv_msg.
    METHODS setup_columns
      IMPORTING
        io_columns            TYPE REF TO cl_salv_columns_table
        iv_selection_mode     TYPE salv_de_constant
        iv_select_column_text TYPE csequence
        it_columns_to_display TYPE Lif_abapgit_popups=>ty_alv_column_tt
      RAISING
        cx_salv_msg.
    METHODS setup_toolbar
      IMPORTING
        !iv_selection_mode TYPE salv_de_constant
        !iv_object_list    TYPE abap_bool.
    METHODS ask_user_for_obj_category
      RETURNING
        VALUE(rv_category) TYPE string.
    METHODS mark_category
      IMPORTING
        iv_category TYPE string.
    METHODS mark_all
      IMPORTING
        iv_selected TYPE abap_bool.
    METHODS mark_visible
      IMPORTING
        iv_selected TYPE abap_bool.
    METHODS mark_selected.
    METHODS mark_indexed
      IMPORTING
        iv_selected TYPE abap_bool DEFAULT abap_true
        iv_invert   TYPE abap_bool DEFAULT abap_false
        it_scope    TYPE lvc_t_fidx.
    METHODS are_all_marked
      IMPORTING
        it_scope      TYPE lvc_t_fidx
      RETURNING
        VALUE(rv_yes) TYPE abap_bool.

ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLAS76BTU IMPLEMENTATION.

  METHOD display.

    mo_alv->display( ).

    IF mv_cancel = abap_true.
      RAISE EXCEPTION TYPE Lcx_abapgit_cancel.
    ENDIF.

  ENDMETHOD.

  METHOD get_selected.

    DATA:
      lv_condition     TYPE string,
      lr_exporting     TYPE REF TO data,
      lo_data_descr    TYPE REF TO cl_abap_datadescr,
      lo_selections    TYPE REF TO cl_salv_selections,
      lt_selected_rows TYPE salv_t_row.

    FIELD-SYMBOLS:
      <lg_exporting>    TYPE any,
      <lt_table>        TYPE STANDARD TABLE,
      <ls_line>         TYPE any,
      <lg_value>        TYPE any,
      <lv_selected>     TYPE abap_bool,
      <lv_selected_row> TYPE LINE OF salv_t_row.

    CLEAR et_list.

    " Make sure we don't accidentally return anything
    IF mv_cancel = abap_true.
      RETURN.
    ENDIF.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    lo_selections = mo_alv->get_selections( ).

    IF lo_selections->get_selection_mode( ) = if_salv_c_selection_mode=>single.

      lt_selected_rows = lo_selections->get_selected_rows( ).

      LOOP AT lt_selected_rows ASSIGNING <lv_selected_row>.

        READ TABLE <lt_table> ASSIGNING <ls_line> INDEX <lv_selected_row>.
        CHECK <ls_line> IS ASSIGNED.

        ASSIGN COMPONENT c_fieldname_selected OF STRUCTURE <ls_line> TO <lv_selected>.
        CHECK <lv_selected> IS ASSIGNED.

        <lv_selected> = abap_true.

      ENDLOOP.

    ENDIF.

    lv_condition = |{ c_fieldname_selected } = ABAP_TRUE|.

    CREATE DATA lr_exporting LIKE LINE OF et_list.
    ASSIGN lr_exporting->* TO <lg_exporting>.

    lo_data_descr = mo_table_descr->get_table_line_type( ).

    LOOP AT <lt_table> ASSIGNING <ls_line> WHERE (lv_condition).
      CLEAR <lg_exporting>.

      CASE lo_data_descr->kind.
        WHEN cl_abap_elemdescr=>kind_elem.
          ASSIGN COMPONENT c_default_column OF STRUCTURE <ls_line> TO <lg_value>.
          ASSERT <lg_value> IS ASSIGNED.
          <lg_exporting> = <lg_value>.

        WHEN OTHERS.
          MOVE-CORRESPONDING <ls_line> TO <lg_exporting>.

      ENDCASE.
      APPEND <lg_exporting> TO et_list.
    ENDLOOP.

  ENDMETHOD.

  METHOD create_alv.

    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    cl_salv_table=>factory(
      IMPORTING
        r_salv_table = ro_alv
      CHANGING
        t_table      = <lt_table> ).

  ENDMETHOD.

  METHOD constructor.

    DATA:
      lv_object_list  TYPE abap_bool,
      lo_events       TYPE REF TO cl_salv_events_table,
      lo_columns      TYPE REF TO cl_salv_columns_table,
      lo_table_header TYPE REF TO cl_salv_form_text.

    create_new_selectable_table( it_list ).
    preselect( it_preselected_rows ).

    TRY.
        mo_alv = create_alv( ).
        mo_alv->set_screen_popup(
          start_column = is_position-start_column
          end_column   = is_position-end_column
          start_line   = is_position-start_row
          end_line     = is_position-end_row ).
        ms_position = is_position.

        lo_events = mo_alv->get_event( ).

        SET HANDLER on_select_list_link_click FOR lo_events.
        SET HANDLER on_select_list_function_click FOR lo_events.
        SET HANDLER on_double_click FOR lo_events.

        IF iv_title CN ' _0'.
          mo_alv->get_display_settings( )->set_list_header( iv_title ).
        ENDIF.

        IF iv_header_text CN ' _0'.
          CREATE OBJECT lo_table_header EXPORTING text = iv_header_text.
          mo_alv->set_top_of_list( lo_table_header ).
        ENDIF.

        mo_alv->get_display_settings( )->set_striped_pattern( iv_striped_pattern ).
        mo_alv->get_selections( )->set_selection_mode( iv_selection_mode ).

        lo_columns = mo_alv->get_columns( ).
        lo_columns->set_optimize( iv_optimize_col_width ).

        TRY.
            lo_columns->get_column( |{ c_fieldname_obj_type }| ).
            lv_object_list = abap_true.
          CATCH cx_salv_not_found.
        ENDTRY.

        setup_columns(
          io_columns            = lo_columns
          iv_selection_mode     = iv_selection_mode
          iv_select_column_text = iv_select_column_text
          it_columns_to_display = it_columns_to_display ).

        setup_toolbar(
          iv_object_list    = lv_object_list
          iv_selection_mode = iv_selection_mode ).

      CATCH cx_salv_msg.
        Lcx_abapgit_exception=>raise( 'ALV error from object decision list' ).
    ENDTRY.


  ENDMETHOD.

  METHOD create_new_selectable_table.

    " create and populate a table on the fly derived from
    " it_data with a select column

    DATA:
      lr_struct        TYPE REF TO data,
      lt_components    TYPE cl_abap_structdescr=>component_table,
      lo_data_descr    TYPE REF TO cl_abap_datadescr,
      lo_elem_descr    TYPE REF TO cl_abap_elemdescr,
      lo_struct_descr  TYPE REF TO cl_abap_structdescr,
      lo_struct_descr2 TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS:
      <lt_table>     TYPE STANDARD TABLE,
      <ls_component> TYPE abap_componentdescr,
      <ls_line>      TYPE data,
      <lg_data>      TYPE any,
      <lg_value>     TYPE any.

    mo_table_descr ?= cl_abap_tabledescr=>describe_by_data( it_list ).
    lo_data_descr = mo_table_descr->get_table_line_type( ).

    CASE lo_data_descr->kind.
      WHEN cl_abap_elemdescr=>kind_elem.
        lo_elem_descr ?= mo_table_descr->get_table_line_type( ).
        INSERT INITIAL LINE INTO lt_components ASSIGNING <ls_component> INDEX 1.
        <ls_component>-name = c_default_column.
        <ls_component>-type = lo_elem_descr.

      WHEN cl_abap_elemdescr=>kind_struct.
        lo_struct_descr ?= mo_table_descr->get_table_line_type( ).
        lt_components = lo_struct_descr->get_components( ).

    ENDCASE.

    IF lt_components IS INITIAL.
      RETURN.
    ENDIF.

    INSERT INITIAL LINE INTO lt_components ASSIGNING <ls_component> INDEX 1.
    <ls_component>-name = c_fieldname_selected.
    <ls_component>-type ?= cl_abap_datadescr=>describe_by_name( 'FLAG' ).

    lo_struct_descr2 = cl_abap_structdescr=>create( lt_components ).
    mo_table_descr = cl_abap_tabledescr=>create( lo_struct_descr2 ).

    CREATE DATA mr_table TYPE HANDLE mo_table_descr.
    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    CREATE DATA lr_struct TYPE HANDLE lo_struct_descr2.
    ASSIGN lr_struct->* TO <ls_line>.
    ASSERT sy-subrc = 0.

    LOOP AT it_list ASSIGNING <lg_data>.
      CLEAR <ls_line>.
      CASE lo_data_descr->kind.
        WHEN cl_abap_elemdescr=>kind_elem.
          ASSIGN COMPONENT c_default_column OF STRUCTURE <lg_data> TO <lg_value>.
          ASSERT <lg_value> IS ASSIGNED.
          <ls_line> = <lg_value>.

        WHEN OTHERS.
          MOVE-CORRESPONDING <lg_data> TO <ls_line>.

      ENDCASE.
      INSERT <ls_line> INTO TABLE <lt_table>.
    ENDLOOP.

  ENDMETHOD.

  METHOD preselect.

    FIELD-SYMBOLS:
      <lt_table>    TYPE STANDARD TABLE,
      <lv_row>      LIKE LINE OF it_preselected_rows,
      <ls_line>     TYPE any,
      <lv_selected> TYPE data.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    LOOP AT it_preselected_rows ASSIGNING <lv_row>.

      READ TABLE <lt_table> INDEX <lv_row> ASSIGNING <ls_line>.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Preselected row { <lv_row> } doesn't exist| ).
      ENDIF.

      ASSIGN COMPONENT c_fieldname_selected OF STRUCTURE <ls_line> TO <lv_selected>.
      ASSERT sy-subrc = 0.
      <lv_selected> = abap_true.

    ENDLOOP.

  ENDMETHOD.

  METHOD on_double_click.

    DATA lo_selections TYPE REF TO cl_salv_selections.

    lo_selections = mo_alv->get_selections( ).

    IF lo_selections->get_selection_mode( ) = if_salv_c_selection_mode=>single.
      mo_alv->close_screen( ).
    ENDIF.

  ENDMETHOD.

  METHOD on_select_list_function_click.

    " Work for functions of SAPMSVIM and OWN
    CASE e_salv_function.
      WHEN 'O.K.' OR 'OK'.
        mv_cancel = abap_false.
        mo_alv->close_screen( ).

      WHEN 'ABR' OR 'CANCEL'.
        " Canceled: clear list to overwrite nothing
        mv_cancel = abap_true.
        mo_alv->close_screen( ).

      WHEN 'SALL' OR 'SEL_ALL'.
        mark_visible( abap_true ).
        mo_alv->refresh( ).

      WHEN 'DSEL' OR 'SEL_DEL'.
        mark_visible( abap_false ).
        mo_alv->refresh( ).

      WHEN 'SEL_KEY'.
        mark_selected( ).
        mo_alv->refresh( ).

      WHEN 'SEL_CAT'.
        mark_category( ask_user_for_obj_category( ) ).
        mo_alv->refresh( ).

      WHEN OTHERS.
        mv_cancel = abap_true.
        mo_alv->close_screen( ).

    ENDCASE.

  ENDMETHOD.

  METHOD mark_all.

    FIELD-SYMBOLS:
      <lt_table>    TYPE STANDARD TABLE,
      <ls_line>     TYPE any,
      <lv_selected> TYPE abap_bool.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    LOOP AT <lt_table> ASSIGNING <ls_line>.

      ASSIGN COMPONENT c_fieldname_selected OF STRUCTURE <ls_line> TO <lv_selected>.
      ASSERT sy-subrc = 0.
      <lv_selected> = iv_selected.

    ENDLOOP.

  ENDMETHOD.

  METHOD are_all_marked.

    DATA lv_index LIKE LINE OF it_scope.

    FIELD-SYMBOLS:
      <lt_table>    TYPE STANDARD TABLE,
      <ls_line>     TYPE any,
      <lv_selected> TYPE abap_bool.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    LOOP AT it_scope INTO lv_index.

      READ TABLE <lt_table> ASSIGNING <ls_line> INDEX lv_index.
      CHECK sy-subrc = 0.

      ASSIGN COMPONENT c_fieldname_selected OF STRUCTURE <ls_line> TO <lv_selected>.
      ASSERT sy-subrc = 0.

      IF <lv_selected> = abap_true.
        rv_yes = abap_true.
      ELSE.
        rv_yes = abap_false.
        RETURN.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD mark_selected.

    DATA lt_clear TYPE salv_t_row.
    DATA lt_scope TYPE lvc_t_fidx.

    lt_scope = mo_alv->get_selections( )->get_selected_rows( ).

    IF lines( lt_scope ) > 0.
      mark_indexed(
        it_scope    = lt_scope
        iv_selected = boolc( are_all_marked( lt_scope ) = abap_false ) ).
      mo_alv->get_selections( )->set_selected_rows( lt_clear ).
    ELSE.
      MESSAGE 'Select rows first to mark them' TYPE 'S'.
    ENDIF.

  ENDMETHOD.

  METHOD mark_visible.

    DATA lt_filters TYPE lvc_t_filt.
    DATA lt_scope   TYPE lvc_t_fidx.

    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    " If nothing selected, select all VISIBLE
    lt_filters = cl_salv_controller_metadata=>get_lvc_filter( mo_alv->get_filters( ) ).
    IF lines( lt_filters ) = 0.
      mark_all( iv_selected ). " No filters - just select all
      RETURN.
    ENDIF.

    CALL FUNCTION 'LVC_FILTER_APPLY'
      EXPORTING
        it_filter              = lt_filters
      IMPORTING
        et_filter_index_inside = lt_scope
      TABLES
        it_data                = <lt_table>.

    mark_indexed(
      it_scope    = lt_scope
      iv_selected = iv_selected ).

  ENDMETHOD.

  METHOD mark_indexed.

    DATA lv_index LIKE LINE OF it_scope.

    FIELD-SYMBOLS:
      <lt_table>    TYPE STANDARD TABLE,
      <ls_line>     TYPE any,
      <lv_selected> TYPE abap_bool.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    LOOP AT it_scope INTO lv_index.

      READ TABLE <lt_table> ASSIGNING <ls_line> INDEX lv_index.
      CHECK sy-subrc = 0.

      ASSIGN COMPONENT c_fieldname_selected OF STRUCTURE <ls_line> TO <lv_selected>.
      ASSERT sy-subrc = 0.

      IF iv_invert = abap_true.
        <lv_selected> = boolc( <lv_selected> = abap_false ).
      ELSE.
        <lv_selected> = iv_selected.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD ask_user_for_obj_category.

    DATA:
      lv_answer    TYPE c LENGTH 1,
      ls_position  TYPE Lif_abapgit_popups=>ty_popup_position,
      ls_selection TYPE spopli,
      lt_selection TYPE TABLE OF spopli.

    ls_selection-varoption = 'Packages'.
    APPEND ls_selection TO lt_selection.
    ls_selection-varoption = 'DDIC objects'.
    APPEND ls_selection TO lt_selection.
    ls_selection-varoption = 'Source code'.
    APPEND ls_selection TO lt_selection.
    ls_selection-varoption = 'Enhancements'.
    APPEND ls_selection TO lt_selection.

    ls_position-start_column = ms_position-start_column + 20.
    ls_position-start_row    = ms_position-start_row + 5.

    CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
      EXPORTING
        titel      = 'Selection'
        textline1  = 'Which objects should be added to the selection?'
        start_col  = ls_position-start_column
        start_row  = ls_position-start_row
        cursorline = 1
      IMPORTING
        answer     = lv_answer
      TABLES
        t_spopli   = lt_selection
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0 OR lv_answer = c_answer_cancel.
      RETURN.
    ENDIF.

    READ TABLE lt_selection INDEX lv_answer INTO ls_selection.
    IF sy-subrc = 0.
      rv_category = ls_selection-varoption.
    ENDIF.

  ENDMETHOD.

  METHOD mark_category.

    FIELD-SYMBOLS:
      <lt_table>    TYPE STANDARD TABLE,
      <lv_obj_type> TYPE tadir-object,
      <ls_line>     TYPE any,
      <lv_selected> TYPE abap_bool.

    IF iv_category IS INITIAL.
      RETURN.
    ENDIF.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    LOOP AT <lt_table> ASSIGNING <ls_line>.

      ASSIGN COMPONENT c_fieldname_obj_type OF STRUCTURE <ls_line> TO <lv_obj_type>.
      ASSERT sy-subrc = 0.

      ASSIGN COMPONENT c_fieldname_selected OF STRUCTURE <ls_line> TO <lv_selected>.
      ASSERT sy-subrc = 0.

      CASE iv_category.
        WHEN 'Packages'.
          IF <lv_obj_type> <> 'DEVC'.
            CONTINUE.
          ENDIF.
        WHEN 'DDIC objects'.
          IF Lcl_abapgit_objects_activation=>is_ddic_type( <lv_obj_type> ) = abap_false.
            CONTINUE.
          ENDIF.
        WHEN 'Source code'.
          IF 'CLAS,FUGR,INTF,PROG,TYPE' NS <lv_obj_type>.
            CONTINUE.
          ENDIF.
        WHEN 'Enhancements'.
          IF 'ENHO,ENHS,ENHC,ENSC' NS <lv_obj_type>.
            CONTINUE.
          ENDIF.
        WHEN OTHERS.
          RETURN. " Unexpected category
      ENDCASE.

      <lv_selected> = abap_true.

    ENDLOOP.

  ENDMETHOD.

  METHOD on_select_list_link_click.

    FIELD-SYMBOLS:
      <lt_table>    TYPE STANDARD TABLE,
      <ls_line>     TYPE any,
      <lv_selected> TYPE abap_bool.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    READ TABLE <lt_table> ASSIGNING <ls_line> INDEX row.
    IF sy-subrc = 0.

      ASSIGN COMPONENT c_fieldname_selected OF STRUCTURE <ls_line> TO <lv_selected>.
      ASSERT sy-subrc = 0.
      <lv_selected> = boolc( <lv_selected> = abap_false ).

    ENDIF.

    mo_alv->refresh( ).

  ENDMETHOD.

  METHOD setup_columns.

    DATA:
      lt_columns TYPE salv_t_column_ref,
      ls_column  TYPE salv_s_column_ref,
      lo_column  TYPE REF TO cl_salv_column_list.

    FIELD-SYMBOLS <ls_column_to_display> TYPE Lif_abapgit_popups=>ty_alv_column.

    lt_columns = io_columns->get( ).

    LOOP AT lt_columns INTO ls_column.

      lo_column ?= ls_column-r_column.

      IF iv_selection_mode    = if_salv_c_selection_mode=>multiple AND
         ls_column-columnname = c_fieldname_selected.
        lo_column->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ).
        lo_column->set_output_length( 20 ).
        lo_column->set_short_text( |{ iv_select_column_text }| ).
        lo_column->set_medium_text( |{ iv_select_column_text }| ).
        lo_column->set_long_text( |{ iv_select_column_text }| ).
        CONTINUE.
      ENDIF.

      READ TABLE it_columns_to_display
        ASSIGNING <ls_column_to_display>
        WITH KEY name = ls_column-columnname.

      CASE sy-subrc.
        WHEN 0.
          IF <ls_column_to_display>-text CN ' _0'.
            lo_column->set_short_text( |{ <ls_column_to_display>-text }| ).
            lo_column->set_medium_text( |{ <ls_column_to_display>-text }| ).
            lo_column->set_long_text( |{ <ls_column_to_display>-text }| ).
          ENDIF.

          IF <ls_column_to_display>-length > 0.
            lo_column->set_output_length( <ls_column_to_display>-length ).
          ENDIF.

          IF <ls_column_to_display>-show_icon = abap_true.
            lo_column->set_icon( abap_true ).
          ENDIF.

          IF <ls_column_to_display>-center = abap_true.
            lo_column->set_alignment( if_salv_c_alignment=>centered ).
          ENDIF.

        WHEN OTHERS.
          lo_column->set_technical( abap_true ). " Hide column

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD setup_toolbar.

    DATA:
      lv_report    TYPE sy-repid,
      lv_pfstatus  TYPE sy-pfkey,
      lo_functions TYPE REF TO cl_salv_functions_list,
      lt_func_list TYPE salv_t_ui_func,
      lv_fn        TYPE string,
      ls_func      LIKE LINE OF lt_func_list.

    CALL FUNCTION 'RS_CUA_STATUS_CHECK'
      EXPORTING
        program          = sy-cprog
        objectname       = c_own_pfstatus
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.

    IF sy-subrc = 0.

      mo_alv->set_screen_status(
        report   = sy-cprog
        pfstatus = c_own_pfstatus ).

    ELSE.

      lv_report  = 'SAPMSVIM'.

      IF iv_selection_mode = if_salv_c_selection_mode=>single.
        lv_pfstatus = '110'.
      ELSE.
        lv_pfstatus = '102'.
      ENDIF.

      mo_alv->set_screen_status(
        report   = lv_report
        pfstatus = lv_pfstatus ).

    ENDIF.

    lo_functions = mo_alv->get_functions( ).

    lt_func_list = lo_functions->get_functions( ).
    LOOP AT lt_func_list INTO ls_func.
      lv_fn = ls_func-r_function->get_name( ).
      IF lv_fn = 'OK' OR lv_fn = 'CANCEL'.
        ls_func-r_function->set_visible( abap_true ).
      ELSEIF iv_object_list = abap_true.
        ls_func-r_function->set_visible( abap_true ).
      ELSE.
        ls_func-r_function->set_visible( abap_false ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

class LCL_ABAPGIT_POPUPS implementation.
*"* method's implementations
*include methods.
  METHOD add_field.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF ct_fields.

    APPEND INITIAL LINE TO ct_fields ASSIGNING <ls_field>.
    <ls_field>-tabname    = iv_tabname.
    <ls_field>-fieldname  = iv_fieldname.
    <ls_field>-fieldtext  = iv_fieldtext.
    <ls_field>-value      = iv_value.
    <ls_field>-field_attr = iv_field_attr.
    <ls_field>-field_obl  = iv_obligatory.

  ENDMETHOD.
  METHOD center.

    CONSTANTS:
      lc_min_size TYPE i VALUE 10,
      lc_min_pos  TYPE i VALUE 5.

    " Magic math to approximate starting position of popup
    IF sy-scols > lc_min_size AND iv_width > 0 AND sy-scols > iv_width.
      rs_position-start_column = nmax(
        val1 = ( sy-scols - iv_width ) / 2
        val2 = lc_min_pos ).
    ELSE.
      rs_position-start_column = lc_min_pos.
    ENDIF.

    IF sy-srows > lc_min_size AND iv_height > 0 AND sy-srows > iv_height.
      rs_position-start_row = nmax(
        val1 = ( sy-srows - iv_height ) / 2 - 1
        val2 = lc_min_pos ).
    ELSE.
      rs_position-start_row = lc_min_pos.
    ENDIF.

    rs_position-end_column = rs_position-start_column + iv_width.
    rs_position-end_row = rs_position-start_row + iv_height.

  ENDMETHOD.
  METHOD commit_list_build.

    DATA:
      lv_unix_time   TYPE Lcl_abapgit_git_time=>ty_unixtime,
      lv_date        TYPE d,
      lv_date_string TYPE c LENGTH 12,
      lv_time        TYPE t,
      lv_time_string TYPE c LENGTH 10.

    FIELD-SYMBOLS:
      <ls_commit>    TYPE Lif_abapgit_git_definitions=>ty_commit,
      <ls_value_tab> TYPE ty_commit_value_tab.

    CLEAR: et_commits, et_value_tab.

    et_commits = Lcl_abapgit_git_commit=>get_by_branch( iv_branch_name  = iv_branch_name
                                                        iv_repo_url     = iv_repo_url
                                                        iv_deepen_level = 99
                                                        iv_sorted       = abap_false )-commits.

    IF et_commits IS INITIAL.
      Lcx_abapgit_exception=>raise( |No commits are available in this branch.| ).
    ENDIF.

    SORT et_commits BY time DESCENDING.

    LOOP AT et_commits ASSIGNING <ls_commit>.

      APPEND INITIAL LINE TO et_value_tab ASSIGNING <ls_value_tab>.
      <ls_value_tab>-commit  = <ls_commit>-sha1.
      <ls_value_tab>-message = <ls_commit>-message.
      lv_unix_time = <ls_commit>-time.
      Lcl_abapgit_git_time=>get_utc(
        EXPORTING
          iv_unix = lv_unix_time
        IMPORTING
          ev_time = lv_time
          ev_date = lv_date ).
      WRITE: lv_date TO lv_date_string,
             lv_time TO lv_time_string.
      <ls_value_tab>-datetime = |{ lv_date_string }, | &&
                                |{ lv_time_string }|.

    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_popups~branch_list_popup.

    DATA: lo_branches    TYPE REF TO Lcl_abapgit_git_branch_list,
          lt_branches    TYPE Lif_abapgit_git_definitions=>ty_git_branch_list_tt,
          lv_answer      TYPE c LENGTH 1,
          lv_default     TYPE i,
          lv_head_suffix TYPE string,
          lv_head_symref TYPE string,
          lv_text        TYPE string,
          lt_selection   TYPE TABLE OF spopli.

    FIELD-SYMBOLS: <ls_sel>    LIKE LINE OF lt_selection,
                   <ls_branch> LIKE LINE OF lt_branches.


    lo_branches    = Lcl_abapgit_git_transport=>branches( iv_url ).
    lt_branches    = lo_branches->get_branches_only( ).
    lv_head_suffix = | ({ Lif_abapgit_git_definitions=>c_head_name })|.
    lv_head_symref = lo_branches->get_head_symref( ).

    IF iv_hide_branch IS NOT INITIAL.
      DELETE lt_branches WHERE name = iv_hide_branch.
    ENDIF.

    IF iv_hide_head IS NOT INITIAL.
      DELETE lt_branches WHERE name    = Lif_abapgit_git_definitions=>c_head_name
                            OR is_head = abap_true.
    ENDIF.

    IF lt_branches IS INITIAL.
      IF iv_hide_head IS NOT INITIAL.
        lv_text = 'main'.
      ENDIF.
      IF iv_hide_branch IS NOT INITIAL AND iv_hide_branch <> Lif_abapgit_git_definitions=>c_git_branch-main.
        IF lv_text IS INITIAL.
          lv_text = iv_hide_branch && ' is'.
        ELSE.
          CONCATENATE lv_text 'and' iv_hide_branch 'are' INTO lv_text SEPARATED BY space.
        ENDIF.
      ELSE.
        lv_text = lv_text && ' is'.
      ENDIF.
      IF lv_text IS NOT INITIAL.
        Lcx_abapgit_exception=>raise( 'No branches available to select (' && lv_text && ' hidden)' ).
      ELSE.
        Lcx_abapgit_exception=>raise( 'No branches are available to select' ).
      ENDIF.
    ENDIF.

    LOOP AT lt_branches ASSIGNING <ls_branch>.

      CHECK <ls_branch>-name IS NOT INITIAL. " To ensure some below ifs

      IF <ls_branch>-is_head = abap_true.

        IF <ls_branch>-name = Lif_abapgit_git_definitions=>c_head_name. " HEAD
          IF <ls_branch>-name <> lv_head_symref AND lv_head_symref IS NOT INITIAL.
            " HEAD but other HEAD symref exists - ignore
            CONTINUE.
          ELSE.
            INSERT INITIAL LINE INTO lt_selection INDEX 1 ASSIGNING <ls_sel>.
            <ls_sel>-varoption = <ls_branch>-name.
          ENDIF.
        ELSE.
          INSERT INITIAL LINE INTO lt_selection INDEX 1 ASSIGNING <ls_sel>.
          <ls_sel>-varoption = <ls_branch>-display_name && lv_head_suffix.
        ENDIF.

        IF lv_default > 0. " Shift down default if set
          lv_default = lv_default + 1.
        ENDIF.
      ELSE.
        APPEND INITIAL LINE TO lt_selection ASSIGNING <ls_sel>.
        <ls_sel>-varoption = <ls_branch>-display_name.
      ENDIF.

      IF <ls_branch>-name = iv_default_branch.
        IF <ls_branch>-is_head = abap_true.
          lv_default = 1.
        ELSE.
          lv_default = sy-tabix.
        ENDIF.
      ENDIF.

    ENDLOOP.

    IF iv_show_new_option = abap_true.
      APPEND INITIAL LINE TO lt_selection ASSIGNING <ls_sel>.
      <ls_sel>-varoption = Lif_abapgit_popups=>c_new_branch_label.
    ENDIF.

    ms_position = center(
      iv_width  = 30
      iv_height = lines( lt_selection ) ).

    CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
      EXPORTING
        titel      = 'Select Branch'
        textline1  = 'Select a branch'
        start_col  = ms_position-start_column
        start_row  = ms_position-start_row
        cursorline = lv_default
      IMPORTING
        answer     = lv_answer
      TABLES
        t_spopli   = lt_selection
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Error from POPUP_TO_DECIDE_LIST' ).
    ENDIF.

    IF lv_answer = c_answer_cancel.
      RETURN.
    ENDIF.

    READ TABLE lt_selection ASSIGNING <ls_sel> WITH KEY selflag = abap_true.
    ASSERT sy-subrc = 0.

    IF iv_show_new_option = abap_true AND <ls_sel>-varoption = Lif_abapgit_popups=>c_new_branch_label.
      rs_branch-name = Lif_abapgit_popups=>c_new_branch_label.
    ELSE.
      REPLACE FIRST OCCURRENCE OF lv_head_suffix IN <ls_sel>-varoption WITH ''.
      READ TABLE lt_branches WITH KEY display_name = <ls_sel>-varoption ASSIGNING <ls_branch>.
      IF sy-subrc <> 0.
* branch name longer than 65 characters
        LOOP AT lt_branches ASSIGNING <ls_branch> WHERE display_name CS <ls_sel>-varoption.
          EXIT. " current loop
        ENDLOOP.
      ENDIF.
      ASSERT <ls_branch> IS ASSIGNED.
      rs_branch = lo_branches->find_by_name( <ls_branch>-name ).
      lv_text = |Branch switched from { Lcl_abapgit_git_branch_list=>get_display_name( iv_default_branch ) } to {
        Lcl_abapgit_git_branch_list=>get_display_name( rs_branch-name ) } |.
      MESSAGE lv_text TYPE 'S'.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_popups~choose_code_insp_check_variant.

    DATA: lt_return TYPE STANDARD TABLE OF ddshretval.

    FIELD-SYMBOLS: <ls_return> LIKE LINE OF lt_return.

    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname           = 'SCI_DYNP'
        fieldname         = 'CHKV'
      TABLES
        return_tab        = lt_return
      EXCEPTIONS
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        no_values_found   = 4
        OTHERS            = 5.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    READ TABLE lt_return ASSIGNING <ls_return>
                         WITH KEY retfield = 'SCI_DYNP-CHKV'.
    IF sy-subrc = 0.
      rv_check_variant = <ls_return>-fieldval.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_popups~commit_list_popup.

    DATA:
      lt_commits         TYPE Lif_abapgit_git_definitions=>ty_commit_tt,
      lt_value_tab       TYPE ty_commit_value_tab_tt,
      lt_selected_values TYPE ty_commit_value_tab_tt,
      lt_columns         TYPE Lif_abapgit_popups=>ty_alv_column_tt.

    FIELD-SYMBOLS:
      <ls_value_tab> TYPE ty_commit_value_tab,
      <ls_column>    TYPE Lif_abapgit_popups=>ty_alv_column.

    commit_list_build(
      EXPORTING
        iv_branch_name = iv_branch_name
        iv_repo_url    = iv_repo_url
      IMPORTING
        et_value_tab   = lt_value_tab
        et_commits     = lt_commits ).

    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name   = 'COMMIT'.
    <ls_column>-text   = 'Hash'.
    <ls_column>-length = 8.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'MESSAGE'.
    <ls_column>-text = 'Message'.
    <ls_column>-length = 60.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'DATETIME'.
    <ls_column>-text = 'Datetime'.
    <ls_column>-length = 17.

    Lif_abapgit_popups~popup_to_select_from_list(
      EXPORTING
        it_list               = lt_value_tab
        iv_title              = |Select a commit|
        iv_end_column         = 100
        iv_striped_pattern    = abap_true
        iv_optimize_col_width = abap_false
        iv_selection_mode     = if_salv_c_selection_mode=>single
        it_columns_to_display = lt_columns
      IMPORTING
        et_list               = lt_selected_values ).

    IF lt_selected_values IS INITIAL.
      RAISE EXCEPTION TYPE Lcx_abapgit_cancel.
    ENDIF.

    READ TABLE lt_selected_values ASSIGNING <ls_value_tab> INDEX 1.
    ASSERT sy-subrc = 0.

    READ TABLE lt_commits INTO rs_commit WITH KEY sha1 = <ls_value_tab>-commit.
    ASSERT sy-subrc = 0.

  ENDMETHOD.
  METHOD Lif_abapgit_popups~create_branch_popup.

    DATA: lt_fields TYPE TABLE OF sval.
    DATA: lv_name   TYPE spo_value.

    CLEAR: ev_name, ev_cancel.

    add_field( EXPORTING iv_tabname   = 'TEXTL'
                         iv_fieldname = 'LINE'
                         iv_fieldtext = 'Name'
                         iv_value     = 'new-branch-name'
               CHANGING  ct_fields    = lt_fields ).

    TRY.

        _popup_3_get_values(
          EXPORTING iv_popup_title = |Create branch from {
            Lcl_abapgit_git_branch_list=>get_display_name( iv_source_branch_name ) }|
          IMPORTING ev_value_1     = lv_name
          CHANGING  ct_fields      = lt_fields ).

        ev_name = Lcl_abapgit_git_branch_list=>complete_heads_branch_name(
              Lcl_abapgit_git_branch_list=>normalize_branch_name( lv_name ) ).

      CATCH Lcx_abapgit_cancel.
        ev_cancel = abap_true.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_popups~popup_folder_logic.

    DATA:
      lt_selection TYPE TABLE OF spopli,
      lv_answer    TYPE c LENGTH 1.

    FIELD-SYMBOLS: <ls_sel> LIKE LINE OF lt_selection.

    APPEND INITIAL LINE TO lt_selection ASSIGNING <ls_sel>.
    <ls_sel>-selflag   = abap_true.
    <ls_sel>-varoption = Lif_abapgit_dot_abapgit=>c_folder_logic-prefix.

    APPEND INITIAL LINE TO lt_selection ASSIGNING <ls_sel>.
    <ls_sel>-varoption = Lif_abapgit_dot_abapgit=>c_folder_logic-full.

    APPEND INITIAL LINE TO lt_selection ASSIGNING <ls_sel>.
    <ls_sel>-varoption = Lif_abapgit_dot_abapgit=>c_folder_logic-mixed.

    CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
      EXPORTING
        titel     = 'Folder logic'
        textline1 = 'Select folder logic'
        start_col = ms_position-start_column
        start_row = ms_position-start_row
      IMPORTING
        answer    = lv_answer
      TABLES
        t_spopli  = lt_selection
      EXCEPTIONS
        OTHERS    = 1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Error from POPUP_TO_DECIDE_LIST' ).
    ENDIF.

    IF lv_answer = c_answer_cancel.
      Lcx_abapgit_cancel=>raise( |Canceled| ).
    ENDIF.

    READ TABLE lt_selection ASSIGNING <ls_sel> WITH KEY selflag = abap_true.
    ASSERT sy-subrc = 0.

    rv_folder_logic = <ls_sel>-varoption.

  ENDMETHOD.
  METHOD Lif_abapgit_popups~popup_search_help.

    DATA lt_ret TYPE TABLE OF ddshretval.
    DATA ls_ret LIKE LINE OF lt_ret.
    DATA lv_tabname TYPE dfies-tabname.
    DATA lv_fieldname TYPE dfies-fieldname.

    SPLIT iv_tab_field AT '-' INTO lv_tabname lv_fieldname.
    lv_tabname = to_upper( lv_tabname ).
    lv_fieldname = to_upper( lv_fieldname ).

    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname    = lv_tabname
        fieldname  = lv_fieldname
      TABLES
        return_tab = lt_ret
      EXCEPTIONS
        OTHERS     = 5.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |F4IF_FIELD_VALUE_REQUEST error [{ iv_tab_field }]| ).
    ENDIF.

    IF lines( lt_ret ) > 0.
      READ TABLE lt_ret WITH KEY fieldname = lv_fieldname INTO ls_ret.
      IF sy-subrc = 0.
        rv_value = ls_ret-fieldval.
      ELSE.
        READ TABLE lt_ret INDEX 1 INTO ls_ret.
        ASSERT sy-subrc = 0.
        rv_value = ls_ret-fieldval.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_popups~popup_select_tr_requests.
    DATA ls_r_trkorr TYPE LINE OF Lif_abapgit_definitions=>ty_trrngtrkor_tt.
    DATA lr_request TYPE REF TO trwbo_request_header.
    DATA lt_request TYPE trwbo_request_headers.

    ms_position = center(
      iv_width  = 120
      iv_height = 10 ).

    CALL FUNCTION 'TRINT_SELECT_REQUESTS'
      EXPORTING
        iv_username_pattern    = iv_username_pattern
        is_selection           = is_selection
        iv_complete_projects   = abap_false
        is_popup               = ms_position
        iv_via_selscreen       = 'X'
        iv_title               = iv_title
      IMPORTING
        et_requests            = lt_request
      EXCEPTIONS
        action_aborted_by_user = 1
        OTHERS                 = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Selection canceled' ).
    ENDIF.

    IF lt_request IS INITIAL.
      Lcx_abapgit_exception=>raise( 'No Request Found' ).
    ENDIF.

    IF lines( lt_request ) > 10000.
      Lcx_abapgit_exception=>raise( 'Too many requests selected (max 10000)' ).
    ENDIF.

    LOOP AT lt_request REFERENCE INTO lr_request.
      ls_r_trkorr-sign = 'I'.
      ls_r_trkorr-option = 'EQ'.
      ls_r_trkorr-low = lr_request->trkorr.
      INSERT ls_r_trkorr INTO TABLE rt_r_trkorr.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_popups~popup_select_wb_tc_tr_and_tsk.
    DATA ls_selection  TYPE trwbo_selection.
    DATA lv_title TYPE trwbo_title.

    ls_selection-trkorrpattern = space.
    ls_selection-connect_req_task_conditions = 'X'.
    ls_selection-reqfunctions = 'KTRXS'.
    ls_selection-reqstatus = 'RNODL'.
    ls_selection-taskstatus = 'RNODL'.
    CONDENSE ls_selection-reqfunctions NO-GAPS.
    ls_selection-taskfunctions = 'QRSX'.
    CONCATENATE sy-sysid '*' INTO ls_selection-trkorrpattern.

    lv_title = 'Select Transports / Tasks'.

    rt_r_trkorr = Lif_abapgit_popups~popup_select_tr_requests(
      is_selection        = ls_selection
      iv_title            = lv_title
      iv_username_pattern = '*' ).
  ENDMETHOD.
  METHOD Lif_abapgit_popups~popup_to_confirm.

    ms_position = center(
      iv_width  = 65
      iv_height = 5 ).

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = iv_titlebar
        text_question         = iv_text_question
        text_button_1         = iv_text_button_1
        icon_button_1         = iv_icon_button_1
        text_button_2         = iv_text_button_2
        icon_button_2         = iv_icon_button_2
        default_button        = iv_default_button
        display_cancel_button = iv_display_cancel_button
        popup_type            = iv_popup_type
        start_column          = ms_position-start_column
        start_row             = ms_position-start_row
      IMPORTING
        answer                = rv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from POPUP_TO_CONFIRM' ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_popups~popup_to_create_package.
    IF Lcl_abapgit_factory=>get_function_module( )->function_exists( 'PB_POPUP_PACKAGE_CREATE' ) = abap_false.
* looks like the function module used does not exist on all
* versions since 702, so show an error
      Lcx_abapgit_exception=>raise( 'Your system does not support automatic creation of packages.' &&
        'Please, create the package manually.' ).
    ENDIF.

    CALL FUNCTION 'PB_POPUP_PACKAGE_CREATE'
      CHANGING
        p_object_data    = es_package_data
      EXCEPTIONS
        action_cancelled = 1.
    ev_create = boolc( sy-subrc = 0 ).
  ENDMETHOD.
  METHOD Lif_abapgit_popups~popup_to_create_transp_branch.
    DATA: lt_fields             TYPE TABLE OF sval,
          lv_transports_as_text TYPE string,
          lv_desc_as_text       TYPE string,
          ls_transport_header   LIKE LINE OF it_transport_headers.
    DATA: lv_branch_name        TYPE spo_value.
    DATA: lv_commit_text        TYPE spo_value.

    CLEAR: rs_transport_branch-branch_name, rs_transport_branch-commit_text.

    " If we only have one transport selected set branch name to Transport
    " name and commit description to transport description.
    IF lines( it_transport_headers ) = 1.
      READ TABLE it_transport_headers INDEX 1 INTO ls_transport_header.
      lv_transports_as_text = ls_transport_header-trkorr.
      lv_desc_as_text = Lcl_abapgit_factory=>get_cts_api( )->read_description( ls_transport_header-trkorr ).
    ELSE.   " Else set branch name and commit message to 'Transport(s)_TRXXXXXX_TRXXXXX'
      lv_transports_as_text = 'Transport(s)'.
      LOOP AT it_transport_headers INTO ls_transport_header.
        CONCATENATE lv_transports_as_text '_' ls_transport_header-trkorr INTO lv_transports_as_text.
      ENDLOOP.
      lv_desc_as_text = lv_transports_as_text.

    ENDIF.
    add_field( EXPORTING iv_tabname   = 'TEXTL'
                         iv_fieldname = 'LINE'
                         iv_fieldtext = 'Branch name'
                         iv_value     = lv_transports_as_text
               CHANGING  ct_fields    = lt_fields ).

    add_field( EXPORTING iv_tabname   = 'ABAPTXT255'
                         iv_fieldname = 'LINE'
                         iv_fieldtext = 'Commit text'
                         iv_value     = lv_desc_as_text
               CHANGING  ct_fields    = lt_fields ).

    _popup_3_get_values( EXPORTING iv_popup_title = 'Transport to new Branch'
                         IMPORTING ev_value_1     = lv_branch_name
                                   ev_value_2     = lv_commit_text
                         CHANGING  ct_fields      = lt_fields ).

    rs_transport_branch-branch_name = lv_branch_name.
    rs_transport_branch-commit_text = lv_commit_text.

  ENDMETHOD.
  METHOD Lif_abapgit_popups~popup_to_select_from_list.

    DATA lo_popup TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLAS76BTU.

    CLEAR et_list.

    ms_position = center(
      iv_width  = iv_end_column - iv_start_column
      iv_height = iv_end_line - iv_start_line ).

    CREATE OBJECT lo_popup
      EXPORTING
        it_list               = it_list
        iv_title              = iv_title
        iv_header_text        = iv_header_text
        is_position           = ms_position
        iv_striped_pattern    = iv_striped_pattern
        iv_optimize_col_width = iv_optimize_col_width
        iv_selection_mode     = iv_selection_mode
        iv_select_column_text = iv_select_column_text
        it_columns_to_display = it_columns_to_display
        it_preselected_rows   = it_preselected_rows.

    lo_popup->display( ).
    lo_popup->get_selected( IMPORTING et_list = et_list ).

  ENDMETHOD.
  METHOD Lif_abapgit_popups~popup_to_select_labels.

    DATA:
      lt_all_labels         TYPE Lif_abapgit_repo_srv=>ty_labels,
      ls_label              LIKE LINE OF lt_all_labels,
      lt_current_labels     TYPE string_table,
      lt_selected_labels    LIKE lt_all_labels,
      lt_columns_to_display TYPE Lif_abapgit_popups=>ty_alv_column_tt,
      lt_preselected_rows   TYPE Lif_abapgit_popups=>ty_rows,
      ls_columns_to_display LIKE LINE OF lt_columns_to_display,
      lv_save_tabix         TYPE i,
      li_popup              TYPE REF TO Lif_abapgit_popups.

    FIELD-SYMBOLS: <lv_label>         TYPE Lif_abapgit_repo_srv=>ty_label,
                   <lv_current_label> TYPE LINE OF string_table.

    lt_current_labels = Lcl_abapgit_repo_labels=>split( iv_labels ).

    lt_all_labels = Lcl_abapgit_repo_srv=>get_instance( )->get_label_list( ).

    " Add labels which are not saved yet
    LOOP AT lt_current_labels ASSIGNING <lv_current_label>.

      READ TABLE lt_all_labels TRANSPORTING NO FIELDS
                               WITH KEY key_label
                               COMPONENTS label = <lv_current_label>.
      IF sy-subrc <> 0.
        ls_label-label = <lv_current_label>.
        INSERT ls_label INTO TABLE lt_all_labels.
      ENDIF.

    ENDLOOP.

    IF lines( lt_all_labels ) = 0.
      Lcx_abapgit_exception=>raise( |No labels maintained yet| ).
    ENDIF.

    SORT lt_all_labels.
    DELETE ADJACENT DUPLICATES FROM lt_all_labels.

    " Preselect current labels
    LOOP AT lt_all_labels ASSIGNING <lv_label>.

      lv_save_tabix = sy-tabix.

      READ TABLE lt_current_labels TRANSPORTING NO FIELDS
                                   WITH KEY table_line = <lv_label>-label.
      IF sy-subrc = 0.
        INSERT lv_save_tabix INTO TABLE lt_preselected_rows.
      ENDIF.

    ENDLOOP.

    ls_columns_to_display-name = 'LABEL'.
    ls_columns_to_display-text = 'Label'.
    INSERT ls_columns_to_display INTO TABLE lt_columns_to_display.

    li_popup = Lcl_abapgit_ui_factory=>get_popups( ).
    li_popup->popup_to_select_from_list(
      EXPORTING
        iv_header_text        = 'Select labels'
        iv_select_column_text = 'Add label'
        it_list               = lt_all_labels
        iv_selection_mode     = if_salv_c_selection_mode=>multiple
        it_columns_to_display = lt_columns_to_display
        it_preselected_rows   = lt_preselected_rows
        iv_start_column       = 15
        iv_end_column         = 55
      IMPORTING
        et_list               = lt_selected_labels ).

    LOOP AT lt_selected_labels ASSIGNING <lv_label>.
      IF rv_labels IS NOT INITIAL.
        rv_labels = rv_labels && ','.
      ENDIF.
      rv_labels = rv_labels && <lv_label>-label.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_popups~popup_to_select_transports.

* todo, method to be renamed, it only returns one transport

    DATA: lv_trkorr TYPE e070-trkorr,
          ls_trkorr LIKE LINE OF rt_trkorr.


    CALL FUNCTION 'TR_F4_REQUESTS'
      IMPORTING
        ev_selected_request = lv_trkorr.

    IF NOT lv_trkorr IS INITIAL.
      ls_trkorr-trkorr = lv_trkorr.
      APPEND ls_trkorr TO rt_trkorr.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_popups~popup_transport_request.

    DATA: lt_e071    TYPE STANDARD TABLE OF e071,
          lt_e071k   TYPE STANDARD TABLE OF e071k,
          lv_order   TYPE trkorr,
          ls_e070use TYPE e070use.
    DATA lv_category TYPE e070-korrdev.

    " If default transport is set and its type matches, then use it as default for the popup
    ls_e070use = Lcl_abapgit_default_transport=>get_instance( )->get( ).

    IF ( ls_e070use-trfunction = is_transport_type-request OR ls_e070use-trfunction IS INITIAL )
      AND iv_use_default_transport = abap_true.
      lv_order = ls_e070use-ordernum.
    ENDIF.

    " Differentiate between customizing and WB requests
    IF is_transport_type-request = Lif_abapgit_cts_api=>c_transport_type-cust_request.
      lv_category = Lif_abapgit_cts_api=>c_transport_category-customizing.
    ELSE.
      lv_category = Lif_abapgit_cts_api=>c_transport_category-workbench.
    ENDIF.

    CALL FUNCTION 'TRINT_ORDER_CHOICE'
      EXPORTING
        wi_order_type          = is_transport_type-request
        wi_task_type           = is_transport_type-task
        wi_category            = lv_category
        wi_order               = lv_order
      IMPORTING
        we_order               = rv_transport
      TABLES
        wt_e071                = lt_e071
        wt_e071k               = lt_e071k
      EXCEPTIONS
        no_correction_selected = 1
        display_mode           = 2
        object_append_error    = 3
        recursive_call         = 4
        wrong_order_type       = 5
        OTHERS                 = 6.

    IF sy-subrc = 1.
      RAISE EXCEPTION TYPE Lcx_abapgit_cancel.
    ELSEIF sy-subrc > 1.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_popups~tag_list_popup.

    DATA: lo_branches  TYPE REF TO Lcl_abapgit_git_branch_list,
          lt_tags      TYPE Lif_abapgit_git_definitions=>ty_git_branch_list_tt,
          ls_branch    TYPE Lif_abapgit_git_definitions=>ty_git_branch,
          lv_answer    TYPE c LENGTH 1,
          lv_default   TYPE i,
          lv_tag       TYPE string,
          lt_selection TYPE TABLE OF spopli.

    FIELD-SYMBOLS: <ls_sel> LIKE LINE OF lt_selection,
                   <ls_tag> LIKE LINE OF lt_tags.


    lo_branches = Lcl_abapgit_git_transport=>branches( iv_url ).
    lt_tags     = lo_branches->get_tags_only( ).

    LOOP AT lt_tags ASSIGNING <ls_tag> WHERE name NP '*' && Lif_abapgit_git_definitions=>c_git_branch-peel.

      APPEND INITIAL LINE TO lt_selection ASSIGNING <ls_sel>.
      <ls_sel>-varoption = Lcl_abapgit_git_tag=>remove_tag_prefix( <ls_tag>-name ).

    ENDLOOP.

    IF lt_selection IS INITIAL.
      Lcx_abapgit_exception=>raise( 'No tags are available to select' ).
    ENDIF.

    ms_position = center(
      iv_width  = 30
      iv_height = lines( lt_selection ) ).

    CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
      EXPORTING
        titel      = 'Select Tag'
        textline1  = 'Select a tag'
        start_col  = ms_position-start_column
        start_row  = ms_position-start_row
        cursorline = lv_default
      IMPORTING
        answer     = lv_answer
      TABLES
        t_spopli   = lt_selection
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Error from POPUP_TO_DECIDE_LIST' ).
    ENDIF.

    IF lv_answer = c_answer_cancel.
      RETURN.
    ENDIF.

    READ TABLE lt_selection ASSIGNING <ls_sel> WITH KEY selflag = abap_true.
    ASSERT sy-subrc = 0.

    lv_tag = Lcl_abapgit_git_tag=>add_tag_prefix( <ls_sel>-varoption ).

    READ TABLE lt_tags WITH KEY name_key COMPONENTS name = lv_tag ASSIGNING <ls_tag>.
    IF sy-subrc <> 0.
      " tag name longer than 65 characters
      LOOP AT lt_tags ASSIGNING <ls_tag> WHERE name CS lv_tag.
        EXIT.
      ENDLOOP.
    ENDIF.
    ASSERT <ls_tag> IS ASSIGNED.

    ls_branch = lo_branches->find_by_name( <ls_tag>-name ).
    MOVE-CORRESPONDING ls_branch TO rs_tag.

  ENDMETHOD.
  METHOD _popup_3_get_values.

    DATA lv_answer TYPE c LENGTH 1.
    FIELD-SYMBOLS: <ls_field> TYPE sval.

    ms_position = center(
      iv_width  = 120
      iv_height = lines( ct_fields ) ).

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        no_value_check = iv_no_value_check
        popup_title    = iv_popup_title
        start_column   = ms_position-start_column
        start_row      = ms_position-start_row
      IMPORTING
        returncode     = lv_answer
      TABLES
        fields         = ct_fields
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Error from POPUP_GET_VALUES' ).
    ENDIF.

    IF lv_answer = c_answer_cancel.
      RAISE EXCEPTION TYPE Lcx_abapgit_cancel.
    ENDIF.

    IF ev_value_1 IS SUPPLIED.
      READ TABLE ct_fields INDEX 1 ASSIGNING <ls_field>.
      ASSERT sy-subrc = 0.
      ev_value_1 = <ls_field>-value.
    ENDIF.

    IF ev_value_2 IS SUPPLIED.
      READ TABLE ct_fields INDEX 2 ASSIGNING <ls_field>.
      ASSERT sy-subrc = 0.
      ev_value_2 = <ls_field>-value.
    ENDIF.

    IF ev_value_3 IS SUPPLIED.
      READ TABLE ct_fields INDEX 3 ASSIGNING <ls_field>.
      ASSERT sy-subrc = 0.
      ev_value_3 = <ls_field>-value.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_POPUPS implementation

*>>>>>>> ZCL_ABAPGIT_UI_INJECTOR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ui_injector=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ui_injector=======ccimp.
CLASS SHRIS5ZPAUXVKEPN5HWETLLATAABTU DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_gui_services.
    CLASS-METHODS create
      RETURNING
        VALUE(ro_instance) TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLATAABTU.
ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLATAABTU IMPLEMENTATION.
  METHOD create.
    CREATE OBJECT ro_instance.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_services~cache_asset.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_services~register_event_handler.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_services~get_current_page_name.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_services~get_hotkeys_ctl.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_services~get_html_parts.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_services~get_log.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_services~register_page_asset.
  ENDMETHOD.

ENDCLASS.

*"* test class
*include zcl_abapgit_ui_injector=======ccau.






class LCL_ABAPGIT_UI_INJECTOR implementation.
*"* method's implementations
*include methods.
  METHOD get_dummy_gui_services.

    ri_gui_services = SHRIS5ZPAUXVKEPN5HWETLLATAABTU=>create( ).

  ENDMETHOD.
  METHOD set_frontend_services.

    Lcl_abapgit_ui_factory=>gi_fe_services = ii_fe_serv.

  ENDMETHOD.
  METHOD set_gui_services.

    Lcl_abapgit_ui_factory=>gi_gui_services = ii_gui_services.

  ENDMETHOD.
  METHOD set_html_viewer.

    Lcl_abapgit_ui_factory=>gi_html_viewer = ii_html_viewer.

  ENDMETHOD.
  METHOD set_popups.

    Lcl_abapgit_ui_factory=>gi_popups = ii_popups.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_UI_INJECTOR implementation

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
lo_buf->add( '.rl-orrange {' ).
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
lo_buf->add( '  font-size: smaller;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog li.dialog-commands input[type="button"],' ).
lo_buf->add( '.dialog li.dialog-commands input[type="submit"] {' ).
lo_buf->add( '  border: 1px solid;' ).
lo_buf->add( '  padding: 6px 12px;' ).
lo_buf->add( '  border-radius: 3px;' ).
lo_buf->add( '  cursor: pointer;' ).
lo_buf->add( '}' ).
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
lo_buf->add( '.dialog li.dialog-commands a {' ).
lo_buf->add( '  border-color: #ccc;' ).
lo_buf->add( '  background-color: #ddd;' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog li.dialog-commands input[type="submit"] {' ).
lo_buf->add( '  border-color: #ccc;' ).
lo_buf->add( '  background-color: #ddd;' ).
lo_buf->add( '}' ).
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
lo_buf->add( '.dialog li.dialog-commands a {' ).
lo_buf->add( '  border-color: #ccc;' ).
lo_buf->add( '  background-color: var(--theme-greyscale-dark);' ).
lo_buf->add( '}' ).
lo_buf->add( '.dialog li.dialog-commands input[type="submit"] {' ).
lo_buf->add( '  border-color: #ccc;' ).
lo_buf->add( '  background-color: var(--theme-greyscale-dark);' ).
lo_buf->add( '  color: #fff;' ).
lo_buf->add( '}' ).
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
lo_buf->add( '.icon-circle-solid:before { content: "\f10e"; }' ).
lo_buf->add( '.icon-cloud-commit:before { content: "\f10f"; }' ).
lo_buf->add( '.icon-cloud-solid:before { content: "\f110"; }' ).
lo_buf->add( '.icon-cloud-upload-alt:before { content: "\f111"; }' ).
lo_buf->add( '.icon-code-branch:before { content: "\f112"; }' ).
lo_buf->add( '.icon-code-commit:before { content: "\f113"; }' ).
lo_buf->add( '.icon-code-solid:before { content: "\f114"; }' ).
lo_buf->add( '.icon-cog:before { content: "\f115"; }' ).
lo_buf->add( '.icon-copy-solid:before { content: "\f116"; }' ).
lo_buf->add( '.icon-download-solid:before { content: "\f117"; }' ).
lo_buf->add( '.icon-edit-solid:before { content: "\f118"; }' ).
lo_buf->add( '.icon-exclamation-circle:before { content: "\f119"; }' ).
lo_buf->add( '.icon-exclamation-triangle:before { content: "\f11a"; }' ).
lo_buf->add( '.icon-file-alt:before { content: "\f11b"; }' ).
lo_buf->add( '.icon-file-code:before { content: "\f11c"; }' ).
lo_buf->add( '.icon-file-image:before { content: "\f11d"; }' ).
lo_buf->add( '.icon-file:before { content: "\f11e"; }' ).
lo_buf->add( '.icon-fire-alt:before { content: "\f11f"; }' ).
lo_buf->add( '.icon-flow:before { content: "\f120"; }' ).
lo_buf->add( '.icon-folder:before { content: "\f121"; }' ).
lo_buf->add( '.icon-git-alt:before { content: "\f122"; }' ).
lo_buf->add( '.icon-github:before { content: "\f123"; }' ).
lo_buf->add( '.icon-heart-regular:before { content: "\f124"; }' ).
lo_buf->add( '.icon-info-circle-solid:before { content: "\f125"; }' ).
lo_buf->add( '.icon-language-solid:before { content: "\f126"; }' ).
lo_buf->add( '.icon-lock:before { content: "\f127"; }' ).
lo_buf->add( '.icon-magnifying-glass-solid:before { content: "\f128"; }' ).
lo_buf->add( '.icon-markdown:before { content: "\f129"; }' ).
lo_buf->add( '.icon-paste-solid:before { content: "\f12a"; }' ).
lo_buf->add( '.icon-plug:before { content: "\f12b"; }' ).
lo_buf->add( '.icon-question-circle-solid:before { content: "\f12c"; }' ).
lo_buf->add( '.icon-redo-alt-solid:before { content: "\f12d"; }' ).
lo_buf->add( '.icon-server-solid:before { content: "\f12e"; }' ).
lo_buf->add( '.icon-sliders-h:before { content: "\f12f"; }' ).
lo_buf->add( '.icon-snowflake:before { content: "\f130"; }' ).
lo_buf->add( '.icon-star:before { content: "\f131"; }' ).
lo_buf->add( '.icon-tag-solid:before { content: "\f132"; }' ).
lo_buf->add( '.icon-times-solid:before { content: "\f133"; }' ).
lo_buf->add( '.icon-tools-solid:before { content: "\f134"; }' ).
lo_buf->add( '.icon-truck-solid:before { content: "\f135"; }' ).
lo_buf->add( '.icon-upload-solid:before { content: "\f136"; }' ).
lo_buf->add( '.icon-user-cog-solid:before { content: "\f137"; }' ).
lo_buf->add( '.icon-user-solid:before { content: "\f138"; }' ).
lo_buf->add( '.icon-vial-solid:before { content: "\f139"; }' ).
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

*>>>>>>> ZCL_ABAPGIT_GUI <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui===============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui===============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI implementation.
*"* method's implementations
*include methods.
  METHOD back.

    DATA lv_index TYPE i.
    DATA ls_stack LIKE LINE OF mt_stack.

    " If viewer is showing Internet page, then use browser navigation
    IF mi_html_viewer->get_url( ) CP 'http*'.
      mi_html_viewer->back( ).
      RETURN.
    ENDIF.

    lv_index = lines( mt_stack ).

    IF lv_index = 0.
      rv_exit = abap_true.
      RETURN.
    ENDIF.

    IF iv_graceful = abap_true AND back_graceful( ) = abap_true.
      RETURN.
    ENDIF.

    DO lv_index TIMES.
      READ TABLE mt_stack INDEX lv_index INTO ls_stack.
      ASSERT sy-subrc = 0.

      DELETE mt_stack INDEX lv_index.
      ASSERT sy-subrc = 0.

      lv_index = lv_index - 1.

      IF iv_to_bookmark = abap_false OR ls_stack-bookmark = abap_true.
        EXIT.
      ENDIF.
    ENDDO.

    mi_cur_page = ls_stack-page. " last page always stays
    render( ).

  ENDMETHOD.
  METHOD back_graceful.

    DATA li_handler TYPE REF TO Lif_abapgit_gui_event_handler.
    DATA ls_handled TYPE Lif_abapgit_gui_event_handler=>ty_handling_result.

    " This code can be potentially improved
    " Why send go_back to the topmost handler only ? It makes sense to notify the whole stack
    " But than how to handle re-render ? render if at least one handler asks for it ?
    " Probably that's the way but needs a relevant example. Postponed arch decision.
    READ TABLE mt_event_handlers INTO li_handler INDEX 1.
    IF sy-subrc = 0.
      ls_handled = li_handler->on_event( Lcl_abapgit_gui_event=>new(
        iv_action       = Lif_abapgit_definitions=>c_action-go_back
        ii_gui_services = me ) ).
      IF ls_handled-state = c_event_state-re_render. " soft exit, probably popup
        render( ).
        rv_handled = abap_true.
      ELSEIF ls_handled-state = c_event_state-no_more_act. " soft exit, probably GUI popup
        rv_handled = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD cache_html.

    rv_url = Lif_abapgit_gui_services~cache_asset(
      iv_text    = iv_text
      iv_type    = 'text'
      iv_subtype = 'html' ).

  ENDMETHOD.
  METHOD call_page.

    DATA: ls_stack TYPE ty_page_stack.

    IF iv_replacing = abap_false AND NOT mi_cur_page IS INITIAL.
      ls_stack-page     = mi_cur_page.
      ls_stack-bookmark = iv_with_bookmark.
      APPEND ls_stack TO mt_stack.
    ENDIF.

    mi_cur_page = ii_page.
    render( ).

  ENDMETHOD.
  METHOD constructor.

    IF io_component IS BOUND.
      IF Lcl_abapgit_gui_utils=>is_renderable( io_component ) = abap_true.
        mi_cur_page ?= io_component. " direct page
      ELSE.
        IF Lcl_abapgit_gui_utils=>is_event_handler( io_component ) = abap_false.
          Lcx_abapgit_exception=>raise( 'Component must be renderable or be an event handler' ).
        ENDIF.
        mi_router ?= io_component.
      ENDIF.
    ENDIF.

    CREATE OBJECT mo_html_parts.

    mv_rollback_on_error = iv_rollback_on_error.
    mi_asset_man      = ii_asset_man.
    mi_hotkey_ctl     = ii_hotkey_ctl.
    mi_html_processor = ii_html_processor. " Maybe improve to middlewares stack ??
    startup( ).

  ENDMETHOD.
  METHOD free.

    SET HANDLER on_event FOR mi_html_viewer ACTIVATION space.
    mi_html_viewer->close_document( ).
    mi_html_viewer->free( ).
    FREE mi_html_viewer.

  ENDMETHOD.
  METHOD go_home.

    DATA ls_stack LIKE LINE OF mt_stack.

    IF mi_router IS BOUND.
      CLEAR: mt_stack, mt_event_handlers.
      APPEND mi_router TO mt_event_handlers.

      on_event( action = |{ iv_action }| ).
    ELSE.
      IF lines( mt_stack ) > 0.
        READ TABLE mt_stack INTO ls_stack INDEX 1.
        mi_cur_page = ls_stack-page.
      ENDIF.
      render( ).
    ENDIF.

  ENDMETHOD.
  METHOD handle_action.

    DATA:
      lx_exception TYPE REF TO Lcx_abapgit_exception,
      li_handler   TYPE REF TO Lif_abapgit_gui_event_handler,
      li_event     TYPE REF TO Lif_abapgit_gui_event,
      ls_handled   TYPE Lif_abapgit_gui_event_handler=>ty_handling_result.

    CREATE OBJECT li_event TYPE Lcl_abapgit_gui_event
      EXPORTING
        ii_gui_services = me
        iv_action       = iv_action
        iv_getdata      = iv_getdata
        it_postdata     = it_postdata.

    TRY.
        ls_handled = Lcl_abapgit_exit=>get_instance( )->on_event( li_event ).

        IF ls_handled-state = c_event_state-not_handled.
          LOOP AT mt_event_handlers INTO li_handler.
            ls_handled = li_handler->on_event( li_event ).
            IF ls_handled-state IS NOT INITIAL AND ls_handled-state <> c_event_state-not_handled. " is handled
              EXIT.
            ENDIF.
          ENDLOOP.
        ENDIF.

        IF is_page_modal( mi_cur_page ) = abap_true AND NOT (
          ls_handled-state = c_event_state-re_render OR
          ls_handled-state = c_event_state-go_back OR
          ls_handled-state = c_event_state-no_more_act ).
          " Restrict new page switching from modals
          ls_handled-state = c_event_state-no_more_act.
        ENDIF.

        CASE ls_handled-state.
          WHEN c_event_state-re_render.
            render( ).
          WHEN c_event_state-new_page.
            call_page( ls_handled-page ).
          WHEN c_event_state-new_page_w_bookmark.
            call_page(
              ii_page = ls_handled-page
              iv_with_bookmark = abap_true ).
          WHEN c_event_state-new_page_replacing.
            call_page(
              ii_page = ls_handled-page
              iv_replacing = abap_true ).
          WHEN c_event_state-go_back.
            back( ).
          WHEN c_event_state-go_back_to_bookmark.
            back( iv_to_bookmark = abap_true ).
          WHEN c_event_state-no_more_act.
            " Do nothing, handling completed
          WHEN OTHERS.
            Lcx_abapgit_exception=>raise( |Unknown action: { iv_action }| ).
        ENDCASE.

      CATCH Lcx_abapgit_cancel ##NO_HANDLER.
        " Do nothing = c_event_state-no_more_act
      CATCH Lcx_abapgit_exception INTO lx_exception.
        handle_error( lx_exception ).
    ENDTRY.

  ENDMETHOD.
  METHOD handle_error.

    DATA: li_gui_error_handler TYPE REF TO Lif_abapgit_gui_error_handler,
          lx_exception         TYPE REF TO cx_root.

    IF mv_rollback_on_error = abap_true.
      ROLLBACK WORK.
    ENDIF.

    TRY.
        li_gui_error_handler ?= mi_cur_page.

        IF li_gui_error_handler IS BOUND AND li_gui_error_handler->handle_error( ix_exception ) = abap_true.
          " We rerender the current page to display the error box
          render( ).
        ELSEIF ix_exception->mi_log IS BOUND.
          mi_common_log = ix_exception->mi_log.
          IF mi_common_log->get_log_level( ) >= Lif_abapgit_log=>c_log_level-warning.
            Lcl_abapgit_log_viewer=>show_log( mi_common_log ).
          ENDIF.
        ELSE.
          MESSAGE ix_exception TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

      CATCH Lcx_abapgit_exception cx_sy_move_cast_error INTO lx_exception.
        " In case of fire we just fallback to plain old message
        MESSAGE lx_exception TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.
  METHOD is_page_modal.

    DATA li_modal TYPE REF TO Lif_abapgit_gui_modal.

    TRY.
        IF ii_page IS BOUND.
          li_modal ?= ii_page.
          rv_yes = li_modal->is_modal( ).
        ENDIF.
      CATCH cx_sy_move_cast_error.
    ENDTRY.

  ENDMETHOD.
  METHOD on_event.

    handle_action(
      iv_action   = action
      iv_getdata  = getdata
      it_postdata = postdata ).

  ENDMETHOD.
  METHOD render.

    DATA: lv_url  TYPE string,
          lv_html TYPE string,
          li_html TYPE REF TO Lif_abapgit_html.

    IF mi_cur_page IS NOT BOUND.
      Lcx_abapgit_exception=>raise( 'GUI error: no current page' ).
    ENDIF.

    CLEAR mt_event_handlers.
    mo_html_parts->clear( ).

    IF mi_router IS BOUND AND is_page_modal( mi_cur_page ) = abap_false.
      " No global commands in modals
      APPEND mi_router TO mt_event_handlers.
    ENDIF.

    IF mi_hotkey_ctl IS BOUND.
      mi_hotkey_ctl->reset( ).
    ENDIF.

    li_html = mi_cur_page->render( ).
    lv_html = li_html->render( abap_true ).

    IF mi_html_processor IS BOUND.
      lv_html = mi_html_processor->process(
        iv_html         = lv_html
        ii_gui_services = me ).
    ENDIF.

    lv_url = cache_html( lv_html ).
    mi_html_viewer->show_url( lv_url ).

  ENDMETHOD.
  METHOD set_focus.
    mi_html_viewer->set_focus( ).
  ENDMETHOD.
  METHOD startup.

    DATA: lt_events TYPE cntl_simple_events,
          ls_event  LIKE LINE OF lt_events,
          lt_assets TYPE Lif_abapgit_gui_asset_manager=>ty_web_assets.

    FIELD-SYMBOLS <ls_asset> LIKE LINE OF lt_assets.


    mi_html_viewer = Lcl_abapgit_ui_factory=>get_html_viewer( ).

    IF mi_asset_man IS BOUND.
      lt_assets = mi_asset_man->get_all_assets( ).
      LOOP AT lt_assets ASSIGNING <ls_asset> WHERE is_cacheable = abap_true.
        Lif_abapgit_gui_services~cache_asset(
          iv_xdata   = <ls_asset>-content
          iv_url     = <ls_asset>-url
          iv_type    = <ls_asset>-type
          iv_subtype = <ls_asset>-subtype ).
      ENDLOOP.
    ENDIF.

    ls_event-eventid    = mi_html_viewer->c_id_sapevent.
    ls_event-appl_event = abap_true.
    APPEND ls_event TO lt_events.

    mi_html_viewer->set_registered_events( lt_events ).
    SET HANDLER on_event FOR mi_html_viewer.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_services~cache_asset.

    TYPES ty_hex TYPE x LENGTH 200.
    TYPES ty_char TYPE c LENGTH 200.

    DATA: lt_xdata TYPE STANDARD TABLE OF ty_hex WITH DEFAULT KEY,
          lv_size  TYPE i,
          lt_html  TYPE STANDARD TABLE OF ty_char WITH DEFAULT KEY.

    ASSERT iv_text IS SUPPLIED OR iv_xdata IS SUPPLIED.

    IF iv_text IS SUPPLIED. " String input
      Lcl_abapgit_convert=>string_to_tab(
         EXPORTING
           iv_str  = iv_text
         IMPORTING
           ev_size = lv_size
           et_tab  = lt_html ).

      mi_html_viewer->load_data(
        EXPORTING
          iv_type         = iv_type
          iv_subtype      = iv_subtype
          iv_size         = lv_size
          iv_url          = iv_url
        IMPORTING
          ev_assigned_url = rv_url
        CHANGING
          ct_data_table   = lt_html ).
    ELSE. " Raw input
      Lcl_abapgit_convert=>xstring_to_bintab(
        EXPORTING
          iv_xstr   = iv_xdata
        IMPORTING
          ev_size   = lv_size
          et_bintab = lt_xdata ).

      mi_html_viewer->load_data(
        EXPORTING
          iv_type         = iv_type
          iv_subtype      = iv_subtype
          iv_size         = lv_size
          iv_url          = iv_url
        IMPORTING
          ev_assigned_url = rv_url
        CHANGING
          ct_data_table   = lt_xdata ).
    ENDIF.

    ASSERT sy-subrc = 0. " Image data error

  ENDMETHOD.
  METHOD Lif_abapgit_gui_services~get_current_page_name.

    DATA li_page_hoc TYPE REF TO Lcl_abapgit_gui_page_hoc.

    IF mi_cur_page IS BOUND.
      rv_page_name = cl_abap_classdescr=>describe_by_object_ref( mi_cur_page )->get_relative_name( ).

      " For HOC components return name of child component instead
      IF rv_page_name = 'LCL_ABAPGIT_GUI_PAGE_HOC'.
        li_page_hoc ?= mi_cur_page.
        IF li_page_hoc->get_child( ) IS BOUND.
          rv_page_name = cl_abap_classdescr=>describe_by_object_ref(
                           li_page_hoc->get_child( ) )->get_relative_name( ).
        ENDIF.
      ENDIF.
    ENDIF." ELSE - return is empty => initial page

  ENDMETHOD.
  METHOD Lif_abapgit_gui_services~get_hotkeys_ctl.
    ri_hotkey_ctl = mi_hotkey_ctl.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_services~get_html_parts.
    ro_parts = mo_html_parts.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_services~get_log.

    IF iv_create_new = abap_true OR mi_common_log IS NOT BOUND.
      CREATE OBJECT mi_common_log TYPE Lcl_abapgit_log.
    ENDIF.

    ri_log = mi_common_log.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_services~register_event_handler.
    ASSERT ii_event_handler IS BOUND.
    INSERT ii_event_handler INTO mt_event_handlers INDEX 1.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_services~register_page_asset.

    " Maybe forbid registering cachable existing assets, maybe this is the right place (see also asset_man commments)

    mi_asset_man->register_asset(
      iv_url = iv_url
      iv_type = iv_type
      iv_mime_name = iv_mime_name
      iv_inline = iv_inline
      " This registering will happen after initialization so all cachable already cached
      iv_cachable = abap_false ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI implementation

*>>>>>>> ZCL_ABAPGIT_GUI_ASSET_MANAGER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_asset_manager=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_asset_manager=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_gui_asset_manager=ccau.


class LCL_ABAPGIT_GUI_ASSET_MANAGER implementation.
*"* method's implementations
*include methods.
  METHOD create.
    CREATE OBJECT ri_asset_manager TYPE Lcl_abapgit_gui_asset_manager.
  ENDMETHOD.
  METHOD get_mime_asset.

    DATA: ls_key    TYPE wwwdatatab,
          lv_size_c TYPE wwwparams-value,
          lv_size   TYPE i,
          lt_w3mime TYPE STANDARD TABLE OF w3mime,
          ls_w3mime LIKE LINE OF lt_w3mime.

    ls_key-relid = 'MI'.
    ls_key-objid = iv_mime_name.

    " Get exact file size
    CALL FUNCTION 'WWWPARAMS_READ'
      EXPORTING
        relid            = ls_key-relid
        objid            = ls_key-objid
        name             = 'filesize'
      IMPORTING
        value            = lv_size_c
      EXCEPTIONS
        entry_not_exists = 1.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_size = lv_size_c.

    " Get binary data
    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key               = ls_key
      TABLES
        mime              = lt_w3mime
      EXCEPTIONS
        wrong_object_type = 1
        import_error      = 2.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_w3mime INTO ls_w3mime.
      CONCATENATE rv_xdata ls_w3mime-line INTO rv_xdata IN BYTE MODE.
    ENDLOOP.
    rv_xdata = rv_xdata(lv_size).

  ENDMETHOD.
  METHOD load_asset.

    MOVE-CORRESPONDING is_asset_entry TO rs_asset.
    IF rs_asset-content IS INITIAL AND is_asset_entry-mime_name IS NOT INITIAL.
      " inline content has the priority
      rs_asset-content = get_mime_asset( is_asset_entry-mime_name ).
    ENDIF.
    IF rs_asset-content IS INITIAL.
      Lcx_abapgit_exception=>raise( |failed to load GUI asset: { is_asset_entry-url }| ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_asset_manager~get_all_assets.

    FIELD-SYMBOLS <ls_a> LIKE LINE OF mt_asset_register.

    LOOP AT mt_asset_register ASSIGNING <ls_a>.
      APPEND load_asset( <ls_a> ) TO rt_assets.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_asset_manager~get_asset.

    FIELD-SYMBOLS <ls_a> LIKE LINE OF mt_asset_register.

    READ TABLE mt_asset_register WITH KEY url = iv_url ASSIGNING <ls_a>.
    IF <ls_a> IS NOT ASSIGNED.
      Lcx_abapgit_exception=>raise( |Cannot find GUI asset: { iv_url }| ).
    ENDIF.
    rs_asset = load_asset( <ls_a> ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_asset_manager~get_text_asset.

    DATA ls_asset TYPE Lif_abapgit_gui_asset_manager~ty_web_asset.
    ls_asset = Lif_abapgit_gui_asset_manager~get_asset( iv_url ).

    IF ls_asset-type <> 'text'.
      Lcx_abapgit_exception=>raise( |Not a text asset: { iv_url }| ).
    ENDIF.

    IF iv_assert_subtype IS NOT INITIAL AND ls_asset-subtype <> iv_assert_subtype.
      Lcx_abapgit_exception=>raise( |Wrong subtype ({ iv_assert_subtype }): { iv_url }| ).
    ENDIF.

    rv_asset = Lcl_abapgit_convert=>xstring_to_string_utf8( ls_asset-content ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_asset_manager~register_asset.

    DATA ls_asset LIKE LINE OF mt_asset_register.

    SPLIT iv_type AT '/' INTO ls_asset-type ls_asset-subtype.
    ls_asset-url          = iv_url.
    ls_asset-mime_name    = iv_mime_name.
    ls_asset-is_cacheable = iv_cachable.
    IF iv_base64 IS NOT INITIAL.
      ls_asset-content = Lcl_abapgit_convert=>base64_to_xstring( iv_base64 ).
    ELSEIF iv_inline IS NOT INITIAL.
      ls_asset-content = Lcl_abapgit_convert=>string_to_xstring( iv_inline ).
    ENDIF.

    DELETE mt_asset_register WHERE url = iv_url.
    " TODO: Maybe forbid averwriting cachable assets as they were probably already cached ... agrueable
    APPEND ls_asset TO mt_asset_register.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_ASSET_MANAGER implementation

*>>>>>>> ZCL_ABAPGIT_GUI_CSS_PROCESSOR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_css_processor=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_css_processor=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_gui_css_processor=ccau.






class LCL_ABAPGIT_GUI_CSS_PROCESSOR implementation.
*"* method's implementations
*include methods.
  METHOD add_file.
    APPEND iv_url TO mt_files.
  ENDMETHOD.
  METHOD constructor.
    mi_asset_manager = ii_asset_manager.
  ENDMETHOD.
  METHOD get_css_vars_in_string.
    CONSTANTS: lc_root_pattern     TYPE string VALUE `:root\s*\{([^\}]*)\}`,
               lc_variable_pattern TYPE string VALUE `\-\-([\w\d-]+)\s*:\s*([^\n\r;]*);`.
    DATA: lv_root     TYPE string,
          lo_matcher  TYPE REF TO cl_abap_matcher,
          lo_regex    TYPE REF TO cl_abap_regex,
          ls_variable LIKE LINE OF rt_variables.

    " Only the :root element may define variables for now

    FIND FIRST OCCURRENCE OF REGEX lc_root_pattern IN iv_string SUBMATCHES lv_root.
    IF sy-subrc = 0 AND lv_root IS NOT INITIAL.
      CREATE OBJECT lo_regex
        EXPORTING
          pattern = lc_variable_pattern.
      lo_matcher = lo_regex->create_matcher( text = lv_root ).
      WHILE lo_matcher->find_next( ) = abap_true.
        ls_variable-name = lo_matcher->get_submatch( 1 ).
        ls_variable-value = lo_matcher->get_submatch( 2 ).
        INSERT ls_variable INTO TABLE rt_variables.
        IF sy-subrc <> 0.
          MODIFY TABLE rt_variables FROM ls_variable.
        ENDIF.
      ENDWHILE.
    ENDIF.
  ENDMETHOD.
  METHOD process.
    DATA:
          lt_contents         TYPE STANDARD TABLE OF string,
          lv_content          TYPE string,
          lt_css_variables    TYPE ty_css_vars,
          lt_css_vars_in_file TYPE ty_css_vars.
    FIELD-SYMBOLS: <lv_url>          TYPE string,
                   <ls_css_variable> LIKE LINE OF lt_css_vars_in_file,
                   <lv_content>      LIKE LINE OF lt_contents.

    " 1. Determine all variables and their values. Later definitions overwrite previous ones.
    LOOP AT mt_files ASSIGNING <lv_url>.
      lv_content = mi_asset_manager->get_text_asset(
        iv_url = <lv_url>
        iv_assert_subtype = 'css' ).

      lt_css_vars_in_file = get_css_vars_in_string( lv_content ).

      LOOP AT lt_css_vars_in_file ASSIGNING <ls_css_variable>.
        INSERT <ls_css_variable> INTO TABLE lt_css_variables.
        IF sy-subrc <> 0.
          MODIFY TABLE lt_css_variables FROM <ls_css_variable>.
        ENDIF.
      ENDLOOP.

      APPEND lv_content TO lt_contents.
    ENDLOOP.

    " 2. Replace all variable usages in variables
    LOOP AT lt_css_variables ASSIGNING <ls_css_variable> WHERE value CS 'var(--'.
      resolve_var_recursively( EXPORTING iv_variable_name = <ls_css_variable>-name
                               CHANGING  ct_variables     = lt_css_variables ).
    ENDLOOP.

    " 3. Replace all other variable usages by inlining the values.
    LOOP AT lt_contents ASSIGNING <lv_content>.
      LOOP AT lt_css_variables ASSIGNING <ls_css_variable>.
        REPLACE ALL OCCURRENCES OF |var(--{ <ls_css_variable>-name })|
                IN <lv_content>
                WITH <ls_css_variable>-value.
      ENDLOOP.
    ENDLOOP.

    rv_result = concat_lines_of( table = lt_contents
                                 sep = cl_abap_char_utilities=>newline ).
  ENDMETHOD.
  METHOD resolve_var_recursively.
    CONSTANTS: lc_variable_usage_pattern TYPE string VALUE `var\(\-\-([^\)]*)\)`.
    DATA: lv_variable_name  TYPE string.
    FIELD-SYMBOLS: <ls_variable>       LIKE LINE OF ct_variables,
                   <ls_other_variable> LIKE LINE OF ct_variables.

    READ TABLE ct_variables WITH TABLE KEY name = iv_variable_name ASSIGNING <ls_variable>.
    IF sy-subrc = 0.
      DO.
        FIND FIRST OCCURRENCE OF REGEX lc_variable_usage_pattern
             IN <ls_variable>-value
             SUBMATCHES lv_variable_name.
        IF sy-subrc = 0.
          resolve_var_recursively( EXPORTING iv_variable_name = lv_variable_name
                                   CHANGING  ct_variables     = ct_variables ).
          READ TABLE ct_variables WITH TABLE KEY name = lv_variable_name ASSIGNING <ls_other_variable>.
          REPLACE FIRST OCCURRENCE OF |var(--{ lv_variable_name })|
                  IN <ls_variable>-value
                  WITH <ls_other_variable>-value.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
    ELSE.
      Lcx_abapgit_exception=>raise( |CSS variable { iv_variable_name } not resolveable| ).
    ENDIF.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_CSS_PROCESSOR implementation

*>>>>>>> ZCL_ABAPGIT_GUI_EVENT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_event=========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_event=========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_gui_event=========ccau.



*CLASS zcl_abapgit_gui_event DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLATASBTU.


class LCL_ABAPGIT_GUI_EVENT implementation.
*"* method's implementations
*include methods.
  METHOD class_constructor.

    CONSTANTS lc_nbsp TYPE xstring VALUE 'C2A0'. " &nbsp;

    TRY.
        gv_non_breaking_space = Lcl_abapgit_convert=>xstring_to_string_utf8( lc_nbsp ).
      CATCH Lcx_abapgit_exception.
        ASSERT 0 = 1.
    ENDTRY.

  ENDMETHOD.
  METHOD constructor.

    " Edge Webview control returns upper case action but abapGit requires lower case (#4841)
    Lif_abapgit_gui_event~mi_gui_services = ii_gui_services.
    Lif_abapgit_gui_event~mv_action       = to_lower( iv_action ).
    Lif_abapgit_gui_event~mv_getdata      = iv_getdata.
    Lif_abapgit_gui_event~mt_postdata     = it_postdata.

    IF ii_gui_services IS BOUND.
      Lif_abapgit_gui_event~mv_current_page_name = ii_gui_services->get_current_page_name( ).
    ENDIF.

  ENDMETHOD.
  METHOD fields_to_map.
    FIELD-SYMBOLS <ls_field> LIKE LINE OF it_fields.

    CREATE OBJECT ro_string_map EXPORTING iv_case_insensitive = abap_true.
    LOOP AT it_fields ASSIGNING <ls_field>.
      ro_string_map->set(
        iv_key = <ls_field>-name
        iv_val = <ls_field>-value ).
    ENDLOOP.
  ENDMETHOD.
  METHOD field_keys_to_upper.

    FIELD-SYMBOLS <ls_field> LIKE LINE OF ct_fields.

    LOOP AT ct_fields ASSIGNING <ls_field>.
      <ls_field>-name = to_upper( <ls_field>-name ).
    ENDLOOP.

  ENDMETHOD.
  METHOD new.
    CREATE OBJECT ro_instance
      EXPORTING
        ii_gui_services = ii_gui_services
        iv_action       = iv_action
        iv_getdata      = iv_getdata
        it_postdata     = it_postdata.
  ENDMETHOD.
  METHOD parse_fields.

    DATA:
      lt_substrings TYPE string_table,
      ls_field      LIKE LINE OF rt_fields.

    FIELD-SYMBOLS <lv_substring> LIKE LINE OF lt_substrings.

    SPLIT iv_string AT '&' INTO TABLE lt_substrings.

    LOOP AT lt_substrings ASSIGNING <lv_substring>.

      CLEAR ls_field.
      " On attempt to change unescaping -> run unit tests to check !

      " Unescape name and value separately
      ls_field-name = unescape( substring_before(
        val = <lv_substring>
        sub = '=' ) ).

      ls_field-value = unescape( substring_after(
        val = <lv_substring>
        sub = '=' ) ).

      IF ls_field IS INITIAL. " Not a field with proper structure
        CONTINUE.
      ENDIF.

      APPEND ls_field TO rt_fields.

    ENDLOOP.

    IF iv_upper_cased = abap_true.
      field_keys_to_upper( CHANGING ct_fields = rt_fields ).
    ENDIF.

  ENDMETHOD.
  METHOD parse_fields_upper_case_name.

    rt_fields = parse_fields(
      iv_string      = iv_string
      iv_upper_cased = abap_true ).

  ENDMETHOD.
  METHOD parse_post_form_data.

    DATA lv_serialized_post_data TYPE string.

    lv_serialized_post_data = translate_postdata( it_post_data ).
    IF iv_upper_cased = abap_true.
      rt_fields = parse_fields_upper_case_name( lv_serialized_post_data ).
    ELSE.
      rt_fields = parse_fields( lv_serialized_post_data ).
    ENDIF.

  ENDMETHOD.
  METHOD translate_postdata.

    DATA: lt_post_data       TYPE Lif_abapgit_html_viewer=>ty_post_data,
          ls_last_line       LIKE LINE OF it_postdata,
          lv_last_line_index TYPE i.

    IF it_postdata IS INITIAL.
      RETURN. "Nothing to do
    ENDIF.

    lt_post_data = it_postdata.

    "Save the last line for separate merge, because we don't need its trailing spaces
    WHILE ls_last_line IS INITIAL.
      lv_last_line_index = lines( lt_post_data ).
      READ TABLE lt_post_data INTO ls_last_line INDEX lv_last_line_index.
      DELETE lt_post_data INDEX lv_last_line_index.
    ENDWHILE.

    CONCATENATE LINES OF lt_post_data INTO rv_string
      IN CHARACTER MODE RESPECTING BLANKS.
    CONCATENATE rv_string ls_last_line INTO rv_string
      IN CHARACTER MODE.

  ENDMETHOD.
  METHOD unescape.

* do not use cl_http_utility as it does strange things with the encoding
    rv_string = iv_string.

* todo, more to be added here
    REPLACE ALL OCCURRENCES OF '%3A' IN rv_string WITH ':' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '%3F' IN rv_string WITH '?' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '%3D' IN rv_string WITH '=' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '%2F' IN rv_string WITH '/' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '%25' IN rv_string WITH '%' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '%26' IN rv_string WITH '&' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF gv_non_breaking_space IN rv_string WITH ` `.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event~form_data.

    IF mo_form_data IS NOT BOUND.
      mo_form_data = fields_to_map( parse_post_form_data( Lif_abapgit_gui_event~mt_postdata ) ).
      mo_form_data->freeze( ).
    ENDIF.
    ro_string_map = mo_form_data.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event~query.

    IF mo_query IS NOT BOUND.
      mo_query = fields_to_map( parse_fields( Lif_abapgit_gui_event~mv_getdata ) ).
      mo_query->freeze( ).
    ENDIF.
    ro_string_map = mo_query.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_EVENT implementation

*>>>>>>> ZCL_ABAPGIT_GUI_HTML_PROCESSOR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_html_processorccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_html_processorccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_gui_html_processorccau.



*CLASS SHRIS5ZPAUXVKEPN5HWETLLATAVBTU DEFINITION DEFERRED.
*CLASS zcl_abapgit_gui_html_processor DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLATAVBTU.



class LCL_ABAPGIT_GUI_HTML_PROCESSOR implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    mi_asset_man = ii_asset_man.
  ENDMETHOD.
  METHOD find_head_offset.

    rv_head_end = find( val = iv_html
                        regex = |{ cl_abap_char_utilities=>newline }?\\s*</head>|
                        case = abap_false ).
    IF rv_head_end <= 0.
      rv_head_end = find( val = iv_html
                          regex = |</head>|
                          case = abap_false ).
      IF rv_head_end <= 0.
        Lcx_abapgit_exception=>raise( 'HTML preprocessor: </head> not found' ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD is_preserved.
    READ TABLE mt_preserve_css TRANSPORTING NO FIELDS WITH KEY table_line = iv_css_url.
    rv_yes = boolc( sy-subrc = 0 ).
  ENDMETHOD.
  METHOD patch_html.

    CONSTANTS lc_css_re TYPE string VALUE `<link\s+rel="stylesheet"\s+type="text/css"\s+href="(\S+)">`.

    DATA lv_head_end TYPE i.
    DATA lo_css_re   TYPE REF TO cl_abap_regex.
    DATA lo_matcher  TYPE REF TO cl_abap_matcher.
    DATA lv_css_path TYPE string.
    DATA lv_marker   TYPE string.

    DATA lv_off TYPE i.
    DATA lv_len TYPE i.
    DATA lv_cur TYPE i.

    DATA lv_css_build TYPE string VALUE '<link rel="stylesheet" type="text/css" href="$BUILD_NAME">'.
    REPLACE FIRST OCCURRENCE OF '$BUILD_NAME' IN lv_css_build WITH c_css_build_name. " Mmmm

    CLEAR: ev_html, et_css_urls.

    lv_head_end = find_head_offset( iv_html ).

    CREATE OBJECT lo_css_re
      EXPORTING
        ignore_case = abap_true
        pattern     = lc_css_re.

    lo_matcher = lo_css_re->create_matcher( text = substring( val = iv_html len = lv_head_end ) ).
    WHILE lo_matcher->find_next( ) = abap_true.
      lv_css_path = lo_matcher->get_submatch( 1 ).
      IF abap_false = is_preserved( lv_css_path ).
        lv_off = lo_matcher->get_offset( ).
        lv_len = lo_matcher->get_length( ).
        ev_html = ev_html && substring( val = iv_html
                                        off = lv_cur
                                        len = lv_off - lv_cur ).
        ev_html = ev_html && c_comment_start && substring( val = iv_html
                                                           off = lv_off
                                                           len = lv_len ) && c_comment_end.
        lv_cur  = lv_off + lv_len.
        APPEND lv_css_path TO et_css_urls.
      ENDIF.
    ENDWHILE.

    ev_html = ev_html && substring( val = iv_html
                                    off = lv_cur
                                    len = lv_head_end - lv_cur ).
    IF lines( et_css_urls ) > 0.
      lv_marker = cl_abap_char_utilities=>newline
        && `    ` " Assume 4 space indent, maybe improve and detect ?
        && c_preprocess_marker
        && cl_abap_char_utilities=>newline
        && `    `.
      ev_html = ev_html && lv_marker && lv_css_build.
    ENDIF.
    ev_html = ev_html && substring( val = iv_html
                                    off = lv_head_end ).

  ENDMETHOD.
  METHOD preserve_css.
    APPEND iv_css_url TO mt_preserve_css.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_html_processor~process.

    DATA lo_css_processor TYPE REF TO Lcl_abapgit_gui_css_processor.
    DATA lt_css_urls TYPE string_table.
    DATA lv_css_build TYPE string.

    FIELD-SYMBOLS <lv_url> LIKE LINE OF lt_css_urls.

    patch_html(
      EXPORTING
        iv_html = iv_html
      IMPORTING
        ev_html = rv_html
        et_css_urls = lt_css_urls ).

    IF lines( lt_css_urls ) > 0.
      CREATE OBJECT lo_css_processor
        EXPORTING
          ii_asset_manager = mi_asset_man.

      LOOP AT lt_css_urls ASSIGNING <lv_url>.
        lo_css_processor->add_file( <lv_url> ).
      ENDLOOP.

      lv_css_build = lo_css_processor->process( ).

      ii_gui_services->cache_asset(
        iv_url     = |{ c_css_build_name }|
        iv_type    = 'text'
        iv_subtype = 'css'
        iv_text    = lv_css_build ).
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_HTML_PROCESSOR implementation

*>>>>>>> ZCL_ABAPGIT_GUI_UTILS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_utils=========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_utils=========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_gui_utils=========ccau.
CLASS SHRIS5ZPAUXVKEPN5HWETLLATAXBTU DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_gui_renderable.
ENDCLASS.
CLASS SHRIS5ZPAUXVKEPN5HWETLLATAXBTU IMPLEMENTATION.
  METHOD Lif_abapgit_gui_renderable~render.
  ENDMETHOD.
ENDCLASS.
CLASS SHRIS5ZPAUXVKEPN5HWETLLATAZBTU DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_gui_event_handler.
ENDCLASS.
CLASS SHRIS5ZPAUXVKEPN5HWETLLATAZBTU IMPLEMENTATION.
  METHOD Lif_abapgit_gui_event_handler~on_event.
  ENDMETHOD.
ENDCLASS.




class LCL_ABAPGIT_GUI_UTILS implementation.
*"* method's implementations
*include methods.
  METHOD is_event_handler.
    DATA li_event_handler TYPE REF TO Lif_abapgit_gui_event_handler.
    TRY.
        li_event_handler ?= io_obj.
        rv_yes = abap_true.
      CATCH cx_sy_move_cast_error.
        rv_yes = abap_false.
    ENDTRY.
  ENDMETHOD.
  METHOD is_renderable.
    DATA li_renderable TYPE REF TO Lif_abapgit_gui_renderable.
    TRY.
        li_renderable ?= io_obj.
        rv_yes = abap_true.
      CATCH cx_sy_move_cast_error.
        rv_yes = abap_false.
    ENDTRY.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_UTILS implementation

*>>>>>>> ZCL_ABAPGIT_HTML <<<<<<<*

*"* macro definitions
*include zcl_abapgit_html==============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_html==============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_html==============ccau.
CLASS SHRIS5ZPAUXVKEPN5HWETLLATA5BTU DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_gui_renderable.
ENDCLASS.
CLASS SHRIS5ZPAUXVKEPN5HWETLLATA5BTU IMPLEMENTATION.
  METHOD Lif_abapgit_gui_renderable~render.
    ri_html = Lcl_abapgit_html=>create( 'Hello' ).
  ENDMETHOD.
ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLATA7BTU DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_gui_renderable.
ENDCLASS.
CLASS SHRIS5ZPAUXVKEPN5HWETLLATA7BTU IMPLEMENTATION.
  METHOD Lif_abapgit_gui_renderable~render.
    Lcx_abapgit_exception=>raise( 'Fail!' ).
  ENDMETHOD.
ENDCLASS.




class LCL_ABAPGIT_HTML implementation.
*"* method's implementations
*include methods.
  METHOD checkbox.

    DATA: lv_checked TYPE string.

    IF iv_checked = abap_true.
      lv_checked = |checked|.
    ENDIF.

    rv_html = |<input type="checkbox" { lv_checked } |.
    IF iv_id IS NOT INITIAL.
      rv_html = rv_html && |id="{ iv_id }"|.
    ENDIF.

    rv_html = rv_html && `/>`.

  ENDMETHOD.
  METHOD class_constructor.

    DATA lv_mode TYPE tabname.

    CREATE OBJECT go_single_tags_re
      EXPORTING
        pattern     = '<(AREA|BASE|BR|COL|COMMAND|EMBED|HR|IMG|INPUT|LINK|META|PARAM|SOURCE|!)'
        ignore_case = abap_false.

    gv_spaces = repeat(
      val = ` `
      occ = c_max_indent ).

    GET PARAMETER ID 'DBT' FIELD lv_mode.
    gv_debug_mode = boolc( lv_mode = 'HREF' ).

  ENDMETHOD.
  METHOD create.
    CREATE OBJECT ri_instance TYPE Lcl_abapgit_html.
    IF iv_initial_chunk IS NOT INITIAL.
      ri_instance->add( iv_initial_chunk ).
    ENDIF.
  ENDMETHOD.
  METHOD icon.

    DATA: lv_hint       TYPE string,
          lv_name       TYPE string,
          lv_color      TYPE string,
          lv_class      TYPE string,
          lv_large_icon TYPE string,
          lv_xpixel     TYPE i,
          lv_onclick    TYPE string.

    SPLIT iv_name AT '/' INTO lv_name lv_color.

    IF iv_hint IS NOT INITIAL.
      lv_hint  = | title="{ iv_hint }"|.
    ENDIF.
    IF iv_onclick IS NOT INITIAL.
      lv_onclick = | onclick="{ iv_onclick }"|.
    ENDIF.
    IF iv_class IS NOT INITIAL.
      lv_class = | { iv_class }|.
    ENDIF.
    IF lv_color IS NOT INITIAL.
      lv_color = | { lv_color }|.
    ENDIF.

    " Automatic icon scaling (could be overwritten by personal setting)
    " see zcl_abapgit_gui_page->html_head
    lv_xpixel = cl_gui_cfw=>compute_pixel_from_metric( x_or_y = 'X'
                                                       in = 1 ).
    IF lv_xpixel >= 2.
      lv_large_icon = ' large'.
    ENDIF.

    rv_str = |<i class="icon{ lv_large_icon } icon-{ lv_name }{ lv_color }|.
    rv_str = |{ rv_str }{ lv_class }"{ lv_onclick }{ lv_hint }></i>|.

  ENDMETHOD.
  METHOD indent_line.

    DATA: ls_study  TYPE ty_study_result,
          lv_spaces TYPE i.

    ls_study = study_line(
      is_context = cs_context
      iv_line    = cv_line ).

    " No indent for textarea tags
    IF ls_study-textarea_open = abap_true.
      cs_context-within_textarea = abap_true.
      RETURN.
    ELSEIF ls_study-textarea_close = abap_true.
      cs_context-within_textarea = abap_false.
      RETURN.
    ELSEIF cs_context-within_textarea = abap_true.
      RETURN.
    ENDIF.

    " First closing tag - shift back exceptionally
    IF ( ls_study-script_close = abap_true
        OR ls_study-style_close = abap_true
        OR ls_study-curly_close = abap_true
        OR ls_study-tag_close = abap_true )
        AND cs_context-indent > 0.
      lv_spaces = ( cs_context-indent - 1 ) * c_indent_size.
      IF lv_spaces <= c_max_indent.
        cv_line  = gv_spaces(lv_spaces) && cv_line.
      ELSE.
        cv_line = gv_spaces && cv_line.
      ENDIF.
    ELSE.
      cv_line = cs_context-indent_str && cv_line.
    ENDIF.

    " Context status update
    CASE abap_true.
      WHEN ls_study-script_open.
        cs_context-within_js    = abap_true.
        cs_context-within_style = abap_false.
      WHEN ls_study-style_open.
        cs_context-within_js    = abap_false.
        cs_context-within_style = abap_true.
      WHEN ls_study-script_close OR ls_study-style_close.
        cs_context-within_js    = abap_false.
        cs_context-within_style = abap_false.
        ls_study-closings       = ls_study-closings + 1.
    ENDCASE.

    " More-less logic chosen due to possible double tags in a line '<a><b>'
    IF ls_study-openings <> ls_study-closings.
      IF ls_study-openings > ls_study-closings.
        cs_context-indent = cs_context-indent + 1.
      ELSEIF cs_context-indent > 0. " AND ls_study-openings < ls_study-closings
        cs_context-indent = cs_context-indent - 1.
      ENDIF.
      lv_spaces = cs_context-indent * c_indent_size.
      IF lv_spaces <= c_max_indent.
        cs_context-indent_str = gv_spaces(lv_spaces).
      ELSE.
        cv_line = gv_spaces && cv_line.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD study_line.

    DATA: lv_line TYPE string,
          lv_len  TYPE i.

    lv_line = to_upper( shift_left( val = iv_line
                                    sub = ` ` ) ).
    lv_len  = strlen( lv_line ).

    " Some assumptions for simplification and speed
    " - style & scripts tag should be opened/closed in a separate line
    " - style & scripts opening and closing in one line is possible but only once

    " TODO & Issues
    " - What if the string IS a well formed html already not just single line ?

    IF is_context-within_js = abap_true OR is_context-within_style = abap_true.

      IF is_context-within_js = abap_true AND lv_len >= 8 AND lv_line(8) = '</SCRIPT'.
        rs_result-script_close = abap_true.
      ELSEIF is_context-within_style = abap_true AND lv_len >= 7 AND lv_line(7) = '</STYLE'.
        rs_result-style_close = abap_true.
      ENDIF.

      IF is_context-no_indent_jscss = abap_false.
        IF lv_len >= 1 AND lv_line(1) = '}'.
          rs_result-curly_close = abap_true.
        ENDIF.

        FIND ALL OCCURRENCES OF '{' IN lv_line MATCH COUNT rs_result-openings.
        FIND ALL OCCURRENCES OF '}' IN lv_line MATCH COUNT rs_result-closings.
      ENDIF.

    ELSE.
      IF lv_len >= 7 AND lv_line(7) = '<SCRIPT'.
        FIND FIRST OCCURRENCE OF '</SCRIPT' IN lv_line.
        IF sy-subrc > 0. " Not found
          rs_result-script_open = abap_true.
        ENDIF.
      ENDIF.
      IF lv_len >= 6 AND lv_line(6) = '<STYLE'.
        FIND FIRST OCCURRENCE OF '</STYLE' IN lv_line.
        IF sy-subrc > 0. " Not found
          rs_result-style_open = abap_true.
        ENDIF.
      ENDIF.
      IF lv_len >= 2 AND lv_line(2) = '</'.
        rs_result-tag_close = abap_true.
      ENDIF.

      FIND ALL OCCURRENCES OF '<'  IN lv_line MATCH COUNT rs_result-openings.
      FIND ALL OCCURRENCES OF '</' IN lv_line MATCH COUNT rs_result-closings.
      IF rs_result-closings <> rs_result-openings.
* if everything is closings, there are no single tags
        FIND ALL OCCURRENCES OF REGEX go_single_tags_re IN lv_line MATCH COUNT rs_result-singles.
      ENDIF.
      rs_result-openings = rs_result-openings - rs_result-closings - rs_result-singles.

    ENDIF.

    " Textarea (same assumptions as above)
    IF is_context-within_textarea = abap_true AND lv_len >= 10 AND lv_line(10) = '</TEXTAREA'.
      rs_result-textarea_close = abap_true.
    ELSEIF is_context-within_textarea = abap_false AND lv_len >= 9 AND lv_line(9) = '<TEXTAREA'.
      FIND FIRST OCCURRENCE OF '</TEXTAREA' IN lv_line.
      IF sy-subrc > 0. " Not found
        rs_result-textarea_open = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_html~a.

    DATA: lv_class TYPE string,
          lv_href  TYPE string,
          lv_click TYPE string,
          lv_id    TYPE string,
          lv_act   TYPE string,
          lv_style TYPE string,
          lv_title TYPE string.

    lv_class = iv_class.

    IF iv_opt CA Lif_abapgit_html=>c_html_opt-strong.
      lv_class = lv_class && ' emphasis'.
    ENDIF.
    IF iv_opt CA Lif_abapgit_html=>c_html_opt-cancel.
      lv_class = lv_class && ' attention'.
    ENDIF.
    IF iv_opt CA Lif_abapgit_html=>c_html_opt-crossout.
      lv_class = lv_class && ' crossout grey'.
    ENDIF.
    IF lv_class IS NOT INITIAL.
      SHIFT lv_class LEFT DELETING LEADING space.
      lv_class = | class="{ lv_class }"|.
    ENDIF.

    lv_href = ' href="#"'. " Default, dummy
    lv_act  = iv_act.
    IF ( iv_act IS NOT INITIAL OR iv_typ = Lif_abapgit_html=>c_action_type-dummy )
        AND iv_opt NA Lif_abapgit_html=>c_html_opt-crossout.
      CASE iv_typ.
        WHEN Lif_abapgit_html=>c_action_type-url.
          IF iv_query IS NOT INITIAL.
            lv_act = lv_act && `?` && iv_query.
          ENDIF.
          lv_href  = | href="{ lv_act }"|.
        WHEN Lif_abapgit_html=>c_action_type-sapevent.
          IF iv_query IS NOT INITIAL.
            lv_act = lv_act && `?` && iv_query.
          ENDIF.
          lv_href  = | href="sapevent:{ lv_act }"|.
        WHEN Lif_abapgit_html=>c_action_type-onclick.
          lv_href  = ' href="#"'.
          lv_click = | onclick="{ iv_act }"|.
        WHEN Lif_abapgit_html=>c_action_type-dummy.
          lv_href  = ' href="#"'.
      ENDCASE.
    ENDIF.

    IF iv_id IS NOT INITIAL.
      lv_id = | id="{ iv_id }"|.
    ENDIF.

    IF iv_style IS NOT INITIAL.
      lv_style = | style="{ iv_style }"|.
    ENDIF.

    IF iv_title IS NOT INITIAL.
      lv_title = | title="{ iv_title }"|.
    ENDIF.

    " Debug option to display href-link on hover
    IF gv_debug_mode = abap_true.
      lv_title = | title="{ escape(
        val    = lv_href
        format = cl_abap_format=>e_html_attr ) }"|.
    ENDIF.

    rv_str = |<a{ lv_id }{ lv_class }{ lv_href }{ lv_click }{ lv_style }{ lv_title }>|
          && |{ iv_txt }</a>|.

  ENDMETHOD.
  METHOD Lif_abapgit_html~add.

    DATA: lv_type       TYPE c,
          li_renderable TYPE REF TO Lif_abapgit_gui_renderable,
          lx_error      TYPE REF TO Lcx_abapgit_exception,
          lo_html       TYPE REF TO Lcl_abapgit_html.

    FIELD-SYMBOLS: <lt_tab> TYPE string_table.

    lv_type = cl_abap_typedescr=>describe_by_data( ig_chunk )->type_kind.

    CASE lv_type.
      WHEN 'C' OR 'g'.  " Char or string
        APPEND ig_chunk TO mt_buffer.
      WHEN 'h'.         " Table
        ASSIGN ig_chunk TO <lt_tab>. " Assuming table of strings ! Will dump otherwise
        APPEND LINES OF <lt_tab> TO mt_buffer.
      WHEN 'r'.         " Object ref
        ASSERT ig_chunk IS BOUND. " Dev mistake
        TRY.
            lo_html ?= ig_chunk.
          CATCH cx_sy_move_cast_error.
            TRY.
                li_renderable ?= ig_chunk.
                lo_html ?= li_renderable->render( ).
              CATCH cx_sy_move_cast_error.
                ASSERT 1 = 0. " Dev mistake
              CATCH Lcx_abapgit_exception INTO lx_error.
                lo_html ?= create( |<span class="error">Render error: { lx_error->get_text( ) }</span>| ).
            ENDTRY.
        ENDTRY.
        APPEND LINES OF lo_html->mt_buffer TO mt_buffer.
      WHEN OTHERS.
        ASSERT 1 = 0. " Dev mistake
    ENDCASE.

    ri_self = me.

  ENDMETHOD.
  METHOD Lif_abapgit_html~add_a.

    Lif_abapgit_html~add( Lif_abapgit_html~a(
      iv_txt   = iv_txt
      iv_act   = iv_act
      iv_query = iv_query
      iv_typ   = iv_typ
      iv_opt   = iv_opt
      iv_class = iv_class
      iv_id    = iv_id
      iv_style = iv_style
      iv_title = iv_title ) ).

    ri_self = me.

  ENDMETHOD.
  METHOD Lif_abapgit_html~add_checkbox.

    Lif_abapgit_html~add( checkbox(
      iv_id      = iv_id
      iv_checked = iv_checked ) ).

    ri_self = me.

  ENDMETHOD.
  METHOD Lif_abapgit_html~add_icon.

    Lif_abapgit_html~add( icon(
      iv_name    = iv_name
      iv_class   = iv_class
      iv_hint    = iv_hint
      iv_onclick = iv_onclick ) ).

    ri_self = me.

  ENDMETHOD.
  METHOD Lif_abapgit_html~icon.

    rv_str = icon(
      iv_name    = iv_name
      iv_hint    = iv_hint
      iv_class   = iv_class
      iv_onclick = iv_onclick ).

  ENDMETHOD.
  METHOD Lif_abapgit_html~is_empty.
    rv_yes = boolc( lines( mt_buffer ) = 0 ).
  ENDMETHOD.
  METHOD Lif_abapgit_html~render.

    DATA: ls_context TYPE ty_indent_context,
          lt_temp    TYPE string_table.

    FIELD-SYMBOLS: <lv_line>   LIKE LINE OF lt_temp,
                   <lv_line_c> LIKE LINE OF lt_temp.

    ls_context-no_indent_jscss = iv_no_indent_jscss.

    LOOP AT mt_buffer ASSIGNING <lv_line>.
      APPEND <lv_line> TO lt_temp ASSIGNING <lv_line_c>.
      indent_line( CHANGING cs_context = ls_context cv_line = <lv_line_c> ).
    ENDLOOP.

    CONCATENATE LINES OF lt_temp INTO rv_html SEPARATED BY cl_abap_char_utilities=>newline.

  ENDMETHOD.
  METHOD Lif_abapgit_html~set_title.
    Lif_abapgit_html~mv_chunk_title = iv_title.
    ri_self = me.
  ENDMETHOD.
  METHOD Lif_abapgit_html~td.
    Lif_abapgit_html~wrap(
      iv_format_single_line = iv_format_single_line
      iv_tag   = 'td'
      iv_content = iv_content
      ii_content = ii_content
      iv_id    = iv_id
      iv_class = iv_class
      iv_hint  = iv_hint ).
    ri_self = me.
  ENDMETHOD.
  METHOD Lif_abapgit_html~th.
    Lif_abapgit_html~wrap(
      iv_format_single_line = iv_format_single_line
      iv_tag   = 'th'
      iv_content = iv_content
      ii_content = ii_content
      iv_id    = iv_id
      iv_class = iv_class
      iv_hint  = iv_hint ).
    ri_self = me.
  ENDMETHOD.
  METHOD Lif_abapgit_html~wrap.

    DATA lv_open_tag TYPE string.
    DATA lv_close_tag TYPE string.

    DATA: lv_class TYPE string,
          lv_id    TYPE string,
          lv_title TYPE string.

    IF iv_id IS NOT INITIAL.
      lv_id = | id="{ iv_id }"|.
    ENDIF.

    IF iv_class IS NOT INITIAL.
      lv_class = | class="{ iv_class }"|.
    ENDIF.

    IF iv_hint IS NOT INITIAL.
      lv_title = | title="{ iv_hint }"|.
    ENDIF.

    lv_open_tag = |<{ iv_tag }{ lv_id }{ lv_class }{ lv_title }>|.
    lv_close_tag = |</{ iv_tag }>|.

    IF ii_content IS NOT BOUND AND iv_content IS INITIAL.
      lv_open_tag = lv_open_tag && lv_close_tag.
      CLEAR lv_close_tag.
    ENDIF.

    IF iv_format_single_line = abap_true AND iv_content IS NOT INITIAL.
      Lif_abapgit_html~add( lv_open_tag && iv_content && lv_close_tag ).
    ELSE.
      Lif_abapgit_html~add( lv_open_tag ).
      IF ii_content IS BOUND.
        Lif_abapgit_html~add( ii_content ).
      ELSEIF iv_content IS NOT INITIAL.
        Lif_abapgit_html~add( iv_content ).
      ENDIF.
      IF lv_close_tag IS NOT INITIAL.
        Lif_abapgit_html~add( lv_close_tag ).
      ENDIF.
    ENDIF.

    ri_self = me.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_HTML implementation

*>>>>>>> ZCL_ABAPGIT_HTML_PARTS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_html_parts========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_html_parts========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_html_parts========ccau.


class LCL_ABAPGIT_HTML_PARTS implementation.
*"* method's implementations
*include methods.
  METHOD add_part.

    DATA lr_collection TYPE REF TO ty_named_collection.
    lr_collection = get_collection(
      iv_collection = iv_collection
      iv_create_if_missing = abap_true ).
    APPEND ii_part TO lr_collection->pile.

  ENDMETHOD.
  METHOD clear.
    CLEAR mt_part_collections.
  ENDMETHOD.
  METHOD get_collection.

    READ TABLE mt_part_collections REFERENCE INTO rr_collection WITH KEY name = iv_collection.
    IF sy-subrc <> 0 AND iv_create_if_missing = abap_true.
      APPEND INITIAL LINE TO mt_part_collections REFERENCE INTO rr_collection.
      rr_collection->name = iv_collection.
    ENDIF.

  ENDMETHOD.
  METHOD get_collection_names.

    FIELD-SYMBOLS <ls_coll> LIKE LINE OF mt_part_collections.
    LOOP AT mt_part_collections ASSIGNING <ls_coll>.
      APPEND <ls_coll>-name TO rt_list.
    ENDLOOP.

  ENDMETHOD.
  METHOD get_collection_size.

    DATA lr_collection TYPE REF TO ty_named_collection.
    lr_collection = get_collection( iv_collection ).
    IF lr_collection IS BOUND.
      rv_size = lines( lr_collection->pile ).
    ENDIF.

  ENDMETHOD.
  METHOD get_parts.

    DATA lr_collection TYPE REF TO ty_named_collection.
    lr_collection = get_collection( iv_collection ).
    IF lr_collection IS BOUND.
      rt_parts = lr_collection->pile.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_HTML_PARTS implementation

*>>>>>>> ZCL_ABAPGIT_HTML_VIEWER_GUI <<<<<<<*

*"* macro definitions
*include zcl_abapgit_html_viewer_gui===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_html_viewer_gui===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_HTML_VIEWER_GUI implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    DATA: lt_events TYPE cntl_simple_events,
          ls_event  LIKE LINE OF lt_events.

    CREATE OBJECT mo_html_viewer
      EXPORTING
        query_table_disabled = iv_disable_query_table
        parent               = io_container.

    ls_event-eventid    = Lif_abapgit_html_viewer=>c_id_sapevent.
    ls_event-appl_event = abap_true.
    APPEND ls_event TO lt_events.

    mo_html_viewer->set_registered_events( lt_events ).
    SET HANDLER on_event FOR mo_html_viewer.

  ENDMETHOD.
  METHOD on_event.

    RAISE EVENT Lif_abapgit_html_viewer~sapevent
      EXPORTING
        action      = action
        frame       = frame
        getdata     = getdata
        postdata    = postdata
        query_table = query_table.

  ENDMETHOD.
  METHOD Lif_abapgit_html_viewer~back.

    mo_html_viewer->go_back( ).

  ENDMETHOD.
  METHOD Lif_abapgit_html_viewer~close_document.

    mo_html_viewer->close_document( ).

  ENDMETHOD.
  METHOD Lif_abapgit_html_viewer~free.

    mo_html_viewer->free( ).

  ENDMETHOD.
  METHOD Lif_abapgit_html_viewer~get_url.

    DATA lv_url TYPE c LENGTH 250.
    mo_html_viewer->get_current_url( IMPORTING url = lv_url ).
    cl_gui_cfw=>flush( ).
    rv_url = lv_url.

  ENDMETHOD.
  METHOD Lif_abapgit_html_viewer~load_data.

    DATA lv_url TYPE c LENGTH 250.
    DATA lv_assigned TYPE c LENGTH 250.

    ASSERT strlen( iv_url ) <= 250.
    lv_url = iv_url.
    mo_html_viewer->load_data(
      EXPORTING
        url                    = lv_url
        type                   = iv_type
        subtype                = iv_subtype
        size                   = iv_size
      IMPORTING
        assigned_url           = lv_assigned
      CHANGING
        data_table             = ct_data_table
      EXCEPTIONS
        dp_invalid_parameter   = 1
        dp_error_general       = 2
        cntl_error             = 3
        " html_syntax_notcorrect = 4  " not in lower releases
        OTHERS                 = 5 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Error loading data for HTML viewer' ).
    ENDIF.
    ev_assigned_url = lv_assigned.

  ENDMETHOD.
  METHOD Lif_abapgit_html_viewer~set_registered_events.

    mo_html_viewer->set_registered_events(
      EXPORTING
        events                    = it_events
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3
        OTHERS                    = 4 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Error registering events for HTML viewer' ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_html_viewer~set_visiblity.
    DATA: lv_visible TYPE c LENGTH 1.

    IF iv_visible = abap_true.
      lv_visible = cl_gui_container=>visible_true.
    ELSE.
      lv_visible = cl_gui_container=>visible_false.
    ENDIF.

    mo_html_viewer->set_visible( lv_visible ).
  ENDMETHOD.
  METHOD Lif_abapgit_html_viewer~show_url.

    DATA lv_url TYPE c LENGTH 250.
    lv_url = iv_url.
    mo_html_viewer->show_url(
      EXPORTING
        url                    = lv_url
      EXCEPTIONS
        cntl_error             = 1
        cnht_error_not_allowed = 2
        cnht_error_parameter   = 3
        dp_error_general       = 4
        OTHERS                 = 5 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Error showing URL in HTML viewer' ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_html_viewer~set_focus.
    cl_gui_control=>set_focus(
      EXPORTING
        control           = mo_html_viewer
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error in: cl_gui_control=>set_focus - SUBRC = { sy-subrc }| ).
    ENDIF.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_HTML_VIEWER_GUI implementation

*>>>>>>> ZCX_ABAPGIT_CANCEL <<<<<<<*

*"* macro definitions
*include zcx_abapgit_cancel============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcx_abapgit_cancel============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCX_ABAPGIT_CANCEL implementation.
*"* method's implementations
*include methods.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor(
      previous = previous
      log      = log
      msgv1    = msgv1
      msgv2    = msgv2
      msgv3    = msgv3
      msgv4    = msgv4
      longtext = longtext ).

    CLEAR me->textid.

    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
endclass. "ZCX_ABAPGIT_CANCEL implementation

*>>>>>>> ZCL_ABAPGIT_GUI_HOTKEY_CTL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_hotkey_ctl====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_hotkey_ctl====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_HOTKEY_CTL implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( ).

    ms_user_settings = Lcl_abapgit_persistence_user=>get_instance( )->get_settings( ).

  ENDMETHOD.
  METHOD render_scripts.

    DATA lv_json TYPE string.

    FIELD-SYMBOLS: <ls_hotkey> LIKE LINE OF it_hotkeys.

    lv_json = `{`.

    LOOP AT it_hotkeys ASSIGNING <ls_hotkey>.

      IF sy-tabix > 1.
        lv_json = lv_json && |,|.
      ENDIF.

      lv_json = lv_json && |  "{ <ls_hotkey>-hotkey }" : "{ <ls_hotkey>-action }" |.

    ENDLOOP.

    lv_json = lv_json && `}`.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.
    ri_html->set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).
    ri_html->add( |setKeyBindings({ lv_json });| ).

  ENDMETHOD.
  METHOD should_show_hint.
    IF gv_hint_was_shown = abap_false.
      rv_yes = abap_true.
      gv_hint_was_shown = abap_true.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA ls_hotkey LIKE LINE OF rt_hotkey_actions.

    ls_hotkey-ui_component = 'Hotkeys'.
    ls_hotkey-action       = c_showhotkeys_action.
    ls_hotkey-description  = 'Show Hotkeys Help'.
    ls_hotkey-hotkey       = '?'.
    INSERT ls_hotkey INTO TABLE rt_hotkey_actions.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_hotkey_ctl~get_registered_hotkeys.
    rt_registered_hotkeys = mt_hotkeys.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_hotkey_ctl~register_hotkeys.

    FIELD-SYMBOLS <ls_hotkey> LIKE LINE OF it_hotkeys.

    " Compress duplicates
    LOOP AT it_hotkeys ASSIGNING <ls_hotkey>.
      READ TABLE mt_hotkeys WITH KEY hotkey = <ls_hotkey>-hotkey TRANSPORTING NO FIELDS.
      IF sy-subrc = 0. " If found command with same hotkey
        DELETE mt_hotkeys INDEX sy-tabix. " Later registered commands enjoys the priority
      ENDIF.

      IF ms_user_settings-link_hints_enabled = abap_true AND
         ms_user_settings-link_hint_key      = <ls_hotkey>-hotkey.
        " Link hint activation key is more important
        CONTINUE.
      ENDIF.

      APPEND <ls_hotkey> TO mt_hotkeys.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_hotkey_ctl~reset.
    CLEAR mt_hotkeys.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_hotkey_ctl~set_visible.

    mv_visible = iv_visible.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    DATA:
      lv_hint               TYPE string,
      lt_registered_hotkeys TYPE Lif_abapgit_gui_hotkeys=>ty_hotkeys_with_descr,
      lv_hotkey             TYPE string,
      ls_user_settings      TYPE Lif_abapgit_definitions=>ty_s_user_settings.

    FIELD-SYMBOLS <ls_hotkey> LIKE LINE OF lt_registered_hotkeys.

    register_handlers( ).

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    lt_registered_hotkeys = Lif_abapgit_gui_hotkey_ctl~get_registered_hotkeys( ).
    SORT lt_registered_hotkeys BY ui_component description.

    register_deferred_script( render_scripts( lt_registered_hotkeys ) ).

    " Render hotkeys
    ri_html->add( '<ul class="hotkeys">' ).
    LOOP AT lt_registered_hotkeys ASSIGNING <ls_hotkey>.
      ri_html->add( |<li>|
        && |<span class="key-id">{ <ls_hotkey>-hotkey }</span>|
        && |<span class="key-descr">{ <ls_hotkey>-description }</span>|
        && |</li>| ).
    ENDLOOP.

    " render link hints activation key
    ls_user_settings = Lcl_abapgit_persistence_user=>get_instance( )->get_settings( ).
    IF ls_user_settings-link_hints_enabled = abap_true.
      ri_html->add( |<li>|
         && |<span class="key-id">{ ls_user_settings-link_hint_key }</span>|
         && |<span class="key-descr">Link Hints</span>|
         && |</li>| ).
      ri_html->add( |<li>|
         && |<span class="key-id">y{ ls_user_settings-link_hint_key }</span>|
         && |<span class="key-descr">Copy Link Text</span>|
         && |</li>| ).
    ENDIF.

    ri_html->add( '</ul>' ).

    CLEAR lv_hotkey.

    READ TABLE lt_registered_hotkeys ASSIGNING <ls_hotkey>
      WITH KEY action = c_showhotkeys_action.
    IF sy-subrc = 0.
      lv_hotkey = <ls_hotkey>-hotkey.
    ENDIF.

    lv_hint = |Close window with upper right corner 'X'|.
    IF lv_hotkey IS NOT INITIAL.
      lv_hint = lv_hint && | or press '{ <ls_hotkey>-hotkey }'|.
    ENDIF.

    ri_html = Lcl_abapgit_gui_chunk_lib=>render_infopanel(
      iv_div_id     = 'hotkeys'
      iv_title      = 'Hotkeys'
      iv_hint       = lv_hint
      iv_hide       = boolc( mv_visible = abap_false )
      iv_scrollable = abap_false
      io_content    = ri_html ).

    IF lv_hotkey IS NOT INITIAL AND should_show_hint( ) = abap_true.
      ri_html->add( |<div id="hotkeys-hint" class="corner-hint">|
        && |Press '{ <ls_hotkey>-hotkey }' to get keyboard shortcuts list|
        && |</div>| ).
    ENDIF.

    " Always reset visibility here. Closing of the popup has to be done by the
    " user and is handeled in JS.
    mv_visible = abap_false.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_HOTKEY_CTL implementation

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

*>>>>>>> ZCL_ABAPGIT_GUI_BUTTONS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_buttons=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_buttons=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_BUTTONS implementation.
*"* method's implementations
*include methods.
  METHOD advanced.
    rv_html_string = Lcl_abapgit_html=>icon(
      iv_name = 'tools-solid'
      iv_hint = 'Utilities' ).
  ENDMETHOD.
  METHOD experimental.
    rv_html_string = Lcl_abapgit_html=>icon(
      iv_name = 'vial-solid/red'
      iv_hint = 'Experimental Features are Enabled' ).
  ENDMETHOD.
  METHOD help.
    rv_html_string = Lcl_abapgit_html=>icon(
      iv_name = 'question-circle-solid'
      iv_hint = 'Help' ).
  ENDMETHOD.
  METHOD new_offline.
    rv_html_string = Lcl_abapgit_html=>icon( 'plug' ) && ' New Offline'.
  ENDMETHOD.
  METHOD new_online.
    rv_html_string = Lcl_abapgit_html=>icon( 'cloud-upload-alt' ) && ' New Online'.
  ENDMETHOD.
  METHOD repo_list.
    rv_html_string = Lcl_abapgit_html=>icon( 'bars' ) && ' Repository List'.
  ENDMETHOD.
  METHOD settings.
    rv_html_string = Lcl_abapgit_html=>icon( 'cog' ) && ' Settings'.
  ENDMETHOD.
  METHOD flow.
    rv_html_string = Lcl_abapgit_html=>icon( 'flow' ) && ' Flow'.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_BUTTONS implementation

*>>>>>>> ZCL_ABAPGIT_GUI_CHUNK_LIB <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_chunk_lib=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_chunk_lib=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_gui_chunk_lib=====ccau.
*"* use this source file for your ABAP unit test classes







*CLASS zcl_abapgit_gui_chunk_lib DEFINITION
*  LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLATBFBTU
*                SHRIS5ZPAUXVKEPN5HWETLLATBIBTU.









class LCL_ABAPGIT_GUI_CHUNK_LIB implementation.
*"* method's implementations
*include methods.
  METHOD class_constructor.

    DATA lv_fm TYPE string.
    lv_fm = 'GET_SYSTEM_TIMEZONE'.

    TRY.
        CALL METHOD ('CL_ABAP_TSTMP')=>get_system_timezone
          RECEIVING
            system_timezone = gv_time_zone.
      CATCH cx_sy_dyn_call_illegal_method.
        CALL FUNCTION lv_fm
          IMPORTING
            timezone            = gv_time_zone
          EXCEPTIONS
            customizing_missing = 1
            OTHERS              = 2.
    ENDTRY.

  ENDMETHOD.
  METHOD get_item_icon.

    CASE is_item-obj_type.
      WHEN 'PROG' OR 'CLAS' OR 'FUGR' OR 'INTF' OR 'TYPE'.
        rv_html = Lcl_abapgit_html=>icon( iv_name = 'file-code/darkgrey'
                                          iv_hint = 'Code' ).
      WHEN 'W3MI' OR 'W3HT' OR 'SFPF'.
        rv_html = Lcl_abapgit_html=>icon( iv_name = 'file-image/darkgrey'
                                          iv_hint = 'Binary' ).
      WHEN 'DEVC'.
        rv_html = Lcl_abapgit_html=>icon( iv_name = 'box/darkgrey'
                                          iv_hint = 'Package' ).
      WHEN ''.
        rv_html = space. " no icon
      WHEN OTHERS.
        rv_html = Lcl_abapgit_html=>icon( 'file-alt/darkgrey' ).
    ENDCASE.

    IF is_item-is_dir = abap_true.
      rv_html = Lcl_abapgit_html=>icon( iv_name = 'folder/darkgrey'
                                        iv_hint = 'Folder' ).
    ENDIF.

  ENDMETHOD.
  METHOD get_item_link.

    DATA lv_encode TYPE string.
    DATA li_html TYPE REF TO Lif_abapgit_html.

    CREATE OBJECT li_html TYPE Lcl_abapgit_html.

    lv_encode = Lcl_abapgit_html_action_utils=>jump_encode(
      iv_obj_type = is_item-obj_type
      iv_obj_name = is_item-obj_name ).

    rv_html = li_html->a(
      iv_txt = |{ is_item-obj_name }|
      iv_act = |{ Lif_abapgit_definitions=>c_action-jump }?{ lv_encode }| ).

  ENDMETHOD.
  METHOD get_t100_text.

    MESSAGE ID iv_msgid TYPE 'S' NUMBER iv_msgno WITH '&1' '&2' '&3' '&4' INTO rv_text.

    " Don't return any generic messages like `&1 &2 &3 &4`
    IF rv_text CO ' 0123456789&'.
      CLEAR rv_text.
    ENDIF.

  ENDMETHOD.
  METHOD normalize_program_name.

    rv_normalized_program_name = substring_before(
      val   = iv_program_name
      regex = `(=+CP)?$` ).

  ENDMETHOD.
  METHOD render_branch_name.

    DATA:
      lv_key              TYPE string,
      lv_branch           TYPE string,
      lv_selected_commit  TYPE string,
      lv_commit_short_sha TYPE string,
      lv_text             TYPE string,
      lv_icon             TYPE string,
      lv_hint             TYPE string,
      lv_class            TYPE string.

    IF iv_repo_key IS NOT INITIAL.
      lv_key = iv_repo_key.
    ELSEIF io_repo IS BOUND.
      lv_key = io_repo->get_key( ).
    ELSE.
      Lcx_abapgit_exception=>raise( 'Either iv_repo_key or io_repo must be supplied' ).
    ENDIF.

    IF iv_branch IS NOT INITIAL.
      lv_branch = iv_branch.
      lv_text = Lcl_abapgit_git_branch_list=>get_display_name( lv_branch ).
    ELSEIF io_repo IS BOUND.
      lv_selected_commit = io_repo->get_selected_commit( ).
      IF lv_selected_commit IS NOT INITIAL.
        "Convert to short commit. Example: (ae623b9...)
        lv_commit_short_sha = lv_selected_commit+0(7).
        lv_text = |({ lv_commit_short_sha }...)|.
      ELSE.
        lv_branch = io_repo->get_selected_branch( ).
        lv_text = Lcl_abapgit_git_branch_list=>get_display_name( lv_branch ).
      ENDIF.
    ELSE.
      Lcx_abapgit_exception=>raise( 'Either iv_branch or io_repo must be supplied' ).
    ENDIF.

    CASE Lcl_abapgit_git_branch_list=>get_type( lv_branch ).
      WHEN Lif_abapgit_git_definitions=>c_git_branch_type-branch.
        lv_class = 'branch branch_branch'.
        lv_icon  = 'code-branch/grey70'.
        lv_hint  = 'Current branch'.
      WHEN Lif_abapgit_git_definitions=>c_git_branch_type-annotated_tag
        OR Lif_abapgit_git_definitions=>c_git_branch_type-lightweight_tag.
        lv_class = 'branch'.
        lv_icon  = 'tag-solid/grey70'.
        lv_hint  = 'Current tag'.
      WHEN OTHERS.
        lv_class = 'branch branch_branch'.
        lv_icon  = 'code-branch/grey70'.
        lv_hint  = 'Current commit'.
    ENDCASE.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.
    ri_html->add( |<span class="{ lv_class }">| ).
    ri_html->add_icon( iv_name = lv_icon
                       iv_hint = lv_hint ).
    IF iv_interactive = abap_true.
      ri_html->add_a( iv_act = |{ Lif_abapgit_definitions=>c_action-git_branch_switch }?key={ lv_key }|
                      iv_txt = lv_text ).
    ELSE.
      ri_html->add( lv_text ).
    ENDIF.
    ri_html->add( '</span>' ).

  ENDMETHOD.
  METHOD render_error.

    DATA lv_error TYPE string.
    DATA lv_class TYPE string VALUE 'panel error center'.

    IF iv_extra_style IS NOT INITIAL.
      lv_class = lv_class && ` ` && iv_extra_style.
    ENDIF.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    IF ix_error IS BOUND.
      lv_error = ix_error->get_text( ).
    ELSE.
      lv_error = iv_error.
    ENDIF.

    ri_html->add( |<div class="{ lv_class }">| ).
    ri_html->add( |{ ri_html->icon( 'exclamation-circle/red' ) } { lv_error }| ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
  METHOD render_error_message_box.

    DATA:
      lv_error_text          TYPE string,
      lv_longtext            TYPE string,
      lt_longtext_paragraphs TYPE string_table,
      lv_program_name        TYPE sy-repid,
      lv_title               TYPE string,
      lv_text                TYPE string.
    FIELD-SYMBOLS:
      <lv_longtext_paragraph> TYPE string.


    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    lv_error_text = ix_error->get_text( ).
    lv_longtext = ix_error->if_message~get_longtext( abap_true ).

    IF lv_longtext IS NOT INITIAL.
      lv_error_text = |{ lv_error_text } <span class="emphasis">More...</span>|.

      REPLACE FIRST OCCURRENCE OF REGEX
        |({ Lcx_abapgit_exception=>c_section_text-cause }{ cl_abap_char_utilities=>newline })|
        IN lv_longtext WITH |<h3>$1</h3>|.

      REPLACE FIRST OCCURRENCE OF REGEX
        |({ Lcx_abapgit_exception=>c_section_text-system_response }{ cl_abap_char_utilities=>newline })|
        IN lv_longtext WITH |<h3>$1</h3>|.

      REPLACE FIRST OCCURRENCE OF REGEX
        |({ Lcx_abapgit_exception=>c_section_text-what_to_do }{ cl_abap_char_utilities=>newline })|
        IN lv_longtext WITH |<h3>$1</h3>|.

      REPLACE FIRST OCCURRENCE OF REGEX
        |({ Lcx_abapgit_exception=>c_section_text-sys_admin }{ cl_abap_char_utilities=>newline })|
        IN lv_longtext WITH |<h3>$1</h3>|.

      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
        IN lv_longtext
        WITH cl_abap_char_utilities=>newline.

      SPLIT lv_longtext AT cl_abap_char_utilities=>newline INTO TABLE lt_longtext_paragraphs.
      CLEAR lv_longtext.

      LOOP AT lt_longtext_paragraphs ASSIGNING <lv_longtext_paragraph>.
        CONDENSE <lv_longtext_paragraph>.

        IF <lv_longtext_paragraph> IS INITIAL.
          CONTINUE.
        ENDIF.

        lv_longtext = |{ lv_longtext }<p>{ <lv_longtext_paragraph> }</p>{ cl_abap_char_utilities=>newline }|.
      ENDLOOP.
    ENDIF.

    ri_html->add( |<div id="message" class="message-panel">| ).
    ri_html->add( |{ ri_html->icon( 'exclamation-circle/red' ) } { lv_error_text }| ).
    ri_html->add( |<div class="message-panel-bar">| ).

    ri_html->add_a(
      iv_txt   = `&#x274c;`
      iv_act   = `toggleDisplay('message')`
      iv_class = `close-btn`
      iv_typ   = Lif_abapgit_html=>c_action_type-onclick ).

    ri_html->add( |</div>| ).

    ri_html->add( |<div class="message-panel-bar message-panel-commands">| ).

    IF ix_error->if_t100_message~t100key-msgid IS NOT INITIAL.

      lv_title = get_t100_text(
        iv_msgid = ix_error->if_t100_message~t100key-msgid
        iv_msgno = ix_error->if_t100_message~t100key-msgno ).

      IF lv_title IS NOT INITIAL.
        lv_text = |Message ({ ix_error->if_t100_message~t100key-msgid }/{ ix_error->if_t100_message~t100key-msgno })|.

        ri_html->add_a(
          iv_txt   = lv_text
          iv_typ   = Lif_abapgit_html=>c_action_type-sapevent
          iv_act   = Lif_abapgit_definitions=>c_action-goto_message
          iv_title = lv_title
          iv_id    = `a_goto_message` ).
      ENDIF.
    ENDIF.

    ix_error->get_source_position( IMPORTING program_name = lv_program_name ).

    lv_title = normalize_program_name( lv_program_name ).

    ri_html->add_a(
      iv_txt   = `Goto source`
      iv_act   = Lif_abapgit_definitions=>c_action-goto_source
      iv_typ   = Lif_abapgit_html=>c_action_type-sapevent
      iv_title = lv_title
      iv_id    = `a_goto_source` ).

    ri_html->add_a(
      iv_txt = `Callstack`
      iv_act = Lif_abapgit_definitions=>c_action-show_callstack
      iv_typ = Lif_abapgit_html=>c_action_type-sapevent
      iv_id  = `a_callstack` ).

    ri_html->add( |</div>| ).
    ri_html->add( |<div class="message-panel-commands">| ).
    ri_html->add( |{ lv_longtext }| ).
    ri_html->add( |</div>| ).
    ri_html->add( |</div>| ).

  ENDMETHOD.
  METHOD render_event_as_form.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add(
      |<form id="form_{ is_event-name }" method="{ is_event-method }" action="sapevent:{ is_event-name }"></form>| ).

  ENDMETHOD.
  METHOD render_help_hint.

    " TODO potentially move to or integrate with zcl_abapgit_html_form

    DATA lt_fragments TYPE string_table.
    DATA li_html TYPE REF TO Lif_abapgit_html.
    li_html = Lcl_abapgit_html=>create( ).

    APPEND `<div class="form-field-help-tooltip">` TO lt_fragments.
    APPEND li_html->icon(
      iv_name = 'question-circle-solid'
      iv_class = 'blue' ) TO lt_fragments.
    APPEND `<div class="form-field-help-tooltip-text">` TO lt_fragments.
    APPEND iv_text_to_wrap TO lt_fragments.
    APPEND `</div>` TO lt_fragments.
    APPEND `</div>` TO lt_fragments.

    rv_html = concat_lines_of( lt_fragments ).

  ENDMETHOD.
  METHOD render_infopanel.

    DATA lv_display TYPE string.
    DATA lv_class TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    IF iv_hide = abap_true. " Initially hide
      lv_display = 'display:none'.
    ENDIF.

    lv_class = 'info-panel'.
    IF iv_scrollable = abap_false. " Initially hide
      lv_class = lv_class && ' info-panel-fixed'.
    ENDIF.

    ri_html->add( |<div id="{ iv_div_id }" class="{ lv_class }" style="{ lv_display }">| ).

    ri_html->add( |<div class="info-title">{ iv_title }|
               && '<div class="float-right">'
               && ri_html->a(
                    iv_txt   = '&#x274c;'
                    iv_typ   = Lif_abapgit_html=>c_action_type-onclick
                    iv_act   = |toggleDisplay('{ iv_div_id }')|
                    iv_class = 'close-btn' )
               && '</div></div>' ).

    IF iv_hint IS NOT INITIAL.
      ri_html->add( '<div class="info-hint">'
        && ri_html->icon( iv_name = 'exclamation-triangle'
                          iv_class = 'pad-right' )
        && iv_hint
        && '</div>' ).
    ENDIF.

    ri_html->add( |<div class="info-list">| ).
    ri_html->add( io_content ).
    ri_html->add( '</div>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
  METHOD render_item_state.

    DATA: lv_system TYPE string.

    FIELD-SYMBOLS <lv_state> TYPE char1.


    rv_html = '<span class="state-block">'.

    DO 2 TIMES.
      CASE sy-index.
        WHEN 1.
          ASSIGN iv_lstate TO <lv_state>.
          lv_system = 'Local:'.
        WHEN 2.
          ASSIGN iv_rstate TO <lv_state>.
          lv_system = 'Remote:'.
      ENDCASE.

      CASE <lv_state>.
        WHEN Lif_abapgit_definitions=>c_state-unchanged.  "None or unchanged
          IF iv_lstate = Lif_abapgit_definitions=>c_state-added OR iv_rstate = Lif_abapgit_definitions=>c_state-added.
            rv_html = rv_html && |<span class="none" title="{ lv_system } Not exists">X</span>|.
          ELSE.
            rv_html = rv_html && |<span class="none" title="{ lv_system } No changes">&nbsp;</span>|.
          ENDIF.
        WHEN Lif_abapgit_definitions=>c_state-modified.   "Changed
          rv_html = rv_html && |<span class="changed" title="{ lv_system } Modified">M</span>|.
        WHEN Lif_abapgit_definitions=>c_state-added.      "Added new
          rv_html = rv_html && |<span class="added" title="{ lv_system } Added new">A</span>|.
        WHEN Lif_abapgit_definitions=>c_state-mixed.      "Multiple changes (multifile)
          rv_html = rv_html && |<span class="mixed" title="{ lv_system } Multiple changes">&#x25A0;</span>|.
        WHEN Lif_abapgit_definitions=>c_state-deleted.    "Deleted
          rv_html = rv_html && |<span class="deleted" title="{ lv_system } Deleted">D</span>|.
      ENDCASE.
    ENDDO.

    rv_html = rv_html && '</span>'.

  ENDMETHOD.
  METHOD render_js_error_banner.
    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.
    ri_html->add( '<div id="js-error-banner" class="dummydiv error">' ).
    ri_html->add( |{ ri_html->icon( 'exclamation-triangle/red' ) }| &&
                  ' If this does not disappear soon,' &&
                  ' then there is a JS init error, please log an issue' ).
    ri_html->add( '</div>' ).
  ENDMETHOD.
  METHOD render_label_list.

    DATA lt_fragments TYPE string_table.
    DATA lv_l TYPE string.
    DATA lv_class TYPE string.
    DATA lv_style TYPE string.
    DATA ls_parsed_color TYPE Lcl_abapgit_repo_labels=>ty_color.
    DATA li_html TYPE REF TO Lif_abapgit_html.

    IF it_labels IS INITIAL.
      RETURN.
    ENDIF.

    li_html = Lcl_abapgit_html=>create( ).

    APPEND `<ul class="repo-labels">` TO lt_fragments.

    LOOP AT it_labels INTO lv_l WHERE table_line IS NOT INITIAL.
      CLEAR lv_class.
      CLEAR lv_style.
      ls_parsed_color = Lcl_abapgit_repo_labels=>parse_color( io_label_colors->get( lv_l ) ).
      IF ls_parsed_color-cls IS NOT INITIAL.
        lv_class = | class="rl-{ ls_parsed_color-cls }"|.
      ELSEIF ls_parsed_color-fg IS NOT INITIAL OR ls_parsed_color-bg IS NOT INITIAL.
        lv_style = ` style="`.
        IF ls_parsed_color-fg IS NOT INITIAL.
          lv_style = lv_style && |color:#{ ls_parsed_color-fg };|.
        ENDIF.
        IF ls_parsed_color-bg IS NOT INITIAL.
          lv_style = lv_style && |background-color:#{ ls_parsed_color-bg };|.
          lv_style = lv_style && |border-color:#{ ls_parsed_color-bg };|.
        ENDIF.
        lv_style = lv_style && `"`.
      ENDIF.

      IF iv_clickable_action IS NOT INITIAL.
        lv_l = li_html->a(
          iv_txt = lv_l
          iv_act = |{ iv_clickable_action }|
          iv_class = 'command'
          iv_query = lv_l ).
      ENDIF.
      lv_l = |<li{ lv_class }{ lv_style }>{ lv_l }</li>|.
      APPEND lv_l TO lt_fragments.
    ENDLOOP.

    APPEND `</ul>` TO lt_fragments.

    rv_html = concat_lines_of( lt_fragments ).

  ENDMETHOD.
  METHOD render_news.

    DATA: lv_text TYPE string,
          lv_hint TYPE string,
          lv_ul   TYPE abap_bool,
          lt_log  TYPE Lcl_abapgit_news=>ty_logs.

    FIELD-SYMBOLS: <ls_line> LIKE LINE OF lt_log.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    IF io_news IS NOT BOUND OR io_news->has_news( ) = abap_false.
      RETURN.
    ENDIF.

    lt_log = io_news->get_log( ).

    " Render news
    LOOP AT lt_log ASSIGNING <ls_line>.
      IF <ls_line>-is_header = abap_true.
        IF <ls_line>-pos_to_cur > 0.
          lv_text = <ls_line>-text && '<span class="version-marker update">update</span>'.
        ELSEIF <ls_line>-pos_to_cur = 0.
          lv_text = <ls_line>-text && '<span class="version-marker">current</span>'.
        ELSE. " < 0
          lv_text = <ls_line>-text.
        ENDIF.
        IF lv_ul = abap_true.
          ri_html->add( |</ul>| ).
        ENDIF.
        ri_html->add( |<h1>{ lv_text }</h1>| ).
        ri_html->add( |<ul>| ).
        lv_ul = abap_true.
      ELSE.
        <ls_line>-text = escape( val    = <ls_line>-text
                                 format = cl_abap_format=>e_html_text ).
        ri_html->add( |<li>{ <ls_line>-text }</li>| ).
      ENDIF.
    ENDLOOP.
    IF lv_ul = abap_true.
      ri_html->add( |</ul>| ).
    ENDIF.

    " Wrap
    IF io_news->has_important( ) = abap_true.
      lv_hint = 'Please note changes marked with "!"'.
