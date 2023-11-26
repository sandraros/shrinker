********************************************************************************
*
* LICENSE and NOTICE
*
* See include program ZSHRINKER_ABAPGIT_LICENSE
*
********************************************************************************
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
CLASS SHRITEFUH64VYIPN5I4UHL45BNYR4A DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS get_sap_basis_component
      RETURNING
        VALUE(rs_result) TYPE cvers_sdu
      RAISING
        Lcx_abapgit_exception.

ENDCLASS.


CLASS SHRITEFUH64VYIPN5I4UHL45BNYR4A IMPLEMENTATION.


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



*CLASS SHRITEFUH64VYIPN5I4UHL45BPAR4A DEFINITION DEFERRED.
*CLASS zcl_abapgit_syntax_abap DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BPAR4A.

*----------------------------------------------------------------------*
*       CLASS SHRITEFUH64VYIPN5I4UHL45BPAR4A DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS ltcl_syntax_highlighter IMPLEMENTATION
*----------------------------------------------------------------------*

*CLASS SHRITEFUH64VYIPN5I4UHL45BPCR4A DEFINITION DEFERRED.
*CLASS zcl_abapgit_syntax_abap DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BPCR4A.

*----------------------------------------------------------------------*
*       CLASS SHRITEFUH64VYIPN5I4UHL45BPCR4A definition
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS SHRITEFUH64VYIPN5I4UHL45BPCR4A IMPLEMENTATION
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



*CLASS SHRITEFUH64VYIPN5I4UHL45BPIR4A DEFINITION DEFERRED.
*CLASS zcl_abapgit_syntax_xml DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BPIR4A.

*----------------------------------------------------------------------*
*       CLASS SHRITEFUH64VYIPN5I4UHL45BPIR4A definition
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS SHRITEFUH64VYIPN5I4UHL45BPIR4A IMPLEMENTATION
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

*>>>>>>> ZCL_ABAPGIT_DATA_FACTORY <<<<<<<*

*"* macro definitions
*include zcl_abapgit_data_factory======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_data_factory======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_DATA_FACTORY implementation.
*"* method's implementations
*include methods.
  METHOD get_supporter.

    IF gi_supporter IS INITIAL.
      CREATE OBJECT gi_supporter TYPE Lcl_abapgit_data_supporter.
    ENDIF.

    ri_supporter = gi_supporter.

  ENDMETHOD.
  METHOD get_config.
    CREATE OBJECT ri_config TYPE Lcl_abapgit_data_config.
  ENDMETHOD.
  METHOD get_deserializer.

    IF gi_deserializer IS INITIAL.
      CREATE OBJECT gi_deserializer TYPE Lcl_abapgit_data_deserializer.
    ENDIF.

    ri_deserializer = gi_deserializer.

  ENDMETHOD.
  METHOD get_serializer.

    IF gi_serializer IS INITIAL.
      CREATE OBJECT gi_serializer TYPE Lcl_abapgit_data_serializer.
    ENDIF.

    ri_serializer = gi_serializer.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_DATA_FACTORY implementation

*>>>>>>> ZCL_ABAPGIT_DATA_INJECTOR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_data_injector=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_data_injector=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_DATA_INJECTOR implementation.
*"* method's implementations
*include methods.
  METHOD set_supporter.
    Lcl_abapgit_data_factory=>gi_supporter = ii_supporter.
  ENDMETHOD.
  METHOD set_deserializer.
    Lcl_abapgit_data_factory=>gi_deserializer = ii_deserializer.
  ENDMETHOD.
  METHOD set_serializer.
    Lcl_abapgit_data_factory=>gi_serializer = ii_serializer.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_DATA_INJECTOR implementation

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
endclass. "ZCL_ABAPGIT_FRONTEND_SERVICES implementation

*>>>>>>> ZCL_ABAPGIT_GIT_TAG <<<<<<<*

*"* macro definitions
*include zcl_abapgit_git_tag===========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_git_tag===========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GIT_TAG implementation.
*"* method's implementations
*include methods.
  METHOD add_peel.

    rv_text = iv_text && Lif_abapgit_git_definitions=>c_git_branch-peel.

  ENDMETHOD.
  METHOD add_tag_prefix.

    rv_text = Lif_abapgit_git_definitions=>c_git_branch-tags_prefix && iv_text.

  ENDMETHOD.
  METHOD remove_peel.

    rv_text = iv_text.

    REPLACE Lif_abapgit_git_definitions=>c_git_branch-peel IN rv_text WITH ''.

  ENDMETHOD.
  METHOD remove_tag_prefix.

    rv_text = iv_text.

    REPLACE FIRST OCCURRENCE OF Lif_abapgit_git_definitions=>c_git_branch-tags_prefix
            IN rv_text
            WITH ''.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GIT_TAG implementation

*>>>>>>> ZCL_ABAPGIT_AJSON_UTILITIES <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ajson_utilities===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ajson_utilities===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_ajson_utilities===ccau.
**********************************************************************
* UTIL
**********************************************************************

CLASS SHRITEFUH64VYIPN5I4UHL45BEXR4A DEFINITION FINAL.
  PUBLIC SECTION.

    DATA mt_nodes TYPE Lif_abapgit_ajson_types=>ty_nodes_tt READ-ONLY.

    METHODS add
      IMPORTING
        iv_str TYPE string.
    METHODS sorted
      RETURNING
        VALUE(rt_nodes) TYPE Lif_abapgit_ajson_types=>ty_nodes_ts.

ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BEXR4A IMPLEMENTATION.
  METHOD add.

    FIELD-SYMBOLS <n> LIKE LINE OF mt_nodes.
    DATA lv_children TYPE string.
    DATA lv_index TYPE string.

    APPEND INITIAL LINE TO mt_nodes ASSIGNING <n>.

    SPLIT iv_str AT '|' INTO
      <n>-path
      <n>-name
      <n>-type
      <n>-value
      lv_index
      lv_children.
    CONDENSE <n>-path.
    CONDENSE <n>-name.
    CONDENSE <n>-type.
    CONDENSE <n>-value.
    <n>-index = lv_index.
    <n>-children = lv_children.

  ENDMETHOD.

  METHOD sorted.
    rt_nodes = mt_nodes.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
* PARSER
**********************************************************************



**********************************************************************
* JSON UTILITIES
**********************************************************************



class LCL_ABAPGIT_AJSON_UTILITIES implementation.
*"* method's implementations
*include methods.
  METHOD delete_empty_nodes.

    DATA ls_json_tree LIKE LINE OF io_json->mt_json_tree.
    DATA lv_done TYPE abap_bool.

    DO.
      lv_done = abap_true.

      IF iv_keep_empty_arrays = abap_false.
        LOOP AT io_json->mt_json_tree INTO ls_json_tree
          WHERE type = Lif_abapgit_ajson_types=>node_type-array AND children = 0.

          io_json->delete( ls_json_tree-path && ls_json_tree-name ).

        ENDLOOP.
        IF sy-subrc = 0.
          lv_done = abap_false.
        ENDIF.
      ENDIF.

      LOOP AT io_json->mt_json_tree INTO ls_json_tree
        WHERE type = Lif_abapgit_ajson_types=>node_type-object AND children = 0.

        io_json->delete( ls_json_tree-path && ls_json_tree-name ).

      ENDLOOP.
      IF sy-subrc = 0.
        lv_done = abap_false.
      ENDIF.

      IF lv_done = abap_true.
        EXIT. " nothing else to delete
      ENDIF.
    ENDDO.

  ENDMETHOD.
  METHOD diff.

    mo_json_a = normalize_input(
      iv_json = iv_json_a
      io_json = io_json_a ).

    mo_json_b = normalize_input(
      iv_json = iv_json_b
      io_json = io_json_b ).

    mo_insert = Lcl_abapgit_ajson=>create_empty( ).
    mo_delete = Lcl_abapgit_ajson=>create_empty( ).
    mo_change = Lcl_abapgit_ajson=>create_empty( ).

    diff_a_b( '/' ).
    diff_b_a( '/' ).

    eo_insert ?= mo_insert.
    eo_delete ?= mo_delete.
    eo_change ?= mo_change.

    delete_empty_nodes(
      io_json              = eo_insert
      iv_keep_empty_arrays = iv_keep_empty_arrays ).
    delete_empty_nodes(
      io_json              = eo_delete
      iv_keep_empty_arrays = iv_keep_empty_arrays ).
    delete_empty_nodes(
      io_json              = eo_change
      iv_keep_empty_arrays = iv_keep_empty_arrays ).

  ENDMETHOD.
  METHOD diff_a_b.

    DATA:
      lv_path_a TYPE string,
      lv_path_b TYPE string.

    FIELD-SYMBOLS:
      <node_a> LIKE LINE OF mo_json_a->mt_json_tree,
      <node_b> LIKE LINE OF mo_json_a->mt_json_tree.

    LOOP AT mo_json_a->mt_json_tree ASSIGNING <node_a> WHERE path = iv_path.
      lv_path_a = <node_a>-path && <node_a>-name && '/'.

      READ TABLE mo_json_b->mt_json_tree ASSIGNING <node_b>
        WITH TABLE KEY path = <node_a>-path name = <node_a>-name.
      IF sy-subrc = 0.
        lv_path_b = <node_b>-path && <node_b>-name && '/'.

        IF <node_a>-type = <node_b>-type.
          CASE <node_a>-type.
            WHEN Lif_abapgit_ajson_types=>node_type-array.
              mo_insert->touch_array( lv_path_a ).
              mo_change->touch_array( lv_path_a ).
              mo_delete->touch_array( lv_path_a ).
              diff_a_b( lv_path_a ).
            WHEN Lif_abapgit_ajson_types=>node_type-object.
              diff_a_b( lv_path_a ).
            WHEN OTHERS.
              IF <node_a>-value <> <node_b>-value.
                " save as changed value
                mo_change->set(
                  iv_path      = lv_path_b
                  iv_val       = <node_b>-value
                  iv_node_type = <node_b>-type ).
              ENDIF.
          ENDCASE.
        ELSE.
          " save changed type as delete + insert
          CASE <node_a>-type.
            WHEN Lif_abapgit_ajson_types=>node_type-array.
              mo_delete->touch_array( lv_path_a ).
              diff_a_b( lv_path_a ).
            WHEN Lif_abapgit_ajson_types=>node_type-object.
              diff_a_b( lv_path_a ).
            WHEN OTHERS.
              mo_delete->set(
                iv_path      = lv_path_a
                iv_val       = <node_a>-value
                iv_node_type = <node_a>-type ).
          ENDCASE.
          CASE <node_b>-type.
            WHEN Lif_abapgit_ajson_types=>node_type-array.
              mo_insert->touch_array( lv_path_b ).
              diff_b_a( lv_path_b ).
            WHEN Lif_abapgit_ajson_types=>node_type-object.
              diff_b_a( lv_path_b ).
            WHEN OTHERS.
              mo_insert->set(
                iv_path      = lv_path_b
                iv_val       = <node_b>-value
                iv_node_type = <node_b>-type ).
          ENDCASE.
        ENDIF.
      ELSE.
        " save as delete
        CASE <node_a>-type.
          WHEN Lif_abapgit_ajson_types=>node_type-array.
            mo_delete->touch_array( lv_path_a ).
            diff_a_b( lv_path_a ).
          WHEN Lif_abapgit_ajson_types=>node_type-object.
            diff_a_b( lv_path_a ).
          WHEN OTHERS.
            mo_delete->set(
              iv_path      = lv_path_a
              iv_val       = <node_a>-value
              iv_node_type = <node_a>-type ).
        ENDCASE.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD diff_b_a.

    DATA lv_path TYPE string.

    FIELD-SYMBOLS <node_b> LIKE LINE OF mo_json_b->mt_json_tree.

    LOOP AT mo_json_b->mt_json_tree ASSIGNING <node_b> WHERE path = iv_path.
      lv_path = <node_b>-path && <node_b>-name && '/'.

      CASE <node_b>-type.
        WHEN Lif_abapgit_ajson_types=>node_type-array.
          mo_insert->touch_array( lv_path ).
          diff_b_a(
            iv_path  = lv_path
            iv_array = abap_true ).
        WHEN Lif_abapgit_ajson_types=>node_type-object.
          diff_b_a( lv_path ).
        WHEN OTHERS.
          IF iv_array = abap_false.
            READ TABLE mo_json_a->mt_json_tree TRANSPORTING NO FIELDS
              WITH TABLE KEY path = <node_b>-path name = <node_b>-name.
            IF sy-subrc <> 0.
              " save as insert
              mo_insert->set(
                iv_path      = lv_path
                iv_val       = <node_b>-value
                iv_node_type = <node_b>-type ).
            ENDIF.
          ELSE.
            READ TABLE mo_insert->mt_json_tree TRANSPORTING NO FIELDS
              WITH KEY path = <node_b>-path value = <node_b>-value.
            IF sy-subrc <> 0.
              " save as new array value
              mo_insert->push(
                iv_path = iv_path
                iv_val  = <node_b>-value ).
            ENDIF.
          ENDIF.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.
  METHOD is_equal.

    DATA li_ins TYPE REF TO Lif_abapgit_ajson.
    DATA li_del TYPE REF TO Lif_abapgit_ajson.
    DATA li_mod TYPE REF TO Lif_abapgit_ajson.

    diff(
      EXPORTING
        iv_json_a = iv_json_a
        iv_json_b = iv_json_b
        io_json_a = ii_json_a
        io_json_b = ii_json_b
      IMPORTING
        eo_insert = li_ins
        eo_delete = li_del
        eo_change = li_mod ).

    rv_yes = boolc(
      li_ins->is_empty( ) = abap_true AND
      li_del->is_empty( ) = abap_true AND
      li_mod->is_empty( ) = abap_true ).

  ENDMETHOD.
  METHOD merge.

    mo_json_a = normalize_input(
      iv_json = iv_json_a
      io_json = io_json_a ).

    mo_json_b = normalize_input(
      iv_json = iv_json_b
      io_json = io_json_b ).

    " Start with first JSON...
    mo_insert = mo_json_a.

    " ...and add all nodes from second JSON
    diff_b_a( '/' ).

    ro_json ?= mo_insert.

    delete_empty_nodes(
      io_json              = ro_json
      iv_keep_empty_arrays = iv_keep_empty_arrays ).

  ENDMETHOD.
  METHOD new.
    CREATE OBJECT ro_instance.
  ENDMETHOD.
  METHOD normalize_input.

    IF boolc( iv_json IS INITIAL ) = boolc( io_json IS INITIAL ).
      Lcx_abapgit_ajson_error=>raise( 'Either supply JSON string or instance, but not both' ).
    ENDIF.

    IF iv_json IS NOT INITIAL.
      ro_json = Lcl_abapgit_ajson=>parse( iv_json ).
    ELSEIF io_json IS NOT INITIAL.
      ro_json = io_json.
    ELSE.
      Lcx_abapgit_ajson_error=>raise( 'Supply either JSON string or instance' ).
    ENDIF.

  ENDMETHOD.
  METHOD sort.

    DATA lo_json TYPE REF TO Lif_abapgit_ajson.

    lo_json = normalize_input(
      iv_json = iv_json
      io_json = io_json ).

    " Nodes are parsed into a sorted table, so no explicit sorting required
    rv_sorted = lo_json->stringify( 2 ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_AJSON_UTILITIES implementation

*>>>>>>> ZCL_ABAPGIT_BACKGROUND <<<<<<<*

*"* macro definitions
*include zcl_abapgit_background========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_background========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_BACKGROUND implementation.
*"* method's implementations
*include methods.
  METHOD dequeue.
    CALL FUNCTION 'DEQUEUE_EZABAPGIT'
      EXPORTING
        type = c_enq_type.
  ENDMETHOD.
  METHOD enqueue.
    CALL FUNCTION 'ENQUEUE_EZABAPGIT'
      EXPORTING
        mode_zabapgit  = 'E'
        type           = c_enq_type
        _scope         = '3'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.
  ENDMETHOD.
  METHOD list_methods.

    DATA: ls_method       LIKE LINE OF rt_methods,
          ls_key          TYPE seoclskey,
          lt_implementing TYPE seor_implementing_keys,
          ls_implementing LIKE LINE OF lt_implementing.

    FIELD-SYMBOLS: <ls_method> LIKE LINE OF rt_methods.


* in order to handle local classes in the compiled report
    ls_method-class = 'LCL_ABAPGIT_BACKGROUND_PULL'.
    INSERT ls_method INTO TABLE rt_methods.
    ls_method-class = 'LCL_ABAPGIT_BACKGROUND_PUSH_AU'.
    INSERT ls_method INTO TABLE rt_methods.
    ls_method-class = 'LCL_ABAPGIT_BACKGROUND_PUSH_FI'.
    INSERT ls_method INTO TABLE rt_methods.

    ls_key-clsname = 'LIF_ABAPGIT_BACKGROUND'.

    CALL FUNCTION 'SEO_INTERFACE_IMPLEM_GET_ALL'
      EXPORTING
        intkey       = ls_key
      IMPORTING
        impkeys      = lt_implementing
      EXCEPTIONS
        not_existing = 1
        OTHERS       = 2 ##FM_SUBRC_OK.
    LOOP AT lt_implementing INTO ls_implementing.
      ls_method-class = ls_implementing-clsname.
      INSERT ls_method INTO TABLE rt_methods.
    ENDLOOP.

    LOOP AT rt_methods ASSIGNING <ls_method>.
      CALL METHOD (<ls_method>-class)=>Lif_abapgit_background~get_description
        RECEIVING
          rv_description = <ls_method>-description.
    ENDLOOP.

  ENDMETHOD.
  METHOD run.

    DATA: lo_per        TYPE REF TO Lcl_abapgit_persist_background,
          lo_repo       TYPE REF TO Lcl_abapgit_repo_online,
          lt_list       TYPE Lcl_abapgit_persist_background=>ty_background_keys,
          li_background TYPE REF TO Lif_abapgit_background,
          li_log        TYPE REF TO Lif_abapgit_log,
          lx_error      TYPE REF TO Lcx_abapgit_exception,
          lv_repo_name  TYPE string.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF lt_list.

    TRY.
        enqueue( ).
      CATCH Lcx_abapgit_exception.
        WRITE: / 'Another intance of the program is already running'.
        RETURN.
    ENDTRY.

    CREATE OBJECT lo_per.
    lt_list = lo_per->list( ).

    WRITE: / 'Background mode'.

    LOOP AT lt_list ASSIGNING <ls_list>.
      CREATE OBJECT li_log TYPE Lcl_abapgit_log.

      TRY.
          lo_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->get( <ls_list>-key ).
          lv_repo_name = lo_repo->get_name( ).
          WRITE: / <ls_list>-method, lv_repo_name.

          Lcl_abapgit_login_manager=>set(
            iv_uri      = lo_repo->get_url( )
            iv_username = <ls_list>-username
            iv_password = <ls_list>-password ).

          CREATE OBJECT li_background TYPE (<ls_list>-method).

          li_background->run(
            io_repo     = lo_repo
            ii_log      = li_log
            it_settings = <ls_list>-settings ).

          " Clear auth buffer to allow different user/password per repository in background mode
          Lcl_abapgit_login_manager=>clear( ).

        CATCH Lcx_abapgit_exception INTO lx_error.
          li_log->add_exception( lx_error ).
      ENDTRY.

      Lcl_abapgit_log_viewer=>write_log( li_log ).
    ENDLOOP.

    IF lines( lt_list ) = 0.
      WRITE: / 'Nothing configured'.
    ENDIF.

    dequeue( ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_BACKGROUND implementation

*>>>>>>> ZCL_ABAPGIT_CONVERT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_convert===========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_convert===========ccimp.
CLASS SHRITEFUH64VYIPN5I4UHL45BFDR4A DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS convert
      IMPORTING
        !iv_data         TYPE xsequence
        !iv_length       TYPE i OPTIONAL
      RETURNING
        VALUE(rv_string) TYPE string
      RAISING
        Lcx_abapgit_exception.
  PRIVATE SECTION.
    CLASS-DATA go_conv_new TYPE REF TO object.
    CLASS-DATA go_conv_old TYPE REF TO object.
ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BFDR4A IMPLEMENTATION.
  METHOD convert.

    DATA lv_class TYPE string.
    DATA lx_error TYPE REF TO cx_root.

    IF go_conv_new IS INITIAL AND go_conv_old IS INITIAL.
      TRY.
          CALL METHOD ('CL_ABAP_CONV_CODEPAGE')=>create_in
            RECEIVING
              instance = go_conv_new.
        CATCH cx_sy_dyn_call_illegal_class.
          lv_class = 'CL_ABAP_CONV_IN_CE'.
          CALL METHOD (lv_class)=>create
            EXPORTING
              encoding = 'UTF-8'
            RECEIVING
              conv     = go_conv_old.
      ENDTRY.
    ENDIF.

    TRY.
        IF go_conv_new IS NOT INITIAL.
          CALL METHOD go_conv_new->('IF_ABAP_CONV_IN~CONVERT')
            EXPORTING
              source = iv_data
            RECEIVING
              result = rv_string.
        ELSE.
          CALL METHOD go_conv_old->('CONVERT')
            EXPORTING
              input = iv_data
              n     = iv_length
            IMPORTING
              data  = rv_string.
        ENDIF.
      CATCH cx_parameter_invalid_range
        cx_sy_codepage_converter_init
        cx_sy_conversion_codepage
        cx_parameter_invalid_type INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BFFR4A DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS convert
      IMPORTING
        !iv_string        TYPE string
      RETURNING
        VALUE(rv_xstring) TYPE xstring
      RAISING
        Lcx_abapgit_exception.
  PRIVATE SECTION.
    CLASS-DATA go_conv_new TYPE REF TO object.
    CLASS-DATA go_conv_old TYPE REF TO object.
ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BFFR4A IMPLEMENTATION.
  METHOD convert.
    DATA lx_error TYPE REF TO cx_root.
    DATA lv_class TYPE string.

    IF go_conv_new IS INITIAL AND go_conv_old IS INITIAL.
      TRY.
          CALL METHOD ('CL_ABAP_CONV_CODEPAGE')=>create_out
            RECEIVING
              instance = go_conv_new.
        CATCH cx_sy_dyn_call_illegal_class.
          lv_class = 'CL_ABAP_CONV_OUT_CE'.
          CALL METHOD (lv_class)=>create
            EXPORTING
              encoding = 'UTF-8'
            RECEIVING
              conv     = go_conv_old.
      ENDTRY.
    ENDIF.

    TRY.
        IF go_conv_new IS NOT INITIAL.
          CALL METHOD go_conv_new->('IF_ABAP_CONV_OUT~CONVERT')
            EXPORTING
              source = iv_string
            RECEIVING
              result = rv_xstring.
        ELSE.
          CALL METHOD go_conv_old->('CONVERT')
            EXPORTING
              data   = iv_string
            IMPORTING
              buffer = rv_xstring.
        ENDIF.
      CATCH cx_parameter_invalid_range
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

*"* test class
*include zcl_abapgit_convert===========ccau.
*----------------------------------------------------------------------*
*       CLASS SHRITEFUH64VYIPN5I4UHL45BFHR4A DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS SHRITEFUH64VYIPN5I4UHL45BFHR4A IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*

class LCL_ABAPGIT_CONVERT implementation.
*"* method's implementations
*include methods.
  METHOD base64_to_xstring.

    rv_xstr = cl_http_utility=>decode_x_base64( iv_base64 ).

  ENDMETHOD.
  METHOD bitbyte_to_int.

    DATA: lv_bitbyte TYPE string,
          lv_len     TYPE i,
          lv_offset  TYPE i.

    lv_bitbyte = iv_bits.
    SHIFT lv_bitbyte LEFT DELETING LEADING '0 '.
    lv_len     = strlen( lv_bitbyte ).
    lv_offset  = lv_len - 1.

    rv_int = 0.
    DO lv_len TIMES.

      IF sy-index = 1.
        "Intialize
        IF lv_bitbyte+lv_offset(1) = '1'.
          rv_int = 1.
        ENDIF.
      ELSEIF lv_bitbyte+lv_offset(1) = '1'.
        rv_int = rv_int + ( 2 ** ( sy-index - 1 ) ).
      ENDIF.

      lv_offset = lv_offset - 1. "Move Cursor

    ENDDO.

  ENDMETHOD.
  METHOD conversion_exit_isola_output.

    language_sap1_to_sap2(
      EXPORTING
        im_lang_sap1  = iv_spras
      RECEIVING
        re_lang_sap2  = rv_spras
      EXCEPTIONS
        no_assignment = 1
        OTHERS        = 2 ).                              "#EC CI_SUBRC

    TRANSLATE rv_spras TO UPPER CASE.

  ENDMETHOD.
  METHOD int_to_xstring4.
* returns xstring of length 4 containing the integer value iv_i

    DATA lv_x TYPE x LENGTH 4.

    lv_x = iv_i.
    rv_xstring = lv_x.

  ENDMETHOD.
  METHOD split_string.

    FIND FIRST OCCURRENCE OF cl_abap_char_utilities=>cr_lf IN iv_string.

    " Convert string into table depending on separator type CR_LF vs. LF
    IF sy-subrc = 0.
      SPLIT iv_string AT cl_abap_char_utilities=>cr_lf INTO TABLE rt_lines.
    ELSE.
      SPLIT iv_string AT cl_abap_char_utilities=>newline INTO TABLE rt_lines.
    ENDIF.

  ENDMETHOD.
  METHOD string_to_tab.

    DATA lv_length TYPE i.
    DATA lv_iterations TYPE i.
    DATA lv_offset TYPE i.

    FIELD-SYMBOLS <lg_line> TYPE any.


    CLEAR et_tab.
    ev_size = strlen( iv_str ).

    APPEND INITIAL LINE TO et_tab ASSIGNING <lg_line>.
    <lg_line> = iv_str.
    lv_length = cl_abap_typedescr=>describe_by_data( <lg_line> )->length / cl_abap_char_utilities=>charsize.
    lv_iterations = ev_size DIV lv_length.

    DO lv_iterations TIMES.
      lv_offset = sy-index * lv_length.
      APPEND INITIAL LINE TO et_tab ASSIGNING <lg_line>.
      <lg_line> = iv_str+lv_offset.
    ENDDO.

  ENDMETHOD.
  METHOD string_to_xstring.

    rv_xstr = string_to_xstring_utf8( iv_str ).

  ENDMETHOD.
  METHOD string_to_xstring_utf8.

    rv_xstring = SHRITEFUH64VYIPN5I4UHL45BFFR4A=>convert( iv_string ).

  ENDMETHOD.
  METHOD string_to_xstring_utf8_bom.

    IF iv_string IS INITIAL.
      RETURN.
    ENDIF.

    rv_xstring = string_to_xstring_utf8( iv_string ).

    " Add UTF-8 BOM
    IF xstrlen( rv_xstring ) < 3 OR rv_xstring(3) <> cl_abap_char_utilities=>byte_order_mark_utf8.
      rv_xstring = cl_abap_char_utilities=>byte_order_mark_utf8 && rv_xstring.
    ENDIF.

  ENDMETHOD.
  METHOD xstring_to_bintab.

    DATA lv_length TYPE i.
    DATA lv_iterations TYPE i.
    DATA lv_offset TYPE i.
    DATA lv_struct TYPE abap_bool.

    FIELD-SYMBOLS <lg_line> TYPE any.


    CLEAR et_bintab.
    ev_size = xstrlen( iv_xstr ).

    APPEND INITIAL LINE TO et_bintab ASSIGNING <lg_line>.
    lv_struct = boolc(
      cl_abap_typedescr=>describe_by_data( <lg_line> )->type_kind = cl_abap_typedescr=>typekind_struct1 ).
    IF lv_struct = abap_true.
      ASSIGN COMPONENT 1 OF STRUCTURE <lg_line> TO <lg_line>.
    ENDIF.
    <lg_line> = iv_xstr.

    lv_length = cl_abap_typedescr=>describe_by_data( <lg_line> )->length.
    lv_iterations = ev_size DIV lv_length.

    DO lv_iterations TIMES.
      lv_offset = sy-index * lv_length.
      APPEND INITIAL LINE TO et_bintab ASSIGNING <lg_line>.
      IF lv_struct = abap_true.
        ASSIGN COMPONENT 1 OF STRUCTURE <lg_line> TO <lg_line>.
      ENDIF.
      <lg_line> = iv_xstr+lv_offset.
    ENDDO.

  ENDMETHOD.
  METHOD xstring_to_int.

* use the built-in type conversion
    rv_i = iv_xstring.

  ENDMETHOD.
  METHOD xstring_to_string_utf8.

    DATA lv_data   TYPE xstring.
    DATA lv_length TYPE i.

    " Remove BOM for non-Unicode systems
    lv_data = xstring_remove_bom( iv_data ).

    lv_length = iv_length.
    IF lv_length <= 0.
      lv_length = xstrlen( lv_data ).
    ENDIF.

    rv_string = SHRITEFUH64VYIPN5I4UHL45BFDR4A=>convert(
      iv_data   = lv_data
      iv_length = lv_length ).

  ENDMETHOD.
  METHOD x_to_bitbyte.

    CLEAR rv_bitbyte.

    GET BIT 1 OF iv_x INTO rv_bitbyte+0(1).
    GET BIT 2 OF iv_x INTO rv_bitbyte+1(1).
    GET BIT 3 OF iv_x INTO rv_bitbyte+2(1).
    GET BIT 4 OF iv_x INTO rv_bitbyte+3(1).
    GET BIT 5 OF iv_x INTO rv_bitbyte+4(1).
    GET BIT 6 OF iv_x INTO rv_bitbyte+5(1).
    GET BIT 7 OF iv_x INTO rv_bitbyte+6(1).
    GET BIT 8 OF iv_x INTO rv_bitbyte+7(1).

  ENDMETHOD.
  METHOD xstring_remove_bom.

    rv_xstr = iv_xstr.

    " cl_abap_conv_in_ce does not handle BOM in non-Unicode systems, so we remove it
    IF cl_abap_char_utilities=>charsize = 1 AND xstrlen( rv_xstr ) > 3
        AND rv_xstr(3) = cl_abap_char_utilities=>byte_order_mark_utf8.

      rv_xstr = rv_xstr+3.

    ENDIF.

  ENDMETHOD.
  METHOD xstring_to_string_utf8_bom.

    DATA lv_xstring TYPE xstring.

    IF iv_xstring IS INITIAL.
      RETURN.
    ENDIF.

    lv_xstring = iv_xstring.
    " Add UTF-8 BOM
    IF xstrlen( lv_xstring ) < 3 OR lv_xstring(3) <> cl_abap_char_utilities=>byte_order_mark_utf8.
      lv_xstring = cl_abap_char_utilities=>byte_order_mark_utf8 && lv_xstring.
    ENDIF.

    rv_string = xstring_to_string_utf8( lv_xstring ).

  ENDMETHOD.
  METHOD language_sap1_to_sap2.

    DATA lv_class TYPE string.

    TRY.
        SELECT SINGLE languageisocode FROM ('I_LANGUAGE')
          INTO re_lang_sap2
          WHERE language = im_lang_sap1.
        IF sy-subrc <> 0.
          RAISE no_assignment.
        ENDIF.
      CATCH cx_sy_dynamic_osql_error.
        lv_class = 'CL_I18N_LANGUAGES'.
        CALL METHOD (lv_class)=>sap1_to_sap2
          EXPORTING
            im_lang_sap1  = im_lang_sap1
          RECEIVING
            re_lang_sap2  = re_lang_sap2
          EXCEPTIONS
            no_assignment = 1
            OTHERS        = 2.
        IF sy-subrc = 1.
          RAISE no_assignment.
        ENDIF.
    ENDTRY.
  ENDMETHOD.
  METHOD language_sap2_to_sap1.

    DATA lv_class TYPE string.

    TRY.
        SELECT SINGLE language FROM ('I_LANGUAGE')
          INTO re_lang_sap1
          WHERE languageisocode = im_lang_sap2.
        IF sy-subrc <> 0.
          RAISE no_assignment.
        ENDIF.
      CATCH cx_sy_dynamic_osql_error.
        lv_class = 'CL_I18N_LANGUAGES'.
        CALL METHOD (lv_class)=>sap2_to_sap1
          EXPORTING
            im_lang_sap2  = im_lang_sap2
          RECEIVING
            re_lang_sap1  = re_lang_sap1
          EXCEPTIONS
            no_assignment = 1
            OTHERS        = 2.
        IF sy-subrc = 1.
          RAISE no_assignment.
        ENDIF.
    ENDTRY.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_CONVERT implementation

*>>>>>>> ZCL_ABAPGIT_DATA_UTILS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_data_utils========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_data_utils========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_data_utils========ccau.


class LCL_ABAPGIT_DATA_UTILS implementation.
*"* method's implementations
*include methods.
  METHOD build_table_itab.

    DATA lo_type   TYPE REF TO cl_abap_typedescr.
    DATA lo_data   TYPE REF TO cl_abap_structdescr.
    DATA lo_table  TYPE REF TO cl_abap_tabledescr.
    DATA lt_keys   TYPE abap_table_keydescr_tab.
    DATA lt_names  TYPE ty_names.

    FIELD-SYMBOLS <lv_name>      LIKE LINE OF lt_names.
    FIELD-SYMBOLS <ls_key>       LIKE LINE OF lt_keys.
    FIELD-SYMBOLS <ls_component> LIKE LINE OF <ls_key>-components.

    cl_abap_structdescr=>describe_by_name(
      EXPORTING
        p_name         = iv_name
      RECEIVING
        p_descr_ref    = lo_type
      EXCEPTIONS
        type_not_found = 1 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Table { iv_name } not found for data serialization| ).
    ENDIF.

    TRY.
        lo_data ?= lo_type.

        " Get primary key to ensure unique entries
        IF lo_data->is_ddic_type( ) = abap_true.
          lt_names = list_key_fields( iv_name ).

          APPEND INITIAL LINE TO lt_keys ASSIGNING <ls_key>.
          <ls_key>-access_kind = cl_abap_tabledescr=>tablekind_hashed.
          <ls_key>-key_kind    = cl_abap_tabledescr=>keydefkind_user.
          <ls_key>-is_primary  = abap_true.
          <ls_key>-is_unique   = abap_true.

          LOOP AT lt_names ASSIGNING <lv_name>.
            APPEND INITIAL LINE TO <ls_key>-components ASSIGNING <ls_component>.
            <ls_component>-name = <lv_name>.
          ENDLOOP.
        ENDIF.

        IF lines( lt_names ) = 0.
          lo_table = cl_abap_tabledescr=>get( lo_data ).
        ELSE.
          lo_table = cl_abap_tabledescr=>get_with_keys(
            p_line_type = lo_data
            p_keys      = lt_keys ).
        ENDIF.

        CREATE DATA rr_data TYPE HANDLE lo_table.

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |Error creating internal table for data serialization| ).
    ENDTRY.

  ENDMETHOD.
  METHOD jump.

    " Run SE16 with authorization check
    CALL FUNCTION 'RS_TABLE_LIST_CREATE'
      EXPORTING
        table_name         = is_item-obj_name
      EXCEPTIONS
        table_is_structure = 1
        table_not_exists   = 2
        db_not_exists      = 3
        no_permission      = 4
        no_change_allowed  = 5
        table_is_gtt       = 6
        OTHERS             = 7.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Table { is_item-obj_name } cannot be displayed| ).
    ENDIF.

  ENDMETHOD.
  METHOD list_key_fields.
    DATA lo_obj        TYPE REF TO object.
    DATA lv_tabname    TYPE c LENGTH 16.
    DATA lr_ddfields   TYPE REF TO data.
    DATA lv_workaround TYPE c LENGTH 20.
    DATA lr_struct     TYPE REF TO cl_abap_structdescr.
    FIELD-SYMBOLS <lg_any> TYPE any.
    FIELD-SYMBOLS <lv_field> TYPE simple.
    FIELD-SYMBOLS <lt_ddfields> TYPE ANY TABLE.

* convert to correct type,
    lv_tabname = iv_name.

    TRY.
        CALL METHOD ('XCO_CP_ABAP_DICTIONARY')=>database_table
          EXPORTING
            iv_name           = lv_tabname
          RECEIVING
            ro_database_table = lo_obj.
        ASSIGN lo_obj->('IF_XCO_DATABASE_TABLE~FIELDS->IF_XCO_DBT_FIELDS_FACTORY~KEY') TO <lg_any>.
        IF sy-subrc  <> 0.
* fallback to RTTI, KEY field does not exist in S/4 2020
          RAISE EXCEPTION TYPE cx_sy_dyn_call_illegal_class.
        ENDIF.
        lo_obj = <lg_any>.
        CALL METHOD lo_obj->('IF_XCO_DBT_FIELDS~GET_NAMES')
          RECEIVING
            rt_names = rt_names.
      CATCH cx_sy_dyn_call_illegal_class cx_no_check.
        lv_workaround = 'DDFIELDS'.
        CREATE DATA lr_ddfields TYPE (lv_workaround).
        ASSIGN lr_ddfields->* TO <lt_ddfields>.
        ASSERT sy-subrc = 0.
        lr_struct ?= cl_abap_typedescr=>describe_by_name( lv_tabname ).
        lr_struct->get_ddic_field_list(
          RECEIVING
            p_field_list = <lt_ddfields>
          EXCEPTIONS
            not_found    = 1
            no_ddic_type = 2 ).
        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise( |Table { iv_name } not found| ).
        ENDIF.
        LOOP AT <lt_ddfields> ASSIGNING <lg_any>.
          ASSIGN COMPONENT 'KEYFLAG' OF STRUCTURE <lg_any> TO <lv_field>.
          IF sy-subrc <> 0 OR <lv_field> <> abap_true.
            CONTINUE.
          ENDIF.
          ASSIGN COMPONENT 'FIELDNAME' OF STRUCTURE <lg_any> TO <lv_field>.
          ASSERT sy-subrc = 0.
          APPEND <lv_field> TO rt_names.
        ENDLOOP.
    ENDTRY.

  ENDMETHOD.
  METHOD build_config_filename.

    rv_filename = to_lower( |{ is_config-name }.{ Lif_abapgit_data_config=>c_config }|
      && |.{ Lif_abapgit_data_config=>c_default_format }| ).

    REPLACE ALL OCCURRENCES OF '/' IN rv_filename WITH '#'.

  ENDMETHOD.
  METHOD build_data_filename.

    rv_filename = to_lower( |{ is_config-name }.{ is_config-type }|
      && |.{ Lif_abapgit_data_config=>c_default_format }| ).

    REPLACE ALL OCCURRENCES OF '/' IN rv_filename WITH '#'.

  ENDMETHOD.
  METHOD is_customizing_table.

    DATA lv_contflag       TYPE c LENGTH 1.
    DATA lo_table          TYPE REF TO object.
    DATA lo_content        TYPE REF TO object.
    DATA lo_delivery_class TYPE REF TO object.
    FIELD-SYMBOLS <ls_any> TYPE any.

    TRY.
        CALL METHOD ('XCO_CP_ABAP_DICTIONARY')=>database_table
          EXPORTING
            iv_name           = iv_name(16)
          RECEIVING
            ro_database_table = lo_table.
        CALL METHOD lo_table->('IF_XCO_DATABASE_TABLE~CONTENT')
          RECEIVING
            ro_content = lo_content.
        CALL METHOD lo_content->('IF_XCO_DBT_CONTENT~GET_DELIVERY_CLASS')
          RECEIVING
            ro_delivery_class = lo_delivery_class.
        ASSIGN lo_delivery_class->('VALUE') TO <ls_any>.
        lv_contflag = <ls_any>.
      CATCH cx_sy_dyn_call_illegal_class.
        SELECT SINGLE contflag FROM ('DD02L') INTO lv_contflag WHERE tabname = iv_name.
    ENDTRY.

    IF lv_contflag = 'C'.
      rv_customizing = abap_true.
    ELSEIF lv_contflag IS NOT INITIAL.
      rv_customizing = abap_false.
    ELSE.
      rv_customizing = abap_undefined. " table does not exist
    ENDIF.

  ENDMETHOD.
  METHOD does_table_exist.

    " This is slow but ensures that the table actually exists and is not just buffered by RTTI
    " If we just rely on RTTI, uninstalling and reinstalling a table in the same session will lead to dumps
    TRY.
        build_table_itab( iv_name ).
        rv_exists = abap_true.
      CATCH Lcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_DATA_UTILS implementation

*>>>>>>> ZCL_ABAPGIT_HASH <<<<<<<*

*"* macro definitions
*include zcl_abapgit_hash==============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_hash==============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_hash==============ccau.




class LCL_ABAPGIT_HASH implementation.
*"* method's implementations
*include methods.
  METHOD adler32.

    CONSTANTS: lc_adler TYPE i VALUE 65521,
               lc_max_b TYPE i VALUE 1800000000.

    DATA: lv_index TYPE i,
          lv_a     TYPE i VALUE 1,
          lv_b     TYPE i VALUE 0,
          lv_x     TYPE x LENGTH 2,
          lv_ca    TYPE c LENGTH 4,
          lv_cb    TYPE c LENGTH 4,
          lv_char8 TYPE c LENGTH 8.


    DO xstrlen( iv_xstring ) TIMES.
      lv_index = sy-index - 1.

      lv_a = lv_a + iv_xstring+lv_index(1).
      lv_b = lv_b + lv_a.

* delay the MOD operation until the integer might overflow
* articles describe 5552 additions are allowed, but this assumes unsigned integers
* instead of allowing a fixed number of additions before running MOD, then
* just compare value of lv_b, this is 1 operation less than comparing and adding
      IF lv_b > lc_max_b.
        lv_a = lv_a MOD lc_adler.
        lv_b = lv_b MOD lc_adler.
      ENDIF.
    ENDDO.

    lv_a = lv_a MOD lc_adler.
    lv_b = lv_b MOD lc_adler.

    lv_x = lv_a.
    lv_ca = lv_x.

    lv_x = lv_b.
    lv_cb = lv_x.

    CONCATENATE lv_cb lv_ca INTO lv_char8.

    rv_checksum = lv_char8.

  ENDMETHOD.
  METHOD sha1.

    DATA: lv_len     TYPE i,
          lv_char10  TYPE c LENGTH 10,
          lv_string  TYPE string,
          lv_xstring TYPE xstring.


    lv_len = xstrlen( iv_data ).
    lv_char10 = lv_len.
    CONDENSE lv_char10.
    CONCATENATE iv_type lv_char10 INTO lv_string SEPARATED BY space.
    lv_xstring = Lcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).

    lv_string = lv_xstring.
    CONCATENATE lv_string '00' INTO lv_string.
    lv_xstring = lv_string.

    CONCATENATE lv_xstring iv_data INTO lv_xstring IN BYTE MODE.

    rv_sha1 = sha1_raw( lv_xstring ).

  ENDMETHOD.
  METHOD sha1_blob.
    rv_sha1 = sha1( iv_type = Lif_abapgit_git_definitions=>c_type-blob
                    iv_data = iv_data ).
  ENDMETHOD.
  METHOD sha1_commit.
    rv_sha1 = sha1( iv_type = Lif_abapgit_git_definitions=>c_type-commit
                    iv_data = iv_data ).
  ENDMETHOD.
  METHOD sha1_raw.

    DATA: lv_hash  TYPE string,
          lv_key   TYPE xstring,
          lx_error TYPE REF TO cx_abap_message_digest.
    TRY.
        cl_abap_hmac=>calculate_hmac_for_raw(
      EXPORTING
        if_key        = lv_key
        if_data       = iv_data
      IMPORTING
        ef_hmacstring = lv_hash ).
      CATCH cx_abap_message_digest INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    rv_sha1 = lv_hash.
    TRANSLATE rv_sha1 TO LOWER CASE.

  ENDMETHOD.
  METHOD sha1_string.

    DATA: lv_hash  TYPE string,
          lv_key   TYPE xstring,
          lx_error TYPE REF TO cx_abap_message_digest.
    TRY.
        cl_abap_hmac=>calculate_hmac_for_char(
      EXPORTING
        if_key        = lv_key
        if_data       = iv_data
      IMPORTING
        ef_hmacstring = lv_hash ).
      CATCH cx_abap_message_digest INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    rv_sha1 = lv_hash.
    TRANSLATE rv_sha1 TO LOWER CASE.

  ENDMETHOD.
  METHOD sha1_tag.
    rv_sha1 = sha1( iv_type = Lif_abapgit_git_definitions=>c_type-tag
                    iv_data = iv_data ).
  ENDMETHOD.
  METHOD sha1_tree.
    rv_sha1 = sha1( iv_type = Lif_abapgit_git_definitions=>c_type-tree
                    iv_data = iv_data ).
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_HASH implementation

*>>>>>>> ZCL_ABAPGIT_DIFF <<<<<<<*

*"* macro definitions
*include zcl_abapgit_diff==============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_diff==============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_diff==============ccau.


class LCL_ABAPGIT_DIFF implementation.
*"* method's implementations
*include methods.
  METHOD adjust_diff.

    " ABAP kernel diff traverses files from bottom up which leads to odd display of diffs
    " SAP won't adjust this kernel service so we will do it here
    " https://github.com/abapGit/abapGit/issues/4395

    TYPES:
      BEGIN OF ty_diff_block,
        start TYPE i,
        len   TYPE i,
      END OF ty_diff_block.

    DATA:
      lv_block_begin TYPE i,
      lv_block_end   TYPE i,
      ls_diff_block  TYPE ty_diff_block,
      lt_diff_block  TYPE STANDARD TABLE OF ty_diff_block WITH DEFAULT KEY.

    FIELD-SYMBOLS:
      <ls_diff>       LIKE LINE OF mt_diff,
      <ls_diff_begin> LIKE LINE OF mt_diff,
      <ls_diff_end>   LIKE LINE OF mt_diff.

    " Determine start and length of diff blocks
    LOOP AT mt_diff ASSIGNING <ls_diff>.
      IF <ls_diff>-result = Lif_abapgit_definitions=>c_diff-insert OR
         <ls_diff>-result = Lif_abapgit_definitions=>c_diff-delete.
        IF ls_diff_block IS INITIAL.
          ls_diff_block-start = sy-tabix.
        ENDIF.
        ls_diff_block-len = ls_diff_block-len + 1.
      ELSEIF ls_diff_block-start IS NOT INITIAL.
        APPEND ls_diff_block TO lt_diff_block.
        CLEAR ls_diff_block.
      ENDIF.
    ENDLOOP.

    " For each diff block, check if beginning is same as end of block
    " If yes, move diff block down
    LOOP AT lt_diff_block INTO ls_diff_block.
      DO ls_diff_block-len TIMES.
        lv_block_begin = ls_diff_block-start + sy-index - 1.
        READ TABLE mt_diff ASSIGNING <ls_diff_begin> INDEX lv_block_begin.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        lv_block_end = ls_diff_block-start + ls_diff_block-len + sy-index - 1.
        READ TABLE mt_diff ASSIGNING <ls_diff_end> INDEX lv_block_end.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        CASE <ls_diff_begin>-result.
          WHEN Lif_abapgit_definitions=>c_diff-insert.
            IF <ls_diff_begin>-new = <ls_diff_end>-new.
              <ls_diff_begin>-old_num = <ls_diff_end>-old_num.
              <ls_diff_begin>-old     = <ls_diff_end>-old.
              <ls_diff_end>-result    = <ls_diff_begin>-result.
              CLEAR: <ls_diff_begin>-result, <ls_diff_end>-old_num, <ls_diff_end>-old.
            ELSE.
              EXIT.
            ENDIF.
          WHEN Lif_abapgit_definitions=>c_diff-delete.
            IF <ls_diff_begin>-old = <ls_diff_end>-old.
              <ls_diff_begin>-new_num = <ls_diff_end>-new_num.
              <ls_diff_begin>-new     = <ls_diff_end>-new.
              <ls_diff_end>-result    = <ls_diff_begin>-result.
              CLEAR: <ls_diff_begin>-result, <ls_diff_end>-new_num, <ls_diff_end>-new.
            ELSE.
              EXIT.
            ENDIF.
          WHEN OTHERS.
            EXIT.
        ENDCASE.
      ENDDO.
    ENDLOOP.

  ENDMETHOD.
  METHOD calculate_stats.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF mt_diff.

    LOOP AT mt_diff ASSIGNING <ls_diff>.
      CASE <ls_diff>-result.
        WHEN Lif_abapgit_definitions=>c_diff-insert.
          ms_stats-insert = ms_stats-insert + 1.
        WHEN Lif_abapgit_definitions=>c_diff-delete.
          ms_stats-delete = ms_stats-delete + 1.
        WHEN Lif_abapgit_definitions=>c_diff-update.
          ms_stats-update = ms_stats-update + 1.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.
  METHOD compute_and_render.

    DATA:
      lv_i     TYPE i,
      ls_diff  LIKE LINE OF rt_diff,
      lt_delta TYPE STANDARD TABLE OF rsedcresul WITH DEFAULT KEY.

    FIELD-SYMBOLS:
      <ls_old>   LIKE LINE OF it_old,
      <ls_new>   LIKE LINE OF it_new,
      <ls_delta> LIKE LINE OF lt_delta.

    " Note: Ignore case is for keywords, variables, types etc, but not for literals
    CALL FUNCTION 'RS_CMP_COMPUTE_DELTA'
      EXPORTING
        compare_mode            = mv_compare_mode
        ignore_case_differences = mv_ignore_case
      TABLES
        text_tab1               = it_new
        text_tab2               = it_old
        text_tab_res            = lt_delta
      EXCEPTIONS
        parameter_invalid       = 1
        difference_not_found    = 2
        OTHERS                  = 3.

    IF sy-subrc = 0.
      " Process delta
      LOOP AT lt_delta ASSIGNING <ls_delta>.
        CLEAR ls_diff.
        IF <ls_delta>-line1 > 0.
          lv_i = <ls_delta>-line1.
          ls_diff-old_num = lv_i.
          ls_diff-old     = <ls_delta>-text1.
        ENDIF.
        IF <ls_delta>-line2 > 0.
          lv_i = <ls_delta>-line2.
          ls_diff-new_num = lv_i.
          ls_diff-new     = <ls_delta>-text2.
        ENDIF.
        IF <ls_delta>-flag1 = 'D'.
          ls_diff-result = Lif_abapgit_definitions=>c_diff-delete.
        ELSEIF <ls_delta>-flag2 = 'I'.
          ls_diff-result = Lif_abapgit_definitions=>c_diff-insert.
        ELSEIF <ls_delta>-flag1 = 'M' AND <ls_delta>-flag2 = 'M'.
          ls_diff-result = Lif_abapgit_definitions=>c_diff-update.
        ELSEIF <ls_delta>-flag1 = '' AND <ls_delta>-flag2 = ''.
          ls_diff-result = Lif_abapgit_definitions=>c_diff-unchanged.
        ELSEIF <ls_delta>-flag1 = '' AND <ls_delta>-flag2 = 'E'. " ignore comment
          ls_diff-result = Lif_abapgit_definitions=>c_diff-unchanged.
        ELSEIF <ls_delta>-flag1 = 'E' AND <ls_delta>-flag2 = ''. " ignore comment
          ls_diff-result = Lif_abapgit_definitions=>c_diff-unchanged.
        ELSE.
          ASSERT 0 = 1. " unknown comparison result
        ENDIF.
        APPEND ls_diff TO rt_diff.
      ENDLOOP.
    ELSEIF sy-subrc = 2.
      " Copy input... but it might not be identical
      LOOP AT it_old ASSIGNING <ls_old>.
        CLEAR ls_diff.
        ls_diff-old_num = sy-tabix.
        ls_diff-old     = <ls_old>.
        READ TABLE it_new ASSIGNING <ls_new> INDEX sy-tabix.
        ASSERT sy-subrc = 0.
        ls_diff-new_num = sy-tabix.
        ls_diff-new     = <ls_new>.
        " SAP function ignores lines that contain only whitespace so we compare directly
        IF ( mv_compare_mode = 1 OR mv_compare_mode = 3 ) AND <ls_old> <> <ls_new> AND
           ( strlen( condense( <ls_old> ) ) = 0 OR strlen( condense( <ls_new> ) ) = 0 ).
          ls_diff-result = Lif_abapgit_definitions=>c_diff-update.
        ENDIF.
        APPEND ls_diff TO rt_diff.
      ENDLOOP.
    ELSE.
      ASSERT 0 = 1. " incorrect function call
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    DATA: lt_new TYPE rswsourcet,
          lt_old TYPE rswsourcet.

    mv_compare_mode = 1.
    IF iv_ignore_indentation = abap_true.
      mv_compare_mode = mv_compare_mode + 1.
    ENDIF.
    IF iv_ignore_comments = abap_true.
      mv_compare_mode = mv_compare_mode + 2.
    ENDIF.
    mv_ignore_case = iv_ignore_case.

    unpack( EXPORTING iv_new = iv_new
                      iv_old = iv_old
            IMPORTING et_new = lt_new
                      et_old = lt_old ).

    mt_diff = compute_and_render( it_new = lt_new
                                  it_old = lt_old ).

    adjust_diff( ).

    calculate_stats( ).
    map_beacons( ).
    shortlist( ).

  ENDMETHOD.
  METHOD create_regex_set.

    DATA: lo_regex TYPE REF TO cl_abap_regex,
          lt_regex TYPE Lif_abapgit_definitions=>ty_string_tt,
          lv_regex LIKE LINE OF lt_regex.

    APPEND '^\s*(CLASS|FORM|MODULE|REPORT|METHOD|INTERFACE|FUNCTION)\s[^=]' TO lt_regex.
    APPEND '^\s*(PUBLIC|PROTECTED|PRIVATE)\sSECTION(\s|\.)' TO lt_regex.
    APPEND '^\s*(CLASS|INTERFACE|FUNCTION|TYPE)-POOL\s' TO lt_regex.
    APPEND '^\s*(START|END)-OF-SELECTION(\s|\.)' TO lt_regex.
    APPEND '^\s*INITIALIZATION(\s|\.)' TO lt_regex.
    APPEND '^\s*(TOP-OF-PAGE|END-OF-PAGE)(\s|\.)' TO lt_regex.
    APPEND '^\s*AT\s*(SELECTION-SCREEN|LINE-SELECTION|USER-COMMAND|PF\d+)(\s|\.)' TO lt_regex.
    APPEND '^\s*(DEFINE|ENHANCEMENT)\s' TO lt_regex.

    LOOP AT lt_regex INTO lv_regex.
      CREATE OBJECT lo_regex
        EXPORTING
          pattern     = lv_regex
          ignore_case = abap_true.
      APPEND lo_regex TO rt_regex_set.
    ENDLOOP.

  ENDMETHOD.
  METHOD get.
    rt_diff = mt_diff.
  ENDMETHOD.
  METHOD get_beacons.
    rt_beacons = mt_beacons.
  ENDMETHOD.
  METHOD is_line_patched.

    FIELD-SYMBOLS: <ls_diff> TYPE Lif_abapgit_definitions=>ty_diff.

    READ TABLE mt_diff INDEX iv_index
                       ASSIGNING <ls_diff>.
    IF sy-subrc = 0.
      rv_patched = <ls_diff>-patch_flag.
    ELSE.
      Lcx_abapgit_exception=>raise( |Diff line not found { iv_index }| ).
    ENDIF.

  ENDMETHOD.
  METHOD map_beacons.

    DATA: lv_beacon_idx  TYPE i VALUE c_starting_beacon,
          lv_offs        TYPE i,
          lv_beacon_str  TYPE string,
          lv_beacon_2lev TYPE string,
          lv_submatch    TYPE string,
          lo_regex       TYPE REF TO cl_abap_regex,
          lt_regex       TYPE ty_regexset_tt.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF mt_diff.

    lt_regex = create_regex_set( ).
    LOOP AT mt_diff ASSIGNING <ls_diff>.

      CLEAR lv_offs.
      <ls_diff>-beacon = lv_beacon_idx.

      LOOP AT lt_regex INTO lo_regex.
        FIND FIRST OCCURRENCE OF REGEX lo_regex IN <ls_diff>-new SUBMATCHES lv_submatch.
        IF sy-subrc = 0. " Match
          lv_beacon_str = <ls_diff>-new.
          lv_submatch = to_upper( lv_submatch ).

          " Get rid of comments and end of line
          FIND FIRST OCCURRENCE OF '.' IN lv_beacon_str MATCH OFFSET lv_offs.
          IF sy-subrc <> 0.
            FIND FIRST OCCURRENCE OF '"' IN lv_beacon_str MATCH OFFSET lv_offs.
          ENDIF.

          IF lv_offs > 0.
            lv_beacon_str = lv_beacon_str(lv_offs).
          ENDIF.
          lv_beacon_str = condense( val = lv_beacon_str
                                    del = ` ` ).

          IF lv_submatch = 'CLASS'.
            lv_beacon_2lev = replace( val   = lv_beacon_str
                                      regex = '\s+(DEFINITION|IMPLEMENTATION)'
                                      with  = ''
                                      occ   = 0 ).
          ELSEIF lv_submatch = 'METHOD'.
            lv_beacon_str = lv_beacon_2lev && ` => ` && lv_beacon_str.
          ELSEIF lv_submatch = 'PUBLIC' OR lv_submatch = 'PROTECTED' OR lv_submatch = 'PRIVATE'.
            lv_beacon_str = lv_beacon_2lev && ` ` && lv_beacon_str.
          ENDIF.

          APPEND lv_beacon_str TO mt_beacons.
          lv_beacon_idx    = sy-tabix.
          <ls_diff>-beacon = lv_beacon_idx.
          EXIT. "Loop
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.
  METHOD set_patch_by_old_diff.

    FIELD-SYMBOLS: <ls_diff> TYPE Lif_abapgit_definitions=>ty_diff.

    LOOP AT mt_diff ASSIGNING <ls_diff>
                    USING KEY new_num
                    WHERE old     = is_diff_old-old
                      AND new     = is_diff_old-new
                      AND new_num = is_diff_old-new_num
                      AND old_num = is_diff_old-old_num.

      <ls_diff>-patch_flag = iv_patch_flag.
      EXIT.

    ENDLOOP.

  ENDMETHOD.
  METHOD set_patch_new.

    FIELD-SYMBOLS: <ls_diff> TYPE Lif_abapgit_definitions=>ty_diff.

    READ TABLE mt_diff WITH TABLE KEY new_num
                       COMPONENTS new_num = iv_line_new
                       ASSIGNING <ls_diff>.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Invalid new line number { iv_line_new }| ).
    ENDIF.

    <ls_diff>-patch_flag = iv_patch_flag.

  ENDMETHOD.
  METHOD set_patch_old.

    FIELD-SYMBOLS: <ls_diff> TYPE Lif_abapgit_definitions=>ty_diff.

    READ TABLE mt_diff WITH TABLE KEY old_num
                       COMPONENTS old_num = iv_line_old
                       ASSIGNING <ls_diff>.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Invalid old line number { iv_line_old }| ).
    ENDIF.

    <ls_diff>-patch_flag = iv_patch_flag.

  ENDMETHOD.
  METHOD shortlist.

    DATA: lv_index TYPE i.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF mt_diff.

    IF lines( mt_diff ) < 20.
      LOOP AT mt_diff ASSIGNING <ls_diff>.
        <ls_diff>-short = abap_true.
      ENDLOOP.
    ELSE.
      LOOP AT mt_diff TRANSPORTING NO FIELDS
          WHERE NOT result IS INITIAL AND short = abap_false.
        lv_index = sy-tabix.

        DO 8 TIMES. " Backward
          READ TABLE mt_diff INDEX ( lv_index - sy-index ) ASSIGNING <ls_diff>.
          IF sy-subrc <> 0 OR <ls_diff>-short = abap_true. " tab bound or prev marker
            EXIT.
          ENDIF.
          <ls_diff>-short = abap_true.
        ENDDO.

        DO 8 TIMES. " Forward
          READ TABLE mt_diff INDEX ( lv_index + sy-index - 1 ) ASSIGNING <ls_diff>.
          IF sy-subrc <> 0. " tab bound reached
            EXIT.
          ENDIF.
          CHECK <ls_diff>-short = abap_false. " skip marked
          <ls_diff>-short = abap_true.
        ENDDO.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD stats.
    rs_count = ms_stats.
  ENDMETHOD.
  METHOD unpack.

    DATA: lv_new      TYPE string,
          lv_old      TYPE string,
          lv_new_last TYPE c LENGTH 1,
          lv_old_last TYPE c LENGTH 1.

    lv_new = Lcl_abapgit_convert=>xstring_to_string_utf8( iv_new ).
    lv_old = Lcl_abapgit_convert=>xstring_to_string_utf8( iv_old ).

    " Check if one value contains a final newline but the other not
    " If yes, add a special characters that's visible in diff render
    IF lv_new IS NOT INITIAL.
      lv_new_last = substring(
        val = lv_new
        off = strlen( lv_new ) - 1 ).
    ENDIF.
    IF lv_old IS NOT INITIAL.
      lv_old_last = substring(
        val = lv_old
        off = strlen( lv_old ) - 1 ).
    ENDIF.

    IF lv_new_last = cl_abap_char_utilities=>newline AND lv_old_last <> cl_abap_char_utilities=>newline
      AND lv_old IS NOT INITIAL.
      lv_old = lv_old && cl_abap_char_utilities=>form_feed.
    ELSEIF lv_new_last <> cl_abap_char_utilities=>newline AND lv_old_last = cl_abap_char_utilities=>newline
      AND lv_new IS NOT INITIAL.
      lv_new = lv_new && cl_abap_char_utilities=>form_feed.
    ENDIF.

    SPLIT lv_new AT cl_abap_char_utilities=>newline INTO TABLE et_new.
    SPLIT lv_old AT cl_abap_char_utilities=>newline INTO TABLE et_old.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_DIFF implementation

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

*CLASS zcl_abapgit_i18n_params DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BJZR4A.


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

*>>>>>>> ZCL_ABAPGIT_ECATT_HELPER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ecatt_helper======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ecatt_helper======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ECATT_HELPER implementation.
*"* method's implementations
*include methods.
  METHOD build_xml_of_object.

    " downport of CL_APL_ECATT_DOWNLOAD=>BUILD_XML_OF_OBJECT

    DATA: lo_load_help_dummy TYPE REF TO cl_apl_ecatt_load_help,
          lx_ecatt           TYPE REF TO cx_ecatt_apl,
          lv_text            TYPE string,
          li_download        TYPE REF TO Lif_abapgit_ecatt_download.

    "download method will create the xml stream
    "note: it's the redefined download( ) of each object type specific download, which is called
    TRY.
        CREATE OBJECT lo_load_help_dummy
          EXPORTING
            im_maintain_function = ''.

        io_download->download( im_object_name    = iv_object_name
                               im_object_version = iv_object_version
                               im_object_type    = iv_object_type
                               im_load_help      = lo_load_help_dummy ).

      CATCH cx_ecatt_apl INTO lx_ecatt.
        lv_text = lx_ecatt->get_text( ).
        Lcx_abapgit_exception=>raise( lv_text ).
        " note, exception cx_ecatt_ui_attachment doesn't exist in 702
      CATCH cx_ecatt.
        "will never be raised from download, when called with mv_generate_xml_no_download = 'X'.
    ENDTRY.

    li_download ?= io_download.

    rv_xml_stream = li_download->get_xml_stream( ).

  ENDMETHOD.
  METHOD download_data.

    DATA:
      lo_xml TYPE REF TO cl_apl_ecatt_xml.

    TRY.
        CALL METHOD cl_apl_ecatt_xml=>('CREATE') " doesn't exist in 702
          EXPORTING
            im_type = c_xml
          RECEIVING
            re_xml  = lo_xml.

        lo_xml->set_attributes( im_dom = ii_template_over_all ).

        lo_xml->get_attributes( IMPORTING ex_xml = rv_xml_stream ).

      CATCH cx_ecatt_apl_xml.
        RETURN.
    ENDTRY.

  ENDMETHOD.
  METHOD upload_data_from_stream.

    DATA:
      lo_xml           TYPE REF TO cl_apl_ecatt_xml,
      lv_xstr          TYPE xstring,
      li_nc_xmlref_typ TYPE REF TO if_ixml_node_collection,
      li_n_xmlref_typ  TYPE REF TO if_ixml_node,
      lv_index         TYPE i VALUE 0,
      lv_count         TYPE i.

    lv_xstr = iv_xml_stream.

    CALL METHOD cl_apl_ecatt_xml=>('CREATE') " doesn't exist in 702
      EXPORTING
        im_type = c_xml
      RECEIVING
        re_xml  = lo_xml.

* whitespace stripping needs a namespace
* remove white spaces only at the time of upload
    lo_xml->stream_to_dom( im_xstream            = lv_xstr
                           im_ignore_white_space = 'X'
                           im_uri                = cl_apl_xml_const=>schema_uri ).

    lo_xml->get_attributes( IMPORTING ex_dom = ri_template_over_all ).

* MD: Workaround, because nodes starting with "XML" are not allowed
    li_nc_xmlref_typ ?= ri_template_over_all->get_elements_by_tag_name_ns( 'XMLREF_TYP' ).
    CALL METHOD li_nc_xmlref_typ->('GET_LENGTH')  " downport
      RECEIVING
        rval = lv_count.

    WHILE lv_index < lv_count.
      li_n_xmlref_typ = li_nc_xmlref_typ->get_item( lv_index ).
      li_n_xmlref_typ->set_name( 'X-MLREF_TYP' ).
      lv_index = lv_index + 1.
    ENDWHILE.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ECATT_HELPER implementation

*>>>>>>> ZCL_ABAPGIT_FILE_DESERIALIZE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_file_deserialize==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_file_deserialize==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_file_deserialize==ccau.
*CLASS SHRITEFUH64VYIPN5I4UHL45BGDR4A DEFINITION DEFERRED.
*CLASS SHRITEFUH64VYIPN5I4UHL45BGFR4A DEFINITION DEFERRED.

*CLASS zcl_abapgit_file_deserialize DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BGDR4A.
*CLASS zcl_abapgit_file_deserialize DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BGFR4A.





class LCL_ABAPGIT_FILE_DESERIALIZE implementation.
*"* method's implementations
*include methods.
  METHOD filter_files_to_deserialize.

    DATA lt_objects LIKE rt_results.
    DATA lr_object  TYPE REF TO Lif_abapgit_definitions=>ty_result.
    DATA ls_item    TYPE Lif_abapgit_definitions=>ty_item.
    DATA lv_tabix   TYPE sy-tabix.

    rt_results = it_results.

    "preparation for object logging, sort all file entries by objects
    IF ii_log IS BOUND.
      lt_objects = rt_results.
      SORT lt_objects
        BY obj_type
           obj_name.
      DELETE ADJACENT DUPLICATES FROM lt_objects COMPARING obj_type obj_name.
      DELETE lt_objects WHERE obj_type IS INITIAL AND obj_name IS INITIAL.
    ENDIF.

    "ignore objects w/o changes
    DELETE rt_results WHERE match = abap_true.     " Full match
    "log objects w/o changes
    IF sy-subrc = 0 AND ii_log IS BOUND.
      SORT rt_results BY obj_type obj_name.
      LOOP AT lt_objects REFERENCE INTO lr_object.
        lv_tabix = sy-tabix.
        READ TABLE rt_results WITH KEY obj_type = lr_object->obj_type
                                       obj_name = lr_object->obj_name
                              BINARY SEARCH TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          "all parts of the objects have not changed
          ls_item-devclass = lr_object->package.
          ls_item-obj_type = lr_object->obj_type.
          ls_item-obj_name = lr_object->obj_name.
          ii_log->add_success(
            iv_msg  = |Object { ls_item-obj_name } (type { ls_item-obj_type }) not changed; no import required|
            is_item = ls_item ).
          "ignore object for further messages
          DELETE lt_objects INDEX lv_tabix.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "ignore objects w/o object type
    DELETE rt_results WHERE obj_type IS INITIAL.
    "log objects w/o object type
    IF sy-subrc = 0 AND ii_log IS BOUND.
      " Note: Moving the CHECK condition to the LOOP WHERE clause will lead to a
      " syntax warning in higher releases and syntax error in 702
      LOOP AT lt_objects REFERENCE INTO lr_object.
        CHECK lr_object->obj_type IS INITIAL AND lr_object->obj_name IS NOT INITIAL.
        ls_item-devclass = lr_object->package.
        ls_item-obj_type = lr_object->obj_type.
        ls_item-obj_name = lr_object->obj_name.
        ii_log->add_warning(
          iv_msg  = |Object type for { ls_item-obj_name } not defined - will be ignored by abapGit|
          is_item = ls_item ).
      ENDLOOP.
      DELETE lt_objects WHERE obj_type IS INITIAL.
    ENDIF.

    "ignore objects that exists only local
    DELETE rt_results WHERE lstate = Lif_abapgit_definitions=>c_state-added AND rstate IS INITIAL.
    "ignore objects that where deleted remotely
    DELETE rt_results WHERE rstate = Lif_abapgit_definitions=>c_state-deleted.
    "log objects that exists only local or where deleted remotely
    IF sy-subrc = 0 AND ii_log IS BOUND.
      SORT rt_results BY obj_type obj_name.
      LOOP AT lt_objects REFERENCE INTO lr_object.
        lv_tabix = sy-tabix.
        READ TABLE rt_results WITH KEY obj_type = lr_object->obj_type
                                       obj_name = lr_object->obj_name
                              BINARY SEARCH TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          "all parts exists only local
          "no log message; ignore object for further messages
          DELETE lt_objects INDEX lv_tabix.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "ignore table content
    DELETE rt_results WHERE path = Lif_abapgit_data_config=>c_default_path.

    SORT rt_results
      BY obj_type ASCENDING
         obj_name ASCENDING
         rstate   DESCENDING  " ensures that non-empty rstate is kept
         lstate   DESCENDING. " ensures that non-empty lstate is kept
    DELETE ADJACENT DUPLICATES FROM rt_results COMPARING obj_type obj_name.

  ENDMETHOD.
  METHOD get_results.

    DATA lt_results TYPE Lif_abapgit_definitions=>ty_results_tt.

    lt_results = filter_files_to_deserialize(
      it_results = Lcl_abapgit_repo_status=>calculate( io_repo )
      ii_log     = ii_log ).

    rt_results = prioritize_deser(
      ii_log     = ii_log
      it_results = lt_results ).

  ENDMETHOD.
  METHOD map_results_to_items.

    DATA ls_item LIKE LINE OF rt_items.
    FIELD-SYMBOLS: <ls_result> TYPE Lif_abapgit_definitions=>ty_result.

    LOOP AT it_results ASSIGNING <ls_result>.
      ls_item-devclass = <ls_result>-package.
      ls_item-obj_type = <ls_result>-obj_type.
      ls_item-obj_name = <ls_result>-obj_name.
      INSERT ls_item INTO TABLE rt_items.
    ENDLOOP.

  ENDMETHOD.
  METHOD prioritize_deser.

    DATA lt_items    TYPE Lif_abapgit_definitions=>ty_items_tt.
    DATA ls_item     LIKE LINE OF lt_items.
    DATA lt_requires TYPE Lif_abapgit_definitions=>ty_items_tt.
    DATA ls_require  LIKE LINE OF lt_requires.
    DATA ls_result   LIKE LINE OF it_results.
    DATA lo_graph    TYPE REF TO Lcl_abapgit_item_graph.

    lt_items = map_results_to_items( it_results ).

    CREATE OBJECT lo_graph EXPORTING it_items = lt_items.

    LOOP AT lt_items INTO ls_item.
      CLEAR lt_requires.

* TODO: BEGIN extract to object handler method in ZIF_ABAPGIT_OBJECT:
*    METHODS get_deserialize_order
*      IMPORTING
*        it_items TYPE ty_items_tt
*      RETURNING
*        VALUE(rt_requries) TYPE ty_items_tt

      CASE ls_item-obj_type.
        WHEN 'SPRX'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'WEBI'.
        WHEN 'CLAS'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'SPRX'
            AND obj_type <> 'INTF'
            AND obj_type <> 'XSLT'.
        WHEN 'PROG'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'XSLT'.
        WHEN 'INTF'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'SPRX'
            AND obj_type <> 'XSLT'.
        WHEN 'TABL'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'SPRX'.
        WHEN 'IARP'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'IASP'.
        WHEN 'IATU' OR 'IAXU' OR 'IAMU'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'IASP'
            AND obj_type <> 'PROG'
            AND obj_type <> 'IARP'.
        WHEN 'DCLS'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'DDLS'.
        WHEN 'ODSO'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'IOBJ'.
        WHEN 'SCP1'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'TOBJ'.
        WHEN 'CHAR'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'OTGR'.
        WHEN 'PINF'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'CLAS'
            AND obj_type <> 'INTF'
            AND obj_type <> 'TABL'
            AND obj_type <> 'DOMA'
            AND obj_type <> 'DTEL'.
        WHEN 'DEVC'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'PINF'.
        WHEN 'ENHC'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'ENHO'.
        WHEN 'ENHO'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'ENSC' AND obj_type <> 'ENHS'.
        WHEN 'ENSC'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'ENHS'.
      ENDCASE.
* TODO: END extract to object handler method

      LOOP AT lt_requires INTO ls_require.
        lo_graph->add_edge(
          is_from = ls_require
          is_to   = ls_item ).
      ENDLOOP.
    ENDLOOP.

    WHILE lo_graph->has_vertices( ) = abap_true.
      ls_item = lo_graph->get_next( ii_log ).
      READ TABLE it_results INTO ls_result WITH KEY sec_key COMPONENTS
        obj_name = ls_item-obj_name
        obj_type = ls_item-obj_type.
      ASSERT sy-subrc = 0.
      APPEND ls_result TO rt_results.
    ENDWHILE.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_FILE_DESERIALIZE implementation

*>>>>>>> ZCL_ABAPGIT_OBJECTS_BRIDGE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_objects_bridge====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_objects_bridge====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECTS_BRIDGE implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    DATA ls_objtype_map LIKE LINE OF gt_objtype_map.

    super->constructor( is_item = is_item
                        iv_language = Lif_abapgit_definitions=>c_english ).

    initialize( ).

*    determine the responsible plugin
    READ TABLE gt_objtype_map INTO ls_objtype_map
      WITH TABLE KEY obj_typ = is_item-obj_type.
    IF sy-subrc = 0.
      CREATE OBJECT mo_plugin TYPE (ls_objtype_map-plugin_class).

      CALL METHOD mo_plugin->('SET_ITEM')
        EXPORTING
          iv_obj_type = is_item-obj_type
          iv_obj_name = is_item-obj_name.
    ELSE.
      RAISE EXCEPTION TYPE cx_sy_create_object_error
        EXPORTING
          classname = 'LCL_OBJECTS_BRIDGE'.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    rv_user = c_user_unknown. " todo
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.
    DATA lx_plugin TYPE REF TO cx_static_check.

    TRY.
        CALL METHOD mo_plugin->('LIF_ABAPGITP_PLUGIN~DELETE').
      CATCH cx_static_check INTO lx_plugin.
        Lcx_abapgit_exception=>raise( lx_plugin->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lx_plugin        TYPE REF TO cx_static_check.

    TRY.
        CALL METHOD mo_plugin->('WRAP_DESERIALIZE')
          EXPORTING
            iv_package = iv_package
            io_xml     = io_xml.
      CATCH cx_static_check INTO lx_plugin.
        Lcx_abapgit_exception=>raise( lx_plugin->get_text( ) ).
    ENDTRY.
  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    CALL METHOD mo_plugin->('LIF_ABAPGITP_PLUGIN~EXISTS')
      RECEIVING
        rv_bool = rv_bool.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.

    DATA ls_meta TYPE ty_metadata.

    CALL METHOD mo_plugin->('LIF_ABAPGITP_PLUGIN~GET_METADATA')
      RECEIVING
        rs_metadata = ls_meta.

    IF ls_meta-late_deser = abap_true.
      APPEND Lif_abapgit_object=>gc_step_id-late TO rt_steps.
    ELSEIF ls_meta-ddic = abap_true.
      APPEND Lif_abapgit_object=>gc_step_id-ddic TO rt_steps.
    ELSE.
      APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.

    DATA ls_meta TYPE ty_metadata.

    CALL METHOD mo_plugin->('LIF_ABAPGITP_PLUGIN~GET_METADATA')
      RECEIVING
        rs_metadata = ls_meta.

    MOVE-CORRESPONDING ls_meta TO rs_metadata.

  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = abap_true.
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    CALL METHOD mo_plugin->('LIF_ABAPGITP_PLUGIN~JUMP').
    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    CALL METHOD mo_plugin->('WRAP_SERIALIZE')
      EXPORTING
        io_xml = io_xml.

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
  METHOD initialize.

    DATA lt_plugin_class    TYPE STANDARD TABLE OF seoclsname WITH DEFAULT KEY.
    DATA lv_plugin_class    LIKE LINE OF lt_plugin_class.
    DATA lo_plugin          TYPE REF TO object.
    DATA lt_plugin_obj_type TYPE STANDARD TABLE OF tadir-object WITH DEFAULT KEY.
    DATA ls_objtype_map     LIKE LINE OF gt_objtype_map.

    IF gv_init = abap_true.
      RETURN.
    ENDIF.
    gv_init = abap_true.

    SELECT clsname
      FROM seometarel
      INTO TABLE lt_plugin_class
      WHERE refclsname LIKE 'LCL_ABAPGITP_OBJECT%'
      AND version = '1'
      ORDER BY clsname.                                   "#EC CI_SUBRC

    CLEAR gt_objtype_map.
    LOOP AT lt_plugin_class INTO lv_plugin_class
        WHERE table_line <> 'LCL_ABAPGITP_OBJECT_BY_SOBJ'.
* have the generic plugin only as fallback
      TRY.
          CREATE OBJECT lo_plugin TYPE (lv_plugin_class).
        CATCH cx_sy_create_object_error.
          CONTINUE. ">>>>>>>>>>>>>>
      ENDTRY.

      CALL METHOD lo_plugin->('GET_SUPPORTED_OBJ_TYPES')
        IMPORTING
          rt_obj_type = lt_plugin_obj_type.

      ls_objtype_map-plugin_class = lv_plugin_class.
      LOOP AT lt_plugin_obj_type INTO ls_objtype_map-obj_typ.
        INSERT ls_objtype_map INTO TABLE gt_objtype_map.
        IF sy-subrc <> 0.
* No exception in class-contructor possible.
* Anyway, a shortdump is more appropriate in this case
          ASSERT 'There must not be' =
            |multiple abapGit-Plugins for the same object type {
            ls_objtype_map-obj_typ }|.
        ENDIF.
      ENDLOOP.
    ENDLOOP. "at plugins

* and the same for the generic plugin if exists
* have the generic plugin only as fallback
    LOOP AT lt_plugin_class INTO lv_plugin_class
        WHERE table_line = 'LCL_ABAPGITP_OBJECT_BY_SOBJ'.
      CREATE OBJECT lo_plugin TYPE (lv_plugin_class).

      CALL METHOD lo_plugin->('GET_SUPPORTED_OBJ_TYPES')
        RECEIVING
          rt_obj_type = lt_plugin_obj_type.

      ls_objtype_map-plugin_class = lv_plugin_class.
      LOOP AT lt_plugin_obj_type INTO ls_objtype_map-obj_typ.
        INSERT ls_objtype_map INTO TABLE gt_objtype_map. "knowingly ignore the subrc
      ENDLOOP.
    ENDLOOP. "at plugins

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECTS_BRIDGE implementation

*>>>>>>> ZCL_ABAPGIT_GIT_PACK <<<<<<<*

*"* macro definitions
*include zcl_abapgit_git_pack==========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_git_pack==========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS SHRITEFUH64VYIPN5I4UHL45BG3R4A IMPLEMENTATION.

  METHOD constructor.
    mv_data = iv_data.
  ENDMETHOD.

  METHOD eat_byte.
    rv_x = mv_data(1).
    mv_data = mv_data+1.
  ENDMETHOD.

  METHOD get.
    rv_data = mv_data.
  ENDMETHOD.

  METHOD eat_bytes.
    rv_x = mv_data(iv_length).
    mv_data = mv_data+iv_length.
  ENDMETHOD.

ENDCLASS.

*"* test class
*include zcl_abapgit_git_pack==========ccau.





*CLASS SHRITEFUH64VYIPN5I4UHL45BHBR4A DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_pack DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BHBR4A.



*CLASS SHRITEFUH64VYIPN5I4UHL45BHDR4A DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_pack DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BHDR4A.









*CLASS SHRITEFUH64VYIPN5I4UHL45BHJR4A DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_pack DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BHJR4A.



class LCL_ABAPGIT_GIT_PACK implementation.
*"* method's implementations
*include methods.
  METHOD decode.

    DATA: lv_x              TYPE x,
          lv_data           TYPE xstring,
          lv_type           TYPE c LENGTH 6,
          lv_zlib           TYPE x LENGTH 2,
          lv_objects        TYPE i,
          lv_len            TYPE i,
          lv_sha1           TYPE Lif_abapgit_git_definitions=>ty_sha1,
          lv_ref_delta      TYPE Lif_abapgit_git_definitions=>ty_sha1,
          lv_compressed_len TYPE i,
          lv_compressed     TYPE xstring,
          lv_decompressed   TYPE xstring,
          lv_decompress_len TYPE i,
          lv_xstring        TYPE xstring,
          lv_expected       TYPE i,
          ls_object         LIKE LINE OF rt_objects,
          lv_uindex         TYPE sy-index.


    lv_data = iv_data.

* header
    IF NOT xstrlen( lv_data ) > 4 OR lv_data(4) <> c_pack_start.
      Lcx_abapgit_exception=>raise( |Unexpected pack header| ).
    ENDIF.
    lv_data = lv_data+4.

* version
    IF lv_data(4) <> c_version.
      Lcx_abapgit_exception=>raise( |Version not supported| ).
    ENDIF.
    lv_data = lv_data+4.

* number of objects
    lv_xstring = lv_data(4).
    lv_objects = Lcl_abapgit_convert=>xstring_to_int( lv_xstring ).
    lv_data = lv_data+4.


    DO lv_objects TIMES.

      lv_uindex = sy-index.

      lv_x = lv_data(1).
      lv_type = get_type( lv_x ).

      get_length( IMPORTING ev_length = lv_expected
                  CHANGING cv_data = lv_data ).

      IF lv_type = Lif_abapgit_git_definitions=>c_type-ref_d.
        lv_ref_delta = lv_data(20).
        lv_data = lv_data+20.
      ENDIF.

* strip header, '789C', CMF + FLG
      lv_zlib = lv_data(2).
      IF lv_zlib <> c_zlib AND lv_zlib <> c_zlib_hmm.
        Lcx_abapgit_exception=>raise( |Unexpected zlib header| ).
      ENDIF.
      lv_data = lv_data+2.

*******************************

      IF lv_zlib = c_zlib.
        cl_abap_gzip=>decompress_binary(
          EXPORTING
            gzip_in     = lv_data
          IMPORTING
            raw_out     = lv_decompressed
            raw_out_len = lv_decompress_len ).

        IF lv_expected <> lv_decompress_len.
          Lcx_abapgit_exception=>raise( |Decompression falied| ).
        ENDIF.

        cl_abap_gzip=>compress_binary(
          EXPORTING
            raw_in         = lv_decompressed
          IMPORTING
            gzip_out       = lv_compressed
            gzip_out_len   = lv_compressed_len ).

        IF xstrlen( lv_data ) <= lv_compressed_len OR
          lv_compressed(lv_compressed_len) <> lv_data(lv_compressed_len).
          "Lets try with zlib before error in out for good
          "This fixes issues with TFS 2017 and visualstudio.com Git repos
          zlib_decompress( CHANGING cv_data = lv_data
                                    cv_decompressed = lv_decompressed ).
        ELSE.
          lv_data = lv_data+lv_compressed_len.
        ENDIF.

      ELSEIF lv_zlib = c_zlib_hmm.
* cl_abap_gzip compression works for header '789C', but does not work for
* '7801', call custom implementation of DEFLATE algorithm.
* The custom implementation could handle both, but most likely the kernel
* implementation runs faster than the custom ABAP.
        zlib_decompress( CHANGING cv_data = lv_data
                                  cv_decompressed = lv_decompressed ).
      ENDIF.

      CLEAR ls_object.
      ls_object-adler32 = lv_data(4).
      lv_data = lv_data+4. " skip adler checksum

      IF lv_type = Lif_abapgit_git_definitions=>c_type-ref_d.
        ls_object-sha1 = lv_ref_delta.
        TRANSLATE ls_object-sha1 TO LOWER CASE.
      ELSE.
        ls_object-sha1 = Lcl_abapgit_hash=>sha1(
          iv_type = lv_type
          iv_data = lv_decompressed ).
      ENDIF.
      ls_object-type = lv_type.
      ls_object-data = lv_decompressed.
      ls_object-index = lv_uindex.
      APPEND ls_object TO rt_objects.
    ENDDO.

* check SHA1 at end of pack
    lv_len = xstrlen( iv_data ) - 20.
    lv_xstring = iv_data(lv_len).
    lv_sha1 = Lcl_abapgit_hash=>sha1_raw( lv_xstring ).
    IF to_upper( lv_sha1 ) <> lv_data.
      Lcx_abapgit_exception=>raise( |SHA1 at end of pack doesnt match| ).
    ENDIF.

    decode_deltas( CHANGING ct_objects = rt_objects ).

  ENDMETHOD.
  METHOD decode_commit.

    DATA: lv_string        TYPE string,
          lv_word          TYPE string,
          lv_offset        TYPE i,
          lv_length        TYPE i,
          lv_length_gpgsig TYPE i,
          lv_trash         TYPE string ##NEEDED,
          lt_string        TYPE TABLE OF string.

    FIELD-SYMBOLS: <lv_string> LIKE LINE OF lt_string.


    lv_string = Lcl_abapgit_convert=>xstring_to_string_utf8( iv_data ).

    SPLIT lv_string AT cl_abap_char_utilities=>newline INTO TABLE lt_string.

    LOOP AT lt_string ASSIGNING <lv_string>.
      lv_length = strlen( <lv_string> ) + 1.
      lv_string = lv_string+lv_length.

      SPLIT <lv_string> AT space INTO lv_word lv_trash.
      CASE lv_word.
        WHEN 'tree'.
          rs_commit-tree = <lv_string>+5.
        WHEN 'parent'.
          IF rs_commit-parent IS INITIAL.
            rs_commit-parent = <lv_string>+7.
          ELSE.
            rs_commit-parent2 = <lv_string>+7.
          ENDIF.
        WHEN 'author'.
          rs_commit-author = <lv_string>+7.
        WHEN 'committer'.
          rs_commit-committer = <lv_string>+10.
          EXIT. " current loop
        WHEN OTHERS.
          ASSERT 1 = 0.
      ENDCASE.

    ENDLOOP.

    lv_length = strlen( lv_string ).
    IF lv_length >= 6 AND lv_string+0(6) = 'gpgsig'.
      FIND REGEX |-----END PGP SIGNATURE-----[[:space:]]+|
        IN lv_string
        MATCH OFFSET lv_offset
        MATCH LENGTH lv_length.
      lv_length = lv_length - 1.
      lv_length_gpgsig = lv_offset + lv_length - 7.
      lv_length = lv_offset + lv_length.
      rs_commit-gpgsig = lv_string+7(lv_length_gpgsig).
      lv_string = lv_string+lv_length.
    ENDIF.

    rs_commit-body = lv_string+1.

    IF rs_commit-author IS INITIAL
        OR rs_commit-committer IS INITIAL
        OR rs_commit-tree IS INITIAL.
      Lcx_abapgit_exception=>raise( |multiple parents? not supported| ).
    ENDIF.

  ENDMETHOD.
  METHOD decode_deltas.

    DATA: ls_object   LIKE LINE OF ct_objects,
          li_progress TYPE REF TO Lif_abapgit_progress,
          lt_deltas   LIKE ct_objects.


    LOOP AT ct_objects INTO ls_object
        USING KEY type
        WHERE type = Lif_abapgit_git_definitions=>c_type-ref_d.
      INSERT ls_object INTO TABLE lt_deltas.
    ENDLOOP.

    DELETE ct_objects
      USING KEY type
      WHERE type = Lif_abapgit_git_definitions=>c_type-ref_d.

    "Restore correct Delta Order
    SORT lt_deltas BY index.

    li_progress = Lcl_abapgit_progress=>get_instance( lines( lt_deltas ) ).

    LOOP AT lt_deltas INTO ls_object.
      li_progress->show( iv_current = sy-tabix
                         iv_text    = 'Decode deltas' ).

      delta( EXPORTING is_object = ls_object
             CHANGING ct_objects = ct_objects ).
    ENDLOOP.

  ENDMETHOD.
  METHOD decode_tag.

    DATA: lv_string TYPE string,
          lv_word   TYPE string,
          lv_trash  TYPE string ##NEEDED,
          lt_string TYPE TABLE OF string.

    FIELD-SYMBOLS: <lv_string> LIKE LINE OF lt_string.


    lv_string = Lcl_abapgit_convert=>xstring_to_string_utf8( iv_data ).

    SPLIT lv_string AT cl_abap_char_utilities=>newline INTO TABLE lt_string.

    LOOP AT lt_string ASSIGNING <lv_string>.

      SPLIT <lv_string> AT space INTO lv_word lv_trash.

      CASE lv_word.
        WHEN 'object'.
          rs_tag-object = lv_trash.
        WHEN 'type'.
          rs_tag-type = lv_trash.
        WHEN 'tag'.
          rs_tag-tag = lv_trash.
        WHEN 'tagger'.

          FIND FIRST OCCURRENCE OF REGEX `(.*)<(.*)>`
                     IN lv_trash
                     SUBMATCHES rs_tag-tagger_name
                                rs_tag-tagger_email.

          rs_tag-tagger_name = condense( rs_tag-tagger_name ).

        WHEN ''.
          " ignore blank lines
          CONTINUE.
        WHEN OTHERS.

          " these are the non empty line which don't start with a key word
          " the first one is the message, the rest are cumulated to the body

          IF rs_tag-message IS INITIAL.
            rs_tag-message = <lv_string>.
          ELSE.

            IF rs_tag-body IS NOT INITIAL.
              rs_tag-body = rs_tag-body && cl_abap_char_utilities=>newline.
            ENDIF.

            rs_tag-body = rs_tag-body && <lv_string>.

          ENDIF.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.
  METHOD decode_tree.

    CONSTANTS: lc_sha_length TYPE i VALUE 20,
               lc_null       TYPE x VALUE '00'.

    DATA: lv_xstring TYPE xstring,
          lv_chmod   TYPE Lif_abapgit_git_definitions=>ty_chmod,
          lv_name    TYPE string,
          lv_string  TYPE string,
          lv_len     TYPE i,
          lv_offset  TYPE i,
          lv_cursor  TYPE i,
          lv_match   TYPE i,
          ls_node    TYPE ty_node.


    DO.
      FIND FIRST OCCURRENCE OF lc_null IN SECTION OFFSET lv_cursor OF iv_data
        IN BYTE MODE MATCH OFFSET lv_match.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      lv_len = lv_match - lv_cursor.
      lv_xstring = iv_data+lv_cursor(lv_len).

      lv_string = Lcl_abapgit_convert=>xstring_to_string_utf8( lv_xstring ).
      SPLIT lv_string AT space INTO lv_chmod lv_name.

      CLEAR ls_node.
      ls_node-chmod = lv_chmod.
      IF ls_node-chmod <> Lif_abapgit_git_definitions=>c_chmod-dir
          AND ls_node-chmod <> Lif_abapgit_git_definitions=>c_chmod-file
          AND ls_node-chmod <> Lif_abapgit_git_definitions=>c_chmod-executable
          AND ls_node-chmod <> Lif_abapgit_git_definitions=>c_chmod-submodule.
        Lcx_abapgit_exception=>raise( |Unknown chmod| ).
      ENDIF.

      lv_offset = lv_match + 1.
      ls_node-name = lv_name.
      ls_node-sha1 = iv_data+lv_offset(lc_sha_length).
      TRANSLATE ls_node-sha1 TO LOWER CASE.
      APPEND ls_node TO rt_nodes.

      lv_cursor = lv_match + 1 + lc_sha_length.
    ENDDO.

  ENDMETHOD.
  METHOD delta.

    CONSTANTS: lc_1   TYPE x VALUE '01',
               lc_2   TYPE x VALUE '02',
               lc_4   TYPE x VALUE '04',
               lc_8   TYPE x VALUE '08',
               lc_16  TYPE x VALUE '10',
               lc_32  TYPE x VALUE '20',
               lc_64  TYPE x VALUE '40',
               lc_128 TYPE x VALUE '80'.

    DATA: lv_base   TYPE xstring,
          lv_result TYPE xstring,
          lv_offset TYPE i,
          lo_stream TYPE REF TO SHRITEFUH64VYIPN5I4UHL45BG3R4A,
          lv_sha1   TYPE Lif_abapgit_git_definitions=>ty_sha1,
          ls_object LIKE LINE OF ct_objects,
          lv_len    TYPE i,
          lv_tmp    TYPE xstring,
          lv_org    TYPE x.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF ct_objects.


    CREATE OBJECT lo_stream
      EXPORTING
        iv_data = is_object-data.

* find base
    READ TABLE ct_objects ASSIGNING <ls_object>
      WITH KEY sha COMPONENTS sha1 = is_object-sha1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Base not found, { is_object-sha1 }| ).
    ELSEIF <ls_object>-type = Lif_abapgit_git_definitions=>c_type-ref_d.
* sanity check
      Lcx_abapgit_exception=>raise( |Delta, base eq delta| ).
    ENDIF.

    lv_base = <ls_object>-data.

* skip the 2 headers
    delta_header( lo_stream ).
    delta_header( lo_stream ).

    WHILE xstrlen( lo_stream->get( ) ) > 0.

      lv_org = lo_stream->eat_byte( ).

      IF lv_org BIT-AND lc_128 = lc_128. " MSB = 1

        lv_offset = 0.
        IF lv_org BIT-AND lc_1 = lc_1.
          lv_offset = lo_stream->eat_byte( ).
        ENDIF.
        IF lv_org BIT-AND lc_2 = lc_2.
          lv_offset = lv_offset + lo_stream->eat_byte( ) * 256.
        ENDIF.
        IF lv_org BIT-AND lc_4 = lc_4.
          lv_offset = lv_offset + lo_stream->eat_byte( ) * 65536.
        ENDIF.
        IF lv_org BIT-AND lc_8 = lc_8.
          lv_offset = lv_offset + lo_stream->eat_byte( ) * 16777216. " hmm, overflow?
        ENDIF.

        lv_len = 0.
        IF lv_org BIT-AND lc_16 = lc_16.
          lv_len = lo_stream->eat_byte( ).
        ENDIF.
        IF lv_org BIT-AND lc_32 = lc_32.
          lv_len = lv_len + lo_stream->eat_byte( ) * 256.
        ENDIF.
        IF lv_org BIT-AND lc_64 = lc_64.
          lv_len = lv_len + lo_stream->eat_byte( ) * 65536.
        ENDIF.

        IF lv_len = 0.
          lv_len = 65536.
        ENDIF.

        CONCATENATE lv_result lv_base+lv_offset(lv_len)
          INTO lv_result IN BYTE MODE.
      ELSE. " lv_bitbyte(1) = '0'
* insert from delta
        lv_len = lv_org. " convert to int
        lv_tmp = lo_stream->eat_bytes( lv_len ).
        CONCATENATE lv_result lv_tmp INTO lv_result IN BYTE MODE.
      ENDIF.

    ENDWHILE.

    lv_sha1 = Lcl_abapgit_hash=>sha1( iv_type = <ls_object>-type
                                      iv_data = lv_result ).

    CLEAR ls_object.
    ls_object-sha1 = lv_sha1.
    ls_object-type = <ls_object>-type.
    ls_object-data = lv_result.
    ls_object-index = <ls_object>-index. "Retain sort index
    APPEND ls_object TO ct_objects.

  ENDMETHOD.
  METHOD delta_header.

    DATA: lv_bitbyte TYPE Lif_abapgit_git_definitions=>ty_bitbyte,
          lv_bits    TYPE string,
          lv_x       TYPE x.


    lv_bits = ''.
    DO.
      lv_x = io_stream->eat_byte( ).
      lv_bitbyte = Lcl_abapgit_convert=>x_to_bitbyte( lv_x ).
      CONCATENATE lv_bitbyte+1 lv_bits INTO lv_bits.
      IF lv_bitbyte(1) = '0'.
        EXIT. " current loop
      ENDIF.
    ENDDO.
    rv_header = Lcl_abapgit_convert=>bitbyte_to_int( lv_bits ).

  ENDMETHOD.
  METHOD encode.

    DATA: lv_sha1          TYPE x LENGTH 20,
          lv_adler32       TYPE Lif_abapgit_git_definitions=>ty_adler32,
          lv_compressed    TYPE xstring,
          lv_xstring       TYPE xstring,
          li_progress      TYPE REF TO Lif_abapgit_progress,
          lv_objects_total TYPE i.

    FIELD-SYMBOLS: <ls_object>  LIKE LINE OF it_objects.


    rv_data = c_pack_start.

    CONCATENATE rv_data c_version INTO rv_data IN BYTE MODE.

    lv_xstring = Lcl_abapgit_convert=>int_to_xstring4( lines( it_objects ) ).
    CONCATENATE rv_data lv_xstring INTO rv_data IN BYTE MODE.

    lv_objects_total = lines( it_objects ).

    li_progress = Lcl_abapgit_progress=>get_instance( lv_objects_total ).

    LOOP AT it_objects ASSIGNING <ls_object>.
      li_progress->show(
        iv_current = sy-tabix
        iv_text    = |Encoding objects ( { sy-tabix } of { lv_objects_total } )| ).

      lv_xstring = type_and_length(
        iv_type   = <ls_object>-type
        iv_length = xstrlen( <ls_object>-data ) ).
      CONCATENATE rv_data lv_xstring INTO rv_data IN BYTE MODE.

      cl_abap_gzip=>compress_binary(
        EXPORTING
          raw_in   = <ls_object>-data
        IMPORTING
          gzip_out = lv_compressed ).

      CONCATENATE rv_data c_zlib lv_compressed INTO rv_data IN BYTE MODE.

      IF NOT <ls_object>-adler32 IS INITIAL.
        lv_adler32 = <ls_object>-adler32.
      ELSE.
        lv_adler32 = Lcl_abapgit_hash=>adler32( <ls_object>-data ).
      ENDIF.
      CONCATENATE rv_data lv_adler32 INTO rv_data IN BYTE MODE.

    ENDLOOP.

    lv_sha1 = to_upper( Lcl_abapgit_hash=>sha1_raw( rv_data ) ).
    CONCATENATE rv_data lv_sha1 INTO rv_data IN BYTE MODE.

  ENDMETHOD.
  METHOD encode_commit.

    DATA: lv_string       TYPE string,
          lv_tmp          TYPE string,
          lv_tree_lower   TYPE string,
          lv_parent_lower TYPE string.


    lv_tree_lower = is_commit-tree.
    TRANSLATE lv_tree_lower TO LOWER CASE.

    lv_string = ''.

    CONCATENATE 'tree' lv_tree_lower INTO lv_tmp SEPARATED BY space.
    CONCATENATE lv_string lv_tmp cl_abap_char_utilities=>newline INTO lv_string.

    IF NOT is_commit-parent IS INITIAL.
      lv_parent_lower = is_commit-parent.
      TRANSLATE lv_parent_lower TO LOWER CASE.

      CONCATENATE 'parent' lv_parent_lower
        INTO lv_tmp SEPARATED BY space.
      CONCATENATE lv_string lv_tmp cl_abap_char_utilities=>newline INTO lv_string.
    ENDIF.

    IF NOT is_commit-parent2 IS INITIAL.
      lv_parent_lower = is_commit-parent2.
      TRANSLATE lv_parent_lower TO LOWER CASE.

      CONCATENATE 'parent' lv_parent_lower
        INTO lv_tmp SEPARATED BY space.
      CONCATENATE lv_string lv_tmp cl_abap_char_utilities=>newline INTO lv_string.
    ENDIF.

    CONCATENATE 'author' is_commit-author
      INTO lv_tmp SEPARATED BY space.
    CONCATENATE lv_string lv_tmp cl_abap_char_utilities=>newline INTO lv_string.

    CONCATENATE 'committer' is_commit-committer
      INTO lv_tmp SEPARATED BY space.
    CONCATENATE lv_string lv_tmp cl_abap_char_utilities=>newline INTO lv_string.

    IF NOT is_commit-gpgsig IS INITIAL.
      CONCATENATE 'gpgsig' is_commit-gpgsig
        INTO lv_tmp SEPARATED BY space.
      CONCATENATE lv_string lv_tmp INTO lv_string.
    ENDIF.

    CONCATENATE lv_string cl_abap_char_utilities=>newline is_commit-body INTO lv_string.

    rv_data = Lcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).

  ENDMETHOD.
  METHOD encode_tag.

    DATA: lv_string TYPE string,
          lv_time   TYPE Lcl_abapgit_git_time=>ty_unixtime.

    lv_time = Lcl_abapgit_git_time=>get_unix( ).

    lv_string = |object { is_tag-object }{ cl_abap_char_utilities=>newline }|
             && |type { is_tag-type }{ cl_abap_char_utilities=>newline }|
             && |tag { Lcl_abapgit_git_tag=>remove_tag_prefix( is_tag-tag ) }{ cl_abap_char_utilities=>newline }|
             && |tagger { is_tag-tagger_name } <{ is_tag-tagger_email }> { lv_time }|
             && |{ cl_abap_char_utilities=>newline }|
             && |{ cl_abap_char_utilities=>newline }|
             && |{ is_tag-message }|.

    rv_data = Lcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).

  ENDMETHOD.
  METHOD encode_tree.

    CONSTANTS: lc_null TYPE x VALUE '00'.

    DATA: lv_string  TYPE string,
          lt_nodes   LIKE it_nodes,
          lv_hex20   TYPE x LENGTH 20,
          lv_xstring TYPE xstring.

    FIELD-SYMBOLS: <ls_node> LIKE LINE OF it_nodes.


    lt_nodes = sort_tree( it_nodes ).

    LOOP AT lt_nodes ASSIGNING <ls_node>.
      ASSERT NOT <ls_node>-chmod IS INITIAL.
      ASSERT NOT <ls_node>-name IS INITIAL.
      ASSERT NOT <ls_node>-sha1 IS INITIAL.

      CONCATENATE <ls_node>-chmod <ls_node>-name INTO lv_string SEPARATED BY space.
      lv_xstring = Lcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).

      lv_hex20 = to_upper( <ls_node>-sha1 ).
      CONCATENATE rv_data lv_xstring lc_null lv_hex20 INTO rv_data IN BYTE MODE.
    ENDLOOP.

  ENDMETHOD.
  METHOD get_length.

* https://github.com/git/git/blob/master/Documentation/technical/pack-format.txt

* n-byte sizeN (as long as MSB is set, each 7-bit)
*    size0..sizeN form 4+7+7+..+7 bit integer, size0
*    is the least significant part, and sizeN is the
*    most significant part.

    DATA: lv_x           TYPE x,
          lv_length_bits TYPE string,
          lv_bitbyte     TYPE Lif_abapgit_git_definitions=>ty_bitbyte.


    lv_x = cv_data(1).
    lv_bitbyte = Lcl_abapgit_convert=>x_to_bitbyte( lv_x ).

    cv_data = cv_data+1.
    lv_length_bits = lv_bitbyte+4.

    WHILE lv_bitbyte(1) <> '0'.
      lv_x = cv_data(1).
      lv_bitbyte = Lcl_abapgit_convert=>x_to_bitbyte( lv_x ).
      cv_data = cv_data+1.
      CONCATENATE lv_bitbyte+1 lv_length_bits INTO lv_length_bits.
    ENDWHILE.

    ev_length = Lcl_abapgit_convert=>bitbyte_to_int( lv_length_bits ).

  ENDMETHOD.
  METHOD get_type.

    CONSTANTS: lc_mask TYPE x VALUE 112.
    DATA: lv_xtype TYPE x.

    lv_xtype = iv_x BIT-AND lc_mask.

    CASE lv_xtype.
      WHEN 16.
        rv_type = Lif_abapgit_git_definitions=>c_type-commit.
      WHEN 32.
        rv_type = Lif_abapgit_git_definitions=>c_type-tree.
      WHEN 48.
        rv_type = Lif_abapgit_git_definitions=>c_type-blob.
      WHEN 64.
        rv_type = Lif_abapgit_git_definitions=>c_type-tag.
      WHEN 112.
        rv_type = Lif_abapgit_git_definitions=>c_type-ref_d.
      WHEN OTHERS.
        Lcx_abapgit_exception=>raise( |Todo, unknown git pack type| ).
    ENDCASE.

  ENDMETHOD.
  METHOD sort_tree.

    TYPES: BEGIN OF ty_sort,
             sort TYPE string,
             node TYPE ty_node,
           END OF ty_sort.

    DATA: lt_sort TYPE STANDARD TABLE OF ty_sort WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_sort> LIKE LINE OF lt_sort,
                   <ls_node> LIKE LINE OF it_nodes.


    LOOP AT it_nodes ASSIGNING <ls_node>.
      APPEND INITIAL LINE TO lt_sort ASSIGNING <ls_sort>.
      IF <ls_node>-chmod = Lif_abapgit_git_definitions=>c_chmod-dir.
        CONCATENATE <ls_node>-name '/' INTO <ls_sort>-sort.
      ELSE.
        <ls_sort>-sort = <ls_node>-name.
      ENDIF.
      <ls_sort>-node = <ls_node>.
    ENDLOOP.

* following has to be done, or unpack will fail on server side
    SORT lt_sort BY sort ASCENDING.

    LOOP AT lt_sort ASSIGNING <ls_sort>.
      APPEND <ls_sort>-node TO rt_nodes.
    ENDLOOP.

  ENDMETHOD.
  METHOD type_and_length.

* see http://stefan.saasen.me/articles/git-clone-in-haskell-from-the-bottom-up/#pack_file_objects

    DATA: lv_type   TYPE i,
          lv_length TYPE i,
          lv_hex    TYPE x LENGTH 1.


    CASE iv_type.
      WHEN Lif_abapgit_git_definitions=>c_type-commit.
        lv_type = 16.
      WHEN Lif_abapgit_git_definitions=>c_type-tree.
        lv_type = 32.
      WHEN Lif_abapgit_git_definitions=>c_type-blob.
        lv_type = 48.
      WHEN Lif_abapgit_git_definitions=>c_type-tag.
        lv_type = 64.
      WHEN Lif_abapgit_git_definitions=>c_type-ref_d.
        lv_type = 112.
      WHEN OTHERS.
        Lcx_abapgit_exception=>raise( |Unexpected object type while encoding pack| ).
    ENDCASE.

    lv_length = iv_length.

* first byte
    IF lv_length > 15.
      lv_hex = 128.
    ENDIF.
    lv_hex = lv_hex + lv_type + lv_length MOD 16.
    rv_xstring = lv_hex.
    lv_length = lv_length DIV 16.

* subsequent bytes
    WHILE lv_length >= 128.
      lv_hex = 128 + lv_length MOD 128.
      CONCATENATE rv_xstring lv_hex INTO rv_xstring IN BYTE MODE.
      lv_length = lv_length DIV 128.
    ENDWHILE.

* last byte
    IF lv_length > 0.
      lv_hex = lv_length.
      CONCATENATE rv_xstring lv_hex INTO rv_xstring IN BYTE MODE.
    ENDIF.

  ENDMETHOD.
  METHOD zlib_decompress.

    DATA: ls_data           TYPE Lcl_abapgit_zlib=>ty_decompress,
          lv_compressed_len TYPE i,
          lv_adler32        TYPE Lif_abapgit_git_definitions=>ty_adler32.


    ls_data = Lcl_abapgit_zlib=>decompress( cv_data ).
    lv_compressed_len = ls_data-compressed_len.
    cv_decompressed = ls_data-raw.

    IF lv_compressed_len IS INITIAL.
      Lcx_abapgit_exception=>raise( |Decompression falied :o/| ).
    ENDIF.

    cv_data = cv_data+lv_compressed_len.

    lv_adler32 = Lcl_abapgit_hash=>adler32( cv_decompressed ).
    IF cv_data(4) <> lv_adler32.
      cv_data = cv_data+1.
    ENDIF.
    IF cv_data(4) <> lv_adler32.
      cv_data = cv_data+1.
    ENDIF.
    IF cv_data(4) <> lv_adler32.
      Lcx_abapgit_exception=>raise( |Wrong Adler checksum| ).
    ENDIF.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GIT_PACK implementation

*>>>>>>> ZCL_ABAPGIT_GIT_PORCELAIN <<<<<<<*

*"* macro definitions
*include zcl_abapgit_git_porcelain=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_git_porcelain=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_git_porcelain=====ccau.
*CLASS SHRITEFUH64VYIPN5I4UHL45BHLR4A DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_porcelain DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BHLR4A.



class LCL_ABAPGIT_GIT_PORCELAIN implementation.
*"* method's implementations
*include methods.
  METHOD build_trees.

    DATA: lt_nodes   TYPE Lcl_abapgit_git_pack=>ty_nodes_tt,
          ls_tree    LIKE LINE OF rt_trees,
          lv_len     TYPE i,
          lt_folders TYPE ty_folders_tt.

    FIELD-SYMBOLS: <ls_folder> LIKE LINE OF lt_folders,
                   <ls_node>   LIKE LINE OF lt_nodes,
                   <ls_sub>    LIKE LINE OF lt_folders,
                   <ls_exp>    LIKE LINE OF it_expanded.


    lt_folders = find_folders( it_expanded ).

* start with the deepest folders
    SORT lt_folders BY count DESCENDING.

    LOOP AT lt_folders ASSIGNING <ls_folder>.
      CLEAR lt_nodes.

* files
      LOOP AT it_expanded ASSIGNING <ls_exp> USING KEY path_name WHERE path = <ls_folder>-path.
        APPEND INITIAL LINE TO lt_nodes ASSIGNING <ls_node>.
        <ls_node>-chmod = <ls_exp>-chmod.
        <ls_node>-name  = <ls_exp>-name.
        <ls_node>-sha1  = <ls_exp>-sha1.
      ENDLOOP.

* folders
      LOOP AT lt_folders ASSIGNING <ls_sub> WHERE count = <ls_folder>-count + 1.
        lv_len = strlen( <ls_folder>-path ).
        IF strlen( <ls_sub>-path ) > lv_len AND <ls_sub>-path(lv_len) = <ls_folder>-path.
          APPEND INITIAL LINE TO lt_nodes ASSIGNING <ls_node>.
          <ls_node>-chmod = Lif_abapgit_git_definitions=>c_chmod-dir.

* extract folder name, this can probably be done easier using regular expressions
          <ls_node>-name = <ls_sub>-path+lv_len.
          lv_len = strlen( <ls_node>-name ) - 1.
          <ls_node>-name = <ls_node>-name(lv_len).

          <ls_node>-sha1 = <ls_sub>-sha1.
        ENDIF.
      ENDLOOP.

      CLEAR ls_tree.
      ls_tree-path = <ls_folder>-path.
      ls_tree-data = Lcl_abapgit_git_pack=>encode_tree( lt_nodes ).
      ls_tree-sha1 = Lcl_abapgit_hash=>sha1_tree( ls_tree-data ).
      APPEND ls_tree TO rt_trees.

      <ls_folder>-sha1 = ls_tree-sha1.
    ENDLOOP.

  ENDMETHOD.
  METHOD create_annotated_tag.

    DATA: lv_tag          TYPE xstring,
          lt_objects      TYPE Lif_abapgit_definitions=>ty_objects_tt,
          lv_pack         TYPE xstring,
          ls_object       LIKE LINE OF lt_objects,
          ls_tag          TYPE Lcl_abapgit_git_pack=>ty_tag,
          lv_new_tag_sha1 TYPE Lif_abapgit_git_definitions=>ty_sha1.

* new tag
    ls_tag-object       = is_tag-sha1.
    ls_tag-type         = Lif_abapgit_git_definitions=>c_type-commit.
    ls_tag-tag          = is_tag-name.
    ls_tag-tagger_name  = is_tag-tagger_name.
    ls_tag-tagger_email = is_tag-tagger_email.
    ls_tag-message      = is_tag-message
                      && |{ cl_abap_char_utilities=>newline }|
                      && |{ cl_abap_char_utilities=>newline }|
                      && is_tag-body.

    lv_tag = Lcl_abapgit_git_pack=>encode_tag( ls_tag ).

    lv_new_tag_sha1 = Lcl_abapgit_hash=>sha1_tag( lv_tag ).

    ls_object-sha1  = lv_new_tag_sha1.
    ls_object-type  = Lif_abapgit_git_definitions=>c_type-tag.
    ls_object-data  = lv_tag.
    ls_object-index = 1.
    APPEND ls_object TO lt_objects.

    lv_pack = Lcl_abapgit_git_pack=>encode( lt_objects ).

    Lcl_abapgit_git_transport=>receive_pack(
      iv_url         = iv_url
      iv_old         = c_zero
      iv_new         = lv_new_tag_sha1
      iv_branch_name = is_tag-name
      iv_pack        = lv_pack ).

  ENDMETHOD.
  METHOD create_branch.

    IF iv_name CS ` `.
      Lcx_abapgit_exception=>raise( 'Branch name cannot contain blank spaces' ).
    ENDIF.

    Lcl_abapgit_git_transport=>receive_pack(
      iv_url         = iv_url
      iv_old         = c_zero
      iv_new         = iv_from
      iv_branch_name = iv_name
      iv_pack        = empty_packfile( ) ).

  ENDMETHOD.
  METHOD create_lightweight_tag.

    Lcl_abapgit_git_transport=>receive_pack(
      iv_url         = iv_url
      iv_old         = c_zero
      iv_new         = is_tag-sha1
      iv_branch_name = is_tag-name
      iv_pack        = empty_packfile( ) ).

  ENDMETHOD.
  METHOD create_tag.

    IF is_tag-name CS ` `.
      Lcx_abapgit_exception=>raise( 'Tag name cannot contain blank spaces' ).
    ENDIF.

    CASE is_tag-type.
      WHEN Lif_abapgit_git_definitions=>c_git_branch_type-annotated_tag.

        create_annotated_tag(
          is_tag = is_tag
          iv_url = iv_url ).

      WHEN Lif_abapgit_git_definitions=>c_git_branch_type-lightweight_tag.

        create_lightweight_tag(
          is_tag = is_tag
          iv_url = iv_url ).

      WHEN OTHERS.

        Lcx_abapgit_exception=>raise( |Invalid tag type: { is_tag-type }| ).

    ENDCASE.

  ENDMETHOD.
  METHOD delete_annotated_tag.

    DATA:
      lo_branches TYPE REF TO Lcl_abapgit_git_branch_list,
      lv_tag      TYPE string,
      ls_tag      TYPE Lif_abapgit_git_definitions=>ty_git_branch,
      lt_tags     TYPE Lif_abapgit_git_definitions=>ty_git_branch_list_tt.

    " For annotated tags, find the correct commit
    lo_branches = Lcl_abapgit_git_transport=>branches( iv_url ).
    lt_tags     = lo_branches->get_tags_only( ).
    lv_tag      = Lcl_abapgit_git_tag=>remove_peel( is_tag-name ).

    READ TABLE lt_tags INTO ls_tag WITH KEY name_key COMPONENTS name = lv_tag.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Annotated tag { lv_tag } not found| ).
    ENDIF.

    Lcl_abapgit_git_transport=>receive_pack(
      iv_url         = iv_url
      iv_old         = ls_tag-sha1
      iv_new         = c_zero
      iv_branch_name = ls_tag-name ).

  ENDMETHOD.
  METHOD delete_branch.

    Lcl_abapgit_git_transport=>receive_pack(
      iv_url         = iv_url
      iv_old         = is_branch-sha1
      iv_new         = c_zero
      iv_branch_name = is_branch-name ).

  ENDMETHOD.
  METHOD delete_lightweight_tag.

    Lcl_abapgit_git_transport=>receive_pack(
      iv_url         = iv_url
      iv_old         = is_tag-sha1
      iv_new         = c_zero
      iv_branch_name = is_tag-name ).

  ENDMETHOD.
  METHOD delete_tag.

    IF is_tag-name CS Lif_abapgit_git_definitions=>c_git_branch-peel.

      delete_annotated_tag(
        is_tag = is_tag
        iv_url = iv_url ).

    ELSE.

      delete_lightweight_tag(
        is_tag = is_tag
        iv_url = iv_url ).

    ENDIF.

  ENDMETHOD.
  METHOD empty_packfile.

    " For avoiding "client MUST send an empty packfile" error
    " https://github.com/git/git/blob/master/Documentation/gitprotocol-pack.txt#L595-L599

    DATA lt_objects TYPE Lif_abapgit_definitions=>ty_objects_tt.

    rv_pack = Lcl_abapgit_git_pack=>encode( lt_objects ).

  ENDMETHOD.
  METHOD find_folders.

    DATA: lt_paths TYPE TABLE OF string,
          lv_split TYPE string,
          lv_path  TYPE string.

    FIELD-SYMBOLS: <ls_folder> LIKE LINE OF rt_folders,
                   <ls_new>    LIKE LINE OF rt_folders,
                   <ls_exp>    LIKE LINE OF it_expanded.


    LOOP AT it_expanded ASSIGNING <ls_exp>.
      READ TABLE rt_folders WITH KEY path = <ls_exp>-path TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO rt_folders ASSIGNING <ls_folder>.
        <ls_folder>-path = <ls_exp>-path.
      ENDIF.
    ENDLOOP.

* add empty folders
    LOOP AT rt_folders ASSIGNING <ls_folder>.
      SPLIT <ls_folder>-path AT '/' INTO TABLE lt_paths.

      CLEAR lv_path.
      LOOP AT lt_paths INTO lv_split.
        CONCATENATE lv_path lv_split '/' INTO lv_path.
        READ TABLE rt_folders WITH KEY path = lv_path TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO rt_folders ASSIGNING <ls_new>.
          <ls_new>-path = lv_path.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    LOOP AT rt_folders ASSIGNING <ls_folder>.
      FIND ALL OCCURRENCES OF '/' IN <ls_folder>-path MATCH COUNT <ls_folder>-count.
    ENDLOOP.

  ENDMETHOD.
  METHOD full_tree.

    DATA: ls_object LIKE LINE OF it_objects,
          ls_commit TYPE Lcl_abapgit_git_pack=>ty_commit.

    READ TABLE it_objects INTO ls_object
      WITH KEY type COMPONENTS
        type = Lif_abapgit_git_definitions=>c_type-commit
        sha1 = iv_parent.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'commit not found' ).
    ENDIF.
    ls_commit = Lcl_abapgit_git_pack=>decode_commit( ls_object-data ).

    rt_expanded = walk_tree( it_objects = it_objects
                             iv_tree    = ls_commit-tree
                             iv_base    = '/' ).

  ENDMETHOD.
  METHOD pull.

    DATA: ls_object TYPE Lif_abapgit_definitions=>ty_object,
          ls_commit TYPE Lcl_abapgit_git_pack=>ty_commit.

    READ TABLE it_objects INTO ls_object
      WITH KEY type COMPONENTS
        type = Lif_abapgit_git_definitions=>c_type-commit
        sha1 = iv_commit.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Commit/Branch not found.' ).
    ENDIF.

    ls_commit = Lcl_abapgit_git_pack=>decode_commit( ls_object-data ).

    walk( EXPORTING it_objects = it_objects
                    iv_sha1    = ls_commit-tree
                    iv_path    = '/'
          CHANGING  ct_files   = rt_files ).

  ENDMETHOD.
  METHOD pull_by_branch.

    Lcl_abapgit_git_transport=>upload_pack_by_branch(
      EXPORTING
        iv_url          = iv_url
        iv_branch_name  = iv_branch_name
        iv_deepen_level = iv_deepen_level
      IMPORTING
        et_objects      = rs_result-objects
        ev_branch       = rs_result-commit ).

    rs_result-files = pull( iv_commit  = rs_result-commit
                            it_objects = rs_result-objects ).

  ENDMETHOD.
  METHOD pull_by_commit.

    Lcl_abapgit_git_transport=>upload_pack_by_commit(
      EXPORTING
        iv_url          = iv_url
        iv_hash         = iv_commit_hash
        iv_deepen_level = iv_deepen_level
      IMPORTING
        et_objects      = rs_result-objects
        ev_commit       = rs_result-commit ).

    rs_result-files = pull( iv_commit  = rs_result-commit
                            it_objects = rs_result-objects ).

  ENDMETHOD.
  METHOD push.

    DATA: lt_expanded TYPE Lif_abapgit_git_definitions=>ty_expanded_tt,
          lt_blobs    TYPE Lif_abapgit_git_definitions=>ty_files_tt,
          lv_sha1     TYPE Lif_abapgit_git_definitions=>ty_sha1,
          lv_new_tree TYPE Lif_abapgit_git_definitions=>ty_sha1,
          lt_trees    TYPE ty_trees_tt,
          lt_stage    TYPE Lif_abapgit_definitions=>ty_stage_tt.

    FIELD-SYMBOLS: <ls_stage>   LIKE LINE OF lt_stage,
                   <ls_updated> LIKE LINE OF rs_result-updated_files,
                   <ls_exp>     LIKE LINE OF lt_expanded.

    lt_expanded = full_tree( it_objects = it_old_objects
                             iv_parent  = iv_parent ).

    lt_stage = io_stage->get_all( ).
    LOOP AT lt_stage ASSIGNING <ls_stage>.

      " Save file ref to updated files table
      APPEND INITIAL LINE TO rs_result-updated_files ASSIGNING <ls_updated>.
      MOVE-CORRESPONDING <ls_stage>-file TO <ls_updated>.

      CASE <ls_stage>-method.
        WHEN Lif_abapgit_definitions=>c_method-add.

          APPEND <ls_stage>-file TO lt_blobs.

          READ TABLE lt_expanded ASSIGNING <ls_exp> WITH TABLE KEY path_name COMPONENTS
            name = <ls_stage>-file-filename
            path = <ls_stage>-file-path.
          IF sy-subrc <> 0. " new files
            APPEND INITIAL LINE TO lt_expanded ASSIGNING <ls_exp>.
            <ls_exp>-name  = <ls_stage>-file-filename.
            <ls_exp>-path  = <ls_stage>-file-path.
            <ls_exp>-chmod = Lif_abapgit_git_definitions=>c_chmod-file.
          ENDIF.

          lv_sha1 = Lcl_abapgit_hash=>sha1_blob( <ls_stage>-file-data ).
          IF <ls_exp>-sha1 <> lv_sha1.
            <ls_exp>-sha1 = lv_sha1.
          ENDIF.

          <ls_updated>-sha1 = lv_sha1.   "New sha1

        WHEN Lif_abapgit_definitions=>c_method-rm.
          READ TABLE lt_expanded ASSIGNING <ls_exp> WITH TABLE KEY path_name COMPONENTS
            name = <ls_stage>-file-filename
            path = <ls_stage>-file-path.
          ASSERT sy-subrc = 0.

          CLEAR <ls_exp>-sha1.           " Mark as deleted
          CLEAR <ls_updated>-sha1.       " Mark as deleted

        WHEN OTHERS.
          Lcx_abapgit_exception=>raise( 'stage method not supported, todo' ).
      ENDCASE.
    ENDLOOP.

    DELETE lt_expanded WHERE sha1 IS INITIAL.

    lt_trees = build_trees( lt_expanded ).

    receive_pack_push(
      EXPORTING
        is_comment     = is_comment
        it_trees       = lt_trees
        iv_branch_name = iv_branch_name
        iv_url         = iv_url
        iv_parent      = iv_parent
        iv_parent2     = io_stage->get_merge_source( )
        it_blobs       = lt_blobs
      IMPORTING
        ev_new_commit  = rs_result-branch
        et_new_objects = rs_result-new_objects
        ev_new_tree    = lv_new_tree ).

    APPEND LINES OF it_old_objects TO rs_result-new_objects.

    walk( EXPORTING it_objects = rs_result-new_objects
                    iv_sha1    = lv_new_tree
                    iv_path    = '/'
          CHANGING  ct_files   = rs_result-new_files ).

  ENDMETHOD.
  METHOD receive_pack_push.

    DATA: lv_time   TYPE Lcl_abapgit_git_time=>ty_unixtime,
          lv_commit TYPE xstring,
          lv_pack   TYPE xstring,
          ls_object LIKE LINE OF et_new_objects,
          ls_commit TYPE Lcl_abapgit_git_pack=>ty_commit,
          lv_uindex TYPE sy-index.

    FIELD-SYMBOLS: <ls_tree> LIKE LINE OF it_trees,
                   <ls_blob> LIKE LINE OF it_blobs.


    lv_time = Lcl_abapgit_git_time=>get_unix( ).

    READ TABLE it_trees ASSIGNING <ls_tree> WITH KEY path = '/'.
    ASSERT sy-subrc = 0.

* new commit
    ls_commit-committer = |{ is_comment-committer-name
      } <{ is_comment-committer-email }> { lv_time }|.
    IF is_comment-author-name IS NOT INITIAL.
      ls_commit-author = |{ is_comment-author-name
        } <{ is_comment-author-email }> { lv_time }|.
    ELSE.
      ls_commit-author = ls_commit-committer.
    ENDIF.

    ls_commit-tree      = <ls_tree>-sha1.
    ls_commit-parent    = iv_parent.
    ls_commit-parent2   = iv_parent2.
    ls_commit-body      = is_comment-comment.
    lv_commit = Lcl_abapgit_git_pack=>encode_commit( ls_commit ).

    ls_object-sha1 = Lcl_abapgit_hash=>sha1_commit( lv_commit ).
    ls_object-type = Lif_abapgit_git_definitions=>c_type-commit.
    ls_object-data = lv_commit.
    APPEND ls_object TO et_new_objects.

    LOOP AT it_trees ASSIGNING <ls_tree>.
      CLEAR ls_object.
      ls_object-sha1 = <ls_tree>-sha1.

      READ TABLE et_new_objects
        WITH KEY type COMPONENTS
          type = Lif_abapgit_git_definitions=>c_type-tree
          sha1 = ls_object-sha1
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
* two identical trees added at the same time, only add one to the pack
        CONTINUE.
      ENDIF.

      ls_object-type = Lif_abapgit_git_definitions=>c_type-tree.
      ls_object-data = <ls_tree>-data.
      lv_uindex = lv_uindex + 1.
      ls_object-index = lv_uindex.
      APPEND ls_object TO et_new_objects.
    ENDLOOP.

    LOOP AT it_blobs ASSIGNING <ls_blob>.
      CLEAR ls_object.
      ls_object-sha1 = Lcl_abapgit_hash=>sha1_blob( <ls_blob>-data ).

      READ TABLE et_new_objects
        WITH KEY type COMPONENTS
          type = Lif_abapgit_git_definitions=>c_type-blob
          sha1 = ls_object-sha1
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
* two identical files added at the same time, only add one blob to the pack
        CONTINUE.
      ENDIF.

      ls_object-type = Lif_abapgit_git_definitions=>c_type-blob.
* note <ls_blob>-data can be empty, #1857 allow empty files - some more checks needed?
      ls_object-data = <ls_blob>-data.
      lv_uindex = lv_uindex + 1.
      ls_object-index = lv_uindex.
      APPEND ls_object TO et_new_objects.
    ENDLOOP.

    lv_pack = Lcl_abapgit_git_pack=>encode( et_new_objects ).

    ev_new_commit = Lcl_abapgit_hash=>sha1_commit( lv_commit ).

    Lcl_abapgit_git_transport=>receive_pack(
      iv_url         = iv_url
      iv_old         = iv_parent
      iv_new         = ev_new_commit
      iv_branch_name = iv_branch_name
      iv_pack        = lv_pack ).

    ev_new_tree = ls_commit-tree.

  ENDMETHOD.
  METHOD walk.

    DATA: lv_path  TYPE string,
          ls_file  LIKE LINE OF ct_files,
          lt_nodes TYPE Lcl_abapgit_git_pack=>ty_nodes_tt.

    FIELD-SYMBOLS: <ls_tree> LIKE LINE OF it_objects,
                   <ls_blob> LIKE LINE OF it_objects,
                   <ls_node> LIKE LINE OF lt_nodes.


    READ TABLE it_objects ASSIGNING <ls_tree>
      WITH KEY type COMPONENTS
        type = Lif_abapgit_git_definitions=>c_type-tree
        sha1 = iv_sha1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Walk, tree not found' ).
    ENDIF.

    lt_nodes = Lcl_abapgit_git_pack=>decode_tree( <ls_tree>-data ).

    LOOP AT lt_nodes ASSIGNING <ls_node>.
      IF <ls_node>-chmod = Lif_abapgit_git_definitions=>c_chmod-file.
        READ TABLE it_objects ASSIGNING <ls_blob>
          WITH KEY type COMPONENTS
            type = Lif_abapgit_git_definitions=>c_type-blob
            sha1 = <ls_node>-sha1.
        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise( 'Walk, blob not found' ).
        ENDIF.

        CLEAR ls_file.
        ls_file-path     = iv_path.
        ls_file-filename = <ls_node>-name.
        ls_file-data     = <ls_blob>-data.
        ls_file-sha1     = <ls_blob>-sha1.
        APPEND ls_file TO ct_files.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_nodes ASSIGNING <ls_node> WHERE chmod = Lif_abapgit_git_definitions=>c_chmod-dir.
      CONCATENATE iv_path <ls_node>-name '/' INTO lv_path.

      walk( EXPORTING it_objects = it_objects
                      iv_sha1    = <ls_node>-sha1
                      iv_path    = lv_path
            CHANGING  ct_files   = ct_files ).
    ENDLOOP.

  ENDMETHOD.
  METHOD walk_tree.

    DATA: ls_object   LIKE LINE OF it_objects,
          lt_expanded LIKE rt_expanded,
          lt_nodes    TYPE Lcl_abapgit_git_pack=>ty_nodes_tt.

    FIELD-SYMBOLS: <ls_exp>  LIKE LINE OF rt_expanded,
                   <ls_node> LIKE LINE OF lt_nodes.


    READ TABLE it_objects INTO ls_object
      WITH KEY type COMPONENTS
        type = Lif_abapgit_git_definitions=>c_type-tree
        sha1 = iv_tree.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'tree not found' ).
    ENDIF.
    lt_nodes = Lcl_abapgit_git_pack=>decode_tree( ls_object-data ).

    LOOP AT lt_nodes ASSIGNING <ls_node>.
      CASE <ls_node>-chmod.
        WHEN Lif_abapgit_git_definitions=>c_chmod-file
            OR Lif_abapgit_git_definitions=>c_chmod-executable
            OR Lif_abapgit_git_definitions=>c_chmod-submodule.
          APPEND INITIAL LINE TO rt_expanded ASSIGNING <ls_exp>.
          <ls_exp>-path  = iv_base.
          <ls_exp>-name  = <ls_node>-name.
          <ls_exp>-sha1  = <ls_node>-sha1.
          <ls_exp>-chmod = <ls_node>-chmod.
        WHEN Lif_abapgit_git_definitions=>c_chmod-dir.
          lt_expanded = walk_tree(
            it_objects = it_objects
            iv_tree    = <ls_node>-sha1
            iv_base    = iv_base && <ls_node>-name && '/' ).
          APPEND LINES OF lt_expanded TO rt_expanded.
        WHEN OTHERS.
          Lcx_abapgit_exception=>raise( 'walk_tree: unknown chmod' ).
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GIT_PORCELAIN implementation

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
endclass. "ZCL_ABAPGIT_GUI implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_AQBG <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_aqbg=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_aqbg=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_AQBG implementation.
*"* method's implementations
*include methods.
  METHOD get_field_rules.

    ro_result = Lcl_abapgit_field_rules=>create( ).

    ro_result->add(
      iv_table     = 'AQGDBBG'
      iv_field     = 'BGCNAM'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = 'AQGDBBG'
      iv_field     = 'BGUNAM'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user ).

    ro_result->add(
      iv_table     = 'AQGDBBG'
      iv_field     = 'BGCDAT'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-date
    )->add(
      iv_table     = 'AQGDBBG'
      iv_field     = 'BGUDAT'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-date ).

    ro_result->add(
      iv_table     = 'AQGDBBG'
      iv_field     = 'DEVC'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-package ).

  ENDMETHOD.
  METHOD get_generic.
    " transaction SQ03
    CREATE OBJECT ro_generic
      EXPORTING
        is_item        = ms_item
        io_field_rules = get_field_rules( )
        iv_language    = mv_language.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    SELECT SINGLE bgunam FROM aqgdbbg INTO rv_user WHERE num = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
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
    <ls_bdcdata>-program  = 'SAPMS38S'.
    <ls_bdcdata>-dynpro   = '0050'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'RS38S-BGNUM'.
    <ls_bdcdata>-fval = ms_item-obj_name.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode      = 'SQ03'
      it_bdcdata    = lt_bdcdata ).

    rv_exit = abap_true.

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
endclass. "ZCL_ABAPGIT_OBJECT_AQBG implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_AQQU <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_aqqu=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_aqqu=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_AQQU implementation.
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
    " transaction SQ01
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
    <ls_bdcdata>-program  = 'SAPMS38R'.
    <ls_bdcdata>-dynpro   = '0050'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'RS38R-QNUM'.
    <ls_bdcdata>-fval = ms_item-obj_name.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode      = 'SQ01'
      it_bdcdata    = lt_bdcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.
    get_generic( )->serialize( io_xml ).
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_AQQU implementation

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

*>>>>>>> ZCL_ABAPGIT_AJSON <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ajson=============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ajson=============ccimp.
**********************************************************************
* UTILS
**********************************************************************

INTERFACE SHRITEFUH64VYIPN5I4UHL45BCSR4A.

  TYPES ty_kind TYPE c LENGTH 1.

  CONSTANTS:
    any         TYPE ty_kind VALUE cl_abap_typedescr=>typekind_any,
    date        TYPE ty_kind VALUE cl_abap_typedescr=>typekind_date,
    time        TYPE ty_kind VALUE cl_abap_typedescr=>typekind_time,
    packed      TYPE ty_kind VALUE cl_abap_typedescr=>typekind_packed,
    table       TYPE ty_kind VALUE cl_abap_typedescr=>typekind_table,
    struct_flat TYPE ty_kind VALUE cl_abap_typedescr=>typekind_struct1,
    struct_deep TYPE ty_kind VALUE cl_abap_typedescr=>typekind_struct2,
    data_ref    TYPE ty_kind VALUE cl_abap_typedescr=>typekind_dref,
    object_ref  TYPE ty_kind VALUE cl_abap_typedescr=>typekind_oref,
    enum        TYPE ty_kind VALUE 'k'. " cl_abap_typedescr=>typekind_enum not in lower releases

  CONSTANTS:
    BEGIN OF numeric,
      int1       TYPE ty_kind VALUE cl_abap_tabledescr=>typekind_int1,
      int2       TYPE ty_kind VALUE cl_abap_tabledescr=>typekind_int2,
      int4       TYPE ty_kind VALUE cl_abap_tabledescr=>typekind_int,
      int8       TYPE ty_kind VALUE '8', " cl_abap_tabledescr=>typekind_int8 not in lower releases
      float      TYPE ty_kind VALUE cl_abap_tabledescr=>typekind_float,
      packed     TYPE ty_kind VALUE cl_abap_tabledescr=>typekind_packed,
      decfloat16 TYPE ty_kind VALUE cl_abap_tabledescr=>typekind_decfloat16,
      decfloat34 TYPE ty_kind VALUE cl_abap_tabledescr=>typekind_decfloat34,
    END OF numeric.

  CONSTANTS:
    BEGIN OF texts,
      char   TYPE ty_kind VALUE cl_abap_tabledescr=>typekind_char,
      numc   TYPE ty_kind VALUE cl_abap_tabledescr=>typekind_num,
      string TYPE ty_kind VALUE cl_abap_tabledescr=>typekind_string,
    END OF texts.

  CONSTANTS:
    BEGIN OF binary,
      hex     TYPE ty_kind VALUE cl_abap_tabledescr=>typekind_hex,
      xstring TYPE ty_kind VALUE cl_abap_tabledescr=>typekind_xstring,
    END OF binary.

  CONSTANTS:
    BEGIN OF deep_targets,
      table       TYPE ty_kind VALUE cl_abap_typedescr=>typekind_table,
      struct_flat TYPE ty_kind VALUE cl_abap_typedescr=>typekind_struct1,
      struct_deep TYPE ty_kind VALUE cl_abap_typedescr=>typekind_struct2,
      data_ref    TYPE ty_kind VALUE cl_abap_typedescr=>typekind_dref,
      object_ref  TYPE ty_kind VALUE cl_abap_typedescr=>typekind_oref,
    END OF deep_targets.

ENDINTERFACE.

CLASS SHRITEFUH64VYIPN5I4UHL45BCTR4A DEFINITION FINAL.
  PUBLIC SECTION.

    CLASS-METHODS normalize_path
      IMPORTING
        iv_path TYPE string
      RETURNING
        VALUE(rv_path) TYPE string.
    CLASS-METHODS split_path
      IMPORTING
        iv_path TYPE string
      RETURNING
        VALUE(rv_path_name) TYPE Lif_abapgit_ajson_types=>ty_path_name.
    CLASS-METHODS validate_array_index
      IMPORTING
        iv_path TYPE string
        iv_index TYPE string
      RETURNING
        VALUE(rv_index) TYPE i
      RAISING
        Lcx_abapgit_ajson_error.
    CLASS-METHODS string_to_xstring_utf8
      IMPORTING
        iv_str TYPE string
      RETURNING
        VALUE(rv_xstr) TYPE xstring.

ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BCTR4A IMPLEMENTATION.

  METHOD string_to_xstring_utf8.

    DATA lo_conv TYPE REF TO object.
    DATA lv_out_ce TYPE string.

    lv_out_ce = 'CL_ABAP_CONV_OUT_CE'.

    TRY.
        CALL METHOD ('CL_ABAP_CONV_CODEPAGE')=>create_out
        RECEIVING
          instance = lo_conv.
        CALL METHOD lo_conv->('IF_ABAP_CONV_OUT~CONVERT')
        EXPORTING
          source = iv_str
        RECEIVING
          result = rv_xstr.
      CATCH cx_sy_dyn_call_illegal_class.
        CALL METHOD (lv_out_ce)=>create
        EXPORTING
          encoding = 'UTF-8'
        RECEIVING
          conv = lo_conv.
        CALL METHOD lo_conv->('CONVERT')
        EXPORTING
          data = iv_str
        IMPORTING
          buffer = rv_xstr.
    ENDTRY.

  ENDMETHOD.

  METHOD validate_array_index.

    IF NOT iv_index CO '0123456789'.
      Lcx_abapgit_ajson_error=>raise( |Cannot add non-numeric key [{ iv_index }] to array [{ iv_path }]| ).
    ENDIF.
    rv_index = iv_index.
    IF rv_index = 0.
      Lcx_abapgit_ajson_error=>raise( |Cannot add zero key to array [{ iv_path }]| ).
    ENDIF.

  ENDMETHOD.

  METHOD normalize_path.

    rv_path = iv_path.
    IF strlen( rv_path ) = 0.
      rv_path = '/'.
    ENDIF.
    IF rv_path+0(1) <> '/'.
      rv_path = '/' && rv_path.
    ENDIF.
    IF substring( val = rv_path
                  off = strlen( rv_path ) - 1 ) <> '/'.
      rv_path = rv_path && '/'.
    ENDIF.

  ENDMETHOD.

  METHOD split_path.

    DATA lv_offs TYPE i.
    DATA lv_len TYPE i.
    DATA lv_trim_slash TYPE i.

    lv_len = strlen( iv_path ).
    IF lv_len = 0 OR iv_path = '/'.
      RETURN. " empty path is the alias for root item = '' + ''
    ENDIF.

    IF substring( val = iv_path
                  off = lv_len - 1 ) = '/'.
      lv_trim_slash = 1. " ignore last '/'
    ENDIF.

    lv_offs = find( val = reverse( iv_path )
                    sub = '/'
                    off = lv_trim_slash ).
    IF lv_offs = -1.
      lv_offs  = lv_len. " treat whole string as the 'name' part
    ENDIF.
    lv_offs = lv_len - lv_offs.

    rv_path_name-path = normalize_path( substring( val = iv_path
                                                   len = lv_offs ) ).
    rv_path_name-name = substring( val = iv_path
                                   off = lv_offs
                                   len = lv_len - lv_offs - lv_trim_slash ).

  ENDMETHOD.

ENDCLASS.


**********************************************************************
* PARSER
**********************************************************************

CLASS SHRITEFUH64VYIPN5I4UHL45BCVR4A DEFINITION FINAL.
  PUBLIC SECTION.

    METHODS parse
      IMPORTING
        iv_json TYPE string
        iv_keep_item_order TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_json_tree) TYPE Lif_abapgit_ajson_types=>ty_nodes_tt
      RAISING
        Lcx_abapgit_ajson_error.

  PRIVATE SECTION.

    TYPES:
      ty_stack_tt TYPE STANDARD TABLE OF REF TO Lif_abapgit_ajson_types=>ty_node.

    DATA mt_stack TYPE ty_stack_tt.
    DATA mv_stack_path TYPE string.
    DATA mv_keep_item_order TYPE abap_bool.

    METHODS raise
      IMPORTING
        iv_error TYPE string
      RAISING
        Lcx_abapgit_ajson_error.

    METHODS _parse
      IMPORTING
        iv_json TYPE string
      RETURNING
        VALUE(rt_json_tree) TYPE Lif_abapgit_ajson_types=>ty_nodes_tt
      RAISING
        Lcx_abapgit_ajson_error cx_dynamic_check. " cx_sxml_error is not released on Steampunk #153

    METHODS _get_location
      IMPORTING
        iv_json            TYPE string
        iv_offset          TYPE i
      RETURNING
        VALUE(rv_location) TYPE string.

ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BCVR4A IMPLEMENTATION.

  METHOD parse.
    DATA lx_sxml_parse TYPE REF TO cx_sxml_parse_error.
    DATA lx_sxml TYPE REF TO cx_dynamic_check.
    DATA lv_location TYPE string.

    mv_keep_item_order = iv_keep_item_order.

    TRY.
      " TODO sane JSON check:
      " JSON can be true,false,null,(-)digits
      " or start from " or from {
        rt_json_tree = _parse( iv_json ).
      CATCH cx_sxml_parse_error INTO lx_sxml_parse.
        lv_location = _get_location(
        iv_json   = iv_json
        iv_offset = lx_sxml_parse->xml_offset ).
        Lcx_abapgit_ajson_error=>raise(
        iv_msg      = |Json parsing error (SXML): { lx_sxml_parse->get_text( ) }|
        iv_location = lv_location ).
      CATCH cx_dynamic_check INTO lx_sxml. " cx_sxml_error
        Lcx_abapgit_ajson_error=>raise(
        iv_msg      = |Json parsing error (SXML): { lx_sxml->get_text( ) }|
        iv_location = '@PARSER' ).
    ENDTRY.

  ENDMETHOD.

  METHOD _get_location.

    DATA lv_json TYPE string.
    DATA lv_offset TYPE i.
    DATA lt_text TYPE TABLE OF string.
    DATA lv_text TYPE string.
    DATA lv_line TYPE i.
    DATA lv_pos TYPE i.

    lv_offset = iv_offset.
    IF lv_offset < 0.
      lv_offset = 0.
    ENDIF.
    IF lv_offset > strlen( iv_json ).
      lv_offset = strlen( iv_json ).
    ENDIF.

    lv_json = iv_json(lv_offset).

    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
      IN lv_json WITH cl_abap_char_utilities=>newline.

    SPLIT lv_json AT cl_abap_char_utilities=>newline INTO TABLE lt_text.

    lv_line = lines( lt_text ).
    IF lv_line = 0.
      lv_line = 1.
      lv_pos = 1.
    ELSE.
      READ TABLE lt_text INDEX lv_line INTO lv_text.
      lv_pos = strlen( lv_text ) + 1.
    ENDIF.

    rv_location = |Line { lv_line }, Offset { lv_pos }|.

  ENDMETHOD.

  METHOD _parse.

    DATA lo_reader TYPE REF TO if_sxml_reader.
    DATA lr_stack_top LIKE LINE OF mt_stack.
    DATA lo_node TYPE REF TO if_sxml_node.
    FIELD-SYMBOLS <item> LIKE LINE OF rt_json_tree.

    CLEAR mt_stack.
    CLEAR mv_stack_path.
    IF iv_json IS INITIAL.
      RETURN.
    ENDIF.
    lo_reader = cl_sxml_string_reader=>create( SHRITEFUH64VYIPN5I4UHL45BCTR4A=>string_to_xstring_utf8( iv_json ) ).

    " TODO: self protection, check non-empty, check starting from object ...

    DO.
      lo_node = lo_reader->read_next_node( ).
      IF lo_node IS NOT BOUND.
        EXIT.
      ENDIF.


      CASE lo_node->type.
        WHEN if_sxml_node=>co_nt_element_open.
          DATA lt_attributes TYPE if_sxml_attribute=>attributes.
          DATA lo_attr LIKE LINE OF lt_attributes.
          DATA lo_open TYPE REF TO if_sxml_open_element.
          lo_open ?= lo_node.

          APPEND INITIAL LINE TO rt_json_tree ASSIGNING <item>.

          <item>-type = lo_open->qname-name.

          READ TABLE mt_stack INDEX 1 INTO lr_stack_top.
          IF sy-subrc = 0.
            " Using string is faster than rebuilding path from stack
            <item>-path = mv_stack_path.
            lr_stack_top->children = lr_stack_top->children + 1.

            IF lr_stack_top->type = `array`. " This is parser type not ajson type
              <item>-name = |{ lr_stack_top->children }|.
              <item>-index = lr_stack_top->children.
            ELSE.
              lt_attributes = lo_open->get_attributes( ).
              LOOP AT lt_attributes INTO lo_attr.
                IF lo_attr->qname-name = 'name' AND lo_attr->value_type = if_sxml_value=>co_vt_text.
                  <item>-name = lo_attr->get_value( ).
                ENDIF.
              ENDLOOP.
              IF mv_keep_item_order = abap_true.
                <item>-order = lr_stack_top->children.
              ENDIF.
            ENDIF.
            IF <item>-name IS INITIAL.
              raise( 'Node without name (maybe not JSON)' ).
            ENDIF.
          ENDIF.

          GET REFERENCE OF <item> INTO lr_stack_top.
          INSERT lr_stack_top INTO mt_stack INDEX 1.
          " add path component
          mv_stack_path = mv_stack_path && <item>-name && '/'.

        WHEN if_sxml_node=>co_nt_element_close.
          DATA lo_close TYPE REF TO if_sxml_close_element.
          lo_close ?= lo_node.

          READ TABLE mt_stack INDEX 1 INTO lr_stack_top.
          DELETE mt_stack INDEX 1.
          IF lo_close->qname-name <> lr_stack_top->type.
            raise( 'Unexpected closing node type' ).
          ENDIF.

          " remove last path component
          mv_stack_path = substring( val = mv_stack_path
                                     len = find( val = mv_stack_path sub = '/' occ = -2 ) + 1 ).
        WHEN if_sxml_node=>co_nt_value.
          DATA lo_value TYPE REF TO if_sxml_value_node.
          lo_value ?= lo_node.

          <item>-value = lo_value->get_value( ).

        WHEN OTHERS.
          raise( 'Unexpected node type' ).
      ENDCASE.
    ENDDO.

    IF lines( mt_stack ) > 0.
      raise( 'Unexpected end of data' ).
    ENDIF.

  ENDMETHOD.

  METHOD raise.

    Lcx_abapgit_ajson_error=>raise(
      iv_location = mv_stack_path
      iv_msg      = |JSON PARSER: { iv_error } @ { mv_stack_path }| ).

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* SERIALIZER
**********************************************************************

CLASS SHRITEFUH64VYIPN5I4UHL45BCXR4A DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.

    CLASS-METHODS stringify
      IMPORTING
        it_json_tree TYPE Lif_abapgit_ajson_types=>ty_nodes_ts
        iv_indent TYPE i DEFAULT 0
        iv_keep_item_order TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_json_string) TYPE string
      RAISING
        Lcx_abapgit_ajson_error.

    CLASS-METHODS class_constructor.

  PRIVATE SECTION.

    CLASS-DATA gv_comma_with_lf TYPE string.

    DATA mt_json_tree TYPE Lif_abapgit_ajson_types=>ty_nodes_ts.
    DATA mv_keep_item_order TYPE abap_bool.
    DATA mt_buffer TYPE string_table.
    DATA mv_indent_step TYPE i.
    DATA mv_level TYPE i.

    CLASS-METHODS escape_string
      IMPORTING
        iv_unescaped TYPE string
      RETURNING
        VALUE(rv_escaped) TYPE string.

    METHODS _stringify
      RETURNING
        VALUE(rv_json_string) TYPE string
      RAISING
        Lcx_abapgit_ajson_error.

    METHODS stringify_node
      IMPORTING
        is_node TYPE Lif_abapgit_ajson_types=>ty_node
      RAISING
        Lcx_abapgit_ajson_error.

    METHODS stringify_set
      IMPORTING
        iv_parent_path TYPE string
        iv_array TYPE abap_bool
      RAISING
        Lcx_abapgit_ajson_error.

ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BCXR4A IMPLEMENTATION.

  METHOD class_constructor.
    gv_comma_with_lf = ',' && cl_abap_char_utilities=>newline.
  ENDMETHOD.

  METHOD stringify.

    DATA lo TYPE REF TO SHRITEFUH64VYIPN5I4UHL45BCXR4A.
    CREATE OBJECT lo.
    lo->mt_json_tree = it_json_tree.
    lo->mv_indent_step = iv_indent.
    lo->mv_keep_item_order = iv_keep_item_order.
    rv_json_string = lo->_stringify( ).

  ENDMETHOD.

  METHOD _stringify.

    FIELD-SYMBOLS <n> LIKE LINE OF mt_json_tree.
    READ TABLE mt_json_tree ASSIGNING <n>
      WITH KEY
        path = ''
        name = ''. " Root
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    stringify_node( <n> ).

    rv_json_string = concat_lines_of( table = mt_buffer ).

  ENDMETHOD.

  METHOD stringify_node.

    DATA lv_item TYPE string.
    DATA lv_indent_prefix TYPE string.

    IF mv_indent_step > 0.
      lv_indent_prefix = repeat( val = ` `
                                 occ = mv_indent_step * mv_level ).
      lv_item = lv_indent_prefix.
    ENDIF.

    IF is_node-name IS NOT INITIAL AND is_node-index IS INITIAL. " Not root, not array item
      IF mv_indent_step > 0.
        lv_item = lv_item && |"{ is_node-name }": |.
      ELSE.
        lv_item = |"{ is_node-name }":|.
      ENDIF.
    ENDIF.

    CASE is_node-type.
      WHEN Lif_abapgit_ajson_types=>node_type-array.
        lv_item = lv_item && '['.
      WHEN Lif_abapgit_ajson_types=>node_type-object.
        lv_item = lv_item && '{'.
      WHEN Lif_abapgit_ajson_types=>node_type-string.
        lv_item = lv_item && |"{ escape_string( is_node-value ) }"|.
      WHEN Lif_abapgit_ajson_types=>node_type-boolean OR Lif_abapgit_ajson_types=>node_type-number.
        lv_item = lv_item && is_node-value.
      WHEN Lif_abapgit_ajson_types=>node_type-null.
        lv_item = lv_item && 'null'.
      WHEN OTHERS.
        Lcx_abapgit_ajson_error=>raise(
          iv_msg = |Unexpected type [{ is_node-type }]|
          iv_location = is_node-path && is_node-name ).
    ENDCASE.

    IF mv_indent_step > 0
      AND ( is_node-type = Lif_abapgit_ajson_types=>node_type-array OR is_node-type = Lif_abapgit_ajson_types=>node_type-object )
      AND is_node-children > 0.
      mv_level = mv_level + 1.
      lv_item = lv_item && cl_abap_char_utilities=>newline.
    ENDIF.

    APPEND lv_item TO mt_buffer.

    " finish complex item

    IF is_node-type = Lif_abapgit_ajson_types=>node_type-array OR is_node-type = Lif_abapgit_ajson_types=>node_type-object.
      DATA lv_children_path TYPE string.
      DATA lv_tail TYPE string.

      lv_children_path = is_node-path && is_node-name && '/'. " for root: path = '' and name = '', so result is '/'

      CASE is_node-type.
        WHEN Lif_abapgit_ajson_types=>node_type-array.
          IF is_node-children > 0.
            stringify_set(
              iv_parent_path = lv_children_path
              iv_array       = abap_true ).
          ENDIF.
          lv_tail = ']'.
        WHEN Lif_abapgit_ajson_types=>node_type-object.
          IF is_node-children > 0.
            stringify_set(
              iv_parent_path = lv_children_path
              iv_array       = abap_false ).
          ENDIF.
          lv_tail = '}'.
      ENDCASE.

      IF mv_indent_step > 0 AND is_node-children > 0.
        lv_tail = lv_indent_prefix && lv_tail.
        mv_level = mv_level - 1.
      ENDIF.
      APPEND lv_tail TO mt_buffer.
    ENDIF.

  ENDMETHOD.

  METHOD stringify_set.

    DATA lv_tab_key TYPE string.
    DATA lv_first_done TYPE abap_bool.
    FIELD-SYMBOLS <n> LIKE LINE OF mt_json_tree.

    IF iv_array = abap_true.
      lv_tab_key = 'array_index'. " path + index
    ELSEIF mv_keep_item_order = abap_true.
      lv_tab_key = 'item_order'. " path + order
    ELSE.
      lv_tab_key = 'primary_key'. " path + name
    ENDIF.

    LOOP AT mt_json_tree ASSIGNING <n> USING KEY (lv_tab_key) WHERE path = iv_parent_path.
      IF lv_first_done = abap_false.
        lv_first_done = abap_true.
      ELSEIF mv_indent_step > 0.
        APPEND gv_comma_with_lf TO mt_buffer.
      ELSE.
        APPEND ',' TO mt_buffer.
      ENDIF.
      stringify_node( <n> ).
    ENDLOOP.

    IF mv_indent_step > 0 AND lv_first_done = abap_true. " only of items were in the list
      APPEND cl_abap_char_utilities=>newline TO mt_buffer.
    ENDIF.

  ENDMETHOD.

  METHOD escape_string.

    rv_escaped = iv_unescaped.
    IF rv_escaped CA |"\\\t\n\r|.
      " TODO consider performance ...
      " see also https://www.json.org/json-en.html
      rv_escaped = replace(
        val = rv_escaped
        sub = '\'
        with = '\\'
        occ = 0 ).
      rv_escaped = replace(
        val = rv_escaped
        sub = |\n|
        with = '\n'
        occ = 0 ).
      rv_escaped = replace(
        val = rv_escaped
        sub = |\r|
        with = '\r'
        occ = 0 ).
      rv_escaped = replace(
        val = rv_escaped
        sub = |\t|
        with = '\t'
        occ = 0 ).
      rv_escaped = replace(
        val = rv_escaped
        sub = '"'
        with = '\"'
        occ = 0 ).

    ENDIF.

  ENDMETHOD.

ENDCLASS.


**********************************************************************
* JSON_TO_ABAP
**********************************************************************

CLASS SHRITEFUH64VYIPN5I4UHL45BCZR4A DEFINITION FINAL.
  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_corresponding  TYPE abap_bool DEFAULT abap_false
        !ii_custom_mapping TYPE REF TO Lif_abapgit_ajson_mapping OPTIONAL.

    METHODS to_abap
      IMPORTING
        it_nodes     TYPE Lif_abapgit_ajson_types=>ty_nodes_ts
      CHANGING
        c_container TYPE any
      RAISING
        Lcx_abapgit_ajson_error.

    METHODS to_timestamp
      IMPORTING
        iv_value         TYPE Lif_abapgit_ajson_types=>ty_node-value
      RETURNING
        VALUE(rv_result) TYPE timestamp
      RAISING
        Lcx_abapgit_ajson_error.

    METHODS to_date
      IMPORTING
        iv_value         TYPE Lif_abapgit_ajson_types=>ty_node-value
      RETURNING
        VALUE(rv_result) TYPE d
      RAISING
        Lcx_abapgit_ajson_error.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_type_cache,
        type_path         TYPE string,
        target_field_name TYPE string,
        dd                TYPE REF TO cl_abap_datadescr,
        type_kind         LIKE SHRITEFUH64VYIPN5I4UHL45BCSR4A=>any,
        tab_item_buf      TYPE REF TO data,
      END OF ty_type_cache.
    DATA mt_node_type_cache TYPE HASHED TABLE OF ty_type_cache WITH UNIQUE KEY type_path.

    DATA mr_nodes TYPE REF TO Lif_abapgit_ajson_types=>ty_nodes_ts.
    DATA mi_custom_mapping TYPE REF TO Lif_abapgit_ajson_mapping.
    DATA mv_corresponding TYPE abap_bool.

    METHODS any_to_abap
      IMPORTING
        iv_path        TYPE string
        is_parent_type TYPE ty_type_cache OPTIONAL
        i_container_ref TYPE REF TO data
      RAISING
        Lcx_abapgit_ajson_error.

    METHODS value_to_abap
      IMPORTING
        is_node      TYPE Lif_abapgit_ajson_types=>ty_node
        is_node_type TYPE ty_type_cache
        i_container_ref TYPE REF TO data
      RAISING
        Lcx_abapgit_ajson_error
        cx_sy_conversion_no_number.

    METHODS get_node_type
      IMPORTING
        is_node            TYPE Lif_abapgit_ajson_types=>ty_node OPTIONAL " Empty for root
        is_parent_type     TYPE ty_type_cache OPTIONAL
        i_container_ref    TYPE REF TO data OPTIONAL
      RETURNING
        VALUE(rs_node_type) TYPE ty_type_cache
      RAISING
        Lcx_abapgit_ajson_error.

ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BCZR4A IMPLEMENTATION.

  METHOD constructor.
    mi_custom_mapping = ii_custom_mapping.
    mv_corresponding  = iv_corresponding.
  ENDMETHOD.

  METHOD to_abap.

    DATA lr_ref TYPE REF TO data.

    CLEAR c_container. " what about data/obj refs ?
    CLEAR mt_node_type_cache.

    GET REFERENCE OF c_container INTO lr_ref.
    GET REFERENCE OF it_nodes INTO mr_nodes.

    get_node_type( i_container_ref = lr_ref ). " Pre-cache root node type

    any_to_abap(
      iv_path         = ''
      i_container_ref = lr_ref ).

  ENDMETHOD.

  METHOD get_node_type.

    DATA lv_node_type_path TYPE string.
    DATA lo_sdescr TYPE REF TO cl_abap_structdescr.
    DATA lo_tdescr TYPE REF TO cl_abap_tabledescr.
    DATA lo_ddescr TYPE REF TO cl_abap_datadescr.

    " Calculate type path
    IF is_parent_type-type_kind = SHRITEFUH64VYIPN5I4UHL45BCSR4A=>table.
      lv_node_type_path = is_parent_type-type_path && '/-'. " table item type
    ELSEIF is_parent_type-type_kind IS NOT INITIAL.
      lv_node_type_path = is_parent_type-type_path && '/' && is_node-name.
    ENDIF. " For root node lv_node_type_path remains ''

    " Get or create cached
    READ TABLE mt_node_type_cache INTO rs_node_type WITH KEY type_path = lv_node_type_path.
    IF sy-subrc <> 0.

      rs_node_type-type_path         = lv_node_type_path.

      IF mi_custom_mapping IS BOUND.
        rs_node_type-target_field_name = to_upper( mi_custom_mapping->to_abap(
          iv_path = is_node-path
          iv_name = is_node-name ) ).
        IF rs_node_type-target_field_name IS INITIAL.
          rs_node_type-target_field_name = to_upper( is_node-name ).
        ENDIF.
      ELSE.
        rs_node_type-target_field_name = to_upper( is_node-name ).
      ENDIF.

      CASE is_parent_type-type_kind.
        WHEN SHRITEFUH64VYIPN5I4UHL45BCSR4A=>table.
          lo_tdescr ?= is_parent_type-dd.
          rs_node_type-dd = lo_tdescr->get_table_line_type( ).

        WHEN SHRITEFUH64VYIPN5I4UHL45BCSR4A=>struct_flat OR SHRITEFUH64VYIPN5I4UHL45BCSR4A=>struct_deep.
          lo_sdescr ?= is_parent_type-dd.
          lo_sdescr->get_component_type(
            EXPORTING
              p_name      = rs_node_type-target_field_name
            RECEIVING
              p_descr_ref = rs_node_type-dd
            EXCEPTIONS
              component_not_found = 4 ).
          IF sy-subrc <> 0.
            IF mv_corresponding = abap_false.
              Lcx_abapgit_ajson_error=>raise( |Path not found| ).
            ELSE.
              CLEAR rs_node_type.
              RETURN.
            ENDIF.
          ENDIF.

        WHEN ''. " Root node
          rs_node_type-dd ?= cl_abap_typedescr=>describe_by_data_ref( i_container_ref ).

        WHEN OTHERS.
          Lcx_abapgit_ajson_error=>raise( |Unexpected parent type| ).
      ENDCASE.

      rs_node_type-type_kind         = rs_node_type-dd->type_kind. " for caching and cleaner unintialized access
      IF rs_node_type-type_kind = SHRITEFUH64VYIPN5I4UHL45BCSR4A=>table.
        lo_tdescr ?= rs_node_type-dd.
        IF lo_tdescr->table_kind <> cl_abap_tabledescr=>tablekind_std.
          lo_ddescr = lo_tdescr->get_table_line_type( ).
          CREATE DATA rs_node_type-tab_item_buf TYPE HANDLE lo_ddescr.
        ENDIF.
      ENDIF.

      INSERT rs_node_type INTO TABLE mt_node_type_cache.
    ENDIF.

  ENDMETHOD.

  METHOD any_to_abap.

    DATA ls_node_type LIKE LINE OF mt_node_type_cache.
    DATA lx_ajson TYPE REF TO Lcx_abapgit_ajson_error.
    DATA lx_root TYPE REF TO cx_root.
    DATA lr_target_field TYPE REF TO data.

    FIELD-SYMBOLS <n> TYPE Lif_abapgit_ajson_types=>ty_node.
    FIELD-SYMBOLS <parent_stdtab> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <parent_anytab> TYPE ANY TABLE.
    FIELD-SYMBOLS <parent_struc> TYPE any.
    FIELD-SYMBOLS <tab_item> TYPE any.

    " Assign container
    CASE is_parent_type-type_kind.
      WHEN SHRITEFUH64VYIPN5I4UHL45BCSR4A=>table.
        IF is_parent_type-tab_item_buf IS BOUND. " Indirect hint that table was sorted/hashed, see get_node_type.
          ASSIGN i_container_ref->* TO <parent_anytab>.
          ASSERT sy-subrc = 0.

          lr_target_field = is_parent_type-tab_item_buf. " For hashed/sorted table - same buffer for all children
          ASSIGN is_parent_type-tab_item_buf->* TO <tab_item>.
          ASSERT sy-subrc = 0.

        ELSE.
          ASSIGN i_container_ref->* TO <parent_stdtab>.
          ASSERT sy-subrc = 0.
        ENDIF.

      WHEN SHRITEFUH64VYIPN5I4UHL45BCSR4A=>struct_flat OR SHRITEFUH64VYIPN5I4UHL45BCSR4A=>struct_deep.
        ASSIGN i_container_ref->* TO <parent_struc>.
        ASSERT sy-subrc = 0.
    ENDCASE.

    TRY.

      " array_index because stringified index goes in wrong order [1, 10, 2 ...]
        LOOP AT mr_nodes->* ASSIGNING <n> USING KEY array_index WHERE path = iv_path.

        " Get or create type cache record
          IF is_parent_type-type_kind <> SHRITEFUH64VYIPN5I4UHL45BCSR4A=>table OR ls_node_type-type_kind IS INITIAL.
          " table records are the same, no need to refetch twice

            ls_node_type = get_node_type(
            is_node        = <n>
            is_parent_type = is_parent_type ).

            IF mv_corresponding = abap_true AND ls_node_type IS INITIAL.
              CONTINUE.
            ENDIF.

          ENDIF.

        " Validate node type
          IF ls_node_type-type_kind = SHRITEFUH64VYIPN5I4UHL45BCSR4A=>data_ref OR
           ls_node_type-type_kind = SHRITEFUH64VYIPN5I4UHL45BCSR4A=>object_ref.
          " TODO maybe in future
            Lcx_abapgit_ajson_error=>raise( 'Cannot assign to ref' ).
          ENDIF.

        " Find target field reference
          CASE is_parent_type-type_kind.
            WHEN SHRITEFUH64VYIPN5I4UHL45BCSR4A=>table.
              IF NOT ls_node_type-target_field_name CO '0123456789'.
              " Does not affect anything actually but for integrity
                Lcx_abapgit_ajson_error=>raise( 'Need index to access tables' ).
              ENDIF.

              IF is_parent_type-tab_item_buf IS NOT BOUND. " Indirect hint that table was srt/hsh, see get_node_type
                APPEND INITIAL LINE TO <parent_stdtab> REFERENCE INTO lr_target_field.
                ASSERT sy-subrc = 0.
              ENDIF.

            WHEN SHRITEFUH64VYIPN5I4UHL45BCSR4A=>struct_flat OR SHRITEFUH64VYIPN5I4UHL45BCSR4A=>struct_deep.
              FIELD-SYMBOLS <field> TYPE any.
              ASSIGN COMPONENT ls_node_type-target_field_name OF STRUCTURE <parent_struc> TO <field>.
              ASSERT sy-subrc = 0.
              GET REFERENCE OF <field> INTO lr_target_field.

            WHEN ''. " Root node
              lr_target_field = i_container_ref.

            WHEN OTHERS.
              Lcx_abapgit_ajson_error=>raise( 'Unexpected parent type' ).
          ENDCASE.

        " Process value assignment
          CASE <n>-type.
            WHEN Lif_abapgit_ajson_types=>node_type-object.
              IF ls_node_type-type_kind <> SHRITEFUH64VYIPN5I4UHL45BCSR4A=>struct_flat AND
               ls_node_type-type_kind <> SHRITEFUH64VYIPN5I4UHL45BCSR4A=>struct_deep.
                Lcx_abapgit_ajson_error=>raise( 'Expected structure' ).
              ENDIF.
              any_to_abap(
              iv_path         = <n>-path && <n>-name && '/'
              is_parent_type  = ls_node_type
              i_container_ref = lr_target_field ).

            WHEN Lif_abapgit_ajson_types=>node_type-array.
              IF NOT ls_node_type-type_kind = SHRITEFUH64VYIPN5I4UHL45BCSR4A=>table.
                Lcx_abapgit_ajson_error=>raise( 'Expected table' ).
              ENDIF.
              any_to_abap(
              iv_path         = <n>-path && <n>-name && '/'
              is_parent_type  = ls_node_type
              i_container_ref = lr_target_field ).

            WHEN OTHERS.
              value_to_abap(
              is_node         = <n>
              is_node_type    = ls_node_type
              i_container_ref = lr_target_field ).
          ENDCASE.

          IF is_parent_type-tab_item_buf IS BOUND. " Indirect hint that table was sorted/hashed, see get_node_type.
            TRY.
                INSERT <tab_item> INTO TABLE <parent_anytab>.
                IF sy-subrc <> 0.
                  Lcx_abapgit_ajson_error=>raise( 'Duplicate insertion' ).
                ENDIF.
              CATCH cx_sy_itab_duplicate_key.
                Lcx_abapgit_ajson_error=>raise( 'Duplicate insertion' ).
            ENDTRY.
          ENDIF.

        ENDLOOP.

      CATCH Lcx_abapgit_ajson_error INTO lx_ajson.
        IF lx_ajson->location IS INITIAL.
          lx_ajson->set_location( <n>-path && <n>-name ).
        ENDIF.
        RAISE EXCEPTION lx_ajson.
      CATCH cx_sy_conversion_no_number.
        Lcx_abapgit_ajson_error=>raise(
        iv_msg = 'Source is not a number'
        iv_location = <n>-path && <n>-name ).
      CATCH cx_root INTO lx_root.
        Lcx_abapgit_ajson_error=>raise(
        iv_msg = lx_root->get_text( )
        iv_location = <n>-path && <n>-name ).
    ENDTRY.

  ENDMETHOD.

  METHOD value_to_abap.

    FIELD-SYMBOLS <container> TYPE any.

    IF is_node_type-type_kind CA SHRITEFUH64VYIPN5I4UHL45BCSR4A=>deep_targets.
      Lcx_abapgit_ajson_error=>raise( |Unsupported target for value [{ is_node_type-type_kind }]| ).
    ENDIF.

    ASSIGN i_container_ref->* TO <container>.
    ASSERT sy-subrc = 0.

    CASE is_node-type.
      WHEN Lif_abapgit_ajson_types=>node_type-null.
        " Do nothing
      WHEN Lif_abapgit_ajson_types=>node_type-boolean.
        " TODO: check type ?
        <container> = boolc( is_node-value = 'true' ).
      WHEN Lif_abapgit_ajson_types=>node_type-number.
        " TODO: check type ?
        <container> = is_node-value.

      WHEN Lif_abapgit_ajson_types=>node_type-string.
        " TODO: check type ?
        IF is_node_type-type_kind = SHRITEFUH64VYIPN5I4UHL45BCSR4A=>date AND is_node-value IS NOT INITIAL.
          <container> = to_date( is_node-value ).
        ELSEIF is_node_type-type_kind = SHRITEFUH64VYIPN5I4UHL45BCSR4A=>packed AND is_node-value IS NOT INITIAL.
          <container> = to_timestamp( is_node-value ).
        ELSE.
          <container> = is_node-value.
        ENDIF.
      WHEN OTHERS.
        Lcx_abapgit_ajson_error=>raise( |Unexpected JSON type [{ is_node-type }]| ).
    ENDCASE.

  ENDMETHOD.

  METHOD to_date.

    DATA lv_y TYPE c LENGTH 4.
    DATA lv_m TYPE c LENGTH 2.
    DATA lv_d TYPE c LENGTH 2.

    FIND FIRST OCCURRENCE OF REGEX '^(\d{4})-(\d{2})-(\d{2})(T|$)'
      IN iv_value
      SUBMATCHES lv_y lv_m lv_d.
    IF sy-subrc <> 0.
      Lcx_abapgit_ajson_error=>raise( 'Unexpected date format' ).
    ENDIF.
    CONCATENATE lv_y lv_m lv_d INTO rv_result.

  ENDMETHOD.

  METHOD to_timestamp.

    CONSTANTS lc_utc TYPE c LENGTH 6 VALUE 'UTC'.
    CONSTANTS lc_regex_ts_with_hour TYPE string
      VALUE `^(\d{4})-(\d{2})-(\d{2})(T)(\d{2}):(\d{2}):(\d{2})(\+)(\d{2}):(\d{2})`.
    CONSTANTS lc_regex_ts_utc TYPE string
      VALUE `^(\d{4})-(\d{2})-(\d{2})(T)(\d{2}):(\d{2}):(\d{2})(Z|$)`.

    DATA:
      BEGIN OF ls_timestamp,
        year         TYPE c LENGTH 4,
        month        TYPE c LENGTH 2,
        day          TYPE c LENGTH 2,
        t            TYPE c LENGTH 1,
        hour         TYPE c LENGTH 2,
        minute       TYPE c LENGTH 2,
        second       TYPE c LENGTH 2,
        local_sign   TYPE c LENGTH 1,
        local_hour   TYPE c LENGTH 2,
        local_minute TYPE c LENGTH 2,
      END OF ls_timestamp.

    DATA lv_date TYPE d.
    DATA lv_time TYPE t.
    DATA lv_seconds_conv TYPE i.
    DATA lv_timestamp TYPE timestampl.

    FIND FIRST OCCURRENCE OF REGEX lc_regex_ts_with_hour
      IN iv_value SUBMATCHES
        ls_timestamp-year ls_timestamp-month ls_timestamp-day ls_timestamp-t
        ls_timestamp-hour ls_timestamp-minute ls_timestamp-second
        ls_timestamp-local_sign ls_timestamp-local_hour ls_timestamp-local_minute.

    IF sy-subrc = 0.

      lv_seconds_conv = ( ls_timestamp-local_hour * 3600 ) + ( ls_timestamp-local_minute * 60 ).

    ELSE.

      FIND FIRST OCCURRENCE OF REGEX lc_regex_ts_utc
        IN iv_value SUBMATCHES
          ls_timestamp-year ls_timestamp-month ls_timestamp-day ls_timestamp-t
          ls_timestamp-hour ls_timestamp-minute ls_timestamp-second.

      IF sy-subrc <> 0.
        Lcx_abapgit_ajson_error=>raise( 'Unexpected timestamp format' ).
      ENDIF.

    ENDIF.

    CONCATENATE ls_timestamp-year ls_timestamp-month ls_timestamp-day INTO lv_date.
    CONCATENATE ls_timestamp-hour ls_timestamp-minute ls_timestamp-second INTO lv_time.

    CONVERT DATE lv_date TIME lv_time INTO TIME STAMP lv_timestamp TIME ZONE lc_utc.

    TRY.

        CASE ls_timestamp-local_sign.
          WHEN '-'.
            lv_timestamp = cl_abap_tstmp=>add(
            tstmp = lv_timestamp
            secs  = lv_seconds_conv ).
          WHEN '+'.
            lv_timestamp = cl_abap_tstmp=>subtractsecs(
            tstmp = lv_timestamp
            secs  = lv_seconds_conv ).
        ENDCASE.

      CATCH cx_parameter_invalid_range cx_parameter_invalid_type.
        Lcx_abapgit_ajson_error=>raise( 'Unexpected error calculating timestamp' ).
    ENDTRY.

    IF lv_timestamp IS NOT INITIAL.
      cl_abap_tstmp=>move(
        EXPORTING
          tstmp_src = lv_timestamp
        IMPORTING
          tstmp_tgt = rv_result ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* ABAP_TO_JSON
**********************************************************************

CLASS SHRITEFUH64VYIPN5I4UHL45BC3R4A DEFINITION FINAL.
  PUBLIC SECTION.

    CLASS-METHODS convert
      IMPORTING
        iv_data            TYPE any
        is_prefix          TYPE Lif_abapgit_ajson_types=>ty_path_name OPTIONAL
        iv_array_index     TYPE i DEFAULT 0
        ii_custom_mapping  TYPE REF TO Lif_abapgit_ajson_mapping OPTIONAL
        is_opts            TYPE Lif_abapgit_ajson=>ty_opts OPTIONAL
        iv_item_order      TYPE i DEFAULT 0
      RETURNING
        VALUE(rt_nodes)   TYPE Lif_abapgit_ajson_types=>ty_nodes_tt
      RAISING
        Lcx_abapgit_ajson_error.

    CLASS-METHODS insert_with_type
      IMPORTING
        iv_data            TYPE any
        iv_type            TYPE Lif_abapgit_ajson_types=>ty_node_type
        is_prefix          TYPE Lif_abapgit_ajson_types=>ty_path_name OPTIONAL
        iv_array_index     TYPE i DEFAULT 0
        ii_custom_mapping  TYPE REF TO Lif_abapgit_ajson_mapping OPTIONAL
        is_opts            TYPE Lif_abapgit_ajson=>ty_opts OPTIONAL
        iv_item_order      TYPE i DEFAULT 0
      RETURNING
        VALUE(rt_nodes)   TYPE Lif_abapgit_ajson_types=>ty_nodes_tt
      RAISING
        Lcx_abapgit_ajson_error.

    CLASS-METHODS format_date
      IMPORTING
        iv_date TYPE d
      RETURNING
        VALUE(rv_str) TYPE string.
    CLASS-METHODS format_time
      IMPORTING
        iv_time TYPE t
      RETURNING
        VALUE(rv_str) TYPE string.
    CLASS-METHODS format_timestamp
      IMPORTING
        iv_ts TYPE timestamp
      RETURNING
        VALUE(rv_str) TYPE string.

    CLASS-METHODS class_constructor.

  PRIVATE SECTION.

    CLASS-DATA gv_ajson_absolute_type_name TYPE string.
    DATA mi_custom_mapping TYPE REF TO Lif_abapgit_ajson_mapping.
    DATA mv_keep_item_order TYPE abap_bool.
    DATA mv_format_datetime TYPE abap_bool.

    METHODS convert_any
      IMPORTING
        iv_data TYPE any
        io_type TYPE REF TO cl_abap_typedescr
        is_prefix TYPE Lif_abapgit_ajson_types=>ty_path_name
        iv_index TYPE i DEFAULT 0
        iv_item_order TYPE i DEFAULT 0
      CHANGING
        ct_nodes TYPE Lif_abapgit_ajson_types=>ty_nodes_tt
      RAISING
        Lcx_abapgit_ajson_error.

    METHODS convert_ajson
      IMPORTING
        io_json TYPE REF TO Lif_abapgit_ajson
        is_prefix TYPE Lif_abapgit_ajson_types=>ty_path_name
        iv_index TYPE i DEFAULT 0
        iv_item_order TYPE i DEFAULT 0
      CHANGING
        ct_nodes TYPE Lif_abapgit_ajson_types=>ty_nodes_tt
      RAISING
        Lcx_abapgit_ajson_error.

    METHODS convert_value
      IMPORTING
        iv_data TYPE any
        io_type TYPE REF TO cl_abap_typedescr
        is_prefix TYPE Lif_abapgit_ajson_types=>ty_path_name
        iv_index TYPE i DEFAULT 0
        iv_item_order TYPE i DEFAULT 0
      CHANGING
        ct_nodes TYPE Lif_abapgit_ajson_types=>ty_nodes_tt
      RAISING
        Lcx_abapgit_ajson_error.

    METHODS convert_ref
      IMPORTING
        iv_data TYPE any
        is_prefix TYPE Lif_abapgit_ajson_types=>ty_path_name
        iv_index TYPE i DEFAULT 0
        iv_item_order TYPE i DEFAULT 0
      CHANGING
        ct_nodes TYPE Lif_abapgit_ajson_types=>ty_nodes_tt
      RAISING
        Lcx_abapgit_ajson_error.

    METHODS convert_struc
      IMPORTING
        iv_data TYPE any
        io_type TYPE REF TO cl_abap_typedescr
        is_prefix TYPE Lif_abapgit_ajson_types=>ty_path_name
        iv_index TYPE i DEFAULT 0
        iv_item_order TYPE i DEFAULT 0
      CHANGING
        ct_nodes TYPE Lif_abapgit_ajson_types=>ty_nodes_tt
      RAISING
        Lcx_abapgit_ajson_error.

    METHODS convert_table
      IMPORTING
        iv_data TYPE any
        io_type TYPE REF TO cl_abap_typedescr
        is_prefix TYPE Lif_abapgit_ajson_types=>ty_path_name
        iv_index TYPE i DEFAULT 0
        iv_item_order TYPE i DEFAULT 0
      CHANGING
        ct_nodes TYPE Lif_abapgit_ajson_types=>ty_nodes_tt
      RAISING
        Lcx_abapgit_ajson_error.

    METHODS insert_value_with_type
      IMPORTING
        iv_data TYPE any
        iv_type TYPE Lif_abapgit_ajson_types=>ty_node_type
        io_type TYPE REF TO cl_abap_typedescr
        is_prefix TYPE Lif_abapgit_ajson_types=>ty_path_name
        iv_index TYPE i DEFAULT 0
        iv_item_order TYPE i DEFAULT 0
      CHANGING
        ct_nodes TYPE Lif_abapgit_ajson_types=>ty_nodes_tt
      RAISING
        Lcx_abapgit_ajson_error.

ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BC3R4A IMPLEMENTATION.

  METHOD class_constructor.

    DATA lo_dummy TYPE REF TO Lcl_abapgit_ajson.
    DATA lo_type TYPE REF TO cl_abap_refdescr.
    lo_type ?= cl_abap_typedescr=>describe_by_data( lo_dummy ).
    gv_ajson_absolute_type_name = lo_type->get_referenced_type( )->absolute_name.

  ENDMETHOD.

  METHOD convert.

    DATA lo_type TYPE REF TO cl_abap_typedescr.
    DATA lo_converter TYPE REF TO SHRITEFUH64VYIPN5I4UHL45BC3R4A.

    lo_type = cl_abap_typedescr=>describe_by_data( iv_data ).

    CREATE OBJECT lo_converter.
    lo_converter->mi_custom_mapping  = ii_custom_mapping.
    lo_converter->mv_keep_item_order = is_opts-keep_item_order.
    lo_converter->mv_format_datetime = is_opts-format_datetime.

    lo_converter->convert_any(
      EXPORTING
        iv_data       = iv_data
        io_type       = lo_type
        is_prefix     = is_prefix
        iv_index      = iv_array_index
        iv_item_order = iv_item_order
      CHANGING
        ct_nodes = rt_nodes ).

  ENDMETHOD.

  METHOD convert_any.

    CASE io_type->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        convert_value(
          EXPORTING
            iv_data   = iv_data
            io_type   = io_type
            is_prefix = is_prefix
            iv_index  = iv_index
            iv_item_order = iv_item_order
          CHANGING
            ct_nodes = ct_nodes ).

      WHEN cl_abap_typedescr=>kind_struct.
        convert_struc(
          EXPORTING
            iv_data   = iv_data
            io_type   = io_type
            is_prefix = is_prefix
            iv_index  = iv_index
            iv_item_order = iv_item_order
          CHANGING
            ct_nodes = ct_nodes ).

      WHEN cl_abap_typedescr=>kind_table.
        convert_table(
          EXPORTING
            iv_data   = iv_data
            io_type   = io_type
            is_prefix = is_prefix
            iv_index  = iv_index
            iv_item_order = iv_item_order
          CHANGING
            ct_nodes = ct_nodes ).

      WHEN OTHERS.

        IF io_type->type_kind = SHRITEFUH64VYIPN5I4UHL45BCSR4A=>data_ref OR iv_data IS INITIAL.
          " Convert data references and initial references to other types (like ref to class or interface)
          " Initial references will result in "null"
          convert_ref(
            EXPORTING
              iv_data   = iv_data
              is_prefix = is_prefix
              iv_index  = iv_index
              iv_item_order = iv_item_order
            CHANGING
              ct_nodes = ct_nodes ).

        ELSEIF io_type->type_kind = SHRITEFUH64VYIPN5I4UHL45BCSR4A=>object_ref
          AND cl_abap_typedescr=>describe_by_object_ref( iv_data )->absolute_name = gv_ajson_absolute_type_name.
          convert_ajson(
            EXPORTING
              io_json   = iv_data
              is_prefix = is_prefix
              iv_index  = iv_index
              iv_item_order = iv_item_order
            CHANGING
              ct_nodes = ct_nodes ).
        ELSE.
          Lcx_abapgit_ajson_error=>raise( |Unsupported type [{ io_type->type_kind
            }] @{ is_prefix-path && is_prefix-name }| ).
        ENDIF.

    ENDCASE.

  ENDMETHOD.

  METHOD convert_ajson.

    FIELD-SYMBOLS <src> LIKE LINE OF ct_nodes.
    FIELD-SYMBOLS <dst> LIKE LINE OF ct_nodes.

    IF io_json IS NOT BOUND.
      RETURN.
    ENDIF.

    LOOP AT io_json->mt_json_tree ASSIGNING <src>.
      APPEND <src> TO ct_nodes ASSIGNING <dst>.

      IF <dst>-path IS INITIAL AND <dst>-name IS INITIAL. " root node
        <dst>-path  = is_prefix-path.
        <dst>-name  = is_prefix-name.
        <dst>-index = iv_index.
        <dst>-order = iv_item_order.
      ELSE.
        <dst>-path = is_prefix-path && is_prefix-name && <dst>-path.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD format_date.
    IF iv_date IS NOT INITIAL.
      rv_str = iv_date+0(4) && '-' && iv_date+4(2) && '-' && iv_date+6(2).
    ENDIF.
  ENDMETHOD.

  METHOD format_time.
    IF iv_time IS NOT INITIAL.
      rv_str = iv_time+0(2) && ':' && iv_time+2(2) && ':' && iv_time+4(2).
    ENDIF.
  ENDMETHOD.

  METHOD format_timestamp.

    CONSTANTS lc_utc TYPE c LENGTH 6 VALUE 'UTC'.

    DATA lv_date TYPE d.
    DATA lv_time TYPE t.

    CONVERT TIME STAMP iv_ts TIME ZONE lc_utc
      INTO DATE lv_date TIME lv_time.

    rv_str =
      lv_date+0(4) && '-' && lv_date+4(2) && '-' && lv_date+6(2) &&
      'T' &&
      lv_time+0(2) && ':' && lv_time+2(2) && ':' && lv_time+4(2) &&
      'Z'.

  ENDMETHOD.

  METHOD convert_value.

    DATA ls_node LIKE LINE OF ct_nodes.

    ls_node-path  = is_prefix-path.
    ls_node-name  = is_prefix-name.
    ls_node-index = iv_index.
    ls_node-order = iv_item_order.

    IF ls_node-name IS INITIAL.
      ls_node-name  = is_prefix-name.
    ENDIF.

    IF io_type->absolute_name = '\TYPE-POOL=ABAP\TYPE=ABAP_BOOL'
        OR io_type->absolute_name = '\TYPE=ABAP_BOOLEAN'
        OR io_type->absolute_name = '\TYPE=XSDBOOLEAN'
        OR io_type->absolute_name = '\TYPE=FLAG'
        OR io_type->absolute_name = '\TYPE=XFELD'.
      ls_node-type = Lif_abapgit_ajson_types=>node_type-boolean.
      IF iv_data IS NOT INITIAL.
        ls_node-value = 'true'.
      ELSE.
        ls_node-value = 'false'.
      ENDIF.
    ELSEIF io_type->absolute_name = '\TYPE=TIMESTAMP'.
      IF mv_format_datetime = abap_true.
        ls_node-type  = Lif_abapgit_ajson_types=>node_type-string.
        ls_node-value = format_timestamp( iv_data ).
      ELSE.
        ls_node-type  = Lif_abapgit_ajson_types=>node_type-number.
        ls_node-value = |{ iv_data }|.
      ENDIF.
    ELSEIF io_type->type_kind CO SHRITEFUH64VYIPN5I4UHL45BCSR4A=>texts OR
           io_type->type_kind CO SHRITEFUH64VYIPN5I4UHL45BCSR4A=>binary OR
           io_type->type_kind CO SHRITEFUH64VYIPN5I4UHL45BCSR4A=>enum.
      ls_node-type = Lif_abapgit_ajson_types=>node_type-string.
      ls_node-value = |{ iv_data }|.
    ELSEIF io_type->type_kind = SHRITEFUH64VYIPN5I4UHL45BCSR4A=>date.
      ls_node-type = Lif_abapgit_ajson_types=>node_type-string.
      IF mv_format_datetime = abap_true.
        ls_node-value = format_date( iv_data ).
      ELSE.
        ls_node-value = |{ iv_data }|.
      ENDIF.
    ELSEIF io_type->type_kind = SHRITEFUH64VYIPN5I4UHL45BCSR4A=>time.
      ls_node-type = Lif_abapgit_ajson_types=>node_type-string.
      IF mv_format_datetime = abap_true.
        ls_node-value = format_time( iv_data ).
      ELSE.
        ls_node-value = |{ iv_data }|.
      ENDIF.
    ELSEIF io_type->type_kind CO SHRITEFUH64VYIPN5I4UHL45BCSR4A=>numeric.
      ls_node-type = Lif_abapgit_ajson_types=>node_type-number.
      ls_node-value = |{ iv_data }|.
    ELSE.
      Lcx_abapgit_ajson_error=>raise( |Unexpected elementary type [{
        io_type->type_kind }] @{ is_prefix-path && is_prefix-name }| ).
    ENDIF.

    APPEND ls_node TO ct_nodes.

  ENDMETHOD.

  METHOD convert_ref.

    DATA ls_node LIKE LINE OF ct_nodes.

    ls_node-path  = is_prefix-path.
    ls_node-name  = is_prefix-name.
    ls_node-index = iv_index.
    ls_node-order = iv_item_order.

    IF mi_custom_mapping IS BOUND.
      ls_node-name = mi_custom_mapping->to_json(
        iv_path = is_prefix-path
        iv_name = is_prefix-name ).
    ENDIF.

    IF ls_node-name IS INITIAL.
      ls_node-name  = is_prefix-name.
    ENDIF.

    IF iv_data IS INITIAL.
      ls_node-type  = Lif_abapgit_ajson_types=>node_type-null.
      ls_node-value = 'null'.
    ELSE.
      " TODO support data references
      Lcx_abapgit_ajson_error=>raise( |Unexpected reference @{ is_prefix-path && is_prefix-name }| ).
    ENDIF.

    APPEND ls_node TO ct_nodes.

  ENDMETHOD.

  METHOD convert_struc.

    DATA lo_struc TYPE REF TO cl_abap_structdescr.
    DATA lt_comps TYPE cl_abap_structdescr=>included_view.
    DATA ls_next_prefix LIKE is_prefix.
    DATA lv_mapping_prefix_name LIKE is_prefix-name.
    DATA lv_item_order TYPE i.
    DATA ls_root LIKE LINE OF ct_nodes.

    FIELD-SYMBOLS <root> LIKE ls_root.
    FIELD-SYMBOLS <c> LIKE LINE OF lt_comps.
    FIELD-SYMBOLS <val> TYPE any.

    " Object root

    ls_root-path  = is_prefix-path.
    ls_root-name  = is_prefix-name.
    ls_root-type  = Lif_abapgit_ajson_types=>node_type-object.
    ls_root-index = iv_index.

    IF mi_custom_mapping IS BOUND.
      ls_root-name = mi_custom_mapping->to_json(
        iv_path = is_prefix-path
        iv_name = is_prefix-name ).
    ENDIF.

    IF ls_root-name IS INITIAL.
      ls_root-name  = is_prefix-name.
    ENDIF.

    ls_root-order = iv_item_order.

    APPEND ls_root TO ct_nodes ASSIGNING <root>.

    " Object attributes

    lo_struc ?= io_type.
    lt_comps = lo_struc->get_included_view( ).
    " replaced call to get_components() with get_included_view() to avoid problems with suffixes in includes.
    " get_components is potentially much slower than lo_struc->components
    " but ! we still need it to identify booleans
    " and rtti seems to cache type descriptions really well (https://github.com/sbcgua/benchmarks.git)
    " the structures will be repeated in real life

    ls_next_prefix-path = is_prefix-path && <root>-name && '/'.

    LOOP AT lt_comps ASSIGNING <c>.
      CLEAR lv_mapping_prefix_name.

      <root>-children = <root>-children + 1.
      ls_next_prefix-name = to_lower( <c>-name ).
      ASSIGN COMPONENT <c>-name OF STRUCTURE iv_data TO <val>.
      ASSERT sy-subrc = 0.

      IF mi_custom_mapping IS BOUND AND <c>-type->kind = cl_abap_typedescr=>kind_elem.
        lv_mapping_prefix_name = mi_custom_mapping->to_json( iv_path = ls_next_prefix-path
                                                             iv_name = ls_next_prefix-name ).
      ENDIF.

      IF lv_mapping_prefix_name IS NOT INITIAL.
        ls_next_prefix-name = lv_mapping_prefix_name.
      ENDIF.

      IF mv_keep_item_order = abap_true.
        lv_item_order = <root>-children.
      ENDIF.

      convert_any(
        EXPORTING
          iv_data   = <val>
          io_type   = <c>-type
          is_prefix = ls_next_prefix
          iv_item_order = lv_item_order
        CHANGING
          ct_nodes = ct_nodes ).

    ENDLOOP.

  ENDMETHOD.

  METHOD convert_table.

    DATA lo_table TYPE REF TO cl_abap_tabledescr.
    DATA lo_ltype TYPE REF TO cl_abap_typedescr.
    DATA ls_next_prefix LIKE is_prefix.
    DATA lv_tabix TYPE sy-tabix.
    DATA ls_root LIKE LINE OF ct_nodes.

    FIELD-SYMBOLS <root> LIKE ls_root.
    FIELD-SYMBOLS <tab> TYPE ANY TABLE.
    FIELD-SYMBOLS <val> TYPE any.

    " Array root

    ls_root-path  = is_prefix-path.
    ls_root-name  = is_prefix-name.
    ls_root-type  = Lif_abapgit_ajson_types=>node_type-array.
    ls_root-index = iv_index.
    ls_root-order = iv_item_order.

    IF mi_custom_mapping IS BOUND.
      ls_root-name = mi_custom_mapping->to_json(
        iv_path = is_prefix-path
        iv_name = is_prefix-name ).
    ENDIF.

    IF ls_root-name IS INITIAL.
      ls_root-name  = is_prefix-name.
    ENDIF.

    APPEND ls_root TO ct_nodes ASSIGNING <root>.

    " Array items

    lo_table ?= io_type.
    lo_ltype  = lo_table->get_table_line_type( ).

    ls_next_prefix-path = is_prefix-path && <root>-name && '/'.
    ASSIGN iv_data TO <tab>.

    lv_tabix = 1.
    LOOP AT <tab> ASSIGNING <val>.
      ls_next_prefix-name = to_lower( |{ lv_tabix }| ).

      convert_any(
        EXPORTING
          iv_data   = <val>
          io_type   = lo_ltype
          is_prefix = ls_next_prefix
          iv_index  = <root>-children + 1
        CHANGING
          ct_nodes = ct_nodes ).

      <root>-children = <root>-children + 1.
      lv_tabix = lv_tabix + 1.
    ENDLOOP.

  ENDMETHOD.

  METHOD insert_with_type.

    DATA lo_type TYPE REF TO cl_abap_typedescr.
    DATA lo_converter TYPE REF TO SHRITEFUH64VYIPN5I4UHL45BC3R4A.

    lo_type = cl_abap_typedescr=>describe_by_data( iv_data ).

    CREATE OBJECT lo_converter.
    lo_converter->mi_custom_mapping  = ii_custom_mapping.
    lo_converter->mv_keep_item_order = is_opts-keep_item_order.
    lo_converter->mv_format_datetime = is_opts-format_datetime.

    lo_converter->insert_value_with_type(
      EXPORTING
        iv_data       = iv_data
        iv_type       = iv_type
        io_type       = lo_type
        is_prefix     = is_prefix
        iv_index      = iv_array_index
        iv_item_order = iv_item_order
      CHANGING
        ct_nodes = rt_nodes ).

  ENDMETHOD.

  METHOD insert_value_with_type.

    DATA lv_prefix TYPE string.
    DATA ls_node LIKE LINE OF ct_nodes.

    lv_prefix = is_prefix-path && is_prefix-name.
    IF io_type->type_kind CO SHRITEFUH64VYIPN5I4UHL45BCSR4A=>texts OR
       io_type->type_kind CO SHRITEFUH64VYIPN5I4UHL45BCSR4A=>date OR
       io_type->type_kind CO SHRITEFUH64VYIPN5I4UHL45BCSR4A=>time.
      IF iv_type = Lif_abapgit_ajson_types=>node_type-boolean AND iv_data <> 'true' AND iv_data <> 'false'.
        Lcx_abapgit_ajson_error=>raise( |Unexpected boolean value [{ iv_data }] @{ lv_prefix }| ).
      ELSEIF iv_type = Lif_abapgit_ajson_types=>node_type-null AND iv_data IS NOT INITIAL.
        Lcx_abapgit_ajson_error=>raise( |Unexpected null value [{ iv_data }] @{ lv_prefix }| ).
      ELSEIF iv_type = Lif_abapgit_ajson_types=>node_type-number AND iv_data CN '0123456789. E+-'.
        Lcx_abapgit_ajson_error=>raise( |Unexpected numeric value [{ iv_data }] @{ lv_prefix }| ).
      ELSEIF iv_type <> Lif_abapgit_ajson_types=>node_type-string AND iv_type <> Lif_abapgit_ajson_types=>node_type-boolean
        AND iv_type <> Lif_abapgit_ajson_types=>node_type-null AND iv_type <> Lif_abapgit_ajson_types=>node_type-number.
        Lcx_abapgit_ajson_error=>raise( |Unexpected type for value [{ iv_type },{ iv_data }] @{ lv_prefix }| ).
      ENDIF.
    ELSEIF io_type->type_kind CO SHRITEFUH64VYIPN5I4UHL45BCSR4A=>numeric.
      IF iv_type <> Lif_abapgit_ajson_types=>node_type-number.
        Lcx_abapgit_ajson_error=>raise( |Unexpected value for numeric [{ iv_data }] @{ lv_prefix }| ).
      ENDIF.
    ELSE.
      Lcx_abapgit_ajson_error=>raise( |Unexpected type [{ io_type->type_kind }] @{ lv_prefix }| ).
    ENDIF.

    ls_node-path  = is_prefix-path.
    ls_node-name  = is_prefix-name.
    ls_node-index = iv_index.
    ls_node-value = iv_data.
    ls_node-type  = iv_type.
    ls_node-order = iv_item_order.

    IF mi_custom_mapping IS BOUND.
      ls_node-name = mi_custom_mapping->to_json(
        iv_path = is_prefix-path
        iv_name = is_prefix-name ).
    ENDIF.

    IF ls_node-name IS INITIAL.
      ls_node-name  = is_prefix-name.
    ENDIF.

    APPEND ls_node TO ct_nodes.

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* MUTATOR INTERFACE
**********************************************************************

INTERFACE SHRITEFUH64VYIPN5I4UHL45BC5R4A.
  METHODS run
    IMPORTING
      it_source_tree TYPE Lif_abapgit_ajson_types=>ty_nodes_ts
    EXPORTING
      et_dest_tree TYPE Lif_abapgit_ajson_types=>ty_nodes_ts
    RAISING
      Lcx_abapgit_ajson_error.
ENDINTERFACE.

**********************************************************************
* FILTER RUNNER
**********************************************************************

CLASS SHRITEFUH64VYIPN5I4UHL45BC6R4A DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES SHRITEFUH64VYIPN5I4UHL45BC5R4A.
    CLASS-METHODS new
      IMPORTING
        ii_filter TYPE REF TO Lif_abapgit_ajson_filter
      RETURNING
        VALUE(ro_instance) TYPE REF TO SHRITEFUH64VYIPN5I4UHL45BC6R4A.
    METHODS constructor
      IMPORTING
        ii_filter TYPE REF TO Lif_abapgit_ajson_filter.

  PRIVATE SECTION.
    DATA mi_filter TYPE REF TO Lif_abapgit_ajson_filter.
    DATA mr_source_tree TYPE REF TO Lif_abapgit_ajson_types=>ty_nodes_ts.
    DATA mr_dest_tree TYPE REF TO Lif_abapgit_ajson_types=>ty_nodes_ts.

    METHODS walk
      IMPORTING
        iv_path TYPE string
      CHANGING
        cs_parent TYPE Lif_abapgit_ajson_types=>ty_node OPTIONAL
      RAISING
        Lcx_abapgit_ajson_error.

ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BC6R4A IMPLEMENTATION.

  METHOD new.
    CREATE OBJECT ro_instance EXPORTING ii_filter = ii_filter.
  ENDMETHOD.

  METHOD constructor.
    ASSERT ii_filter IS BOUND.
    mi_filter = ii_filter.
  ENDMETHOD.

  METHOD SHRITEFUH64VYIPN5I4UHL45BC5R4A~run.

    CLEAR et_dest_tree.
    GET REFERENCE OF it_source_tree INTO mr_source_tree.
    GET REFERENCE OF et_dest_tree INTO mr_dest_tree.

    walk( iv_path = '' ).

  ENDMETHOD.

  METHOD walk.

    DATA ls_node TYPE Lif_abapgit_ajson_types=>ty_node.

    LOOP AT mr_source_tree->* INTO ls_node WHERE path = iv_path.
      CASE ls_node-type.
        WHEN Lif_abapgit_ajson_types=>node_type-boolean OR Lif_abapgit_ajson_types=>node_type-null
          OR Lif_abapgit_ajson_types=>node_type-number OR Lif_abapgit_ajson_types=>node_type-string.

          IF mi_filter->keep_node( ls_node ) = abap_false.
            CONTINUE.
          ENDIF.

        WHEN Lif_abapgit_ajson_types=>node_type-array OR Lif_abapgit_ajson_types=>node_type-object.

          IF mi_filter->keep_node(
              is_node  = ls_node
              iv_visit = Lif_abapgit_ajson_filter=>visit_type-open ) = abap_false.
            CONTINUE.
          ENDIF.

          " Intentionally clear AFTER "open"
          CLEAR ls_node-children.

          walk(
            EXPORTING
              iv_path = iv_path && ls_node-name && `/`
            CHANGING
              cs_parent    = ls_node ).

          IF mi_filter->keep_node(
              is_node  = ls_node
              iv_visit = Lif_abapgit_ajson_filter=>visit_type-close ) = abap_false.
            CONTINUE.
          ENDIF.

        WHEN OTHERS.
          Lcx_abapgit_ajson_error=>raise( |Unexpected node type { ls_node-type }| ).
      ENDCASE.

      IF cs_parent IS SUPPLIED.
        cs_parent-children = cs_parent-children + 1.
        IF cs_parent-type = Lif_abapgit_ajson_types=>node_type-array.
          ls_node-name  = |{ cs_parent-children }|.
          ls_node-index = cs_parent-children.
        ENDIF.
      ENDIF.
      INSERT ls_node INTO TABLE mr_dest_tree->*.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* MAPPER RUNNER
**********************************************************************

CLASS SHRITEFUH64VYIPN5I4UHL45BDAR4A DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES SHRITEFUH64VYIPN5I4UHL45BC5R4A.
    CLASS-METHODS new
      IMPORTING
        ii_mapper TYPE REF TO Lif_abapgit_ajson_mapping
      RETURNING
        VALUE(ro_instance) TYPE REF TO SHRITEFUH64VYIPN5I4UHL45BDAR4A.
    METHODS constructor
      IMPORTING
        ii_mapper TYPE REF TO Lif_abapgit_ajson_mapping.

  PRIVATE SECTION.
    DATA mi_mapper TYPE REF TO Lif_abapgit_ajson_mapping.
    DATA mr_source_tree TYPE REF TO Lif_abapgit_ajson_types=>ty_nodes_ts.
    DATA mr_dest_tree TYPE REF TO Lif_abapgit_ajson_types=>ty_nodes_ts.

    METHODS process_deep_node
      IMPORTING
        iv_path         TYPE string
        iv_renamed_path TYPE string
        iv_node_type    TYPE Lif_abapgit_ajson_types=>ty_node-type
      RAISING
        Lcx_abapgit_ajson_error.

ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BDAR4A IMPLEMENTATION.

  METHOD new.
    CREATE OBJECT ro_instance EXPORTING ii_mapper = ii_mapper.
  ENDMETHOD.

  METHOD constructor.
    ASSERT ii_mapper IS BOUND.
    mi_mapper = ii_mapper.
  ENDMETHOD.

  METHOD SHRITEFUH64VYIPN5I4UHL45BC5R4A~run.

    FIELD-SYMBOLS <root> LIKE LINE OF it_source_tree.

    READ TABLE it_source_tree WITH KEY path = `` name = `` ASSIGNING <root>.
    IF sy-subrc <> 0
      OR NOT ( <root>-type = Lif_abapgit_ajson_types=>node_type-array OR <root>-type = Lif_abapgit_ajson_types=>node_type-object ).
      " empty or one-value-only tree
      et_dest_tree = it_source_tree.
      RETURN.
    ENDIF.

    CLEAR et_dest_tree.
    GET REFERENCE OF it_source_tree INTO mr_source_tree.
    GET REFERENCE OF et_dest_tree INTO mr_dest_tree.
    INSERT <root> INTO TABLE et_dest_tree.

    process_deep_node(
      iv_path         = `/`
      iv_renamed_path = `/`
      iv_node_type    = <root>-type ).

  ENDMETHOD.

  METHOD process_deep_node.


    FIELD-SYMBOLS <item> LIKE LINE OF mr_source_tree->*.
    DATA ls_renamed_node LIKE <item>.

    LOOP AT mr_source_tree->* ASSIGNING <item> WHERE path = iv_path.
      ls_renamed_node = <item>.
      IF iv_node_type <> Lif_abapgit_ajson_types=>node_type-array.
        " don't rename array item names -> they are numeric index
        mi_mapper->rename_node(
          EXPORTING
            is_node = <item>
          CHANGING
            cv_name = ls_renamed_node-name ).
        IF ls_renamed_node-name IS INITIAL.
          Lcx_abapgit_ajson_error=>raise(
            iv_msg  = 'Renamed node name cannot be empty'
            is_node = <item> ).
        ENDIF.
      ENDIF.
      ls_renamed_node-path = iv_renamed_path.

      INSERT ls_renamed_node INTO TABLE mr_dest_tree->*.
      IF sy-subrc <> 0. " = 4 ?
        Lcx_abapgit_ajson_error=>raise(
          iv_msg  = 'Renamed node has a duplicate'
          is_node = ls_renamed_node ).
      ENDIF.

      " maybe also catch CX_SY_ITAB_DUPLICATE_KEY but secondary keys are not changed here, so not for now

      IF <item>-type = Lif_abapgit_ajson_types=>node_type-array OR <item>-type = Lif_abapgit_ajson_types=>node_type-object.
        process_deep_node(
          iv_path         = iv_path && <item>-name && `/`
          iv_renamed_path = iv_renamed_path && ls_renamed_node-name && `/`
          iv_node_type    = <item>-type ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* MUTATOR QUEUE
**********************************************************************

CLASS SHRITEFUH64VYIPN5I4UHL45BDCR4A DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES SHRITEFUH64VYIPN5I4UHL45BC5R4A.
    CLASS-METHODS new
      RETURNING
        VALUE(ro_instance) TYPE REF TO SHRITEFUH64VYIPN5I4UHL45BDCR4A.
    METHODS add
      IMPORTING
        ii_mutator TYPE REF TO SHRITEFUH64VYIPN5I4UHL45BC5R4A
      RETURNING
        VALUE(ro_self) TYPE REF TO SHRITEFUH64VYIPN5I4UHL45BDCR4A.

  PRIVATE SECTION.
    DATA mt_queue TYPE STANDARD TABLE OF REF TO SHRITEFUH64VYIPN5I4UHL45BC5R4A.

ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BDCR4A IMPLEMENTATION.

  METHOD add.
    IF ii_mutator IS BOUND.
      APPEND ii_mutator TO mt_queue.
    ENDIF.
    ro_self = me.
  ENDMETHOD.

  METHOD new.
    CREATE OBJECT ro_instance.
  ENDMETHOD.

  METHOD SHRITEFUH64VYIPN5I4UHL45BC5R4A~run.

    DATA li_mutator TYPE REF TO SHRITEFUH64VYIPN5I4UHL45BC5R4A.
    DATA lv_qsize TYPE i.
    FIELD-SYMBOLS <from> LIKE it_source_tree.
    FIELD-SYMBOLS <to> LIKE it_source_tree.
    DATA lr_buf TYPE REF TO Lif_abapgit_ajson_types=>ty_nodes_ts.

    lv_qsize = lines( mt_queue ).

    IF lv_qsize = 0.
      et_dest_tree = it_source_tree.
      RETURN.
    ENDIF.

    LOOP AT mt_queue INTO li_mutator.
      IF sy-tabix = 1.
        ASSIGN it_source_tree TO <from>.
      ELSE.
        ASSIGN lr_buf->* TO <from>.
      ENDIF.

      IF sy-tabix = lv_qsize.
        ASSIGN et_dest_tree TO <to>.
      ELSE.
        CREATE DATA lr_buf.
        ASSIGN lr_buf->* TO <to>.
      ENDIF.

      li_mutator->run(
        EXPORTING
          it_source_tree = <from>
        IMPORTING
          et_dest_tree = <to> ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

*"* test class
*include zcl_abapgit_ajson=============ccau.
**********************************************************************
* UTIL
**********************************************************************
CLASS SHRITEFUH64VYIPN5I4UHL45BDER4A DEFINITION FINAL.
  PUBLIC SECTION.

    DATA mt_nodes TYPE Lif_abapgit_ajson_types=>ty_nodes_tt.
    METHODS add
      IMPORTING
        iv_str TYPE string.
    METHODS clear.
    METHODS sorted
      RETURNING
        VALUE(rt_nodes) TYPE Lif_abapgit_ajson_types=>ty_nodes_ts.

ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BDER4A IMPLEMENTATION.
  METHOD add.

    FIELD-SYMBOLS <n> LIKE LINE OF mt_nodes.
    DATA lv_children TYPE string.
    DATA lv_index TYPE string.
    DATA lv_order TYPE string.

    APPEND INITIAL LINE TO mt_nodes ASSIGNING <n>.

    SPLIT iv_str AT '|' INTO
      <n>-path
      <n>-name
      <n>-type
      <n>-value
      lv_index
      lv_children
      lv_order.
    CONDENSE <n>-path.
    CONDENSE <n>-name.
    CONDENSE <n>-type.
    CONDENSE <n>-value.
    <n>-index = lv_index.
    <n>-children = lv_children.
    <n>-order = lv_order.

  ENDMETHOD.

  METHOD sorted.
    rt_nodes = mt_nodes.
  ENDMETHOD.

  METHOD clear.
    CLEAR mt_nodes.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
* PARSER
**********************************************************************



**********************************************************************
* SERIALIZER
**********************************************************************



**********************************************************************
* UTILS
**********************************************************************


*CLASS zcl_abapgit_ajson DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BDKR4A.


**********************************************************************
* READER
**********************************************************************


*CLASS zcl_abapgit_ajson DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BDLR4A.



**********************************************************************
* JSON TO ABAP
**********************************************************************


*CLASS zcl_abapgit_ajson DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BDMR4A.


**********************************************************************
* WRITER
**********************************************************************


*CLASS zcl_abapgit_ajson DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BDNR4A.



**********************************************************************
* INTEGRATED
**********************************************************************


**********************************************************************
* ABAP TO JSON
**********************************************************************

*CLASS zcl_abapgit_ajson DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BDQR4A.


**********************************************************************
* FILTER TEST
**********************************************************************



**********************************************************************
* MAPPER TEST
**********************************************************************



**********************************************************************
* CLONING TEST
**********************************************************************



class LCL_ABAPGIT_AJSON implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    ms_opts-keep_item_order = iv_keep_item_order.
    ms_opts-to_abap_corresponding_only = iv_to_abap_corresponding_only.
    format_datetime( iv_format_datetime ).
  ENDMETHOD.
  METHOD create_empty.
    CREATE OBJECT ro_instance
      EXPORTING
        iv_to_abap_corresponding_only = iv_to_abap_corresponding_only
        iv_format_datetime = iv_format_datetime
        iv_keep_item_order = iv_keep_item_order.
    ro_instance->mi_custom_mapping = ii_custom_mapping.
  ENDMETHOD.
  METHOD create_from.

    DATA lo_mutator_queue TYPE REF TO SHRITEFUH64VYIPN5I4UHL45BDCR4A.

    IF ii_source_json IS NOT BOUND.
      Lcx_abapgit_ajson_error=>raise( 'Source not bound' ).
    ENDIF.

    CREATE OBJECT ro_instance
      EXPORTING
        iv_to_abap_corresponding_only = ii_source_json->opts( )-to_abap_corresponding_only
        iv_format_datetime = ii_source_json->opts( )-format_datetime
        iv_keep_item_order = ii_source_json->opts( )-keep_item_order.

    IF ii_filter IS NOT BOUND AND ii_mapper IS NOT BOUND.
      ro_instance->mt_json_tree = ii_source_json->mt_json_tree.
    ELSE.
      CREATE OBJECT lo_mutator_queue.
      IF ii_mapper IS BOUND.
        " Mapping goes first. But maybe it should be a freely definable queue of processors ?
        lo_mutator_queue->add( SHRITEFUH64VYIPN5I4UHL45BDAR4A=>new( ii_mapper ) ).
      ENDIF.
      IF ii_filter IS BOUND.
        lo_mutator_queue->add( SHRITEFUH64VYIPN5I4UHL45BC6R4A=>new( ii_filter ) ).
      ENDIF.
      lo_mutator_queue->SHRITEFUH64VYIPN5I4UHL45BC5R4A~run(
        EXPORTING
          it_source_tree = ii_source_json->mt_json_tree
        IMPORTING
          et_dest_tree = ro_instance->mt_json_tree ).
    ENDIF.

  ENDMETHOD.
  METHOD delete_subtree.

    DATA lv_parent_path TYPE string.
    DATA lr_parent LIKE ir_parent.

    READ TABLE mt_json_tree INTO rs_top_node
      WITH KEY
        path = iv_path
        name = iv_name.
    IF sy-subrc <> 0.
      RETURN. " Not found ? nothing to delete !
    ENDIF.

    DELETE mt_json_tree INDEX sy-tabix. " where path = iv_path and name = iv_name.

    IF rs_top_node-children > 0. " only for objects and arrays
      lv_parent_path = iv_path && iv_name && '/*'.
      DELETE mt_json_tree WHERE path CP lv_parent_path.
    ENDIF.

    " decrement parent children
    IF ir_parent IS SUPPLIED.
      ir_parent->children = ir_parent->children - 1.
    ELSE.
      lr_parent = get_item( iv_path ).
      IF lr_parent IS NOT INITIAL.
        lr_parent->children = lr_parent->children - 1.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD get_item.

    FIELD-SYMBOLS <item> LIKE LINE OF mt_json_tree.
    DATA ls_path_name TYPE Lif_abapgit_ajson_types=>ty_path_name.
    ls_path_name = SHRITEFUH64VYIPN5I4UHL45BCTR4A=>split_path( iv_path ).

    READ TABLE mt_json_tree
      ASSIGNING <item>
      WITH KEY
        path = ls_path_name-path
        name = ls_path_name-name.
    IF sy-subrc = 0.
      GET REFERENCE OF <item> INTO rv_item.
    ENDIF.

  ENDMETHOD.
  METHOD new.
    CREATE OBJECT ro_instance
      EXPORTING
        iv_to_abap_corresponding_only = iv_to_abap_corresponding_only
        iv_format_datetime = iv_format_datetime
        iv_keep_item_order = iv_keep_item_order.
  ENDMETHOD.
  METHOD parse.

    DATA lo_parser TYPE REF TO SHRITEFUH64VYIPN5I4UHL45BCVR4A.

    CREATE OBJECT ro_instance.
    CREATE OBJECT lo_parser.
    ro_instance->mt_json_tree = lo_parser->parse(
      iv_json            = iv_json
      iv_keep_item_order = iv_keep_item_order ).
    ro_instance->mi_custom_mapping = ii_custom_mapping.
    ro_instance->ms_opts-keep_item_order = iv_keep_item_order.

    IF iv_freeze = abap_true.
      ro_instance->freeze( ).
    ENDIF.

  ENDMETHOD.
  METHOD prove_path_exists.

    DATA lt_path TYPE string_table.
    DATA lr_node_parent LIKE rr_end_node.
    DATA lv_cur_path TYPE string.
    DATA lv_cur_name TYPE string.
    DATA ls_new_node LIKE LINE OF mt_json_tree.

    SPLIT iv_path AT '/' INTO TABLE lt_path.
    DELETE lt_path WHERE table_line IS INITIAL.

    DO.
      lr_node_parent = rr_end_node.
      READ TABLE mt_json_tree REFERENCE INTO rr_end_node
        WITH KEY
          path = lv_cur_path
          name = lv_cur_name.
      IF sy-subrc <> 0. " New node, assume it is always object as it has a named child, use touch_array to init array
        CLEAR ls_new_node.
        IF lr_node_parent IS NOT INITIAL. " if has parent
          lr_node_parent->children = lr_node_parent->children + 1.
          IF lr_node_parent->type = Lif_abapgit_ajson_types=>node_type-array.
            ls_new_node-index = SHRITEFUH64VYIPN5I4UHL45BCTR4A=>validate_array_index(
              iv_path  = lv_cur_path
              iv_index = lv_cur_name ).
          ENDIF.
        ENDIF.
        ls_new_node-path = lv_cur_path.
        ls_new_node-name = lv_cur_name.
        ls_new_node-type = Lif_abapgit_ajson_types=>node_type-object.
        INSERT ls_new_node INTO TABLE mt_json_tree REFERENCE INTO rr_end_node.
      ENDIF.
      lv_cur_path = lv_cur_path && lv_cur_name && '/'.
      READ TABLE lt_path INDEX sy-index INTO lv_cur_name.
      IF sy-subrc <> 0.
        EXIT. " no more segments
      ENDIF.
    ENDDO.

  ENDMETHOD.
  METHOD read_only_watchdog.
    IF ms_opts-read_only = abap_true.
      Lcx_abapgit_ajson_error=>raise( 'This json instance is read only' ).
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_ajson~array_to_string_table.

    DATA lv_normalized_path TYPE string.
    DATA lr_node TYPE REF TO Lif_abapgit_ajson_types=>ty_node.
    FIELD-SYMBOLS <item> LIKE LINE OF mt_json_tree.

    lv_normalized_path = SHRITEFUH64VYIPN5I4UHL45BCTR4A=>normalize_path( iv_path ).
    lr_node = get_item( iv_path ).

    IF lr_node IS INITIAL.
      Lcx_abapgit_ajson_error=>raise( |Path not found: { iv_path }| ).
    ENDIF.
    IF lr_node->type <> Lif_abapgit_ajson_types=>node_type-array.
      Lcx_abapgit_ajson_error=>raise( |Array expected at: { iv_path }| ).
    ENDIF.

    LOOP AT mt_json_tree ASSIGNING <item> WHERE path = lv_normalized_path.
      CASE <item>-type.
        WHEN Lif_abapgit_ajson_types=>node_type-number OR Lif_abapgit_ajson_types=>node_type-string.
          APPEND <item>-value TO rt_string_table.
        WHEN Lif_abapgit_ajson_types=>node_type-null.
          APPEND '' TO rt_string_table.
        WHEN Lif_abapgit_ajson_types=>node_type-boolean.
          DATA lv_tmp TYPE string.
          IF <item>-value = 'true'.
            lv_tmp = abap_true.
          ELSE.
            CLEAR lv_tmp.
          ENDIF.
          APPEND lv_tmp TO rt_string_table.
        WHEN OTHERS.
          Lcx_abapgit_ajson_error=>raise( |Cannot convert [{ <item>-type
            }] to string at [{ <item>-path }{ <item>-name }]| ).
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~clear.

    read_only_watchdog( ).
    CLEAR mt_json_tree.

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~clone.
    ri_json = create_from( me ).
  ENDMETHOD.
  METHOD Lif_abapgit_ajson~delete.

    read_only_watchdog( ).

    DATA ls_split_path TYPE Lif_abapgit_ajson_types=>ty_path_name.
    ls_split_path = SHRITEFUH64VYIPN5I4UHL45BCTR4A=>split_path( iv_path ).

    delete_subtree(
      iv_path = ls_split_path-path
      iv_name = ls_split_path-name ).

    ri_json = me.

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~exists.
    rv_exists = boolc( get_item( iv_path ) IS NOT INITIAL ).
  ENDMETHOD.
  METHOD Lif_abapgit_ajson~filter.
    ri_json = create_from(
      ii_source_json = me
      ii_filter      = ii_filter ).
  ENDMETHOD.
  METHOD Lif_abapgit_ajson~format_datetime.
    ms_opts-format_datetime = iv_use_iso.
    ri_json = me.
  ENDMETHOD.
  METHOD Lif_abapgit_ajson~freeze.
    ms_opts-read_only = abap_true.
  ENDMETHOD.
  METHOD Lif_abapgit_ajson~get.

    DATA lr_item TYPE REF TO Lif_abapgit_ajson_types=>ty_node.
    lr_item = get_item( iv_path ).
    IF lr_item IS NOT INITIAL.
      rv_value = lr_item->value.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~get_boolean.

    DATA lr_item TYPE REF TO Lif_abapgit_ajson_types=>ty_node.
    lr_item = get_item( iv_path ).
    IF lr_item IS INITIAL OR lr_item->type = Lif_abapgit_ajson_types=>node_type-null.
      RETURN.
    ELSEIF lr_item->type = Lif_abapgit_ajson_types=>node_type-boolean.
      rv_value = boolc( lr_item->value = 'true' ).
    ELSEIF lr_item->value IS NOT INITIAL.
      rv_value = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~get_date.

    DATA lr_item TYPE REF TO Lif_abapgit_ajson_types=>ty_node.
    DATA lv_y TYPE c LENGTH 4.
    DATA lv_m TYPE c LENGTH 2.
    DATA lv_d TYPE c LENGTH 2.

    lr_item = get_item( iv_path ).

    IF lr_item IS NOT INITIAL AND lr_item->type = Lif_abapgit_ajson_types=>node_type-string.
      FIND FIRST OCCURRENCE OF REGEX '^(\d{4})-(\d{2})-(\d{2})(T|$)'
        IN lr_item->value
        SUBMATCHES lv_y lv_m lv_d.
      CONCATENATE lv_y lv_m lv_d INTO rv_value.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~get_integer.

    DATA lr_item TYPE REF TO Lif_abapgit_ajson_types=>ty_node.
    lr_item = get_item( iv_path ).
    IF lr_item IS NOT INITIAL AND lr_item->type = Lif_abapgit_ajson_types=>node_type-number.
      rv_value = lr_item->value.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~get_node_type.

    DATA lr_item TYPE REF TO Lif_abapgit_ajson_types=>ty_node.
    lr_item = get_item( iv_path ).
    IF lr_item IS NOT INITIAL.
      rv_node_type = lr_item->type.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~get_number.

    DATA lr_item TYPE REF TO Lif_abapgit_ajson_types=>ty_node.
    lr_item = get_item( iv_path ).
    IF lr_item IS NOT INITIAL AND lr_item->type = Lif_abapgit_ajson_types=>node_type-number.
      rv_value = lr_item->value.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~get_string.

    DATA lr_item TYPE REF TO Lif_abapgit_ajson_types=>ty_node.
    lr_item = get_item( iv_path ).
    IF lr_item IS NOT INITIAL AND lr_item->type <> Lif_abapgit_ajson_types=>node_type-null.
      rv_value = lr_item->value.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~get_timestamp.

    DATA lo_to_abap TYPE REF TO SHRITEFUH64VYIPN5I4UHL45BCZR4A.
    DATA lr_item TYPE REF TO Lif_abapgit_ajson_types=>ty_node.

    lr_item = get_item( iv_path ).

    IF lr_item IS INITIAL.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_to_abap.

    TRY.
        rv_value = lo_to_abap->to_timestamp( lr_item->value ).
      CATCH Lcx_abapgit_ajson_error.
        RETURN.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~is_empty.
    rv_yes = boolc( lines( mt_json_tree ) = 0 ).
  ENDMETHOD.
  METHOD Lif_abapgit_ajson~keep_item_order.
    ms_opts-keep_item_order = abap_true.
    ri_json = me.
  ENDMETHOD.
  METHOD Lif_abapgit_ajson~map.
    ri_json = create_from(
      ii_source_json = me
      ii_mapper      = ii_mapper ).
  ENDMETHOD.
  METHOD Lif_abapgit_ajson~members.

    DATA lv_normalized_path TYPE string.
    FIELD-SYMBOLS <item> LIKE LINE OF mt_json_tree.

    lv_normalized_path = SHRITEFUH64VYIPN5I4UHL45BCTR4A=>normalize_path( iv_path ).

    LOOP AT mt_json_tree ASSIGNING <item> WHERE path = lv_normalized_path.
      APPEND <item>-name TO rt_members.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~opts.
    rs_opts = ms_opts.
  ENDMETHOD.
  METHOD Lif_abapgit_ajson~push.

    DATA lr_parent TYPE REF TO Lif_abapgit_ajson_types=>ty_node.
    DATA lr_new_node TYPE REF TO Lif_abapgit_ajson_types=>ty_node.

    read_only_watchdog( ).

    lr_parent = get_item( iv_path ).

    IF lr_parent IS INITIAL.
      Lcx_abapgit_ajson_error=>raise( |Path [{ iv_path }] does not exist| ).
    ENDIF.

    IF lr_parent->type <> Lif_abapgit_ajson_types=>node_type-array.
      Lcx_abapgit_ajson_error=>raise( |Path [{ iv_path }] is not array| ).
    ENDIF.

    DATA lt_new_nodes TYPE Lif_abapgit_ajson_types=>ty_nodes_tt.
    DATA ls_new_path TYPE Lif_abapgit_ajson_types=>ty_path_name.
    DATA lv_new_index TYPE i.

    lv_new_index     = lr_parent->children + 1.
    ls_new_path-path = SHRITEFUH64VYIPN5I4UHL45BCTR4A=>normalize_path( iv_path ).
    ls_new_path-name = |{ lv_new_index }|.

    lt_new_nodes = SHRITEFUH64VYIPN5I4UHL45BC3R4A=>convert(
      is_opts            = ms_opts
      iv_data   = iv_val
      is_prefix = ls_new_path ).
    READ TABLE lt_new_nodes INDEX 1 REFERENCE INTO lr_new_node. " assume first record is the array item - not ideal !
    ASSERT sy-subrc = 0.
    lr_new_node->index = lv_new_index.

    " update data
    lr_parent->children = lv_new_index.
    INSERT LINES OF lt_new_nodes INTO TABLE mt_json_tree.

    ri_json = me.

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~set.

    DATA ls_split_path TYPE Lif_abapgit_ajson_types=>ty_path_name.
    DATA lr_parent TYPE REF TO Lif_abapgit_ajson_types=>ty_node.
    DATA ls_deleted_node TYPE Lif_abapgit_ajson_types=>ty_node.
    DATA lv_item_order TYPE Lif_abapgit_ajson_types=>ty_node-order.

    read_only_watchdog( ).

    ri_json = me.

    IF iv_val IS INITIAL AND iv_ignore_empty = abap_true AND iv_node_type IS INITIAL.
      RETURN. " nothing to assign
    ENDIF.

    IF iv_node_type IS NOT INITIAL
      AND iv_node_type <> Lif_abapgit_ajson_types=>node_type-boolean AND iv_node_type <> Lif_abapgit_ajson_types=>node_type-null
      AND iv_node_type <> Lif_abapgit_ajson_types=>node_type-number AND iv_node_type <> Lif_abapgit_ajson_types=>node_type-string.
      Lcx_abapgit_ajson_error=>raise( |Unexpected type { iv_node_type }| ).
    ENDIF.

    ls_split_path = SHRITEFUH64VYIPN5I4UHL45BCTR4A=>split_path( iv_path ).
    IF ls_split_path IS INITIAL. " Assign root, exceptional processing
      IF iv_node_type IS NOT INITIAL.
        mt_json_tree = SHRITEFUH64VYIPN5I4UHL45BC3R4A=>insert_with_type(
          is_opts            = ms_opts
          iv_data            = iv_val
          iv_type            = iv_node_type
          is_prefix          = ls_split_path
          ii_custom_mapping  = mi_custom_mapping ).
      ELSE.
        mt_json_tree = SHRITEFUH64VYIPN5I4UHL45BC3R4A=>convert(
          is_opts            = ms_opts
          iv_data            = iv_val
          is_prefix          = ls_split_path
          ii_custom_mapping  = mi_custom_mapping ).
      ENDIF.
      RETURN.
    ENDIF.

    " Ensure whole path exists
    lr_parent = prove_path_exists( ls_split_path-path ).
    ASSERT lr_parent IS NOT INITIAL.

    " delete if exists with subtree
    ls_deleted_node = delete_subtree(
      ir_parent = lr_parent
      iv_path   = ls_split_path-path
      iv_name   = ls_split_path-name ).
    lv_item_order = ls_deleted_node-order.

    " convert to json
    DATA lt_new_nodes TYPE Lif_abapgit_ajson_types=>ty_nodes_tt.
    DATA lv_array_index TYPE i.

    IF lr_parent->type = Lif_abapgit_ajson_types=>node_type-array.
      lv_array_index = SHRITEFUH64VYIPN5I4UHL45BCTR4A=>validate_array_index(
        iv_path  = ls_split_path-path
        iv_index = ls_split_path-name ).
    ELSEIF lr_parent->type = Lif_abapgit_ajson_types=>node_type-object
      AND lv_item_order = 0 AND ms_opts-keep_item_order = abap_true.
      lv_item_order = lr_parent->children + 1.
    ENDIF.

    IF iv_node_type IS NOT INITIAL.
      lt_new_nodes = SHRITEFUH64VYIPN5I4UHL45BC3R4A=>insert_with_type(
        is_opts            = ms_opts
        iv_item_order      = lv_item_order
        iv_data            = iv_val
        iv_type            = iv_node_type
        iv_array_index     = lv_array_index
        is_prefix          = ls_split_path
        ii_custom_mapping  = mi_custom_mapping ).
    ELSE.
      lt_new_nodes = SHRITEFUH64VYIPN5I4UHL45BC3R4A=>convert(
        is_opts            = ms_opts
        iv_item_order      = lv_item_order
        iv_data            = iv_val
        iv_array_index     = lv_array_index
        is_prefix          = ls_split_path
        ii_custom_mapping  = mi_custom_mapping ).
    ENDIF.

    " update nodes
    IF lines( lt_new_nodes ) > 0.
      lr_parent->children = lr_parent->children + 1.
      INSERT LINES OF lt_new_nodes INTO TABLE mt_json_tree.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~setx.

    DATA lv_path TYPE string.
    DATA lv_val TYPE string.
    DATA lv_int TYPE i.
    DATA lv_dec TYPE decfloat34.
    DATA lv_last TYPE i.

    IF iv_param IS INITIAL.
      ri_json = me.
      RETURN.
    ENDIF.

    SPLIT iv_param AT ':' INTO lv_path lv_val.
    CONDENSE lv_path.
    CONDENSE lv_val.

    IF lv_val IS INITIAL.
      ri_json = me.
      RETURN. " Hmm ? or empty string ? or null ?
    ENDIF.

    IF go_float_regex IS NOT BOUND.
      CREATE OBJECT go_float_regex EXPORTING pattern = '^([1-9][0-9]*|0)\.[0-9]+$'.
      " expects fractional, because ints are detected separately
    ENDIF.

    IF lv_val = 'null'.
      Lif_abapgit_ajson~set_null( lv_path ).
    ELSEIF lv_val = 'true'.
      Lif_abapgit_ajson~set_boolean(
        iv_path = lv_path
        iv_val  = abap_true ).
    ELSEIF lv_val = 'false'.
      Lif_abapgit_ajson~set_boolean(
        iv_path = lv_path
        iv_val  = abap_false ).
    ELSEIF lv_val CO '0123456789'.
      lv_int = lv_val.
      Lif_abapgit_ajson~set_integer(
        iv_path = lv_path
        iv_val  = lv_int ).
    ELSEIF lv_val CO '0123456789.' AND go_float_regex->create_matcher( text = lv_val )->match( ) = abap_true.
      lv_dec = lv_val.
      Lif_abapgit_ajson~set(
        iv_path = lv_path
        iv_val  = lv_dec ).
    ELSEIF lv_val+0(1) = '{' OR lv_val+0(1) = '['.
      "Expect object/array, but no further checks, parser will catch errors
      Lif_abapgit_ajson~set(
        iv_path = lv_path
        iv_val  = parse(
          iv_json = lv_val
          iv_keep_item_order = ms_opts-keep_item_order ) ).
    ELSE. " string
      lv_last = strlen( lv_val ) - 1.
      IF lv_val+0(1) = '"' AND lv_val+lv_last(1) = '"'.
        lv_val = substring(
          val = lv_val
          off = 1
          len = lv_last - 1 ).
      ENDIF.
      Lif_abapgit_ajson~set_string(
        iv_path = lv_path
        iv_val  = lv_val ).
    ENDIF.

    ri_json = me.

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~set_boolean.

    ri_json = me.

    DATA lv_bool TYPE abap_bool.
    lv_bool = boolc( iv_val IS NOT INITIAL ).
    Lif_abapgit_ajson~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_bool ).

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~set_date.

    ri_json = me.

    DATA lv_val TYPE string.
    lv_val = SHRITEFUH64VYIPN5I4UHL45BC3R4A=>format_date( iv_val ).

    Lif_abapgit_ajson~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_val ).
