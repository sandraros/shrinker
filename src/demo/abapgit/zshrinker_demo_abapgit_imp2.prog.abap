********************************************************************************
*
* LICENSE and NOTICE
*
* See include program ZSHRINKER_DEMO_ABAPGIT_LICENSE
*
********************************************************************************
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
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
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

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESWB_EO'
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_ADT_LINK=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
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
endclass. "ZCL_ABAPGIT_OBJECT_DDLX implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_DEVC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_devc=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_devc=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_DEVC implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    super->constructor( is_item     = is_item
                        iv_language = iv_language ).
    IF is_item-devclass IS NOT INITIAL.
      mv_local_devclass = is_item-devclass.
    ELSE.
      mv_local_devclass = is_item-obj_name.
    ENDIF.
  ENDMETHOD.
  METHOD get_package.
    IF Lif_abapgit_object~exists( ) = abap_true.
      ri_package = load_package( mv_local_devclass ).
    ENDIF.
  ENDMETHOD.
  METHOD is_empty.

    DATA: lv_object_name TYPE tadir-obj_name,
          lt_subpackages TYPE Lif_abapgit_sap_package=>ty_devclass_tt.

    lt_subpackages = Lcl_abapgit_factory=>get_sap_package( iv_package_name )->list_subpackages( ).

    IF lines( lt_subpackages ) > 0.
      rv_is_empty = abap_false.
      RETURN.
    ENDIF.

    " Ignore the SOTR if is linked to the current SAP package (DEVC)
    SELECT SINGLE obj_name
           FROM tadir
           INTO lv_object_name
           WHERE pgmid = 'R3TR'
           AND NOT ( ( object = 'DEVC' OR object = 'SOTR' ) AND obj_name = iv_package_name )
           AND devclass = iv_package_name.
    rv_is_empty = boolc( sy-subrc <> 0 ).

  ENDMETHOD.
  METHOD is_local.

    DATA lv_dlvunit TYPE tdevc-dlvunit.

    SELECT SINGLE dlvunit FROM tdevc INTO lv_dlvunit
        WHERE devclass = iv_package_name AND intsys <> 'SAP'.
    IF sy-subrc = 0 AND lv_dlvunit = 'LOCAL'.
      rv_is_local = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD load_package.

    cl_package_factory=>load_package(
      EXPORTING
        i_package_name             = iv_package_name
        i_force_reload             = abap_true
      IMPORTING
        e_package                  = ri_package
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        no_access                  = 4
        object_locked_and_modified = 5
        OTHERS                     = 6 ).
    IF sy-subrc = 1.
      RETURN.
    ELSEIF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD remove_obsolete_tadir.

    DATA:
      lv_pack  TYPE devclass,
      lt_pack  TYPE STANDARD TABLE OF devclass,
      ls_tadir TYPE Lif_abapgit_definitions=>ty_tadir,
      lt_tadir TYPE Lif_abapgit_definitions=>ty_tadir_tt,
      ls_item  TYPE Lif_abapgit_definitions=>ty_item.

    " TADIR entries must remain for transportable packages
    IF is_local( iv_package_name ) = abap_false.
      RETURN.
    ENDIF.

    " Clean-up sub packages first
    SELECT devclass FROM tdevc INTO TABLE lt_pack
      WHERE parentcl = iv_package_name
      ORDER BY PRIMARY KEY.

    LOOP AT lt_pack INTO lv_pack.
      remove_obsolete_tadir( lv_pack ).
    ENDLOOP.

    " Remove TADIR entries for objects that do not exist anymore
    SELECT * FROM tadir INTO CORRESPONDING FIELDS OF TABLE lt_tadir
      WHERE devclass = iv_package_name
      ORDER BY PRIMARY KEY ##TOO_MANY_ITAB_FIELDS.

    LOOP AT lt_tadir INTO ls_tadir.
      ls_item-obj_type = ls_tadir-object.
      ls_item-obj_name = ls_tadir-obj_name.

      IF Lcl_abapgit_objects=>exists( ls_item ) = abap_false.
        CALL FUNCTION 'TR_TADIR_INTERFACE'
          EXPORTING
            wi_delete_tadir_entry = abap_true
            wi_tadir_pgmid        = 'R3TR'
            wi_tadir_object       = ls_tadir-object
            wi_tadir_obj_name     = ls_tadir-obj_name
            wi_test_modus         = abap_false
          EXCEPTIONS
            OTHERS                = 1 ##FM_SUBRC_OK.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD set_lock.

    DATA: lv_changeable TYPE abap_bool.

    ii_package->get_changeable( IMPORTING e_changeable = lv_changeable ).
    IF lv_changeable <> iv_lock.
      TRY.
          CALL METHOD ii_package->('SET_CHANGEABLE')
            EXPORTING
              i_changeable                = iv_lock
              i_suppress_dialog           = abap_true " Parameter missing in 702
            EXCEPTIONS
              object_locked_by_other_user = 1
              permission_failure          = 2
              object_already_changeable   = 3
              object_already_unlocked     = 4
              object_just_created         = 5
              object_deleted              = 6
              object_modified             = 7
              object_not_existing         = 8
              object_invalid              = 9
              unexpected_error            = 10
              OTHERS                      = 11.
        CATCH cx_sy_dyn_call_param_not_found.
          ii_package->set_changeable(
            EXPORTING
              i_changeable                = iv_lock
            EXCEPTIONS
              object_locked_by_other_user = 1
              permission_failure          = 2
              object_already_changeable   = 3
              object_already_unlocked     = 4
              object_just_created         = 5
              object_deleted              = 6
              object_modified             = 7
              object_not_existing         = 8
              object_invalid              = 9
              unexpected_error            = 10
              OTHERS                      = 11 ).
      ENDTRY.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

    TRY.
        CALL METHOD ii_package->('SET_PERMISSIONS_CHANGEABLE')
          EXPORTING
            i_changeable                = iv_lock
            i_suppress_dialog           = abap_true " Parameter missing in 702
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
            OTHERS                      = 10.
      CATCH cx_sy_dyn_call_param_not_found.
        ii_package->set_permissions_changeable(
          EXPORTING
            i_changeable                = iv_lock
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
    ENDTRY.
    IF ( sy-subrc = 1 AND iv_lock = abap_true ) OR ( sy-subrc = 2 AND iv_lock = abap_false ).
      " There's no getter to find out beforehand...
    ELSEIF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD unlock_and_raise_error.

    DATA ls_msg TYPE bal_s_msg.

    " Remember message since unlock overwrites it (for example with XT465)
    MOVE-CORRESPONDING sy TO ls_msg.

    set_lock( ii_package = ii_package
              iv_lock    = abap_false ).

    Lcx_abapgit_exception=>raise_t100(
      iv_msgid = ls_msg-msgid
      iv_msgno = ls_msg-msgno
      iv_msgv1 = ls_msg-msgv1
      iv_msgv2 = ls_msg-msgv2
      iv_msgv3 = ls_msg-msgv3
      iv_msgv4 = ls_msg-msgv4 ).

  ENDMETHOD.
  METHOD update_pinf_usages.
    DATA: lt_current_permissions TYPE tpak_permission_to_use_list,
          li_usage               TYPE REF TO if_package_permission_to_use,
          ls_data_sign           TYPE scomppsign,
          ls_add_permission_data TYPE pkgpermdat,
          lt_handled             TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.
    FIELD-SYMBOLS: <ls_usage_data> LIKE LINE OF it_usage_data.

    " Get the current permissions
    ii_package->get_permissions_to_use(
      IMPORTING
        e_permissions    = lt_current_permissions
      EXCEPTIONS
        object_invalid   = 1
        unexpected_error = 2
        OTHERS           = 3 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    ls_data_sign-err_sever = abap_true.

    " New permissions
    LOOP AT it_usage_data ASSIGNING <ls_usage_data>.
      READ TABLE lt_current_permissions
           WITH KEY table_line->package_interface_name = <ls_usage_data>-intf_name
           INTO li_usage.

      IF sy-subrc = 0 AND li_usage IS BOUND.
        INSERT sy-tabix INTO TABLE lt_handled.

        " Permission already exists, update attributes
        li_usage->set_all_attributes(
          EXPORTING
            i_permission_data     = <ls_usage_data>
            i_data_sign           = ls_data_sign
          EXCEPTIONS
            object_not_changeable = 1
            object_invalid        = 2
            intern_err            = 3
            OTHERS                = 4 ).
        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise_t100( ).
        ENDIF.

      ELSE.
        " Permission does not exist yet, add it
        MOVE-CORRESPONDING <ls_usage_data> TO ls_add_permission_data.
        ii_package->add_permission_to_use(
          EXPORTING
            i_pkg_permission_data   = ls_add_permission_data
          EXCEPTIONS
            object_not_changeable   = 1
            object_access_error     = 2
            object_already_existing = 3
            object_invalid          = 4
            unexpected_error        = 5
            OTHERS                  = 6 ).
        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise_t100( ).
        ENDIF.

      ENDIF.

      FREE li_usage.
    ENDLOOP.

    " Delete missing usages
    LOOP AT lt_current_permissions INTO li_usage.
      READ TABLE lt_handled WITH TABLE KEY table_line = sy-tabix TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      li_usage->delete(
        EXCEPTIONS
          object_not_changeable = 1
          object_invalid        = 2
*          deletion_not_allowed  = 3 downport, does not exist in 7.30
          intern_err            = 4
          OTHERS                = 5 ).
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    DATA li_package TYPE REF TO if_package.

    li_package = get_package( ).
    IF li_package IS BOUND.
      rv_user = li_package->changed_by.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: li_package TYPE REF TO if_package,
          lv_package TYPE devclass.

    " Package deletion is a bit tricky. A package can only be deleted if there are no objects
    " contained in it. This includes subpackages, so first the leaf packages need to be deleted.
    " Unfortunately deleted objects that are still contained in an unreleased transport request
    " also count towards the contained objects counter.
    " -> Currently we delete only empty packages
    "
    " If objects are deleted, the TADIR entry is deleted when the transport request is released.
    " So before we can delete the package, the transport which deletes the objects
    " in the package has to be released.

    lv_package = ms_item-obj_name.

    " Remove remaining OTR entries
    Lcl_abapgit_sotr_handler=>delete_sotr_package( iv_package ).

    remove_obsolete_tadir( lv_package ).

    IF is_empty( lv_package ) = abap_true.

      li_package = load_package( lv_package ).

      IF li_package IS NOT BOUND.
        RETURN.
      ENDIF.

      IF lv_package(1) = '$'.
        Lcl_abapgit_persist_packages=>get_instance( )->modify( lv_package ).
      ENDIF.

      set_lock( ii_package = li_package
                iv_lock    = abap_true ).

      TRY.
          CALL METHOD li_package->('DELETE')
            EXPORTING
              i_suppress_dialog     = abap_true  " Parameter missing in 702
            EXCEPTIONS
              object_not_empty      = 1
              object_not_changeable = 2
              object_invalid        = 3
              intern_err            = 4
              OTHERS                = 5.

        CATCH cx_sy_dyn_call_param_not_found.

          li_package->delete(
            EXCEPTIONS
              object_not_empty      = 1
              object_not_changeable = 2
              object_invalid        = 3
              intern_err            = 4
              OTHERS                = 5 ).

      ENDTRY.

      IF sy-subrc <> 0.
        unlock_and_raise_error( li_package ).
      ENDIF.

      TRY.
          CALL METHOD li_package->('SAVE')
            EXPORTING
              i_suppress_dialog     = abap_true
            EXCEPTIONS
              object_invalid        = 1
              object_not_changeable = 2
              cancelled_in_corr     = 3
              permission_failure    = 4
              unexpected_error      = 5
              intern_err            = 6
              OTHERS                = 7.

        CATCH cx_sy_dyn_call_param_not_found.

          li_package->save(
            EXCEPTIONS
              object_invalid        = 1
              object_not_changeable = 2
              cancelled_in_corr     = 3
              permission_failure    = 4
              unexpected_error      = 5
              intern_err            = 6
              OTHERS                = 7 ).

      ENDTRY.

      IF sy-subrc <> 0.
        unlock_and_raise_error( li_package ).
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: li_package      TYPE REF TO if_package,
          ls_package_data TYPE scompkdtln,
          ls_data_sign    TYPE scompksign,
          lt_usage_data   TYPE scomppdata,
          ls_save_sign    TYPE paksavsign.

    FIELD-SYMBOLS: <ls_usage_data> TYPE scomppdtln.
    FIELD-SYMBOLS: <lg_field> TYPE any.

    mv_local_devclass = iv_package.

    io_xml->read(
      EXPORTING
        iv_name = 'DEVC'
      CHANGING
        cg_data = ls_package_data ).

    IF mv_local_devclass(1) = '$'.
      IF ls_package_data-mainpack = 'X'.
        Lcx_abapgit_exception=>raise( |Main package { iv_package } cannot be used in local package| ).
      ELSEIF ls_package_data-mainpack = 'S'.
        Lcx_abapgit_exception=>raise( |Structure package { iv_package } cannot be used in local package| ).
      ENDIF.
    ENDIF.

    li_package = get_package( ).

    " Swap out repository package name with the local installation package name
    ls_package_data-devclass = mv_local_devclass.
    IF li_package IS BOUND.
      ls_package_data-pdevclass = li_package->transport_layer.
    ENDIF.

    " For local packages store application component
    IF ls_package_data-devclass(1) = '$'.
      Lcl_abapgit_persist_packages=>get_instance( )->modify(
        iv_package    = ls_package_data-devclass
        iv_component  = ls_package_data-component
        iv_comp_posid = ls_package_data-comp_posid ).
    ENDIF.

    " Parent package is not changed. Assume the folder logic already created the package and set
    " the hierarchy before.
    CLEAR ls_package_data-parentcl.

    ASSIGN COMPONENT 'PACKKIND' OF STRUCTURE ls_package_data TO <lg_field>.
    IF sy-subrc = 0.
      set_abap_language_version( CHANGING cv_abap_language_version = <lg_field> ).
    ENDIF.
    ASSIGN COMPONENT 'PACKKIND' OF STRUCTURE ls_data_sign TO <lg_field>.
    IF sy-subrc = 0.
      <lg_field> = abap_true.
    ENDIF.

* Fields not set:
* korrflag
* dlvunit
* parentcl
* cli_check
* intprefx
    ls_data_sign-ctext            = abap_true.
    ls_data_sign-as4user          = abap_true.
    ls_data_sign-pdevclass        = abap_true.
    ls_data_sign-comp_posid       = abap_true.
    ls_data_sign-component        = abap_true.
    ls_data_sign-perminher        = abap_true.
    ls_data_sign-packtype         = abap_true.
    ls_data_sign-restricted       = abap_true.
    ls_data_sign-mainpack         = abap_true.
    ls_data_sign-srv_check        = abap_true.
    ls_data_sign-ext_alias        = abap_true.
    ls_data_sign-project_guid     = abap_true.
    ls_data_sign-project_id       = abap_true.
    ls_data_sign-project_passdown = abap_true.

    IF ls_package_data-ctext IS INITIAL.
      ls_package_data-ctext = mv_local_devclass.
    ENDIF.
    IF ls_package_data-dlvunit IS INITIAL.
      ls_package_data-dlvunit = 'HOME'.
    ENDIF.

    ls_package_data-as4user = sy-uname.

    IF li_package IS BOUND.
      " Package already exists, change it
      set_lock( ii_package = li_package
                iv_lock    = abap_true ).

      li_package->set_all_attributes(
        EXPORTING
          i_package_data             = ls_package_data
          i_data_sign                = ls_data_sign
        EXCEPTIONS
          object_not_changeable      = 1
          object_deleted             = 2
          object_invalid             = 3
          short_text_missing         = 4
          author_not_existing        = 5
          local_package              = 6
          software_component_invalid = 7
          layer_invalid              = 8
          korrflag_invalid           = 9
          component_not_existing     = 10
          component_missing          = 11
          authorize_failure          = 12
          prefix_in_use              = 13
          unexpected_error           = 14
          intern_err                 = 15
*          wrong_mainpack_value       = 16  downport, does not exist in 7.30
*          superpackage_invalid       = 17  downport, does not exist in 7.30
          OTHERS                     = 18 ).
      IF sy-subrc <> 0.
        unlock_and_raise_error( li_package ).
      ENDIF.

    ELSE.
      " Package does not exist yet, create it
      " This shouldn't really happen, because the folder logic initially creates the packages.
      cl_package_factory=>create_new_package(
        IMPORTING
          e_package                  = li_package
        CHANGING
          c_package_data             = ls_package_data
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
*          invalid_translation_depth  = 18 downport, does not exist in 7.30
*          wrong_mainpack_value       = 19 downport, does not exist in 7.30
*          superpackage_invalid       = 20 downport, does not exist in 7.30
*          error_in_cts_checks        = 21 downport, does not exist in 7.31
          OTHERS                     = 22 ).
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

    " Load package interface usages
    TRY.
        io_xml->read(
          EXPORTING
            iv_name = 'PERMISSION'
          CHANGING
            cg_data = lt_usage_data ).
      CATCH Lcx_abapgit_exception ##NO_HANDLER.
        " No permissions saved
    ENDTRY.

    LOOP AT lt_usage_data ASSIGNING <ls_usage_data>.
      <ls_usage_data>-client_pak = mv_local_devclass.
    ENDLOOP.

    update_pinf_usages( ii_package    = li_package
                        it_usage_data = lt_usage_data ).

    ls_save_sign-pack   = abap_true.
    ls_save_sign-permis = abap_true.
    ls_save_sign-elems  = abap_true.
    ls_save_sign-interf = abap_true.

    li_package->save_generic(
      EXPORTING
        i_save_sign           = ls_save_sign
        i_transport_request   = iv_transport
        i_suppress_dialog     = abap_true
      EXCEPTIONS
        cancelled_in_corr     = 1
        permission_failure    = 2
        object_not_changeable = 3
        object_invalid        = 4
        OTHERS                = 5 ).
    IF sy-subrc <> 0.
      unlock_and_raise_error( li_package ).
    ENDIF.

    set_lock( ii_package = li_package
              iv_lock    = abap_false ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.
    " Check remote package if deserialize has not been called before this
    IF mv_local_devclass IS INITIAL.
      rv_bool = abap_false.
    ELSE.
      cl_package_helper=>check_package_existence(
        EXPORTING
          i_package_name          = mv_local_devclass
        IMPORTING
          e_package_exists        = rv_bool
        EXCEPTIONS
          intern_err              = 1
          OTHERS                  = 2 ).
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.
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
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'EEUDB'
                                            iv_argument    = ms_item-obj_name
                                            iv_prefix      = 'DV' ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.

    IF iv_filename <> Lcl_abapgit_filename_logic=>c_package_file.
      Lcx_abapgit_exception=>raise( |Unexpected filename for package { cs_item-obj_name }| ).
    ENDIF.

    " Try to get a unique package name for DEVC by using the path
    cs_item-obj_name = Lcl_abapgit_folder_logic=>get_instance( )->path_to_package(
      iv_top                  = iv_package
      io_dot                  = io_dot
      iv_create_if_not_exists = abap_false
      iv_path                 = iv_path ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.

    " Packages have a fixed filename so that the repository can be installed to a different
    " package(-hierarchy) on the client and not show up as a different package in the repo.
    cv_filename = Lcl_abapgit_filename_logic=>c_package_file.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.
    DATA: ls_package_data TYPE scompkdtln,
          ls_package_comp TYPE Lcl_abapgit_persist_packages=>ty_package,
          li_package      TYPE REF TO if_package,
          lt_intf_usages  TYPE tpak_permission_to_use_list,
          lt_usage_data   TYPE scomppdata,
          ls_usage_data   TYPE scomppdtln,
          li_usage        TYPE REF TO if_package_permission_to_use.

    FIELD-SYMBOLS: <lg_field> TYPE any.


    li_package = get_package( ).
    IF li_package IS NOT BOUND.
      Lcx_abapgit_exception=>raise( |Could not find package to serialize.| ).
    ENDIF.

    li_package->get_all_attributes(
      IMPORTING
        e_package_data  = ls_package_data
      EXCEPTIONS
        object_invalid  = 1
        package_deleted = 2
        intern_err      = 3
        OTHERS          = 4 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    " For local packages get application component
    IF is_local( ls_package_data-devclass ) = abap_true.
      ls_package_comp = Lcl_abapgit_persist_packages=>get_instance( )->read( ls_package_data-devclass ).
      ls_package_data-component  = ls_package_comp-component.
      ls_package_data-comp_posid = ls_package_comp-comp_posid.
    ENDIF.

    CLEAR: ls_package_data-devclass,
           ls_package_data-parentcl.

    " Clear administrative data to prevent diffs
    CLEAR: ls_package_data-created_by,
           ls_package_data-created_on,
           ls_package_data-changed_by,
           ls_package_data-changed_on,
           ls_package_data-as4user.

    " Clear text descriptions that might be localized
    CLEAR: ls_package_data-comp_text,
           ls_package_data-dlvu_text,
           ls_package_data-layer_text.

    " Clear obsolete fields
    CLEAR: ls_package_data-intfprefx,
           ls_package_data-cli_check.

    ASSIGN COMPONENT 'TRANSLATION_DEPTH_TEXT'
           OF STRUCTURE ls_package_data
           TO <lg_field>.
    IF sy-subrc = 0.
      CLEAR: <lg_field>.
    ENDIF.

    ASSIGN COMPONENT 'TRANSLATION_GRAPH_DEPTH_TEXT'
           OF STRUCTURE ls_package_data
           TO <lg_field>.
    IF sy-subrc = 0.
      CLEAR: <lg_field>.
    ENDIF.

    " Clear things related to local installation package
    CLEAR: ls_package_data-namespace,
           ls_package_data-dlvunit,
           ls_package_data-tpclass,
           ls_package_data-pdevclass.

    " Not usable on customer systems
    ASSIGN COMPONENT 'TRANSLATION_DEPTH'
           OF STRUCTURE ls_package_data
           TO <lg_field>.
    IF sy-subrc = 0.
      CLEAR: <lg_field>.
    ENDIF.

    ASSIGN COMPONENT 'TRANSLATION_GRAPH_DEPTH'
           OF STRUCTURE ls_package_data
           TO <lg_field>.
    IF sy-subrc = 0.
      CLEAR: <lg_field>.
    ENDIF.

    CLEAR: ls_package_data-korrflag.

    ASSIGN COMPONENT 'PACKKIND' OF STRUCTURE ls_package_data TO <lg_field>.
    IF sy-subrc = 0.
      clear_abap_language_version( CHANGING cv_abap_language_version = <lg_field> ).
    ENDIF.

    io_xml->add( iv_name = 'DEVC'
                 ig_data = ls_package_data ).

    " Save package interface usages
    li_package->get_permissions_to_use(
      IMPORTING
        e_permissions    = lt_intf_usages
      EXCEPTIONS
        object_invalid   = 1
        unexpected_error = 2
        OTHERS           = 3 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    LOOP AT lt_intf_usages INTO li_usage.
      li_usage->get_all_attributes(
        IMPORTING
          e_permission_data = ls_usage_data
        EXCEPTIONS
          object_invalid    = 1
          intern_err        = 2
          OTHERS            = 3 ).
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      CLEAR: ls_usage_data-pack_name, ls_usage_data-client_pak.

      APPEND ls_usage_data TO lt_usage_data.
    ENDLOOP.

    IF lt_usage_data IS NOT INITIAL.
      io_xml->add( iv_name = 'PERMISSION'
                   ig_data = lt_usage_data ).
    ENDIF.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_DEVC implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_DIAL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_dial=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_dial=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_DIAL implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.
    rv_user = c_user_unknown. " not stored by SAP
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: ls_bcdata TYPE bdcdata,
          lt_bcdata TYPE STANDARD TABLE OF bdcdata.

    ls_bcdata-program  = 'SAPMSDIA'.
    ls_bcdata-dynpro   = '1010'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam     = 'DIAPAR-DNAM'.
    ls_bcdata-fval     = ms_item-obj_name.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam     = 'RS38L-PARM'.
    ls_bcdata-fval     = abap_true.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=DELF'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-program  = 'SAPLSPO1'.
    ls_bcdata-dynpro   = '0100'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=YES'.
    APPEND ls_bcdata TO lt_bcdata.

    ls_bcdata-program  = 'SAPMSDIA'.
    ls_bcdata-dynpro   = '1010'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=BACK'.
    APPEND ls_bcdata TO lt_bcdata.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode      = 'SE35'
      it_bdcdata    = lt_bcdata
      iv_new_window = abap_false ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_dialog_module TYPE ty_dialog_module.

    io_xml->read(
      EXPORTING
        iv_name = 'DIAL'
      CHANGING
        cg_data = ls_dialog_module ).

    CALL FUNCTION 'RS_DIALOG_CREATE'
      EXPORTING
        dialogname            = ls_dialog_module-tdct-dnam
        dynpronumber          = ls_dialog_module-tdct-dynr
        programname           = ls_dialog_module-tdct-prog
        suppress_corr_check   = abap_false
*     It seems that dia_par parameter doesn't do anything, but we can't omit it
*     Parameters are inserted below
      TABLES
        dia_par               = ls_dialog_module-dia_pars
      EXCEPTIONS
        dialog_already_exists = 1
        invalid_name          = 2
        OTHERS                = 3.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error deserializing dialogmodule { ms_item-obj_name }| ).
    ENDIF.

    " It seems that there's no API for diapar, therefore we manipulate it directly
    INSERT diapar FROM TABLE ls_dialog_module-dia_pars.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: ls_tdct TYPE tdct.

    ls_tdct = _read_tdct( ).

    rv_bool = boolc( ls_tdct IS NOT INITIAL ).

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

    DATA: lv_objectname TYPE tdct-dnam.

    lv_objectname = ms_item-obj_name.

    CALL FUNCTION 'RS_DIALOG_SHOW'
      EXPORTING
        objectname       = lv_objectname
        type             = 'VW'
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.

    rv_exit = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA ls_dialog_module TYPE ty_dialog_module.

    ls_dialog_module-tdct = _read_tdct( ).

    SELECT * FROM diapar
      INTO TABLE ls_dialog_module-dia_pars
      WHERE dnam = ls_dialog_module-tdct-dnam
      ORDER BY PRIMARY KEY.

    io_xml->add( iv_name = 'DIAL'
                 ig_data = ls_dialog_module ).

  ENDMETHOD.
  METHOD _read_tdct.

    DATA: lv_dnam TYPE tdct-dnam.

    lv_dnam = ms_item-obj_name.

    SELECT SINGLE * FROM tdct
           INTO rs_tdct
           WHERE dnam = lv_dnam.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_DIAL implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_DOCT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_doct=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_doct=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_DOCT implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor(
        is_item     = is_item
        iv_language = iv_language ).

    mi_longtexts = Lcl_abapgit_factory=>get_longtexts( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    rv_user = mi_longtexts->changed_by(
                  iv_object_name = ms_item-obj_name
                  iv_longtext_id = c_id ).

    IF rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    mi_longtexts->delete(
        iv_object_name = ms_item-obj_name
        iv_longtext_id = c_id ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    mi_longtexts->deserialize(
      iv_longtext_name = c_name
      iv_object_name   = ms_item-obj_name
      iv_longtext_id   = c_id
      ii_xml           = io_xml
      iv_main_language = mv_language ).

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_id     TYPE dokil-id,
          lv_object TYPE dokhl-object.


    lv_object = ms_item-obj_name.

    SELECT SINGLE id FROM dokil INTO lv_id
      WHERE id         = c_id
        AND object     = lv_object.                     "#EC CI_GENBUFF

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
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: ls_dokentry TYPE dokentry,
          ls_bcdata   TYPE bdcdata,
          lt_bcdata   TYPE STANDARD TABLE OF bdcdata.

    " We need to modify dokentry directly, otherwise
    " Batch Input on SE61 wouldn't work because it stores
    " the last seen Document Class in this table. There's
    " no standard function to do this. SE61 does this
    " directly in its dialog modules
    ls_dokentry-username = sy-uname.
    ls_dokentry-langu    = mv_language.
    ls_dokentry-class    = c_id.
    MODIFY dokentry FROM ls_dokentry.

    ls_bcdata-program  = 'SAPMSDCU'.
    ls_bcdata-dynpro   = '0100'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam     = 'RSDCU-OBJECT7'.
    ls_bcdata-fval     = ms_item-obj_name.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=SHOW'.
    APPEND ls_bcdata TO lt_bcdata.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SE61'
      it_bdcdata = lt_bcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    mi_longtexts->serialize(
        iv_longtext_name = c_name
        iv_object_name = ms_item-obj_name
        iv_longtext_id = c_id
        io_i18n_params = mo_i18n_params
        ii_xml         = io_xml ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_DOCT implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_DOCV <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_docv=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_docv=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_DOCV implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    DATA: lv_prefix    TYPE namespace,
          lv_bare_name TYPE progname.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    IF ms_item-obj_name(2) <> 'DT'. " IN, MO, UO, UP
      mv_id         = ms_item-obj_name(2).
      mv_doc_object = ms_item-obj_name+2.
    ELSE. " DT
      CALL FUNCTION 'RS_NAME_SPLIT_NAMESPACE'
        EXPORTING
          name_with_namespace    = ms_item-obj_name
        IMPORTING
          namespace              = lv_prefix
          name_without_namespace = lv_bare_name
        EXCEPTIONS
          delimiter_error        = 1
          OTHERS                 = 2.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Error determining namespace for { ms_item-obj_type } { ms_item-obj_name }| ).
      ENDIF.

      mv_id         = lv_bare_name(2).
      mv_doc_object = |{ lv_prefix }{ lv_bare_name+2 }|.
    ENDIF.

  ENDMETHOD.
  METHOD read.

    CALL FUNCTION 'DOCU_READ'
      EXPORTING
        id       = mv_id
        langu    = mv_language
        object   = mv_doc_object
        typ      = c_typ
        version  = c_version
      IMPORTING
        doktitle = rs_data-doctitle
        head     = rs_data-head
      TABLES
        line     = rs_data-lines.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    rv_user = read( )-head-tdluser.
    IF rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    CALL FUNCTION 'DOCU_DEL'
      EXPORTING
        id       = mv_id
        langu    = mv_language
        object   = mv_doc_object
        typ      = c_typ
      EXCEPTIONS
        ret_code = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_data TYPE ty_data.


    io_xml->read( EXPORTING iv_name = c_name
                  CHANGING cg_data = ls_data ).

    CALL FUNCTION 'DOCU_UPDATE'
      EXPORTING
        head    = ls_data-head
        state   = 'A'
        typ     = c_typ
        version = c_version
      TABLES
        line    = ls_data-lines.

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    SELECT SINGLE id FROM dokil INTO mv_id
      WHERE id     = mv_id
        AND object = mv_doc_object.                     "#EC CI_GENBUFF

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
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_data   TYPE ty_data.


    ls_data = read( ).

    CLEAR: ls_data-head-tdfuser,
           ls_data-head-tdfreles,
           ls_data-head-tdfdate,
           ls_data-head-tdftime,
           ls_data-head-tdluser,
           ls_data-head-tdlreles,
           ls_data-head-tdldate,
           ls_data-head-tdltime.

    io_xml->add( iv_name = c_name
                 ig_data = ls_data ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_DOCV implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_DOMA <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_doma=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_doma=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_DOMA implementation.
*"* method's implementations
*include methods.
  METHOD adjust_exit.

    DATA lv_function TYPE funcname.

    IF cv_exit IS NOT INITIAL.
      lv_function = |CONVERSION_EXIT_{ cv_exit }_INPUT|.

      " If exit function does not exist, remove it
      IF Lcl_abapgit_factory=>get_function_module( )->function_exists( lv_function ) = abap_false.
        cv_exit = ''.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD check_exit.

    DATA lv_exit TYPE dd01v-convexit.

    rv_done = abap_true.

    IF iv_exit IS NOT INITIAL.
      " Check if exit function is set correctly
      SELECT SINGLE convexit FROM dd01v INTO lv_exit WHERE domname = ms_item-obj_name.
      IF sy-subrc = 0 AND lv_exit <> iv_exit.
        rv_done = abap_false.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD deserialize_texts.

    DATA: lv_name       TYPE ddobjname,
          lv_valpos     TYPE valpos,
          ls_dd01v_tmp  TYPE dd01v,
          lt_dd07v_tmp  TYPE TABLE OF dd07v,
          lt_i18n_langs TYPE TABLE OF langu,
          lt_dd01_texts TYPE ty_dd01_texts,
          lt_dd07_texts TYPE ty_dd07_texts.

    FIELD-SYMBOLS: <lv_lang>      LIKE LINE OF lt_i18n_langs,
                   <ls_dd07v>     LIKE LINE OF it_dd07v,
                   <ls_dd01_text> LIKE LINE OF lt_dd01_texts,
                   <ls_dd07_text> LIKE LINE OF lt_dd07_texts.

    lv_name = ms_item-obj_name.

    ii_xml->read( EXPORTING iv_name = 'I18N_LANGS'
                  CHANGING  cg_data = lt_i18n_langs ).

    ii_xml->read( EXPORTING iv_name = 'DD01_TEXTS'
                  CHANGING  cg_data = lt_dd01_texts ).

    ii_xml->read( EXPORTING iv_name = 'DD07_TEXTS'
                  CHANGING  cg_data = lt_dd07_texts ).

    mo_i18n_params->trim_saplang_list( CHANGING ct_sap_langs = lt_i18n_langs ).

    SORT lt_i18n_langs.
    SORT lt_dd07_texts BY ddlanguage. " Optimization

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.

      " Domain description
      ls_dd01v_tmp = is_dd01v.
      READ TABLE lt_dd01_texts ASSIGNING <ls_dd01_text> WITH KEY ddlanguage = <lv_lang>.
      IF sy-subrc > 0.
        Lcx_abapgit_exception=>raise( |DD01_TEXTS cannot find lang { <lv_lang> } in XML| ).
      ENDIF.
      MOVE-CORRESPONDING <ls_dd01_text> TO ls_dd01v_tmp.

      " Domain values
      lt_dd07v_tmp = it_dd07v.
      LOOP AT lt_dd07v_tmp ASSIGNING <ls_dd07v>.
        lv_valpos = <ls_dd07v>-valpos.
        " it_dd07v was potentially renumbered so lookup by value
        READ TABLE lt_dd07_texts ASSIGNING <ls_dd07_text>
          WITH KEY ddlanguage = <lv_lang> domvalue_l = <ls_dd07v>-domvalue_l domvalue_h = <ls_dd07v>-domvalue_h.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING <ls_dd07_text> TO <ls_dd07v>.
          <ls_dd07v>-valpos = lv_valpos.
          DELETE lt_dd07_texts INDEX sy-tabix. " Optimization
        ELSE.
          " no translation -> keep entry but clear texts
          <ls_dd07v>-ddlanguage = <lv_lang>.
          CLEAR: <ls_dd07v>-ddtext, <ls_dd07v>-domval_ld, <ls_dd07v>-domval_hd.
        ENDIF.
      ENDLOOP.

      CALL FUNCTION 'DDIF_DOMA_PUT'
        EXPORTING
          name              = lv_name
          dd01v_wa          = ls_dd01v_tmp
        TABLES
          dd07v_tab         = lt_dd07v_tmp
        EXCEPTIONS
          doma_not_found    = 1
          name_inconsistent = 2
          doma_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD handle_dependencies.

    " For domains with dependency on conversion exit function, we use two phases:
    " 1) DDIC phase:
    "    - If function does not exit, remove the exit function
    " 2) LATE phase
    "    - If function was removed, change it to the correct exit function
    CASE iv_step.
      WHEN Lif_abapgit_object=>gc_step_id-ddic.
        adjust_exit( CHANGING cv_exit = cv_exit ).

      WHEN Lif_abapgit_object=>gc_step_id-late.
        cv_done = check_exit( cv_exit ).

      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.
  METHOD serialize_texts.

    DATA: lv_name            TYPE ddobjname,
          lv_index           TYPE i,
          ls_dd01v           TYPE dd01v,
          lt_dd07v           TYPE TABLE OF dd07v,
          lt_i18n_langs      TYPE TABLE OF langu,
          lt_dd01_texts      TYPE ty_dd01_texts,
          lt_dd07_texts      TYPE ty_dd07_texts,
          lt_language_filter TYPE Lif_abapgit_environment=>ty_system_language_filter.

    FIELD-SYMBOLS: <lv_lang>      LIKE LINE OF lt_i18n_langs,
                   <ls_dd07v>     LIKE LINE OF lt_dd07v,
                   <ls_dd07v_tmp> LIKE LINE OF lt_dd07v,
                   <ls_dd01_text> LIKE LINE OF lt_dd01_texts,
                   <ls_dd07_text> LIKE LINE OF lt_dd07_texts.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      RETURN.
    ENDIF.

    lv_name = ms_item-obj_name.

    " Collect additional languages, skip main lang - it was serialized already
    lt_language_filter = mo_i18n_params->build_language_filter( ).

    SELECT DISTINCT ddlanguage AS langu INTO TABLE lt_i18n_langs
      FROM dd01v
      WHERE domname = lv_name
      AND ddlanguage IN lt_language_filter
      AND ddlanguage <> mv_language.                      "#EC CI_SUBRC

    SELECT DISTINCT ddlanguage AS langu APPENDING TABLE lt_i18n_langs
      FROM dd07v
      WHERE domname = lv_name
      AND ddlanguage IN lt_language_filter
      AND ddlanguage <> mv_language.                      "#EC CI_SUBRC

    SORT lt_i18n_langs.
    DELETE ADJACENT DUPLICATES FROM lt_i18n_langs.

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.
      lv_index = sy-tabix.

      CALL FUNCTION 'DDIF_DOMA_GET'
        EXPORTING
          name          = lv_name
          langu         = <lv_lang>
        IMPORTING
          dd01v_wa      = ls_dd01v
        TABLES
          dd07v_tab     = lt_dd07v
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        DELETE lt_i18n_langs INDEX lv_index. " Don't save this lang
        CONTINUE.
      ENDIF.

      IF ls_dd01v-ddlanguage IS INITIAL.
        ls_dd01v-ddlanguage = <lv_lang>.
      ENDIF.

      APPEND INITIAL LINE TO lt_dd01_texts ASSIGNING <ls_dd01_text>.
      MOVE-CORRESPONDING ls_dd01v TO <ls_dd01_text>.

      " Process main language entries and find corresponding translation
      LOOP AT it_dd07v ASSIGNING <ls_dd07v> WHERE NOT ddlanguage IS INITIAL.
        APPEND INITIAL LINE TO lt_dd07_texts ASSIGNING <ls_dd07_text>.
        READ TABLE lt_dd07v ASSIGNING <ls_dd07v_tmp>
          WITH KEY ddlanguage = <lv_lang> domvalue_l = <ls_dd07v>-domvalue_l domvalue_h = <ls_dd07v>-domvalue_h.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING <ls_dd07v_tmp> TO <ls_dd07_text>.
        ELSE.
          " no translation -> keep entry but clear texts
          MOVE-CORRESPONDING <ls_dd07v> TO <ls_dd07_text>.
          <ls_dd07_text>-ddlanguage = <lv_lang>.
          CLEAR: <ls_dd07_text>-ddtext, <ls_dd07_text>-domval_ld, <ls_dd07_text>-domval_hd.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

    SORT lt_i18n_langs ASCENDING.
    SORT lt_dd01_texts BY ddlanguage ASCENDING.
    SORT lt_dd07_texts BY valpos ASCENDING ddlanguage ASCENDING.

    IF lines( lt_i18n_langs ) > 0.
      ii_xml->add( iv_name = 'I18N_LANGS'
                   ig_data = lt_i18n_langs ).

      ii_xml->add( iv_name = 'DD01_TEXTS'
                   ig_data = lt_dd01_texts ).

      ii_xml->add( iv_name = 'DD07_TEXTS'
                   ig_data = lt_dd07_texts ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE as4user FROM dd01l INTO rv_user
      WHERE domname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    delete_ddic( iv_objtype              = 'D'
                 iv_no_ask_delete_append = abap_true ).

    delete_longtexts( c_longtext_id_doma ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

* package SEDD
* package SDIC

    DATA: lv_name  TYPE ddobjname,
          lv_done  TYPE abap_bool,
          ls_dd01v TYPE dd01v,
          lt_dd07v TYPE TABLE OF dd07v.

    FIELD-SYMBOLS <ls_dd07v> TYPE dd07v.

    io_xml->read( EXPORTING iv_name = 'DD01V'
                  CHANGING  cg_data = ls_dd01v ).
    io_xml->read( EXPORTING iv_name = 'DD07V_TAB'
                  CHANGING  cg_data = lt_dd07v ).

    handle_dependencies(
      EXPORTING
        iv_step = iv_step
      CHANGING
        cv_exit = ls_dd01v-convexit
        cv_done = lv_done ).

    IF lv_done = abap_true.
      RETURN.
    ENDIF.

    corr_insert( iv_package      = iv_package
                 ig_object_class = 'DICT' ).

    lv_name = ms_item-obj_name. " type conversion

    LOOP AT lt_dd07v ASSIGNING <ls_dd07v>.
      <ls_dd07v>-domname = lv_name.
      <ls_dd07v>-valpos = sy-tabix.
    ENDLOOP.

    CALL FUNCTION 'DDIF_DOMA_PUT'
      EXPORTING
        name              = lv_name
        dd01v_wa          = ls_dd01v
      TABLES
        dd07v_tab         = lt_dd07v
      EXCEPTIONS
        doma_not_found    = 1
        name_inconsistent = 2
        doma_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
      deserialize_texts(
        ii_xml   = io_xml
        is_dd01v = ls_dd01v
        it_dd07v = lt_dd07v ).
    ENDIF.

    deserialize_longtexts( ii_xml         = io_xml
                           iv_longtext_id = c_longtext_id_doma ).

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA lv_domname TYPE dd01l-domname.

    SELECT SINGLE domname FROM dd01l INTO lv_domname
      WHERE domname = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
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
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESDICT'
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECT=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_name    TYPE ddobjname,
          lv_state   TYPE ddgotstate,
          ls_dd01v   TYPE dd01v,
          lv_masklen TYPE c LENGTH 4,
          lt_dd07v   TYPE TABLE OF dd07v.

    FIELD-SYMBOLS <ls_dd07v> TYPE dd07v.

    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_DOMA_GET'
      EXPORTING
        name          = lv_name
        state         = 'A'
        langu         = mv_language
      IMPORTING
        gotstate      = lv_state
        dd01v_wa      = ls_dd01v
      TABLES
        dd07v_tab     = lt_dd07v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF ls_dd01v IS INITIAL OR lv_state <> 'A'.
      RETURN.
    ENDIF.

    CLEAR: ls_dd01v-as4user,
           ls_dd01v-as4date,
           ls_dd01v-as4time,
           ls_dd01v-appexist.

* make sure XML serialization does not dump if the field contains invalid data
* note that this is a N field, so '' is not valid
    IF ls_dd01v-authclass = ''.
      CLEAR ls_dd01v-authclass.
    ENDIF.
    lv_masklen = ls_dd01v-masklen.
    IF lv_masklen = '' OR NOT lv_masklen CO '0123456789'.
      CLEAR ls_dd01v-masklen.
    ENDIF.

    DELETE lt_dd07v WHERE appval = abap_true.

    SORT lt_dd07v BY
      valpos ASCENDING
      ddlanguage ASCENDING.

    LOOP AT lt_dd07v ASSIGNING <ls_dd07v>.
      CLEAR <ls_dd07v>-domname.
    ENDLOOP.

    io_xml->add( iv_name = 'DD01V'
                 ig_data = ls_dd01v ).
    io_xml->add( iv_name = 'DD07V_TAB'
                 ig_data = lt_dd07v ).

    IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
      serialize_texts(
        ii_xml   = io_xml
        it_dd07v = lt_dd07v ).
    ENDIF.

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_doma ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_DOMA implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_DRUL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_drul=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_drul=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_DRUL implementation.
*"* method's implementations
*include methods.
  METHOD clear_field.

    FIELD-SYMBOLS: <lv_value> TYPE data.

    ASSIGN COMPONENT iv_fieldname OF STRUCTURE cs_dependency_rule
           TO <lv_value>.
    ASSERT sy-subrc = 0.

    CLEAR: <lv_value>.

  ENDMETHOD.
  METHOD clear_fields.

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CREATED_AT'
      CHANGING
        cs_dependency_rule = cs_dependency_rule ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CREATED_BY'
      CHANGING
        cs_dependency_rule = cs_dependency_rule ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CHANGED_AT'
      CHANGING
        cs_dependency_rule = cs_dependency_rule ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CHANGED_BY'
      CHANGING
        cs_dependency_rule = cs_dependency_rule ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-MASTER_LANGUAGE'
      CHANGING
        cs_dependency_rule = cs_dependency_rule ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-RESPONSIBLE'
      CHANGING
        cs_dependency_rule = cs_dependency_rule ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-PACKAGE_REF'
      CHANGING
        cs_dependency_rule = cs_dependency_rule ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'CONTENT-SOURCE'
      CHANGING
        cs_dependency_rule = cs_dependency_rule ).

  ENDMETHOD.
  METHOD constructor.

    super->constructor(
        is_item     = is_item
        iv_language = iv_language ).

    mv_dependency_rule_key = ms_item-obj_name.

    TRY.
        CREATE DATA mr_dependency_rule TYPE ('CL_BLUE_SOURCE_OBJECT_DATA=>TY_OBJECT_DATA').
        CREATE OBJECT mi_persistence TYPE ('CL_DRUL_WB_OBJECT_PERSIST').

      CATCH cx_sy_create_error.
        Lcx_abapgit_exception=>raise( |DRUL not supported by your NW release| ).
    ENDTRY.

  ENDMETHOD.
  METHOD fill_metadata_from_db.

    DATA:
      li_wb_object_operator          TYPE REF TO object,
      lr_dependency_rule_old         TYPE REF TO data,
      lv_drul_object_data_clas_exist TYPE c LENGTH 1.

    FIELD-SYMBOLS:
      <ls_dependency_rule_old> TYPE any,
      <lv_created_at>          TYPE xsddatetime_z,
      <lv_created_by>          TYPE syuname,
      <lv_created_at_old>      TYPE xsddatetime_z,
      <lv_created_by_old>      TYPE syuname.

    li_wb_object_operator = get_wb_object_operator( ).

    CREATE DATA lr_dependency_rule_old TYPE ('CL_BLUE_SOURCE_OBJECT_DATA2=>TY_OBJECT_DATA').
    CALL FUNCTION 'CHECK_EXIST_CLAS'
      EXPORTING
        name            = 'CL_DRUL_WB_OBJECT_DATA'
      IMPORTING
        exist           = lv_drul_object_data_clas_exist
      EXCEPTIONS
        tr_invalid_type = 1
        OTHERS          = 2.

    IF sy-subrc = 0 AND lv_drul_object_data_clas_exist = abap_true.
      CREATE DATA lr_dependency_rule_old TYPE ('CL_DRUL_WB_OBJECT_DATA=>TY_OBJECT_DATA').
    ELSE.
      CREATE DATA lr_dependency_rule_old TYPE ('CL_BLUE_SOURCE_OBJECT_DATA=>TY_OBJECT_DATA').
    ENDIF.
    ASSIGN lr_dependency_rule_old->* TO <ls_dependency_rule_old>.
    ASSERT sy-subrc = 0.

    CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
      IMPORTING
        data = <ls_dependency_rule_old>.

    ASSIGN COMPONENT 'METADATA-CREATED_BY' OF STRUCTURE cs_dependency_rule
           TO <lv_created_by>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'METADATA-CREATED_AT' OF STRUCTURE cs_dependency_rule
           TO <lv_created_at>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'METADATA-CREATED_BY' OF STRUCTURE <ls_dependency_rule_old>
           TO <lv_created_by_old>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'METADATA-CREATED_AT' OF STRUCTURE <ls_dependency_rule_old>
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

    ls_object_type-objtype_tr = 'DRUL'.
    ls_object_type-subtype_wb = 'DRL'.

    TRY.
        CALL METHOD ('CL_WB_OBJECT_OPERATOR')=>('CREATE_INSTANCE')
          EXPORTING
            object_type = ls_object_type
            object_key  = mv_dependency_rule_key
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

    DATA:
      li_object_data_model           TYPE REF TO if_wb_object_data_model,
      li_wb_object_operator          TYPE REF TO object,
      lx_error                       TYPE REF TO cx_root,
      lv_drul_object_data_clas_exist TYPE c LENGTH 1.

    FIELD-SYMBOLS:
      <ls_dependency_rule> TYPE any,
      <lv_source>          TYPE data.

    ASSIGN mr_dependency_rule->* TO <ls_dependency_rule>.
    ASSERT sy-subrc = 0.

    io_xml->read(
      EXPORTING
        iv_name = 'DRUL'
      CHANGING
        cg_data = <ls_dependency_rule> ).

    li_wb_object_operator = get_wb_object_operator( ).

    TRY.
        CALL FUNCTION 'CHECK_EXIST_CLAS'
          EXPORTING
            name            = 'CL_DRUL_WB_OBJECT_DATA'
          IMPORTING
            exist           = lv_drul_object_data_clas_exist
          EXCEPTIONS
            tr_invalid_type = 1
            OTHERS          = 2.

        IF sy-subrc = 0 AND lv_drul_object_data_clas_exist = abap_true.
          CREATE OBJECT li_object_data_model TYPE ('CL_DRUL_WB_OBJECT_DATA').
        ELSE.
          CREATE OBJECT li_object_data_model TYPE ('CL_BLUE_SOURCE_OBJECT_DATA').
        ENDIF.

        ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <ls_dependency_rule>
               TO <lv_source>.
        ASSERT sy-subrc = 0.

        <lv_source> = Lif_abapgit_object~mo_files->read_string( 'asdrul' ).

        tadir_insert( iv_package ).

        IF Lif_abapgit_object~exists( ) = abap_true.

          " We need to populate created_at, created_by, because otherwise update  is not possible
          fill_metadata_from_db( CHANGING cs_dependency_rule = <ls_dependency_rule> ).
          li_object_data_model->set_data( <ls_dependency_rule> ).

          CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
            EXPORTING
              io_object_data    = li_object_data_model
              transport_request = iv_transport.

        ELSE.

          li_object_data_model->set_data( <ls_dependency_rule> ).

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
            p_object_key           = mv_dependency_rule_key
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
      lx_error              TYPE REF TO cx_root,
      lv_source             TYPE string.

    FIELD-SYMBOLS:
      <ls_dependency_rule> TYPE any,
      <lv_source>          TYPE string.

    ASSIGN mr_dependency_rule->* TO <ls_dependency_rule>.
    ASSERT sy-subrc = 0.

    li_wb_object_operator = get_wb_object_operator( ).

    TRY.
        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
          EXPORTING
            version        = 'A'
          IMPORTING
            data           = <ls_dependency_rule>
            eo_object_data = li_object_data_model.

        ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <ls_dependency_rule>
               TO <lv_source>.
        ASSERT sy-subrc = 0.

        lv_source = <lv_source>.

        clear_fields( CHANGING cs_dependency_rule = <ls_dependency_rule> ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    io_xml->add(
        iv_name = 'DRUL'
        ig_data = <ls_dependency_rule> ).

    Lif_abapgit_object~mo_files->add_string(
        iv_ext    = 'asdrul'
        iv_string = lv_source ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_DRUL implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_DSYS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_dsys=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_dsys=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_DSYS implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    DATA: lv_prefix    TYPE namespace,
          lv_bare_name TYPE progname.

    super->constructor( is_item = is_item
                        iv_language = iv_language ).

    IF ms_item-obj_name(1) = '/'.

      CALL FUNCTION 'RS_NAME_SPLIT_NAMESPACE'
        EXPORTING
          name_with_namespace    = ms_item-obj_name
        IMPORTING
          namespace              = lv_prefix
          name_without_namespace = lv_bare_name.

      mv_doc_object = |{ lv_bare_name+0(4) }{ lv_prefix }{ lv_bare_name+4(*) }|.
    ELSE.

      mv_doc_object = ms_item-obj_name.
    ENDIF.

  ENDMETHOD.
  METHOD deserialize_dsys.

    DATA: ls_data      TYPE ty_data,
          ls_docu_info TYPE dokil,
          lv_version   TYPE dokvers,
          lv_doku_obj  TYPE doku_obj.

    lv_doku_obj = mv_doc_object.
    ii_xml->read( EXPORTING iv_name = 'DSYS'
                  CHANGING cg_data = ls_data ).

    CALL FUNCTION 'DOCU_INIT'
      EXPORTING
        id     = c_id
        langu  = mv_language
        object = lv_doku_obj
        typ    = c_typ
      IMPORTING
        xdokil = ls_docu_info.

    lv_version = ls_docu_info-version.

    CALL FUNCTION 'DOCU_UPDATE'
      EXPORTING
        head    = ls_data-head
        state   = 'A'
        typ     = c_typ
        version = lv_version
      TABLES
        line    = ls_data-lines.

  ENDMETHOD.
  METHOD get_main_lang.

    SELECT SINGLE langu FROM dokil INTO rv_language
      WHERE id = c_id
      AND object = mv_doc_object
      AND masterlang = abap_true.

    IF sy-subrc <> 0.
      rv_language = mv_language.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    rv_user = Lcl_abapgit_factory=>get_longtexts( )->changed_by(
      iv_object_name = mv_doc_object
      iv_longtext_id = c_id ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    Lcl_abapgit_factory=>get_longtexts( )->delete(
      iv_object_name = mv_doc_object
      iv_longtext_id = c_id ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_metadata TYPE Lif_abapgit_definitions=>ty_metadata.

    ls_metadata = io_xml->get_metadata( ).

    CASE ls_metadata-version.

      WHEN 'v1.0.0'.
        deserialize_dsys( io_xml ).

      WHEN 'v2.0.0'.
        Lcl_abapgit_factory=>get_longtexts( )->deserialize(
          ii_xml           = io_xml
          iv_object_name   = mv_doc_object
          iv_longtext_id   = c_id
          iv_main_language = mv_language ).

      WHEN OTHERS.
        Lcx_abapgit_exception=>raise( 'unsupported DSYS version' ).

    ENDCASE.

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_count TYPE i.

    SELECT SINGLE COUNT( * ) FROM dokil INTO lv_count
           WHERE id   = c_id
           AND object = mv_doc_object.                  "#EC CI_GENBUFF

    rv_bool = boolc( lv_count > 0 ).

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
    rs_metadata-version = 'v2.0.0'.
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA lv_lang TYPE sy-langu.

    lv_lang = get_main_lang( ).

    CALL FUNCTION 'DSYS_EDIT'
      EXPORTING
        dokclass            = mv_doc_object+0(4)
        dokname             = mv_doc_object+4(*)
        doklangu            = lv_lang
      EXCEPTIONS
        not_hypertext_class = 1
        no_editor           = 2
        OTHERS              = 3.

    rv_exit = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    Lcl_abapgit_factory=>get_longtexts( )->serialize(
      iv_object_name = mv_doc_object
      iv_longtext_id = c_id
      io_i18n_params = mo_i18n_params
      ii_xml         = io_xml ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_DSYS implementation

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
  METHOD Lif_abapgit_object~get_deserialize_order.
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
endclass. "ZCL_ABAPGIT_OBJECT_DTDC implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_DTEL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_dtel=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_dtel=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_DTEL implementation.
*"* method's implementations
*include methods.
  METHOD deserialize_texts.

    DATA: lv_name       TYPE ddobjname,
          ls_dd04v_tmp  TYPE dd04v,
          lt_i18n_langs TYPE TABLE OF langu,
          lt_dd04_texts TYPE ty_dd04_texts.

    FIELD-SYMBOLS: <lv_lang>      LIKE LINE OF lt_i18n_langs,
                   <ls_dd04_text> LIKE LINE OF lt_dd04_texts.


    lv_name = ms_item-obj_name.

    ii_xml->read( EXPORTING iv_name = 'I18N_LANGS'
                  CHANGING  cg_data = lt_i18n_langs ).

    ii_xml->read( EXPORTING iv_name = 'DD04_TEXTS'
                  CHANGING  cg_data = lt_dd04_texts ).

    mo_i18n_params->trim_saplang_list( CHANGING ct_sap_langs = lt_i18n_langs ).

    SORT lt_i18n_langs.
    SORT lt_dd04_texts BY ddlanguage. " Optimization

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.

      " Data element description
      ls_dd04v_tmp = is_dd04v.
      READ TABLE lt_dd04_texts ASSIGNING <ls_dd04_text> WITH KEY ddlanguage = <lv_lang>.
      IF sy-subrc > 0.
        Lcx_abapgit_exception=>raise( |DD04_TEXTS cannot find lang { <lv_lang> } in XML| ).
      ENDIF.
      MOVE-CORRESPONDING <ls_dd04_text> TO ls_dd04v_tmp.
      CALL FUNCTION 'DDIF_DTEL_PUT'
        EXPORTING
          name              = lv_name
          dd04v_wa          = ls_dd04v_tmp
        EXCEPTIONS
          dtel_not_found    = 1
          name_inconsistent = 2
          dtel_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD serialize_texts.

    DATA: lv_name            TYPE ddobjname,
          lv_index           TYPE i,
          ls_dd04v           TYPE dd04v,
          lt_dd04_texts      TYPE ty_dd04_texts,
          lt_i18n_langs      TYPE TABLE OF langu,
          lt_language_filter TYPE Lif_abapgit_environment=>ty_system_language_filter.

    FIELD-SYMBOLS: <lv_lang>      LIKE LINE OF lt_i18n_langs,
                   <ls_dd04_text> LIKE LINE OF lt_dd04_texts.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      RETURN.
    ENDIF.

    lv_name = ms_item-obj_name.

    " Collect additional languages, skip main lang - it was serialized already
    lt_language_filter = mo_i18n_params->build_language_filter( ).

    SELECT DISTINCT ddlanguage AS langu INTO TABLE lt_i18n_langs
      FROM dd04v
      WHERE rollname = lv_name
      AND ddlanguage IN lt_language_filter
      AND ddlanguage <> mv_language.                      "#EC CI_SUBRC

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.
      lv_index = sy-tabix.
      CALL FUNCTION 'DDIF_DTEL_GET'
        EXPORTING
          name          = lv_name
          langu         = <lv_lang>
        IMPORTING
          dd04v_wa      = ls_dd04v
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.
      IF sy-subrc <> 0 OR ls_dd04v-ddlanguage IS INITIAL.
        DELETE lt_i18n_langs INDEX lv_index. " Don't save this lang
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO lt_dd04_texts ASSIGNING <ls_dd04_text>.
      MOVE-CORRESPONDING ls_dd04v TO <ls_dd04_text>.

    ENDLOOP.

    SORT lt_i18n_langs ASCENDING.
    SORT lt_dd04_texts BY ddlanguage ASCENDING.

    IF lines( lt_i18n_langs ) > 0.
      ii_xml->add( iv_name = 'I18N_LANGS'
                   ig_data = lt_i18n_langs ).

      ii_xml->add( iv_name = 'DD04_TEXTS'
                   ig_data = lt_dd04_texts ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE as4user FROM dd04l INTO rv_user
      WHERE rollname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    delete_ddic( 'E' ).

    delete_longtexts( c_longtext_id_dtel ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_dd04v TYPE dd04v,
          lv_name  TYPE ddobjname.


    io_xml->read( EXPORTING iv_name = 'DD04V'
                  CHANGING cg_data = ls_dd04v ).

    corr_insert( iv_package = iv_package
                 ig_object_class = 'DICT' ).

    lv_name = ms_item-obj_name. " type conversion

    CALL FUNCTION 'DDIF_DTEL_PUT'
      EXPORTING
        name              = lv_name
        dd04v_wa          = ls_dd04v
      EXCEPTIONS
        dtel_not_found    = 1
        name_inconsistent = 2
        dtel_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
      deserialize_texts(
        ii_xml   = io_xml
        is_dd04v = ls_dd04v ).
    ENDIF.

    deserialize_longtexts( ii_xml         = io_xml
                           iv_longtext_id = c_longtext_id_dtel ).

    deserialize_longtexts( ii_xml           = io_xml
                           iv_longtext_name = 'LONGTEXTS_' && c_longtext_id_dtel_suppl
                           iv_longtext_id   = c_longtext_id_dtel_suppl ).

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_rollname TYPE dd04l-rollname.

    lv_rollname = ms_item-obj_name.

    " Check nametab because it's fast
    CALL FUNCTION 'DD_GET_NAMETAB_HEADER'
      EXPORTING
        tabname   = lv_rollname
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      " Check for inactive or modified versions
      SELECT SINGLE rollname FROM dd04l INTO lv_rollname
        WHERE rollname = lv_rollname.
    ENDIF.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
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
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.
* fm DDIF_DTEL_GET bypasses buffer, so SELECTs are
* done directly from here

    DATA: lv_name  TYPE ddobjname,
          ls_dd04v TYPE dd04v.

    lv_name = ms_item-obj_name.

    SELECT SINGLE * FROM dd04l
      INTO CORRESPONDING FIELDS OF ls_dd04v
      WHERE rollname = lv_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    IF sy-subrc <> 0 OR ls_dd04v IS INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM dd04t
      INTO CORRESPONDING FIELDS OF ls_dd04v
      WHERE rollname = lv_name
      AND ddlanguage = mv_language
      AND as4local = 'A'
      AND as4vers = '0000'.

    CLEAR: ls_dd04v-as4user,
           ls_dd04v-as4date,
           ls_dd04v-as4time.

    IF ls_dd04v-refkind = 'D'.
* clear values inherited from domain
      CLEAR: ls_dd04v-datatype,
             ls_dd04v-leng,
             ls_dd04v-decimals,
             ls_dd04v-outputlen,
             ls_dd04v-valexi,
             ls_dd04v-lowercase,
             ls_dd04v-signflag,
             ls_dd04v-convexit,
             ls_dd04v-entitytab.
    ENDIF.

    IF ls_dd04v-routputlen = ''.
* numeric field, make sure it is initial or XML serilization will dump
      CLEAR ls_dd04v-routputlen.
    ENDIF.
    IF ls_dd04v-authclass = ''.
      CLEAR ls_dd04v-authclass.
    ENDIF.

    io_xml->add( iv_name = 'DD04V'
                 ig_data = ls_dd04v ).

    IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
      serialize_texts( io_xml ).
    ENDIF.

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_dtel ).

    serialize_longtexts( ii_xml           = io_xml
                         iv_longtext_name = 'LONGTEXTS_' && c_longtext_id_dtel_suppl
                         iv_longtext_id   = c_longtext_id_dtel_suppl ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_DTEL implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ECAT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ecat=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ecat=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ECAT implementation.
*"* method's implementations
*include methods.
  METHOD get_download.

    CREATE OBJECT ro_download TYPE Lcl_abapgit_ecatt_script_downl.

  ENDMETHOD.
  METHOD get_lock_object.

    rv_lock_object = 'E_ECATT'.

  ENDMETHOD.
  METHOD get_object_type.

    rv_object_type = cl_apl_ecatt_const=>obj_type_test_script.

  ENDMETHOD.
  METHOD get_upload.

    CREATE OBJECT ro_upload TYPE Lcl_abapgit_ecatt_script_upl.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ECAT implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ECATT_SUPER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ecatt_superccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ecatt_superccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_object_ecatt_superccau.
*CLASS SHRIS5ZPAUXVKEPN5HWETLLAS3OBTU DEFINITION DEFERRED.

*CLASS zcl_abapgit_object_ecatt_super DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLAS3OBTU.




class LCL_ABAPGIT_OBJECT_ECATT_SUPER implementation.
*"* method's implementations
*include methods.
  METHOD clear_attributes.

    DATA: li_element     TYPE REF TO if_ixml_element,
          lv_object_type TYPE etobj_type.

    lv_object_type = get_object_type( ).

    li_element = ci_document->find_from_name( |{ lv_object_type }| ).
    li_element->remove_attribute( |SAPRL| ).
    li_element->remove_attribute( |DOWNLOADDATE| ).
    li_element->remove_attribute( |DOWNLOADTIME| ).

  ENDMETHOD.
  METHOD clear_element.

    DATA: li_element TYPE REF TO if_ixml_element.

    li_element = ci_document->find_from_name( iv_name ).

    IF li_element IS BOUND.
      li_element->set_value( || ).
    ENDIF.

  ENDMETHOD.
  METHOD clear_elements.

    clear_element( EXPORTING iv_name     = |FUSER|
                   CHANGING  ci_document = ci_document ).

    clear_element( EXPORTING iv_name     = |FDATE|
                   CHANGING  ci_document = ci_document ).

    clear_element( EXPORTING iv_name     = |LUSER|
                   CHANGING  ci_document = ci_document ).

    clear_element( EXPORTING iv_name     = |LDATE|
                   CHANGING  ci_document = ci_document ).

    clear_element( EXPORTING iv_name     = |LTIME|
                   CHANGING  ci_document = ci_document ).

    clear_element( EXPORTING iv_name     = |TWB_RESP|
                   CHANGING  ci_document = ci_document ).

    clear_element( EXPORTING iv_name     = |DEVCLASS|
                   CHANGING  ci_document = ci_document ).

    clear_element( EXPORTING iv_name     = |TADIR_RESP|
                   CHANGING  ci_document = ci_document ).

    " Clearing just VAR_EXT_PATH will lead to diffs in batch
    clear_element( EXPORTING iv_name     = |ETVAR_EXT|
                   CHANGING  ci_document = ci_document ).

    " SORTLNR is part of ETPAR_VARI and causing diffs
    " We can clear it since it's automatically filled during deserialize
    clear_element_collection( EXPORTING iv_name     = |SORTLNR|
                              CHANGING  ci_document = ci_document ).

  ENDMETHOD.
  METHOD clear_element_collection.

    DATA:
      lo_node_collection TYPE REF TO if_ixml_node_collection,
      lo_node            TYPE REF TO if_ixml_node,
      lv_index           TYPE i.

    lo_node_collection = ci_document->get_elements_by_tag_name( iv_name ).

    lv_index = 0.
    WHILE lv_index < lo_node_collection->get_length( ).
      lo_node = lo_node_collection->get_item( lv_index ).
      lo_node->set_value( '' ).
      lv_index = lv_index + 1.
    ENDWHILE.

  ENDMETHOD.
  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_object_name = ms_item-obj_name.

  ENDMETHOD.
  METHOD deserialize_version.

    DATA: ls_object   TYPE etmobjects,
          lo_upload   TYPE REF TO cl_apl_ecatt_upload,
          li_upload   TYPE REF TO Lif_abapgit_ecatt_upload,
          lv_xml      TYPE xstring,
          li_document TYPE REF TO if_ixml_document,
          lv_version  TYPE string,
          lx_error    TYPE REF TO cx_ecatt.

    lv_version = get_version_from_node( ii_version_node ).

    IF lv_version IS INITIAL.
      RETURN.
    ENDIF.

    lo_upload  = get_upload( ).
    li_upload ?= lo_upload.

    li_document = cl_ixml=>create( )->create_document( ).
    li_document->append_child( ii_version_node->get_first_child( ) ).

    lv_xml = cl_ixml_80_20=>render_to_xstring( li_document ).

    li_upload->set_stream_for_upload( lv_xml ).

    ls_object-d_obj_name  = mv_object_name.
    ls_object-s_obj_type  = get_object_type( ).
    ls_object-d_devclass  = iv_package.
    ls_object-d_obj_ver   = lv_version.
    ls_object-d_overwrite = abap_true.

    TRY.
        lo_upload->upload( CHANGING ch_object = ls_object ).

      CATCH cx_ecatt INTO lx_error.
        Lcx_abapgit_exception=>raise( lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
  METHOD get_changed_by_user.

    rv_changed_by_user = ii_document->find_from_name( 'LUSER' )->get_value( ).

  ENDMETHOD.
  METHOD get_changed_date.

    DATA: lv_changed_date_external TYPE string.

    lv_changed_date_external = ii_document->find_from_name( 'LDATE' )->get_value( ).

    REPLACE ALL OCCURRENCES OF '-' IN lv_changed_date_external WITH ''.
    rv_changed_date = lv_changed_date_external.

  ENDMETHOD.
  METHOD get_changed_time.

    DATA: lv_changed_time_external TYPE string.

    lv_changed_time_external = ii_document->find_from_name( 'LTIME' )->get_value( ).

    REPLACE ALL OCCURRENCES OF ':' IN lv_changed_time_external WITH ''.
    rv_changed_time = lv_changed_time_external.

  ENDMETHOD.
  METHOD get_change_information.

    DATA: li_document    TYPE REF TO if_ixml_document,
          lv_xml         TYPE xstring,
          lo_download    TYPE REF TO cl_apl_ecatt_download,
          lv_object_type TYPE etobj_type.

    lo_download = get_download( ).

    lv_object_type = get_object_type( ).

    lv_xml = Lcl_abapgit_ecatt_helper=>build_xml_of_object(
                 iv_object_name    = mv_object_name
                 iv_object_version = is_version_info-version
                 iv_object_type    = lv_object_type
                 io_download       = lo_download ).

    li_document = cl_ixml_80_20=>parse_to_document( stream_xstring = lv_xml ).

    rs_change_information-ldate = get_changed_date( li_document ).
    rs_change_information-ltime = get_changed_time( li_document ).
    rs_change_information-luser = get_changed_by_user( li_document ).

  ENDMETHOD.
  METHOD get_version_from_node.

    TRY.
        rv_version = ii_node->get_first_child(
                           )->get_first_child(
                           )->get_first_child(
                           )->get_first_child(
                           )->get_value( ).

      CATCH cx_sy_ref_is_initial.
        RETURN.
    ENDTRY.

  ENDMETHOD.
  METHOD is_change_more_recent_than.

    IF is_currently_changed-ldate > is_last_changed-ldate
      OR (     is_currently_changed-ldate = is_last_changed-ldate
           AND is_currently_changed-ltime > is_last_changed-ltime ).

      rv_is_change_more_recent = abap_true.

    ENDIF.

  ENDMETHOD.
  METHOD serialize_version.

    DATA: li_document    TYPE REF TO if_ixml_document,
          lv_xml         TYPE xstring,
          li_node        TYPE REF TO if_ixml_element,
          lo_download    TYPE REF TO cl_apl_ecatt_download,
          lv_object_type TYPE etobj_type.

    lo_download = get_download( ).

    lv_object_type = get_object_type( ).

    lv_xml = Lcl_abapgit_ecatt_helper=>build_xml_of_object(
                 iv_object_name    = mv_object_name
                 iv_object_version = iv_version
                 iv_object_type    = lv_object_type
                 io_download       = lo_download ).

    IF lv_xml IS INITIAL.
      Lcx_abapgit_exception=>raise( |ECATT, empty xml, { mv_object_name }| ).
    ENDIF.

    li_document = cl_ixml_80_20=>parse_to_document( stream_xstring = lv_xml ).

    clear_attributes( CHANGING ci_document = li_document ).

    clear_elements( CHANGING ci_document = li_document ).

    li_node = li_document->create_element( c_name-version ).
    li_node->append_child( li_document->get_root_element( ) ).

    ci_node->append_child( li_node ).

  ENDMETHOD.
  METHOD serialize_versions.

    DATA: li_versions_node TYPE REF TO if_ixml_element.
    FIELD-SYMBOLS: <ls_version_info> LIKE LINE OF it_version_info.

    li_versions_node = ci_document->create_element( c_name-versions ).

    IF lines( it_version_info ) > 0.

      LOOP AT it_version_info ASSIGNING <ls_version_info>.

        serialize_version(
          EXPORTING
            iv_version = <ls_version_info>-version
          CHANGING
            ci_node    = li_versions_node ).

      ENDLOOP.

    ELSE.

      serialize_version(
        EXPORTING
          iv_version = c_default_version
        CHANGING
          ci_node    = li_versions_node ).

    ENDIF.

    ci_document->append_child( li_versions_node ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: ls_last_changed      TYPE ty_last_changed,
          ls_currently_changed TYPE ty_last_changed,
          lt_version_info      TYPE etversinfo_tabtype,
          lx_error             TYPE REF TO cx_static_check,
          lv_text              TYPE string,
          lv_object_type       TYPE etobj_type.

    FIELD-SYMBOLS: <ls_version_info> LIKE LINE OF lt_version_info.

    TRY.
        lv_object_type = get_object_type( ).

        cl_apl_ecatt_object=>get_version_info_object(
          EXPORTING
            im_name          = mv_object_name
            im_obj_type      = lv_object_type
          IMPORTING
            ex_version_info  = lt_version_info ).

        LOOP AT lt_version_info ASSIGNING <ls_version_info>.

          ls_currently_changed = get_change_information( <ls_version_info> ).

          IF is_change_more_recent_than( is_currently_changed = ls_currently_changed
                                         is_last_changed      = ls_last_changed ) = abap_true.
            ls_last_changed = ls_currently_changed.
          ENDIF.

        ENDLOOP.

      CATCH cx_static_check INTO lx_error.
        lv_text = lx_error->get_text( ).
        MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

    IF ls_last_changed-luser IS NOT INITIAL.
      rv_user = ls_last_changed-luser.
    ELSE.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lx_error       TYPE REF TO cx_ecatt_apl,
          lv_text        TYPE string,
          lv_object_type TYPE etobj_type.

    lv_object_type = get_object_type( ).

    TRY.
        cl_apl_ecatt_object=>delete_object( im_obj_type            = lv_object_type
                                            im_name                = mv_object_name
                                            " we have to supply a version, so let's use the default version
                                            " and delete them all
                                            im_version             = c_default_version
                                            im_delete_all_versions = abap_true ).

      CATCH cx_ecatt_apl INTO lx_error.
        lv_text = lx_error->get_text( ).
        Lcx_abapgit_exception=>raise( lv_text ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: li_document         TYPE REF TO if_ixml_document,
          li_versions         TYPE REF TO if_ixml_node_collection,
          li_version_iterator TYPE REF TO if_ixml_node_iterator,
          li_version_node     TYPE REF TO if_ixml_node.

    li_document = io_xml->get_raw( ).

    li_versions = li_document->get_elements_by_tag_name( depth = 0
                                                         name  = c_name-version ).

    li_version_iterator = li_versions->create_iterator( ).

    DO.
      li_version_node = li_version_iterator->get_next( ).

      IF li_version_node IS NOT BOUND.
        EXIT.
      ENDIF.

      deserialize_version( ii_version_node = li_version_node
                           iv_package      = iv_package ).

    ENDDO.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_object_type TYPE etobj_type.

    lv_object_type = get_object_type( ).

    TRY.
        rv_bool = cl_apl_ecatt_object=>existence_check_object( im_name               = mv_object_name
                                                               im_version            = c_default_version
                                                               im_obj_type           = lv_object_type
                                                               im_exists_any_version = abap_true ).

      CATCH cx_ecatt.
        RETURN.
    ENDTRY.

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

    DATA: lv_object TYPE seqg3-garg.

    lv_object = ms_item-obj_name.
    OVERLAY lv_object WITH '                              '.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = get_lock_object( )
                                            iv_argument    = lv_object ).

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
      lt_version_info TYPE etversinfo_tabtype,
      li_document     TYPE REF TO if_ixml_document,
      lx_error        TYPE REF TO cx_ecatt,
      lv_text         TYPE string,
      lv_object_type  TYPE etobj_type.

    lv_object_type = get_object_type( ).

    TRY.
        cl_apl_ecatt_object=>get_version_info_object(
          EXPORTING
            im_name         = mv_object_name
            im_obj_type     = lv_object_type
          IMPORTING
            ex_version_info = lt_version_info ).

        SORT lt_version_info BY version.

        li_document = cl_ixml=>create( )->create_document( ).

        serialize_versions(
          EXPORTING
            it_version_info = lt_version_info
          CHANGING
            ci_document     = li_document ).

        io_xml->set_raw( li_document->get_root_element( ) ).

      CATCH cx_ecatt INTO lx_error.
        lv_text = lx_error->get_text( ).
        MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ECATT_SUPER implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ECSD <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ecsd=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ecsd=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ECSD implementation.
*"* method's implementations
*include methods.
  METHOD get_download.

    CREATE OBJECT ro_download TYPE Lcl_abapgit_ecatt_system_downl.

  ENDMETHOD.
  METHOD get_lock_object.

    rv_lock_object = 'E_ECATT_SD'.

  ENDMETHOD.
  METHOD get_object_type.

    rv_object_type = cl_apl_ecatt_const=>obj_type_system_data.

  ENDMETHOD.
  METHOD get_upload.

    CREATE OBJECT ro_upload TYPE Lcl_abapgit_ecatt_system_upl.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ECSD implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ECSP <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ecsp=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ecsp=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ECSP implementation.
*"* method's implementations
*include methods.
  METHOD get_download.

    CREATE OBJECT ro_download TYPE Lcl_abapgit_ecatt_sp_download.

  ENDMETHOD.
  METHOD get_lock_object.

    rv_lock_object = 'E_ECATT_SP'.

  ENDMETHOD.
  METHOD get_object_type.

* constant missing in 702, cl_apl_ecatt_const=>obj_type_start_profile
    rv_object_type = 'ECSP'.

  ENDMETHOD.
  METHOD get_upload.

    CREATE OBJECT ro_upload TYPE Lcl_abapgit_ecatt_sp_upload.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ECSP implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ECTC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ectc=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ectc=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ECTC implementation.
*"* method's implementations
*include methods.
  METHOD get_download.

    CREATE OBJECT ro_download TYPE Lcl_abapgit_ecatt_config_downl.

  ENDMETHOD.
  METHOD get_lock_object.

    rv_lock_object = 'E_ECATT_TC'.

  ENDMETHOD.
  METHOD get_object_type.

    rv_object_type = cl_apl_ecatt_const=>obj_type_test_config.

  ENDMETHOD.
  METHOD get_upload.

    CREATE OBJECT ro_upload TYPE Lcl_abapgit_ecatt_config_upl.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ECTC implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ECTD <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ectd=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ectd=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ECTD implementation.
*"* method's implementations
*include methods.
  METHOD get_download.

    CREATE OBJECT ro_download TYPE Lcl_abapgit_ecatt_data_downl.

  ENDMETHOD.
  METHOD get_lock_object.

    rv_lock_object = 'E_ECATT_TD'.

  ENDMETHOD.
  METHOD get_object_type.

    rv_object_type = cl_apl_ecatt_const=>obj_type_test_data.

  ENDMETHOD.
  METHOD get_upload.

    CREATE OBJECT ro_upload TYPE Lcl_abapgit_ecatt_data_upload.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ECTD implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ECVO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ecvo=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ecvo=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ECVO implementation.
*"* method's implementations
*include methods.
  METHOD get_download.

    CREATE OBJECT ro_download TYPE Lcl_abapgit_ecatt_val_obj_down.

  ENDMETHOD.
  METHOD get_lock_object.

    rv_lock_object = 'E_ECATT_TD'.

  ENDMETHOD.
  METHOD get_object_type.

* constant missing in 702, cl_apl_ecatt_const=>obj_type_ecatt_vo
    rv_object_type = 'ECVO'.

  ENDMETHOD.
  METHOD get_upload.

    CREATE OBJECT ro_upload TYPE Lcl_abapgit_ecatt_val_obj_upl.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ECVO implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ENHC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_enhc=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_enhc=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ENHC implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_composite_id = ms_item-obj_name.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE changedby INTO rv_user FROM enhcompheader
      WHERE enhcomposite = ms_item-obj_name AND version = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lx_enh_root   TYPE REF TO cx_enh_root,
          li_enh_object TYPE REF TO if_enh_object.

    TRY.
        li_enh_object = cl_enh_factory=>load_enhancement_composite(
          name = mv_composite_id
          lock = abap_true ).

        li_enh_object->delete( nevertheless_delete = abap_true
                               run_dark            = abap_true ).
        li_enh_object->unlock( ).

      CATCH cx_enh_root INTO lx_enh_root.
        Lcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lx_enh_root         TYPE REF TO cx_enh_root,
          li_enh_composite    TYPE REF TO if_enh_composite,
          lv_package          TYPE devclass,
          lt_composite_childs TYPE enhcompositename_it,
          lt_enh_childs       TYPE enhname_it,
          lv_longtext_id      TYPE enhdocuobject,
          lv_vers             TYPE enhcompheader-version,
          lv_shorttext        TYPE string.

    FIELD-SYMBOLS: <lv_composite_child> TYPE enhcompositename,
                   <lv_enh_child>       LIKE LINE OF lt_enh_childs.

    lv_package = iv_package.


    io_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING  cg_data = lv_shorttext ).
    io_xml->read( EXPORTING iv_name = 'COMPOSITE_CHILDS'
                  CHANGING  cg_data = lt_composite_childs ).
    io_xml->read( EXPORTING iv_name = 'ENH_CHILDS'
                  CHANGING  cg_data = lt_enh_childs ).
    io_xml->read( EXPORTING iv_name = 'LONGTEXT_ID'
                  CHANGING  cg_data = lv_longtext_id ).

    SELECT SINGLE version FROM enhcompheader INTO lv_vers WHERE enhcomposite = ms_item-obj_name.
    IF sy-subrc = 0.
      " If object exists already, then set TADIR entry to deleted
      " otherwise create_enhancement_composite will fail
      tadir_delete( ).
    ENDIF.

    TRY.
        cl_enh_factory=>create_enhancement_composite(
          EXPORTING
            name      = mv_composite_id
            run_dark  = abap_true
          IMPORTING
            composite = li_enh_composite
          CHANGING
            devclass  = lv_package ).

        li_enh_composite->if_enh_object_docu~set_shorttext( lv_shorttext ).

        LOOP AT lt_composite_childs ASSIGNING <lv_composite_child>.
          li_enh_composite->add_composite_child( <lv_composite_child> ).
        ENDLOOP.

        LOOP AT lt_enh_childs ASSIGNING <lv_enh_child>.
          li_enh_composite->add_enh_child( <lv_enh_child> ).
        ENDLOOP.

        li_enh_composite->set_longtext_id( lv_longtext_id ).

        li_enh_composite->if_enh_object~save( ).
        li_enh_composite->if_enh_object~activate( ).
        li_enh_composite->if_enh_object~unlock( ).

      CATCH cx_enh_root INTO lx_enh_root.
        Lcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    TRY.
        cl_enh_factory=>load_enhancement_composite(
          name = mv_composite_id
          lock = abap_false ).
        rv_bool = abap_true.
      CATCH cx_enh_root.
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
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    DATA: lv_argument TYPE seqg3-garg.

    lv_argument = |{ mv_composite_id }|.
    OVERLAY lv_argument WITH '                                  '.
    lv_argument = |{ lv_argument }*|.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = |E_ENHANCE|
                                            iv_argument    = lv_argument ).

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

    DATA: lx_enh_root         TYPE REF TO cx_enh_root,
          li_enh_composite    TYPE REF TO if_enh_composite,
          lt_composite_childs TYPE enhcompositename_it,
          lt_enh_childs       TYPE enhname_it,
          lv_longtext_id      TYPE enhdocuobject,
          lv_shorttext        TYPE string.

    TRY.
        li_enh_composite = cl_enh_factory=>load_enhancement_composite(
          name = mv_composite_id
          lock = abap_false ).

        lv_shorttext = li_enh_composite->if_enh_object_docu~get_shorttext( ).

        lt_composite_childs = li_enh_composite->get_composite_childs( ).
        lt_enh_childs       = li_enh_composite->get_enh_childs( ).
        lv_longtext_id      = li_enh_composite->get_longtext_id( ).

        io_xml->add( iv_name = 'SHORTTEXT'
                     ig_data = lv_shorttext ).
        io_xml->add( iv_name = 'COMPOSITE_CHILDS'
                     ig_data = lt_composite_childs ).
        io_xml->add( iv_name = 'ENH_CHILDS'
                     ig_data = lt_enh_childs ).
        io_xml->add( iv_name = 'LONGTEXT_ID'
                     ig_data = lv_longtext_id ).

      CATCH cx_enh_root INTO lx_enh_root.
        Lcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENHC implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ENHO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_enho=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_enho=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ENHO implementation.
*"* method's implementations
*include methods.
  METHOD factory.

    CASE iv_tool.
      WHEN cl_enh_tool_badi_impl=>tooltype.
        CREATE OBJECT ri_enho TYPE Lcl_abapgit_object_enho_badi
          EXPORTING
            is_item = ms_item.
      WHEN cl_enh_tool_hook_impl=>tooltype.
        CREATE OBJECT ri_enho TYPE Lcl_abapgit_object_enho_hook
          EXPORTING
            is_item  = ms_item
            io_files = Lif_abapgit_object~mo_files.
      WHEN cl_enh_tool_class=>tooltype.
        CREATE OBJECT ri_enho TYPE Lcl_abapgit_object_enho_class
          EXPORTING
            is_item  = ms_item
            io_files = Lif_abapgit_object~mo_files.
      WHEN cl_enh_tool_intf=>tooltype.
        CREATE OBJECT ri_enho TYPE Lcl_abapgit_object_enho_intf
          EXPORTING
            is_item  = ms_item
            io_files = Lif_abapgit_object~mo_files.
      WHEN cl_wdr_cfg_enhancement=>tooltype.
        CREATE OBJECT ri_enho TYPE Lcl_abapgit_object_enho_wdyc
          EXPORTING
            is_item = ms_item.
      WHEN 'FUGRENH'.
        CREATE OBJECT ri_enho TYPE Lcl_abapgit_object_enho_fugr
          EXPORTING
            is_item  = ms_item
            io_files = Lif_abapgit_object~mo_files.
      WHEN 'WDYENH'.
        CREATE OBJECT ri_enho TYPE Lcl_abapgit_object_enho_wdyn
          EXPORTING
            is_item = ms_item.
      WHEN OTHERS.
        Lcx_abapgit_exception=>raise( |Unsupported ENHO type { iv_tool }| ).
    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lv_enh_id   TYPE enhname,
          lt_log      TYPE enh_log_it,
          li_log_obj  TYPE REF TO if_enh_log,
          ls_enhlog   TYPE enhlog,
          lv_lines    TYPE i,
          lt_enhlog   TYPE STANDARD TABLE OF enhlog WITH DEFAULT KEY,
          li_enh_tool TYPE REF TO if_enh_tool.


    lv_enh_id = ms_item-obj_name.
    TRY.
        li_enh_tool = cl_enh_factory=>get_enhancement(
          enhancement_id   = lv_enh_id
          run_dark         = abap_true
          bypassing_buffer = abap_true ).
      CATCH cx_enh_root.
        rv_user = c_user_unknown.
        RETURN.
    ENDTRY.

    lt_log = li_enh_tool->get_log( ).

    LOOP AT lt_log INTO li_log_obj.
      ls_enhlog = li_log_obj->get_enhlog( ).
      APPEND ls_enhlog TO lt_enhlog.
    ENDLOOP.

    lv_lines = lines( lt_enhlog ).
    READ TABLE lt_enhlog INTO ls_enhlog INDEX lv_lines.
    IF sy-subrc = 0.
      rv_user = ls_enhlog-loguser.
    ELSE.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_enh_id     TYPE enhname,
          li_enh_object TYPE REF TO if_enh_object,
          lx_enh_root   TYPE REF TO cx_enh_root,
          lv_corrnum    TYPE trkorr.

    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lv_corrnum = iv_transport.

    lv_enh_id = ms_item-obj_name.
    TRY.
        li_enh_object = cl_enh_factory=>get_enhancement(
          enhancement_id = lv_enh_id
          run_dark       = abap_true
          lock           = abap_true ).
        li_enh_object->delete(
          EXPORTING
            nevertheless_delete = abap_true
            run_dark            = abap_true
          CHANGING
            trkorr              = lv_corrnum ).
        li_enh_object->unlock( ).
      CATCH cx_enh_root INTO lx_enh_root.
        Lcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_tool TYPE enhtooltype,
          li_enho TYPE REF TO Lif_abapgit_object_enho.

    IF Lif_abapgit_object~exists( ) = abap_true.
      Lif_abapgit_object~delete( iv_package   = iv_package
                                 iv_transport = iv_transport ).
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'TOOL'
                  CHANGING cg_data = lv_tool ).

    li_enho = factory( lv_tool ).

    li_enho->deserialize( ii_xml     = io_xml
                          iv_package = iv_package ).

    Lcl_abapgit_sotr_handler=>create_sotr(
      iv_package = iv_package
      io_xml     = io_xml ).

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_enh_id TYPE enhname.


    lv_enh_id = ms_item-obj_name.
    TRY.
        cl_enh_factory=>get_enhancement(
          enhancement_id   = lv_enh_id
          run_dark         = abap_true
          bypassing_buffer = abap_true ).
        rv_bool = abap_true.
      CATCH cx_enh_root.
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

    lv_object = |{ ms_item-obj_type }{ ms_item-obj_name }|.
    OVERLAY lv_object WITH '                                          '.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'E_ENHANCE'
                                            iv_argument    = lv_object ).

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

    DATA: lv_enh_id   TYPE enhname,
          li_enho     TYPE REF TO Lif_abapgit_object_enho,
          li_enh_tool TYPE REF TO if_enh_tool,
          lx_enh_root TYPE REF TO cx_enh_root.

    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lv_enh_id = ms_item-obj_name.
    TRY.
        li_enh_tool = cl_enh_factory=>get_enhancement(
          enhancement_id   = lv_enh_id
          run_dark         = abap_true
          bypassing_buffer = abap_true ).
      CATCH cx_enh_root INTO lx_enh_root.
        Lcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

    li_enho = factory( li_enh_tool->get_tool( ) ).

    li_enho->serialize( ii_xml      = io_xml
                        ii_enh_tool = li_enh_tool ).

    Lcl_abapgit_sotr_handler=>read_sotr(
      iv_pgmid    = 'R3TR'
      iv_object   = ms_item-obj_type
      iv_obj_name = ms_item-obj_name
      io_i18n_params = mo_i18n_params
      io_xml      = io_xml ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENHO implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ENHS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_enhs=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_enhs=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ENHS implementation.
*"* method's implementations
*include methods.
  METHOD factory.

    CASE iv_tool.
      WHEN cl_enh_tool_badi_def=>tooltype.
        CREATE OBJECT ri_enho TYPE Lcl_abapgit_object_enhs_badi_d.
      WHEN cl_enh_tool_hook_def=>tool_type.
        CREATE OBJECT ri_enho TYPE Lcl_abapgit_object_enhs_hook_d.
      WHEN OTHERS.
        Lcx_abapgit_exception=>raise( |ENHS: Unsupported tool { iv_tool }| ).
    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lv_spot_name TYPE enhspotname,
          li_spot_ref  TYPE REF TO if_enh_spot_tool.

    lv_spot_name = ms_item-obj_name.

    TRY.
        li_spot_ref = cl_enh_factory=>get_enhancement_spot( spot_name = lv_spot_name
                                                            run_dark  = abap_true ).
        li_spot_ref->get_attributes( IMPORTING changedby = rv_user ).

      CATCH cx_enh_root.
        rv_user = c_user_unknown.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_spot_name  TYPE enhspotname,
          lx_enh_root   TYPE REF TO cx_enh_root,
          li_enh_object TYPE REF TO if_enh_object.

    lv_spot_name  = ms_item-obj_name.

    TRY.
        li_enh_object ?= cl_enh_factory=>get_enhancement_spot( spot_name = lv_spot_name
                                                               run_dark  = abap_true
                                                               lock      = abap_true ).

        li_enh_object->delete( nevertheless_delete = abap_true
                               run_dark            = abap_true ).

        li_enh_object->unlock( ).

      CATCH cx_enh_root INTO lx_enh_root.
        Lcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_parent    TYPE enhspotcompositename,
          lv_spot_name TYPE enhspotname,
          lv_tool      TYPE enhspottooltype,
          lv_package   LIKE iv_package,
          lx_enh_root  TYPE REF TO cx_enh_root,
          li_spot_ref  TYPE REF TO if_enh_spot_tool,
          li_enhs      TYPE REF TO Lif_abapgit_object_enhs.

    IF Lif_abapgit_object~exists( ) = abap_true.
      Lif_abapgit_object~delete( iv_package   = iv_package
                                 iv_transport = iv_transport ).
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'TOOL'
                  CHANGING  cg_data = lv_tool ).

    lv_spot_name = ms_item-obj_name.
    lv_package   = iv_package.

    TRY.
        cl_enh_factory=>create_enhancement_spot(
          EXPORTING
            spot_name      = lv_spot_name
            tooltype       = lv_tool
            dark           = abap_false
            compositename  = lv_parent
          IMPORTING
            spot           = li_spot_ref
          CHANGING
            devclass       = lv_package ).

      CATCH cx_enh_root INTO lx_enh_root.
        Lcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

    li_enhs = factory( lv_tool ).

    li_enhs->deserialize( ii_xml           = io_xml
                          iv_package       = iv_package
                          ii_enh_spot_tool = li_spot_ref ).

    Lcl_abapgit_sotr_handler=>create_sotr(
      iv_package = iv_package
      io_xml     = io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_spot_name TYPE enhspotname,
          li_spot_ref  TYPE REF TO if_enh_spot_tool.

    lv_spot_name = ms_item-obj_name.

    TRY.
        li_spot_ref = cl_enh_factory=>get_enhancement_spot( spot_name = lv_spot_name
                                                            run_dark  = abap_true ).

        rv_bool = abap_true.

      CATCH cx_enh_root.
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

    DATA: lv_spot_name TYPE enhspotname,
          li_spot_ref  TYPE REF TO if_enh_spot_tool,
          li_enhs      TYPE REF TO Lif_abapgit_object_enhs,
          lx_enh_root  TYPE REF TO cx_enh_root.

    lv_spot_name = ms_item-obj_name.

    TRY.
        li_spot_ref = cl_enh_factory=>get_enhancement_spot( spot_name = lv_spot_name
                                                            run_dark  = abap_true ).

      CATCH cx_enh_root INTO lx_enh_root.
        Lcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

    li_enhs = factory( li_spot_ref->get_tool( ) ).

    li_enhs->serialize( ii_xml           = io_xml
                        ii_enh_spot_tool = li_spot_ref ).

    Lcl_abapgit_sotr_handler=>read_sotr(
      iv_pgmid    = 'R3TR'
      iv_object   = ms_item-obj_type
      iv_obj_name = ms_item-obj_name
      io_i18n_params = mo_i18n_params
      io_xml      = io_xml ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENHS implementation

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
  METHOD Lif_abapgit_object~get_deserialize_order.
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
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
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
endclass. "ZCL_ABAPGIT_OBJECT_ENQU implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ENSC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ensc=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ensc=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ENSC implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lv_spot_name TYPE enhspotcompositename,
          li_spot_ref  TYPE REF TO if_enh_spot_composite,
          lo_spot_ref  TYPE REF TO cl_enh_spot_composite.

    lv_spot_name = ms_item-obj_name.

    TRY.
        li_spot_ref = cl_enh_factory=>get_enhancement_spot_comp(
          lock     = ''
          run_dark = abap_true
          name     = lv_spot_name ).

        lo_spot_ref ?= li_spot_ref.

        lo_spot_ref->if_enh_spot_composite~get_change_attributes( IMPORTING changedby = rv_user ).
      CATCH cx_root.
        rv_user = c_user_unknown.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.
    DATA: lv_spot_name TYPE enhspotcompositename,
          lv_message   TYPE string,
          lx_root      TYPE REF TO cx_root,
          li_spot_ref  TYPE REF TO if_enh_spot_composite.

    lv_spot_name = ms_item-obj_name.

    TRY.
        li_spot_ref = cl_enh_factory=>get_enhancement_spot_comp(
          lock     = abap_true
          run_dark = abap_true
          name     = lv_spot_name ).

        IF li_spot_ref IS BOUND.
          li_spot_ref->if_enh_object~delete(
            nevertheless_delete = abap_true
            run_dark            = abap_true ).
        ENDIF.
        li_spot_ref->if_enh_object~unlock( ).
      CATCH cx_enh_root INTO lx_root.
        lv_message = `Error occured while deleting ENSC: `
          && lx_root->get_text( ).
        Lcx_abapgit_exception=>raise( lv_message ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_spot_name  TYPE enhspotcompositename,
          lv_message    TYPE string,
          lv_enh_shtext TYPE string,
          lv_enh_spot   TYPE enhspotname,
          lt_enh_spots  TYPE enhspotname_it,
          lt_comp_spots TYPE enhspotname_it,
          lx_root       TYPE REF TO cx_root,
          lv_package    LIKE iv_package,
          li_spot_ref   TYPE REF TO if_enh_spot_composite,
          lo_spot_ref   TYPE REF TO cl_enh_spot_composite.


    lv_spot_name = ms_item-obj_name.

    io_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING  cg_data = lv_enh_shtext ).
    io_xml->read( EXPORTING iv_name = 'ENH_SPOTS'     "Enhancement spots
                  CHANGING  cg_data = lt_enh_spots ).
    io_xml->read( EXPORTING iv_name = 'COMP_ENH_SPOTS' "Composite enhancement spots
                  CHANGING  cg_data = lt_comp_spots ).

    IF Lif_abapgit_object~exists( ) = abap_true.
      Lif_abapgit_object~delete( iv_package   = iv_package
                                 iv_transport = iv_transport ).
    ENDIF.

    lv_package = iv_package.

    TRY.
        cl_enh_factory=>create_enhancement_spot_comp(
          EXPORTING
            name      = lv_spot_name
            run_dark  = abap_true
          IMPORTING
            composite = li_spot_ref
          CHANGING
            devclass  = lv_package ).

        lo_spot_ref ?= li_spot_ref.

        lo_spot_ref->if_enh_object_docu~set_shorttext( lv_enh_shtext ).
        "Add subsequent enhancement spots
        LOOP AT lt_enh_spots INTO lv_enh_spot.
          lo_spot_ref->if_enh_spot_composite~add_enh_spot_child( lv_enh_spot ).
        ENDLOOP.
        "Add subsequent composite enhancement spots
        LOOP AT lt_comp_spots INTO lv_enh_spot.
          lo_spot_ref->if_enh_spot_composite~add_composite_child( lv_enh_spot ).
        ENDLOOP.

        lo_spot_ref->if_enh_object~save( ).
        lo_spot_ref->if_enh_object~activate( ).
        lo_spot_ref->if_enh_object~unlock( ).

        Lcl_abapgit_sotr_handler=>create_sotr(
          iv_package = iv_package
          io_xml     = io_xml ).

      CATCH cx_enh_root INTO lx_root.
        lv_message = `Error occured while deserializing ENSC: `
          && lx_root->get_text( ).
        Lcx_abapgit_exception=>raise( lv_message ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_spot_name TYPE enhspotcompositename.


    lv_spot_name = ms_item-obj_name.

    TRY.
        cl_enh_factory=>get_enhancement_spot_comp(
          lock     = ''
          run_dark = abap_true
          name     = lv_spot_name ).
        rv_bool = abap_true.
      CATCH cx_enh_root.
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

    DATA: lv_spot_name  TYPE enhspotcompositename,
          lv_message    TYPE string,
          lv_enh_shtext TYPE string,
          lt_enh_spots  TYPE enhspotname_it,
          lt_comp_spots TYPE enhspotname_it,
          lx_root       TYPE REF TO cx_root,
          li_spot_ref   TYPE REF TO if_enh_spot_composite,
          lo_spot_ref   TYPE REF TO cl_enh_spot_composite.


    lv_spot_name = ms_item-obj_name.

    TRY.
        li_spot_ref = cl_enh_factory=>get_enhancement_spot_comp(
          lock     = ''
          run_dark = abap_true
          name     = lv_spot_name ).

        lo_spot_ref ?= li_spot_ref.

        lv_enh_shtext = li_spot_ref->if_enh_object_docu~get_shorttext( ).
        "find subsequent enhancement spots
        lt_enh_spots = lo_spot_ref->if_enh_spot_composite~get_enh_spot_childs( ).
        "find subsequent composite enhancement spots
        lt_comp_spots = lo_spot_ref->if_enh_spot_composite~get_composite_childs( ).

        io_xml->add( ig_data = lv_enh_shtext
                     iv_name = 'SHORTTEXT' ).
        io_xml->add( ig_data = lt_enh_spots
                     iv_name = 'ENH_SPOTS' ).         "Enhancement spots
        io_xml->add( ig_data = lt_comp_spots
                     iv_name = 'COMP_ENH_SPOTS' ).    "Composite enhancement spots

        Lcl_abapgit_sotr_handler=>read_sotr(
          iv_pgmid    = 'R3TR'
          iv_object   = ms_item-obj_type
          iv_obj_name = ms_item-obj_name
          io_i18n_params = mo_i18n_params
          io_xml      = io_xml ).

      CATCH cx_enh_root INTO lx_root.
        lv_message = `Error occured while serializing ENSC: `
          && lx_root->get_text( ).
        Lcx_abapgit_exception=>raise( lv_message ).
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENSC implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_FDT0 <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_fdt0=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_fdt0=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_FDT0 implementation.
*"* method's implementations
*include methods.
  METHOD before_xml_deserialize.

    DATA lv_application_id TYPE fdt_admn_0000s-application_id.
    DATA lv_timestamp TYPE timestamp.
    DATA lv_transport TYPE string.
    DATA lv_dlvunit TYPE tdevc-dlvunit.
    DATA lo_node_local TYPE REF TO if_ixml_element.
    DATA lo_node_package TYPE REF TO if_ixml_element.
    DATA lo_node_id TYPE REF TO if_ixml_element.
    DATA lo_xml_element TYPE REF TO if_ixml_element.
    DATA lv_count TYPE i.

    lo_node_local = co_dom_tree->find_from_name( name      = 'Local'
                                                 namespace = 'FDTNS' ).

    IF lo_node_local IS BOUND.
      ev_is_local = lo_node_local->get_value( ).
    ENDIF.

    lo_node_package = co_dom_tree->find_from_name(
      name      = 'DevelopmentPackage'
      namespace = 'FDTNS' ).
    IF lo_node_package IS BOUND.
      lo_node_package->set_value( |{ iv_package }| ).
    ENDIF.

    lo_node_id = co_dom_tree->find_from_name(
      name      = 'ApplicationId'
      namespace = 'FDTNS' ).
    IF lo_node_id IS BOUND.
      lv_application_id = lo_node_id->get_value( ).
      SELECT COUNT( * ) FROM fdt_admn_0000s INTO lv_count
        WHERE object_type = 'AP'
        AND id = lv_application_id
        AND deleted = ''.
      ev_create = boolc( lv_count = 0 ).
    ENDIF.

    " Fill in user/time/system-specific fields
    GET TIME STAMP FIELD lv_timestamp.
    lv_transport = |${ sy-sysid }0000000000000001|.

    lo_xml_element = co_dom_tree->get_root_element( ).

    IF ev_create = abap_true.
      set_field(
        EXPORTING
          iv_name         = 'CreationUser'
          iv_value        = |{ sy-uname }|
        CHANGING
          co_ixml_element = lo_xml_element ).

      set_field(
        EXPORTING
          iv_name         = 'CreationTimestamp'
          iv_value        = |{ lv_timestamp }|
        CHANGING
          co_ixml_element = lo_xml_element ).
    ENDIF.

    set_field(
      EXPORTING
        iv_name         = 'ChangeUser'
        iv_value        = |{ sy-uname }|
      CHANGING
        co_ixml_element = lo_xml_element ).

    set_field(
      EXPORTING
        iv_name         = 'ChangeTimestamp'
        iv_value        = |{ lv_timestamp }|
      CHANGING
        co_ixml_element = lo_xml_element ).

    set_field(
      EXPORTING
        iv_name         = 'User'
        iv_value        = |{ sy-uname }|
      CHANGING
        co_ixml_element = lo_xml_element ).

    set_field(
      EXPORTING
        iv_name         = 'Timestamp'
        iv_value        = |{ lv_timestamp }|
      CHANGING
        co_ixml_element = lo_xml_element ).

    set_field(
      EXPORTING
        iv_name         = 'Trrequest'
        iv_value        = lv_transport
      CHANGING
        co_ixml_element = lo_xml_element ).

    set_field(
      EXPORTING
        iv_name         = 'Trversion'
        iv_value        = '000001'
      CHANGING
        co_ixml_element = lo_xml_element ).

    set_field(
      EXPORTING
        iv_name         = 'Trtimestamp'
        iv_value        = |{ lv_timestamp }|
      CHANGING
        co_ixml_element = lo_xml_element ).

    set_field(
      EXPORTING
        iv_name         = 'Trsysid'
        iv_value        = |{ sy-sysid }|
      CHANGING
        co_ixml_element = lo_xml_element ).

    set_field(
      EXPORTING
        iv_name         = 'Trclient'
        iv_value        = |{ sy-mandt }|
      CHANGING
        co_ixml_element = lo_xml_element ).

    set_field(
      EXPORTING
        iv_name         = 'OversId'
        iv_value        = |{ lv_application_id }|
      CHANGING
        co_ixml_element = lo_xml_element ).

    SELECT SINGLE dlvunit FROM tdevc INTO lv_dlvunit WHERE devclass = iv_package.
    IF sy-subrc = 0.
      set_field(
        EXPORTING
          iv_name         = 'SoftwareComponent'
          iv_value        = |{ lv_dlvunit }|
        CHANGING
          co_ixml_element = lo_xml_element ).
    ENDIF.

    lo_xml_element->set_attribute(
      name  = 'Client'
      value = |{ sy-mandt }| ).
    lo_xml_element->set_attribute(
      name  = 'Date'
      value = |{ sy-datum }| ).
    lo_xml_element->set_attribute(
      name  = 'SAPRelease'
      value = |{ sy-saprl }| ).
    lo_xml_element->set_attribute(
      name  = 'Server'
      value = |{ sy-host }| ).
    lo_xml_element->set_attribute(
      name  = 'SourceExportReqID'
      value = lv_transport ).
    lo_xml_element->set_attribute(
      name  = 'SystemID'
      value = |{ sy-sysid }| ).
    lo_xml_element->set_attribute(
      name  = 'Time'
      value = |{ sy-uzeit }| ).
    lo_xml_element->set_attribute(
      name  = 'User'
      value = |{ sy-uname }| ).

  ENDMETHOD.
  METHOD check_is_local.

    SELECT SINGLE local_object FROM fdt_admn_0000s INTO rv_is_local
      WHERE object_type = 'AP'
      AND name = ms_item-obj_name.

  ENDMETHOD.
  METHOD filter_xml_serialize.

    DATA lo_components_node TYPE REF TO if_ixml_element.

    lo_components_node = co_ixml_element->find_from_name( name      = 'ComponentReleases'
                                                          namespace = 'FDTNS' ).
    IF lo_components_node IS BOUND.
      co_ixml_element->remove_child( lo_components_node ).
    ENDIF.

    " Clear user/time/system-specific fields
    set_field(
      EXPORTING
        iv_name         = 'CreationUser'
      CHANGING
        co_ixml_element = co_ixml_element ).

    set_field(
      EXPORTING
        iv_name         = 'CreationTimestamp'
      CHANGING
        co_ixml_element = co_ixml_element ).

    set_field(
      EXPORTING
        iv_name         = 'ChangeUser'
      CHANGING
        co_ixml_element = co_ixml_element ).

    set_field(
      EXPORTING
        iv_name         = 'ChangeTimestamp'
      CHANGING
        co_ixml_element = co_ixml_element ).

    set_field(
      EXPORTING
        iv_name         = 'User'
      CHANGING
        co_ixml_element = co_ixml_element ).

    set_field(
      EXPORTING
        iv_name         = 'Timestamp'
      CHANGING
        co_ixml_element = co_ixml_element ).

    set_field(
      EXPORTING
        iv_name         = 'Trrequest'
      CHANGING
        co_ixml_element = co_ixml_element ).

    set_field(
      EXPORTING
        iv_name         = 'Trversion'
      CHANGING
        co_ixml_element = co_ixml_element ).

    set_field(
      EXPORTING
        iv_name         = 'Trtimestamp'
      CHANGING
        co_ixml_element = co_ixml_element ).

    set_field(
      EXPORTING
        iv_name         = 'Trsysid'
      CHANGING
        co_ixml_element = co_ixml_element ).

    set_field(
      EXPORTING
        iv_name         = 'Trclient'
      CHANGING
        co_ixml_element = co_ixml_element ).

    set_field(
      EXPORTING
        iv_name         = 'OversId'
      CHANGING
        co_ixml_element = co_ixml_element ).

    set_field(
      EXPORTING
        iv_name         = 'SoftwareComponent'
      CHANGING
        co_ixml_element = co_ixml_element ).

    set_field(
      EXPORTING
        iv_name         = 'DevelopmentPackage'
      CHANGING
        co_ixml_element = co_ixml_element ).

    " Clear attributes of root FDTNS:Fdt node
    co_ixml_element->set_attribute(
      name  = 'Client'
      value = '' ).
    co_ixml_element->set_attribute(
      name  = 'Date'
      value = '' ).
    co_ixml_element->set_attribute(
      name  = 'SAPRelease'
      value = '' ).
    co_ixml_element->set_attribute(
      name  = 'Server'
      value = '' ).
    co_ixml_element->set_attribute(
      name  = 'SourceExportReqID'
      value = '' ).
    co_ixml_element->set_attribute(
      name  = 'SystemID'
      value = '' ).
    co_ixml_element->set_attribute(
      name  = 'Time'
      value = '' ).
    co_ixml_element->set_attribute(
      name  = 'User'
      value = '' ).

  ENDMETHOD.
  METHOD get_application_id.

    SELECT SINGLE application_id FROM fdt_admn_0000s INTO rv_application_id
      WHERE object_type = 'AP'
      AND name = ms_item-obj_name.

  ENDMETHOD.
  METHOD set_field.

    DATA:
      lo_node_collection TYPE REF TO if_ixml_node_collection,
      lo_node            TYPE REF TO if_ixml_node,
      lv_index           TYPE i.

    lo_node_collection = co_ixml_element->get_elements_by_tag_name(
      namespace = 'FDTNS'
      name      = iv_name ).

    lv_index = 0.
    WHILE lv_index < lo_node_collection->get_length( ).
      lo_node = lo_node_collection->get_item( lv_index ).
      lo_node->set_value( iv_value ).
      lv_index = lv_index + 1.
    ENDWHILE.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA lv_ch_user TYPE fdt_admn_0000s-ch_user.

    SELECT SINGLE ch_user FROM fdt_admn_0000s INTO lv_ch_user
      WHERE object_type = 'AP'
      AND name = ms_item-obj_name.

    rv_user = lv_ch_user.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA lv_is_local TYPE abap_bool.
    DATA lt_application_id TYPE TABLE OF fdt_admn_0000s-application_id.
    DATA ls_object_category_sel TYPE if_fdt_query=>s_object_category_sel.
    DATA lv_failure TYPE abap_bool.
    DATA lx_fdt_input TYPE REF TO cx_fdt_input.

    lv_is_local = check_is_local( ).

    SELECT application_id FROM fdt_admn_0000s INTO TABLE lt_application_id
      WHERE object_type = 'AP'
      AND name = ms_item-obj_name.

    ls_object_category_sel-system_objects = 'X'.

    TRY.
        IF lv_is_local = abap_true.

          cl_fdt_delete_handling=>mark_for_delete_via_job(
            EXPORTING
              is_object_category_sel     = ls_object_category_sel
              ita_application_id         = lt_application_id
              iv_background              = abap_true
              iv_local_option            = '1'
              iv_appl_transported_option = '2'
              iv_obj_transported_option  = '2'
            IMPORTING
              ev_failure                 = lv_failure ).
          IF lv_failure IS INITIAL.
            cl_fdt_delete_handling=>delete_logical_via_job(
              EXPORTING
                is_object_category_sel     = ls_object_category_sel
                ita_application_id         = lt_application_id
                iv_retention_time          = 0
                iv_background              = abap_true
                iv_local_option            = '1'
                iv_appl_transported_option = '2'
                iv_obj_transported_option  = '2'
              IMPORTING
                ev_failure                 = lv_failure ).
            IF lv_failure IS INITIAL.
              cl_fdt_delete_handling=>delete_physical_via_job(
                EXPORTING
                  is_object_category_sel     = ls_object_category_sel
                  ita_application_id         = lt_application_id
                  iv_retention_time          = 0
                  iv_background              = abap_true
                  iv_local_option            = '1'
                  iv_appl_transported_option = '2'
                IMPORTING
                  ev_failure                 = lv_failure ).
            ENDIF.
          ENDIF.

        ELSE.

          tadir_insert( iv_package ).

          corr_insert( iv_package ).

          cl_fdt_delete_handling=>mark_for_delete_via_job(
            EXPORTING
              is_object_category_sel     = ls_object_category_sel
              ita_application_id         = lt_application_id
              iv_background              = abap_true
              iv_local_option            = '2'
              iv_appl_transported_option = '1'
              iv_obj_transported_option  = '1'
            IMPORTING
              ev_failure                 = lv_failure ).
          IF lv_failure IS INITIAL.
            cl_fdt_delete_handling=>delete_logical_via_job(
              EXPORTING
                is_object_category_sel     = ls_object_category_sel
                ita_application_id         = lt_application_id
                iv_retention_time          = 0
                iv_background              = abap_true
                iv_local_option            = '2'
                iv_appl_transported_option = '1'
                iv_obj_transported_option  = '1'
              IMPORTING
                ev_failure                 = lv_failure ).
            IF lv_failure IS INITIAL.
              cl_fdt_delete_handling=>delete_physical_via_job(
                EXPORTING
                  is_object_category_sel     = ls_object_category_sel
                  ita_application_id         = lt_application_id
                  iv_retention_time          = 0
                  iv_background              = abap_true
                  iv_local_option            = '2'
                  iv_appl_transported_option = '1'
                IMPORTING
                  ev_failure                 = lv_failure ).
            ENDIF.
          ENDIF.

        ENDIF.

        IF lv_failure = abap_true.
          Lcx_abapgit_exception=>raise( |Error deleting { ms_item-obj_type } { ms_item-obj_name }| ).
        ENDIF.

      CATCH cx_fdt_input INTO lx_fdt_input.
        Lcx_abapgit_exception=>raise_with_text( lx_fdt_input ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA lo_dexc TYPE REF TO if_fdt_data_exchange.
    DATA lx_fdt_input TYPE REF TO cx_fdt_input.
    DATA lo_dom_tree TYPE REF TO if_ixml_document.
    DATA lv_transportable_package TYPE abap_bool.
    DATA lv_is_local TYPE abap_bool.
    DATA lt_message TYPE if_fdt_types=>t_message.
    DATA lv_create TYPE abap_bool.

    FIELD-SYMBOLS <ls_message> TYPE if_fdt_types=>s_message.

    lo_dom_tree = io_xml->get_raw( ).

    before_xml_deserialize(
      EXPORTING
        iv_package  = iv_package
      IMPORTING
        ev_create   = lv_create
        ev_is_local = lv_is_local
      CHANGING
        co_dom_tree = lo_dom_tree ).

    lv_transportable_package = Lcl_abapgit_factory=>get_sap_package( iv_package )->are_changes_recorded_in_tr_req( ).

    IF lv_transportable_package = abap_true AND lv_is_local = abap_true.
      Lcx_abapgit_exception=>raise( 'Local applications can only be imported into a local package' ).
    ELSEIF lv_transportable_package = abap_false AND lv_is_local = abap_false.
      Lcx_abapgit_exception=>raise( 'Transportable application can only be imported into transportable package' ).
    ENDIF.

    lo_dexc = cl_fdt_factory=>if_fdt_factory~get_instance( )->get_data_exchange( ).

    TRY.

        IF lv_is_local = abap_true. "Local Object

          lo_dexc->import_xml(
            EXPORTING
              io_dom_tree = lo_dom_tree
              iv_create   = lv_create
              iv_activate = abap_true
              iv_simulate = abap_false
            IMPORTING
              et_message  = lt_message ).

        ELSE. "Transportable Object

          tadir_insert( iv_package ).

          corr_insert( iv_package ).

          lo_dexc->import_xml(
            EXPORTING
              io_dom_tree            = lo_dom_tree
              iv_create              = lv_create
              iv_activate            = abap_true
              iv_simulate            = abap_false
              iv_workbench_trrequest = iv_transport
            IMPORTING
              et_message             = lt_message ).

        ENDIF.

        LOOP AT lt_message ASSIGNING <ls_message>.
          ii_log->add(
            iv_msg  = <ls_message>-text
            iv_type = <ls_message>-msgty ).
        ENDLOOP.

      CATCH cx_fdt_input INTO lx_fdt_input.
        Lcx_abapgit_exception=>raise_with_text( lx_fdt_input ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA lv_count TYPE i.

    SELECT COUNT( * ) FROM fdt_admn_0000s INTO lv_count
      WHERE object_type = 'AP'
      AND name = ms_item-obj_name
      AND deleted = ''.

    rv_bool = boolc( lv_count > 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.

    DATA lo_local_version_output TYPE REF TO Lcl_abapgit_xml_output.
    DATA lo_local_version_input  TYPE REF TO Lcl_abapgit_xml_input.

    CREATE OBJECT lo_local_version_output.
    Lif_abapgit_object~serialize( lo_local_version_output ).

    CREATE OBJECT lo_local_version_input
      EXPORTING
        iv_xml = lo_local_version_output->Lif_abapgit_xml_output~render( ).

    CREATE OBJECT ri_comparator TYPE Lcl_abapgit_object_tabl_compar
      EXPORTING
        ii_local = lo_local_version_input.

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

    DATA lv_application_id TYPE fdt_admn_0000s-application_id.
    DATA lx_fdt_input TYPE REF TO cx_fdt_input.
    DATA lo_instance TYPE REF TO if_fdt_admin_data.
    DATA lt_version TYPE if_fdt_admin_data=>ts_version.
    DATA lv_index TYPE sy-tabix.

    FIELD-SYMBOLS <ls_version> LIKE LINE OF lt_version.

    lv_application_id = get_application_id( ).

    TRY.
        cl_fdt_factory=>get_instance_generic(
          EXPORTING
            iv_id       = lv_application_id
          IMPORTING
            eo_instance = lo_instance ).

      CATCH cx_fdt_input INTO lx_fdt_input.
        Lcx_abapgit_exception=>raise_with_text( lx_fdt_input ).
    ENDTRY.

    lo_instance->get_versions( IMPORTING ets_version = lt_version ).
    lv_index = lines( lt_version ).
    READ TABLE lt_version ASSIGNING <ls_version> INDEX lv_index.

    rv_active = boolc( <ls_version>-state = 'A' ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    DATA lv_application_id TYPE string.

    lv_application_id = get_application_id( ).

    rv_is_locked = exists_a_lock_entry_for(
      iv_lock_object = 'FDT_ENQUEUE_ID'
      iv_argument    = |$ST{ lv_application_id }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA lv_application_id TYPE fdt_admn_0000s-application_id.
    DATA lx_root TYPE REF TO cx_root.
    DATA lo_fdt_wd TYPE REF TO if_fdt_wd_factory.

    lv_application_id = get_application_id( ).

    IF lv_application_id IS NOT INITIAL.
      TRY.
          lo_fdt_wd = cl_fdt_wd_factory=>if_fdt_wd_factory~get_instance( ).
          lo_fdt_wd->get_ui_execution( )->execute_workbench( iv_id = lv_application_id ).
        CATCH cx_root INTO lx_root.
          Lcx_abapgit_exception=>raise_with_text( lx_root ).
      ENDTRY.
    ELSE.
      Lcx_abapgit_exception=>raise( 'Could not open BRF+ Workbench' ).
    ENDIF.

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA lo_dexc TYPE REF TO if_fdt_data_exchange.
    DATA lv_application_id TYPE fdt_admn_0000s-application_id.
    DATA lx_fdt_input TYPE REF TO cx_fdt_input.
    DATA lv_xml_fdt0_application TYPE string.
    DATA lo_xml_document TYPE REF TO if_ixml_document.
    DATA lo_xml_element TYPE REF TO if_ixml_element.

    lv_application_id = get_application_id( ).

    lo_dexc = cl_fdt_factory=>if_fdt_factory~get_instance( )->get_data_exchange( ).

    TRY.
        lo_dexc->export_xml_application(
          EXPORTING
            iv_application_id = lv_application_id
            iv_schema         = if_fdt_data_exchange=>gc_xml_schema_type_external
            iv_xml_version    = if_fdt_data_exchange=>gc_xml_version
          IMPORTING
            ev_string         = lv_xml_fdt0_application ).

        lo_xml_document = cl_ixml_80_20=>parse_to_document( stream_string = lv_xml_fdt0_application ).
        lo_xml_element = lo_xml_document->get_root_element( ).

        filter_xml_serialize( CHANGING co_ixml_element = lo_xml_element ).

        io_xml->set_raw( lo_xml_element ).

      CATCH cx_fdt_input INTO lx_fdt_input.
        Lcx_abapgit_exception=>raise_with_text( lx_fdt_input ).
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_FDT0 implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_FORM <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_form=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_form=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_FORM implementation.
*"* method's implementations
*include methods.
  METHOD build_extra_from_header.

    DATA lv_tdspras TYPE laiso.

    lv_tdspras = Lcl_abapgit_convert=>conversion_exit_isola_output( is_header-tdspras ).

    rv_result = c_objectname_tdlines && '_' && lv_tdspras.

  ENDMETHOD.
  METHOD build_extra_from_header_old.
    rv_result = c_objectname_tdlines && '_' && is_header-tdspras.
  ENDMETHOD.
  METHOD compress_lines.

    DATA lv_string TYPE string.
    DATA li_xml TYPE REF TO Lif_abapgit_xml_output.

    CREATE OBJECT li_xml TYPE Lcl_abapgit_xml_output.
    li_xml->add( iv_name = c_objectname_tdlines
                 ig_data = it_lines ).
    lv_string = li_xml->render( ).
    IF lv_string IS NOT INITIAL.
      Lif_abapgit_object~mo_files->add_string( iv_extra  =
                    build_extra_from_header( is_form_data-form_header )
                            iv_ext    = c_extension_xml
                            iv_string = lv_string ).
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_form_name = ms_item-obj_name.

  ENDMETHOD.
  METHOD extract_tdlines.

    DATA lv_string TYPE string.
    DATA li_xml TYPE REF TO Lif_abapgit_xml_input.

    TRY.
        lv_string = Lif_abapgit_object~mo_files->read_string( iv_extra =
                                   build_extra_from_header( is_form_data-form_header )
                                           iv_ext   = c_extension_xml ).
      CATCH Lcx_abapgit_exception.

        lv_string = Lif_abapgit_object~mo_files->read_string( iv_extra =
                               build_extra_from_header_old( is_form_data-form_header )
                                           iv_ext   = c_extension_xml ).

    ENDTRY.

    CREATE OBJECT li_xml TYPE Lcl_abapgit_xml_input EXPORTING iv_xml = lv_string.
    li_xml->read( EXPORTING iv_name = c_objectname_tdlines
                  CHANGING  cg_data = rt_lines ).

  ENDMETHOD.
  METHOD find_form.

    DATA: lv_text_name TYPE thead-tdname.

    lv_text_name = iv_object_name.

    CALL FUNCTION 'SELECT_TEXT'
      EXPORTING
        database_only = abap_true
        id            = 'TXT'
        language      = '*'
        name          = lv_text_name
        object        = c_objectname_form
      TABLES
        selections    = rt_text_header
      EXCEPTIONS
        OTHERS        = 1 ##FM_SUBRC_OK.  "#EC CI_SUBRC

  ENDMETHOD.
  METHOD get_last_changes.

    DATA: lv_form_name         TYPE thead-tdform.

    CLEAR rs_last_changed.

    lv_form_name = iv_form_name.

    CALL FUNCTION 'READ_FORM'
      EXPORTING
        form             = lv_form_name
        read_only_header = abap_true
      IMPORTING
        form_header      = rs_last_changed.

  ENDMETHOD.
  METHOD order_check_and_insert.

    DATA: lv_order TYPE e071k-trkorr.

    CALL FUNCTION 'SAPSCRIPT_ORDER_CHECK'
      EXPORTING
        objecttype           = ms_item-obj_type
        form                 = mv_form_name
      EXCEPTIONS
        invalid_input        = 1
        object_locked        = 2
        object_not_available = 3
        OTHERS               = 4.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'SAPSCRIPT_ORDER_INSERT'
      EXPORTING
        objecttype     = ms_item-obj_type
        form           = mv_form_name
        masterlang     = mv_language
      CHANGING
        order          = lv_order
      EXCEPTIONS
        invalid_input  = 1
        order_canceled = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: ls_last_changed TYPE ty_s_form_header.

    ls_last_changed = get_last_changes( ms_item-obj_name ).

    IF ls_last_changed-tdluser IS NOT INITIAL.
      rv_user = ls_last_changed-tdluser.
    ELSE.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    CALL FUNCTION 'DELETE_FORM'
      EXPORTING
        form     = mv_form_name
        language = '*'.

    order_check_and_insert( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lt_form_data            TYPE ty_t_form_data.
    DATA: lt_lines TYPE ty_t_lines.
    FIELD-SYMBOLS: <ls_form_data> TYPE LINE OF ty_t_form_data.

    io_xml->read( EXPORTING iv_name = c_objectname_form
                  CHANGING  cg_data = lt_form_data ).

    LOOP AT lt_form_data ASSIGNING <ls_form_data>.

      lt_lines = extract_tdlines( <ls_form_data> ).

      _save_form( EXPORTING it_lines     = lt_lines
                  CHANGING  cs_form_data = <ls_form_data> ).

    ENDLOOP.

    CALL FUNCTION 'SAPSCRIPT_DELETE_LOAD'
      EXPORTING
        delete = abap_true
        form   = '*'
        write  = space.

    tadir_insert( iv_package ).

    order_check_and_insert( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    CALL FUNCTION 'READ_FORM'
      EXPORTING
        form             = mv_form_name
        read_only_header = abap_true
      IMPORTING
        found            = rv_bool.

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

    DATA: lv_object TYPE seqg3-garg.

    " example lock entry
    "'001FORM      ZTEST_SAPSCRIPT                                                       TXT'
    lv_object = |{ sy-mandt }{ ms_item-obj_type }      { ms_item-obj_name }|.
    OVERLAY lv_object WITH '                                                                                   '.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESSFORM'
                                            iv_argument    = lv_object ).


  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPMSSCF'.
    <ls_bdcdata>-dynpro   = '1102'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=SHOW'.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'RSSCF-TDFORM'.
    <ls_bdcdata>-fval = ms_item-obj_name.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SE71'
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

    DATA: lt_form_data              TYPE ty_t_form_data.
    DATA: ls_form_data              TYPE ty_s_form_data.
    DATA: lt_text_header            TYPE ty_t_text_header.
    DATA: lt_lines                  TYPE ty_t_lines.
    DATA: lv_form_found             TYPE abap_bool.
    FIELD-SYMBOLS: <ls_text_header> LIKE LINE OF lt_text_header.

    lt_text_header = find_form( ms_item-obj_name ).

    LOOP AT lt_text_header ASSIGNING <ls_text_header>.
      CLEAR lt_lines.
      CLEAR ls_form_data.

      _read_form( EXPORTING is_text_header = <ls_text_header>
                  IMPORTING ev_form_found = lv_form_found
                            es_form_data  = ls_form_data
                            et_lines      = lt_lines ).

      IF lv_form_found = abap_true.

        _clear_changed_fields( CHANGING cs_form_data = ls_form_data ).

        compress_lines( is_form_data = ls_form_data
                        it_lines     = lt_lines ).

        INSERT ls_form_data INTO TABLE lt_form_data.

      ENDIF.

    ENDLOOP.

    IF lt_form_data IS NOT INITIAL.

      io_xml->add( iv_name = c_objectname_form
                   ig_data = lt_form_data ).

    ENDIF.

  ENDMETHOD.
  METHOD _clear_changed_fields.

    CLEAR: cs_form_data-form_header-tdfuser,
           cs_form_data-form_header-tdfdate,
           cs_form_data-form_header-tdftime,
           cs_form_data-form_header-tdfreles,
           cs_form_data-form_header-tdluser,
           cs_form_data-form_header-tdldate,
           cs_form_data-form_header-tdltime,
           cs_form_data-form_header-tdlreles.
    CLEAR: cs_form_data-text_header-tdfuser,
           cs_form_data-text_header-tdfdate,
           cs_form_data-text_header-tdftime,
           cs_form_data-text_header-tdfreles,
           cs_form_data-text_header-tdluser,
           cs_form_data-text_header-tdldate,
           cs_form_data-text_header-tdltime,
           cs_form_data-text_header-tdlreles.

  ENDMETHOD.
  METHOD _read_form.

    CLEAR es_form_data.

    CALL FUNCTION 'READ_FORM'
      EXPORTING
        form         = is_text_header-tdform
        language     = is_text_header-tdspras
        status       = ' '
      IMPORTING
        form_header  = es_form_data-form_header
        found        = ev_form_found
        header       = es_form_data-text_header
        olanguage    = es_form_data-orig_language
      TABLES
        form_lines   = et_lines
        pages        = es_form_data-pages
        page_windows = es_form_data-page_windows
        paragraphs   = es_form_data-paragraphs
        strings      = es_form_data-strings
        tabs         = es_form_data-tabs
        windows      = es_form_data-windows.

    _sort_tdlines_by_windows( CHANGING ct_form_windows  = es_form_data-windows
                                       ct_lines         = et_lines ).

    es_form_data-form_header-tdversion = '00001'.
    es_form_data-text_header-tdversion = '00001'.

  ENDMETHOD.
  METHOD _save_form.

    CALL FUNCTION 'SAVE_FORM'
      EXPORTING
        form_header  = cs_form_data-form_header
      TABLES
        form_lines   = it_lines
        pages        = cs_form_data-pages
        page_windows = cs_form_data-page_windows
        paragraphs   = cs_form_data-paragraphs
        strings      = cs_form_data-strings
        tabs         = cs_form_data-tabs
        windows      = cs_form_data-windows.

    CALL FUNCTION 'SAPSCRIPT_CHANGE_OLANGUAGE'
      EXPORTING
        forced    = abap_true
        name      = cs_form_data-text_header-tdname
        object    = cs_form_data-text_header-tdobject
        olanguage = cs_form_data-orig_language
      EXCEPTIONS
        OTHERS    = 1 ##FM_SUBRC_OK.                                                   "#EC CI_SUBRC

  ENDMETHOD.
  METHOD _sort_tdlines_by_windows.
    DATA lt_lines        TYPE ty_t_lines.
    DATA ls_lines        LIKE LINE OF lt_lines.
    DATA ls_form_windows LIKE LINE OF ct_form_windows.
    DATA lv_elt_windows  TYPE tdformat VALUE '/W'.
    DATA lv_firstloop    TYPE abap_bool.

    lt_lines = ct_lines.
    CLEAR ct_lines.

    SORT ct_form_windows BY tdwindow.

    LOOP AT ct_form_windows INTO ls_form_windows.
      lv_firstloop = abap_true.
      READ TABLE lt_lines INTO ls_lines WITH KEY tdformat = lv_elt_windows
                                                 tdline   = ls_form_windows-tdwindow.
      IF sy-subrc <> 0.
        CONTINUE. " current loop
      ENDIF.
      LOOP AT lt_lines INTO ls_lines FROM sy-tabix.
        IF lv_firstloop = abap_false AND
           ls_lines-tdformat = lv_elt_windows.
          EXIT.
        ENDIF.
        APPEND ls_lines TO ct_lines.
        lv_firstloop = abap_false.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_FORM implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_FTGL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ftgl=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ftgl=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_FTGL implementation.
*"* method's implementations
*include methods.
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

    mv_toggle_id = ms_item-obj_name.

    TRY.
        CREATE DATA mr_toggle TYPE ('FTGL_S_WB_FEATURE_TOGGLE').
      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |FTGL not supported in your NW release| ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE changedby FROM ('FTGL_ID') INTO rv_user
      WHERE feature_id = ms_item-obj_name AND version = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA:
      lv_return_code TYPE i.

    CALL METHOD ('CL_FEATURE_TOGGLE_OBJECT')=>delete
      EXPORTING
        iv_toggle_id = mv_toggle_id
      RECEIVING
        rv_rc        = lv_return_code.

    IF lv_return_code <> 0.
      Lcx_abapgit_exception=>raise( |Cannot delete feature toggle { mv_toggle_id }. |
                                 && |Error { sy-subrc } from cl_feature_toggle_object=>delete| ).
    ENDIF.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA:
      lo_toggle TYPE REF TO object,
      lx_error  TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_toggle> TYPE data.

    ASSIGN mr_toggle->* TO <lg_toggle>.
    ASSERT sy-subrc = 0.

    io_xml->read(
      EXPORTING
        iv_name = 'FTGL'
      CHANGING
        cg_data = <lg_toggle> ).

    TRY.
        CALL METHOD ('CL_FEATURE_TOGGLE_OBJECT')=>create_toggle_by_content
          EXPORTING
            is_content = <lg_toggle>
          RECEIVING
            ro_toggle  = lo_toggle.

        CALL METHOD lo_toggle->('SAVE').

        tadir_insert( iv_package ).
        corr_insert( iv_package ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    CALL METHOD ('CL_FEATURE_TOGGLE')=>is_defined
      EXPORTING
        iv_toggle_id = mv_toggle_id
      RECEIVING
        rc_exists    = rv_bool.

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
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'E_FTGL'
                                            iv_argument    = |{ mv_toggle_id }*| ).
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
      lx_error  TYPE REF TO cx_root,
      lo_toggle TYPE REF TO object.

    FIELD-SYMBOLS: <lg_toggle> TYPE data.

    ASSIGN mr_toggle->* TO <lg_toggle>.
    ASSERT sy-subrc = 0.

    TRY.
        CALL METHOD ('CL_FEATURE_TOGGLE_OBJECT')=>create_toggle_by_id
          EXPORTING
            iv_toggle_id = mv_toggle_id
          RECEIVING
            ro_toggle    = lo_toggle.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    CALL METHOD lo_toggle->('GET_CONTENT')
      RECEIVING
        rs_content = <lg_toggle>.

    clear_field( EXPORTING iv_fieldname = 'HEADER-OWNER'        CHANGING cg_header = <lg_toggle> ).
    clear_field( EXPORTING iv_fieldname = 'HEADER-CREATED_DATE' CHANGING cg_header = <lg_toggle> ).
    clear_field( EXPORTING iv_fieldname = 'HEADER-CREATED_TIME' CHANGING cg_header = <lg_toggle> ).
    clear_field( EXPORTING iv_fieldname = 'HEADER-CHANGEDBY   ' CHANGING cg_header = <lg_toggle> ).
    clear_field( EXPORTING iv_fieldname = 'HEADER-CHANGED_DATE' CHANGING cg_header = <lg_toggle> ).
    clear_field( EXPORTING iv_fieldname = 'HEADER-CHANGED_TIME' CHANGING cg_header = <lg_toggle> ).

    io_xml->add(
        iv_name = 'FTGL'
        ig_data = <lg_toggle> ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_FTGL implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_FUGR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_fugr=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_fugr=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_FUGR implementation.
*"* method's implementations
*include methods.
  METHOD check_rfc_parameters.

* function module RS_FUNCTIONMODULE_INSERT does the same deep down, but the right error
* message is not returned to the user, this is a workaround to give a proper error
* message to the user

    DATA: ls_parameter TYPE rsfbpara,
          lt_fupa      TYPE rsfb_param,
          ls_fupa      LIKE LINE OF lt_fupa.


    IF is_function-remote_call = 'R'.
      cl_fb_parameter_conversion=>convert_parameter_old_to_fupa(
        EXPORTING
          functionname = is_function-funcname
          import       = is_function-import
          export       = is_function-export
          change       = is_function-changing
          tables       = is_function-tables
          except       = is_function-exception
        IMPORTING
          fupararef    = lt_fupa ).

      LOOP AT lt_fupa INTO ls_fupa WHERE paramtype = 'I' OR paramtype = 'E' OR paramtype = 'C' OR paramtype = 'T'.
        cl_fb_parameter_conversion=>convert_intern_to_extern(
          EXPORTING
            parameter_db  = ls_fupa
          IMPORTING
            parameter_vis = ls_parameter ).

        CALL FUNCTION 'RS_FB_CHECK_PARAMETER_REMOTE'
          EXPORTING
            parameter             = ls_parameter
            basxml_enabled        = is_function-remote_basxml
          EXCEPTIONS
            not_remote_compatible = 1
            OTHERS                = 2.
        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise_t100( ).
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD deserialize_functions.

    DATA: lv_include   TYPE rs38l-include,
          lv_area      TYPE rs38l-area,
          lv_group     TYPE rs38l-area,
          lv_namespace TYPE rs38l-namespace,
          lt_source    TYPE TABLE OF abaptxt255,
          lv_msg       TYPE string,
          lx_error     TYPE REF TO Lcx_abapgit_exception.

    FIELD-SYMBOLS: <ls_func> LIKE LINE OF it_functions.

    LOOP AT it_functions ASSIGNING <ls_func>.

      lt_source = Lif_abapgit_object~mo_files->read_abap( iv_extra = <ls_func>-funcname ).

      lv_area = ms_item-obj_name.

      CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
        EXPORTING
          complete_area = lv_area
        IMPORTING
          namespace     = lv_namespace
          group         = lv_group
        EXCEPTIONS
          OTHERS        = 12.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
        ii_log->add_error( iv_msg  = |Function module { <ls_func>-funcname }: { lv_msg }|
                           is_item = ms_item ).
        CONTINUE. "with next function module
      ENDIF.

      IF Lcl_abapgit_factory=>get_function_module( )->function_exists( <ls_func>-funcname ) = abap_true.
* delete the function module to make sure the parameters are updated
* havent found a nice way to update the paramters
        CALL FUNCTION 'FUNCTION_DELETE'
          EXPORTING
            funcname                 = <ls_func>-funcname
            suppress_success_message = abap_true
          EXCEPTIONS
            error_message            = 1
            OTHERS                   = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
          ii_log->add_error( iv_msg = |Function module { <ls_func>-funcname }: { lv_msg }|
                             is_item = ms_item ).
          CONTINUE. "with next function module
        ENDIF.
      ENDIF.

      TRY.
          check_rfc_parameters( <ls_func> ).
        CATCH Lcx_abapgit_exception INTO lx_error.
          ii_log->add_error(
            iv_msg  = |Function module { <ls_func>-funcname }: { lx_error->get_text( ) }|
            is_item = ms_item ).
          CONTINUE. "with next function module
      ENDTRY.

      CALL FUNCTION 'RS_FUNCTIONMODULE_INSERT'
        EXPORTING
          funcname                = <ls_func>-funcname
          function_pool           = lv_group
          interface_global        = <ls_func>-global_flag
          remote_call             = <ls_func>-remote_call
          short_text              = <ls_func>-short_text
          update_task             = <ls_func>-update_task
          exception_class         = <ls_func>-exception_classes
          namespace               = lv_namespace
          remote_basxml_supported = <ls_func>-remote_basxml
          corrnum                 = iv_transport
        IMPORTING
          function_include        = lv_include
        TABLES
          import_parameter        = <ls_func>-import
          export_parameter        = <ls_func>-export
          tables_parameter        = <ls_func>-tables
          changing_parameter      = <ls_func>-changing
          exception_list          = <ls_func>-exception
          parameter_docu          = <ls_func>-documentation
        EXCEPTIONS
          double_task             = 1
          error_message           = 2
          function_already_exists = 3
          invalid_function_pool   = 4
          invalid_name            = 5
          too_many_functions      = 6
          no_modify_permission    = 7
          no_show_permission      = 8
          enqueue_system_failure  = 9
          canceled_in_corr        = 10
          OTHERS                  = 11.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
        ii_log->add_error( iv_msg = |Function module { <ls_func>-funcname }: { lv_msg }|
                           is_item = ms_item ).
        CONTINUE.  "with next function module
      ENDIF.

      Lcl_abapgit_factory=>get_sap_report( )->insert_report(
        iv_name    = lv_include
        iv_package = iv_package
        iv_version = iv_version
        it_source  = lt_source ).

      ii_log->add_success( iv_msg = |Function module { <ls_func>-funcname } imported|
                           is_item = ms_item ).
    ENDLOOP.

  ENDMETHOD.
  METHOD deserialize_function_docs.

    FIELD-SYMBOLS <ls_func> LIKE LINE OF it_functions.

    Lcl_abapgit_factory=>get_longtexts( )->deserialize(
      iv_longtext_id   = c_longtext_id_prog
      iv_object_name   = iv_prog_name
      ii_xml           = ii_xml
      iv_main_language = mv_language ).

    LOOP AT it_functions ASSIGNING <ls_func>.
      Lcl_abapgit_factory=>get_longtexts( )->deserialize(
        iv_longtext_name = |LONGTEXTS_{ <ls_func>-funcname }|
        iv_longtext_id   = c_longtext_id_func
        iv_object_name   = <ls_func>-funcname
        ii_xml           = ii_xml
        iv_main_language = mv_language ).
      Lcl_abapgit_factory=>get_longtexts( )->deserialize(
        iv_longtext_name = |LONGTEXTS_{ <ls_func>-funcname }___EXC|
        iv_longtext_id   = c_longtext_id_func_exc
        iv_object_name   = <ls_func>-funcname
        ii_xml           = ii_xml
        iv_main_language = mv_language ).
    ENDLOOP.

  ENDMETHOD.
  METHOD deserialize_includes.

    DATA: lo_xml       TYPE REF TO Lif_abapgit_xml_input,
          ls_progdir   TYPE Lif_abapgit_sap_report=>ty_progdir,
          lt_includes  TYPE ty_sobj_name_tt,
          lt_tpool     TYPE textpool_table,
          lt_tpool_ext TYPE Lif_abapgit_definitions=>ty_tpool_tt,
          lt_source    TYPE TABLE OF abaptxt255,
          lx_exc       TYPE REF TO Lcx_abapgit_exception.

    FIELD-SYMBOLS: <lv_include> LIKE LINE OF lt_includes.


    tadir_insert( iv_package ).

    ii_xml->read( EXPORTING iv_name = 'INCLUDES'
                  CHANGING cg_data = lt_includes ).

    LOOP AT lt_includes ASSIGNING <lv_include>.

      "ignore simple transformation includes (as long as they remain in existing repositories)
      IF strlen( <lv_include> ) = 33 AND <lv_include>+30(3) = 'XTI'.
        ii_log->add_warning( iv_msg = |Simple Transformation include { <lv_include> } ignored|
                             is_item = ms_item ).
        CONTINUE.
      ENDIF.

      TRY.
          lt_source = Lif_abapgit_object~mo_files->read_abap( iv_extra = <lv_include> ).

          lo_xml = Lif_abapgit_object~mo_files->read_xml( <lv_include> ).

          lo_xml->read( EXPORTING iv_name = 'PROGDIR'
                        CHANGING cg_data = ls_progdir ).

          set_abap_language_version( CHANGING cv_abap_language_version = ls_progdir-uccheck ).

          lo_xml->read( EXPORTING iv_name = 'TPOOL'
                        CHANGING cg_data = lt_tpool_ext ).
          lt_tpool = read_tpool( lt_tpool_ext ).

          deserialize_program( is_progdir = ls_progdir
                               it_source  = lt_source
                               it_tpool   = lt_tpool
                               iv_package = iv_package ).

          deserialize_textpool( iv_program    = <lv_include>
                                it_tpool      = lt_tpool
                                iv_is_include = abap_true ).

          ii_log->add_success( iv_msg = |Include { ls_progdir-name } imported|
                               is_item = ms_item ).

        CATCH Lcx_abapgit_exception INTO lx_exc.
          ii_log->add_exception( ix_exc = lx_exc
                                 is_item = ms_item ).
          CONTINUE.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.
  METHOD deserialize_texts.
    DATA: lt_tpool_i18n TYPE ty_tpools_i18n,
          lt_tpool      TYPE textpool_table.

    FIELD-SYMBOLS <ls_tpool> LIKE LINE OF lt_tpool_i18n.
    ii_xml->read( EXPORTING iv_name = 'I18N_TPOOL'
                  CHANGING  cg_data = lt_tpool_i18n ).

    LOOP AT lt_tpool_i18n ASSIGNING <ls_tpool>.
      lt_tpool = read_tpool( <ls_tpool>-textpool ).
      deserialize_textpool( iv_program  = iv_prog_name
                            iv_language = <ls_tpool>-language
                            it_tpool    = lt_tpool ).
    ENDLOOP.
  ENDMETHOD.
  METHOD deserialize_xml.

    DATA: lv_complete  TYPE rs38l-area,
          lv_namespace TYPE rs38l-namespace,
          lv_areat     TYPE tlibt-areat,
          lv_stext     TYPE tftit-stext,
          lv_group     TYPE rs38l-area.

    lv_complete = ms_item-obj_name.

    CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
      EXPORTING
        complete_area                = lv_complete
      IMPORTING
        namespace                    = lv_namespace
        group                        = lv_group
      EXCEPTIONS
        include_not_exists           = 1
        group_not_exists             = 2
        no_selections                = 3
        no_function_include          = 4
        no_function_pool             = 5
        delimiter_wrong_position     = 6
        no_customer_function_group   = 7
        no_customer_function_include = 8
        reserved_name_customer       = 9
        namespace_too_long           = 10
        area_length_error            = 11
        OTHERS                       = 12.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    ii_xml->read( EXPORTING iv_name = 'AREAT'
                  CHANGING cg_data = lv_areat ).
    lv_stext = lv_areat.

    CALL FUNCTION 'RS_FUNCTION_POOL_INSERT'
      EXPORTING
        function_pool           = lv_group
        short_text              = lv_stext
        namespace               = lv_namespace
        devclass                = iv_package
        unicode_checks          = iv_version
        corrnum                 = iv_transport
        suppress_corr_check     = abap_false
      EXCEPTIONS
        name_already_exists     = 1
        name_not_correct        = 2
        function_already_exists = 3
        invalid_function_pool   = 4
        invalid_name            = 5
        too_many_functions      = 6
        no_modify_permission    = 7
        no_show_permission      = 8
        enqueue_system_failure  = 9
        canceled_in_corr        = 10
        undefined_error         = 11
        OTHERS                  = 12.

    CASE sy-subrc.
      WHEN 0.
        " Everything is ok
      WHEN 1 OR 3.
        " If the function group exists we need to manually update the short text
        update_func_group_short_text( iv_group      = lv_group
                                      iv_short_text = lv_stext ).
      WHEN OTHERS.
        Lcx_abapgit_exception=>raise_t100( ).
    ENDCASE.

  ENDMETHOD.
  METHOD functions.

    DATA: lv_area TYPE rs38l-area.
    FIELD-SYMBOLS: <ls_functab> TYPE LINE OF ty_rs38l_incl_tt.

    lv_area = ms_item-obj_name.


    CALL FUNCTION 'RS_FUNCTION_POOL_CONTENTS'
      EXPORTING
        function_pool           = lv_area
      TABLES
        functab                 = rt_functab
      EXCEPTIONS
        function_pool_not_found = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

* The result can also contain function which are lowercase.
    LOOP AT rt_functab ASSIGNING <ls_functab>.
      TRANSLATE <ls_functab> TO UPPER CASE.
    ENDLOOP.

    SORT rt_functab BY funcname ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rt_functab COMPARING funcname.

  ENDMETHOD.
  METHOD get_abap_version.

    DATA: lt_includes TYPE ty_sobj_name_tt,
          ls_progdir  TYPE Lif_abapgit_sap_report=>ty_progdir,
          lo_xml      TYPE REF TO Lif_abapgit_xml_input.

    FIELD-SYMBOLS: <lv_include> LIKE LINE OF lt_includes.

    ii_xml->read( EXPORTING iv_name = 'INCLUDES'
                  CHANGING cg_data = lt_includes ).

    LOOP AT lt_includes ASSIGNING <lv_include>.

      lo_xml = Lif_abapgit_object~mo_files->read_xml( <lv_include> ).

      lo_xml->read( EXPORTING iv_name = 'PROGDIR'
                    CHANGING cg_data = ls_progdir ).

      IF ls_progdir-uccheck IS INITIAL.
        CONTINUE.
      ELSEIF rv_abap_version IS INITIAL.
        rv_abap_version = ls_progdir-uccheck.
        CONTINUE.
      ELSEIF rv_abap_version <> ls_progdir-uccheck.
*** All includes need to have the same ABAP language version
        Lcx_abapgit_exception=>raise( 'different ABAP Language Versions' ).
      ENDIF.
    ENDLOOP.

    IF rv_abap_version IS INITIAL.
      set_abap_language_version( CHANGING cv_abap_language_version = rv_abap_version ).
    ENDIF.

  ENDMETHOD.
  METHOD includes.

    TYPES: BEGIN OF ty_reposrc,
             progname TYPE reposrc-progname,
           END OF ty_reposrc.

    DATA: lt_reposrc        TYPE STANDARD TABLE OF ty_reposrc WITH DEFAULT KEY,
          ls_reposrc        LIKE LINE OF lt_reposrc,
          lv_program        TYPE program,
          lv_maintviewname  LIKE LINE OF rt_includes,
          lv_offset_ns      TYPE i,
          lv_tabix          LIKE sy-tabix,
          lt_functab        TYPE ty_rs38l_incl_tt,
          lt_tadir_includes TYPE HASHED TABLE OF objname WITH UNIQUE KEY table_line.

    FIELD-SYMBOLS: <lv_include> LIKE LINE OF rt_includes,
                   <ls_func>    LIKE LINE OF lt_functab.


    IF lines( mt_includes_cache ) > 0.
      rt_includes = mt_includes_cache.
      RETURN.
    ENDIF.

    lv_program = main_name( ).
    lt_functab = functions( ).

    CALL FUNCTION 'RS_GET_ALL_INCLUDES'
      EXPORTING
        program      = lv_program
      TABLES
        includetab   = rt_includes
      EXCEPTIONS
        not_existent = 1
        no_program   = 2
        OTHERS       = 3.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Error from RS_GET_ALL_INCLUDES' ).
    ENDIF.

    LOOP AT lt_functab ASSIGNING <ls_func>.
      DELETE TABLE rt_includes FROM <ls_func>-include.
    ENDLOOP.

* handle generated maintenance views
    IF ms_item-obj_name(1) <> '/'.
      "FGroup name does not contain a namespace
      lv_maintviewname = |L{ ms_item-obj_name }T00|.
    ELSE.
      "FGroup name contains a namespace
      lv_offset_ns = find( val = ms_item-obj_name+1
                           sub = '/' ).
      lv_offset_ns = lv_offset_ns + 2.
      lv_maintviewname = |{ ms_item-obj_name(lv_offset_ns) }L{ ms_item-obj_name+lv_offset_ns }T00|.
    ENDIF.

    READ TABLE rt_includes WITH KEY table_line = lv_maintviewname TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      APPEND lv_maintviewname TO rt_includes.
    ENDIF.

    SORT rt_includes.
    IF lines( rt_includes ) > 0.
      " check which includes have their own tadir entry
      " these includes might reside in a different package or might be shared between multiple function groups
      " or other programs and are hence no part of the to serialized FUGR object
      " they will be handled as individual objects when serializing their package
      " in addition, referenced XTI includes referencing (simple) transformations must be ignored
      SELECT obj_name
        INTO TABLE lt_tadir_includes
        FROM tadir
        FOR ALL ENTRIES IN rt_includes
        WHERE pgmid      = 'R3TR'
              AND object = 'PROG'
              AND obj_name = rt_includes-table_line.
      LOOP AT rt_includes ASSIGNING <lv_include>.
        " skip autogenerated includes from Table Maintenance Generator
        IF <lv_include> CP 'LSVIM*'.
          DELETE rt_includes INDEX sy-tabix.
          CONTINUE.
        ENDIF.
        READ TABLE lt_tadir_includes WITH KEY table_line = <lv_include> TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          DELETE rt_includes.
          CONTINUE.
        ENDIF.
        IF strlen( <lv_include> ) = 33 AND <lv_include>+30(3) = 'XTI'.
          "ignore referenced (simple) transformation includes
          DELETE rt_includes.
          CONTINUE.
        ENDIF.
      ENDLOOP.

      IF lines( rt_includes ) > 0.
        SELECT progname FROM reposrc
          INTO TABLE lt_reposrc
          FOR ALL ENTRIES IN rt_includes
          WHERE progname = rt_includes-table_line
          AND r3state = 'A'.
      ENDIF.
      SORT lt_reposrc BY progname ASCENDING.
    ENDIF.

    LOOP AT rt_includes ASSIGNING <lv_include>.
      lv_tabix = sy-tabix.

* make sure the include exists
      READ TABLE lt_reposrc INTO ls_reposrc
        WITH KEY progname = <lv_include> BINARY SEARCH.
      IF sy-subrc <> 0.
        DELETE rt_includes INDEX lv_tabix.
        CONTINUE.
      ENDIF.

      "Make sure that the include does not belong to another function group
      IF is_part_of_other_fugr( <lv_include> ) = abap_true.
        DELETE rt_includes.
      ENDIF.
    ENDLOOP.

    APPEND lv_program TO rt_includes.
    SORT rt_includes.

    mt_includes_cache = rt_includes.

  ENDMETHOD.
  METHOD is_any_function_module_locked.

    DATA: lt_functions TYPE ty_rs38l_incl_tt.

    FIELD-SYMBOLS: <ls_function> TYPE rs38l_incl.

    TRY.
        lt_functions = functions( ).
      CATCH Lcx_abapgit_exception.
        RETURN.
    ENDTRY.

    LOOP AT lt_functions ASSIGNING <ls_function>.

      IF exists_a_lock_entry_for( iv_lock_object = 'ESFUNCTION'
                                  iv_argument    = |{ <ls_function>-funcname }| ) = abap_true.
        rv_any_function_module_locked = abap_true.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD is_any_include_locked.

    DATA: lt_includes TYPE ty_sobj_name_tt.
    FIELD-SYMBOLS: <lv_include> TYPE sobj_name.

    TRY.
        lt_includes = includes( ).
      CATCH Lcx_abapgit_exception.
        RETURN.
    ENDTRY.

    LOOP AT lt_includes ASSIGNING <lv_include>.

      IF exists_a_lock_entry_for( iv_lock_object = 'ESRDIRE'
                                  iv_argument    = |{ <lv_include> }| ) = abap_true.
        rv_is_any_include_locked = abap_true.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD is_function_group_locked.
    rv_is_functions_group_locked = exists_a_lock_entry_for( iv_lock_object = 'EEUDB'
                                                            iv_argument    = ms_item-obj_name
                                                            iv_prefix      = 'FG' ).
  ENDMETHOD.
  METHOD is_part_of_other_fugr.
    " make sure that the include belongs to the function group
    " like in LSEAPFAP Form TADIR_MAINTENANCE
    DATA ls_tadir TYPE tadir.
    DATA lv_namespace TYPE rs38l-namespace.
    DATA lv_area TYPE rs38l-area.
    DATA lv_include TYPE rs38l-include.

    rv_belongs_to_other_fugr = abap_false.
    IF iv_include(1) = 'L' OR iv_include+1 CS '/L'.
      lv_include = iv_include.
      ls_tadir-object = 'FUGR'.

      CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
        IMPORTING
          namespace = lv_namespace
          group     = lv_area
        CHANGING
          include   = lv_include
        EXCEPTIONS
          OTHERS    = 1.
      IF lv_area(1) = 'X'.    " "EXIT"-function-module
        ls_tadir-object = 'FUGS'.
      ENDIF.
      IF sy-subrc = 0.
        CONCATENATE lv_namespace lv_area INTO ls_tadir-obj_name.
        IF ls_tadir-obj_name <> ms_item-obj_name.
          rv_belongs_to_other_fugr = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD main_name.

    DATA: lv_area      TYPE rs38l-area,
          lv_namespace TYPE rs38l-namespace,
          lv_group     TYPE rs38l-area.


    lv_area = ms_item-obj_name.

    CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
      EXPORTING
        complete_area                = lv_area
      IMPORTING
        namespace                    = lv_namespace
        group                        = lv_group
      EXCEPTIONS
        include_not_exists           = 1
        group_not_exists             = 2
        no_selections                = 3
        no_function_include          = 4
        no_function_pool             = 5
        delimiter_wrong_position     = 6
        no_customer_function_group   = 7
        no_customer_function_include = 8
        reserved_name_customer       = 9
        namespace_too_long           = 10
        area_length_error            = 11
        OTHERS                       = 12.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CONCATENATE lv_namespace 'SAPL' lv_group INTO rv_program.

  ENDMETHOD.
  METHOD serialize_functions.

    DATA:
      lt_source     TYPE TABLE OF rssource,
      lt_functab    TYPE ty_rs38l_incl_tt,
      lt_new_source TYPE rsfb_source,
      ls_function   LIKE LINE OF rt_functions.

    FIELD-SYMBOLS: <ls_func>          LIKE LINE OF lt_functab,
                   <ls_documentation> TYPE LINE OF ty_function-documentation.

    lt_functab = functions( ).

    LOOP AT lt_functab ASSIGNING <ls_func>.
* fm RPY_FUNCTIONMODULE_READ does not support source code
* lines longer than 72 characters
      CLEAR ls_function.
      MOVE-CORRESPONDING <ls_func> TO ls_function.

      CLEAR lt_new_source.
      CLEAR lt_source.

      CALL FUNCTION 'RPY_FUNCTIONMODULE_READ_NEW'
        EXPORTING
          functionname            = <ls_func>-funcname
        IMPORTING
          global_flag             = ls_function-global_flag
          remote_call             = ls_function-remote_call
          update_task             = ls_function-update_task
          short_text              = ls_function-short_text
          remote_basxml_supported = ls_function-remote_basxml
        TABLES
          import_parameter        = ls_function-import
          changing_parameter      = ls_function-changing
          export_parameter        = ls_function-export
          tables_parameter        = ls_function-tables
          exception_list          = ls_function-exception
          documentation           = ls_function-documentation
          source                  = lt_source
        CHANGING
          new_source              = lt_new_source
        EXCEPTIONS
          error_message           = 1
          function_not_found      = 2
          invalid_name            = 3
          OTHERS                  = 4.
      IF sy-subrc = 2.
        CONTINUE.
      ELSEIF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'Error from RPY_FUNCTIONMODULE_READ_NEW' ).
      ENDIF.

      LOOP AT ls_function-documentation ASSIGNING <ls_documentation>.
        CLEAR <ls_documentation>-index.
      ENDLOOP.

      SELECT SINGLE exten3 INTO ls_function-exception_classes FROM enlfdir
        WHERE funcname = <ls_func>-funcname.              "#EC CI_SUBRC

      APPEND ls_function TO rt_functions.

      IF NOT lt_new_source IS INITIAL.
        strip_generation_comments( CHANGING ct_source = lt_new_source ).
        Lif_abapgit_object~mo_files->add_abap(
          iv_extra = <ls_func>-funcname
          it_abap  = lt_new_source ).
      ELSE.
        strip_generation_comments( CHANGING ct_source = lt_source ).
        Lif_abapgit_object~mo_files->add_abap(
          iv_extra = <ls_func>-funcname
          it_abap  = lt_source ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD serialize_function_docs.

    FIELD-SYMBOLS <ls_func> LIKE LINE OF it_functions.

    Lcl_abapgit_factory=>get_longtexts( )->serialize(
      iv_longtext_id = c_longtext_id_prog
      iv_object_name = iv_prog_name
      io_i18n_params = mo_i18n_params
      ii_xml         = ii_xml ).

    LOOP AT it_functions ASSIGNING <ls_func>.
      Lcl_abapgit_factory=>get_longtexts( )->serialize(
        iv_longtext_name = |LONGTEXTS_{ <ls_func>-funcname }|
        iv_longtext_id   = c_longtext_id_func
        iv_object_name   = <ls_func>-funcname
        io_i18n_params   = mo_i18n_params
        ii_xml           = ii_xml ).
      Lcl_abapgit_factory=>get_longtexts( )->serialize(
        iv_longtext_name = |LONGTEXTS_{ <ls_func>-funcname }___EXC|
        iv_longtext_id   = c_longtext_id_func_exc
        iv_object_name   = <ls_func>-funcname
        io_i18n_params   = mo_i18n_params
        ii_xml           = ii_xml ).
    ENDLOOP.

  ENDMETHOD.
  METHOD serialize_includes.

    DATA: lt_includes TYPE ty_sobj_name_tt.

    FIELD-SYMBOLS: <lv_include> LIKE LINE OF lt_includes.


    lt_includes = includes( ).

    LOOP AT lt_includes ASSIGNING <lv_include>.

* todo, filename is not correct, a include can be used in several programs
      serialize_program( is_item    = ms_item
                         io_files   = Lif_abapgit_object~mo_files
                         iv_program = <lv_include>
                         iv_extra   = <lv_include> ).

    ENDLOOP.

  ENDMETHOD.
  METHOD serialize_texts.
    DATA: lt_tpool_i18n TYPE ty_tpools_i18n,
          lt_tpool      TYPE textpool_table.

    FIELD-SYMBOLS <ls_tpool> LIKE LINE OF lt_tpool_i18n.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      RETURN.
    ENDIF.

    " Table d010tinf stores info. on languages in which program is maintained
    " Select all active translations of program texts
    " Skip main language - it was already serialized
    SELECT DISTINCT language
      INTO CORRESPONDING FIELDS OF TABLE lt_tpool_i18n
      FROM d010tinf
      WHERE r3state = 'A'
      AND prog = iv_prog_name
      AND language <> mv_language
      ORDER BY language ##TOO_MANY_ITAB_FIELDS.

    mo_i18n_params->trim_saplang_keyed_table(
      EXPORTING
        iv_lang_field_name = 'LANGUAGE'
      CHANGING
        ct_tab = lt_tpool_i18n ).

    SORT lt_tpool_i18n BY language ASCENDING.
    LOOP AT lt_tpool_i18n ASSIGNING <ls_tpool>.
      READ TEXTPOOL iv_prog_name
        LANGUAGE <ls_tpool>-language
        INTO lt_tpool.
      <ls_tpool>-textpool = add_tpool( lt_tpool ).
    ENDLOOP.

    IF lines( lt_tpool_i18n ) > 0.
      ii_xml->add( iv_name = 'I18N_TPOOL'
                   ig_data = lt_tpool_i18n ).
    ENDIF.
  ENDMETHOD.
  METHOD serialize_xml.

    DATA: lt_includes TYPE ty_sobj_name_tt,
          lv_areat    TYPE tlibt-areat.


    SELECT SINGLE areat INTO lv_areat
      FROM tlibt
      WHERE spras = mv_language
      AND area = ms_item-obj_name.        "#EC CI_GENBUFF "#EC CI_SUBRC

    lt_includes = includes( ).

    ii_xml->add( iv_name = 'AREAT'
                 ig_data = lv_areat ).
    ii_xml->add( iv_name = 'INCLUDES'
                 ig_data = lt_includes ).

  ENDMETHOD.
  METHOD update_func_group_short_text.

    " We update the short text directly.
    " SE80 does the same in
    "   Program SAPLSEUF / LSEUFF07
    "   FORM GROUP_CHANGE

    UPDATE tlibt SET areat = iv_short_text
      WHERE spras = mv_language AND area = iv_group.

  ENDMETHOD.
  METHOD update_where_used.
* make extra sure the where-used list is updated after deletion
* Experienced some problems with the T00 include
* this method just tries to update everything

    DATA: lv_include LIKE LINE OF it_includes,
          lo_cross   TYPE REF TO cl_wb_crossreference.


    LOOP AT it_includes INTO lv_include.

      CREATE OBJECT lo_cross
        EXPORTING
          p_name    = lv_include
          p_include = lv_include.

      lo_cross->index_actualize( ).

    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    TYPES: BEGIN OF ty_stamps,
             user TYPE syuname,
             date TYPE d,
             time TYPE t,
           END OF ty_stamps.

    DATA:
      lt_stamps    TYPE STANDARD TABLE OF ty_stamps WITH DEFAULT KEY,
      lv_program   TYPE program,
      lv_found     TYPE abap_bool,
      lt_functions TYPE ty_rs38l_incl_tt.

    FIELD-SYMBOLS:
      <ls_function> LIKE LINE OF lt_functions,
      <lv_include>  LIKE LINE OF mt_includes_all,
      <ls_stamp>    LIKE LINE OF lt_stamps.

    lv_program = main_name( ).

    IF mt_includes_all IS INITIAL.
      CALL FUNCTION 'RS_GET_ALL_INCLUDES'
        EXPORTING
          program      = lv_program
        TABLES
          includetab   = mt_includes_all
        EXCEPTIONS
          not_existent = 1
          no_program   = 2
          OTHERS       = 3.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'Error from RS_GET_ALL_INCLUDES' ).
      ENDIF.
    ENDIF.

    " Check if changed_by for include object was requested
    LOOP AT mt_includes_all ASSIGNING <lv_include> WHERE table_line = to_upper( iv_extra ).
      lv_program = <lv_include>.
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
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
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
endclass. "ZCL_ABAPGIT_OBJECT_FUGR implementation

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
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    get_generic( )->serialize( io_xml ).

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
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    get_generic( )->serialize( io_xml ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_G4BS implementation

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

    DATA: ls_attr TYPE w3tempattr.


    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    ls_attr = read( ).

    io_xml->add( iv_name = 'ATTR'
                 ig_data = ls_attr ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_IAXU implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_IDOC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_idoc=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_idoc=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_IDOC implementation.
*"* method's implementations
*include methods.
  METHOD clear_idoc_segement_field.

    FIELD-SYMBOLS <lg_any_field> TYPE any.

    ASSIGN COMPONENT iv_fieldname OF STRUCTURE cg_structure TO <lg_any_field>.
    IF sy-subrc = 0.
      CLEAR <lg_any_field>.
    ENDIF.

  ENDMETHOD.
  METHOD clear_idoc_segement_fields.

    clear_idoc_segement_field( EXPORTING iv_fieldname = 'DEVC'
                               CHANGING  cg_structure = cg_structure ).
    clear_idoc_segement_field( EXPORTING iv_fieldname = 'PLAST'
                               CHANGING  cg_structure = cg_structure ).
    clear_idoc_segement_field( EXPORTING iv_fieldname = 'PWORK'
                               CHANGING  cg_structure = cg_structure ).
    clear_idoc_segement_field( EXPORTING iv_fieldname = 'PRESP'
                               CHANGING  cg_structure = cg_structure ).
    clear_idoc_segement_field( EXPORTING iv_fieldname = 'CREDATE'
                               CHANGING  cg_structure = cg_structure ).
    clear_idoc_segement_field( EXPORTING iv_fieldname = 'CRETIME'
                               CHANGING  cg_structure = cg_structure ).
    clear_idoc_segement_field( EXPORTING iv_fieldname = 'LDATE'
                               CHANGING  cg_structure = cg_structure ).
    clear_idoc_segement_field( EXPORTING iv_fieldname = 'LTIME'
                               CHANGING  cg_structure = cg_structure ).
  ENDMETHOD.
  METHOD constructor.

    super->constructor( is_item = is_item
                        iv_language = iv_language ).

    mv_idoctyp = ms_item-obj_name.

  ENDMETHOD.
  METHOD is_closed.

    DATA ls_idoc TYPE ty_idoc.

    CALL FUNCTION 'IDOCTYPE_READ'
      EXPORTING
        pi_idoctyp       = mv_idoctyp
      IMPORTING
        pe_attributes    = ls_idoc-attributes
      EXCEPTIONS
        object_not_found = 1
        db_error         = 2
        no_authority     = 3
        OTHERS           = 4.
    rv_closed = boolc( sy-subrc = 0 AND ls_idoc-attributes-closed = abap_true ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: ls_attributes TYPE edi_iapi01.

    CALL FUNCTION 'IDOCTYPE_READ'
      EXPORTING
        pi_idoctyp       = mv_idoctyp
      IMPORTING
        pe_attributes    = ls_attributes
      EXCEPTIONS
        object_not_found = 1
        db_error         = 2
        no_authority     = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    rv_user = ls_attributes-plast.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    CALL FUNCTION 'IDOCTYPE_DELETE'
      EXPORTING
        pi_idoctyp          = mv_idoctyp
      EXCEPTIONS
        object_not_found    = 1
        lock_error          = 2
        action_not_possible = 3
        transport_error     = 4
        db_error            = 5
        no_authority        = 6
        OTHERS              = 7.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_idoc       TYPE ty_idoc,
          lv_transport  TYPE trkorr,
          ls_edbas      TYPE edbas,
          ls_attributes TYPE edi_iapi05.

    io_xml->read(
      EXPORTING
        iv_name = 'IDOC'
      CHANGING
        cg_data = ls_idoc ).

    MOVE-CORRESPONDING ls_idoc-attributes TO ls_attributes.

    IF Lif_abapgit_object~exists( ) = abap_false.
      CALL FUNCTION 'IDOCTYPE_CREATE'
        EXPORTING
          pi_idoctyp       = mv_idoctyp
          pi_devclass      = iv_package
          pi_attributes    = ls_attributes
        TABLES
          pt_syntax        = ls_idoc-t_syntax
        EXCEPTIONS
          object_not_found = 1
          object_exists    = 2
          syntax_error     = 3
          segment_error    = 4
          transport_error  = 5
          db_error         = 6
          no_authority     = 7
          OTHERS           = 8.
    ELSE.
      IF is_closed( ) = abap_true.
        CALL FUNCTION 'IDOCTYPE_UNCLOSE'
          EXPORTING
            pi_idoctyp          = mv_idoctyp
          EXCEPTIONS
            object_not_found    = 1
            action_not_possible = 2
            db_error            = 3
            no_authority        = 4
            OTHERS              = 5.
        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise_t100( ).
        ENDIF.
      ENDIF.

      CALL FUNCTION 'IDOCTYPE_UPDATE'
        EXPORTING
          pi_idoctyp       = mv_idoctyp
          pi_attributes    = ls_attributes
        TABLES
          pt_syntax        = ls_idoc-t_syntax
        EXCEPTIONS
          object_not_found = 1
          object_exists    = 2
          syntax_error     = 3
          segment_error    = 4
          transport_error  = 5
          db_error         = 6
          no_authority     = 7
          OTHERS           = 8.
    ENDIF.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF ls_idoc-attributes-closed = abap_true.
      IF iv_transport IS NOT INITIAL.
        lv_transport = iv_transport.

        CALL FUNCTION 'IDOCTYPE_CLOSE'
          EXPORTING
            pi_idoctyp          = mv_idoctyp
          CHANGING
            pc_order            = lv_transport
          EXCEPTIONS
            object_not_found    = 1
            action_not_possible = 2
            db_error            = 3
            no_authority        = 4
            OTHERS              = 5.
        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise_t100( ).
        ENDIF.
      ENDIF.

      " IDOCTYPE_CLOSE saves current release but it should be same as in repo
      SELECT SINGLE * FROM edbas INTO ls_edbas WHERE idoctyp = mv_idoctyp.
      ls_edbas-released = ls_idoc-attributes-released.
      ls_edbas-applrel  = ls_idoc-attributes-applrel.
      ls_edbas-closed   = ls_idoc-attributes-closed.
      UPDATE edbas FROM ls_edbas.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Error updating IDOC { mv_idoctyp }| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    CALL FUNCTION 'IDOCTYPE_EXISTENCE_CHECK'
      EXPORTING
        pi_idoctyp       = mv_idoctyp
      EXCEPTIONS
        object_not_found = 1
        db_error         = 2
        OTHERS           = 3.

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
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPMSED5'.
    <ls_bdcdata>-dynpro   = '0010'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'SED5STRUC-OBJECT'.
    <ls_bdcdata>-fval = ms_item-obj_name.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'SED5STRUC-SELECT_ORG'.
    <ls_bdcdata>-fval = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=DISP'.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'WE30'
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

    DATA: ls_idoc TYPE ty_idoc.

    CALL FUNCTION 'IDOCTYPE_READ'
      EXPORTING
        pi_idoctyp       = mv_idoctyp
      IMPORTING
        pe_attributes    = ls_idoc-attributes
      TABLES
        pt_syntax        = ls_idoc-t_syntax
      EXCEPTIONS
        object_not_found = 1
        db_error         = 2
        no_authority     = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    clear_idoc_segement_fields( CHANGING cg_structure = ls_idoc-attributes ).

    io_xml->add( iv_name = 'IDOC'
                 ig_data = ls_idoc ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_IDOC implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_IEXT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_iext=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_iext=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_IEXT implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_extension = ms_item-obj_name.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: ls_attributes TYPE edi_iapi01.

    CALL FUNCTION 'EXTTYPE_READ'
      EXPORTING
        pi_cimtyp     = mv_extension
      IMPORTING
        pe_attributes = ls_attributes
      EXCEPTIONS
        OTHERS        = 1.

    rv_user = ls_attributes-plast.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    CALL FUNCTION 'EXTTYPE_DELETE'
      EXPORTING
        pi_cimtyp = mv_extension
      EXCEPTIONS
        OTHERS    = 1.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_extension  TYPE ty_extention,
          ls_attributes TYPE edi_iapi05.

    io_xml->read( EXPORTING iv_name = c_dataname_iext
                  CHANGING  cg_data = ls_extension ).

    MOVE-CORRESPONDING ls_extension-attributes TO ls_attributes.
    ls_attributes-presp = sy-uname.
    ls_attributes-pwork = ls_attributes-presp.

    IF Lif_abapgit_object~exists( ) = abap_true.
      CALL FUNCTION 'EXTTYPE_UPDATE'
        EXPORTING
          pi_cimtyp     = mv_extension
          pi_attributes = ls_attributes
        TABLES
          pt_syntax     = ls_extension-t_syntax
        EXCEPTIONS
          OTHERS        = 1.
    ELSE.
      CALL FUNCTION 'EXTTYPE_CREATE'
        EXPORTING
          pi_cimtyp     = mv_extension
          pi_devclass   = iv_package
          pi_attributes = ls_attributes
        TABLES
          pt_syntax     = ls_extension-t_syntax
        EXCEPTIONS
          OTHERS        = 1.
    ENDIF.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.


  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    CALL FUNCTION 'EXTTYPE_READ'
      EXPORTING
        pi_cimtyp = mv_extension
      EXCEPTIONS
        OTHERS    = 1.

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
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPMSED5'.
    <ls_bdcdata>-dynpro   = '0010'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'SED5STRUC-OBJECT'.
    <ls_bdcdata>-fval = ms_item-obj_name.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'SED5STRUC-SELECT_EXT'.
    <ls_bdcdata>-fval = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=DISP'.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'WE30'
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

    DATA ls_extension           TYPE ty_extention.

    CALL FUNCTION 'EXTTYPE_READ'
      EXPORTING
        pi_cimtyp     = mv_extension
      IMPORTING
        pe_attributes = ls_extension-attributes
      TABLES
        pt_syntax     = ls_extension-t_syntax
      EXCEPTIONS
        OTHERS        = 1.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    Lcl_abapgit_object_idoc=>clear_idoc_segement_fields( CHANGING cg_structure = ls_extension-attributes ).

    io_xml->add( iv_name = c_dataname_iext
                 ig_data = ls_extension ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_IEXT implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_INTF <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_intf=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_intf=======ccimp.
CLASS SHRIS5ZPAUXVKEPN5HWETLLAS3QBTU DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      get_descriptions_compo_subco
        IMPORTING iv_language          TYPE sy-langu
                  iv_clif_name         TYPE seoclsname
        RETURNING VALUE(rs_properties) TYPE Lif_abapgit_aff_oo_types_v1=>ty_descriptions ,
      get_descr_comp_subc_w_exposure
        IMPORTING iv_language          TYPE sy-langu
                  iv_clif_name         TYPE seoclsname
                  iv_exposure          TYPE seoexpose DEFAULT seoc_exposure_public
        RETURNING VALUE(rs_properties) TYPE Lif_abapgit_aff_oo_types_v1=>ty_descriptions ,
      set_descriptions_compo_subco
        IMPORTING iv_clif_name  TYPE seoclsname
                  iv_language   TYPE langu
                  is_properties TYPE Lif_abapgit_aff_oo_types_v1=>ty_descriptions .
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_component,
        visibility TYPE seoexpose,
        cmpname    TYPE seocmpname,
        descript   TYPE seodescr,
        cmptype    TYPE seocmptype,
      END OF ty_component,
      BEGIN OF ty_sub_component,
        cmpname  TYPE seocmpname,
        sconame  TYPE seosconame,
        descript TYPE seodescr,
        scotype  TYPE seoscotype,
      END OF ty_sub_component,
      ty_compontents     TYPE STANDARD TABLE OF ty_component,
      ty_sub_compontents TYPE STANDARD TABLE OF ty_sub_component.

    CLASS-METHODS:
      get_attributes
        IMPORTING is_components    TYPE ty_compontents
        RETURNING VALUE(rs_result) TYPE Lif_abapgit_aff_oo_types_v1=>ty_component_descriptions,
      get_methods
        IMPORTING is_components     TYPE ty_compontents
                  is_sub_components TYPE ty_sub_compontents
        RETURNING VALUE(rs_result)  TYPE Lif_abapgit_aff_oo_types_v1=>ty_methods,
      get_types
        IMPORTING is_components    TYPE ty_compontents
        RETURNING VALUE(rs_result) TYPE Lif_abapgit_aff_oo_types_v1=>ty_component_descriptions,
      get_events
        IMPORTING is_components     TYPE ty_compontents
                  is_sub_components TYPE ty_sub_compontents
        RETURNING VALUE(rs_result)  TYPE Lif_abapgit_aff_oo_types_v1=>ty_events,
      set_methods
        IMPORTING iv_clif_name  TYPE seoclsname
                  iv_language   TYPE langu
                  is_properties TYPE Lif_abapgit_aff_oo_types_v1=>ty_descriptions,
      set_attributes
        IMPORTING iv_clif_name  TYPE seoclsname
                  iv_language   TYPE langu
                  is_properties TYPE Lif_abapgit_aff_oo_types_v1=>ty_descriptions,
      set_events
        IMPORTING iv_clif_name  TYPE seoclsname
                  iv_language   TYPE langu
                  is_properties TYPE Lif_abapgit_aff_oo_types_v1=>ty_descriptions,
      set_types
        IMPORTING iv_clif_name  TYPE seoclsname
                  iv_language   TYPE langu
                  is_properties TYPE Lif_abapgit_aff_oo_types_v1=>ty_descriptions .
ENDCLASS.


CLASS SHRIS5ZPAUXVKEPN5HWETLLAS3QBTU IMPLEMENTATION.

  METHOD get_descr_comp_subc_w_exposure.
    DATA:
      lt_components     TYPE ty_compontents,
      lt_sub_components TYPE ty_sub_compontents.


    SELECT df~exposure AS visibility component~cmpname component_text~descript component~cmptype
      INTO TABLE lt_components
      FROM seocompo AS component
      LEFT OUTER JOIN seocompotx AS component_text
      ON component~cmpname = component_text~cmpname AND component~clsname = component_text~clsname AND
         component_text~langu = iv_language
      INNER JOIN seocompodf AS df
      ON component~clsname = df~clsname AND
         component~cmpname = df~cmpname
      WHERE component~clsname = iv_clif_name AND
            df~exposure       = iv_exposure.           "#EC CI_BUFFJOIN

    SELECT sub_component~cmpname sub_component~sconame sub_component_text~descript sub_component~scotype
      INTO TABLE lt_sub_components
      FROM seosubco AS sub_component JOIN seosubcotx AS sub_component_text
      ON sub_component~clsname = sub_component_text~clsname AND
         sub_component~cmpname = sub_component_text~cmpname AND
         sub_component~sconame = sub_component_text~sconame
      INNER JOIN seocompodf AS df
      ON sub_component~clsname = df~clsname AND
         sub_component~cmpname = df~cmpname
      WHERE sub_component~clsname    = iv_clif_name
        AND df~exposure              = iv_exposure
        AND sub_component_text~langu = iv_language
        AND sub_component_text~descript <> space.      "#EC CI_BUFFJOIN



    rs_properties-attributes = get_attributes( lt_components ).
    rs_properties-methods = get_methods( is_components = lt_components
                                         is_sub_components = lt_sub_components ).
    rs_properties-events = get_events( is_components = lt_components
                                       is_sub_components = lt_sub_components ).
    rs_properties-types = get_types( lt_components ).
  ENDMETHOD.


  METHOD get_descriptions_compo_subco.
    TYPES:
      BEGIN OF ty_helper_type,
        cmpname  TYPE seocmpname,
        descript TYPE seodescr,
        cmptype  TYPE seocmptype,
      END OF ty_helper_type.
    DATA:
      lt_components     TYPE STANDARD TABLE OF ty_helper_type,
      lt_sub_components TYPE ty_sub_compontents,
      lt_components_exp TYPE ty_compontents,
      ls_component_exp  LIKE LINE OF lt_components_exp.
    FIELD-SYMBOLS:
      <ls_component> LIKE LINE OF lt_components.


    SELECT component~cmpname component_text~descript component~cmptype
      INTO TABLE lt_components
      FROM seocompo AS component
     LEFT OUTER JOIN seocompotx AS component_text
      ON component~cmpname = component_text~cmpname AND component~clsname    = component_text~clsname
                                                    AND component_text~langu = iv_language
      WHERE component~clsname = iv_clif_name.          "#EC CI_BUFFJOIN

    SELECT sub_component~cmpname sub_component~sconame sub_component_text~descript sub_component~scotype
      INTO TABLE lt_sub_components
      FROM seosubco AS sub_component JOIN seosubcotx AS sub_component_text
      ON sub_component~clsname      = sub_component_text~clsname
          AND sub_component~cmpname = sub_component_text~cmpname
          AND sub_component~sconame = sub_component_text~sconame
      WHERE sub_component~clsname    = iv_clif_name
        AND sub_component_text~langu = iv_language
        AND sub_component_text~descript <> space.      "#EC CI_BUFFJOIN

    LOOP AT lt_components ASSIGNING <ls_component>.
      CLEAR ls_component_exp.
      MOVE-CORRESPONDING <ls_component> TO ls_component_exp.
      INSERT ls_component_exp INTO TABLE lt_components_exp.
    ENDLOOP.

    rs_properties-attributes = get_attributes( lt_components_exp ).
    rs_properties-methods = get_methods( is_components = lt_components_exp
                                         is_sub_components = lt_sub_components ).
    rs_properties-events = get_events( is_components = lt_components_exp
                                       is_sub_components = lt_sub_components ).
    rs_properties-types = get_types( lt_components_exp ).

  ENDMETHOD.


  METHOD get_attributes.
    DATA:
      lo_component TYPE Lif_abapgit_aff_oo_types_v1=>ty_component_description.
    FIELD-SYMBOLS <lo_attribute> TYPE ty_component.

    LOOP AT is_components ASSIGNING <lo_attribute> WHERE cmptype = seoo_cmptype_attribute AND descript IS NOT INITIAL.
      lo_component-name = <lo_attribute>-cmpname.
      lo_component-description = <lo_attribute>-descript.
      INSERT lo_component INTO TABLE rs_result.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_methods.
    DATA:
      lo_method    TYPE Lif_abapgit_aff_oo_types_v1=>ty_method,
      lo_exception TYPE Lif_abapgit_aff_oo_types_v1=>ty_component_description,
      lo_parameter TYPE Lif_abapgit_aff_oo_types_v1=>ty_component_description.

    FIELD-SYMBOLS <ls_sub_component> TYPE ty_sub_component.
    FIELD-SYMBOLS <ls_component> TYPE ty_component.

    LOOP AT is_components ASSIGNING <ls_component> WHERE cmptype = seoo_cmptype_method.
      lo_method-name = <ls_component>-cmpname.
      lo_method-description = <ls_component>-descript.

      LOOP AT is_sub_components ASSIGNING <ls_sub_component> WHERE cmpname = <ls_component>-cmpname.
        CASE <ls_sub_component>-scotype.
          WHEN seos_scotype_parameter.
            lo_parameter-name = <ls_sub_component>-sconame.
            lo_parameter-description = <ls_sub_component>-descript.
            INSERT lo_parameter INTO TABLE lo_method-parameters.
          WHEN seos_scotype_exception.
            lo_exception-name = <ls_sub_component>-sconame.
            lo_exception-description = <ls_sub_component>-descript.
            INSERT lo_exception INTO TABLE lo_method-exceptions.
        ENDCASE.
      ENDLOOP.

      IF lo_method-description IS NOT INITIAL
          OR lo_method-exceptions IS NOT INITIAL
          OR lo_method-parameters IS NOT INITIAL.
        INSERT lo_method INTO TABLE rs_result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_types.
    DATA:
        lo_type TYPE Lif_abapgit_aff_oo_types_v1=>ty_component_description.
    FIELD-SYMBOLS: <ls_types> TYPE ty_component.

    LOOP AT is_components ASSIGNING <ls_types>
        WHERE cmptype = seoo_cmptype_type AND descript IS NOT INITIAL.
      lo_type-name = <ls_types>-cmpname.
      lo_type-description = <ls_types>-descript.
      INSERT lo_type INTO TABLE rs_result.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_events.
    DATA:
      lo_parameter TYPE Lif_abapgit_aff_oo_types_v1=>ty_component_description,
      lo_event     TYPE Lif_abapgit_aff_oo_types_v1=>ty_event.
    FIELD-SYMBOLS <ls_event> TYPE ty_component.
    FIELD-SYMBOLS <ls_sub_component> TYPE ty_sub_component.

    LOOP AT is_components ASSIGNING <ls_event> WHERE cmptype = seoo_cmptype_event.
      lo_event-name = <ls_event>-cmpname.
      lo_event-description = <ls_event>-descript.

      LOOP AT is_sub_components ASSIGNING <ls_sub_component> WHERE cmpname = <ls_event>-cmpname.
        lo_parameter-name = <ls_sub_component>-sconame.
        lo_parameter-description = <ls_sub_component>-descript.
        INSERT lo_parameter INTO TABLE lo_event-parameters.
      ENDLOOP.

      IF lo_event-description IS NOT INITIAL OR lo_event-parameters IS NOT INITIAL.
        INSERT lo_event INTO TABLE rs_result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_attributes.
    DATA:
      lo_attribute TYPE seocompotx.
    FIELD-SYMBOLS: <ls_attribute> TYPE Lif_abapgit_aff_oo_types_v1=>ty_component_description.

    LOOP AT is_properties-attributes ASSIGNING <ls_attribute>.
      lo_attribute-clsname  = iv_clif_name.
      lo_attribute-cmpname  = <ls_attribute>-name.
      lo_attribute-langu    = iv_language.
      lo_attribute-descript = <ls_attribute>-description.
      MODIFY seocompotx FROM lo_attribute.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_methods.
    DATA:
      lo_method           TYPE seocompotx,
      lo_method_exception TYPE seosubcotx,
      lo_method_parameter TYPE seosubcotx.
    FIELD-SYMBOLS: <ls_method>    TYPE Lif_abapgit_aff_oo_types_v1=>ty_method,
                   <ls_parameter> TYPE Lif_abapgit_aff_oo_types_v1=>ty_component_description,
                   <ls_exception> TYPE Lif_abapgit_aff_oo_types_v1=>ty_component_description.

    LOOP AT is_properties-methods ASSIGNING <ls_method>.
      lo_method-clsname  = iv_clif_name.
      lo_method-cmpname  = <ls_method>-name.
      lo_method-langu    = iv_language.
      lo_method-descript = <ls_method>-description.
      MODIFY seocompotx FROM lo_method.

      LOOP AT <ls_method>-parameters ASSIGNING <ls_parameter>.
        lo_method_parameter-clsname  = iv_clif_name.
        lo_method_parameter-cmpname  = <ls_method>-name.
        lo_method_parameter-sconame  = <ls_parameter>-name.
        lo_method_parameter-langu    = iv_language.
        lo_method_parameter-descript = <ls_parameter>-description.
        MODIFY seosubcotx FROM lo_method_parameter.
      ENDLOOP.

      LOOP AT <ls_method>-exceptions ASSIGNING <ls_exception>.
        lo_method_exception-clsname  = iv_clif_name.
        lo_method_exception-cmpname  = <ls_method>-name.
        lo_method_exception-sconame  = <ls_exception>-name.
        lo_method_exception-langu    = iv_language.
        lo_method_exception-descript = <ls_exception>-description.
        MODIFY seosubcotx FROM lo_method_exception.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_events.
    DATA:
      lo_event_parameter TYPE seosubcotx,
      lo_event           TYPE seocompotx.
    FIELD-SYMBOLS: <ls_event>     TYPE Lif_abapgit_aff_oo_types_v1=>ty_event,
                   <ls_parameter> TYPE Lif_abapgit_aff_oo_types_v1=>ty_component_description.

    LOOP AT is_properties-events ASSIGNING <ls_event>.
      lo_event-clsname  = iv_clif_name.
      lo_event-cmpname  = <ls_event>-name.
      lo_event-langu    = iv_language.
      lo_event-descript = <ls_event>-description.
      MODIFY seocompotx FROM lo_event.

      LOOP AT <ls_event>-parameters ASSIGNING <ls_parameter>.
        lo_event_parameter-clsname  = iv_clif_name.
        lo_event_parameter-cmpname  = <ls_event>-name.
        lo_event_parameter-sconame  = <ls_parameter>-name.
        lo_event_parameter-langu    = iv_language.
        lo_event_parameter-descript = <ls_parameter>-description.
        MODIFY seosubcotx FROM lo_event_parameter.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_types.
    DATA:
      lo_type TYPE seocompotx.
    FIELD-SYMBOLS: <ls_type> TYPE Lif_abapgit_aff_oo_types_v1=>ty_component_description.

    LOOP AT is_properties-types ASSIGNING <ls_type>.
      lo_type-clsname  = iv_clif_name.
      lo_type-cmpname  = <ls_type>-name.
      lo_type-langu    = iv_language.
      lo_type-descript = <ls_type>-description.
      MODIFY seocompotx FROM lo_type.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_descriptions_compo_subco.
    set_attributes( is_properties = is_properties
                    iv_clif_name = iv_clif_name
                    iv_language = iv_language ).
    set_methods( is_properties = is_properties
                 iv_clif_name = iv_clif_name
                 iv_language = iv_language ).
    set_events( is_properties = is_properties
                iv_clif_name = iv_clif_name
                iv_language = iv_language ).
    set_types( is_properties = is_properties
               iv_clif_name = iv_clif_name
               iv_language = iv_language ).
  ENDMETHOD.

ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLAS3SBTU DEFINITION.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_aff_type_mapping.
  PRIVATE SECTION.
    METHODS set_abapgit_descriptions
      IMPORTING is_clsname          TYPE seoclsname
                is_intf_aff         TYPE Lif_abapgit_aff_intf_v1=>ty_main
      EXPORTING et_descriptions     TYPE Lif_abapgit_oo_object_fnc=>ty_seocompotx_tt
                et_descriptions_sub TYPE Lif_abapgit_oo_object_fnc=>ty_seosubcotx_tt.
ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLAS3SBTU IMPLEMENTATION.

  METHOD Lif_abapgit_aff_type_mapping~to_aff.
    DATA:
      ls_data_abapgit TYPE Lcl_abapgit_object_intf=>ty_intf,
      ls_data_aff     TYPE Lif_abapgit_aff_intf_v1=>ty_main.

    ls_data_abapgit = iv_data.

    ls_data_aff-format_version = '1'.

    " get header
    ls_data_aff-header-description = ls_data_abapgit-vseointerf-descript.
    ls_data_aff-header-abap_language_version = ls_data_abapgit-vseointerf-unicode.
    ls_data_aff-header-original_language = ls_data_abapgit-vseointerf-langu.

    " get category and proxy
    ls_data_aff-category = ls_data_abapgit-vseointerf-category.
    ls_data_aff-proxy = ls_data_abapgit-vseointerf-clsproxy.

    " get descriptions
    ls_data_aff-descriptions = SHRIS5ZPAUXVKEPN5HWETLLAS3QBTU=>get_descriptions_compo_subco(
                              iv_language  = ls_data_aff-header-original_language
                              iv_clif_name = ls_data_abapgit-vseointerf-clsname ).

    es_data = ls_data_aff.
  ENDMETHOD.

  METHOD Lif_abapgit_aff_type_mapping~to_abapgit.
    DATA:
      ls_data_abapgit TYPE Lcl_abapgit_object_intf=>ty_intf,
      ls_data_aff     TYPE Lif_abapgit_aff_intf_v1=>ty_main,
      lv_classname    TYPE seoclsname.


    ls_data_aff = iv_data.

    lv_classname = iv_object_name.

    set_abapgit_descriptions( EXPORTING is_clsname          = lv_classname
                                        is_intf_aff         = ls_data_aff
                              IMPORTING et_descriptions     = ls_data_abapgit-description
                                        et_descriptions_sub = ls_data_abapgit-description_sub ).

    ls_data_abapgit-vseointerf-clsname = iv_object_name.
    ls_data_abapgit-vseointerf-descript = ls_data_aff-header-description.
    ls_data_abapgit-vseointerf-category = ls_data_aff-category.
    ls_data_abapgit-vseointerf-unicode  = ls_data_aff-header-abap_language_version.
    ls_data_abapgit-vseointerf-langu    = ls_data_aff-header-original_language.
    ls_data_abapgit-vseointerf-clsproxy = ls_data_aff-proxy.
    ls_data_abapgit-vseointerf-exposure = seoc_exposure_public.
    ls_data_abapgit-vseointerf-state    = seoc_state_implemented.

    es_data = ls_data_abapgit.

  ENDMETHOD.

  METHOD set_abapgit_descriptions.

    DATA ls_description       TYPE seocompotx.
    DATA ls_description_subco TYPE seosubcotx.
    FIELD-SYMBOLS <ls_description>      TYPE Lif_abapgit_aff_oo_types_v1=>ty_component_description.
    FIELD-SYMBOLS <ls_meth_description> TYPE Lif_abapgit_aff_oo_types_v1=>ty_method.
    FIELD-SYMBOLS <ls_evt_description>  TYPE Lif_abapgit_aff_oo_types_v1=>ty_event.


    LOOP AT is_intf_aff-descriptions-types ASSIGNING <ls_description>.
      ls_description-clsname  = is_clsname.
      ls_description-cmpname  = <ls_description>-name.
      ls_description-langu    = is_intf_aff-header-original_language.
      ls_description-descript = <ls_description>-description.
      APPEND ls_description TO et_descriptions.
    ENDLOOP.

    LOOP AT is_intf_aff-descriptions-attributes ASSIGNING <ls_description>.
      ls_description-clsname  = is_clsname.
      ls_description-cmpname  = <ls_description>-name.
      ls_description-langu    = is_intf_aff-header-original_language.
      ls_description-descript = <ls_description>-description.
      APPEND ls_description TO et_descriptions.
    ENDLOOP.

    LOOP AT is_intf_aff-descriptions-methods ASSIGNING <ls_meth_description>.
      ls_description-clsname  = is_clsname.
      ls_description-cmpname  = <ls_meth_description>-name.
      ls_description-langu    = is_intf_aff-header-original_language.
      ls_description-descript = <ls_meth_description>-description.
      APPEND ls_description TO et_descriptions.

      LOOP AT <ls_meth_description>-parameters ASSIGNING <ls_description>.
        ls_description_subco-clsname  = ls_description-clsname.
        ls_description_subco-cmpname  = ls_description-cmpname.
        ls_description_subco-langu    = ls_description-langu.
        ls_description_subco-sconame  = <ls_description>-name.
        ls_description_subco-descript = <ls_description>-description.
        APPEND ls_description_subco TO et_descriptions_sub.
      ENDLOOP.

      LOOP AT <ls_meth_description>-exceptions ASSIGNING <ls_description>.
        ls_description_subco-clsname  = ls_description-clsname.
        ls_description_subco-cmpname  = ls_description-cmpname.
        ls_description_subco-langu    = ls_description-langu.
        ls_description_subco-sconame  = <ls_description>-name.
        ls_description_subco-descript = <ls_description>-description.
        APPEND ls_description_subco TO et_descriptions_sub.
      ENDLOOP.
    ENDLOOP.

    LOOP AT is_intf_aff-descriptions-events ASSIGNING <ls_evt_description>.
      ls_description-clsname  = is_clsname.
      ls_description-cmpname  = <ls_evt_description>-name.
      ls_description-langu    = is_intf_aff-header-original_language.
      ls_description-descript = <ls_evt_description>-description.
      APPEND ls_description TO et_descriptions.

      LOOP AT <ls_evt_description>-parameters ASSIGNING <ls_description>.
        ls_description_subco-clsname  = ls_description-clsname.
        ls_description_subco-cmpname  = ls_description-cmpname.
        ls_description_subco-langu    = ls_description-langu.
        ls_description_subco-sconame  = <ls_description>-name.
        ls_description_subco-descript = <ls_description>-description.
        APPEND ls_description_subco TO et_descriptions_sub.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.


CLASS SHRIS5ZPAUXVKEPN5HWETLLAS3UBTU DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS serialize
      IMPORTING is_intf          TYPE Lcl_abapgit_object_intf=>ty_intf
      RETURNING VALUE(rv_result) TYPE xstring
      RAISING   Lcx_abapgit_exception.
    CLASS-METHODS deserialize
      IMPORTING iv_data          TYPE xstring
      RETURNING VALUE(rv_result) TYPE Lif_abapgit_aff_intf_v1=>ty_main
      RAISING   Lcx_abapgit_exception.
  PRIVATE SECTION.
    CLASS-METHODS:
      "! For serialization
      "! @parameter rt_result | Map/table that associates ABAP values to JSON values (enums)
      get_mappings
        RETURNING VALUE(rt_result) TYPE Lcl_abapgit_json_handler=>ty_enum_mappings,
      "! For serialization
      "! @parameter rt_result | Paths that will not be serialized (depending on value)
      get_paths_to_skip
        RETURNING VALUE(rt_result) TYPE Lcl_abapgit_json_handler=>ty_skip_paths.
ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLAS3UBTU IMPLEMENTATION.

  METHOD serialize.
    DATA:
      ls_data_aff      TYPE Lif_abapgit_aff_intf_v1=>ty_main,
      lx_exception     TYPE REF TO cx_root,
      lo_aff_handler   TYPE REF TO Lcl_abapgit_json_handler,
      lo_aff_mapper    TYPE REF TO Lif_abapgit_aff_type_mapping,
      lt_enum_mappings TYPE Lcl_abapgit_json_handler=>ty_enum_mappings,
      lt_paths_to_skip TYPE Lcl_abapgit_json_handler=>ty_skip_paths.


    CREATE OBJECT lo_aff_mapper TYPE SHRIS5ZPAUXVKEPN5HWETLLAS3SBTU.
    lo_aff_mapper->to_aff( EXPORTING iv_data = is_intf
                           IMPORTING es_data = ls_data_aff ).

    lt_enum_mappings = get_mappings( ).
    lt_paths_to_skip = get_paths_to_skip( ).

    CREATE OBJECT lo_aff_handler.
    TRY.
        rv_result = lo_aff_handler->serialize( iv_data          = ls_data_aff
                                               iv_enum_mappings = lt_enum_mappings
                                               iv_skip_paths    = lt_paths_to_skip ).
      CATCH cx_root INTO lx_exception.
        Lcx_abapgit_exception=>raise_with_text( lx_exception ).
    ENDTRY.

  ENDMETHOD.

  METHOD get_mappings.
    DATA:
      ls_category_mapping   TYPE Lcl_abapgit_json_handler=>ty_enum_mapping,
      ls_json_abap_mapping  TYPE Lcl_abapgit_json_handler=>ty_json_abap_mapping,
      lt_json_abap_mappings TYPE Lcl_abapgit_json_handler=>ty_json_abap_mappings.

    ls_json_abap_mapping-abap = Lif_abapgit_aff_intf_v1=>co_category-general.
    ls_json_abap_mapping-json = 'standard'.
    APPEND ls_json_abap_mapping TO lt_json_abap_mappings.
    ls_json_abap_mapping-abap = Lif_abapgit_aff_intf_v1=>co_category-classic_badi.
    ls_json_abap_mapping-json = 'classicBadi'.
    APPEND ls_json_abap_mapping TO lt_json_abap_mappings.
    ls_json_abap_mapping-abap = Lif_abapgit_aff_intf_v1=>co_category-business_static_components.
    ls_json_abap_mapping-json = 'businessStaticComponents'.
    APPEND ls_json_abap_mapping TO lt_json_abap_mappings.
    ls_json_abap_mapping-abap = Lif_abapgit_aff_intf_v1=>co_category-db_procedure_proxy.
    ls_json_abap_mapping-json = 'dbProcedureProxy'.
    APPEND ls_json_abap_mapping TO lt_json_abap_mappings.
    ls_json_abap_mapping-abap = Lif_abapgit_aff_intf_v1=>co_category-web_dynpro_runtime.
    ls_json_abap_mapping-json = 'webDynproRuntime'.
    APPEND ls_json_abap_mapping TO lt_json_abap_mappings.
    ls_json_abap_mapping-abap = Lif_abapgit_aff_intf_v1=>co_category-enterprise_service.
    ls_json_abap_mapping-json = 'enterpriseService'.
    APPEND ls_json_abap_mapping TO lt_json_abap_mappings.

    ls_category_mapping-path = '/category'.
    ls_category_mapping-mappings = lt_json_abap_mappings.

    APPEND ls_category_mapping TO rt_result.
  ENDMETHOD.

  METHOD get_paths_to_skip.
    DATA:
      ls_path_to_skipp TYPE Lcl_abapgit_json_handler=>ty_path_value_pair.

    ls_path_to_skipp-path  = '/category'.
    ls_path_to_skipp-value = 'standard'.

    APPEND ls_path_to_skipp TO rt_result.
  ENDMETHOD.

  METHOD deserialize.
    DATA:
      lo_ajson                      TYPE REF TO Lcl_abapgit_json_handler,
      lx_exception                  TYPE REF TO cx_static_check,
      lt_enum_mappings              TYPE Lcl_abapgit_json_handler=>ty_enum_mappings,
      lt_default_abap_langu_version TYPE Lcl_abapgit_json_handler=>ty_path_value_pair,
      lt_values_for_initial         TYPE Lcl_abapgit_json_handler=>ty_skip_paths.

    lt_values_for_initial = get_paths_to_skip( ).

    lt_default_abap_langu_version-path  = '/header/abap_language_version'.
    lt_default_abap_langu_version-value = Lif_abapgit_dot_abapgit=>c_abap_language_version-standard.
    APPEND lt_default_abap_langu_version TO lt_values_for_initial.

    lt_enum_mappings = get_mappings( ).


    CREATE OBJECT lo_ajson.
    TRY.
        lo_ajson->deserialize(
           EXPORTING
             iv_content = iv_data
             iv_defaults = lt_values_for_initial
             iv_enum_mappings = lt_enum_mappings
           IMPORTING
             ev_data    = rv_result ).
      CATCH cx_static_check INTO lx_exception.
        Lcx_abapgit_exception=>raise_with_text( lx_exception ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

*"* test class
*include zcl_abapgit_object_intf=======ccau.
*CLASS SHRIS5ZPAUXVKEPN5HWETLLAS3WBTU DEFINITION DEFERRED.
*CLASS zcl_abapgit_object_intf DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLAS3WBTU.









class LCL_ABAPGIT_OBJECT_INTF implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).
    mi_object_oriented_object_fct = Lcl_abapgit_oo_factory=>make( ms_item-obj_type ).

    mv_aff_enabled = Lcl_abapgit_feature=>is_enabled( Lcl_abapgit_abap_language_vers=>c_feature_flag ).
  ENDMETHOD.
  METHOD deserialize_descriptions.
    DATA:  ls_clskey TYPE seoclskey.
    ls_clskey-clsname = ms_item-obj_name.

    mi_object_oriented_object_fct->update_descriptions(
      is_key          = ls_clskey
      it_descriptions = it_description ).
  ENDMETHOD.
  METHOD deserialize_descr_sub.
    DATA:  ls_clskey TYPE seoclskey.
    ls_clskey-clsname = ms_item-obj_name.

    mi_object_oriented_object_fct->update_descriptions_sub(
      is_key          = ls_clskey
      it_descriptions = it_description ).
  ENDMETHOD.
  METHOD deserialize_docu.
    DATA: lv_object     TYPE dokhl-object,
          ls_i18n_lines TYPE Lif_abapgit_lang_definitions=>ty_i18n_line.

    lv_object = ms_item-obj_name.

    IF lines( is_docu-lines ) = 0.
      mi_object_oriented_object_fct->delete_documentation(
        iv_id          = c_longtext_id-interface
        iv_object_name = lv_object
        iv_language    = mv_language ).
      RETURN.
    ENDIF.

    mi_object_oriented_object_fct->create_documentation(
      it_lines       = is_docu-lines
      iv_id          = c_longtext_id-interface
      iv_object_name = lv_object
      iv_language    = mv_language ).

    LOOP AT is_docu-i18n_lines INTO ls_i18n_lines.
      mi_object_oriented_object_fct->create_documentation(
        it_lines         = ls_i18n_lines-lines
        iv_id            = c_longtext_id-interface
        iv_object_name   = lv_object
        iv_language      = ls_i18n_lines-language
        iv_no_masterlang = abap_true ).
    ENDLOOP.

    deserialize_longtexts(
      ii_xml           = ii_xml
      iv_longtext_name = c_longtext_name-attributes
      iv_longtext_id   = c_longtext_id-attributes ).

    deserialize_longtexts(
      ii_xml           = ii_xml
      iv_longtext_name = c_longtext_name-methods
      iv_longtext_id   = c_longtext_id-methods ).

    deserialize_longtexts(
      ii_xml           = ii_xml
      iv_longtext_name = c_longtext_name-events
      iv_longtext_id   = c_longtext_id-events ).

  ENDMETHOD.
  METHOD deserialize_pre_ddic.

    DATA ls_intf TYPE ty_intf.

    IF mv_aff_enabled = abap_true.
      ls_intf = read_json( ).
    ELSE.
      ii_xml->read( EXPORTING iv_name = 'VSEOINTERF'
                    CHANGING  cg_data = ls_intf-vseointerf ).
    ENDIF.

    set_abap_language_version( CHANGING cv_abap_language_version = ls_intf-vseointerf-unicode ).

    mi_object_oriented_object_fct->create(
      EXPORTING
        iv_check      = abap_false
        iv_package    = iv_package
      CHANGING
        cg_properties = ls_intf-vseointerf ).

  ENDMETHOD.
  METHOD deserialize_proxy.

    DATA: lv_transport    TYPE trkorr,
          li_proxy_object TYPE REF TO if_px_main,
          lv_name         TYPE prx_r3name,
          lx_proxy_fault  TYPE REF TO cx_proxy_fault.

    lv_name = ms_item-obj_name.

    lv_transport = iv_transport.

    TRY.
        li_proxy_object = cl_pxn_factory=>create(
                              application  = 'PROXY_UI'
                              display_only = abap_false
                              saveable     = abap_true
                          )->if_pxn_factory~load_by_abap_name(
                              object   = ms_item-obj_type
                              obj_name = lv_name ).

        li_proxy_object->activate(
          EXPORTING
            activate_all     = abap_true
          CHANGING
            transport_number = lv_transport ).

        li_proxy_object->dequeue( ).

      CATCH cx_proxy_fault INTO lx_proxy_fault.
        IF li_proxy_object IS BOUND.
          TRY.
              li_proxy_object->dequeue( ).
            CATCH cx_proxy_fault ##NO_HANDLER.
          ENDTRY.
        ENDIF.
        Lcx_abapgit_exception=>raise_with_text( lx_proxy_fault ).
    ENDTRY.

  ENDMETHOD.
  METHOD read_json.
    DATA lv_json_data TYPE xstring.
    DATA ls_intf_aff TYPE Lif_abapgit_aff_intf_v1=>ty_main.
    DATA lo_aff_mapper TYPE REF TO Lif_abapgit_aff_type_mapping.

    lv_json_data = Lif_abapgit_object~mo_files->read_raw( 'json' ).
    ls_intf_aff = SHRIS5ZPAUXVKEPN5HWETLLAS3UBTU=>deserialize( lv_json_data ).

    CREATE OBJECT lo_aff_mapper TYPE SHRIS5ZPAUXVKEPN5HWETLLAS3SBTU.
    lo_aff_mapper->to_abapgit( EXPORTING iv_data = ls_intf_aff
                                         iv_object_name = ms_item-obj_name
                               IMPORTING es_data = rs_intf ).
  ENDMETHOD.
  METHOD read_xml.
    ii_xml->read( EXPORTING iv_name = 'VSEOINTERF'
                  CHANGING  cg_data = rs_intf-vseointerf ).
    ii_xml->read( EXPORTING iv_name = 'DESCRIPTIONS'
                  CHANGING  cg_data = rs_intf-description ).
    ii_xml->read( EXPORTING iv_name = 'DESCRIPTIONS_SUB'
                  CHANGING  cg_data = rs_intf-description_sub ).
    ii_xml->read( EXPORTING iv_name = 'LINES'
                  CHANGING  cg_data = rs_intf-docu-lines ).
    ii_xml->read( EXPORTING iv_name = 'I18N_LINES'
                  CHANGING  cg_data = rs_intf-docu-i18n_lines ).
  ENDMETHOD.
  METHOD serialize_descr.

    DATA: lt_descriptions    TYPE Lif_abapgit_oo_object_fnc=>ty_seocompotx_tt,
          lv_language        TYPE spras,
          lt_language_filter TYPE Lif_abapgit_environment=>ty_system_language_filter.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      lv_language = mv_language.
    ENDIF.

    lt_descriptions = mi_object_oriented_object_fct->read_descriptions(
      iv_object_name = iv_clsname
      iv_language    = lv_language ).

    " Remove technical languages
    lt_language_filter = mo_i18n_params->build_language_filter( ).
    DELETE lt_descriptions WHERE NOT langu IN lt_language_filter AND langu <> mv_language.

    IF lines( lt_descriptions ) = 0.
      RETURN.
    ENDIF.

    rs_description = lt_descriptions.

  ENDMETHOD.
  METHOD serialize_descr_sub.

    DATA: lt_descriptions    TYPE Lif_abapgit_oo_object_fnc=>ty_seosubcotx_tt,
          lv_language        TYPE spras,
          lt_language_filter TYPE Lif_abapgit_environment=>ty_system_language_filter.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      lv_language = mv_language.
    ENDIF.

    lt_descriptions = mi_object_oriented_object_fct->read_descriptions_sub(
      iv_object_name = iv_clsname
      iv_language    = lv_language ).

    " Remove technical languages
    lt_language_filter = mo_i18n_params->build_language_filter( ).
    DELETE lt_descriptions WHERE NOT langu IN lt_language_filter AND langu <> mv_language.

    IF lines( lt_descriptions ) = 0.
      RETURN.
    ENDIF.

    rs_description = lt_descriptions.

  ENDMETHOD.
  METHOD serialize_docu.

    DATA: lt_lines      TYPE tlinetab,
          lv_object     TYPE dokhl-object,
          lv_langu      TYPE sy-langu,
          lt_i18n_lines TYPE Lif_abapgit_lang_definitions=>ty_i18n_lines,
          ls_i18n_lines TYPE Lif_abapgit_lang_definitions=>ty_i18n_line.

    lv_object = iv_clsname.

    lt_lines = mi_object_oriented_object_fct->read_documentation(
      iv_id          = c_longtext_id-interface
      iv_object_name = lv_object
      iv_language    = mv_language ).

    rs_docu-lines = lt_lines.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      RETURN.
    ENDIF.

    LOOP AT it_langu_additional INTO lv_langu.

      lt_lines = mi_object_oriented_object_fct->read_documentation(
        iv_id          = c_longtext_id-interface
        iv_object_name = lv_object
        iv_language    = lv_langu ).

      IF lines( lt_lines ) > 0.
        CLEAR ls_i18n_lines.
        ls_i18n_lines-language = lv_langu.
        ls_i18n_lines-lines    = lt_lines.
        INSERT ls_i18n_lines INTO TABLE lt_i18n_lines.
      ENDIF.

    ENDLOOP.

    rs_docu-i18n_lines = lt_i18n_lines.

  ENDMETHOD.
  METHOD serialize_xml.

    DATA:
      ls_intf             TYPE ty_intf,
      ls_clskey           TYPE seoclskey,
      lv_serialized_data  TYPE xstring,
      lt_langu_additional TYPE Lif_abapgit_lang_definitions=>ty_langus.

    ls_clskey-clsname = ms_item-obj_name.

    ls_intf-vseointerf = mi_object_oriented_object_fct->get_interface_properties( ls_clskey ).

    clear_abap_language_version( CHANGING cv_abap_language_version = ls_intf-vseointerf-unicode ).

    " Select all active translations of documentation
    " Skip main language - it was already serialized
    SELECT DISTINCT langu
      INTO TABLE lt_langu_additional
      FROM dokhl
      WHERE id     = c_longtext_id-interface
        AND object = ls_clskey-clsname
        AND langu  <> mv_language
      ORDER BY langu.

    ls_intf-docu = serialize_docu(
      iv_clsname          = ls_clskey-clsname
      it_langu_additional = lt_langu_additional ).

    ls_intf-description = serialize_descr( ls_clskey-clsname ).
    ls_intf-description_sub = serialize_descr_sub( ls_clskey-clsname ).

    " HERE: switch with feature flag for XML or JSON file format
    IF mv_aff_enabled = abap_true.
      lv_serialized_data = SHRIS5ZPAUXVKEPN5HWETLLAS3UBTU=>serialize( ls_intf ).
      Lif_abapgit_object~mo_files->add_raw( iv_ext  = 'json'
                                            iv_data = lv_serialized_data ).

    ELSE.
      io_xml->add( iv_name = 'VSEOINTERF'
                   ig_data = ls_intf-vseointerf ).
      io_xml->add( iv_name = 'DESCRIPTIONS'
                   ig_data = ls_intf-description ).
      io_xml->add( iv_name = 'DESCRIPTIONS_SUB'
                   ig_data = ls_intf-description_sub ).
      io_xml->add( iv_name = 'LINES'
                   ig_data = ls_intf-docu-lines ).
      io_xml->add( iv_name = 'I18N_LINES'
                   ig_data = ls_intf-docu-i18n_lines ).

      serialize_longtexts(
        ii_xml           = io_xml
        iv_longtext_name = c_longtext_name-attributes
        iv_longtext_id   = c_longtext_id-attributes ).

      serialize_longtexts(
        ii_xml           = io_xml
        iv_longtext_name = c_longtext_name-methods
        iv_longtext_id   = c_longtext_id-methods ).

      serialize_longtexts(
        ii_xml           = io_xml
        iv_longtext_name = c_longtext_name-events
        iv_longtext_id   = c_longtext_id-events ).

    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    TYPES: BEGIN OF ty_includes,
             programm TYPE syrepid,
           END OF ty_includes.

    TYPES: BEGIN OF ty_reposrc,
             unam  TYPE reposrc-unam,
             udat  TYPE reposrc-udat,
             utime TYPE reposrc-utime,
           END OF ty_reposrc.

    DATA: lt_reposrc  TYPE STANDARD TABLE OF ty_reposrc,
          ls_reposrc  LIKE LINE OF lt_reposrc,
          lt_includes TYPE STANDARD TABLE OF ty_includes.

    lt_includes = mi_object_oriented_object_fct->get_includes( ms_item-obj_name ).
    ASSERT lines( lt_includes ) > 0.

    SELECT unam udat utime FROM reposrc
      INTO TABLE lt_reposrc
      FOR ALL ENTRIES IN lt_includes
      WHERE progname = lt_includes-programm
      AND r3state = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ELSE.
      SORT lt_reposrc BY udat DESCENDING utime DESCENDING.
      READ TABLE lt_reposrc INDEX 1 INTO ls_reposrc.
      ASSERT sy-subrc = 0.
      rv_user = ls_reposrc-unam.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.
    DATA: ls_clskey     TYPE seoclskey,
          ls_vseointerf TYPE vseointerf.

    ls_clskey-clsname = ms_item-obj_name.
    ls_vseointerf = mi_object_oriented_object_fct->get_interface_properties( ls_clskey ).

    IF ls_vseointerf-clsproxy = abap_true.
      " Proxy interfaces are managed via SPRX
      RETURN.
    ENDIF.

    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    corr_insert( iv_package ).

    mi_object_oriented_object_fct->delete( ls_clskey ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.
    DATA: lt_source TYPE rswsourcet,
          ls_clskey TYPE seoclskey,
          ls_intf   TYPE ty_intf.

    IF iv_step = Lif_abapgit_object=>gc_step_id-abap.
      " HERE: switch with feature flag between XML and JSON file format
      IF mv_aff_enabled = abap_true.
        ls_intf = read_json( ).
      ELSE.
        ls_intf = read_xml( io_xml ).
      ENDIF.

      set_abap_language_version( CHANGING cv_abap_language_version = ls_intf-vseointerf-unicode ).

      IF ls_intf-vseointerf-clsproxy = abap_true.
        " Proxy interfaces are managed via SPRX
        deserialize_proxy( iv_transport ).

      ELSE.
        mi_object_oriented_object_fct->create(
          EXPORTING
            iv_check      = abap_true
            iv_package    = iv_package
          CHANGING
            cg_properties = ls_intf-vseointerf ).

        ls_clskey-clsname = ms_item-obj_name.
        lt_source = Lif_abapgit_object~mo_files->read_abap( ).

        mi_object_oriented_object_fct->deserialize_source(
          is_key     = ls_clskey
          iv_package = iv_package
          iv_version = ls_intf-vseointerf-unicode
          it_source  = lt_source ).

        deserialize_descriptions( ls_intf-description ).

        deserialize_descr_sub( ls_intf-description_sub ).

        deserialize_docu(
          is_docu = ls_intf-docu
          ii_xml  = io_xml ).

        mi_object_oriented_object_fct->add_to_activation_list( ms_item ).
      ENDIF.

    ELSEIF iv_step = Lif_abapgit_object=>gc_step_id-early.

      " If interface does not exist, create it
      " so DDIC that depends on it does not fail activation
      IF Lif_abapgit_object~exists( ) = abap_false.
        deserialize_pre_ddic(
          ii_xml     = io_xml
          iv_package = iv_package ).
      ELSE.
        corr_insert( iv_package ).
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: ls_class_key TYPE seoclskey,
          lv_category  TYPE seoclassdf-category.

    ls_class_key-clsname = ms_item-obj_name.

    rv_bool = mi_object_oriented_object_fct->exists( ls_class_key ).

    IF rv_bool = abap_true.
      SELECT SINGLE category FROM seoclassdf INTO lv_category
        WHERE clsname = ls_class_key-clsname
        AND ( version = '1'
        OR version = '0' ) ##WARN_OK.                   "#EC CI_GENBUFF
      IF sy-subrc = 0 AND lv_category = seoc_category_webdynpro_class.
        rv_bool = abap_false.
      ELSE.
        SELECT SINGLE obj_name FROM sproxhdr INTO ls_class_key-clsname
          WHERE object = 'INTF' AND obj_name = ls_class_key-clsname.
        IF sy-subrc = 0.
          " generated by proxy
          rv_bool = abap_false.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-early TO rt_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    DATA: lv_object TYPE eqegraarg.

    lv_object = |{ ms_item-obj_name }|.
    OVERLAY lv_object WITH '==============================P'.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESEOCLASS'
                                            iv_argument    = lv_object ).

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

    DATA: lt_source        TYPE seop_source_string,
          ls_interface_key TYPE seoclskey.

    ls_interface_key-clsname = ms_item-obj_name.

    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        version = seoc_version_active
        force   = abap_true.
    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        version = seoc_version_inactive
        force   = abap_true.

    lt_source = mi_object_oriented_object_fct->serialize_abap( ls_interface_key ).

    Lif_abapgit_object~mo_files->add_abap( lt_source ).

    serialize_xml( io_xml ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_INTF implementation

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
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
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
endclass. "ZCL_ABAPGIT_OBJECT_IOBJ implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_IWMO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_iwmo=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_iwmo=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_IWMO implementation.
*"* method's implementations
*include methods.
  METHOD get_field_rules.
    ro_result = Lcl_abapgit_field_rules=>create( ).
    ro_result->add(
      iv_table     = '/IWBEP/I_MGW_OHD'
      iv_field     = 'CREATED_BY'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWBEP/I_MGW_OHD'
      iv_field     = 'CREATED_TIMESTMP'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-timestamp
    )->add(
      iv_table     = '/IWBEP/I_MGW_OHD'
      iv_field     = 'CHANGED_BY'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWBEP/I_MGW_OHD'
      iv_field     = 'CHANGED_TIMESTMP'
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
    SELECT created_by changed_by INTO (lv_created, lv_changed) FROM ('/IWBEP/I_MGW_OHD')
      WHERE technical_name = ms_item-obj_name.
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

    DATA: lv_mdl_technical_name TYPE c LENGTH 32,
          lv_version            TYPE bdc_fval,
          lt_bdcdata            TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.

    lv_mdl_technical_name = ms_item-obj_name.
    lv_version = ms_item-obj_name+32(4).

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = '/IWBEP/R_DST_MODEL_BUILDER'.
    <ls_bdcdata>-dynpro   = '0100'.
    <ls_bdcdata>-dynbegin = 'X'.
    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'GS_MODEL_SCREEN_100-TECHNICAL_NAME'.
    <ls_bdcdata>-fval = lv_mdl_technical_name.
    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'GS_MODEL_SCREEN_100-VERSION'.
    <ls_bdcdata>-fval = lv_version.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = '/IWBEP/REG_MODEL'
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

    get_generic( )->serialize( io_xml ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_IWMO implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_IWOM <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_iwom=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_iwom=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_IWOM implementation.
*"* method's implementations
*include methods.
  METHOD get_field_rules.
    ro_result = Lcl_abapgit_field_rules=>create( ).
    ro_result->add(
      iv_table     = '/IWFND/I_MED_OHD'
      iv_field     = 'CREATED_BY'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWFND/I_MED_OHD'
      iv_field     = 'CREATED_TIMESTMP'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-timestamp
    )->add(
      iv_table     = '/IWFND/I_MED_OHD'
      iv_field     = 'CHANGED_BY'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWFND/I_MED_OHD'
      iv_field     = 'CHANGED_TIMESTMP'
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

    SELECT SINGLE changed_by FROM ('/IWFND/I_MED_OHD') INTO rv_user
      WHERE model_identifier = ms_item-obj_name.
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
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    get_generic( )->serialize( io_xml ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_IWOM implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_IWPR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_iwpr=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_iwpr=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_IWPR implementation.
*"* method's implementations
*include methods.
  METHOD get_field_rules.
    ro_result = Lcl_abapgit_field_rules=>create( ).
    ro_result->add(
      iv_table     = '/IWBEP/I_SBD_GA'
      iv_field     = 'CREATION_USER_ID'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWBEP/I_SBD_GA'
      iv_field     = 'CREATION_TIME'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-timestamp
    )->add(
      iv_table     = '/IWBEP/I_SBD_GA'
      iv_field     = 'LAST_CHG_USER_ID'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWBEP/I_SBD_GA'
      iv_field     = 'LAST_CHG_TIME'
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

    SELECT SINGLE last_chg_user_id FROM ('/IWBEP/I_SBD_PR') INTO rv_user
      WHERE project = ms_item-obj_name.
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

    SUBMIT /iwbep/r_sbui_service_builder
      WITH i_prname = ms_item-obj_name
      AND RETURN.

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    get_generic( )->serialize( io_xml ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_IWPR implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_IWSG <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_iwsg=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_iwsg=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_IWSG implementation.
*"* method's implementations
*include methods.
  METHOD get_field_rules.

    ro_result = Lcl_abapgit_field_rules=>create( ).
    ro_result->add(
      iv_table     = '/IWFND/I_MED_SRH'
      iv_field     = 'CREATED_BY'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWFND/I_MED_SRH'
      iv_field     = 'CREATED_TIMESTMP'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-timestamp
    )->add(
      iv_table     = '/IWFND/I_MED_SRH'
      iv_field     = 'CHANGED_BY'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWFND/I_MED_SRH'
      iv_field     = 'CHANGED_TIMESTMP'
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

    SELECT SINGLE changed_by FROM ('/IWFND/I_MED_SRH') INTO rv_user
      WHERE srv_identifier = ms_item-obj_name.
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
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    get_generic( )->serialize( io_xml ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_IWSG implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_IWSV <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_iwsv=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_iwsv=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_IWSV implementation.
*"* method's implementations
*include methods.
  METHOD get_field_rules.
    ro_result = Lcl_abapgit_field_rules=>create( ).
    ro_result->add(
      iv_table     = '/IWBEP/I_MGW_SRH'
      iv_field     = 'CREATED_BY'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWBEP/I_MGW_SRH'
      iv_field     = 'CREATED_TIMESTMP'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-timestamp
    )->add(
      iv_table     = '/IWBEP/I_MGW_SRH'
      iv_field     = 'CHANGED_BY'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWBEP/I_MGW_SRH'
      iv_field     = 'CHANGED_TIMESTMP'
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
    SELECT created_by changed_by INTO (lv_created, lv_changed) FROM ('/IWBEP/I_MGW_SRH')
      WHERE technical_name = ms_item-obj_name.
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

    DATA: lv_technical_name TYPE c LENGTH 35,
          lv_version        TYPE bdc_fval,
          lt_bdcdata        TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.

    lv_technical_name = ms_item-obj_name(36).
    lv_version = ms_item-obj_name+36(4).

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = '/IWBEP/R_DST_SERVICE_BUILDER'.
    <ls_bdcdata>-dynpro   = '0100'.
    <ls_bdcdata>-dynbegin = 'X'.
    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'GS_SCREEN_100-TECHNICAL_NAME'.
    <ls_bdcdata>-fval = lv_technical_name.
    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'GS_SCREEN_100-VERSION'.
    <ls_bdcdata>-fval = lv_version.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = '/IWBEP/REG_SERVICE'
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

    get_generic( )->serialize( io_xml ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_IWSV implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_IWVB <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_iwvb=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_iwvb=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_IWVB implementation.
*"* method's implementations
*include methods.
  METHOD get_field_rules.
    ro_result = Lcl_abapgit_field_rules=>create( ).
    ro_result->add(
      iv_table     = '/IWBEP/I_MGW_VAH'
      iv_field     = 'CREATED_BY'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWBEP/I_MGW_VAH'
      iv_field     = 'CREATED_TIMESTMP'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-timestamp
    )->add(
      iv_table     = '/IWBEP/I_MGW_VAH'
      iv_field     = 'CHANGED_BY'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWBEP/I_MGW_VAH'
      iv_field     = 'CHANGED_TIMESTMP'
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
    SELECT created_by changed_by INTO (lv_created, lv_changed) FROM ('/IWBEP/I_MGW_VAH')
      WHERE technical_name = ms_item-obj_name.
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

    SUBMIT /iwbep/r_dst_vocan_register
      WITH ip_aname = ms_item-obj_name
      WITH ip_avers = ms_item-obj_name+32(4)
      AND RETURN.

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    get_generic( )->serialize( io_xml ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_IWVB implementation

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
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
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
endclass. "ZCL_ABAPGIT_OBJECT_JOBD implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_MSAG <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_msag=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_msag=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_MSAG implementation.
*"* method's implementations
*include methods.
  METHOD delete_documentation.
    DATA: lv_key_s TYPE dokhl-object.

    CLEAR lv_key_s.
    CALL FUNCTION 'DOCU_OBJECT_NAME_CONCATENATE'
      EXPORTING
        docu_id  = c_longtext_id_msag
        element  = iv_message_id
        addition = '   '
      IMPORTING
        object   = lv_key_s
      EXCEPTIONS
        OTHERS   = 0.

    CALL FUNCTION 'DOKU_DELETE_ALL'
      EXPORTING
        doku_id                        = c_longtext_id_msag
        doku_object                    = lv_key_s
        generic_use                    = 'X'
        suppress_authority             = space
        suppress_enqueue               = space
        suppress_transport             = space
      EXCEPTIONS
        header_without_text            = 1
        index_without_header           = 2
        no_authority_for_devclass_xxxx = 3
        no_docu_found                  = 4
        object_is_already_enqueued     = 5
        object_is_enqueued_by_corr     = 6
        user_break                     = 7.

  ENDMETHOD.
  METHOD delete_msgid.

    delete_documentation( iv_message_id ).

    DELETE FROM t100a WHERE arbgb = iv_message_id.
    IF sy-subrc = 0 OR sy-subrc = 4.
      CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
        EXPORTING
          object    = iv_message_id
          operation = 'DELETE'
          program   = space
          type      = 'CN'.
      DELETE FROM t100o WHERE arbgb = iv_message_id.
      DELETE FROM t100t WHERE arbgb = iv_message_id.    "#EC CI_NOFIRST
      DELETE FROM t100u WHERE arbgb = iv_message_id.
      DELETE FROM t100x WHERE arbgb = iv_message_id.
      DELETE FROM t100 WHERE arbgb = iv_message_id.
    ENDIF.


  ENDMETHOD.
  METHOD deserialize_texts.

    DATA: lv_msg_id     TYPE rglif-message_id,
          ls_t100       TYPE t100,
          lt_t100t      TYPE TABLE OF t100t,
          lt_t100_texts TYPE ty_t100_texts,
          lt_t100u      TYPE TABLE OF t100u.

    FIELD-SYMBOLS: <ls_t100_text> TYPE ty_t100_text.


    lv_msg_id = ms_item-obj_name.

    SELECT * FROM t100u INTO TABLE lt_t100u
      WHERE arbgb = lv_msg_id ORDER BY PRIMARY KEY.     "#EC CI_GENBUFF

    ii_xml->read( EXPORTING iv_name = 'T100_TEXTS'
                  CHANGING  cg_data = lt_t100_texts ).

    ii_xml->read( EXPORTING iv_name = 'T100T'
                  CHANGING  cg_data = lt_t100t ).

    mo_i18n_params->trim_saplang_keyed_table(
      EXPORTING
        iv_lang_field_name = 'SPRSL'
      CHANGING
        ct_tab = lt_t100_texts ).
    mo_i18n_params->trim_saplang_keyed_table(
      EXPORTING
        iv_lang_field_name = 'SPRSL'
      CHANGING
        ct_tab = lt_t100t ).

    MODIFY t100t FROM TABLE lt_t100t.                     "#EC CI_SUBRC

    LOOP AT lt_t100_texts ASSIGNING <ls_t100_text>.
      "check if message exists
      READ TABLE lt_t100u TRANSPORTING NO FIELDS
        WITH KEY arbgb = lv_msg_id msgnr = <ls_t100_text>-msgnr BINARY SEARCH.
      CHECK sy-subrc = 0. "if original message doesn't exist no translations added

      MOVE-CORRESPONDING <ls_t100_text> TO ls_t100.
      ls_t100-arbgb = lv_msg_id.
      MODIFY t100 FROM ls_t100.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'MSAG: Table T100 modify failed' ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD free_access_permission.
    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
        mode         = 'FREE'
        object       = iv_message_id
        object_class = 'T100'.
  ENDMETHOD.
  METHOD serialize_longtexts_msag.

    DATA: lv_doku_object_name  TYPE dokhl-object,
          lt_doku_object_names TYPE STANDARD TABLE OF dokhl-object
                          WITH NON-UNIQUE DEFAULT KEY,
          lt_dokil             TYPE Lif_abapgit_definitions=>ty_dokil_tt,
          ls_dokil             LIKE LINE OF lt_dokil,
          lt_language_filter   TYPE Lif_abapgit_environment=>ty_system_language_filter.

    FIELD-SYMBOLS: <ls_t100>  TYPE t100.

    IF lines( it_t100 ) = 0.
      RETURN.
    ENDIF.

    LOOP AT it_t100 ASSIGNING <ls_t100>.

      lv_doku_object_name = <ls_t100>-arbgb && <ls_t100>-msgnr.
      INSERT lv_doku_object_name INTO TABLE lt_doku_object_names.

    ENDLOOP.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      SELECT * FROM dokil
        INTO TABLE lt_dokil
        FOR ALL ENTRIES IN lt_doku_object_names
        WHERE id = c_longtext_id_msag
        AND object = lt_doku_object_names-table_line
        AND masterlang = abap_true
        ORDER BY PRIMARY KEY.
    ELSE.
      lt_language_filter = mo_i18n_params->build_language_filter( ).
      SELECT * FROM dokil
        INTO TABLE lt_dokil
        FOR ALL ENTRIES IN lt_doku_object_names
        WHERE id = c_longtext_id_msag
        AND object = lt_doku_object_names-table_line
        AND langu IN lt_language_filter
        ORDER BY PRIMARY KEY.
    ENDIF.

    CLEAR ls_dokil-dokstate.
    MODIFY lt_dokil FROM ls_dokil TRANSPORTING dokstate WHERE dokstate IS NOT INITIAL.

    IF lines( lt_dokil ) > 0.
      serialize_longtexts( ii_xml   = ii_xml
                           it_dokil = lt_dokil ).
    ENDIF.

  ENDMETHOD.
  METHOD serialize_texts.

    DATA: lv_msg_id          TYPE rglif-message_id,
          lt_t100_texts      TYPE ty_t100_texts,
          lt_t100t           TYPE TABLE OF t100t,
          lt_i18n_langs      TYPE TABLE OF langu,
          lt_language_filter TYPE Lif_abapgit_environment=>ty_system_language_filter.

    lv_msg_id = ms_item-obj_name.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      RETURN. " skip
    ENDIF.

    " Collect additional languages
    " Skip main lang - it has been already serialized and also technical languages
    lt_language_filter = mo_i18n_params->build_language_filter( ).

    SELECT DISTINCT sprsl AS langu INTO TABLE lt_i18n_langs
      FROM t100t
      WHERE arbgb = lv_msg_id
      AND sprsl IN lt_language_filter
      AND sprsl <> mv_language.          "#EC CI_BYPASS "#EC CI_GENBUFF

    SORT lt_i18n_langs ASCENDING.

    IF lines( lt_i18n_langs ) > 0.

      SELECT * FROM t100t INTO CORRESPONDING FIELDS OF TABLE lt_t100t
        WHERE sprsl IN lt_language_filter
        AND sprsl <> mv_language
        AND arbgb = lv_msg_id.                          "#EC CI_GENBUFF

      SELECT * FROM t100 INTO CORRESPONDING FIELDS OF TABLE lt_t100_texts
        WHERE sprsl IN lt_language_filter
        AND sprsl <> mv_language
        AND arbgb = lv_msg_id
        ORDER BY PRIMARY KEY.             "#EC CI_SUBRC "#EC CI_GENBUFF

      SORT lt_t100t BY sprsl ASCENDING.
      SORT lt_t100_texts BY sprsl msgnr ASCENDING.

      ii_xml->add( iv_name = 'I18N_LANGS'
                   ig_data = lt_i18n_langs ).

      ii_xml->add( iv_name = 'T100T'
                   ig_data = lt_t100t ).

      ii_xml->add( iv_name = 'T100_TEXTS'
                   ig_data = lt_t100_texts ).

    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE lastuser FROM t100a INTO rv_user
      WHERE arbgb = ms_item-obj_name.                   "#EC CI_GENBUFF
    IF sy-subrc <> 0 OR rv_user = ''.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.
    DATA: ls_t100a          TYPE t100a,
          lv_frozen         TYPE abap_bool,
          lv_message_id     TYPE arbgb.

* parameter SUPPRESS_DIALOG doesnt exist in all versions of FM RS_DELETE_MESSAGE_ID
* replaced with a copy
    lv_message_id = ms_item-obj_name.
    IF ms_item-obj_name = space.
      Lcx_abapgit_exception=>raise( 'Error from (copy of) RS_DELETE_MESSAGE_ID' )."blank message id
    ENDIF.

    SELECT SINGLE * FROM t100a INTO ls_t100a WHERE arbgb = ms_item-obj_name.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Error from (copy of) RS_DELETE_MESSAGE_ID' )."not found
    ENDIF.

    CLEAR lv_frozen.
    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
        authority_check = 'X'
        global_lock     = 'X'
        mode            = 'MODIFY'
        object          = lv_message_id
        object_class    = 'T100'
      IMPORTING
        frozen          = lv_frozen
      EXCEPTIONS
        OTHERS          = 1.

    IF sy-subrc <> 0 OR lv_frozen <> space.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    Lcl_abapgit_factory=>get_cts_api( )->insert_transport_object(
      iv_object   = 'MSAG'
      iv_obj_name = lv_message_id
      iv_package  = iv_package
      iv_language = mv_language
      iv_mode     = Lif_abapgit_cts_api=>c_transport_mode-delete ).

    delete_msgid( lv_message_id ).

    free_access_permission( lv_message_id ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.
* fm RPY_MESSAGE_ID_INSERT almost works, but not in older versions

    DATA: ls_t100a  TYPE t100a,
          ls_t100t  TYPE t100t,
          ls_t100u  TYPE t100u,
          lt_t100   TYPE TABLE OF t100,
          lt_before TYPE TABLE OF t100u.

    FIELD-SYMBOLS: <ls_t100> LIKE LINE OF lt_t100.


    io_xml->read( EXPORTING iv_name = 'T100A'
                  CHANGING cg_data = ls_t100a ).
    io_xml->read( EXPORTING iv_name = 'T100'
                  CHANGING cg_data = lt_t100 ).

    corr_insert( iv_package ).

    SELECT * FROM t100u INTO TABLE lt_before
      WHERE arbgb = ls_t100a-arbgb ORDER BY msgnr. "#EC CI_GENBUFF "#EC CI_BYPASS

    LOOP AT lt_t100 ASSIGNING <ls_t100>.
      DELETE lt_before WHERE msgnr = <ls_t100>-msgnr.
      MODIFY t100 FROM <ls_t100>.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'MSAG: Table T100 modify failed' ).
      ENDIF.
      CLEAR ls_t100u.
      MOVE-CORRESPONDING <ls_t100> TO ls_t100u ##ENH_OK.
      ls_t100u-name    = sy-uname.
      ls_t100u-datum   = sy-datum.
      ls_t100u-selfdef = '3'.
      MODIFY t100u FROM ls_t100u.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'MSAG: Table T100U modify failed' ).
      ENDIF.
    ENDLOOP.

    ls_t100a-masterlang = mv_language.
    ls_t100a-lastuser = sy-uname.
    ls_t100a-respuser = sy-uname.
    ls_t100a-ldate = sy-datum.
    ls_t100a-ltime = sy-uzeit.
    MODIFY t100a FROM ls_t100a.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'MSAG: Table T100A modify failed' ).
    ENDIF.

    ls_t100t-sprsl = mv_language.
    ls_t100t-arbgb = ls_t100a-arbgb.
    ls_t100t-stext = ls_t100a-stext.
    MODIFY t100t FROM ls_t100t.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'MSAG: Table T100T modify failed' ).
    ENDIF.

    LOOP AT lt_before INTO ls_t100u.
      DELETE FROM t100 WHERE arbgb = ls_t100u-arbgb
        AND msgnr = ls_t100u-msgnr.                       "#EC CI_SUBRC

      DELETE FROM t100u WHERE arbgb = ls_t100u-arbgb
        AND msgnr = ls_t100u-msgnr.                       "#EC CI_SUBRC
    ENDLOOP.

    deserialize_longtexts( ii_xml         = io_xml
                           iv_longtext_id = c_longtext_id_msag ).

    IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
      deserialize_texts( io_xml ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_arbgb TYPE t100a-arbgb.


    SELECT SINGLE arbgb FROM t100a INTO lv_arbgb
      WHERE arbgb = ms_item-obj_name.                   "#EC CI_GENBUFF
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
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    DATA: lv_argument TYPE seqg3-garg.

    lv_argument   = |{ ms_item-obj_name }|.
    OVERLAY lv_argument WITH '                     '.
    lv_argument = lv_argument && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = |ES_MSGSI|
                                            iv_argument    = lv_argument ).

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

    DATA: lv_msg_id TYPE rglif-message_id,
          ls_inf    TYPE t100a,
          lt_source TYPE ty_t100s.


    lv_msg_id = ms_item-obj_name.

    SELECT SINGLE * FROM t100a INTO ls_inf
      WHERE arbgb = lv_msg_id.                          "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    CLEAR ls_inf-respuser.

    SELECT * FROM t100 INTO TABLE lt_source
      WHERE sprsl = mv_language
      AND arbgb = lv_msg_id
      ORDER BY PRIMARY KEY.               "#EC CI_SUBRC "#EC CI_GENBUFF

    CLEAR: ls_inf-lastuser,
           ls_inf-ldate,
           ls_inf-ltime.

    io_xml->add( iv_name = 'T100A'
                 ig_data = ls_inf ).
    io_xml->add( ig_data = lt_source
                 iv_name = 'T100' ).

    serialize_longtexts_msag( it_t100 = lt_source
                              ii_xml  = io_xml ).

    IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
      serialize_texts( io_xml ).
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_MSAG implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_NROB <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_nrob=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_nrob=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_NROB implementation.
*"* method's implementations
*include methods.
  METHOD delete_intervals.

    DATA: lv_error    TYPE c LENGTH 1,
          ls_error    TYPE inrer,
          lt_list     TYPE STANDARD TABLE OF inriv WITH DEFAULT KEY,
          lt_error_iv TYPE STANDARD TABLE OF inriv WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF lt_list.


    CALL FUNCTION 'NUMBER_RANGE_INTERVAL_LIST'
      EXPORTING
        object                     = iv_object
      TABLES
        interval                   = lt_list
      EXCEPTIONS
        nr_range_nr1_not_found     = 1
        nr_range_nr1_not_intern    = 2
        nr_range_nr2_must_be_space = 3
        nr_range_nr2_not_extern    = 4
        nr_range_nr2_not_found     = 5
        object_not_found           = 6
        subobject_must_be_space    = 7
        subobject_not_found        = 8
        OTHERS                     = 9.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF lines( lt_list ) = 0.
      RETURN.
    ENDIF.

    LOOP AT lt_list ASSIGNING <ls_list>.
      CLEAR <ls_list>-nrlevel.
      <ls_list>-procind = 'D'.
    ENDLOOP.

    CALL FUNCTION 'NUMBER_RANGE_INTERVAL_UPDATE'
      EXPORTING
        object           = iv_object
      IMPORTING
        error            = ls_error
        error_occured    = lv_error
      TABLES
        error_iv         = lt_error_iv
        interval         = lt_list
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0 OR lv_error = abap_true.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'NUMBER_RANGE_UPDATE_CLOSE'
      EXPORTING
        object                 = iv_object
      EXCEPTIONS
        no_changes_made        = 1
        object_not_initialized = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lv_objectid TYPE cdhdr-objectid,
          lt_cdhdr    TYPE cdhdr_tab.

    FIELD-SYMBOLS: <ls_cdhdr> LIKE LINE OF lt_cdhdr.


    lv_objectid = ms_item-obj_name.

    CALL FUNCTION 'CHANGEDOCUMENT_READ_HEADERS'
      EXPORTING
        objectclass                = 'NRKROBJ'
        objectid                   = lv_objectid
      TABLES
        i_cdhdr                    = lt_cdhdr
      EXCEPTIONS
        no_position_found          = 1
        wrong_access_to_archive    = 2
        time_zone_conversion_error = 3
        OTHERS                     = 4.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
      RETURN.
    ENDIF.

    SORT lt_cdhdr BY udate DESCENDING utime DESCENDING.

    READ TABLE lt_cdhdr INDEX 1 ASSIGNING <ls_cdhdr>.
    ASSERT sy-subrc = 0.

    rv_user = <ls_cdhdr>-username.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_object TYPE tnro-object.


    lv_object = ms_item-obj_name.

    delete_intervals( lv_object ).

    CALL FUNCTION 'NUMBER_RANGE_OBJECT_DELETE'
      EXPORTING
        language           = mv_language
        object             = lv_object
      EXCEPTIONS
        delete_not_allowed = 1
        object_not_found   = 2
        wrong_indicator    = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lt_errors     TYPE TABLE OF inoer,
          ls_attributes TYPE tnro,
          ls_text       TYPE tnrot.

    FIELD-SYMBOLS <lv_any> TYPE any.

    io_xml->read( EXPORTING iv_name = 'ATTRIBUTES'
                  CHANGING cg_data = ls_attributes ).
    io_xml->read( EXPORTING iv_name = 'TEXT'
                  CHANGING cg_data = ls_text ).

    ASSIGN COMPONENT 'CHANGED_AT' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      GET TIME STAMP FIELD <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-uname.
    ENDIF.
    ASSIGN COMPONENT 'ENAME' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-uname.
    ENDIF.
    ASSIGN COMPONENT 'EDATE' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-datum.
    ENDIF.
    ASSIGN COMPONENT 'ETIME' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-uzeit.
    ENDIF.

    ASSIGN COMPONENT 'UNAME' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-uname.
    ENDIF.
    ASSIGN COMPONENT 'UDATE' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-datum.
    ENDIF.
    ASSIGN COMPONENT 'UTIME' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-uzeit.
    ENDIF.
    ASSIGN COMPONENT 'ENAME' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-uname.
    ENDIF.
    ASSIGN COMPONENT 'EDATE' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-datum.
    ENDIF.
    ASSIGN COMPONENT 'ETIME' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-uzeit.
    ENDIF.

    CALL FUNCTION 'NUMBER_RANGE_OBJECT_UPDATE'
      EXPORTING
        indicator                 = 'I'
        object_attributes         = ls_attributes
        object_text               = ls_text
      TABLES
        errors                    = lt_errors
      EXCEPTIONS
        object_already_exists     = 1
        object_attributes_missing = 2
        object_not_found          = 3
        object_text_missing       = 4
        wrong_indicator           = 5
        OTHERS                    = 6.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    tadir_insert( iv_package ).
    corr_insert( iv_package ).

    CALL FUNCTION 'NUMBER_RANGE_OBJECT_CLOSE'
      EXPORTING
        object                 = ls_attributes-object
      EXCEPTIONS
        object_not_initialized = 1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_object TYPE tnro-object.


    SELECT SINGLE object FROM tnro INTO lv_object
      WHERE object = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

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
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: ls_bcdata TYPE bdcdata,
          lt_bcdata TYPE STANDARD TABLE OF bdcdata.

    ls_bcdata-program  = 'SAPMSNRO'.
    ls_bcdata-dynpro   = '0150'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam     = 'NRIV-OBJECT'.
    ls_bcdata-fval     = ms_item-obj_name.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=DISP'.
    APPEND ls_bcdata TO lt_bcdata.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SNRO'
      it_bdcdata = lt_bcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_object     TYPE tnro-object,
          ls_attributes TYPE tnro,
          ls_text       TYPE tnrot.

    FIELD-SYMBOLS <lv_any> TYPE any.


    lv_object = ms_item-obj_name.

    CALL FUNCTION 'NUMBER_RANGE_OBJECT_READ'
      EXPORTING
        language          = mv_language
        object            = lv_object
      IMPORTING
        object_attributes = ls_attributes
        object_text       = ls_text
      EXCEPTIONS
        object_not_found  = 1
        OTHERS            = 2.
    IF sy-subrc = 1.
      RETURN.
    ELSEIF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    ASSIGN COMPONENT 'CHANGED_AT' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'ENAME' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'EDATE' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'ETIME' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.

    ASSIGN COMPONENT 'UNAME' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'UDATE' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'UTIME' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'ENAME' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'EDATE' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'ETIME' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.

    io_xml->add( iv_name = 'ATTRIBUTES'
                 ig_data = ls_attributes ).
    io_xml->add( iv_name = 'TEXT'
                 ig_data = ls_text ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_NROB implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_NSPC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_nspc=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_nspc=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_NSPC implementation.
*"* method's implementations
*include methods.
  METHOD add_to_transport.

    DATA: li_sap_package TYPE REF TO Lif_abapgit_sap_package.

    li_sap_package = Lcl_abapgit_factory=>get_sap_package( iv_package ).

    IF li_sap_package->are_changes_recorded_in_tr_req( ) = abap_true.
      corr_insert( iv_package ).
    ENDIF.

  ENDMETHOD.
  METHOD deserialize_texts.

    DATA:
      ls_trnspacett TYPE trnspacett,
      lt_i18n_langs TYPE TABLE OF langu,
      lt_nspc_texts TYPE ty_nspc_texts.

    FIELD-SYMBOLS:
      <lv_lang>      LIKE LINE OF lt_i18n_langs,
      <ls_nspc_text> LIKE LINE OF lt_nspc_texts.

    ii_xml->read( EXPORTING iv_name = 'I18N_LANGS'
                  CHANGING  cg_data = lt_i18n_langs ).

    ii_xml->read( EXPORTING iv_name = 'NSPC_TEXTS'
                  CHANGING  cg_data = lt_nspc_texts ).

    SORT lt_i18n_langs.
    SORT lt_nspc_texts BY spras. " Optimization

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.
      ls_trnspacett-namespace = iv_namespace.
      READ TABLE lt_nspc_texts ASSIGNING <ls_nspc_text> WITH KEY spras = <lv_lang>.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |NSPC_TEXTS cannot find lang { <lv_lang> } in XML| ).
      ENDIF.
      MOVE-CORRESPONDING <ls_nspc_text> TO ls_trnspacett.

      MODIFY trnspacett FROM ls_trnspacett.
      IF sy-subrc <> 0.
        INSERT trnspacett FROM ls_trnspacett.
      ENDIF.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Error upserting text for namespace| ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD serialize_texts.

    DATA:
      ls_trnspacett TYPE trnspacett,
      lt_nspc_texts TYPE ty_nspc_texts,
      lt_i18n_langs TYPE TABLE OF langu.

    FIELD-SYMBOLS:
      <lv_lang>      LIKE LINE OF lt_i18n_langs,
      <ls_nspc_text> LIKE LINE OF lt_nspc_texts.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      RETURN.
    ENDIF.

    " Collect additional languages, skip main lang - it was serialized already
    SELECT DISTINCT spras AS langu FROM trnspacett INTO TABLE lt_i18n_langs
      WHERE namespace = ms_item-obj_name AND spras <> mv_language. "#EC CI_SUBRC

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.
      SELECT SINGLE * FROM trnspacett INTO ls_trnspacett
        WHERE namespace = ms_item-obj_name AND spras = <lv_lang>.
      IF sy-subrc = 0.
        APPEND INITIAL LINE TO lt_nspc_texts ASSIGNING <ls_nspc_text>.
        MOVE-CORRESPONDING ls_trnspacett TO <ls_nspc_text>.
      ENDIF.
    ENDLOOP.

    SORT lt_i18n_langs ASCENDING.
    SORT lt_nspc_texts BY spras ASCENDING.

    IF lines( lt_i18n_langs ) > 0.
      ii_xml->add( iv_name = 'I18N_LANGS'
                   ig_data = lt_i18n_langs ).

      ii_xml->add( iv_name = 'NSPC_TEXTS'
                   ig_data = lt_nspc_texts ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    SELECT SINGLE changeuser FROM trnspacet INTO rv_user
       WHERE namespace = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.
    RETURN. " not supported
  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA:
      ls_nspc       TYPE ty_nspc,
      ls_nspc_text  TYPE ty_nspc_text,
      lv_modifiable TYPE abap_bool,
      ls_trnspacet  TYPE trnspacet,
      ls_trnspacett TYPE trnspacett.

    io_xml->read( EXPORTING iv_name = 'NSPC'
                  CHANGING  cg_data = ls_nspc ).

    io_xml->read( EXPORTING iv_name = 'NSPC_TEXT'
                  CHANGING  cg_data = ls_nspc_text ).

    add_to_transport( iv_package ).

    SELECT SINGLE * FROM trnspacet INTO ls_trnspacet WHERE namespace = ls_nspc-namespace.
    IF sy-subrc = 0.
      " For existing namespace, check if it's modifiable (SE03)
      SELECT SINGLE editflag FROM trnspace INTO lv_modifiable WHERE namespace = ls_nspc-namespace.
      IF sy-subrc = 0 AND lv_modifiable = abap_false.
        Lcx_abapgit_exception=>raise( |Namespace is not modifiable| ).
      ENDIF.

      " keep existing role
      ls_trnspacet-replicense = ls_nspc-replicense.
      ls_trnspacet-sscrflag   = ls_nspc-sscrflag.
      ls_trnspacet-sapflag    = ls_nspc-sapflag.
      ls_trnspacet-gen_only   = ls_nspc-gen_only.
      ls_trnspacet-changeuser = sy-uname.
      ls_trnspacet-changedate = sy-datum.
      MODIFY trnspacet FROM ls_trnspacet.
    ELSE.
      MOVE-CORRESPONDING ls_nspc TO ls_trnspacet.
      ls_trnspacet-role       = 'C'. " customer repair license
      ls_trnspacet-changeuser = sy-uname.
      ls_trnspacet-changedate = sy-datum.
      INSERT trnspacet FROM ls_trnspacet.
    ENDIF.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error upserting namespace| ).
    ENDIF.

    SELECT SINGLE * FROM trnspacett INTO ls_trnspacett
      WHERE namespace = ls_nspc-namespace AND spras = mv_language.
    IF sy-subrc = 0.
      ls_trnspacett-descriptn = ls_nspc_text-descriptn.
      ls_trnspacett-owner     = ls_nspc_text-owner.
      MODIFY trnspacett FROM ls_trnspacett.
    ELSE.
      MOVE-CORRESPONDING ls_nspc_text TO ls_trnspacett.
      ls_trnspacett-namespace = ls_nspc-namespace.
      INSERT trnspacett FROM ls_trnspacett.
    ENDIF.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error upserting text for namespace| ).
    ENDIF.

    deserialize_texts( ii_xml       = io_xml
                       iv_namespace = ls_nspc-namespace ).

    " Fill trnspace and trnspacel tables
    CALL FUNCTION 'TR_ACTIVATE_NAMESPACE'
      EXPORTING
        iv_namespace         = ls_nspc-namespace
      EXCEPTIONS
        deletion_not_allowed = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error activating namespace| ).
    ENDIF.

    " Make namespace modifiable
    UPDATE trnspace SET editflag = abap_true WHERE namespace = ls_nspc-namespace.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA lv_namespace TYPE trnspace-namespace.

    lv_namespace = ms_item-obj_name.

    CALL FUNCTION 'TR_CHECK_NAMESPACE'
      EXPORTING
        iv_namespace        = lv_namespace
      EXCEPTIONS
        namespace_not_valid = 1
        OTHERS              = 2.

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
    rv_active = Lif_abapgit_object~exists( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Launch general maintenance for namespaces
    CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
      EXPORTING
        action                       = 'S'
        view_name                    = 'V_TRNSPACE'
        no_warning_for_clientindep   = 'X'
        variant_for_selection        = 'STANDARD'
      EXCEPTIONS
        client_reference             = 1
        foreign_lock                 = 2
        invalid_action               = 3
        no_clientindependent_auth    = 4
        no_database_function         = 5
        no_editor_function           = 6
        no_show_auth                 = 7
        no_tvdir_entry               = 8
        no_upd_auth                  = 9
        only_show_allowed            = 10
        system_failure               = 11
        unknown_field_in_dba_sellist = 12
        view_not_found               = 13
        OTHERS                       = 14.

    rv_exit = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA:
      ls_nspc      TYPE ty_nspc,
      ls_nspc_text TYPE ty_nspc_text.

    SELECT SINGLE * FROM trnspacet INTO CORRESPONDING FIELDS OF ls_nspc
      WHERE namespace = ms_item-obj_name.

    SELECT SINGLE * FROM trnspacett INTO CORRESPONDING FIELDS OF ls_nspc_text
      WHERE namespace = ms_item-obj_name AND spras = mv_language.

    io_xml->add( iv_name = 'NSPC'
                 ig_data = ls_nspc ).

    io_xml->add( iv_name = 'NSPC_TEXT'
                 ig_data = ls_nspc_text ).

    serialize_texts( io_xml ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_NSPC implementation

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
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
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
endclass. "ZCL_ABAPGIT_OBJECT_OA2P implementation

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
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
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
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ECLS_ATTRIBUTE'
                                            iv_argument    = |{ ms_item-obj_name }*| ).
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
  METHOD Lif_abapgit_object~get_deserialize_order.
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
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
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
endclass. "ZCL_ABAPGIT_OBJECT_PARA implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_PDTS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_pdts=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_pdts=======ccimp.
CLASS SHRIS5ZPAUXVKEPN5HWETLLAS35BTU DEFINITION
  INHERITING FROM cl_workflow_general_task_def
  CREATE PUBLIC
  FINAL.
  PUBLIC SECTION.

    CLASS-METHODS set_objid IMPORTING iv_objid TYPE hrobject-objid
                                      io_task  TYPE REF TO cl_workflow_general_task_def.

    CLASS-METHODS set_container_id IMPORTING iv_id   TYPE guid_32
                                             io_task TYPE REF TO cl_workflow_general_task_def. "#EC NEEDED
ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLAS35BTU IMPLEMENTATION.

  METHOD set_container_id.

    FIELD-SYMBOLS <lv_object> TYPE REF TO if_swf_cnt_container.

    ASSIGN ('IO_TASK->CONTAINER') TO <lv_object>.
    ASSERT sy-subrc = 0.

    CALL METHOD <lv_object>->('SET_GUID')
      EXPORTING
        guid_32 = iv_id.

  ENDMETHOD.


  METHOD set_objid.
    io_task->objid = iv_objid.
  ENDMETHOD.

ENDCLASS.


CLASS SHRIS5ZPAUXVKEPN5HWETLLAS37BTU DEFINITION
  CREATE PUBLIC
  FINAL.

  PUBLIC SECTION.

    INTERFACES SHRIS5ZPAUXVKEPN5HWETLLAS34BTU.

    CLASS-METHODS load IMPORTING iv_objid         TYPE hrobject-objid
                       RETURNING VALUE(ri_result) TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLAS34BTU
                       RAISING   Lcx_abapgit_exception.

    CLASS-METHODS create IMPORTING iv_objid         TYPE hrobject-objid
                                   is_task_data     TYPE SHRIS5ZPAUXVKEPN5HWETLLAS34BTU=>ty_task_data
                         RETURNING VALUE(ri_result) TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLAS34BTU
                         RAISING   Lcx_abapgit_exception.


  PRIVATE SECTION.
    CONSTANTS c_subty_task_description TYPE hr_s_subty VALUE '0120'.

    DATA mo_taskdef TYPE REF TO cl_workflow_task_ts.
    DATA ms_task TYPE SHRIS5ZPAUXVKEPN5HWETLLAS34BTU=>ty_task_data.

    DATA: mv_objid                      TYPE hrobjid.

    METHODS supply_instance RAISING Lcx_abapgit_exception.
    METHODS check_subrc_for IMPORTING iv_call TYPE clike OPTIONAL
                            RAISING   Lcx_abapgit_exception.

ENDCLASS.


CLASS SHRIS5ZPAUXVKEPN5HWETLLAS37BTU IMPLEMENTATION.

  METHOD load.

    DATA lo_taskdef TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLAS37BTU.

    CREATE OBJECT lo_taskdef.
    lo_taskdef->mv_objid = iv_objid.
    lo_taskdef->supply_instance( ).

    ri_result = lo_taskdef.

  ENDMETHOD.

  METHOD check_subrc_for.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( iv_call && ' returned ' && sy-subrc ).
    ENDIF.
  ENDMETHOD.


  METHOD supply_instance.

    cl_workflow_factory=>create_ts(
      EXPORTING
        objid                        = mv_objid
      RECEIVING
        ts_inst                      = mo_taskdef
      EXCEPTIONS
        standard_task_does_not_exist = 1
        object_could_not_be_locked   = 2
        objid_not_given              = 3
        OTHERS                       = 4 )  ##SUBRC_OK.

    check_subrc_for( 'CREATE_TS' ).

    ms_task-wi_text                    = mo_taskdef->wi_text.
    ms_task-short_text                 = mo_taskdef->short_text.
    ms_task-plvar                      = mo_taskdef->plvar.
    ms_task-method                     = mo_taskdef->method.
    ms_task-method_binding             = mo_taskdef->method_binding.
    ms_task-starting_events            = mo_taskdef->starting_events.
    ms_task-starting_events_binding    = mo_taskdef->starting_events_binding.
    ms_task-terminating_events         = mo_taskdef->terminating_events.
    ms_task-terminating_events_binding = mo_taskdef->terminating_events_binding.
    ms_task-descriptions               = mo_taskdef->descriptions.

  ENDMETHOD.

  METHOD SHRIS5ZPAUXVKEPN5HWETLLAS34BTU~clear_origin_data.

    FIELD-SYMBOLS: <ls_description>             TYPE hrs1002,
                   <ls_method_binding>          TYPE hrs1214,
                   <ls_starting_events_binding> TYPE hrs1212,
                   <ls_term_events_binding>     TYPE hrs1212.

    CLEAR: ms_task-method-aedtm,
           ms_task-method-uname.

    LOOP AT ms_task-method_binding ASSIGNING <ls_method_binding>.
      CLEAR: <ls_method_binding>-aedtm,
             <ls_method_binding>-uname.
    ENDLOOP.

    LOOP AT ms_task-starting_events_binding ASSIGNING <ls_starting_events_binding>.
      CLEAR: <ls_starting_events_binding>-aedtm,
             <ls_starting_events_binding>-uname.
    ENDLOOP.

    LOOP AT ms_task-descriptions ASSIGNING <ls_description>.
      CLEAR: <ls_description>-aedtm,
             <ls_description>-uname.
    ENDLOOP.

    LOOP AT ms_task-terminating_events_binding ASSIGNING <ls_term_events_binding>.
      CLEAR: <ls_term_events_binding>-aedtm,
             <ls_term_events_binding>-uname.
    ENDLOOP.

  ENDMETHOD.

  METHOD SHRIS5ZPAUXVKEPN5HWETLLAS34BTU~get_definition.
    rs_result = me->ms_task.
  ENDMETHOD.

  METHOD SHRIS5ZPAUXVKEPN5HWETLLAS34BTU~get_container.
    ri_result = mo_taskdef->container.
  ENDMETHOD.

  METHOD SHRIS5ZPAUXVKEPN5HWETLLAS34BTU~get_user_container.

    DATA: li_container       TYPE REF TO if_swf_cnt_element_access_1,
          lt_user_elements   TYPE swfdnamtab,
          lt_system_elements TYPE swfdnamtab,
          lv_element         TYPE swfdname.

    li_container = mo_taskdef->container.
    lt_user_elements = li_container->all_elements_list( ).
    lt_system_elements = li_container->all_elements_list( list_system = abap_true ).

    LOOP AT lt_system_elements INTO lv_element.
      READ TABLE lt_user_elements WITH KEY table_line = lv_element TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        TRY.
            li_container->element_remove( name = lv_element ).
          CATCH cx_swf_cnt_container.
            "Shouldn't happen, doesn't matter if it does
        ENDTRY.
      ENDIF.
    ENDLOOP.

    ri_result ?= li_container.

  ENDMETHOD.

  METHOD create.
    DATA lo_task TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLAS37BTU.

    CREATE OBJECT lo_task TYPE SHRIS5ZPAUXVKEPN5HWETLLAS37BTU.
    lo_task->mv_objid = iv_objid.
    lo_task->ms_task = is_task_data.
    ri_result = lo_task.

  ENDMETHOD.


  METHOD SHRIS5ZPAUXVKEPN5HWETLLAS34BTU~import_container.

    DATA lt_exception_list TYPE swf_cx_tab.
    DATA lx_exception TYPE REF TO cx_swf_ifs_exception.

    mo_taskdef->container->import_from_xml(
            EXPORTING xml_stream     = iv_xml_string
            IMPORTING exception_list = lt_exception_list ).

    IF lt_exception_list IS NOT INITIAL.
      READ TABLE lt_exception_list INDEX 1 INTO lx_exception.
      Lcx_abapgit_exception=>raise_with_text( lx_exception ).
    ENDIF.

  ENDMETHOD.

  METHOD SHRIS5ZPAUXVKEPN5HWETLLAS34BTU~create_task.

    cl_workflow_factory=>create_new_ts(
      EXPORTING
        short_text          = |{ ms_task-short_text }|
        text                = |{ ms_task-wi_text }|
      RECEIVING
        task_object         = mo_taskdef
      EXCEPTIONS
        text_exists_already = 1
        OTHERS              = 2 ).                        "#EC SUBRC_OK

    check_subrc_for( `CREATE_NEW_TS` ).

    SHRIS5ZPAUXVKEPN5HWETLLAS35BTU=>set_objid( iv_objid = mv_objid
                                           io_task  = mo_taskdef ).

    SHRIS5ZPAUXVKEPN5HWETLLAS35BTU=>set_container_id( iv_id    = |TS{ mv_objid }|
                                            io_task  = mo_taskdef ).

  ENDMETHOD.

  METHOD SHRIS5ZPAUXVKEPN5HWETLLAS34BTU~change_start_events.

    mo_taskdef->change_start_events_complete(
      EXPORTING
        starting_events    = ms_task-starting_events
      EXCEPTIONS
        no_changes_allowed = 1
        OTHERS             = 2 ).                         "#EC SUBRC_OK

    check_subrc_for( `CHANGE_START_EVENTS_COMPLETE` ).

    mo_taskdef->change_start_evt_bind_complete(
      EXPORTING
        new_bindings       = ms_task-starting_events_binding
      EXCEPTIONS
        no_changes_allowed = 1
        OTHERS             = 2 ).                         "#EC SUBRC_OK

    check_subrc_for( `CHANGE_START_EVT_BIND_COMPLETE` ).

  ENDMETHOD.


  METHOD SHRIS5ZPAUXVKEPN5HWETLLAS34BTU~save.

    DATA ls_hrsobject TYPE hrsobject.
    ls_hrsobject-otype = 'TS'. "swfco_org_standard_task - todo: linter can't resolve this
    ls_hrsobject-objid = mv_objid.
    INSERT hrsobject FROM ls_hrsobject.

    mo_taskdef->save_standard_task(
      EXPORTING
        development_class          = iv_package
        iv_force_gen               = abap_true
      EXCEPTIONS
        no_changes_allowed         = 1
        no_client_indep_maint      = 2
        update_error               = 3
        insert_error_new_ts        = 4
        new_ts_could_not_be_locked = 5
        save_abort_by_user         = 6
        OTHERS                     = 7 ).                 "#EC SUBRC_OK

    check_subrc_for( `SAVE_STANDARD_TASK` ).

  ENDMETHOD.


  METHOD SHRIS5ZPAUXVKEPN5HWETLLAS34BTU~change_wi_text.

    mo_taskdef->change_wi_text(
      EXPORTING
        new_wi_text        = ms_task-wi_text
      EXCEPTIONS
        no_changes_allowed = 1
        OTHERS             = 2 ).                         "#EC SUBRC_OK

    check_subrc_for( `CHANGE_WI_TEXT` ).

  ENDMETHOD.


  METHOD SHRIS5ZPAUXVKEPN5HWETLLAS34BTU~change_method.

    FIELD-SYMBOLS <ls_method_binding> TYPE hrs1214.

    mo_taskdef->change_method(
      EXPORTING
        new_method                   = ms_task-method    " New Method or Settings
      EXCEPTIONS
        no_changes_allowed           = 1
        problem_method_web_enabling  = 2
        problem_method_phon_enabling = 3
        OTHERS                       = 4 ).               "#EC SUBRC_OK

    check_subrc_for( `CHANGE_METHOD` ).

    LOOP AT ms_task-method_binding ASSIGNING <ls_method_binding>.

      mo_taskdef->change_method_binding(
        EXPORTING
          binding                       = <ls_method_binding>
          delete                        = abap_false
          insert                        = abap_true
        EXCEPTIONS
          no_changes_allowed            = 1
          desired_action_not_clear      = 2
          ts_cnt_element_does_not_exist = 3
          binding_could_not_be_deleted  = 4
          OTHERS                        = 5 ).            "#EC SUBRC_OK

      check_subrc_for( `CHANGE_METHOD_BINDING` ).

    ENDLOOP.

  ENDMETHOD.


  METHOD SHRIS5ZPAUXVKEPN5HWETLLAS34BTU~change_terminating_events.

    mo_taskdef->change_term_events_complete(
      EXPORTING
        terminating_events = ms_task-terminating_events
      EXCEPTIONS
        no_changes_allowed = 1
        OTHERS             = 2 ).                         "#EC SUBRC_OK

    check_subrc_for( `CHANGE_TERM_EVENTS_COMPLETE` ).

    mo_taskdef->change_term_evt_bind_complete(
      EXPORTING
        new_bindings       = ms_task-terminating_events_binding
      EXCEPTIONS
        no_changes_allowed = 1
        OTHERS             = 2 ).                         "#EC SUBRC_OK

    check_subrc_for( `CHANGE_TERM_EVT_BIND_COMPLETE` ).

  ENDMETHOD.


  METHOD SHRIS5ZPAUXVKEPN5HWETLLAS34BTU~change_text.

    mo_taskdef->change_text(
      EXPORTING
        subty              = c_subty_task_description
        new_text           = ms_task-descriptions
      EXCEPTIONS
        no_changes_allowed = 1
        OTHERS             = 2 ).                         "#EC SUBRC_OK

    check_subrc_for( `CHANGE_TEXT` ).

  ENDMETHOD.

ENDCLASS.

*"* test class
*include zcl_abapgit_object_pdts=======ccau.








class LCL_ABAPGIT_OBJECT_PDTS implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    ms_objkey-otype = 'TS'.
    ms_objkey-objid = ms_item-obj_name.

    mv_objid = ms_item-obj_name.  "Todo: Obsolete

  ENDMETHOD.
  METHOD extract_container.

    DATA li_stream TYPE REF TO if_ixml_ostream.
    DATA li_container_element TYPE REF TO if_ixml_element.
    DATA li_document TYPE REF TO if_ixml_document.

    li_document = io_xml->get_raw( ).

    li_container_element = li_document->find_from_name_ns( 'CONTAINER' ).

    IF li_container_element IS BOUND.

      li_document = cl_ixml=>create( )->create_document( ).

      li_stream = cl_ixml=>create( )->create_stream_factory( )->create_ostream_xstring( rv_result ).

      li_document->append_child( li_container_element ).

      cl_ixml=>create( )->create_renderer(
          document = li_document
          ostream  = li_stream
      )->render( ).

    ENDIF.

  ENDMETHOD.
  METHOD get_container_xml.

    DATA li_xml_dom TYPE REF TO if_ixml_document.
    DATA li_elements TYPE REF TO if_ixml_node_collection.
    DATA li_iterator TYPE REF TO if_ixml_node_iterator.
    DATA li_element TYPE REF TO if_ixml_node.
    DATA li_children TYPE REF TO if_ixml_node_list.
    DATA li_child_iterator TYPE REF TO if_ixml_node_iterator.
    DATA li_attributes TYPE REF TO if_ixml_named_node_map.
    DATA lv_name TYPE string.
    DATA li_container TYPE REF TO if_swf_cnt_container.

    "Todo: get_user_container strips out system elements, but to_xml adds them back in (hardcoded internally)
    "      Dirty hack further down to remove them from XML until we get this to work properly
    li_container = ii_task->get_user_container( ).
    li_container->to_xml(
      EXPORTING
        include_null_values        = abap_true
        include_initial_values     = abap_true
        include_typenames          = abap_true
        include_change_data        = abap_true
        include_texts              = abap_false  "Todo: Get texts to work properly #4164
        include_extension_elements = abap_true
        save_delta_handling_info   = abap_true
        use_xslt                   = abap_false
      IMPORTING
        xml_dom                    = li_xml_dom
      EXCEPTIONS
        conversion_error           = 1
        OTHERS                     = 2 ).                 "#EC SUBRC_OK

    check_subrc_for( `TO_XML` ).

    ri_first_element ?= li_xml_dom->get_first_child( ).
    li_elements = ri_first_element->get_elements_by_tag_name( 'ELEMENTS' ).
    li_iterator = li_elements->create_iterator( ).

    DO.
      li_element = li_iterator->get_next( ).

      IF li_element IS NOT BOUND.
        EXIT.
      ENDIF.

      li_children = li_element->get_children( ).
      li_child_iterator = li_children->create_iterator( ).

      DO.

        li_element = li_child_iterator->get_next( ).
        IF li_element IS NOT BOUND.
          EXIT.
        ENDIF.

        "Remove system container elements - causing too much trouble
        "Todo: This is a bad hack, but obsolete if we can fix todo above
        li_attributes = li_element->get_attributes( ).
        lv_name = li_attributes->get_named_item( 'NAME' )->get_value( ).
        IF lv_name(1) = '_'.
          li_element->remove_node( ).
          li_child_iterator->reset( ).
          CONTINUE.
        ENDIF.

        li_attributes->remove_named_item( 'CHGDTA' ).

      ENDDO.

    ENDDO.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_task       TYPE SHRIS5ZPAUXVKEPN5HWETLLAS34BTU=>ty_task_data,
          lv_xml_string TYPE xstring,
          li_task       TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLAS34BTU.

    io_xml->read( EXPORTING iv_name = 'PDTS'
      CHANGING cg_data = ls_task ).

    li_task = SHRIS5ZPAUXVKEPN5HWETLLAS37BTU=>create(
                      iv_objid     = mv_objid
                      is_task_data = ls_task ).

    li_task->create_task( ).
    li_task->change_wi_text( ).
    li_task->change_method( ).

    lv_xml_string = extract_container( io_xml ).
    li_task->import_container( lv_xml_string ).

    li_task->change_start_events( ).
    li_task->change_terminating_events( ).
    li_task->change_text( ).

    tadir_insert( iv_package ).

    li_task->save( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA li_task TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLAS34BTU.

    li_task = SHRIS5ZPAUXVKEPN5HWETLLAS37BTU=>load( mv_objid ).
    li_task->clear_origin_data( ).
    io_xml->add( iv_name = 'PDTS'
                 ig_data = li_task->get_definition( ) ).

    io_xml->add_xml( iv_name = 'CONTAINER'
                     ii_xml  = get_container_xml( li_task ) ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_PDTS implementation

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
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
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
endclass. "ZCL_ABAPGIT_OBJECT_PERS implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_PINF <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_pinf=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_pinf=======ccimp.
CLASS SHRIS5ZPAUXVKEPN5HWETLLAS4MBTU IMPLEMENTATION.

  METHOD constructor.

    mi_interface = ii_interface.

  ENDMETHOD.

  METHOD SHRIS5ZPAUXVKEPN5HWETLLAS4LBTU~get_elements.

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

  METHOD SHRIS5ZPAUXVKEPN5HWETLLAS4LBTU~set_elements_changeable.

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

  METHOD SHRIS5ZPAUXVKEPN5HWETLLAS4LBTU~save_elements.

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

  METHOD SHRIS5ZPAUXVKEPN5HWETLLAS4LBTU~get_all_attributes.

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

  METHOD SHRIS5ZPAUXVKEPN5HWETLLAS4LBTU~set_changeable.

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

  METHOD SHRIS5ZPAUXVKEPN5HWETLLAS4LBTU~delete.

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

  METHOD SHRIS5ZPAUXVKEPN5HWETLLAS4LBTU~save.

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

  METHOD SHRIS5ZPAUXVKEPN5HWETLLAS4LBTU~remove_elements.

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

  METHOD SHRIS5ZPAUXVKEPN5HWETLLAS4LBTU~add_elements.

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

  METHOD SHRIS5ZPAUXVKEPN5HWETLLAS4LBTU~set_all_attributes.

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

  METHOD SHRIS5ZPAUXVKEPN5HWETLLAS4LBTU~get_changeable.

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

    CREATE OBJECT ri_facade TYPE SHRIS5ZPAUXVKEPN5HWETLLAS4MBTU
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

    DATA: li_interface TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLAS4LBTU.

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

    DATA: li_interface TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLAS4LBTU,
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
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'EEUDB'
                                            iv_argument    = ms_item-obj_name
                                            iv_prefix      = 'PF' ).
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

    DATA: ls_pinf      TYPE ty_pinf,
          lt_elements  TYPE ty_elements,
          li_interface TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLAS4LBTU.

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
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
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
endclass. "ZCL_ABAPGIT_OBJECT_PROG implementation

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
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
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
endclass. "ZCL_ABAPGIT_OBJECT_SCP1 implementation

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
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
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
endclass. "ZCL_ABAPGIT_OBJECT_SCVI implementation

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
  METHOD Lif_abapgit_object~get_deserialize_order.
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
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
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
  METHOD Lif_abapgit_object~get_deserialize_order.
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
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
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
endclass. "ZCL_ABAPGIT_OBJECT_SFBS implementation

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
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
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
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_xstr     TYPE xstring,
          li_document TYPE REF TO if_ixml_document.


    lv_xstr = interface_to_xstring( ).
    li_document = cl_ixml_80_20=>parse_to_document( stream_xstring = lv_xstr ).
    Lcl_abapgit_object_sfpf=>fix_oref( li_document ).
    io_xml->set_raw( li_document->get_root_element( ) ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SFPI implementation

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
  METHOD Lif_abapgit_object~get_deserialize_order.
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
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
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
endclass. "ZCL_ABAPGIT_OBJECT_SFSW implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SHI3 <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_shi3=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_shi3=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SHI3 implementation.
*"* method's implementations
*include methods.
  METHOD clear_fields.

    FIELD-SYMBOLS <ls_node> LIKE LINE OF ct_nodes.

    CLEAR: cs_head-luser, cs_head-ldate, cs_head-ltime.
    CLEAR: cs_head-fuser, cs_head-fdate, cs_head-ftime.
    CLEAR: cs_head-frelease, cs_head-lrelease.
    CLEAR: cs_head-responsibl.

    LOOP AT ct_nodes ASSIGNING <ls_node>.
      CLEAR: <ls_node>-luser, <ls_node>-ldate, <ls_node>-ltime.
      CLEAR: <ls_node>-fuser, <ls_node>-fdate, <ls_node>-ftime.
      CLEAR: <ls_node>-frelease, <ls_node>-lrelease.
    ENDLOOP.

  ENDMETHOD.
  METHOD constructor.
    super->constructor( is_item = is_item
                        iv_language = iv_language ).
    mv_tree_id = ms_item-obj_name.
  ENDMETHOD.
  METHOD delete_tree_structure.
    CALL FUNCTION 'STREE_EXTERNAL_DELETE'
      EXPORTING
        structure_id          = iv_structure_id
        no_confirmation_popup = abap_true.
  ENDMETHOD.
  METHOD has_authorization.

    AUTHORITY-CHECK OBJECT 'S_DEVELOP'
      ID 'DEVCLASS'  FIELD iv_devclass
      ID 'OBJTYPE'   FIELD 'MENU'
      ID 'OBJNAME'   FIELD iv_structure_id
      ID 'P_GROUP'   DUMMY
      ID 'ACTVT'     FIELD iv_activity.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( iv_msgid = 'S#'
                                         iv_msgno = '203' ).
    ENDIF.
  ENDMETHOD.
  METHOD insert_transport.

    DATA:
      ls_msg     TYPE hier_mess,
      ls_object  TYPE e071,
      lt_objects TYPE TABLE OF e071,
      lt_keys    TYPE TABLE OF e071k,
      ls_ko200   TYPE ko200,
      lt_ko200   TYPE TABLE OF ko200.

    " This function shows a popup so get objects and keys and insert
    " them into transport below
    CALL FUNCTION 'STREE_INSERT_ALL_IN_TRANSPORT'
      EXPORTING
        structure_id               = mv_tree_id
        iv_return_objects_and_keys = abap_true
      IMPORTING
        message                    = ls_msg
      TABLES
        et_objects                 = lt_objects
        et_keys                    = lt_keys.
    IF ls_msg-msgty = 'E'.
      MESSAGE ID ls_msg-msgid TYPE ls_msg-msgty NUMBER ls_msg-msgno
        WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4 INTO Lcx_abapgit_exception=>null.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    LOOP AT lt_objects INTO ls_object.
      MOVE-CORRESPONDING ls_object TO ls_ko200.
      INSERT ls_ko200 INTO TABLE lt_ko200.
    ENDLOOP.

    CALL FUNCTION 'TR_RECORD_OBJ_CHANGE_TO_REQ'
      EXPORTING
        iv_request = iv_transport
        it_objects = lt_ko200
        it_keys    = lt_keys
      EXCEPTIONS
        cancel     = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD is_used.

    DATA: lt_used_in_structures TYPE STANDARD TABLE OF ttree WITH DEFAULT KEY.

    CALL FUNCTION 'STREE_GET_STRUCTURE_USAGE'
      EXPORTING
        structure_id       = iv_structure_id
      TABLES
        used_in_structures = lt_used_in_structures.

    IF lt_used_in_structures IS NOT INITIAL.
      Lcx_abapgit_exception=>raise( |IMG structure ID { iv_structure_id } is still used| ).
    ENDIF.

  ENDMETHOD.
  METHOD jump_sbach04.
    DATA: ls_message      TYPE hier_mess,
          lv_structure_id TYPE hier_treeg.

    lv_structure_id = ms_item-obj_name.

    CALL FUNCTION 'STREE_EXTERNAL_EDIT'
      EXPORTING
        structure_id   = lv_structure_id
        language       = mv_language
        edit_structure = abap_false
        no_commit_work = abap_false
        activity       = 'D'
      IMPORTING
        message        = ls_message.
    IF ls_message IS NOT INITIAL.
      Lcx_abapgit_exception=>raise_t100(
        iv_msgid = ls_message-msgid
        iv_msgno = ls_message-msgno
        iv_msgv1 = ls_message-msgv1
        iv_msgv2 = ls_message-msgv2
        iv_msgv3 = ls_message-msgv3
        iv_msgv4 = ls_message-msgv4 ).
    ENDIF.
  ENDMETHOD.
  METHOD jump_se43.

    DATA: lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPLBMEN'.
    <ls_bdcdata>-dynpro   = '0200'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=SHOW'.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BMENUNAME-ID'.
    <ls_bdcdata>-fval = ms_item-obj_name.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SE43'
      it_bdcdata = lt_bdcdata ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: ls_head TYPE ttree.

    CALL FUNCTION 'STREE_STRUCTURE_READ'
      EXPORTING
        structure_id     = mv_tree_id
      IMPORTING
        structure_header = ls_head.

    rv_user = ls_head-luser.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    CONSTANTS lc_activity_delete_06 TYPE activ_auth VALUE '06'.

    TRY.
        IF Lif_abapgit_object~exists( ) = abap_false.
          RETURN.
        ENDIF.
      CATCH Lcx_abapgit_exception.
        RETURN.
    ENDTRY.

    has_authorization( iv_structure_id = mv_tree_id
                       iv_devclass     = ms_item-devclass
                       iv_activity     = lc_activity_delete_06 ).

    is_used( mv_tree_id ).

    delete_tree_structure( mv_tree_id ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_msg    TYPE hier_mess,
          ls_head   TYPE ttree,
          ls_ttree  TYPE ttree,
          lt_titles TYPE TABLE OF ttreet,
          lt_nodes  TYPE TABLE OF hier_iface,
          lt_texts  TYPE TABLE OF hier_texts,
          lt_refs   TYPE TABLE OF hier_ref.

    io_xml->read( EXPORTING iv_name = 'TREE_HEAD'
                  CHANGING  cg_data = ls_head ).
    io_xml->read( EXPORTING iv_name = 'TREE_TITLES'
                  CHANGING  cg_data = lt_titles ).
    io_xml->read( EXPORTING iv_name = 'TREE_NODES'
                  CHANGING  cg_data = lt_nodes ).
    io_xml->read( EXPORTING iv_name = 'TREE_REFS'
                  CHANGING  cg_data = lt_refs ).
    io_xml->read( EXPORTING iv_name = 'TREE_TEXTS'
                  CHANGING  cg_data = lt_texts ).

    mo_i18n_params->trim_saplang_keyed_table(
      EXPORTING
        iv_lang_field_name = 'SPRAS'
        iv_keep_master_lang = abap_true
      CHANGING
        ct_tab = lt_titles ).
    mo_i18n_params->trim_saplang_keyed_table(
      EXPORTING
        iv_lang_field_name = 'SPRAS'
        iv_keep_master_lang = abap_true
      CHANGING
        ct_tab = lt_texts ).

    IF Lif_abapgit_object~exists( ) = abap_true.
      delete_tree_structure( mv_tree_id ).
    ENDIF.

    CALL FUNCTION 'STREE_HIERARCHY_SAVE'
      EXPORTING
        structure_id             = mv_tree_id
        structure_type           = ls_head-type
        structure_description    = space
        structure_masterlanguage = mv_language
        structure_responsible    = sy-uname
        structure_buffermode     = ls_head-buffermode
        development_class        = iv_package
      IMPORTING
        message                  = ls_msg
      TABLES
        list_of_nodes            = lt_nodes
        list_of_references       = lt_refs
        list_of_texts            = lt_texts
        structure_descriptions   = lt_titles
      EXCEPTIONS
        no_nodes_given           = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ELSEIF ls_msg-msgty = 'E'.
      MESSAGE ID ls_msg-msgid TYPE ls_msg-msgty NUMBER ls_msg-msgno
        WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4 INTO Lcx_abapgit_exception=>null.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    " Set buffer mode for menus (see function BMENU_CREATE_TREE)
    SELECT SINGLE * FROM ttree INTO ls_ttree
      WHERE type = 'BMENU' AND id = mv_tree_id.
    IF sy-subrc = 0.
      ls_ttree-buffermode = ls_head-buffermode.
      ls_ttree-buffervar  = ls_head-buffervar.
      MODIFY ttree FROM ls_ttree.
    ENDIF.

    IF Lcl_abapgit_factory=>get_sap_package( iv_package )->are_changes_recorded_in_tr_req( ) = abap_true.
      " Add necessary SHI6, SHI7, and TABU entries to transport (SAP Note 455542)
      insert_transport( iv_transport ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: ls_msg    TYPE hier_mess,
          ls_header TYPE ttree,
          ls_tadir  TYPE tadir.

    " Ignore buffer and get state from DB
    CALL FUNCTION 'STREE_STRUCTURE_EXIST'
      EXPORTING
        structure_id         = mv_tree_id
        read_from_database   = abap_true
        do_not_read_devclass = abap_false
      IMPORTING
        message              = ls_msg
        structure_header     = ls_header
        structure_tadir      = ls_tadir.

    rv_bool = boolc( ls_header-id IS NOT INITIAL ).

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

    DATA: ls_head TYPE ttree.

    CALL FUNCTION 'STREE_STRUCTURE_READ'
      EXPORTING
        structure_id     = mv_tree_id
      IMPORTING
        structure_header = ls_head.

    CASE ls_head-type.
      WHEN 'BMENU'.
        jump_se43( ).
        rv_exit = abap_true.
      WHEN 'GHIER'.
        jump_sbach04( ).
        rv_exit = abap_true.
    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_msg           TYPE hier_mess,
          ls_head          TYPE ttree,
          lt_titles        TYPE TABLE OF ttreet,
          lt_nodes         TYPE TABLE OF hier_iface,
          lt_texts         TYPE TABLE OF hier_texts,
          lt_refs          TYPE TABLE OF hier_ref,
          lv_all_languages TYPE abap_bool.


    CALL FUNCTION 'STREE_STRUCTURE_READ'
      EXPORTING
        structure_id     = mv_tree_id
      IMPORTING
        message          = ls_msg
        structure_header = ls_head
      TABLES
        description      = lt_titles.

    IF mo_i18n_params->ms_params-main_language_only = abap_true OR mo_i18n_params->is_lxe_applicable( ) = abap_true.
      lv_all_languages = abap_false.
      DELETE lt_titles WHERE spras <> mv_language.
    ELSE.
      lv_all_languages = abap_true.
      mo_i18n_params->trim_saplang_keyed_table(
        EXPORTING
            iv_lang_field_name = 'SPRAS'
            iv_keep_master_lang = abap_true
          CHANGING
            ct_tab = lt_titles ).
    ENDIF.

    CALL FUNCTION 'STREE_HIERARCHY_READ'
      EXPORTING
        structure_id       = mv_tree_id
        read_also_texts    = abap_true
        all_languages      = lv_all_languages
        language           = mv_language
      IMPORTING
        message            = ls_msg
      TABLES
        list_of_nodes      = lt_nodes
        list_of_references = lt_refs
        list_of_texts      = lt_texts.

    clear_fields( CHANGING cs_head  = ls_head
                           ct_nodes = lt_nodes ).

    SORT lt_titles BY id.
    DELETE ADJACENT DUPLICATES FROM lt_titles COMPARING spras id.

    SORT lt_texts BY spras.
    DELETE ADJACENT DUPLICATES FROM lt_texts COMPARING spras node_id.

    mo_i18n_params->trim_saplang_keyed_table(
      EXPORTING
        iv_lang_field_name = 'SPRAS'
        iv_keep_master_lang = abap_true
      CHANGING
        ct_tab = lt_texts ).

    io_xml->add( iv_name = 'TREE_HEAD'
                 ig_data = ls_head ).
    io_xml->add( iv_name = 'TREE_TITLES'
                 ig_data = lt_titles ).
    io_xml->add( iv_name = 'TREE_NODES'
                 ig_data = lt_nodes ).
    io_xml->add( iv_name = 'TREE_REFS'
                 ig_data = lt_refs ).
    io_xml->add( iv_name = 'TREE_TEXTS'
                 ig_data = lt_texts ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SHI3 implementation

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
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
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
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
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
  METHOD Lif_abapgit_object~get_deserialize_order.
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
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
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
endclass. "ZCL_ABAPGIT_OBJECT_SHLP implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SHMA <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_shma=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_shma=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SHMA implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE chg_user
      FROM shma_attributes
      INTO rv_user
      WHERE area_name = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    " We can't use FM SHMA_DELETE_AREA because it depends
    " on the corresponding class, but in abapGit it has its own
    " lifecycle. Therefore we have to reimplement most of the
    " FMs logic

    CONSTANTS: lc_request_delete TYPE i VALUE 4.

    DATA: lv_request   TYPE i,
          lv_area_name TYPE shm_area_name,
          lv_order     TYPE e070-trkorr,
          lv_task      TYPE e070-trkorr,
          lv_append    TYPE abap_bool,
          ls_tdevc     TYPE tdevc,
          lo_cts_if    TYPE REF TO object.

    lv_area_name = ms_item-obj_name.

    TRY.
        CALL FUNCTION 'ENQUEUE_E_SHM_AREA'
          EXPORTING
            mode_shma_attributes = 'E'
            area_name            = lv_area_name
            x_area_name          = ' '
            _scope               = '2'
            _wait                = ' '
            _collect             = ' '
          EXCEPTIONS
            foreign_lock         = 1
            system_failure       = 2
            OTHERS               = 3.

        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise_t100( ).
        ENDIF.

        CALL METHOD ('\PROGRAM=SAPMSHM_MONITOR\CLASS=LCL_SHMM')=>('FREE_AREA_BY_NAME')
          EXPORTING
            area_name     = lv_area_name
            affect_server = cl_shm_area=>affect_all_servers.

        CREATE OBJECT lo_cts_if TYPE ('\FUNCTION-POOL=SHMA\CLASS=LCL_CTS_INTERFACE')
          EXPORTING
            area = lv_area_name.

        CALL METHOD lo_cts_if->('CHECK_AREA')
          EXPORTING
            request     = lc_request_delete
          IMPORTING
            access_mode = lv_request
            appendable  = lv_append.

        IF lv_request <> lc_request_delete.
          Lcx_abapgit_exception=>raise( |Error deleting SHMA { ms_item-obj_name }| ).
        ENDIF.

        CALL METHOD lo_cts_if->('INSERT_AREA')
          EXPORTING
            request = lc_request_delete
          IMPORTING
            order   = lv_order
            task    = lv_task.

        DELETE FROM shma_attributes  WHERE area_name = lv_area_name.
        DELETE FROM shma_start       WHERE area_name = lv_area_name.

        CALL FUNCTION 'TR_DEVCLASS_GET'
          EXPORTING
            iv_devclass = iv_package
          IMPORTING
            es_tdevc    = ls_tdevc
          EXCEPTIONS
            OTHERS      = 1.

        IF sy-subrc = 0 AND ls_tdevc-korrflag IS INITIAL.

          " TADIR entries for local objects must be deleted 'by hand'
          tadir_delete( ).

        ENDIF.

        CALL METHOD ('\PROGRAM=SAPLSHMA\CLASS=LCL_SHMA_HELPER')=>('DELETE_RUNTIME_SETTINGS')
          EXPORTING
            area_name = lv_area_name.

        CALL FUNCTION 'DEQUEUE_E_SHM_AREA'
          EXPORTING
            mode_shma_attributes = 'E'
            area_name            = lv_area_name
            x_area_name          = ' '
            _scope               = '3'
            _synchron            = ' '
            _collect             = ' '.

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |Error deleting SHMA { ms_item-obj_name }| ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_area_name       TYPE shm_area_name,
          ls_area_attributes TYPE shma_attributes.

    lv_area_name = ms_item-obj_name.

    io_xml->read(
      EXPORTING
        iv_name = 'AREA_ATTRIBUTES'
      CHANGING
        cg_data = ls_area_attributes ).

    tadir_insert( iv_package ).

    TRY.
        CALL METHOD ('\PROGRAM=SAPLSHMA\CLASS=LCL_SHMA_HELPER')=>('INSERT_AREA')
          EXPORTING
            area_name           = lv_area_name
            attributes          = ls_area_attributes
            force_overwrite     = abap_true
            no_class_generation = abap_false
            silent_mode         = abap_true.

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |Error serializing SHMA { ms_item-obj_name }| ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_area_name TYPE shm_area_name.

    SELECT SINGLE area_name
           FROM shma_attributes
           INTO lv_area_name
           WHERE area_name = ms_item-obj_name.

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
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: ls_bcdata TYPE bdcdata,
          lt_bcdata TYPE STANDARD TABLE OF bdcdata.

    ls_bcdata-program  = 'SAPLSHMA'.
    ls_bcdata-dynpro   = '0100'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam = 'SHMA_ATTRIBUTES-AREA_NAME'.
    ls_bcdata-fval = ms_item-obj_name.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=SHOW'.
    APPEND ls_bcdata TO lt_bcdata.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SHMA'
      it_bdcdata = lt_bcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_area_name       TYPE shm_area_name,
          ls_area_attributes TYPE shma_attributes.

    lv_area_name = ms_item-obj_name.

    TRY.
        CALL METHOD ('\PROGRAM=SAPLSHMA\CLASS=LCL_SHMA_HELPER')=>('READ_AREA_ATTRIBUTES_ALL')
          EXPORTING
            area_name       = lv_area_name
          IMPORTING
            area_attributes = ls_area_attributes.

        CLEAR: ls_area_attributes-chg_user,
               ls_area_attributes-chg_date,
               ls_area_attributes-chg_time,
               ls_area_attributes-cls_gen_user,
               ls_area_attributes-cls_gen_date,
               ls_area_attributes-cls_gen_time.

        io_xml->add( iv_name = 'AREA_ATTRIBUTES'
                     ig_data = ls_area_attributes ).

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |Error serializing SHMA { ms_item-obj_name }| ).
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SHMA implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SICF <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sicf=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sicf=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SICF implementation.
*"* method's implementations
*include methods.
  METHOD change_sicf.

    DATA: lt_icfhndlist TYPE icfhndlist,
          lt_existing   TYPE TABLE OF icfhandler,
          ls_icfserdesc TYPE icfserdesc.

    FIELD-SYMBOLS: <ls_existing> LIKE LINE OF lt_existing.


    lt_icfhndlist = to_icfhndlist( it_icfhandler ).

* Do not add handlers if they already exist, it will make the below
* call to SAP standard code raise an exception
    SELECT * FROM icfhandler INTO TABLE lt_existing
      WHERE icf_name = is_icfservice-icf_name.
    LOOP AT lt_existing ASSIGNING <ls_existing>.
      DELETE TABLE lt_icfhndlist FROM <ls_existing>-icfhandler.
    ENDLOOP.

    MOVE-CORRESPONDING is_icfservice TO ls_icfserdesc.

    cl_icf_tree=>if_icf_tree~change_node(
      EXPORTING
        icf_name                  = is_icfservice-orig_name
        icfparguid                = iv_parent
        icfdocu                   = is_icfdocu
        doculang                  = mv_language
        icfhandlst                = lt_icfhndlist
        package                   = iv_package
        application               = space
        icfserdesc                = ls_icfserdesc
        icfactive                 = abap_true
      EXCEPTIONS
        empty_icf_name            = 1
        no_new_virtual_host       = 2
        special_service_error     = 3
        parent_not_existing       = 4
        enqueue_error             = 5
        node_already_existing     = 6
        empty_docu                = 7
        doculang_not_installed    = 8
        security_info_error       = 9
        user_password_error       = 10
        password_encryption_error = 11
        invalid_url               = 12
        invalid_otr_concept       = 13
        formflg401_error          = 14
        handler_error             = 15
        transport_error           = 16
        tadir_error               = 17
        package_not_found         = 18
        wrong_application         = 19
        not_allow_application     = 20
        no_application            = 21
        invalid_icfparguid        = 22
        alt_name_invalid          = 23
        alternate_name_exist      = 24
        wrong_icf_name            = 25
        no_authority              = 26
        OTHERS                    = 27 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD deserialize_otr.

    DATA:
      lt_sots     TYPE Lcl_abapgit_sots_handler=>ty_sots_tt,
      lt_sots_use TYPE Lcl_abapgit_sots_handler=>ty_sots_use_tt.

    FIELD-SYMBOLS:
      <ls_sots_use> LIKE LINE OF lt_sots_use.

    io_xml->read( EXPORTING iv_name = 'SOTS'
                  CHANGING cg_data = lt_sots ).
    io_xml->read( EXPORTING iv_name = 'SOTS_USE'
                  CHANGING cg_data = lt_sots_use ).

    LOOP AT lt_sots_use ASSIGNING <ls_sots_use>.
      <ls_sots_use>-obj_name = ms_item-obj_name.
    ENDLOOP.

    Lcl_abapgit_sots_handler=>create_sots_from_data(
      iv_package  = iv_package
      it_sots     = lt_sots
      it_sots_use = lt_sots_use ).

  ENDMETHOD.
  METHOD find_parent.

    cl_icf_tree=>if_icf_tree~service_from_url(
      EXPORTING
        url                   = iv_url
        hostnumber            = 0
      IMPORTING
        icfnodguid            = rv_parent
      EXCEPTIONS
        wrong_application     = 1
        no_application        = 2
        not_allow_application = 3
        wrong_url             = 4
        no_authority          = 5
        OTHERS                = 6 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD get_hash_from_object.

    DATA:
      lv_icfnodguid TYPE icfservice-icfnodguid,
      lv_url        TYPE icfurlbuf,
      lv_ext_url    TYPE string.

    SELECT SINGLE icfnodguid FROM icfservice INTO lv_icfnodguid
      WHERE icf_name = iv_obj_name(15)
      AND icfparguid = iv_obj_name+15.

    IF sy-subrc = 0.
      CALL FUNCTION 'HTTP_GET_URL_FROM_NODGUID'
        EXPORTING
          nodguid      = lv_icfnodguid
        IMPORTING
          url          = lv_url
          extended_url = lv_ext_url
        EXCEPTIONS
          icf_inconst  = 1
          OTHERS       = 2.
      IF sy-subrc = 0.
        " It's possible that the URL contains the system id, for example for WD applications with names
        " longer than 15 characters. In that case, use the extended URL to generate the hash (#5064)
        IF lv_ext_url <> lv_url.
          rv_hash = Lcl_abapgit_hash=>sha1_raw( Lcl_abapgit_convert=>string_to_xstring_utf8( |{ lv_ext_url }| ) ).
        ELSE.
          rv_hash = Lcl_abapgit_hash=>sha1_raw( Lcl_abapgit_convert=>string_to_xstring_utf8( |{ lv_url }| ) ).
        ENDIF.
      ENDIF.
    ELSE.
      rv_hash = to_lower( iv_obj_name+15 ).
    ENDIF.

  ENDMETHOD.
  METHOD insert_sicf.

    DATA: lt_icfhndlist TYPE icfhndlist,
          ls_icfserdesc TYPE icfserdesc,
          ls_icfdocu    TYPE icfdocu,
          lv_icfnodguid TYPE icfnodguid,
          lv_parent     TYPE icfparguid.


    lt_icfhndlist = to_icfhndlist( it_icfhandler ).
    lv_parent = find_parent( iv_url ).

* nice, it seems that the structure should be mistreated
    ls_icfdocu = is_icfdocu-icf_docu.

    MOVE-CORRESPONDING is_icfservice TO ls_icfserdesc.

    cl_icf_tree=>if_icf_tree~insert_node(
      EXPORTING
        icf_name                  = is_icfservice-orig_name
        icfparguid                = lv_parent
        icfdocu                   = ls_icfdocu
        doculang                  = mv_language
        icfhandlst                = lt_icfhndlist
        package                   = iv_package
        application               = space
        icfserdesc                = ls_icfserdesc
        icfactive                 = abap_true
        icfaltnme                 = is_icfservice-icfaltnme
      IMPORTING
        icfnodguid                = lv_icfnodguid
      EXCEPTIONS
        empty_icf_name            = 1
        no_new_virtual_host       = 2
        special_service_error     = 3
        parent_not_existing       = 4
        enqueue_error             = 5
        node_already_existing     = 6
        empty_docu                = 7
        doculang_not_installed    = 8
        security_info_error       = 9
        user_password_error       = 10
        password_encryption_error = 11
        invalid_url               = 12
        invalid_otr_concept       = 13
        formflg401_error          = 14
        handler_error             = 15
        transport_error           = 16
        tadir_error               = 17
        package_not_found         = 18
        wrong_application         = 19
        not_allow_application     = 20
        no_application            = 21
        invalid_icfparguid        = 22
        alt_name_invalid          = 23
        alternate_name_exist      = 24
        wrong_icf_name            = 25
        no_authority              = 26
        OTHERS                    = 27 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    " Update item with name assigned by system
    SELECT SINGLE icfparguid INTO ms_item-obj_name+15 FROM icfservice
      WHERE icfnodguid = lv_icfnodguid.

  ENDMETHOD.
  METHOD read.

    DATA: lt_serv_info TYPE icfservtbl,
          ls_serv_info LIKE LINE OF lt_serv_info,
          ls_key       TYPE ty_sicf_key.

    FIELD-SYMBOLS: <ls_icfhandler> LIKE LINE OF et_icfhandler.


    CLEAR es_icfservice.
    CLEAR es_icfdocu.
    CLEAR et_icfhandler.
    CLEAR ev_url.

    ls_key-icf_name   = ms_item-obj_name(15).
    ls_key-icfparguid = ms_item-obj_name+15.

    cl_icf_tree=>if_icf_tree~get_info_from_serv(
      EXPORTING
        icf_name          = ls_key-icf_name
        icfparguid        = ls_key-icfparguid
        icf_langu         = mv_language
      IMPORTING
        serv_info         = lt_serv_info
        icfdocu           = es_icfdocu
        url               = ev_url
      EXCEPTIONS
        wrong_name        = 1
        wrong_parguid     = 2
        incorrect_service = 3
        no_authority      = 4
        OTHERS            = 5 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    ASSERT lines( lt_serv_info ) = 1.
    READ TABLE lt_serv_info INDEX 1 INTO ls_serv_info.
    ASSERT sy-subrc = 0.

    MOVE-CORRESPONDING ls_serv_info-service TO es_icfservice.
    IF iv_clear = abap_true.
      CLEAR es_icfservice-icf_cuser.
      CLEAR es_icfservice-icf_cdate.
      CLEAR es_icfservice-icf_muser.
      CLEAR es_icfservice-icf_mdate.
    ENDIF.

    CLEAR es_icfdocu-icfparguid.

    APPEND LINES OF ls_serv_info-handlertbl TO et_icfhandler.
    LOOP AT et_icfhandler ASSIGNING <ls_icfhandler>.
      CLEAR <ls_icfhandler>-icfparguid.
    ENDLOOP.

  ENDMETHOD.
  METHOD serialize_otr.

    DATA:
      lt_sots     TYPE Lcl_abapgit_sots_handler=>ty_sots_tt,
      lt_sots_use TYPE Lcl_abapgit_sots_handler=>ty_sots_use_tt.

    FIELD-SYMBOLS:
      <ls_sots_use> LIKE LINE OF lt_sots_use.

    Lcl_abapgit_sots_handler=>read_sots(
      EXPORTING
        iv_object   = ms_item-obj_type
        iv_obj_name = ms_item-obj_name
        io_i18n_params = mo_i18n_params
      IMPORTING
        et_sots     = lt_sots
        et_sots_use = lt_sots_use ).

    LOOP AT lt_sots_use ASSIGNING <ls_sots_use>.
      CLEAR <ls_sots_use>-obj_name.
    ENDLOOP.

    io_xml->add( iv_name = 'SOTS'
                 ig_data = lt_sots ).
    io_xml->add( iv_name = 'SOTS_USE'
                 ig_data = lt_sots_use ).

  ENDMETHOD.
  METHOD to_icfhndlist.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF it_list.


    " Convert to sorted table
    LOOP AT it_list ASSIGNING <ls_list>.
      INSERT <ls_list>-icfhandler INTO TABLE rt_list.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: ls_icfservice TYPE icfservice.


    read( EXPORTING iv_clear = abap_false
          IMPORTING es_icfservice = ls_icfservice ).

    rv_user = ls_icfservice-icf_muser.

    IF rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA ls_icfservice TYPE icfservice.

    read( IMPORTING es_icfservice = ls_icfservice ).

    IF ls_icfservice IS INITIAL.
      " It seems that the ICF service doesn't exist anymore.
      " But that's ok, because some objects like SAPC manage
      " the lifecycle of its ICF service by itself and already
      " deleted the service.
      RETURN.
    ENDIF.

    IF ls_icfservice-icfparguid CO '0'.
      " not supported by the SAP standard API
      Lcx_abapgit_exception=>raise( 'SICF - cannot delete root node, delete node manually' ).
    ENDIF.

    " OTR long texts
    Lcl_abapgit_sots_handler=>delete_sots(
      iv_object   = ms_item-obj_type
      iv_obj_name = ms_item-obj_name ).

    " Delete Application Customizing Data the hard way, as it isn't done by the API.
    " If we wouldn't we would get errors from the API if entrys exist.
    " Transaction SICF does the same.
    DELETE FROM icfapplcust
      WHERE icf_name = ls_icfservice-icf_name
      AND icfparguid = ls_icfservice-icfparguid.

    cl_icf_tree=>if_icf_tree~delete_node(
      EXPORTING
        icfparguid                  = ls_icfservice-icfparguid
      CHANGING
        icf_name                    = ls_icfservice-icf_name
      EXCEPTIONS
        no_virtual_host_delete      = 1
        special_service_error       = 2
        enqueue_error               = 3
        node_not_existing           = 4
        node_has_childs             = 5
        node_is_aliased             = 6
        node_not_in_original_system = 7
        transport_error             = 8
        tadir_error                 = 9
        db_error                    = 10
        no_authority                = 11
        OTHERS                      = 12 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_icfservice TYPE icfservice,
          ls_read       TYPE icfservice,
          ls_icfdocu    TYPE icfdocu,
          lv_url        TYPE string,
          lv_exists     TYPE abap_bool,
          lt_icfhandler TYPE TABLE OF icfhandler.

    io_xml->read( EXPORTING iv_name = 'URL'
                  CHANGING cg_data = lv_url ).
    io_xml->read( EXPORTING iv_name = 'ICFSERVICE'
                  CHANGING cg_data = ls_icfservice ).
    io_xml->read( EXPORTING iv_name = 'ICFDOCU'
                  CHANGING cg_data = ls_icfdocu ).
    io_xml->read( EXPORTING iv_name = 'ICFHANDLER_TABLE'
                  CHANGING cg_data = lt_icfhandler ).

    lv_exists = Lif_abapgit_object~exists( ).
    IF lv_exists = abap_false.
      insert_sicf( is_icfservice = ls_icfservice
                   is_icfdocu    = ls_icfdocu
                   it_icfhandler = lt_icfhandler
                   iv_package    = iv_package
                   iv_url        = lv_url ).
    ELSE.
      read( IMPORTING es_icfservice = ls_read ).
      change_sicf( is_icfservice = ls_icfservice
                   is_icfdocu    = ls_icfdocu
                   it_icfhandler = lt_icfhandler
                   iv_package    = iv_package
                   iv_parent     = ls_read-icfparguid ).
    ENDIF.

    " OTR long texts
    deserialize_otr(
      iv_package = iv_package
      io_xml     = io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA ls_key TYPE ty_sicf_key.

    SELECT SINGLE icfaltnme FROM icfservice INTO ls_key-icf_name
      WHERE icf_name = ms_item-obj_name(15)
      AND icfparguid = ms_item-obj_name+15.
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
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    DATA: lv_argument TYPE seqg3-garg.

    lv_argument = ms_item-obj_name(15).
    lv_argument+15(1) = '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESICFSER'
                                            iv_argument    = lv_argument ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: ls_bcdata TYPE bdcdata,
          lt_bcdata TYPE STANDARD TABLE OF bdcdata.

    ls_bcdata-program  = 'RSICFTREE'.
    ls_bcdata-dynpro   = '1000'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    ls_bcdata-dynpro   = space.
    ls_bcdata-dynbegin = space.
    ls_bcdata-fnam     = 'ICF_SERV'.
    ls_bcdata-fval     = ms_item-obj_name.
    APPEND ls_bcdata TO lt_bcdata.

    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=ONLI'.
    APPEND ls_bcdata TO lt_bcdata.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SICF'
      it_bdcdata = lt_bcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.

    DATA:
      lt_tadir    TYPE Lif_abapgit_definitions=>ty_tadir_tt,
      lv_hash     TYPE ty_hash,
      lv_obj_name TYPE tadir-obj_name.

    FIELD-SYMBOLS <ls_tadir> LIKE LINE OF lt_tadir.

    lv_obj_name = to_upper( iv_filename(15) ) && '%'.
    lv_hash     = iv_filename+15(25).

    SELECT * FROM tadir INTO CORRESPONDING FIELDS OF TABLE lt_tadir
      WHERE pgmid = 'R3TR'
      AND object  = 'SICF'
      AND obj_name LIKE lv_obj_name
      ORDER BY PRIMARY KEY ##TOO_MANY_ITAB_FIELDS.      "#EC CI_GENBUFF

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      IF get_hash_from_object( <ls_tadir>-obj_name ) = lv_hash.
        cs_item-obj_name = <ls_tadir>-obj_name.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.

    DATA:
      lv_rest     TYPE string,
      lv_old_name TYPE string.

    SPLIT cv_filename AT '.' INTO lv_old_name lv_rest.
    cv_filename = |{ cv_filename(15) }{ get_hash_from_object( is_item-obj_name ) }.{ lv_rest }|.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_icfservice TYPE icfservice,
          ls_icfdocu    TYPE icfdocu,
          lv_url        TYPE string,
          lt_icfhandler TYPE TABLE OF icfhandler.

    read( IMPORTING es_icfservice = ls_icfservice
                    es_icfdocu    = ls_icfdocu
                    et_icfhandler = lt_icfhandler
                    ev_url        = lv_url ).

    IF ls_icfservice IS INITIAL.
      RETURN.
    ENDIF.

    CLEAR ls_icfservice-icf_mandt.
    CLEAR ls_icfservice-icfnodguid.
    CLEAR ls_icfservice-icfparguid.
    CLEAR ls_icfservice-icfchildno.
    CLEAR ls_icfservice-icfaliasno.
    CLEAR ls_icfservice-icf_user.
    CLEAR ls_icfservice-icf_cclnt.
    CLEAR ls_icfservice-icf_mclnt.
    CLEAR ls_icfservice-icfaltnme_orig.
    CLEAR ls_icfservice-icfbitmap.

    io_xml->add( iv_name = 'URL'
                 ig_data = lv_url ).
    io_xml->add( iv_name = 'ICFSERVICE'
                 ig_data = ls_icfservice ).
    io_xml->add( iv_name = 'ICFDOCU'
                 ig_data = ls_icfdocu ).
    io_xml->add( iv_name = 'ICFHANDLER_TABLE'
                 ig_data = lt_icfhandler ).

    " OTR long texts
    serialize_otr( io_xml ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SICF implementation

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
endclass. "ZCL_ABAPGIT_OBJECT_SMIM implementation

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
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'E_SMTG'
                                            iv_argument    = |{ mv_template_id }| ).
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
endclass. "ZCL_ABAPGIT_OBJECT_SMTG implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SOTS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sots=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sots=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SOTS implementation.
*"* method's implementations
*include methods.
  METHOD create_sots.

    " Reimplementation of SOTR_STRING_CREATE_CONCEPT because we can't supply
    " concept and it would then be generated.

    DATA: lv_subrc                 TYPE sy-subrc,
          lv_source_langu          TYPE spras,
          ls_header                TYPE btfr_head,
          lv_flag_is_string        TYPE btfr_flag VALUE abap_true,
          lt_text_tab              TYPE sotr_text_tt,
          lv_concept_default       TYPE sotr_conc,
          lt_entries               TYPE sotr_textl_tt,
          lv_concept               LIKE is_sots-header-concept,
          lv_flag_correction_entry TYPE abap_bool VALUE abap_true.

    lt_entries = is_sots-entries.

    ls_header-paket          = iv_package.
    ls_header-crea_lan       = mv_language.
    ls_header-alias_name     = is_sots-header-alias_name.
    lv_source_langu          = mv_language.
    lv_concept               = is_sots-header-concept.

    PERFORM btfr_create
      IN PROGRAM saplsotr_db_string
      USING iv_object
            lv_source_langu
            lv_flag_correction_entry
            lv_flag_is_string
      CHANGING lt_text_tab
               lt_entries
               ls_header
               lv_concept
               lv_concept_default
               lv_subrc.

    CASE lv_subrc.
      WHEN 1.
        Lcx_abapgit_exception=>raise( |No entry found| ).
      WHEN 2.
        Lcx_abapgit_exception=>raise( |OTR concept not found| ).
      WHEN 3.
        Lcx_abapgit_exception=>raise( |Enter a permitted object type| ).
      WHEN 4.
        "The concept will be created in the non-original system (not an error)
        RETURN.
      WHEN 5.
        Lcx_abapgit_exception=>raise( |Invalid alias| ).
      WHEN 6.
        Lcx_abapgit_exception=>raise( |No correction entry has been created| ).
      WHEN 7.
        Lcx_abapgit_exception=>raise( |Error in database operation| ).
      WHEN 9.
        Lcx_abapgit_exception=>raise( |Action canceled by user| ).
    ENDCASE.

  ENDMETHOD.
  METHOD get_raw_text_filename.

    DATA lv_langu TYPE string.

    " Lower case language codes can cause duplicate filenames therefore add suffix to make them unique
    " Note: Using ISO code would be better but is not compatible with existing files
    lv_langu = is_entry-langu.
    IF lv_langu = to_lower( lv_langu ).
      lv_langu = lv_langu && '-'.
    ENDIF.

    rv_filename =
        to_lower( |{ is_entry-concept }_|
               && |{ lv_langu         }_|
               && |{ is_entry-object  }_|
               && |{ is_entry-lfd_num }| ).

  ENDMETHOD.
  METHOD read_sots.

    DATA: lt_sotr_head TYPE STANDARD TABLE OF sotr_headu,
          lt_objects   TYPE sotr_objects,
          lv_object    LIKE LINE OF lt_objects,
          ls_sots      LIKE LINE OF rt_sots.

    FIELD-SYMBOLS: <ls_sotr_head> TYPE sotr_head,
                   <ls_entry>     LIKE LINE OF ls_sots-entries.


    SELECT * FROM sotr_headu
             INTO TABLE lt_sotr_head
             WHERE paket = ms_item-obj_name
             ORDER BY PRIMARY KEY.

    LOOP AT lt_sotr_head ASSIGNING <ls_sotr_head>.

      CALL FUNCTION 'SOTR_OBJECT_GET_OBJECTS'
        EXPORTING
          object_vector    = <ls_sotr_head>-objid_vec
        IMPORTING
          objects          = lt_objects
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      READ TABLE lt_objects INDEX 1 INTO lv_object.
      ASSERT sy-subrc = 0.

      " Handled by object serializer
      CHECK lv_object <> 'SICF' AND lv_object <> 'CPUB'.

      CLEAR: ls_sots.

      CALL FUNCTION 'SOTR_STRING_GET_CONCEPT'
        EXPORTING
          concept        = <ls_sotr_head>-concept
        IMPORTING
          header         = ls_sots-header
          entries        = ls_sots-entries
        EXCEPTIONS
          no_entry_found = 1
          OTHERS         = 2.

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CLEAR:
        ls_sots-header-paket,
        ls_sots-header-crea_name,
        ls_sots-header-crea_tstut,
        ls_sots-header-chan_name,
        ls_sots-header-chan_tstut.

      LOOP AT ls_sots-entries ASSIGNING <ls_entry>.
        CLEAR: <ls_entry>-version,
               <ls_entry>-crea_name,
               <ls_entry>-crea_tstut,
               <ls_entry>-chan_name,
               <ls_entry>-chan_tstut.
      ENDLOOP.

      INSERT ls_sots INTO TABLE rt_sots.

    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    SELECT SINGLE chan_name FROM sotr_headu INTO rv_user
      WHERE paket = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lt_sots TYPE ty_sots_tt.

    FIELD-SYMBOLS: <ls_sots> TYPE ty_sots.

    lt_sots = read_sots( ).

    LOOP AT lt_sots ASSIGNING <ls_sots>.
      " Remove any usage to ensure deletion, see function module BTFR_CHECK
      DELETE FROM sotr_useu WHERE concept = <ls_sots>-header-concept.

      CALL FUNCTION 'BTFR_DELETE_SINGLE_TEXT'
        EXPORTING
          concept             = <ls_sots>-header-concept
          flag_string         = abap_true
        EXCEPTIONS
          text_not_found      = 1
          invalid_package     = 2
          text_not_changeable = 3
          text_enqueued       = 4
          no_correction       = 5
          parameter_error     = 6
          OTHERS              = 7.

      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lt_sots    TYPE ty_sots_tt,
          lt_objects TYPE sotr_objects,
          lv_object  LIKE LINE OF lt_objects.

    FIELD-SYMBOLS: <ls_sots>  TYPE ty_sots,
                   <ls_entry> LIKE LINE OF <ls_sots>-entries.

    io_xml->read(
      EXPORTING
        iv_name = 'SOTS'
      CHANGING
        cg_data = lt_sots ).

    tadir_insert( iv_package ).

    LOOP AT lt_sots ASSIGNING <ls_sots>.

      CLEAR: lt_objects.

      CALL FUNCTION 'SOTR_OBJECT_GET_OBJECTS'
        EXPORTING
          object_vector    = <ls_sots>-header-objid_vec
        IMPORTING
          objects          = lt_objects
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.

      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'error from SOTR_OBJECT_GET_OBJECTS' ).
      ENDIF.

      READ TABLE lt_objects INDEX 1 INTO lv_object.
      ASSERT sy-subrc = 0.

      LOOP AT <ls_sots>-entries ASSIGNING <ls_entry>.

        TRY.
            <ls_entry>-text = Lif_abapgit_object~mo_files->read_string(
              iv_extra = get_raw_text_filename( <ls_entry> )
              iv_ext   = 'txt' ).

          CATCH Lcx_abapgit_exception.
            " Most probably file not found -> ignore
            CONTINUE.
        ENDTRY.

      ENDLOOP.

      create_sots(
          is_sots    = <ls_sots>
          iv_package = iv_package
          iv_object  = lv_object ).

    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_object_type TYPE trobjtype,
          lv_object_name TYPE trobj_name.

    lv_object_type = ms_item-obj_type.
    lv_object_name = ms_item-obj_name.

    CALL FUNCTION 'SOTR_WBO_OBJECTS_CHECK'
      EXPORTING
        pgmid          = 'R3TR'
        object         = lv_object_type
        obj_name       = lv_object_name
      IMPORTING
        object_exist   = rv_bool
      EXCEPTIONS
        unknown_object = 1
        OTHERS         = 2.

    IF sy-subrc <> 0.
      rv_bool = abap_false.
    ENDIF.

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

    DATA: lt_sots TYPE ty_sots_tt.

    FIELD-SYMBOLS: <ls_sots>  TYPE ty_sots,
                   <ls_entry> TYPE sotr_textl.

    lt_sots = read_sots( ).

    LOOP AT lt_sots ASSIGNING <ls_sots>.

      LOOP AT <ls_sots>-entries ASSIGNING <ls_entry>.

        Lif_abapgit_object~mo_files->add_string(
          iv_extra  = get_raw_text_filename( <ls_entry> )
          iv_ext    = 'txt'
          iv_string = <ls_entry>-text ).

        CLEAR: <ls_entry>-text.

      ENDLOOP.

    ENDLOOP.

    io_xml->add( iv_name = 'SOTS'
                 ig_data = lt_sots ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SOTS implementation

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
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
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
endclass. "ZCL_ABAPGIT_OBJECT_SPLO implementation

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
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    get_generic( )->serialize( io_xml ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SPPF implementation

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
    rv_active = abap_true. "dummy implementation
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
endclass. "ZCL_ABAPGIT_OBJECT_SPRX implementation

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
