********************************************************************************
*
* LICENSE and NOTICE
*
* See include program ZSHRINKER_ABAPGIT_LICENSE
*
********************************************************************************
*>>>>>>> ZCL_ABAPGIT_GIT_ADD_PATCH <<<<<<<*

*"* macro definitions
*include zcl_abapgit_git_add_patch=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_git_add_patch=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_git_add_patch=====ccau.
*"* use this source file for your ABAP unit test classes




class LCL_ABAPGIT_GIT_ADD_PATCH implementation.
*"* method's implementations
*include methods.
  METHOD calculate_patch.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF mt_diff.

    LOOP AT mt_diff ASSIGNING <ls_diff>.

      CASE <ls_diff>-result.
        WHEN Lif_abapgit_definitions=>c_diff-unchanged.

          INSERT <ls_diff>-old INTO TABLE rt_patch.

        WHEN Lif_abapgit_definitions=>c_diff-insert.

          IF <ls_diff>-patch_flag = abap_true.
            INSERT <ls_diff>-new INTO TABLE rt_patch.
          ENDIF.

        WHEN Lif_abapgit_definitions=>c_diff-delete.

          IF <ls_diff>-patch_flag = abap_false.
            INSERT <ls_diff>-old INTO TABLE rt_patch.
          ENDIF.

        WHEN Lif_abapgit_definitions=>c_diff-update.

          IF <ls_diff>-patch_flag = abap_true.
            INSERT <ls_diff>-new INTO TABLE rt_patch.
          ELSE.
            INSERT <ls_diff>-old INTO TABLE rt_patch.
          ENDIF.

        WHEN OTHERS.

          Lcx_abapgit_exception=>raise( |Unknown result| ).

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.
  METHOD constructor.

    mt_diff = it_diff.

  ENDMETHOD.
  METHOD get_patch.

    IF mt_patch IS INITIAL.
      mt_patch = calculate_patch( ).
    ENDIF.

    rt_patch = mt_patch.

  ENDMETHOD.
  METHOD get_patch_binary.

    DATA: lv_string TYPE string.

    IF mt_patch IS INITIAL.
      mt_patch = calculate_patch( ).
    ENDIF.

    CONCATENATE LINES OF mt_patch INTO lv_string SEPARATED BY cl_abap_char_utilities=>newline.
    lv_string = lv_string && cl_abap_char_utilities=>newline.

    rv_patch_binary = Lcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GIT_ADD_PATCH implementation

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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_DRUL implementation

*>>>>>>> ZCL_ABAPGIT_GIT_URL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_git_url===========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_git_url===========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_git_url===========ccau.
*CLASS SHRITEFUH64VYIPO5I47WOOA5WHASM DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_url DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5WHASM.




class LCL_ABAPGIT_GIT_URL implementation.
*"* method's implementations
*include methods.
  METHOD get_commit_display_url.

    DATA li_exit TYPE REF TO Lif_abapgit_exit.

    rv_url = get_default_commit_display_url(
      iv_repo_url = io_repo->get_url( )
      iv_hash     = io_repo->get_current_remote( ) ).

    li_exit = Lcl_abapgit_exit=>get_instance( ).
    li_exit->adjust_display_commit_url(
      EXPORTING
        iv_repo_url    = io_repo->get_url( )
        iv_repo_name   = io_repo->get_name( )
        iv_repo_key    = io_repo->get_key( )
        iv_commit_hash = io_repo->get_current_remote( )
      CHANGING
        cv_display_url = rv_url ).

    IF rv_url IS INITIAL.
      Lcx_abapgit_exception=>raise( |provider not yet supported| ).
    ENDIF.

  ENDMETHOD.
  METHOD get_default_commit_display_url.

    DATA ls_result TYPE match_result.
    FIELD-SYMBOLS <ls_provider_match> TYPE submatch_result.

    rv_commit_url = iv_repo_url.

    FIND REGEX '^http(?:s)?:\/\/(?:www\.)?(github\.com|bitbucket\.org|gitlab\.com)\/'
      IN rv_commit_url
      RESULTS ls_result.
    IF sy-subrc = 0.
      READ TABLE ls_result-submatches INDEX 1 ASSIGNING <ls_provider_match>.
      CASE rv_commit_url+<ls_provider_match>-offset(<ls_provider_match>-length).
        WHEN 'github.com'.
          REPLACE REGEX '\.git$' IN rv_commit_url WITH space.
          rv_commit_url = rv_commit_url && |/commit/| && iv_hash.
        WHEN 'bitbucket.org'.
          REPLACE REGEX '\.git$' IN rv_commit_url WITH space.
          rv_commit_url = rv_commit_url && |/commits/| && iv_hash.
        WHEN 'gitlab.com'.
          REPLACE REGEX '\.git$' IN rv_commit_url WITH space.
          rv_commit_url = rv_commit_url && |/-/commit/| && iv_hash.
      ENDCASE.
    ENDIF.

  ENDMETHOD.
  METHOD validate_url.

    DATA lv_provider TYPE string.

    lv_provider = Lcl_abapgit_url=>host( to_lower( iv_url ) ).

    " Provider-specific check for URLs that don't work
    IF lv_provider CS 'gitlab.com'.
      FIND REGEX '\.git$' IN iv_url IGNORING CASE.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'Repo URL for GitLab must end in ".git"' ).
      ENDIF.
    ELSEIF lv_provider CS 'dev.azure.com'.
      FIND REGEX '\.git$' IN iv_url IGNORING CASE.
      IF sy-subrc = 0.
        Lcx_abapgit_exception=>raise( 'Repo URL for Azure DevOps must not end in ".git"' ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GIT_URL implementation

*>>>>>>> ZCL_ABAPGIT_GIT_UTILS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_git_utils=========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_git_utils=========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_git_utils=========ccau.


class LCL_ABAPGIT_GIT_UTILS implementation.
*"* method's implementations
*include methods.
  METHOD get_null.

* must be length 4, or it gives a syntax error on lower versions
    DATA: lv_x TYPE x LENGTH 4 VALUE '00000000'.
    FIELD-SYMBOLS <lv_y> TYPE c.

    ASSIGN lv_x TO <lv_y> CASTING.
    rv_c = <lv_y>.

  ENDMETHOD.
  METHOD length_utf8_hex.

    DATA: lv_xstring TYPE xstring,
          lv_char4   TYPE c LENGTH 4,
          lv_x       TYPE x LENGTH 2.

    IF xstrlen( iv_data ) < 4.
      Lcx_abapgit_exception=>raise( 'error converting to hex, LENGTH_UTF8_HEX' ).
    ENDIF.

    lv_xstring = iv_data(4).

    lv_char4 = Lcl_abapgit_convert=>xstring_to_string_utf8(
      iv_data   = lv_xstring
      iv_length = 4 ).

    TRANSLATE lv_char4 TO UPPER CASE.
    lv_x = lv_char4.
    rv_len = lv_x.

  ENDMETHOD.
  METHOD pkt_string.

    DATA: lv_x   TYPE x,
          lv_len TYPE i.


    lv_len = strlen( iv_string ).

    IF lv_len >= 255.
      Lcx_abapgit_exception=>raise( 'PKT, todo' ).
    ENDIF.

    lv_x = lv_len + 4.

    rv_pkt = '00' && lv_x && iv_string.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GIT_UTILS implementation

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
  METHOD link_transport.
* call to CL_CTS_ADT_TM_URI_BUILDER=>CREATE_ADT_URI replaced with logic that works on all systems,
    rv_link = |adt://{ sy-sysid }/sap/bc/adt/cts/transportrequests/{ iv_transport }|.
  ENDMETHOD.
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
endclass. "ZCL_ABAPGIT_ADT_LINK implementation

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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENHO implementation

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

*>>>>>>> ZCL_ABAPGIT_AUTH <<<<<<<*

*"* macro definitions
*include zcl_abapgit_auth==============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_auth==============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_auth==============ccau.




class LCL_ABAPGIT_AUTH implementation.
*"* method's implementations
*include methods.
  METHOD is_allowed.

    DATA: li_auth TYPE REF TO Lif_abapgit_auth.

    TRY.
        CREATE OBJECT li_auth TYPE ('LCL_ABAPGIT_AUTH_EXIT').
        rv_allowed = li_auth->is_allowed( iv_authorization = iv_authorization
                                          iv_param         = iv_param ).
      CATCH cx_sy_create_object_error.
        rv_allowed = abap_true.
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_AUTH implementation

*>>>>>>> ZCL_ABAPGIT_BACKGROUND_PUSH_AU <<<<<<<*

*"* macro definitions
*include zcl_abapgit_background_push_auccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_background_push_auccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_BACKGROUND_PUSH_AU implementation.
*"* method's implementations
*include methods.
  METHOD build_comment.

    DATA: lt_objects TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
          lv_str     TYPE string.

    FIELD-SYMBOLS: <ls_local> LIKE LINE OF is_files-local.


    LOOP AT is_files-local ASSIGNING <ls_local>.
      lv_str = |{ <ls_local>-item-obj_type } { <ls_local>-item-obj_name }|.
      APPEND lv_str TO lt_objects.
    ENDLOOP.

    SORT lt_objects AS TEXT.
    DELETE ADJACENT DUPLICATES FROM lt_objects.

    IF lines( lt_objects ) = 1.
      rv_comment = |BG: { lv_str }|.
    ELSE.
      rv_comment = 'BG: Multiple objects'.
      LOOP AT lt_objects INTO lv_str.
        CONCATENATE rv_comment cl_abap_char_utilities=>newline lv_str INTO rv_comment.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD determine_user_details.

    DATA: lo_user_record TYPE REF TO Lcl_abapgit_user_record.


    lo_user_record = Lcl_abapgit_user_record=>get_instance( iv_changed_by ).
    rs_user-name = lo_user_record->get_name( ).
    rs_user-email = lo_user_record->get_email( ).

*   If no email, fall back to localhost/default email
    IF rs_user-email IS INITIAL.
      rs_user-email = |{ iv_changed_by }@localhost|.
    ENDIF.

*   If no full name maintained, just use changed by user name
    IF rs_user-name IS INITIAL.
      rs_user-name  = iv_changed_by.
    ENDIF.

  ENDMETHOD.
  METHOD push_auto.

    TYPES: BEGIN OF ty_changed,
             filename   TYPE string,
             path       TYPE string,
             changed_by TYPE syuname,
           END OF ty_changed.

    DATA: ls_comment    TYPE Lif_abapgit_git_definitions=>ty_comment,
          ls_files      TYPE Lif_abapgit_definitions=>ty_stage_files,
          lt_changed    TYPE STANDARD TABLE OF ty_changed WITH DEFAULT KEY,
          lt_users      TYPE STANDARD TABLE OF syuname WITH DEFAULT KEY,
          ls_user_files LIKE ls_files,
          lv_changed_by LIKE LINE OF lt_users,
          lo_stage      TYPE REF TO Lcl_abapgit_stage.

    FIELD-SYMBOLS: <ls_changed> LIKE LINE OF lt_changed,
                   <ls_remote>  LIKE LINE OF ls_files-remote,
                   <ls_local>   LIKE LINE OF ls_files-local.


    ls_files = Lcl_abapgit_factory=>get_stage_logic( )->get( io_repo ).

    LOOP AT ls_files-local ASSIGNING <ls_local>.
      lv_changed_by = Lcl_abapgit_objects=>changed_by(
        is_item     = <ls_local>-item
        iv_filename = <ls_local>-file-filename ).
      APPEND lv_changed_by TO lt_users.
      APPEND INITIAL LINE TO lt_changed ASSIGNING <ls_changed>.
      <ls_changed>-changed_by = lv_changed_by.
      <ls_changed>-filename   = <ls_local>-file-filename.
      <ls_changed>-path       = <ls_local>-file-path.
    ENDLOOP.

    SORT lt_users ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_users.

    LOOP AT lt_users INTO lv_changed_by.
      CLEAR: ls_comment.

*     Fill user details
      ls_comment-committer = determine_user_details( lv_changed_by ).

      CREATE OBJECT lo_stage.

      CLEAR ls_user_files.

      LOOP AT ls_files-local ASSIGNING <ls_local>.
        READ TABLE lt_changed WITH KEY
          path = <ls_local>-file-path
          filename = <ls_local>-file-filename
          changed_by = lv_changed_by
          TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          mi_log->add_info( |stage: {
            ls_comment-committer-name } {
            <ls_local>-file-path } {
            <ls_local>-file-filename }| ).

          lo_stage->add( iv_path     = <ls_local>-file-path
                         iv_filename = <ls_local>-file-filename
                         iv_data     = <ls_local>-file-data ).

          APPEND <ls_local> TO ls_user_files-local.

          LOOP AT ls_files-remote ASSIGNING <ls_remote>
              USING KEY file
              WHERE filename = <ls_local>-file-filename
              AND path <> <ls_local>-file-path
              AND filename <> 'package.devc.xml'.
            mi_log->add_info( |rm: { <ls_remote>-path } { <ls_remote>-filename }| ).

* rm old file when object has moved
            lo_stage->rm(
              iv_path     = <ls_remote>-path
              iv_filename = <ls_remote>-filename ).
            EXIT. " assumption: only one file
          ENDLOOP.
        ENDIF.
      ENDLOOP.

      ls_comment-comment = build_comment( ls_user_files ).

      io_repo->push( is_comment = ls_comment
                     io_stage   = lo_stage ).
    ENDLOOP.

    IF lines( ls_files-remote ) > 0.
      push_deletions( io_repo  = io_repo
                      is_files = ls_files ).
    ENDIF.

  ENDMETHOD.
  METHOD push_deletions.

    DATA: lo_stage   TYPE REF TO Lcl_abapgit_stage,
          ls_comment TYPE Lif_abapgit_git_definitions=>ty_comment.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF is_files-remote.

    ASSERT lines( is_files-remote ) > 0.

    CREATE OBJECT lo_stage.

    ls_comment-comment = 'BG: Deletion'.

    LOOP AT is_files-remote ASSIGNING <ls_remote>.

      mi_log->add_info( |removed: { <ls_remote>-path } { <ls_remote>-filename }| ).

      lo_stage->rm( iv_path     = <ls_remote>-path
                    iv_filename = <ls_remote>-filename ).

      CONCATENATE ls_comment-comment cl_abap_char_utilities=>newline <ls_remote>-filename
        INTO ls_comment-comment.

    ENDLOOP.

    ls_comment-committer-name  = 'Deletion'.
    ls_comment-committer-email = 'deletion@localhost'.

    io_repo->push( is_comment = ls_comment
                   io_stage   = lo_stage ).

  ENDMETHOD.
  METHOD Lif_abapgit_background~get_description.

    rv_description = 'Automatic push, auto author'.

  ENDMETHOD.
  METHOD Lif_abapgit_background~get_settings.

    RETURN.

  ENDMETHOD.
  METHOD Lif_abapgit_background~run.

    DATA: ls_files TYPE Lif_abapgit_definitions=>ty_stage_files.

    mi_log = ii_log.
    ls_files = Lcl_abapgit_factory=>get_stage_logic( )->get( io_repo ).

    IF lines( ls_files-local ) = 0 AND lines( ls_files-remote ) = 0.
      ii_log->add_info( 'Nothing to stage' ).
      RETURN.
    ENDIF.

    push_auto( io_repo ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_BACKGROUND_PUSH_AU implementation

*>>>>>>> ZCL_ABAPGIT_GUI_COMPONENT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_component=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_component=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_COMPONENT implementation.
*"* method's implementations
*include methods.
  METHOD gui_services.
    IF mi_gui_services IS NOT BOUND.
      mi_gui_services = Lcl_abapgit_ui_factory=>get_gui_services( ).
    ENDIF.
    ri_gui_services = mi_gui_services.
  ENDMETHOD.
  METHOD register_deferred_script.
    gui_services( )->get_html_parts( )->add_part(
      iv_collection = c_html_parts-scripts
      ii_part       = ii_part ).
  ENDMETHOD.
  METHOD register_event_handler.

    DATA li_event_handler TYPE REF TO Lif_abapgit_gui_event_handler.

    IF ii_event_handler IS BOUND.
      li_event_handler = ii_event_handler.
    ELSE.
      TRY.
          li_event_handler ?= me.
        CATCH cx_root.
          RETURN.
      ENDTRY.
    ENDIF.

    gui_services( )->register_event_handler( li_event_handler ).

  ENDMETHOD.
  METHOD register_handlers.
    register_event_handler( ).
    register_hotkeys( ).
  ENDMETHOD.
  METHOD register_hotkeys.

    DATA li_hotkey_provider TYPE REF TO Lif_abapgit_gui_hotkeys.

    IF ii_hotkey_provider IS BOUND.
      li_hotkey_provider = ii_hotkey_provider.
    ELSE.
      TRY.
          li_hotkey_provider ?= me.
        CATCH cx_root.
          RETURN.
      ENDTRY.
    ENDIF.

    gui_services( )->get_hotkeys_ctl( )->register_hotkeys( li_hotkey_provider->get_hotkey_actions( ) ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_COMPONENT implementation

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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENHS implementation

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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENSC implementation

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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_FORM implementation

*>>>>>>> ZCL_ABAPGIT_DATA_CONFIG <<<<<<<*

*"* macro definitions
*include zcl_abapgit_data_config=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_data_config=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_data_config=======ccau.



class LCL_ABAPGIT_DATA_CONFIG implementation.
*"* method's implementations
*include methods.
  METHOD dump.

    DATA lo_ajson TYPE REF TO Lcl_abapgit_ajson.
    DATA lx_ajson TYPE REF TO Lcx_abapgit_ajson_error.

    TRY.
        lo_ajson = Lcl_abapgit_ajson=>create_empty( ).
        lo_ajson->Lif_abapgit_ajson~set(
          iv_path = '/'
          iv_val  = is_config ).
        rv_json = Lcl_abapgit_convert=>string_to_xstring_utf8( lo_ajson->stringify( 2 ) ).
      CATCH Lcx_abapgit_ajson_error INTO lx_ajson.
        Lcx_abapgit_exception=>raise( lx_ajson->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_data_config~add_config.

    DATA lv_where TYPE string.

    FIELD-SYMBOLS <ls_config> LIKE LINE OF mt_config.

    ASSERT is_config-type IS NOT INITIAL.
    ASSERT is_config-name IS NOT INITIAL.
    ASSERT is_config-name = to_upper( is_config-name ).

    INSERT is_config INTO TABLE mt_config.
    IF sy-subrc <> 0.
* append to existing
      READ TABLE mt_config ASSIGNING <ls_config> WITH KEY type = is_config-type name = is_config-name.
      ASSERT sy-subrc = 0.
      LOOP AT is_config-where INTO lv_where.
        READ TABLE <ls_config>-where TRANSPORTING NO FIELDS WITH KEY table_line = lv_where.
        IF sy-subrc <> 0.
          INSERT lv_where INTO TABLE <ls_config>-where.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_data_config~from_json.

    DATA ls_file LIKE LINE OF it_files.
    DATA ls_config TYPE Lif_abapgit_data_config=>ty_config.
    DATA lo_ajson TYPE REF TO Lcl_abapgit_ajson.
    DATA lx_ajson TYPE REF TO Lcx_abapgit_ajson_error.

    CLEAR mt_config.
    LOOP AT it_files INTO ls_file
        USING KEY file_path
        WHERE path = Lif_abapgit_data_config=>c_default_path
        AND filename CP |*.{ Lif_abapgit_data_config=>c_config }.{ Lif_abapgit_data_config=>c_default_format }|.
      TRY.
          lo_ajson = Lcl_abapgit_ajson=>parse( Lcl_abapgit_convert=>xstring_to_string_utf8( ls_file-data ) ).
          lo_ajson->Lif_abapgit_ajson~to_abap( IMPORTING ev_container = ls_config ).
        CATCH Lcx_abapgit_ajson_error INTO lx_ajson.
          Lcx_abapgit_exception=>raise( lx_ajson->get_text( ) ).
      ENDTRY.

      Lif_abapgit_data_config~add_config( ls_config ).
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_data_config~get_configs.
    rt_configs = mt_config.
  ENDMETHOD.
  METHOD Lif_abapgit_data_config~remove_config.

    ASSERT is_config-type IS NOT INITIAL.
    ASSERT is_config-name IS NOT INITIAL.
    ASSERT is_config-name = to_upper( is_config-name ).

    DELETE mt_config WHERE name = is_config-name AND type = is_config-type.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Not found' ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_data_config~to_json.

    DATA ls_config LIKE LINE OF mt_config.
    DATA ls_file LIKE LINE OF rt_files.

    ls_file-path = Lif_abapgit_data_config=>c_default_path.

    LOOP AT mt_config INTO ls_config.
      ls_file-data = dump( ls_config ).
      ls_file-sha1 = Lcl_abapgit_hash=>sha1_blob( ls_file-data ).
      ls_config-type = Lif_abapgit_data_config=>c_config.
      ls_file-filename = Lcl_abapgit_data_utils=>build_data_filename( ls_config ).
      APPEND ls_file TO rt_files.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_data_config~update_config.

    Lif_abapgit_data_config~remove_config( is_config ).
    Lif_abapgit_data_config~add_config( is_config ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_DATA_CONFIG implementation

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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_IEXT implementation

*>>>>>>> ZCL_ABAPGIT_FILENAME_LOGIC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_filename_logic====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_filename_logic====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_filename_logic====ccau.
*CLASS SHRITEFUH64VYIPO5I47WOOA5UVASM DEFINITION DEFERRED.
*CLASS zcl_abapgit_filename_logic DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5UVASM.

CLASS SHRITEFUH64VYIPO5I47WOOA5UWASM DEFINITION.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_persist_settings.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5I47WOOA5UWASM IMPLEMENTATION.
  METHOD Lif_abapgit_persist_settings~modify.
    RETURN.
  ENDMETHOD.

  METHOD Lif_abapgit_persist_settings~read.
    CREATE OBJECT ro_settings.
  ENDMETHOD.
ENDCLASS.



class LCL_ABAPGIT_FILENAME_LOGIC implementation.
*"* method's implementations
*include methods.
  METHOD detect_obj_definition.

    ev_is_xml  = boolc( iv_ext = to_upper( c_package_file-extension ) AND strlen( iv_type ) = 4 ).
    ev_is_json = boolc( iv_ext = to_upper( c_json_file-extension ) AND strlen( iv_type ) = 4 ).

  ENDMETHOD.
  METHOD file_to_object.

    DATA:
      lv_name TYPE string,
      lv_type TYPE string,
      lv_ext  TYPE string.

    " Guess object type and name
    SPLIT to_upper( iv_filename ) AT '.' INTO lv_name lv_type lv_ext.

    " Handle namespaces
    REPLACE ALL OCCURRENCES OF '#' IN lv_name WITH '/'.
    REPLACE ALL OCCURRENCES OF '#' IN lv_type WITH '/'.
    REPLACE ALL OCCURRENCES OF '#' IN lv_ext WITH '/'.

    " Assume AFF namespace convention
    CREATE OBJECT go_aff_registry TYPE Lcl_abapgit_aff_registry.

    IF go_aff_registry->is_supported_object_type( |{ lv_type }| ) = abap_true.
      REPLACE ALL OCCURRENCES OF '(' IN lv_name WITH '/'.
      REPLACE ALL OCCURRENCES OF ')' IN lv_name WITH '/'.
    ENDIF.

    " Get original object name
    lv_name = name_unescape( lv_name ).

    CLEAR es_item.
    es_item-obj_type = lv_type.
    es_item-obj_name = lv_name.

    " Get mapping specific to object type
    map_filename_to_object(
      EXPORTING
        iv_filename = iv_filename
        iv_path     = iv_path
        io_dot      = io_dot
        iv_package  = iv_devclass
      CHANGING
        cs_item     = es_item ).

    detect_obj_definition(
      EXPORTING
        iv_ext     = lv_ext
        iv_type    = lv_type
      IMPORTING
        ev_is_xml  = ev_is_xml
        ev_is_json = ev_is_json ).

  ENDMETHOD.
  METHOD is_obj_definition_file.

    DATA:
      lv_xml  TYPE abap_bool,
      lv_json TYPE abap_bool,
      lv_name TYPE string,
      lv_type TYPE string,
      lv_ext  TYPE string.

    SPLIT to_upper( iv_filename ) AT '.' INTO lv_name lv_type lv_ext.

    detect_obj_definition(
      EXPORTING
        iv_ext     = lv_ext
        iv_type    = lv_type
      IMPORTING
        ev_is_xml  = lv_xml
        ev_is_json = lv_json ).

    rv_yes = boolc( lv_json = abap_true OR lv_xml = abap_true ).

  ENDMETHOD.
  METHOD object_to_file.

    DATA lv_obj_name TYPE string.
    DATA lv_nb_of_slash TYPE string.

    " Get escaped object name
    lv_obj_name = to_lower( name_escape( is_item-obj_name ) ).

    IF iv_extra IS INITIAL.
      CONCATENATE lv_obj_name '.' is_item-obj_type INTO rv_filename.
    ELSE.
      CONCATENATE lv_obj_name '.' is_item-obj_type '.' iv_extra INTO rv_filename.
    ENDIF.

    IF iv_ext IS NOT INITIAL.
      CONCATENATE rv_filename '.' iv_ext INTO rv_filename.
    ENDIF.

    " Get mapping specific to object type
    TRY.
        map_object_to_filename(
          EXPORTING
            is_item     = is_item
          CHANGING
            cv_filename = rv_filename ).
      CATCH Lcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

    " Handle namespaces
    CREATE OBJECT go_aff_registry TYPE Lcl_abapgit_aff_registry.

    IF go_aff_registry->is_supported_object_type( is_item-obj_type ) = abap_true.
      FIND ALL OCCURRENCES OF `/` IN rv_filename MATCH COUNT lv_nb_of_slash.
      IF lv_nb_of_slash = 2.
        REPLACE FIRST OCCURRENCE OF `/` IN rv_filename WITH `(`.
        REPLACE `/` IN rv_filename WITH `)`.
      ENDIF.
    ELSE.
      REPLACE ALL OCCURRENCES OF '/' IN rv_filename WITH '#'.
    ENDIF.

    TRANSLATE rv_filename TO LOWER CASE.

  ENDMETHOD.
  METHOD map_filename_to_object.

    DATA lv_class TYPE seoclsname.

    " TODO: Add check for supported object types to avoid calls to non-existing classes
    " zcl_abapgit_objects=>is_type_supported( is_item-obj_type )
    " This will trigger class constructor of zcl_abapgit_objects_bridge reading table seometarel
    " which is currently not supported by abaplint test runner

    TRY.
        lv_class = 'LCL_ABAPGIT_OBJECT_' && cs_item-obj_type.

        CALL METHOD (lv_class)=>('LIF_ABAPGIT_OBJECT~MAP_FILENAME_TO_OBJECT')
          EXPORTING
            iv_filename = iv_filename
            iv_path     = iv_path
            io_dot      = io_dot
            iv_package  = iv_package
          CHANGING
            cs_item     = cs_item.
      CATCH cx_sy_dyn_call_illegal_class ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.
  METHOD map_object_to_filename.

    DATA lv_class TYPE seoclsname.

    " TODO: Add check for supported object types to avoid calls to non-existing classes
    " zcl_abapgit_objects=>is_type_supported( is_item-obj_type )
    " This will trigger class constructor of zcl_abapgit_objects_bridge reading table seometarel
    " which is currently not supported by abaplint test runner

    TRY.
        lv_class = 'LCL_ABAPGIT_OBJECT_' && is_item-obj_type.

        CALL METHOD (lv_class)=>('LIF_ABAPGIT_OBJECT~MAP_OBJECT_TO_FILENAME')
          EXPORTING
            is_item     = is_item
          CHANGING
            cv_filename = cv_filename.
      CATCH cx_sy_dyn_call_illegal_class ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.
  METHOD name_escape.
    " Some characters in object names cause problems when identifying the object later
    " -> we escape these characters here
    " cl_http_utility=>escape_url doesn't do dots but escapes slash which we use for namespaces
    " -> we escape just some selected characters
    rv_name = iv_name.
    REPLACE ALL OCCURRENCES OF `#` IN rv_name WITH '%23'.
    REPLACE ALL OCCURRENCES OF `%` IN rv_name WITH '%25'.
    REPLACE ALL OCCURRENCES OF `.` IN rv_name WITH '%2e'.
    REPLACE ALL OCCURRENCES OF `<` IN rv_name WITH '%3c'.
    REPLACE ALL OCCURRENCES OF `=` IN rv_name WITH '%3d'.
    REPLACE ALL OCCURRENCES OF `>` IN rv_name WITH '%3e'.
    REPLACE ALL OCCURRENCES OF `?` IN rv_name WITH '%3f'.
  ENDMETHOD.
  METHOD name_unescape.
    " Replace all %xy with encoded character
    rv_name = cl_http_utility=>unescape_url( iv_name ).
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_FILENAME_LOGIC implementation

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
endclass. "ZCL_ABAPGIT_HTML_VIEWER_GUI implementation

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

*>>>>>>> ZCL_ABAPGIT_GUI_IN_PAGE_MODAL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_in_page_modal=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_in_page_modal=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_IN_PAGE_MODAL implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    ms_attrs-width  = iv_width.
    ms_attrs-height = iv_height.
    mi_child        = ii_child.

  ENDMETHOD.
  METHOD create.
    CREATE OBJECT ro_wrap
      EXPORTING
        ii_child  = ii_child
        iv_width  = iv_width
        iv_height = iv_height.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    DATA lo_style TYPE REF TO Lcl_abapgit_string_buffer.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.
    CREATE OBJECT lo_style.

    IF ms_attrs-width IS NOT INITIAL.
      lo_style->add( |width:{ ms_attrs-width }px;| ).
    ENDIF.
    IF ms_attrs-height IS NOT INITIAL.
      lo_style->add( |height:{ ms_attrs-height }px;| ).
    ENDIF.

    ri_html->add( |<div class="modal" style="{ lo_style->join_w_space_and_flush( ) }">| ).
    ri_html->add( |<div class="modal-guts">| ).
    ri_html->add( mi_child->render( ) ).
    ri_html->add( |</div>| ).
    ri_html->add( |</div>| ).
    ri_html->add( |<div class="modal-overlay"></div>| ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_IN_PAGE_MODAL implementation

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
CLASS SHRITEFUH64VYIPO5I47WOOA5XTASM DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_gui_renderable.
ENDCLASS.
CLASS SHRITEFUH64VYIPO5I47WOOA5XTASM IMPLEMENTATION.
  METHOD Lif_abapgit_gui_renderable~render.
  ENDMETHOD.
ENDCLASS.
CLASS SHRITEFUH64VYIPO5I47WOOA5XVASM DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_gui_event_handler.
ENDCLASS.
CLASS SHRITEFUH64VYIPO5I47WOOA5XVASM IMPLEMENTATION.
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

*>>>>>>> ZCL_ABAPGIT_LOGIN_MANAGER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_login_manager=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_login_manager=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_login_manager=====ccau.


class LCL_ABAPGIT_LOGIN_MANAGER implementation.
*"* method's implementations
*include methods.
  METHOD append.

    FIELD-SYMBOLS: <ls_auth> LIKE LINE OF gt_auth.

    READ TABLE gt_auth WITH KEY uri = Lcl_abapgit_url=>host( iv_uri )
      TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO gt_auth ASSIGNING <ls_auth>.
      <ls_auth>-uri           = Lcl_abapgit_url=>host( iv_uri ).
      <ls_auth>-authorization = iv_auth.
    ENDIF.

  ENDMETHOD.
  METHOD clear.

    CLEAR gt_auth.

  ENDMETHOD.
  METHOD get.

    DATA ls_auth LIKE LINE OF gt_auth.

    READ TABLE gt_auth INTO ls_auth WITH KEY uri = Lcl_abapgit_url=>host( iv_uri ).
    IF sy-subrc = 0.
      rv_auth = ls_auth-authorization.
    ENDIF.

  ENDMETHOD.
  METHOD load.

    DATA ls_auth LIKE LINE OF gt_auth.

    READ TABLE gt_auth INTO ls_auth WITH KEY uri = Lcl_abapgit_url=>host( iv_uri ).
    IF sy-subrc = 0.
      rv_authorization = ls_auth-authorization.
    ENDIF.

  ENDMETHOD.
  METHOD save.

    IF NOT iv_authorization IS INITIAL.
      append( iv_uri  = iv_uri
              iv_auth = iv_authorization ).
    ENDIF.

  ENDMETHOD.
  METHOD set.

    DATA: lv_concat TYPE string.

    ASSERT NOT iv_uri IS INITIAL.

    IF iv_username IS INITIAL OR iv_password IS INITIAL.
      RETURN.
    ENDIF.

    CONCATENATE iv_username ':' iv_password INTO lv_concat.

    rv_auth = cl_http_utility=>encode_base64( lv_concat ).

    CONCATENATE 'Basic' rv_auth INTO rv_auth
      SEPARATED BY space.

    append( iv_uri  = iv_uri
            iv_auth = rv_auth ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_LOGIN_MANAGER implementation

*>>>>>>> ZCL_ABAPGIT_LOG_VIEWER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_log_viewer========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_log_viewer========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_LOG_VIEWER implementation.
*"* method's implementations
*include methods.
  METHOD calculate_cell_type.

    FIELD-SYMBOLS: <ls_log> LIKE LINE OF gt_log.
    DATA: ls_cell_type LIKE LINE OF <ls_log>-cell_type.

    LOOP AT gt_log ASSIGNING <ls_log>.

      IF <ls_log>-longtext IS NOT INITIAL.
        ls_cell_type-columnname = `LONGTEXT`.
        ls_cell_type-value      = if_salv_c_cell_type=>hotspot.
        INSERT ls_cell_type INTO TABLE <ls_log>-cell_type.
      ENDIF.

      IF <ls_log>-t100 IS NOT INITIAL.
        ls_cell_type-columnname = `T100`.
        ls_cell_type-value      = if_salv_c_cell_type=>hotspot.
        INSERT ls_cell_type INTO TABLE <ls_log>-cell_type.
      ENDIF.

      IF <ls_log>-source IS NOT INITIAL.
        ls_cell_type-columnname = `SOURCE`.
        ls_cell_type-value      = if_salv_c_cell_type=>hotspot.
        INSERT ls_cell_type INTO TABLE <ls_log>-cell_type.
      ENDIF.

      IF <ls_log>-callstack IS NOT INITIAL.
        ls_cell_type-columnname = `CALLSTACK`.
        ls_cell_type-value      = if_salv_c_cell_type=>hotspot.
        INSERT ls_cell_type INTO TABLE <ls_log>-cell_type.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD dispatch.

    CASE iv_column.
      WHEN `LONGTEXT`.

        show_longtext( is_log ).

      WHEN `T100`.

        goto_t100_message( is_log ).

      WHEN `SOURCE`.

        goto_source( is_log ).

      WHEN `CALLSTACK`.

        goto_callstack( is_log ).

    ENDCASE.

  ENDMETHOD.
  METHOD get_exception_viewer.

    DATA:
      lx_abapgit TYPE REF TO Lcx_abapgit_exception.

    ASSERT is_log-exception IS BOUND.
    lx_abapgit ?= is_log-exception.

    CREATE OBJECT ro_exception_viewer
      EXPORTING
        ix_error = lx_abapgit.

  ENDMETHOD.
  METHOD goto_callstack.

    get_exception_viewer( is_log )->show_callstack( ).

  ENDMETHOD.
  METHOD goto_source.

    get_exception_viewer( is_log )->goto_source( ).

  ENDMETHOD.
  METHOD goto_t100_message.

    get_exception_viewer( is_log )->goto_message( ).

  ENDMETHOD.
  METHOD on_link_click.

    DATA: lx_error TYPE REF TO Lcx_abapgit_exception.
    FIELD-SYMBOLS: <ls_log> TYPE ty_log_out.

    IF row IS INITIAL
    OR column IS INITIAL.
      RETURN.
    ENDIF.

    READ TABLE gt_log ASSIGNING <ls_log>
                      INDEX row.
    ASSERT sy-subrc = 0.

    TRY.
        dispatch(
            is_log    = <ls_log>
            iv_column = column ).

      CATCH Lcx_abapgit_exception INTO lx_error.
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.
  METHOD prepare_log_for_display.

    DATA: lt_message      TYPE Lif_abapgit_log=>ty_log_outs,
          lr_message      TYPE REF TO Lif_abapgit_log=>ty_log_out,
          ls_log          TYPE ty_log_out,
          li_t100_message TYPE REF TO if_t100_message,
          lx_abapgit      TYPE REF TO Lcx_abapgit_exception.

    lt_message = ii_log->get_messages( ).

    LOOP AT lt_message REFERENCE INTO lr_message.

      CLEAR: ls_log.

      ls_log-msg = lr_message->text.
      ls_log-exception = lr_message->exception.

      CASE lr_message->type.
        WHEN 'E' OR 'A' OR 'X'.
          ls_log-type = icon_led_red.
        WHEN 'W'.
          ls_log-type = icon_led_yellow.
        WHEN 'I' OR 'S'.
          ls_log-type = icon_led_green.
        WHEN OTHERS.
          ls_log-type = icon_led_inactive.
      ENDCASE.

      IF lr_message->exception IS BOUND.

        TRY.
            li_t100_message ?= lr_message->exception.

            IF li_t100_message->t100key IS NOT INITIAL.
              ls_log-t100 = icon_message_information.
            ENDIF.

          CATCH cx_sy_move_cast_error ##NO_HANDLER.
        ENDTRY.

        TRY.
            lx_abapgit ?= lr_message->exception.

            IF lx_abapgit->mt_callstack IS NOT INITIAL.
              ls_log-longtext  = icon_system_help.
              ls_log-callstack = icon_stack.
              ls_log-source    = icon_abap.
            ENDIF.

          CATCH cx_sy_move_cast_error ##NO_HANDLER.
        ENDTRY.

      ENDIF.

      ls_log-obj_type = lr_message->obj_type.
      ls_log-obj_name = lr_message->obj_name.

      INSERT ls_log INTO TABLE rt_log_out.

    ENDLOOP.

  ENDMETHOD.
  METHOD show_log.

    DATA: lr_log         TYPE REF TO ty_log_out,
          lo_alv         TYPE REF TO cl_salv_table,
          lx_error       TYPE REF TO cx_salv_error,
          lo_form_header TYPE REF TO cl_salv_form_header_info,
          lo_columns     TYPE REF TO cl_salv_columns_table,
          lo_column      TYPE REF TO cl_salv_column,
          lo_functions   TYPE REF TO cl_salv_functions_list,
          ls_position    TYPE Lif_abapgit_popups=>ty_popup_position,
          lv_add_obj_col TYPE abap_bool,
          lo_event       TYPE REF TO cl_salv_events_table.

    gt_log = prepare_log_for_display( ii_log ).

    "check if log contains any object info
    LOOP AT gt_log REFERENCE INTO lr_log.
      IF lr_log->obj_type IS NOT INITIAL OR lr_log->obj_name IS NOT INITIAL.
        lv_add_obj_col = abap_true.
      ENDIF.
    ENDLOOP.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = gt_log ).

        lo_functions = lo_alv->get_functions( ).
        lo_functions->set_all( ).

        lo_alv->get_display_settings( )->set_list_header( |abapGit Log Viewer| ).

        lo_columns = lo_alv->get_columns( ).

        lo_columns->set_optimize( ).
        lo_columns->set_cell_type_column( |CELL_TYPE| ).

        calculate_cell_type( ).

        lo_column = lo_columns->get_column( |TYPE| ).
        lo_column->set_medium_text( |Type| ).

        lo_column = lo_columns->get_column( |MSG| ).
        lo_column->set_medium_text( |Message| ).

        lo_column = lo_columns->get_column( |LONGTEXT| ).
        lo_column->set_medium_text( |Longtext| ).

        lo_column = lo_columns->get_column( |T100| ).
        lo_column->set_medium_text( |Goto message| ).

        lo_column = lo_columns->get_column( |SOURCE| ).
        lo_column->set_medium_text( |Goto source| ).

        lo_column = lo_columns->get_column( |CALLSTACK| ).
        lo_column->set_medium_text( |Show callstack| ).

        IF lv_add_obj_col = abap_true.
          lo_column = lo_columns->get_column( |OBJ_TYPE| ).
          lo_column->set_medium_text( |Object Type| ).

          lo_column = lo_columns->get_column( |OBJ_NAME| ).
          lo_column->set_medium_text( |Object Name| ).
        ELSE.
          "hide object columns
          lo_column = lo_columns->get_column( |OBJ_TYPE| ).
          lo_column->set_technical( abap_true ).

          lo_column = lo_columns->get_column( |OBJ_NAME| ).
          lo_column->set_technical( abap_true ).
        ENDIF.

        "hide empty columns
        LOOP AT gt_log TRANSPORTING NO FIELDS WHERE t100 IS NOT INITIAL.
          EXIT.
        ENDLOOP.
        IF sy-subrc <> 0.
          lo_column = lo_columns->get_column( |T100| ).
          lo_column->set_technical( abap_true ).
        ENDIF.

        LOOP AT gt_log TRANSPORTING NO FIELDS WHERE source IS NOT INITIAL.
          EXIT.
        ENDLOOP.
        IF sy-subrc <> 0.
          lo_column = lo_columns->get_column( |SOURCE| ).
          lo_column->set_technical( abap_true ).
        ENDIF.

        LOOP AT gt_log TRANSPORTING NO FIELDS WHERE longtext IS NOT INITIAL.
          EXIT.
        ENDLOOP.
        IF sy-subrc <> 0.
          lo_column = lo_columns->get_column( |LONGTEXT| ).
          lo_column->set_technical( abap_true ).
        ENDIF.

        LOOP AT gt_log TRANSPORTING NO FIELDS WHERE callstack IS NOT INITIAL.
          EXIT.
        ENDLOOP.
        IF sy-subrc <> 0.
          lo_column = lo_columns->get_column( |CALLSTACK| ).
          lo_column->set_technical( abap_true ).
        ENDIF.

        ls_position = Lcl_abapgit_popups=>center(
          iv_width  = 125
          iv_height = 20 ).

        lo_alv->set_screen_popup( start_column = ls_position-start_column
                                  end_column   = ls_position-end_column
                                  start_line   = ls_position-start_row
                                  end_line     = ls_position-end_row ).

        CREATE OBJECT lo_form_header
          EXPORTING
            text = ii_log->get_title( ).

        lo_alv->set_top_of_list( lo_form_header ).

        lo_event = lo_alv->get_event( ).

        SET HANDLER on_link_click FOR lo_event.

        lo_alv->display( ).

      CATCH cx_salv_error INTO lx_error.
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.
  METHOD show_longtext.

    DATA: lx_abapgit TYPE REF TO Lcx_abapgit_exception.

    DATA: lv_docu_object TYPE dokhl-object,
          lt_dummy1      TYPE TABLE OF dselc,
          lt_dummy2      TYPE TABLE OF dval,
          ls_help_info   TYPE help_info.

    IF is_log-exception IS NOT BOUND.
      RETURN.
    ENDIF.

    TRY.
        lx_abapgit ?= is_log-exception.
      CATCH cx_sy_move_cast_error.
        RETURN.
    ENDTRY.

    lv_docu_object   = lx_abapgit->if_t100_message~t100key-msgid.
    lv_docu_object+2 = lx_abapgit->if_t100_message~t100key-msgno.

    ls_help_info-call       = 'D'.
    ls_help_info-spras      = sy-langu.
    ls_help_info-messageid  = lx_abapgit->if_t100_message~t100key-msgid.
    ls_help_info-messagenr  = lx_abapgit->if_t100_message~t100key-msgno.
    ls_help_info-message    = is_log-msg.
    ls_help_info-title      = 'Longtext'.
    ls_help_info-docuid     = 'NA'.
    ls_help_info-docuobject = lv_docu_object.
    ls_help_info-msgv1      = lx_abapgit->msgv1.
    ls_help_info-msgv2      = lx_abapgit->msgv2.
    ls_help_info-msgv3      = lx_abapgit->msgv3.
    ls_help_info-msgv4      = lx_abapgit->msgv4.

    CALL FUNCTION 'HELP_START'
      EXPORTING
        help_infos   = ls_help_info
      TABLES
        dynpselect   = lt_dummy1
        dynpvaluetab = lt_dummy2
      EXCEPTIONS
        OTHERS       = 1.

    IF sy-subrc IS NOT INITIAL.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD to_html.

    DATA: lt_message TYPE Lif_abapgit_log=>ty_log_outs,
          lr_message TYPE REF TO Lif_abapgit_log=>ty_log_out,
          lv_class   TYPE string,
          lv_icon    TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    IF ii_log->count( ) = 0.
      RETURN.
    ENDIF.

    lt_message = ii_log->get_messages( ).

    LOOP AT lt_message REFERENCE INTO lr_message.
      CASE lr_message->type.
        WHEN 'W'.
          lv_icon  = 'attention'.
          lv_class = 'warning'.
        WHEN 'E'.
          lv_icon  = 'error'.
          lv_class = 'error'.
        WHEN OTHERS. " ??? unexpected
          lv_icon  = 'error'.
          lv_class = 'error'.
      ENDCASE.

      ri_html->add( |<span class="{ lv_class }">| ).
      ri_html->add_icon( lv_icon ).
      ri_html->add( lr_message->text ).
      ri_html->add( '</span>' ).
    ENDLOOP.

  ENDMETHOD.
  METHOD write_log.

    DATA: lt_message TYPE Lif_abapgit_log=>ty_log_outs,
          lr_message TYPE REF TO Lif_abapgit_log=>ty_log_out,
          lv_text    TYPE string.

    lt_message = ii_log->get_messages( ).

    LOOP AT lt_message REFERENCE INTO lr_message.
      IF lr_message->obj_name IS NOT INITIAL AND lr_message->obj_type IS NOT INITIAL.
        lv_text = |{ lr_message->type }: { lr_message->text } ({ lr_message->obj_type }/{ lr_message->obj_name })|.
      ELSE.
        lv_text = |{ lr_message->type }: { lr_message->text }|.
      ENDIF.
      WRITE: / lv_text.
    ENDLOOP.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_LOG_VIEWER implementation

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
endclass. "ZCL_ABAPGIT_OBJECT_IWOM implementation

*>>>>>>> ZCL_ABAPGIT_HTTP_AGENT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_http_agent========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_http_agent========ccimp.
CLASS SHRITEFUH64VYIPO5I47WOOA5YNASM DEFINITION FINAL.
  PUBLIC SECTION.

    INTERFACES Lif_abapgit_http_response.

    CLASS-METHODS create
      IMPORTING
        ii_client          TYPE REF TO if_http_client
      RETURNING
        VALUE(ri_response) TYPE REF TO Lif_abapgit_http_response.

  PRIVATE SECTION.
    DATA mi_client TYPE REF TO if_http_client.
    DATA mi_response TYPE REF TO if_http_response.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5I47WOOA5YNASM IMPLEMENTATION.

  METHOD create.
    DATA lo_response TYPE REF TO SHRITEFUH64VYIPO5I47WOOA5YNASM.
    CREATE OBJECT lo_response.
    lo_response->mi_client   = ii_client.
    lo_response->mi_response = ii_client->response.
    ri_response ?= lo_response.
  ENDMETHOD.

  METHOD Lif_abapgit_http_response~close.
    mi_client->close( ).
  ENDMETHOD.

  METHOD Lif_abapgit_http_response~is_ok.
    DATA lv_code TYPE i.
    lv_code = Lif_abapgit_http_response~code( ).
    rv_yes = boolc( lv_code >= 200 AND lv_code < 300 ).
  ENDMETHOD.

  METHOD Lif_abapgit_http_response~data.
    rv_data = mi_response->get_data( ).
  ENDMETHOD.

  METHOD Lif_abapgit_http_response~cdata.
    rv_data = mi_response->get_cdata( ).
  ENDMETHOD.

  METHOD Lif_abapgit_http_response~code.
    DATA lv_msg TYPE string ##NEEDED.
    mi_response->get_status(
      IMPORTING
        reason = lv_msg " for debug
        code   = rv_code ).
  ENDMETHOD.

  METHOD Lif_abapgit_http_response~json.

    ri_json = Lcl_abapgit_ajson=>parse( Lif_abapgit_http_response~cdata( ) ).

  ENDMETHOD.

  METHOD Lif_abapgit_http_response~error.
    rv_message = mi_response->get_cdata( ).
  ENDMETHOD.

  METHOD Lif_abapgit_http_response~headers.

    DATA lt_headers TYPE tihttpnvp.
    FIELD-SYMBOLS <ls_h> LIKE LINE OF lt_headers.

    CREATE OBJECT ro_headers.

    mi_response->get_header_fields( CHANGING fields = lt_headers ).
    LOOP AT lt_headers ASSIGNING <ls_h>.
      ro_headers->set(
        iv_key = <ls_h>-name
        iv_val = <ls_h>-value ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

class LCL_ABAPGIT_HTTP_AGENT implementation.
*"* method's implementations
*include methods.
  METHOD attach_payload.

    DATA lo_type TYPE REF TO cl_abap_typedescr.
    lo_type = cl_abap_typedescr=>describe_by_data( iv_payload ).

    IF lo_type->type_kind = cl_abap_typedescr=>typekind_xstring.
      ii_request->set_data( iv_payload ).

    ELSEIF lo_type->type_kind = cl_abap_typedescr=>typekind_string.
      ii_request->set_cdata( iv_payload ).

    ELSE.
      Lcx_abapgit_exception=>raise( |Unexpected payload type { lo_type->absolute_name }| ).
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    CREATE OBJECT mo_global_headers.

  ENDMETHOD.
  METHOD create.

    CREATE OBJECT ri_instance TYPE Lcl_abapgit_http_agent.

  ENDMETHOD.
  METHOD Lif_abapgit_http_agent~global_headers.

    ro_global_headers = mo_global_headers.

  ENDMETHOD.
  METHOD Lif_abapgit_http_agent~request.

    DATA li_client TYPE REF TO if_http_client.
    DATA lo_proxy_configuration TYPE REF TO Lcl_abapgit_proxy_config.
    DATA lv_code TYPE i.
    DATA lv_message TYPE string.
    FIELD-SYMBOLS <ls_entry> LIKE LINE OF io_query->mt_entries.

    CREATE OBJECT lo_proxy_configuration.

    cl_http_client=>create_by_url(
      EXPORTING
        url           = iv_url
        ssl_id        = Lcl_abapgit_exit=>get_instance( )->get_ssl_id( )
        proxy_host    = lo_proxy_configuration->get_proxy_url( iv_url )
        proxy_service = lo_proxy_configuration->get_proxy_port( iv_url )
      IMPORTING
        client = li_client ).

    li_client->request->set_version( if_http_request=>co_protocol_version_1_1 ).
    li_client->request->set_method( iv_method ).

    IF io_query IS BOUND.
      LOOP AT io_query->mt_entries ASSIGNING <ls_entry>.
        li_client->request->set_form_field(
          name  = <ls_entry>-k
          value = <ls_entry>-v ).
      ENDLOOP.
    ENDIF.

    LOOP AT mo_global_headers->mt_entries ASSIGNING <ls_entry>.
      li_client->request->set_header_field(
        name  = to_lower( <ls_entry>-k )
        value = <ls_entry>-v ).
    ENDLOOP.

    IF io_headers IS BOUND.
      LOOP AT io_headers->mt_entries ASSIGNING <ls_entry>.
        li_client->request->set_header_field(
          name  = to_lower( <ls_entry>-k )
          value = <ls_entry>-v ).
      ENDLOOP.
    ENDIF.

    IF iv_method = Lif_abapgit_http_agent=>c_methods-post
      OR iv_method = Lif_abapgit_http_agent=>c_methods-put
      OR iv_method = Lif_abapgit_http_agent=>c_methods-patch.
      attach_payload(
        ii_request = li_client->request
        iv_payload = iv_payload ).
    ENDIF.

    li_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).
    IF sy-subrc = 0.
      li_client->receive(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4 ).
    ENDIF.

    IF sy-subrc <> 0.
      li_client->get_last_error(
        IMPORTING
          code    = lv_code
          message = lv_message ).
      Lcx_abapgit_exception=>raise( |HTTP error: [{ lv_code }] { lv_message }| ).
    ENDIF.

    ri_response = SHRITEFUH64VYIPO5I47WOOA5YNASM=>create( li_client ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_HTTP_AGENT implementation

*>>>>>>> ZCL_ABAPGIT_HTTP_DIGEST <<<<<<<*

*"* macro definitions
*include zcl_abapgit_http_digest=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_http_digest=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_HTTP_DIGEST implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    parse( ii_client ).

    mv_ha1 = md5( |{ iv_username }:{ mv_realm }:{ iv_password }| ).

    mv_username = iv_username.

  ENDMETHOD.
  METHOD hash.

    DATA: lv_ha2 TYPE string.


    lv_ha2 = md5( |{ iv_method }:{ iv_uri }| ).

    ASSERT NOT iv_cnonse IS INITIAL.

    rv_response = md5( |{ mv_ha1 }:{ iv_nonce }:{ gv_nc }:{ iv_cnonse }:{ iv_qop }:{ lv_ha2 }| ).

  ENDMETHOD.
  METHOD md5.

    DATA lv_xstr TYPE xstring.
    DATA lv_hash TYPE xstring.
    DATA lv_empty TYPE xstring.

    lv_xstr = Lcl_abapgit_convert=>string_to_xstring_utf8( iv_data ).

    TRY.
        cl_abap_hmac=>calculate_hmac_for_raw(
          EXPORTING
            if_algorithm   = 'MD5'
            if_key         = lv_empty
            if_data        = lv_xstr
          IMPORTING
            ef_hmacxstring = lv_hash ).
      CATCH cx_abap_message_digest.
        Lcx_abapgit_exception=>raise( 'error calculating md5' ).
    ENDTRY.

    rv_hash = lv_hash.
    TRANSLATE rv_hash TO LOWER CASE.

  ENDMETHOD.
  METHOD parse.

    DATA: lv_value TYPE string.


    lv_value = ii_client->response->get_header_field( 'www-authenticate' ).

    FIND REGEX 'realm="([\w ]+)"' IN lv_value SUBMATCHES mv_realm.
    FIND REGEX 'qop="(\w+)"' IN lv_value SUBMATCHES mv_qop.
    FIND REGEX 'nonce="([\w=/+\$]+)"' IN lv_value SUBMATCHES mv_nonce.

  ENDMETHOD.
  METHOD run.

    DATA: lv_response TYPE string,
          lv_method   TYPE string,
          lv_cnonce   TYPE string,
          lv_uri      TYPE string,
          lv_auth     TYPE string.


    ASSERT NOT mv_nonce IS INITIAL.

    lv_method = ii_client->request->get_header_field( '~request_method' ).
    lv_uri = ii_client->request->get_header_field( '~request_uri' ).

    CALL FUNCTION 'GENERAL_GET_RANDOM_STRING'
      EXPORTING
        number_chars  = 24
      IMPORTING
        random_string = lv_cnonce.

    lv_response = hash(
      iv_qop    = mv_qop
      iv_nonce  = mv_nonce
      iv_uri    = lv_uri
      iv_method = lv_method
      iv_cnonse = lv_cnonce ).

* client response
    lv_auth = |Digest username="{ mv_username
      }", realm="{ mv_realm
      }", nonce="{ mv_nonce
      }", uri="{ lv_uri
      }", qop={ mv_qop
      }, nc={ gv_nc
      }, cnonce="{ lv_cnonce
      }", response="{ lv_response }"|.

    ii_client->request->set_header_field(
      name  = 'Authorization'
      value = lv_auth ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_HTTP_DIGEST implementation

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
endclass. "ZCL_ABAPGIT_OBJECT_IWSG implementation

*>>>>>>> ZCL_ABAPGIT_LANGUAGE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_language==========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_language==========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_LANGUAGE implementation.
*"* method's implementations
*include methods.
  METHOD class_constructor.

    DATA lv_dummy TYPE string.

    GET LOCALE LANGUAGE gv_login_language COUNTRY lv_dummy MODIFIER lv_dummy.

  ENDMETHOD.
  METHOD restore_login_language.

    SET LOCALE LANGUAGE gv_login_language.

  ENDMETHOD.
  METHOD set_current_language.

    SET LOCALE LANGUAGE iv_language.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_LANGUAGE implementation

*>>>>>>> ZCL_ABAPGIT_LOG <<<<<<<*

*"* macro definitions
*include zcl_abapgit_log===============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_log===============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_log===============ccau.




class LCL_ABAPGIT_LOG implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    Lif_abapgit_log~set_title( iv_title ).

  ENDMETHOD.
  METHOD from_exception.

    CREATE OBJECT ro_log.

    IF io_x IS BOUND.
      ro_log->Lif_abapgit_log~add_exception( io_x ).
    ENDIF.

  ENDMETHOD.
  METHOD get_messages_status.

    DATA lr_msg TYPE REF TO Lif_abapgit_log=>ty_msg.
    rv_status = 'S'.
    LOOP AT it_msg REFERENCE INTO lr_msg.
      CASE lr_msg->type.
        WHEN 'E' OR 'A' OR 'X'.
          rv_status = 'E'. "not okay
          EXIT.
        WHEN 'W'.
          rv_status = 'W'. "maybe
          CONTINUE.
        WHEN 'S' OR 'I'.
          IF rv_status <> 'W'.
            rv_status = 'S'. "okay
          ENDIF.
          CONTINUE.
        WHEN OTHERS. "unknown
          CONTINUE.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_log~add.

    FIELD-SYMBOLS: <ls_log> LIKE LINE OF mt_log.

    APPEND INITIAL LINE TO mt_log ASSIGNING <ls_log>.
    <ls_log>-msg-text  = iv_msg.
    <ls_log>-msg-type  = iv_type.
    <ls_log>-item      = is_item.
    <ls_log>-exception = ix_exc.

    CASE iv_type.
      WHEN 'E' OR 'A' OR 'X'.
        <ls_log>-msg-level = Lif_abapgit_log=>c_log_level-error.
      WHEN 'W'.
        <ls_log>-msg-level = Lif_abapgit_log=>c_log_level-warning.
      WHEN 'S' OR 'I'.
        <ls_log>-msg-level = Lif_abapgit_log=>c_log_level-info.
      WHEN OTHERS. "unknown
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_log~add_error.

    Lif_abapgit_log~add(
      iv_msg  = iv_msg
      iv_type = 'E'
      is_item = is_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_log~add_exception.

    DATA lx_exc TYPE REF TO cx_root.
    DATA lv_msg TYPE string.
    lx_exc = ix_exc.
    DO.
      lv_msg = lx_exc->get_text( ).
      Lif_abapgit_log~add( iv_msg  = lv_msg
                           iv_type = 'E'
                           is_item = is_item
                           ix_exc  = lx_exc ).
      IF lx_exc->previous IS BOUND.
        lx_exc = lx_exc->previous.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD.
  METHOD Lif_abapgit_log~add_info.

    Lif_abapgit_log~add(
      iv_msg  = iv_msg
      iv_type = 'I'
      is_item = is_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_log~add_success.

    Lif_abapgit_log~add(
      iv_msg  = iv_msg
      iv_type = 'S'
      is_item = is_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_log~add_warning.

    Lif_abapgit_log~add(
      iv_msg  = iv_msg
      iv_type = 'W'
      is_item = is_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_log~clear.
    CLEAR mt_log.
  ENDMETHOD.
  METHOD Lif_abapgit_log~clone.

    DATA lo_log TYPE REF TO Lcl_abapgit_log.

    CREATE OBJECT lo_log EXPORTING iv_title = mv_title.
    lo_log->mt_log = mt_log.
    ri_log = lo_log.

  ENDMETHOD.
  METHOD Lif_abapgit_log~count.
    rv_count = lines( mt_log ).
  ENDMETHOD.
  METHOD Lif_abapgit_log~get_item_status.

    DATA lr_log         TYPE REF TO ty_log.
    DATA ls_msg         TYPE Lif_abapgit_log=>ty_msg.
    DATA ls_item_status TYPE Lif_abapgit_log=>ty_item_status_out.
    DATA lr_item_status TYPE REF TO Lif_abapgit_log=>ty_item_status_out.

    "collect all message for all objects
    LOOP AT mt_log REFERENCE INTO lr_log.
      CLEAR ls_item_status.
      ls_item_status-item = lr_log->item.
      READ TABLE rt_item_status REFERENCE INTO lr_item_status
           WITH KEY item-obj_type = ls_item_status-item-obj_type
                    item-obj_name = ls_item_status-item-obj_name.
      IF sy-subrc <> 0.
        INSERT ls_item_status INTO TABLE rt_item_status.
        GET REFERENCE OF ls_item_status INTO lr_item_status.
      ENDIF.
      CLEAR ls_msg.
      ls_msg-type = lr_log->msg-type.
      ls_msg-text = lr_log->msg-text.
      INSERT ls_msg INTO TABLE lr_item_status->messages.
    ENDLOOP.

    "determine object status from object messages
    LOOP AT rt_item_status REFERENCE INTO lr_item_status.
      lr_item_status->status = get_messages_status( lr_item_status->messages ).
      IF lr_item_status->messages IS INITIAL.
        CLEAR ls_msg.
        ls_msg-type = 'I'.
        ls_msg-text = 'No message'.
        INSERT ls_msg INTO TABLE lr_item_status->messages.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_log~get_log_level.

    FIELD-SYMBOLS <ls_log> LIKE LINE OF mt_log.

    rv_level = Lif_abapgit_log=>c_log_level-empty.

    LOOP AT mt_log ASSIGNING <ls_log>.
      IF <ls_log>-msg-level = Lif_abapgit_log=>c_log_level-error.
        rv_level = Lif_abapgit_log=>c_log_level-error.
        EXIT.
      ELSEIF <ls_log>-msg-level > rv_level.
        rv_level = <ls_log>-msg-level.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_log~get_messages.
    DATA ls_msg TYPE Lif_abapgit_log~ty_log_out.
    FIELD-SYMBOLS <ls_log> TYPE ty_log.
    LOOP AT mt_log ASSIGNING <ls_log>.
      ls_msg-type      = <ls_log>-msg-type.
      ls_msg-text      = <ls_log>-msg-text.
      ls_msg-obj_type  = <ls_log>-item-obj_type.
      ls_msg-obj_name  = <ls_log>-item-obj_name.
      ls_msg-exception = <ls_log>-exception.
      APPEND ls_msg TO rt_msg.
    ENDLOOP.
    DELETE ADJACENT DUPLICATES FROM rt_msg.
  ENDMETHOD.
  METHOD Lif_abapgit_log~get_status.

    DATA lr_log TYPE REF TO ty_log.
    rv_status = Lif_abapgit_log=>c_status-ok.
    LOOP AT mt_log REFERENCE INTO lr_log.
      CASE lr_log->msg-type.
        WHEN 'E' OR 'A' OR 'X'.
          rv_status = Lif_abapgit_log=>c_status-error.
          EXIT.
        WHEN 'W'.
          rv_status = Lif_abapgit_log=>c_status-warning.
          CONTINUE.
        WHEN 'S' OR 'I'.
          IF rv_status <> Lif_abapgit_log=>c_status-warning.
            rv_status = Lif_abapgit_log=>c_status-ok.
          ENDIF.
          CONTINUE.
        WHEN OTHERS. "unknown
          ASSERT 0 = 1.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_log~get_title.
    rv_title = mv_title.
    IF rv_title IS INITIAL.
      rv_title = 'Log'.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_log~merge_with.

    DATA lo_log TYPE REF TO Lcl_abapgit_log.
    DATA lt_log_temp LIKE lo_log->mt_log.

    IF ii_log IS BOUND.
      lo_log ?= ii_log.
      IF iv_min_level > 0.
        lt_log_temp = lo_log->mt_log.
        DELETE lt_log_temp WHERE msg-level < iv_min_level.
        APPEND LINES OF lt_log_temp TO mt_log.
      ELSE.
        APPEND LINES OF lo_log->mt_log TO mt_log.
      ENDIF.
    ENDIF.

    ri_log = me.

  ENDMETHOD.
  METHOD Lif_abapgit_log~set_title.
    mv_title = iv_title.
    ri_log = me.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_LOG implementation

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

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_HOC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_hoc======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_hoc======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_HOC implementation.
*"* method's implementations
*include methods.
  METHOD create.

    DATA lo_page TYPE REF TO Lcl_abapgit_gui_page_hoc.
    DATA ls_control TYPE Lcl_abapgit_gui_page=>ty_control.

    ls_control-page_title          = iv_page_title.
    ls_control-page_layout         = iv_page_layout.
    ls_control-page_menu           = io_page_menu.
    ls_control-page_menu_provider  = ii_page_menu_provider.
    ls_control-page_title_provider = ii_page_title_provider.
    ls_control-extra_css_url       = iv_extra_css_url.
    ls_control-extra_js_url        = iv_extra_js_url.
    ls_control-show_as_modal       = iv_show_as_modal.

    CREATE OBJECT lo_page
      EXPORTING
        ii_child_component = ii_child_component
        is_control         = ls_control.

    ri_page_wrap = lo_page.

  ENDMETHOD.
  METHOD get_child.
    ri_child = mi_child.
  ENDMETHOD.
  METHOD render_content.

    IF mi_child IS BOUND.
      ri_html = mi_child->render( ).
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    super->constructor( ).

    mi_child = ii_child_component.
    ms_control = is_control.

    IF ms_control-show_as_modal = abap_false.
      ms_control-show_as_modal = detect_modal( ).
    ENDIF.

    IF ms_control-page_menu_provider IS NOT BOUND.
      ms_control-page_menu_provider = detect_menu_provider( ).
    ENDIF.

    IF ms_control-page_title_provider IS NOT BOUND.
      ms_control-page_title_provider = detect_title_provider( ).
    ENDIF.

  ENDMETHOD.
  METHOD detect_menu_provider.
    TRY.
        ri_ref ?= mi_child.
      CATCH cx_sy_move_cast_error.
    ENDTRY.
  ENDMETHOD.
  METHOD detect_modal.

    DATA li_modal TYPE REF TO Lif_abapgit_gui_modal.

    TRY.
        li_modal ?= mi_child.
        rv_is_modal = li_modal->is_modal( ).
      CATCH cx_sy_move_cast_error.
    ENDTRY.

  ENDMETHOD.
  METHOD detect_title_provider.
    TRY.
        ri_ref ?= mi_child.
      CATCH cx_sy_move_cast_error.
    ENDTRY.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_HOC implementation

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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_NROB implementation

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

CLASS SHRITEFUH64VYIPO5I47WOOA5TNASM DEFINITION FINAL.
  PUBLIC SECTION.

    DATA mt_nodes TYPE Lif_abapgit_ajson_types=>ty_nodes_tt READ-ONLY.

    METHODS add
      IMPORTING
        iv_str TYPE string.
    METHODS sorted
      RETURNING
        VALUE(rt_nodes) TYPE Lif_abapgit_ajson_types=>ty_nodes_ts.

ENDCLASS.

CLASS SHRITEFUH64VYIPO5I47WOOA5TNASM IMPLEMENTATION.
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
CLASS SHRITEFUH64VYIPO5I47WOOA5TZASM DEFINITION.
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

CLASS SHRITEFUH64VYIPO5I47WOOA5TZASM IMPLEMENTATION.
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

CLASS SHRITEFUH64VYIPO5I47WOOA5T3ASM DEFINITION.
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

CLASS SHRITEFUH64VYIPO5I47WOOA5T3ASM IMPLEMENTATION.
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
*       CLASS SHRITEFUH64VYIPO5I47WOOA5T5ASM DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS SHRITEFUH64VYIPO5I47WOOA5T5ASM IMPLEMENTATION
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

    rv_xstring = SHRITEFUH64VYIPO5I47WOOA5T3ASM=>convert( iv_string ).

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

    rv_string = SHRITEFUH64VYIPO5I47WOOA5TZASM=>convert(
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

*CLASS zcl_abapgit_i18n_params DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5YPASM.


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
*CLASS SHRITEFUH64VYIPO5I47WOOA5UZASM DEFINITION DEFERRED.
*CLASS SHRITEFUH64VYIPO5I47WOOA5U3ASM DEFINITION DEFERRED.

*CLASS zcl_abapgit_file_deserialize DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5UZASM.
*CLASS zcl_abapgit_file_deserialize DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5U3ASM.





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

CLASS SHRITEFUH64VYIPO5I47WOOA5VRASM IMPLEMENTATION.

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





*CLASS SHRITEFUH64VYIPO5I47WOOA5VXASM DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_pack DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5VXASM.



*CLASS SHRITEFUH64VYIPO5I47WOOA5VZASM DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_pack DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5VZASM.









*CLASS SHRITEFUH64VYIPO5I47WOOA5V7ASM DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_pack DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5V7ASM.



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
          lo_stream TYPE REF TO SHRITEFUH64VYIPO5I47WOOA5VRASM,
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
*CLASS SHRITEFUH64VYIPO5I47WOOA5WBASM DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_porcelain DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5WBASM.



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

INTERFACE SHRITEFUH64VYIPO5I47WOOA5RIASM.

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

CLASS SHRITEFUH64VYIPO5I47WOOA5RJASM DEFINITION FINAL.
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

CLASS SHRITEFUH64VYIPO5I47WOOA5RJASM IMPLEMENTATION.

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

CLASS SHRITEFUH64VYIPO5I47WOOA5RLASM DEFINITION FINAL.
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

CLASS SHRITEFUH64VYIPO5I47WOOA5RLASM IMPLEMENTATION.

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
    lo_reader = cl_sxml_string_reader=>create( SHRITEFUH64VYIPO5I47WOOA5RJASM=>string_to_xstring_utf8( iv_json ) ).

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

CLASS SHRITEFUH64VYIPO5I47WOOA5RNASM DEFINITION FINAL CREATE PRIVATE.
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

CLASS SHRITEFUH64VYIPO5I47WOOA5RNASM IMPLEMENTATION.

  METHOD class_constructor.
    gv_comma_with_lf = ',' && cl_abap_char_utilities=>newline.
  ENDMETHOD.

  METHOD stringify.

    DATA lo TYPE REF TO SHRITEFUH64VYIPO5I47WOOA5RNASM.
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

CLASS SHRITEFUH64VYIPO5I47WOOA5RPASM DEFINITION FINAL.
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
        type_kind         LIKE SHRITEFUH64VYIPO5I47WOOA5RIASM=>any,
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

CLASS SHRITEFUH64VYIPO5I47WOOA5RPASM IMPLEMENTATION.

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
    IF is_parent_type-type_kind = SHRITEFUH64VYIPO5I47WOOA5RIASM=>table.
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
        WHEN SHRITEFUH64VYIPO5I47WOOA5RIASM=>table.
          lo_tdescr ?= is_parent_type-dd.
          rs_node_type-dd = lo_tdescr->get_table_line_type( ).

        WHEN SHRITEFUH64VYIPO5I47WOOA5RIASM=>struct_flat OR SHRITEFUH64VYIPO5I47WOOA5RIASM=>struct_deep.
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
      IF rs_node_type-type_kind = SHRITEFUH64VYIPO5I47WOOA5RIASM=>table.
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
      WHEN SHRITEFUH64VYIPO5I47WOOA5RIASM=>table.
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

      WHEN SHRITEFUH64VYIPO5I47WOOA5RIASM=>struct_flat OR SHRITEFUH64VYIPO5I47WOOA5RIASM=>struct_deep.
        ASSIGN i_container_ref->* TO <parent_struc>.
        ASSERT sy-subrc = 0.
    ENDCASE.

    TRY.

      " array_index because stringified index goes in wrong order [1, 10, 2 ...]
        LOOP AT mr_nodes->* ASSIGNING <n> USING KEY array_index WHERE path = iv_path.

        " Get or create type cache record
          IF is_parent_type-type_kind <> SHRITEFUH64VYIPO5I47WOOA5RIASM=>table OR ls_node_type-type_kind IS INITIAL.
          " table records are the same, no need to refetch twice

            ls_node_type = get_node_type(
            is_node        = <n>
            is_parent_type = is_parent_type ).

            IF mv_corresponding = abap_true AND ls_node_type IS INITIAL.
              CONTINUE.
            ENDIF.

          ENDIF.

        " Validate node type
          IF ls_node_type-type_kind = SHRITEFUH64VYIPO5I47WOOA5RIASM=>data_ref OR
           ls_node_type-type_kind = SHRITEFUH64VYIPO5I47WOOA5RIASM=>object_ref.
          " TODO maybe in future
            Lcx_abapgit_ajson_error=>raise( 'Cannot assign to ref' ).
          ENDIF.

        " Find target field reference
          CASE is_parent_type-type_kind.
            WHEN SHRITEFUH64VYIPO5I47WOOA5RIASM=>table.
              IF NOT ls_node_type-target_field_name CO '0123456789'.
              " Does not affect anything actually but for integrity
                Lcx_abapgit_ajson_error=>raise( 'Need index to access tables' ).
              ENDIF.

              IF is_parent_type-tab_item_buf IS NOT BOUND. " Indirect hint that table was srt/hsh, see get_node_type
                APPEND INITIAL LINE TO <parent_stdtab> REFERENCE INTO lr_target_field.
                ASSERT sy-subrc = 0.
              ENDIF.

            WHEN SHRITEFUH64VYIPO5I47WOOA5RIASM=>struct_flat OR SHRITEFUH64VYIPO5I47WOOA5RIASM=>struct_deep.
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
              IF ls_node_type-type_kind <> SHRITEFUH64VYIPO5I47WOOA5RIASM=>struct_flat AND
               ls_node_type-type_kind <> SHRITEFUH64VYIPO5I47WOOA5RIASM=>struct_deep.
                Lcx_abapgit_ajson_error=>raise( 'Expected structure' ).
              ENDIF.
              any_to_abap(
              iv_path         = <n>-path && <n>-name && '/'
              is_parent_type  = ls_node_type
              i_container_ref = lr_target_field ).

            WHEN Lif_abapgit_ajson_types=>node_type-array.
              IF NOT ls_node_type-type_kind = SHRITEFUH64VYIPO5I47WOOA5RIASM=>table.
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

    IF is_node_type-type_kind CA SHRITEFUH64VYIPO5I47WOOA5RIASM=>deep_targets.
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
        IF is_node_type-type_kind = SHRITEFUH64VYIPO5I47WOOA5RIASM=>date AND is_node-value IS NOT INITIAL.
          <container> = to_date( is_node-value ).
        ELSEIF is_node_type-type_kind = SHRITEFUH64VYIPO5I47WOOA5RIASM=>packed AND is_node-value IS NOT INITIAL.
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

CLASS SHRITEFUH64VYIPO5I47WOOA5RRASM DEFINITION FINAL.
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

CLASS SHRITEFUH64VYIPO5I47WOOA5RRASM IMPLEMENTATION.

  METHOD class_constructor.

    DATA lo_dummy TYPE REF TO Lcl_abapgit_ajson.
    DATA lo_type TYPE REF TO cl_abap_refdescr.
    lo_type ?= cl_abap_typedescr=>describe_by_data( lo_dummy ).
    gv_ajson_absolute_type_name = lo_type->get_referenced_type( )->absolute_name.

  ENDMETHOD.

  METHOD convert.

    DATA lo_type TYPE REF TO cl_abap_typedescr.
    DATA lo_converter TYPE REF TO SHRITEFUH64VYIPO5I47WOOA5RRASM.

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

        IF io_type->type_kind = SHRITEFUH64VYIPO5I47WOOA5RIASM=>data_ref OR iv_data IS INITIAL.
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

        ELSEIF io_type->type_kind = SHRITEFUH64VYIPO5I47WOOA5RIASM=>object_ref
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
    ELSEIF io_type->type_kind CO SHRITEFUH64VYIPO5I47WOOA5RIASM=>texts OR
           io_type->type_kind CO SHRITEFUH64VYIPO5I47WOOA5RIASM=>binary OR
           io_type->type_kind CO SHRITEFUH64VYIPO5I47WOOA5RIASM=>enum.
      ls_node-type = Lif_abapgit_ajson_types=>node_type-string.
      ls_node-value = |{ iv_data }|.
    ELSEIF io_type->type_kind = SHRITEFUH64VYIPO5I47WOOA5RIASM=>date.
      ls_node-type = Lif_abapgit_ajson_types=>node_type-string.
      IF mv_format_datetime = abap_true.
        ls_node-value = format_date( iv_data ).
      ELSE.
        ls_node-value = |{ iv_data }|.
      ENDIF.
    ELSEIF io_type->type_kind = SHRITEFUH64VYIPO5I47WOOA5RIASM=>time.
      ls_node-type = Lif_abapgit_ajson_types=>node_type-string.
      IF mv_format_datetime = abap_true.
        ls_node-value = format_time( iv_data ).
      ELSE.
        ls_node-value = |{ iv_data }|.
      ENDIF.
    ELSEIF io_type->type_kind CO SHRITEFUH64VYIPO5I47WOOA5RIASM=>numeric.
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
    DATA lo_converter TYPE REF TO SHRITEFUH64VYIPO5I47WOOA5RRASM.

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
    IF io_type->type_kind CO SHRITEFUH64VYIPO5I47WOOA5RIASM=>texts OR
       io_type->type_kind CO SHRITEFUH64VYIPO5I47WOOA5RIASM=>date OR
       io_type->type_kind CO SHRITEFUH64VYIPO5I47WOOA5RIASM=>time.
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
    ELSEIF io_type->type_kind CO SHRITEFUH64VYIPO5I47WOOA5RIASM=>numeric.
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

INTERFACE SHRITEFUH64VYIPO5I47WOOA5RTASM.
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

CLASS SHRITEFUH64VYIPO5I47WOOA5RUASM DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES SHRITEFUH64VYIPO5I47WOOA5RTASM.
    CLASS-METHODS new
      IMPORTING
        ii_filter TYPE REF TO Lif_abapgit_ajson_filter
      RETURNING
        VALUE(ro_instance) TYPE REF TO SHRITEFUH64VYIPO5I47WOOA5RUASM.
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

CLASS SHRITEFUH64VYIPO5I47WOOA5RUASM IMPLEMENTATION.

  METHOD new.
    CREATE OBJECT ro_instance EXPORTING ii_filter = ii_filter.
  ENDMETHOD.

  METHOD constructor.
    ASSERT ii_filter IS BOUND.
    mi_filter = ii_filter.
  ENDMETHOD.

  METHOD SHRITEFUH64VYIPO5I47WOOA5RTASM~run.

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

CLASS SHRITEFUH64VYIPO5I47WOOA5RWASM DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES SHRITEFUH64VYIPO5I47WOOA5RTASM.
    CLASS-METHODS new
      IMPORTING
        ii_mapper TYPE REF TO Lif_abapgit_ajson_mapping
      RETURNING
        VALUE(ro_instance) TYPE REF TO SHRITEFUH64VYIPO5I47WOOA5RWASM.
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

CLASS SHRITEFUH64VYIPO5I47WOOA5RWASM IMPLEMENTATION.

  METHOD new.
    CREATE OBJECT ro_instance EXPORTING ii_mapper = ii_mapper.
  ENDMETHOD.

  METHOD constructor.
    ASSERT ii_mapper IS BOUND.
    mi_mapper = ii_mapper.
  ENDMETHOD.

  METHOD SHRITEFUH64VYIPO5I47WOOA5RTASM~run.

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

CLASS SHRITEFUH64VYIPO5I47WOOA5RYASM DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES SHRITEFUH64VYIPO5I47WOOA5RTASM.
    CLASS-METHODS new
      RETURNING
        VALUE(ro_instance) TYPE REF TO SHRITEFUH64VYIPO5I47WOOA5RYASM.
    METHODS add
      IMPORTING
        ii_mutator TYPE REF TO SHRITEFUH64VYIPO5I47WOOA5RTASM
      RETURNING
        VALUE(ro_self) TYPE REF TO SHRITEFUH64VYIPO5I47WOOA5RYASM.

  PRIVATE SECTION.
    DATA mt_queue TYPE STANDARD TABLE OF REF TO SHRITEFUH64VYIPO5I47WOOA5RTASM.

ENDCLASS.

CLASS SHRITEFUH64VYIPO5I47WOOA5RYASM IMPLEMENTATION.

  METHOD add.
    IF ii_mutator IS BOUND.
      APPEND ii_mutator TO mt_queue.
    ENDIF.
    ro_self = me.
  ENDMETHOD.

  METHOD new.
    CREATE OBJECT ro_instance.
  ENDMETHOD.

  METHOD SHRITEFUH64VYIPO5I47WOOA5RTASM~run.

    DATA li_mutator TYPE REF TO SHRITEFUH64VYIPO5I47WOOA5RTASM.
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
CLASS SHRITEFUH64VYIPO5I47WOOA5R2ASM DEFINITION FINAL.
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

CLASS SHRITEFUH64VYIPO5I47WOOA5R2ASM IMPLEMENTATION.
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


*CLASS zcl_abapgit_ajson DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5SAASM.


**********************************************************************
* READER
**********************************************************************


*CLASS zcl_abapgit_ajson DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5SBASM.



**********************************************************************
* JSON TO ABAP
**********************************************************************


*CLASS zcl_abapgit_ajson DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5SCASM.


**********************************************************************
* WRITER
**********************************************************************


*CLASS zcl_abapgit_ajson DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5SDASM.



**********************************************************************
* INTEGRATED
**********************************************************************


**********************************************************************
* ABAP TO JSON
**********************************************************************

*CLASS zcl_abapgit_ajson DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5SGASM.


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

    DATA lo_mutator_queue TYPE REF TO SHRITEFUH64VYIPO5I47WOOA5RYASM.

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
        lo_mutator_queue->add( SHRITEFUH64VYIPO5I47WOOA5RWASM=>new( ii_mapper ) ).
      ENDIF.
      IF ii_filter IS BOUND.
        lo_mutator_queue->add( SHRITEFUH64VYIPO5I47WOOA5RUASM=>new( ii_filter ) ).
      ENDIF.
      lo_mutator_queue->SHRITEFUH64VYIPO5I47WOOA5RTASM~run(
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
    ls_path_name = SHRITEFUH64VYIPO5I47WOOA5RJASM=>split_path( iv_path ).

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

    DATA lo_parser TYPE REF TO SHRITEFUH64VYIPO5I47WOOA5RLASM.

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
            ls_new_node-index = SHRITEFUH64VYIPO5I47WOOA5RJASM=>validate_array_index(
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

    lv_normalized_path = SHRITEFUH64VYIPO5I47WOOA5RJASM=>normalize_path( iv_path ).
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
    ls_split_path = SHRITEFUH64VYIPO5I47WOOA5RJASM=>split_path( iv_path ).

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

    DATA lo_to_abap TYPE REF TO SHRITEFUH64VYIPO5I47WOOA5RPASM.
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

    lv_normalized_path = SHRITEFUH64VYIPO5I47WOOA5RJASM=>normalize_path( iv_path ).

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
    ls_new_path-path = SHRITEFUH64VYIPO5I47WOOA5RJASM=>normalize_path( iv_path ).
    ls_new_path-name = |{ lv_new_index }|.

    lt_new_nodes = SHRITEFUH64VYIPO5I47WOOA5RRASM=>convert(
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

    ls_split_path = SHRITEFUH64VYIPO5I47WOOA5RJASM=>split_path( iv_path ).
    IF ls_split_path IS INITIAL. " Assign root, exceptional processing
      IF iv_node_type IS NOT INITIAL.
        mt_json_tree = SHRITEFUH64VYIPO5I47WOOA5RRASM=>insert_with_type(
          is_opts            = ms_opts
          iv_data            = iv_val
          iv_type            = iv_node_type
          is_prefix          = ls_split_path
          ii_custom_mapping  = mi_custom_mapping ).
      ELSE.
        mt_json_tree = SHRITEFUH64VYIPO5I47WOOA5RRASM=>convert(
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
      lv_array_index = SHRITEFUH64VYIPO5I47WOOA5RJASM=>validate_array_index(
        iv_path  = ls_split_path-path
        iv_index = ls_split_path-name ).
    ELSEIF lr_parent->type = Lif_abapgit_ajson_types=>node_type-object
      AND lv_item_order = 0 AND ms_opts-keep_item_order = abap_true.
      lv_item_order = lr_parent->children + 1.
    ENDIF.

    IF iv_node_type IS NOT INITIAL.
      lt_new_nodes = SHRITEFUH64VYIPO5I47WOOA5RRASM=>insert_with_type(
        is_opts            = ms_opts
        iv_item_order      = lv_item_order
        iv_data            = iv_val
        iv_type            = iv_node_type
        iv_array_index     = lv_array_index
        is_prefix          = ls_split_path
        ii_custom_mapping  = mi_custom_mapping ).
    ELSE.
      lt_new_nodes = SHRITEFUH64VYIPO5I47WOOA5RRASM=>convert(
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
    lv_val = SHRITEFUH64VYIPO5I47WOOA5RRASM=>format_date( iv_val ).

    Lif_abapgit_ajson~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_val ).

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~set_integer.

    ri_json = me.

    Lif_abapgit_ajson~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = iv_val ).

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~set_null.

    ri_json = me.

    DATA lv_null_ref TYPE REF TO data.
    Lif_abapgit_ajson~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_null_ref ).

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~set_string.

    ri_json = me.

    DATA lv_val TYPE string.
    lv_val = iv_val.
    Lif_abapgit_ajson~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_val ).

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~set_timestamp.

    ri_json = me.

    DATA lv_timestamp_iso TYPE string.
    lv_timestamp_iso = SHRITEFUH64VYIPO5I47WOOA5RRASM=>format_timestamp( iv_val ).

    Lif_abapgit_ajson~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_timestamp_iso ).

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~slice.

    DATA lo_section         TYPE REF TO Lcl_abapgit_ajson.
    DATA ls_item            LIKE LINE OF mt_json_tree.
    DATA lv_normalized_path TYPE string.
    DATA ls_path_parts      TYPE Lif_abapgit_ajson_types=>ty_path_name.
    DATA lv_path_len        TYPE i.
    DATA lv_path_pattern    TYPE string.

    CREATE OBJECT lo_section.
    lv_normalized_path = SHRITEFUH64VYIPO5I47WOOA5RJASM=>normalize_path( iv_path ).
    lv_path_len        = strlen( lv_normalized_path ).
    ls_path_parts      = SHRITEFUH64VYIPO5I47WOOA5RJASM=>split_path( lv_normalized_path ).

    READ TABLE mt_json_tree INTO ls_item
      WITH KEY path = ls_path_parts-path name = ls_path_parts-name.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CLEAR: ls_item-path, ls_item-name, ls_item-order. " this becomes a new root
    INSERT ls_item INTO TABLE lo_section->mt_json_tree.

    lv_path_pattern = lv_normalized_path && `*`.

    LOOP AT mt_json_tree INTO ls_item WHERE path CP lv_path_pattern.

      ls_item-path = substring( val = ls_item-path
                                off = lv_path_len - 1 ). " less closing '/'
      INSERT ls_item INTO TABLE lo_section->mt_json_tree.

    ENDLOOP.

    ri_json = lo_section.

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~stringify.

    rv_json = SHRITEFUH64VYIPO5I47WOOA5RNASM=>stringify(
      it_json_tree       = mt_json_tree
      iv_keep_item_order = ms_opts-keep_item_order
      iv_indent          = iv_indent ).

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~touch_array.

    DATA lr_node TYPE REF TO Lif_abapgit_ajson_types=>ty_node.
    DATA ls_deleted_node TYPE Lif_abapgit_ajson_types=>ty_node.
    DATA ls_new_node LIKE LINE OF mt_json_tree.
    DATA ls_split_path TYPE Lif_abapgit_ajson_types=>ty_path_name.

    read_only_watchdog( ).

    ls_split_path = SHRITEFUH64VYIPO5I47WOOA5RJASM=>split_path( iv_path ).
    IF ls_split_path IS INITIAL. " Assign root, exceptional processing
      ls_new_node-path = ls_split_path-path.
      ls_new_node-name = ls_split_path-name.
      ls_new_node-type = Lif_abapgit_ajson_types=>node_type-array.
      INSERT ls_new_node INTO TABLE mt_json_tree.
      RETURN.
    ENDIF.

    IF iv_clear = abap_true.
      ls_deleted_node = delete_subtree(
        iv_path = ls_split_path-path
        iv_name = ls_split_path-name ).
    ELSE.
      lr_node = get_item( iv_path ).
    ENDIF.

    IF lr_node IS INITIAL. " Or node was cleared

      DATA lr_parent TYPE REF TO Lif_abapgit_ajson_types=>ty_node.
      lr_parent = prove_path_exists( ls_split_path-path ).
      ASSERT lr_parent IS NOT INITIAL.

      lr_parent->children = lr_parent->children + 1.

      ls_new_node-path = ls_split_path-path.
      ls_new_node-name = ls_split_path-name.
      ls_new_node-type = Lif_abapgit_ajson_types=>node_type-array.

      IF ms_opts-keep_item_order = abap_true AND ls_deleted_node IS NOT INITIAL.
        ls_new_node-order = ls_deleted_node-order.
      ENDIF.

      INSERT ls_new_node INTO TABLE mt_json_tree.

    ELSEIF lr_node->type <> Lif_abapgit_ajson_types=>node_type-array.
      Lcx_abapgit_ajson_error=>raise( |Path [{ iv_path }] already used and is not array| ).
    ENDIF.

    ri_json = me.

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~to_abap.

    DATA lo_to_abap TYPE REF TO SHRITEFUH64VYIPO5I47WOOA5RPASM.

    CLEAR ev_container.
    CREATE OBJECT lo_to_abap
      EXPORTING
        iv_corresponding  = boolc( iv_corresponding = abap_true OR ms_opts-to_abap_corresponding_only = abap_true )
        ii_custom_mapping = mi_custom_mapping.

    lo_to_abap->to_abap(
      EXPORTING
        it_nodes    = Lif_abapgit_ajson~mt_json_tree
      CHANGING
        c_container = ev_container ).

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~to_abap_corresponding_only.
    ms_opts-to_abap_corresponding_only = iv_enable.
    ri_json = me.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_AJSON implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_CHDO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_chdo=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_chdo=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_CHDO implementation.
*"* method's implementations
*include methods.
  METHOD after_import.

    DATA: lt_cts_object_entry TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY,
          ls_cts_object_entry LIKE LINE OF lt_cts_object_entry,
          lt_errormsg         TYPE STANDARD TABLE OF sprot_u WITH DEFAULT KEY.

    ls_cts_object_entry-pgmid    = 'R3TR'.
    ls_cts_object_entry-object   = ms_item-obj_type.
    ls_cts_object_entry-obj_name = ms_item-obj_name.
    INSERT ls_cts_object_entry INTO TABLE lt_cts_object_entry.

    CALL FUNCTION 'AFTER_IMP_CHDO'
      EXPORTING
        iv_tarclient  = sy-mandt
        iv_is_upgrade = abap_false
      TABLES
        tt_e071       = lt_cts_object_entry
        tt_errormsg   = lt_errormsg.

    LOOP AT lt_errormsg TRANSPORTING NO FIELDS WHERE severity = 'E' OR severity = 'A'.
      EXIT.
    ENDLOOP.

    IF sy-subrc = 0.
      Lcx_abapgit_exception=>raise( 'Error from AFTER_IMP_CHDO' ).
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_object = is_item-obj_name.

  ENDMETHOD.
  METHOD delete_tadir_cdnames.

    DATA: lv_obj_name TYPE sobj_name.

    IF is_cdnames-repnamec IS NOT INITIAL.
      lv_obj_name = is_cdnames-repnamec.
      CALL FUNCTION 'TR_TADIR_INTERFACE'
        EXPORTING
          wi_delete_tadir_entry    = abap_true
          wi_tadir_pgmid           = 'R3TR'
          wi_tadir_object          = 'PROG'
          wi_tadir_obj_name        = lv_obj_name
          wi_test_modus            = abap_false
        EXCEPTIONS
          tadir_entry_not_existing = 1
          OTHERS                   = 2.
      IF sy-subrc > 1.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

    IF is_cdnames-repnamet IS NOT INITIAL.
      lv_obj_name = is_cdnames-repnamet.
      CALL FUNCTION 'TR_TADIR_INTERFACE'
        EXPORTING
          wi_delete_tadir_entry    = abap_true
          wi_tadir_pgmid           = 'R3TR'
          wi_tadir_object          = 'PROG'
          wi_tadir_obj_name        = lv_obj_name
          wi_test_modus            = abap_false
        EXCEPTIONS
          tadir_entry_not_existing = 1
          OTHERS                   = 2.
      IF sy-subrc > 1.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

    IF is_cdnames-repnamefix IS NOT INITIAL.
      lv_obj_name = is_cdnames-repnamefix.
      CALL FUNCTION 'TR_TADIR_INTERFACE'
        EXPORTING
          wi_delete_tadir_entry    = abap_true
          wi_tadir_pgmid           = 'R3TR'
          wi_tadir_object          = 'PROG'
          wi_tadir_obj_name        = lv_obj_name
          wi_test_modus            = abap_false
        EXCEPTIONS
          tadir_entry_not_existing = 1
          OTHERS                   = 2.
      IF sy-subrc > 1.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

    IF is_cdnames-repnamevar IS NOT INITIAL.
      lv_obj_name = is_cdnames-repnamevar.
      CALL FUNCTION 'TR_TADIR_INTERFACE'
        EXPORTING
          wi_delete_tadir_entry    = abap_true
          wi_tadir_pgmid           = 'R3TR'
          wi_tadir_object          = 'PROG'
          wi_tadir_obj_name        = lv_obj_name
          wi_test_modus            = abap_false
        EXCEPTIONS
          tadir_entry_not_existing = 1
          OTHERS                   = 2.
      IF sy-subrc > 1.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

    IF is_cdnames-fgrp IS NOT INITIAL.
      lv_obj_name = is_cdnames-fgrp.
      CALL FUNCTION 'TR_TADIR_INTERFACE'
        EXPORTING
          wi_delete_tadir_entry    = abap_true
          wi_tadir_pgmid           = 'R3TR'
          wi_tadir_object          = 'FUGR'
          wi_tadir_obj_name        = lv_obj_name
          wi_test_modus            = abap_false
        EXCEPTIONS
          tadir_entry_not_existing = 1
          OTHERS                   = 2.
      IF sy-subrc > 1.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD delete_tadir_tabl.

    DATA: lv_obj_name TYPE sobj_name.

    IF is_tcdrs-tabname IS NOT INITIAL.
      lv_obj_name = is_tcdrs-tabname.
      CALL FUNCTION 'TR_TADIR_INTERFACE'
        EXPORTING
          wi_delete_tadir_entry    = abap_true
          wi_tadir_pgmid           = 'R3TR'
          wi_tadir_object          = 'TABL'
          wi_tadir_obj_name        = lv_obj_name
          wi_test_modus            = abap_false
        EXCEPTIONS
          tadir_entry_not_existing = 1
          OTHERS                   = 2.
      IF sy-subrc > 1.
        Lcx_abapgit_exception=>raise( |Error from TR_TADIR_INTERFACE (subrc={ sy-subrc } ).| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE updname INTO rv_user
      FROM tcdrp
      WHERE object = mv_object.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lt_cdnames TYPE STANDARD TABLE OF cdnames,
          ls_cdnames TYPE cdnames,
          lt_tcdrs   TYPE STANDARD TABLE OF tcdrs,
          ls_tcdrs   TYPE tcdrs,
          lv_msg     TYPE symsgv.

    CALL FUNCTION 'CDNAMES_GET'
      EXPORTING
        iv_object        = mv_object
      TABLES
        it_tcdrs         = lt_tcdrs
        it_names         = lt_cdnames
      EXCEPTIONS
        object_space     = 1
        object_not_found = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'CHDO_DELETE'
      EXPORTING
        iv_object        = mv_object
        iv_with_tadir    = abap_true
      EXCEPTIONS
        object_is_space  = 1
        object_not_found = 2
        other_error      = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      lv_msg = mv_object.
      Lcx_abapgit_exception=>raise_t100( iv_msgid = 'CD'
                                         iv_msgno = '869'
                                         iv_msgv1 = lv_msg ).
    ENDIF.

    LOOP AT lt_cdnames INTO ls_cdnames.
      delete_tadir_cdnames( ls_cdnames ).
    ENDLOOP.

    LOOP AT lt_tcdrs INTO ls_tcdrs.
      delete_tadir_tabl( ls_tcdrs ).
    ENDLOOP.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_change_object TYPE ty_change_document.
    FIELD-SYMBOLS: <ls_report_generated> LIKE LINE OF ls_change_object-reports_generated.

    io_xml->read( EXPORTING iv_name = 'CHDO'
                  CHANGING  cg_data = ls_change_object ).

    DELETE FROM tcdobs  WHERE object = mv_object.
    DELETE FROM tcdobts WHERE object = mv_object.
    DELETE FROM tcdrps  WHERE object = mv_object.

    LOOP AT ls_change_object-reports_generated ASSIGNING <ls_report_generated>.
      <ls_report_generated>-devclass = iv_package.
    ENDLOOP.

    INSERT tcdobs  FROM TABLE ls_change_object-objects.
    INSERT tcdobts FROM TABLE ls_change_object-objects_text.
    INSERT tcdrps  FROM TABLE ls_change_object-reports_generated.

    tadir_insert( iv_package ).

    after_import( ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    SELECT COUNT(*)
      FROM tcdrp
      WHERE object = mv_object.

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

    DATA: lt_bdcdata TYPE STANDARD TABLE OF bdcdata,
          ls_bdcdata LIKE LINE OF lt_bdcdata.

    CLEAR: ls_bdcdata.
    ls_bdcdata-program  = 'SAPMSCDO_NEW'.
    ls_bdcdata-dynpro   = '0100'.
    ls_bdcdata-dynbegin = abap_true.
    APPEND ls_bdcdata TO lt_bdcdata.

    CLEAR: ls_bdcdata.
    ls_bdcdata-fnam = 'TCDOB-OBJECT'.
    ls_bdcdata-fval = mv_object.
    APPEND ls_bdcdata TO lt_bdcdata.

    CLEAR: ls_bdcdata.
    ls_bdcdata-fnam = 'BDC_OKCODE'.
    ls_bdcdata-fval = '=DISP'.
    APPEND ls_bdcdata TO lt_bdcdata.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SCDO'
      it_bdcdata = lt_bdcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_change_object TYPE ty_change_document,
          lt_tcdrp         TYPE STANDARD TABLE OF tcdrp,
          lt_tcdob         TYPE STANDARD TABLE OF tcdob,
          lt_tcdobt        TYPE STANDARD TABLE OF tcdobt,
          BEGIN OF ls_nulldatetime, " hack ro reset fields when they exist without syntax errors when they don't
            udate TYPE sy-datum,
            utime TYPE sy-uzeit,
          END OF ls_nulldatetime.

    FIELD-SYMBOLS: <ls_reports_generated> LIKE LINE OF ls_change_object-reports_generated,
                   <ls_objects>           LIKE LINE OF ls_change_object-objects,
                   <ls_objects_text>      LIKE LINE OF ls_change_object-objects_text.

    CALL FUNCTION 'CDNAMES_GET'
      EXPORTING
        iv_object        = mv_object
      TABLES
        it_tcdrp         = lt_tcdrp
        it_tcdob         = lt_tcdob
        it_tcdobt        = lt_tcdobt
      EXCEPTIONS
        object_space     = 1
        object_not_found = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    ls_change_object-reports_generated = lt_tcdrp.
    ls_change_object-objects           = lt_tcdob.
    ls_change_object-objects_text      = lt_tcdobt.

    " At import, when CHDO is generated date & time change, so always detects changes for this fields
    LOOP AT ls_change_object-reports_generated ASSIGNING <ls_reports_generated>.
      CLEAR: <ls_reports_generated>-datum, <ls_reports_generated>-uzeit,
             <ls_reports_generated>-author, <ls_reports_generated>-updname,
             <ls_reports_generated>-devclass.
    ENDLOOP.

    LOOP AT ls_change_object-objects ASSIGNING <ls_objects>.
      MOVE-CORRESPONDING ls_nulldatetime TO <ls_objects>. " reset date and time
    ENDLOOP.

    LOOP AT ls_change_object-objects_text ASSIGNING <ls_objects_text>.
      MOVE-CORRESPONDING ls_nulldatetime TO <ls_objects_text>. " reset date and time
    ENDLOOP.

    io_xml->add( iv_name = 'CHDO'
                 ig_data = ls_change_object ).

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
endclass. "ZCL_ABAPGIT_OBJECT_CHDO implementation

*>>>>>>> ZCL_ABAPGIT_AJSON_FILTER_LIB <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ajson_filter_lib==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ajson_filter_lib==ccimp.
**********************************************************************
*  FILTER EMPTY VALUES
**********************************************************************

CLASS SHRITEFUH64VYIPO5I47WOOA5SNASM DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_ajson_filter.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5I47WOOA5SNASM IMPLEMENTATION.
  METHOD Lif_abapgit_ajson_filter~keep_node.

    rv_keep = boolc(
      ( iv_visit = Lif_abapgit_ajson_filter=>visit_type-value AND is_node-value IS NOT INITIAL ) OR
      ( iv_visit <> Lif_abapgit_ajson_filter=>visit_type-value AND is_node-children > 0 ) ).
    " children = 0 on open for initially empty nodes and on close for filtered ones

  ENDMETHOD.
ENDCLASS.

**********************************************************************
*  FILTER PREDEFINED PATHS
**********************************************************************

CLASS SHRITEFUH64VYIPO5I47WOOA5SPASM DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_ajson_filter.
    METHODS constructor
      IMPORTING
        it_skip_paths TYPE string_table OPTIONAL
        iv_skip_paths TYPE string OPTIONAL
        iv_pattern_search TYPE abap_bool
      RAISING
        Lcx_abapgit_ajson_error.
  PRIVATE SECTION.
    DATA mt_skip_paths TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
    DATA mv_pattern_search TYPE abap_bool.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5I47WOOA5SPASM IMPLEMENTATION.

  METHOD Lif_abapgit_ajson_filter~keep_node.

    DATA lv_full_path TYPE string.
    FIELD-SYMBOLS <p> LIKE LINE OF mt_skip_paths.

    lv_full_path = is_node-path && is_node-name.

    IF mv_pattern_search = abap_true.
      rv_keep = abap_true.
      LOOP AT mt_skip_paths ASSIGNING <p>.
        IF lv_full_path CP <p>.
          rv_keep = abap_false.
          EXIT.
        ENDIF.
      ENDLOOP.
    ELSE.
      READ TABLE mt_skip_paths WITH KEY table_line = lv_full_path TRANSPORTING NO FIELDS.
      rv_keep = boolc( sy-subrc <> 0 ).
    ENDIF.

  ENDMETHOD.

  METHOD constructor.

    DATA lv_s TYPE string.
    DATA lt_tab TYPE string_table.
    FIELD-SYMBOLS <s> TYPE string.

    IF boolc( iv_skip_paths IS INITIAL ) = boolc( it_skip_paths IS INITIAL ). " XOR
      Lcx_abapgit_ajson_error=>raise( 'no filter path specified' ).
    ENDIF.

    LOOP AT it_skip_paths INTO lv_s.
      lv_s = to_lower( lv_s ).
      APPEND lv_s TO lt_tab.
    ENDLOOP.

    IF iv_skip_paths IS NOT INITIAL.
      SPLIT iv_skip_paths AT ',' INTO TABLE lt_tab.
      LOOP AT lt_tab ASSIGNING <s>.
        IF <s> IS INITIAL.
          DELETE lt_tab INDEX sy-tabix.
          CONTINUE.
        ENDIF.
        <s> = condense( to_lower( <s> ) ).
      ENDLOOP.
    ENDIF.

    SORT lt_tab BY table_line.
    DELETE ADJACENT DUPLICATES FROM lt_tab.

    mt_skip_paths = lt_tab.
    mv_pattern_search = iv_pattern_search.

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* MULTI FILTER
**********************************************************************

CLASS SHRITEFUH64VYIPO5I47WOOA5SRASM DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_ajson_filter.
    METHODS constructor
      IMPORTING
        it_filters TYPE Lif_abapgit_ajson_filter=>ty_filter_tab
      RAISING
        Lcx_abapgit_ajson_error.
  PRIVATE SECTION.
    DATA mt_filters TYPE Lif_abapgit_ajson_filter=>ty_filter_tab.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5I47WOOA5SRASM IMPLEMENTATION.

  METHOD Lif_abapgit_ajson_filter~keep_node.

    DATA li_filter LIKE LINE OF mt_filters.

    rv_keep = abap_true.
    LOOP AT mt_filters INTO li_filter.
      rv_keep = li_filter->keep_node(
        is_node  = is_node
        iv_visit = iv_visit ).
      IF rv_keep = abap_false.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD constructor.

    DATA li_filter LIKE LINE OF it_filters.

    LOOP AT it_filters INTO li_filter WHERE table_line IS BOUND.
      APPEND li_filter TO mt_filters.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

*"* test class
*include zcl_abapgit_ajson_filter_lib==ccau.



class LCL_ABAPGIT_AJSON_FILTER_LIB implementation.
*"* method's implementations
*include methods.
  METHOD create_and_filter.
    CREATE OBJECT ri_filter TYPE SHRITEFUH64VYIPO5I47WOOA5SRASM
      EXPORTING
        it_filters = it_filters.
  ENDMETHOD.
  METHOD create_empty_filter.
    CREATE OBJECT ri_filter TYPE SHRITEFUH64VYIPO5I47WOOA5SNASM.
  ENDMETHOD.
  METHOD create_path_filter.
    CREATE OBJECT ri_filter TYPE SHRITEFUH64VYIPO5I47WOOA5SPASM
      EXPORTING
        iv_pattern_search = iv_pattern_search
        it_skip_paths = it_skip_paths
        iv_skip_paths = iv_skip_paths.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_AJSON_FILTER_LIB implementation

*>>>>>>> ZCL_ABAPGIT_AJSON_MAPPING <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ajson_mapping=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ajson_mapping=====ccimp.
CLASS SHRITEFUH64VYIPO5I47WOOA5SVASM IMPLEMENTATION. "DEPRECATED


  METHOD constructor.

    DATA ls_mapping_field LIKE LINE OF mt_mapping_fields.

    LOOP AT it_mapping_fields INTO ls_mapping_field.
      ls_mapping_field-abap = to_upper( ls_mapping_field-abap ).
      INSERT ls_mapping_field INTO TABLE mt_mapping_fields.
    ENDLOOP.

  ENDMETHOD.


  METHOD Lif_abapgit_ajson_mapping~to_abap.

    DATA ls_mapping_field LIKE LINE OF mt_mapping_fields.

    READ TABLE mt_mapping_fields INTO ls_mapping_field
      WITH KEY json COMPONENTS json = iv_name.
    IF sy-subrc = 0.
      rv_result = ls_mapping_field-abap.
    ENDIF.

  ENDMETHOD.


  METHOD Lif_abapgit_ajson_mapping~to_json.

    DATA lv_field TYPE string.
    DATA ls_mapping_field LIKE LINE OF mt_mapping_fields.

    lv_field = to_upper( iv_name ).

    READ TABLE mt_mapping_fields INTO ls_mapping_field
      WITH KEY abap COMPONENTS abap = lv_field.
    IF sy-subrc = 0.
      rv_result = ls_mapping_field-json.
    ENDIF.

  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~rename_node.

  ENDMETHOD.

ENDCLASS.

CLASS SHRITEFUH64VYIPO5I47WOOA5SWASM IMPLEMENTATION.

  METHOD constructor.
    mt_rename_map = it_rename_map.
    mv_rename_by = iv_rename_by.
  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~to_abap.
  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~to_json.
  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~rename_node.

    DATA lv_full_path TYPE string.
    DATA lv_pair_found TYPE abap_bool.
    FIELD-SYMBOLS <r> LIKE LINE OF mt_rename_map.

    CASE mv_rename_by.
      WHEN Lcl_abapgit_ajson_mapping=>rename_by-attr_name.
        READ TABLE mt_rename_map ASSIGNING <r> WITH TABLE KEY by_name COMPONENTS from = cv_name.
        lv_pair_found = boolc( sy-subrc = 0 ).
      WHEN Lcl_abapgit_ajson_mapping=>rename_by-full_path.
        lv_full_path = is_node-path && cv_name.
        READ TABLE mt_rename_map ASSIGNING <r> WITH TABLE KEY by_name COMPONENTS from = lv_full_path.
        lv_pair_found = boolc( sy-subrc = 0 ).
      WHEN Lcl_abapgit_ajson_mapping=>rename_by-pattern.
        lv_full_path = is_node-path && cv_name.
        LOOP AT mt_rename_map ASSIGNING <r>.
          IF lv_full_path CP <r>-from.
            lv_pair_found = abap_true.
            EXIT.
          ENDIF.
        ENDLOOP.
      WHEN OTHERS.
        lv_pair_found = abap_false. " No rename
    ENDCASE.

    IF lv_pair_found = abap_true.
      cv_name = <r>-to.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS SHRITEFUH64VYIPO5I47WOOA5SXASM IMPLEMENTATION.


  METHOD constructor.

    mi_mapping_fields = Lcl_abapgit_ajson_mapping=>create_field_mapping( it_mapping_fields ).

  ENDMETHOD.


  METHOD Lif_abapgit_ajson_mapping~to_abap.

    rv_result = mi_mapping_fields->to_abap( iv_path = iv_path
                                            iv_name = iv_name ).

  ENDMETHOD.


  METHOD Lif_abapgit_ajson_mapping~to_json.

    rv_result = mi_mapping_fields->to_json( iv_path = iv_path
                                            iv_name = iv_name ).

    IF rv_result IS NOT INITIAL. " Mapping found
      RETURN.
    ENDIF.

    rv_result = to_upper( iv_name ).

  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~rename_node.

    cv_name = to_upper( cv_name ).

  ENDMETHOD.

ENDCLASS.


CLASS SHRITEFUH64VYIPO5I47WOOA5SYASM IMPLEMENTATION.


  METHOD constructor.

    mi_mapping_fields = Lcl_abapgit_ajson_mapping=>create_field_mapping( it_mapping_fields ).

  ENDMETHOD.


  METHOD Lif_abapgit_ajson_mapping~to_abap.

    rv_result = mi_mapping_fields->to_abap( iv_path = iv_path
                                            iv_name = iv_name ).

  ENDMETHOD.


  METHOD Lif_abapgit_ajson_mapping~to_json.

    rv_result = mi_mapping_fields->to_json( iv_path = iv_path
                                            iv_name = iv_name ).

    IF rv_result IS NOT INITIAL. " Mapping found
      RETURN.
    ENDIF.

    rv_result = to_lower( iv_name ).

  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~rename_node.

    cv_name = to_lower( cv_name ).

  ENDMETHOD.

ENDCLASS.


CLASS SHRITEFUH64VYIPO5I47WOOA5SZASM IMPLEMENTATION. "DEPRECATED


  METHOD constructor.

    mi_mapping_fields   = Lcl_abapgit_ajson_mapping=>create_field_mapping( it_mapping_fields ).
    mv_first_json_upper = iv_first_json_upper.

  ENDMETHOD.


  METHOD Lif_abapgit_ajson_mapping~to_abap.

    rv_result = mi_mapping_fields->to_abap( iv_path = iv_path
                                            iv_name = iv_name ).

    IF rv_result IS NOT INITIAL. " Mapping found
      RETURN.
    ENDIF.

    rv_result = iv_name.

    REPLACE ALL OCCURRENCES OF REGEX `([a-z])([A-Z])` IN rv_result WITH `$1_$2`.

  ENDMETHOD.


  METHOD Lif_abapgit_ajson_mapping~to_json.

    TYPES ty_token TYPE c LENGTH 255.
    DATA lt_tokens TYPE STANDARD TABLE OF ty_token.
    DATA lv_from TYPE i.
    FIELD-SYMBOLS <token> LIKE LINE OF lt_tokens.

    rv_result = mi_mapping_fields->to_json( iv_path = iv_path
                                            iv_name = iv_name ).

    IF rv_result IS NOT INITIAL. " Mapping found
      RETURN.
    ENDIF.

    rv_result = iv_name.

    REPLACE ALL OCCURRENCES OF `__` IN rv_result WITH `*`.

    TRANSLATE rv_result TO LOWER CASE.
    TRANSLATE rv_result USING `/_:_~_`.

    IF mv_first_json_upper = abap_true.
      lv_from = 1.
    ELSE.
      lv_from = 2.
    ENDIF.

    SPLIT rv_result AT `_` INTO TABLE lt_tokens.
    LOOP AT lt_tokens ASSIGNING <token> FROM lv_from.
      TRANSLATE <token>(1) TO UPPER CASE.
    ENDLOOP.

    CONCATENATE LINES OF lt_tokens INTO rv_result.
    REPLACE ALL OCCURRENCES OF `*` IN rv_result WITH `_`.

  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~rename_node.

  ENDMETHOD.

ENDCLASS.

CLASS SHRITEFUH64VYIPO5I47WOOA5S2ASM IMPLEMENTATION.

  METHOD constructor.
    mt_queue = it_queue.
  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~rename_node.

    DATA ls_node LIKE is_node.
    DATA li_mapper LIKE LINE OF mt_queue.

    ls_node = is_node.

    LOOP AT mt_queue INTO li_mapper.
      li_mapper->rename_node(
        EXPORTING
          is_node = ls_node
        CHANGING
          cv_name = cv_name ).
      ls_node-name = cv_name.
    ENDLOOP.

  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~to_abap.

  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~to_json.

  ENDMETHOD.

ENDCLASS.

CLASS SHRITEFUH64VYIPO5I47WOOA5S3ASM IMPLEMENTATION.

  METHOD Lif_abapgit_ajson_mapping~rename_node.

    REPLACE ALL OCCURRENCES OF REGEX `([a-z])([A-Z])` IN cv_name WITH `$1_$2`.
    cv_name = to_lower( cv_name ).

  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~to_abap.

  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~to_json.

  ENDMETHOD.

ENDCLASS.

CLASS SHRITEFUH64VYIPO5I47WOOA5S4ASM IMPLEMENTATION.

  METHOD constructor.
    mv_first_json_upper = iv_first_json_upper.
  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~rename_node.

    TYPES lty_token TYPE c LENGTH 255.
    CONSTANTS lc_forced_underscore_marker TYPE c LENGTH 1 VALUE cl_abap_char_utilities=>horizontal_tab.

    DATA lt_tokens TYPE STANDARD TABLE OF lty_token.
    DATA lv_from TYPE i.
    FIELD-SYMBOLS <token> LIKE LINE OF lt_tokens.

    IF mv_first_json_upper = abap_true.
      lv_from = 1.
    ELSE.
      lv_from = 2.
    ENDIF.
    REPLACE ALL OCCURRENCES OF `__` IN cv_name WITH lc_forced_underscore_marker. " Force underscore

    SPLIT cv_name AT `_` INTO TABLE lt_tokens.
    DELETE lt_tokens WHERE table_line IS INITIAL.
    LOOP AT lt_tokens ASSIGNING <token> FROM lv_from.
      TRANSLATE <token>+0(1) TO UPPER CASE.
    ENDLOOP.

    CONCATENATE LINES OF lt_tokens INTO cv_name.
    REPLACE ALL OCCURRENCES OF lc_forced_underscore_marker IN cv_name WITH `_`.

  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~to_abap.

  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~to_json.

  ENDMETHOD.

ENDCLASS.

*"* test class
*include zcl_abapgit_ajson_mapping=====ccau.


















class LCL_ABAPGIT_AJSON_MAPPING implementation.
*"* method's implementations
*include methods.
  METHOD create_camel_case.

    CREATE OBJECT ri_mapping TYPE SHRITEFUH64VYIPO5I47WOOA5SZASM
      EXPORTING
        it_mapping_fields   = it_mapping_fields
        iv_first_json_upper = iv_first_json_upper.

  ENDMETHOD.
  METHOD create_compound_mapper.

    DATA lt_queue TYPE Lif_abapgit_ajson_mapping=>ty_table_of.

    APPEND ii_mapper1 TO lt_queue.
    APPEND ii_mapper2 TO lt_queue.
    APPEND ii_mapper3 TO lt_queue.
    APPEND LINES OF it_more TO lt_queue.
    DELETE lt_queue WHERE table_line IS INITIAL.

    CREATE OBJECT ri_mapping TYPE SHRITEFUH64VYIPO5I47WOOA5S2ASM
      EXPORTING
        it_queue = lt_queue.

  ENDMETHOD.
  METHOD create_field_mapping.

    CREATE OBJECT ri_mapping TYPE SHRITEFUH64VYIPO5I47WOOA5SVASM
      EXPORTING
        it_mapping_fields = it_mapping_fields.

  ENDMETHOD.
  METHOD create_lower_case.

    CREATE OBJECT ri_mapping TYPE SHRITEFUH64VYIPO5I47WOOA5SYASM
      EXPORTING
        it_mapping_fields = it_mapping_fields.

  ENDMETHOD.
  METHOD create_rename.

    CREATE OBJECT ri_mapping TYPE SHRITEFUH64VYIPO5I47WOOA5SWASM
      EXPORTING
        it_rename_map = it_rename_map
        iv_rename_by = iv_rename_by.

  ENDMETHOD.
  METHOD create_to_camel_case.

    CREATE OBJECT ri_mapping TYPE SHRITEFUH64VYIPO5I47WOOA5S4ASM
      EXPORTING
        iv_first_json_upper = iv_first_json_upper.

  ENDMETHOD.
  METHOD create_to_snake_case.

    CREATE OBJECT ri_mapping TYPE SHRITEFUH64VYIPO5I47WOOA5S3ASM.

  ENDMETHOD.
  METHOD create_upper_case.

    CREATE OBJECT ri_mapping TYPE SHRITEFUH64VYIPO5I47WOOA5SXASM
      EXPORTING
        it_mapping_fields = it_mapping_fields.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_AJSON_MAPPING implementation

*>>>>>>> ZCL_ABAPGIT_APACK_MIGRATION <<<<<<<*

*"* macro definitions
*include zcl_abapgit_apack_migration===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_apack_migration===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_APACK_MIGRATION implementation.
*"* method's implementations
*include methods.
  METHOD add_interface_source.
    DATA: lo_factory     TYPE REF TO object,
          lo_source      TYPE REF TO object,
          lt_source_code TYPE Lif_abapgit_definitions=>ty_string_tt.

    "Buffer needs to be refreshed,
    "otherwise standard SAP CLIF_SOURCE reorder methods alphabetically
    CALL FUNCTION 'SEO_BUFFER_INIT'.
    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        cifkey  = is_clskey
        version = seoc_version_inactive.

    TRY.
        CALL METHOD ('CL_OO_FACTORY')=>('CREATE_INSTANCE')
          RECEIVING
            result = lo_factory.

        CALL METHOD lo_factory->('CREATE_CLIF_SOURCE')
          EXPORTING
            clif_name = is_clskey-clsname
          RECEIVING
            result    = lo_source.

        TRY.
            CALL METHOD lo_source->('IF_OO_CLIF_SOURCE~LOCK').
          CATCH cx_oo_access_permission.
            Lcx_abapgit_exception=>raise( 'source_new, access permission exception' ).
        ENDTRY.

        lt_source_code = get_interface_source( ).

        CALL METHOD lo_source->('IF_OO_CLIF_SOURCE~SET_SOURCE')
          EXPORTING
            source = lt_source_code.

        CALL METHOD lo_source->('IF_OO_CLIF_SOURCE~SAVE').
        CALL METHOD lo_source->('IF_OO_CLIF_SOURCE~UNLOCK').

      CATCH cx_sy_dyn_call_error.
        add_interface_source_classic( is_clskey ).
    ENDTRY.

  ENDMETHOD.
  METHOD add_interface_source_classic.
    DATA: lo_source      TYPE REF TO object,
          lt_source_code TYPE Lif_abapgit_definitions=>ty_string_tt.

    CREATE OBJECT lo_source TYPE ('CL_OO_SOURCE')
      EXPORTING
        clskey             = is_clskey
      EXCEPTIONS
        class_not_existing = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from CL_OO_SOURCE' ).
    ENDIF.

    TRY.
        CALL METHOD lo_source->('ACCESS_PERMISSION')
          EXPORTING
            access_mode = seok_access_modify.
        lt_source_code = get_interface_source( ).
        CALL METHOD lo_source->('SET_SOURCE')
          EXPORTING
            i_source = lt_source_code.
        CALL METHOD lo_source->('SAVE').
        CALL METHOD lo_source->('ACCESS_PERMISSION')
          EXPORTING
            access_mode = seok_access_free.
      CATCH cx_oo_access_permission.
        Lcx_abapgit_exception=>raise( 'permission error' ).
      CATCH cx_oo_source_save_failure.
        Lcx_abapgit_exception=>raise( 'save failure' ).
    ENDTRY.
  ENDMETHOD.
  METHOD add_intf_source_and_activate.

    DATA: ls_clskey           TYPE seoclskey,
          ls_inactive_object  TYPE dwinactiv,
          lt_inactive_objects TYPE TABLE OF dwinactiv.

    ls_clskey-clsname = Lif_abapgit_apack_definitions=>c_apack_interface_cust.

    add_interface_source( ls_clskey ).

    ls_inactive_object-object   = 'INTF'.
    ls_inactive_object-obj_name = Lif_abapgit_apack_definitions=>c_apack_interface_cust.
    INSERT ls_inactive_object INTO TABLE lt_inactive_objects.

    CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
      TABLES
        objects                = lt_inactive_objects
      EXCEPTIONS
        excecution_error       = 1
        cancelled              = 2
        insert_into_corr_error = 3
        OTHERS                 = 4.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD create_interface.

    DATA: ls_interface_properties TYPE vseointerf.

    ls_interface_properties-clsname  = Lif_abapgit_apack_definitions=>c_apack_interface_cust.
    ls_interface_properties-version  = '1'.
    ls_interface_properties-langu    = 'E'.
    ls_interface_properties-descript = 'APACK: Manifest interface'.
    ls_interface_properties-exposure = '2'.
    ls_interface_properties-state    = '1'.
    ls_interface_properties-unicode  = abap_true.

    TRY.
        CALL FUNCTION 'SEO_INTERFACE_CREATE_COMPLETE'
          EXPORTING
            devclass        = '$TMP'
            suppress_dialog = abap_true " Parameter missing in 702
          CHANGING
            interface       = ls_interface_properties
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
            devclass        = '$TMP'
          CHANGING
            interface       = ls_interface_properties
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

    add_intf_source_and_activate( ).

  ENDMETHOD.
  METHOD get_interface_source.

    INSERT `INTERFACE zif_apack_manifest PUBLIC.` INTO TABLE rt_source.
    INSERT `` INTO TABLE rt_source.
    INSERT `  TYPES: BEGIN OF ty_dependency,` INTO TABLE rt_source.
    INSERT `           group_id       TYPE string,` INTO TABLE rt_source.
    INSERT `           artifact_id    TYPE string,` INTO TABLE rt_source.
    INSERT `           version        TYPE string,` INTO TABLE rt_source.
    INSERT `           git_url        TYPE string,` INTO TABLE rt_source.
    INSERT `           target_package TYPE devclass,` INTO TABLE rt_source.
    INSERT `         END OF ty_dependency,` INTO TABLE rt_source.
    INSERT `         ty_dependencies    TYPE STANDARD TABLE OF ty_dependency` INTO TABLE rt_source.
    INSERT `                            WITH NON-UNIQUE DEFAULT KEY,` INTO TABLE rt_source.
    INSERT `         ty_repository_type TYPE string,` INTO TABLE rt_source.
    INSERT `         BEGIN OF ty_descriptor,` INTO TABLE rt_source.
    INSERT `           group_id        TYPE string,` INTO TABLE rt_source.
    INSERT `           artifact_id     TYPE string,` INTO TABLE rt_source.
    INSERT `           version         TYPE string,` INTO TABLE rt_source.
    INSERT `           repository_type TYPE ty_repository_type,` INTO TABLE rt_source.
    INSERT `           git_url         TYPE string,` INTO TABLE rt_source.
    INSERT `           dependencies    TYPE ty_dependencies,` INTO TABLE rt_source.
    INSERT `         END OF ty_descriptor.` INTO TABLE rt_source.
    INSERT `` INTO TABLE rt_source.
    INSERT `  CONSTANTS: co_file_name         TYPE string VALUE '.apack-manifest.xml',` INTO TABLE rt_source.
    INSERT `             co_abap_git          TYPE ty_repository_type VALUE 'abapGit',` INTO TABLE rt_source.
    INSERT `             co_interface_version TYPE i VALUE 1.` INTO TABLE rt_source.
    INSERT `` INTO TABLE rt_source.
    INSERT `  DATA: descriptor TYPE ty_descriptor READ-ONLY.` INTO TABLE rt_source.
    INSERT `` INTO TABLE rt_source.
    INSERT `ENDINTERFACE.` INTO TABLE rt_source.

  ENDMETHOD.
  METHOD interface_exists.

    DATA: lv_interface_name TYPE seoclsname.

    SELECT SINGLE clsname FROM seoclass INTO lv_interface_name
      WHERE clsname = Lif_abapgit_apack_definitions=>c_apack_interface_cust.
    rv_interface_exists = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD interface_valid.

    FIELD-SYMBOLS: <lv_interface_vers> TYPE i.

    ASSIGN (Lif_abapgit_apack_definitions=>c_apack_interface_cust)=>('CO_INTERFACE_VERSION') TO <lv_interface_vers>.
    rv_interface_valid = boolc( <lv_interface_vers> IS ASSIGNED
      AND <lv_interface_vers> >= c_apack_interface_version ).

  ENDMETHOD.
  METHOD perform_migration.

    IF interface_exists( ) = abap_false.
      create_interface( ).
    ELSEIF interface_valid( ) = abap_false.
      add_intf_source_and_activate( ).
    ENDIF.

  ENDMETHOD.
  METHOD run.

    DATA: lo_apack_migration TYPE REF TO Lcl_abapgit_apack_migration.

    CREATE OBJECT lo_apack_migration.
    lo_apack_migration->perform_migration( ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_APACK_MIGRATION implementation

*>>>>>>> ZCL_ABAPGIT_APACK_WRITER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_apack_writer======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_apack_writer======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_apack_writer======ccau.
*"* use this source file for your ABAP unit test classes


class LCL_ABAPGIT_APACK_WRITER implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    ms_manifest_descriptor = is_apack_manifest_descriptor.
  ENDMETHOD.
  METHOD create_instance.
    CREATE OBJECT ro_manifest_writer EXPORTING is_apack_manifest_descriptor = is_apack_manifest_descriptor.
  ENDMETHOD.
  METHOD serialize.

    DATA: ls_manifest_descriptor LIKE ms_manifest_descriptor.
    FIELD-SYMBOLS: <ls_dependency> LIKE LINE OF ls_manifest_descriptor-dependencies.

    " Setting repository type automatically to 'abapGit' as there is no other one right now
    ms_manifest_descriptor-repository_type = Lif_abapgit_apack_definitions=>c_repository_type_abapgit.

    ls_manifest_descriptor = ms_manifest_descriptor.
    CLEAR: ls_manifest_descriptor-sem_version.

    LOOP AT ls_manifest_descriptor-dependencies ASSIGNING <ls_dependency>.
      CLEAR: <ls_dependency>-sem_version.
    ENDLOOP.

    CALL TRANSFORMATION id
      OPTIONS initial_components = 'suppress'
      SOURCE data = ls_manifest_descriptor
      RESULT XML rv_xml.

    rv_xml = Lcl_abapgit_xml_pretty=>print( rv_xml ).

    REPLACE FIRST OCCURRENCE
      OF REGEX '<\?xml version="1\.0" encoding="[\w-]+"\?>'
      IN rv_xml
      WITH '<?xml version="1.0" encoding="utf-8"?>'.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_APACK_WRITER implementation

*>>>>>>> ZCL_ABAPGIT_BACKGROUND_PULL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_background_pull===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_background_pull===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_BACKGROUND_PULL implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_background~get_description.

    rv_description = 'Automatic pull'.

  ENDMETHOD.
  METHOD Lif_abapgit_background~get_settings.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_background~run.

    DATA: ls_checks TYPE Lif_abapgit_definitions=>ty_deserialize_checks.

    FIELD-SYMBOLS: <ls_overwrite> LIKE LINE OF ls_checks-overwrite.


    ls_checks = io_repo->deserialize_checks( ).

    LOOP AT ls_checks-overwrite ASSIGNING <ls_overwrite>.
      <ls_overwrite>-decision = Lif_abapgit_definitions=>c_yes.
    ENDLOOP.

    io_repo->deserialize( is_checks = ls_checks
                          ii_log    = ii_log ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_BACKGROUND_PULL implementation

*>>>>>>> ZCL_ABAPGIT_BACKGROUND_PUSH_FI <<<<<<<*

*"* macro definitions
*include zcl_abapgit_background_push_ficcmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_background_push_ficcimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_BACKGROUND_PUSH_FI implementation.
*"* method's implementations
*include methods.
  METHOD build_comment.

    DATA: lt_objects TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
          lv_str     TYPE string.

    FIELD-SYMBOLS: <ls_local> LIKE LINE OF is_files-local.


    LOOP AT is_files-local ASSIGNING <ls_local>.
      lv_str = |{ <ls_local>-item-obj_type } { <ls_local>-item-obj_name }|.
      APPEND lv_str TO lt_objects.
    ENDLOOP.

    SORT lt_objects AS TEXT.
    DELETE ADJACENT DUPLICATES FROM lt_objects.

    IF lines( lt_objects ) = 1.
      rv_comment = |BG: { lv_str }|.
    ELSE.
      rv_comment = 'BG: Multiple objects'.
      LOOP AT lt_objects INTO lv_str.
        CONCATENATE rv_comment cl_abap_char_utilities=>newline lv_str INTO rv_comment.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD push_fixed.

    DATA: ls_comment TYPE Lif_abapgit_git_definitions=>ty_comment,
          ls_files   TYPE Lif_abapgit_definitions=>ty_stage_files,
          lo_stage   TYPE REF TO Lcl_abapgit_stage.

    FIELD-SYMBOLS: <ls_local>  LIKE LINE OF ls_files-local,
                   <ls_remote> LIKE LINE OF ls_files-remote.


    ls_files = Lcl_abapgit_factory=>get_stage_logic( )->get( io_repo ).
    ASSERT lines( ls_files-local ) > 0
        OR lines( ls_files-remote ) > 0.

    CREATE OBJECT lo_stage.

    LOOP AT ls_files-local ASSIGNING <ls_local>.
      mi_log->add_info( |stage: { <ls_local>-file-path } { <ls_local>-file-filename }| ).
      lo_stage->add( iv_path     = <ls_local>-file-path
                     iv_filename = <ls_local>-file-filename
                     iv_data     = <ls_local>-file-data ).
    ENDLOOP.

    LOOP AT ls_files-remote ASSIGNING <ls_remote>.

      mi_log->add_info( |removed: { <ls_remote>-path } { <ls_remote>-filename }| ).

      lo_stage->rm( iv_path     = <ls_remote>-path
                    iv_filename = <ls_remote>-filename ).

    ENDLOOP.

    ls_comment-committer-name  = iv_name.
    ls_comment-committer-email = iv_email.
    ls_comment-comment         = build_comment( ls_files ).

    io_repo->push( is_comment = ls_comment
                   io_stage   = lo_stage ).

  ENDMETHOD.
  METHOD Lif_abapgit_background~get_description.

    rv_description = 'Automatic push, fixed author'.

  ENDMETHOD.
  METHOD Lif_abapgit_background~get_settings.

    DATA: ls_setting LIKE LINE OF ct_settings.


    READ TABLE ct_settings WITH KEY key = c_settings-name INTO ls_setting.
    IF sy-subrc <> 0.
      ls_setting-key = c_settings-name.
      ls_setting-value = 'foobar'.
      APPEND ls_setting TO ct_settings.
    ENDIF.

    READ TABLE ct_settings WITH KEY key = c_settings-email INTO ls_setting.
    IF sy-subrc <> 0.
      ls_setting-key = c_settings-email.
      ls_setting-value = 'foobar@localhost'.
      APPEND ls_setting TO ct_settings.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_background~run.

    DATA: ls_files   TYPE Lif_abapgit_definitions=>ty_stage_files,
          ls_setting LIKE LINE OF it_settings,
          lv_name    TYPE string,
          lv_email   TYPE string.

    mi_log = ii_log.
    ls_files = Lcl_abapgit_factory=>get_stage_logic( )->get( io_repo ).

    IF lines( ls_files-local ) = 0 AND lines( ls_files-remote ) = 0.
      ii_log->add_info( 'Nothing to stage' ).
      RETURN.
    ENDIF.

    READ TABLE it_settings WITH KEY key = c_settings-name INTO ls_setting. "#EC CI_SUBRC
    lv_name = ls_setting-value.

    READ TABLE it_settings WITH KEY key = c_settings-email INTO ls_setting. "#EC CI_SUBRC
    lv_email = ls_setting-value.

    push_fixed(
      io_repo  = io_repo
      iv_name  = lv_name
      iv_email = lv_email ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_BACKGROUND_PUSH_FI implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_GSMP <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_gsmp=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_gsmp=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_GSMP implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    DATA lv_name TYPE c LENGTH 180.
    DATA lv_user  TYPE string.
    DATA lx_root TYPE REF TO cx_root.


    TRY.
        lv_name = ms_item-obj_name.

        SELECT SINGLE changed_by INTO lv_user
          FROM ('GSM_MD_PRV_W')
          WHERE provider_id = lv_name AND version = 'I'.

        IF lv_user IS INITIAL.
          SELECT SINGLE changed_by INTO lv_user
            FROM ('GSM_MD_PRV_W')
            WHERE provider_id = lv_name AND version = 'A'.
        ENDIF.

        rv_user = lv_user.
      CATCH cx_root INTO lx_root.
        Lcx_abapgit_exception=>raise( iv_text     = lx_root->get_text( )
                                     ix_previous = lx_root ).
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_GSMP implementation

*>>>>>>> ZCL_ABAPGIT_DEFAULT_TRANSPORT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_default_transport=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_default_transport=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_DEFAULT_TRANSPORT implementation.
*"* method's implementations
*include methods.
  METHOD clear.

    CALL FUNCTION 'TR_TASK_RESET'
      EXPORTING
        iv_username      = is_default_task-username
        iv_order         = is_default_task-ordernum
        iv_task          = is_default_task-tasknum
        iv_dialog        = abap_false
      EXCEPTIONS
        invalid_username = 1
        invalid_order    = 2
        invalid_task     = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    store( ).

  ENDMETHOD.
  METHOD get.

    DATA: lt_e070use TYPE STANDARD TABLE OF e070use.

    CALL FUNCTION 'TR_TASK_GET'
      TABLES
        tt_e070use       = lt_e070use
      EXCEPTIONS
        invalid_username = 1
        invalid_category = 2
        invalid_client   = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    READ TABLE lt_e070use INTO rs_default_task
                          INDEX 1.

  ENDMETHOD.
  METHOD get_instance.

    IF go_instance IS NOT BOUND.
      CREATE OBJECT go_instance.
    ENDIF.

    ro_instance = go_instance.

  ENDMETHOD.
  METHOD reset.

    DATA: ls_default_task TYPE e070use.

    IF mv_is_set_by_abapgit = abap_false.
      " if the default transport request task isn't set
      " by us there is nothing to do.
      RETURN.
    ENDIF.

    CLEAR mv_is_set_by_abapgit.

    ls_default_task = get( ).

    IF ls_default_task IS NOT INITIAL.

      clear( ls_default_task ).

    ENDIF.

    restore( ).

  ENDMETHOD.
  METHOD restore.

    IF ms_save IS INITIAL.
      " There wasn't a default transport request before
      " so we needn't restore anything.
      RETURN.
    ENDIF.

    CALL FUNCTION 'TR_TASK_SET'
      EXPORTING
        iv_order          = ms_save-ordernum
        iv_task           = ms_save-tasknum
      EXCEPTIONS
        invalid_username  = 1
        invalid_category  = 2
        invalid_client    = 3
        invalid_validdays = 4
        invalid_order     = 5
        invalid_task      = 6
        OTHERS            = 7.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD set.

    " checks whether object changes of the package are rerorded in transport
    " requests. If true then we set the default task, so that no annoying
    " transport request popups are shown while deserializing.

    IF mv_is_set_by_abapgit = abap_true.
      " the default transport request task is already set by us
      " -> no reason to do it again.
      RETURN.
    ENDIF.

    IF iv_transport IS INITIAL.
      Lcx_abapgit_exception=>raise( |No transport request was supplied| ).
    ENDIF.

    set_internal( iv_transport ).

    mv_is_set_by_abapgit = abap_true.

  ENDMETHOD.
  METHOD set_internal.

    CALL FUNCTION 'TR_TASK_SET'
      EXPORTING
        iv_order          = iv_transport
        iv_validdays      = 1
      EXCEPTIONS
        invalid_username  = 1
        invalid_category  = 2
        invalid_client    = 3
        invalid_validdays = 4
        invalid_order     = 5
        invalid_task      = 6
        OTHERS            = 7.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD store.

    ms_save = get( ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_DEFAULT_TRANSPORT implementation

*>>>>>>> ZCL_ABAPGIT_ECATT_CONFIG_UPL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ecatt_config_upl==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ecatt_config_upl==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ECATT_CONFIG_UPL implementation.
*"* method's implementations
*include methods.
  METHOD upload_data_from_stream.

    " Downport
    template_over_all = Lcl_abapgit_ecatt_helper=>upload_data_from_stream( mv_external_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_ecatt_upload~set_stream_for_upload.

    " downport from CL_ABAPGIT_ECATT_DATA_UPLOAD SET_STREAM_FOR_UPLOAD
    mv_external_xml = iv_xml.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ECATT_CONFIG_UPL implementation

*>>>>>>> ZCL_ABAPGIT_ECATT_SCRIPT_UPL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ecatt_script_upl==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ecatt_script_upl==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ECATT_SCRIPT_UPL implementation.
*"* method's implementations
*include methods.
  METHOD upload_data_from_stream.

    " Downport
    template_over_all = Lcl_abapgit_ecatt_helper=>upload_data_from_stream( mv_external_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_ecatt_upload~set_stream_for_upload.

    " downport from CL_ABAPGIT_ECATT_DATA_UPLOAD SET_STREAM_FOR_UPLOAD
    mv_external_xml = iv_xml.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ECATT_SCRIPT_UPL implementation

*>>>>>>> ZCL_ABAPGIT_USER_RECORD <<<<<<<*

*"* macro definitions
*include zcl_abapgit_user_record=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_user_record=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_user_record=======ccau.
*CLASS SHRITEFUH64VYIPO5I47WOOA56SASM DEFINITION DEFERRED.

*CLASS zcl_abapgit_user_record DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA56SASM.




class LCL_ABAPGIT_USER_RECORD implementation.
*"* method's implementations
*include methods.
  METHOD check_user_exists.

    DATA lt_return  TYPE STANDARD TABLE OF bapiret2 WITH DEFAULT KEY.
    DATA ls_address TYPE bapiaddr3.
    DATA lt_smtp    TYPE TABLE OF bapiadsmtp.
    DATA ls_smtp    LIKE LINE OF lt_smtp.

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = iv_user
      IMPORTING
        address  = ls_address
      TABLES
        return   = lt_return
        addsmtp  = lt_smtp.
    LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type CA 'EA'.
      Lcx_abapgit_exception=>raise( |User: { iv_user } not found| ).
    ENDLOOP.

    ev_fullname = ls_address-fullname.

    " Choose the first email from SU01
    SORT lt_smtp BY consnumber ASCENDING.

    LOOP AT lt_smtp INTO ls_smtp.
      ev_email = ls_smtp-e_mail.
      EXIT.
    ENDLOOP.

  ENDMETHOD.
  METHOD constructor.

    DATA ls_user TYPE ty_user.

    " Get user details
    TRY.
        check_user_exists(
          EXPORTING
            iv_user     = iv_user
          IMPORTING
            ev_fullname = ms_user-name
            ev_email    = ms_user-email ).
      CATCH Lcx_abapgit_exception.
        " Could not find user, try to get from other clients
        get_user_dtls_from_other_clnt( iv_user ).
    ENDTRY.

    " If the user has been found add it to the list
    IF ms_user-name IS NOT INITIAL AND ms_user-email IS NOT INITIAL.
      ls_user-user = iv_user.
      ls_user-o_user = me.
      INSERT ls_user INTO TABLE gt_user.
    ENDIF.

  ENDMETHOD.
  METHOD get_email.

    rv_email = ms_user-email.

  ENDMETHOD.
  METHOD get_instance.

    FIELD-SYMBOLS <ls_user> TYPE ty_user.

    READ TABLE gt_user ASSIGNING <ls_user> WITH TABLE KEY user = iv_user.
    IF sy-subrc = 0.
      ro_user = <ls_user>-o_user.
    ELSE.
      CREATE OBJECT ro_user
        EXPORTING
          iv_user = iv_user.
    ENDIF.

  ENDMETHOD.
  METHOD get_name.

    rv_name = ms_user-name.

  ENDMETHOD.
  METHOD get_user_dtls_from_other_clnt.

    CONSTANTS lc_cc_category TYPE string VALUE 'C'.
    TYPES ty_dev_clients TYPE SORTED TABLE OF sy-mandt WITH UNIQUE KEY table_line.
    DATA lt_dev_clients TYPE ty_dev_clients.
    FIELD-SYMBOLS <lv_dev_client> LIKE LINE OF lt_dev_clients.

    " Could not find the user, try other development clients
    SELECT mandt FROM t000 INTO TABLE lt_dev_clients
        WHERE cccategory = lc_cc_category AND mandt <> sy-mandt
        ORDER BY PRIMARY KEY.

    LOOP AT lt_dev_clients ASSIGNING <lv_dev_client>.
      SELECT SINGLE p~name_text a~smtp_addr INTO (ms_user-name, ms_user-email)
          FROM usr21 AS u
          INNER JOIN adrp AS p ON p~persnumber = u~persnumber
                              AND p~client     = u~mandt
          INNER JOIN adr6 AS a ON a~persnumber = u~persnumber
                              AND a~addrnumber = u~addrnumber
                              AND a~client     = u~mandt
          CLIENT SPECIFIED
          WHERE u~mandt      = <lv_dev_client>
            AND u~bname      = iv_user
            AND p~date_from <= sy-datum
            AND p~date_to   >= sy-datum
            AND a~date_from <= sy-datum.
      IF sy-subrc = 0.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD reset.
    CLEAR gt_user.
  ENDMETHOD.
  METHOD get_title.
* the queried username might not exist, so this method is static

    DATA ls_user_address TYPE addr3_val.

    CALL FUNCTION 'SUSR_USER_ADDRESS_READ'
      EXPORTING
        user_name              = iv_username
      IMPORTING
        user_address           = ls_user_address
      EXCEPTIONS
        user_address_not_found = 1
        OTHERS                 = 2.
    IF sy-subrc = 0.
      rv_title = ls_user_address-name_text.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_USER_RECORD implementation

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

*>>>>>>> ZCL_ABAPGIT_XML_PRETTY <<<<<<<*

*"* macro definitions
*include zcl_abapgit_xml_pretty========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_xml_pretty========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_xml_pretty========ccau.




class LCL_ABAPGIT_XML_PRETTY implementation.
*"* method's implementations
*include methods.
  METHOD print.

    DATA: li_ixml           TYPE REF TO if_ixml,
          li_xml_doc        TYPE REF TO if_ixml_document,
          li_stream_factory TYPE REF TO if_ixml_stream_factory,
          li_istream        TYPE REF TO if_ixml_istream,
          li_parser         TYPE REF TO if_ixml_parser,
          lv_xstring        TYPE xstring,
          li_encoding       TYPE REF TO if_ixml_encoding,
          li_ostream        TYPE REF TO if_ixml_ostream,
          li_renderer       TYPE REF TO if_ixml_renderer.


    ASSERT NOT iv_xml IS INITIAL.

    li_ixml    = cl_ixml=>create( ).
    li_xml_doc = li_ixml->create_document( ).

    li_stream_factory = li_ixml->create_stream_factory( ).
    li_istream        = li_stream_factory->create_istream_xstring(
      Lcl_abapgit_convert=>string_to_xstring_utf8( iv_xml ) ).
    li_parser         = li_ixml->create_parser( stream_factory = li_stream_factory
                                                istream        = li_istream
                                                document       = li_xml_doc ).
    li_parser->set_normalizing( abap_true ).
    IF li_parser->parse( ) <> 0.
      IF iv_ignore_errors = abap_true.
        rv_xml = iv_xml.
        RETURN.
      ELSE.
        Lcx_abapgit_exception=>raise( 'error parsing xml' ).
      ENDIF.
    ENDIF.


    li_ostream  = li_stream_factory->create_ostream_xstring( lv_xstring ).

    li_encoding = li_ixml->create_encoding(
      character_set = 'utf-8'
      byte_order    = if_ixml_encoding=>co_big_endian ).

    li_xml_doc->set_encoding( li_encoding ).
    li_renderer = li_ixml->create_renderer( ostream  = li_ostream
                                            document = li_xml_doc ).

    li_renderer->set_normalizing( boolc( iv_unpretty = abap_false ) ).

    li_renderer->render( ).

    rv_xml = Lcl_abapgit_convert=>xstring_to_string_utf8_bom( lv_xstring ).
    REPLACE FIRST OCCURRENCE OF 'utf-8' IN rv_xml WITH 'utf-16'.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_XML_PRETTY implementation

*>>>>>>> ZCX_ABAPGIT_NOT_FOUND <<<<<<<*

*"* macro definitions
*include zcx_abapgit_not_found=========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcx_abapgit_not_found=========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCX_ABAPGIT_NOT_FOUND implementation.
*"* method's implementations
*include methods.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor(
      textid   = textid
      previous = previous ).
  ENDMETHOD.
endclass. "ZCX_ABAPGIT_NOT_FOUND implementation

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

*>>>>>>> ZCL_ABAPGIT_OBJECTS_FILES <<<<<<<*

*"* macro definitions
*include zcl_abapgit_objects_files=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_objects_files=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_objects_files=====ccau.


class LCL_ABAPGIT_OBJECTS_FILES implementation.
*"* method's implementations
*include methods.
  METHOD add.
    APPEND is_file TO mt_files.
  ENDMETHOD.
  METHOD add_abap.

    DATA: lv_source TYPE string,
          ls_file   TYPE Lif_abapgit_git_definitions=>ty_file.


    CONCATENATE LINES OF it_abap INTO lv_source SEPARATED BY cl_abap_char_utilities=>newline.
* when editing files via eg. GitHub web interface it adds a newline at end of file
    lv_source = lv_source && cl_abap_char_utilities=>newline.

    ls_file-path = '/'.
    ls_file-filename = Lcl_abapgit_filename_logic=>object_to_file(
      is_item  = ms_item
      iv_extra = iv_extra
      iv_ext   = 'abap' ).
    ls_file-data = Lcl_abapgit_convert=>string_to_xstring_utf8( lv_source ).

    APPEND ls_file TO mt_files.

  ENDMETHOD.
  METHOD add_raw.

    DATA: ls_file TYPE Lif_abapgit_git_definitions=>ty_file.

    ls_file-path     = '/'.
    ls_file-data     = iv_data.
    ls_file-filename = Lcl_abapgit_filename_logic=>object_to_file(
      is_item  = ms_item
      iv_extra = iv_extra
      iv_ext   = iv_ext ).

    APPEND ls_file TO mt_files.

  ENDMETHOD.
  METHOD add_string.

    DATA: ls_file TYPE Lif_abapgit_git_definitions=>ty_file.


    ls_file-path = '/'.
    ls_file-filename = Lcl_abapgit_filename_logic=>object_to_file(
      is_item  = ms_item
      iv_extra = iv_extra
      iv_ext   = iv_ext ).
    ls_file-data = Lcl_abapgit_convert=>string_to_xstring_utf8( iv_string ).

    APPEND ls_file TO mt_files.

  ENDMETHOD.
  METHOD add_xml.

    DATA: lv_xml  TYPE string,
          ls_file TYPE Lif_abapgit_git_definitions=>ty_file.

    lv_xml = ii_xml->render( iv_normalize = iv_normalize
                             is_metadata = is_metadata ).
    ls_file-path = '/'.

    ls_file-filename = Lcl_abapgit_filename_logic=>object_to_file(
      is_item  = ms_item
      iv_extra = iv_extra
      iv_ext   = 'xml' ).

    REPLACE FIRST OCCURRENCE
      OF REGEX '<\?xml version="1\.0" encoding="[\w-]+"\?>'
      IN lv_xml
      WITH '<?xml version="1.0" encoding="utf-8"?>'.
    ASSERT sy-subrc = 0.

    ls_file-data = Lcl_abapgit_convert=>string_to_xstring_utf8_bom( lv_xml ).

    APPEND ls_file TO mt_files.
  ENDMETHOD.
  METHOD constructor.
    ms_item = is_item.
    mv_path = iv_path.
  ENDMETHOD.
  METHOD contains_file.
    DATA: lv_filename TYPE string.

    lv_filename = Lcl_abapgit_filename_logic=>object_to_file(
      is_item  = ms_item
      iv_extra = iv_extra
      iv_ext   = iv_ext ).

    IF mv_path IS NOT INITIAL.
      READ TABLE mt_files TRANSPORTING NO FIELDS
          WITH KEY file_path
          COMPONENTS path     = mv_path
                     filename = lv_filename.
    ELSE.
      READ TABLE mt_files TRANSPORTING NO FIELDS
          WITH KEY file
          COMPONENTS filename = lv_filename.
    ENDIF.

    IF sy-subrc = 0.
      rv_present = abap_true.
    ENDIF.
  ENDMETHOD.
  METHOD get_accessed_files.
    rt_files = mt_accessed_files.
  ENDMETHOD.
  METHOD get_files.
    rt_files = mt_files.
  ENDMETHOD.
  METHOD get_file_pattern.
    rv_pattern = Lcl_abapgit_filename_logic=>object_to_file(
      is_item  = ms_item
      iv_ext   = '*' ).
    " Escape special characters for use with 'covers pattern' (CP)
    REPLACE ALL OCCURRENCES OF '#' IN rv_pattern WITH '##'.
    REPLACE ALL OCCURRENCES OF '+' IN rv_pattern WITH '#+'.
  ENDMETHOD.
  METHOD is_json_metadata.

    DATA lv_pattern TYPE string.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF mt_files.

    lv_pattern = |*.{ to_lower( ms_item-obj_type ) }.json|.

    LOOP AT mt_files ASSIGNING <ls_file> WHERE filename CP lv_pattern.
      rv_result = abap_true.
      EXIT.
    ENDLOOP.

  ENDMETHOD.
  METHOD read_abap.

    DATA: lv_filename TYPE string,
          lv_data     TYPE xstring,
          lv_abap     TYPE string.


    lv_filename = Lcl_abapgit_filename_logic=>object_to_file(
      is_item  = ms_item
      iv_extra = iv_extra
      iv_ext   = 'abap' ).

    lv_data = read_file( iv_filename = lv_filename
                         iv_error    = iv_error ).

    IF lv_data IS INITIAL. " Post-handling of iv_error = false
      RETURN.
    ENDIF.

    lv_abap = Lcl_abapgit_convert=>xstring_to_string_utf8( lv_data ).

    SPLIT lv_abap AT cl_abap_char_utilities=>newline INTO TABLE rt_abap.

  ENDMETHOD.
  METHOD read_file.

    FIELD-SYMBOLS <ls_file>     LIKE LINE OF mt_files.

    IF mv_path IS NOT INITIAL.
      READ TABLE mt_files ASSIGNING <ls_file>
          WITH KEY file_path
          COMPONENTS path     = mv_path
                     filename = iv_filename.
    ELSE.
      READ TABLE mt_files ASSIGNING <ls_file>
          WITH KEY file
          COMPONENTS filename = iv_filename.
    ENDIF.

    IF sy-subrc <> 0.
      IF iv_error = abap_true.
        Lcx_abapgit_exception=>raise( |File not found: { iv_filename }| ).
      ELSE.
        RETURN.
      ENDIF.
    ENDIF.

    " Update access table
    mark_accessed(
      iv_path = <ls_file>-path
      iv_file = <ls_file>-filename
      iv_sha1 = <ls_file>-sha1 ).

    rv_data = <ls_file>-data.

  ENDMETHOD.
  METHOD read_raw.

    DATA: lv_filename TYPE string.

    lv_filename = Lcl_abapgit_filename_logic=>object_to_file(
      is_item  = ms_item
      iv_extra = iv_extra
      iv_ext   = iv_ext ).

    rv_data = read_file( lv_filename ).

  ENDMETHOD.
  METHOD read_string.

    DATA: lv_filename TYPE string,
          lv_data     TYPE xstring.

    lv_filename = Lcl_abapgit_filename_logic=>object_to_file(
      is_item  = ms_item
      iv_extra = iv_extra
      iv_ext   = iv_ext ).

    lv_data = read_file( lv_filename ).

    rv_string = Lcl_abapgit_convert=>xstring_to_string_utf8( lv_data ).

  ENDMETHOD.
  METHOD read_xml.

    DATA: lv_filename TYPE string,
          lv_data     TYPE xstring,
          lv_xml      TYPE string.

    lv_filename = Lcl_abapgit_filename_logic=>object_to_file(
      is_item  = ms_item
      iv_extra = iv_extra
      iv_ext   = 'xml' ).

    lv_data = read_file( lv_filename ).

    lv_xml = Lcl_abapgit_convert=>xstring_to_string_utf8( lv_data ).

    CREATE OBJECT ri_xml
      TYPE Lcl_abapgit_xml_input
      EXPORTING
        iv_xml      = lv_xml
        iv_filename = lv_filename.

  ENDMETHOD.
  METHOD set_files.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF it_files.

    CLEAR mt_files.

    " Set only files matching the pattern for this object
    " If a path has been defined in the constructor, then the path has to match, too
    LOOP AT it_files ASSIGNING <ls_file> WHERE filename CP get_file_pattern( ).
      IF mv_path IS INITIAL.
        INSERT <ls_file> INTO TABLE mt_files.
      ELSEIF mv_path = <ls_file>-path.
        INSERT <ls_file> INTO TABLE mt_files.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD add_i18n_file.

    DATA ls_file TYPE Lif_abapgit_git_definitions=>ty_file.

    ls_file-data = ii_i18n_file->render( ).
    IF ls_file-data IS INITIAL.
      RETURN. " Don't add empty files
    ENDIF.

    ls_file-path     = '/'.
    ls_file-filename = Lcl_abapgit_filename_logic=>object_to_file(
      is_item  = ms_item
      iv_extra = |i18n.{ ii_i18n_file->lang( ) }|
      iv_ext   = ii_i18n_file->ext( ) ).

    APPEND ls_file TO mt_files.

  ENDMETHOD.
  METHOD mark_accessed.

    FIELD-SYMBOLS <ls_accessed> LIKE LINE OF mt_accessed_files.

    READ TABLE mt_accessed_files TRANSPORTING NO FIELDS
      WITH KEY path = iv_path filename = iv_file.
    IF sy-subrc > 0. " Not found ? -> Add
      APPEND INITIAL LINE TO mt_accessed_files ASSIGNING <ls_accessed>.
      <ls_accessed>-path     = iv_path.
      <ls_accessed>-filename = iv_file.
      <ls_accessed>-sha1     = iv_sha1.
    ENDIF.

  ENDMETHOD.
  METHOD read_i18n_files.

    DATA lv_lang TYPE laiso.
    DATA lv_ext TYPE string.
    DATA lo_po TYPE REF TO Lcl_abapgit_po_file.
    FIELD-SYMBOLS <ls_file> LIKE LINE OF mt_files.

    LOOP AT mt_files ASSIGNING <ls_file>.

      " TODO: Maybe this should be in zcl_abapgit_filename_logic
      FIND FIRST OCCURRENCE OF REGEX 'i18n\.([^.]{2})\.([^.]+)$' IN <ls_file>-filename SUBMATCHES lv_lang lv_ext.
      CHECK sy-subrc = 0.

      CASE lv_ext.
        WHEN 'po'.
          CREATE OBJECT lo_po EXPORTING iv_lang = lv_lang.
          lo_po->parse( <ls_file>-data ).
          APPEND lo_po TO rt_i18n_files.
        WHEN OTHERS.
          CONTINUE. " Unsupported i18n file type
      ENDCASE.

      mark_accessed(
        iv_path = <ls_file>-path
        iv_file = <ls_file>-filename
        iv_sha1 = <ls_file>-sha1 ).

    ENDLOOP.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECTS_FILES implementation

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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SHI3 implementation

*>>>>>>> ZCL_ABAPGIT_OBJECTS_GENERIC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_objects_generic===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_objects_generic===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_objects_generic===ccau.
*CLASS SHRITEFUH64VYIPO5I47WOOA5ZUASM DEFINITION DEFERRED.
*CLASS zcl_abapgit_objects_generic DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5ZUASM.



class LCL_ABAPGIT_OBJECTS_GENERIC implementation.
*"* method's implementations
*include methods.
  METHOD after_import.

    DATA: lt_cts_object_entry TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY,
          ls_cts_object_entry LIKE LINE OF lt_cts_object_entry,
          lt_cts_key          TYPE STANDARD TABLE OF e071k WITH DEFAULT KEY.

    FIELD-SYMBOLS <ls_object_method> LIKE LINE OF mt_object_method.


    ls_cts_object_entry-pgmid    = 'R3TR'.
    ls_cts_object_entry-object   = ms_item-obj_type.
    ls_cts_object_entry-obj_name = ms_item-obj_name.
    INSERT ls_cts_object_entry INTO TABLE lt_cts_object_entry.

    READ TABLE mt_object_method ASSIGNING <ls_object_method>
      WITH KEY
        objectname = ms_item-obj_type
        objecttype = 'L'
        method = 'AFTER_IMP'.
    IF sy-subrc = 0.
* client is actually optional for most AIM, but let's supply it and hope
* that those client-independent-ones just ignore it
      CALL FUNCTION <ls_object_method>-methodname
        EXPORTING
          iv_tarclient  = sy-mandt
          iv_is_upgrade = abap_false
        TABLES
          tt_e071       = lt_cts_object_entry
          tt_e071k      = lt_cts_key.
    ENDIF.

  ENDMETHOD.
  METHOD apply_clear_logic.
    IF mo_field_rules IS BOUND.
      mo_field_rules->apply_clear_logic( EXPORTING iv_table = |{ iv_table }|
                                         CHANGING  ct_data  = ct_data ).
    ENDIF.
  ENDMETHOD.
  METHOD apply_fill_logic.
    IF mo_field_rules IS BOUND.
      mo_field_rules->apply_fill_logic(
        EXPORTING
          iv_table   = |{ iv_table }|
          iv_package = iv_package
        CHANGING
          ct_data    = ct_data ).
    ENDIF.
  ENDMETHOD.
  METHOD before_export.

    DATA: lt_cts_object_entry TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY,
          ls_cts_object_entry LIKE LINE OF lt_cts_object_entry,
          lt_cts_key          TYPE STANDARD TABLE OF e071k WITH DEFAULT KEY,
          lv_client           TYPE trclient.

    FIELD-SYMBOLS <ls_object_method> LIKE LINE OF mt_object_method.


    READ TABLE mt_object_method ASSIGNING <ls_object_method>
      WITH KEY
        objectname = ms_item-obj_type
        objecttype = 'L'
        method     = 'BEFORE_EXP'.
    IF sy-subrc = 0.
      lv_client = sy-mandt.

      ls_cts_object_entry-pgmid    = 'R3TR'.
      ls_cts_object_entry-object   = ms_item-obj_type.
      ls_cts_object_entry-obj_name = ms_item-obj_name.
      INSERT ls_cts_object_entry INTO TABLE lt_cts_object_entry.

      CALL FUNCTION <ls_object_method>-methodname
        EXPORTING
          iv_client = lv_client
        TABLES
          tt_e071   = lt_cts_object_entry
          tt_e071k  = lt_cts_key.
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    CONSTANTS lc_logical_transport_object TYPE c LENGTH 1 VALUE 'L'.


    SELECT SINGLE * FROM objh INTO ms_object_header
      WHERE objectname = is_item-obj_type
      AND objecttype = lc_logical_transport_object.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Not found in OBJH, or not supported' ).
    ENDIF.

    " object tables
    SELECT * FROM objsl INTO CORRESPONDING FIELDS OF TABLE mt_object_table
      WHERE objectname = is_item-obj_type
      AND objecttype = lc_logical_transport_object
      AND tobject = 'TABU'
      ORDER BY PRIMARY KEY.
    IF mt_object_table IS INITIAL.
      Lcx_abapgit_exception=>raise( |Obviously corrupted object-type { is_item-obj_type }: No tables defined| ).
    ENDIF.

    " remove duplicate table/table-key entries
    " same table with different keys is ok
    SORT mt_object_table BY tobj_name tobjkey.
    DELETE ADJACENT DUPLICATES FROM mt_object_table COMPARING tobj_name tobjkey.

    " object methods
    SELECT * FROM objm INTO TABLE mt_object_method
      WHERE objectname = is_item-obj_type
      AND objecttype = lc_logical_transport_object
      ORDER BY PRIMARY KEY.

    ms_item = is_item.
    mv_language = iv_language.
    mo_field_rules = io_field_rules.

  ENDMETHOD.
  METHOD corr_insert.

* this will also insert into TADIR
    Lcl_abapgit_factory=>get_cts_api( )->insert_transport_object(
      iv_object   = ms_item-obj_type
      iv_obj_name = ms_item-obj_name
      iv_package  = iv_package
      iv_language = mv_language ).

  ENDMETHOD.
  METHOD delete.

    DATA: lv_where   TYPE string,
          lv_primary TYPE objsl-tobj_name.

    FIELD-SYMBOLS <ls_table> LIKE LINE OF mt_object_table.


    lv_primary = get_primary_table( ).

    LOOP AT mt_object_table ASSIGNING <ls_table>.
      lv_where = get_where_clause( <ls_table>-tobj_name ).
      ASSERT NOT lv_where IS INITIAL.

      DELETE FROM (<ls_table>-tobj_name) WHERE (lv_where).

      IF <ls_table>-tobj_name = lv_primary.
        ASSERT sy-dbcnt <= 1. "Just to be on the very safe side
      ENDIF.
    ENDLOOP.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD deserialize.

    validate( io_xml ).

    delete( iv_package ).

    deserialize_data(
      io_xml     = io_xml
      iv_package = iv_package ).

    after_import( ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD deserialize_data.

    DATA: lr_ref TYPE REF TO data.

    FIELD-SYMBOLS: <lt_data>  TYPE STANDARD TABLE,
                   <ls_table> LIKE LINE OF mt_object_table.


    LOOP AT mt_object_table ASSIGNING <ls_table>.

      CREATE DATA lr_ref TYPE STANDARD TABLE OF (<ls_table>-tobj_name).
      ASSIGN lr_ref->* TO <lt_data>.

      io_xml->read(
        EXPORTING
          iv_name = <ls_table>-tobj_name
        CHANGING
          cg_data = <lt_data> ).
      apply_fill_logic(
        EXPORTING
          iv_table   = <ls_table>-tobj_name
          iv_package = iv_package
        CHANGING
          ct_data    = <lt_data> ).

      INSERT (<ls_table>-tobj_name) FROM TABLE <lt_data>.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Error inserting data, { <ls_table>-tobj_name }| ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD distribute_name_to_components.

    DATA: lt_key_component_uncovered  LIKE it_key_component,
          ls_key_component_uncovered  LIKE LINE OF lt_key_component_uncovered,
          ls_objkey_sub               LIKE cs_objkey,
          lv_objkey_sub_pos           TYPE i,
          lv_remaining_length         TYPE i,
          lv_count_components_covered LIKE ls_objkey_sub-num.

    DATA lv_len LIKE ls_key_component_uncovered-leng.


    lt_key_component_uncovered = it_key_component.
    ls_objkey_sub-num = cs_objkey-num.
    lv_objkey_sub_pos = 0.

*    we want to fill the atribute values which are not covered by explicit key components yet
    lv_count_components_covered = ls_objkey_sub-num - 1.
    DO lv_count_components_covered TIMES.
      DELETE lt_key_component_uncovered INDEX 1.
    ENDDO.

    LOOP AT lt_key_component_uncovered INTO ls_key_component_uncovered.
      CLEAR ls_objkey_sub-value.

*      Some datatype used in the key might exceed the total remaining characters length (e. g. SICF)
      TRY.
          lv_remaining_length = strlen( |{ substring( val = cs_objkey-value
                                                      off = lv_objkey_sub_pos ) }| ).
        CATCH cx_sy_range_out_of_bounds.
          lv_remaining_length = 0.
          RETURN. ">>>>>>>>>>>>>>>>>>>>>>>>>>>
      ENDTRY.
      IF ls_key_component_uncovered-leng <= lv_remaining_length.
        lv_len = ls_key_component_uncovered-leng.
      ELSE.
        lv_len = lv_remaining_length.
      ENDIF.

      ls_objkey_sub-value = |{ substring( val = cs_objkey-value
                                          off = lv_objkey_sub_pos
                                          len = lv_len ) }|.
      ls_objkey_sub-num = cv_non_value_pos.

      INSERT ls_objkey_sub INTO TABLE ct_objkey.

      lv_objkey_sub_pos = lv_objkey_sub_pos + ls_key_component_uncovered-leng.
      cv_non_value_pos = cv_non_value_pos + 1.
      CLEAR ls_objkey_sub.

      IF lv_objkey_sub_pos = strlen( cs_objkey-value ).
        cs_objkey-num = cv_non_value_pos.
        EXIT. "end splitting - all characters captured
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD exists.

    DATA: lv_where_clause TYPE string,
          lv_primary      TYPE objsl-tobj_name,
          lr_table_line   TYPE REF TO data.

    FIELD-SYMBOLS: <lg_table_line> TYPE any.


    lv_primary = get_primary_table( ).

    lv_where_clause = get_where_clause( lv_primary ).

    CREATE DATA lr_table_line TYPE (lv_primary).
    ASSIGN lr_table_line->* TO <lg_table_line>.

    SELECT SINGLE * FROM (lv_primary) INTO <lg_table_line> WHERE (lv_where_clause).
    rv_bool = boolc( sy-dbcnt > 0 ).

  ENDMETHOD.
  METHOD get_key_fields.

    DATA: lv_table TYPE ddobjname.


    lv_table = iv_table.

    CALL FUNCTION 'DDIF_NAMETAB_GET'
      EXPORTING
        tabname   = lv_table
      TABLES
        dfies_tab = rt_keys
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    DELETE rt_keys WHERE keyflag = abap_false.

  ENDMETHOD.
  METHOD get_primary_table.

    DATA: ls_object_table LIKE LINE OF mt_object_table.
    DATA: lt_object_table LIKE mt_object_table.

    " There might be several tables marked as "primary"
    " Sort by DB key so we get first one in the list
    lt_object_table = mt_object_table.
    SORT lt_object_table.

    READ TABLE lt_object_table INTO ls_object_table WITH KEY prim_table = abap_true.
    IF sy-subrc <> 0.
      " Fallback. For some objects, no primary table is explicitly flagged
      " Then, the one with only one key field shall be chosen
      READ TABLE lt_object_table INTO ls_object_table WITH KEY tobjkey = '/&'. "#EC CI_SUBRC
    ENDIF.
    IF ls_object_table IS INITIAL.
      Lcx_abapgit_exception=>raise( |Object { ms_item-obj_type } has got no defined primary table| ).
    ENDIF.

    rv_table = ls_object_table-tobj_name.

  ENDMETHOD.
  METHOD get_where_clause.

    DATA: lv_objkey_pos      TYPE i,
          lv_next_objkey_pos TYPE i,
          lv_value_pos       TYPE i,
          lv_objkey_length   TYPE i,
          lt_objkey          TYPE ty_t_objkey,
          ls_objkey          LIKE LINE OF lt_objkey,
          lv_non_value_pos   TYPE numc3,
          lt_key_fields      TYPE ddfields.

    DATA: lv_is_asterix      TYPE abap_bool,
          lv_where_statement TYPE string,
          lv_key_pos         TYPE i,
          lv_value128        TYPE string.

    FIELD-SYMBOLS <ls_object_table> LIKE LINE OF mt_object_table.

    FIELD-SYMBOLS <ls_table_field> LIKE LINE OF lt_key_fields.


    READ TABLE mt_object_table ASSIGNING <ls_object_table> WITH KEY tobj_name = iv_tobj_name.
    ASSERT sy-subrc = 0.

    lt_key_fields = get_key_fields( iv_tobj_name ).

*   analyze the object key and compose the key (table)
    CLEAR lt_objkey.
    CLEAR ls_objkey.
    lv_objkey_pos = 0.
    lv_non_value_pos = 1.
    lv_value_pos = 0.
    lv_objkey_length = strlen( <ls_object_table>-tobjkey ).

    WHILE lv_objkey_pos <= lv_objkey_length.
      ls_objkey-num = lv_non_value_pos.
*     command
      IF <ls_object_table>-tobjkey+lv_objkey_pos(1) = '/'.
        IF NOT ls_objkey-value IS INITIAL.
*        We reached the end of a key-definition.
*        this key part may address multiple fields.
*        E. g. six characters may address one boolean field and a five-digit version field.
*        Thus, we need to analyze the remaining key components which have not been covered yet.
          split_value_to_keys(
            EXPORTING
              it_key_component = lt_key_fields
            CHANGING
              ct_objkey        = lt_objkey
              cs_objkey        = ls_objkey
              cv_non_value_pos = lv_non_value_pos ).
        ENDIF.
        lv_next_objkey_pos = lv_objkey_pos + 1.
*       '*' means all further key values
        IF <ls_object_table>-tobjkey+lv_next_objkey_pos(1) = '*'.
          ls_objkey-value = '*'.
          INSERT ls_objkey INTO TABLE lt_objkey.
          CLEAR ls_objkey.
          lv_non_value_pos = lv_non_value_pos + 1.
          lv_objkey_pos = lv_objkey_pos + 1.
*       object name
        ELSEIF <ls_object_table>-tobjkey+lv_next_objkey_pos(1) = '&'.
          ls_objkey-value = ms_item-obj_name.
*    The object name might comprise multiple key components (e. g. WDCC)
*    This string needs to be split
          distribute_name_to_components(
            EXPORTING
              it_key_component = lt_key_fields
            CHANGING
              ct_objkey        = lt_objkey
              cs_objkey        = ls_objkey
              cv_non_value_pos = lv_non_value_pos ).
          CLEAR ls_objkey.
          lv_objkey_pos = lv_objkey_pos + 1.
*       language
        ELSEIF <ls_object_table>-tobjkey+lv_next_objkey_pos(1) = 'L'.
          ls_objkey-value = mv_language.
          INSERT ls_objkey INTO TABLE lt_objkey.
          CLEAR ls_objkey.
          lv_non_value_pos = lv_non_value_pos + 1.
          lv_objkey_pos = lv_objkey_pos + 1.
*       Client
        ELSEIF <ls_object_table>-tobjkey+lv_next_objkey_pos(1) = 'C'.
          ls_objkey-value = sy-mandt.
          INSERT ls_objkey INTO TABLE lt_objkey.
          CLEAR ls_objkey.
          lv_non_value_pos = lv_non_value_pos + 1.
          lv_objkey_pos = lv_objkey_pos + 1.
        ENDIF.
        lv_value_pos = 0.
*     value
      ELSE.
        ls_objkey-value+lv_value_pos(1) = <ls_object_table>-tobjkey+lv_objkey_pos(1).
        lv_value_pos = lv_value_pos + 1.
      ENDIF.

      lv_objkey_pos = lv_objkey_pos + 1.
    ENDWHILE.

*    Similarly to that, fixed values might be supplied in the object key which actually make up key components
    IF NOT ls_objkey-value IS INITIAL.
      split_value_to_keys(
        EXPORTING
          it_key_component = lt_key_fields
        CHANGING
          ct_objkey        = lt_objkey
          cs_objkey        = ls_objkey
          cv_non_value_pos = lv_non_value_pos ).
    ENDIF.

*   compose the where clause
    lv_is_asterix = abap_false.
    lv_key_pos = 1.

    LOOP AT lt_key_fields ASSIGNING <ls_table_field>.
      READ TABLE lt_objkey INTO ls_objkey
        WITH TABLE KEY num = lv_key_pos.
      IF sy-subrc <> 0 OR <ls_table_field>-fieldname = 'LANGU'.
        CLEAR ls_objkey.
        lv_key_pos = lv_key_pos + 1.
        CONTINUE.
      ENDIF.
      IF ls_objkey-value = '*'.
        lv_is_asterix = abap_true.
      ENDIF.
      IF lv_is_asterix = abap_true.
        CONTINUE.
      ENDIF.
      IF NOT lv_where_statement IS INITIAL.
        CONCATENATE lv_where_statement 'AND' INTO lv_where_statement
          SEPARATED BY space.
      ENDIF.
      lv_value128 = cl_abap_dyn_prg=>quote( ls_objkey-value ).
      CONCATENATE lv_where_statement <ls_table_field>-fieldname '='
        lv_value128 INTO lv_where_statement SEPARATED BY space.
      lv_key_pos = lv_key_pos + 1.
    ENDLOOP.

    rv_where = condense( lv_where_statement ).

  ENDMETHOD.
  METHOD serialize.

    before_export( ).

    serialize_data( io_xml ).

  ENDMETHOD.
  METHOD serialize_data.

    DATA: lr_ref   TYPE REF TO data,
          lv_where TYPE string.

    FIELD-SYMBOLS: <lt_data>         TYPE STANDARD TABLE,
                   <ls_object_table> LIKE LINE OF mt_object_table.


    LOOP AT mt_object_table ASSIGNING <ls_object_table>.

      CREATE DATA lr_ref TYPE STANDARD TABLE OF (<ls_object_table>-tobj_name).
      ASSIGN lr_ref->* TO <lt_data>.

      lv_where = get_where_clause( <ls_object_table>-tobj_name ).

      SELECT * FROM (<ls_object_table>-tobj_name)
        INTO TABLE <lt_data>
        WHERE (lv_where)
        ORDER BY PRIMARY KEY.

      apply_clear_logic( EXPORTING iv_table = <ls_object_table>-tobj_name
                         CHANGING  ct_data  = <lt_data> ).

      io_xml->add(
        iv_name = <ls_object_table>-tobj_name
        ig_data = <lt_data> ).

    ENDLOOP.

  ENDMETHOD.
  METHOD split_value_to_keys.

    DATA: lt_key_component_uncovered LIKE it_key_component,
          ls_dummy                   LIKE LINE OF ct_objkey,
          ls_key_component_uncovered LIKE LINE OF lt_key_component_uncovered,
          ls_objkey_sub              LIKE cs_objkey,
          lv_objkey_sub_pos          TYPE i.


    lt_key_component_uncovered = it_key_component.

*    we want to fill the atribute values which are not covered by explicit key components yet
    LOOP AT ct_objkey INTO ls_dummy.
      DELETE lt_key_component_uncovered INDEX 1.
    ENDLOOP.

    ls_objkey_sub-num = cs_objkey-num.
    lv_objkey_sub_pos = 0.
    LOOP AT lt_key_component_uncovered INTO ls_key_component_uncovered.
      CLEAR ls_objkey_sub-value.
      ls_objkey_sub-value = cs_objkey-value+lv_objkey_sub_pos(ls_key_component_uncovered-leng).
      ls_objkey_sub-num = cv_non_value_pos.

      INSERT ls_objkey_sub INTO TABLE ct_objkey.

      lv_objkey_sub_pos = lv_objkey_sub_pos + ls_key_component_uncovered-leng.
      cv_non_value_pos = cv_non_value_pos + 1.
      CLEAR ls_objkey_sub.

      IF lv_objkey_sub_pos = strlen( cs_objkey-value ).
        cs_objkey-num = cv_non_value_pos.
        EXIT. "end splitting - all characters captured
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD validate.

    DATA: lv_where   TYPE string,
          lv_primary TYPE objsl-tobj_name,
          lr_ref     TYPE REF TO data.

    FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE.


    lv_primary = get_primary_table( ).

    CREATE DATA lr_ref TYPE STANDARD TABLE OF (lv_primary).
    ASSIGN lr_ref->* TO <lt_data>.

    io_xml->read(
      EXPORTING
        iv_name = lv_primary
      CHANGING
        cg_data = <lt_data> ).

    IF lines( <lt_data> ) = 0.
      Lcx_abapgit_exception=>raise( |Primary table { lv_primary } not found in imported container| ).
    ELSEIF lines( <lt_data> ) <> 1.
      Lcx_abapgit_exception=>raise( |Primary table { lv_primary } contains more than one instance!| ).
    ENDIF.

    lv_where = get_where_clause( lv_primary ).

*  validate that max one local instance was affected by the import
    SELECT COUNT(*) FROM (lv_primary) WHERE (lv_where).
    IF sy-dbcnt > 1.
      Lcx_abapgit_exception=>raise( |More than one instance exists locally in primary table { lv_primary }| ).
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECTS_GENERIC implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PICKLIST <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_picklist======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_picklist======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PICKLIST implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    FIELD-SYMBOLS <lt_tab> TYPE STANDARD TABLE.

    super->constructor( ).

    " copy contents of table to local scope
    CREATE DATA mr_list LIKE it_list.
    ASSIGN mr_list->* TO <lt_tab>.
    APPEND LINES OF it_list TO <lt_tab>.

    mv_attr_name = to_upper( iv_attr_name ).
    mi_item_renderer = ii_item_renderer.
    mv_in_page = iv_in_page.
    mv_id      = iv_id.
    mv_title   = iv_title.

    IF mi_item_renderer IS NOT BOUND AND mv_attr_name IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Renderer or attr name required' ).
    ENDIF.

    CREATE OBJECT mo_form_data.
    CREATE OBJECT mo_validation_log.
    mo_form = get_form_schema( ).
    mo_form_util = Lcl_abapgit_html_form_utils=>create( mo_form ).

  ENDMETHOD.
  METHOD get_form_schema.

    FIELD-SYMBOLS <lt_list> TYPE ANY TABLE.
    FIELD-SYMBOLS <lv_val> TYPE any.
    FIELD-SYMBOLS <ls_row> TYPE any.
    DATA lv_index TYPE i.
    DATA lv_label TYPE string.

    ro_form = Lcl_abapgit_html_form=>create( ).

    ro_form->radio(
      iv_name     = c_radio_name
      iv_label    = mv_title ).

    ASSIGN mr_list->* TO <lt_list>.
    LOOP AT <lt_list> ASSIGNING <ls_row>.
      lv_index = sy-tabix.

      IF mv_attr_name IS NOT INITIAL.
        ASSIGN COMPONENT mv_attr_name OF STRUCTURE <ls_row> TO <lv_val>.
        ASSERT sy-subrc = 0.
        lv_label = <lv_val>.
      ELSEIF mi_item_renderer IS BOUND.
        lv_label = mi_item_renderer->render(
          iv_item  = <ls_row>
          iv_index = lv_index )->render( ).
      ENDIF.

      ro_form->option(
        iv_label = lv_label
        iv_value = |{ lv_index }| ).

    ENDLOOP.

    ro_form->command(
      iv_label    = 'Choose'
      iv_cmd_type = Lif_abapgit_html_form=>c_cmd_type-input_main
      iv_action   = c_event-choose
    )->command(
      iv_label    = 'Back'
      iv_action   = c_event-back ).

  ENDMETHOD.
  METHOD get_result_idx.
    rv_index = mv_selected.
  ENDMETHOD.
  METHOD get_result_item.

    FIELD-SYMBOLS <lt_tab> TYPE STANDARD TABLE.

    CLEAR cs_selected.

    IF mv_selected > 0.
      ASSIGN mr_list->* TO <lt_tab>.
      READ TABLE <lt_tab> INDEX mv_selected INTO cs_selected.
      ASSERT sy-subrc = 0.
    ENDIF.

  ENDMETHOD.
  METHOD id.
    rv_id = mv_id.
  ENDMETHOD.
  METHOD is_fulfilled.
    rv_yes = mv_fulfilled.
  ENDMETHOD.
  METHOD is_in_page.
    rv_yes = mv_in_page.
  ENDMETHOD.
  METHOD return_state.
    IF mv_in_page = abap_true.
      rv_state = Lcl_abapgit_gui=>c_event_state-re_render.
    ELSE.
      rv_state = Lcl_abapgit_gui=>c_event_state-go_back.
    ENDIF.
  ENDMETHOD.
  METHOD set_id.
    mv_id = iv_id.
    ro_me = me.
  ENDMETHOD.
  METHOD set_in_page.
    mv_in_page = iv_in_page.
    ro_me = me.
  ENDMETHOD.
  METHOD was_cancelled.
    rv_yes = mv_cancelled.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    mo_form_data = mo_form_util->normalize( ii_event->form_data( ) ).
    mo_validation_log->clear( ).

    CASE ii_event->mv_action.
      WHEN c_event-back OR Lif_abapgit_definitions=>c_action-go_back.
        " Handle go_back as a "graceful back" - implicit cancel by F3/ESC
        mv_fulfilled = abap_true.
        mv_cancelled = abap_true.
        rs_handled-state = return_state( ).
      WHEN c_event-choose.
        mv_selected = mo_form_data->get( c_radio_name ).
        IF mv_selected = 0.
          mo_validation_log->set(
            iv_key = c_radio_name
            iv_val = 'You have to select one item' ).
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
        ELSE.
          mv_fulfilled = abap_true.
          rs_handled-state = return_state( ).
        ENDIF.
    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_page_title~get_page_title.
    rv_title = mv_title.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    ri_html = Lcl_abapgit_html=>create( mo_form->render(
      io_values         = mo_form_data
      io_validation_log = mo_validation_log ) ).
    register_handlers( ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PICKLIST implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SOD1 <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sod1=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sod1=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SOD1 implementation.
*"* method's implementations
*include methods.
  METHOD clear_content_fields.

    FIELD-SYMBOLS <ls_content_data> TYPE any.

    ASSIGN COMPONENT 'CONTENT' OF STRUCTURE cs_data TO <ls_content_data>.

    clear_field(
      EXPORTING
        iv_fieldname = 'CHANGE_USER'
      CHANGING
        cs_metadata  = <ls_content_data> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CHANGE_TIMESTAMP'
      CHANGING
        cs_metadata  = <ls_content_data> ).

  ENDMETHOD.
  METHOD clear_field.

    FIELD-SYMBOLS: <lv_value> TYPE data.

    ASSIGN COMPONENT iv_fieldname OF STRUCTURE cs_metadata TO <lv_value>.
    IF sy-subrc = 0.
      CLEAR: <lv_value>.
    ENDIF.

  ENDMETHOD.
  METHOD clear_metadata_fields.

    FIELD-SYMBOLS <ls_metadata> TYPE any.

    ASSIGN COMPONENT 'METADATA' OF STRUCTURE cs_data TO <ls_metadata>.

    clear_field(
      EXPORTING
        iv_fieldname = 'VERSION'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CREATED_AT'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CREATED_BY'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CHANGED_AT'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CHANGED_BY'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'RESPONSIBLE'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'PACKAGE_REF'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'MASTER_SYSTEM'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'DT_UUID'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'ABAP_LANGU_VERSION'
      CHANGING
        cs_metadata  = <ls_metadata> ).
    clear_field(
      EXPORTING
        iv_fieldname = 'ABAP_LANGU_VERSION'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'LINKS'
      CHANGING
        cs_metadata  = <ls_metadata> ).

  ENDMETHOD.
  METHOD constructor.

    DATA lo_data_model TYPE REF TO object.

    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).

    TRY.
        CREATE OBJECT lo_data_model TYPE (c_data_model_class_name).
      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |Object type { is_item-obj_type } is not supported by this system| ).
    ENDTRY.

  ENDMETHOD.
  METHOD create_wb_object_operator.

    DATA lx_error TYPE REF TO cx_root.

    TRY.

        CALL METHOD ('CL_WB_OBJECT_OPERATOR_FACTORY')=>('CREATE_OBJECT_OPERATOR')
          EXPORTING
            object_type       = is_object_type
            object_key        = iv_object_key
            transport_request = iv_transport_request
            do_commits        = iv_do_commits
            run_in_test_mode  = iv_run_in_test_mode
          RECEIVING
            result            = ro_wb_object_operator.

      CATCH cx_root INTO lx_error.

        Lcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

  ENDMETHOD.
  METHOD get_wb_object_operator.

    DATA lx_error TYPE REF TO cx_root.

    TRY.

        CALL METHOD ('CL_WB_OBJECT_OPERATOR_FACTORY')=>('GET_OBJECT_OPERATOR')
          EXPORTING
            object_type       = is_object_type
            object_key        = iv_object_key
            transport_request = iv_transport_request
          RECEIVING
            result            = ro_wb_object_operator.

      CATCH cx_root INTO lx_error.

        Lcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lo_data_model  TYPE REF TO if_wb_object_data_model,
          lo_factory     TYPE REF TO object,
          ls_object_type TYPE wbobjtype,
          lv_object_key  TYPE seu_objkey,
          lx_error       TYPE REF TO cx_root.

    TRY.

        ls_object_type-objtype_tr = ms_item-obj_type.
        lv_object_key             = ms_item-obj_name.

        lo_factory = create_wb_object_operator( is_object_type = ls_object_type
                                                iv_object_key  = lv_object_key ).

        CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~READ')
          IMPORTING
            eo_object_data = lo_data_model.

        rv_user = lo_data_model->get_changed_by( ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: ls_object_type TYPE wbobjtype,
          lv_object_key  TYPE seu_objkey,
          lo_factory     TYPE REF TO object,
          lx_error       TYPE REF TO cx_root.

    ls_object_type-objtype_tr = ms_item-obj_type.
    lv_object_key             = ms_item-obj_name.

    TRY.

        lo_factory = get_wb_object_operator( is_object_type       = ls_object_type
                                             iv_object_key        = lv_object_key
                                             iv_transport_request = iv_transport ).

        CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~DELETE').

      CATCH cx_root INTO lx_error.

        Lcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lo_factory               TYPE REF TO object,
          lo_data_model            TYPE REF TO if_wb_object_data_model,
          lv_data_type_name        TYPE string,
          ls_data                  TYPE REF TO data,
          ls_object_type           TYPE wbobjtype,
          lv_object_key            TYPE seu_objkey,
          lv_transport_request     TYPE trkorr,
          lo_logger                TYPE REF TO cl_wb_checklist,
          lx_create_error          TYPE REF TO cx_root,
          lx_error                 TYPE REF TO cx_root,
          lt_msgs                  TYPE TABLE OF string,
          lt_error_msgs_create     TYPE swbme_error_tab,
          ls_error_msg_create      LIKE LINE OF lt_error_msgs_create,
          lv_error_msg             TYPE string,
          lv_abap_language_version TYPE c LENGTH 1. " abap_language_version


    FIELD-SYMBOLS <ls_data> TYPE any.

    CREATE OBJECT lo_data_model TYPE (c_data_model_class_name).

    " if_wb_object_data_selection_co=>c_all_data
    CALL METHOD lo_data_model->('GET_DATATYPE_NAME')
      EXPORTING
        p_data_selection = 'AL'
      RECEIVING
        result           = lv_data_type_name.

    CREATE DATA ls_data TYPE (lv_data_type_name).
    ASSIGN ls_data->* TO <ls_data>.

    io_xml->read(
      EXPORTING
        iv_name = c_xml_transformation_name
      CHANGING
        cg_data = <ls_data> ).

    CALL METHOD lo_data_model->('SET_SELECTED_DATA')
      EXPORTING
        p_data_selection = 'AL' " if_wb_object_data_selection_co=>c_all_data
        p_data           = <ls_data>.

    TRY.

        ls_object_type-objtype_tr = ms_item-obj_type.
        lv_object_key             = ms_item-obj_name.

        lo_factory = get_wb_object_operator( is_object_type = ls_object_type
                                             iv_object_key  = lv_object_key ).

        lv_transport_request = Lcl_abapgit_default_transport=>get_instance( )->get( )-ordernum.

        IF Lif_abapgit_object~exists( ) = abap_true.

          CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~UPDATE')
            EXPORTING
              io_object_data    = lo_data_model
              version           = 'A'
              transport_request = lv_transport_request.

        ELSE.

          TRY.

              CALL METHOD lo_data_model->('GET_ABAP_LANGUAGE_VERSION')
                RECEIVING
                  result = lv_abap_language_version.

              CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~CREATE')
                EXPORTING
                  io_object_data        = lo_data_model
                  version               = 'A'
                  package               = iv_package
                  abap_language_version = lv_abap_language_version
                  transport_request     = lv_transport_request
                IMPORTING
                  logger                = lo_logger.

            CATCH cx_root INTO lx_create_error.

              " Check for error messages from Workbench API to provide more error infos to user
              lo_logger->get_error_messages( IMPORTING p_error_tab = lt_error_msgs_create ).

              IF lt_error_msgs_create IS NOT INITIAL.

                LOOP AT lt_error_msgs_create INTO ls_error_msg_create.

                  APPEND LINES OF ls_error_msg_create-mtext TO lt_msgs.

                ENDLOOP.

                CONCATENATE LINES OF lt_msgs INTO lv_error_msg SEPARATED BY '; '.
                Lcx_abapgit_exception=>raise( iv_text     = lv_error_msg
                                              ix_previous = lx_create_error ).

              ELSE.

                Lcx_abapgit_exception=>raise_with_text( lx_create_error ).

              ENDIF.

          ENDTRY.

        ENDIF.

      CATCH cx_root INTO lx_error.

        Lcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lo_factory     TYPE REF TO object,
          ls_object_type TYPE wbobjtype,
          lv_object_key  TYPE seu_objkey,
          lx_error       TYPE REF TO cx_root.

    TRY.

        ls_object_type-objtype_tr = ms_item-obj_type.
        lv_object_key             = ms_item-obj_name.

        lo_factory = get_wb_object_operator( is_object_type = ls_object_type
                                             iv_object_key  = lv_object_key ).

        CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~CHECK_EXISTENCE')
          RECEIVING
            r_result = rv_bool.

      CATCH cx_root INTO lx_error.

        Lcx_abapgit_exception=>raise_with_text( lx_error ).

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
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }*| ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lo_data_model     TYPE REF TO if_wb_object_data_model,
          lv_data_type_name TYPE string,
          lo_factory        TYPE REF TO object,
          ls_object_type    TYPE wbobjtype,
          lv_object_key     TYPE seu_objkey,
          lx_error          TYPE REF TO cx_root.

    DATA ls_data TYPE REF TO data.
    FIELD-SYMBOLS <ls_data> TYPE any.

    TRY.

        ls_object_type-objtype_tr = ms_item-obj_type.
        lv_object_key             = ms_item-obj_name.

        lo_factory = create_wb_object_operator( is_object_type = ls_object_type
                                                iv_object_key  = lv_object_key ).

        CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~READ')
          IMPORTING
            eo_object_data = lo_data_model.

        " if_wb_object_data_selection_co=>c_all_data
        CALL METHOD lo_data_model->('GET_DATATYPE_NAME')
          EXPORTING
            p_data_selection = 'AL'
          RECEIVING
            result           = lv_data_type_name.

        CREATE DATA ls_data TYPE (lv_data_type_name).
        ASSIGN ls_data->* TO <ls_data>.

        CALL METHOD lo_data_model->('GET_SELECTED_DATA')
          EXPORTING
            p_data_selection = 'AL' " if_wb_object_data_selection_co=>c_all_data
          IMPORTING
            p_data           = <ls_data>.

        clear_metadata_fields( CHANGING cs_data = <ls_data> ).
        clear_content_fields( CHANGING cs_data = <ls_data> ).
        clear_field( EXPORTING iv_fieldname = 'PLUGIN_CONFIG'
                     CHANGING  cs_metadata  = <ls_data> ).

        io_xml->add( iv_name = c_xml_transformation_name
                     ig_data = <ls_data> ).

      CATCH cx_root INTO lx_error.

        Lcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SOD1 implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SOD2 <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sod2=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sod2=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SOD2 implementation.
*"* method's implementations
*include methods.
  METHOD clear_content_fields.

    FIELD-SYMBOLS <ls_content_data> TYPE any.

    ASSIGN COMPONENT 'CONTENT' OF STRUCTURE cs_data TO <ls_content_data>.

    clear_field(
      EXPORTING
        iv_fieldname = 'CHANGE_USER'
      CHANGING
        cs_metadata  = <ls_content_data> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CHANGE_TIMESTAMP'
      CHANGING
        cs_metadata  = <ls_content_data> ).

  ENDMETHOD.
  METHOD clear_field.

    FIELD-SYMBOLS: <lv_value> TYPE data.

    ASSIGN COMPONENT iv_fieldname OF STRUCTURE cs_metadata TO <lv_value>.
    IF sy-subrc = 0.
      CLEAR: <lv_value>.
    ENDIF.

  ENDMETHOD.
  METHOD clear_metadata_fields.

    FIELD-SYMBOLS <ls_metadata> TYPE any.

    ASSIGN COMPONENT 'METADATA' OF STRUCTURE cs_data TO <ls_metadata>.

    clear_field(
      EXPORTING
        iv_fieldname = 'VERSION'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CREATED_AT'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CREATED_BY'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CHANGED_AT'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CHANGED_BY'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'RESPONSIBLE'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'PACKAGE_REF'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'MASTER_SYSTEM'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'DT_UUID'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'ABAP_LANGU_VERSION'
      CHANGING
        cs_metadata  = <ls_metadata> ).
    clear_field(
      EXPORTING
        iv_fieldname = 'ABAP_LANGU_VERSION'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'LINKS'
      CHANGING
        cs_metadata  = <ls_metadata> ).

  ENDMETHOD.
  METHOD constructor.

    DATA lo_data_model TYPE REF TO object.

    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).

    TRY.
        CREATE OBJECT lo_data_model TYPE (c_data_model_class_name).
      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |Object type { is_item-obj_type } is not supported by this system| ).
    ENDTRY.

  ENDMETHOD.
  METHOD create_wb_object_operator.

    DATA lx_error TYPE REF TO cx_root.

    TRY.

        CALL METHOD ('CL_WB_OBJECT_OPERATOR_FACTORY')=>('CREATE_OBJECT_OPERATOR')
          EXPORTING
            object_type       = is_object_type
            object_key        = iv_object_key
            transport_request = iv_transport_request
            do_commits        = iv_do_commits
            run_in_test_mode  = iv_run_in_test_mode
          RECEIVING
            result            = ro_wb_object_operator.

      CATCH cx_root INTO lx_error.

        Lcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

  ENDMETHOD.
  METHOD get_wb_object_operator.

    DATA lx_error TYPE REF TO cx_root.

    TRY.

        CALL METHOD ('CL_WB_OBJECT_OPERATOR_FACTORY')=>('GET_OBJECT_OPERATOR')
          EXPORTING
            object_type       = is_object_type
            object_key        = iv_object_key
            transport_request = iv_transport_request
          RECEIVING
            result            = ro_wb_object_operator.

      CATCH cx_root INTO lx_error.

        Lcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lo_data_model  TYPE REF TO if_wb_object_data_model,
          lo_factory     TYPE REF TO object,
          ls_object_type TYPE wbobjtype,
          lv_object_key  TYPE seu_objkey,
          lx_error       TYPE REF TO cx_root.

    TRY.

        ls_object_type-objtype_tr = ms_item-obj_type.
        lv_object_key             = ms_item-obj_name.

        lo_factory = create_wb_object_operator( is_object_type = ls_object_type
                                                iv_object_key  = lv_object_key ).

        CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~READ')
          IMPORTING
            eo_object_data = lo_data_model.

        rv_user = lo_data_model->get_changed_by( ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: ls_object_type TYPE wbobjtype,
          lv_object_key  TYPE seu_objkey,
          lo_factory     TYPE REF TO object,
          lx_error       TYPE REF TO cx_root.

    ls_object_type-objtype_tr = ms_item-obj_type.
    lv_object_key             = ms_item-obj_name.

    TRY.

        lo_factory = get_wb_object_operator( is_object_type       = ls_object_type
                                             iv_object_key        = lv_object_key
                                             iv_transport_request = iv_transport ).

        CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~DELETE').

      CATCH cx_root INTO lx_error.

        Lcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lo_factory               TYPE REF TO object,
          lo_data_model            TYPE REF TO if_wb_object_data_model,
          lv_data_type_name        TYPE string,
          ls_data                  TYPE REF TO data,
          ls_object_type           TYPE wbobjtype,
          lv_object_key            TYPE seu_objkey,
          lv_transport_request     TYPE trkorr,
          lo_logger                TYPE REF TO cl_wb_checklist,
          lx_create_error          TYPE REF TO cx_root,
          lx_error                 TYPE REF TO cx_root,
          lt_msgs                  TYPE TABLE OF string,
          lt_error_msgs_create     TYPE swbme_error_tab,
          ls_error_msg_create      LIKE LINE OF lt_error_msgs_create,
          lv_error_msg             TYPE string,
          lv_abap_language_version TYPE c LENGTH 1. " abap_language_version

    FIELD-SYMBOLS <ls_data> TYPE any.

    CREATE OBJECT lo_data_model TYPE (c_data_model_class_name).

    " if_wb_object_data_selection_co=>c_all_data
    CALL METHOD lo_data_model->('GET_DATATYPE_NAME')
      EXPORTING
        p_data_selection = 'AL'
      RECEIVING
        result           = lv_data_type_name.

    CREATE DATA ls_data TYPE (lv_data_type_name).
    ASSIGN ls_data->* TO <ls_data>.

    io_xml->read(
      EXPORTING
        iv_name = c_xml_transformation_name
      CHANGING
        cg_data = <ls_data> ).

    CALL METHOD lo_data_model->('SET_SELECTED_DATA')
      EXPORTING
        p_data_selection = 'AL' " if_wb_object_data_selection_co=>c_all_data
        p_data           = <ls_data>.

    TRY.

        ls_object_type-objtype_tr = ms_item-obj_type.
        lv_object_key             = ms_item-obj_name.

        lo_factory = get_wb_object_operator( is_object_type = ls_object_type
                                             iv_object_key  = lv_object_key ).

        lv_transport_request = Lcl_abapgit_default_transport=>get_instance( )->get( )-ordernum.

        IF Lif_abapgit_object~exists( ) = abap_true.

          CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~UPDATE')
            EXPORTING
              io_object_data    = lo_data_model
              version           = 'A'
              transport_request = lv_transport_request.

        ELSE.

          TRY.

              CALL METHOD lo_data_model->('GET_ABAP_LANGUAGE_VERSION')
                RECEIVING
                  result = lv_abap_language_version.

              CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~CREATE')
                EXPORTING
                  io_object_data        = lo_data_model
                  version               = 'A'
                  package               = iv_package
                  abap_language_version = lv_abap_language_version
                  transport_request     = lv_transport_request
                IMPORTING
                  logger                = lo_logger.

            CATCH cx_root INTO lx_create_error.

              " Check for error messages from Workbench API to provide more error infos to user
              lo_logger->get_error_messages( IMPORTING p_error_tab = lt_error_msgs_create ).

              IF lt_error_msgs_create IS NOT INITIAL.

                LOOP AT lt_error_msgs_create INTO ls_error_msg_create.

                  APPEND LINES OF ls_error_msg_create-mtext TO lt_msgs.

                ENDLOOP.

                CONCATENATE LINES OF lt_msgs INTO lv_error_msg SEPARATED BY '; '.
                Lcx_abapgit_exception=>raise( iv_text     = lv_error_msg
                                              ix_previous = lx_create_error ).

              ELSE.

                Lcx_abapgit_exception=>raise_with_text( lx_create_error ).

              ENDIF.

          ENDTRY.

        ENDIF.

      CATCH cx_root INTO lx_error.

        Lcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lo_factory     TYPE REF TO object,
          ls_object_type TYPE wbobjtype,
          lv_object_key  TYPE seu_objkey,
          lx_error       TYPE REF TO cx_root.

    TRY.

        ls_object_type-objtype_tr = ms_item-obj_type.
        lv_object_key             = ms_item-obj_name.

        lo_factory = get_wb_object_operator( is_object_type = ls_object_type
                                             iv_object_key  = lv_object_key ).

        CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~CHECK_EXISTENCE')
          RECEIVING
            r_result = rv_bool.

      CATCH cx_root INTO lx_error.

        Lcx_abapgit_exception=>raise_with_text( lx_error ).

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
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }*| ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lo_data_model     TYPE REF TO if_wb_object_data_model,
          lv_data_type_name TYPE string,
          lo_factory        TYPE REF TO object,
          ls_object_type    TYPE wbobjtype,
          lv_object_key     TYPE seu_objkey,
          lx_error          TYPE REF TO cx_root.

    DATA ls_data TYPE REF TO data.
    FIELD-SYMBOLS <ls_data> TYPE any.

    TRY.

        ls_object_type-objtype_tr = ms_item-obj_type.
        lv_object_key             = ms_item-obj_name.

        lo_factory = create_wb_object_operator( is_object_type = ls_object_type
                                                iv_object_key  = lv_object_key ).

        CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~READ')
          IMPORTING
            eo_object_data = lo_data_model.

        " if_wb_object_data_selection_co=>c_all_data
        CALL METHOD lo_data_model->('GET_DATATYPE_NAME')
          EXPORTING
            p_data_selection = 'AL'
          RECEIVING
            result           = lv_data_type_name.

        CREATE DATA ls_data TYPE (lv_data_type_name).
        ASSIGN ls_data->* TO <ls_data>.

        CALL METHOD lo_data_model->('GET_SELECTED_DATA')
          EXPORTING
            p_data_selection = 'AL' " if_wb_object_data_selection_co=>c_all_data
          IMPORTING
            p_data           = <ls_data>.

        clear_metadata_fields( CHANGING cs_data = <ls_data> ).
        clear_content_fields( CHANGING cs_data = <ls_data> ).
        clear_field( EXPORTING iv_fieldname = 'PLUGIN_CONFIG'
                     CHANGING  cs_metadata  = <ls_data> ).

        io_xml->add( iv_name = c_xml_transformation_name
                     ig_data = <ls_data> ).

      CATCH cx_root INTO lx_error.

        Lcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SOD2 implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_CHKC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_chkc=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_chkc=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_CHKC implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lr_data        TYPE REF TO data,
          lo_chkc_db_api TYPE REF TO object,
          lv_name        TYPE c LENGTH 30,
          lx_error       TYPE REF TO cx_root.

    FIELD-SYMBOLS: <ls_chkc_header> TYPE any,
                   <ls_chkc_user>   TYPE any.

    TRY.
        CREATE OBJECT lo_chkc_db_api TYPE ('CL_CHKC_DB_API').
        CREATE DATA lr_data TYPE ('CL_CHKC_DB_API=>TY_HEADER').
        ASSIGN lr_data->* TO <ls_chkc_header>.

        lv_name = ms_item-obj_name.

        CALL METHOD lo_chkc_db_api->('GET_HEADER')
          EXPORTING
            name    = lv_name
            version = 'I'
          RECEIVING
            header  = <ls_chkc_header>.

        IF <ls_chkc_header> IS INITIAL.
          CALL METHOD lo_chkc_db_api->('GET_HEADER')
            EXPORTING
              name    = lv_name
              version = 'A'
            RECEIVING
              header  = <ls_chkc_header>.
        ENDIF.

        ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <ls_chkc_header> TO <ls_chkc_user>.
        rv_user = <ls_chkc_user>.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_CHKC implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_CHKO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_chko=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_chko=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_CHKO implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lr_data        TYPE REF TO data,
          lo_chko_db_api TYPE REF TO object,
          lv_name        TYPE c LENGTH 30,
          lx_error       TYPE REF TO cx_root.

    FIELD-SYMBOLS: <ls_chko_header> TYPE any,
                   <ls_chko_user>   TYPE any.


    TRY.
        CREATE OBJECT lo_chko_db_api TYPE ('CL_CHKO_DB_API').
        CREATE DATA lr_data TYPE ('CL_CHKO_DB_API=>TY_HEADER').
        ASSIGN lr_data->* TO <ls_chko_header>.

        lv_name = ms_item-obj_name.

        CALL METHOD lo_chko_db_api->('GET_HEADER')
          EXPORTING
            name    = lv_name
            version = 'I'
          RECEIVING
            header  = <ls_chko_header>.

        IF <ls_chko_header> IS INITIAL.
          CALL METHOD lo_chko_db_api->('GET_HEADER')
            EXPORTING
              name    = lv_name
              version = 'A'
            RECEIVING
              header  = <ls_chko_header>.
        ENDIF.

        ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <ls_chko_header> TO <ls_chko_user>.
        rv_user = <ls_chko_user>.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_CHKO implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_CHKV <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_chkv=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_chkv=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_CHKV implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lr_data        TYPE REF TO data,
          lo_chkv_db_api TYPE REF TO object,
          lv_name        TYPE c LENGTH 180,
          lx_error       TYPE REF TO cx_root.

    FIELD-SYMBOLS: <ls_chkv_header> TYPE any,
                   <ls_chkv_user>   TYPE any.


    TRY.
        CREATE OBJECT lo_chkv_db_api TYPE ('CL_CHKV_DB_API').
        CREATE DATA lr_data TYPE ('CL_CHKV_DB_API=>TY_HEADER').
        ASSIGN lr_data->* TO <ls_chkv_header>.

        lv_name = ms_item-obj_name.

        CALL METHOD lo_chkv_db_api->('GET_HEADER')
          EXPORTING
            object_key = lv_name
            version    = 'I'
          RECEIVING
            header  = <ls_chkv_header>.

        IF <ls_chkv_header> IS INITIAL.
          CALL METHOD lo_chkv_db_api->('GET_HEADER')
            EXPORTING
              object_key = lv_name
              version    = 'A'
            RECEIVING
              header  = <ls_chkv_header>.
        ENDIF.

        ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <ls_chkv_header> TO <ls_chkv_user>.
        rv_user = <ls_chkv_user>.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_CHKV implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SSST <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ssst=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ssst=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SSST implementation.
*"* method's implementations
*include methods.
  METHOD validate_font.

    DATA: lv_tdfamily TYPE tfo01-tdfamily.


    SELECT SINGLE tdfamily FROM tfo01 INTO lv_tdfamily
      WHERE tdfamily = iv_tdfamily.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Font family not found' ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE lastuser FROM stxsadm INTO rv_user
      WHERE stylename = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_stylename TYPE tdssname.


    lv_stylename = ms_item-obj_name.

    CALL FUNCTION 'SSF_DELETE_STYLE'
      EXPORTING
        i_stylename           = lv_stylename
        i_with_dialog         = abap_false
        i_with_confirm_dialog = abap_false
      EXCEPTIONS
        no_name               = 1
        no_style              = 2
        style_locked          = 3
        cancelled             = 4
        no_access_permission  = 5
        illegal_language      = 6
        OTHERS                = 7.
    IF sy-subrc <> 0 AND sy-subrc <> 2.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.
* see fm SSF_UPLOAD_STYLE

    DATA: ls_header     TYPE ssfcats,
          ls_new_header TYPE ssfcats,
          lt_paragraphs TYPE TABLE OF ssfparas,
          lt_strings    TYPE TABLE OF ssfstrings,
          lt_tabstops   TYPE TABLE OF stxstab.

    FIELD-SYMBOLS: <lv_spras> TYPE spras.


    io_xml->read( EXPORTING iv_name = 'HEADER'
                  CHANGING cg_data = ls_header ).
    io_xml->read( EXPORTING iv_name = 'SSFPARAS'
                  CHANGING cg_data = lt_paragraphs ).
    io_xml->read( EXPORTING iv_name = 'SSFSTRINGS'
                  CHANGING cg_data = lt_strings ).
    io_xml->read( EXPORTING iv_name = 'STXSTAB'
                  CHANGING cg_data = lt_tabstops ).

    validate_font( ls_header-tdfamily ).

    CALL FUNCTION 'SSF_READ_STYLE' "Just load FG
      EXPORTING
        i_style_name        = ls_header-stylename
        i_style_active_flag = 'A'
      EXCEPTIONS
        OTHERS              = 0.

    set_default_package( iv_package ).
    ASSIGN ('(SAPLSTXBS)MASTER_LANGUAGE') TO <lv_spras>.
    IF sy-subrc = 0.
      <lv_spras> = ls_header-masterlang.
    ENDIF.

    tadir_insert( iv_package ).

    CALL FUNCTION 'SSF_SAVE_STYLE'
      EXPORTING
        i_header     = ls_header
      IMPORTING
        e_header     = ls_new_header
      TABLES
        i_paragraphs = lt_paragraphs
        i_strings    = lt_strings
        i_tabstops   = lt_tabstops.

    IF ls_new_header IS NOT INITIAL.

      CALL FUNCTION 'SSF_ACTIVATE_STYLE'
        EXPORTING
          i_stylename          = ls_header-stylename
          redirect_error_msg   = abap_true " otherwise warnings write list output
        EXCEPTIONS
          no_name              = 1
          no_style             = 2
          cancelled            = 3
          no_access_permission = 4
          illegal_language     = 5
          OTHERS               = 6.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_stylename TYPE stxsadm-stylename.

    SELECT SINGLE stylename
      FROM stxshead INTO lv_stylename
      WHERE active    = c_style_active
        AND stylename = ms_item-obj_name
        AND vari      = ''.
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
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'E_SMSTYLE'
                                            iv_argument    = |{ ms_item-obj_name }| ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: ls_bcdata TYPE bdcdata,
          lt_bcdata TYPE STANDARD TABLE OF bdcdata.

    ls_bcdata-program  = 'SAPMSSFS'.
    ls_bcdata-dynpro   = '0100'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam     = 'SSFSCREENS-SNAME'.
    ls_bcdata-fval     = ms_item-obj_name.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=DISPLAY'.
    APPEND ls_bcdata TO lt_bcdata.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SMARTSTYLES'
      it_bdcdata = lt_bcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.
* see fm SSF_DOWNLOAD_STYLE

    DATA: lv_style_name TYPE tdssname,
          ls_header     TYPE ssfcats,
          lt_paragraphs TYPE TABLE OF ssfparas,
          lt_strings    TYPE TABLE OF ssfstrings,
          lt_tabstops   TYPE TABLE OF stxstab.


    lv_style_name = ms_item-obj_name.

    CALL FUNCTION 'SSF_READ_STYLE'
      EXPORTING
        i_style_name             = lv_style_name
        i_style_active_flag      = c_style_active
        i_style_variant          = '%MAIN'
        i_style_language         = mv_language
      IMPORTING
        e_header                 = ls_header
      TABLES
        e_paragraphs             = lt_paragraphs
        e_strings                = lt_strings
        e_tabstops               = lt_tabstops
      EXCEPTIONS
        no_name                  = 1
        no_style                 = 2
        active_style_not_found   = 3
        inactive_style_not_found = 4
        no_variant               = 5
        no_main_variant          = 6
        cancelled                = 7
        no_access_permission     = 8
        OTHERS                   = 9.
    IF sy-subrc = 2.
      RETURN.
    ELSEIF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CLEAR ls_header-version.
    CLEAR ls_header-firstuser.
    CLEAR ls_header-firstdate.
    CLEAR ls_header-firsttime.
    CLEAR ls_header-lastuser.
    CLEAR ls_header-lastdate.
    CLEAR ls_header-lasttime.

    io_xml->add( iv_name = 'HEADER'
                 ig_data = ls_header ).
    io_xml->add( ig_data = lt_paragraphs
                 iv_name = 'SSFPARAS' ).
    io_xml->add( ig_data = lt_strings
                 iv_name = 'SSFSTRINGS' ).
    io_xml->add( ig_data = lt_tabstops
                 iv_name = 'STXSTAB' ).

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
endclass. "ZCL_ABAPGIT_OBJECT_SSST implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_APIS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_apis=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_apis=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_APIS implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    DATA lr_data TYPE REF TO data.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    TRY.
        CREATE DATA lr_data TYPE (c_model).
      CATCH cx_sy_create_error.
        Lcx_abapgit_exception=>raise( |APIS not supported by your NW release| ).
    ENDTRY.

  ENDMETHOD.
  METHOD initialize.

    IF mo_handler IS NOT BOUND.
      CREATE OBJECT mo_handler TYPE ('CL_ARS_API_ABAPGIT')
        EXPORTING
          iv_api_object_name = ms_item-obj_name.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    initialize( ).

    TRY.
        CALL METHOD mo_handler->('IF_ARS_API_ABAPGIT~GET_CHANGED_BY')
          RECEIVING
            rv_changed_by = rv_user.
      CATCH cx_root.
        rv_user = c_user_unknown.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

* IF_ARS_API_ABAPGIT~DELETE_API_STATE dumps and checks fail, even tho I as a developer can delete it

    DATA lo_db   TYPE REF TO object.
    DATA lr_data TYPE REF TO data.
    FIELD-SYMBOLS <ls_api_key> TYPE any.


    CREATE DATA lr_data TYPE ('IF_ARS_STATE_DB_ACCESS=>TY_S_API_KEY').
    ASSIGN lr_data->* TO <ls_api_key>.
    <ls_api_key> = ms_item-obj_name.
    ASSERT <ls_api_key> IS NOT INITIAL.

    CALL METHOD ('CL_ARS_STATE_DB_ACCESS')=>('GET_INSTANCE')
      RECEIVING
        ro_state_db_access = lo_db.

    CALL METHOD lo_db->('IF_ARS_STATE_DB_ACCESS~DELETE')
      EXPORTING
        is_api_key = <ls_api_key>.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

* IF_ARS_API_ABAPGIT~SAVE_API_STATE dumps in some package checks

    DATA lr_data              TYPE REF TO data.
    DATA lo_db                TYPE REF TO object.
    FIELD-SYMBOLS <ls_data>   TYPE any.
    FIELD-SYMBOLS <lt_data_states> TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_header> TYPE any.
    FIELD-SYMBOLS <lt_states> TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_state>  TYPE any.
    FIELD-SYMBOLS <ls_row>    TYPE any.
    FIELD-SYMBOLS <lv_simple> TYPE simple.


    CREATE DATA lr_data TYPE (c_model).
    ASSIGN lr_data->* TO <ls_data>.
    CREATE DATA lr_data TYPE ('IF_ARS_STATE_DB_ACCESS=>TY_S_HEADER').
    ASSIGN lr_data->* TO <ls_header>.
    CREATE DATA lr_data TYPE ('IF_ARS_STATE_DB_ACCESS=>TY_T_STATE').
    ASSIGN lr_data->* TO <lt_states>.

    io_xml->read(
      EXPORTING
        iv_name = 'APIS'
      CHANGING
        cg_data = <ls_data> ).

    MOVE-CORRESPONDING <ls_data> TO <ls_header>.

    ASSIGN COMPONENT 'API_STATES' OF STRUCTURE <ls_data> TO <lt_data_states>.
    ASSERT sy-subrc = 0.

* the state table is sorted,
    LOOP AT <lt_data_states> ASSIGNING <ls_state>.
      CREATE DATA lr_data TYPE ('IF_ARS_STATE_DB_ACCESS=>TY_S_STATE').
      ASSIGN lr_data->* TO <ls_row>.
      MOVE-CORRESPONDING <ls_state> TO <ls_row>.
      MOVE-CORRESPONDING <ls_header> TO <ls_row>.

      ASSIGN COMPONENT 'SOFTWARE_RELEASE_NAME' OF STRUCTURE <ls_row> TO <lv_simple>.
      ASSERT sy-subrc = 0.
      <lv_simple> = '1908'.
      ASSIGN COMPONENT 'CREATED_AT' OF STRUCTURE <ls_row> TO <lv_simple>.
      ASSERT sy-subrc = 0.
      <lv_simple> = sy-datum.
      ASSIGN COMPONENT 'CREATED_BY' OF STRUCTURE <ls_row> TO <lv_simple>.
      ASSERT sy-subrc = 0.
      <lv_simple> = sy-uname.
      ASSIGN COMPONENT 'LAST_CHANGED_AT' OF STRUCTURE <ls_row> TO <lv_simple>.
      ASSERT sy-subrc = 0.
      <lv_simple> = sy-datum.
      ASSIGN COMPONENT 'LAST_CHANGED_BY' OF STRUCTURE <ls_row> TO <lv_simple>.
      ASSERT sy-subrc = 0.
      <lv_simple> = sy-uname.

      INSERT <ls_row> INTO TABLE <lt_states>.
    ENDLOOP.

    CALL METHOD ('CL_ARS_STATE_DB_ACCESS')=>('GET_INSTANCE')
      RECEIVING
        ro_state_db_access = lo_db.

    CALL METHOD lo_db->('IF_ARS_STATE_DB_ACCESS~SAVE')
      EXPORTING
        is_header         = <ls_header>
        it_release_states = <lt_states>.

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    TRY.
        initialize( ).
        CALL METHOD mo_handler->('IF_ARS_API_ABAPGIT~CHECK_EXISTS')
          RECEIVING
            rv_api_exists = rv_bool.
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
    rs_metadata-version = 'v2.0.0'.
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = Lif_abapgit_object~exists( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
* looks like there is no enqueue lock
* E_ARS_API ?
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " todo
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA lr_data TYPE REF TO data.
    FIELD-SYMBOLS <ls_data> TYPE any.

    CREATE DATA lr_data TYPE (c_model).
    ASSIGN lr_data->* TO <ls_data>.

    initialize( ).

    CALL METHOD mo_handler->('IF_ARS_API_ABAPGIT~GET_API_STATE')
      RECEIVING
        rs_apis_object = <ls_data>.

    io_xml->add( iv_name = 'APIS'
                 ig_data = <ls_data> ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_APIS implementation

*>>>>>>> ZCL_ABAPGIT_ITEM_GRAPH <<<<<<<*

*"* macro definitions
*include zcl_abapgit_item_graph========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_item_graph========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_item_graph========ccau.



class LCL_ABAPGIT_ITEM_GRAPH implementation.
*"* method's implementations
*include methods.
  METHOD add_edge.
    DATA ls_edge LIKE LINE OF mt_edges.
    ASSERT is_from IS NOT INITIAL.
    ASSERT is_to IS NOT INITIAL.
    ls_edge-from = is_from.
    ls_edge-to   = is_to.
    APPEND ls_edge TO mt_edges.
  ENDMETHOD.
  METHOD constructor.
    INSERT LINES OF it_items INTO TABLE mt_vertices.
  ENDMETHOD.
  METHOD get_next.
* find a vertex with no inbound edges, if it does not exist pick anything

    DATA ls_vertex LIKE LINE OF mt_vertices.
    DATA lv_index  TYPE i.

    LOOP AT mt_vertices INTO ls_vertex.
      lv_index = sy-tabix.
      READ TABLE mt_edges WITH KEY sec_key COMPONENTS
        to-obj_type = ls_vertex-obj_type
        to-obj_name = ls_vertex-obj_name
        TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        remove_vertex( lv_index ).
        rs_item = ls_vertex.
        RETURN.
      ENDIF.
    ENDLOOP.

    IF mv_warning = abap_false.
* only issue the warning once per graph
      ii_log->add_warning( |Cycle detected in item graph| ).
      mv_warning = abap_true.
    ENDIF.

    READ TABLE mt_vertices INTO rs_item INDEX 1.
    ASSERT sy-subrc = 0.
    remove_vertex( 1 ).

  ENDMETHOD.
  METHOD has_vertices.
    rv_bool = boolc( lines( mt_vertices ) > 0 ).
  ENDMETHOD.
  METHOD remove_vertex.
    DATA ls_vertex LIKE LINE OF mt_vertices.

    READ TABLE mt_vertices INDEX iv_index INTO ls_vertex.
    ASSERT sy-subrc = 0.

    DELETE mt_vertices INDEX iv_index.
    DELETE mt_edges WHERE
      from-obj_type = ls_vertex-obj_type AND
      from-obj_name = ls_vertex-obj_name.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ITEM_GRAPH implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_STYL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_styl=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_styl=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_STYL implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    DATA: ls_style TYPE ty_style,
          lv_name  TYPE itcda-tdstyle.


    lv_name = ms_item-obj_name.

    CALL FUNCTION 'READ_STYLE'
      EXPORTING
        style        = lv_name
      IMPORTING
        style_header = ls_style-header
      TABLES
        paragraphs   = ls_style-paragraphs
        strings      = ls_style-strings
        tabs         = ls_style-tabs.

    rv_user = ls_style-header-tdluser.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_style TYPE itcda-tdstyle.


    lv_style = ms_item-obj_name.

    CALL FUNCTION 'DELETE_STYLE'
      EXPORTING
        style    = lv_style
        language = '*'.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_style TYPE ty_style.


    io_xml->read( EXPORTING iv_name = 'STYLE'
                  CHANGING cg_data = ls_style ).

    CALL FUNCTION 'SAVE_STYLE'
      EXPORTING
        style_header = ls_style-header
      TABLES
        paragraphs   = ls_style-paragraphs
        strings      = ls_style-strings
        tabs         = ls_style-tabs.

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: ls_style TYPE ty_style,
          lv_name  TYPE itcda-tdstyle,
          lv_found TYPE abap_bool.


    lv_name = ms_item-obj_name.

    CALL FUNCTION 'READ_STYLE'
      EXPORTING
        style      = lv_name
      IMPORTING
        found      = lv_found
      TABLES
        paragraphs = ls_style-paragraphs
        strings    = ls_style-strings
        tabs       = ls_style-tabs.

    rv_bool = boolc( lv_found = abap_true ).

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

    DATA: ls_bcdata TYPE bdcdata,
          lt_bcdata TYPE STANDARD TABLE OF bdcdata.

    ls_bcdata-program  = 'SAPMSSCS'.
    ls_bcdata-dynpro   = '1100'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam     = 'RSSCS-TDSTYLE'.
    ls_bcdata-fval     = ms_item-obj_name.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam     = 'RSSCS-TDSPRAS'.
    ls_bcdata-fval     = mv_language.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam     = 'RSSCS-TDHEADEROB'.
    ls_bcdata-fval     = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=SHOW'.
    APPEND ls_bcdata TO lt_bcdata.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SE72'
      it_bdcdata = lt_bcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_style TYPE ty_style,
          lv_name  TYPE itcda-tdstyle.


    lv_name = ms_item-obj_name.

    CALL FUNCTION 'READ_STYLE'
      EXPORTING
        style        = lv_name
      IMPORTING
        style_header = ls_style-header
      TABLES
        paragraphs   = ls_style-paragraphs
        strings      = ls_style-strings
        tabs         = ls_style-tabs.

    CLEAR: ls_style-header-tdfuser,
           ls_style-header-tdfdate,
           ls_style-header-tdftime,
           ls_style-header-tdfreles,
           ls_style-header-tdluser,
           ls_style-header-tdldate,
           ls_style-header-tdltime,
           ls_style-header-tdlreles.

    io_xml->add( iv_name = 'STYLE'
                 ig_data = ls_style ).

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
endclass. "ZCL_ABAPGIT_OBJECT_STYL implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_CMPT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_cmpt=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_cmpt=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_CMPT implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    TRY.
        CALL METHOD ('CL_CMP_TEMPLATE')=>('S_GET_DB_ACCESS')
          RECEIVING
            r_ref_db_access = mo_cmp_db.

      CATCH cx_root.
    ENDTRY.

    mv_name = ms_item-obj_name.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lo_cmp_template TYPE REF TO object.

    TRY.
        CALL METHOD ('CL_CMP_TEMPLATE')=>('S_CREATE_FROM_DB')
          EXPORTING
            i_name         = mv_name
            i_version      = 'A'
          RECEIVING
            r_ref_template = lo_cmp_template.

        CALL METHOD lo_cmp_template->('IF_CMP_TEMPLATE_EDIT~GET_CHANGE_USER')
          RECEIVING
            r_user = rv_user.

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( 'CMPT not supported' ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_deleted TYPE abap_bool.

    TRY.
        CALL METHOD mo_cmp_db->('IF_CMP_TEMPLATE_DB~DELETE_TEMPLATE')
          EXPORTING
            i_name        = mv_name
            i_version     = 'A'
            i_flg_header  = abap_true
            i_flg_lines   = abap_true
          RECEIVING
            r_flg_deleted = lv_deleted.

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( 'CMPT not supported' ).
    ENDTRY.

    IF lv_deleted = abap_false.
      Lcx_abapgit_exception=>raise( |Error deleting CMPT { ms_item-obj_name }| ).
    ENDIF.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lr_template TYPE REF TO data.
    FIELD-SYMBOLS: <lg_template> TYPE any,
                   <lg_header>   TYPE any,
                   <lg_field>    TYPE any.

    TRY.
        CREATE DATA lr_template TYPE ('IF_CMP_TEMPLATE_DB=>TYP_TEMPLATE').
        ASSIGN lr_template->* TO <lg_template>.

        io_xml->read(
          EXPORTING
            iv_name = 'CMPT'
          CHANGING
            cg_data = <lg_template> ).

        ASSIGN COMPONENT 'STR_HEADER' OF STRUCTURE <lg_template> TO <lg_header>.
        IF sy-subrc = 0.
          ASSIGN COMPONENT 'NAME' OF STRUCTURE <lg_header> TO <lg_field>.
          IF sy-subrc = 0.
            <lg_field> = ms_item-obj_name.
          ENDIF.
          ASSIGN COMPONENT 'VERSION' OF STRUCTURE <lg_header> TO <lg_field>.
          IF sy-subrc = 0.
            <lg_field> = 'A'.
          ENDIF.
        ENDIF.

        CALL METHOD mo_cmp_db->('IF_CMP_TEMPLATE_DB~SAVE_TEMPLATE')
          EXPORTING
            i_template_db = <lg_template>
            i_flg_header  = abap_true
            i_flg_lines   = abap_true.

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( 'CMPT not supported' ).
    ENDTRY.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    TRY.
        CALL METHOD ('CL_CMP_TEMPLATE')=>('S_TEMPLATE_EXISTS')
          EXPORTING
            i_name       = mv_name
            i_version    = 'A'
          RECEIVING
            r_flg_exists = rv_bool.
        IF rv_bool = abap_false.
          CALL METHOD ('CL_CMP_TEMPLATE')=>('S_TEMPLATE_EXISTS')
            EXPORTING
              i_name       = mv_name
              i_version    = 'I'
            RECEIVING
              r_flg_exists = rv_bool.
        ENDIF.
      CATCH cx_root.
        Lcx_abapgit_exception=>raise( 'CMPT not supported' ).
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

    DATA: lr_template TYPE REF TO data.
    FIELD-SYMBOLS: <lg_template> TYPE any,
                   <lg_header>   TYPE any,
                   <lg_field>    TYPE any.

    TRY.
        CREATE DATA lr_template TYPE ('IF_CMP_TEMPLATE_DB=>TYP_TEMPLATE').
        ASSIGN lr_template->* TO <lg_template>.

        CALL METHOD mo_cmp_db->('IF_CMP_TEMPLATE_DB~READ_TEMPLATE')
          EXPORTING
            i_name     = |{ ms_item-obj_name }|
            i_version  = 'A'
          RECEIVING
            r_template = <lg_template>.

        ASSIGN COMPONENT 'STR_HEADER' OF STRUCTURE <lg_template> TO <lg_header>.
        IF sy-subrc = 0.
          ASSIGN COMPONENT 'NAME' OF STRUCTURE <lg_header> TO <lg_field>.
          IF sy-subrc = 0.
            CLEAR <lg_field>.
          ENDIF.
          ASSIGN COMPONENT 'VERSION' OF STRUCTURE <lg_header> TO <lg_field>.
          IF sy-subrc = 0.
            CLEAR <lg_field>.
          ENDIF.
          ASSIGN COMPONENT 'CHANGED_ON' OF STRUCTURE <lg_header> TO <lg_field>.
          IF sy-subrc = 0.
            CLEAR <lg_field>.
          ENDIF.
          ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <lg_header> TO <lg_field>.
          IF sy-subrc = 0.
            CLEAR <lg_field>.
          ENDIF.
          ASSIGN COMPONENT 'CHANGED_TS' OF STRUCTURE <lg_header> TO <lg_field>.
          IF sy-subrc = 0.
            CLEAR <lg_field>.
          ENDIF.
        ENDIF.

        io_xml->add( iv_name = 'CMPT'
                     ig_data = <lg_template> ).

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( 'CMPT not supported' ).
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
endclass. "ZCL_ABAPGIT_OBJECT_CMPT implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ACID <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_acid=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_acid=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ACID implementation.
*"* method's implementations
*include methods.
  METHOD create_object.

    DATA: lv_name TYPE aab_id_name.


    lv_name = ms_item-obj_name.

    CREATE OBJECT ro_aab
      EXPORTING
        im_name          = lv_name
      EXCEPTIONS
        name_not_allowed = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
* looks like "changed by user" is not stored in the database
    rv_user = c_user_unknown.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lo_aab TYPE REF TO cl_aab_id.


    lo_aab = create_object( ).
    lo_aab->enqueue(
      EXCEPTIONS
        foreign_lock = 1
        system_error = 2
        cts_error    = 3
        OTHERS       = 4 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.
    lo_aab->delete(
      EXCEPTIONS
        prop_error       = 1
        propt_error      = 2
        act_error        = 3
        cts_error        = 4
        cts_devclass     = 5
        id_not_found     = 6
        no_authorization = 7
        id_still_used    = 8
        where_used_error = 9
        OTHERS           = 10 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.
    lo_aab->dequeue( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_description TYPE aab_id_descript,
          lo_aab         TYPE REF TO cl_aab_id.


    io_xml->read( EXPORTING iv_name = 'DESCRIPTION'
                  CHANGING  cg_data = lv_description ).

    lo_aab = create_object( ).

    lo_aab->enqueue(
      EXCEPTIONS
        foreign_lock = 1
        system_error = 2
        cts_error    = 3
        OTHERS       = 4 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    lo_aab->set_descript(
      EXPORTING
        im_descript      = lv_description
      EXCEPTIONS
        no_authorization = 1
        OTHERS           = 2 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    tadir_insert( iv_package ).

    lo_aab->save(
      EXCEPTIONS
        no_descript_specified = 1
        no_changes_found      = 2
        prop_error            = 3
        propt_error           = 4
        act_error             = 5
        cts_error             = 6
        sync_attributes_error = 7
        action_canceled       = 8
        OTHERS                = 9 ).
    IF sy-subrc >= 3.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    lo_aab->dequeue( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_state TYPE abap_bool,
          lo_aab   TYPE REF TO cl_aab_id.


    lo_aab = create_object( ).

    lo_aab->get_state( IMPORTING ex_state = lv_state ).
    rv_bool = boolc( lv_state = abap_true ).

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

    DATA: lo_aab         TYPE REF TO cl_aab_id,
          lv_description TYPE aab_id_descript.


    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lo_aab = create_object( ).

    lo_aab->get_descript(
      IMPORTING ex_descript = lv_description
      EXCEPTIONS no_description_found = 1 ).

    io_xml->add( iv_name = 'DESCRIPTION'
                 ig_data = lv_description ).

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
endclass. "ZCL_ABAPGIT_OBJECT_ACID implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_AMSD <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_amsd=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_amsd=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_AMSD implementation.
*"* method's implementations
*include methods.
  METHOD clear_field.

    FIELD-SYMBOLS: <lv_value> TYPE data.

    ASSIGN COMPONENT iv_fieldname OF STRUCTURE cs_logical_db_schema
           TO <lv_value>.
    ASSERT sy-subrc = 0.

    CLEAR: <lv_value>.

  ENDMETHOD.
  METHOD clear_fields.

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CREATED_AT'
      CHANGING
        cs_logical_db_schema = cs_logical_db_schema ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CREATED_BY'
      CHANGING
        cs_logical_db_schema = cs_logical_db_schema ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CHANGED_AT'
      CHANGING
        cs_logical_db_schema = cs_logical_db_schema ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CHANGED_BY'
      CHANGING
        cs_logical_db_schema = cs_logical_db_schema ).

  ENDMETHOD.
  METHOD constructor.

    super->constructor(
        is_item     = is_item
        iv_language = iv_language ).

    mv_logical_db_schema_key = ms_item-obj_name.

    TRY.
        CREATE DATA mr_logical_db_schema TYPE ('CL_AMDP_SCHEMA_OBJECT_DATA=>TY_OBJECT_DATA').
        CREATE OBJECT mi_persistence TYPE ('CL_AMDP_SCHEMA_OBJECT_PERSIST').

      CATCH cx_sy_create_error.
        Lcx_abapgit_exception=>raise( |AMSD not supported by your NW release| ).
    ENDTRY.

  ENDMETHOD.
  METHOD fill_metadata_from_db.

    DATA:
      li_wb_object_operator    TYPE REF TO object,
      lr_logical_db_schema_old TYPE REF TO data.

    FIELD-SYMBOLS:
      <ls_logical_db_schema_old> TYPE any,
      <lv_created_at>            TYPE xsddatetime_z,
      <lv_created_by>            TYPE syuname,
      <lv_created_at_old>        TYPE xsddatetime_z,
      <lv_created_by_old>        TYPE syuname.

    li_wb_object_operator = get_wb_object_operator( ).

    CREATE DATA lr_logical_db_schema_old TYPE ('CL_AMDP_SCHEMA_OBJECT_DATA=>TY_OBJECT_DATA').
    ASSIGN lr_logical_db_schema_old->* TO <ls_logical_db_schema_old>.
    ASSERT sy-subrc = 0.

    CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
      IMPORTING
        data = <ls_logical_db_schema_old>.

    ASSIGN COMPONENT 'METADATA-CREATED_BY' OF STRUCTURE cs_logical_db_schema
           TO <lv_created_by>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'METADATA-CREATED_AT' OF STRUCTURE cs_logical_db_schema
           TO <lv_created_at>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'METADATA-CREATED_BY' OF STRUCTURE <ls_logical_db_schema_old>
           TO <lv_created_by_old>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'METADATA-CREATED_AT' OF STRUCTURE <ls_logical_db_schema_old>
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

    ls_object_type-objtype_tr = 'AMSD'.
    ls_object_type-subtype_wb = 'TYP'.

    TRY.
        CALL METHOD ('CL_WB_OBJECT_OPERATOR')=>('CREATE_INSTANCE')
          EXPORTING
            object_type = ls_object_type
            object_key  = mv_logical_db_schema_key
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
      li_wb_object_operator TYPE REF TO object,
      lx_error              TYPE REF TO cx_root.

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
      li_object_data_model  TYPE REF TO if_wb_object_data_model,
      li_wb_object_operator TYPE REF TO object,
      lx_error              TYPE REF TO cx_root.

    FIELD-SYMBOLS:
      <ls_logical_db_schema> TYPE any.

    ASSIGN mr_logical_db_schema->* TO <ls_logical_db_schema>.
    ASSERT sy-subrc = 0.

    io_xml->read(
      EXPORTING
        iv_name = 'AMSD'
      CHANGING
        cg_data = <ls_logical_db_schema> ).

    li_wb_object_operator = get_wb_object_operator( ).

    TRY.
        CREATE OBJECT li_object_data_model TYPE ('CL_AMDP_SCHEMA_OBJECT_DATA').

        tadir_insert( iv_package ).

        IF Lif_abapgit_object~exists( ) = abap_true.

          " We need to populate created_at, created_by, because otherwise update  is not possible
          fill_metadata_from_db( CHANGING cs_logical_db_schema = <ls_logical_db_schema> ).
          li_object_data_model->set_data( <ls_logical_db_schema> ).

          CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
            EXPORTING
              io_object_data    = li_object_data_model
              transport_request = iv_transport.

        ELSE.

          li_object_data_model->set_data( <ls_logical_db_schema> ).

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
            p_object_key           = mv_logical_db_schema_key
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
      lx_error              TYPE REF TO cx_root,
      li_wb_object_operator TYPE REF TO object.

    FIELD-SYMBOLS:
      <ls_logical_db_schema> TYPE any.

    ASSIGN mr_logical_db_schema->* TO <ls_logical_db_schema>.
    ASSERT sy-subrc = 0.

    li_wb_object_operator = get_wb_object_operator( ).

    TRY.
        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
          EXPORTING
            version        = 'A'
          IMPORTING
            data           = <ls_logical_db_schema>
            eo_object_data = li_object_data_model.

        clear_fields( CHANGING cs_logical_db_schema = <ls_logical_db_schema> ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    io_xml->add(
        iv_name = 'AMSD'
        ig_data = <ls_logical_db_schema> ).

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
endclass. "ZCL_ABAPGIT_OBJECT_AMSD implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_VCLS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_vcls=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_vcls=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_VCLS implementation.
*"* method's implementations
*include methods.
  METHOD is_locked.

    DATA:
      ls_rstable_key TYPE rstable, " Lock argument for table RSTABLE
      lv_argument    TYPE eqegraarg.

    " Set Values for generic table lock
    ls_rstable_key-tabname = iv_tabname.
    ls_rstable_key-varkey  = iv_argument.

    " include all sub keys
    lv_argument = ls_rstable_key.
    lv_argument = lv_argument && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object         = 'E_TABLEE'
                                            iv_argument            = lv_argument ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    SELECT SINGLE author FROM vcldir INTO rv_user
      WHERE vclname = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.
* Do the same as in VIEWCLUSTER_SAVE_DEFINITION
    DATA: lv_vclname TYPE vcl_name.


    lv_vclname = ms_item-obj_name.

    DELETE FROM vcldir WHERE vclname = lv_vclname.        "#EC CI_SUBRC
    DELETE FROM vcldirt WHERE vclname = lv_vclname. "#EC CI_NOFIRST "#EC CI_SUBRC
    DELETE FROM vclstruc WHERE vclname = lv_vclname.      "#EC CI_SUBRC
    DELETE FROM vclstruct WHERE vclname = lv_vclname. "#EC CI_NOFIRST "#EC CI_SUBRC
    DELETE FROM vclstrudep WHERE vclname = lv_vclname.    "#EC CI_SUBRC
    DELETE FROM vclmf WHERE vclname = lv_vclname.         "#EC CI_SUBRC

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_vcldir_entry TYPE v_vcldir,
          lt_vclstruc     TYPE TABLE OF v_vclstruc,
          lt_vclstrudep   TYPE TABLE OF v_vclstdep,
          lt_vclmf        TYPE TABLE OF v_vclmf,
          lv_objectname   TYPE ob_object.


    io_xml->read( EXPORTING iv_name = 'VCLDIR'
                  CHANGING cg_data = ls_vcldir_entry ).
    io_xml->read( EXPORTING iv_name = 'VLCSTRUC_TAB'
                  CHANGING cg_data = lt_vclstruc ).
    io_xml->read( EXPORTING iv_name = 'VCLSTRUDEP_TAB'
                  CHANGING cg_data = lt_vclstrudep ).
    io_xml->read( EXPORTING iv_name = 'lt_vclstrudep'
                  CHANGING cg_data = lt_vclmf ).

    ls_vcldir_entry-author = sy-uname.
    ls_vcldir_entry-changedate = sy-datum.

    CALL FUNCTION 'VIEWCLUSTER_SAVE_DEFINITION'
      EXPORTING
        vcldir_entry   = ls_vcldir_entry
      TABLES
        vclstruc_tab   = lt_vclstruc
        vclstrudep_tab = lt_vclstrudep
        vclmf_tab      = lt_vclmf.

    corr_insert( iv_package ).

    lv_objectname = ls_vcldir_entry-vclname.

    CALL FUNCTION 'OBJ_GENERATE'
      EXPORTING
        iv_objectname         = lv_objectname
        iv_objecttype         = c_cluster_type
        iv_maint_mode         = c_mode_insert
        iv_devclass           = iv_package
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

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA lv_changedate TYPE vcldir-changedate.

    SELECT SINGLE changedate INTO lv_changedate FROM vcldir
      WHERE vclname = ms_item-obj_name.

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

    DATA lv_changedate TYPE vcldir-changedate.

    SELECT SINGLE changedate INTO lv_changedate FROM vcldir
      WHERE vclname = ms_item-obj_name.

* see logic in function module VIEWCLUSTER_GET_DEFINITION
    rv_active = boolc( lv_changedate IS NOT INITIAL ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    DATA:
      lv_argument       TYPE seqg3-garg,
      lv_argument_langu TYPE seqg3-garg.

    lv_argument       = ms_item-obj_name.
    lv_argument_langu = |@{ ms_item-obj_name }|.

    "Check all relevant maintein tabeles for view clusters
    IF is_locked( iv_tabname = 'VCLDIR'
                  iv_argument = lv_argument ) = abap_true
        OR is_locked( iv_tabname = 'VCLDIRT'
                      iv_argument = lv_argument_langu ) = abap_true
        OR is_locked( iv_tabname = 'VCLSTRUC'
                      iv_argument = lv_argument )       = abap_true
        OR is_locked( iv_tabname = 'VCLSTRUCT'
                      iv_argument = lv_argument_langu ) = abap_true
        OR is_locked( iv_tabname = 'VCLMF'
                      iv_argument = lv_argument )       = abap_true.

      rv_is_locked = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: ls_bcdata TYPE bdcdata,
          lt_bcdata TYPE STANDARD TABLE OF bdcdata.

    ls_bcdata-program  = 'SAPMSVIM'.
    ls_bcdata-dynpro   = '0050'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam     = 'VIMDYNFLDS-VIEWNAME'.
    ls_bcdata-fval     = ms_item-obj_name.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam     = 'VIMDYNFLDS-STRUCT_MNT'.
    ls_bcdata-fval     = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=CLUS'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-program  = 'SAPMSVIM'.
    ls_bcdata-dynpro   = '0052 '.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam     = 'VIMDYNFLDS-VCLNAME'.
    ls_bcdata-fval     = ms_item-obj_name.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=CLSH'.
    APPEND ls_bcdata TO lt_bcdata.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SE54'
      it_bdcdata = lt_bcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_vclname      TYPE vcl_name,
          ls_vcldir_entry TYPE v_vcldir,
          lt_vclstruc     TYPE TABLE OF v_vclstruc,
          lt_vclstrudep   TYPE TABLE OF v_vclstdep,
          lt_vclmf        TYPE TABLE OF v_vclmf.


    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lv_vclname = ms_item-obj_name.

    CALL FUNCTION 'VIEWCLUSTER_GET_DEFINITION'
      EXPORTING
        vclname                = lv_vclname
      IMPORTING
        vcldir_entry           = ls_vcldir_entry
      TABLES
        vclstruc_tab           = lt_vclstruc
        vclstrudep_tab         = lt_vclstrudep
        vclmf_tab              = lt_vclmf
      EXCEPTIONS
        viewcluster_not_found  = 1
        incomplete_viewcluster = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    SORT lt_vclstrudep BY vclname object objfield.

    CLEAR ls_vcldir_entry-author.
    CLEAR ls_vcldir_entry-changedate.

    io_xml->add( iv_name = 'VCLDIR'
                 ig_data = ls_vcldir_entry ).
    io_xml->add( iv_name = 'VLCSTRUC_TAB'
                 ig_data = lt_vclstruc ).
    io_xml->add( iv_name = 'VCLSTRUDEP_TAB'
                 ig_data = lt_vclstrudep ).
    io_xml->add( iv_name = 'VCLMF_TAB'
                 ig_data = lt_vclmf ).

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
endclass. "ZCL_ABAPGIT_OBJECT_VCLS implementation

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

*>>>>>>> ZCL_ABAPGIT_OBJECT_ENHO_BADI <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_enho_badi==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_enho_badi==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ENHO_BADI implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    ms_item = is_item.
  ENDMETHOD.
  METHOD Lif_abapgit_object_enho~deserialize.

    DATA: lv_spot_name TYPE enhspotname,
          lv_shorttext TYPE string,
          lv_enhname   TYPE enhname,
          lo_badi      TYPE REF TO cl_enh_tool_badi_impl,
          li_tool      TYPE REF TO if_enh_tool,
          lv_package   TYPE devclass,
          lt_impl      TYPE enh_badi_impl_data_it,
          lx_enh_root  TYPE REF TO cx_enh_root.

    FIELD-SYMBOLS: <ls_impl> LIKE LINE OF lt_impl.

    ii_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING cg_data  = lv_shorttext ).
    ii_xml->read( EXPORTING iv_name = 'SPOT_NAME'
                  CHANGING cg_data  = lv_spot_name ).
    ii_xml->read( EXPORTING iv_name = 'IMPL'
                  CHANGING cg_data  = lt_impl ).

    lv_enhname = ms_item-obj_name.
    lv_package = iv_package.
    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname     = lv_enhname
            enhtype     = cl_abstract_enh_tool_redef=>credefinition
            enhtooltype = cl_enh_tool_badi_impl=>tooltype
          IMPORTING
            enhancement = li_tool
          CHANGING
            devclass    = lv_package ).
        lo_badi ?= li_tool.

        lo_badi->set_spot_name( lv_spot_name ).
        lo_badi->if_enh_object_docu~set_shorttext( lv_shorttext ).
        LOOP AT lt_impl ASSIGNING <ls_impl>.
          lo_badi->add_implementation( <ls_impl> ).
        ENDLOOP.
        lo_badi->if_enh_object~save( run_dark = abap_true ).
        lo_badi->if_enh_object~unlock( ).
      CATCH cx_enh_root INTO lx_enh_root.
        TRY.
            lo_badi->if_enh_object~unlock( ).
          CATCH cx_sy_ref_is_initial cx_enh_mod_not_allowed ##NO_HANDLER.
        ENDTRY.
        Lcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object_enho~serialize.

    DATA: lo_badi_impl TYPE REF TO cl_enh_tool_badi_impl,
          lv_spot_name TYPE enhspotname,
          lv_shorttext TYPE string,
          lt_impl      TYPE enh_badi_impl_data_it.

    FIELD-SYMBOLS: <ls_impl>   LIKE LINE OF lt_impl,
                   <ls_values> LIKE LINE OF <ls_impl>-filter_values,
                   <ls_filter> LIKE LINE OF <ls_impl>-filters.


    lo_badi_impl ?= ii_enh_tool.

    lv_shorttext = lo_badi_impl->if_enh_object_docu~get_shorttext( ).
    lv_spot_name = lo_badi_impl->get_spot_name( ).
    lt_impl      = lo_badi_impl->get_implementations( ).

    LOOP AT lt_impl ASSIGNING <ls_impl>.
* make sure the XML serialization does not dump, field type = N
      LOOP AT <ls_impl>-filter_values ASSIGNING <ls_values>.
        IF <ls_values>-filter_numeric_value1 CA space.
          CLEAR <ls_values>-filter_numeric_value1.
        ENDIF.
      ENDLOOP.
      LOOP AT <ls_impl>-filters ASSIGNING <ls_filter>.
        IF <ls_filter>-filter_numeric_value1 CA space.
          CLEAR <ls_filter>-filter_numeric_value1.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    ii_xml->add( iv_name = 'TOOL'
                 ig_data = ii_enh_tool->get_tool( ) ).
    ii_xml->add( ig_data = lv_shorttext
                 iv_name = 'SHORTTEXT' ).
    ii_xml->add( iv_name = 'SPOT_NAME'
                 ig_data = lv_spot_name ).
    ii_xml->add( iv_name = 'IMPL'
                 ig_data = lt_impl ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENHO_BADI implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_AVAR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_avar=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_avar=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_AVAR implementation.
*"* method's implementations
*include methods.
  METHOD create_object.

    DATA: lv_name TYPE aab_var_name.

    lv_name = ms_item-obj_name.

    CREATE OBJECT ro_aab_var
      EXPORTING
        im_name          = lv_name
        im_local         = ''
      EXCEPTIONS
        name_not_allowed = 1
        user_not_valid   = 2
        no_authorization = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lo_aab TYPE REF TO cl_aab_variant.

    lo_aab = create_object( ).
    lo_aab->get_author( IMPORTING ex_author = rv_user ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lo_aab TYPE REF TO cl_aab_variant.

    lo_aab = create_object( ).
    lo_aab->enqueue( ).
    lo_aab->delete(
      EXCEPTIONS
        var_not_found    = 1
        prop_error       = 2
        propt_error      = 3
        var_id_error     = 4
        no_authorization = 5
        cts_error        = 6
        cts_devclass     = 7
        OTHERS           = 8 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error deleting AVAR { ms_item-obj_name }| ).
    ENDIF.
    lo_aab->dequeue( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_possible    TYPE abap_bool,
          lv_description TYPE aab_var_descript,
          ls_is          TYPE aab_var_obj_act,
          lt_ids         TYPE aab_var_obj_act_tab,
          lo_aab         TYPE REF TO cl_aab_variant.

    " AVAR can only be created in transportable packages
    lv_possible = Lcl_abapgit_factory=>get_sap_package( iv_package )->are_changes_recorded_in_tr_req( ).
    IF lv_possible = abap_false.
      Lcx_abapgit_exception=>raise( |Global activation variants require a transportable package| ).
    ENDIF.

    " Create AVAR with description and object (id) list
    io_xml->read( EXPORTING iv_name = 'DESCRIPTION'
                  CHANGING  cg_data = lv_description ).

    io_xml->read( EXPORTING iv_name = 'IDS'
                  CHANGING  cg_data = lt_ids ).

    lo_aab = create_object( ).
    lo_aab->enqueue( ).
    lo_aab->set_descript(
      EXPORTING
        im_descript      = lv_description
      EXCEPTIONS
        no_authorization = 1 ).
    IF sy-subrc <> 0.
      lo_aab->dequeue( ).
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    LOOP AT lt_ids INTO ls_is.
      lo_aab->set_id(
        EXPORTING
          im_name              = ls_is-name
          im_object            = ls_is-object
          im_actmode           = ls_is-actmode
        EXCEPTIONS
          no_authorization     = 1
          id_not_exists        = 2
          id_not_transportable = 3
          OTHERS               = 4 ).
      IF sy-subrc <> 0.
        lo_aab->dequeue( ).
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDLOOP.

    tadir_insert( iv_package ).

    lo_aab->save(
      EXCEPTIONS
        no_descript_specified = 1
        prop_error            = 2
        propt_error           = 3
        var_id_error          = 4
        no_changes_found      = 5
        cts_error             = 6 ).
    IF sy-subrc <> 0.
      lo_aab->dequeue( ).
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.
    lo_aab->dequeue( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_state TYPE abap_bool,
          lo_aab   TYPE REF TO cl_aab_variant.

    lo_aab = create_object( ).

    lo_aab->get_state( IMPORTING ex_state = lv_state ).
    rv_bool = boolc( lv_state = abap_true ).

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

    DATA: lo_aab         TYPE REF TO cl_aab_variant,
          lt_ids         TYPE aab_var_obj_act_tab,
          lv_description TYPE aab_var_descript.

    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lo_aab = create_object( ).

    lo_aab->get_descript(
      IMPORTING
        ex_descript = lv_description
      EXCEPTIONS
        no_descript_found = 1 ).
    IF sy-subrc = 0.
      io_xml->add( iv_name = 'DESCRIPTION'
                   ig_data = lv_description ).
    ENDIF.

    lo_aab->get_ids( IMPORTING ex_ids = lt_ids ).

    io_xml->add( iv_name = 'IDS'
                 ig_data = lt_ids ).

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
endclass. "ZCL_ABAPGIT_OBJECT_AVAR implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ENHO_CLIF <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_enho_clif==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_enho_clif==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ENHO_CLIF implementation.
*"* method's implementations
*include methods.
  METHOD deserialize.

    DATA: lt_tab_attributes TYPE enhclasstabattrib,
          lt_tab_types      TYPE enhtype_tab,
          lt_tab_methods    TYPE enhnewmeth_tab,
          ls_type_line      TYPE vseotype,
          ls_header         TYPE vseomethod,
          ls_param          TYPE vseomepara,
          ls_exc            TYPE vseoexcep,
          lt_tab_eventdata  TYPE enhevent_tab,
          ls_event_line     TYPE vseoevent,
          ls_event_param    TYPE vseoeparam.

    FIELD-SYMBOLS: <ls_type>        LIKE LINE OF lt_tab_types,
                   <ls_method>      LIKE LINE OF lt_tab_methods,
                   <ls_param>       LIKE LINE OF <ls_method>-meth_param,
                   <ls_event>       LIKE LINE OF lt_tab_eventdata,
                   <ls_exc>         LIKE LINE OF <ls_method>-meth_exc,
                   <ls_event_param> LIKE LINE OF <ls_event>-event_param.


    io_xml->read( EXPORTING iv_name = 'TAB_ATTRIBUTES'
                  CHANGING cg_data = lt_tab_attributes ).
    io_xml->read( EXPORTING iv_name = 'TAB_TYPES'
                  CHANGING cg_data = lt_tab_types ).
    io_xml->read( EXPORTING iv_name = 'TAB_METHODS'
                  CHANGING cg_data = lt_tab_methods ).
    io_xml->read( EXPORTING iv_name = 'TAB_EVENTDATA'
                  CHANGING cg_data = lt_tab_eventdata ).

    LOOP AT lt_tab_types ASSIGNING <ls_type>.
      MOVE-CORRESPONDING <ls_type> TO ls_type_line.
      TRY.
          io_clif->add_change_enha_type( type_line = ls_type_line ).
        CATCH cx_enh_mod_not_allowed
        cx_enh_is_not_enhanceable.
          " TODO
      ENDTRY.
    ENDLOOP.

    io_clif->set_enhattributes( lt_tab_attributes ).

* SAP standard SET_ENH_NEW_METHOS does not work

    LOOP AT lt_tab_methods ASSIGNING <ls_method>.

      MOVE-CORRESPONDING <ls_method>-meth_header TO ls_header.

      io_clif->add_change_new_enh_method(
        methkey       = <ls_method>-methkey
        method_header = ls_header ).

* parameters
      LOOP AT <ls_method>-meth_param ASSIGNING <ls_param>.
        MOVE-CORRESPONDING <ls_param> TO ls_param.
        io_clif->add_change_enh_methparam(
          methname   = <ls_method>-methkey-cmpname
          param_line = ls_param ).
      ENDLOOP.

* exceptions
      LOOP AT <ls_method>-meth_exc ASSIGNING <ls_exc>.
        MOVE-CORRESPONDING <ls_exc> TO ls_exc.
        io_clif->add_change_enh_methexc(
          methname    = <ls_method>-methkey-cmpname
          except_line = ls_exc ).
      ENDLOOP.

    ENDLOOP.

    " events are renumbered based on
    LOOP AT lt_tab_eventdata ASSIGNING <ls_event>.

      MOVE-CORRESPONDING <ls_event>-event_header TO ls_event_line.

      io_clif->add_change_enha_event(
        event_key  = <ls_event>-eventkey
        event_line = ls_event_line ).

* parameters
      LOOP AT <ls_event>-event_param ASSIGNING <ls_event_param>.
        MOVE-CORRESPONDING <ls_event_param> TO ls_event_param.
        io_clif->add_change_enh_eventparam(
          eventname   = <ls_event>-eventkey-cmpname
          event_param = ls_event_param ).
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.
  METHOD serialize.

    DATA: lt_tab_attributes TYPE enhclasstabattrib,
          lt_tab_types      TYPE enhtype_tab,
          lt_tab_methods    TYPE enhnewmeth_tab,
          lt_tab_eventdata  TYPE enhevent_tab,
          lv_editorder      TYPE i.

    FIELD-SYMBOLS: <ls_attr>        LIKE LINE OF lt_tab_attributes,
                   <ls_type>        LIKE LINE OF lt_tab_types,
                   <ls_meth>        LIKE LINE OF lt_tab_methods,
                   <ls_param>       LIKE LINE OF <ls_meth>-meth_param,
                   <ls_exc>         LIKE LINE OF <ls_meth>-meth_exc,
                   <ls_event>       LIKE LINE OF lt_tab_eventdata,
                   <ls_event_param> LIKE LINE OF <ls_event>-event_param.


    io_clif->get_enhattributes( IMPORTING tab_attributes = lt_tab_attributes ).

    io_clif->get_enhatypes( IMPORTING tab_types = lt_tab_types ).

    io_clif->get_enh_new_methodes( IMPORTING tab_methodes = lt_tab_methods ).

    io_clif->get_enhevents( IMPORTING tab_eventdata = lt_tab_eventdata ).

    LOOP AT lt_tab_attributes ASSIGNING <ls_attr>.
      CLEAR: <ls_attr>-author,
             <ls_attr>-createdon,
             <ls_attr>-changedby,
             <ls_attr>-changedon,
             <ls_attr>-descript_id.
    ENDLOOP.

    LOOP AT lt_tab_types ASSIGNING <ls_type>.
      CLEAR: <ls_type>-author,
             <ls_type>-createdon,
             <ls_type>-changedby,
             <ls_type>-changedon,
             <ls_type>-descript_id.
    ENDLOOP.

    lv_editorder = 0.
    SORT lt_tab_methods BY meth_header-editorder.
    LOOP AT lt_tab_methods ASSIGNING <ls_meth>.
      CLEAR: <ls_meth>-meth_header-author,
             <ls_meth>-meth_header-createdon,
             <ls_meth>-meth_header-changedby,
             <ls_meth>-meth_header-changedon,
             <ls_meth>-meth_header-descript_id.
      lv_editorder = lv_editorder + 1.
      <ls_meth>-meth_header-editorder = lv_editorder.
      LOOP AT <ls_meth>-meth_param ASSIGNING <ls_param>.
        CLEAR: <ls_param>-author,
               <ls_param>-createdon,
               <ls_param>-changedby,
               <ls_param>-changedon,
               <ls_param>-descript_id.
      ENDLOOP.
      LOOP AT <ls_meth>-meth_exc ASSIGNING <ls_exc>.
        CLEAR: <ls_exc>-author,
               <ls_exc>-createdon,
               <ls_exc>-changedby,
               <ls_exc>-changedon,
               <ls_exc>-descript_id.
      ENDLOOP.
    ENDLOOP.

    LOOP AT lt_tab_eventdata ASSIGNING <ls_event>.
      CLEAR: <ls_event>-event_header-author,
             <ls_event>-event_header-createdon,
             <ls_event>-event_header-changedby,
             <ls_event>-event_header-changedon,
             <ls_event>-event_header-descript_id.
      LOOP AT <ls_event>-event_param ASSIGNING <ls_event_param>.
        CLEAR: <ls_event_param>-author,
               <ls_event_param>-createdon,
               <ls_event_param>-changedby,
               <ls_event_param>-changedon,
               <ls_event_param>-descript_id.
      ENDLOOP.
    ENDLOOP.

    io_xml->add( iv_name = 'TAB_ATTRIBUTES'
                 ig_data = lt_tab_attributes ).
    io_xml->add( iv_name = 'TAB_TYPES'
                 ig_data = lt_tab_types ).
    io_xml->add( iv_name = 'TAB_METHODS'
                 ig_data = lt_tab_methods ).
    io_xml->add( iv_name = 'TAB_EVENTDATA'
                 ig_data = lt_tab_eventdata ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENHO_CLIF implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ENHO_FUGR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_enho_fugr==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_enho_fugr==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ENHO_FUGR implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    ms_item = is_item.
    mo_files = io_files.
  ENDMETHOD.
  METHOD Lif_abapgit_object_enho~deserialize.

    DATA: lo_fugrdata  TYPE REF TO cl_enh_tool_fugr,
          ls_enha_data TYPE enhfugrdata,
          li_tool      TYPE REF TO if_enh_tool,
          lv_tool      TYPE enhtooltype,
          lv_package   TYPE devclass,
          lx_enh_root  TYPE REF TO cx_enh_root.

    FIELD-SYMBOLS: <ls_fuba> TYPE enhfugrfuncdata.

    ii_xml->read(
      EXPORTING
        iv_name = 'TOOL'
      CHANGING
        cg_data = lv_tool ).

    ii_xml->read(
      EXPORTING
        iv_name = 'FUGRDATA'
      CHANGING
        cg_data = ls_enha_data ).

    lv_package = iv_package.

    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname     = |{ ms_item-obj_name }|
            enhtype     = ''
            enhtooltype = lv_tool
          IMPORTING
            enhancement = li_tool
          CHANGING
            devclass    = lv_package ).

        lo_fugrdata ?= li_tool.

        lo_fugrdata->set_fugr( ls_enha_data-fugr ).

        LOOP AT ls_enha_data-enh_fubas ASSIGNING <ls_fuba>.

          lo_fugrdata->set_func_data( func_name     = <ls_fuba>-fuba
                                      func_enhadata = <ls_fuba> ).

        ENDLOOP.

        lo_fugrdata->if_enh_object~save( run_dark = abap_true ).
        lo_fugrdata->if_enh_object~unlock( ).
      CATCH cx_enh_root INTO lx_enh_root.
        TRY.
            lo_fugrdata->if_enh_object~unlock( ).
          CATCH cx_sy_ref_is_initial cx_enh_mod_not_allowed ##NO_HANDLER.
        ENDTRY.
        Lcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object_enho~serialize.

    DATA: lo_fugrdata  TYPE REF TO cl_enh_tool_fugr,
          lv_fugr_name TYPE rs38l-area,
          ls_enha_data TYPE enhfugrdata.

    FIELD-SYMBOLS: <ls_docuobj> TYPE enhfugrparamdocu.


    lo_fugrdata ?= ii_enh_tool.

    lo_fugrdata->get_fugr( IMPORTING fugr_name = lv_fugr_name ).

    TRY.
        lo_fugrdata->get_all_data_for_fugr(
          EXPORTING
            fugr_name = lv_fugr_name
          IMPORTING
            enha_data = ls_enha_data ).

        LOOP AT ls_enha_data-docuobjs ASSIGNING <ls_docuobj>.
          CLEAR: <ls_docuobj>-shorttext,
                 <ls_docuobj>-longtext.
        ENDLOOP.

      CATCH cx_enh_not_found.
        Lcx_abapgit_exception=>raise( |error deserializing ENHO fugrdata { ms_item-obj_name }| ).
    ENDTRY.

    ii_xml->add( iv_name = 'TOOL'
                 ig_data = lo_fugrdata->if_enh_tool~get_tool( ) ).

    ii_xml->add( iv_name = 'FUGRDATA'
                 ig_data = ls_enha_data ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENHO_FUGR implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ENHO_HOOK <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_enho_hook==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_enho_hook==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ENHO_HOOK implementation.
*"* method's implementations
*include methods.
  METHOD add_sources.

    DATA lv_source TYPE string.
    DATA ls_file LIKE LINE OF ct_files.

    FIELD-SYMBOLS <ls_enhancement> LIKE LINE OF ct_enhancements.

    LOOP AT ct_enhancements ASSIGNING <ls_enhancement>.
      " Use hash as filename since full_name is very long
      CLEAR ls_file.
      ls_file-name = <ls_enhancement>-full_name.
      ls_file-file = substring(
        val = Lcl_abapgit_hash=>sha1_string( <ls_enhancement>-full_name )
        len = 8 ).
      INSERT ls_file INTO TABLE ct_files.

      " Add full name as comment and put code between enhancement statements
      lv_source = c_enhancement.
      REPLACE '*' IN lv_source WITH ms_item-obj_name.
      INSERT lv_source INTO <ls_enhancement>-source INDEX 1.

      lv_source = |"Name: { <ls_enhancement>-full_name }|.
      INSERT lv_source INTO <ls_enhancement>-source INDEX 1.

      APPEND c_endenhancement TO <ls_enhancement>-source.

      mo_files->add_abap( iv_extra = ls_file-file
                          it_abap  = <ls_enhancement>-source ).

      CLEAR <ls_enhancement>-source.
    ENDLOOP.

  ENDMETHOD.
  METHOD constructor.
    ms_item = is_item.
    mo_files = io_files.
  ENDMETHOD.
  METHOD hook_impl_deserialize.

    FIELD-SYMBOLS: <ls_impl>   LIKE LINE OF ct_impl,
                   <lv_line>   TYPE string,
                   <lv_space>  TYPE i,
                   <ls_spaces> LIKE LINE OF it_spaces.


    LOOP AT ct_impl ASSIGNING <ls_impl>.
      READ TABLE it_spaces ASSIGNING <ls_spaces> WITH KEY full_name = <ls_impl>-full_name.
      IF sy-subrc = 0.
        LOOP AT <ls_impl>-source ASSIGNING <lv_line>.
          READ TABLE <ls_spaces>-spaces ASSIGNING <lv_space> INDEX sy-tabix.
          IF sy-subrc = 0 AND <lv_space> > 0.
            DO <lv_space> TIMES.
              CONCATENATE space <lv_line> INTO <lv_line> RESPECTING BLANKS.
            ENDDO.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD read_sources.

    DATA lv_source TYPE string.
    DATA ls_file LIKE LINE OF ct_files.
    DATA lv_from TYPE i.
    DATA lv_to TYPE i.

    FIELD-SYMBOLS <ls_enhancement> LIKE LINE OF ct_enhancements.

    LOOP AT ct_enhancements ASSIGNING <ls_enhancement>.
      READ TABLE ct_files INTO ls_file WITH TABLE KEY name = <ls_enhancement>-full_name.
      IF sy-subrc = 0.
        <ls_enhancement>-source = mo_files->read_abap( iv_extra = ls_file-file ).
        " Get code between enhancement statements
        LOOP AT <ls_enhancement>-source INTO lv_source.
          IF lv_source CP c_enhancement.
            lv_from = sy-tabix.
          ENDIF.
          IF lv_source CP c_endenhancement.
            lv_to = sy-tabix.
          ENDIF.
        ENDLOOP.
        DELETE <ls_enhancement>-source FROM lv_to.
        DELETE <ls_enhancement>-source TO lv_from.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_object_enho~deserialize.

    DATA: lv_shorttext       TYPE string,
          lo_hook_impl       TYPE REF TO cl_enh_tool_hook_impl,
          li_tool            TYPE REF TO if_enh_tool,
          lv_enhname         TYPE enhname,
          lv_package         TYPE devclass,
          ls_original_object TYPE enh_hook_admin,
          lt_spaces          TYPE ty_spaces_tt,
          lt_files           TYPE ty_files,
          lt_enhancements    TYPE enh_hook_impl_it,
          lx_enh_root        TYPE REF TO cx_enh_root.

    FIELD-SYMBOLS: <ls_enhancement> LIKE LINE OF lt_enhancements.


    ii_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING cg_data  = lv_shorttext ).
    ii_xml->read( EXPORTING iv_name = 'ORIGINAL_OBJECT'
                  CHANGING cg_data  = ls_original_object ).
    ii_xml->read( EXPORTING iv_name = 'ENHANCEMENTS'
                  CHANGING cg_data  = lt_enhancements ).
    ii_xml->read( EXPORTING iv_name = 'FILES'
                  CHANGING cg_data  = lt_files ).
    ii_xml->read( EXPORTING iv_name = 'SPACES'
                  CHANGING cg_data  = lt_spaces ).

    " todo: kept for compatibility, remove after grace period #3680
    hook_impl_deserialize( EXPORTING it_spaces = lt_spaces
                           CHANGING ct_impl    = lt_enhancements ).

    read_sources( CHANGING ct_enhancements = lt_enhancements
                           ct_files        = lt_files ).

    lv_enhname = ms_item-obj_name.
    lv_package = iv_package.
    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname     = lv_enhname
            enhtype     = cl_abstract_enh_tool_redef=>credefinition
            enhtooltype = cl_enh_tool_hook_impl=>tooltype
          IMPORTING
            enhancement = li_tool
          CHANGING
            devclass    = lv_package ).
        lo_hook_impl ?= li_tool.

        lo_hook_impl->if_enh_object_docu~set_shorttext( lv_shorttext ).
        lo_hook_impl->set_original_object(
            pgmid       = ls_original_object-pgmid
            obj_name    = ls_original_object-org_obj_name
            obj_type    = ls_original_object-org_obj_type
            program     = ls_original_object-programname
            main_type   = ls_original_object-org_main_type
            main_name   = ls_original_object-org_main_name ).
        lo_hook_impl->set_include_bound( ls_original_object-include_bound ).

        LOOP AT lt_enhancements ASSIGNING <ls_enhancement>.
          lo_hook_impl->add_hook_impl(
              overwrite        = <ls_enhancement>-overwrite
              method           = <ls_enhancement>-method
              enhmode          = <ls_enhancement>-enhmode
              full_name        = <ls_enhancement>-full_name
              source           = <ls_enhancement>-source
              spot             = <ls_enhancement>-spotname
              parent_full_name = <ls_enhancement>-parent_full_name ).
        ENDLOOP.
        lo_hook_impl->if_enh_object~save( run_dark = abap_true ).
        lo_hook_impl->if_enh_object~unlock( ).
      CATCH cx_enh_root INTO lx_enh_root.
        TRY.
            lo_hook_impl->if_enh_object~unlock( ).
          CATCH cx_sy_ref_is_initial cx_enh_mod_not_allowed ##NO_HANDLER.
        ENDTRY.
        Lcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object_enho~serialize.

    DATA: lv_shorttext       TYPE string,
          lo_hook_impl       TYPE REF TO cl_enh_tool_hook_impl,
          ls_original_object TYPE enh_hook_admin,
          lt_spaces          TYPE ty_spaces_tt,
          lt_files           TYPE ty_files,
          lt_enhancements    TYPE enh_hook_impl_it.

    FIELD-SYMBOLS: <ls_enhancement> LIKE LINE OF lt_enhancements.

    lo_hook_impl ?= ii_enh_tool.

    lv_shorttext = lo_hook_impl->if_enh_object_docu~get_shorttext( ).
    lo_hook_impl->get_original_object(
      IMPORTING
        pgmid     = ls_original_object-pgmid
        obj_name  = ls_original_object-org_obj_name
        obj_type  = ls_original_object-org_obj_type
        main_type = ls_original_object-org_main_type
        main_name = ls_original_object-org_main_name
        program   = ls_original_object-programname ).
    ls_original_object-include_bound = lo_hook_impl->get_include_bound( ).
    lt_enhancements = lo_hook_impl->get_hook_impls( ).

    LOOP AT lt_enhancements ASSIGNING <ls_enhancement>.
      CLEAR: <ls_enhancement>-extid,
             <ls_enhancement>-id.
    ENDLOOP.

    add_sources( CHANGING ct_enhancements = lt_enhancements
                          ct_files        = lt_files ).

    ii_xml->add( iv_name = 'TOOL'
                 ig_data = ii_enh_tool->get_tool( ) ).
    ii_xml->add( iv_name = 'SHORTTEXT'
                 ig_data = lv_shorttext ).
    ii_xml->add( iv_name = 'ORIGINAL_OBJECT'
                 ig_data = ls_original_object ).
    ii_xml->add( iv_name = 'ENHANCEMENTS'
                 ig_data = lt_enhancements ).
    ii_xml->add( iv_name = 'FILES'
                 ig_data = lt_files ).
    ii_xml->add( iv_name = 'SPACES'
                 ig_data = lt_spaces ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENHO_HOOK implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ENHO_INTF <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_enho_intf==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_enho_intf==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ENHO_INTF implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    ms_item  = is_item.
    mo_files = io_files.
  ENDMETHOD.
  METHOD Lif_abapgit_object_enho~deserialize.

    DATA: lo_enh_intf  TYPE REF TO cl_enh_tool_intf,
          li_tool      TYPE REF TO if_enh_tool,
          lv_shorttext TYPE string,
          lv_class     TYPE seoclsname,
          lv_enhname   TYPE enhname,
          lv_package   TYPE devclass,
          lx_enh_root  TYPE REF TO cx_enh_root.

    ii_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING cg_data  = lv_shorttext ).
    ii_xml->read( EXPORTING iv_name = 'CLASS'
                  CHANGING cg_data  = lv_class ).

    lv_enhname = ms_item-obj_name.
    lv_package = iv_package.
    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname     = lv_enhname
            enhtype     = ''
            enhtooltype = cl_enh_tool_intf=>tooltype
          IMPORTING
            enhancement = li_tool
          CHANGING
            devclass    = lv_package ).
        lo_enh_intf ?= li_tool.

        lo_enh_intf->if_enh_object_docu~set_shorttext( lv_shorttext ).
        lo_enh_intf->set_class( lv_class ).

        Lcl_abapgit_object_enho_clif=>deserialize(
          io_xml  = ii_xml
          io_clif = lo_enh_intf ).

        lo_enh_intf->if_enh_object~save( run_dark = abap_true ).
        lo_enh_intf->if_enh_object~unlock( ).
      CATCH cx_enh_root INTO lx_enh_root.
        TRY.
            lo_enh_intf->if_enh_object~unlock( ).
          CATCH cx_sy_ref_is_initial cx_enh_mod_not_allowed ##NO_HANDLER.
        ENDTRY.
        Lcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object_enho~serialize.

    DATA: lo_enh_intf  TYPE REF TO cl_enh_tool_intf,
          lv_class     TYPE seoclsname,
          lv_shorttext TYPE string.


    lo_enh_intf ?= ii_enh_tool.

    lv_shorttext = lo_enh_intf->if_enh_object_docu~get_shorttext( ).
    lo_enh_intf->get_class( IMPORTING class_name = lv_class ).

    ii_xml->add( iv_name = 'TOOL'
                 ig_data = ii_enh_tool->get_tool( ) ).
    ii_xml->add( ig_data = lv_shorttext
                 iv_name = 'SHORTTEXT' ).
    ii_xml->add( iv_name = 'CLASS'
                 ig_data = lv_class ).

    Lcl_abapgit_object_enho_clif=>serialize(
      io_xml  = ii_xml
      io_clif = lo_enh_intf ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENHO_INTF implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ENHO_WDYC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_enho_wdyc==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_enho_wdyc==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ENHO_WDYC implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    ms_item = is_item.
  ENDMETHOD.
  METHOD Lif_abapgit_object_enho~deserialize.

    DATA: lv_enhname TYPE enhname,
          lo_wdyconf TYPE REF TO cl_wdr_cfg_enhancement,
          li_tool    TYPE REF TO if_enh_tool,
          ls_obj     TYPE wdy_config_key,
          lv_xml     TYPE string,
          lt_data    TYPE wdy_cfg_expl_data_tab,
          lv_package TYPE devclass.

    ii_xml->read( EXPORTING iv_name = 'ORIGINAL_OBJECT'
                  CHANGING  cg_data = ls_obj ).

    ii_xml->read( EXPORTING iv_name = 'ENHANCEMENT_DATA'
                  CHANGING  cg_data = lv_xml ).

    lv_enhname = ms_item-obj_name.
    lv_package = iv_package.
    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname     = lv_enhname
            enhtype     = ''
            enhtooltype = cl_wdr_cfg_enhancement=>tooltype
          IMPORTING
            enhancement = li_tool
          CHANGING
            devclass    = lv_package ).

        lo_wdyconf ?= li_tool.

        CALL METHOD cl_wdr_cfg_persistence_utils=>('COMP_XML_TO_TABLES')
          EXPORTING
            xml_content   = lv_xml
          IMPORTING
            expl_data_tab = lt_data.

* only works on new ABAP versions, parameters differ between versions
        CALL METHOD lo_wdyconf->('SET_ENHANCEMENT_DATA')
          EXPORTING
            p_enh_data = lt_data.

        lo_wdyconf->if_enh_object~save( run_dark = abap_true ).
        lo_wdyconf->if_enh_object~unlock( ).
      CATCH cx_enh_root cx_static_check.
        TRY.
            lo_wdyconf->if_enh_object~unlock( ).
          CATCH cx_sy_ref_is_initial cx_enh_mod_not_allowed ##NO_HANDLER.
        ENDTRY.
        Lcx_abapgit_exception=>raise( 'error deserializing ENHO wdyconf' ).
    ENDTRY.
  ENDMETHOD.
  METHOD Lif_abapgit_object_enho~serialize.

    DATA: lo_wdyconf  TYPE REF TO cl_wdr_cfg_enhancement,
          lt_data     TYPE wdy_cfg_expl_data_tab,
          ls_outline  TYPE wdy_cfg_outline_data,
          ls_obj      TYPE wdy_config_key,
          li_document TYPE REF TO if_ixml_document,
          li_element  TYPE REF TO if_ixml_element.


    lo_wdyconf ?= ii_enh_tool.

    ls_obj = lo_wdyconf->get_original_object( ).
    ii_xml->add( iv_name = 'TOOL'
                 ig_data = ii_enh_tool->get_tool( ) ).
    ii_xml->add( iv_name = 'ORIGINAL_OBJECT'
                 ig_data = ls_obj ).

* only works on new ABAP versions, parameters differ between versions
    CALL METHOD lo_wdyconf->('GET_ENHANCEMENT_DATA')
      EXPORTING
        p_scope    = 1
      IMPORTING
        p_enh_data = lt_data.

    CALL METHOD cl_wdr_cfg_persistence_utils=>('COMP_TABLES_TO_XML')
      EXPORTING
        outline_data  = ls_outline
        expl_data_tab = lt_data
      IMPORTING
        element       = li_element
      CHANGING
        document      = li_document.

    ii_xml->add_xml( iv_name = 'ENHANCEMENT_DATA'
                     ii_xml = li_element ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENHO_WDYC implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ENHO_WDYN <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_enho_wdyn==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_enho_wdyn==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ENHO_WDYN implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    ms_item = is_item.
  ENDMETHOD.
  METHOD Lif_abapgit_object_enho~deserialize.

    DATA: ls_enh_data  TYPE enhwdyn,
          li_tool      TYPE REF TO if_enh_tool,
          lo_wdyn      TYPE REF TO cl_enh_tool_wdy,
          lv_tool_type TYPE enhtooltype,
          lv_package   TYPE devclass.

    FIELD-SYMBOLS: <ls_controller_data> TYPE enhwdyc,
                   <ls_view_data>       TYPE enhwdyv.


    ii_xml->read(
      EXPORTING
        iv_name = 'TOOL'
      CHANGING
        cg_data = lv_tool_type ).

    ii_xml->read(
      EXPORTING
        iv_name = 'COMPONENT_DATA'
      CHANGING
        cg_data = ls_enh_data ).

    lv_package = iv_package.

    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname     = |{ ms_item-obj_name }|
            enhtype     = ''
            enhtooltype = lv_tool_type
          IMPORTING
            enhancement = li_tool
          CHANGING
            devclass    = lv_package ).

        lo_wdyn ?= li_tool.

        lo_wdyn->initialize( ls_enh_data-component_name ).

        lo_wdyn->set_component_data( ls_enh_data-component_data ).

        LOOP AT ls_enh_data-controller_data ASSIGNING <ls_controller_data>.

          lo_wdyn->set_controller_data( p_controller_name = <ls_controller_data>-controller_name
                                        p_enh_data        = <ls_controller_data> ).

        ENDLOOP.

        LOOP AT ls_enh_data-view_data ASSIGNING <ls_view_data>.

          lo_wdyn->set_view_data( p_view_name = <ls_view_data>-view_name
                                  p_enh_data  = <ls_view_data> ).

        ENDLOOP.

        lo_wdyn->if_enh_object~save( run_dark = abap_true ).
        lo_wdyn->if_enh_object~unlock( ).

      CATCH cx_root.
        TRY.
            lo_wdyn->if_enh_object~unlock( ).
          CATCH cx_sy_ref_is_initial cx_enh_mod_not_allowed ##NO_HANDLER.
        ENDTRY.
        Lcx_abapgit_exception=>raise( |error deserializing ENHO wdyn { ms_item-obj_name }| ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object_enho~serialize.

    DATA: lo_wdyn           TYPE REF TO cl_enh_tool_wdy,
          lv_component_name TYPE wdy_component_name,
          ls_enh_data       TYPE enhwdyn.


    lo_wdyn ?= ii_enh_tool.
    lv_component_name = lo_wdyn->get_component_name( ).

    TRY.
        lo_wdyn->get_all_data_for_comp(
          EXPORTING
            p_component_name = lv_component_name
          IMPORTING
            p_enh_data       = ls_enh_data ).

        ii_xml->add( iv_name = 'TOOL'
                     ig_data = ii_enh_tool->get_tool( ) ).

        ii_xml->add( iv_name = 'COMPONENT_DATA'
                     ig_data = ls_enh_data ).

      CATCH cx_enh_not_found.
        Lcx_abapgit_exception=>raise( |error serializing ENHO wdyn { ms_item-obj_name }| ).
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENHO_WDYN implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_AVAS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_avas=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_avas=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_AVAS implementation.
*"* method's implementations
*include methods.
  METHOD insert_assignments.

    DATA: lt_assignment TYPE STANDARD TABLE OF cls_assignment,
          ls_assignment LIKE LINE OF lt_assignment,
          ls_value      LIKE LINE OF is_avas-values.


    LOOP AT is_avas-values INTO ls_value.
      CLEAR ls_assignment.
      ls_assignment-guid        = is_avas-header-guid.
      ls_assignment-value       = ls_value-value.
      ls_assignment-trobjtype   = is_avas-header-object-trobjtype.
      ls_assignment-sobj_name   = is_avas-header-object-sobj_name.
      ls_assignment-object_type = is_avas-header-object-object_type.
      ls_assignment-sub_key     = is_avas-header-object-sub_key.
      ls_assignment-attribute   = is_avas-header-attribute.
      ls_assignment-set_by      = sy-uname.
      ls_assignment-changed_on  = sy-datum.
      ls_assignment-remark      = ls_value-remark.
      APPEND ls_assignment TO lt_assignment.
    ENDLOOP.

    DELETE FROM cls_assignment WHERE guid = is_avas-header-guid.

    INSERT cls_assignment FROM TABLE lt_assignment.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error inserting into CLS_ASSIGNMENT| ).
    ENDIF.

  ENDMETHOD.
  METHOD instantiate.

    DATA: lv_id  TYPE guid_32,
          lx_err TYPE REF TO cx_root.

    lv_id = ms_item-obj_name.

    TRY.
        CREATE OBJECT ro_avas
          EXPORTING
            im_assignment_id = lv_id.
      CATCH cx_pak_wb_object_locked INTO lx_err.
        Lcx_abapgit_exception=>raise( |AVAS { lv_id }: locked: { lx_err->get_longtext( ) }| ).
      CATCH cx_pak_not_authorized INTO lx_err.
        Lcx_abapgit_exception=>raise( |AVAS { lv_id }: not authorized: { lx_err->get_longtext( ) }| ).
      CATCH cx_pak_invalid_state INTO lx_err.
        Lcx_abapgit_exception=>raise( |AVAS { lv_id }: invalid state: { lx_err->get_longtext( ) }| ).
      CATCH cx_pak_invalid_data INTO lx_err.
        Lcx_abapgit_exception=>raise( |AVAS { lv_id }: invalid data: { lx_err->get_longtext( ) }| ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lo_avas TYPE REF TO cl_cls_attr_value_assignment.


    lo_avas = instantiate( ).

    lo_avas->if_pak_wb_object~get_last_changed( IMPORTING ex_changed_by = rv_user ).

    IF rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lo_avas TYPE REF TO cl_cls_attr_value_assignment.


    lo_avas = instantiate( ).

    TRY.
        lo_avas->if_cls_attr_value_assignment~lock_and_refresh( im_allow_popups = abap_false ).
      CATCH cx_pak_invalid_state
          cx_pak_invalid_data
          cx_pak_not_authorized
          cx_pak_wb_object_locked.
        Lcx_abapgit_exception=>raise( |AVAS error| ).
    ENDTRY.

    lo_avas->if_pak_wb_object~delete( ).

    lo_avas->if_pak_wb_object~save( ).

    lo_avas->if_pak_wb_object_internal~unlock( ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_avas TYPE ty_avas.


    io_xml->read( EXPORTING iv_name = 'AVAS'
                  CHANGING cg_data = ls_avas ).

* The AVAS API cannot be used in this case, as it will always create a new GUID

    ASSERT NOT ls_avas-header-guid IS INITIAL.

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

    insert_assignments( ls_avas ).
* todo, how does links work?

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_guid TYPE cls_assignment-guid.

    SELECT SINGLE guid FROM cls_assignment INTO lv_guid
      WHERE guid = ms_item-obj_name.
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
      iv_lock_object = 'CLS_ENQUEUE_STRU'
      iv_argument    = |{ ms_item-obj_name }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lo_avas TYPE REF TO cl_cls_attr_value_assignment,
          ls_avas TYPE ty_avas.

    FIELD-SYMBOLS: <ls_value> LIKE LINE OF ls_avas-values,
                   <ls_link>  LIKE LINE OF ls_avas-links.


    lo_avas = instantiate( ).

    ls_avas-header-guid      = lo_avas->if_cls_attr_value_assignment~get_guid( ).
    ls_avas-header-attribute = lo_avas->if_cls_attr_value_assignment~get_attribute( ).
    ls_avas-header-object    = lo_avas->if_cls_attr_value_assignment~get_object( ).

    lo_avas->if_cls_attr_value_assignment~get_values( IMPORTING ex_values = ls_avas-values ).

    lo_avas->if_cls_attr_value_assignment~get_links( IMPORTING ex_links = ls_avas-links ).

    LOOP AT ls_avas-values ASSIGNING <ls_value>.
      CLEAR: <ls_value>-set_by, <ls_value>-changed_on.
    ENDLOOP.

    LOOP AT ls_avas-links ASSIGNING <ls_link>.
      CLEAR: <ls_link>-set_by, <ls_link>-changed_on.
    ENDLOOP.

    io_xml->add(
      iv_name = 'AVAS'
      ig_data = ls_avas ).

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
endclass. "ZCL_ABAPGIT_OBJECT_AVAS implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ENHS_BADI_D <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_enhs_badi_dccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_enhs_badi_dccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ENHS_BADI_D implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object_enhs~deserialize.

    DATA: lv_parent          TYPE enhspotcompositename,
          lt_enh_badi        TYPE enh_badi_data_it,
          lo_badidef_tool    TYPE REF TO cl_enh_tool_badi_def,
          lv_enh_shorttext   TYPE string,
          li_enh_object      TYPE REF TO if_enh_object,
          li_enh_object_docu TYPE REF TO if_enh_object_docu,
          lx_enh_root        TYPE REF TO cx_enh_root.

    FIELD-SYMBOLS: <ls_enh_badi> LIKE LINE OF lt_enh_badi.

    ii_xml->read( EXPORTING iv_name = 'PARENT_COMP'
                  CHANGING  cg_data = lv_parent ).

    ii_xml->read( EXPORTING iv_name = 'BADI_DATA'
                  CHANGING  cg_data = lt_enh_badi ).

    ii_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING  cg_data = lv_enh_shorttext ).

    li_enh_object ?= ii_enh_spot_tool.
    li_enh_object_docu ?= ii_enh_spot_tool.

    TRY.
        li_enh_object_docu->set_shorttext( lv_enh_shorttext ).

        lo_badidef_tool ?= ii_enh_spot_tool.

        LOOP AT lt_enh_badi ASSIGNING <ls_enh_badi>.
          lo_badidef_tool->add_badi_def( <ls_enh_badi> ).
        ENDLOOP.

        li_enh_object->save( ).
        li_enh_object->activate( ).
        li_enh_object->unlock( ).

      CATCH cx_enh_root INTO lx_enh_root.
        Lcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object_enhs~serialize.

    DATA: lv_spot_name       TYPE enhspotname,
          lv_parent          TYPE enhspotcompositename,
          lt_enh_badi        TYPE enh_badi_data_it,
          lo_badidef_tool    TYPE REF TO cl_enh_tool_badi_def,
          lv_enh_shorttext   TYPE string,
          li_enh_object_docu TYPE REF TO if_enh_object_docu.

    lo_badidef_tool ?= ii_enh_spot_tool.

    li_enh_object_docu ?= ii_enh_spot_tool.
    lv_enh_shorttext = li_enh_object_docu->get_shorttext( ).

    "get parent = composite enhs (ENHC)
    lv_parent = cl_r3standard_persistence=>enh_find_parent_composite( lv_spot_name ).
    "get subsequent BADI definitions
    lt_enh_badi = lo_badidef_tool->get_badi_defs( ).

    ii_xml->add( ig_data = ii_enh_spot_tool->get_tool( )
                 iv_name = 'TOOL' ).

    ii_xml->add( ig_data = lv_enh_shorttext
                 iv_name = 'SHORTTEXT' ).

    ii_xml->add( ig_data = lv_parent
                 iv_name = 'PARENT_COMP' ).

    ii_xml->add( ig_data = lt_enh_badi
                 iv_name = 'BADI_DATA' ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENHS_BADI_D implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ENHS_HOOK_D <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_enhs_hook_dccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_enhs_hook_dccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ENHS_HOOK_D implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object_enhs~deserialize.

    DATA: lv_enh_shorttext       TYPE string,
          ls_enh_hook_definition TYPE enh_hook_def,
          ls_hook_definition     TYPE ty_hook_defifnition,
          li_enh_object          TYPE REF TO if_enh_object,
          li_enh_object_docu     TYPE REF TO if_enh_object_docu,
          lo_hookdef_tool        TYPE REF TO cl_enh_tool_hook_def,
          lx_enh_root            TYPE REF TO cx_enh_root.

    FIELD-SYMBOLS: <ls_hook_definition> TYPE enh_hook_def_ext.

    ii_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING  cg_data = lv_enh_shorttext ).

    ii_xml->read( EXPORTING iv_name = 'BADI_DATA'
                  CHANGING  cg_data = ls_hook_definition ).

    li_enh_object ?= ii_enh_spot_tool.
    li_enh_object_docu ?= ii_enh_spot_tool.

    TRY.
        li_enh_object_docu->set_shorttext( lv_enh_shorttext ).

        lo_hookdef_tool ?= ii_enh_spot_tool.

        lo_hookdef_tool->set_original_object( pgmid     = ls_hook_definition-pgmid
                                              obj_name  = ls_hook_definition-obj_name
                                              obj_type  = ls_hook_definition-obj_type
                                              program   = ls_hook_definition-program
                                              main_type = ls_hook_definition-main_type
                                              main_name = ls_hook_definition-main_name ).

        LOOP AT ls_hook_definition-def_hooks ASSIGNING <ls_hook_definition>.
          MOVE-CORRESPONDING <ls_hook_definition> TO ls_enh_hook_definition.
          lo_hookdef_tool->add_hook_def( ls_enh_hook_definition ).
        ENDLOOP.

        li_enh_object->save( ).
        li_enh_object->activate( ).
        li_enh_object->unlock( ).

      CATCH cx_enh_root INTO lx_enh_root.
        Lcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object_enhs~serialize.

    DATA: lo_hookdef_tool    TYPE REF TO cl_enh_tool_hook_def,
          lv_enh_shorttext   TYPE string,
          li_enh_object_docu TYPE REF TO if_enh_object_docu,
          ls_hook_definition TYPE ty_hook_defifnition.

    lo_hookdef_tool ?= ii_enh_spot_tool.

    li_enh_object_docu ?= ii_enh_spot_tool.
    lv_enh_shorttext = li_enh_object_docu->get_shorttext( ).

    ls_hook_definition-def_hooks = lo_hookdef_tool->get_hook_defs( ).

    lo_hookdef_tool->get_original_object(
      IMPORTING
        pgmid     = ls_hook_definition-pgmid
        obj_name  = ls_hook_definition-obj_name
        obj_type  = ls_hook_definition-obj_type
        main_type = ls_hook_definition-main_type
        main_name = ls_hook_definition-main_name
        program   = ls_hook_definition-program ).

    ii_xml->add( ig_data = ii_enh_spot_tool->get_tool( )
                 iv_name = 'TOOL' ).

    ii_xml->add( ig_data = lv_enh_shorttext
                 iv_name = 'SHORTTEXT' ).

    ii_xml->add( ig_data = ls_hook_definition
                 iv_name = 'BADI_DATA' ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENHS_HOOK_D implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_WAPA <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_wapa=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_wapa=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_WAPA implementation.
*"* method's implementations
*include methods.
  METHOD create_new_application.

    DATA: ls_item   LIKE ms_item,
          lv_objkey TYPE seu_objkey.

    cl_o2_api_application=>create_new(
      EXPORTING
        p_application_data      = is_attributes
        p_nodes                 = it_nodes
        p_navgraph              = it_navgraph
      IMPORTING
        p_application           = ro_bsp
      EXCEPTIONS
        object_already_existing = 1
        object_just_created     = 2
        not_authorized          = 3
        undefined_name          = 4
        author_not_existing     = 5
        action_cancelled        = 6
        error_occured           = 7
        invalid_parameter       = 8 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |WAPA - error from create_new: { sy-subrc }| ).
    ENDIF.

    ro_bsp->save( ).

    ro_bsp->set_changeable(
      p_changeable           = abap_false
      p_complete_application = abap_true ).

    ls_item-obj_type = 'WAPD'.
    ls_item-obj_name = ms_item-obj_name.
    Lcl_abapgit_objects_activation=>add_item( ls_item ).

    lv_objkey = ls_item-obj_name.
* todo, hmm, the WAPD is not added to the worklist during activation
    cl_o2_api_application=>activate( lv_objkey ).


  ENDMETHOD.
  METHOD create_new_page.

    cl_o2_api_pages=>create_new_page(
      EXPORTING
        p_pageattrs = is_page_attributes
      IMPORTING
        p_page      = ro_page
      EXCEPTIONS
        object_already_exists = 1
        invalid_name          = 2
        error_occured         = 3
        o2appl_not_existing   = 4
        OTHERS                = 5 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error { sy-subrc } from CL_O2_API_PAGES=>CREATE_NEW_PAGE| ).
    ENDIF.

  ENDMETHOD.
  METHOD delete_superfluous_pages.

    DATA: ls_pagekey TYPE o2pagkey.
    FIELD-SYMBOLS: <ls_local_page> LIKE LINE OF it_local_pages.

    " delete local pages which doesn't exists remotely
    LOOP AT it_local_pages ASSIGNING <ls_local_page>.

      READ TABLE it_remote_pages WITH KEY attributes-pagekey = <ls_local_page>-pagekey
                               TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        " page exists locally but not remotely -> delete

        ls_pagekey-applname = <ls_local_page>-applname.
        ls_pagekey-pagekey = <ls_local_page>-pagekey.

        cl_o2_page=>delete_page_for_application(
          EXPORTING
            p_pagekey           = ls_pagekey
          EXCEPTIONS
            object_not_existing = 1
            error_occured       = 2 ).

        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise( |Error { sy-subrc } from CL_O2_PAGE=>DELETE_PAGE_FOR_APPLICATION| ).
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD get_page_content.

    DATA: lt_content TYPE o2pageline_table,
          lv_string  TYPE string.

    io_page->get_page(
      IMPORTING
        p_content = lt_content
      EXCEPTIONS
        invalid_call = 1
        page_deleted = 2
        OTHERS       = 3 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |WAPA - error from get_page_content| ).
    ENDIF.

    CONCATENATE LINES OF lt_content INTO lv_string SEPARATED BY cl_abap_char_utilities=>newline RESPECTING BLANKS.

    rv_content = Lcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).

  ENDMETHOD.
  METHOD read_page.

    DATA: lv_name    TYPE o2applname,
          ls_pagekey TYPE o2pagkey,
          lv_content TYPE xstring,
          lv_extra   TYPE string,
          lv_ext     TYPE string,
          lo_page    TYPE REF TO cl_o2_api_pages.


    lv_name = ms_item-obj_name.

    ls_pagekey-applname = lv_name.
    ls_pagekey-pagekey = is_page-pagekey.

    cl_o2_api_pages=>load(
      EXPORTING
        p_pagekey = ls_pagekey
      IMPORTING
        p_page    = lo_page ).

    lo_page->get_attrs( IMPORTING p_attrs = rs_page-attributes ).

    IF rs_page-attributes-pagetype <> so2_controller.

      lo_page->get_event_handlers(
        IMPORTING
          p_ev_handler = rs_page-event_handlers
        EXCEPTIONS
          page_deleted = 1
          invalid_call = 2 ).
      ASSERT sy-subrc = 0.

      lo_page->get_parameters(
        IMPORTING
          p_parameters = rs_page-parameters
        EXCEPTIONS
          page_deleted = 1
          invalid_call = 2
          OTHERS       = 3 ).
      ASSERT sy-subrc = 0.

      lo_page->get_type_source(
        IMPORTING
          p_source     = rs_page-types
        EXCEPTIONS
          page_deleted = 1
          invalid_call = 2
          OTHERS       = 3 ).
      ASSERT sy-subrc = 0.

      lv_content = get_page_content( lo_page ).
      SPLIT is_page-pagename AT '.' INTO lv_extra lv_ext.
      REPLACE ALL OCCURRENCES OF '/' IN lv_ext WITH '_-'.
      REPLACE ALL OCCURRENCES OF '/' IN lv_extra WITH '_-'.
      IF iv_no_files_add = abap_false.
        Lif_abapgit_object~mo_files->add_raw(
          iv_extra = lv_extra
          iv_ext   = lv_ext
          iv_data  = lv_content ).
      ENDIF.

      CLEAR: rs_page-attributes-implclass.

    ENDIF.

    CLEAR: rs_page-attributes-author,
           rs_page-attributes-createdon,
           rs_page-attributes-changedby,
           rs_page-attributes-changedon,
           rs_page-attributes-changetime,
           rs_page-attributes-gendate,
           rs_page-attributes-gentime,
           rs_page-attributes-devclass.

  ENDMETHOD.
  METHOD to_page_content.

    DATA: lv_string TYPE string.


    lv_string = Lcl_abapgit_convert=>xstring_to_string_utf8( iv_content ).

    SPLIT lv_string AT cl_abap_char_utilities=>newline INTO TABLE rt_content.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lv_name   TYPE o2applname,
          lt_pages  TYPE STANDARD TABLE OF o2pagdir WITH DEFAULT KEY,
          ls_latest LIKE LINE OF lt_pages.


    lv_name = ms_item-obj_name.

    SELECT * FROM o2pagdir INTO TABLE lt_pages WHERE applname = lv_name
      ORDER BY changedon DESCENDING changetime DESCENDING.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
      RETURN.
    ENDIF.

    READ TABLE lt_pages INDEX 1 INTO ls_latest.
    ASSERT sy-subrc = 0.

    rv_user = ls_latest-changedby.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_name        TYPE o2applname,
          lo_bsp         TYPE REF TO cl_o2_api_application,
          ls_pagekey     TYPE o2pagkey,
          lv_object      TYPE seu_objkey,
          lt_pages       TYPE o2pagelist,
          lt_local_mimes TYPE o2pagename_table.

    FIELD-SYMBOLS: <ls_page>       LIKE LINE OF lt_pages,
                   <ls_local_mime> TYPE o2pagename.

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
    ASSERT sy-subrc = 0.

    lo_bsp->set_changeable(
      p_changeable           = abap_true
      p_complete_application = abap_true ).

    cl_o2_api_pages=>get_all_pages(
      EXPORTING
        p_applname = lv_name
        p_version  = c_active
      IMPORTING
        p_pages    = lt_pages ).

    LOOP AT lt_pages ASSIGNING <ls_page>.
      CLEAR ls_pagekey.
      ls_pagekey-applname = lv_name.
      ls_pagekey-pagekey  = <ls_page>-pagekey.

      cl_o2_page=>delete_page_for_application(
        EXPORTING
          p_pagekey           = ls_pagekey
        EXCEPTIONS
          object_not_existing = 1
          error_occured       = 2 ).
      ASSERT sy-subrc = 0.
    ENDLOOP.

    lo_bsp->get_local_mimes(
      IMPORTING
        p_local_mimes  = lt_local_mimes
      EXCEPTIONS
        object_invalid = 1
        object_deleted = 2
        error_occured  = 3
        OTHERS         = 4 ).

    LOOP AT lt_local_mimes ASSIGNING <ls_local_mime>.
      CLEAR ls_pagekey.
      ls_pagekey-applname = <ls_local_mime>-applname.
      ls_pagekey-pagekey  = <ls_local_mime>-pagekey.

      cl_o2_page=>delete_page_for_application(
        EXPORTING
          p_pagekey           = ls_pagekey
        EXCEPTIONS
          object_not_existing = 1
          error_occured       = 2 ).
      ASSERT sy-subrc = 0.
    ENDLOOP.

    lo_bsp->delete(
      EXCEPTIONS
        object_not_empty      = 1
        object_not_changeable = 2
        object_invalid        = 3
        action_cancelled      = 4
        permission_failure    = 5
        error_occured         = 6
        OTHERS                = 7 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |WAPA - error from delete: { sy-subrc }| ).
    ENDIF.

* release lock
    lv_object = lv_name.
    cl_o2_api_application=>call_access_permission(
      p_mode                 = 'FREE'
      p_object               = lv_object
      p_complete_application = abap_true ).
