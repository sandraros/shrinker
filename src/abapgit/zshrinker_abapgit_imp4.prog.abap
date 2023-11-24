********************************************************************************
*
* LICENSE and NOTICE
*
* See include program ZSHRINKER_ABAPGIT_LICENSE
*
********************************************************************************
      ls_gentab-tabix = sy-tabix.

      get_ddic_type(
        EXPORTING
          iv_obj_type = <ls_object>-object
          iv_obj_name = <ls_object>-obj_name
        IMPORTING
          ev_type     = ls_gentab-type
          ev_name     = ls_gentab-name
          ev_id       = ls_gentab-indx ).

      INSERT ls_gentab INTO TABLE lt_gentab.
    ENDLOOP.

    IF lt_gentab IS NOT INITIAL.

      lv_logname = |ABAPGIT_{ sy-datum }_{ sy-uzeit }|.

      IF lines( lt_gentab ) = 1.
        ii_log->add_info( |> Mass activating 1 DDIC object| ).
      ELSE.
        ii_log->add_info( |> Mass activating { lines( lt_gentab ) } DDIC objects| ).
      ENDIF.
      ii_log->add_info( |Log name: { lv_logname }| ).

      CALL FUNCTION 'DD_MASS_ACT_C3'
        EXPORTING
          ddmode         = 'O'         " activate changes in Original System
          frcact         = abap_true   " force Activation
          medium         = 'T'         " transport order
          device         = 'T'         " saves to table DDRPH?
          version        = 'M'         " activate newest version
          logname        = lv_logname
          write_log      = abap_true
          log_head_tail  = abap_true
          t_on           = space
          prid           = 1
        IMPORTING
          act_rc         = lv_rc
        TABLES
          gentab         = lt_gentab
          deltab         = lt_deltab
          cnvtab         = lt_action_tab
        EXCEPTIONS
          access_failure = 1
          no_objects     = 2
          locked         = 3
          internal_error = 4
          OTHERS         = 5.

      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      IF lv_rc > 0.
        add_errors_and_warnings_to_log(
          iv_logname = lv_logname
          ii_log     = ii_log ).
      ENDIF.

      IF lv_rc > 4.
        Lcx_abapgit_exception=>raise( 'Activation cancelled. Check the inactive objects.' ).
      ENDIF.

      " Remove objects from activation queue to avoid double activation in activate_old
      LOOP AT lt_gentab INTO ls_gentab.
        DELETE gt_objects WHERE object = ls_gentab-type AND obj_name = ls_gentab-name.
      ENDLOOP.
      DELETE gt_objects WHERE object = 'INDX' OR object = 'XINX' OR object = 'MCID'.

    ENDIF.

  ENDMETHOD.
  METHOD activate_new.

    IF gt_objects IS INITIAL.
      RETURN.
    ENDIF.

    IF iv_ddic = abap_true.

      activate_ddic( ii_log ).

    ELSE.

      activate_old( ii_log ).

    ENDIF.

  ENDMETHOD.
  METHOD activate_old.

    DATA:
      lv_popup     TYPE abap_bool,
      lv_no_ui     TYPE abap_bool,
      lv_try_again TYPE abap_bool,
      lv_msg       TYPE string,
      lo_checklist TYPE REF TO cl_wb_checklist.

    IF gt_objects IS NOT INITIAL.

      IF Lcl_abapgit_ui_factory=>get_frontend_services( )->gui_is_available( ) = abap_true.
        IF Lcl_abapgit_persist_factory=>get_settings( )->read( )->get_activate_wo_popup( ) = abap_true.
          lv_popup = abap_false.
        ELSE.
          lv_popup = abap_true.
        ENDIF.
      ELSE.
        lv_popup = abap_false.
      ENDIF.

      lv_no_ui = boolc( lv_popup = abap_false ).

      IF iv_ddic = abap_true.
        lv_msg = |(with DDIC)|.
      ENDIF.
      IF lines( gt_objects ) = 1.
        ii_log->add_info( |> Activating 1 object { lv_msg }| ).
      ELSE.
        ii_log->add_info( |> Activating { lines( gt_objects ) } objects { lv_msg }| ).
      ENDIF.

      TRY.
          CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
            EXPORTING
              activate_ddic_objects  = iv_ddic
              with_popup             = lv_popup
              ui_decoupled           = lv_no_ui
            IMPORTING
              p_checklist            = lo_checklist
            TABLES
              objects                = gt_objects
            EXCEPTIONS
              excecution_error       = 1
              cancelled              = 2
              insert_into_corr_error = 3
              OTHERS                 = 4 ##SUBRC_OK.
        CATCH cx_sy_dyn_call_param_not_found.
          CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
            EXPORTING
              activate_ddic_objects  = iv_ddic
              with_popup             = lv_popup
            IMPORTING
              p_checklist            = lo_checklist
            TABLES
              objects                = gt_objects
            EXCEPTIONS
              excecution_error       = 1
              cancelled              = 2
              insert_into_corr_error = 3
              OTHERS                 = 4 ##SUBRC_OK.
      ENDTRY.
      CASE sy-subrc.
        WHEN 1 OR 3 OR 4.
          Lcx_abapgit_exception=>raise_t100( ).
        WHEN 2.
          lv_msg = 'Check the log and inactive objects.'.
          IF lv_popup = abap_false.
            lv_try_again = add_activation_errors_to_log(
              ii_log       = ii_log
              io_checklist = lo_checklist ).
            IF lv_try_again = abap_true.
              lv_msg = 'Turn on "Activation Popup" in "Personal Settings" and try again'.
            ENDIF.
          ENDIF.
          Lcx_abapgit_exception=>raise( |Activation cancelled. { lv_msg }| ).
      ENDCASE.

    ENDIF.

  ENDMETHOD.
  METHOD add.

* function group SEWORKINGAREA
* function module RS_INSERT_INTO_WORKING_AREA
* class CL_WB_ACTIVATION_WORK_AREA

    FIELD-SYMBOLS: <ls_object>  TYPE dwinactiv,
                   <ls_classes> LIKE LINE OF gt_classes.

    IF iv_type = 'CLAS' OR iv_type = 'INTF'.
      APPEND INITIAL LINE TO gt_classes ASSIGNING <ls_classes>.
      <ls_classes>-object  = iv_type.
      <ls_classes>-clsname = iv_name.
    ELSE.
      APPEND INITIAL LINE TO gt_objects ASSIGNING <ls_object>.
      <ls_object>-object     = iv_type.
      <ls_object>-obj_name   = iv_name.
      <ls_object>-delet_flag = iv_delete.
    ENDIF.

  ENDMETHOD.
  METHOD add_activation_errors_to_log.

    DATA:
      ls_item    TYPE Lif_abapgit_definitions=>ty_item,
      lt_message TYPE swbme_error_tab.

    FIELD-SYMBOLS:
      <lv_msg>     TYPE string,
      <ls_message> LIKE LINE OF lt_message.

    io_checklist->get_error_messages( IMPORTING p_error_tab = lt_message ).

    LOOP AT lt_message ASSIGNING <ls_message> WHERE mtype = 'E'.
      " When activting without popup, includes used in multiple main programs cause error
      " Run again WITH activation popup (see abapGit, Personal Settings)
      IF <ls_message>-message-msgid = 'EU' AND <ls_message>-message-msgno = '404'.
        rv_try_again = abap_true.
      ENDIF.
      CLEAR ls_item.
      IF strlen( <ls_message>-object_text ) > 5.
        ls_item-obj_type = <ls_message>-object_text(4).
        ls_item-obj_name = <ls_message>-object_text+5(*).
      ELSE.
        ls_item-obj_name = <ls_message>-show_req->object_name.
        SELECT SINGLE tadir FROM euobjedit INTO ls_item-obj_type
          WHERE type = <ls_message>-show_req->object_type.
      ENDIF.
      LOOP AT <ls_message>-mtext ASSIGNING <lv_msg>.
        ii_log->add_error(
          iv_msg  = <lv_msg>
          is_item = ls_item ).
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.
  METHOD add_errors_and_warnings_to_log.

    DATA: lt_lines      TYPE STANDARD TABLE OF trlog,
          lv_logname_db TYPE ddprh-protname.

    FIELD-SYMBOLS: <ls_line> LIKE LINE OF lt_lines.


    lv_logname_db = iv_logname.

    CALL FUNCTION 'TR_READ_LOG'
      EXPORTING
        iv_log_type   = 'DB'
        iv_logname_db = lv_logname_db
      TABLES
        et_lines      = lt_lines
      EXCEPTIONS
        invalid_input = 1
        access_error  = 2
        OTHERS        = 3.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    " Only error messsages
    DELETE lt_lines WHERE severity <> 'E'
                      AND severity <> 'W'.
    " Remove "Return code..." message
    DELETE lt_lines WHERE class = 'D0' AND number = '319'.

    LOOP AT lt_lines ASSIGNING <ls_line>.
      ii_log->add( iv_msg  = <ls_line>-line
                   iv_type = <ls_line>-severity ).
    ENDLOOP.

    ii_log->add_info( |View complete activation log in program RSPUTPRT (type D, log name { iv_logname })| ).

  ENDMETHOD.
  METHOD add_item.
    add( iv_type = is_item-obj_type
         iv_name = is_item-obj_name ).
  ENDMETHOD.
  METHOD clear.
    CLEAR gt_objects.
    CLEAR gt_classes.
  ENDMETHOD.
  METHOD get_ddic_type.

    DATA lv_obj_name TYPE e071-obj_name.

    ev_type = iv_obj_type.

    IF ev_type = 'INDX' OR ev_type = 'XINX' OR ev_type = 'MCID'.
      lv_obj_name = iv_obj_name. "cast

      CALL FUNCTION 'DD_E071_TO_DD'
        EXPORTING
          object        = ev_type
          obj_name      = lv_obj_name
        IMPORTING
          name          = ev_name
          id            = ev_id
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ELSE.
      ev_name = iv_obj_name.
    ENDIF.

  ENDMETHOD.
  METHOD is_active.

    " Checks if object is active or not
    "
    " Note: If object does not exist, this method returns true
    " is_not_inactive might be a better name but we avoid the double negative

    IF is_ddic_type( is_item-obj_type ) = abap_true
      AND c_para     NS is_item-obj_type
      AND c_switches NS is_item-obj_type.
      rv_active = is_ddic_active( is_item ).
    ELSE.
      rv_active = is_non_ddic_active( is_item ).
    ENDIF.

  ENDMETHOD.
  METHOD is_ddic_active.

    DATA:
      lv_type  TYPE ddobjtyp,
      lv_name  TYPE ddobjname,
      lv_id    TYPE ddobjectid,
      lv_state TYPE ddgotstate.

    get_ddic_type(
      EXPORTING
        iv_obj_type = is_item-obj_type
        iv_obj_name = is_item-obj_name
      IMPORTING
        ev_type     = lv_type
        ev_name     = lv_name
        ev_id       = lv_id ).

    " Check if an inactive version of the DDIC object exists
    " state = 'A' checks if an active version exists but does not detect new or modified objects
    " state = 'M' checks for all possible versions so we can find out if an inactive one exists
    " See documentation of the function module
    CALL FUNCTION 'DDIF_STATE_GET'
      EXPORTING
        type          = lv_type
        name          = lv_name
        id            = lv_id
        state         = 'M'
      IMPORTING
        gotstate      = lv_state
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    rv_active = boolc( sy-subrc = 0 AND ( lv_state = '' OR lv_state = 'A' ) ).

  ENDMETHOD.
  METHOD is_ddic_type.

    " Determine if object can be handled by mass activation (see RADMASUTC form ma_tab_check)

    rv_result = abap_true.

    IF c_domain   NS iv_obj_type AND c_types      NS iv_obj_type AND
       c_technset NS iv_obj_type AND c_f4_objects NS iv_obj_type AND
       c_enqueue  NS iv_obj_type AND c_sqsc       NS iv_obj_type AND
       c_stob     NS iv_obj_type AND c_ntab       NS iv_obj_type AND
       c_ddls     NS iv_obj_type AND c_para       NS iv_obj_type AND
       c_switches NS iv_obj_type AND iv_obj_type <> c_enhd.
      rv_result = abap_false.
    ENDIF.

  ENDMETHOD.
  METHOD is_non_ddic_active.

    DATA:
      lt_messages TYPE STANDARD TABLE OF sprot_u WITH DEFAULT KEY,
      ls_e071     TYPE e071,
      lt_e071     TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY.

    ls_e071-object   = is_item-obj_type.
    ls_e071-obj_name = is_item-obj_name.
    INSERT ls_e071 INTO TABLE lt_e071.

    CALL FUNCTION 'RS_INACTIVE_OBJECTS_WARNING'
      EXPORTING
        suppress_protocol         = abap_false
        with_program_includes     = abap_false
        suppress_dictionary_check = abap_false
      TABLES
        p_e071                    = lt_e071
        p_xmsg                    = lt_messages.

    rv_active = boolc( lt_messages IS INITIAL ).

  ENDMETHOD.
  METHOD update_where_used.

    DATA: ls_class   LIKE LINE OF gt_classes,
          lo_cross   TYPE REF TO cl_wb_crossreference,
          ls_item    TYPE Lif_abapgit_definitions=>ty_item,
          lv_msg     TYPE string,
          lv_error   TYPE c LENGTH 1,
          lv_include TYPE syrepid.

    LOOP AT gt_classes INTO ls_class.
      CASE ls_class-object.
        WHEN 'CLAS'.
          lv_include = cl_oo_classname_service=>get_classpool_name( ls_class-clsname ).
        WHEN 'INTF'.
          lv_include = cl_oo_classname_service=>get_interfacepool_name( ls_class-clsname ).
      ENDCASE.

      CREATE OBJECT lo_cross
        EXPORTING
          p_name    = lv_include
          p_include = lv_include.

      lo_cross->index_actualize( IMPORTING p_error = lv_error ).

      IF lv_error = abap_true.
        ls_item-obj_type = ls_class-object.
        ls_item-obj_name = ls_class-clsname.
        lv_msg = |Error updating where-used list for { ls_item-obj_type } { ls_item-obj_name }.|
          && | Check for syntax errors|.
        ii_log->add(
          iv_msg  = lv_msg
          is_item = ls_item ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD use_new_activation_logic.

    IF Lcl_abapgit_factory=>get_function_module( )->function_exists( 'DD_MASS_ACT_C3' ) = abap_true.
      rv_use_new_activation_logic = abap_true.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECTS_ACTIVATION implementation

*>>>>>>> ZCL_ABAPGIT_OBJECTS_PROGRAM <<<<<<<*

*"* macro definitions
*include zcl_abapgit_objects_program===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_objects_program===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_objects_program===ccau.

*CLASS zcl_abapgit_objects_program DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KW4ESFY.


class LCL_ABAPGIT_OBJECTS_PROGRAM implementation.
*"* method's implementations
*include methods.
  METHOD add_tpool.

    FIELD-SYMBOLS: <ls_tpool_in>  LIKE LINE OF it_tpool,
                   <ls_tpool_out> LIKE LINE OF rt_tpool.


    LOOP AT it_tpool ASSIGNING <ls_tpool_in>.
      APPEND INITIAL LINE TO rt_tpool ASSIGNING <ls_tpool_out>.
      MOVE-CORRESPONDING <ls_tpool_in> TO <ls_tpool_out>.
      IF <ls_tpool_out>-id = 'S'.
        <ls_tpool_out>-split = <ls_tpool_out>-entry.
        <ls_tpool_out>-entry = <ls_tpool_out>-entry+8.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD auto_correct_cua_adm.
    " issue #1807 automatic correction of CUA interfaces saved incorrectly in the past (ADM was not saved in the XML)

    CONSTANTS:
      lc_num_n_space TYPE string VALUE ' 0123456789',
      lc_num_only    TYPE string VALUE '0123456789'.

    FIELD-SYMBOLS:
      <ls_pfk> TYPE rsmpe_pfk,
      <ls_act> TYPE rsmpe_act,
      <ls_men> TYPE rsmpe_men.

    IF cs_adm IS NOT INITIAL
        AND cs_adm-actcode CO lc_num_n_space
        AND cs_adm-mencode CO lc_num_n_space
        AND cs_adm-pfkcode CO lc_num_n_space. "Check performed in form check_adm of include LSMPIF03
      RETURN.
    ENDIF.

    LOOP AT is_cua-act ASSIGNING <ls_act>.
      IF <ls_act>-code+6(14) IS INITIAL AND <ls_act>-code(6) CO lc_num_only.
        cs_adm-actcode = <ls_act>-code.
      ENDIF.
    ENDLOOP.

    LOOP AT is_cua-men ASSIGNING <ls_men>.
      IF <ls_men>-code+6(14) IS INITIAL AND <ls_men>-code(6) CO lc_num_only.
        cs_adm-mencode = <ls_men>-code.
      ENDIF.
    ENDLOOP.

    LOOP AT is_cua-pfk ASSIGNING <ls_pfk>.
      IF <ls_pfk>-code+6(14) IS INITIAL AND <ls_pfk>-code(6) CO lc_num_only.
        cs_adm-pfkcode = <ls_pfk>-code.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD deserialize_cua.

    DATA: ls_tr_key TYPE trkey,
          ls_adm    TYPE rsmpe_adm.


    IF lines( is_cua-sta ) = 0
        AND lines( is_cua-fun ) = 0
        AND lines( is_cua-men ) = 0
        AND lines( is_cua-mtx ) = 0
        AND lines( is_cua-act ) = 0
        AND lines( is_cua-but ) = 0
        AND lines( is_cua-pfk ) = 0
        AND lines( is_cua-set ) = 0
        AND lines( is_cua-doc ) = 0
        AND lines( is_cua-tit ) = 0
        AND lines( is_cua-biv ) = 0.
      RETURN.
    ENDIF.

    SELECT SINGLE devclass INTO ls_tr_key-devclass
      FROM tadir
      WHERE pgmid = 'R3TR'
      AND object = ms_item-obj_type
      AND obj_name = ms_item-obj_name.                  "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'not found in tadir' ).
    ENDIF.

    ls_tr_key-obj_type = ms_item-obj_type.
    ls_tr_key-obj_name = ms_item-obj_name.
    ls_tr_key-sub_type = 'CUAD'.
    ls_tr_key-sub_name = iv_program_name.

    ls_adm = is_cua-adm.
    auto_correct_cua_adm( EXPORTING is_cua = is_cua CHANGING cs_adm = ls_adm ).

    sy-tcode = 'SE41' ##WRITE_OK. " evil hack, workaround to handle fixes in note 2159455
    CALL FUNCTION 'RS_CUA_INTERNAL_WRITE'
      EXPORTING
        program   = iv_program_name
        language  = mv_language
        tr_key    = ls_tr_key
        adm       = ls_adm
        state     = c_state-inactive
      TABLES
        sta       = is_cua-sta
        fun       = is_cua-fun
        men       = is_cua-men
        mtx       = is_cua-mtx
        act       = is_cua-act
        but       = is_cua-but
        pfk       = is_cua-pfk
        set       = is_cua-set
        doc       = is_cua-doc
        tit       = is_cua-tit
        biv       = is_cua-biv
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
* if moving code from SAPlink, see https://github.com/abapGit/abapGit/issues/562
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    Lcl_abapgit_objects_activation=>add(
      iv_type = 'CUAD'
      iv_name = iv_program_name ).

  ENDMETHOD.
  METHOD deserialize_dynpros.

    CONSTANTS lc_rpyty_force_off TYPE c LENGTH 1 VALUE '/'.

    DATA: lv_name            TYPE dwinactiv-obj_name,
          lt_d020s_to_delete TYPE TABLE OF d020s,
          ls_d020s           LIKE LINE OF lt_d020s_to_delete,
          ls_dynpro          LIKE LINE OF it_dynpros.

    FIELD-SYMBOLS: <ls_field> TYPE rpy_dyfatc.

    " Delete DYNPROs which are not in the list
    CALL FUNCTION 'RS_SCREEN_LIST'
      EXPORTING
        dynnr     = ''
        progname  = ms_item-obj_name
      TABLES
        dynpros   = lt_d020s_to_delete
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc = 2.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    SORT lt_d020s_to_delete BY dnum ASCENDING.

* ls_dynpro is changed by the function module, a field-symbol will cause
* the program to dump since it_dynpros cannot be changed
    LOOP AT it_dynpros INTO ls_dynpro.

      READ TABLE lt_d020s_to_delete WITH KEY dnum = ls_dynpro-header-screen
        TRANSPORTING NO FIELDS
        BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE lt_d020s_to_delete INDEX sy-tabix.
      ENDIF.

      " todo: kept for compatibility, remove after grace period #3680
      ls_dynpro-flow_logic = uncondense_flow(
        it_flow = ls_dynpro-flow_logic
        it_spaces = ls_dynpro-spaces ).


      LOOP AT ls_dynpro-fields ASSIGNING <ls_field>.
* if the DDIC element has a PARAMETER_ID and the flag "from_dict" is active
* the import will enable the SET-/GET_PARAM flag. In this case: "force off"
        IF <ls_field>-param_id IS NOT INITIAL
            AND <ls_field>-from_dict = abap_true.
          IF <ls_field>-set_param IS INITIAL.
            <ls_field>-set_param = lc_rpyty_force_off.
          ENDIF.
          IF <ls_field>-get_param IS INITIAL.
            <ls_field>-get_param = lc_rpyty_force_off.
          ENDIF.
        ENDIF.

* If the previous conditions are met the value 'F' will be taken over
* during de-serialization potentially overlapping other fields in the screen,
* we set the tag to the correct value 'X'
        IF <ls_field>-type = 'CHECK'
            AND <ls_field>-from_dict = abap_true
            AND <ls_field>-text IS INITIAL
            AND <ls_field>-modific IS INITIAL.
          <ls_field>-modific = 'X'.
        ENDIF.

        "fix for issue #2747:
        IF <ls_field>-foreignkey IS INITIAL.
          <ls_field>-foreignkey = lc_rpyty_force_off.
        ENDIF.

      ENDLOOP.

      CALL FUNCTION 'RPY_DYNPRO_INSERT'
        EXPORTING
          header                 = ls_dynpro-header
          suppress_exist_checks  = abap_true
          suppress_generate      = ls_dynpro-header-no_execute
        TABLES
          containers             = ls_dynpro-containers
          fields_to_containers   = ls_dynpro-fields
          flow_logic             = ls_dynpro-flow_logic
        EXCEPTIONS
          cancelled              = 1
          already_exists         = 2
          program_not_exists     = 3
          not_executed           = 4
          missing_required_field = 5
          illegal_field_value    = 6
          field_not_allowed      = 7
          not_generated          = 8
          illegal_field_position = 9
          OTHERS                 = 10.
      IF sy-subrc <> 2 AND sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
* todo, RPY_DYNPRO_UPDATE?

      CONCATENATE ls_dynpro-header-program ls_dynpro-header-screen
        INTO lv_name RESPECTING BLANKS.
      ASSERT NOT lv_name IS INITIAL.

      Lcl_abapgit_objects_activation=>add(
        iv_type = 'DYNP'
        iv_name = lv_name ).

    ENDLOOP.

    " Delete obsolete screens
    LOOP AT lt_d020s_to_delete INTO ls_d020s.

      CALL FUNCTION 'RS_SCRP_DELETE'
        EXPORTING
          dynnr                  = ls_d020s-dnum
          progname               = ms_item-obj_name
          with_popup             = abap_false
        EXCEPTIONS
          enqueued_by_user       = 1
          enqueue_system_failure = 2
          not_executed           = 3
          not_exists             = 4
          no_modify_permission   = 5
          popup_canceled         = 6.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD deserialize_program.

    DATA:
      lv_progname TYPE reposrc-progname,
      lv_title    TYPE rglif-title.

    Lcl_abapgit_factory=>get_cts_api( )->insert_transport_object(
      iv_object   = 'ABAP'
      iv_obj_name = is_progdir-name
      iv_package  = iv_package
      iv_language = mv_language ).

    lv_title = get_program_title( it_tpool ).

    " Check if program already exists
    SELECT SINGLE progname FROM reposrc INTO lv_progname
      WHERE progname = is_progdir-name
      AND r3state = c_state-active.

    IF sy-subrc = 0.
      update_program(
        is_progdir = is_progdir
        it_source  = it_source
        iv_title   = lv_title ).
    ELSE.
      insert_program(
        is_progdir = is_progdir
        it_source  = it_source
        iv_title   = lv_title
        iv_package = iv_package ).
    ENDIF.

    Lcl_abapgit_factory=>get_sap_report( )->update_progdir(
      is_progdir = is_progdir
      iv_package = iv_package ).

    Lcl_abapgit_objects_activation=>add(
      iv_type = 'REPS'
      iv_name = is_progdir-name ).

  ENDMETHOD.
  METHOD deserialize_textpool.

    DATA lv_language TYPE sy-langu.
    DATA lv_state    TYPE c.
    DATA lv_delete   TYPE abap_bool.

    IF iv_language IS INITIAL.
      lv_language = mv_language.
    ELSE.
      lv_language = iv_language.
    ENDIF.

    IF lv_language = mv_language.
      lv_state = c_state-inactive. "Textpool in main language needs to be activated
    ELSE.
      lv_state = c_state-active. "Translations are always active
    ENDIF.

    IF it_tpool IS INITIAL.
      IF iv_is_include = abap_false OR lv_state = c_state-active.
        DELETE TEXTPOOL iv_program "Remove initial description from textpool if
          LANGUAGE lv_language     "original program does not have a textpool
          STATE lv_state.

        lv_delete = abap_true.
      ELSE.
        INSERT TEXTPOOL iv_program "In case of includes: Deletion of textpool in
          FROM it_tpool            "main language cannot be activated because
          LANGUAGE lv_language     "this woul activate the deletion of the textpool
          STATE lv_state.          "of the mail program -> insert empty textpool
      ENDIF.
    ELSE.
      INSERT TEXTPOOL iv_program
        FROM it_tpool
        LANGUAGE lv_language
        STATE lv_state.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'error from INSERT TEXTPOOL' ).
      ENDIF.
    ENDIF.

    IF lv_state = c_state-inactive. "Textpool in main language needs to be activated
      Lcl_abapgit_objects_activation=>add(
        iv_type   = 'REPT'
        iv_name   = iv_program
        iv_delete = lv_delete ).
    ENDIF.
  ENDMETHOD.
  METHOD get_program_title.

    DATA ls_tpool LIKE LINE OF it_tpool.

    FIELD-SYMBOLS <lg_any> TYPE any.

    READ TABLE it_tpool INTO ls_tpool WITH KEY id = 'R'.
    IF sy-subrc = 0.
      " there is a bug in RPY_PROGRAM_UPDATE, the header line of TTAB is not
      " cleared, so the title length might be inherited from a different program.
      ASSIGN ('(SAPLSIFP)TTAB') TO <lg_any>.
      IF sy-subrc = 0.
        CLEAR <lg_any>.
      ENDIF.

      rv_title = ls_tpool-entry.
    ENDIF.

  ENDMETHOD.
  METHOD insert_program.

    TRY.
        CALL FUNCTION 'RPY_PROGRAM_INSERT'
          EXPORTING
            development_class = iv_package
            program_name      = is_progdir-name
            program_type      = is_progdir-subc
            title_string      = iv_title
            save_inactive     = c_state-inactive
            suppress_dialog   = abap_true
            uccheck           = is_progdir-uccheck " does not exist on lower releases
          TABLES
            source_extended   = it_source
          EXCEPTIONS
            already_exists    = 1
            cancelled         = 2
            name_not_allowed  = 3
            permission_error  = 4
            OTHERS            = 5.
      CATCH cx_sy_dyn_call_param_not_found.
        CALL FUNCTION 'RPY_PROGRAM_INSERT'
          EXPORTING
            development_class = iv_package
            program_name      = is_progdir-name
            program_type      = is_progdir-subc
            title_string      = iv_title
            save_inactive     = c_state-inactive
            suppress_dialog   = abap_true
          TABLES
            source_extended   = it_source
          EXCEPTIONS
            already_exists    = 1
            cancelled         = 2
            name_not_allowed  = 3
            permission_error  = 4
            OTHERS            = 5.
    ENDTRY.
    IF sy-subrc = 3.

      " For cases that standard function does not handle (like FUGR),
      " we save active and inactive version of source with the given PROGRAM TYPE.
      " Without the active version, the code will not be visible in case of activation errors.
      Lcl_abapgit_factory=>get_sap_report( )->insert_report(
        iv_name         = is_progdir-name
        iv_package      = iv_package
        it_source       = it_source
        iv_state        = c_state-active
        iv_version      = is_progdir-uccheck
        iv_program_type = is_progdir-subc ).

      Lcl_abapgit_factory=>get_sap_report( )->insert_report(
        iv_name         = is_progdir-name
        iv_package      = iv_package
        it_source       = it_source
        iv_state        = c_state-inactive
        iv_version      = is_progdir-uccheck
        iv_program_type = is_progdir-subc ).

    ELSEIF sy-subrc > 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD is_any_dynpro_locked.

    DATA: lt_dynpros TYPE ty_dynpro_tt,
          lv_object  TYPE seqg3-garg.

    FIELD-SYMBOLS: <ls_dynpro> TYPE ty_dynpro.

    lt_dynpros = serialize_dynpros( iv_program ).

    LOOP AT lt_dynpros ASSIGNING <ls_dynpro>.

      lv_object = |{ <ls_dynpro>-header-screen }{ <ls_dynpro>-header-program }|.

      IF exists_a_lock_entry_for( iv_lock_object = 'ESCRP'
                                  iv_argument    = lv_object ) = abap_true.
        rv_is_any_dynpro_locked = abap_true.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD is_cua_locked.

    DATA: lv_object TYPE eqegraarg.

    lv_object = |CU{ iv_program }|.
    OVERLAY lv_object WITH '                                          '.
    lv_object = lv_object && '*'.

    rv_is_cua_locked = exists_a_lock_entry_for( iv_lock_object = 'ESCUAPAINT'
                                                iv_argument    = lv_object ).

  ENDMETHOD.
  METHOD is_text_locked.

    DATA: lv_object TYPE eqegraarg.

    lv_object = |*{ iv_program }|.

    rv_is_text_locked = exists_a_lock_entry_for( iv_lock_object = 'EABAPTEXTE'
                                                 iv_argument    = lv_object ).

  ENDMETHOD.
  METHOD read_tpool.

    FIELD-SYMBOLS: <ls_tpool_in>  LIKE LINE OF it_tpool,
                   <ls_tpool_out> LIKE LINE OF rt_tpool.


    LOOP AT it_tpool ASSIGNING <ls_tpool_in>.
      APPEND INITIAL LINE TO rt_tpool ASSIGNING <ls_tpool_out>.
      MOVE-CORRESPONDING <ls_tpool_in> TO <ls_tpool_out>.
      IF <ls_tpool_out>-id = 'S'.
        CONCATENATE <ls_tpool_in>-split <ls_tpool_in>-entry
          INTO <ls_tpool_out>-entry
          RESPECTING BLANKS.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD serialize_cua.

    CALL FUNCTION 'RS_CUA_INTERNAL_FETCH'
      EXPORTING
        program         = iv_program_name
        language        = mv_language
        state           = c_state-active
      IMPORTING
        adm             = rs_cua-adm
      TABLES
        sta             = rs_cua-sta
        fun             = rs_cua-fun
        men             = rs_cua-men
        mtx             = rs_cua-mtx
        act             = rs_cua-act
        but             = rs_cua-but
        pfk             = rs_cua-pfk
        set             = rs_cua-set
        doc             = rs_cua-doc
        tit             = rs_cua-tit
        biv             = rs_cua-biv
      EXCEPTIONS
        not_found       = 1
        unknown_version = 2
        OTHERS          = 3.
    IF sy-subrc > 1.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD serialize_dynpros.
    DATA: ls_header               TYPE rpy_dyhead,
          lt_containers           TYPE dycatt_tab,
          lt_fields_to_containers TYPE dyfatc_tab,
          lt_flow_logic           TYPE swydyflow,
          lt_d020s                TYPE TABLE OF d020s,
          lt_fieldlist_int        TYPE TABLE OF d021s. "internal format

    FIELD-SYMBOLS: <ls_d020s>       LIKE LINE OF lt_d020s,
                   <lv_outputstyle> TYPE scrpostyle,
                   <ls_container>   LIKE LINE OF lt_containers,
                   <ls_field>       LIKE LINE OF lt_fields_to_containers,
                   <ls_dynpro>      LIKE LINE OF rt_dynpro,
                   <ls_field_int>   LIKE LINE OF lt_fieldlist_int.

    "#2746: relevant flag values (taken from include MSEUSBIT)
    CONSTANTS: lc_flg1ddf TYPE x VALUE '20',
               lc_flg3fku TYPE x VALUE '08',
               lc_flg3for TYPE x VALUE '04',
               lc_flg3fdu TYPE x VALUE '02'.


    CALL FUNCTION 'RS_SCREEN_LIST'
      EXPORTING
        dynnr     = ''
        progname  = iv_program_name
      TABLES
        dynpros   = lt_d020s
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc = 2.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    SORT lt_d020s BY dnum ASCENDING.

* loop dynpros and skip generated selection screens
    LOOP AT lt_d020s ASSIGNING <ls_d020s>
        WHERE type <> 'S' AND type <> 'W' AND type <> 'J'
        AND NOT dnum IS INITIAL.

      CALL FUNCTION 'RPY_DYNPRO_READ'
        EXPORTING
          progname             = iv_program_name
          dynnr                = <ls_d020s>-dnum
        IMPORTING
          header               = ls_header
        TABLES
          containers           = lt_containers
          fields_to_containers = lt_fields_to_containers
          flow_logic           = lt_flow_logic
        EXCEPTIONS
          cancelled            = 1
          not_found            = 2
          permission_error     = 3
          OTHERS               = 4.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      "#2746: we need the dynpro fields in internal format:
      FREE lt_fieldlist_int.

      CALL FUNCTION 'RPY_DYNPRO_READ_NATIVE'
        EXPORTING
          progname  = iv_program_name
          dynnr     = <ls_d020s>-dnum
        TABLES
          fieldlist = lt_fieldlist_int.


      LOOP AT lt_fields_to_containers ASSIGNING <ls_field>.
* output style is a NUMC field, the XML conversion will fail if it contains invalid value
* field does not exist in all versions
        ASSIGN COMPONENT 'OUTPUTSTYLE' OF STRUCTURE <ls_field> TO <lv_outputstyle>.
        IF sy-subrc = 0 AND <lv_outputstyle> = '  '.
          CLEAR <lv_outputstyle>.
        ENDIF.

        "2746: we apply the same logic as in SAPLWBSCREEN
        "for setting or unsetting the foreignkey field:
        UNASSIGN <ls_field_int>.
        READ TABLE lt_fieldlist_int ASSIGNING <ls_field_int> WITH KEY fnam = <ls_field>-name.
        IF <ls_field_int> IS ASSIGNED.
          IF <ls_field_int>-flg1 O lc_flg1ddf AND
              <ls_field_int>-flg3 O lc_flg3for AND
              <ls_field_int>-flg3 Z lc_flg3fdu AND
              <ls_field_int>-flg3 Z lc_flg3fku.
            <ls_field>-foreignkey = 'X'.
          ELSE.
            CLEAR <ls_field>-foreignkey.
          ENDIF.
        ENDIF.

        IF <ls_field>-from_dict = abap_true AND
           <ls_field>-modific   <> 'F' AND
           <ls_field>-modific   <> 'X'.
          CLEAR <ls_field>-text.
        ENDIF.
      ENDLOOP.

      LOOP AT lt_containers ASSIGNING <ls_container>.
        IF <ls_container>-c_resize_v = abap_false.
          CLEAR <ls_container>-c_line_min.
        ENDIF.
        IF <ls_container>-c_resize_h = abap_false.
          CLEAR <ls_container>-c_coln_min.
        ENDIF.
      ENDLOOP.

      APPEND INITIAL LINE TO rt_dynpro ASSIGNING <ls_dynpro>.
      <ls_dynpro>-header     = ls_header.
      <ls_dynpro>-containers = lt_containers.
      <ls_dynpro>-fields     = lt_fields_to_containers.

      <ls_dynpro>-flow_logic = lt_flow_logic.

    ENDLOOP.

  ENDMETHOD.
  METHOD serialize_program.

    DATA: ls_progdir      TYPE Lif_abapgit_sap_report=>ty_progdir,
          lv_program_name TYPE syrepid,
          lt_dynpros      TYPE ty_dynpro_tt,
          ls_cua          TYPE ty_cua,
          li_report       TYPE REF TO Lif_abapgit_sap_report,
          lt_source       TYPE TABLE OF abaptxt255,
          lt_tpool        TYPE textpool_table,
          ls_tpool        LIKE LINE OF lt_tpool,
          li_xml          TYPE REF TO Lif_abapgit_xml_output.

    IF iv_program IS INITIAL.
      lv_program_name = is_item-obj_name.
    ELSE.
      lv_program_name = iv_program.
    ENDIF.

    Lcl_abapgit_language=>set_current_language( mv_language ).

    CALL FUNCTION 'RPY_PROGRAM_READ'
      EXPORTING
        program_name     = lv_program_name
        with_includelist = abap_false
        with_lowercase   = abap_true
      TABLES
        source_extended  = lt_source
        textelements     = lt_tpool
      EXCEPTIONS
        cancelled        = 1
        not_found        = 2
        permission_error = 3
        OTHERS           = 4.

    IF sy-subrc = 2.
      Lcl_abapgit_language=>restore_login_language( ).
      RETURN.
    ELSEIF sy-subrc <> 0.
      Lcl_abapgit_language=>restore_login_language( ).
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    Lcl_abapgit_language=>restore_login_language( ).

    " If inactive version exists, then RPY_PROGRAM_READ does not return the active code
    li_report = Lcl_abapgit_factory=>get_sap_report( ).

    TRY.
        " Raises exception if inactive version does not exist
        ls_progdir = li_report->read_progdir(
          iv_name  = lv_program_name
          iv_state = c_state-inactive ).

        " Explicitly request active source code
        lt_source = li_report->read_report(
          iv_name  = lv_program_name
          iv_state = c_state-active ).
      CATCH Lcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

    ls_progdir = li_report->read_progdir(
      iv_name  = lv_program_name
      iv_state = c_state-active ).

    clear_abap_language_version( CHANGING cv_abap_language_version = ls_progdir-uccheck ).

    IF io_xml IS BOUND.
      li_xml = io_xml.
    ELSE.
      CREATE OBJECT li_xml TYPE Lcl_abapgit_xml_output.
    ENDIF.

    li_xml->add( iv_name = 'PROGDIR'
                 ig_data = ls_progdir ).
    IF ls_progdir-subc = '1' OR ls_progdir-subc = 'M'.
      lt_dynpros = serialize_dynpros( lv_program_name ).
      li_xml->add( iv_name = 'DYNPROS'
                   ig_data = lt_dynpros ).

      ls_cua = serialize_cua( lv_program_name ).
      IF NOT ls_cua IS INITIAL.
        li_xml->add( iv_name = 'CUA'
                     ig_data = ls_cua ).
      ENDIF.
    ENDIF.

    READ TABLE lt_tpool WITH KEY id = 'R' INTO ls_tpool.
    IF sy-subrc = 0 AND ls_tpool-key = '' AND ls_tpool-length = 0.
      DELETE lt_tpool INDEX sy-tabix.
    ENDIF.

    li_xml->add( iv_name = 'TPOOL'
                 ig_data = add_tpool( lt_tpool ) ).

    IF NOT io_xml IS BOUND.
      io_files->add_xml( iv_extra = iv_extra
                         ii_xml   = li_xml ).
    ENDIF.

    strip_generation_comments( CHANGING ct_source = lt_source ).

    io_files->add_abap( iv_extra = iv_extra
                        it_abap  = lt_source ).

  ENDMETHOD.
  METHOD strip_generation_comments.

    FIELD-SYMBOLS <lv_line> TYPE any. " Assuming CHAR (e.g. abaptxt255_tab) or string (FUGR)

    IF ms_item-obj_type <> 'FUGR'.
      RETURN.
    ENDIF.

    " Case 1: MV FM main prog and TOPs
    READ TABLE ct_source INDEX 1 ASSIGNING <lv_line>.
    IF sy-subrc = 0 AND <lv_line> CP '#**regenerated at *'.
      DELETE ct_source INDEX 1.
      RETURN.
    ENDIF.

    " Case 2: MV FM includes
    IF lines( ct_source ) < 5. " Generation header length
      RETURN.
    ENDIF.

    READ TABLE ct_source INDEX 1 ASSIGNING <lv_line>.
    ASSERT sy-subrc = 0.
    IF NOT <lv_line> CP '#*---*'.
      RETURN.
    ENDIF.

    READ TABLE ct_source INDEX 2 ASSIGNING <lv_line>.
    ASSERT sy-subrc = 0.
    IF NOT <lv_line> CP '#**'.
      RETURN.
    ENDIF.

    READ TABLE ct_source INDEX 3 ASSIGNING <lv_line>.
    ASSERT sy-subrc = 0.
    IF NOT <lv_line> CP '#**generation date:*'.
      RETURN.
    ENDIF.

    READ TABLE ct_source INDEX 4 ASSIGNING <lv_line>.
    ASSERT sy-subrc = 0.
    IF NOT <lv_line> CP '#**generator version:*'.
      RETURN.
    ENDIF.

    READ TABLE ct_source INDEX 5 ASSIGNING <lv_line>.
    ASSERT sy-subrc = 0.
    IF NOT <lv_line> CP '#*---*'.
      RETURN.
    ENDIF.

    DELETE ct_source INDEX 4.
    DELETE ct_source INDEX 3.

  ENDMETHOD.
  METHOD uncondense_flow.

    DATA: lv_spaces LIKE LINE OF it_spaces.

    FIELD-SYMBOLS: <ls_flow>   LIKE LINE OF it_flow,
                   <ls_output> LIKE LINE OF rt_flow.


    LOOP AT it_flow ASSIGNING <ls_flow>.
      APPEND INITIAL LINE TO rt_flow ASSIGNING <ls_output>.
      <ls_output>-line = <ls_flow>-line.

      READ TABLE it_spaces INDEX sy-tabix INTO lv_spaces.
      IF sy-subrc = 0.
        SHIFT <ls_output>-line RIGHT BY lv_spaces PLACES IN CHARACTER MODE.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD update_program.

    Lcl_abapgit_language=>set_current_language( mv_language ).

    CALL FUNCTION 'RPY_PROGRAM_UPDATE'
      EXPORTING
        program_name     = is_progdir-name
        title_string     = iv_title
        save_inactive    = c_state-inactive
      TABLES
        source_extended  = it_source
      EXCEPTIONS
        cancelled        = 1
        permission_error = 2
        not_found        = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      Lcl_abapgit_language=>restore_login_language( ).

      IF sy-msgid = 'EU' AND sy-msgno = '510'.
        Lcx_abapgit_exception=>raise( 'User is currently editing program' ).
      ELSEIF sy-msgid = 'EU' AND sy-msgno = '522'.
        " for generated table maintenance function groups, the author is set to SAP* instead of the user which
        " generates the function group. This hits some standard checks, pulling new code again sets the author
        " to the current user which avoids the check
        Lcx_abapgit_exception=>raise( |Delete function group and pull again, { is_progdir-name } (EU522)| ).
      ELSE.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

    Lcl_abapgit_language=>restore_login_language( ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECTS_PROGRAM implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_COMMON_AFF <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_common_aff=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_common_aff=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_object_common_aff=ccau.

*CLASS zcl_abapgit_object_common_aff DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KW4FSFY.

class LCL_ABAPGIT_OBJECT_COMMON_AFF implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    DATA:
      lv_is_supported TYPE abap_bool,
      li_aff_registry TYPE REF TO Lif_abapgit_aff_registry,
      lo_handler      TYPE REF TO object.

    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).

    " Check if AFF handler exists and if object type is registered and supported
    TRY.
        lo_handler = get_object_handler( ).

        CREATE OBJECT li_aff_registry TYPE Lcl_abapgit_aff_registry.

        lv_is_supported = li_aff_registry->is_supported_object_type( is_item-obj_type ).
      CATCH cx_root.
        lv_is_supported = abap_false.
    ENDTRY.

    IF lv_is_supported IS INITIAL.
      Lcx_abapgit_exception=>raise( |Object type { is_item-obj_type } is not supported by this system| ).
    ENDIF.

  ENDMETHOD.
  METHOD get_additional_extensions.
    RETURN.
  ENDMETHOD.
  METHOD get_object_handler.

    DATA lo_handler_factory TYPE REF TO object.

    CREATE OBJECT lo_handler_factory TYPE ('CL_AFF_OBJECT_HANDLER_FACTORY').

    CALL METHOD lo_handler_factory->('IF_AFF_OBJECT_HANDLER_FACTORY~GET_OBJECT_HANDLER')
      EXPORTING
        object_type = ms_item-obj_type
      RECEIVING
        result      = ro_object_handler.

  ENDMETHOD.
  METHOD is_file_empty.

    CALL METHOD io_object_json_file->('IF_AFF_FILE~IS_DELETION')
      RECEIVING
        result = rv_is_empty.

  ENDMETHOD.
  METHOD remove_abap_language_version.
    DATA lv_json TYPE string.
    DATA lv_json_wo_alv TYPE string.
    DATA li_json TYPE REF TO Lif_abapgit_ajson.

    lv_json = Lcl_abapgit_convert=>xstring_to_string_utf8( iv_json_as_xstring ).

    TRY.
        li_json = Lcl_abapgit_ajson=>parse( iv_json            = lv_json
                                            iv_keep_item_order = abap_true ).
        li_json->delete( '/header/abapLanguageVersion' ).
        lv_json_wo_alv = li_json->stringify( 2 ).

        rv_json_as_xstring_wo_alv = Lcl_abapgit_convert=>string_to_xstring_utf8( lv_json_wo_alv ).

      CATCH Lcx_abapgit_ajson_error.
        rv_json_as_xstring_wo_alv = iv_json_as_xstring.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lr_intf_aff_obj   TYPE REF TO data,
          lr_intf_aff_log   TYPE REF TO data,
          lr_messages       TYPE REF TO data,
          lo_object_handler TYPE REF TO object,
          lo_object_aff     TYPE REF TO object,
          lo_aff_factory    TYPE REF TO object,
          lv_name           TYPE c LENGTH 120,
          lx_error          TYPE REF TO cx_root,
          lo_aff_log        TYPE REF TO object.

    FIELD-SYMBOLS: <ls_intf_aff_obj> TYPE any,
                   <ls_intf_aff_log> TYPE any,
                   <ls_messages>     TYPE ANY TABLE,
                   <ls_message>      TYPE any,
                   <ls_msg>          TYPE symsg.

    lv_name = ms_item-obj_name.

    TRY.
        lo_object_handler = get_object_handler( ).

        CREATE OBJECT lo_object_aff TYPE ('CL_AFF_OBJ')
           EXPORTING
             package = iv_package
             name    = lv_name
             type    = ms_item-obj_type.

        CREATE DATA lr_intf_aff_obj TYPE REF TO ('IF_AFF_OBJ').
        ASSIGN lr_intf_aff_obj->* TO <ls_intf_aff_obj>.
        <ls_intf_aff_obj> ?= lo_object_aff.

        CREATE OBJECT lo_aff_factory TYPE ('CL_AFF_FACTORY').
        CALL METHOD lo_aff_factory->('CREATE_LOG')
          RECEIVING
            result = lo_aff_log.

        CREATE DATA lr_intf_aff_log TYPE REF TO ('IF_AFF_LOG').
        ASSIGN lr_intf_aff_log->* TO <ls_intf_aff_log>.
        <ls_intf_aff_log> ?= lo_aff_log.

        CALL METHOD lo_object_handler->('IF_AFF_OBJECT_HANDLER~DELETE')
          EXPORTING
            object = <ls_intf_aff_obj>
            log    = <ls_intf_aff_log>.

        CREATE DATA lr_messages TYPE ('IF_AFF_LOG=>TT_LOG_OUT').
        ASSIGN lr_messages->* TO <ls_messages>.

        CALL METHOD lo_aff_log->('IF_AFF_LOG~GET_MESSAGES')
          RECEIVING
            messages = <ls_messages>.

        LOOP AT <ls_messages> ASSIGNING <ls_message>.
          ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE <ls_message> TO <ls_msg>.
          IF <ls_msg>-msgty = 'E'.
            Lcx_abapgit_exception=>raise_t100(
              iv_msgid = <ls_msg>-msgid
              iv_msgno = <ls_msg>-msgno
              iv_msgv1 = <ls_msg>-msgv1
              iv_msgv2 = <ls_msg>-msgv2
              iv_msgv3 = <ls_msg>-msgv3
              iv_msgv4 = <ls_msg>-msgv4 ).
          ENDIF.
        ENDLOOP.

        tadir_delete( ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.
  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lr_intf_aff_obj          TYPE REF TO data,
          lr_intf_aff_file         TYPE REF TO data,
          lr_intf_files_container  TYPE REF TO data,
          lr_intf_aff_log          TYPE REF TO data,
          lr_intf_aff_settings     TYPE REF TO data,
          lo_object_handler        TYPE REF TO object,
          lo_object_aff            TYPE REF TO object,
          lo_object_json_file      TYPE REF TO object,
          lo_object_file           TYPE REF TO object,
          lo_files_container       TYPE REF TO object,
          lo_settings              TYPE REF TO object,
          lo_aff_log               TYPE REF TO object,
          lo_aff_factory           TYPE REF TO object,
          lr_messages              TYPE REF TO data,
          lv_json_as_xstring       TYPE xstring,
          lx_exception             TYPE REF TO cx_root,
          lv_file_name             TYPE string,
          lo_file_name_mapper      TYPE REF TO object,
          lv_name                  TYPE c LENGTH 120,
          lv_file_as_xstring       TYPE xstring,
          ls_additional_extensions TYPE ty_extension_mapper_pairs.

    FIELD-SYMBOLS: <ls_intf_aff_obj>          TYPE any,
                   <ls_intf_aff_file>         TYPE any,
                   <ls_intf_files_container>  TYPE any,
                   <ls_intf_aff_log>          TYPE any,
                   <ls_intf_aff_settings>     TYPE any,
                   <ls_messages>              TYPE ANY TABLE,
                   <ls_message>               TYPE any,
                   <ls_text>                  TYPE any,
                   <ls_type>                  TYPE any,
                   <ls_msg>                   TYPE symsg,
                   <ls_extension_mapper_pair> LIKE LINE OF ls_additional_extensions.

    lv_json_as_xstring = Lif_abapgit_object~mo_files->read_raw( 'json' ).
    lv_name = ms_item-obj_name.

    " beyond here there will be dragons....
    TRY.
        lo_object_handler = get_object_handler( ).

        CREATE OBJECT lo_object_aff TYPE ('CL_AFF_OBJ')
          EXPORTING
            package = ms_item-devclass
            name    = lv_name
            type    = ms_item-obj_type.

        CREATE DATA lr_intf_aff_obj TYPE REF TO ('IF_AFF_OBJ').
        ASSIGN lr_intf_aff_obj->* TO <ls_intf_aff_obj>.
        <ls_intf_aff_obj> ?= lo_object_aff.

        CREATE OBJECT lo_files_container TYPE ('CL_AFF_FILES_CONTAINER')
          EXPORTING
            object = <ls_intf_aff_obj>.

        CALL METHOD ('CL_AFF_FILE_NAME_MAPPER')=>for_json
          RECEIVING
            result = lo_file_name_mapper.

        CALL METHOD lo_file_name_mapper->('IF_AFF_FILE_NAME_MAPPER~GET_FILE_NAME_FROM_OBJECT')
          EXPORTING
            object = <ls_intf_aff_obj>
          RECEIVING
            result = lv_file_name.


        CREATE OBJECT lo_settings TYPE ('CL_AFF_SETTINGS_DESERIALIZE')
          EXPORTING
            version               = 'A'
            language              = mv_language
            user                  = sy-uname
            abap_language_version = ms_item-abap_language_version.

        CREATE OBJECT lo_object_json_file TYPE ('CL_AFF_FILE')
          EXPORTING
            name    = lv_file_name
            content = lv_json_as_xstring.

        CREATE DATA lr_intf_aff_file TYPE REF TO ('IF_AFF_FILE').
        ASSIGN lr_intf_aff_file->* TO <ls_intf_aff_file>.
        <ls_intf_aff_file> ?= lo_object_json_file.

        CALL METHOD lo_files_container->('ADD_FILE')
          EXPORTING
            file = <ls_intf_aff_file>.

        ls_additional_extensions = get_additional_extensions( ).

        LOOP AT ls_additional_extensions ASSIGNING <ls_extension_mapper_pair>.

          lv_file_as_xstring = Lif_abapgit_object~mo_files->read_raw( <ls_extension_mapper_pair>-extension ).

          CALL METHOD <ls_extension_mapper_pair>-file_name_mapper->('IF_AFF_FILE_NAME_MAPPER~GET_FILE_NAME_FROM_OBJECT')
            EXPORTING
              object = <ls_intf_aff_obj>
            RECEIVING
              result = lv_file_name.

          CREATE OBJECT lo_object_file TYPE ('CL_AFF_FILE')
          EXPORTING
            name    = lv_file_name
            content = lv_file_as_xstring.

          CREATE DATA lr_intf_aff_file TYPE REF TO ('IF_AFF_FILE').
          ASSIGN lr_intf_aff_file->* TO <ls_intf_aff_file>.
          <ls_intf_aff_file> ?= lo_object_file.

          CALL METHOD lo_files_container->('ADD_FILE')
            EXPORTING
              file = <ls_intf_aff_file>.

        ENDLOOP.

        CREATE OBJECT lo_aff_factory TYPE ('CL_AFF_FACTORY').
        CALL METHOD lo_aff_factory->('CREATE_LOG')
          RECEIVING
            result = lo_aff_log.

        CREATE DATA lr_intf_files_container TYPE REF TO ('IF_AFF_FILES_CONTAINER').
        ASSIGN lr_intf_files_container->* TO <ls_intf_files_container>.
        <ls_intf_files_container> ?= lo_files_container.

        CREATE DATA lr_intf_aff_log TYPE REF TO ('IF_AFF_LOG').
        ASSIGN lr_intf_aff_log->* TO <ls_intf_aff_log>.
        <ls_intf_aff_log> ?= lo_aff_log.

        CREATE DATA lr_intf_aff_settings TYPE REF TO ('IF_AFF_SETTINGS_DESERIALIZE').
        ASSIGN lr_intf_aff_settings->* TO <ls_intf_aff_settings>.
        <ls_intf_aff_settings> ?= lo_settings.

        CALL METHOD lo_object_handler->('IF_AFF_OBJECT_HANDLER~DESERIALIZE')
          EXPORTING
            files_container = <ls_intf_files_container>
            log             = <ls_intf_aff_log>
            settings        = <ls_intf_aff_settings>.

        CREATE DATA lr_messages TYPE ('IF_AFF_LOG=>TT_LOG_OUT').
        ASSIGN lr_messages->* TO <ls_messages>.

        CALL METHOD lo_aff_log->('IF_AFF_LOG~GET_MESSAGES')
          RECEIVING
            messages = <ls_messages>.

        LOOP AT <ls_messages> ASSIGNING <ls_message>.
          ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE <ls_message> TO <ls_msg>.
          ASSIGN COMPONENT 'TEXT' OF STRUCTURE <ls_message> TO <ls_text>.
          ASSIGN COMPONENT 'TYPE' OF STRUCTURE <ls_message> TO <ls_type>.
          ii_log->add(
            iv_msg  = <ls_text>
            iv_type = <ls_type>
            is_item = ms_item ).

          IF <ls_msg>-msgty = 'E'.
            Lcx_abapgit_exception=>raise_t100(
              iv_msgid = <ls_msg>-msgid
              iv_msgno = <ls_msg>-msgno
              iv_msgv1 = <ls_msg>-msgv1
              iv_msgv2 = <ls_msg>-msgv2
              iv_msgv3 = <ls_msg>-msgv3
              iv_msgv4 = <ls_msg>-msgv4 ).
          ENDIF.
        ENDLOOP.

        IF is_active( ) = abap_false.
          " as DDIC-object e.g. are not deserialized in active state, activation must be performed
          Lcl_abapgit_objects_activation=>add_item( ms_item ).
        ENDIF.

        tadir_insert( ms_item-devclass ).

      CATCH cx_root INTO lx_exception.
        ii_log->add_error( is_item = ms_item
                           iv_msg  = 'Error at deserialize' ).
        ii_log->add_exception(
          ix_exc  = lx_exception
          is_item = ms_item ).
    ENDTRY.
  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.
    DATA: lr_intf_aff_obj   TYPE REF TO data,
          lo_object_handler TYPE REF TO object,
          lo_object_aff     TYPE REF TO object,
          lv_name           TYPE c LENGTH 120,
          lx_error          TYPE REF TO cx_root.

    FIELD-SYMBOLS: <ls_intf_aff_obj> TYPE any.

    lv_name = ms_item-obj_name.

    TRY.
        lo_object_handler = get_object_handler( ).

        CREATE OBJECT lo_object_aff TYPE ('CL_AFF_OBJ')
           EXPORTING
             package = ms_item-devclass
             name    = lv_name
             type    = ms_item-obj_type.

        CREATE DATA lr_intf_aff_obj TYPE REF TO ('IF_AFF_OBJ').
        ASSIGN lr_intf_aff_obj->* TO <ls_intf_aff_obj>.
        <ls_intf_aff_obj> ?= lo_object_aff.

        CALL METHOD lo_object_handler->('IF_AFF_OBJECT_HANDLER~EXISTS')
          EXPORTING
            object = <ls_intf_aff_obj>
          RECEIVING
            result = rv_bool.

      CATCH cx_root INTO lx_error.
        " return false instead of raising exception, because abapGit assumes
        " that raising exception = existing object
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
    DATA lv_lock_object   TYPE string.
    DATA lv_argument      TYPE seqg3-garg.

    lv_lock_object = |{ ms_item-obj_type }{ ms_item-obj_name }*|.
    lv_argument  = lv_lock_object.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESWB_EO'
                                            iv_argument    = lv_argument ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.
    DATA: lr_intf_aff_obj           TYPE REF TO data,
          lr_intf_aff_log           TYPE REF TO data,
          lr_intf_aff_settings      TYPE REF TO data,
          lr_messages               TYPE REF TO data,
          lo_object_handler         TYPE REF TO object,
          lo_object_aff             TYPE REF TO object,
          lo_object_json_file       TYPE REF TO object,
          lo_files_container        TYPE REF TO object,
          lo_settings               TYPE REF TO object,
          lo_aff_log                TYPE REF TO object,
          lo_aff_factory            TYPE REF TO object,
          lo_object_file            TYPE REF TO object,
          lv_json_as_xstring        TYPE xstring,
          lv_json_as_xstring_wo_alv TYPE xstring,
          lx_exception              TYPE REF TO cx_root,
          lv_name                   TYPE c LENGTH 120,
          lv_file_name              TYPE string,
          lo_file_name_mapper       TYPE REF TO object,
          ls_additional_extensions  TYPE ty_extension_mapper_pairs,
          lv_file_as_xstring        TYPE xstring.

    FIELD-SYMBOLS: <ls_intf_aff_obj>          TYPE any,
                   <ls_intf_aff_log>          TYPE any,
                   <ls_intf_aff_settings>     TYPE any,
                   <ls_messages>              TYPE ANY TABLE,
                   <ls_message>               TYPE any,
                   <ls_msg>                   TYPE symsg,
                   <ls_extension_mapper_pair> LIKE LINE OF ls_additional_extensions.

    lv_name = ms_item-obj_name.

    TRY.
        lo_object_handler = get_object_handler( ).

        CREATE OBJECT lo_object_aff TYPE ('CL_AFF_OBJ')
           EXPORTING
             package = ms_item-devclass
             name    = lv_name
             type    = ms_item-obj_type.

        CREATE OBJECT lo_settings TYPE ('CL_AFF_SETTINGS_SERIALIZE')
          EXPORTING
            version = 'A'
            language = mv_language.

        CREATE OBJECT lo_aff_factory TYPE ('CL_AFF_FACTORY').
        CALL METHOD lo_aff_factory->('CREATE_LOG')
          RECEIVING
            result = lo_aff_log.

        CREATE DATA lr_intf_aff_log TYPE REF TO ('IF_AFF_LOG').
        ASSIGN lr_intf_aff_log->* TO <ls_intf_aff_log>.
        <ls_intf_aff_log> ?= lo_aff_log.

        CREATE DATA lr_intf_aff_settings TYPE REF TO ('IF_AFF_SETTINGS_SERIALIZE').
        ASSIGN lr_intf_aff_settings->* TO <ls_intf_aff_settings>.
        <ls_intf_aff_settings> ?= lo_settings.

        CREATE DATA lr_intf_aff_obj TYPE REF TO ('IF_AFF_OBJ').
        ASSIGN lr_intf_aff_obj->* TO <ls_intf_aff_obj>.
        <ls_intf_aff_obj> ?= lo_object_aff.

        CALL METHOD lo_object_handler->('IF_AFF_OBJECT_HANDLER~SERIALIZE')
          EXPORTING
            object   = <ls_intf_aff_obj>
            log      = <ls_intf_aff_log>
            settings = <ls_intf_aff_settings>
          RECEIVING
            result   = lo_files_container.

        CREATE DATA lr_messages TYPE ('IF_AFF_LOG=>TT_LOG_OUT').
        ASSIGN lr_messages->* TO <ls_messages>.

        CALL METHOD lo_aff_log->('IF_AFF_LOG~GET_MESSAGES')
          RECEIVING
            messages = <ls_messages>.

        LOOP AT <ls_messages> ASSIGNING <ls_message>.
          ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE <ls_message> TO <ls_msg>.
          IF <ls_msg>-msgty = 'E'.
            Lcx_abapgit_exception=>raise_t100(
              iv_msgid = <ls_msg>-msgid
              iv_msgno = <ls_msg>-msgno
              iv_msgv1 = <ls_msg>-msgv1
              iv_msgv2 = <ls_msg>-msgv2
              iv_msgv3 = <ls_msg>-msgv3
              iv_msgv4 = <ls_msg>-msgv4 ).
          ENDIF.
        ENDLOOP.

        CALL METHOD ('CL_AFF_FILE_NAME_MAPPER')=>for_json
          RECEIVING
            result = lo_file_name_mapper.

        CALL METHOD lo_file_name_mapper->('IF_AFF_FILE_NAME_MAPPER~GET_FILE_NAME_FROM_OBJECT')
          EXPORTING
            object = <ls_intf_aff_obj>
          RECEIVING
            result = lv_file_name.

        CALL METHOD lo_files_container->('IF_AFF_FILES_CONTAINER~GET_FILE')
          EXPORTING
            name   = lv_file_name
          RECEIVING
            result = lo_object_json_file.

        " avoid to serialize empty content (object was never activated, exists inactive only).
        IF is_file_empty( lo_object_json_file ) = abap_true.
          MESSAGE s821(eu) WITH lv_name INTO Lcx_abapgit_exception=>null.
          Lcx_abapgit_exception=>raise_t100( ).
        ENDIF.

        CALL METHOD lo_object_json_file->('IF_AFF_FILE~GET_CONTENT')
          RECEIVING
            result = lv_json_as_xstring.

        lv_json_as_xstring_wo_alv = remove_abap_language_version( lv_json_as_xstring ).

        Lif_abapgit_object~mo_files->add_raw(
          iv_ext  = 'json'
          iv_data = lv_json_as_xstring_wo_alv ).

        ls_additional_extensions = get_additional_extensions( ).

        LOOP AT ls_additional_extensions ASSIGNING <ls_extension_mapper_pair>.

          CALL METHOD <ls_extension_mapper_pair>-file_name_mapper->('IF_AFF_FILE_NAME_MAPPER~GET_FILE_NAME_FROM_OBJECT')
            EXPORTING
              object = <ls_intf_aff_obj>
            RECEIVING
              result = lv_file_name.

          CALL METHOD lo_files_container->('IF_AFF_FILES_CONTAINER~GET_FILE')
            EXPORTING
              name   = lv_file_name
            RECEIVING
              result = lo_object_file.

          CALL METHOD lo_object_file->('IF_AFF_FILE~GET_CONTENT')
            RECEIVING
              result = lv_file_as_xstring.

          Lif_abapgit_object~mo_files->add_raw(
            iv_ext  = <ls_extension_mapper_pair>-extension
            iv_data = lv_file_as_xstring ).

        ENDLOOP.

      CATCH cx_root INTO lx_exception.
        Lcx_abapgit_exception=>raise_with_text( lx_exception ).
    ENDTRY.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_COMMON_AFF implementation

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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENHC implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_PDTS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_pdts=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_pdts=======ccimp.
CLASS SHRITEFUH64VYIPO5IWUYB3KW4XSFY DEFINITION
  INHERITING FROM cl_workflow_general_task_def
  CREATE PUBLIC
  FINAL.
  PUBLIC SECTION.

    CLASS-METHODS set_objid IMPORTING iv_objid TYPE hrobject-objid
                                      io_task  TYPE REF TO cl_workflow_general_task_def.

    CLASS-METHODS set_container_id IMPORTING iv_id   TYPE guid_32
                                             io_task TYPE REF TO cl_workflow_general_task_def. "#EC NEEDED
ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KW4XSFY IMPLEMENTATION.

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


CLASS SHRITEFUH64VYIPO5IWUYB3KW4ZSFY DEFINITION
  CREATE PUBLIC
  FINAL.

  PUBLIC SECTION.

    INTERFACES SHRITEFUH64VYIPO5IWUYB3KW4WSFY.

    CLASS-METHODS load IMPORTING iv_objid         TYPE hrobject-objid
                       RETURNING VALUE(ri_result) TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KW4WSFY
                       RAISING   Lcx_abapgit_exception.

    CLASS-METHODS create IMPORTING iv_objid         TYPE hrobject-objid
                                   is_task_data     TYPE SHRITEFUH64VYIPO5IWUYB3KW4WSFY=>ty_task_data
                         RETURNING VALUE(ri_result) TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KW4WSFY
                         RAISING   Lcx_abapgit_exception.


  PRIVATE SECTION.
    CONSTANTS c_subty_task_description TYPE hr_s_subty VALUE '0120'.

    DATA mo_taskdef TYPE REF TO cl_workflow_task_ts.
    DATA ms_task TYPE SHRITEFUH64VYIPO5IWUYB3KW4WSFY=>ty_task_data.

    DATA: mv_objid                      TYPE hrobjid.

    METHODS supply_instance RAISING Lcx_abapgit_exception.
    METHODS check_subrc_for IMPORTING iv_call TYPE clike OPTIONAL
                            RAISING   Lcx_abapgit_exception.

ENDCLASS.


CLASS SHRITEFUH64VYIPO5IWUYB3KW4ZSFY IMPLEMENTATION.

  METHOD load.

    DATA lo_taskdef TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KW4ZSFY.

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

  METHOD SHRITEFUH64VYIPO5IWUYB3KW4WSFY~clear_origin_data.

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

  METHOD SHRITEFUH64VYIPO5IWUYB3KW4WSFY~get_definition.
    rs_result = me->ms_task.
  ENDMETHOD.

  METHOD SHRITEFUH64VYIPO5IWUYB3KW4WSFY~get_container.
    ri_result = mo_taskdef->container.
  ENDMETHOD.

  METHOD SHRITEFUH64VYIPO5IWUYB3KW4WSFY~get_user_container.

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
    DATA lo_task TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KW4ZSFY.

    CREATE OBJECT lo_task TYPE SHRITEFUH64VYIPO5IWUYB3KW4ZSFY.
    lo_task->mv_objid = iv_objid.
    lo_task->ms_task = is_task_data.
    ri_result = lo_task.

  ENDMETHOD.


  METHOD SHRITEFUH64VYIPO5IWUYB3KW4WSFY~import_container.

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

  METHOD SHRITEFUH64VYIPO5IWUYB3KW4WSFY~create_task.

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

    SHRITEFUH64VYIPO5IWUYB3KW4XSFY=>set_objid( iv_objid = mv_objid
                                           io_task  = mo_taskdef ).

    SHRITEFUH64VYIPO5IWUYB3KW4XSFY=>set_container_id( iv_id    = |TS{ mv_objid }|
                                            io_task  = mo_taskdef ).

  ENDMETHOD.

  METHOD SHRITEFUH64VYIPO5IWUYB3KW4WSFY~change_start_events.

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


  METHOD SHRITEFUH64VYIPO5IWUYB3KW4WSFY~save.

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


  METHOD SHRITEFUH64VYIPO5IWUYB3KW4WSFY~change_wi_text.

    mo_taskdef->change_wi_text(
      EXPORTING
        new_wi_text        = ms_task-wi_text
      EXCEPTIONS
        no_changes_allowed = 1
        OTHERS             = 2 ).                         "#EC SUBRC_OK

    check_subrc_for( `CHANGE_WI_TEXT` ).

  ENDMETHOD.


  METHOD SHRITEFUH64VYIPO5IWUYB3KW4WSFY~change_method.

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


  METHOD SHRITEFUH64VYIPO5IWUYB3KW4WSFY~change_terminating_events.

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


  METHOD SHRITEFUH64VYIPO5IWUYB3KW4WSFY~change_text.

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

    DATA: ls_task       TYPE SHRITEFUH64VYIPO5IWUYB3KW4WSFY=>ty_task_data,
          lv_xml_string TYPE xstring,
          li_task       TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KW4WSFY.

    io_xml->read( EXPORTING iv_name = 'PDTS'
      CHANGING cg_data = ls_task ).

    li_task = SHRITEFUH64VYIPO5IWUYB3KW4ZSFY=>create(
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

    DATA li_task TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KW4WSFY.

    li_task = SHRITEFUH64VYIPO5IWUYB3KW4ZSFY=>load( mv_objid ).
    li_task->clear_origin_data( ).
    io_xml->add( iv_name = 'PDTS'
                 ig_data = li_task->get_definition( ) ).

    io_xml->add_xml( iv_name = 'CONTAINER'
                     ii_xml  = get_container_xml( li_task ) ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_PDTS implementation

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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SHMA implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SUSC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_susc=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_susc=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SUSC implementation.
*"* method's implementations
*include methods.
  METHOD delete_class.

    DELETE FROM tobc  WHERE oclss = iv_auth_object_class.
    DELETE FROM tobct WHERE oclss = iv_auth_object_class.

  ENDMETHOD.
  METHOD has_authorization.

    AUTHORITY-CHECK OBJECT 'S_DEVELOP'
           ID 'DEVCLASS' DUMMY
           ID 'OBJTYPE' FIELD 'SUSC'
           ID 'OBJNAME' FIELD iv_class
           ID 'P_GROUP' DUMMY
           ID 'ACTVT'   FIELD iv_activity.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( iv_msgid = '01'
                                         iv_msgno = '467' ).
    ENDIF.

  ENDMETHOD.
  METHOD is_used.

    DATA: lv_used_auth_object_class TYPE tobc-oclss.

    SELECT SINGLE oclss
      FROM tobj
      INTO lv_used_auth_object_class
      WHERE oclss = iv_auth_object_class ##WARN_OK.
    IF sy-subrc = 0.
      Lcx_abapgit_exception=>raise_t100( iv_msgid = '01'
                                         iv_msgno = '212'
                                         iv_msgv1 = |{ iv_auth_object_class }| ).
    ENDIF.

  ENDMETHOD.
  METHOD put_delete_to_transport.

    DATA: lv_tr_object_name TYPE e071-obj_name,
          lv_tr_return      TYPE char1,
          ls_package_info   TYPE tdevc.


    lv_tr_object_name = ms_item-obj_name.

    CALL FUNCTION 'SUSR_COMMEDITCHECK'
      EXPORTING
        objectname       = lv_tr_object_name
        transobjecttype  = c_transobjecttype_class
      IMPORTING
        return_from_korr = lv_tr_return.

    IF lv_tr_return <> 'M'.
      Lcx_abapgit_exception=>raise( |error in SUSC delete at SUSR_COMMEDITCHECK| ).
    ENDIF.

    CALL FUNCTION 'TR_DEVCLASS_GET'
      EXPORTING
        iv_devclass = ms_item-devclass
      IMPORTING
        es_tdevc    = ls_package_info
      EXCEPTIONS
        OTHERS      = 1.
    IF sy-subrc = 0 AND ls_package_info-korrflag IS INITIAL.
      tadir_delete( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    rv_user = c_user_unknown. " not stored by SAP
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    CONSTANTS lc_activity_delete_06 TYPE activ_auth VALUE '06'.

    DATA: lv_auth_object_class TYPE tobc-oclss.


    lv_auth_object_class = ms_item-obj_name.

    TRY.
        IF Lif_abapgit_object~exists( ) = abap_false.
          RETURN.
        ENDIF.
      CATCH Lcx_abapgit_exception.
        RETURN.
    ENDTRY.

    has_authorization( iv_class    = lv_auth_object_class
                       iv_activity = lc_activity_delete_06 ).

    is_used( lv_auth_object_class ).

    delete_class( lv_auth_object_class ).

    put_delete_to_transport( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.
* see function group SUSA

    DATA: ls_tobc       TYPE tobc,
          lv_objectname TYPE e071-obj_name,
          ls_tobct      TYPE tobct.


    io_xml->read( EXPORTING iv_name = 'TOBC'
                  CHANGING cg_data = ls_tobc ).
    io_xml->read( EXPORTING iv_name = 'TOBCT'
                  CHANGING cg_data = ls_tobct ).

    tadir_insert( iv_package ).

    lv_objectname = ms_item-obj_name.
    CALL FUNCTION 'SUSR_COMMEDITCHECK'
      EXPORTING
        objectname      = lv_objectname
        transobjecttype = c_transobjecttype_class.

    INSERT tobc FROM ls_tobc.                             "#EC CI_SUBRC
* ignore sy-subrc as all fields are key fields

    MODIFY tobct FROM ls_tobct.                           "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_oclss TYPE tobc-oclss.


    SELECT SINGLE oclss FROM tobc INTO lv_oclss
      WHERE oclss = ms_item-obj_name.
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

    DATA: lv_objclass TYPE tobc-oclss.

    lv_objclass = ms_item-obj_name.
    CALL FUNCTION 'SUSR_SHOW_OBJECT_CLASS'
      EXPORTING
        objclass = lv_objclass.

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_tobc  TYPE tobc,
          ls_tobct TYPE tobct.


    SELECT SINGLE * FROM tobc INTO ls_tobc
      WHERE oclss = ms_item-obj_name.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM tobct INTO ls_tobct
      WHERE oclss = ms_item-obj_name
      AND langu = mv_language.

    io_xml->add( iv_name = 'TOBC'
                 ig_data = ls_tobc ).
    io_xml->add( iv_name = 'TOBCT'
                 ig_data = ls_tobct ).

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
endclass. "ZCL_ABAPGIT_OBJECT_SUSC implementation

*>>>>>>> ZCL_ABAPGIT_PR_ENUM_GITHUB <<<<<<<*

*"* macro definitions
*include zcl_abapgit_pr_enum_github====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_pr_enum_github====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_PR_ENUM_GITHUB implementation.
*"* method's implementations
*include methods.
  METHOD clean_url.
    rv_url = replace(
      val = iv_url
      regex = '\{.*\}$'
      with = '' ).
  ENDMETHOD.
  METHOD constructor.

    DATA lv_search TYPE string.

    mv_repo_url   = |https://api.github.com/repos/{ iv_user_and_repo }|.
    mi_http_agent = ii_http_agent.
    mi_http_agent->global_headers( )->set(
      iv_key = 'Accept'
      iv_val = 'application/vnd.github.v3+json' ).

    IF Lcl_abapgit_login_manager=>get( mv_repo_url ) IS NOT INITIAL.
      mi_http_agent->global_headers( )->set(
        iv_key = 'Authorization'
        iv_val = Lcl_abapgit_login_manager=>get( mv_repo_url ) ).
    ELSE.
* fallback, try searching for the git credentials
      lv_search = mv_repo_url.
      REPLACE FIRST OCCURRENCE OF 'api.github.com/repos' IN lv_search WITH 'github.com'.
      IF Lcl_abapgit_login_manager=>get( lv_search ) IS NOT INITIAL.
        mi_http_agent->global_headers( )->set(
          iv_key = 'Authorization'
          iv_val = Lcl_abapgit_login_manager=>get( lv_search ) ).
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD convert_list.

    DATA lt_items TYPE string_table.
    DATA lv_i TYPE string.
    FIELD-SYMBOLS <ls_p> LIKE LINE OF rt_pulls.

    lt_items = ii_json->members( '/' ).

    LOOP AT lt_items INTO lv_i.
      APPEND INITIAL LINE TO rt_pulls ASSIGNING <ls_p>.
      <ls_p>-base_url        = ii_json->get( |/{ lv_i }/base/repo/clone_url| ).
      <ls_p>-number          = ii_json->get( |/{ lv_i }/number| ).
      <ls_p>-title           = ii_json->get( |/{ lv_i }/title| ).
      <ls_p>-user            = ii_json->get( |/{ lv_i }/user/login| ).
      <ls_p>-head_url        = ii_json->get( |/{ lv_i }/head/repo/clone_url| ).
      <ls_p>-head_branch     = ii_json->get( |/{ lv_i }/head/ref| ).
      <ls_p>-created_at      = ii_json->get( |/{ lv_i }/created_at| ).
      <ls_p>-draft           = ii_json->get_boolean( |/{ lv_i }/draft| ).
      <ls_p>-html_url        = ii_json->get( |/{ lv_i }/html_url| ).
    ENDLOOP.

  ENDMETHOD.
  METHOD fetch_repo_by_url.

    DATA li_pulls_json TYPE REF TO Lif_abapgit_ajson.
    DATA lv_pull_url TYPE string.
    DATA li_response TYPE REF TO Lif_abapgit_http_response.
    DATA lx_ajson TYPE REF TO Lcx_abapgit_ajson_error.

    li_response = mi_http_agent->request( iv_repo_url ).

    TRY.
        rs_info-repo_json = li_response->json( ).
        li_response->headers( ). " for debug
        lv_pull_url = clean_url( rs_info-repo_json->get( '/pulls_url' ) ).
        IF lv_pull_url IS INITIAL OR rs_info-repo_json->get( '/message' ) = 'Not Found'.
          RETURN.
        ENDIF.
        li_pulls_json = mi_http_agent->request( lv_pull_url )->json( ).
      CATCH Lcx_abapgit_ajson_error INTO lx_ajson.
        Lcx_abapgit_exception=>raise_with_text( lx_ajson ).
    ENDTRY.

    rs_info-pulls = convert_list( li_pulls_json ).

  ENDMETHOD.
  METHOD Lif_abapgit_pr_enum_provider~list_pull_requests.

    DATA lv_upstream_url TYPE string.
    DATA ls_repo_info TYPE ty_info.
    FIELD-SYMBOLS <ls_p> LIKE LINE OF ls_repo_info-pulls.

    ls_repo_info = fetch_repo_by_url( mv_repo_url ).
    APPEND LINES OF ls_repo_info-pulls TO rt_pulls.

    IF ls_repo_info-repo_json->get_boolean( '/fork' ) = abap_true.
      lv_upstream_url = ls_repo_info-repo_json->get( '/source/url' ). " parent ?
      ls_repo_info = fetch_repo_by_url( lv_upstream_url ).
      LOOP AT ls_repo_info-pulls ASSIGNING <ls_p>.
        <ls_p>-is_for_upstream = abap_true.
        APPEND <ls_p> TO rt_pulls.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_PR_ENUM_GITHUB implementation

*>>>>>>> ZCL_ABAPGIT_SETTINGS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_settings==========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_settings==========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_SETTINGS implementation.
*"* method's implementations
*include methods.
  METHOD get_activate_wo_popup.
    rv_act_wo_popup = ms_user_settings-activate_wo_popup.
  ENDMETHOD.
  METHOD get_adt_jump_enabled.
    rv_adt_jump_enabled = ms_user_settings-adt_jump_enabled.
  ENDMETHOD.
  METHOD get_commitmsg_body_size.
    rv_length = ms_settings-commitmsg_body_size.
  ENDMETHOD.
  METHOD get_commitmsg_comment_default.
    rv_default = ms_settings-commitmsg_comment_deflt.
  ENDMETHOD.
  METHOD get_commitmsg_comment_length.
    rv_length = ms_settings-commitmsg_comment_length.
  ENDMETHOD.
  METHOD get_commitmsg_hide_author.
    rv_hide_author = ms_settings-commitmsg_hide_author.
  ENDMETHOD.
  METHOD get_experimental_features.
    IF Lcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_false.
      rv_features = ms_settings-experimental_features.
    ENDIF.
  ENDMETHOD.
  METHOD get_icon_scaling.
    rv_scaling = ms_user_settings-icon_scaling.
  ENDMETHOD.
  METHOD get_link_hints_enabled.
    rv_link_hints_enabled = ms_user_settings-link_hints_enabled.
  ENDMETHOD.
  METHOD get_link_hint_key.
    rv_link_hint_key = ms_user_settings-link_hint_key.
  ENDMETHOD.
  METHOD get_max_lines.
    rv_lines = ms_user_settings-max_lines.
  ENDMETHOD.
  METHOD get_parallel_proc_disabled.
    rv_disable_parallel_proc = ms_user_settings-parallel_proc_disabled.
  ENDMETHOD.
  METHOD get_proxy_authentication.
    rv_auth = ms_settings-proxy_auth.
  ENDMETHOD.
  METHOD get_proxy_bypass.
    rt_bypass = ms_settings-proxy_bypass.
  ENDMETHOD.
  METHOD get_proxy_port.
    rv_port = ms_settings-proxy_port.
  ENDMETHOD.
  METHOD get_proxy_url.
    rv_proxy_url = ms_settings-proxy_url.
  ENDMETHOD.
  METHOD get_run_critical_tests.
    IF Lcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_false.
      rv_run = ms_settings-run_critical_tests.
    ENDIF.
  ENDMETHOD.
  METHOD get_settings_xml.

    DATA: li_output TYPE REF TO Lif_abapgit_xml_output.


    CREATE OBJECT li_output TYPE Lcl_abapgit_xml_output.

    li_output->add( iv_name = Lcl_abapgit_persistence_db=>c_type_settings
                    ig_data = ms_settings ).

    rv_settings_xml = li_output->render( ).

  ENDMETHOD.
  METHOD get_show_default_repo.
    rv_show_default_repo = ms_user_settings-show_default_repo.
  ENDMETHOD.
  METHOD get_ui_theme.
    DATA lv_frontend_theme TYPE string.
    DATA lv_cl_gui TYPE string.

    lv_cl_gui = 'CL_GUI_RESOURCES'.

    rv_ui_theme = ms_user_settings-ui_theme.

    IF rv_ui_theme = c_ui_theme-synced_with_gui AND iv_resolve_synced = abap_true.
      TRY.
          CALL METHOD (lv_cl_gui)=>get_themename
            IMPORTING
              themename              = lv_frontend_theme
            EXCEPTIONS
              get_std_resource_error = 1
              OTHERS                 = 2.
          IF sy-subrc <> 0.
            rv_ui_theme = c_ui_theme-default.
            RETURN.
          ENDIF.
        CATCH cx_sy_dyn_call_error.
          rv_ui_theme = c_ui_theme-default.
          RETURN.
      ENDTRY.

      CASE lv_frontend_theme.
        WHEN 'Belize'.
          rv_ui_theme = c_ui_theme-belize.
        WHEN OTHERS.
          rv_ui_theme = c_ui_theme-default.
      ENDCASE.
    ENDIF.
  ENDMETHOD.
  METHOD get_user_settings.
    rs_settings = ms_user_settings.
  ENDMETHOD.
  METHOD set_activate_wo_popup.
    ms_user_settings-activate_wo_popup = iv_act_wo_popup.
  ENDMETHOD.
  METHOD set_adt_jump_enanbled.
    ms_user_settings-adt_jump_enabled = iv_adt_jump_enabled.
  ENDMETHOD.
  METHOD set_commitmsg_body_size.
    ms_settings-commitmsg_body_size = iv_length.
  ENDMETHOD.
  METHOD set_commitmsg_comment_default.
    ms_settings-commitmsg_comment_deflt = iv_default.
  ENDMETHOD.
  METHOD set_commitmsg_comment_length.
    ms_settings-commitmsg_comment_length = iv_length.
  ENDMETHOD.
  METHOD set_commitmsg_hide_author.
    ms_settings-commitmsg_hide_author = iv_hide_author.
  ENDMETHOD.
  METHOD set_defaults.

    CLEAR ms_settings.

    set_proxy_authentication( abap_false ).
    set_run_critical_tests( abap_false ).
    set_experimental_features( '' ).
    set_max_lines( 500 ).
    set_adt_jump_enanbled( abap_true ).
    set_show_default_repo( abap_false ).
    set_commitmsg_comment_length( c_commitmsg_comment_length_dft ).
    set_commitmsg_body_size( c_commitmsg_body_size_dft ).
    set_default_link_hint_key( ).
    set_icon_scaling( '' ).

  ENDMETHOD.
  METHOD set_default_link_hint_key.
    " Since #5859 'f' is used for "focus filter", we use 't' as the new default
    set_link_hint_key( |t| ).
  ENDMETHOD.
  METHOD set_experimental_features.
    ms_settings-experimental_features = iv_features.
  ENDMETHOD.
  METHOD set_icon_scaling.
    ms_user_settings-icon_scaling = iv_scaling.
    IF ms_user_settings-icon_scaling NA c_icon_scaling.
      ms_user_settings-icon_scaling = ''. " Reset to default
    ENDIF.
  ENDMETHOD.
  METHOD set_link_hints_enabled.
    ms_user_settings-link_hints_enabled = iv_link_hints_enabled.
  ENDMETHOD.
  METHOD set_link_hint_key.
    ms_user_settings-link_hint_key = iv_link_hint_key.
  ENDMETHOD.
  METHOD set_max_lines.
    ms_user_settings-max_lines = iv_lines.
  ENDMETHOD.
  METHOD set_parallel_proc_disabled.
    ms_user_settings-parallel_proc_disabled = iv_disable_parallel_proc.
  ENDMETHOD.
  METHOD set_proxy_authentication.
    ms_settings-proxy_auth = iv_auth.
  ENDMETHOD.
  METHOD set_proxy_bypass.
    ms_settings-proxy_bypass = it_bypass.
  ENDMETHOD.
  METHOD set_proxy_port.
    ms_settings-proxy_port = iv_port.
  ENDMETHOD.
  METHOD set_proxy_url.
    ms_settings-proxy_url = iv_url.
  ENDMETHOD.
  METHOD set_run_critical_tests.
    ms_settings-run_critical_tests = iv_run.
  ENDMETHOD.
  METHOD set_show_default_repo.
    ms_user_settings-show_default_repo = iv_show_default_repo.
  ENDMETHOD.
  METHOD set_ui_theme.
    ms_user_settings-ui_theme = iv_ui_theme.
    IF ms_user_settings-ui_theme <> c_ui_theme-default
        AND ms_user_settings-ui_theme <> c_ui_theme-dark
        AND ms_user_settings-ui_theme <> c_ui_theme-belize
        AND ms_user_settings-ui_theme <> c_ui_theme-synced_with_gui.
      ms_user_settings-ui_theme = c_ui_theme-default. " Reset to default
    ENDIF.
  ENDMETHOD.
  METHOD set_user_settings.
    ms_user_settings = is_user_settings.

    IF ms_user_settings-link_hint_key IS INITIAL.
      set_default_link_hint_key( ).
    ENDIF.

  ENDMETHOD.
  METHOD set_xml_settings.

    DATA: lo_input TYPE REF TO Lif_abapgit_xml_input.


    CREATE OBJECT lo_input TYPE Lcl_abapgit_xml_input EXPORTING iv_xml = iv_settings_xml.

    CLEAR ms_settings.

    lo_input->read(
      EXPORTING
        iv_name = Lcl_abapgit_persistence_db=>c_type_settings
      CHANGING
        cg_data = ms_settings ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SETTINGS implementation

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

*>>>>>>> ZCL_ABAPGIT_ZLIB <<<<<<<*

*"* macro definitions
*include zcl_abapgit_zlib==============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_zlib==============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_zlib==============ccau.




class LCL_ABAPGIT_ZLIB implementation.
*"* method's implementations
*include methods.
  METHOD copy_out.

* copy one byte at a time, it is not possible to copy using
* string offsets, as it might copy data that does not exist
* in mv_out yet

    DATA: lv_distance TYPE i,
          lv_index    TYPE i,
          lv_x        TYPE x LENGTH 1.


    lv_distance = xstrlen( gv_out ) - is_pair-distance.
    DO is_pair-length TIMES.
      lv_index = sy-index - 1 + lv_distance.
      lv_x = gv_out+lv_index(1).
      CONCATENATE gv_out lv_x INTO gv_out IN BYTE MODE.
    ENDDO.

  ENDMETHOD.
  METHOD decode.

    DATA: lv_bit   TYPE c LENGTH 1,
          lv_len   TYPE i,
          lv_count TYPE i,
          lv_code  TYPE i,
          lv_index TYPE i,
          lv_first TYPE i,
          lv_bits  TYPE string.


    DO Lcl_abapgit_zlib_huffman=>c_maxbits TIMES.
      lv_len = sy-index.

      lv_bit = go_stream->take_bits( 1 ).
      CONCATENATE lv_bits lv_bit INTO lv_bits.
      lv_code = Lcl_abapgit_zlib_convert=>bits_to_int( lv_bits ).
      lv_count = io_huffman->get_count( lv_len ).

      IF lv_code - lv_count < lv_first.
        rv_symbol = io_huffman->get_symbol( lv_index + lv_code - lv_first + 1 ).
        RETURN.
      ENDIF.
      lv_index = lv_index + lv_count.
      lv_first = lv_first + lv_count.
      lv_first = lv_first * 2.
    ENDDO.

  ENDMETHOD.
  METHOD decode_loop.

    DATA lv_x TYPE x.
    DATA lv_symbol TYPE i.

    DO.
      lv_symbol = decode( go_lencode ).

      IF lv_symbol < 256.
        lv_x = lv_symbol.
        CONCATENATE gv_out lv_x INTO gv_out IN BYTE MODE.
      ELSEIF lv_symbol = 256.
        EXIT.
      ELSE.
        copy_out( read_pair( lv_symbol ) ).
      ENDIF.

    ENDDO.

  ENDMETHOD.
  METHOD decompress.

    DATA: lv_bfinal TYPE c LENGTH 1,
          lv_btype  TYPE c LENGTH 2.


    IF iv_compressed IS INITIAL.
      RETURN.
    ENDIF.

    CLEAR gv_out.
    CREATE OBJECT go_stream
      EXPORTING
        iv_data = iv_compressed.

    DO.
      lv_bfinal = go_stream->take_bits( 1 ).

      lv_btype = go_stream->take_bits( 2 ).
      CASE lv_btype.
        WHEN '00'.
          not_compressed( ).
        WHEN '01'.
          fixed( ).
          decode_loop( ).
        WHEN '10'.
          dynamic( ).
          decode_loop( ).
        WHEN OTHERS.
          ASSERT 1 = 0.
      ENDCASE.

      IF lv_bfinal = '1'.
        EXIT.
      ENDIF.

    ENDDO.

    rs_data-raw = gv_out.
    rs_data-compressed_len = xstrlen( iv_compressed ) - go_stream->remaining( ).

  ENDMETHOD.
  METHOD dynamic.

    DATA: lv_nlen    TYPE i,
          lv_ndist   TYPE i,
          lv_ncode   TYPE i,
          lv_index   TYPE i,
          lv_length  TYPE i,
          lv_symbol  TYPE i,
          lt_order   TYPE TABLE OF i,
          lt_lengths TYPE Lcl_abapgit_zlib_huffman=>ty_lengths,
          lt_dists   TYPE Lcl_abapgit_zlib_huffman=>ty_lengths.

    FIELD-SYMBOLS: <lv_length> LIKE LINE OF lt_lengths.


    APPEND 16 TO lt_order.
    APPEND 17 TO lt_order.
    APPEND 18 TO lt_order.
    APPEND 0 TO lt_order.
    APPEND 8 TO lt_order.
    APPEND 7 TO lt_order.
    APPEND 9 TO lt_order.
    APPEND 6 TO lt_order.
    APPEND 10 TO lt_order.
    APPEND 5 TO lt_order.
    APPEND 11 TO lt_order.
    APPEND 4 TO lt_order.
    APPEND 12 TO lt_order.
    APPEND 3 TO lt_order.
    APPEND 13 TO lt_order.
    APPEND 2 TO lt_order.
    APPEND 14 TO lt_order.
    APPEND 1 TO lt_order.
    APPEND 15 TO lt_order.

    lv_nlen = go_stream->take_int( 5 ) + 257.
    lv_ndist = go_stream->take_int( 5 ) + 1.
    lv_ncode = go_stream->take_int( 4 ) + 4.

    DO 19 TIMES.
      APPEND 0 TO lt_lengths.
    ENDDO.

    DO lv_ncode TIMES.
      READ TABLE lt_order INDEX sy-index INTO lv_index.
      ASSERT sy-subrc = 0.
      lv_index = lv_index + 1.
      READ TABLE lt_lengths INDEX lv_index ASSIGNING <lv_length>.
      ASSERT sy-subrc = 0.
      <lv_length> = go_stream->take_int( 3 ).
    ENDDO.

    CREATE OBJECT go_lencode
      EXPORTING
        it_lengths = lt_lengths.

    CLEAR lt_lengths.
    WHILE lines( lt_lengths ) < lv_nlen + lv_ndist.
      lv_symbol = decode( go_lencode ).

      IF lv_symbol < 16.
        APPEND lv_symbol TO lt_lengths.
      ELSE.
        lv_length = 0.
        IF lv_symbol = 16.
          READ TABLE lt_lengths INDEX lines( lt_lengths ) INTO lv_length.
          ASSERT sy-subrc = 0.
          lv_symbol = go_stream->take_int( 2 ) + 3.
        ELSEIF lv_symbol = 17.
          lv_symbol = go_stream->take_int( 3 ) + 3.
        ELSE.
          lv_symbol = go_stream->take_int( 7 ) + 11.
        ENDIF.
        DO lv_symbol TIMES.
          APPEND lv_length TO lt_lengths.
        ENDDO.
      ENDIF.
    ENDWHILE.

    lt_dists = lt_lengths.
    DELETE lt_lengths FROM lv_nlen + 1.
    DELETE lt_dists TO lv_nlen.

    CREATE OBJECT go_lencode
      EXPORTING
        it_lengths = lt_lengths.

    CREATE OBJECT go_distcode
      EXPORTING
        it_lengths = lt_dists.

  ENDMETHOD.
  METHOD fixed.

    DATA: lt_lengths TYPE Lcl_abapgit_zlib_huffman=>ty_lengths.


    DO 144 TIMES.
      APPEND 8 TO lt_lengths.
    ENDDO.
    DO 112 TIMES.
      APPEND 9 TO lt_lengths.
    ENDDO.
    DO 24 TIMES.
      APPEND 7 TO lt_lengths.
    ENDDO.
    DO 8 TIMES.
      APPEND 8 TO lt_lengths.
    ENDDO.

    CREATE OBJECT go_lencode
      EXPORTING
        it_lengths = lt_lengths.

    CLEAR lt_lengths.
    DO c_maxdcodes TIMES.
      APPEND 5 TO lt_lengths.
    ENDDO.

    CREATE OBJECT go_distcode
      EXPORTING
        it_lengths = lt_lengths.

  ENDMETHOD.
  METHOD map_distance.

    CASE iv_code.
      WHEN 0.
        rv_distance = go_stream->take_int( 0 ) + 1.
      WHEN 1.
        rv_distance = go_stream->take_int( 0 ) + 2.
      WHEN 2.
        rv_distance = go_stream->take_int( 0 ) + 3.
      WHEN 3.
        rv_distance = go_stream->take_int( 0 ) + 4.
      WHEN 4.
        rv_distance = go_stream->take_int( 1 ) + 5.
      WHEN 5.
        rv_distance = go_stream->take_int( 1 ) + 7.
      WHEN 6.
        rv_distance = go_stream->take_int( 2 ) + 9.
      WHEN 7.
        rv_distance = go_stream->take_int( 2 ) + 13.
      WHEN 8.
        rv_distance = go_stream->take_int( 3 ) + 17.
      WHEN 9.
        rv_distance = go_stream->take_int( 3 ) + 25.
      WHEN 10.
        rv_distance = go_stream->take_int( 4 ) + 33.
      WHEN 11.
        rv_distance = go_stream->take_int( 4 ) + 49.
      WHEN 12.
        rv_distance = go_stream->take_int( 5 ) + 65.
      WHEN 13.
        rv_distance = go_stream->take_int( 5 ) + 97.
      WHEN 14.
        rv_distance = go_stream->take_int( 6 ) + 129.
      WHEN 15.
        rv_distance = go_stream->take_int( 6 ) + 193.
      WHEN 16.
        rv_distance = go_stream->take_int( 7 ) + 257.
      WHEN 17.
        rv_distance = go_stream->take_int( 7 ) + 385.
      WHEN 18.
        rv_distance = go_stream->take_int( 8 ) + 513.
      WHEN 19.
        rv_distance = go_stream->take_int( 8 ) + 769.
      WHEN 20.
        rv_distance = go_stream->take_int( 9 ) + 1025.
      WHEN 21.
        rv_distance = go_stream->take_int( 9 ) + 1537.
      WHEN 22.
        rv_distance = go_stream->take_int( 10 ) + 2049.
      WHEN 23.
        rv_distance = go_stream->take_int( 10 ) + 3073.
      WHEN 24.
        rv_distance = go_stream->take_int( 11 ) + 4097.
      WHEN 25.
        rv_distance = go_stream->take_int( 11 ) + 6145.
      WHEN 26.
        rv_distance = go_stream->take_int( 12 ) + 8193.
      WHEN 27.
        rv_distance = go_stream->take_int( 12 ) + 12289.
      WHEN 28.
        rv_distance = go_stream->take_int( 13 ) + 16385.
      WHEN 29.
        rv_distance = go_stream->take_int( 13 ) + 24577.
      WHEN OTHERS.
        ASSERT 1 = 0.
    ENDCASE.

  ENDMETHOD.
  METHOD map_length.

    CASE iv_code.
      WHEN 257.
        rv_length = go_stream->take_int( 0 ) + 3.
      WHEN 258.
        rv_length = go_stream->take_int( 0 ) + 4.
      WHEN 259.
        rv_length = go_stream->take_int( 0 ) + 5.
      WHEN 260.
        rv_length = go_stream->take_int( 0 ) + 6.
      WHEN 261.
        rv_length = go_stream->take_int( 0 ) + 7.
      WHEN 262.
        rv_length = go_stream->take_int( 0 ) + 8.
      WHEN 263.
        rv_length = go_stream->take_int( 0 ) + 9.
      WHEN 264.
        rv_length = go_stream->take_int( 0 ) + 10.
      WHEN 265.
        rv_length = go_stream->take_int( 1 ) + 11.
      WHEN 266.
        rv_length = go_stream->take_int( 1 ) + 13.
      WHEN 267.
        rv_length = go_stream->take_int( 1 ) + 15.
      WHEN 268.
        rv_length = go_stream->take_int( 1 ) + 17.
      WHEN 269.
        rv_length = go_stream->take_int( 2 ) + 19.
      WHEN 270.
        rv_length = go_stream->take_int( 2 ) + 23.
      WHEN 271.
        rv_length = go_stream->take_int( 2 ) + 27.
      WHEN 272.
        rv_length = go_stream->take_int( 2 ) + 31.
      WHEN 273.
        rv_length = go_stream->take_int( 3 ) + 35.
      WHEN 274.
        rv_length = go_stream->take_int( 3 ) + 43.
      WHEN 275.
        rv_length = go_stream->take_int( 3 ) + 51.
      WHEN 276.
        rv_length = go_stream->take_int( 3 ) + 59.
      WHEN 277.
        rv_length = go_stream->take_int( 4 ) + 67.
      WHEN 278.
        rv_length = go_stream->take_int( 4 ) + 83.
      WHEN 279.
        rv_length = go_stream->take_int( 4 ) + 99.
      WHEN 280.
        rv_length = go_stream->take_int( 4 ) + 115.
      WHEN 281.
        rv_length = go_stream->take_int( 5 ) + 131.
      WHEN 282.
        rv_length = go_stream->take_int( 5 ) + 163.
      WHEN 283.
        rv_length = go_stream->take_int( 5 ) + 195.
      WHEN 284.
        rv_length = go_stream->take_int( 5 ) + 227.
      WHEN 285.
        rv_length = go_stream->take_int( 0 ) + 258.
      WHEN OTHERS.
        ASSERT 1 = 0.
    ENDCASE.

  ENDMETHOD.
  METHOD not_compressed.

    DATA: lv_len  TYPE i,
          lv_nlen TYPE i ##NEEDED.
    DATA lv_bytes TYPE xstring.

* skip any remaining bits in current partially processed byte
    go_stream->clear_bits( ).

    lv_len = go_stream->take_int( 16 ).
    lv_nlen = go_stream->take_int( 16 ).

    lv_bytes = go_stream->take_bytes( lv_len ).
    CONCATENATE gv_out lv_bytes INTO gv_out IN BYTE MODE.

  ENDMETHOD.
  METHOD read_pair.

    DATA: lv_symbol TYPE i.


    rs_pair-length = map_length( iv_length ).

    lv_symbol = decode( go_distcode ).
    rs_pair-distance = map_distance( lv_symbol ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ZLIB implementation

*>>>>>>> ZCL_ABAPGIT_ZLIB_CONVERT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_zlib_convert======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_zlib_convert======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_zlib_convert======ccau.




class LCL_ABAPGIT_ZLIB_CONVERT implementation.
*"* method's implementations
*include methods.
  METHOD bits_to_int.

    DATA lv_i      TYPE i.
    DATA lv_offset TYPE i.

    DO strlen( iv_bits ) TIMES.
      lv_i = iv_bits+lv_offset(1).
      rv_int = rv_int * 2 + lv_i.
      lv_offset = lv_offset + 1.
    ENDDO.

  ENDMETHOD.
  METHOD hex_to_bits.

    DATA: lv_x   TYPE x LENGTH 1,
          lv_c   TYPE c LENGTH 1,
          lv_bit TYPE i,
          lv_hex TYPE xstring.


    lv_hex = iv_hex.
    WHILE NOT lv_hex IS INITIAL.
      lv_x = lv_hex.
      DO 8 TIMES.
        lv_bit = sy-index.
        GET BIT lv_bit OF lv_x INTO lv_c.
        CONCATENATE rv_bits lv_c INTO rv_bits.
      ENDDO.
      lv_hex = lv_hex+1.
    ENDWHILE.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ZLIB_CONVERT implementation

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

INTERFACE SHRITEFUH64VYIPO5IWUYB3KWTWSFY.

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

CLASS SHRITEFUH64VYIPO5IWUYB3KWTXSFY DEFINITION FINAL.
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

CLASS SHRITEFUH64VYIPO5IWUYB3KWTXSFY IMPLEMENTATION.

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

CLASS SHRITEFUH64VYIPO5IWUYB3KWTZSFY DEFINITION FINAL.
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

CLASS SHRITEFUH64VYIPO5IWUYB3KWTZSFY IMPLEMENTATION.

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
    lo_reader = cl_sxml_string_reader=>create( SHRITEFUH64VYIPO5IWUYB3KWTXSFY=>string_to_xstring_utf8( iv_json ) ).

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

CLASS SHRITEFUH64VYIPO5IWUYB3KWT3SFY DEFINITION FINAL CREATE PRIVATE.
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

CLASS SHRITEFUH64VYIPO5IWUYB3KWT3SFY IMPLEMENTATION.

  METHOD class_constructor.
    gv_comma_with_lf = ',' && cl_abap_char_utilities=>newline.
  ENDMETHOD.

  METHOD stringify.

    DATA lo TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KWT3SFY.
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

CLASS SHRITEFUH64VYIPO5IWUYB3KWT5SFY DEFINITION FINAL.
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
        type_kind         LIKE SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>any,
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

CLASS SHRITEFUH64VYIPO5IWUYB3KWT5SFY IMPLEMENTATION.

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
    IF is_parent_type-type_kind = SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>table.
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
        WHEN SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>table.
          lo_tdescr ?= is_parent_type-dd.
          rs_node_type-dd = lo_tdescr->get_table_line_type( ).

        WHEN SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>struct_flat OR SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>struct_deep.
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
      IF rs_node_type-type_kind = SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>table.
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
      WHEN SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>table.
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

      WHEN SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>struct_flat OR SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>struct_deep.
        ASSIGN i_container_ref->* TO <parent_struc>.
        ASSERT sy-subrc = 0.
    ENDCASE.

    TRY.

      " array_index because stringified index goes in wrong order [1, 10, 2 ...]
        LOOP AT mr_nodes->* ASSIGNING <n> USING KEY array_index WHERE path = iv_path.

        " Get or create type cache record
          IF is_parent_type-type_kind <> SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>table OR ls_node_type-type_kind IS INITIAL.
          " table records are the same, no need to refetch twice

            ls_node_type = get_node_type(
            is_node        = <n>
            is_parent_type = is_parent_type ).

            IF mv_corresponding = abap_true AND ls_node_type IS INITIAL.
              CONTINUE.
            ENDIF.

          ENDIF.

        " Validate node type
          IF ls_node_type-type_kind = SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>data_ref OR
           ls_node_type-type_kind = SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>object_ref.
          " TODO maybe in future
            Lcx_abapgit_ajson_error=>raise( 'Cannot assign to ref' ).
          ENDIF.

        " Find target field reference
          CASE is_parent_type-type_kind.
            WHEN SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>table.
              IF NOT ls_node_type-target_field_name CO '0123456789'.
              " Does not affect anything actually but for integrity
                Lcx_abapgit_ajson_error=>raise( 'Need index to access tables' ).
              ENDIF.

              IF is_parent_type-tab_item_buf IS NOT BOUND. " Indirect hint that table was srt/hsh, see get_node_type
                APPEND INITIAL LINE TO <parent_stdtab> REFERENCE INTO lr_target_field.
                ASSERT sy-subrc = 0.
              ENDIF.

            WHEN SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>struct_flat OR SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>struct_deep.
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
              IF ls_node_type-type_kind <> SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>struct_flat AND
               ls_node_type-type_kind <> SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>struct_deep.
                Lcx_abapgit_ajson_error=>raise( 'Expected structure' ).
              ENDIF.
              any_to_abap(
              iv_path         = <n>-path && <n>-name && '/'
              is_parent_type  = ls_node_type
              i_container_ref = lr_target_field ).

            WHEN Lif_abapgit_ajson_types=>node_type-array.
              IF NOT ls_node_type-type_kind = SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>table.
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

    IF is_node_type-type_kind CA SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>deep_targets.
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
        IF is_node_type-type_kind = SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>date AND is_node-value IS NOT INITIAL.
          <container> = to_date( is_node-value ).
        ELSEIF is_node_type-type_kind = SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>packed AND is_node-value IS NOT INITIAL.
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

CLASS SHRITEFUH64VYIPO5IWUYB3KWT7SFY DEFINITION FINAL.
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

CLASS SHRITEFUH64VYIPO5IWUYB3KWT7SFY IMPLEMENTATION.

  METHOD class_constructor.

    DATA lo_dummy TYPE REF TO Lcl_abapgit_ajson.
    DATA lo_type TYPE REF TO cl_abap_refdescr.
    lo_type ?= cl_abap_typedescr=>describe_by_data( lo_dummy ).
    gv_ajson_absolute_type_name = lo_type->get_referenced_type( )->absolute_name.

  ENDMETHOD.

  METHOD convert.

    DATA lo_type TYPE REF TO cl_abap_typedescr.
    DATA lo_converter TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KWT7SFY.

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

        IF io_type->type_kind = SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>data_ref OR iv_data IS INITIAL.
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

        ELSEIF io_type->type_kind = SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>object_ref
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
    ELSEIF io_type->type_kind CO SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>texts OR
           io_type->type_kind CO SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>binary OR
           io_type->type_kind CO SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>enum.
      ls_node-type = Lif_abapgit_ajson_types=>node_type-string.
      ls_node-value = |{ iv_data }|.
    ELSEIF io_type->type_kind = SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>date.
      ls_node-type = Lif_abapgit_ajson_types=>node_type-string.
      IF mv_format_datetime = abap_true.
        ls_node-value = format_date( iv_data ).
      ELSE.
        ls_node-value = |{ iv_data }|.
      ENDIF.
    ELSEIF io_type->type_kind = SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>time.
      ls_node-type = Lif_abapgit_ajson_types=>node_type-string.
      IF mv_format_datetime = abap_true.
        ls_node-value = format_time( iv_data ).
      ELSE.
        ls_node-value = |{ iv_data }|.
      ENDIF.
    ELSEIF io_type->type_kind CO SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>numeric.
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
    DATA lo_converter TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KWT7SFY.

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
    IF io_type->type_kind CO SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>texts OR
       io_type->type_kind CO SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>date OR
       io_type->type_kind CO SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>time.
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
    ELSEIF io_type->type_kind CO SHRITEFUH64VYIPO5IWUYB3KWTWSFY=>numeric.
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

INTERFACE SHRITEFUH64VYIPO5IWUYB3KWUBSFY.
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

CLASS SHRITEFUH64VYIPO5IWUYB3KWUCSFY DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES SHRITEFUH64VYIPO5IWUYB3KWUBSFY.
    CLASS-METHODS new
      IMPORTING
        ii_filter TYPE REF TO Lif_abapgit_ajson_filter
      RETURNING
        VALUE(ro_instance) TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KWUCSFY.
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

CLASS SHRITEFUH64VYIPO5IWUYB3KWUCSFY IMPLEMENTATION.

  METHOD new.
    CREATE OBJECT ro_instance EXPORTING ii_filter = ii_filter.
  ENDMETHOD.

  METHOD constructor.
    ASSERT ii_filter IS BOUND.
    mi_filter = ii_filter.
  ENDMETHOD.

  METHOD SHRITEFUH64VYIPO5IWUYB3KWUBSFY~run.

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

CLASS SHRITEFUH64VYIPO5IWUYB3KWUESFY DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES SHRITEFUH64VYIPO5IWUYB3KWUBSFY.
    CLASS-METHODS new
      IMPORTING
        ii_mapper TYPE REF TO Lif_abapgit_ajson_mapping
      RETURNING
        VALUE(ro_instance) TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KWUESFY.
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

CLASS SHRITEFUH64VYIPO5IWUYB3KWUESFY IMPLEMENTATION.

  METHOD new.
    CREATE OBJECT ro_instance EXPORTING ii_mapper = ii_mapper.
  ENDMETHOD.

  METHOD constructor.
    ASSERT ii_mapper IS BOUND.
    mi_mapper = ii_mapper.
  ENDMETHOD.

  METHOD SHRITEFUH64VYIPO5IWUYB3KWUBSFY~run.

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

CLASS SHRITEFUH64VYIPO5IWUYB3KWUGSFY DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES SHRITEFUH64VYIPO5IWUYB3KWUBSFY.
    CLASS-METHODS new
      RETURNING
        VALUE(ro_instance) TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KWUGSFY.
    METHODS add
      IMPORTING
        ii_mutator TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KWUBSFY
      RETURNING
        VALUE(ro_self) TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KWUGSFY.

  PRIVATE SECTION.
    DATA mt_queue TYPE STANDARD TABLE OF REF TO SHRITEFUH64VYIPO5IWUYB3KWUBSFY.

ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KWUGSFY IMPLEMENTATION.

  METHOD add.
    IF ii_mutator IS BOUND.
      APPEND ii_mutator TO mt_queue.
    ENDIF.
    ro_self = me.
  ENDMETHOD.

  METHOD new.
    CREATE OBJECT ro_instance.
  ENDMETHOD.

  METHOD SHRITEFUH64VYIPO5IWUYB3KWUBSFY~run.

    DATA li_mutator TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KWUBSFY.
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
CLASS SHRITEFUH64VYIPO5IWUYB3KWUISFY DEFINITION FINAL.
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

CLASS SHRITEFUH64VYIPO5IWUYB3KWUISFY IMPLEMENTATION.
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


*CLASS zcl_abapgit_ajson DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KWUOSFY.


**********************************************************************
* READER
**********************************************************************


*CLASS zcl_abapgit_ajson DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KWUPSFY.



**********************************************************************
* JSON TO ABAP
**********************************************************************


*CLASS zcl_abapgit_ajson DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KWUQSFY.


**********************************************************************
* WRITER
**********************************************************************


*CLASS zcl_abapgit_ajson DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KWURSFY.



**********************************************************************
* INTEGRATED
**********************************************************************


**********************************************************************
* ABAP TO JSON
**********************************************************************

*CLASS zcl_abapgit_ajson DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KWUUSFY.


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

    DATA lo_mutator_queue TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KWUGSFY.

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
        lo_mutator_queue->add( SHRITEFUH64VYIPO5IWUYB3KWUESFY=>new( ii_mapper ) ).
      ENDIF.
      IF ii_filter IS BOUND.
        lo_mutator_queue->add( SHRITEFUH64VYIPO5IWUYB3KWUCSFY=>new( ii_filter ) ).
      ENDIF.
      lo_mutator_queue->SHRITEFUH64VYIPO5IWUYB3KWUBSFY~run(
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
    ls_path_name = SHRITEFUH64VYIPO5IWUYB3KWTXSFY=>split_path( iv_path ).

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

    DATA lo_parser TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KWTZSFY.

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
            ls_new_node-index = SHRITEFUH64VYIPO5IWUYB3KWTXSFY=>validate_array_index(
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

    lv_normalized_path = SHRITEFUH64VYIPO5IWUYB3KWTXSFY=>normalize_path( iv_path ).
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
    ls_split_path = SHRITEFUH64VYIPO5IWUYB3KWTXSFY=>split_path( iv_path ).

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

    DATA lo_to_abap TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KWT5SFY.
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

    lv_normalized_path = SHRITEFUH64VYIPO5IWUYB3KWTXSFY=>normalize_path( iv_path ).

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
    ls_new_path-path = SHRITEFUH64VYIPO5IWUYB3KWTXSFY=>normalize_path( iv_path ).
    ls_new_path-name = |{ lv_new_index }|.

    lt_new_nodes = SHRITEFUH64VYIPO5IWUYB3KWT7SFY=>convert(
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

    ls_split_path = SHRITEFUH64VYIPO5IWUYB3KWTXSFY=>split_path( iv_path ).
    IF ls_split_path IS INITIAL. " Assign root, exceptional processing
      IF iv_node_type IS NOT INITIAL.
        mt_json_tree = SHRITEFUH64VYIPO5IWUYB3KWT7SFY=>insert_with_type(
          is_opts            = ms_opts
          iv_data            = iv_val
          iv_type            = iv_node_type
          is_prefix          = ls_split_path
          ii_custom_mapping  = mi_custom_mapping ).
      ELSE.
        mt_json_tree = SHRITEFUH64VYIPO5IWUYB3KWT7SFY=>convert(
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
      lv_array_index = SHRITEFUH64VYIPO5IWUYB3KWTXSFY=>validate_array_index(
        iv_path  = ls_split_path-path
        iv_index = ls_split_path-name ).
    ELSEIF lr_parent->type = Lif_abapgit_ajson_types=>node_type-object
      AND lv_item_order = 0 AND ms_opts-keep_item_order = abap_true.
      lv_item_order = lr_parent->children + 1.
    ENDIF.

    IF iv_node_type IS NOT INITIAL.
      lt_new_nodes = SHRITEFUH64VYIPO5IWUYB3KWT7SFY=>insert_with_type(
        is_opts            = ms_opts
        iv_item_order      = lv_item_order
        iv_data            = iv_val
        iv_type            = iv_node_type
        iv_array_index     = lv_array_index
        is_prefix          = ls_split_path
        ii_custom_mapping  = mi_custom_mapping ).
    ELSE.
      lt_new_nodes = SHRITEFUH64VYIPO5IWUYB3KWT7SFY=>convert(
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
    lv_val = SHRITEFUH64VYIPO5IWUYB3KWT7SFY=>format_date( iv_val ).

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
    lv_timestamp_iso = SHRITEFUH64VYIPO5IWUYB3KWT7SFY=>format_timestamp( iv_val ).

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
    lv_normalized_path = SHRITEFUH64VYIPO5IWUYB3KWTXSFY=>normalize_path( iv_path ).
    lv_path_len        = strlen( lv_normalized_path ).
    ls_path_parts      = SHRITEFUH64VYIPO5IWUYB3KWTXSFY=>split_path( lv_normalized_path ).

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

    rv_json = SHRITEFUH64VYIPO5IWUYB3KWT3SFY=>stringify(
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

    ls_split_path = SHRITEFUH64VYIPO5IWUYB3KWTXSFY=>split_path( iv_path ).
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

    DATA lo_to_abap TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KWT5SFY.

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
*CLASS SHRITEFUH64VYIPO5IWUYCIJCOUSFY DEFINITION DEFERRED.

*CLASS zcl_abapgit_user_record DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYCIJCOUSFY.




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

*>>>>>>> ZCL_ABAPGIT_ABAP_LANGUAGE_VERS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_abap_language_versccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_abap_language_versccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_abap_language_versccau.
* Helper to toggle (cloud) enviroment
CLASS SHRITEFUH64VYIPO5IWUYB3KWTOSFY DEFINITION.

  PUBLIC SECTION.
    INTERFACES Lif_abapgit_environment.

    DATA mv_is_cloud TYPE abap_bool.

    METHODS set_cloud
      IMPORTING
        iv_is_cloud TYPE abap_bool.

ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KWTOSFY IMPLEMENTATION.

  METHOD set_cloud.
    mv_is_cloud = iv_is_cloud.
  ENDMETHOD.

  METHOD Lif_abapgit_environment~is_sap_cloud_platform.
    rv_result = mv_is_cloud.
  ENDMETHOD.

  METHOD Lif_abapgit_environment~compare_with_inactive.
  ENDMETHOD.
  METHOD Lif_abapgit_environment~get_basis_release.
  ENDMETHOD.
  METHOD Lif_abapgit_environment~get_system_language_filter.
  ENDMETHOD.
  METHOD Lif_abapgit_environment~is_merged.
  ENDMETHOD.
  METHOD Lif_abapgit_environment~is_repo_object_changes_allowed.
  ENDMETHOD.
  METHOD Lif_abapgit_environment~is_restart_required.
  ENDMETHOD.
  METHOD Lif_abapgit_environment~is_sap_object_allowed.
  ENDMETHOD.
  METHOD Lif_abapgit_environment~is_variant_maintenance.
  ENDMETHOD.
  METHOD Lif_abapgit_environment~init_parallel_processing.
  ENDMETHOD.

ENDCLASS.

* Helper to toggle experimental features
CLASS SHRITEFUH64VYIPO5IWUYB3KWTQSFY DEFINITION.

  PUBLIC SECTION.
    INTERFACES Lif_abapgit_persist_settings.

    DATA mo_settings TYPE REF TO Lcl_abapgit_settings.

    METHODS constructor.

ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KWTQSFY IMPLEMENTATION.

  METHOD constructor.
    CREATE OBJECT mo_settings.
  ENDMETHOD.

  METHOD Lif_abapgit_persist_settings~modify.
  ENDMETHOD.

  METHOD Lif_abapgit_persist_settings~read.
    ro_settings = mo_settings.
  ENDMETHOD.

ENDCLASS.

* Test cases


class LCL_ABAPGIT_ABAP_LANGUAGE_VERS implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    mo_dot_abapgit = io_dot_abapgit.

    IF Lcl_abapgit_feature=>is_enabled( c_feature_flag ) = abap_false.
      mv_has_abap_language_vers = abap_false.
    ELSEIF get_abap_language_vers_by_repo( ) = Lif_abapgit_dot_abapgit=>c_abap_language_version-undefined.
      mv_has_abap_language_vers = abap_false.
    ELSE.
      mv_has_abap_language_vers = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD get_abap_language_vers_by_devc.

    DATA lv_class TYPE string.
    DATA lv_abap_lang_version_devc TYPE string.
    DATA lo_abap_language_version_cfg TYPE REF TO object.

    lv_class = 'CL_ABAP_LANGUAGE_VERSION_CFG'.

    TRY.

        CALL METHOD (lv_class)=>('GET_INSTANCE')
          RECEIVING
            ro_instance = lo_abap_language_version_cfg.

        " For non-existing packages, GET_PACKAGE_DEFAULT_VERSION returns "standard"
        " but we want to return "undefined" in this case to allow any new packages
        IF Lcl_abapgit_factory=>get_sap_package( iv_package )->exists( ) = abap_true.
          CALL METHOD lo_abap_language_version_cfg->('IF_ABAP_LANGUAGE_VERSION_CFG~GET_PACKAGE_DEFAULT_VERSION')
            EXPORTING
              iv_package_name             = iv_package
            RECEIVING
              rv_default_language_version = lv_abap_lang_version_devc.
        ELSE.
          lv_abap_lang_version_devc = '-'.
        ENDIF.

        CASE lv_abap_lang_version_devc.
          WHEN Lif_abapgit_aff_types_v1=>co_abap_language_version-standard.
            rv_abap_language_version = Lif_abapgit_dot_abapgit=>c_abap_language_version-standard.
          WHEN Lif_abapgit_aff_types_v1=>co_abap_language_version-key_user.
            rv_abap_language_version = Lif_abapgit_dot_abapgit=>c_abap_language_version-key_user.
          WHEN Lif_abapgit_aff_types_v1=>co_abap_language_version-cloud_development.
            rv_abap_language_version = Lif_abapgit_dot_abapgit=>c_abap_language_version-cloud_development.
          WHEN OTHERS.
            rv_abap_language_version = Lif_abapgit_dot_abapgit=>c_abap_language_version-undefined.
        ENDCASE.
      CATCH cx_root.
        rv_abap_language_version = Lif_abapgit_dot_abapgit=>c_abap_language_version-undefined.
    ENDTRY.

  ENDMETHOD.
  METHOD get_abap_language_vers_by_objt.

    DATA lv_class TYPE string.
    DATA lo_abap_language_version TYPE REF TO object.

    IF mv_has_abap_language_vers = abap_false.
      rv_allowed_abap_langu_version = c_any_abap_language_version.
      RETURN. ">>>
    ENDIF.

    lv_class = 'CL_ABAP_LANGUAGE_VERSION'.

    TRY.

        CALL METHOD (lv_class)=>('GET_INSTANCE')
          RECEIVING
            ro_version_handler = lo_abap_language_version.

        CALL METHOD lo_abap_language_version->('IF_ABAP_LANGUAGE_VERSION~GET_DEFAULT_VERSION')
          EXPORTING
            iv_object_type     = iv_object_type
            iv_package         = iv_package
          RECEIVING
            rv_default_version = rv_allowed_abap_langu_version.

      CATCH cx_root.
        rv_allowed_abap_langu_version = get_default_abap_language_vers( iv_object_type ).
    ENDTRY.

  ENDMETHOD.
  METHOD get_abap_language_vers_by_repo.
    rv_abap_language_version = mo_dot_abapgit->get_abap_language_version( ).
    IF rv_abap_language_version IS INITIAL.
      rv_abap_language_version = Lif_abapgit_dot_abapgit=>c_abap_language_version-undefined.
    ENDIF.
  ENDMETHOD.
  METHOD get_default_abap_language_vers.

    IF Lcl_abapgit_factory=>get_environment( )->is_sap_cloud_platform( ) = abap_true.
      " On BTP, default to ABAP for Cloud Development
      rv_abap_language_version = Lif_abapgit_aff_types_v1=>co_abap_language_version_cloud-cloud_development.
    ELSE.
      " Differentiate between source code object and non-source code objects
      CASE iv_object_type.
        WHEN 'BDEF' OR 'CLAS' OR 'FUGR' OR 'FUGS' OR 'INTF' OR 'PROG' OR 'TYPE'.
          rv_abap_language_version = Lif_abapgit_aff_types_v1=>co_abap_language_version_src-standard.
        WHEN OTHERS.
          rv_abap_language_version = Lif_abapgit_aff_types_v1=>co_abap_language_version-standard.
      ENDCASE.
    ENDIF.

  ENDMETHOD.
  METHOD get_repo_abap_language_version.

    DATA lv_abap_language_version TYPE string.

    IF mv_has_abap_language_vers = abap_true.
      lv_abap_language_version = mo_dot_abapgit->get_abap_language_version( ).
    ENDIF.

    CASE lv_abap_language_version.
      WHEN Lif_abapgit_dot_abapgit=>c_abap_language_version-standard.
        rv_abap_language_version = Lif_abapgit_aff_types_v1=>co_abap_language_version_src-standard.
      WHEN Lif_abapgit_dot_abapgit=>c_abap_language_version-key_user.
        rv_abap_language_version = Lif_abapgit_aff_types_v1=>co_abap_language_version_src-key_user.
      WHEN Lif_abapgit_dot_abapgit=>c_abap_language_version-cloud_development.
        rv_abap_language_version = Lif_abapgit_aff_types_v1=>co_abap_language_version_src-cloud_development.
      WHEN OTHERS. " undefined or feature off
        rv_abap_language_version = c_any_abap_language_version.
    ENDCASE.

  ENDMETHOD.
  METHOD is_import_allowed.

    DATA lv_package_version TYPE string.

    lv_package_version = get_abap_language_vers_by_devc( iv_package ).

    CASE get_abap_language_vers_by_repo( ).
      WHEN Lif_abapgit_dot_abapgit=>c_abap_language_version-undefined.
        rv_allowed = abap_true.
      WHEN OTHERS.
        IF get_abap_language_vers_by_repo( ) = lv_package_version.
          " allow packages that match repo setting
          rv_allowed = abap_true.
        ELSEIF lv_package_version = Lif_abapgit_dot_abapgit=>c_abap_language_version-undefined.
          " always allow new packages
          rv_allowed = abap_true.
        ELSE.
          rv_allowed = abap_false.
        ENDIF.
    ENDCASE.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ABAP_LANGUAGE_VERS implementation

*>>>>>>> ZCL_ABAPGIT_AFF_REGISTRY <<<<<<<*

*"* macro definitions
*include zcl_abapgit_aff_registry======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_aff_registry======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_aff_registry======ccau.
"! @testing zcl_abapgit_filename_logic



class LCL_ABAPGIT_AFF_REGISTRY implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    mv_aff_enabled = Lcl_abapgit_feature=>is_enabled( c_aff_feature ).
  ENDMETHOD.
  METHOD initialize_registry_table.
    register( 'CHKC' ).
    register( 'CHKO' ).
    register( 'CHKV' ).
    register( 'EVTB' ).
    register( 'EEEC' ).
    register( 'GSMP' ).
    register( iv_obj_type     = 'INTF'
              iv_experimental = abap_true ).
    register( 'SMBC' ).
    register( 'NONT' ).
    register( 'RONT' ).
  ENDMETHOD.
  METHOD register.
    DATA ls_registry_entry TYPE ty_registry_entry.

    ls_registry_entry-obj_type = iv_obj_type.
    ls_registry_entry-experimental = iv_experimental.
    INSERT ls_registry_entry INTO TABLE gt_registry.
  ENDMETHOD.
  METHOD Lif_abapgit_aff_registry~is_supported_object_type.

    DATA ls_registry_entry TYPE ty_registry_entry.

    IF gt_registry IS INITIAL.
      initialize_registry_table( ).
    ENDIF.

    READ TABLE gt_registry WITH TABLE KEY obj_type = iv_obj_type INTO ls_registry_entry.
    IF sy-subrc = 0 AND ls_registry_entry-experimental = abap_false.
      rv_result = abap_true.
    ELSEIF sy-subrc = 0 AND mv_aff_enabled = abap_true.
      rv_result = abap_true.
    ELSE.
      rv_result = abap_false.
    ENDIF.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_AFF_REGISTRY implementation

*>>>>>>> ZCL_ABAPGIT_APACK_HELPER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_apack_helper======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_apack_helper======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_APACK_HELPER implementation.
*"* method's implementations
*include methods.
  METHOD are_dependencies_met.

    DATA: lt_dependencies_status TYPE ty_dependency_statuses.

    IF it_dependencies IS INITIAL.
      rv_status = Lif_abapgit_definitions=>c_yes.
      RETURN.
    ENDIF.

    lt_dependencies_status = get_dependencies_met_status( it_dependencies ).

    LOOP AT lt_dependencies_status TRANSPORTING NO FIELDS WHERE met <> Lif_abapgit_definitions=>c_yes.
      EXIT.
    ENDLOOP.

    IF sy-subrc = 0.
      rv_status = Lif_abapgit_definitions=>c_no.
    ELSE.
      rv_status = Lif_abapgit_definitions=>c_yes.
    ENDIF.

  ENDMETHOD.
  METHOD dependencies_popup.

    DATA: lt_met_status TYPE ty_dependency_statuses,
          lv_answer     TYPE c LENGTH 1.

    lt_met_status = get_dependencies_met_status( it_dependencies ).

    show_dependencies_popup( lt_met_status ).

    lv_answer = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
      iv_titlebar      = 'Warning'
      iv_text_question = 'The project has unmet dependencies. Do you want to continue?' ).

    IF lv_answer <> '1'.
      Lcx_abapgit_exception=>raise( 'Cancelling because of unmet dependencies.' ).
    ENDIF.

  ENDMETHOD.
  METHOD get_color_table.

    DATA:
      lo_functional_settings TYPE REF TO cl_salv_functional_settings,
      lo_hyperlinks          TYPE REF TO cl_salv_hyperlinks,
      lt_color_negative      TYPE lvc_t_scol,
      lt_color_normal        TYPE lvc_t_scol,
      lt_color_positive      TYPE lvc_t_scol,
      ls_color               TYPE lvc_s_scol,
      lv_handle              TYPE i,
      ls_hyperlink           TYPE salv_s_int4_column,
      lv_hyperlink           TYPE service_rl.

    FIELD-SYMBOLS:
      <ls_line>       TYPE ty_color_line,
      <ls_dependency> LIKE LINE OF it_dependencies.

    CLEAR: ls_color.
    ls_color-color-col = col_negative.
    APPEND ls_color TO lt_color_negative.

    CLEAR: ls_color.
    ls_color-color-col = col_normal.
    APPEND ls_color TO lt_color_normal.

    CLEAR: ls_color.
    ls_color-color-col = col_positive.
    APPEND ls_color TO lt_color_positive.

    lo_functional_settings = io_alv->get_functional_settings( ).
    lo_hyperlinks = lo_functional_settings->get_hyperlinks( ).

    CLEAR: lv_handle, ls_color.
    LOOP AT it_dependencies ASSIGNING <ls_dependency>.
      lv_handle = lv_handle + 1.

      APPEND INITIAL LINE TO ct_color_table ASSIGNING <ls_line>.
      MOVE-CORRESPONDING <ls_dependency> TO <ls_line>.

      CASE <ls_line>-met.
        WHEN Lif_abapgit_definitions=>c_yes.
          <ls_line>-color     = lt_color_positive.
          <ls_line>-exception = '3'.
        WHEN Lif_abapgit_definitions=>c_partial.
          <ls_line>-color     = lt_color_normal.
          <ls_line>-exception = '2'.
        WHEN Lif_abapgit_definitions=>c_no.
          <ls_line>-color     = lt_color_negative.
          <ls_line>-exception = '1'.
      ENDCASE.

      CLEAR: ls_hyperlink.
      ls_hyperlink-columnname = 'GIT_URL'.
      ls_hyperlink-value      = lv_handle.
      APPEND ls_hyperlink TO <ls_line>-t_hyperlink.

      lv_hyperlink = <ls_line>-git_url.
      lo_hyperlinks->add_hyperlink( handle    = lv_handle
                                    hyperlink = lv_hyperlink ).

    ENDLOOP.

  ENDMETHOD.
  METHOD get_dependencies_met_status.

    DATA: lt_installed_packages TYPE Lif_abapgit_apack_definitions=>ty_descriptors,
          ls_installed_package  TYPE Lif_abapgit_apack_definitions=>ty_descriptor,
          ls_dependecy          TYPE Lif_abapgit_apack_definitions=>ty_dependency,
          ls_dependecy_popup    TYPE ty_dependency_status.

    IF it_dependencies IS INITIAL.
      RETURN.
    ENDIF.

    lt_installed_packages = get_installed_packages( ).

    LOOP AT it_dependencies INTO ls_dependecy.
      CLEAR: ls_dependecy_popup.

      MOVE-CORRESPONDING ls_dependecy TO ls_dependecy_popup.

      READ TABLE lt_installed_packages INTO ls_installed_package
        WITH KEY group_id    = ls_dependecy-group_id
                 artifact_id = ls_dependecy-artifact_id.
      IF sy-subrc <> 0.
        ls_dependecy_popup-met = Lif_abapgit_definitions=>c_no.
      ELSE.
        TRY.
            Lcl_abapgit_version=>check_dependant_version( is_current   = ls_installed_package-sem_version
                                                          is_dependant = ls_dependecy-sem_version ).
            ls_dependecy_popup-met = Lif_abapgit_definitions=>c_yes.
          CATCH Lcx_abapgit_exception.
            ls_dependecy_popup-met = Lif_abapgit_definitions=>c_partial.
        ENDTRY.
      ENDIF.

      INSERT ls_dependecy_popup INTO TABLE rt_status.

    ENDLOOP.

  ENDMETHOD.
  METHOD get_installed_packages.

    DATA: lo_apack_reader            TYPE REF TO Lcl_abapgit_apack_reader,
          lt_manifest_implementation TYPE ty_manifest_declarations,
          ls_manifest_implementation TYPE ty_manifest_declaration,
          lo_manifest_provider       TYPE REF TO object,
          ls_descriptor              TYPE Lif_abapgit_apack_definitions=>ty_descriptor.

    SELECT seometarel~clsname tadir~devclass FROM seometarel "#EC CI_NOORDER
       INNER JOIN tadir ON seometarel~clsname = tadir~obj_name "#EC CI_BUFFJOIN
       INTO TABLE lt_manifest_implementation
       WHERE tadir~pgmid = 'R3TR'
         AND tadir~object = 'CLAS'
         AND seometarel~version = '1'
         AND ( seometarel~refclsname = Lif_abapgit_apack_definitions=>c_apack_interface_cust
            OR seometarel~refclsname = Lif_abapgit_apack_definitions=>c_apack_interface_sap )
      ORDER BY clsname devclass.

    LOOP AT lt_manifest_implementation INTO ls_manifest_implementation.
      CLEAR: lo_manifest_provider, lo_apack_reader.

      TRY.
          CREATE OBJECT lo_manifest_provider TYPE (ls_manifest_implementation-clsname).
        CATCH cx_sy_create_object_error.
          CLEAR: lo_manifest_provider.
      ENDTRY.

      IF lo_manifest_provider IS NOT BOUND.
        CONTINUE.
      ENDIF.

      lo_apack_reader = Lcl_abapgit_apack_reader=>create_instance( ls_manifest_implementation-devclass ).
      lo_apack_reader->copy_manifest_descriptor( lo_manifest_provider ).
      ls_descriptor = lo_apack_reader->get_manifest_descriptor( ).

      IF ls_descriptor IS NOT INITIAL.
        INSERT ls_descriptor INTO TABLE rt_packages.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD show_dependencies_popup.

    DATA: lo_alv          TYPE REF TO cl_salv_table,
          lo_column       TYPE REF TO cl_salv_column,
          lo_column_table TYPE REF TO cl_salv_column_table,
          lo_columns      TYPE REF TO cl_salv_columns_table,
          lt_columns      TYPE salv_t_column_ref,
          ls_column       LIKE LINE OF lt_columns,
          lt_color_table  TYPE ty_color_tab,
          ls_position     TYPE Lif_abapgit_popups=>ty_popup_position,
          lx_ex           TYPE REF TO cx_root.

    IF it_dependencies IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table  = lo_alv
                                CHANGING  t_table       = lt_color_table ).

        lo_columns = lo_alv->get_columns( ).
        lt_columns = lo_columns->get( ).
        LOOP AT lt_columns INTO ls_column WHERE columnname CP 'SEM_VERSION-*'.
          ls_column-r_column->set_technical( ).
        ENDLOOP.

        lo_column = lo_columns->get_column( 'MET' ).
        lo_column->set_technical( ).

        lo_column = lo_columns->get_column( 'GROUP_ID' ).
        lo_column->set_short_text( 'Org/ProjId' ).

        lo_columns->set_color_column( 'COLOR' ).
        lo_columns->set_exception_column( 'EXCEPTION' ).
        lo_columns->set_hyperlink_entry_column( 'T_HYPERLINK' ).
        lo_columns->set_optimize( ).

        lo_column = lo_columns->get_column( 'GROUP_ID' ).
        lo_column->set_short_text( 'Org/ProjId' ).

        lo_column = lo_columns->get_column( 'ARTIFACT_ID' ).
        lo_column->set_short_text( 'Proj. Name' ).

        lo_column = lo_columns->get_column( 'GIT_URL' ).
        lo_column->set_short_text( 'Git URL' ).

        lo_column_table ?= lo_column.
        lo_column_table->set_cell_type( if_salv_c_cell_type=>link ).

        lo_column = lo_columns->get_column( 'VERSION' ).
        lo_column->set_short_text( 'Version' ).

        lo_column = lo_columns->get_column( 'TARGET_PACKAGE' ).
        lo_column->set_technical( ).

        get_color_table(
          EXPORTING
            io_alv          = lo_alv
            it_dependencies = it_dependencies
          CHANGING
            ct_color_table  = lt_color_table ).

        ls_position = Lcl_abapgit_popups=>center(
          iv_width  = 90
          iv_height = 10 ).

        lo_alv->set_screen_popup( start_column = ls_position-start_column
                                  end_column   = ls_position-end_column
                                  start_line   = ls_position-start_row
                                  end_line     = ls_position-end_row ).

        lo_alv->get_display_settings( )->set_list_header( 'APACK dependencies' ).
        lo_alv->display( ).

      CATCH cx_salv_msg cx_salv_not_found cx_salv_data_error cx_salv_existing INTO lx_ex.
        Lcx_abapgit_exception=>raise( lx_ex->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
  METHOD to_file.

    DATA: lo_manifest_reader TYPE REF TO Lcl_abapgit_apack_reader,
          ls_descriptor      TYPE Lif_abapgit_apack_definitions=>ty_descriptor,
          lo_manifest_writer TYPE REF TO Lcl_abapgit_apack_writer.

    lo_manifest_reader = Lcl_abapgit_apack_reader=>create_instance( iv_package ).
    IF lo_manifest_reader->has_manifest( ) = abap_true.
      ls_descriptor = lo_manifest_reader->get_manifest_descriptor( ).
      lo_manifest_writer = Lcl_abapgit_apack_writer=>create_instance( ls_descriptor ).
      rs_file-path     = Lif_abapgit_definitions=>c_root_dir.
      rs_file-filename = Lif_abapgit_apack_definitions=>c_dot_apack_manifest.
      rs_file-data     = Lcl_abapgit_convert=>string_to_xstring_utf8( lo_manifest_writer->serialize( ) ).
      rs_file-sha1     = Lcl_abapgit_hash=>sha1_blob( rs_file-data ).
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_APACK_HELPER implementation

*>>>>>>> ZCL_ABAPGIT_APACK_READER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_apack_reader======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_apack_reader======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_apack_reader======ccau.
*"* use this source file for your ABAP unit test classes


class LCL_ABAPGIT_APACK_READER implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    mv_package_name = iv_package_name.
  ENDMETHOD.
  METHOD copy_manifest_descriptor.

    DATA: ls_my_manifest_wo_deps TYPE Lif_abapgit_apack_definitions=>ty_descriptor_wo_dependencies,
          ls_my_dependency       TYPE Lif_abapgit_apack_definitions=>ty_dependency,
          ls_descriptor          TYPE Lif_abapgit_apack_definitions=>ty_descriptor,
          lv_descriptor_cust     TYPE string,
          lv_descriptor_sap      TYPE string.

    FIELD-SYMBOLS: <lg_descriptor>   TYPE any,
                   <lt_dependencies> TYPE ANY TABLE,
                   <lg_dependency>   TYPE any.

    lv_descriptor_cust = Lif_abapgit_apack_definitions=>c_apack_interface_cust && '~DESCRIPTOR'.
    lv_descriptor_sap  = Lif_abapgit_apack_definitions=>c_apack_interface_sap && '~DESCRIPTOR'.

    ASSIGN io_manifest_provider->(lv_descriptor_cust) TO <lg_descriptor>.
    IF <lg_descriptor> IS NOT ASSIGNED.
      ASSIGN io_manifest_provider->(lv_descriptor_sap) TO <lg_descriptor>.
    ENDIF.
    IF <lg_descriptor> IS ASSIGNED.
      " A little more complex than a normal MOVE-CORRSPONDING
      " to avoid dumps in case of future updates to the dependencies table structure
      ASSIGN COMPONENT 'DEPENDENCIES' OF STRUCTURE <lg_descriptor> TO <lt_dependencies>.
      IF <lt_dependencies> IS ASSIGNED.
        LOOP AT <lt_dependencies> ASSIGNING <lg_dependency>.
          MOVE-CORRESPONDING <lg_dependency> TO ls_my_dependency.
          INSERT ls_my_dependency INTO TABLE ls_descriptor-dependencies.
        ENDLOOP.
        MOVE-CORRESPONDING <lg_descriptor> TO ls_my_manifest_wo_deps.
        MOVE-CORRESPONDING ls_my_manifest_wo_deps TO ls_descriptor.
      ENDIF.
    ENDIF.

    set_manifest_descriptor( ls_descriptor ).

  ENDMETHOD.
  METHOD create_instance.
    CREATE OBJECT ro_manifest_reader
      EXPORTING
        iv_package_name = iv_package_name.
  ENDMETHOD.
  METHOD deserialize.

    DATA: lv_xml  TYPE string,
          ls_data TYPE Lif_abapgit_apack_definitions=>ty_descriptor.

    lv_xml = Lcl_abapgit_convert=>xstring_to_string_utf8( iv_xstr ).

    ls_data = from_xml( lv_xml ).

    ro_manifest_reader = create_instance( iv_package_name ).
    ro_manifest_reader->set_manifest_descriptor( ls_data ).

  ENDMETHOD.
  METHOD format_version.

    FIELD-SYMBOLS: <ls_dependency> TYPE Lif_abapgit_apack_definitions=>ty_dependency.

    TRANSLATE ms_cached_descriptor-version TO LOWER CASE.
    ms_cached_descriptor-sem_version = Lcl_abapgit_version=>conv_str_to_version( ms_cached_descriptor-version ).

    LOOP AT ms_cached_descriptor-dependencies ASSIGNING <ls_dependency>.
      <ls_dependency>-sem_version = Lcl_abapgit_version=>conv_str_to_version( <ls_dependency>-version ).
    ENDLOOP.

  ENDMETHOD.
  METHOD from_xml.

    DATA: lv_xml TYPE string.

    lv_xml = iv_xml.

    CALL TRANSFORMATION id
      OPTIONS value_handling = 'accept_data_loss'
      SOURCE XML lv_xml
      RESULT data = rs_data.

  ENDMETHOD.
  METHOD get_manifest_descriptor.

    DATA: lo_manifest_provider       TYPE REF TO object,
          lv_package                 TYPE devclass,
          lt_packages                TYPE Lif_abapgit_sap_package=>ty_devclass_tt,
          ls_manifest_implementation TYPE ty_s_manifest_declaration,
          lt_manifest_implementation TYPE STANDARD TABLE OF ty_s_manifest_declaration WITH DEFAULT KEY.
    DATA lt_refclsname TYPE RANGE OF abap_classname.
    DATA ls_refclsname LIKE LINE OF lt_refclsname.


    IF mv_package_name IS NOT INITIAL.
      lt_packages = Lcl_abapgit_factory=>get_sap_package( mv_package_name )->list_subpackages( ).
      INSERT mv_package_name INTO TABLE lt_packages.
    ENDIF.

    IF mv_is_cached IS INITIAL AND lt_packages IS NOT INITIAL.
      ls_refclsname-sign = 'I'.
      ls_refclsname-option = 'EQ'.
      ls_refclsname-low = Lif_abapgit_apack_definitions=>c_apack_interface_cust.
      INSERT ls_refclsname INTO TABLE lt_refclsname.

      ls_refclsname-sign = 'I'.
      ls_refclsname-option = 'EQ'.
      ls_refclsname-low = Lif_abapgit_apack_definitions=>c_apack_interface_sap.
      INSERT ls_refclsname INTO TABLE lt_refclsname.

      IF mv_package_name CA '/'.
        ls_refclsname-sign = 'I'.
        ls_refclsname-option = 'CP'.
        ls_refclsname-low = Lif_abapgit_apack_definitions=>c_apack_interface_nspc.
        INSERT ls_refclsname INTO TABLE lt_refclsname.
      ENDIF.

      " Find all classes that implement customer or SAP version of APACK interface
      SELECT seometarel~clsname tadir~devclass FROM seometarel "#EC CI_NOORDER
         INNER JOIN tadir ON seometarel~clsname = tadir~obj_name "#EC CI_BUFFJOIN
         INTO TABLE lt_manifest_implementation
         WHERE tadir~pgmid = 'R3TR' AND
               tadir~object = 'CLAS' AND
               seometarel~version = '1' AND
               seometarel~refclsname IN lt_refclsname
         ORDER BY clsname devclass.

      LOOP AT lt_packages INTO lv_package.
        READ TABLE lt_manifest_implementation INTO ls_manifest_implementation WITH KEY devclass = lv_package.
        IF sy-subrc = 0.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF ls_manifest_implementation IS NOT INITIAL.
        TRY.
            CREATE OBJECT lo_manifest_provider TYPE (ls_manifest_implementation-clsname).
          CATCH cx_sy_create_object_error.
            CLEAR: rs_manifest_descriptor.
        ENDTRY.
        IF lo_manifest_provider IS BOUND.
          copy_manifest_descriptor( lo_manifest_provider ).
        ENDIF.
      ENDIF.

      mv_is_cached = abap_true.

    ENDIF.

    rs_manifest_descriptor = ms_cached_descriptor.
  ENDMETHOD.
  METHOD has_manifest.

    DATA: ls_returned_manifest TYPE Lif_abapgit_apack_definitions=>ty_descriptor.

    ls_returned_manifest = get_manifest_descriptor( ).

    rv_has_manifest = abap_false.
    IF ls_returned_manifest IS NOT INITIAL.
      rv_has_manifest = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD set_manifest_descriptor.
    mv_is_cached = abap_true.
    ms_cached_descriptor = is_manifest_descriptor.
    format_version( ).
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_APACK_READER implementation

*>>>>>>> ZCL_ABAPGIT_CODE_INSPECTOR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_code_inspector====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_code_inspector====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_CODE_INSPECTOR implementation.
*"* method's implementations
*include methods.
  METHOD cleanup.

    IF mo_inspection IS BOUND.

      mo_inspection->delete(
        EXCEPTIONS
          locked              = 1
          error_in_enqueue    = 2
          not_authorized      = 3
          exceptn_appl_exists = 4
          OTHERS              = 5 ).

      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Couldn't delete inspection. Subrc = { sy-subrc }| ).
      ENDIF.

    ENDIF.

    io_set->delete(
      EXCEPTIONS
        exists_in_insp   = 1
        locked           = 2
        error_in_enqueue = 3
        not_authorized   = 4
        exists_in_objs   = 5
        OTHERS           = 6 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Couldn't delete objectset. Subrc = { sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    IF iv_package IS INITIAL.
      Lcx_abapgit_exception=>raise( |Please supply package| ).
    ENDIF.

    mv_package = iv_package.

    " We create the inspection and objectset with dummy names.
    " Because we want to persist them so we can run it in parallel.
    " Both are deleted afterwards.
    mv_name = |{ sy-uname }_{ sy-datum }_{ sy-uzeit }|.
    mv_run_mode = decide_run_mode( ).

  ENDMETHOD.
  METHOD create_inspection.

    cl_ci_inspection=>create(
      EXPORTING
        p_user           = sy-uname
        p_name           = mv_name
      RECEIVING
        p_ref            = ro_inspection
      EXCEPTIONS
        locked           = 1
        error_in_enqueue = 2
        not_authorized   = 3
        OTHERS           = 4 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Failed to create inspection. Subrc = { sy-subrc }| ).
    ENDIF.

    ro_inspection->set(
      p_chkv = io_variant
      p_objs = io_set ).

    ro_inspection->save(
      EXCEPTIONS
        missing_information = 1
        insp_no_name        = 2
        not_enqueued        = 3
        OTHERS              = 4 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Failed to save inspection. Subrc = { sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD create_objectset.

    DATA: lt_objs       TYPE scit_objs,
          ls_obj        TYPE scir_objs,
          lt_objs_check TYPE scit_objs,
          ls_item       TYPE Lif_abapgit_definitions=>ty_item,
          lt_packages   TYPE Lif_abapgit_sap_package=>ty_devclass_tt.

    lt_packages = Lcl_abapgit_factory=>get_sap_package( mv_package )->list_subpackages( ).
    INSERT mv_package INTO TABLE lt_packages.

    SELECT object AS objtype obj_name AS objname
      FROM tadir
      INTO CORRESPONDING FIELDS OF TABLE lt_objs
      FOR ALL ENTRIES IN lt_packages
      WHERE devclass = lt_packages-table_line
      AND delflag = abap_false
      AND pgmid = 'R3TR' ##TOO_MANY_ITAB_FIELDS.        "#EC CI_GENBUFF

    LOOP AT lt_objs INTO ls_obj.

      IF skip_object( ls_obj ) = abap_true.
        CONTINUE.
      ENDIF.

      ls_item-obj_type = ls_obj-objtype.
      ls_item-obj_name = ls_obj-objname.

      IF Lcl_abapgit_objects=>exists( ls_item ) = abap_false.
        CONTINUE.
      ENDIF.

      INSERT ls_obj INTO TABLE lt_objs_check.

    ENDLOOP.

    ro_set = cl_ci_objectset=>save_from_list(
      p_name    = mv_name
      p_objects = lt_objs_check ).

  ENDMETHOD.
  METHOD create_variant.

    IF iv_variant IS INITIAL.
      Lcx_abapgit_exception=>raise( |No check variant supplied.| ).
    ENDIF.

    cl_ci_checkvariant=>get_ref(
      EXPORTING
        p_user                   = ''
        p_name                   = iv_variant
      RECEIVING
        p_ref                    = ro_variant
      EXCEPTIONS
        chkv_not_exists          = 1
        missing_parameter        = 2
        OTHERS                   = 3 ).

    CASE sy-subrc.
      WHEN 1.
        Lcx_abapgit_exception=>raise( |Check variant { iv_variant } doesn't exist| ).
      WHEN 2.
        Lcx_abapgit_exception=>raise( |Parameter missing for check variant { iv_variant }| ).
    ENDCASE.

  ENDMETHOD.
  METHOD decide_run_mode.

    DATA lo_settings TYPE REF TO Lcl_abapgit_settings.
    lo_settings = Lcl_abapgit_persist_factory=>get_settings( )->read( ).

    IF sy-batch = abap_true.
      " We have to disable parallelization in batch because of lock errors.
      rv_run_mode = co_run_mode-run_via_rfc.
    ELSEIF lo_settings->get_parallel_proc_disabled( ) = abap_false.
      rv_run_mode = co_run_mode-run_loc_parallel.
    ELSE.
      rv_run_mode = co_run_mode-run_via_rfc.
    ENDIF.

  ENDMETHOD.
  METHOD filter_inspection.

    " Remove findings in LSVIM* includes which are part of generated maintenance screens
    DELETE ct_list WHERE sobjtype = 'PROG' AND sobjname CP 'LSVIM*'.

  ENDMETHOD.
  METHOD run_inspection.

    io_inspection->run(
      EXPORTING
        p_howtorun            = mv_run_mode
      EXCEPTIONS
        invalid_check_version = 1
        OTHERS                = 2 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Code inspector run failed. Subrc = { sy-subrc }| ).
    ENDIF.

    io_inspection->plain_list( IMPORTING p_list = rt_list ).

    filter_inspection( CHANGING ct_list = rt_list ).

    SORT rt_list BY objtype objname test code sobjtype sobjname line col.

    DELETE ADJACENT DUPLICATES FROM rt_list.

  ENDMETHOD.
  METHOD skip_object.

    DATA ls_program_type TYPE subc.

    CASE is_obj-objtype.
      WHEN 'PROG'.

        SELECT SINGLE subc
          INTO ls_program_type
          FROM trdir
          WHERE name = is_obj-objname.

        rv_skip = boolc( ls_program_type = 'I' ). " Include program.

      WHEN OTHERS.
        rv_skip = abap_false.

    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_code_inspector~get_summary.
    rv_summary = mv_summary.
  ENDMETHOD.
  METHOD Lif_abapgit_code_inspector~is_successful.

    rv_success = mv_success.

  ENDMETHOD.
  METHOD Lif_abapgit_code_inspector~run.

    DATA: lo_set     TYPE REF TO cl_ci_objectset,
          lo_variant TYPE REF TO cl_ci_checkvariant,
          lv_count   TYPE i,
          lt_list    TYPE scit_alvlist,
          ls_list    LIKE LINE OF lt_list,
          ls_result  LIKE LINE OF rt_list,
          lo_timer   TYPE REF TO Lcl_abapgit_timer,
          lx_error   TYPE REF TO Lcx_abapgit_exception.

    TRY.
        lo_set = create_objectset( ).

        lv_count = lines( lo_set->iobjlst-objects ).
        IF lv_count = 0.
          " no objects, nothing to check
          RETURN.
        ENDIF.

        lo_timer = Lcl_abapgit_timer=>create( iv_count = lv_count )->start( ).

        lo_variant = create_variant( iv_variant ).

        mo_inspection = create_inspection(
          io_set     = lo_set
          io_variant = lo_variant ).

        lt_list = run_inspection( mo_inspection ).

        cleanup( lo_set ).

        LOOP AT lt_list INTO ls_list.
          MOVE-CORRESPONDING ls_list TO ls_result.
          INSERT ls_result INTO TABLE rt_list.
        ENDLOOP.

        IF iv_save = abap_true.
          READ TABLE rt_list TRANSPORTING NO FIELDS WITH KEY kind = 'E'.
          mv_success = boolc( sy-subrc <> 0 ).
        ENDIF.

      CATCH Lcx_abapgit_exception INTO lx_error.

        " ensure cleanup
        cleanup( lo_set ).
        Lcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

    mv_summary = lo_timer->end( ).

  ENDMETHOD.
  METHOD Lif_abapgit_code_inspector~list_global_variants.

    SELECT scichkv_hd~checkvname AS name
      scichkv_tx~text AS description
      INTO TABLE rt_list
      FROM scichkv_hd
      LEFT OUTER JOIN scichkv_tx
      ON scichkv_hd~checkvid = scichkv_tx~checkvid
      AND scichkv_hd~ciuser  = scichkv_tx~ciuser
      AND scichkv_tx~language = sy-langu
      WHERE scichkv_hd~ciuser = space
      ORDER BY name.

  ENDMETHOD.
  METHOD Lif_abapgit_code_inspector~validate_check_variant.

    cl_ci_checkvariant=>get_ref(
      EXPORTING
        p_user                   = ''
        p_name                   = iv_check_variant_name
      EXCEPTIONS
        chkv_not_exists          = 1
        missing_parameter        = 2
        OTHERS                   = 3 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |No valid check variant { iv_check_variant_name  }| ).
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_CODE_INSPECTOR implementation

*>>>>>>> ZCL_ABAPGIT_CTS_API <<<<<<<*

*"* macro definitions
*include zcl_abapgit_cts_api===========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_cts_api===========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_CTS_API implementation.
*"* method's implementations
*include methods.
  METHOD get_current_transport_for_obj.
    DATA: lv_object_lockable   TYPE abap_bool,
          lv_locked            TYPE abap_bool,
          lv_transport_request TYPE trkorr,
          lv_task              TYPE trkorr,
          lv_tr_object_name    TYPE trobj_name.

    lv_tr_object_name = iv_object_name.

    CALL FUNCTION 'TR_CHECK_OBJECT_LOCK'
      EXPORTING
        wi_pgmid             = iv_program_id
        wi_object            = iv_object_type
        wi_objname           = lv_tr_object_name
      IMPORTING
        we_lockable_object   = lv_object_lockable
        we_locked            = lv_locked
        we_lock_order        = lv_transport_request
        we_lock_task         = lv_task
      EXCEPTIONS
        empty_key            = 1
        no_systemname        = 2
        no_systemtype        = 3
        unallowed_lock_order = 4
        OTHERS               = 5.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF lv_locked = abap_false.
      Lcx_abapgit_exception=>raise( |Object { iv_program_id }-{ iv_object_type }-{ iv_object_name } is not locked| ).
    ENDIF.

    IF lv_object_lockable = abap_false.
      Lcx_abapgit_exception=>raise( |Object type { iv_program_id }-{ iv_object_type } not lockable| ).
    ENDIF.

    rv_transport = lv_transport_request.

  ENDMETHOD.
  METHOD get_current_transport_from_db.

    " This method is used for objects that are included in transports but not locked
    " for example, namespaces (NSPC)
    SELECT SINGLE a~trkorr FROM e070 AS a JOIN e071 AS b ON a~trkorr = b~trkorr
      INTO rv_transport
      WHERE ( a~trstatus = 'D' OR a~trstatus = 'L' )
        AND a~trfunction <> 'G'
        AND b~pgmid = iv_program_id AND b~object = iv_object_type AND b~obj_name = iv_object_name.

  ENDMETHOD.
  METHOD is_object_locked_in_transport.
    DATA: ls_object_key        TYPE e071,
          lv_type_check_result TYPE c LENGTH 1,
          ls_lock_key          TYPE tlock_int,
          lv_lock_flag         TYPE c LENGTH 1.

    ls_object_key-pgmid = iv_program_id.
    ls_object_key-object = iv_object_type.
    ls_object_key-obj_name = iv_object_name.

    CALL FUNCTION 'TR_CHECK_TYPE'
      EXPORTING
        wi_e071     = ls_object_key
      IMPORTING
        pe_result   = lv_type_check_result
        we_lock_key = ls_lock_key.

    IF lv_type_check_result <> 'L'.
      Lcx_abapgit_exception=>raise( |Object type { iv_program_id }-{ iv_object_type } not lockable| ).
    ENDIF.

    CALL FUNCTION 'TRINT_CHECK_LOCKS'
      EXPORTING
        wi_lock_key = ls_lock_key
      IMPORTING
        we_lockflag = lv_lock_flag
      EXCEPTIONS
        empty_key   = 1
        OTHERS      = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    rv_locked = boolc( lv_lock_flag <> space ).
  ENDMETHOD.
  METHOD is_object_type_lockable.
    DATA: ls_object_key        TYPE e071,
          lv_type_check_result TYPE c LENGTH 1.

    ls_object_key-pgmid = iv_program_id.
    ls_object_key-object = iv_object_type.
    ls_object_key-obj_name = '_'. " Dummy value #2071

    CALL FUNCTION 'TR_CHECK_TYPE'
      EXPORTING
        wi_e071   = ls_object_key
      IMPORTING
        pe_result = lv_type_check_result.

    rv_lockable = boolc( lv_type_check_result = 'L' ).
  ENDMETHOD.
  METHOD is_object_type_transportable.
    DATA: ls_object_key        TYPE e071,
          lv_type_check_result TYPE c LENGTH 1.

    ls_object_key-pgmid = iv_program_id.
    ls_object_key-object = iv_object_type.
    ls_object_key-obj_name = '_'. " Dummy value #2071

    CALL FUNCTION 'TR_CHECK_TYPE'
      EXPORTING
        wi_e071   = ls_object_key
      IMPORTING
        pe_result = lv_type_check_result.

    rv_transportable = boolc( lv_type_check_result CA 'RTL' ).
  ENDMETHOD.
  METHOD Lif_abapgit_cts_api~get_r3tr_obj_for_limu_obj.

    CLEAR ev_object.
    CLEAR ev_obj_name.

    IF iv_object = 'MESS'.
      ev_object = 'MSAG'.
      ev_obj_name = substring( val = iv_obj_name
                               len = strlen( iv_obj_name ) - 3 ).
      RETURN.
    ENDIF.

    CALL FUNCTION 'GET_R3TR_OBJECT_FROM_LIMU_OBJ'
      EXPORTING
        p_limu_objtype = iv_object
        p_limu_objname = iv_obj_name
      IMPORTING
        p_r3tr_objtype = ev_object
        p_r3tr_objname = ev_obj_name
      EXCEPTIONS
        no_mapping     = 1
        OTHERS         = 2.
    IF sy-subrc <> 0 OR ev_obj_name IS INITIAL.
      Lcx_abapgit_exception=>raise( |No R3TR Object found for { iv_object } { iv_obj_name }| ).
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_cts_api~get_transports_for_list.

    DATA lv_request TYPE trkorr.
    DATA lt_tlock TYPE SORTED TABLE OF tlock WITH NON-UNIQUE KEY object hikey.
    DATA ls_object_key TYPE e071.
    DATA lv_type_check_result TYPE c LENGTH 1.
    DATA ls_lock_key TYPE tlock_int.
    DATA ls_transport LIKE LINE OF rt_transports.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF it_items.
    FIELD-SYMBOLS <ls_tlock> LIKE LINE OF lt_tlock.

* Workarounds to improve performance, note that IT_ITEMS might
* contain 1000s of rows, see standard logic in function module
* TR_CHECK_OBJECT_LOCK

* avoid database lookups in TLOCK for each item,
    SELECT * FROM tlock INTO TABLE lt_tlock.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT it_items ASSIGNING <ls_item>.
      CLEAR lv_request.

      ls_object_key-pgmid = 'R3TR'.
      ls_object_key-object = <ls_item>-obj_type.
      ls_object_key-obj_name = <ls_item>-obj_name.

      CALL FUNCTION 'TR_CHECK_TYPE'
        EXPORTING
          wi_e071     = ls_object_key
        IMPORTING
          we_lock_key = ls_lock_key
          pe_result   = lv_type_check_result.

      IF lv_type_check_result = 'L'.
        LOOP AT lt_tlock ASSIGNING <ls_tlock>
            WHERE object = ls_lock_key-obj
            AND hikey >= ls_lock_key-low
            AND lokey <= ls_lock_key-hi.                  "#EC PORTABLE
          lv_request = <ls_tlock>-trkorr.
          EXIT.
        ENDLOOP.
      ELSEIF is_object_type_transportable( <ls_item>-obj_type ) = abap_true.
        lv_request = get_current_transport_from_db(
          iv_object_type = <ls_item>-obj_type
          iv_object_name = <ls_item>-obj_name ).
      ENDIF.

      IF lv_request IS NOT INITIAL.
        ls_transport-obj_type = <ls_item>-obj_type.
        ls_transport-obj_name = <ls_item>-obj_name.
        ls_transport-trkorr = lv_request.
        INSERT ls_transport INTO TABLE rt_transports.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_cts_api~get_transport_for_object.

    IF is_item-obj_type IS NOT INITIAL AND is_item-obj_name IS NOT INITIAL.

      IF is_object_type_lockable( is_item-obj_type ) = abap_true AND
         is_object_locked_in_transport(
           iv_object_type = is_item-obj_type
           iv_object_name = is_item-obj_name ) = abap_true.

        rv_transport = get_current_transport_for_obj(
          iv_object_type = is_item-obj_type
          iv_object_name = is_item-obj_name ).

      ELSEIF is_object_type_transportable( is_item-obj_type ) = abap_true.

        rv_transport = get_current_transport_from_db(
          iv_object_type = is_item-obj_type
          iv_object_name = is_item-obj_name ).

      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_cts_api~is_chrec_possible_for_package.
    IF iv_package IS NOT INITIAL.
      rv_possible = Lcl_abapgit_factory=>get_sap_package( iv_package )->are_changes_recorded_in_tr_req( ).
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_cts_api~read_description.

    SELECT SINGLE as4text FROM e07t
      INTO rv_description
      WHERE trkorr = iv_trkorr
      AND langu = sy-langu ##SUBRC_OK.

  ENDMETHOD.
  METHOD Lif_abapgit_cts_api~read_user.

    SELECT SINGLE as4user FROM e070 INTO rv_uname
      WHERE trkorr = iv_trkorr ##SUBRC_OK.

  ENDMETHOD.
  METHOD Lif_abapgit_cts_api~confirm_transport_messages.

    TYPES: BEGIN OF ty_s_message,
             id TYPE symsgid,
             ty TYPE symsgty,
             no TYPE symsgno,
             v1 TYPE symsgv,
             v2 TYPE symsgv,
             v3 TYPE symsgv,
             v4 TYPE symsgv,
           END OF ty_s_message.

    DATA ls_message TYPE ty_s_message.

    FIELD-SYMBOLS: <lt_confirmed_messages> TYPE STANDARD TABLE.

    IF mv_confirm_transp_msgs_called = abap_true.
      RETURN.
    ENDIF.

    " remember the call to avoid duplicates in GT_CONFIRMED_MESSAGES
    mv_confirm_transp_msgs_called = abap_true.


    " Auto-confirm certain messages (requires SAP Note 1609940)
    PERFORM dummy IN PROGRAM saplstrd IF FOUND.  "load function group STRD once into memory

    ASSIGN ('(SAPLSTRD)GT_CONFIRMED_MESSAGES') TO <lt_confirmed_messages>.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Object can only be created in package of namespace
    ls_message-id = 'TR'.
    ls_message-no = '007'.
    INSERT ls_message INTO TABLE <lt_confirmed_messages>.

    " Original system set to "SAP"
    ls_message-id = 'TR'.
    ls_message-no = '013'.
    INSERT ls_message INTO TABLE <lt_confirmed_messages>.

    " Make repairs in foreign namespaces only if they are urgent
    ls_message-id = 'TR'.
    ls_message-no = '852'.
    INSERT ls_message INTO TABLE <lt_confirmed_messages>.

    " Make repairs in foreign namespaces only if they are urgent
    ls_message-id = 'TK'.
    ls_message-no = '016'.
    INSERT ls_message INTO TABLE <lt_confirmed_messages>.

    rv_messages_confirmed = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_cts_api~create_transport_entries.

    DATA lt_tables      TYPE tredt_objects.
    DATA lt_table_keys  TYPE STANDARD TABLE OF e071k.
    DATA lv_with_dialog TYPE abap_bool.

    cl_table_utilities_brf=>create_transport_entries(
      EXPORTING
        it_table_ins = it_table_ins
        it_table_upd = it_table_upd
        it_table_del = it_table_del
        iv_tabname   = iv_tabname
      CHANGING
        ct_e071      = lt_tables
        ct_e071k     = lt_table_keys ).

    " cl_table_utilities_brf=>write_transport_entries does not allow passing a request

    CALL FUNCTION 'TR_OBJECTS_CHECK'
      TABLES
        wt_ko200                = lt_tables
      EXCEPTIONS
        cancel_edit_other_error = 1
        show_only_other_error   = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF iv_transport IS INITIAL.
      lv_with_dialog = abap_true.
    ENDIF.

    CALL FUNCTION 'TRINT_OBJECTS_CHECK_AND_INSERT'
      EXPORTING
        iv_order       = iv_transport
        iv_with_dialog = lv_with_dialog
      CHANGING
        ct_ko200       = lt_tables
        ct_e071k       = lt_table_keys
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_cts_api~insert_transport_object.

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = iv_obj_name
        object_class        = iv_object
        devclass            = iv_package
        master_language     = iv_language
        mode                = iv_mode
        global_lock         = abap_true
        suppress_dialog     = abap_true
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_cts_api~list_open_requests_by_user.

    TYPES: BEGIN OF ty_e070,
             trkorr     TYPE e070-trkorr,
             trfunction TYPE e070-trfunction,
             strkorr    TYPE e070-strkorr,
           END OF ty_e070.
    DATA lt_e070 TYPE STANDARD TABLE OF ty_e070 WITH DEFAULT KEY.

* find all tasks first
    SELECT trkorr trfunction strkorr
      FROM e070 INTO TABLE lt_e070
      WHERE as4user = sy-uname
      AND trstatus = Lif_abapgit_cts_api=>c_transport_status-modifiable
      AND strkorr <> ''
      ORDER BY PRIMARY KEY.

    IF lines( lt_e070 ) > 0.
      SELECT trkorr FROM e070
        INTO TABLE rt_trkorr
        FOR ALL ENTRIES IN lt_e070
        WHERE trkorr = lt_e070-strkorr
        AND trfunction = Lif_abapgit_cts_api=>c_transport_type-wb_request.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_cts_api~read.

    DATA ls_request TYPE trwbo_request.
    DATA ls_key     LIKE LINE OF ls_request-keys.

    FIELD-SYMBOLS <ls_key> LIKE LINE OF rs_request-keys.


    ls_request-h-trkorr = iv_trkorr.

    CALL FUNCTION 'TRINT_READ_REQUEST'
      EXPORTING
        iv_read_e070       = abap_true
        iv_read_e07t       = abap_true
        iv_read_e070c      = abap_true
        iv_read_e070m      = abap_true
        iv_read_objs_keys  = abap_true
        iv_read_objs       = abap_true
        iv_read_attributes = abap_true
      CHANGING
        cs_request         = ls_request
      EXCEPTIONS
        error_occured      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

* move to output structure
    rs_request-trstatus = ls_request-h-trstatus.
    LOOP AT ls_request-keys INTO ls_key.
      APPEND INITIAL LINE TO rs_request-keys ASSIGNING <ls_key>.
      MOVE-CORRESPONDING ls_key TO <ls_key>.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_cts_api~validate_transport_request.

    CONSTANTS:
      BEGIN OF c_tr_status,
        modifiable           TYPE trstatus VALUE 'D',
        modifiable_protected TYPE trstatus VALUE 'L',
      END OF c_tr_status.

    DATA ls_request TYPE Lif_abapgit_cts_api=>ty_transport_data.

    ls_request = Lif_abapgit_cts_api~read( iv_transport_request ).

    IF ls_request-trstatus <> c_tr_status-modifiable
        AND ls_request-trstatus <> c_tr_status-modifiable_protected.
      " Task/request &1 has already been released
      MESSAGE e064(tk) WITH iv_transport_request INTO Lcx_abapgit_exception=>null.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_cts_api~list_r3tr_by_request.

    TYPES: BEGIN OF ty_contents,
             trkorr   TYPE e071-trkorr,
             as4pos   TYPE e071-as4pos,
             pgmid    TYPE e071-pgmid,
             object   TYPE e071-object,
             obj_name TYPE e071-obj_name,
           END OF ty_contents.

    DATA lt_tasks    TYPE STANDARD TABLE OF trkorr WITH DEFAULT KEY.
    DATA lt_contents TYPE STANDARD TABLE OF ty_contents WITH DEFAULT KEY.
    DATA ls_contents LIKE LINE OF lt_contents.
    DATA ls_list     LIKE LINE OF rt_list.


    SELECT trkorr FROM e070 INTO TABLE lt_tasks
      WHERE strkorr = iv_request
      ORDER BY PRIMARY KEY.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT trkorr as4pos pgmid object obj_name FROM e071
      INTO TABLE lt_contents
      FOR ALL ENTRIES IN lt_tasks
      WHERE trkorr = lt_tasks-table_line
      ORDER BY PRIMARY KEY.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_contents INTO ls_contents.
      CASE ls_contents-pgmid.
        WHEN 'R3TR'.
          ls_list-object = ls_contents-object.
          ls_list-obj_name = ls_contents-obj_name.
          INSERT ls_list INTO TABLE rt_list.
        WHEN 'LIMU'.
          TRY.
              Lif_abapgit_cts_api~get_r3tr_obj_for_limu_obj(
                EXPORTING
                  iv_object   = ls_contents-object
                  iv_obj_name = ls_contents-obj_name
                IMPORTING
                  ev_object   = ls_list-object
                  ev_obj_name = ls_list-obj_name ).
              INSERT ls_list INTO TABLE rt_list.
            CATCH Lcx_abapgit_exception.
          ENDTRY.
      ENDCASE.
    ENDLOOP.

    SORT rt_list BY object obj_name.
    DELETE ADJACENT DUPLICATES FROM rt_list COMPARING object obj_name.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_CTS_API implementation

*>>>>>>> ZCL_ABAPGIT_DATA_DESERIALIZER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_data_deserializer=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_data_deserializer=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_data_deserializer=ccau.
*CLASS SHRITEFUH64VYIPO5IWUYB3KWWPSFY DEFINITION DEFERRED.
*CLASS zcl_abapgit_data_deserializer DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KWWPSFY.




class LCL_ABAPGIT_DATA_DESERIALIZER implementation.
*"* method's implementations
*include methods.
  METHOD convert_json_to_itab.

    DATA lo_ajson TYPE REF TO Lcl_abapgit_ajson.
    DATA lx_ajson TYPE REF TO Lcx_abapgit_ajson_error.

    FIELD-SYMBOLS <lg_tab> TYPE ANY TABLE.

    ASSIGN ir_data->* TO <lg_tab>.

    TRY.
        lo_ajson = Lcl_abapgit_ajson=>parse( Lcl_abapgit_convert=>xstring_to_string_utf8( is_file-data ) ).
        lo_ajson->Lif_abapgit_ajson~to_abap( IMPORTING ev_container = <lg_tab> ).
      CATCH Lcx_abapgit_ajson_error INTO lx_ajson.
        Lcx_abapgit_exception=>raise( lx_ajson->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
  METHOD preview_database_changes.

* method currently distinguishes between records be deleted and inserted (comparison of complete record)

    FIELD-SYMBOLS <lg_old> TYPE ANY TABLE.
    FIELD-SYMBOLS <lg_new> TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_del> TYPE any.
    FIELD-SYMBOLS <ls_ins> TYPE any.
    FIELD-SYMBOLS <lg_del> TYPE ANY TABLE.
    FIELD-SYMBOLS <lg_ins> TYPE ANY TABLE.
    FIELD-SYMBOLS <lg_upd> TYPE ANY TABLE.

    ASSIGN ir_db_data->* TO <lg_old>.
    ASSIGN ir_lc_data->* TO <lg_new>.

    rs_result-type = Lif_abapgit_data_config=>c_data_type-tabu.
    rs_result-name = iv_name.
    rs_result-deletes = Lcl_abapgit_data_utils=>build_table_itab( iv_name ).
    rs_result-inserts = Lcl_abapgit_data_utils=>build_table_itab( iv_name ).
    rs_result-updates = Lcl_abapgit_data_utils=>build_table_itab( iv_name ).
    ASSIGN rs_result-deletes->* TO <lg_del>.
    ASSIGN rs_result-inserts->* TO <lg_ins>.
    ASSIGN rs_result-updates->* TO <lg_upd>.

    <lg_del> = <lg_old>.
    <lg_ins> = <lg_new>.

    " Remove identical records
    LOOP AT <lg_del> ASSIGNING <ls_del>.
      READ TABLE <lg_ins> ASSIGNING <ls_ins> FROM <ls_del>.
      IF sy-subrc = 0.
        IF <ls_del> <> <ls_ins>.
          " Identical key but not identical component values
          INSERT <ls_ins> INTO TABLE <lg_upd>.
        ENDIF.
        DELETE TABLE <lg_del> FROM <ls_del>.
        DELETE TABLE <lg_ins> FROM <ls_ins>.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD read_database_table.

    DATA lv_where LIKE LINE OF it_where.

    FIELD-SYMBOLS <lg_tab> TYPE ANY TABLE.

    rr_data = Lcl_abapgit_data_utils=>build_table_itab( iv_name ).
    ASSIGN rr_data->* TO <lg_tab>.

    LOOP AT it_where INTO lv_where.
      SELECT * FROM (iv_name) APPENDING TABLE <lg_tab> WHERE (lv_where) ORDER BY PRIMARY KEY.
    ENDLOOP.
    IF lines( it_where ) = 0.
      SELECT * FROM (iv_name) INTO TABLE <lg_tab> ORDER BY PRIMARY KEY.
    ENDIF.

  ENDMETHOD.
  METHOD write_database_table.

    FIELD-SYMBOLS <lg_del> TYPE ANY TABLE.
    FIELD-SYMBOLS <lg_ins> TYPE ANY TABLE.
    FIELD-SYMBOLS <lg_upd> TYPE ANY TABLE.

    IF Lcl_abapgit_data_utils=>does_table_exist( iv_name ) = abap_false.
      Lcx_abapgit_exception=>raise( |Table { iv_name } not found for data deserialization| ).
    ENDIF.

    ASSIGN ir_del->* TO <lg_del>.
    ASSIGN ir_ins->* TO <lg_ins>.
    ASSIGN ir_upd->* TO <lg_upd>.

    IF lines( <lg_del> ) > 0.
      DELETE (iv_name) FROM TABLE <lg_del>.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Error deleting { lines( <lg_del> ) } records from table { iv_name }| ).
      ENDIF.
    ENDIF.

    IF lines( <lg_ins> ) > 0.
      INSERT (iv_name) FROM TABLE <lg_ins>.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Error inserting { lines( <lg_ins> ) } records into table { iv_name }| ).
      ENDIF.
    ENDIF.

    IF lines( <lg_upd> ) > 0.
      UPDATE (iv_name) FROM TABLE <lg_upd>.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Error updating { lines( <lg_upd> ) } records into table { iv_name }| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_data_deserializer~actualize.

* this method updates the database

    DATA ls_result  LIKE LINE OF it_result.
    DATA li_cts_api TYPE REF TO Lif_abapgit_cts_api.

    FIELD-SYMBOLS:
      <lt_ins> TYPE ANY TABLE,
      <lt_del> TYPE ANY TABLE,
      <lt_upd> TYPE ANY TABLE.

    LOOP AT it_result INTO ls_result.
      ASSERT ls_result-type = Lif_abapgit_data_config=>c_data_type-tabu. " todo
      ASSERT ls_result-name IS NOT INITIAL.

      " Did the user flagged this object for update?
      READ TABLE is_checks-overwrite TRANSPORTING NO FIELDS
        WITH KEY object_type_and_name
        COMPONENTS
          obj_type = ls_result-type
          obj_name = ls_result-name
          decision = Lif_abapgit_definitions=>c_yes.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF is_table_allowed_to_edit( ls_result ) = abap_false.
        Lcx_abapgit_exception=>raise( |Table { ls_result-name } not supported for updating data| ).
      ENDIF.

      write_database_table(
        iv_name = ls_result-name
        ir_del  = ls_result-deletes
        ir_ins  = ls_result-inserts
        ir_upd  = ls_result-updates ).

      ASSIGN ls_result-inserts->* TO <lt_ins>.
      ASSIGN ls_result-deletes->* TO <lt_del>.
      ASSIGN ls_result-updates->* TO <lt_upd>.

      IF Lcl_abapgit_data_utils=>is_customizing_table( ls_result-name ) = abap_true.
        IF li_cts_api IS INITIAL.
          li_cts_api = Lcl_abapgit_factory=>get_cts_api( ).
        ENDIF.

        li_cts_api->create_transport_entries(
          iv_transport = is_checks-customizing-transport
          it_table_ins = <lt_ins>
          it_table_upd = <lt_upd>
          it_table_del = <lt_del>
          iv_tabname   = |{ ls_result-name }| ).
      ENDIF.

      INSERT ls_result-file INTO TABLE rt_accessed_files. " data file
      INSERT ls_result-config INTO TABLE rt_accessed_files. " config file
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_data_deserializer~deserialize.

* this method does not persist any changes to the database

    DATA lt_configs TYPE Lif_abapgit_data_config=>ty_config_tt.
    DATA ls_config  LIKE LINE OF lt_configs.
    DATA lr_lc_data TYPE REF TO data.
    DATA lr_db_data TYPE REF TO data.
    DATA ls_file    LIKE LINE OF it_files.
    DATA ls_result  LIKE LINE OF rt_result.

    lt_configs = ii_config->get_configs( ).

    LOOP AT lt_configs INTO ls_config.
      ASSERT ls_config-type = Lif_abapgit_data_config=>c_data_type-tabu. " todo
      ASSERT ls_config-name IS NOT INITIAL.

      lr_lc_data = Lcl_abapgit_data_utils=>build_table_itab( ls_config-name ).

      READ TABLE it_files INTO ls_file
        WITH KEY file_path
        COMPONENTS path     = Lif_abapgit_data_config=>c_default_path
                   filename = Lcl_abapgit_data_utils=>build_data_filename( ls_config ).
      IF sy-subrc = 0.
        convert_json_to_itab(
          ir_data = lr_lc_data
          is_file = ls_file ).

        lr_db_data = read_database_table(
          iv_name  = ls_config-name
          it_where = ls_config-where ).

        ls_result = preview_database_changes(
          iv_name    = ls_config-name
          ir_lc_data = lr_lc_data
          ir_db_data = lr_db_data ).

        MOVE-CORRESPONDING ls_file TO ls_result-file. " data file

        READ TABLE it_files INTO ls_file
          WITH KEY file_path
          COMPONENTS path     = Lif_abapgit_data_config=>c_default_path
                     filename = Lcl_abapgit_data_utils=>build_config_filename( ls_config ).
        ASSERT sy-subrc = 0.

        MOVE-CORRESPONDING ls_file TO ls_result-config. " config file

        INSERT ls_result INTO TABLE rt_result.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD determine_transport_request.

    DATA li_exit TYPE REF TO Lif_abapgit_exit.

    li_exit = Lcl_abapgit_exit=>get_instance( ).

    " Use transport from repo settings if maintained, or determine via user exit.
    " If transport keeps empty here, it'll requested later via popup.
    rv_transport_request = io_repo->get_local_settings( )-customizing_request.

    li_exit->determine_transport_request(
      EXPORTING
        io_repo              = io_repo
        iv_transport_type    = iv_transport_type
      CHANGING
        cv_transport_request = rv_transport_request ).

  ENDMETHOD.
  METHOD Lif_abapgit_data_deserializer~deserialize_check.

    DATA lt_configs TYPE Lif_abapgit_data_config=>ty_config_tt.

    lt_configs = ii_config->get_configs( ).

    IF lt_configs IS NOT INITIAL.
      rs_checks-required     = abap_true.
      rs_checks-type-request = Lif_abapgit_cts_api=>c_transport_type-cust_request.
      rs_checks-type-task    = Lif_abapgit_cts_api=>c_transport_type-cust_task.
      rs_checks-transport    = determine_transport_request(
                                 io_repo           = io_repo
                                 iv_transport_type = rs_checks-type ).
    ENDIF.

  ENDMETHOD.
  METHOD is_table_allowed_to_edit.

    " Is the object supported (by default or based on exit)?
    rv_allowed_to_edit = Lcl_abapgit_data_factory=>get_supporter( )->is_object_supported(
      iv_type = is_result-type
      iv_name = is_result-name ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_DATA_DESERIALIZER implementation

*>>>>>>> ZCL_ABAPGIT_DATA_SERIALIZER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_data_serializer===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_data_serializer===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_data_serializer===ccau.
*CLASS SHRITEFUH64VYIPO5IWUYB3KWWRSFY DEFINITION DEFERRED.
*CLASS zcl_abapgit_data_serializer DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KWWRSFY.




class LCL_ABAPGIT_DATA_SERIALIZER implementation.
*"* method's implementations
*include methods.
  METHOD convert_itab_to_json.

    DATA lo_ajson  TYPE REF TO Lcl_abapgit_ajson.
    DATA lv_string TYPE string.
    DATA lx_ajson  TYPE REF TO Lcx_abapgit_ajson_error.

    FIELD-SYMBOLS <lg_tab> TYPE ANY TABLE.

    ASSIGN ir_data->* TO <lg_tab>.

    TRY.
        lo_ajson = Lcl_abapgit_ajson=>create_empty( ).
        lo_ajson->keep_item_order( ).
        lo_ajson->set(
          iv_path = '/'
          iv_val = <lg_tab> ).

        IF iv_skip_initial = abap_true.
          lo_ajson = Lcl_abapgit_ajson=>create_from(
            ii_source_json = lo_ajson
            ii_filter = Lcl_abapgit_ajson_filter_lib=>create_empty_filter( ) ).
        ENDIF.

        lv_string = lo_ajson->stringify( 2 ).
      CATCH Lcx_abapgit_ajson_error INTO lx_ajson.
        Lcx_abapgit_exception=>raise( lx_ajson->get_text( ) ).
    ENDTRY.

    rv_data = Lcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).

  ENDMETHOD.
  METHOD read_database_table.

    DATA lv_records TYPE i.
    DATA lv_where LIKE LINE OF it_where.
    DATA lx_sql TYPE REF TO cx_sy_sql_error.

    FIELD-SYMBOLS <lg_tab> TYPE ANY TABLE.

    rr_data = Lcl_abapgit_data_utils=>build_table_itab( iv_name ).
    ASSIGN rr_data->* TO <lg_tab>.

    TRY.
        LOOP AT it_where INTO lv_where.
          SELECT * FROM (iv_name) APPENDING TABLE <lg_tab> WHERE (lv_where) ORDER BY PRIMARY KEY.
        ENDLOOP.
        IF lines( it_where ) = 0.
          SELECT * FROM (iv_name) INTO TABLE <lg_tab> ORDER BY PRIMARY KEY.
        ENDIF.
      CATCH cx_sy_sql_error INTO lx_sql.
        Lcx_abapgit_exception=>raise(
          iv_text     = lx_sql->get_text( )
          ix_previous = lx_sql ).
    ENDTRY.

    lv_records = lines( <lg_tab> ).
    IF lv_records > c_max_records.
      Lcx_abapgit_exception=>raise( |Too many records selected from table { iv_name
        } (selected { lv_records }, max { c_max_records })| ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_data_serializer~serialize.

    DATA lt_configs TYPE Lif_abapgit_data_config=>ty_config_tt.
    DATA ls_config  LIKE LINE OF lt_configs.
    DATA ls_file    LIKE LINE OF rt_files.
    DATA lr_data    TYPE REF TO data.

    ls_file-path = Lif_abapgit_data_config=>c_default_path.
    lt_configs = ii_config->get_configs( ).

    LOOP AT lt_configs INTO ls_config.
      ASSERT ls_config-type = Lif_abapgit_data_config=>c_data_type-tabu. " todo
      ASSERT ls_config-name IS NOT INITIAL.

      IF Lcl_abapgit_data_utils=>does_table_exist( ls_config-name ) = abap_true.
        lr_data = read_database_table(
          iv_name  = ls_config-name
          it_where = ls_config-where ).

        ls_file-data = convert_itab_to_json(
          ir_data         = lr_data
          iv_skip_initial = ls_config-skip_initial ).
      ELSE.
        ls_file-data = Lcl_abapgit_convert=>string_to_xstring_utf8( '[]' ).
      ENDIF.

      ls_file-filename = Lcl_abapgit_data_utils=>build_data_filename( ls_config ).
      ls_file-sha1 = Lcl_abapgit_hash=>sha1_blob( ls_file-data ).
      APPEND ls_file TO rt_files.
    ENDLOOP.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_DATA_SERIALIZER implementation

*>>>>>>> ZCL_ABAPGIT_DATA_SUPPORTER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_data_supporter====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_data_supporter====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_data_supporter====ccau.
CLASS SHRITEFUH64VYIPO5IWUYB3KWWTSFY DEFINITION.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_data_supporter.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KWWTSFY IMPLEMENTATION.
  METHOD Lif_abapgit_data_supporter~is_object_supported.

    IF iv_type = Lif_abapgit_data_config=>c_data_type-tabu AND iv_name = 'T005'.
      rv_supported = abap_true.
    ENDIF.

  ENDMETHOD.

ENDCLASS.


*CLASS zcl_abapgit_data_supporter DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KWWVSFY.


class LCL_ABAPGIT_DATA_SUPPORTER implementation.
*"* method's implementations
*include methods.
  METHOD get_supported_objects.

    DATA:
      lt_tables  TYPE STANDARD TABLE OF tabname,
      lv_tabname TYPE tabname,
      ls_object  LIKE LINE OF mt_supported_objects,
      li_exit    TYPE REF TO Lif_abapgit_exit.

    " For safety reasons, by default only customer-defined customizing tables are supported
    SELECT dd02l~tabname
      FROM dd09l JOIN dd02l
        ON dd09l~tabname = dd02l~tabname
        AND dd09l~as4local = dd02l~as4local
        AND dd09l~as4vers = dd02l~as4vers
      INTO TABLE lt_tables
      WHERE dd02l~tabclass = 'TRANSP'
        AND dd09l~tabart = 'APPL2'
        AND dd09l~as4user <> 'SAP'
        AND dd09l~as4local = 'A' "Only active tables
        AND dd02l~contflag = 'C' "Only customizing tables
      ORDER BY dd02l~tabname.

    LOOP AT lt_tables INTO lv_tabname.
      ls_object-type = Lif_abapgit_data_config=>c_data_type-tabu.
      ls_object-name = lv_tabname.
      INSERT ls_object INTO TABLE mt_supported_objects.
    ENDLOOP.

    " The list of supported objects can be enhanced using an exit
    " Name patterns are allowed. For example, TABU T009*
    li_exit = Lcl_abapgit_exit=>get_instance( ).
    li_exit->change_supported_data_objects( CHANGING ct_objects = mt_supported_objects ).

  ENDMETHOD.
  METHOD Lif_abapgit_data_supporter~is_object_supported.

    FIELD-SYMBOLS <ls_object> LIKE LINE OF mt_supported_objects.

    IF mt_supported_objects IS INITIAL.
      get_supported_objects( ).
    ENDIF.

    READ TABLE mt_supported_objects TRANSPORTING NO FIELDS
      WITH TABLE KEY type = iv_type name = iv_name.
    IF sy-subrc = 0.
      rv_supported = abap_true.
    ELSE.
      " Check if object name matches pattern
      LOOP AT mt_supported_objects ASSIGNING <ls_object> WHERE type = iv_type.
        IF iv_name CP <ls_object>-name.
          rv_supported = abap_true.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_DATA_SUPPORTER implementation

*>>>>>>> ZCL_ABAPGIT_DEPENDENCIES <<<<<<<*

*"* macro definitions
*include zcl_abapgit_dependencies======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_dependencies======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_dependencies======ccau.



*CLASS zcl_abapgit_dependencies DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KWW2SFY.


class LCL_ABAPGIT_DEPENDENCIES implementation.
*"* method's implementations
*include methods.
  METHOD get_ddls_dependencies.

    DATA: lt_ddls_name TYPE TABLE OF ddsymtab,
          ls_ddls_name TYPE ddsymtab.

    ls_ddls_name-name = iv_ddls_name.
    INSERT ls_ddls_name INTO TABLE lt_ddls_name.

    PERFORM ('DDLS_GET_DEP') IN PROGRAM ('RADMASDL')
                             TABLES lt_ddls_name rt_dependency.

  ENDMETHOD.
  METHOD resolve.

    DATA: lv_tabclass TYPE dd02l-tabclass.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF ct_tadir.

    " misuse field KORRNUM to fix deletion sequence
    " higher value means later deletion

    LOOP AT ct_tadir ASSIGNING <ls_tadir>.
      CASE <ls_tadir>-object.
        WHEN 'DEVC'.
          " Packages last
          <ls_tadir>-korrnum = '999000'.
        WHEN 'DOMA'.
          <ls_tadir>-korrnum = '900000'.
        WHEN 'SPRX'.
          <ls_tadir>-korrnum = '850000'.
        WHEN 'WEBI'.
          <ls_tadir>-korrnum = '840000'.
        WHEN 'PARA'.
          " PARA after DTEL
          <ls_tadir>-korrnum = '810000'.
        WHEN 'DTEL'.
          <ls_tadir>-korrnum = '800000'.
        WHEN 'SHLP'.
          " SHLP after TABL
          <ls_tadir>-korrnum = '760000'.
        WHEN 'TTYP' OR 'TABL' OR 'VIEW'.
          SELECT SINGLE tabclass FROM dd02l
            INTO lv_tabclass
            WHERE tabname = <ls_tadir>-obj_name
            AND as4local = 'A'
            AND as4vers = '0000'.
          IF sy-subrc = 0 AND lv_tabclass = 'APPEND'.
            " delete append structures before database tables
            <ls_tadir>-korrnum = '730000'.
          ELSE.
            <ls_tadir>-korrnum = '750000'.
          ENDIF.
        WHEN 'ENQU'.
          " ENQU before TABL
          <ls_tadir>-korrnum = '725000'.
        WHEN 'DDLS'.
          " DDLS after DCLS but before other DDIC
          <ls_tadir>-korrnum = '720000'.
        WHEN 'DDLX'.
          " DDLX before DDLS
          <ls_tadir>-korrnum = '719000'.
        WHEN 'AUTH'.
          " AUTH after DCLS
          <ls_tadir>-korrnum = '715000'.
        WHEN 'SUSH'.
          " SUSH after SUSC
          <ls_tadir>-korrnum = '712000'.
        WHEN 'SUSC'.
          " SUSC after SUSO
          <ls_tadir>-korrnum = '711000'.
        WHEN 'SUSO'.
          " SUSO after DCLS
          <ls_tadir>-korrnum = '710000'.
        WHEN 'DCLS'.
          " AUTH and SUSO after DCLS
          <ls_tadir>-korrnum = '705000'.
        WHEN 'IASP'.
          <ls_tadir>-korrnum = '552000'.
        WHEN 'IARP'.
          <ls_tadir>-korrnum = '551000'.
        WHEN 'IATU'.
          <ls_tadir>-korrnum = '550000'.
        WHEN 'ACID'.
          " ACID after PROG/FUGR/CLAS
          <ls_tadir>-korrnum = '300000'.
        WHEN 'FUGR'.
          <ls_tadir>-korrnum = '260000'.
        WHEN 'PROG'.
          " delete includes after main programs
          SELECT COUNT(*) FROM reposrc
            WHERE progname = <ls_tadir>-obj_name
            AND r3state = 'A'
            AND subc = 'I'.
          IF sy-subrc = 0.
            <ls_tadir>-korrnum = '250000'.
          ELSE.
            <ls_tadir>-korrnum = '240000'.
          ENDIF.
        WHEN 'INTF'.
          <ls_tadir>-korrnum = '230000'.
        WHEN 'CLAS'.
          <ls_tadir>-korrnum = '220000'.
        WHEN 'IDOC'.
          <ls_tadir>-korrnum = '200000'.
        WHEN 'IOBJ'.
          <ls_tadir>-korrnum = '195000'.
        WHEN 'ODSO'.
          <ls_tadir>-korrnum = '190000'.
        WHEN 'WDCA'.
          <ls_tadir>-korrnum = '174000'.
        WHEN 'WDYA'.
          <ls_tadir>-korrnum = '173000'.
        WHEN 'WDCC'.
          <ls_tadir>-korrnum = '172000'.
        WHEN 'WDYN'.
          <ls_tadir>-korrnum = '171000'.
        WHEN 'IEXT'.
          <ls_tadir>-korrnum = '150000'.
        WHEN 'SAPC'.
          " SAPC after SICF
          <ls_tadir>-korrnum = '140000'.
        WHEN 'PINF'.
          " PINF before exposed objects
          <ls_tadir>-korrnum = '130000'.
        WHEN OTHERS.
          <ls_tadir>-korrnum = '100000'.
      ENDCASE.
    ENDLOOP.

    resolve_ddic( CHANGING ct_tadir = ct_tadir ).
    resolve_packages( CHANGING ct_tadir = ct_tadir ).

    SORT ct_tadir BY korrnum ASCENDING.

  ENDMETHOD.
  METHOD resolve_ddic.
* this will make sure the deletion sequence of structures/tables work
* in case they have dependencies with .INCLUDE

    TYPES: BEGIN OF ty_edge,
             from TYPE ty_item,
             to   TYPE ty_item,
           END OF ty_edge.

    DATA: lt_nodes        TYPE TABLE OF ty_item,
          lt_edges        TYPE TABLE OF ty_edge,
          lt_findstrings  TYPE TABLE OF rsfind,
          lv_plus         TYPE i VALUE 1,
          lv_find_obj_cls TYPE euobj-id,
          lv_index        TYPE i,
          lv_before       TYPE i,
          lt_founds       TYPE TABLE OF rsfindlst,
          lt_scope        TYPE STANDARD TABLE OF seu_obj,
          lt_dependency   TYPE ty_dedenpencies.

    FIELD-SYMBOLS: <ls_tadir_ddls>      TYPE Lif_abapgit_definitions=>ty_tadir,
                   <ls_dependency>      TYPE ty_dependency,
                   <ls_tadir_dependent> TYPE Lif_abapgit_definitions=>ty_tadir,
                   <ls_tadir>           LIKE LINE OF ct_tadir,
                   <ls_edge>            LIKE LINE OF lt_edges,
                   <ls_found>           LIKE LINE OF lt_founds,
                   <ls_node>            LIKE LINE OF lt_nodes.

    " build nodes
    LOOP AT ct_tadir ASSIGNING <ls_tadir>
        WHERE object = 'TABL'
        OR object = 'VIEW'
        OR object = 'TTYP'.
      APPEND INITIAL LINE TO lt_nodes ASSIGNING <ls_node>.
      <ls_node>-obj_name = <ls_tadir>-obj_name.
      <ls_node>-obj_type = <ls_tadir>-object.
    ENDLOOP.

    APPEND 'TABL' TO lt_scope.
    APPEND 'VIEW' TO lt_scope.
    APPEND 'STRU' TO lt_scope.
    APPEND 'TTYP' TO lt_scope.

    " build edges
    LOOP AT lt_nodes ASSIGNING <ls_node>.

      CLEAR lt_findstrings.
      APPEND <ls_node>-obj_name TO lt_findstrings.
      lv_find_obj_cls = <ls_node>-obj_type.

      CALL FUNCTION 'RS_EU_CROSSREF'
        EXPORTING
          i_find_obj_cls           = lv_find_obj_cls
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
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      LOOP AT lt_founds ASSIGNING <ls_found>.
        APPEND INITIAL LINE TO lt_edges ASSIGNING <ls_edge>.
        <ls_edge>-from = <ls_node>.

        <ls_edge>-to-obj_name   = <ls_found>-object.
        CASE <ls_found>-object_cls.
          WHEN 'DS'
              OR 'DT'.
            <ls_edge>-to-obj_type = 'TABL'.
          WHEN 'DV'.
            <ls_edge>-to-obj_type = 'VIEW'.
          WHEN 'DA'.
            <ls_edge>-to-obj_type = 'TTYP'.
          WHEN OTHERS.
            Lcx_abapgit_exception=>raise( 'resolve_ddic, unknown object_cls' ).
        ENDCASE.
      ENDLOOP.

    ENDLOOP.

    " build DDLS edges
    SORT ct_tadir. "binary search
    LOOP AT ct_tadir ASSIGNING <ls_tadir_ddls>
                     WHERE object = 'DDLS'.

      CLEAR: lt_dependency.

      APPEND INITIAL LINE TO lt_nodes ASSIGNING <ls_node>.
      <ls_node>-obj_name = <ls_tadir_ddls>-obj_name.
      <ls_node>-obj_type = <ls_tadir_ddls>-object.

      lt_dependency = get_ddls_dependencies( <ls_tadir_ddls>-obj_name ).

      LOOP AT lt_dependency ASSIGNING <ls_dependency>
                            WHERE deptyp = 'DDLS'
                            AND refname = <ls_tadir_ddls>-obj_name.

        READ TABLE ct_tadir ASSIGNING <ls_tadir_dependent>
                            WITH KEY pgmid    = 'R3TR'
                                     object   = 'DDLS'
                                     obj_name = <ls_dependency>-depname
                            BINARY SEARCH.
        CHECK sy-subrc = 0.

        APPEND INITIAL LINE TO lt_edges ASSIGNING <ls_edge>.
        <ls_edge>-from = <ls_node>.
        <ls_edge>-to-obj_name = <ls_dependency>-depname.
        <ls_edge>-to-obj_type = 'DDLS'.

      ENDLOOP.

    ENDLOOP.

    DO.
      lv_before = lines( lt_nodes ).
      LOOP AT lt_nodes ASSIGNING <ls_node>.
        lv_index = sy-tabix.
        READ TABLE lt_edges WITH KEY
          from-obj_name = <ls_node>-obj_name
          from-obj_type = <ls_node>-obj_type
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          LOOP AT ct_tadir ASSIGNING <ls_tadir>
              WHERE obj_name = <ls_node>-obj_name
              AND object = <ls_node>-obj_type.
            <ls_tadir>-korrnum = <ls_tadir>-korrnum + lv_plus.
            CONDENSE <ls_tadir>-korrnum.
          ENDLOOP.
          DELETE lt_edges
            WHERE to-obj_name = <ls_node>-obj_name
            AND to-obj_type = <ls_node>-obj_type.
          DELETE lt_nodes INDEX lv_index.
          EXIT. " make sure the sequence is fixed
        ENDIF.
      ENDLOOP.
      IF lv_before = lines( lt_nodes ).
        EXIT.
      ENDIF.
      lv_plus = lv_plus + 1.
    ENDDO.

  ENDMETHOD.
  METHOD resolve_packages.

    DATA: lt_subpackages TYPE Lif_abapgit_sap_package=>ty_devclass_tt.

    FIELD-SYMBOLS: <ls_tadir>            LIKE LINE OF ct_tadir,
                   <lv_subpackage>       LIKE LINE OF lt_subpackages,
                   <ls_tadir_subpackage> LIKE LINE OF ct_tadir.

    " List subpackage before corresponding superpackage

    LOOP AT ct_tadir ASSIGNING <ls_tadir>
                     WHERE object = 'DEVC'.

      lt_subpackages = Lcl_abapgit_factory=>get_sap_package( |{ <ls_tadir>-obj_name }| )->list_subpackages( ).

      LOOP AT lt_subpackages ASSIGNING <lv_subpackage>.

        READ TABLE ct_tadir ASSIGNING <ls_tadir_subpackage>
                            WITH KEY object   = 'DEVC'
                                     obj_name = <lv_subpackage>.
        IF sy-subrc = 0.
          <ls_tadir_subpackage>-korrnum = condense( |{ <ls_tadir_subpackage>-korrnum - 1 }| ).
        ENDIF.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_DEPENDENCIES implementation

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
*CLASS SHRITEFUH64VYIPO5IWUYB3KWW5SFY DEFINITION DEFERRED.
*CLASS zcl_abapgit_dot_abapgit DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KWW5SFY.



class LCL_ABAPGIT_DOT_ABAPGIT implementation.
*"* method's implementations
*include methods.
  METHOD get_name.
    rv_name = ms_data-name.
  ENDMETHOD.
  METHOD set_name.
    ms_data-name = iv_name.
  ENDMETHOD.
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
  METHOD get_abap_language_version.
    rv_abap_language_version = ms_data-abap_language_version.
  ENDMETHOD.
  METHOD set_abap_language_version.
    ms_data-abap_language_version = iv_abap_language_version.
  ENDMETHOD.
  METHOD use_lxe.

    IF iv_yes <> abap_undefined.
      ms_data-use_lxe = iv_yes.
    ENDIF.

    rv_yes = ms_data-use_lxe.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_DOT_ABAPGIT implementation

*>>>>>>> ZCL_ABAPGIT_ENVIRONMENT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_environment=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_environment=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_environment=======ccau.
*CLASS SHRITEFUH64VYIPO5IWUYB3KWW7SFY DEFINITION DEFERRED.
*CLASS zcl_abapgit_environment DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KWW7SFY.




class LCL_ABAPGIT_ENVIRONMENT implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_environment~init_parallel_processing.

    DATA: lv_group TYPE rzlli_apcl.

    lv_group = iv_group.

    " SPBT_INITIALIZE gives error PBT_ENV_ALREADY_INITIALIZED if called
    " multiple times in same session
    CALL FUNCTION 'SPBT_INITIALIZE'
      EXPORTING
        group_name                     = lv_group
      IMPORTING
        free_pbt_wps                   = rv_free_work_processes
      EXCEPTIONS
        invalid_group_name             = 1
        internal_error                 = 2
        pbt_env_already_initialized    = 3
        currently_no_resources_avail   = 4
        no_pbt_resources_found         = 5
        cant_init_different_pbt_groups = 6
        OTHERS                         = 7.
    " If SPBT_INITIALIZE fails, check transactions RZ12, SM50, SM21, SARFC

  ENDMETHOD.
  METHOD is_system_changes_allowed.

    DATA:
      lv_systemedit         TYPE tadir-edtflag,
      lv_sys_cliinddep_edit TYPE t000-ccnocliind,
      lv_is_shadow          TYPE abap_bool,
      ls_upginfo            TYPE uvers,
      lv_is_upgrade         TYPE abap_bool.

    CALL FUNCTION 'TR_SYS_PARAMS'
      IMPORTING
        systemedit         = lv_systemedit
        sys_cliinddep_edit = lv_sys_cliinddep_edit
      EXCEPTIONS
        no_systemname      = 1
        no_systemtype      = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      " Assume system can't be changed
      RETURN.
    ENDIF.

    CALL FUNCTION 'UPG_IS_SHADOW_SYSTEM'
      IMPORTING
        ev_shadow = lv_is_shadow.

    CALL FUNCTION 'UPG_GET_ACTIVE_COMP_UPGRADE'
      EXPORTING
        iv_component = 'SAP_BASIS'
        iv_upgtype   = 'A'
        iv_buffered  = abap_false
      IMPORTING
        ev_upginfo   = ls_upginfo
      EXCEPTIONS
        OTHERS       = 4.
    IF sy-subrc = 0 AND ls_upginfo-putstatus NA 'ITU'.
      lv_is_upgrade = abap_true.
    ENDIF.

    " SAP system has status 'not modifiable' (TK 102)
    " Changes to repository objects are not permitted in this client (TK 729)
    " Shadow system
    " Running upgrade
    rv_result = boolc(
      lv_systemedit <> 'N' AND
      lv_sys_cliinddep_edit NA '23' AND
      lv_is_shadow <> abap_true AND
      lv_is_upgrade <> abap_true ).

  ENDMETHOD.
  METHOD Lif_abapgit_environment~compare_with_inactive.
    rv_result = Lif_abapgit_environment~is_sap_cloud_platform( ).
  ENDMETHOD.
  METHOD Lif_abapgit_environment~get_basis_release.

    SELECT SINGLE release extrelease FROM cvers INTO (rs_result-release, rs_result-sp)
      WHERE component = 'SAP_BASIS' ##SUBRC_OK.

  ENDMETHOD.
  METHOD Lif_abapgit_environment~get_system_language_filter.
    DATA lv_translation_detective_lang TYPE spras.
    DATA lv_pseudo_translation_language TYPE spras.
    FIELD-SYMBOLS <ls_system_language_filter> LIKE LINE OF rt_system_language_filter.

    " Translation Object Detective
    " https://help.sap.com/docs/ABAP_PLATFORM_NEW/ceb25152cb0d4adba664cebea2bf4670/88a3d3cbccf64601975acabaccdfde45.html
    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
      EXPORTING
        input            = '1Q'
      IMPORTING
        output           = lv_translation_detective_lang
      EXCEPTIONS
        unknown_language = 1
        OTHERS           = 2.
    IF sy-subrc = 1.
      " The language for Translation Object Detective was not setup
    ENDIF.
    IF NOT lv_translation_detective_lang IS INITIAL.
      APPEND INITIAL LINE TO rt_system_language_filter ASSIGNING <ls_system_language_filter>.
      <ls_system_language_filter>-sign = 'E'.
      <ls_system_language_filter>-option = 'EQ'.
      <ls_system_language_filter>-low = lv_translation_detective_lang.
    ENDIF.
    " 1943470 - Using technical language key 2Q to create pseudo-translations of ABAP developments
    " https://launchpad.support.sap.com/#/notes/1943470
    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
      EXPORTING
        input            = '2Q'
      IMPORTING
        output           = lv_pseudo_translation_language
      EXCEPTIONS
        unknown_language = 1
        OTHERS           = 2.
    IF sy-subrc = 1.
      " The language for Pseudo Translation was not setup
    ENDIF.
    IF NOT lv_pseudo_translation_language IS INITIAL.
      APPEND INITIAL LINE TO rt_system_language_filter ASSIGNING <ls_system_language_filter>.
      <ls_system_language_filter>-sign = 'E'.
      <ls_system_language_filter>-option = 'EQ'.
      <ls_system_language_filter>-low = lv_pseudo_translation_language.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_environment~is_merged.
    DATA lr_marker TYPE REF TO data ##NEEDED.

    IF mv_is_merged = abap_undefined.
      TRY.
          CREATE DATA lr_marker TYPE REF TO ('LIF_ABAPMERGE_MARKER').
          "No exception --> marker found
          mv_is_merged = abap_true.

        CATCH cx_sy_create_data_error.
          mv_is_merged = abap_false.
      ENDTRY.
    ENDIF.
    rv_result = mv_is_merged.
  ENDMETHOD.
  METHOD Lif_abapgit_environment~is_repo_object_changes_allowed.
    IF mv_modifiable = abap_undefined.
      mv_modifiable = is_system_changes_allowed( ).
    ENDIF.
    rv_result = mv_modifiable.
  ENDMETHOD.
  METHOD Lif_abapgit_environment~is_restart_required.
    " This method will be used in the context of SAP Cloud Platform:
    " Pull/Push operations are executed in background jobs.
    " In case of the respective application server needs to be restarted,
    " it is required to terminate the background job and reschedule again.
    rv_result = abap_false.
    TRY.
        CALL METHOD ('CL_APJ_SCP_TOOLS')=>('IS_RESTART_REQUIRED')
          RECEIVING
            restart_required = rv_result.
      CATCH cx_sy_dyn_call_illegal_method cx_sy_dyn_call_illegal_class.
        rv_result = abap_false.
    ENDTRY.
  ENDMETHOD.
  METHOD Lif_abapgit_environment~is_sap_cloud_platform.
    IF mv_cloud = abap_undefined.
      TRY.
          CALL METHOD ('CL_COS_UTILITIES')=>('IS_SAP_CLOUD_PLATFORM')
            RECEIVING
              rv_is_sap_cloud_platform = mv_cloud.
        CATCH cx_sy_dyn_call_error.
          mv_cloud = abap_false.
      ENDTRY.
    ENDIF.
    rv_result = mv_cloud.
  ENDMETHOD.
  METHOD Lif_abapgit_environment~is_sap_object_allowed.

    rv_allowed = cl_enh_badi_def_utility=>is_sap_system( ).
    IF rv_allowed = abap_true.
      RETURN.
    ENDIF.

    rv_allowed = Lcl_abapgit_exit=>get_instance( )->allow_sap_objects( ).

  ENDMETHOD.
  METHOD Lif_abapgit_environment~is_variant_maintenance.

    DATA:
      lt_variscreens TYPE STANDARD TABLE OF rsdynnr
                          WITH NON-UNIQUE DEFAULT KEY.

    " Memory is set in LSVARF08 / EXPORT_SCREEN_TABLES.
    IMPORT variscreens = lt_variscreens FROM MEMORY ID '%_SCRNR_%'.

    rv_is_variant_maintenance = boolc( lines( lt_variscreens ) > 0 ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ENVIRONMENT implementation

*>>>>>>> ZCL_ABAPGIT_EXIT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_exit==============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_exit==============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_exit==============ccau.
*"* use this source file for your ABAP unit test classes

*CLASS SHRITEFUH64VYIPO5IWUYB3KWTMSFY DEFINITION DEFERRED.
*CLASS zcl_abapgit_exit DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KWTMSFY.

" The class name SHRITEFUH64VYIPO5IWUYB3KWTMSFY is hardcoded in zcl_abapgit_exit=>is_running_in_test_context



class LCL_ABAPGIT_EXIT implementation.
*"* method's implementations
*include methods.
  METHOD get_instance.

    DATA lv_class_name TYPE string.

    IF gi_global_exit IS NOT INITIAL.
      ri_exit = gi_global_exit.
      RETURN.
    ENDIF.

    lv_class_name = 'LCL_ABAPGIT_USER_EXIT'.

    IF Lcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_true.
      " Prevent accidental usage of exit handlers in the developer version
      lv_class_name = |\\PROGRAM={ sy-repid }\\CLASS={ lv_class_name }|.
    ENDIF.

    " Prevent non-mocked exit calls in unit tests
    IF is_running_in_test_context( ) = abap_false.
      TRY.
          CREATE OBJECT gi_exit TYPE (lv_class_name).
        CATCH cx_sy_create_object_error ##NO_HANDLER.
      ENDTRY.
    ENDIF.

    CREATE OBJECT gi_global_exit TYPE Lcl_abapgit_exit. " this class

    ri_exit = gi_global_exit.

  ENDMETHOD.
  METHOD is_running_in_test_context.

    IF sy-sysid = 'ABC'.
      " always run on open-abap
      rv_running_in_test_context = abap_true.
      RETURN.
    ENDIF.

    " Check if the local test class can be accessed by RTTI. If so the current process is running in a unit test.
    " Note this approach only works for the developer version. The standalone version will always report not running in
    " test context which should be fine as there are no unit tests delivered in it.
    cl_abap_typedescr=>describe_by_name(
      EXPORTING
        p_name         = |\\PROGRAM={ sy-repid }\\CLASS=SHRITEFUH64VYIPO5IWUYB3KWTMSFY|
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2 ).
    rv_running_in_test_context = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_exit~adjust_display_commit_url.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->adjust_display_commit_url(
            EXPORTING
              iv_repo_url    = iv_repo_url
              iv_repo_name   = iv_repo_name
              iv_repo_key    = iv_repo_key
              iv_commit_hash = iv_commit_hash
            CHANGING
              cv_display_url = cv_display_url ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~adjust_display_filename.

    IF gi_exit IS NOT INITIAL.
      TRY.
          rv_filename = gi_exit->adjust_display_filename(
            is_repo_meta = is_repo_meta
            iv_filename  = iv_filename ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

    IF rv_filename IS INITIAL.
      rv_filename = iv_filename.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~allow_sap_objects.

    IF gi_exit IS NOT INITIAL.
      TRY.
          rv_allowed = gi_exit->allow_sap_objects( ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~change_local_host.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_local_host( CHANGING ct_hosts = ct_hosts ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~change_max_parallel_processes.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_max_parallel_processes(
            EXPORTING
              iv_package       = iv_package
            CHANGING
              cv_max_processes = cv_max_processes ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~change_proxy_authentication.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_proxy_authentication(
            EXPORTING
              iv_repo_url             = iv_repo_url
            CHANGING
              cv_proxy_authentication = cv_proxy_authentication ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~change_proxy_port.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_proxy_port(
            EXPORTING
              iv_repo_url   = iv_repo_url
            CHANGING
              cv_proxy_port = cv_proxy_port ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~change_proxy_url.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_proxy_url(
            EXPORTING
              iv_repo_url  = iv_repo_url
            CHANGING
              cv_proxy_url = cv_proxy_url ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~change_rfc_server_group.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_rfc_server_group( CHANGING cv_group = cv_group ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~change_supported_data_objects.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_supported_data_objects( CHANGING ct_objects = ct_objects ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~change_supported_object_types.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_supported_object_types( CHANGING ct_types = ct_types ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~change_tadir.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_tadir(
            EXPORTING
              iv_package = iv_package
              ii_log     = ii_log
            CHANGING
              ct_tadir   = ct_tadir ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~create_http_client.

    IF gi_exit IS NOT INITIAL.
      TRY.
          ri_client = gi_exit->create_http_client( iv_url ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~custom_serialize_abap_clif.

    " This exit might be called twice per object
    " 1st call: it_source = initial
    "    Can be used for serializing complete source
    "    If source is returned, there will be no second call
    " 2nd call: it_source = code as serialized by abapGit
    "    Can be used for post-processing of source
    IF gi_exit IS NOT INITIAL.
      TRY.
          rt_source = gi_exit->custom_serialize_abap_clif(
            is_class_key = is_class_key
            it_source    = it_source ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

    IF rt_source IS INITIAL.
      rt_source = it_source.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~deserialize_postprocess.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->deserialize_postprocess( is_step = is_step
                                            ii_log  = ii_log ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~determine_transport_request.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->determine_transport_request(
            EXPORTING
              io_repo              = io_repo
              iv_transport_type    = iv_transport_type
            CHANGING
              cv_transport_request = cv_transport_request ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~enhance_repo_toolbar.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->enhance_repo_toolbar(
            io_menu = io_menu
            iv_key  = iv_key
            iv_act  = iv_act ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~get_ci_tests.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->get_ci_tests(
            EXPORTING
              iv_object   = iv_object
            CHANGING
              ct_ci_repos = ct_ci_repos ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~get_ssl_id.

    IF gi_exit IS NOT INITIAL.
      TRY.
          rv_ssl_id = gi_exit->get_ssl_id( ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

    IF rv_ssl_id IS INITIAL.
      rv_ssl_id = 'ANONYM'.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~http_client.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->http_client(
            iv_url    = iv_url
            ii_client = ii_client ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~on_event.

    IF gi_exit IS NOT INITIAL.
      TRY.
          rs_handled = gi_exit->on_event( ii_event ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~pre_calculate_repo_status.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->pre_calculate_repo_status(
            EXPORTING
              is_repo_meta = is_repo_meta
            CHANGING
              ct_local     = ct_local
              ct_remote    = ct_remote ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~serialize_postprocess.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->serialize_postprocess(
            EXPORTING
              iv_package = iv_package
              ii_log     = ii_log
            CHANGING
              ct_files   = ct_files ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~validate_before_push.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->validate_before_push(
            is_comment = is_comment
            io_stage   = io_stage
            io_repo    = io_repo ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~wall_message_list.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->wall_message_list( ii_html ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~wall_message_repo.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->wall_message_repo(
            is_repo_meta = is_repo_meta
            ii_html      = ii_html ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_EXIT implementation

*>>>>>>> ZCL_ABAPGIT_FACTORY <<<<<<<*

*"* macro definitions
*include zcl_abapgit_factory===========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_factory===========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_FACTORY implementation.
*"* method's implementations
*include methods.
  METHOD get_code_inspector.

    DATA: ls_code_inspector LIKE LINE OF gt_code_inspector.
    FIELD-SYMBOLS: <ls_code_inspector> TYPE ty_code_inspector_pack.

    READ TABLE gt_code_inspector ASSIGNING <ls_code_inspector>
      WITH TABLE KEY package = iv_package.
    IF sy-subrc <> 0.
      ls_code_inspector-package = iv_package.

      CREATE OBJECT ls_code_inspector-instance TYPE Lcl_abapgit_code_inspector
        EXPORTING
          iv_package = iv_package.

      INSERT ls_code_inspector
             INTO TABLE gt_code_inspector
             ASSIGNING <ls_code_inspector>.

    ENDIF.

    ri_code_inspector = <ls_code_inspector>-instance.

  ENDMETHOD.
  METHOD get_cts_api.
    IF gi_cts_api IS NOT BOUND.
      CREATE OBJECT gi_cts_api TYPE Lcl_abapgit_cts_api.
    ENDIF.

    ri_cts_api = gi_cts_api.
  ENDMETHOD.
  METHOD get_environment.
    IF gi_environment IS NOT BOUND.
      CREATE OBJECT gi_environment TYPE Lcl_abapgit_environment.
    ENDIF.
    ri_environment = gi_environment.
  ENDMETHOD.
  METHOD get_http_agent.

    IF gi_http_agent IS INITIAL.
      gi_http_agent = Lcl_abapgit_http_agent=>create( ).
    ENDIF.

    ri_http_agent = gi_http_agent.

  ENDMETHOD.
  METHOD get_longtexts.

    IF gi_longtext IS NOT BOUND.
      CREATE OBJECT gi_longtext TYPE Lcl_abapgit_longtexts.
    ENDIF.
    ri_longtexts = gi_longtext.

  ENDMETHOD.
  METHOD get_lxe_texts.

    IF gi_lxe_texts IS NOT BOUND.
      CREATE OBJECT gi_lxe_texts TYPE Lcl_abapgit_lxe_texts.
    ENDIF.
    ri_lxe_texts = gi_lxe_texts.

  ENDMETHOD.
  METHOD get_sap_namespace.

    IF gi_sap_namespace IS NOT BOUND.
      CREATE OBJECT gi_sap_namespace TYPE Lcl_abapgit_sap_namespace.
    ENDIF.

    ri_namespace = gi_sap_namespace.

  ENDMETHOD.
  METHOD get_sap_package.

    DATA: ls_sap_package TYPE ty_sap_package.
    FIELD-SYMBOLS: <ls_sap_package> TYPE ty_sap_package.

    READ TABLE gt_sap_package ASSIGNING <ls_sap_package>
                              WITH TABLE KEY package = iv_package.
    IF sy-subrc <> 0.

      ls_sap_package-package = iv_package.
      CREATE OBJECT ls_sap_package-instance TYPE Lcl_abapgit_sap_package
        EXPORTING
          iv_package = iv_package.

      INSERT ls_sap_package
             INTO TABLE gt_sap_package
             ASSIGNING <ls_sap_package>.

    ENDIF.

    ri_sap_package = <ls_sap_package>-instance.

  ENDMETHOD.
  METHOD get_stage_logic.

    IF gi_stage_logic IS INITIAL.
      CREATE OBJECT gi_stage_logic TYPE Lcl_abapgit_stage_logic.
    ENDIF.

    ri_logic = gi_stage_logic.

  ENDMETHOD.
  METHOD get_tadir.

    IF gi_tadir IS INITIAL.
      CREATE OBJECT gi_tadir TYPE Lcl_abapgit_tadir.
    ENDIF.

    ri_tadir = gi_tadir.

  ENDMETHOD.
  METHOD get_sap_report.

    IF gi_sap_report IS NOT BOUND.
      CREATE OBJECT gi_sap_report TYPE Lcl_abapgit_sap_report.
    ENDIF.

    ri_report = gi_sap_report.

  ENDMETHOD.
  METHOD get_function_module.

    IF gi_function_module IS INITIAL.
      CREATE OBJECT gi_function_module TYPE Lcl_abapgit_function_module.
    ENDIF.

    ri_function_module = gi_function_module.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_FACTORY implementation

*>>>>>>> ZCL_ABAPGIT_FOLDER_LOGIC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_folder_logic======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_folder_logic======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_folder_logic======ccau.










class LCL_ABAPGIT_FOLDER_LOGIC implementation.
*"* method's implementations
*include methods.
  METHOD get_instance.
    CREATE OBJECT ro_instance.
  ENDMETHOD.
  METHOD get_parent.
    DATA: ls_parent LIKE LINE OF mt_parent.

    " Check that package is included in the TOP package hierarchy
    IF mt_top_subpackages IS INITIAL.
      mt_top_subpackages = Lcl_abapgit_factory=>get_sap_package( iv_top )->list_subpackages( ).
    ENDIF.

    READ TABLE mt_top_subpackages TRANSPORTING NO FIELDS WITH KEY devclass = iv_package.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    "Determine Parent Package
    READ TABLE mt_parent INTO ls_parent
      WITH TABLE KEY devclass = iv_package.
    IF sy-subrc <> 0.
      rv_parent = Lcl_abapgit_factory=>get_sap_package( iv_package )->read_parent( ).
      ls_parent-devclass = iv_package.
      ls_parent-parentcl = rv_parent.
      INSERT ls_parent INTO TABLE mt_parent.
    ELSE.
      rv_parent = ls_parent-parentcl.
    ENDIF.
  ENDMETHOD.
  METHOD package_to_path.

    DATA: lv_len          TYPE i,
          lv_path         TYPE string,
          lv_message      TYPE string,
          lv_parentcl     TYPE tdevc-parentcl,
          lv_folder_logic TYPE string.

    IF iv_top = iv_package.
      rv_path = io_dot->get_starting_folder( ).
    ELSE.
      lv_parentcl = get_parent(
        iv_top     = iv_top
        iv_package = iv_package ).

      " If the parent package can not be determined, we return an initial path and handle
      " it outside of this class (in zcl_abapgit_file_status)
      IF lv_parentcl IS NOT INITIAL.
        lv_folder_logic = io_dot->get_folder_logic( ).
        CASE lv_folder_logic.
          WHEN Lif_abapgit_dot_abapgit=>c_folder_logic-full.
            lv_len = 0.
            IF iv_package(1) = '$'.
              lv_len = 1.
            ENDIF.
          WHEN Lif_abapgit_dot_abapgit=>c_folder_logic-prefix.
            lv_len = strlen( lv_parentcl ).

            IF iv_package(lv_len) <> lv_parentcl.
              " If abapGit project is installed in package ZZZ, all subpackages should be named
              " ZZZ_something. This will define the folder name in the zip file to be "something",
              " similarily with online projects. Alternatively change to FULL folder logic
              lv_message = |PREFIX: Unexpected package naming |
                        && |(top: { iv_top }, parent: { lv_parentcl }, child: { iv_package }). |
                        && |Try using the folder logic FULL|.
              Lcx_abapgit_exception=>raise( lv_message ).
            ENDIF.
          WHEN Lif_abapgit_dot_abapgit=>c_folder_logic-mixed.
            lv_len = strlen( iv_top ).

            IF iv_package(lv_len) <> iv_top.
              lv_message = |MIXED: Unexpected package naming |
                        && |(top: { iv_top }, parent: { lv_parentcl }, child: { iv_package }). |
                        && |Try using the folder logic FULL|.
              Lcx_abapgit_exception=>raise( lv_message ).
            ENDIF.
          WHEN OTHERS.
            Lcx_abapgit_exception=>raise( |Invalid folder logic: { lv_folder_logic }| ).
        ENDCASE.

        lv_path = iv_package+lv_len.
        IF strlen( lv_path ) = 0.
          Lcx_abapgit_exception=>raise( |Folder logic: length = 0, parent: {
            lv_parentcl }, child: { iv_package }| ).
        ENDIF.

        IF lv_path(1) = '_'.
          lv_path = lv_path+1.
        ENDIF.
        IF strlen( lv_path ) = 0.
          Lcx_abapgit_exception=>raise( |Folder logic: length = 0, parent: {
            lv_parentcl }, child: { iv_package }| ).
        ENDIF.

        TRANSLATE lv_path USING '/#'.
        TRANSLATE lv_path TO LOWER CASE.
        CONCATENATE lv_path '/' INTO lv_path.

        rv_path = package_to_path( iv_top     = iv_top
                                   io_dot     = io_dot
                                   iv_package = lv_parentcl ).

        CONCATENATE rv_path lv_path INTO rv_path.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD path_to_package.

    DATA: lv_length               TYPE i,
          lv_parent               TYPE devclass,
          ls_package              TYPE scompkdtln,
          lv_new                  TYPE string,
          lv_path                 TYPE string,
          lv_absolute_name        TYPE string,
          lv_folder_logic         TYPE string,
          lt_unique_package_names TYPE HASHED TABLE OF devclass WITH UNIQUE KEY table_line.

    lv_length  = strlen( io_dot->get_starting_folder( ) ).
    IF lv_length > strlen( iv_path ).
* treat as not existing locally
      RETURN.
    ENDIF.
    lv_path    = iv_path+lv_length.
    lv_parent  = iv_top.
    rv_package = iv_top.

    " Automatically create package using minimal properties
    " Details will be updated during deserialization
    IF iv_create_if_not_exists = abap_true.
      IF iv_top(1) = '$'.
        Lcl_abapgit_factory=>get_sap_package( iv_top )->create_local( ).
      ELSE.
        ls_package-devclass = iv_top.
        ls_package-ctext = iv_top.
        ls_package-as4user = sy-uname.
        Lcl_abapgit_factory=>get_sap_package( iv_top )->create( ls_package ).
      ENDIF.
    ENDIF.

    INSERT iv_top INTO TABLE lt_unique_package_names.

    WHILE lv_path CA '/'.
      SPLIT lv_path AT '/' INTO lv_new lv_path.

      lv_folder_logic = io_dot->get_folder_logic( ).
      CASE lv_folder_logic.
        WHEN Lif_abapgit_dot_abapgit=>c_folder_logic-full.
          lv_absolute_name = lv_new.
          TRANSLATE lv_absolute_name USING '#/'.
          IF iv_top(1) = '$'.
            CONCATENATE '$' lv_absolute_name INTO lv_absolute_name.
          ENDIF.
        WHEN Lif_abapgit_dot_abapgit=>c_folder_logic-prefix.
          CONCATENATE rv_package '_' lv_new INTO lv_absolute_name.
        WHEN Lif_abapgit_dot_abapgit=>c_folder_logic-mixed.
          CONCATENATE iv_top '_' lv_new INTO lv_absolute_name.
        WHEN OTHERS.
          Lcx_abapgit_exception=>raise( |Invalid folder logic: { lv_folder_logic }| ).
      ENDCASE.

      TRANSLATE lv_absolute_name TO UPPER CASE.

      IF strlen( lv_absolute_name ) > 30.
        Lcx_abapgit_exception=>raise( |Package { lv_absolute_name } exceeds ABAP 30-characters name limit| ).
      ENDIF.

      rv_package = lv_absolute_name.
      READ TABLE lt_unique_package_names TRANSPORTING NO FIELDS
        WITH TABLE KEY table_line = rv_package.
      IF sy-subrc = 0.
        Lcx_abapgit_exception=>raise( |Package { rv_package } has a subpackage with the same name| ).
      ELSE.
        INSERT rv_package INTO TABLE lt_unique_package_names.
      ENDIF.

      IF Lcl_abapgit_factory=>get_sap_package( rv_package )->exists( ) = abap_false AND
          iv_create_if_not_exists = abap_true.

        Lcl_abapgit_factory=>get_sap_package( lv_parent )->create_child( rv_package ).
      ENDIF.

      lv_parent = rv_package.
    ENDWHILE.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_FOLDER_LOGIC implementation

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
*  LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KWY3SFY
*                SHRITEFUH64VYIPO5IWUYB3KWY6SFY.









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
          IF ls_parsed_color-border IS INITIAL.
            lv_style = lv_style && |border-color:#{ ls_parsed_color-bg };|.
          ELSE.
            lv_style = lv_style && |border-color:#{ ls_parsed_color-border };|.
          ENDIF.
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
    ENDIF.

    ri_html = render_infopanel(
      iv_div_id  = 'news'
      iv_title   = 'Announcement of the latest changes'
      iv_hint    = lv_hint
      iv_hide    = boolc( io_news->has_unseen( ) = abap_false )
      io_content = ri_html ).

  ENDMETHOD.
  METHOD render_package_name.

    DATA:
      lv_obj_name TYPE tadir-obj_name,
      lv_jump     TYPE string,
      lv_title    TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    IF iv_package IS INITIAL.
      RETURN.
    ENDIF.

    IF iv_suppress_title = abap_false.
      lv_title = Lcl_abapgit_factory=>get_sap_package( iv_package )->read_description( ).
    ENDIF.

    lv_obj_name = iv_package.
    lv_jump = Lcl_abapgit_html_action_utils=>jump_encode(
      iv_obj_type = 'DEVC'
      iv_obj_name = lv_obj_name ).

    ri_html->add( |<span class="package-box">| ).
    ri_html->add_icon( iv_name = 'box/grey70'
                       iv_hint = 'SAP package' ).
    IF iv_interactive = abap_true.
      ri_html->add_a( iv_act   = |{ Lif_abapgit_definitions=>c_action-jump }?{ lv_jump }|
                      iv_title = lv_title
                      iv_txt   = |{ iv_package }| ).
    ELSE.
      ri_html->add( iv_package ).
    ENDIF.
    ri_html->add( '</span>' ).

  ENDMETHOD.
  METHOD render_path.

    DATA:
      lv_path    TYPE string,
      lv_jump    TYPE string,
      lv_folder  TYPE string,
      lt_folders TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    IF iv_path IS INITIAL.
      RETURN.
    ENDIF.

    lv_jump = |{ Lcl_abapgit_gui_page_repo_view=>c_actions-change_dir }?PATH=|.

    ri_html->add( |<span class="path-box">| ).

    IF iv_interactive = abap_true.
      SPLIT iv_path AT '/' INTO TABLE lt_folders.

      LOOP AT lt_folders INTO lv_folder.
        IF lv_folder IS INITIAL.
          " root
          lv_path = '/'.
        ELSEIF sy-tabix < lines( lt_folders ).
          lv_path = lv_path && lv_folder && '/'.
          ri_html->add_a( iv_act = lv_jump && lv_path
                          iv_txt = lv_folder ).
        ELSE.
          " no link for current folder
          ri_html->add( | <strong>{ lv_folder }</strong> | ).
        ENDIF.
        ri_html->add( '/' ).
      ENDLOOP.
    ELSE.
      ri_html->add( iv_path ).
    ENDIF.

    ri_html->add( '</span>' ).

  ENDMETHOD.
  METHOD render_repo_palette.

    DATA lt_repo_obj_list TYPE Lif_abapgit_repo_srv=>ty_repo_list.
    DATA lt_repo_list TYPE Lif_abapgit_persistence=>ty_repos.
    DATA lv_repo_json TYPE string.
    DATA lv_size TYPE i.
    DATA ls_repo_data LIKE LINE OF lt_repo_list.

    FIELD-SYMBOLS:
      <ls_repo>     LIKE LINE OF lt_repo_list,
      <lr_repo_obj> LIKE LINE OF lt_repo_obj_list.

    lt_repo_obj_list = Lcl_abapgit_repo_srv=>get_instance( )->list( ).

    LOOP AT lt_repo_obj_list ASSIGNING <lr_repo_obj>.
      ls_repo_data = <lr_repo_obj>->ms_data.
      ls_repo_data-local_settings-display_name = <lr_repo_obj>->get_name( ).
      APPEND ls_repo_data TO lt_repo_list.
    ENDLOOP.

    lv_size = lines( lt_repo_list ).
    SORT lt_repo_list BY local_settings-display_name AS TEXT.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( 'var repoCatalog = [' ). " Maybe separate this into another method if needed in more places
    LOOP AT lt_repo_list ASSIGNING <ls_repo>.
      lv_repo_json = |\{ key: "{ <ls_repo>-key
        }", isOffline: "{ <ls_repo>-offline
        }", displayName: "{ escape( val = <ls_repo>-local_settings-display_name
                                    format = cl_abap_format=>e_html_js ) }"  \}|.
      IF sy-tabix < lv_size.
        lv_repo_json = lv_repo_json && ','.
      ENDIF.
      ri_html->add( lv_repo_json ).
    ENDLOOP.
    ri_html->add( '];' ).

    ri_html->add( |var gGoRepoPalette = new CommandPalette(createRepoCatalogEnumerator(repoCatalog, "{
      iv_action }"), \{| ).
    ri_html->add( '  toggleKey: "F2",' ).
    ri_html->add( '  hotkeyDescription: "Go to Repository ..."' ).
    ri_html->add( '});' ).

  ENDMETHOD.
  METHOD render_repo_top.

    DATA: lo_repo_online TYPE REF TO Lcl_abapgit_repo_online,
          lo_pback       TYPE REF TO Lcl_abapgit_persist_background,
          lx_error       TYPE REF TO Lcx_abapgit_exception,
          lv_hint        TYPE string,
          lv_icon        TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.
    CREATE OBJECT lo_pback.

    IF io_repo->is_offline( ) = abap_true.
      lv_icon = 'plug/darkgrey'.
      lv_hint = 'Offline Repository'.
    ELSE.
      lv_icon = 'cloud-upload-alt/blue'.
      lv_hint = 'On-line Repository'.
    ENDIF.

    ri_html->add( '<table class="w100"><tr>' ).

    ri_html->add( '<td class="repo_name">' ).

    " Repo type and name
    ri_html->add_icon( iv_name = lv_icon
                       iv_hint = lv_hint ).
    ri_html->add( |<span class="name">{ io_repo->get_name( ) }</span>| ).
    IF io_repo->is_offline( ) = abap_false.
      lo_repo_online ?= io_repo.

      ri_html->add( render_repo_url( lo_repo_online->get_url( ) ) ).
    ENDIF.

    IF iv_show_edit = abap_true.
      ri_html->add_a( iv_txt   = ri_html->icon( iv_name  = 'edit-solid'
                                                iv_class = 'pad-sides'
                                                iv_hint  = 'Change Remote' )
                      iv_act   = |{ Lif_abapgit_definitions=>c_action-repo_remote_settings }?| &&
                                 |key={ io_repo->get_key( ) }|
                      iv_class = |url| ).
    ENDIF.

    IF io_repo->is_offline( ) = abap_false.
      lo_repo_online ?= io_repo.

      ri_html->add_a( iv_txt   = ri_html->icon( iv_name  = 'copy-solid'
                                                iv_class = 'pad-sides'
                                                iv_hint  = 'Copy URL to Clipboard' )
                      iv_act   = |{ Lif_abapgit_definitions=>c_action-clipboard }| &&
                                 |?clipboard={ lo_repo_online->get_url( ) }|
                      iv_class = |url| ).
    ENDIF.

    IF io_repo->is_offline( ) = abap_false AND iv_show_commit = abap_true.
      TRY.
          render_repo_top_commit_hash( ii_html        = ri_html
                                       io_repo_online = lo_repo_online ).
        CATCH Lcx_abapgit_exception INTO lx_error.
          " In case of missing or wrong credentials, show message in status bar
          lv_hint = lx_error->get_text( ).
          IF lv_hint CS 'credentials'.
            MESSAGE lv_hint TYPE 'S' DISPLAY LIKE 'E'.
          ENDIF.
      ENDTRY.
    ENDIF.

    " News
    IF io_news IS BOUND AND io_news->has_news( ) = abap_true.
      IF io_news->has_updates( ) = abap_true.
        lv_icon = 'arrow-circle-up/warning'.
      ELSE.
        lv_icon = 'arrow-circle-up'.
      ENDIF.
      ri_html->add_a( iv_act   = |toggleDisplay('news')|
                      iv_typ   = Lif_abapgit_html=>c_action_type-onclick
                      iv_txt   = ri_html->icon( iv_name  = lv_icon
                                                iv_class = 'pad-sides'
                                                iv_hint  = 'Display Changelog' )
                      iv_class = |url| ).
    ENDIF.
    ri_html->add( '</td>' ).

    ri_html->add( '<td class="repo_attr right">' ).

    " Fav
    IF abap_true = Lcl_abapgit_persistence_user=>get_instance( )->is_favorite_repo( io_repo->get_key( ) ).
      lv_icon = 'star/blue'.
    ELSE.
      lv_icon = 'star/grey'.
    ENDIF.
    ri_html->add_a( iv_act = |{ Lif_abapgit_definitions=>c_action-repo_toggle_fav }?key={ io_repo->get_key( ) }|
                    iv_txt = ri_html->icon( iv_name  = lv_icon
                                            iv_class = 'pad-sides'
                                            iv_hint  = 'Toggle Favorite' ) ).

    " BG
    IF lo_pback->exists( io_repo->get_key( ) ) = abap_true.
      ri_html->add( '<span class="bg_marker" title="background">BG</span>' ).
    ENDIF.

    " Write protect
    IF io_repo->get_local_settings( )-write_protected = abap_true.
      ri_html->add_icon( iv_name = 'lock/grey70'
                         iv_hint = 'Locked from Pulls' ).
    ENDIF.
    IF io_repo->get_local_settings( )-flow = abap_true.
      ri_html->add_icon( iv_name = 'flow/grey70'
                         iv_hint = 'Flow' ).
    ENDIF.

    " Branch
    IF io_repo->is_offline( ) = abap_false.
      lo_repo_online ?= io_repo.
      IF iv_show_branch = abap_true.
        ri_html->add( render_branch_name( io_repo        = lo_repo_online
                                          iv_interactive = iv_interactive_branch ) ).
      ENDIF.
    ENDIF.

    " Package
    IF iv_show_package = abap_true.
      ri_html->add( render_package_name( io_repo->get_package( ) ) ).
    ENDIF.

    ri_html->add( '</td>' ).
    ri_html->add( '</tr></table>' ).

  ENDMETHOD.
  METHOD render_repo_top_commit_hash.

    DATA: lv_commit_hash       TYPE Lif_abapgit_git_definitions=>ty_sha1,
          lv_commit_short_hash TYPE Lif_abapgit_git_definitions=>ty_sha1,
          lv_display_url       TYPE Lif_abapgit_persistence=>ty_repo-url,
          lo_url               TYPE REF TO Lcl_abapgit_git_url,
          lv_icon_commit       TYPE string.

    lv_commit_hash = io_repo_online->get_current_remote( ).
    lv_commit_short_hash = lv_commit_hash(7).

    lv_icon_commit = ii_html->icon( iv_name  = 'code-commit'
                                    iv_class = 'pad-sides'
                                    iv_hint  = 'Commit' ).

    CREATE OBJECT lo_url.

    TRY.
        lv_display_url = lo_url->get_commit_display_url( io_repo_online ).

        ii_html->add_a( iv_txt   = |{ lv_icon_commit }{ lv_commit_short_hash }|
                        iv_act   = |{ Lif_abapgit_definitions=>c_action-url }?url={ lv_display_url }|
                        iv_title = 'Commit'
                        iv_class = |url| ).
      CATCH Lcx_abapgit_exception.
        ii_html->add( |<span class="url">{ lv_icon_commit }{ lv_commit_short_hash }</span>| ).
    ENDTRY.

  ENDMETHOD.
  METHOD render_repo_url.

    ri_html = Lcl_abapgit_html=>create( )->add_a(
      iv_txt   = shorten_repo_url( iv_url )
      iv_title = iv_url
      iv_act   = |{ Lif_abapgit_definitions=>c_action-url }?url={ iv_url }|
      iv_class = 'url' ).

    IF iv_render_remote_edit_for_key IS NOT INITIAL.
      ri_html->add_a(
        iv_txt   = ri_html->icon(
          iv_name  = 'edit-solid'
          iv_class = 'pad-sides'
          iv_hint  = 'Change remote' )
        iv_act   = |{ Lif_abapgit_definitions=>c_action-repo_remote_settings }?key={ iv_render_remote_edit_for_key }|
        iv_class = |remote_repo| ).
    ENDIF.

  ENDMETHOD.
  METHOD render_sci_result.

    DATA lv_icon TYPE string.

    lv_icon = ii_html->icon(
      iv_name = 'bug-solid'
      iv_hint = 'Code inspector result' ).

    CASE iv_sci_result.
      WHEN Lif_abapgit_definitions=>c_sci_result-passed.
        ii_html->add( |<span class="boxed green-filled-set">{ lv_icon }PASSED</span>| ).
      WHEN Lif_abapgit_definitions=>c_sci_result-failed.
        ii_html->add( |<span class="boxed red-filled-set">{ lv_icon }FAILED</span>| ).
      WHEN Lif_abapgit_definitions=>c_sci_result-warning.
        ii_html->add( |<span class="boxed yellow-filled-set">{ lv_icon }WARN</span>| ).
      WHEN OTHERS. " Including NO_RUN
        RETURN.
    ENDCASE.

  ENDMETHOD.
  METHOD render_text_input.

    DATA lv_attrs TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    IF iv_value IS NOT INITIAL.
      lv_attrs = | value="{ iv_value }"|.
    ENDIF.

    IF iv_max_length IS NOT INITIAL.
      lv_attrs = lv_attrs && | maxlength="{ iv_max_length }"|.
    ENDIF.

    IF iv_autofocus = abap_true.
      lv_attrs = lv_attrs && | autofocus|.
    ENDIF.

    ri_html->add( |<label for="{ iv_name }">{ iv_label }</label>| ).
    ri_html->add( |<input id="{ iv_name }" name="{ iv_name }" type="text"{ lv_attrs }>| ).

  ENDMETHOD.
  METHOD render_timestamp.

    DATA lv_date TYPE d.
    DATA lv_time TYPE t.

    CONVERT TIME STAMP iv_timestamp
      TIME ZONE gv_time_zone
      INTO DATE lv_date
      TIME lv_time.

    rv_rendered = |{ lv_date DATE = USER } { lv_time TIME = USER }|.

  ENDMETHOD.
  METHOD render_transport.

    DATA:
      lv_title TYPE string,
      lv_jump  TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    IF iv_transport IS INITIAL.
      RETURN.
    ENDIF.

    lv_title = Lcl_abapgit_factory=>get_cts_api( )->read_description( iv_transport ).

    lv_jump = |{ Lif_abapgit_definitions=>c_action-jump_transport }?transport={ iv_transport }|.

    IF iv_icon_only = abap_true.
      ri_html->add_a( iv_act   = lv_jump
                      iv_title = |Transport { iv_transport }|
                      iv_txt   = Lcl_abapgit_html=>icon( 'truck-solid/darkgrey' ) ).
    ELSE.
      ri_html->add( |<span class="transport-box">| ).

      ri_html->add_icon( iv_name = 'truck-solid/grey70'
                         iv_hint = 'Transport' ).
      IF iv_interactive = abap_true.
        ri_html->add_a( iv_act   = lv_jump
                        iv_title = lv_title
                        iv_txt   = |{ iv_transport }| ).
      ELSE.
        ri_html->add( iv_transport ).
      ENDIF.

      ri_html->add( '</span>' ).
    ENDIF.

  ENDMETHOD.
  METHOD render_user_name.

    DATA:
      lv_title TYPE string,
      lv_jump  TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    IF iv_username IS INITIAL.
      RETURN.
    ENDIF.

    IF iv_username <> Lcl_abapgit_objects_super=>c_user_unknown AND iv_suppress_title = abap_false.
      lv_title = Lcl_abapgit_user_record=>get_title( iv_username ).
    ENDIF.

    lv_jump = |{ Lif_abapgit_definitions=>c_action-jump_user }?user={ iv_username }|.

    IF iv_icon_only = abap_true.
      ri_html->add_a( iv_act   = lv_jump
                      iv_title = lv_title
                      iv_txt   = Lcl_abapgit_html=>icon( 'user-solid/darkgrey' ) ).
    ELSE.
      ri_html->add( |<span class="user-box">| ).

      ri_html->add_icon( iv_name = 'user-solid/grey70'
                         iv_hint = 'User name' ).
      IF iv_interactive = abap_true AND iv_username <> Lcl_abapgit_objects_super=>c_user_unknown.
        ri_html->add_a( iv_act   = lv_jump
                        iv_title = lv_title
                        iv_txt   = |{ iv_username }| ).
      ELSE.
        ri_html->add( iv_username ).
      ENDIF.

      ri_html->add( '</span>' ).
    ENDIF.

  ENDMETHOD.
  METHOD render_warning_banner.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.
    ri_html->add( '<div class="dummydiv warning">' ).
    ri_html->add( |{ ri_html->icon( 'exclamation-triangle/yellow' ) } { iv_text }| ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
  METHOD shorten_repo_url.
    DATA lv_new_length TYPE i.
    DATA lv_length_to_truncate_to TYPE i.

    rv_shortened = iv_full_url.

    REPLACE FIRST OCCURRENCE OF 'https://' IN rv_shortened WITH ''.
    REPLACE FIRST OCCURRENCE OF 'http://' IN rv_shortened WITH ''.
    IF rv_shortened CP '*.git'.
      lv_new_length = strlen( rv_shortened ) - 4.
      rv_shortened  = rv_shortened(lv_new_length).
    ENDIF.

    IF strlen( rv_shortened ) > iv_max_length.
      lv_length_to_truncate_to = iv_max_length - 3.
      rv_shortened = rv_shortened(lv_length_to_truncate_to) && `...`.
    ENDIF.
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
  METHOD render_table_footer.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<tfoot>' ).
    ri_html->add( '<tr>' ).
    ri_html->add( '<td colspan="100%">' ).

    ri_html->add( iv_message ).

    ri_html->add( '</td>' ).
    ri_html->add( '</tr>' ).
    ri_html->add( '</tfoot>' ).

  ENDMETHOD.
  METHOD render_table_header.

    DATA:
      lv_tmp       TYPE string,
      lv_disp_name TYPE string.

    FIELD-SYMBOLS <ls_col> LIKE LINE OF it_col_spec.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<thead>' ).
    ri_html->add( '<tr>' ).

    LOOP AT it_col_spec ASSIGNING <ls_col>.
      " e.g. <th class="ro-detail">Created at [{ gv_time_zone }]</th>
      lv_tmp = '<th'.
      IF <ls_col>-css_class IS NOT INITIAL.
        lv_tmp = lv_tmp && | class="{ <ls_col>-css_class }"|.
      ENDIF.
      lv_tmp = lv_tmp && '>'.

      IF <ls_col>-display_name IS NOT INITIAL.
        lv_disp_name = <ls_col>-display_name.
        IF <ls_col>-add_tz = abap_true.
          lv_disp_name = lv_disp_name && | [{ gv_time_zone }]|.
        ENDIF.
        IF <ls_col>-tech_name = iv_order_by.
          IF iv_order_descending = abap_true.
            lv_tmp = lv_tmp && ri_html->a(
              iv_txt   = lv_disp_name
              iv_act   = |{ Lif_abapgit_definitions=>c_action-change_order_by }|
              iv_title = <ls_col>-title ).
          ELSE.
            lv_tmp = lv_tmp && ri_html->a(
              iv_txt   = lv_disp_name
              iv_act   = |{ Lif_abapgit_definitions=>c_action-direction }?direction=DESCENDING|
              iv_title = <ls_col>-title ).
          ENDIF.
        ELSEIF <ls_col>-allow_order_by = abap_true.
          lv_tmp = lv_tmp && ri_html->a(
            iv_txt   = lv_disp_name
            iv_act   = |{ Lif_abapgit_definitions=>c_action-change_order_by }?orderBy={ <ls_col>-tech_name }|
            iv_title = <ls_col>-title ).
        ELSE.
          lv_tmp = lv_tmp && lv_disp_name.
        ENDIF.
      ENDIF.
      IF <ls_col>-tech_name = iv_order_by
      AND iv_order_by IS NOT INITIAL.
        IF iv_order_descending = abap_true.
          lv_tmp = lv_tmp && | &#x25BE;|. " arrow down
        ELSE.
          lv_tmp = lv_tmp && | &#x25B4;|. " arrow up
        ENDIF.
      ENDIF.

      lv_tmp = lv_tmp && '</th>'.
      ri_html->add( lv_tmp ).
    ENDLOOP.

    ri_html->add( '</tr>' ).
    ri_html->add( '</thead>' ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_CHUNK_LIB implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_ADDOFFLIN <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_addofflinccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_addofflinccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_ADDOFFLIN implementation.
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

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_addofflin.

    CREATE OBJECT lo_component.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'New Offline Repository'
      ii_child_component = lo_component ).

  ENDMETHOD.
  METHOD get_form_schema.

    ro_form = Lcl_abapgit_html_form=>create(
                iv_form_id   = 'add-repo-offline-form'
                iv_help_page = 'https://docs.abapgit.org/guide-offline-install.html' ).

    ro_form->text(
      iv_name        = c_id-name
      iv_required    = abap_true
      iv_label       = 'Name'
      iv_hint        = 'Unique name for repository'
    )->text(
      iv_name        = c_id-package
      iv_side_action = c_event-choose_package
      iv_required    = abap_true
      iv_upper_case  = abap_true
      iv_label       = 'Package'
      iv_hint        = 'SAP package for repository (should be a dedicated one)'
      iv_placeholder = 'Z... / $...'
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
      iv_label       = 'Create Offline Repo'
      iv_cmd_type    = Lif_abapgit_html_form=>c_cmd_type-input_main
      iv_action      = c_event-add_offline_repo
    )->command(
      iv_label       = 'Create Package'
      iv_action      = c_event-create_package
    )->command(
      iv_label       = 'Back'
      iv_action      = Lif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.
  METHOD validate_form.

    DATA lx_err TYPE REF TO Lcx_abapgit_exception.

    ro_validation_log = mo_form_util->validate( io_form_data ).

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

    DATA: ls_repo_params      TYPE Lif_abapgit_services_repo=>ty_repo_params,
          lo_new_offline_repo TYPE REF TO Lcl_abapgit_repo_offline.

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

      WHEN c_event-choose_labels.

        choose_labels( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_event-add_offline_repo.

        mo_validation_log = validate_form( mo_form_data ).

        IF mo_validation_log->is_empty( ) = abap_true.
          mo_form_data->to_abap( CHANGING cs_container = ls_repo_params ).
          lo_new_offline_repo = Lcl_abapgit_services_repo=>new_offline( ls_repo_params ).
          rs_handled-page  = Lcl_abapgit_gui_page_repo_view=>create( lo_new_offline_repo->get_key( ) ).
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
endclass. "ZCL_ABAPGIT_GUI_PAGE_ADDOFFLIN implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_DATA <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_data=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_data=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_gui_page_data=====ccau.
*CLASS SHRITEFUH64VYIPO5IWUYB3KWZPSFY DEFINITION DEFERRED.
*CLASS zcl_abapgit_gui_page_data DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KWZPSFY.




class LCL_ABAPGIT_GUI_PAGE_DATA implementation.
*"* method's implementations
*include methods.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_data.

    CREATE OBJECT lo_component
      EXPORTING
        iv_key = iv_key.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Data Config'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_menu_provider~get_menu.

    CREATE OBJECT ro_toolbar.

    ro_toolbar->add( iv_txt = 'Add Via Transport'
                     iv_act = c_event-add_via_transport ).
    ro_toolbar->add( iv_txt = 'Back'
                     iv_act = Lif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    register_handlers( ).

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.
    ri_html->add( '<div class="repo">' ).
    ri_html->add( render_existing( ) ).
    ri_html->add( render_add( ) ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
  METHOD add_via_transport.

    DATA lv_trkorr  TYPE trkorr.
    DATA ls_request TYPE Lif_abapgit_cts_api=>ty_transport_data.
    DATA ls_key     LIKE LINE OF ls_request-keys.
    DATA lv_where   TYPE string.
    DATA ls_config  TYPE Lif_abapgit_data_config=>ty_config.


    lv_trkorr = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_select_transport( ).
    IF lv_trkorr IS INITIAL.
      RETURN.
    ENDIF.

    ls_request = Lcl_abapgit_factory=>get_cts_api( )->read( lv_trkorr ).

    IF lines( ls_request-keys ) = 0.
      Lcx_abapgit_exception=>raise( |No keys found, select task| ).
    ENDIF.

    LOOP AT ls_request-keys INTO ls_key WHERE object = 'TABU'.
      ASSERT ls_key-objname IS NOT INITIAL.
      ASSERT ls_key-tabkey IS NOT INITIAL.

      CLEAR ls_config.
      ls_config-type = Lif_abapgit_data_config=>c_data_type-tabu.
      ls_config-name = to_upper( ls_key-objname ).
      lv_where = concatenated_key_to_where(
        iv_table  = ls_key-objname
        iv_tabkey = ls_key-tabkey ).
      APPEND lv_where TO ls_config-where.
      mi_config->add_config( ls_config ).
    ENDLOOP.

  ENDMETHOD.
  METHOD build_where.

    DATA lv_where LIKE LINE OF rt_where.

    SPLIT io_map->get( c_id-where ) AT |\n| INTO TABLE rt_where.

    DELETE rt_where WHERE table_line IS INITIAL.

    LOOP AT rt_where INTO lv_where.
      IF strlen( lv_where ) <= 2.
        DELETE rt_where INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD concatenated_key_to_where.

    DATA lo_structdescr TYPE REF TO cl_abap_structdescr.
    DATA lo_typedescr   TYPE REF TO cl_abap_typedescr.
    DATA lt_fields      TYPE Lcl_abapgit_data_utils=>ty_names.
    DATA lv_field       LIKE LINE OF lt_fields.
    DATA lv_table       TYPE tadir-obj_name.
    DATA lv_length      TYPE i.
    DATA lv_tabix       TYPE i.
    DATA lv_key         TYPE c LENGTH 900.

    lv_key = iv_tabkey.
    lo_structdescr ?= cl_abap_typedescr=>describe_by_name( iv_table ).

    lv_table = iv_table.
    lt_fields = Lcl_abapgit_data_utils=>list_key_fields( lv_table ).

    LOOP AT lt_fields INTO lv_field.
      lv_tabix = sy-tabix.
      lo_typedescr = cl_abap_typedescr=>describe_by_name( |{ iv_table }-{ lv_field }| ).
      lv_length = lo_typedescr->length / cl_abap_char_utilities=>charsize.

      IF lv_tabix = 1 AND lo_typedescr->get_relative_name( ) = 'MANDT'.
        lv_key = lv_key+lv_length.
        CONTINUE.
      ENDIF.

      IF lv_key = |*|.
        EXIT. " current loop
      ENDIF.
      IF NOT rv_where IS INITIAL.
        rv_where = |{ rv_where } AND |.
      ENDIF.
      rv_where = |{ rv_where }{ to_lower( lv_field ) } = '{ lv_key(lv_length) }'|.
      lv_key = lv_key+lv_length.
    ENDLOOP.

  ENDMETHOD.
  METHOD constructor.

    super->constructor( ).

    mo_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
    mi_config = mo_repo->get_data_config( ).

  ENDMETHOD.
  METHOD event_add.

    DATA lo_map TYPE REF TO Lcl_abapgit_string_map.
    DATA ls_config TYPE Lif_abapgit_data_config=>ty_config.

    lo_map = ii_event->form_data( ).

    ls_config-type         = Lif_abapgit_data_config=>c_data_type-tabu.
    ls_config-name         = to_upper( lo_map->get( c_id-table ) ).
    ls_config-skip_initial = lo_map->get( c_id-skip_initial ).
    ls_config-where        = build_where( lo_map ).

    mi_config->add_config( ls_config ).

  ENDMETHOD.
  METHOD event_remove.

    DATA lo_map TYPE REF TO Lcl_abapgit_string_map.
    DATA ls_config TYPE Lif_abapgit_data_config=>ty_config.

    lo_map = ii_event->form_data( ).

    ls_config-type = Lif_abapgit_data_config=>c_data_type-tabu.
    ls_config-name = to_upper( lo_map->get( c_id-table ) ).

    mi_config->remove_config( ls_config ).

  ENDMETHOD.
  METHOD event_update.

    DATA lo_map TYPE REF TO Lcl_abapgit_string_map.
    DATA ls_config TYPE Lif_abapgit_data_config=>ty_config.

    lo_map = ii_event->form_data( ).

    ls_config-type         = Lif_abapgit_data_config=>c_data_type-tabu.
    ls_config-name         = to_upper( lo_map->get( c_id-table ) ).
    ls_config-skip_initial = lo_map->has( to_upper( c_id-skip_initial ) ).
    ls_config-where        = build_where( lo_map ).

    mi_config->update_config( ls_config ).

  ENDMETHOD.
  METHOD render_add.

    DATA lo_form TYPE REF TO Lcl_abapgit_html_form.
    DATA lo_form_data TYPE REF TO Lcl_abapgit_string_map.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.
    CREATE OBJECT lo_form_data.

    lo_form = Lcl_abapgit_html_form=>create( ).
    lo_form->text(
      iv_label    = 'Table'
      iv_name     = c_id-table
      iv_required = abap_true ).

    lo_form->checkbox(
      iv_label = 'Skip Initial Values'
      iv_name  = c_id-skip_initial ).

    lo_form->textarea(
      iv_label       = 'Where'
      iv_placeholder = 'Conditions separated by newline'
      iv_name        = c_id-where ).

    lo_form->command(
      iv_label       = 'Add'
      iv_cmd_type    = Lif_abapgit_html_form=>c_cmd_type-input_main
      iv_action      = c_event-add ).
    ri_html->add( lo_form->render( lo_form_data ) ).

  ENDMETHOD.
  METHOD render_existing.

    DATA lo_form TYPE REF TO Lcl_abapgit_html_form.
    DATA lo_form_data TYPE REF TO Lcl_abapgit_string_map.
    DATA lt_configs TYPE Lif_abapgit_data_config=>ty_config_tt.
    DATA ls_config LIKE LINE OF lt_configs.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.
    CREATE OBJECT lo_form_data.

    lt_configs = mi_config->get_configs( ).

    LOOP AT lt_configs INTO ls_config.
      lo_form = Lcl_abapgit_html_form=>create( ).
      CREATE OBJECT lo_form_data.

      lo_form_data->set(
        iv_key = c_id-table
        iv_val = |{ ls_config-name }| ).
      lo_form->text(
        iv_label    = 'Table'
        iv_name     = c_id-table
        iv_readonly = abap_true ).

      lo_form_data->set(
        iv_key = c_id-skip_initial
        iv_val = ls_config-skip_initial ).
      lo_form->checkbox(
        iv_label = 'Skip Initial Values'
        iv_name  = c_id-skip_initial ).

      lo_form_data->set(
        iv_key = c_id-where
        iv_val = concat_lines_of( table = ls_config-where sep = |\n| ) ).
      lo_form->textarea(
        iv_label       = 'Where'
        iv_placeholder = 'Conditions separated by newline'
        iv_name        = c_id-where ).

      lo_form->command(
        iv_label       = 'Update'
        iv_cmd_type    = Lif_abapgit_html_form=>c_cmd_type-input_main
        iv_action      = c_event-update ).
      lo_form->command(
        iv_label       = 'Remove'
        iv_action      = c_event-remove ).
      ri_html->add( lo_form->render( lo_form_data ) ).
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_event-add.
        event_add( ii_event ).
        mo_repo->refresh( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_event-update.
        event_update( ii_event ).
        mo_repo->refresh( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_event-remove.
        event_remove( ii_event ).
        mo_repo->refresh( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_event-add_via_transport.
        add_via_transport( ).
        mo_repo->refresh( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
    ENDCASE.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_DATA implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_DB <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_db=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_db=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_DB implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    super->constructor( ).
    register_stylesheet( ).
  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_db.

    CREATE OBJECT lo_component.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Database Utility'
      iv_extra_css_url      = c_css_url
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.
  METHOD do_backup_db.

    DATA:
      lt_data     TYPE Lif_abapgit_persistence=>ty_contents,
      lv_text     TYPE string,
      lt_toc      TYPE string_table,
      lo_zip      TYPE REF TO cl_abap_zip,
      lv_zip      TYPE xstring,
      lv_path     TYPE string,
      lv_filename TYPE string,
      li_fe_serv  TYPE REF TO Lif_abapgit_frontend_services.

    FIELD-SYMBOLS:
      <ls_data> LIKE LINE OF lt_data.

    lt_data = Lcl_abapgit_persistence_db=>get_instance( )->list( ).

    lv_text = |Table of Content\n|.
    INSERT lv_text INTO TABLE lt_toc.
    lv_text = |================\n|.
    INSERT lv_text INTO TABLE lt_toc.
    lv_text = |\n|.
    INSERT lv_text INTO TABLE lt_toc.

    CREATE OBJECT lo_zip.

    LOOP AT lt_data ASSIGNING <ls_data>.
      IF <ls_data>-type = Lcl_abapgit_persistence_db=>c_type_repo_csum.
        CONCATENATE <ls_data>-type '_' <ls_data>-value '.txt' INTO lv_filename.
      ELSE.
        CONCATENATE <ls_data>-type '_' <ls_data>-value '.xml' INTO lv_filename.
      ENDIF.
      lo_zip->add(
        name    = lv_filename
        content = Lcl_abapgit_convert=>string_to_xstring_utf8( <ls_data>-data_str ) ).

      lv_text = explain_content( <ls_data> ).
      REPLACE '<strong>' IN lv_text WITH ''.
      REPLACE '</strong>' IN lv_text WITH ''.
      lv_text = |{ <ls_data>-type },{ <ls_data>-value },{ lv_text }\n|.
      INSERT lv_text INTO TABLE lt_toc.
    ENDLOOP.

    lo_zip->add(
      name    = c_toc_filename
      content = Lcl_abapgit_convert=>string_to_xstring_utf8( concat_lines_of( lt_toc ) ) ).

    lv_zip = lo_zip->save( ).

    CONCATENATE 'abapGit_Backup_' sy-datlo '_' sy-timlo INTO lv_filename.

    li_fe_serv = Lcl_abapgit_ui_factory=>get_frontend_services( ).

    lv_path = li_fe_serv->show_file_save_dialog(
      iv_title            = 'abapGit Backup'
      iv_extension        = 'zip'
      iv_default_filename = lv_filename ).

    li_fe_serv->file_download(
      iv_path = lv_path
      iv_xstr = lv_zip ).

    MESSAGE 'abapGit Backup successfully saved' TYPE 'S'.

  ENDMETHOD.
  METHOD do_delete_entry.

    DATA lv_answer TYPE c LENGTH 1.

    ASSERT is_key-type IS NOT INITIAL.

    lv_answer = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
      iv_titlebar              = 'Warning'
      iv_text_question         = |Are you sure you want to delete entry { is_key-type } { is_key-value }?|
      iv_text_button_1         = 'Yes'
      iv_icon_button_1         = 'ICON_DELETE'
      iv_text_button_2         = 'No'
      iv_icon_button_2         = 'ICON_CANCEL'
      iv_default_button        = '2'
      iv_display_cancel_button = abap_false ).

    IF lv_answer = '2'.
      RAISE EXCEPTION TYPE Lcx_abapgit_cancel.
    ENDIF.

    Lcl_abapgit_persistence_db=>get_instance( )->delete(
      iv_type  = is_key-type
      iv_value = is_key-value ).

    " If deleting repo, also delete corresponding checksums
    " Other way around is ok, since checksums are automatically recreated
    IF is_key-type = Lcl_abapgit_persistence_db=>c_type_repo.
      Lcl_abapgit_persistence_db=>get_instance( )->delete(
        iv_type  = Lcl_abapgit_persistence_db=>c_type_repo_csum
        iv_value = is_key-value ).

      " Initialize repo list
      Lcl_abapgit_repo_srv=>get_instance( )->init( ).
      " TODO: think how to remove this code,
      " maybe implement subscription in persistence_db,
      " so that repo_srv receive a notification on add/delete
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.
  METHOD do_restore_db.

    DATA:
      lv_answer   TYPE c LENGTH 1,
      lo_zip      TYPE REF TO cl_abap_zip,
      lv_zip      TYPE xstring,
      lv_path     TYPE string,
      lv_filename TYPE string,
      lv_data     TYPE xstring,
      ls_data     TYPE Lif_abapgit_persistence=>ty_content,
      lt_data     TYPE Lif_abapgit_persistence=>ty_contents,
      lt_data_old TYPE Lif_abapgit_persistence=>ty_contents,
      li_fe_serv  TYPE REF TO Lif_abapgit_frontend_services.

    FIELD-SYMBOLS:
      <ls_zipfile> LIKE LINE OF lo_zip->files.

    li_fe_serv = Lcl_abapgit_ui_factory=>get_frontend_services( ).

    lv_path = li_fe_serv->show_file_open_dialog(
      iv_title            = 'Restore abapGit Backup'
      iv_extension        = 'zip'
      iv_default_filename = 'abapGit_Backup_*.zip' ).

    lv_zip = li_fe_serv->file_upload( lv_path ).

    CREATE OBJECT lo_zip.

    lo_zip->load(
      EXPORTING
        zip             = lv_zip
      EXCEPTIONS
        zip_parse_error = 1
        OTHERS          = 2 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Error loading ZIP file' ).
    ENDIF.

    LOOP AT lo_zip->files ASSIGNING <ls_zipfile> WHERE name <> c_toc_filename.
      CLEAR ls_data.
      lv_filename = <ls_zipfile>-name.
      REPLACE '.xml' IN lv_filename WITH ''.
      REPLACE '.txt' IN lv_filename WITH ''.
      IF lv_filename CP 'REPO_CS*'.
        ls_data-type  = lv_filename(7).
        ls_data-value = lv_filename+8(*).
      ELSE.
        SPLIT lv_filename AT '_' INTO ls_data-type ls_data-value.
      ENDIF.

      " Validate DB key
      TRY.
          Lcl_abapgit_persistence_db=>validate_entry_type( ls_data-type ).
        CATCH Lcx_abapgit_exception.
          Lcx_abapgit_exception=>raise( |Invalid DB entry type. This is not an abapGit Backup| ).
      ENDTRY.

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
        Lcx_abapgit_exception=>raise( |Error getting file { <ls_zipfile>-name } from ZIP| ).
      ENDIF.

      ls_data-data_str = Lcl_abapgit_convert=>xstring_to_string_utf8( lv_data ).
      INSERT ls_data INTO TABLE lt_data.
    ENDLOOP.

    lv_answer = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
      iv_titlebar              = 'Warning'
      iv_text_question         = 'All existing repositories and settings will be deleted and overwritten! Continue?'
      iv_text_button_1         = 'Restore'
      iv_icon_button_1         = 'ICON_IMPORT'
      iv_text_button_2         = 'Cancel'
      iv_icon_button_2         = 'ICON_CANCEL'
      iv_default_button        = '2'
      iv_display_cancel_button = abap_false ).

    IF lv_answer <> '1'.
      RAISE EXCEPTION TYPE Lcx_abapgit_cancel.
    ENDIF.

    lt_data_old = Lcl_abapgit_persistence_db=>get_instance( )->list( ).
    LOOP AT lt_data_old INTO ls_data.
      Lcl_abapgit_persistence_db=>get_instance( )->delete(
        iv_type  = ls_data-type
        iv_value = ls_data-value ).
    ENDLOOP.

    COMMIT WORK AND WAIT.

    LOOP AT lt_data INTO ls_data.
      Lcl_abapgit_persistence_db=>get_instance( )->add(
        iv_type  = ls_data-type
        iv_value = ls_data-value
        iv_data  = ls_data-data_str ).
    ENDLOOP.

    COMMIT WORK AND WAIT.

    MESSAGE 'abapGit Backup successfully restored' TYPE 'S'.

  ENDMETHOD.
  METHOD explain_content.

    DATA lv_descr TYPE string.
    DATA ls_explanation TYPE ty_explanation.

    CASE is_data-type.
      WHEN Lcl_abapgit_persistence_db=>c_type_repo.
        lv_descr       = 'Repo Settings'.
        ls_explanation = explain_content_repo( is_data ).

      WHEN Lcl_abapgit_persistence_db=>c_type_background.
        lv_descr       = 'Background Settings'.
        ls_explanation = explain_content_background( is_data ).

      WHEN Lcl_abapgit_persistence_db=>c_type_user.
        lv_descr       = 'Personal Settings'.
        ls_explanation-value = Lcl_abapgit_user_record=>get_instance( is_data-value )->get_name( ).

      WHEN Lcl_abapgit_persistence_db=>c_type_settings.
        lv_descr       = 'Global Settings'.

      WHEN Lcl_abapgit_persistence_db=>c_type_packages.
        lv_descr       = 'Local Package Details'.

      WHEN Lcl_abapgit_persistence_db=>c_type_repo_csum.
        lv_descr       = 'Repo Checksums'.
        ls_explanation = explain_content_repo_cs( is_data ).

      WHEN OTHERS.
        IF strlen( is_data-data_str ) >= 250.
          ls_explanation-value = is_data-data_str(250).
        ELSE.
          ls_explanation-value = is_data-data_str.
        ENDIF.

        ls_explanation-value = escape(
          val    = ls_explanation-value
          format = cl_abap_format=>e_html_attr ).
        ls_explanation-value = |<pre>{ ls_explanation-value }</pre>|.

    ENDCASE.

    IF ls_explanation-value IS NOT INITIAL.
      lv_descr = |{ lv_descr }: |.
    ENDIF.

    IF ls_explanation-extra IS NOT INITIAL.
      ls_explanation-extra = | ({ ls_explanation-extra })|.
    ENDIF.

    rv_text = |{ lv_descr }<strong>{ ls_explanation-value }</strong>{ ls_explanation-extra }|.

    IF strlen( rv_text ) >= 250.
      rv_text = rv_text(250) && '...'.
    ENDIF.

  ENDMETHOD.
  METHOD explain_content_background.

    DATA:
      ls_result TYPE match_result,
      ls_match  TYPE submatch_result,
      lv_class  TYPE string,
      ls_method LIKE LINE OF mt_methods.

    rs_expl-value = |{ Lcl_abapgit_repo_srv=>get_instance( )->get( is_data-value )->get_name( ) }|.

    FIND FIRST OCCURRENCE OF REGEX '<METHOD>(.*)</METHOD>'
      IN is_data-data_str IGNORING CASE RESULTS ls_result.
    READ TABLE ls_result-submatches INTO ls_match INDEX 1.
    IF sy-subrc = 0.
      lv_class = is_data-data_str+ls_match-offset(ls_match-length).
    ENDIF.

    IF mt_methods IS INITIAL.
      mt_methods = Lcl_abapgit_background=>list_methods( ).
    ENDIF.

    READ TABLE mt_methods INTO ls_method WITH TABLE KEY class = lv_class.
    IF sy-subrc = 0.
      rs_expl-extra = ls_method-description.
    ELSE.
      rs_expl-extra = lv_class.
    ENDIF.

  ENDMETHOD.
  METHOD explain_content_repo.

    DATA:
      ls_result TYPE match_result,
      ls_match  TYPE submatch_result,
      lv_cnt    TYPE i.

    FIND FIRST OCCURRENCE OF REGEX '<OFFLINE/>'
      IN is_data-data_str IGNORING CASE MATCH COUNT lv_cnt.
    IF lv_cnt > 0.
      rs_expl-extra = 'Online'.
    ELSE.
      rs_expl-extra = 'Offline'.
    ENDIF.

    FIND FIRST OCCURRENCE OF REGEX '<DISPLAY_NAME>(.*)</DISPLAY_NAME>'
      IN is_data-data_str IGNORING CASE RESULTS ls_result.
    READ TABLE ls_result-submatches INTO ls_match INDEX 1.
    IF sy-subrc = 0.
      rs_expl-value = is_data-data_str+ls_match-offset(ls_match-length).
    ELSE.
      FIND FIRST OCCURRENCE OF REGEX '<NAME>(.*)</NAME>'
        IN is_data-data_str IGNORING CASE RESULTS ls_result.
      READ TABLE ls_result-submatches INTO ls_match INDEX 1.
      IF sy-subrc = 0.
        rs_expl-value = is_data-data_str+ls_match-offset(ls_match-length).
      ENDIF.
    ENDIF.

    IF rs_expl-value IS INITIAL.
      FIND FIRST OCCURRENCE OF REGEX '<URL>(.*)</URL>'
        IN is_data-data_str IGNORING CASE RESULTS ls_result.
      READ TABLE ls_result-submatches INTO ls_match INDEX 1.
      IF sy-subrc = 0.
        rs_expl-value = is_data-data_str+ls_match-offset(ls_match-length).
        IF lv_cnt > 0.
          rs_expl-value = Lcl_abapgit_url=>name( rs_expl-value ).
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD explain_content_repo_cs.

    DATA lt_lines TYPE string_table.

    IF strlen( is_data-data_str ) > 0.
      SPLIT is_data-data_str AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.
      rs_expl-extra = |{ lines( lt_lines ) } lines|.

      READ TABLE lt_lines INDEX 1 INTO rs_expl-value.
      IF sy-subrc = 0.
        REPLACE '#repo_name#' IN rs_expl-value WITH ''.
        rs_expl-value = escape(
          val    = rs_expl-value
          format = cl_abap_format=>e_html_attr ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD register_stylesheet.

    DATA lo_buf TYPE REF TO Lcl_abapgit_string_buffer.

    CREATE OBJECT lo_buf.

lo_buf->add( '/*' ).
lo_buf->add( ' * PAGE DB CSS' ).
lo_buf->add( ' */' ).
lo_buf->add( '' ).
lo_buf->add( '/* LAYOUT */' ).
lo_buf->add( '' ).
lo_buf->add( '.db-list {' ).
lo_buf->add( '  padding: 0.5em;' ).
lo_buf->add( '  overflow-x: auto;' ).
lo_buf->add( '}' ).
lo_buf->add( '.db-list table { table-layout: fixed; }' ).
lo_buf->add( '.db-list table pre {' ).
lo_buf->add( '  display: inline-block;' ).
lo_buf->add( '  overflow: hidden;' ).
lo_buf->add( '  word-wrap:break-word;' ).
lo_buf->add( '  white-space: pre-wrap;' ).
lo_buf->add( '  margin: 0px;' ).
lo_buf->add( '  width: 30em;' ).
lo_buf->add( '}' ).
lo_buf->add( '.db-list table th {' ).
lo_buf->add( '  text-align: left;' ).
lo_buf->add( '  padding: 0.5em;' ).
lo_buf->add( '}' ).
lo_buf->add( '.db-list table thead tr { border-bottom: 1px solid; }' ).
lo_buf->add( '.db-list table td {' ).
lo_buf->add( '  padding: 4px 0.5em;' ).
lo_buf->add( '  vertical-align: middle;' ).
lo_buf->add( '  word-break: break-all;' ).
lo_buf->add( '}' ).
lo_buf->add( '.db-list table td.data { font-style: italic; }' ).
lo_buf->add( '' ).
lo_buf->add( '/* COLORS */' ).
lo_buf->add( '' ).
lo_buf->add( '.db-list { background-color: var(--theme-table-background-color); }' ).
lo_buf->add( '.db-list table td      { color: var(--theme-primary-font-color); }' ).
lo_buf->add( '.db-list table td.data { color: var(--theme-greyscale-dark); }' ).
lo_buf->add( '.db-list table tbody tr:hover td  { background-color: rgba(0, 0, 0, 0.075); }' ).
lo_buf->add( '.db-list table tbody tr:active td { background-color: #f4f4f4; } /* Needed? */' ).
lo_buf->add( '.db-list table th { color: var(--theme-link-color); }' ).
lo_buf->add( '.db-list table thead tr { border-color: var(--theme-table-border-color); }' ).
lo_buf->add( '' ).
    gui_services( )->register_page_asset(
      iv_url       = c_css_url
      iv_type      = 'text/css'
      iv_mime_name = 'ZABAPGIT_CSS_PAGE_DB'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

  ENDMETHOD.
  METHOD render_table.

    ri_html = Lcl_abapgit_html_table=>create( me
      )->define_column(
        iv_column_id = 'type'
        iv_column_title = 'Type'
      )->define_column(
        iv_column_id = 'value'
        iv_column_title = 'Key'
      )->define_column(
        iv_column_id = 'expl'
        iv_column_title = 'Data'
      )->define_column( 'cmd'
      )->render( it_db_entries ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    DATA ls_db TYPE Lif_abapgit_persistence=>ty_content.
    DATA lo_query TYPE REF TO Lcl_abapgit_string_map.

    lo_query = ii_event->query( ).
    CASE ii_event->mv_action.
      WHEN c_action-delete.
        lo_query->to_abap( CHANGING cs_container = ls_db ).
        do_delete_entry( ls_db ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_action-backup.
        do_backup_db( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_action-restore.
        do_restore_db( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_menu_provider~get_menu.

    CREATE OBJECT ro_toolbar.

    ro_toolbar->add(
      iv_txt = 'Backup'
      iv_act = c_action-backup ).
    ro_toolbar->add(
      iv_txt = 'Restore'
      iv_act = c_action-restore ).
    ro_toolbar->add(
      iv_txt = 'Back'
      iv_act = c_action-back ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    DATA lt_db_entries TYPE Lif_abapgit_persistence=>ty_contents.

    register_handlers( ).

    lt_db_entries = Lcl_abapgit_persistence_db=>get_instance( )->list( ).

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<div class="db-list">' ).
    ri_html->add( render_table( lt_db_entries ) ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
  METHOD Lif_abapgit_html_table~get_row_attrs.
  ENDMETHOD.
  METHOD Lif_abapgit_html_table~render_cell.

    DATA lv_action  TYPE string.
    DATA lo_toolbar TYPE REF TO Lcl_abapgit_html_toolbar.

    CASE iv_column_id.
      WHEN 'type' OR 'value'.
        rs_render-content = |{ iv_value }|.
      WHEN 'expl'.
        rs_render-content   = explain_content( is_row ).
        rs_render-css_class = 'data'.
      WHEN 'cmd'.
        lv_action  = Lcl_abapgit_html_action_utils=>dbkey_encode( is_row ).
        lo_toolbar = Lcl_abapgit_html_toolbar=>create(
          )->add(
            iv_txt = 'Display'
            iv_act = |{ Lif_abapgit_definitions=>c_action-db_display }?{ lv_action }|
          )->add(
            iv_txt = 'Edit'
            iv_act = |{ Lif_abapgit_definitions=>c_action-db_edit }?{ lv_action }|
          )->add(
            iv_txt = 'Delete'
            iv_act = |{ c_action-delete }?{ lv_action }| ).
        rs_render-html = lo_toolbar->render( ).

    ENDCASE.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_DB implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_FLOW <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_flow=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_flow=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS SHRITEFUH64VYIPO5IWUYB3KWZWSFY DEFINITION.
  PUBLIC SECTION.
    METHODS clear
      RETURNING
        VALUE(ro_stack) TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KWZWSFY.

    METHODS push
      IMPORTING
        iv_sha1 TYPE Lif_abapgit_git_definitions=>ty_sha1.

    METHODS pop
      RETURNING
        VALUE(rv_sha1) TYPE Lif_abapgit_git_definitions=>ty_sha1.

    METHODS size
      RETURNING
        VALUE(rv_size) TYPE i.
  PRIVATE SECTION.
    DATA mt_list TYPE STANDARD TABLE OF Lif_abapgit_git_definitions=>ty_sha1 WITH DEFAULT KEY.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KWZWSFY IMPLEMENTATION.
  METHOD clear.
    CLEAR mt_list.
    ro_stack = me.
  ENDMETHOD.

  METHOD push.
    INSERT iv_sha1 INTO mt_list INDEX 1.
  ENDMETHOD.

  METHOD pop.
    READ TABLE mt_list INDEX 1 INTO rv_sha1.
    ASSERT sy-subrc = 0.
    DELETE mt_list INDEX 1.
  ENDMETHOD.

  METHOD size.
    rv_size = lines( mt_list ).
  ENDMETHOD.
ENDCLASS.

***************************************************

CLASS SHRITEFUH64VYIPO5IWUYB3KWZYSFY DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_object_filter.

    METHODS constructor
      IMPORTING
        it_filter TYPE Lif_abapgit_definitions=>ty_tadir_tt.

  PRIVATE SECTION.
    DATA mt_filter TYPE Lif_abapgit_definitions=>ty_tadir_tt.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KWZYSFY IMPLEMENTATION.
  METHOD constructor.
    mt_filter = it_filter.
  ENDMETHOD.

  METHOD Lif_abapgit_object_filter~get_filter.
    rt_filter = mt_filter.
  ENDMETHOD.
ENDCLASS.

***************************************************

CLASS SHRITEFUH64VYIPO5IWUYB3KWZ2SFY DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS get_information
      RETURNING
        VALUE(rt_features) TYPE SHRITEFUH64VYIPO5IWUYB3KWZVSFY
      RAISING
        Lcx_abapgit_exception.

  PRIVATE SECTION.
    CONSTANTS c_main TYPE string VALUE 'main'.

    TYPES: BEGIN OF ty_transport,
             trkorr   TYPE trkorr,
             title    TYPE string,
             object   TYPE e071-object,
             obj_name TYPE e071-obj_name,
             devclass TYPE tadir-devclass,
           END OF ty_transport.

    TYPES ty_transports_tt TYPE STANDARD TABLE OF ty_transport WITH DEFAULT KEY.

    CLASS-METHODS build_repo_data
      IMPORTING
        io_online      TYPE REF TO Lif_abapgit_repo
      RETURNING
        VALUE(rs_data) TYPE SHRITEFUH64VYIPO5IWUYB3KWZUSFY-repo.

    CLASS-METHODS map_files_to_objects
      IMPORTING
        it_files                  TYPE SHRITEFUH64VYIPO5IWUYB3KWZTSFY
        io_online                 TYPE REF TO Lcl_abapgit_repo_online
      RETURNING
        VALUE(rt_changed_objects) TYPE Lif_abapgit_definitions=>ty_items_ts
      RAISING
        Lcx_abapgit_exception.

    CLASS-METHODS find_changed_files_all
      IMPORTING
        io_online        TYPE REF TO Lcl_abapgit_repo_online
        it_branches      TYPE Lif_abapgit_git_definitions=>ty_git_branch_list_tt
      EXPORTING
        et_main_expanded TYPE Lif_abapgit_git_definitions=>ty_expanded_tt
      CHANGING
        ct_features      TYPE SHRITEFUH64VYIPO5IWUYB3KWZVSFY
      RAISING
        Lcx_abapgit_exception.

    CLASS-METHODS try_matching_transports
      IMPORTING
        ii_repo          TYPE REF TO Lif_abapgit_repo
        it_main_expanded TYPE Lif_abapgit_git_definitions=>ty_expanded_tt
      CHANGING
        ct_features      TYPE SHRITEFUH64VYIPO5IWUYB3KWZVSFY
        ct_transports    TYPE ty_transports_tt
      RAISING
        Lcx_abapgit_exception.

    CLASS-METHODS add_objects_and_files_from_tr
      IMPORTING
        iv_trkorr        TYPE trkorr
        ii_repo          TYPE REF TO Lif_abapgit_repo
        it_transports    TYPE ty_transports_tt
        it_main_expanded TYPE Lif_abapgit_git_definitions=>ty_expanded_tt
      CHANGING
        cs_feature       TYPE SHRITEFUH64VYIPO5IWUYB3KWZUSFY
      RAISING
        Lcx_abapgit_exception.

    CLASS-METHODS find_up_to_date
      IMPORTING
        iv_url      TYPE string
        it_branches TYPE Lif_abapgit_git_definitions=>ty_git_branch_list_tt
      CHANGING
        ct_features TYPE SHRITEFUH64VYIPO5IWUYB3KWZVSFY
      RAISING
        Lcx_abapgit_exception.

    CLASS-METHODS find_prs
      IMPORTING
        iv_url      TYPE string
      CHANGING
        ct_features TYPE SHRITEFUH64VYIPO5IWUYB3KWZVSFY
      RAISING
        Lcx_abapgit_exception.

    CLASS-METHODS add_local_status
      IMPORTING
        io_online   TYPE REF TO Lcl_abapgit_repo_online
      CHANGING
        ct_features TYPE SHRITEFUH64VYIPO5IWUYB3KWZVSFY
      RAISING
        Lcx_abapgit_exception.

    CLASS-METHODS find_open_transports
      RETURNING
        VALUE(rt_transports) TYPE ty_transports_tt
      RAISING
        Lcx_abapgit_exception.

    CLASS-METHODS find_changed_files
      IMPORTING
        it_expanded1    TYPE Lif_abapgit_git_definitions=>ty_expanded_tt
        it_expanded2    TYPE Lif_abapgit_git_definitions=>ty_expanded_tt
      RETURNING
        VALUE(rt_files) TYPE SHRITEFUH64VYIPO5IWUYB3KWZTSFY.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KWZ2SFY IMPLEMENTATION.

  METHOD find_prs.

    DATA lt_pulls TYPE Lif_abapgit_pr_enum_provider=>ty_pull_requests.
    DATA ls_pull LIKE LINE OF lt_pulls.

    FIELD-SYMBOLS <ls_branch> LIKE LINE OF ct_features.


    IF lines( ct_features ) = 0.
      " only main branch
      RETURN.
    ENDIF.

    lt_pulls = Lcl_abapgit_pr_enumerator=>new( iv_url )->get_pulls( ).

    LOOP AT ct_features ASSIGNING <ls_branch>.
      READ TABLE lt_pulls INTO ls_pull WITH KEY head_branch = <ls_branch>-branch-display_name.
      IF sy-subrc = 0.
        <ls_branch>-pr-title = |{ ls_pull-title } #{ ls_pull-number }|.
        <ls_branch>-pr-url = ls_pull-html_url.
        <ls_branch>-pr-draft = ls_pull-draft.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD build_repo_data.
    rs_data-name = io_online->get_name( ).
    rs_data-key = io_online->get_key( ).
    rs_data-package = io_online->get_package( ).
  ENDMETHOD.

  METHOD get_information.

    DATA lt_branches   TYPE Lif_abapgit_git_definitions=>ty_git_branch_list_tt.
    DATA ls_branch     LIKE LINE OF lt_branches.
    DATA ls_result     LIKE LINE OF rt_features.
    DATA lt_favorites  TYPE Lif_abapgit_repo_srv=>ty_repo_list.
    DATA li_favorite   LIKE LINE OF lt_favorites.
    DATA lo_online     TYPE REF TO Lcl_abapgit_repo_online.
    DATA lt_features   LIKE rt_features.
    DATA lt_transports TYPE ty_transports_tt.
    DATA lt_main_expanded TYPE Lif_abapgit_git_definitions=>ty_expanded_tt.

    FIELD-SYMBOLS <ls_feature> LIKE LINE OF lt_features.
    FIELD-SYMBOLS <ls_path_name> LIKE LINE OF <ls_feature>-changed_files.

    lt_transports = find_open_transports( ).

* list branches on favorite + flow enabled + transported repos
    lt_favorites = Lcl_abapgit_repo_srv=>get_instance( )->list_favorites( abap_false ).
    LOOP AT lt_favorites INTO li_favorite.
      IF li_favorite->get_local_settings( )-flow = abap_false.
        CONTINUE.
      ELSEIF Lcl_abapgit_factory=>get_sap_package( li_favorite->get_package( )
          )->are_changes_recorded_in_tr_req( ) = abap_false.
        CONTINUE.
      ENDIF.

      lo_online ?= li_favorite.

      lt_branches = Lcl_abapgit_git_factory=>get_v2_porcelain( )->list_branches(
        iv_url    = lo_online->get_url( )
        iv_prefix = 'refs/heads/' )->get_all( ).

      CLEAR lt_features.
      LOOP AT lt_branches INTO ls_branch WHERE display_name <> c_main.
        ls_result-repo = build_repo_data( lo_online ).
        ls_result-branch-display_name = ls_branch-display_name.
        ls_result-branch-sha1 = ls_branch-sha1.
        INSERT ls_result INTO TABLE lt_features.
      ENDLOOP.

      find_changed_files_all(
        EXPORTING
          io_online        = lo_online
          it_branches      = lt_branches
        IMPORTING
          et_main_expanded = lt_main_expanded
        CHANGING
          ct_features      = lt_features ).

      try_matching_transports(
        EXPORTING
          ii_repo          = li_favorite
          it_main_expanded = lt_main_expanded
        CHANGING
          ct_transports    = lt_transports
          ct_features      = lt_features ).

      find_up_to_date(
        EXPORTING
          iv_url      = lo_online->get_url( )
          it_branches = lt_branches
        CHANGING
          ct_features = lt_features ).

      find_prs(
        EXPORTING
          iv_url      = lo_online->get_url( )
        CHANGING
          ct_features = lt_features ).

      add_local_status(
        EXPORTING
          io_online   = lo_online
        CHANGING
          ct_features = lt_features ).

      LOOP AT lt_features ASSIGNING <ls_feature>.
        <ls_feature>-full_match = abap_true.
        LOOP AT <ls_feature>-changed_files ASSIGNING <ls_path_name>.
          IF <ls_path_name>-remote_sha1 <> <ls_path_name>-local_sha1.
            <ls_feature>-full_match = abap_false.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      INSERT LINES OF lt_features INTO TABLE rt_features.
    ENDLOOP.

    SORT rt_features BY full_match transport-trkorr DESCENDING.

  ENDMETHOD.

  METHOD try_matching_transports.

    DATA lt_trkorr       LIKE ct_transports.
    DATA ls_trkorr       LIKE LINE OF lt_trkorr.
    DATA ls_result       LIKE LINE OF ct_features.
    DATA lt_packages     TYPE Lif_abapgit_sap_package=>ty_devclass_tt.
    DATA lv_package      LIKE LINE OF lt_packages.
    DATA lv_found        TYPE abap_bool.

    FIELD-SYMBOLS <ls_feature>   LIKE LINE OF ct_features.
    FIELD-SYMBOLS <ls_transport> LIKE LINE OF ct_transports.
    FIELD-SYMBOLS <ls_changed>   LIKE LINE OF <ls_feature>-changed_objects.


    SORT ct_transports BY object obj_name.

    LOOP AT ct_features ASSIGNING <ls_feature>.
      LOOP AT <ls_feature>-changed_objects ASSIGNING <ls_changed>.
        READ TABLE ct_transports ASSIGNING <ls_transport>
          WITH KEY object = <ls_changed>-obj_type obj_name = <ls_changed>-obj_name BINARY SEARCH.
        IF sy-subrc = 0.
          <ls_feature>-transport-trkorr = <ls_transport>-trkorr.
          <ls_feature>-transport-title = <ls_transport>-title.

          add_objects_and_files_from_tr(
            EXPORTING
              iv_trkorr        = <ls_transport>-trkorr
              ii_repo          = ii_repo
              it_main_expanded = it_main_expanded
              it_transports    = ct_transports
            CHANGING
              cs_feature       = <ls_feature> ).

          DELETE ct_transports WHERE trkorr = <ls_transport>-trkorr.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

* unmatched transports
    lt_trkorr = ct_transports.
    SORT lt_trkorr BY trkorr.
    DELETE ADJACENT DUPLICATES FROM lt_trkorr COMPARING trkorr.

    lt_packages = Lcl_abapgit_factory=>get_sap_package( ii_repo->get_package( ) )->list_subpackages( ).
    INSERT ii_repo->get_package( ) INTO TABLE lt_packages.

    LOOP AT lt_trkorr INTO ls_trkorr.
      lv_found = abap_false.
      LOOP AT lt_packages INTO lv_package.
        READ TABLE ct_transports ASSIGNING <ls_transport> WITH KEY trkorr = ls_trkorr-trkorr devclass = lv_package.
        IF sy-subrc = 0.
          lv_found = abap_true.
        ENDIF.
      ENDLOOP.
      IF lv_found = abap_false.
        CONTINUE.
      ENDIF.

      CLEAR ls_result.
      ls_result-repo = build_repo_data( ii_repo ).
      ls_result-transport-trkorr = <ls_transport>-trkorr.
      ls_result-transport-title = <ls_transport>-title.

      add_objects_and_files_from_tr(
        EXPORTING
          iv_trkorr        = ls_trkorr-trkorr
          ii_repo          = ii_repo
          it_main_expanded = it_main_expanded
          it_transports    = ct_transports
        CHANGING
          cs_feature       = ls_result ).

      INSERT ls_result INTO TABLE ct_features.
    ENDLOOP.

  ENDMETHOD.

  METHOD add_objects_and_files_from_tr.

    DATA ls_changed      LIKE LINE OF cs_feature-changed_objects.
    DATA lo_filter       TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KWZYSFY.
    DATA lt_filter       TYPE Lif_abapgit_definitions=>ty_tadir_tt.
    DATA lt_local        TYPE Lif_abapgit_definitions=>ty_files_item_tt.
    DATA ls_changed_file LIKE LINE OF cs_feature-changed_files.

    FIELD-SYMBOLS <ls_transport> LIKE LINE OF it_transports.
    FIELD-SYMBOLS <ls_local>     LIKE LINE OF lt_local.
    FIELD-SYMBOLS <ls_filter>    LIKE LINE OF lt_filter.
    FIELD-SYMBOLS <ls_main_expanded> LIKE LINE OF it_main_expanded.


    LOOP AT it_transports ASSIGNING <ls_transport> WHERE trkorr = iv_trkorr.
      ls_changed-obj_type = <ls_transport>-object.
      ls_changed-obj_name = <ls_transport>-obj_name.
      INSERT ls_changed INTO TABLE cs_feature-changed_objects.

      APPEND INITIAL LINE TO lt_filter ASSIGNING <ls_filter>.
      <ls_filter>-object = <ls_transport>-object.
      <ls_filter>-obj_name = <ls_transport>-obj_name.
    ENDLOOP.

    CREATE OBJECT lo_filter EXPORTING it_filter = lt_filter.
    lt_local = ii_repo->get_files_local_filtered( lo_filter ).
    LOOP AT lt_local ASSIGNING <ls_local> WHERE file-filename <> Lif_abapgit_definitions=>c_dot_abapgit.
      ls_changed_file-path       = <ls_local>-file-path.
      ls_changed_file-filename   = <ls_local>-file-filename.
      ls_changed_file-local_sha1 = <ls_local>-file-sha1.

      READ TABLE it_main_expanded ASSIGNING <ls_main_expanded>
        WITH TABLE KEY path_name COMPONENTS
        path = ls_changed_file-path
        name = ls_changed_file-filename.
      IF sy-subrc = 0.
        ls_changed_file-remote_sha1 = <ls_main_expanded>-sha1.
      ENDIF.

      INSERT ls_changed_file INTO TABLE cs_feature-changed_files.
    ENDLOOP.

  ENDMETHOD.

  METHOD find_open_transports.

    DATA lt_trkorr   TYPE Lif_abapgit_cts_api=>ty_trkorr_tt.
    DATA lv_trkorr   LIKE LINE OF lt_trkorr.
    DATA ls_result   LIKE LINE OF rt_transports.
    DATA lt_objects  TYPE Lif_abapgit_cts_api=>ty_transport_obj_tt.
    DATA lv_obj_name TYPE tadir-obj_name.

    FIELD-SYMBOLS <ls_object> LIKE LINE OF lt_objects.


    lt_trkorr = Lcl_abapgit_factory=>get_cts_api( )->list_open_requests_by_user( ).

    LOOP AT lt_trkorr INTO lv_trkorr.
      ls_result-trkorr  = lv_trkorr.
      ls_result-title   = Lcl_abapgit_factory=>get_cts_api( )->read_description( lv_trkorr ).

      lt_objects = Lcl_abapgit_factory=>get_cts_api( )->list_r3tr_by_request( lv_trkorr ).
      LOOP AT lt_objects ASSIGNING <ls_object>.
        ls_result-object = <ls_object>-object.
        ls_result-obj_name = <ls_object>-obj_name.

        lv_obj_name = <ls_object>-obj_name.
        ls_result-devclass = Lcl_abapgit_factory=>get_tadir( )->read_single(
          iv_object   = ls_result-object
          iv_obj_name = lv_obj_name )-devclass.
        INSERT ls_result INTO TABLE rt_transports.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

  METHOD find_up_to_date.

    DATA ls_branch  LIKE LINE OF it_branches.
    DATA lt_commits TYPE Lif_abapgit_definitions=>ty_objects_tt.
    DATA ls_main    LIKE LINE OF it_branches.
    DATA lv_current TYPE Lif_abapgit_git_definitions=>ty_sha1.
    DATA lt_sha1    TYPE Lif_abapgit_git_definitions=>ty_sha1_tt.
    DATA lo_visit   TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KWZWSFY.
    DATA ls_raw     TYPE Lcl_abapgit_git_pack=>ty_commit.

    DATA lt_main_reachable TYPE HASHED TABLE OF Lif_abapgit_git_definitions=>ty_sha1 WITH UNIQUE KEY table_line.

    FIELD-SYMBOLS <ls_branch> LIKE LINE OF ct_features.
    FIELD-SYMBOLS <ls_commit> LIKE LINE OF lt_commits.


    IF lines( it_branches ) = 1.
      " only main branch
      RETURN.
    ENDIF.

    READ TABLE it_branches INTO ls_main WITH KEY display_name = c_main.
    ASSERT sy-subrc = 0.

    LOOP AT it_branches INTO ls_branch WHERE is_head = abap_false.
      APPEND ls_branch-sha1 TO lt_sha1.
    ENDLOOP.

    lt_commits = Lcl_abapgit_git_factory=>get_v2_porcelain( )->commits_last_year(
      iv_url  = iv_url
      it_sha1 = lt_sha1 ).

    CREATE OBJECT lo_visit.
    lo_visit->clear( )->push( ls_main-sha1 ).
    WHILE lo_visit->size( ) > 0.
      lv_current = lo_visit->pop( ).
      INSERT lv_current INTO TABLE lt_main_reachable.
      READ TABLE lt_commits ASSIGNING <ls_commit> WITH TABLE KEY sha COMPONENTS sha1 = lv_current.
      IF sy-subrc = 0.
        ls_raw = Lcl_abapgit_git_pack=>decode_commit( <ls_commit>-data ).
        lo_visit->push( ls_raw-parent ).
        IF ls_raw-parent2 IS NOT INITIAL.
          lo_visit->push( ls_raw-parent2 ).
        ENDIF.
      ENDIF.
    ENDWHILE.

    LOOP AT ct_features ASSIGNING <ls_branch>.
      <ls_branch>-branch-up_to_date = abap_undefined.
      lo_visit->clear( )->push( <ls_branch>-branch-sha1 ).

      WHILE lo_visit->size( ) > 0.
        lv_current = lo_visit->pop( ).
        IF lv_current = ls_main-sha1.
          <ls_branch>-branch-up_to_date = abap_true.
          EXIT.
        ENDIF.

        READ TABLE lt_main_reachable WITH KEY table_line = lv_current TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          <ls_branch>-branch-up_to_date = abap_false.
          EXIT.
        ENDIF.

        READ TABLE lt_commits ASSIGNING <ls_commit> WITH TABLE KEY sha COMPONENTS sha1 = lv_current.
        IF sy-subrc = 0.
          ls_raw = Lcl_abapgit_git_pack=>decode_commit( <ls_commit>-data ).
          lo_visit->push( ls_raw-parent ).
          IF ls_raw-parent2 IS NOT INITIAL.
            lo_visit->push( ls_raw-parent2 ).
          ENDIF.
        ENDIF.
      ENDWHILE.

    ENDLOOP.

  ENDMETHOD.

  METHOD find_changed_files_all.

    DATA ls_branch          LIKE LINE OF it_branches.
    DATA lt_sha1            TYPE Lif_abapgit_git_definitions=>ty_sha1_tt.
    DATA lt_objects         TYPE Lif_abapgit_definitions=>ty_objects_tt.
    DATA lv_starting_folder TYPE string.
    DATA ls_main            LIKE LINE OF it_branches.
    DATA lt_expanded        TYPE Lif_abapgit_git_definitions=>ty_expanded_tt.

    FIELD-SYMBOLS <ls_branch> LIKE LINE OF ct_features.


    LOOP AT it_branches INTO ls_branch WHERE is_head = abap_false.
      APPEND ls_branch-sha1 TO lt_sha1.
    ENDLOOP.

    lt_objects = Lcl_abapgit_git_factory=>get_v2_porcelain( )->list_no_blobs_multi(
      iv_url  = io_online->get_url( )
      it_sha1 = lt_sha1 ).

    lv_starting_folder = io_online->get_dot_abapgit( )->get_starting_folder( ) && '*'.

    READ TABLE it_branches INTO ls_main WITH KEY display_name = c_main.
    ASSERT sy-subrc = 0.

    et_main_expanded = Lcl_abapgit_git_porcelain=>full_tree(
      it_objects = lt_objects
      iv_parent  = ls_main-sha1 ).
    DELETE et_main_expanded WHERE path NP lv_starting_folder.

    LOOP AT ct_features ASSIGNING <ls_branch> WHERE branch-display_name <> c_main.
      lt_expanded = Lcl_abapgit_git_porcelain=>full_tree(
        it_objects = lt_objects
        iv_parent  = <ls_branch>-branch-sha1 ).
      DELETE lt_expanded WHERE path NP lv_starting_folder.

      <ls_branch>-changed_files = find_changed_files(
        it_expanded1 = lt_expanded
        it_expanded2 = et_main_expanded ).

      <ls_branch>-changed_objects = map_files_to_objects(
        io_online = io_online
        it_files  = <ls_branch>-changed_files ).
    ENDLOOP.

  ENDMETHOD.

  METHOD add_local_status.

    DATA lt_local  TYPE Lif_abapgit_definitions=>ty_files_item_tt.
    DATA lo_filter TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KWZYSFY.
    DATA lt_filter TYPE Lif_abapgit_definitions=>ty_tadir_tt.

    FIELD-SYMBOLS <ls_branch>       LIKE LINE OF ct_features.
    FIELD-SYMBOLS <ls_local>        LIKE LINE OF lt_local.
    FIELD-SYMBOLS <ls_changed_file> TYPE SHRITEFUH64VYIPO5IWUYB3KWZRSFY.
    FIELD-SYMBOLS <ls_filter>       LIKE LINE OF lt_filter.
    FIELD-SYMBOLS <ls_object>       LIKE LINE OF <ls_branch>-changed_objects.


    LOOP AT ct_features ASSIGNING <ls_branch>.
      LOOP AT <ls_branch>-changed_objects ASSIGNING <ls_object>.
        APPEND INITIAL LINE TO lt_filter ASSIGNING <ls_filter>.
        <ls_filter>-object = <ls_object>-obj_type.
        <ls_filter>-obj_name = <ls_object>-obj_name.
      ENDLOOP.
    ENDLOOP.
    SORT lt_filter BY object obj_name.
    DELETE ADJACENT DUPLICATES FROM lt_filter COMPARING object obj_name.

    IF lines( lt_filter ) = 0.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_filter EXPORTING it_filter = lt_filter.
    lt_local = io_online->get_files_local_filtered( lo_filter ).

    LOOP AT ct_features ASSIGNING <ls_branch>.
      LOOP AT <ls_branch>-changed_files ASSIGNING <ls_changed_file>.
        READ TABLE lt_local ASSIGNING <ls_local>
          WITH KEY file-filename = <ls_changed_file>-filename
          file-path = <ls_changed_file>-path.
        IF sy-subrc = 0.
          <ls_changed_file>-local_sha1 = <ls_local>-file-sha1.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

  METHOD map_files_to_objects.

    DATA ls_item TYPE Lif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF it_files.

    LOOP AT it_files ASSIGNING <ls_file>.
      Lcl_abapgit_filename_logic=>file_to_object(
        EXPORTING
          iv_filename = <ls_file>-filename
          iv_path     = <ls_file>-path
          iv_devclass = io_online->get_package( )
          io_dot      = io_online->get_dot_abapgit( )
        IMPORTING
          es_item     = ls_item ).
      INSERT ls_item INTO TABLE rt_changed_objects.
    ENDLOOP.

  ENDMETHOD.

  METHOD find_changed_files.
* dont care if its added or removed or changed, just remove identical
* also list identical moved files

    DATA ls_path_name LIKE LINE OF rt_files.

    FIELD-SYMBOLS <ls_expanded1> LIKE LINE OF it_expanded1.
    FIELD-SYMBOLS <ls_expanded2> LIKE LINE OF it_expanded1.

    LOOP AT it_expanded1 ASSIGNING <ls_expanded1>.
      READ TABLE it_expanded2 ASSIGNING <ls_expanded2>
        WITH TABLE KEY path_name COMPONENTS
        path = <ls_expanded1>-path
        name = <ls_expanded1>-name.
      IF sy-subrc = 0 AND <ls_expanded1>-sha1 = <ls_expanded2>-sha1.
        CONTINUE.
      ENDIF.

      MOVE-CORRESPONDING <ls_expanded1> TO ls_path_name.
      ls_path_name-filename = <ls_expanded1>-name.
      ls_path_name-remote_sha1 = <ls_expanded1>-sha1.
      INSERT ls_path_name INTO TABLE rt_files.
    ENDLOOP.

    LOOP AT it_expanded2 ASSIGNING <ls_expanded2>.
      READ TABLE it_expanded1 ASSIGNING <ls_expanded1>
        WITH TABLE KEY path_name COMPONENTS
        path = <ls_expanded2>-path
        name = <ls_expanded2>-name.
      IF sy-subrc = 0 AND <ls_expanded1>-sha1 = <ls_expanded2>-sha1.
        CONTINUE.
      ENDIF.

      MOVE-CORRESPONDING <ls_expanded2> TO ls_path_name.
      ls_path_name-filename = <ls_expanded2>-name.
      ls_path_name-remote_sha1 = <ls_expanded2>-sha1.
      INSERT ls_path_name INTO TABLE rt_files.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

class LCL_ABAPGIT_GUI_PAGE_FLOW implementation.
*"* method's implementations
*include methods.
  METHOD refresh.

    DATA ls_feature LIKE LINE OF mt_features.
    DATA lo_online  TYPE REF TO Lcl_abapgit_repo_online.


    LOOP AT mt_features INTO ls_feature.
      lo_online ?= Lcl_abapgit_repo_srv=>get_instance( )->get( ls_feature-repo-key ).
      lo_online->refresh( ).
    ENDLOOP.

    CLEAR mt_features.

  ENDMETHOD.
  METHOD render_table.

    DATA ls_path_name LIKE LINE OF is_feature-changed_files.
    DATA lo_toolbar   TYPE REF TO Lcl_abapgit_html_toolbar.
    DATA lv_status    TYPE string.
    DATA lv_branch    TYPE string.
    DATA lv_param     TYPE string.


    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( |<table>| ).
    ri_html->add( |<tr><td><u>Filename</u></td><td><u>Remote</u></td><td><u>Local</u></td><td></td></tr>| ).

    lv_branch = is_feature-branch-display_name.
    IF lv_branch IS INITIAL.
      lv_branch = 'main'.
    ENDIF.

    LOOP AT is_feature-changed_files INTO ls_path_name.
      IF ls_path_name-remote_sha1 = ls_path_name-local_sha1.
        lv_status = 'Match'.
      ELSE.
        ASSERT is_feature-repo-key IS NOT INITIAL.
        lv_param = Lcl_abapgit_html_action_utils=>file_encode(
          iv_key   = is_feature-repo-key
          ig_file  = ls_path_name
          iv_extra = lv_branch ).
        lv_status = ri_html->a(
          iv_txt = 'Diff'
          iv_act = |{ Lif_abapgit_definitions=>c_action-go_file_diff }?{ lv_param }| ).
      ENDIF.

      ri_html->add( |<tr><td><tt>{ ls_path_name-path }{ ls_path_name-filename }</tt></td><td>{
        ls_path_name-remote_sha1(7) }</td><td>{
        ls_path_name-local_sha1(7) }</td><td>{ lv_status }</td></tr>| ).
    ENDLOOP.
    ri_html->add( |</table>| ).

* todo: crossout if write protected

    CREATE OBJECT lo_toolbar EXPORTING iv_id = 'toolbar-flow'.
    lo_toolbar->add( iv_txt = 'Pull'
                     iv_act = |{ c_action-pull }?index={ iv_index }&key={ is_feature-repo-key }&branch={ lv_branch }|
                     iv_opt = Lif_abapgit_html=>c_html_opt-strong ).
    lo_toolbar->add( iv_txt = 'Stage'
                     iv_act = |{ c_action-stage }?index={ iv_index }&key={ is_feature-repo-key }&branch={ lv_branch }|
                     iv_opt = Lif_abapgit_html=>c_html_opt-strong ).
    ri_html->add( lo_toolbar->render( ) ).

  ENDMETHOD.
  METHOD set_branch.

    DATA lv_branch TYPE string.
    DATA lo_online TYPE REF TO Lcl_abapgit_repo_online.

    IF iv_branch IS NOT INITIAL.
      lv_branch = 'refs/heads/' && iv_branch.
      lo_online ?= Lcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
      IF lo_online->get_selected_branch( ) <> lv_branch.
        lo_online->select_branch( lv_branch ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD constructor.
    super->constructor( ).

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_flow.

    CREATE OBJECT lo_component.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Flow'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    DATA lv_key     TYPE Lif_abapgit_persistence=>ty_value.
    DATA lv_branch  TYPE string.
    DATA lo_filter  TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KWZYSFY.
    DATA lt_filter  TYPE Lif_abapgit_definitions=>ty_tadir_tt.
    DATA lv_index   TYPE i.
    DATA lo_online  TYPE REF TO Lcl_abapgit_repo_online.
    DATA ls_feature LIKE LINE OF mt_features.

    FIELD-SYMBOLS <ls_object> LIKE LINE OF ls_feature-changed_objects.
    FIELD-SYMBOLS <ls_filter> LIKE LINE OF lt_filter.


    CASE ii_event->mv_action.
      WHEN c_action-refresh.
        refresh( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
      WHEN Lif_abapgit_definitions=>c_action-go_file_diff.
        lv_key = ii_event->query( )->get( 'KEY' ).
        lv_branch = ii_event->query( )->get( 'EXTRA' ).
        set_branch(
          iv_branch = lv_branch
          iv_key    = lv_key ).
* calling the page is done by the global router
      WHEN c_action-stage.
        lv_key = ii_event->query( )->get( 'KEY' ).
        lv_index = ii_event->query( )->get( 'INDEX' ).
        lv_branch = ii_event->query( )->get( 'BRANCH' ).
        lo_online ?= Lcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).

        READ TABLE mt_features INTO ls_feature INDEX lv_index.
        ASSERT sy-subrc = 0.

        LOOP AT ls_feature-changed_objects ASSIGNING <ls_object>.
          APPEND INITIAL LINE TO lt_filter ASSIGNING <ls_filter>.
          <ls_filter>-object = <ls_object>-obj_type.
          <ls_filter>-obj_name = <ls_object>-obj_name.
        ENDLOOP.
        CREATE OBJECT lo_filter EXPORTING it_filter = lt_filter.

        set_branch(
          iv_branch = lv_branch
          iv_key    = lv_key ).

        rs_handled-page = Lcl_abapgit_gui_page_stage=>create(
          io_repo       = lo_online
          ii_obj_filter = lo_filter ).

        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page_w_bookmark.

        refresh( ).
      WHEN c_action-pull.
        lv_key = ii_event->query( )->get( 'KEY' ).
        lv_index = ii_event->query( )->get( 'INDEX' ).
        lv_branch = ii_event->query( )->get( 'BRANCH' ).
        lo_online ?= Lcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).

        READ TABLE mt_features INTO ls_feature INDEX lv_index.
        ASSERT sy-subrc = 0.

        LOOP AT ls_feature-changed_objects ASSIGNING <ls_object>.
          APPEND INITIAL LINE TO lt_filter ASSIGNING <ls_filter>.
          <ls_filter>-object = <ls_object>-obj_type.
          <ls_filter>-obj_name = <ls_object>-obj_name.
        ENDLOOP.
        CREATE OBJECT lo_filter EXPORTING it_filter = lt_filter.

        set_branch(
          iv_branch = lv_branch
          iv_key    = lv_key ).

        rs_handled-page = Lcl_abapgit_gui_page_pull=>create(
          io_repo       = lo_online
          iv_trkorr     = ls_feature-transport-trkorr
          ii_obj_filter = lo_filter ).

        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.

        refresh( ).
    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_menu_provider~get_menu.

    CREATE OBJECT ro_toolbar EXPORTING iv_id = 'toolbar-main'.

    ro_toolbar->add(
      iv_txt = 'Refresh'
      iv_act = c_action-refresh ).

    ro_toolbar->add(
      iv_txt = Lcl_abapgit_gui_buttons=>repo_list( )
      iv_act = Lif_abapgit_definitions=>c_action-abapgit_home ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    DATA ls_feature LIKE LINE OF mt_features.
    DATA lv_index   TYPE i.


    register_handlers( ).
    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.
    ri_html->add( '<div class="repo-overview">' ).

    IF mt_features IS INITIAL.
      mt_features = SHRITEFUH64VYIPO5IWUYB3KWZ2SFY=>get_information( ).
    ENDIF.

    LOOP AT mt_features INTO ls_feature.
      lv_index = sy-tabix.

      IF lines( ls_feature-changed_files ) = 0.
* no changes, eg. only files outside of starting folder changed
        CONTINUE.
      ENDIF.

      ri_html->add( '<b><font size="+2">' && ls_feature-repo-name ).
      IF ls_feature-branch-display_name IS NOT INITIAL.
        ri_html->add( | - | ).
        ri_html->add_icon( 'code-branch' ).
        ri_html->add( ls_feature-branch-display_name ).
      ENDIF.
      IF ls_feature-transport-trkorr IS NOT INITIAL.
        ri_html->add( | - | ).
        ri_html->add_icon( 'truck-solid' ).
        ri_html->add( |<tt>{ ls_feature-transport-trkorr }</tt>| ).
      ENDIF.
      ri_html->add( |</font></b><br>| ).

      IF ls_feature-branch-display_name IS INITIAL.
        ri_html->add( |No branch found, comparing with <tt>main</tt>| ).
      ELSEIF ls_feature-pr IS NOT INITIAL.
        ri_html->add_a(
          iv_txt   = ls_feature-pr-title
          iv_act   = |{ Lif_abapgit_definitions=>c_action-url }?url={ ls_feature-pr-url }|
          iv_class = |url| ).

        IF ls_feature-pr-draft = abap_true.
          ri_html->add( 'DRAFT' ).
        ENDIF.
      ELSE.
        ri_html->add( |No PR found| ).
      ENDIF.
      ri_html->add( |<br>| ).

      IF ls_feature-transport IS NOT INITIAL.
        ri_html->add( |<tt>{ ls_feature-transport-trkorr }</tt> - { ls_feature-transport-title }<br>| ).
      ELSE.
        ri_html->add( |No corresponding transport found<br>| ).
      ENDIF.

      ri_html->add( '<br>' ).
      IF ls_feature-branch IS NOT INITIAL AND ls_feature-branch-up_to_date = abap_false.
        ri_html->add( 'Branch not up to date<br><br>' ).
        CONTINUE.
      ENDIF.

      IF ls_feature-full_match = abap_true.
        ri_html->add( |Full Match<br>| ).
      ELSE.
        ri_html->add( render_table(
          is_feature = ls_feature
          iv_index   = lv_index ) ).
      ENDIF.

* todo      LOOP AT ls_feature-changed_objects INTO ls_item.
* todo       ri_html->add( |<tt><small>{ ls_item-obj_type } { ls_item-obj_name }</small></tt><br>| ).
* todo     ENDLOOP.

      ri_html->add( '<br>' ).
    ENDLOOP.

    ri_html->add( '</div>' ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_FLOW implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_PULL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_pull=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_pull=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_PULL implementation.
*"* method's implementations
*include methods.
  METHOD choose_transport_request.

    DATA lv_transport_request TYPE trkorr.

    lv_transport_request = Lcl_abapgit_ui_factory=>get_popups( )->popup_transport_request( ).

    IF lv_transport_request IS NOT INITIAL.
      mo_form_data->set(
        iv_key = c_id-transport_request
        iv_val = lv_transport_request ).
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    super->constructor( ).

    mo_repo       = io_repo.
    mi_obj_filter = ii_obj_filter.

    CREATE OBJECT mo_form_data.
    mo_form_data->set(
      iv_key = c_id-transport_request
      iv_val = iv_trkorr ).

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_pull.

    CREATE OBJECT lo_component
      EXPORTING
        io_repo       = io_repo
        iv_trkorr     = iv_trkorr
        ii_obj_filter = ii_obj_filter.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Pull'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.
  METHOD form.

    DATA lt_filter TYPE Lif_abapgit_definitions=>ty_tadir_tt.

    FIELD-SYMBOLS <ls_overwrite> LIKE LINE OF ms_checks-overwrite.


    IF mi_obj_filter IS NOT INITIAL.
      lt_filter = mi_obj_filter->get_filter( ).
    ENDIF.

    ro_form = Lcl_abapgit_html_form=>create( iv_form_id = 'pull-form' ).

    ro_form->start_group(
      iv_name  = 'id-objects'
      iv_label = 'Objects' ).

    LOOP AT ms_checks-overwrite ASSIGNING <ls_overwrite>.
      IF lines( lt_filter ) > 0.
        READ TABLE lt_filter WITH KEY object = <ls_overwrite>-obj_type
          obj_name = <ls_overwrite>-obj_name TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
      ENDIF.
      ro_form->checkbox(
        iv_label = |{ <ls_overwrite>-obj_type } { <ls_overwrite>-obj_name }|
        iv_name  = |{ <ls_overwrite>-obj_type }-{ <ls_overwrite>-obj_name }| ).
    ENDLOOP.

    ro_form->text(
      iv_name        = c_id-transport_request
      iv_required    = abap_true
      iv_upper_case  = abap_true
      iv_side_action = c_action-choose_tr
      iv_max         = 10
      iv_label       = |Transport Request| ).

    ro_form->command(
      iv_label    = 'Pull'
      iv_cmd_type = Lif_abapgit_html_form=>c_cmd_type-input_main
      iv_action   = c_action-pull
    )->command(
      iv_label    = 'Back'
      iv_action   = Lif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    DATA lo_log TYPE REF TO Lcl_abapgit_log.
    DATA lv_value TYPE string.

    FIELD-SYMBOLS <ls_overwrite> LIKE LINE OF ms_checks-overwrite.


    mo_form_data = ii_event->form_data( ).

    CASE ii_event->mv_action.
      WHEN c_action-refresh.
        mo_repo->refresh( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_action-choose_tr.
        choose_transport_request( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_action-pull.
        ms_checks-transport-transport = mo_form_data->get( c_id-transport_request ).

        LOOP AT ms_checks-overwrite ASSIGNING <ls_overwrite>.
          lv_value = mo_form_data->get( |{ <ls_overwrite>-obj_type }-{ <ls_overwrite>-obj_name }| ).
          IF lv_value = 'on'.
            <ls_overwrite>-decision = Lif_abapgit_definitions=>c_yes.
          ELSE.
            <ls_overwrite>-decision = Lif_abapgit_definitions=>c_no.
          ENDIF.
        ENDLOOP.

* todo, show log?
        CREATE OBJECT lo_log.
        mo_repo->deserialize(
          is_checks = ms_checks
          ii_log    = lo_log ).

        rs_handled-state = Lcl_abapgit_gui=>c_event_state-go_back.
    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_menu_provider~get_menu.

    CREATE OBJECT ro_toolbar EXPORTING iv_id = 'toolbar-main'.

    ro_toolbar->add(
      iv_txt = 'Refresh'
      iv_act = c_action-refresh ).

    ro_toolbar->add(
      iv_txt = 'Back'
      iv_act = Lif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    register_handlers( ).

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.
    ri_html->add( '<div class="repo-overview">' ).

    ms_checks = mo_repo->deserialize_checks( ).

    IF lines( ms_checks-overwrite ) = 0.
      Lcx_abapgit_exception=>raise(
        'There is nothing to pull. The local state completely matches the remote repository.' ).
    ENDIF.

    ri_html->add( form( )->render( mo_form_data ) ).

    ri_html->add( '</div>' ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_PULL implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_REPO_OVER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_repo_overccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_repo_overccimp.
CLASS SHRITEFUH64VYIPO5IWUYB3KWZ7SFY DEFINITION FINAL.
  " TODO: move to a global class, when table is separated as a component
  PUBLIC SECTION.
    DATA mt_col_spec TYPE Lif_abapgit_definitions=>ty_col_spec_tt READ-ONLY.

    METHODS add_column
      IMPORTING
        iv_tech_name      TYPE string OPTIONAL
        iv_display_name   TYPE string OPTIONAL
        iv_css_class      TYPE string OPTIONAL
        iv_add_tz         TYPE abap_bool OPTIONAL
        iv_title          TYPE string OPTIONAL
        iv_allow_order_by TYPE any OPTIONAL
      RETURNING
        VALUE(ro_me) TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KWZ7SFY.

ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KWZ7SFY IMPLEMENTATION.

  METHOD add_column.

    FIELD-SYMBOLS <ls_col> LIKE LINE OF mt_col_spec.
    APPEND INITIAL LINE TO mt_col_spec ASSIGNING <ls_col>.
    <ls_col>-display_name   = iv_display_name.
    <ls_col>-tech_name      = iv_tech_name.
    <ls_col>-title          = iv_title.
    <ls_col>-css_class      = iv_css_class.
    <ls_col>-add_tz         = iv_add_tz.
    <ls_col>-allow_order_by = iv_allow_order_by.

    ro_me = me.

  ENDMETHOD.

ENDCLASS.

class LCL_ABAPGIT_GUI_PAGE_REPO_OVER implementation.
*"* method's implementations
*include methods.
  METHOD apply_filter.

    DATA lv_pfxl TYPE i.
    DATA lv_idx TYPE i.
    DATA lv_filter_label TYPE string.
    FIELD-SYMBOLS <ls_r> LIKE LINE OF ct_overview.

    IF ms_list_settings-filter IS INITIAL.
      RETURN.
    ENDIF.

    lv_pfxl = strlen( c_label_filter_prefix ).

    IF strlen( ms_list_settings-filter ) > lv_pfxl AND ms_list_settings-filter+0(lv_pfxl) = c_label_filter_prefix.
      lv_filter_label = ms_list_settings-filter+lv_pfxl.
      IF lv_filter_label = 'all'.
        DELETE ct_overview WHERE labels IS INITIAL.
      ELSEIF lv_filter_label = 'none'.
        DELETE ct_overview WHERE labels IS NOT INITIAL.
      ELSE.
        LOOP AT ct_overview ASSIGNING <ls_r>.
          lv_idx = sy-tabix.
          READ TABLE <ls_r>-labels TRANSPORTING NO FIELDS WITH KEY table_line = lv_filter_label.
          IF sy-subrc <> 0.
            DELETE ct_overview INDEX lv_idx.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE. " Regular filter
      DELETE ct_overview WHERE
            key             NS ms_list_settings-filter
        AND name            NS ms_list_settings-filter
        AND url             NS ms_list_settings-filter
        AND package         NS ms_list_settings-filter
        AND branch          NS ms_list_settings-filter
        AND created_by      NS ms_list_settings-filter
        AND created_at      NS ms_list_settings-filter
        AND deserialized_by NS ms_list_settings-filter
        AND deserialized_at NS ms_list_settings-filter.
    ENDIF.

  ENDMETHOD.
  METHOD apply_order_by.

    DATA:
      lt_sort TYPE abap_sortorder_tab,
      ls_sort LIKE LINE OF lt_sort.

    ls_sort-name = 'FAVORITE'.
    ls_sort-descending = abap_true.
    ls_sort-astext = abap_true.
    INSERT ls_sort INTO TABLE lt_sort.

    IF ms_list_settings-order_by IS NOT INITIAL.

      CLEAR ls_sort.

      IF ms_list_settings-order_by = 'CREATED_AT' OR ms_list_settings-order_by = 'DESERIALIZED_AT'.
        ls_sort-name = ms_list_settings-order_by && c_raw_field_suffix.
      ELSE.
        ls_sort-name   = ms_list_settings-order_by.
        ls_sort-astext = abap_true.
      ENDIF.

      ls_sort-descending = ms_list_settings-order_descending.
      INSERT ls_sort INTO TABLE lt_sort.

    ENDIF.

    SORT ct_overview BY (lt_sort).

  ENDMETHOD.
  METHOD build_table_scheme.

    DATA lo_tab_scheme TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KWZ7SFY.

    CREATE OBJECT lo_tab_scheme.

    lo_tab_scheme->add_column(
      iv_tech_name      = 'FAVORITE'
      iv_css_class      = 'wmin'
      iv_allow_order_by = abap_false
    )->add_column(
      iv_tech_name      = 'TYPE'
      iv_css_class      = 'wmin'
      iv_allow_order_by = abap_false
    )->add_column(
      iv_tech_name      = 'NAME'
      iv_display_name   = 'Name'
      iv_allow_order_by = abap_true ).

    IF mt_all_labels IS NOT INITIAL.
      lo_tab_scheme->add_column(
        iv_tech_name      = 'LABELS'
        iv_display_name   = 'Labels'
        iv_allow_order_by = abap_false ).
    ENDIF.

    lo_tab_scheme->add_column(
      iv_tech_name      = 'PACKAGE'
      iv_display_name   = 'Package'
      iv_css_class      = 'package'
      iv_allow_order_by = abap_true
    )->add_column(
      iv_tech_name      = 'URL'
      iv_display_name   = 'Remote'
      iv_allow_order_by = abap_true
    )->add_column(
      iv_tech_name      = 'BRANCH'
      iv_display_name   = 'Branch/Tag'
      iv_allow_order_by = abap_true
    )->add_column(
      iv_tech_name      = 'DESERIALIZED_BY'
      iv_display_name   = 'Deserialized by'
      iv_css_class      = 'ro-detail'
      iv_allow_order_by = abap_true
    )->add_column(
      iv_tech_name      = 'DESERIALIZED_AT'
      iv_display_name   = 'Deserialized at'
      iv_css_class      = 'ro-detail'
      iv_allow_order_by = abap_true
    )->add_column(
      iv_tech_name      = 'CREATED_BY'
      iv_display_name   = 'Created by'
      iv_css_class      = 'ro-detail'
      iv_allow_order_by = abap_true
    )->add_column(
      iv_tech_name      = 'CREATED_AT'
      iv_display_name   = 'Created at'
      iv_css_class      = 'ro-detail'
      iv_add_tz         = abap_true
      iv_allow_order_by = abap_true
    )->add_column(
      iv_tech_name      = 'KEY'
      iv_display_name   = 'Key'
      iv_css_class      = 'ro-detail'
      iv_allow_order_by = abap_true
    )->add_column(
      iv_tech_name      = 'GO'
      iv_css_class      = 'ro-go wmin'
      iv_allow_order_by = abap_false ).

    rt_tab_scheme = lo_tab_scheme->mt_col_spec.

  ENDMETHOD.
  METHOD collect_all_labels.

    FIELD-SYMBOLS <ls_r> LIKE LINE OF it_overview.

    LOOP AT it_overview ASSIGNING <ls_r>.
      APPEND LINES OF <ls_r>-labels TO rt_list.
    ENDLOOP.

    SORT rt_list.
    DELETE rt_list WHERE table_line IS INITIAL.
    DELETE ADJACENT DUPLICATES FROM rt_list.

  ENDMETHOD.
  METHOD constructor.

    super->constructor( ).

    ms_list_settings = Lcl_abapgit_persistence_user=>get_instance( )->get_list_settings( ).

    " Overwrite setting
    IF iv_only_favorites = abap_true.
      ms_list_settings-only_favorites = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_repo_over.

    CREATE OBJECT lo_component
      EXPORTING
        iv_only_favorites = iv_only_favorites.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Repository List'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.
  METHOD map_repo_list_to_overview.

    DATA ls_overview      LIKE LINE OF rt_overview.
    FIELD-SYMBOLS <ls_repo> LIKE LINE OF it_repo_obj_list.

    LOOP AT it_repo_obj_list ASSIGNING <ls_repo>.

      CLEAR ls_overview.

      ls_overview-favorite        = Lcl_abapgit_persistence_user=>get_instance(
        )->is_favorite_repo( <ls_repo>->ms_data-key ).
      ls_overview-type            = <ls_repo>->ms_data-offline.
      ls_overview-key             = <ls_repo>->ms_data-key.
      ls_overview-name            = <ls_repo>->get_name( ).
      ls_overview-labels          = Lcl_abapgit_repo_labels=>split( <ls_repo>->ms_data-local_settings-labels ).
      ls_overview-url             = <ls_repo>->ms_data-url.
      ls_overview-package         = <ls_repo>->ms_data-package.
      ls_overview-branch          = <ls_repo>->ms_data-branch_name.
      ls_overview-created_by      = <ls_repo>->ms_data-created_by.
      ls_overview-write_protected = <ls_repo>->ms_data-local_settings-write_protected.
      ls_overview-flow            = <ls_repo>->ms_data-local_settings-flow.
      ls_overview-created_at_raw  = <ls_repo>->ms_data-created_at.

      IF <ls_repo>->ms_data-created_at IS NOT INITIAL.
        ls_overview-created_at = Lcl_abapgit_gui_chunk_lib=>render_timestamp( <ls_repo>->ms_data-created_at ).
      ENDIF.

      ls_overview-deserialized_by     = <ls_repo>->ms_data-deserialized_by.
      ls_overview-deserialized_at_raw = <ls_repo>->ms_data-deserialized_at.

      IF <ls_repo>->ms_data-deserialized_at IS NOT INITIAL.
        ls_overview-deserialized_at = Lcl_abapgit_gui_chunk_lib=>render_timestamp( <ls_repo>->ms_data-deserialized_at ).
      ENDIF.

      INSERT ls_overview INTO TABLE rt_overview.

    ENDLOOP.

  ENDMETHOD.
  METHOD prepare_overviews.

    DATA lt_repo_obj_list TYPE Lif_abapgit_repo_srv=>ty_repo_list.

    IF ms_list_settings-only_favorites = abap_true.
      lt_repo_obj_list = Lcl_abapgit_repo_srv=>get_instance( )->list_favorites( ).
    ELSE.
      lt_repo_obj_list = Lcl_abapgit_repo_srv=>get_instance( )->list( ).
    ENDIF.

    rt_overviews = map_repo_list_to_overview( lt_repo_obj_list ).

    " Hmmm, side effect, not ideal, but we need label list before filter applied
    mt_all_labels = collect_all_labels( rt_overviews ).

    apply_order_by( CHANGING ct_overview = rt_overviews ).
    apply_filter( CHANGING ct_overview = rt_overviews ).

  ENDMETHOD.
  METHOD render_action_toolbar.

    CONSTANTS:
      lc_dummy_key     TYPE string VALUE `?key=#`,
      lc_offline_class TYPE string VALUE `action_offline_repo`,
      lc_online_class  TYPE string VALUE `action_online_repo`,
      lc_action_class  TYPE string VALUE `action_link`.

    DATA lo_toolbar TYPE REF TO Lcl_abapgit_html_toolbar.
    DATA lo_toolbar_more_sub TYPE REF TO Lcl_abapgit_html_toolbar.

    CREATE OBJECT lo_toolbar EXPORTING iv_id = 'toolbar-ovp'.

    lo_toolbar->add(
      iv_txt      = |Pull|
      iv_act      = |{ Lif_abapgit_definitions=>c_action-git_pull }{ lc_dummy_key }|
      iv_class    = |{ lc_action_class } { lc_online_class }|
      iv_li_class = |{ lc_action_class }| ).

    lo_toolbar->add(
      iv_txt      = |Stage|
      iv_act      = |{ Lif_abapgit_definitions=>c_action-go_stage }{ lc_dummy_key }|
      iv_class    = |{ lc_action_class } { lc_online_class }|
      iv_li_class = |{ lc_action_class }| ).

    lo_toolbar->add(
      iv_txt      = |Patch|
      iv_act      = |{ Lif_abapgit_definitions=>c_action-go_patch }{ lc_dummy_key }|
      iv_class    = |{ lc_action_class } { lc_online_class }|
      iv_li_class = |{ lc_action_class }| ).

    lo_toolbar->add(
      iv_txt      = |Diff|
      iv_act      = |{ Lif_abapgit_definitions=>c_action-go_repo_diff }{ lc_dummy_key }|
      iv_class    = |{ lc_action_class } { lc_online_class }|
      iv_li_class = |{ lc_action_class }| ).

    lo_toolbar->add(
      iv_txt      = |Check|
      iv_act      = |{ Lif_abapgit_definitions=>c_action-repo_code_inspector }{ lc_dummy_key }|
      iv_class    = |{ lc_action_class }|
      iv_li_class = |{ lc_action_class }| ).

    lo_toolbar->add(
      iv_txt      = |Import|
      iv_act      = |{ Lif_abapgit_definitions=>c_action-zip_import }{ lc_dummy_key }|
      iv_class    = |{ lc_action_class } { lc_offline_class }|
      iv_li_class = |{ lc_action_class }| ).

    lo_toolbar->add(
      iv_txt      = |Export|
      iv_act      = |{ Lif_abapgit_definitions=>c_action-zip_export }{ lc_dummy_key }|
      iv_class    = |{ lc_action_class } { lc_offline_class }|
      iv_li_class = |{ lc_action_class }| ).

    lo_toolbar->add(
      iv_txt      = |Settings|
      iv_act      = |{ Lif_abapgit_definitions=>c_action-repo_settings }{ lc_dummy_key }|
      iv_class    = |{ lc_action_class }|
      iv_li_class = |{ lc_action_class }| ).

    CREATE OBJECT lo_toolbar_more_sub EXPORTING iv_id = 'toolbar-ovp-more_sub'.

    lo_toolbar_more_sub->add(
      iv_txt      = |Stage by Transport|
      iv_act      = |{ Lif_abapgit_definitions=>c_action-go_stage_transport }{ lc_dummy_key }|
      iv_class    = |{ lc_action_class } { lc_online_class }|
      iv_li_class = |{ lc_action_class }| ).

    lo_toolbar_more_sub->add(
      iv_txt      = |Export by Transport|
      iv_act      = |{ Lif_abapgit_definitions=>c_action-zip_export_transport }{ lc_dummy_key }|
      iv_class    = |{ lc_action_class } { lc_offline_class }|
      iv_li_class = |{ lc_action_class }| ).

    lo_toolbar_more_sub->add(
      iv_txt = 'Danger'
      iv_typ = Lif_abapgit_html=>c_action_type-separator ).

    lo_toolbar_more_sub->add(
      iv_txt   = |Remove Repository|
      iv_title = |Remove abapGit's records of the repository (the system's |
              && |development objects will remain unaffected)|
      iv_act   = |{ Lif_abapgit_definitions=>c_action-repo_remove }{ lc_dummy_key }|
      iv_class    = |{ lc_action_class }|
      iv_li_class = |{ lc_action_class }| ).

    lo_toolbar_more_sub->add(
      iv_txt      = |Remove Objects|
      iv_title    = |Delete all development objects belonging to this package |
                 && |(and subpackages) from the system, but keep repository in abapGit|
      iv_act      = |{ Lif_abapgit_definitions=>c_action-repo_delete_objects }{ lc_dummy_key }|
      iv_class    = |{ lc_action_class }|
      iv_li_class = |{ lc_action_class }| ).

    lo_toolbar_more_sub->add(
      iv_txt      = |Uninstall|
      iv_title    = |Delete all development objects belonging to this package |
                 && |(and subpackages) from the system, and remove the repository from abapGit|
      iv_act      = |{ Lif_abapgit_definitions=>c_action-repo_purge }{ lc_dummy_key }|
      iv_class    = |{ lc_action_class }|
      iv_li_class = |{ lc_action_class }| ).

    lo_toolbar->add(
      iv_txt      = |More|
      io_sub      = lo_toolbar_more_sub
      iv_class    = |{ lc_action_class }|
      iv_li_class = |{ lc_action_class }| ).

    ri_html = lo_toolbar->render( iv_right = abap_true ).

  ENDMETHOD.
  METHOD render_filter_bar.

    DATA lv_icon_class TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( |<form class="inline" method="post" action="sapevent:{ c_action-apply_filter }">| ).
    ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_text_input(
      iv_name      = |filter|
      iv_label     = |Filter: { render_filter_help_hint( ) }|
      iv_value     = ms_list_settings-filter ) ).
    ri_html->add( |<input type="submit" class="hidden-submit">| ).
    ri_html->add( |</form>| ).

    IF ms_list_settings-only_favorites = abap_true.
      lv_icon_class = `blue`.
    ELSE.
      lv_icon_class = `grey`.
    ENDIF.

    ri_html->add( '<span class="toolbar-light pad-sides">' ).
    ri_html->add( ri_html->a(
      iv_txt   = |<i id="icon-filter-favorite" class="icon icon-check { lv_icon_class }"></i> Only Favorites|
      iv_class = 'command'
      iv_act   = |{ Lif_abapgit_definitions=>c_action-toggle_favorites }| ) ).
    ri_html->add( ri_html->a(
      iv_txt   = '<i id="icon-filter-detail" class="icon icon-check"></i> Detail'
      iv_act   = |gHelper.toggleRepoListDetail()|
      iv_class = 'command'
      iv_typ   = Lif_abapgit_html=>c_action_type-onclick ) ).
    ri_html->add( '</span>' ).

  ENDMETHOD.
  METHOD render_filter_help_hint.

    DATA lt_fragments TYPE string_table.

    APPEND `Filter is applied to all text fields in the below table.` TO lt_fragments.
    APPEND ` Search works for any portion of the text (so can be a mid part as well).` TO lt_fragments.
    APPEND `<br>Starting query from <code>label:xxx</code> will filter appropriate label.` TO lt_fragments.
    APPEND `Two "special" label queries are available:` TO lt_fragments.
    APPEND ` <code>all</code> (to select all repos that has at least one label)` TO lt_fragments.
    APPEND ` and <code>none</code> (to select unlabeled repos).` TO lt_fragments.

    rv_html = Lcl_abapgit_gui_chunk_lib=>render_help_hint( concat_lines_of( table = lt_fragments ) ).

  ENDMETHOD.
  METHOD render_header_bar.

    ii_html->add( |<div class="repo-overview-toolbar">| ).
    ii_html->add( render_filter_bar( ) ).
    ii_html->add( render_action_toolbar( ) ).
    ii_html->add( |</div>| ).

  ENDMETHOD.
  METHOD render_header_label_list.

    IF mt_all_labels IS INITIAL.
      RETURN.
    ENDIF.

    ii_html->add( |<div class="repo-label-catalog">| ).
    ii_html->add( '<label>Filter by label:</label>' ).
    ii_html->add( Lcl_abapgit_gui_chunk_lib=>render_label_list(
      it_labels           = mt_all_labels
      io_label_colors     = mo_label_colors
      iv_clickable_action = c_action-label_filter ) ).
    ii_html->add( |</div>| ).

  ENDMETHOD.
  METHOD render_repo_list.

    ii_html->add( |<table>| ).

    render_table_header( ii_html ).
    render_table_body(
      ii_html      = ii_html
      it_repo_list = it_overview ).
    render_table_footer( ii_html ).

    ii_html->add( |</table>| ).

  ENDMETHOD.
  METHOD render_scripts.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).
    ri_html->add( 'var gHelper = new RepoOverViewHelper({ focusFilterKey: "f" });' ).

  ENDMETHOD.
  METHOD render_table_body.

    FIELD-SYMBOLS <ls_repo> LIKE LINE OF it_repo_list.

    ii_html->add( '<tbody>' ).

    LOOP AT it_repo_list ASSIGNING <ls_repo>.
      render_table_item(
        ii_html = ii_html
        is_repo = <ls_repo> ).
    ENDLOOP.

    ii_html->add( |</tbody>| ).

  ENDMETHOD.
  METHOD render_table_footer.

    DATA lv_action TYPE string.

    IF ms_list_settings-only_favorites = abap_true.
      lv_action = ii_html->a(
        iv_txt = 'Show All'
        iv_act = |{ Lif_abapgit_definitions=>c_action-toggle_favorites }?force_state={ abap_false }| ).

      ii_html->add( Lcl_abapgit_gui_chunk_lib=>render_table_footer( |(Only favorites are shown. { lv_action })| ) ).
    ENDIF.

  ENDMETHOD.
  METHOD render_table_header.

    ii_html->add( Lcl_abapgit_gui_chunk_lib=>render_table_header(
      it_col_spec         = build_table_scheme( )
      iv_order_by         = ms_list_settings-order_by
      iv_order_descending = ms_list_settings-order_descending ) ).

  ENDMETHOD.
  METHOD render_table_item.

    DATA:
      lv_is_online_repo TYPE abap_bool,
      lv_repo_type_icon TYPE string,
      lv_favorite_icon  TYPE string,
      lv_fav_tr_class   TYPE string,
      lv_lock           TYPE string,
      lv_flow           TYPE string.

    lv_is_online_repo = boolc( is_repo-type = abap_false ).

    " Start of row
    IF is_repo-favorite = abap_true.
      lv_fav_tr_class = ' class="favorite"'.
    ELSE.
      lv_fav_tr_class = ''.
    ENDIF.

    ii_html->add( |<tr{ lv_fav_tr_class } data-key="{ is_repo-key }" data-offline="{ is_repo-type }">| ).

    " Favorite
    lv_favorite_icon = ii_html->icon(
      iv_name  = 'star/grey' " blue is added in css, based on TR style
      iv_class = 'pad-sides'
      iv_hint  = 'Click to toggle favorite' ).

    ii_html->td(
      iv_class   = 'wmin'
      iv_content = ii_html->a(
        iv_act = |{ Lif_abapgit_definitions=>c_action-repo_toggle_fav }?key={ is_repo-key }|
        iv_txt = lv_favorite_icon ) ).

    " Online/Offline
    IF lv_is_online_repo = abap_true.
      lv_repo_type_icon = 'cloud-upload-alt/darkgrey'.
    ELSE.
      lv_repo_type_icon = 'plug/darkgrey'.
    ENDIF.

    ii_html->td(
      iv_class   = 'wmin'
      iv_content = ii_html->icon( lv_repo_type_icon ) ).

    " Repo name
    IF is_repo-write_protected = abap_true.
      lv_lock = ii_html->icon(
        iv_name  = 'lock/grey70'
        iv_class = 'm-em5-sides'
        iv_hint  = 'Locked from pulls' ).
    ENDIF.
    IF is_repo-flow = abap_true.
      lv_flow = ii_html->icon(
        iv_name  = 'flow/grey70'
        iv_class = 'm-em5-sides'
        iv_hint  = 'Flow' ).
    ENDIF.

    ii_html->td(
      ii_html->a(
        iv_txt = is_repo-name
        iv_act = |{ c_action-select }?key={ is_repo-key }| ) && lv_lock && lv_flow ).

    " Labels
    IF mt_all_labels IS NOT INITIAL.
      ii_html->td(
        iv_content = Lcl_abapgit_gui_chunk_lib=>render_label_list(
          it_labels           = is_repo-labels
          io_label_colors     = mo_label_colors
          iv_clickable_action = c_action-label_filter )
        iv_class   = 'labels' ).
    ENDIF.

    " Package
    ii_html->td( ii_content = Lcl_abapgit_gui_chunk_lib=>render_package_name(
      iv_package        = is_repo-package
      iv_suppress_title = boolc( NOT ms_list_settings-only_favorites = abap_true ) ) ).

    " Repo URL
    IF lv_is_online_repo = abap_true.
      ii_html->td( ii_content = Lcl_abapgit_gui_chunk_lib=>render_repo_url(
        iv_url = is_repo-url
        iv_render_remote_edit_for_key = is_repo-key ) ).
    ELSE.
      ii_html->td( ).
    ENDIF.

    " Branch
    IF is_repo-branch IS INITIAL.
      ii_html->td( ).
    ELSE.
      ii_html->td( ii_content = Lcl_abapgit_gui_chunk_lib=>render_branch_name(
        iv_branch   = is_repo-branch
        iv_repo_key = is_repo-key ) ).
    ENDIF.

    " Details: deserialized by
    ii_html->td(
      iv_class   = 'ro-detail'
      ii_content = Lcl_abapgit_gui_chunk_lib=>render_user_name(
        iv_username       = is_repo-deserialized_by
        iv_suppress_title = boolc( NOT ms_list_settings-only_favorites = abap_true ) ) ).

    " Details: deserialized at
    ii_html->td(
      iv_class = 'ro-detail'
      iv_content = is_repo-deserialized_at ).

    " Details: created by
    ii_html->td(
      iv_class   = 'ro-detail'
      ii_content = Lcl_abapgit_gui_chunk_lib=>render_user_name(
        iv_username = is_repo-created_by
        iv_suppress_title = boolc( NOT ms_list_settings-only_favorites = abap_true ) ) ).

    " Details: created at
    ii_html->td(
      iv_class = 'ro-detail'
      iv_content = is_repo-created_at ).

    " Details: repo key
    ii_html->td(
      iv_class = 'ro-detail'
      iv_content = |{ is_repo-key }| ).

    " Go-to action
    ii_html->td(
      iv_class = 'ro-go wmin'
      iv_content = ii_html->a(
        iv_title = 'Open'
        iv_txt   = '&rtrif;'
        iv_act   = |{ c_action-select }?key={ is_repo-key }| ) ).

    ii_html->add( `</tr>` ).

  ENDMETHOD.
  METHOD set_filter.

    FIELD-SYMBOLS <lv_postdata> LIKE LINE OF it_postdata.

    READ TABLE it_postdata ASSIGNING <lv_postdata> INDEX 1.
    IF sy-subrc = 0.
      FIND FIRST OCCURRENCE OF REGEX `filter=(.*)`
        IN <lv_postdata>
        SUBMATCHES ms_list_settings-filter.
    ENDIF.

    ms_list_settings-filter = condense( ms_list_settings-filter ).
    save_settings( ).

  ENDMETHOD.
  METHOD set_order_by.
    IF ms_list_settings-order_by <> iv_order_by.
      set_order_direction( abap_false ). " Reset ordering
    ENDIF.
    ms_list_settings-order_by = iv_order_by.
    save_settings( ).
  ENDMETHOD.
  METHOD set_order_direction.
    ms_list_settings-order_descending = iv_order_descending.
    save_settings( ).
  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    DATA lv_key TYPE Lif_abapgit_persistence=>ty_value.

    lv_key = ii_event->query( )->get( 'KEY' ).

    CASE ii_event->mv_action.
      WHEN c_action-select.

        Lcl_abapgit_persistence_user=>get_instance( )->set_repo_show( lv_key ).

        TRY.
            Lcl_abapgit_repo_srv=>get_instance( )->get( lv_key )->refresh( ).
          CATCH Lcx_abapgit_exception ##NO_HANDLER.
        ENDTRY.

        rs_handled-page  = Lcl_abapgit_gui_page_repo_view=>create( lv_key ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.

      WHEN Lif_abapgit_definitions=>c_action-change_order_by.

        set_order_by( ii_event->query( )->get( 'ORDERBY' ) ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

      WHEN Lif_abapgit_definitions=>c_action-toggle_favorites.

        IF ii_event->query( )->has( 'FORCE_STATE' ) = abap_true.
          ms_list_settings-only_favorites = ii_event->query( )->get( 'FORCE_STATE' ).
        ELSE.
          ms_list_settings-only_favorites = boolc( ms_list_settings-only_favorites = abap_false ).
        ENDIF.
        save_settings( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

      WHEN Lif_abapgit_definitions=>c_action-direction.

        set_order_direction( boolc( ii_event->query( )->get( 'DIRECTION' ) = 'DESCENDING' ) ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-apply_filter.

        set_filter( ii_event->mt_postdata ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-label_filter.

        IF ii_event->mv_getdata IS NOT INITIAL.
          ms_list_settings-filter = c_label_filter_prefix && ii_event->mv_getdata.
        ELSE.
          CLEAR ms_list_settings-filter. " Unexpected request
        ENDIF.
        save_settings( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    ls_hotkey_action-ui_component = 'Repo overview'.

    ls_hotkey_action-description   = |New Online Repository|.
    ls_hotkey_action-action = Lif_abapgit_definitions=>c_action-repo_newonline.
    ls_hotkey_action-hotkey = |n|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |New Offline Repository|.
    ls_hotkey_action-action = Lif_abapgit_definitions=>c_action-repo_newoffline.
    ls_hotkey_action-hotkey = |o|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |abapGit Settings|.
    ls_hotkey_action-action = Lif_abapgit_definitions=>c_action-go_settings.
    ls_hotkey_action-hotkey = |x|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Stage|.
    ls_hotkey_action-action = Lif_abapgit_definitions=>c_action-go_stage.
    ls_hotkey_action-hotkey = |s|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Diff|.
    ls_hotkey_action-action = Lif_abapgit_definitions=>c_action-go_repo_diff.
    ls_hotkey_action-hotkey = |d|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Check|.
    ls_hotkey_action-action = Lif_abapgit_definitions=>c_action-repo_code_inspector.
    ls_hotkey_action-hotkey = |c|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Pull|.
    ls_hotkey_action-action = Lif_abapgit_definitions=>c_action-git_pull.
    ls_hotkey_action-hotkey = |p|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Patch|.
    ls_hotkey_action-action = Lif_abapgit_definitions=>c_action-go_patch.
    ls_hotkey_action-hotkey = |a|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    " registered/handled in js
    ls_hotkey_action-description = |Previous Repository|.
    ls_hotkey_action-action = `#`.
    ls_hotkey_action-hotkey = |4|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Next Repository|.
    ls_hotkey_action-action = `##`.
    ls_hotkey_action-hotkey = |6|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Open Repository|.
    ls_hotkey_action-action = `###`.
    ls_hotkey_action-hotkey = |Enter|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Focus Filter|.
    ls_hotkey_action-action = `####`.
    ls_hotkey_action-hotkey = |f|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_menu_provider~get_menu.

    CREATE OBJECT ro_toolbar EXPORTING iv_id = 'toolbar-main'.

    IF Lcl_abapgit_feature=>is_enabled( 'FLOW' ) = abap_true.
      ro_toolbar->add(
        iv_txt = Lcl_abapgit_gui_buttons=>flow( )
        iv_act = Lif_abapgit_definitions=>c_action-flow ).
    ENDIF.

    ro_toolbar->add(
      iv_txt = Lcl_abapgit_gui_buttons=>new_online( )
      iv_act = Lif_abapgit_definitions=>c_action-repo_newonline
    )->add(
      iv_txt = Lcl_abapgit_gui_buttons=>new_offline( )
      iv_act = Lif_abapgit_definitions=>c_action-repo_newoffline
    )->add(
      iv_txt = Lcl_abapgit_gui_buttons=>settings( )
      iv_act = Lif_abapgit_definitions=>c_action-go_settings
    )->add(
      iv_txt = Lcl_abapgit_gui_buttons=>advanced( )
      io_sub = Lcl_abapgit_gui_menus=>advanced( )
    )->add(
      iv_txt = Lcl_abapgit_gui_buttons=>help( )
      io_sub = Lcl_abapgit_gui_menus=>help( ) ).

    Lcl_abapgit_gui_menus=>experimental( ro_toolbar ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    DATA lt_overview TYPE ty_overviews.
    DATA ls_settings TYPE Lif_abapgit_definitions=>ty_s_user_settings.

    ls_settings = Lcl_abapgit_persist_factory=>get_settings( )->read( )->get_user_settings( ).
    mo_label_colors = Lcl_abapgit_repo_labels=>split_colors_into_map( ls_settings-label_colors ).

    lt_overview = prepare_overviews( ).

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    Lcl_abapgit_exit=>get_instance( )->wall_message_list( ri_html ).

    ri_html->add( |<div class="repo-overview">| ).
    render_header_bar( ri_html ).
    render_header_label_list( ri_html ).
    render_repo_list(
      ii_html     = ri_html
      it_overview = lt_overview ).
    ri_html->add( |</div>| ).

    register_deferred_script( render_scripts( ) ).
    register_deferred_script( Lcl_abapgit_gui_chunk_lib=>render_repo_palette( c_action-select ) ).
    register_handlers( ).

  ENDMETHOD.
  METHOD save_settings.
    Lcl_abapgit_persistence_user=>get_instance( )->set_list_settings( ms_list_settings ).
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_REPO_OVER implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_RUNIT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_runit====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_runit====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_RUNIT implementation.
*"* method's implementations
*include methods.
  METHOD build_tadir.

    DATA lt_tadir TYPE Lif_abapgit_definitions=>ty_tadir_tt.
    DATA ls_tadir LIKE LINE OF lt_tadir.
    DATA ls_row   LIKE LINE OF rt_tadir.

    lt_tadir = mo_repo->get_tadir_objects( ).

    LOOP AT lt_tadir INTO ls_tadir.
      CLEAR ls_row.
      ls_row-obj_type = ls_tadir-object.
      ls_row-obj_name = ls_tadir-obj_name.
      APPEND ls_row TO rt_tadir.
    ENDLOOP.

  ENDMETHOD.
  METHOD constructor.

    super->constructor( ).
    mo_repo = io_repo.

    TRY.
        CALL METHOD ('\PROGRAM=SAPLSAUCV_GUI_RUNNER\CLASS=PASSPORT')=>get.
      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |Not supported in your NW release| ).
    ENDTRY.

  ENDMETHOD.
  METHOD run.

    DATA lo_passport TYPE REF TO object.
    DATA lo_runner   TYPE REF TO object.
    DATA lo_timer    TYPE REF TO Lcl_abapgit_timer.
    DATA lt_keys     TYPE ty_keys_tt.
    DATA li_result   TYPE REF TO data.
    FIELD-SYMBOLS <li_result> TYPE any.

    lt_keys = build_tadir( ).

    lo_timer = Lcl_abapgit_timer=>create( iv_count = lines( lt_keys ) )->start( ).

    TRY.
        CALL METHOD ('\PROGRAM=SAPLSAUCV_GUI_RUNNER\CLASS=PASSPORT')=>get
          RECEIVING
            result = lo_passport.

        CALL METHOD ('CL_AUCV_TEST_RUNNER_STANDARD')=>create
          EXPORTING
            i_passport = lo_passport
          RECEIVING
            result     = lo_runner.
      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |Not supported in your NW release| ).
    ENDTRY.

    CREATE DATA li_result TYPE REF TO ('IF_SAUNIT_INTERNAL_RESULT').
    ASSIGN li_result->* TO <li_result>.

    CALL METHOD lo_runner->('RUN_FOR_PROGRAM_KEYS')
      EXPORTING
        i_limit_on_duration_category = '36' " long
        i_limit_on_risk_level        = '33' " critical
        i_program_keys               = lt_keys
      IMPORTING
        e_aunit_result               = <li_result>.

    mv_summary = lo_timer->end( ).

    ro_result = <li_result>.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_actions-rerun.
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
      WHEN OTHERS.
        ASSERT 1 = 1.
    ENDCASE.

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_runit.

    TRY.
        CREATE OBJECT lo_component EXPORTING io_repo = io_repo.

        ri_page = Lcl_abapgit_gui_page_hoc=>create(
          iv_page_title         = |Unit Tests|
          ii_page_menu_provider = lo_component
          ii_child_component    = lo_component ).

      CATCH Lcx_abapgit_exception.

        " Fallback as either SAPLSAUCV_GUI_RUNNER is not available in old releases
        " or passport=>get is private in newer releases NW >= 756
        ri_page = Lcl_abapgit_gui_page_code_insp=>create(
                    io_repo          = io_repo
                    iv_check_variant = 'SWF_ABAP_UNIT' ).

    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_menu_provider~get_menu.

    CREATE OBJECT ro_toolbar.

    ro_toolbar->add(
      iv_txt = 'Re-Run'
      iv_act = c_actions-rerun
      iv_cur = abap_false ).

    ro_toolbar->add(
      iv_txt = 'Back'
      iv_act = Lif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    DATA lo_result         TYPE REF TO object.
    DATA lv_program_ndx    TYPE i.
    DATA lv_class_ndx      TYPE i.
    DATA lv_text           TYPE string.
    DATA lv_count          TYPE i.
    DATA lv_params         TYPE string.
    DATA ls_item           TYPE Lif_abapgit_definitions=>ty_repo_item.

    FIELD-SYMBOLS <ls_task_data>      TYPE any.
    FIELD-SYMBOLS <lt_programs>       TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_alert_by_index> TYPE any.
    FIELD-SYMBOLS <lt_indices>        TYPE ANY TABLE.
    FIELD-SYMBOLS <lt_alerts>         TYPE ANY TABLE.
    FIELD-SYMBOLS <lt_classes>        TYPE ANY TABLE.
    FIELD-SYMBOLS <lt_methods>        TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_alert>          TYPE any.
    FIELD-SYMBOLS <ls_program>        TYPE any.
    FIELD-SYMBOLS <ls_class>          TYPE any.
    FIELD-SYMBOLS <ls_method>         TYPE any.
    FIELD-SYMBOLS <lv_any>            TYPE any.
    FIELD-SYMBOLS <lt_text_info>      TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_text_info>      TYPE any.
    FIELD-SYMBOLS <lt_params>         TYPE string_table.


    register_handlers( ).

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<div class="repo">' ).
    ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_repo_top( io_repo        = mo_repo
                                                              iv_show_commit = abap_false ) ).

    lo_result = run( ).

    ASSIGN lo_result->('F_TASK_DATA') TO <ls_task_data>.
    ASSIGN COMPONENT 'ALERTS_BY_INDICIES' OF STRUCTURE <ls_task_data> TO <lt_indices>.
    ASSIGN COMPONENT 'PROGRAMS' OF STRUCTURE <ls_task_data> TO <lt_programs>.

    IF <lt_programs> IS INITIAL.
      ri_html->add( '<div class="ci-head">' ).
      ri_html->add( 'No unit tests found' ).
      ri_html->add( '</div>' ).
      RETURN.
    ENDIF.

    ri_html->add( |<table class="unit_tests">| ).

    LOOP AT <lt_indices> ASSIGNING <ls_alert_by_index>.
      ASSIGN COMPONENT 'ALERTS' OF STRUCTURE <ls_alert_by_index> TO <lt_alerts>.
      LOOP AT <lt_alerts> ASSIGNING <ls_alert> WHERE ('KIND = ''F'' OR KIND = ''S'' OR KIND = ''E'' OR KIND = ''W''').
        CLEAR lv_text.
        ASSIGN COMPONENT 'HEADER-PARAMS' OF STRUCTURE <ls_alert> TO <lt_params>.
        LOOP AT <lt_params> INTO lv_params.
          lv_text = lv_text && lv_params.
        ENDLOOP.

        ASSIGN COMPONENT 'TEXT_INFOS' OF STRUCTURE <ls_alert> TO <lt_text_info>.
        LOOP AT <lt_text_info> ASSIGNING <ls_text_info>.
          ASSIGN COMPONENT 'PARAMS' OF STRUCTURE <ls_text_info> TO <lt_params>.
          LOOP AT <lt_params> INTO lv_params.
            lv_text = lv_text && lv_params.
          ENDLOOP.
        ENDLOOP.
        IF lv_text NP '*SAUNIT_NO_TEST_CLASS*'.
          ri_html->add( |<tr><td><span class="boxed red-filled-set">{ lv_text }</span></td></tr>| ).
          lv_count = lv_count + 1.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    ri_html->add( '</table>' ).

    ri_html->add( '<div class="ci-head">' ).
    ri_html->add( |Unit tests completed with <strong>{ lv_count } errors</strong> ({ mv_summary })| ).
    ri_html->add( '</div>' ).

    ri_html->add( |<hr><table class="unit_tests">| ).

    LOOP AT <lt_programs> ASSIGNING <ls_program>.
      CLEAR ls_item.
      lv_program_ndx = sy-tabix.

      ASSIGN COMPONENT 'INFO-KEY-OBJ_TYPE' OF STRUCTURE <ls_program> TO <lv_any>.
      IF sy-subrc = 0.
        ls_item-obj_type = <lv_any>.
        ASSIGN COMPONENT 'INFO-KEY-OBJ_NAME' OF STRUCTURE <ls_program> TO <lv_any>.
        ls_item-obj_name = <lv_any>.
        ri_html->add( |<tr><td>{ Lcl_abapgit_gui_chunk_lib=>get_item_icon( ls_item ) } { ls_item-obj_type }|
          && | { Lcl_abapgit_gui_chunk_lib=>get_item_link( ls_item ) }</td><td></td></tr>| ).
      ELSE.
        " KEY field does not exist in 750
        ASSIGN COMPONENT 'INFO-NAME' OF STRUCTURE <ls_program> TO <lv_any>.
        ri_html->add( |<tr><td>{ <lv_any> }</td><td></td></tr>| ).
      ENDIF.

      ASSIGN COMPONENT 'CLASSES' OF STRUCTURE <ls_program> TO <lt_classes>.

      LOOP AT <lt_classes> ASSIGNING <ls_class>.
        lv_class_ndx = sy-tabix.

        ASSIGN COMPONENT 'INFO-NAME' OF STRUCTURE <ls_class> TO <lv_any>.
        ri_html->add( |<tr><td>&nbsp;&nbsp;&nbsp;&nbsp;{ <lv_any> }</td><td></td></tr>| ).
        ASSIGN COMPONENT 'METHODS' OF STRUCTURE <ls_class> TO <lt_methods>.

        LOOP AT <lt_methods> ASSIGNING <ls_method>.

          ri_html->add( get_text_for_method(
            is_method      = <ls_method>
            it_indices     = <lt_indices>
            iv_program_ndx = lv_program_ndx
            iv_class_ndx   = lv_class_ndx
            iv_method_ndx  = sy-tabix ) ).

        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    ri_html->add( '</table></div>' ).

  ENDMETHOD.
  METHOD get_text_for_method.

    DATA lv_params         TYPE string.
    DATA lv_runtime        TYPE timestampl.
    DATA lv_msec           TYPE string.

    FIELD-SYMBOLS <ls_alert_by_index> TYPE any.
    FIELD-SYMBOLS <lt_alerts>         TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_alert>          TYPE any.
    FIELD-SYMBOLS <lt_params>         TYPE string_table.
    FIELD-SYMBOLS <lv_any>            TYPE any.
    FIELD-SYMBOLS <lv_start>          TYPE timestampl.
    FIELD-SYMBOLS <lv_end>            TYPE timestampl.

    READ TABLE it_indices WITH KEY
      ('PROGRAM_NDX') = iv_program_ndx
      ('CLASS_NDX')   = iv_class_ndx
      ('METHOD_NDX')  = iv_method_ndx
      ASSIGNING <ls_alert_by_index>.
    IF sy-subrc = 0.
      ASSIGN COMPONENT 'ALERTS' OF STRUCTURE <ls_alert_by_index> TO <lt_alerts>.
      LOOP AT <lt_alerts> ASSIGNING <ls_alert>.
        ASSIGN COMPONENT 'HEADER-PARAMS' OF STRUCTURE <ls_alert> TO <lt_params>.
        LOOP AT <lt_params> INTO lv_params.
          rv_text = lv_params.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    CLEAR: lv_msec, lv_runtime.
    ASSIGN COMPONENT 'INFO-START_ON' OF STRUCTURE is_method TO <lv_start>.
    IF sy-subrc = 0.
      ASSIGN COMPONENT 'INFO-END_ON' OF STRUCTURE is_method TO <lv_end>.
      IF sy-subrc = 0.
        TRY.
            lv_runtime = cl_abap_tstmp=>subtract(
              tstmp1 = <lv_end>
              tstmp2 = <lv_start> ) * 1000.
            lv_msec = |{ lv_runtime  DECIMALS = 0 } ms|.
          CATCH cx_parameter_invalid ##NO_HANDLER. "ignore
        ENDTRY.
      ENDIF.
    ENDIF.

    IF rv_text IS INITIAL.
      rv_text = |<span class="boxed green-filled-set">PASSED</span>|.
      IF lv_runtime > 100.
        rv_text = rv_text && | <span class="red">{ lv_msec }</span>|.
      ELSE.
        rv_text = rv_text && | { lv_msec }|.
      ENDIF.
    ELSE.
      rv_text = |<span class="boxed red-filled-set">{ rv_text }</span>|.
    ENDIF.

    ASSIGN COMPONENT 'INFO-NAME' OF STRUCTURE is_method TO <lv_any>.
    rv_text = |<tr><td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;{ <lv_any> }</td><td>{ rv_text }</td></tr>|.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_RUNIT implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_SETT_LOCL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_sett_loclccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_sett_loclccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_SETT_LOCL implementation.
*"* method's implementations
*include methods.
  METHOD choose_check_variant.

    DATA ls_variant         TYPE Lif_abapgit_code_inspector=>ty_variant.
    DATA lv_popup_cancelled TYPE abap_bool.

    IF iv_is_return = abap_false.

      mo_popup_picklist = Lcl_abapgit_popup_code_insp=>create(
        )->create_picklist(
        )->set_id( c_event-choose_check_variant
        )->set_in_page( abap_false ).

    ELSE.

      lv_popup_cancelled = mo_popup_picklist->was_cancelled( ).
      IF lv_popup_cancelled = abap_false.
        mo_popup_picklist->get_result_item( CHANGING cs_selected = ls_variant ).
        IF ls_variant IS NOT INITIAL.
          mo_form_data->set(
            iv_key = c_id-code_inspector_check_variant
            iv_val = ls_variant-name ).
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.
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
  METHOD choose_transport_request.

    DATA: lv_transport_request TYPE trkorr.

    lv_transport_request = Lcl_abapgit_ui_factory=>get_popups( )->popup_transport_request( ).

    IF lv_transport_request IS NOT INITIAL.
      mo_form_data->set(
        iv_key = c_id-transport_request
        iv_val = lv_transport_request ).
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    super->constructor( ).
    CREATE OBJECT mo_validation_log.
    CREATE OBJECT mo_form_data.
    mo_repo = io_repo.
    mo_form = get_form_schema( ).
    mo_form_data = read_settings( ).

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_sett_locl.

    CREATE OBJECT lo_component
      EXPORTING
        io_repo = io_repo.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Local Settings & Checks'
      io_page_menu       = Lcl_abapgit_gui_menus=>repo_settings(
                             iv_key = io_repo->get_key( )
                             iv_act = Lif_abapgit_definitions=>c_action-repo_local_settings )
      ii_child_component = lo_component ).

  ENDMETHOD.
  METHOD get_form_schema.

    DATA: li_package TYPE REF TO Lif_abapgit_sap_package.

    li_package = Lcl_abapgit_factory=>get_sap_package( mo_repo->get_package( ) ).

    ro_form = Lcl_abapgit_html_form=>create(
      iv_form_id   = 'repo-local-settings-form'
      iv_help_page = 'https://docs.abapgit.org/settings-local.html' ).

    ro_form->start_group(
      iv_name        = c_id-local
      iv_label       = 'Local Settings'
      iv_hint        = 'Settings valid for this system only'
    )->text(
      iv_name        = c_id-display_name
      iv_label       = 'Display Name'
      iv_hint        = 'Name to show instead of original repo name (optional)' ).

    IF li_package->are_changes_recorded_in_tr_req( ) = abap_true.
      ro_form->text(
        iv_name        = c_id-transport_request
        iv_side_action = c_event-choose_transport_request
        iv_label       = |Transport Request|
        iv_hint        = 'Transport request; All changes are recorded therein and no transport popup appears|' ).
    ENDIF.

    IF is_customizing_included( ) = abap_true.
      ro_form->text(
        iv_name        = c_id-customizing_request
        iv_side_action = c_event-choose_customizing_request
        iv_label       = |Customizing Request|
        iv_hint        = 'Customizing request; All changes are recorded therein and no customizing popup appears|' ).
    ENDIF.

    ro_form->text(
      iv_name        = c_id-labels
      iv_side_action = c_event-choose_labels
      iv_label       = |Labels (comma-separated, allowed chars: "{ Lcl_abapgit_repo_labels=>c_allowed_chars }")|
      iv_hint        = 'Comma-separated labels for grouping and repo organization (optional)'
    )->checkbox(
      iv_name        = c_id-write_protected
      iv_label       = 'Write Protected'
      iv_hint        = 'Lock repository against changes from remote (pull)'
    )->checkbox(
      iv_name        = c_id-ignore_subpackages
      iv_label       = 'Ignore Subpackages'
      iv_hint        = 'Syncronize root package only'
    )->checkbox(
      iv_name        = c_id-only_local_objects
      iv_label       = 'Only Local Objects'
      iv_hint        = 'Ignore objects imported from other systems; serialize only objects created in this system'
    )->checkbox(
      iv_name        = c_id-main_language_only
      iv_label       = 'Only Serialize Main Language'
      iv_hint        = 'Ignore translations; serialize only main language of repository' ).

    IF Lcl_abapgit_feature=>is_enabled( 'FLOW' ) = abap_true
        AND li_package->are_changes_recorded_in_tr_req( ) = abap_true.
      ro_form->checkbox(
        iv_name  = c_id-flow
        iv_label = 'Enable Flow Page' ).
    ENDIF.

    ro_form->start_group(
      iv_name        = c_id-checks
      iv_label       = 'Local Checks'
      iv_hint        = 'Code Inspector check performed to run from menu and before commit'
    )->text(
      iv_name        = c_id-code_inspector_check_variant
      iv_side_action = c_event-choose_check_variant
      iv_label       = 'Code Inspector Check Variant'
      iv_hint        = 'Global check variant for Code Inspector or ABAP Test Cockpit'
    )->checkbox(
      iv_name        = c_id-block_commit
      iv_label       = 'Block Commit If Code Inspection Has Errors'
      iv_hint        = 'Prevent staging if errors of priority 1 or 2 were found during check'
    )->command(
      iv_label       = 'Save Settings'
      iv_cmd_type    = Lif_abapgit_html_form=>c_cmd_type-input_main
      iv_action      = c_event-save
    )->command(
      iv_label       = 'Back'
      iv_action      = Lif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.
  METHOD read_settings.

    DATA: li_package TYPE REF TO Lif_abapgit_sap_package.

    li_package = Lcl_abapgit_factory=>get_sap_package( mo_repo->get_package( ) ).

    " Get settings from DB
    ms_settings = mo_repo->get_local_settings( ).
    CREATE OBJECT ro_form_data.

    " Local Settings
    ro_form_data->set(
      iv_key = c_id-display_name
      iv_val = ms_settings-display_name ).

    IF li_package->are_changes_recorded_in_tr_req( ) = abap_true.
      ro_form_data->set(
        iv_key = c_id-transport_request
        iv_val = ms_settings-transport_request ).
    ENDIF.

    IF is_customizing_included( ) = abap_true.
      ro_form_data->set(
        iv_key = c_id-customizing_request
        iv_val = ms_settings-customizing_request ).
    ENDIF.

    ro_form_data->set(
      iv_key = c_id-labels
      iv_val = ms_settings-labels ).
    ro_form_data->set(
      iv_key = c_id-ignore_subpackages
      iv_val = boolc( ms_settings-ignore_subpackages = abap_true ) ) ##TYPE.
    ro_form_data->set(
      iv_key = c_id-main_language_only
      iv_val = boolc( ms_settings-main_language_only = abap_true ) ) ##TYPE.
    ro_form_data->set(
      iv_key = c_id-flow
      iv_val = boolc( ms_settings-flow = abap_true ) ) ##TYPE.
    ro_form_data->set(
      iv_key = c_id-write_protected
      iv_val = boolc( ms_settings-write_protected = abap_true ) ) ##TYPE.
    ro_form_data->set(
      iv_key = c_id-only_local_objects
      iv_val = boolc( ms_settings-only_local_objects = abap_true ) ) ##TYPE.
    ro_form_data->set(
      iv_key = c_id-code_inspector_check_variant
      iv_val = |{ ms_settings-code_inspector_check_variant }| ).
    ro_form_data->set(
      iv_key = c_id-block_commit
      iv_val = boolc( ms_settings-block_commit = abap_true ) ) ##TYPE.

  ENDMETHOD.
  METHOD save_settings.

    ms_settings-display_name                 = mo_form_data->get( c_id-display_name ).
    ms_settings-transport_request            = mo_form_data->get( c_id-transport_request ).
    ms_settings-customizing_request          = mo_form_data->get( c_id-customizing_request ).
    ms_settings-labels                       = Lcl_abapgit_repo_labels=>normalize( mo_form_data->get( c_id-labels ) ).
    ms_settings-ignore_subpackages           = mo_form_data->get( c_id-ignore_subpackages ).
    ms_settings-main_language_only           = mo_form_data->get( c_id-main_language_only ).
    ms_settings-flow                         = mo_form_data->get( c_id-flow ).
    ms_settings-write_protected              = mo_form_data->get( c_id-write_protected ).
    ms_settings-only_local_objects           = mo_form_data->get( c_id-only_local_objects ).
    ms_settings-code_inspector_check_variant = mo_form_data->get( c_id-code_inspector_check_variant ).
    ms_settings-block_commit                 = mo_form_data->get( c_id-block_commit ).

    mo_repo->set_local_settings( ms_settings ).

    COMMIT WORK AND WAIT.

    MESSAGE 'Settings succesfully saved' TYPE 'S'.

    mo_form_data = read_settings( ).

  ENDMETHOD.
  METHOD validate_form.

    DATA:
      lx_error               TYPE REF TO Lcx_abapgit_exception,
      lv_transport_request   TYPE trkorr,
      lv_customizing_request TYPE trkorr,
      lv_check_variant       TYPE sci_chkv.

    ro_validation_log = Lcl_abapgit_html_form_utils=>create( mo_form )->validate( io_form_data ).

    lv_transport_request = io_form_data->get( c_id-transport_request ).
    IF lv_transport_request IS NOT INITIAL.
      TRY.
          Lcl_abapgit_factory=>get_cts_api( )->validate_transport_request( lv_transport_request ).
        CATCH Lcx_abapgit_exception INTO lx_error.
          ro_validation_log->set(
            iv_key = c_id-transport_request
            iv_val = lx_error->get_text( ) ).
      ENDTRY.
    ENDIF.

    lv_customizing_request = io_form_data->get( c_id-customizing_request ).
    IF lv_customizing_request IS NOT INITIAL.
      TRY.
          Lcl_abapgit_factory=>get_cts_api( )->validate_transport_request( lv_customizing_request ).
        CATCH Lcx_abapgit_exception INTO lx_error.
          ro_validation_log->set(
            iv_key = c_id-customizing_request
            iv_val = lx_error->get_text( ) ).
      ENDTRY.
    ENDIF.

    lv_check_variant = to_upper( io_form_data->get( c_id-code_inspector_check_variant ) ).
    IF lv_check_variant IS NOT INITIAL.
      TRY.
          Lcl_abapgit_factory=>get_code_inspector( mo_repo->get_package( )
            )->validate_check_variant( lv_check_variant ).
        CATCH Lcx_abapgit_exception INTO lx_error.
          ro_validation_log->set(
            iv_key = c_id-code_inspector_check_variant
            iv_val = lx_error->get_text( ) ).
      ENDTRY.
    ENDIF.

    IF io_form_data->get( c_id-block_commit ) = abap_true AND lv_check_variant IS INITIAL.
      ro_validation_log->set(
        iv_key = c_id-block_commit
        iv_val = |If block commit is active, a check variant has to be maintained| ).
    ENDIF.

    TRY.
        Lcl_abapgit_repo_labels=>validate( io_form_data->get( c_id-labels ) ).
      CATCH Lcx_abapgit_exception INTO lx_error.
        ro_validation_log->set(
          iv_key = c_id-labels
          iv_val = lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    mo_form_data->merge( Lcl_abapgit_html_form_utils=>create( mo_form )->normalize( ii_event->form_data( ) ) ).

    CASE ii_event->mv_action.
      WHEN Lif_abapgit_definitions=>c_action-go_back.
        rs_handled-state = Lcl_abapgit_html_form_utils=>create( mo_form )->exit(
          io_form_data    = mo_form_data
          io_compare_with = read_settings( ) ).

      WHEN c_event-choose_transport_request.

        choose_transport_request( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_event-choose_customizing_request.

        choose_customizing_request( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_event-choose_labels.

        choose_labels( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_event-choose_check_variant.

        choose_check_variant( ).

      WHEN c_event-save.
        " Validate form entries before saving
        mo_validation_log = validate_form( mo_form_data ).

        IF mo_validation_log->is_empty( ) = abap_true.
          save_settings( ).
        ENDIF.

        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

    ENDCASE.

    IF mo_popup_picklist IS BOUND. " Uniform popup state handling
      " This should happen only for a new popup because
      " on the first re-render main component event handling is blocked
      " and not called again until the popup distruction
      IF mo_popup_picklist->is_in_page( ) = abap_true.
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
      ELSE.
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.
        rs_handled-page  = Lcl_abapgit_gui_page_hoc=>create(
          ii_child_component = mo_popup_picklist
          iv_show_as_modal   = abap_true ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    handle_picklist_state( ).

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( `<div class="repo">` ).

    ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_repo_top(
      io_repo               = mo_repo
      iv_show_commit        = abap_false
      iv_interactive_branch = abap_true ) ).

    ri_html->add( mo_form->render(
      io_values         = mo_form_data
      io_validation_log = mo_validation_log ) ).

    ri_html->add( `</div>` ).

    IF mo_popup_picklist IS NOT BOUND OR mo_popup_picklist->is_in_page( ) = abap_false.
      register_handlers( ).
    ELSEIF mo_popup_picklist->is_in_page( ) = abap_true.
      " Block usual page events if the popup is an in-page popup
      ri_html->add( Lcl_abapgit_gui_in_page_modal=>create( mo_popup_picklist ) ).
    ENDIF.

  ENDMETHOD.
  METHOD choose_customizing_request.

    DATA:
      ls_transport_type      TYPE Lif_abapgit_definitions=>ty_transport_type,
      lv_customizing_request TYPE trkorr.

    ls_transport_type-request = Lif_abapgit_cts_api=>c_transport_type-cust_request.
    ls_transport_type-task    = Lif_abapgit_cts_api=>c_transport_type-cust_task.

    lv_customizing_request = Lcl_abapgit_ui_factory=>get_popups( )->popup_transport_request( ls_transport_type ).

    IF lv_customizing_request IS NOT INITIAL.
      mo_form_data->set(
        iv_key = c_id-customizing_request
        iv_val = lv_customizing_request ).
    ENDIF.

  ENDMETHOD.
  METHOD is_customizing_included.

    DATA lt_files TYPE Lif_abapgit_definitions=>ty_files_item_tt.

    lt_files = mo_repo->get_files_local( ).

    READ TABLE lt_files TRANSPORTING NO FIELDS
      WITH KEY item-obj_type = Lif_abapgit_data_config=>c_data_type-tabu. "todo
    IF sy-subrc = 0.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD handle_picklist_state.

    IF mo_popup_picklist IS BOUND AND
      ( mo_popup_picklist->is_fulfilled( ) = abap_true OR mo_popup_picklist->is_in_page( ) = abap_false ).
      " Picklist is either fullfilled OR
      " it was on its own page and user went back from it via F3/ESC and the picklist had no "graceful back" handler
      CASE mo_popup_picklist->id( ).
        WHEN c_event-choose_check_variant.
          choose_check_variant( abap_true ).
        WHEN OTHERS.
          Lcx_abapgit_exception=>raise( |Unexpected picklist id { mo_popup_picklist->id( ) }| ).
      ENDCASE.

      CLEAR mo_popup_picklist.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_SETT_LOCL implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_SETT_PERS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_sett_persccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_sett_persccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_SETT_PERS implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( ).
    CREATE OBJECT mo_validation_log.
    CREATE OBJECT mo_form_data.
    mo_form = get_form_schema( ).
    mo_form_data = read_settings( ).

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_sett_pers.

    CREATE OBJECT lo_component.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Personal Settings'
      io_page_menu       = Lcl_abapgit_gui_menus=>settings( Lif_abapgit_definitions=>c_action-go_settings_personal )
      ii_child_component = lo_component ).

  ENDMETHOD.
  METHOD get_form_schema.

    ro_form = Lcl_abapgit_html_form=>create(
      iv_form_id   = 'personal-setting-form'
      iv_help_page = 'https://docs.abapgit.org/guide-settings-personal.html' ).

    ro_form->start_group(
      iv_name          = c_id-startup
      iv_label         = 'Startup'
    )->checkbox(
      iv_name          = c_id-show_default_repo
      iv_label         = 'Show Last Opened Repository'
      iv_hint          = 'Recommended to check, if you are using ADT'
    )->start_group(
      iv_name          = c_id-ui
      iv_label         = 'User Interface'
    )->radio(
      iv_name          = c_id-ui_theme
      iv_default_value = Lcl_abapgit_settings=>c_ui_theme-default
      iv_label         = 'Theme'
    )->option(
      iv_label         = 'Default'
      iv_value         = Lcl_abapgit_settings=>c_ui_theme-default
    )->option(
      iv_label         = 'Dark'
      iv_value         = Lcl_abapgit_settings=>c_ui_theme-dark
    )->option(
      iv_label         = 'Belize'
      iv_value         = Lcl_abapgit_settings=>c_ui_theme-belize
    )->option(
      iv_label         = 'Synced with SAP GUI'
      iv_value         = Lcl_abapgit_settings=>c_ui_theme-synced_with_gui
    )->radio(
      iv_name          = c_id-icon_scaling
      iv_default_value = ''
      iv_label         = 'Icon Scaling (HDPI)'
      iv_hint          = 'Adjust size of icons for High DPI displays'
    )->option(
      iv_label         = 'Automatic'
      iv_value         = ''
    )->option(
      iv_label         = 'Small'
      iv_value         = Lcl_abapgit_settings=>c_icon_scaling-small
    )->option(
      iv_label         = 'Large'
      iv_value         = Lcl_abapgit_settings=>c_icon_scaling-large
    )->number(
      iv_name          = c_id-max_lines
      iv_label         = 'List Size'
      iv_hint          = 'Maximum number of objects listed (0 = All)'
      iv_min           = 0
      iv_max           = 10000
    )->textarea(
      iv_name          = c_id-label_colors
      iv_rows          = 3
      iv_label         = `Repo label colors ` && render_repo_labels_help_hint( )
    )->start_group(
      iv_name          = c_id-interaction
      iv_label         = 'Interaction'
    )->checkbox(
      iv_name          = c_id-activate_wo_popup
      iv_label         = 'Activate Objects Without Popup'
      iv_hint          = 'Activates objects automatically without showing popup'
    )->checkbox(
      iv_name          = c_id-adt_jump_enabled
      iv_label         = 'Enable Jump to ABAP Development Tools (If Available)'
      iv_hint          = 'Recommended to check, if you are using ADT'
    )->checkbox(
      iv_name          = c_id-link_hints_enabled
      iv_label         = 'Enable Vimium-like Link Hints'
      iv_hint          = 'When you hit the key, abapGit will identify clickable things and put a label beside it'
    )->text(
      iv_name          = c_id-link_hint_key
      iv_label         = 'Key to Activate Link Hints'
      iv_min           = 0
      iv_max           = 1
    )->start_group(
      iv_name          = c_id-resources
      iv_label         = 'System Resources'
    )->checkbox(
      iv_name          = c_id-parallel_proc_disabled
      iv_label         = 'Disable Parallel Processing'
      iv_hint          = 'If disabled, abapGit will use only a single thread to serialize objects'
    )->command(
      iv_label         = 'Save Settings'
      iv_cmd_type      = Lif_abapgit_html_form=>c_cmd_type-input_main
      iv_action        = c_event-save
    )->command(
      iv_label         = 'Back'
      iv_action        = Lif_abapgit_definitions=>c_action-go_back ).

    " Not available via this form:
    " - User-specific hotkey settings have been discontinued
    " - hide_sapgui_hint is set via ZCL_ABAPGIT_SERVICES_ABAPGIT-CHECK_SAPGUI

  ENDMETHOD.
  METHOD read_settings.

    " Get settings from DB
    mo_settings = Lcl_abapgit_persist_factory=>get_settings( )->read( ).
    ms_settings = mo_settings->get_user_settings( ).
    CREATE OBJECT ro_form_data.

    " Startup
    ro_form_data->set(
      iv_key = c_id-show_default_repo
      iv_val = |{ ms_settings-show_default_repo }| ).

    " UI
    ro_form_data->set(
      iv_key = c_id-ui_theme
      iv_val = ms_settings-ui_theme ).
    ro_form_data->set(
      iv_key = c_id-icon_scaling
      iv_val = |{ ms_settings-icon_scaling }| ).
    ro_form_data->set(
      iv_key = c_id-max_lines
      iv_val = |{ ms_settings-max_lines }| ).
    ro_form_data->set(
      iv_key = c_id-label_colors
      iv_val = ms_settings-label_colors ).

    " Interaction
    ro_form_data->set(
      iv_key = c_id-activate_wo_popup
      iv_val = boolc( ms_settings-activate_wo_popup = abap_true ) ) ##TYPE.
    ro_form_data->set(
      iv_key = c_id-adt_jump_enabled
      iv_val = boolc( ms_settings-adt_jump_enabled = abap_true ) ) ##TYPE.
    ro_form_data->set(
      iv_key = c_id-link_hints_enabled
      iv_val = boolc( ms_settings-link_hints_enabled = abap_true ) ) ##TYPE.
    ro_form_data->set(
      iv_key = c_id-link_hint_key
      iv_val = |{ ms_settings-link_hint_key }| ).

    " Resources
    ro_form_data->set(
      iv_key = c_id-parallel_proc_disabled
      iv_val = boolc( ms_settings-parallel_proc_disabled = abap_true ) ) ##TYPE.

  ENDMETHOD.
  METHOD render_repo_labels_help_hint.

    DATA lt_fragments TYPE string_table.
    DATA lt_labels TYPE string_table.
    DATA lv_l TYPE string.
    DATA lo_colors TYPE REF TO Lcl_abapgit_string_map.

    APPEND `<p style="margin-bottom: 0.3em">` TO lt_fragments.
    APPEND `Comma-separated list of <code>label:color</code> pairs.` TO lt_fragments.
    APPEND ` <code>color</code> part can be either a pre-defined style (see below), or` TO lt_fragments.
    APPEND ` <code>#fg/bg/border</code> styles, where <code>fg</code>, ` TO lt_fragments.
    APPEND ` <code>bg</code>, and <code>border</code> are RGB color codes (3 or 6 long).` TO lt_fragments.
    APPEND ` You can also specify just <code>fg</code>, <code>bg</code>, or` TO lt_fragments.
    APPEND ` <code>border</code> (defaults will be used for missing parts).` TO lt_fragments.
    APPEND ` E.g. <code>utils:brown, work:#ff0000/880000, client X:#ddd, client Y:#/333</code>` TO lt_fragments.
    APPEND `<br>Available styles:` TO lt_fragments.
    APPEND `</p>` TO lt_fragments.

    APPEND `white` TO lt_labels.
    APPEND `white-b` TO lt_labels.
    APPEND `white-r` TO lt_labels.
    APPEND `grey` TO lt_labels.
    APPEND `dark-w` TO lt_labels.
    APPEND `dark-y` TO lt_labels.
    APPEND `dark-r` TO lt_labels.
    APPEND `dark-b` TO lt_labels.
    APPEND `lightblue` TO lt_labels.
    APPEND `darkblue` TO lt_labels.
    APPEND `lightgreen` TO lt_labels.
    APPEND `darkgreen` TO lt_labels.
    APPEND `lightred` TO lt_labels.
    APPEND `darkred` TO lt_labels.
    APPEND `yellow` TO lt_labels.
    APPEND `darkyellow` TO lt_labels.
    APPEND `orange` TO lt_labels.
    APPEND `brown` TO lt_labels.
    APPEND `pink` TO lt_labels.
    APPEND `teal` TO lt_labels.
    APPEND `darkviolet` TO lt_labels.

    lo_colors = Lcl_abapgit_string_map=>create( ).
    LOOP AT lt_labels INTO lv_l.
      TRY.
          lo_colors->set(
            iv_key = lv_l
            iv_val = lv_l ).
        CATCH Lcx_abapgit_exception.
      ENDTRY.
    ENDLOOP.

    APPEND Lcl_abapgit_gui_chunk_lib=>render_label_list(
      it_labels       = lt_labels
      io_label_colors = lo_colors ) TO lt_fragments.

    APPEND
      `<p style="margin-top: 0.3em">see also <code>rl-*</code> styles in common.css</p>`
      TO lt_fragments.

    rv_html = Lcl_abapgit_gui_chunk_lib=>render_help_hint( concat_lines_of( table = lt_fragments ) ).

  ENDMETHOD.
  METHOD save_settings.

    DATA li_persistence TYPE REF TO Lif_abapgit_persist_settings.

    " Startup
    ms_settings-show_default_repo = mo_form_data->get( c_id-show_default_repo ).

    " UI
    ms_settings-ui_theme = mo_form_data->get( c_id-ui_theme ).
    ms_settings-icon_scaling = mo_form_data->get( c_id-icon_scaling ).
    ms_settings-max_lines = mo_form_data->get( c_id-max_lines ).
    ms_settings-label_colors = Lcl_abapgit_repo_labels=>normalize_colors( mo_form_data->get( c_id-label_colors ) ).

    " Interaction
    ms_settings-activate_wo_popup = mo_form_data->get( c_id-activate_wo_popup ).
    ms_settings-adt_jump_enabled = mo_form_data->get( c_id-adt_jump_enabled ).
    ms_settings-link_hints_enabled = mo_form_data->get( c_id-link_hints_enabled ).
    ms_settings-link_hint_key = mo_form_data->get( c_id-link_hint_key ).

    " Resources
    ms_settings-parallel_proc_disabled = mo_form_data->get( c_id-parallel_proc_disabled ).

    " Store in DB
    mo_settings->set_user_settings( ms_settings ).

    li_persistence = Lcl_abapgit_persist_factory=>get_settings( ).
    li_persistence->modify( mo_settings ).

    COMMIT WORK AND WAIT.

    MESSAGE 'Settings succesfully saved' TYPE 'S'.

    mo_form_data = read_settings( ).

  ENDMETHOD.
  METHOD validate_form.

    DATA lx_error TYPE REF TO Lcx_abapgit_exception.

    ro_validation_log = Lcl_abapgit_html_form_utils=>create( mo_form )->validate( io_form_data ).

    TRY.
        Lcl_abapgit_repo_labels=>validate_colors( io_form_data->get( c_id-label_colors ) ).
      CATCH Lcx_abapgit_exception INTO lx_error.
        ro_validation_log->set(
          iv_key = c_id-label_colors
          iv_val = lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    mo_form_data->merge( Lcl_abapgit_html_form_utils=>create( mo_form )->normalize( ii_event->form_data( ) ) ).

    CASE ii_event->mv_action.
      WHEN Lif_abapgit_definitions=>c_action-go_back.
        rs_handled-state = Lcl_abapgit_html_form_utils=>create( mo_form )->exit(
          io_form_data    = mo_form_data
          io_compare_with = read_settings( ) ).

      WHEN c_event-save.
        " Validate form entries before saving
        mo_validation_log = validate_form( mo_form_data ).

        IF mo_validation_log->is_empty( ) = abap_true.
          save_settings( ).
        ENDIF.

        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

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
endclass. "ZCL_ABAPGIT_GUI_PAGE_SETT_PERS implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_SETT_REMO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_sett_remoccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_sett_remoccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_SETT_REMO implementation.
*"* method's implementations
*include methods.
  METHOD check_protection.

    IF mo_repo->is_offline( ) = abap_true.
      Lcx_abapgit_exception=>raise( 'Unexpected switch for offline repo' ).
    ENDIF.
    IF mo_repo->get_local_settings( )-write_protected = abap_true.
      Lcx_abapgit_exception=>raise( 'Cannot switch. Repository is write-protected in local settings' ).
    ENDIF.

  ENDMETHOD.
  METHOD choose_branch.

    DATA lv_url         TYPE Lif_abapgit_persistence=>ty_repo-url.
    DATA lv_branch_name TYPE Lif_abapgit_persistence=>ty_repo-branch_name.
    DATA ls_branch      TYPE Lif_abapgit_git_definitions=>ty_git_branch.
    DATA lv_popup_cancelled TYPE abap_bool.

    IF iv_is_return = abap_false.

      IF mo_form_data->get( c_id-offline ) = abap_true.
        RETURN.
      ENDIF.

      lv_url         = mo_form_data->get( c_id-url ).
      lv_branch_name = mo_form_data->get( c_id-branch ).

      mo_popup_picklist = Lcl_abapgit_popup_branch_list=>create(
        iv_show_new_option = abap_false
        iv_url             = lv_url
        iv_default_branch  = lv_branch_name
        )->create_picklist(
        )->set_id( c_event-choose_branch
        )->set_in_page( ).

    ELSE.

      lv_popup_cancelled = mo_popup_picklist->was_cancelled( ).
      IF lv_popup_cancelled = abap_false.
        mo_popup_picklist->get_result_item( CHANGING cs_selected = ls_branch ).
        IF ls_branch IS NOT INITIAL.
          mo_form_data->set(
            iv_key = c_id-branch
            iv_val = ls_branch-display_name ).
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD choose_commit.

    DATA:
      lv_url         TYPE string,
      lv_branch_name TYPE Lif_abapgit_persistence=>ty_repo-branch_name,
      li_popups      TYPE REF TO Lif_abapgit_popups.

    IF mo_form_data->get( c_id-offline ) = abap_true.
      RETURN.
    ENDIF.

    lv_url = mo_form_data->get( c_id-url ).
    lv_branch_name = Lif_abapgit_git_definitions=>c_git_branch-heads_prefix && mo_form_data->get( c_id-branch ).

    li_popups = Lcl_abapgit_ui_factory=>get_popups( ).

    rv_commit = li_popups->commit_list_popup(
      iv_repo_url    = lv_url
      iv_branch_name = lv_branch_name )-sha1.

  ENDMETHOD.
  METHOD choose_tag.

    DATA ls_tag TYPE Lif_abapgit_git_definitions=>ty_git_branch.
    DATA lv_url TYPE ty_remote_settings-url.
    DATA lv_popup_cancelled TYPE abap_bool.

    IF iv_is_return = abap_false.

      IF mo_form_data->get( c_id-offline ) = abap_true.
        RETURN.
      ELSEIF mo_repo->is_offline( ) = abap_true.
        MESSAGE 'Please save conversion to online repository before choosing a tag' TYPE 'S'.
        RETURN.
      ENDIF.

      lv_url = mo_form_data->get( c_id-url ).
      mo_popup_picklist = Lcl_abapgit_popup_tag_list=>create( lv_url
        )->create_picklist(
        )->set_id( c_event-choose_tag
        )->set_in_page( ).

    ELSE.

      lv_popup_cancelled = mo_popup_picklist->was_cancelled( ).
      IF lv_popup_cancelled = abap_false.
        mo_popup_picklist->get_result_item( CHANGING cs_selected = ls_tag ).
        IF ls_tag IS NOT INITIAL.
          mo_form_data->set(
            iv_key = c_id-tag
            iv_val = ls_tag-display_name ).
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD choose_url.

    " todo, get url history from DB and show selection popup #3639
    rv_url = ''.

  ENDMETHOD.
  METHOD constructor.

    super->constructor( ).
    mo_repo              = io_repo.
    ms_settings_snapshot = get_remote_settings_from_repo( mo_repo ).
    mo_form              = get_form_schema( ).
    mo_form_data         = initialize_form_data( ).
    CREATE OBJECT mo_validation_log.

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_sett_remo.

    CREATE OBJECT lo_component
      EXPORTING
        io_repo = io_repo.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Remote Settings'
      io_page_menu       = Lcl_abapgit_gui_menus=>repo_settings(
                             iv_key = io_repo->get_key( )
                             iv_act = Lif_abapgit_definitions=>c_action-repo_remote_settings )
      ii_child_component = lo_component ).

  ENDMETHOD.
  METHOD get_form_schema.

    DATA:
      lv_button    TYPE string,
      lv_icon      TYPE string,
      lv_offline   TYPE abap_bool,
      lv_head_type TYPE ty_head_type.

    IF io_existing_form_data IS BOUND AND io_existing_form_data->is_empty( ) = abap_false.
      lv_offline = io_existing_form_data->get( c_id-offline ).
      IF lv_offline = abap_false.
        lv_head_type = io_existing_form_data->get( c_id-head_type ).
      ENDIF.
    ELSE.
      lv_offline   = ms_settings_snapshot-offline.
      lv_head_type = ms_settings_snapshot-head_type.
    ENDIF.

    ro_form = Lcl_abapgit_html_form=>create(
      iv_form_id   = 'repo-remote-settings-form'
      iv_help_page = 'https://docs.abapgit.org/settings-remote.html' ).

    IF lv_offline = abap_true.
      lv_button = 'Switch to Online'.
      lv_icon   = 'plug/darkgrey'.
    ELSE.
      lv_button = 'Switch to Offline'.
      lv_icon   = 'cloud-upload-alt/darkgrey'.
    ENDIF.

    ro_form->start_group(
      iv_name  = c_id-general
      iv_label = 'General'
      iv_hint  = 'Change the general type and origin of the repository'
    )->text(
      iv_name        = c_id-repo_type
      iv_label       = |Type of Repository: { Lcl_abapgit_html=>icon( lv_icon ) }|
      iv_readonly    = abap_true
    )->hidden( c_id-offline ).

    IF lv_offline = abap_false.

      ro_form->text(
        iv_name        = c_id-url
        iv_condense    = abap_true
        iv_label       = 'Git Repository URL'
        iv_hint        = 'URL of original repository'
        iv_placeholder = 'https://github.com/...git' ).

      ro_form->start_group(
        iv_name  = c_id-head_group
        iv_label = 'Head'
      )->radio(
        iv_label  = 'Type'
        iv_name   = c_id-head_type
        iv_action = c_event-change_head_type
      )->option(
        iv_label = 'Branch'
        iv_value = c_head_types-branch
      )->option(
        iv_label = 'Tag'
        iv_value = c_head_types-tag
      )->option(
        iv_label = 'Commit'
        iv_value = c_head_types-commit
      )->option(
        iv_label = 'Pull Request'
        iv_value = c_head_types-pull_request ).

      IF lv_head_type = c_head_types-branch OR
         lv_head_type = c_head_types-commit.
        ro_form->text(
          iv_name        = c_id-branch
          iv_label       = 'Branch'
          iv_required    = abap_true
          iv_side_action = c_event-choose_branch ).
      ENDIF.

      IF lv_head_type = c_head_types-tag.
        ro_form->text(
          iv_name        = c_id-tag
          iv_label       = 'Tag'
          iv_required    = abap_true
          iv_side_action = c_event-choose_tag ).
      ENDIF.

      IF lv_head_type = c_head_types-commit.
        ro_form->text(
          iv_name        = c_id-commit
          iv_label       = 'Commit'
          iv_required    = abap_true
          iv_min         = 40
          iv_max         = 40
          iv_side_action = c_event-choose_commit ).
      ENDIF.

      IF lv_head_type = c_head_types-pull_request.
        ro_form->text(
          iv_name        = c_id-pull_request
          iv_label       = 'Pull Request'
          iv_required    = abap_true
          iv_side_action = c_event-choose_pull_request ).
      ENDIF.

    ENDIF.

    ro_form->command(
      iv_label    = 'Save Settings'
      iv_cmd_type = Lif_abapgit_html_form=>c_cmd_type-input_main
      iv_action   = c_event-save
    )->command(
      iv_label  = lv_button
      iv_action = c_event-switch
    )->command(
      iv_label  = 'Back'
      iv_action = Lif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.
  METHOD get_remote_settings_from_form.

    rs_settings-offline = io_form_data->get( c_id-offline ).

    IF rs_settings-offline = abap_false.
      rs_settings-url       = io_form_data->get( c_id-url ).
      rs_settings-head_type = io_form_data->get( c_id-head_type ).

      CASE rs_settings-head_type.
        WHEN c_head_types-branch.
          rs_settings-branch = Lif_abapgit_git_definitions=>c_git_branch-heads_prefix &&
            io_form_data->get( c_id-branch ).
        WHEN c_head_types-tag.
          rs_settings-tag = Lif_abapgit_git_definitions=>c_git_branch-tags_prefix &&
            io_form_data->get( c_id-tag ).
        WHEN c_head_types-commit.
          rs_settings-branch = Lif_abapgit_git_definitions=>c_git_branch-heads_prefix &&
            io_form_data->get( c_id-branch ).
          rs_settings-commit = io_form_data->get( c_id-commit ).
        WHEN c_head_types-pull_request.
          rs_settings-pull_request = io_form_data->get( c_id-pull_request ).
      ENDCASE.
    ENDIF.

  ENDMETHOD.
  METHOD get_remote_settings_from_repo.

    DATA: lo_repo_online TYPE REF TO Lcl_abapgit_repo_online,
          lv_branch      TYPE ty_remote_settings-branch.

    IF io_repo->is_offline( ) = abap_false.
      lo_repo_online ?= io_repo.

      rs_settings-url = lo_repo_online->get_url( ).
      rs_settings-offline = abap_false.
      rs_settings-switched_origin = lo_repo_online->get_switched_origin( ).

      IF lo_repo_online->get_selected_commit( ) IS NOT INITIAL.
        rs_settings-commit = lo_repo_online->get_selected_commit( ).
        rs_settings-branch = lo_repo_online->get_selected_branch( ).
        rs_settings-head_type = c_head_types-commit.
      ELSEIF lo_repo_online->get_switched_origin( ) IS NOT INITIAL.
        " get_switched_origin( ) returns the original repo url + HEAD concatenated with @
        " get_branch( ) returns the branch of the PR in the source repo
        " get_url( ) returns the source repo of the PR branch

        rs_settings-switched_origin = lo_repo_online->get_switched_origin( ).
        SPLIT rs_settings-switched_origin AT '@' INTO rs_settings-url rs_settings-branch.
        IF rs_settings-branch CP Lif_abapgit_git_definitions=>c_git_branch-tags.
          rs_settings-tag = rs_settings-branch.
          CLEAR rs_settings-branch.
        ENDIF.

        lv_branch = lo_repo_online->get_selected_branch( ).
        REPLACE FIRST OCCURRENCE OF Lif_abapgit_git_definitions=>c_git_branch-heads_prefix IN lv_branch WITH space.
        CONDENSE lv_branch.
        rs_settings-pull_request = |{ lo_repo_online->get_url( ) }@{ lv_branch }|.
        rs_settings-head_type = c_head_types-pull_request.
      ELSE.
        rs_settings-branch = lo_repo_online->get_selected_branch( ).
        rs_settings-head_type = c_head_types-branch.

        IF rs_settings-branch CP Lif_abapgit_git_definitions=>c_git_branch-tags.
          rs_settings-head_type = c_head_types-tag.
          rs_settings-tag = rs_settings-branch.
          CLEAR rs_settings-branch.
        ENDIF.
      ENDIF.

    ELSE.
      rs_settings-offline = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD initialize_form_data.

    DATA:
      lv_type TYPE string,
      lv_head TYPE string.

    CREATE OBJECT ro_form_data.

    IF ms_settings_snapshot-offline = abap_true.
      lv_type = c_repo_type-offline.
    ELSE.
      lv_type = c_repo_type-online.
    ENDIF.

    ro_form_data->set(
      iv_key = c_id-offline
      iv_val = ms_settings_snapshot-offline ).
    ro_form_data->set(
      iv_key = c_id-repo_type
      iv_val = lv_type ).

    IF ms_settings_snapshot-offline = abap_false.
      ro_form_data->set(
        iv_key = c_id-url
        iv_val = ms_settings_snapshot-url ).

      ro_form_data->set(
        iv_key = c_id-head_type
        iv_val = ms_settings_snapshot-head_type ).

      " When pull request is selected the previously selected branch/tag is also loaded to be able to switch back to it
      lv_head = Lcl_abapgit_git_branch_list=>get_display_name( ms_settings_snapshot-branch ).
      ro_form_data->set(
        iv_key = c_id-branch
        iv_val = lv_head ).

      lv_head = Lcl_abapgit_git_branch_list=>get_display_name( ms_settings_snapshot-tag ).
      ro_form_data->set(
        iv_key = c_id-tag
        iv_val = lv_head ).

      ro_form_data->set(
        iv_key = c_id-commit
        iv_val = ms_settings_snapshot-commit ).

      ro_form_data->set(
        iv_key = c_id-pull_request
        iv_val = ms_settings_snapshot-pull_request ).
    ENDIF.

  ENDMETHOD.
  METHOD save_settings.

    DATA:
      lo_repo_online  TYPE REF TO Lcl_abapgit_repo_online,
      ls_settings_new TYPE ty_remote_settings.

    ls_settings_new = get_remote_settings_from_form( mo_form_data ).

    " Switch online / offline
    IF ls_settings_new-offline <> ms_settings_snapshot-offline.
      " Remember key, switch, retrieve new instance (todo, refactor #2244)
      mo_repo->switch_repo_type( ls_settings_new-offline ).
      mo_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->get( mo_repo->get_key( ) ).
    ENDIF.

    IF mo_repo->is_offline( ) = abap_false.
      " Online: Save url
      lo_repo_online ?= mo_repo.
      lo_repo_online->set_url( ls_settings_new-url ).
    ENDIF.

    CASE ls_settings_new-head_type.
      WHEN c_head_types-branch.
        switch_to_pull_req( iv_revert = abap_true ).
        switch_to_commit( iv_revert = abap_true ).
        switch_to_branch_tag( ls_settings_new-branch ).
      WHEN c_head_types-tag.
        switch_to_pull_req( iv_revert = abap_true ).
        switch_to_commit( iv_revert = abap_true ).
        switch_to_branch_tag( ls_settings_new-tag ).
      WHEN c_head_types-commit.
        switch_to_pull_req( iv_revert = abap_true ).
        switch_to_commit( iv_commit = ls_settings_new-commit ).
      WHEN c_head_types-pull_request.
        switch_to_commit( iv_revert = abap_true ).
        switch_to_pull_req( iv_pull = ls_settings_new-pull_request ).
    ENDCASE.

    IF mo_repo->is_offline( ) = abap_false AND ls_settings_new-head_type <> c_head_types-pull_request.
      " Switching from PR to something else will reset the URL in repo->switch_origin( space )
      " -> set URL again
      lo_repo_online->set_url( ls_settings_new-url ).
    ENDIF.

    COMMIT WORK AND WAIT.

    MESSAGE 'Settings succesfully saved' TYPE 'S'.

    mv_refresh_on_back = abap_true.
    ms_settings_snapshot = get_remote_settings_from_repo( mo_repo ).

  ENDMETHOD.
  METHOD switch_online_offline.

    DATA: lv_offline_new TYPE abap_bool,
          lv_url         TYPE ty_remote_settings-url,
          lv_branch      TYPE ty_remote_settings-branch.

    lv_offline_new = boolc( mo_form_data->get( c_id-offline ) = abap_false ).
    mo_form_data->set(
      iv_key = c_id-offline
      iv_val = lv_offline_new ).

    IF lv_offline_new = abap_true.
      lv_url = mo_form_data->get( c_id-url ).
      mv_offline_switch_saved_url = lv_url.
      mo_form_data->set(
        iv_key = c_id-url
        iv_val = '' ).
      mo_form_data->set(
        iv_key = c_id-repo_type
        iv_val = c_repo_type-offline ).
    ELSE.
      mo_form_data->set(
        iv_key = c_id-repo_type
        iv_val = c_repo_type-online ).
      IF mv_offline_switch_saved_url IS NOT INITIAL.
        mo_form_data->set(
          iv_key = c_id-url
          iv_val = mv_offline_switch_saved_url ).
      ENDIF.

      lv_url = mo_form_data->get( c_id-url ).
      IF mo_form_data->get( c_id-head_type ) IS INITIAL.
        TRY.
            mo_form_data->set(
              iv_key = c_id-head_type
              iv_val = c_head_types-branch ).

            IF lv_url CP 'http*'.
              lv_branch = Lcl_abapgit_git_transport=>branches( lv_url )->get_head_symref( ).
              mo_form_data->set(
                iv_key = c_id-branch
                iv_val = lv_branch ).
            ENDIF.
          CATCH Lcx_abapgit_exception ##NO_HANDLER.
        ENDTRY.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD switch_to_branch_tag.

    DATA lo_repo TYPE REF TO Lcl_abapgit_repo_online.

    check_protection( ).
    lo_repo ?= mo_repo.
    lo_repo->select_branch( iv_name ).

  ENDMETHOD.
  METHOD switch_to_commit.

    DATA lo_repo TYPE REF TO Lcl_abapgit_repo_online.

    check_protection( ).

    lo_repo ?= mo_repo.

    IF iv_revert = abap_true.
      lo_repo->select_commit( '' ).
    ELSE.
      lo_repo->select_commit( iv_commit ).
    ENDIF.

  ENDMETHOD.
  METHOD switch_to_pull_req.

    DATA:
      lo_repo   TYPE REF TO Lcl_abapgit_repo_online,
      lv_url    TYPE ty_remote_settings-url,
      lv_branch TYPE ty_remote_settings-branch.

    check_protection( ).

    lo_repo ?= mo_repo.

    " Switching twice does not work so reset to original repo first
    lo_repo->switch_origin( '' ).

    IF iv_revert = abap_false.
      SPLIT iv_pull AT '@' INTO lv_url lv_branch.
      lo_repo->switch_origin(
        iv_url    = lv_url
        iv_branch = Lif_abapgit_git_definitions=>c_git_branch-heads_prefix && lv_branch ).
    ENDIF.

  ENDMETHOD.
  METHOD validate_form.

    DATA:
      lx_error                 TYPE REF TO Lcx_abapgit_exception,
      lo_branch_list           TYPE REF TO Lcl_abapgit_git_branch_list,
      lo_url                   TYPE REF TO Lcl_abapgit_git_url,
      lv_offline               TYPE abap_bool,
      lv_head_type             TYPE ty_head_type,
      lv_branch                TYPE ty_remote_settings-branch,
      lv_url                   TYPE ty_remote_settings-url,
      lv_branch_check_error_id TYPE string,
      lv_pull_request          TYPE ty_remote_settings-pull_request,
      lv_commit                TYPE ty_remote_settings-commit.

    ro_validation_log = Lcl_abapgit_html_form_utils=>create( mo_form )->validate( io_form_data ).
    lv_offline = io_form_data->get( c_id-offline ).
    lv_url = io_form_data->get( c_id-url ).

    IF lv_offline = abap_true AND lv_url IS INITIAL.
      ro_validation_log->set(
        iv_key = c_id-url
        iv_val = 'Enter a name for the repository and save' ).
    ENDIF.

    IF lv_offline = abap_false AND lv_url NP 'http*'.
      ro_validation_log->set(
        iv_key = c_id-url
        iv_val = 'Enter the URL of the repository and save' ).
    ELSEIF lv_offline = abap_false.
      TRY.
          Lcl_abapgit_url=>name(
            iv_url      = lv_url
            iv_validate = abap_true ).

          " Provider-specific URL check
          CREATE OBJECT lo_url.
          lo_url->validate_url( lv_url ).
        CATCH Lcx_abapgit_exception INTO lx_error.
          ro_validation_log->set(
            iv_key = c_id-url
            iv_val = lx_error->get_text( ) ).
      ENDTRY.
    ENDIF.

    IF lv_offline = abap_false.
      lv_head_type = io_form_data->get( c_id-head_type ).

      CASE lv_head_type.
        WHEN c_head_types-branch.
          lv_branch = Lif_abapgit_git_definitions=>c_git_branch-heads_prefix && io_form_data->get( c_id-branch ).
          CONDENSE lv_branch.
          lv_branch_check_error_id = c_id-branch.
        WHEN c_head_types-tag.
          lv_branch = Lif_abapgit_git_definitions=>c_git_branch-tags_prefix && io_form_data->get( c_id-tag ).
          CONDENSE lv_branch.
          lv_branch_check_error_id = c_id-tag.
        WHEN c_head_types-pull_request.
          lv_pull_request = io_form_data->get( c_id-pull_request ).
          SPLIT lv_pull_request AT '@' INTO lv_url lv_branch.
          IF lv_branch IS NOT INITIAL.
            lv_branch = Lif_abapgit_git_definitions=>c_git_branch-heads_prefix && lv_branch.
          ENDIF.
          lv_branch_check_error_id = c_id-pull_request.
        WHEN c_head_types-commit.
          lv_commit = io_form_data->get( c_id-commit ).

          " Cannot check for commit existence currently (needs API that doesn't rely on finding the first commit
          " in the branch), check format instead
          IF lv_commit CN '0123456789abcdef'.
            ro_validation_log->set(
              iv_key = c_id-commit
              iv_val = 'Commit needs to be hexadecimal and in lowercase' ).
          ENDIF.
        WHEN OTHERS.
          ro_validation_log->set(
            iv_key = c_id-head_type
            iv_val = 'Unknown head type' ).
      ENDCASE.

      TRY.
          IF lv_branch IS NOT INITIAL.
            lo_branch_list = Lcl_abapgit_git_transport=>branches( lv_url ).
            lo_branch_list->find_by_name( lv_branch ).
          ENDIF.
        CATCH Lcx_abapgit_exception INTO lx_error.
          ro_validation_log->set(
            iv_key = lv_branch_check_error_id
            iv_val = lx_error->get_text( ) ).
      ENDTRY.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    DATA:
      lv_url    TYPE ty_remote_settings-url,
      lv_commit TYPE ty_remote_settings-commit.

    mo_form_data->merge( Lcl_abapgit_html_form_utils=>create( mo_form )->normalize( ii_event->form_data( ) ) ).

    CASE ii_event->mv_action.
      WHEN Lif_abapgit_definitions=>c_action-go_back.
        IF mv_refresh_on_back = abap_true.
          " Note this doesn't trigger if the tab is switched first
          mo_repo->refresh( ).
        ENDIF.

        rs_handled-state = Lcl_abapgit_html_form_utils=>create( mo_form )->exit(
          io_form_data    = mo_form_data
          io_compare_with = initialize_form_data( ) ).

      WHEN c_event-choose_url.
        lv_url = choose_url( ).

        IF lv_url IS INITIAL.
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.
        ELSE.
          mo_form_data->set(
            iv_key = c_id-url
            iv_val = lv_url ).
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
        ENDIF.

      WHEN c_event-change_head_type.
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
        mo_validation_log->clear( ).

      WHEN c_event-choose_branch.
        choose_branch( ). " Unformly handle state below

      WHEN c_event-choose_tag.
        choose_tag( ). " Unformly handle state below

      WHEN c_event-choose_pull_request.
        choose_pr( ). " Unformly handle state below

      WHEN c_event-choose_commit.
        lv_commit = choose_commit( ).

        IF lv_commit IS INITIAL.
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.
        ELSE.
          mo_form_data->set(
            iv_key = c_id-commit
            iv_val = lv_commit ).
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
        ENDIF.

      WHEN c_event-switch.
        switch_online_offline( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_event-save.
        mo_validation_log = validate_form( mo_form_data ).

        IF mo_validation_log->is_empty( ) = abap_true.
          save_settings( ).
        ENDIF.

        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

    ENDCASE.

    IF mo_popup_picklist IS BOUND. " Uniform popup state handling
      " This should happen only for a new popup because
      " on the first re-render main component event handling is blocked
      " and not called again until the popup distruction
      IF mo_popup_picklist->is_in_page( ) = abap_true.
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
      ELSE.
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.
        rs_handled-page  = Lcl_abapgit_gui_page_hoc=>create(
          ii_child_component = mo_popup_picklist
          iv_show_as_modal   = abap_true ).
      ENDIF.
    ENDIF.

    " If staying on form, initialize it with current settings
    IF rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render AND mo_popup_picklist IS NOT BOUND.
      " Switching tabs must change the form layout
      mo_form = get_form_schema( mo_form_data ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions,
          lv_head_type     TYPE ty_head_type,
          lv_offline       TYPE abap_bool.

    IF mo_form_data IS BOUND AND mo_form_data->is_empty( ) = abap_false.
      lv_offline = mo_form_data->get( c_id-offline ).
      IF lv_offline = abap_false.
        lv_head_type = mo_form_data->get( c_id-head_type ).
      ENDIF.
    ELSE.
      lv_offline = ms_settings_snapshot-offline.
      IF lv_offline = abap_false.
        lv_head_type = ms_settings_snapshot-head_type.
      ENDIF.
    ENDIF.

    ls_hotkey_action-ui_component = 'Remote'.

    ls_hotkey_action-description = 'Choose URL'.
    ls_hotkey_action-action      = c_event-choose_url.
    ls_hotkey_action-hotkey      = 'u'.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    IF lv_head_type = c_head_types-branch OR
       lv_head_type = c_head_types-commit.
      ls_hotkey_action-description = 'Choose Branch'.
      ls_hotkey_action-action      = c_event-choose_branch.
      ls_hotkey_action-hotkey      = 'b'.
      INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.
    ENDIF.

    IF lv_head_type = c_head_types-tag.
      ls_hotkey_action-description = 'Choose Tag'.
      ls_hotkey_action-action      = c_event-choose_tag.
      ls_hotkey_action-hotkey      = 't'.
      INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.
    ENDIF.

    IF lv_head_type = c_head_types-commit.
      ls_hotkey_action-description = 'Choose Commit'.
      ls_hotkey_action-action      = c_event-choose_commit.
      ls_hotkey_action-hotkey      = 'c'.
      INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.
    ENDIF.

    IF lv_head_type = c_head_types-pull_request.
      ls_hotkey_action-description = 'Choose Pull Request'.
      ls_hotkey_action-action      = c_event-choose_pull_request.
      ls_hotkey_action-hotkey      = 'p'.
      INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.
    ENDIF.

    IF lv_offline = abap_true.
      ls_hotkey_action-description = 'Switch to Online'.
      ls_hotkey_action-action      = c_event-switch.
      ls_hotkey_action-hotkey      = 'o'.
      INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.
    ELSE.
      ls_hotkey_action-description = 'Switch to Offline'.
      ls_hotkey_action-action      = c_event-switch.
      ls_hotkey_action-hotkey      = 'o'.
      INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    handle_picklist_state( ).

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->wrap(
      iv_tag     = 'div'
      iv_class   = 'repo' " It's OK because it's repo settings ... for now
      ii_content = render_content( ) ).

    IF mo_popup_picklist IS NOT BOUND OR mo_popup_picklist->is_in_page( ) = abap_false.
      register_handlers( ).
    ELSEIF mo_popup_picklist->is_in_page( ) = abap_true.
      " Block usual page events if the popup is an in-page popup
      ri_html->add( Lcl_abapgit_gui_in_page_modal=>create( mo_popup_picklist ) ).
    ENDIF.

  ENDMETHOD.
  METHOD choose_pr.

    DATA ls_pull         TYPE Lif_abapgit_pr_enum_provider=>ty_pull_request.
    DATA lv_url TYPE ty_remote_settings-url.
    DATA lv_popup_cancelled TYPE abap_bool.

    IF iv_is_return = abap_false.

      IF mo_form_data->get( c_id-offline ) = abap_true.
        Lcx_abapgit_exception=>raise( 'Not possible for offline repositories' ).
      ENDIF.

      lv_url = mo_form_data->get( c_id-url ).
      mo_popup_picklist = Lcl_abapgit_popup_pull_request=>create( lv_url
        )->create_picklist(
        )->set_id( c_event-choose_pull_request
        )->set_in_page( abap_true ).

    ELSE.

      lv_popup_cancelled = mo_popup_picklist->was_cancelled( ).
      IF lv_popup_cancelled = abap_false.
        mo_popup_picklist->get_result_item( CHANGING cs_selected = ls_pull ).
        IF ls_pull IS NOT INITIAL.
          mo_form_data->set(
            iv_key = c_id-pull_request
            iv_val = ls_pull-head_url && '@' && ls_pull-head_branch ).
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD handle_picklist_state.

    IF mo_popup_picklist IS BOUND AND
      ( mo_popup_picklist->is_fulfilled( ) = abap_true OR mo_popup_picklist->is_in_page( ) = abap_false ).
      " Picklist is either fullfilled OR
      " it was on its own page and user went back from it via F3/ESC and the picklist had no "graceful back" handler
      CASE mo_popup_picklist->id( ).
        WHEN c_event-choose_pull_request.
          choose_pr( abap_true ).
        WHEN c_event-choose_branch.
          choose_branch( abap_true ).
        WHEN c_event-choose_tag.
          choose_tag( abap_true ).
        WHEN OTHERS.
          Lcx_abapgit_exception=>raise( |Unexpected picklist id { mo_popup_picklist->id( ) }| ).
      ENDCASE.

      CLEAR mo_popup_picklist.
    ENDIF.

  ENDMETHOD.
  METHOD render_content.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_repo_top(
      io_repo               = mo_repo
      iv_show_commit        = abap_false
      iv_interactive_branch = abap_false ) ).

    ri_html->add( mo_form->render(
      io_values         = mo_form_data
      io_validation_log = mo_validation_log ) ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_SETT_REMO implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_SETT_REPO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_sett_repoccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_sett_repoccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_SETT_REPO implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    DATA lo_settings TYPE REF TO Lcl_abapgit_settings.

    super->constructor( ).

    " Feature for ABAP Language Version
    lo_settings = Lcl_abapgit_persist_factory=>get_settings( )->read( ).

    CREATE OBJECT mo_validation_log.
    CREATE OBJECT mo_form_data.
    mo_repo = io_repo.
    mo_form = get_form_schema( ).
    mo_form_data = read_settings( ).

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_sett_repo.

    CREATE OBJECT lo_component
      EXPORTING
        io_repo = io_repo.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Repository Settings'
      io_page_menu       = Lcl_abapgit_gui_menus=>repo_settings(
                             iv_key = io_repo->get_key( )
                             iv_act = Lif_abapgit_definitions=>c_action-repo_settings )
      ii_child_component = lo_component ).

  ENDMETHOD.
  METHOD get_form_schema.

    ro_form = Lcl_abapgit_html_form=>create(
                iv_form_id   = 'repo-settings-form'
                iv_help_page = 'https://docs.abapgit.org/settings-dot-abapgit.html' ).

    ro_form->start_group(
      iv_name        = c_id-dot
      iv_label       = 'Repository Settings (.abapgit.xml)'
      iv_hint        = 'Settings stored in root folder in .abapgit.xml file'
    )->text(
      iv_name        = c_id-name
      iv_label       = 'Name'
      iv_hint        = 'Official name (can be overwritten by local display name)'
    )->text(
      iv_name        = c_id-version_constant
      iv_label       = 'Version Constant'
      iv_placeholder = 'ZVERSION_CLASS=>VERSION_CONSTANT'
    )->text(
      iv_name        = c_id-version_value
      iv_label       = 'Version Value'
      iv_readonly    = abap_true
    )->start_group(
      iv_name        = c_id-i18n
      iv_label       = 'Texts'
    )->text(
      iv_name        = c_id-main_language
      iv_label       = 'Main Language'
      iv_hint        = 'Main language of repository (cannot be changed)'
      iv_readonly    = abap_true
    )->text(
      iv_name        = c_id-i18n_langs
      iv_label       = 'Serialize Translations for Additional Languages'
      iv_hint        = 'Comma-separate 2-letter ISO language codes e.g. "DE,ES,..." - should not include main language'
    )->checkbox(
      iv_name        = c_id-use_lxe
      iv_label       = 'Use Experimental LXE Approach for Translations'
      iv_hint        = 'It''s mandatory to specify the list of languages above in addition to this setting'
    )->start_group(
      iv_name        = c_id-file_system
      iv_label       = 'Files'
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
      iv_name        = c_id-starting_folder
      iv_label       = 'Starting Folder'
      iv_hint        = 'Root folder that defines where serialization starts'
    )->textarea(
      iv_name        = c_id-ignore
      iv_label       = 'Ignore Files'
      iv_hint        = 'List of files in starting folder that shall not be serialized'
    )->start_group(
      iv_name        = c_id-abap_system
      iv_label       = 'ABAP'
    )->table(
      iv_name        = c_id-requirements
      iv_label       = 'Requirements'
      iv_hint        = 'List of software components with minimum release and patch'
    )->column(
      iv_label       = 'Software Component'
      iv_width       = '40%'
    )->column(
      iv_label       = 'Minimum Release'
      iv_width       = '30%'
    )->column(
      iv_label       = 'Minimum Patch'
      iv_width       = '30%' ).

    IF Lcl_abapgit_feature=>is_enabled( Lcl_abapgit_abap_language_vers=>c_feature_flag ) = abap_true.
      ro_form->radio(
        iv_name        = c_id-abap_langu_vers
        iv_default_value = ''
        iv_condense    = abap_true
        iv_label       = 'ABAP Language Version'
        iv_hint        = 'Define the ABAP language version for objects in the repository'
      )->option(
        iv_label       = 'Any (Object-specific ABAP Language Version)'
        iv_value       = ''
      )->option(
        iv_label       = 'Standard ABAP'
        iv_value       = Lif_abapgit_dot_abapgit=>c_abap_language_version-standard
      )->option(
        iv_label       = 'ABAP for Key Users'
        iv_value       = Lif_abapgit_dot_abapgit=>c_abap_language_version-key_user
      )->option(
        iv_label       = 'ABAP for Cloud Development'
        iv_value       = Lif_abapgit_dot_abapgit=>c_abap_language_version-cloud_development ).
    ENDIF.

    ro_form->command(
      iv_label       = 'Save Settings'
      iv_cmd_type    = Lif_abapgit_html_form=>c_cmd_type-input_main
      iv_action      = c_event-save
    )->command(
      iv_label       = 'Back'
      iv_action      = Lif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.
  METHOD read_settings.

    DATA:
      lo_dot          TYPE REF TO Lcl_abapgit_dot_abapgit,
      ls_dot          TYPE Lif_abapgit_dot_abapgit=>ty_dot_abapgit,
      lv_main_lang    TYPE spras,
      lv_language     TYPE t002t-sptxt,
      lv_ignore       TYPE string,
      ls_requirements LIKE LINE OF ls_dot-requirements,
      lv_row          TYPE i,
      lv_val          TYPE string.

    " Get settings from DB
    lo_dot = mo_repo->get_dot_abapgit( ).
    ls_dot = lo_dot->get_data( ).
    lv_main_lang = lo_dot->get_main_language( ).
    CREATE OBJECT ro_form_data.

    " Repository Settings
    SELECT SINGLE sptxt INTO lv_language FROM t002t
      WHERE spras = sy-langu AND sprsl = lv_main_lang.
    IF sy-subrc <> 0.
      lv_language = 'Unknown language; Check your .abapgit.xml file'.
    ENDIF.

    ro_form_data->set(
      iv_key = c_id-name
      iv_val = ls_dot-name ).
    ro_form_data->set(
      iv_key = c_id-main_language
      iv_val = |{ lv_main_lang } ({ lv_language })| ).
    ro_form_data->set(
      iv_key = c_id-i18n_langs
      iv_val = Lcl_abapgit_lxe_texts=>convert_table_to_lang_string( lo_dot->get_i18n_languages( ) ) ).
    ro_form_data->set(
      iv_key = c_id-use_lxe
      iv_val = boolc( lo_dot->use_lxe( ) = abap_true ) ) ##TYPE.
    ro_form_data->set(
      iv_key = c_id-folder_logic
      iv_val = ls_dot-folder_logic ).
    ro_form_data->set(
      iv_key = c_id-starting_folder
      iv_val = ls_dot-starting_folder ).
    ro_form_data->set(
      iv_key = c_id-version_constant
      iv_val = ls_dot-version_constant ).
    TRY.
        ro_form_data->set(
          iv_key = c_id-version_value
          iv_val = Lcl_abapgit_version=>get_version_constant_value( ls_dot-version_constant ) ).
      CATCH Lcx_abapgit_exception.
        ro_form_data->set(
          iv_key = c_id-version_value
          iv_val = '' ).
    ENDTRY.

    lv_ignore = concat_lines_of(
      table = ls_dot-ignore
      sep   = cl_abap_char_utilities=>newline ).

    ro_form_data->set(
      iv_key = c_id-ignore
      iv_val = lv_ignore ).

    LOOP AT ls_dot-requirements INTO ls_requirements.
      lv_row = lv_row + 1.
      DO 3 TIMES.
        CASE sy-index.
          WHEN 1.
            lv_val = ls_requirements-component.
          WHEN 2.
            lv_val = ls_requirements-min_release.
          WHEN 3.
            lv_val = ls_requirements-min_patch.
        ENDCASE.
        ro_form_data->set(
          iv_key = |{ c_id-requirements }-{ lv_row }-{ sy-index }|
          iv_val = lv_val ).
      ENDDO.
    ENDLOOP.

    DO c_empty_rows TIMES.
      lv_row = lv_row + 1.
      DO 3 TIMES.
        ro_form_data->set(
          iv_key = |{ c_id-requirements }-{ lv_row }-{ sy-index }|
          iv_val = '' ).
      ENDDO.
    ENDDO.

    mv_requirements_count = lv_row.

    ro_form_data->set(
      iv_key = |{ c_id-requirements }-{ Lif_abapgit_html_form=>c_rows }|
      iv_val = |{ mv_requirements_count }| ).

    IF Lcl_abapgit_feature=>is_enabled( Lcl_abapgit_abap_language_vers=>c_feature_flag ) = abap_true.
      ro_form_data->set(
        iv_key = c_id-abap_langu_vers
        iv_val = ls_dot-abap_language_version ).
    ENDIF.

  ENDMETHOD.
  METHOD save_settings.

    DATA:
      lo_dot          TYPE REF TO Lcl_abapgit_dot_abapgit,
      lv_ignore       TYPE string,
      lt_ignore       TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      ls_requirements TYPE Lif_abapgit_dot_abapgit=>ty_requirement,
      lt_requirements TYPE Lif_abapgit_dot_abapgit=>ty_requirement_tt.

    lo_dot = mo_repo->get_dot_abapgit( ).

    lo_dot->set_name( mo_form_data->get( c_id-name ) ).
    lo_dot->set_folder_logic( mo_form_data->get( c_id-folder_logic ) ).
    lo_dot->set_starting_folder( mo_form_data->get( c_id-starting_folder ) ).
    lo_dot->set_version_constant( mo_form_data->get( c_id-version_constant ) ).

    IF Lcl_abapgit_feature=>is_enabled( Lcl_abapgit_abap_language_vers=>c_feature_flag ) = abap_true.
      lo_dot->set_abap_language_version( mo_form_data->get( c_id-abap_langu_vers ) ).
    ENDIF.

    lo_dot->set_i18n_languages(
      Lcl_abapgit_lxe_texts=>convert_lang_string_to_table(
        iv_langs              = mo_form_data->get( c_id-i18n_langs )
        iv_skip_main_language = lo_dot->get_main_language( ) ) ).
    lo_dot->use_lxe( boolc( mo_form_data->get( c_id-use_lxe ) = abap_true ) ).

    " Remove all ignores
    lt_ignore = lo_dot->get_data( )-ignore.
    LOOP AT lt_ignore INTO lv_ignore.
      lo_dot->remove_ignore( iv_path = ''
                             iv_filename = lv_ignore ).
    ENDLOOP.

    " Add newly entered ignores
    lt_ignore = Lcl_abapgit_convert=>split_string( mo_form_data->get( c_id-ignore ) ).
    LOOP AT lt_ignore INTO lv_ignore.
      lv_ignore = condense( lv_ignore ).
      IF lv_ignore IS NOT INITIAL.
        lo_dot->add_ignore( iv_path = ''
                            iv_filename = lv_ignore ).
      ENDIF.
    ENDLOOP.

    " Requirements
    DO mv_requirements_count TIMES.
      ls_requirements-component   = to_upper( mo_form_data->get( |{ c_id-requirements }-{ sy-index }-1| ) ).
      ls_requirements-min_release = mo_form_data->get( |{ c_id-requirements }-{ sy-index }-2| ).
      ls_requirements-min_patch   = mo_form_data->get( |{ c_id-requirements }-{ sy-index }-3| ).
      APPEND ls_requirements TO lt_requirements.
    ENDDO.

    SORT lt_requirements BY component min_release min_patch.
    DELETE lt_requirements WHERE component IS INITIAL.
    DELETE ADJACENT DUPLICATES FROM lt_requirements COMPARING ALL FIELDS.

    lo_dot->set_requirements( lt_requirements ).

    mo_repo->set_dot_abapgit( lo_dot ).
    mo_repo->refresh( ).

    COMMIT WORK AND WAIT.

    MESSAGE 'Settings succesfully saved' TYPE 'S'.

    mo_form_data = read_settings( ).

  ENDMETHOD.
  METHOD validate_form.

    DATA:
      lt_lang_list        TYPE Lif_abapgit_definitions=>ty_languages,
      lv_folder           TYPE string,
      lv_len              TYPE i,
      lv_component        TYPE Lif_abapgit_dot_abapgit=>ty_requirement-component,
      lv_min_release      TYPE Lif_abapgit_dot_abapgit=>ty_requirement-min_release,
      lv_min_patch        TYPE Lif_abapgit_dot_abapgit=>ty_requirement-min_patch,
      lv_version_constant TYPE string,
      lx_exception        TYPE REF TO Lcx_abapgit_exception.

    ro_validation_log = Lcl_abapgit_html_form_utils=>create( mo_form )->validate( io_form_data ).

    lv_folder = io_form_data->get( c_id-starting_folder ).
    lv_len = strlen( lv_folder ) - 1.
    IF lv_len > 0 AND lv_folder(1) <> '/'.
      ro_validation_log->set(
        iv_key = c_id-starting_folder
        iv_val = |The folder must begin with /| ).
    ELSEIF lv_len > 0 AND lv_folder+lv_len(1) <> '/'.
      ro_validation_log->set(
        iv_key = c_id-starting_folder
        iv_val = |The folder must end with /| ).
    ELSEIF lv_folder CA '\'.
      ro_validation_log->set(
        iv_key = c_id-starting_folder
        iv_val = |Use / instead of \\| ).
    ENDIF.

    DO mv_requirements_count TIMES.
      lv_component   = mo_form_data->get( |{ c_id-requirements }-{ sy-index }-1| ).
      lv_min_release = mo_form_data->get( |{ c_id-requirements }-{ sy-index }-2| ).
      lv_min_patch   = mo_form_data->get( |{ c_id-requirements }-{ sy-index }-3| ).

      IF lv_component IS INITIAL AND ( lv_min_release IS NOT INITIAL OR lv_min_patch IS NOT INITIAL ).
        ro_validation_log->set(
          iv_key = c_id-requirements
          iv_val = |If you enter a release or patch, you must also enter a software component| ).
      ELSEIF lv_component IS NOT INITIAL AND lv_min_release IS INITIAL.
        ro_validation_log->set(
          iv_key = c_id-requirements
          iv_val = |If you enter a software component, you must also enter a minumum release| ).
      ENDIF.
    ENDDO.

    TRY.
        lv_version_constant = io_form_data->get( c_id-version_constant ).
        IF lv_version_constant IS NOT INITIAL.
          Lcl_abapgit_version=>get_version_constant_value( lv_version_constant ).
        ENDIF.
      CATCH Lcx_abapgit_exception INTO lx_exception.
        ro_validation_log->set(
          iv_key = c_id-version_constant
          iv_val = lx_exception->get_text( ) ).
    ENDTRY.

    lt_lang_list = Lcl_abapgit_lxe_texts=>convert_lang_string_to_table(
      iv_langs              = io_form_data->get( c_id-i18n_langs )
      iv_skip_main_language = mo_repo->get_dot_abapgit( )->get_main_language( ) ).
    IF io_form_data->get( c_id-use_lxe ) = abap_true AND lt_lang_list IS INITIAL.
      ro_validation_log->set(
        iv_key = c_id-i18n_langs
        iv_val = 'LXE approach requires a non-empty list of languages' ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    mo_form_data->merge( Lcl_abapgit_html_form_utils=>create( mo_form )->normalize( ii_event->form_data( ) ) ).

    CASE ii_event->mv_action.
      WHEN Lif_abapgit_definitions=>c_action-go_back.
        rs_handled-state = Lcl_abapgit_html_form_utils=>create( mo_form )->exit(
          io_form_data    = mo_form_data
          io_compare_with = read_settings( ) ).

      WHEN c_event-save.
        " Validate all form entries
        mo_validation_log = validate_form( mo_form_data ).

        IF mo_validation_log->is_empty( ) = abap_true.
          save_settings( ).
        ENDIF.

        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    register_handlers( ).

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( `<div class="repo">` ).

    ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_repo_top(
                    io_repo               = mo_repo
                    iv_show_commit        = abap_false
                    iv_interactive_branch = abap_true ) ).

    ri_html->add( mo_form->render(
      io_values         = mo_form_data
      io_validation_log = mo_validation_log ) ).

    ri_html->add( `</div>` ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_SETT_REPO implementation

*>>>>>>> ZCL_ABAPGIT_HTML_ACTION_UTILS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_html_action_utils=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_html_action_utils=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_html_action_utils=ccau.



class LCL_ABAPGIT_HTML_ACTION_UTILS implementation.
*"* method's implementations
*include methods.
  METHOD add_field.

    DATA ls_field LIKE LINE OF ct_field.

    FIELD-SYMBOLS <lg_src> TYPE any.

    ls_field-name = iv_name.

    CASE cl_abap_typedescr=>describe_by_data( ig_field )->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        ls_field-value = ig_field.
      WHEN cl_abap_typedescr=>kind_struct.
        ASSIGN COMPONENT iv_name OF STRUCTURE ig_field TO <lg_src>.
        ASSERT <lg_src> IS ASSIGNED.
        ls_field-value = <lg_src>.
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

    APPEND ls_field TO ct_field.

  ENDMETHOD.
  METHOD dbkey_encode.

    DATA lt_fields TYPE tihttpnvp.

    add_field( EXPORTING iv_name = 'TYPE'
                         ig_field = is_key-type CHANGING ct_field = lt_fields ).
    add_field( EXPORTING iv_name = 'VALUE'
                         ig_field = is_key-value CHANGING ct_field = lt_fields ).

    rv_string = fields_to_string( lt_fields ).

  ENDMETHOD.
  METHOD dir_encode.

    DATA lt_fields TYPE tihttpnvp.
    add_field( EXPORTING iv_name = 'PATH'
                         ig_field = iv_path CHANGING ct_field = lt_fields ).
    rv_string = fields_to_string( lt_fields ).

  ENDMETHOD.
  METHOD file_encode.

    DATA lt_fields TYPE tihttpnvp.


    add_field(
      EXPORTING
        iv_name  = 'KEY'
        ig_field = iv_key
      CHANGING
        ct_field = lt_fields ).

    add_field(
      EXPORTING
        iv_name  = 'PATH'
        ig_field = ig_file
      CHANGING
        ct_field = lt_fields ).

    add_field(
      EXPORTING
        iv_name  = 'FILENAME'
        ig_field = ig_file
      CHANGING
        ct_field = lt_fields ).

    IF iv_extra IS SUPPLIED.
      add_field(
        EXPORTING
          iv_name  = 'EXTRA'
          ig_field = iv_extra
        CHANGING
          ct_field = lt_fields ).
    ENDIF.

    rv_string = fields_to_string( lt_fields ).

  ENDMETHOD.
  METHOD jump_encode.

    DATA lt_fields TYPE tihttpnvp.


    add_field( EXPORTING iv_name = 'TYPE'
                         ig_field = iv_obj_type CHANGING ct_field = lt_fields ).
    add_field( EXPORTING iv_name = 'NAME'
                         ig_field = iv_obj_name CHANGING ct_field = lt_fields ).

    IF iv_filename IS NOT INITIAL.
      add_field( EXPORTING iv_name = 'FILE'
                           ig_field = iv_filename CHANGING ct_field = lt_fields ).
    ENDIF.

    rv_string = fields_to_string( lt_fields ).

  ENDMETHOD.
  METHOD obj_encode.

    DATA lt_fields TYPE tihttpnvp.


    add_field( EXPORTING iv_name = 'KEY'
                         ig_field = iv_key CHANGING ct_field = lt_fields ).
    add_field( EXPORTING iv_name = 'OBJ_TYPE'
                         ig_field = ig_object CHANGING ct_field = lt_fields ).
    add_field( EXPORTING iv_name = 'OBJ_NAME'
                         ig_field = ig_object CHANGING ct_field = lt_fields ).

    rv_string = fields_to_string( lt_fields ).

  ENDMETHOD.
  METHOD fields_to_string.

* There is no equivalent to cl_http_utility=>fields_to_string released in ABAP Cloud,
* see cl_web_http_utility

    DATA lt_tab   TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA lv_str   TYPE string.
    DATA ls_field LIKE LINE OF it_fields.

    LOOP AT it_fields INTO ls_field.
      ls_field-value = cl_http_utility=>escape_url( ls_field-value ).
      lv_str = ls_field-name && '=' && ls_field-value.
      APPEND lv_str TO lt_tab.
    ENDLOOP.
    rv_string = concat_lines_of(
      table = lt_tab
      sep   = '&' ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_HTML_ACTION_UTILS implementation

*>>>>>>> ZCL_ABAPGIT_HTML_FORM <<<<<<<*

*"* macro definitions
*include zcl_abapgit_html_form=========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_html_form=========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_HTML_FORM implementation.
*"* method's implementations
*include methods.
  METHOD render_command_link.

    DATA lv_class TYPE string VALUE 'dialog-commands'.

    IF is_cmd-cmd_type = Lif_abapgit_html_form=>c_cmd_type-input_main.
      lv_class = lv_class && ' main'.
    ENDIF.

    ii_html->add_a(
      iv_txt   = is_cmd-label
      iv_act   = is_cmd-action
      iv_class = lv_class ).

  ENDMETHOD.
  METHOD checkbox.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type  = Lif_abapgit_html_form=>c_field_type-checkbox.
    ls_field-name  = iv_name.
    ls_field-label = iv_label.
    ls_field-hint  = iv_hint.

    APPEND ls_field TO mt_fields.

    ro_self = me.

  ENDMETHOD.
  METHOD column.

    FIELD-SYMBOLS <ls_last> LIKE LINE OF mt_fields.
    DATA ls_column LIKE LINE OF <ls_last>-subitems.
    DATA lv_size TYPE i.

    lv_size = lines( mt_fields ).
    ASSERT lv_size > 0. " Exception ? Maybe add zcx_no_check ?

    READ TABLE mt_fields INDEX lv_size ASSIGNING <ls_last>.
    ASSERT sy-subrc = 0.
    ASSERT <ls_last>-type = Lif_abapgit_html_form=>c_field_type-table.

    ls_column-label    = iv_label.
    ls_column-value    = iv_width.
    ls_column-readonly = iv_readonly.

    APPEND ls_column TO <ls_last>-subitems.

    ro_self = me.

  ENDMETHOD.
  METHOD command.

    DATA ls_cmd LIKE LINE OF mt_commands.

    ASSERT iv_cmd_type BETWEEN 1 AND 4.

    ls_cmd-label    = iv_label.
    ls_cmd-action   = iv_action.
    ls_cmd-cmd_type = iv_cmd_type.

    APPEND ls_cmd TO mt_commands.

    ro_self = me.

  ENDMETHOD.
  METHOD create.

    DATA lv_ts TYPE timestampl.

    CREATE OBJECT ro_form.
    ro_form->mv_form_id = iv_form_id.
    ro_form->mv_help_page = iv_help_page.

    IF ro_form->mv_form_id IS INITIAL.
      GET TIME STAMP FIELD lv_ts.
      ro_form->mv_form_id = |form_{ lv_ts }|.
    ENDIF.

    ro_form->mv_webgui = Lcl_abapgit_ui_factory=>get_frontend_services( )->is_webgui( ).

  ENDMETHOD.
  METHOD get_fields.
    rt_fields = mt_fields.
  ENDMETHOD.
  METHOD hidden.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type  = Lif_abapgit_html_form=>c_field_type-hidden.
    ls_field-name  = iv_name.
    APPEND ls_field TO mt_fields.

    ro_self = me.

  ENDMETHOD.
  METHOD number.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type     = Lif_abapgit_html_form=>c_field_type-number.
    ls_field-name     = iv_name.
    ls_field-label    = iv_label.
    ls_field-readonly = iv_readonly.
    ls_field-min      = iv_min.
    ls_field-max      = iv_max.
    ls_field-hint     = iv_hint.
    ls_field-required = iv_required.

    APPEND ls_field TO mt_fields.

    ro_self = me.

  ENDMETHOD.
  METHOD option.

    FIELD-SYMBOLS <ls_last> LIKE LINE OF mt_fields.
    DATA ls_option LIKE LINE OF <ls_last>-subitems.
    DATA lv_size TYPE i.

    lv_size = lines( mt_fields ).
    ASSERT lv_size > 0. " Exception ? Maybe add zcx_no_check ?

    READ TABLE mt_fields INDEX lv_size ASSIGNING <ls_last>.
    ASSERT sy-subrc = 0.
    ASSERT <ls_last>-type = Lif_abapgit_html_form=>c_field_type-radio. " Or dropdown - TODO in future

    ls_option-label = iv_label.
    ls_option-value = iv_value.

    APPEND ls_option TO <ls_last>-subitems.

    ro_self = me.

  ENDMETHOD.
  METHOD radio.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type  = Lif_abapgit_html_form=>c_field_type-radio.
    ls_field-name  = iv_name.
    ls_field-label = iv_label.
    ls_field-default_value = iv_default_value.
    ls_field-hint  = iv_hint.
    ls_field-click = iv_action.

    " put options into one column instead of side-by-side
    ls_field-condense = iv_condense.

    APPEND ls_field TO mt_fields.

    ro_self = me.

  ENDMETHOD.
  METHOD render.

    FIELD-SYMBOLS <ls_field> LIKE LINE OF mt_fields.
    FIELD-SYMBOLS <ls_cmd> LIKE LINE OF mt_commands.
    DATA lv_hint TYPE string.
    DATA ls_form_id TYPE string.
    DATA ls_form_action TYPE string.
    DATA lv_cur_group TYPE string.
    DATA lv_url TYPE string.
    DATA lv_autofocus TYPE abap_bool.

    IF mv_form_id IS NOT INITIAL.
      ls_form_id = | id="{ mv_form_id }"|.
    ENDIF.
    LOOP AT mt_commands ASSIGNING <ls_cmd> WHERE cmd_type = Lif_abapgit_html_form=>c_cmd_type-input_main.
      ls_form_action = | action="sapevent:{ <ls_cmd>-action }"|.
      EXIT.
    ENDLOOP.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( |<div class="dialog { iv_form_class }">| ). " to center use 'dialog-form-center'
    ri_html->add( |<form method="post"{ ls_form_id }{ ls_form_action }>| ).

    " Add hidden button that triggers main command when pressing enter
    LOOP AT mt_commands ASSIGNING <ls_cmd> WHERE cmd_type = Lif_abapgit_html_form=>c_cmd_type-input_main.
      ri_html->add( |<button type="submit" formaction="sapevent:{ <ls_cmd>-action }" class="hidden-submit"|
                 && | aria-hidden="true" tabindex="-1"></button>| ).
      EXIT.
    ENDLOOP.

    lv_autofocus = abap_true.
    LOOP AT mt_fields ASSIGNING <ls_field>.
      AT FIRST.
        IF <ls_field>-type <> Lif_abapgit_html_form=>c_field_type-field_group.
          ri_html->add( |<ul>| ).
        ENDIF.
      ENDAT.

      IF <ls_field>-type = Lif_abapgit_html_form=>c_field_type-field_group.
        IF lv_cur_group IS NOT INITIAL AND lv_cur_group <> <ls_field>-name.
          ri_html->add( |</ul>| ).
          ri_html->add( |</fieldset>| ).
        ENDIF.
        IF <ls_field>-hint IS NOT INITIAL.
          lv_hint = | title="{ <ls_field>-hint }"|.
        ELSE.
          lv_hint = ''.
        ENDIF.
        lv_cur_group = <ls_field>-name.
        ri_html->add( |<fieldset name="{ <ls_field>-name }">| ).
        ri_html->add( |<legend{ lv_hint }>{ <ls_field>-label }</legend>| ).
        ri_html->add( |<ul>| ).
        CONTINUE.
      ENDIF.

      render_field(
        ii_html           = ri_html
        io_values         = io_values
        io_validation_log = io_validation_log
        is_field          = <ls_field>
        iv_autofocus      = lv_autofocus ).

      lv_autofocus = abap_false.

      AT LAST.
        ri_html->add( |</ul>| ).
        IF lv_cur_group IS NOT INITIAL.
          ri_html->add( |</fieldset>| ).
        ENDIF.
      ENDAT.
    ENDLOOP.

    ri_html->add( |<ul>| ).
    ri_html->add( |<li class="dialog-commands">| ).

    IF mv_help_page IS NOT INITIAL.
      lv_url = escape( val    = mv_help_page
                       format = cl_abap_format=>e_url ).
      ri_html->add_a(
        iv_txt   = Lcl_abapgit_gui_buttons=>help( )
        iv_act   = |{ Lif_abapgit_definitions=>c_action-url }?url={ lv_url }|
        iv_class = 'dialog-help'
        iv_title = 'Help' ).
    ENDIF.

    LOOP AT mt_commands ASSIGNING <ls_cmd>.
      render_command(
        ii_html = ri_html
        is_cmd  = <ls_cmd> ).
    ENDLOOP.

    ri_html->add( |</li>| ).
    ri_html->add( |</ul>| ).
    ri_html->add( |</form>| ).
    ri_html->add( |</div>| ).

    register_handlers( ).

  ENDMETHOD.
  METHOD render_command.

    " HTML GUI supports only links for submitting forms
    IF mv_webgui = abap_true.
      render_command_link(
        is_cmd  = is_cmd
        ii_html = ii_html ).
      RETURN.
    ENDIF.

    CASE is_cmd-cmd_type.
      WHEN Lif_abapgit_html_form=>c_cmd_type-link.

        render_command_link(
          is_cmd  = is_cmd
          ii_html = ii_html ).

      WHEN Lif_abapgit_html_form=>c_cmd_type-button.

        ii_html->add( |<button type="submit" name="action" value="{ is_cmd-action }"|
                   && | class="action-commands">{ is_cmd-label }</button>| ).

      WHEN Lif_abapgit_html_form=>c_cmd_type-input.

        ii_html->add( |<input type="submit" value="{ is_cmd-label }" formaction="sapevent:{ is_cmd-action }">| ).

      WHEN Lif_abapgit_html_form=>c_cmd_type-input_main.

        ii_html->add( |<input type="submit" value="{ is_cmd-label }" class="main">| ).

      WHEN OTHERS.
        ASSERT 0 = 1.

    ENDCASE.

  ENDMETHOD.
  METHOD render_field.

    DATA:
      ls_attr       TYPE ty_attr,
      lv_item_class TYPE string.

    " Get value and validation error
    ls_attr-value = io_values->get( is_field-name ).

    IF is_field-type <> Lif_abapgit_html_form=>c_field_type-textarea.
      ls_attr-value = escape( val    = ls_attr-value
                              format = cl_abap_format=>e_html_attr ).
    ENDIF.

    IF io_validation_log IS BOUND.
      ls_attr-error = io_validation_log->get( is_field-name ).
      IF ls_attr-error IS NOT INITIAL.
        ls_attr-error = escape( val    = ls_attr-error
                                format = cl_abap_format=>e_html_text ).
        ls_attr-error = |<small>{ ls_attr-error }</small>|.
      ENDIF.
    ENDIF.

    " Prepare field attributes
    IF is_field-required = abap_true.
      ls_attr-required = ' <em>*</em>'.
    ENDIF.

    IF is_field-hint IS NOT INITIAL.
      ls_attr-hint = escape( val    = is_field-hint
                             format = cl_abap_format=>e_html_attr ).
      ls_attr-hint = | title="{ ls_attr-hint }"|.
    ENDIF.

    IF is_field-placeholder IS NOT INITIAL.
      ls_attr-placeholder = escape( val    = is_field-placeholder
                                    format = cl_abap_format=>e_html_attr ).
      ls_attr-placeholder = | placeholder="{ ls_attr-placeholder }"|.
    ENDIF.

    IF is_field-readonly = abap_true.
      ls_attr-readonly = ' readonly'.
    ENDIF.

    IF iv_autofocus = abap_true.
      ls_attr-autofocus = ' autofocus'.
    ENDIF.

    " Prepare item class
    lv_item_class = is_field-item_class.
    IF ls_attr-error IS NOT INITIAL.
      lv_item_class = condense( lv_item_class && ' error' ).
    ENDIF.
    IF is_field-type = Lif_abapgit_html_form=>c_field_type-text AND is_field-max BETWEEN 1 AND 20.
      " Reduced width for short fields
      lv_item_class = lv_item_class && ' w40'.
    ENDIF.
    IF is_field-type = Lif_abapgit_html_form=>c_field_type-hidden.
      lv_item_class = lv_item_class && ' hidden'.
    ENDIF.
    IF lv_item_class IS NOT INITIAL.
      lv_item_class = | class="{ lv_item_class }"|.
    ENDIF.

    " Render field
    ii_html->add( |<li{ lv_item_class }>| ).

    CASE is_field-type.
      WHEN Lif_abapgit_html_form=>c_field_type-text OR Lif_abapgit_html_form=>c_field_type-number.

        render_field_text(
          ii_html  = ii_html
          is_field = is_field
          is_attr  = ls_attr ).

      WHEN Lif_abapgit_html_form=>c_field_type-textarea.

        render_field_textarea(
          ii_html  = ii_html
          is_field = is_field
          is_attr  = ls_attr ).

      WHEN Lif_abapgit_html_form=>c_field_type-checkbox.

        render_field_checkbox(
          ii_html  = ii_html
          is_field = is_field
          is_attr  = ls_attr ).

      WHEN Lif_abapgit_html_form=>c_field_type-radio.

        render_field_radio(
          ii_html  = ii_html
          is_field = is_field
          is_attr  = ls_attr ).

      WHEN Lif_abapgit_html_form=>c_field_type-table.

        render_field_table(
          ii_html   = ii_html
          is_field  = is_field
          is_attr   = ls_attr
          io_values = io_values ).

      WHEN Lif_abapgit_html_form=>c_field_type-hidden.

        render_field_hidden(
          ii_html  = ii_html
          is_field = is_field
          is_attr  = ls_attr ).

      WHEN OTHERS.
        ASSERT 1 = 0.
    ENDCASE.

    ii_html->add( '</li>' ).

  ENDMETHOD.
  METHOD render_field_checkbox.

    DATA lv_checked TYPE string.

    IF is_attr-error IS NOT INITIAL.
      ii_html->add( is_attr-error ).
    ENDIF.

    IF is_attr-value = abap_true OR is_attr-value = 'on'.
      " boolc return ` ` which is not initial -> bug after 1st validation
      lv_checked = ' checked'.
    ENDIF.

    ii_html->add( |<input type="checkbox" name="{ is_field-name }" id="{ is_field-name }"| &&
                  |{ lv_checked }{ is_attr-readonly }{ is_attr-autofocus }>| ).
    ii_html->add( |<label for="{ is_field-name }"{ is_attr-hint }>{ is_field-label }</label>| ).

  ENDMETHOD.
  METHOD render_field_hidden.

    ii_html->add( |<input type="hidden" name="{ is_field-name }" id="{ is_field-name }" value="{ is_attr-value }">| ).

  ENDMETHOD.
  METHOD render_field_radio.

    DATA:
      lv_checked   TYPE string,
      lv_opt_id    TYPE string,
      lv_opt_value TYPE string,
      lv_onclick   TYPE string.

    FIELD-SYMBOLS <ls_opt> LIKE LINE OF is_field-subitems.

    ii_html->add( |<label{ is_attr-hint }>{ is_field-label }</label>| ).

    IF is_attr-error IS NOT INITIAL.
      ii_html->add( is_attr-error ).
    ENDIF.

    ii_html->add( |<div class="radio-container">| ).

    LOOP AT is_field-subitems ASSIGNING <ls_opt>.

      lv_opt_id = |{ is_field-name }{ sy-tabix }|.
      lv_opt_value = escape( val    = <ls_opt>-value
                             format = cl_abap_format=>e_html_attr ).

      CLEAR lv_checked.
      IF is_attr-value = lv_opt_value OR ( is_attr-value IS INITIAL AND lv_opt_value = is_field-default_value ).
        lv_checked = ' checked'.
      ENDIF.

      " With edge browser control radio buttons aren't checked automatically when
      " activated with link hints. Therefore we need to check them manually.
      IF is_field-click IS NOT INITIAL.
        lv_onclick = |onclick="|
                  && |var form = document.getElementById('{ mv_form_id }');|
                  && |document.getElementById('{ lv_opt_id }').checked = true;|
                  && |form.action = 'sapevent:{ is_field-click }';|
                  && |form.submit();"|.
      ELSE.
        lv_onclick = |onclick="document.getElementById('{ lv_opt_id }').checked = true;"|.
      ENDIF.

      IF is_field-condense = abap_true.
        ii_html->add( '<div>' ).
      ENDIF.
      ii_html->add( |<input type="radio" name="{ is_field-name }" id="{ lv_opt_id }"|
                 && | value="{ lv_opt_value }"{ lv_checked }{ is_attr-autofocus }|
                 && | { lv_onclick }>| ).
      ii_html->add( |<label for="{ lv_opt_id }">{ <ls_opt>-label }</label>| ).
      IF is_field-condense = abap_true.
        ii_html->add( '</div>' ).
      ENDIF.
    ENDLOOP.

    ii_html->add( '</div>' ).

  ENDMETHOD.
  METHOD render_field_table.

    DATA:
      lv_value    TYPE string,
      lv_readonly TYPE string,
      lv_rows     TYPE i,
      lv_cell_id  TYPE string.

    FIELD-SYMBOLS <ls_subitem> LIKE LINE OF is_field-subitems.

    ii_html->add( |<label for="{ is_field-name }"{ is_attr-hint }>{ is_field-label }</label>| ).

    IF is_attr-error IS NOT INITIAL.
      ii_html->add( is_attr-error ).
    ENDIF.

    lv_rows = io_values->get( |{ is_field-name }-{ Lif_abapgit_html_form=>c_rows }| ).

    " Render table only if there are some data rows
    IF lv_rows > 0.

      ii_html->add( |<table name="{ is_field-name }" id="{ is_field-name }" class="table-container">| ).

      ii_html->add( |<thead>| ).
      ii_html->add( |<tr>| ).
      LOOP AT is_field-subitems ASSIGNING <ls_subitem>.
        CLEAR lv_value.
        IF <ls_subitem>-value IS NOT INITIAL.
          lv_value = escape( val    = <ls_subitem>-value
                             format = cl_abap_format=>e_html_attr ).
          lv_value = | width="{ lv_value }"|.
        ENDIF.
        ii_html->add( |<td{ lv_value }>{ <ls_subitem>-label }</td>| ).
      ENDLOOP.
      ii_html->add( |</tr>| ).
      ii_html->add( |</thead>| ).

      ii_html->add( |<tbody>| ).
      DO lv_rows TIMES.
        lv_rows = sy-index.
        ii_html->add( |<tr>| ).
        LOOP AT is_field-subitems ASSIGNING <ls_subitem>.
          lv_cell_id = |{ is_field-name }-{ lv_rows }-{ sy-tabix }|.
          lv_value = escape( val    = io_values->get( lv_cell_id )
                             format = cl_abap_format=>e_html_attr ).
          CLEAR lv_readonly.
          IF <ls_subitem>-readonly = abap_true.
            lv_readonly = | readonly|.
          ENDIF.
          ii_html->add( |<td><input type="text" name="{ lv_cell_id }" id="{
                        lv_cell_id }" value="{ lv_value }"{ lv_readonly }></td>| ).
        ENDLOOP.
        ii_html->add( |</tr>| ).
      ENDDO.
      ii_html->add( |</tbody>| ).

      ii_html->add( |</table>| ).

    ELSE.
      ii_html->add( |<input type="text" name="{ is_field-name }" id="{
                    is_field-name }" value="Not available" readonly>| ).
    ENDIF.

    " Hidden field with number of rows to simplify getting values from form
    lv_value = |{ is_field-name }-{ Lif_abapgit_html_form=>c_rows }|.
    ii_html->add( |<input type="number" name="{ lv_value }" id="{ lv_value }"|
               && | value="{ lv_rows }" style="display:none">| ).

  ENDMETHOD.
  METHOD render_field_text.

    DATA:
      lv_type      TYPE string,
      lv_minlength TYPE string,
      lv_maxlength TYPE string.

    ii_html->add( |<label for="{ is_field-name }"{ is_attr-hint }>{ is_field-label }{ is_attr-required }</label>| ).

    IF is_attr-error IS NOT INITIAL.
      ii_html->add( is_attr-error ).
    ENDIF.

    IF is_field-side_action IS NOT INITIAL.
      ii_html->add( '<div class="input-container">' ). " Ugly :(
    ENDIF.

    IF is_field-type = Lif_abapgit_html_form=>c_field_type-number.
      lv_type = 'number'.
    ELSEIF is_field-password = abap_true.
      lv_type = 'password'.
    ELSE.
      lv_type = 'text'.
    ENDIF.

    IF is_field-min > 0.
      lv_minlength = | minlength={ is_field-min }|.
    ENDIF.
    IF is_field-max > 0 AND is_field-max < cl_abap_math=>max_int4.
      lv_maxlength = | maxlength={ is_field-max }|.
    ENDIF.

    ii_html->add( |<input type="{ lv_type }" name="{ is_field-name }" id="{ is_field-name }"|
               && | value="{ is_attr-value }"{ is_field-dblclick }{ is_attr-placeholder }|
               && |{ is_attr-readonly }{ is_attr-autofocus }{ lv_minlength }{ lv_maxlength }>| ).

    IF is_field-side_action IS NOT INITIAL.
      ii_html->add( '</div>' ).
      ii_html->add( '<div class="command-container">' ).
      ii_html->add( |<input type="submit" value="&#x2026;" formaction="sapevent:{ is_field-side_action }"|
                 && | title="{ is_field-label }">| ).
      ii_html->add( '</div>' ).
    ENDIF.

  ENDMETHOD.
  METHOD render_field_textarea.

    DATA lv_rows TYPE string.
    DATA lv_cols TYPE string.
    DATA lv_html TYPE string.

    ii_html->add( |<label for="{ is_field-name }"{ is_attr-hint }>{ is_field-label }{ is_attr-required }</label>| ).

    IF is_attr-error IS NOT INITIAL.
      ii_html->add( is_attr-error ).
    ENDIF.

    IF is_field-rows > 0.
      lv_rows = | rows="{ is_field-rows }"|.
    ELSEIF is_attr-value IS NOT INITIAL.
      lv_rows = | rows="{ lines( Lcl_abapgit_convert=>split_string( is_attr-value ) ) + 1 }"|.
    ENDIF.

    IF is_field-cols > 0.
      lv_cols = | cols="{ is_field-cols }"|.
    ENDIF.

    " Avoid adding line-breaks inside textarea tag (except for the actual value)
    lv_html = |<textarea name="{ is_field-name }" id="{ is_field-name }"{ lv_rows }{ lv_cols }|
           && |{ is_attr-readonly }{ is_attr-autofocus }{ is_attr-placeholder }>|.
    lv_html = lv_html && escape( val    = is_attr-value
                                 format = cl_abap_format=>e_html_attr ).
    lv_html = lv_html && |</textarea>|.

    ii_html->add( lv_html ).

  ENDMETHOD.
  METHOD start_group.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type  = Lif_abapgit_html_form=>c_field_type-field_group.
    ls_field-label = iv_label.
    ls_field-name  = iv_name.
    ls_field-hint  = iv_hint.

    APPEND ls_field TO mt_fields.

    ro_self = me.

  ENDMETHOD.
  METHOD table.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type  = Lif_abapgit_html_form=>c_field_type-table.
    ls_field-name  = iv_name.
    ls_field-label = iv_label.
    ls_field-hint  = iv_hint.

    APPEND ls_field TO mt_fields.

    ro_self = me.

  ENDMETHOD.
  METHOD text.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type       = Lif_abapgit_html_form=>c_field_type-text.
    ls_field-name       = iv_name.
    ls_field-label      = iv_label.
    ls_field-upper_case = iv_upper_case.
    ls_field-readonly   = iv_readonly.
    ls_field-min        = iv_min.
    ls_field-max        = iv_max.
    ls_field-password   = iv_password.
    ls_field-condense   = iv_condense.
    ls_field-hint       = iv_hint.
    ls_field-required   = iv_required.
    ls_field-placeholder = iv_placeholder.

    IF iv_side_action IS NOT INITIAL AND mv_form_id IS NOT INITIAL.
      ls_field-item_class = 'with-command'.
      ls_field-side_action = iv_side_action.
      ls_field-dblclick = | ondblclick="document.getElementById('{ mv_form_id }').action = 'sapevent:|
                       && |{ iv_side_action }'; document.getElementById('{ mv_form_id }').submit()"|.
    ENDIF.

    APPEND ls_field TO mt_fields.

    ro_self = me.

  ENDMETHOD.
  METHOD textarea.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type        = Lif_abapgit_html_form=>c_field_type-textarea.
    ls_field-name        = iv_name.
    ls_field-label       = iv_label.
    ls_field-readonly    = iv_readonly.
    ls_field-hint        = iv_hint.
    ls_field-required    = iv_required.
    ls_field-placeholder = iv_placeholder.
    ls_field-rows        = iv_rows.
    ls_field-cols        = iv_cols.
    ls_field-upper_case  = iv_upper_case.

    APPEND ls_field TO mt_fields.

    ro_self = me.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions.
    FIELD-SYMBOLS: <ls_command> TYPE Lif_abapgit_html_form=>ty_command.

    ls_hotkey_action-ui_component = |Form-{ mv_form_id }|.

    READ TABLE mt_commands WITH KEY cmd_type = Lif_abapgit_html_form=>c_cmd_type-input_main
                           ASSIGNING <ls_command>.
    IF sy-subrc = 0.
      ls_hotkey_action-description = <ls_command>-label.
      ls_hotkey_action-action      = <ls_command>-action.
      ls_hotkey_action-hotkey      = |Enter|.
      INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.
    ENDIF.

    READ TABLE mt_commands WITH KEY action = Lif_abapgit_definitions=>c_action-go_back
                           ASSIGNING <ls_command>.
    IF sy-subrc = 0.
      ls_hotkey_action-description = <ls_command>-label.
      ls_hotkey_action-action      = <ls_command>-action.
      ls_hotkey_action-hotkey      = |F3|.
      INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_HTML_FORM implementation

*>>>>>>> ZCL_ABAPGIT_HTML_FORM_UTILS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_html_form_utils===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_html_form_utils===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_html_form_utils===ccau.






class LCL_ABAPGIT_HTML_FORM_UTILS implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    mo_form = io_form.
  ENDMETHOD.
  METHOD create.
    CREATE OBJECT ro_form_util
      EXPORTING
        io_form = io_form.
  ENDMETHOD.
  METHOD exit.

    DATA lv_answer TYPE c LENGTH 1.

    IF is_dirty(
      io_form_data    = io_form_data
      io_compare_with = io_compare_with ) = abap_true.
      lv_answer = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
        iv_display_cancel_button = abap_false
        iv_titlebar       = 'abapGit - Unsaved Changes'
        iv_text_question  = 'There are unsaved changes. Do you want to exit the form?'
        iv_default_button = '2' ).

      IF lv_answer = '1'.
        rv_state = Lcl_abapgit_gui=>c_event_state-go_back_to_bookmark.
      ELSE.
        rv_state = Lcl_abapgit_gui=>c_event_state-no_more_act.
      ENDIF.
    ELSE.
      rv_state = Lcl_abapgit_gui=>c_event_state-go_back_to_bookmark.
    ENDIF.

  ENDMETHOD.
  METHOD is_dirty.
    rv_dirty = boolc( io_form_data->mt_entries <> io_compare_with->mt_entries ).
  ENDMETHOD.
  METHOD is_empty.

    DATA:
      lt_fields TYPE Lif_abapgit_html_form=>ty_fields,
      lv_value  TYPE string,
      lv_rows   TYPE i,
      lv_row    TYPE i.

    FIELD-SYMBOLS <ls_field> LIKE LINE OF lt_fields.

    rv_empty = abap_true.
    lt_fields = mo_form->get_fields( ).
    LOOP AT lt_fields ASSIGNING <ls_field> WHERE type <> Lif_abapgit_html_form=>c_field_type-field_group.
      lv_value = condense(
        val = io_form_data->get( <ls_field>-name )
        del = ` ` ).

      IF <ls_field>-type = Lif_abapgit_html_form=>c_field_type-number.
        rv_empty = boolc( lv_value IS INITIAL OR lv_value = '0' ).
      ELSEIF <ls_field>-type = Lif_abapgit_html_form=>c_field_type-table.
        lv_rows = io_form_data->get( |{ <ls_field>-name }-{ Lif_abapgit_html_form=>c_rows }| ).
        DO lv_rows TIMES.
          lv_row = sy-index.
          DO lines( <ls_field>-subitems ) TIMES.
            lv_value = io_form_data->get( |{ <ls_field>-name }-{ lv_row }-{ sy-index }| ).
            rv_empty = boolc( lv_value IS INITIAL ).
            IF rv_empty <> abap_true.
              RETURN.
            ENDIF.
          ENDDO.
        ENDDO.
      ELSEIF <ls_field>-type = Lif_abapgit_html_form=>c_field_type-textarea.
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_value WITH ''.
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_value WITH ''.
        rv_empty = boolc( lv_value IS INITIAL ).
      ELSE.
        rv_empty = boolc( lv_value IS INITIAL ).
      ENDIF.

      IF rv_empty <> abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD normalize.

    DATA:
      lt_fields TYPE Lif_abapgit_html_form=>ty_fields,
      lv_value  TYPE string,
      lv_rows   TYPE i,
      lv_row    TYPE i,
      lv_len    TYPE i.

    FIELD-SYMBOLS <ls_field> LIKE LINE OF lt_fields.

    CREATE OBJECT ro_form_data.

    IF io_form_data->is_empty( ) = abap_true.
      RETURN.
    ENDIF.

    lt_fields = mo_form->get_fields( ).
    LOOP AT lt_fields ASSIGNING <ls_field> WHERE type <> Lif_abapgit_html_form=>c_field_type-field_group
      AND type <> Lif_abapgit_html_form=>c_field_type-hidden.

      CLEAR lv_value.
      lv_value = io_form_data->get( <ls_field>-name ).
      IF <ls_field>-condense = abap_true.
        lv_value = condense( val = lv_value
                             del = ` ` ).
      ENDIF.

      IF <ls_field>-type = Lif_abapgit_html_form=>c_field_type-checkbox.
        ro_form_data->set(
          iv_key = <ls_field>-name
          iv_val = boolc( lv_value = 'on' ) ) ##TYPE.
      ELSEIF ( <ls_field>-type = Lif_abapgit_html_form=>c_field_type-text
          OR <ls_field>-type = Lif_abapgit_html_form=>c_field_type-textarea )
          AND <ls_field>-upper_case = abap_true.
        ro_form_data->set(
          iv_key = <ls_field>-name
          iv_val = to_upper( lv_value ) ).
      ELSEIF <ls_field>-type = Lif_abapgit_html_form=>c_field_type-number.
        " Numeric value is checked in validation
        ro_form_data->set(
          iv_key = <ls_field>-name
          iv_val = condense( val = lv_value del = ` ` ) ).
      ELSEIF <ls_field>-type = Lif_abapgit_html_form=>c_field_type-table.
        lv_rows = io_form_data->get( |{ <ls_field>-name }-{ Lif_abapgit_html_form=>c_rows }| ).
        DO lv_rows TIMES.
          lv_row = sy-index.
          DO lines( <ls_field>-subitems ) TIMES.
            lv_value = io_form_data->get( |{ <ls_field>-name }-{ lv_row }-{ sy-index }| ).
            ro_form_data->set(
              iv_key = |{ <ls_field>-name }-{ lv_row }-{ sy-index }|
              iv_val = lv_value ).
          ENDDO.
        ENDDO.
        ro_form_data->set(
          iv_key = |{ <ls_field>-name }-{ Lif_abapgit_html_form=>c_rows }|
          iv_val = |{ lv_rows }| ).
      ELSEIF <ls_field>-type = Lif_abapgit_html_form=>c_field_type-textarea.
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_value
          WITH cl_abap_char_utilities=>newline.

        " Remove last line if empty (ie 2x newline)
        lv_len = strlen( lv_value ) - 2.
        IF lv_len >= 0 AND lv_value+lv_len(1) = cl_abap_char_utilities=>newline.
          lv_len = lv_len + 1.
          lv_value = lv_value(lv_len).
        ENDIF.

        ro_form_data->set(
          iv_key = <ls_field>-name
          iv_val = lv_value ).
      ELSE.
        ro_form_data->set(
          iv_key = <ls_field>-name
          iv_val = lv_value ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD set_data.
    mo_form_data = io_form_data.
  ENDMETHOD.
  METHOD validate.

    DATA:
      lt_fields TYPE Lif_abapgit_html_form=>ty_fields,
      lv_value  TYPE string,
      lv_number TYPE i.

    FIELD-SYMBOLS <ls_field> LIKE LINE OF lt_fields.

    CREATE OBJECT ro_validation_log.

    lt_fields = mo_form->get_fields( ).
    LOOP AT lt_fields ASSIGNING <ls_field>.
      lv_value = io_form_data->get( <ls_field>-name ).
      IF <ls_field>-condense = abap_true.
        lv_value = condense( val = lv_value
                             del = ` ` ).
      ENDIF.
      IF <ls_field>-required IS NOT INITIAL AND lv_value IS INITIAL.
        ro_validation_log->set(
          iv_key = <ls_field>-name
          iv_val = |{ <ls_field>-label } cannot be empty| ).
      ENDIF.
      CASE <ls_field>-type.
        WHEN Lif_abapgit_html_form=>c_field_type-text.
          IF <ls_field>-min <> cl_abap_math=>min_int4 AND strlen( lv_value ) < <ls_field>-min.
            ro_validation_log->set(
              iv_key = <ls_field>-name
              iv_val = |{ <ls_field>-label } must not be shorter than { <ls_field>-min } characters| ).
          ENDIF.
          IF <ls_field>-max <> cl_abap_math=>max_int4 AND strlen( lv_value ) > <ls_field>-max.
            ro_validation_log->set(
              iv_key = <ls_field>-name
              iv_val = |{ <ls_field>-label } must not be longer than { <ls_field>-max } characters| ).
          ENDIF.
        WHEN Lif_abapgit_html_form=>c_field_type-number.
          TRY.
              lv_number = lv_value.
            CATCH cx_root.
              ro_validation_log->set(
                iv_key = <ls_field>-name
                iv_val = |{ <ls_field>-label } is not numeric| ).
              CONTINUE.
          ENDTRY.
          IF <ls_field>-min <> cl_abap_math=>min_int4 AND lv_number < <ls_field>-min.
            ro_validation_log->set(
              iv_key = <ls_field>-name
              iv_val = |{ <ls_field>-label } must not be lower than { <ls_field>-min }| ).
          ENDIF.
          IF <ls_field>-max <> cl_abap_math=>max_int4 AND lv_number > <ls_field>-max.
            ro_validation_log->set(
              iv_key = <ls_field>-name
              iv_val = |{ <ls_field>-label } must not be higher than { <ls_field>-max }| ).
          ENDIF.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_HTML_FORM_UTILS implementation

*>>>>>>> ZCL_ABAPGIT_INJECTOR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_injector==========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_injector==========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_injector==========ccau.






class LCL_ABAPGIT_INJECTOR implementation.
*"* method's implementations
*include methods.
  METHOD set_exit.
    Lcl_abapgit_exit=>gi_global_exit = ii_exit.
  ENDMETHOD.
  METHOD set_code_inspector.

    DATA: ls_code_inspector LIKE LINE OF Lcl_abapgit_factory=>gt_code_inspector.
    FIELD-SYMBOLS: <ls_code_inspector> LIKE LINE OF Lcl_abapgit_factory=>gt_code_inspector.

    READ TABLE Lcl_abapgit_factory=>gt_code_inspector
         ASSIGNING <ls_code_inspector>
         WITH TABLE KEY package = iv_package.
    IF sy-subrc <> 0.

      ls_code_inspector-package = iv_package.

      INSERT ls_code_inspector
             INTO TABLE Lcl_abapgit_factory=>gt_code_inspector
             ASSIGNING <ls_code_inspector>.

    ENDIF.

    <ls_code_inspector>-instance = ii_code_inspector.

  ENDMETHOD.
  METHOD set_cts_api.
    Lcl_abapgit_factory=>gi_cts_api = ii_cts_api.
  ENDMETHOD.
  METHOD set_environment.
    Lcl_abapgit_factory=>gi_environment = ii_environment.
  ENDMETHOD.
  METHOD set_http_agent.
    Lcl_abapgit_factory=>gi_http_agent = ii_http_agent.
  ENDMETHOD.
  METHOD set_longtexts.
    Lcl_abapgit_factory=>gi_longtext = ii_longtexts.
  ENDMETHOD.
  METHOD set_lxe_texts.
    Lcl_abapgit_factory=>gi_lxe_texts = ii_lxe_texts.
  ENDMETHOD.
  METHOD set_sap_namespace.
    Lcl_abapgit_factory=>gi_sap_namespace = ii_namespace.
  ENDMETHOD.
  METHOD set_sap_package.

    DATA: ls_sap_package TYPE Lcl_abapgit_factory=>ty_sap_package.
    FIELD-SYMBOLS: <ls_sap_package> TYPE Lcl_abapgit_factory=>ty_sap_package.

    READ TABLE Lcl_abapgit_factory=>gt_sap_package
         ASSIGNING <ls_sap_package>
         WITH TABLE KEY package = iv_package.

    IF sy-subrc <> 0.

      ls_sap_package-package = iv_package.
      INSERT ls_sap_package
             INTO TABLE Lcl_abapgit_factory=>gt_sap_package
             ASSIGNING <ls_sap_package>.

    ENDIF.

    <ls_sap_package>-instance = ii_sap_package.

  ENDMETHOD.
  METHOD set_stage_logic.

    Lcl_abapgit_factory=>gi_stage_logic = ii_logic.

  ENDMETHOD.
  METHOD set_tadir.
    Lcl_abapgit_factory=>gi_tadir = ii_tadir.
  ENDMETHOD.
  METHOD set_sap_report.
    Lcl_abapgit_factory=>gi_sap_report = ii_report.
  ENDMETHOD.
  METHOD set_function_module.
    Lcl_abapgit_factory=>gi_function_module = ii_function_module.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_INJECTOR implementation

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

*CLASS zcl_abapgit_longtexts DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KW3OSFY.


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
      WHERE id = iv_longtext_id AND object LIKE lv_object ESCAPE '#'
      ORDER BY PRIMARY KEY.

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
      WHERE id = iv_longtext_id AND object LIKE lv_object ESCAPE '#'
      ORDER BY PRIMARY KEY.

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

*>>>>>>> ZCL_ABAPGIT_MIGRATIONS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_migrations========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_migrations========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_MIGRATIONS implementation.
*"* method's implementations
*include methods.
  METHOD migrate_offline_repos.

    DATA:
      lt_repos TYPE Lif_abapgit_repo_srv=>ty_repo_list,
      li_repo  LIKE LINE OF lt_repos,
      lo_dot   TYPE REF TO Lcl_abapgit_dot_abapgit.

    TRY.
        " Get offline repos only
        lt_repos = Lcl_abapgit_repo_srv=>get_instance( )->list( abap_true ).

        LOOP AT lt_repos INTO li_repo.
          lo_dot = li_repo->get_dot_abapgit( ).
          " Move repo name from URL fields to .abapGit.xml
          IF li_repo->ms_data-url IS NOT INITIAL AND lo_dot->get_name( ) IS INITIAL.
            lo_dot->set_name( li_repo->ms_data-url ).
            li_repo->set_dot_abapgit( lo_dot ).
          ENDIF.
        ENDLOOP.
      CATCH Lcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.
  METHOD run.

    " Migrate STDTEXT to TABLE
    Lcl_abapgit_persist_migrate=>run( ).

    " Create ZIF_APACK_MANIFEST interface
    Lcl_abapgit_apack_migration=>run( ).

    " Migrate checksums from repo metadata to separate DB object
    Lcl_abapgit_repo_cs_migration=>run( ).

    " Migrate offline repo metadata
    migrate_offline_repos( ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_MIGRATIONS implementation

*>>>>>>> ZCL_ABAPGIT_OBJECTS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_objects===========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_objects===========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_objects===========ccau.


*----------------------------------------------------------------------*
*       CLASS SHRITEFUH64VYIPO5IWUYB3KW3YSFY DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS SHRITEFUH64VYIPO5IWUYB3KW3YSFY IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*




*CLASS zcl_abapgit_objects DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KW34SFY.


class LCL_ABAPGIT_OBJECTS implementation.
*"* method's implementations
*include methods.
  METHOD deserialize_step.

    DATA: li_progress TYPE REF TO Lif_abapgit_progress,
          li_exit     TYPE REF TO Lif_abapgit_exit,
          lx_exc      TYPE REF TO Lcx_abapgit_exception.

    FIELD-SYMBOLS: <ls_obj> LIKE LINE OF is_step-objects.


    Lcl_abapgit_objects_activation=>clear( ).

    ii_log->add_success( |>> Step { is_step-order } - { is_step-descr }| ).

    li_progress = Lcl_abapgit_progress=>get_instance( lines( is_step-objects ) ).

    LOOP AT is_step-objects ASSIGNING <ls_obj>.
      li_progress->show(
        iv_current = sy-tabix
        iv_text    = |Step { is_step-order } - { is_step-descr }:| &&
                     | { <ls_obj>-item-obj_type } { <ls_obj>-item-obj_name }| ).

      TRY.
          <ls_obj>-obj->deserialize( iv_package   = <ls_obj>-package
                                     io_xml       = <ls_obj>-xml
                                     iv_step      = is_step-step_id
                                     ii_log       = ii_log
                                     iv_transport = iv_transport ).
          APPEND LINES OF <ls_obj>-obj->mo_files->get_accessed_files( ) TO ct_files.

          ii_log->add_success( iv_msg = |Object { <ls_obj>-item-obj_name } imported|
                               is_item = <ls_obj>-item ).

        CATCH Lcx_abapgit_exception INTO lx_exc.
          ii_log->add_exception( ix_exc = lx_exc
                                 is_item = <ls_obj>-item ).
          ii_log->add_error( iv_msg = |Import of object { <ls_obj>-item-obj_name } failed|
                             is_item = <ls_obj>-item ).
      ENDTRY.

    ENDLOOP.

    li_progress->show( iv_current = lines( is_step-objects )
                       iv_text    = |Step { is_step-order } - Activating Objects| ).

    CASE is_step-step_id.
      WHEN Lif_abapgit_object=>gc_step_id-ddic.
        Lcl_abapgit_objects_activation=>activate(
          iv_ddic = abap_true
          ii_log  = ii_log ).
      WHEN Lif_abapgit_object=>gc_step_id-abap.
        Lcl_abapgit_objects_activation=>activate(
          iv_ddic = abap_false
          ii_log  = ii_log ).
      WHEN Lif_abapgit_object=>gc_step_id-late.
        " late can have both DDIC (like TABL with REF TO) and non-DDIC objects
        Lcl_abapgit_objects_activation=>activate(
          iv_ddic = abap_true
          ii_log  = ii_log ).
        Lcl_abapgit_objects_activation=>activate(
          iv_ddic = abap_false
          ii_log  = ii_log ).
    ENDCASE.

    li_progress->off( ).

*   Call postprocessing
    li_exit = Lcl_abapgit_exit=>get_instance( ).

    li_exit->deserialize_postprocess( is_step = is_step
                                      ii_log  = ii_log ).

  ENDMETHOD.
  METHOD changed_by.

    DATA: li_obj TYPE REF TO Lif_abapgit_object.

    " For unsupported objects, return empty string
    IF is_type_supported( is_item-obj_type ) = abap_false.
      RETURN.
    ENDIF.

    TRY.
        li_obj = create_object( is_item ).
        rv_user = li_obj->changed_by( get_extra_from_filename( iv_filename ) ).
      CATCH Lcx_abapgit_exception ##NO_HANDLER.
        " Ignore errors
    ENDTRY.

    IF rv_user IS INITIAL.
      " Eg. ".abapgit.xml" file
      rv_user = Lcl_abapgit_objects_super=>c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD change_package_assignments.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid    = 'R3TR'
        wi_tadir_object   = is_item-obj_type
        wi_tadir_obj_name = is_item-obj_name
        wi_tadir_devclass = is_item-devclass
        wi_test_modus     = abap_false
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc = 0.
      ii_log->add_success( iv_msg  = |Object { is_item-obj_name } assigned to package { is_item-devclass }|
                           is_item = is_item ).
    ELSE.
      ii_log->add_error( iv_msg  = |Package change of object { is_item-obj_name } failed|
                         is_item = is_item ).
    ENDIF.

  ENDMETHOD.
  METHOD check_duplicates.

    DATA: lt_files          TYPE Lif_abapgit_git_definitions=>ty_files_tt,
          lv_path           TYPE string,
          lv_filename       TYPE string,
          lt_duplicates     TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
          lv_duplicates     LIKE LINE OF lt_duplicates,
          lv_all_duplicates TYPE string.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF it_files.

    lt_files = it_files.
    SORT lt_files BY path ASCENDING filename ASCENDING.

    LOOP AT lt_files ASSIGNING <ls_file>.
      IF lv_path = <ls_file>-path AND lv_filename = <ls_file>-filename.
        CONCATENATE <ls_file>-path <ls_file>-filename INTO lv_duplicates.
        APPEND lv_duplicates TO lt_duplicates.
      ENDIF.
      lv_path = <ls_file>-path.
      lv_filename = <ls_file>-filename.
    ENDLOOP.

    IF lt_duplicates IS NOT INITIAL.
      CONCATENATE LINES OF lt_duplicates INTO lv_all_duplicates SEPARATED BY `, `.
      Lcx_abapgit_exception=>raise( |Duplicates: { lv_all_duplicates }| ).
    ENDIF.

  ENDMETHOD.
  METHOD check_main_package.

    " check package restrictions, closed package, descriptive or
    " functional package
    cl_pak_object_types=>check_object_type(
      EXPORTING
        i_working_mode         = 'I'
        i_package_name         = iv_package
        i_pgmid                = 'R3TR'
        i_object_type          = iv_obj_type
      EXCEPTIONS
        wrong_object_type      = 1
        package_not_extensible = 2
        package_not_loaded     = 3
        OTHERS                 = 4 ).
    CASE sy-subrc.
      WHEN 0.
        RETURN.
      WHEN 2.
        Lcx_abapgit_exception=>raise( |Object type { iv_obj_type } not allowed for package { iv_package }| ).
      WHEN OTHERS.
        Lcx_abapgit_exception=>raise_t100( ).
    ENDCASE.

  ENDMETHOD.
  METHOD check_objects_locked.

    DATA: li_obj TYPE REF TO Lif_abapgit_object.

    FIELD-SYMBOLS: <ls_item> LIKE LINE OF it_items.

    LOOP AT it_items ASSIGNING <ls_item>.

      " You should remember that we ignore not supported objects here,
      " because otherwise the process aborts which is not desired
      IF is_type_supported( <ls_item>-obj_type ) = abap_false.
        CONTINUE.
      ENDIF.

      li_obj = create_object( <ls_item> ).

      IF li_obj->is_locked( ) = abap_true.
        Lcx_abapgit_exception=>raise( |Object { <ls_item>-obj_type } { <ls_item>-obj_name } |
                                   && |is locked. Action not possible.| ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD class_name.

    CONCATENATE 'LCL_ABAPGIT_OBJECT_' is_item-obj_type INTO rv_class_name.

  ENDMETHOD.
  METHOD compare_remote_to_local.
* this method is used for comparing local with remote objects
* before pull, this is useful eg. when overwriting a TABL object.
* only the main XML file is used for comparison

    DATA: ls_remote_file    TYPE Lif_abapgit_git_definitions=>ty_file,
          li_remote_version TYPE REF TO Lif_abapgit_xml_input,
          lv_count          TYPE i,
          ls_result         TYPE Lif_abapgit_comparator=>ty_result,
          lv_answer         TYPE string,
          li_comparator     TYPE REF TO Lif_abapgit_comparator,
          ls_item           TYPE Lif_abapgit_definitions=>ty_item.

    FIND ALL OCCURRENCES OF '.' IN is_result-filename MATCH COUNT lv_count.

    IF is_result-filename CS '.XML' AND lv_count = 2.
      IF ii_object->exists( ) = abap_false.
        RETURN.
      ENDIF.

      READ TABLE it_remote WITH KEY file
        COMPONENTS filename = is_result-filename INTO ls_remote_file.
      IF sy-subrc <> 0. "if file does not exist in remote, we don't need to validate
        RETURN.
      ENDIF.

      li_comparator = ii_object->get_comparator( ).
      IF NOT li_comparator IS BOUND.
        RETURN.
      ENDIF.

      CREATE OBJECT li_remote_version
        TYPE Lcl_abapgit_xml_input
        EXPORTING
          iv_xml      = Lcl_abapgit_convert=>xstring_to_string_utf8( ls_remote_file-data )
          iv_filename = ls_remote_file-filename.

      ls_result = li_comparator->compare( ii_remote = li_remote_version
                                          ii_log = ii_log ).
      IF ls_result-text IS INITIAL.
        RETURN.
      ENDIF.

      "log comparison result
      ls_item-obj_type = is_result-obj_type.
      ls_item-obj_name = is_result-obj_name.
      ii_log->add_warning( iv_msg = ls_result-text
                           is_item = ls_item ).

      "continue or abort?
      IF Lcl_abapgit_ui_factory=>get_frontend_services( )->gui_is_available( ) = abap_true.
        lv_answer = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
          iv_titlebar              = 'Warning'
          iv_text_question         = ls_result-text
          iv_text_button_1         = 'Pull Anyway'
          iv_icon_button_1         = 'ICON_OKAY'
          iv_text_button_2         = 'Cancel'
          iv_icon_button_2         = 'ICON_CANCEL'
          iv_default_button        = '2'
          iv_display_cancel_button = abap_false ).

        IF lv_answer = '2'.
          Lcx_abapgit_exception=>raise( |Deserialization for object { is_result-obj_name } | &
                                        |(type { is_result-obj_type }) aborted by user| ).
        ENDIF.
      ELSE.
        Lcx_abapgit_exception=>raise( |Deserialization for object { is_result-obj_name } | &
                                      |(type { is_result-obj_type }) aborted, user descision required| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD create_object.

    DATA: lv_message            TYPE string,
          lv_class_name         TYPE string,
          ls_obj_serializer_map LIKE LINE OF gt_obj_serializer_map.
    DATA lo_obj_base TYPE REF TO Lcl_abapgit_objects_super.
    DATA lo_i18n_params TYPE REF TO Lcl_abapgit_i18n_params.

    IF io_i18n_params IS BOUND.
      lo_i18n_params = io_i18n_params.
    ELSE.
      lo_i18n_params = Lcl_abapgit_i18n_params=>new( ). " All defaults
    ENDIF.

    READ TABLE gt_obj_serializer_map
      INTO ls_obj_serializer_map WITH KEY item = is_item.
    IF sy-subrc = 0.
      lv_class_name = ls_obj_serializer_map-metadata-class.
    ELSEIF is_metadata IS NOT INITIAL.
*        Metadata is provided only on deserialization
*        Once this has been triggered, the same deserializer shall be used
*        for subsequent processes.
*        Thus, buffer the metadata afterwards
      ls_obj_serializer_map-item      = is_item.
      ls_obj_serializer_map-metadata  = is_metadata.
      INSERT ls_obj_serializer_map INTO TABLE gt_obj_serializer_map.
      lv_class_name = is_metadata-class.
    ELSE.
      lv_class_name = class_name( is_item ).
    ENDIF.

    REPLACE FIRST OCCURRENCE OF 'LCL_O' IN lv_class_name WITH 'LCL_ABAPGIT_O'.

    IF Lcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_true.
      " Prevent accidental usage of object handlers in the developer version
      lv_class_name = |\\PROGRAM={ sy-repid }\\CLASS={ lv_class_name }|.
    ENDIF.

    TRY.
        CREATE OBJECT ri_obj TYPE (lv_class_name)
          EXPORTING
            is_item     = is_item
            iv_language = lo_i18n_params->ms_params-main_language.
      CATCH cx_sy_create_object_error.
        lv_message = |Object type { is_item-obj_type } is not supported by this system|.
        IF iv_native_only = abap_false.
          TRY. " 2nd step, try looking for plugins
              CREATE OBJECT ri_obj TYPE Lcl_abapgit_objects_bridge
                EXPORTING
                  is_item = is_item.
            CATCH cx_sy_create_object_error.
              Lcx_abapgit_exception=>raise( lv_message ).
          ENDTRY.
        ELSE. " No native support? -> fail
          Lcx_abapgit_exception=>raise( lv_message ).
        ENDIF.
    ENDTRY.

    IF ri_obj IS BOUND.
      lo_obj_base ?= ri_obj.
      lo_obj_base->mo_i18n_params = lo_i18n_params.
    ENDIF.

  ENDMETHOD.
  METHOD delete.

    DATA: ls_item     TYPE Lif_abapgit_definitions=>ty_item,
          li_progress TYPE REF TO Lif_abapgit_progress,
          lt_tadir    LIKE it_tadir,
          lt_deleted  LIKE it_tadir,
          lt_items    TYPE Lif_abapgit_definitions=>ty_items_tt,
          lx_error    TYPE REF TO Lcx_abapgit_exception,
          lv_count    TYPE i.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF it_tadir.

    IF it_tadir IS INITIAL.
      RETURN.
    ENDIF.

    lt_tadir = it_tadir.

    IF ii_log IS BOUND.
      IF lines( lt_tadir ) = 1.
        ii_log->add_info( |>>> Deleting 1 object| ).
      ELSE.
        ii_log->add_info( |>>> Deleting { lines( lt_tadir ) } objects| ).
      ENDIF.
    ENDIF.

    IF is_checks-transport-required = abap_true.
      Lcl_abapgit_default_transport=>get_instance( )->set( is_checks-transport-transport ).
    ENDIF.

    TRY.
        Lcl_abapgit_dependencies=>resolve( CHANGING ct_tadir = lt_tadir ).

        li_progress = Lcl_abapgit_progress=>get_instance( lines( lt_tadir ) ).

        lt_items = map_tadir_to_items( lt_tadir ).

        check_objects_locked( lt_items ).

      CATCH Lcx_abapgit_exception INTO lx_error.
        Lcl_abapgit_default_transport=>get_instance( )->reset( ).
        RAISE EXCEPTION lx_error.
    ENDTRY.

    lv_count = 1.
    DO.
      CLEAR lt_deleted.
      LOOP AT lt_tadir ASSIGNING <ls_tadir>.
        li_progress->show( iv_current = lv_count
                           iv_text    = |Delete { <ls_tadir>-obj_name }| ).

        CLEAR ls_item.
        ls_item-obj_type = <ls_tadir>-object.
        ls_item-obj_name = <ls_tadir>-obj_name.

        TRY.
            delete_object(
              iv_package   = <ls_tadir>-devclass
              is_item      = ls_item
              iv_transport = is_checks-transport-transport ).

            INSERT <ls_tadir> INTO TABLE lt_deleted.
            DELETE lt_tadir.
            lv_count = lv_count + 1.

            " make sure to save object deletions
            COMMIT WORK.

            IF ii_log IS BOUND.
              ii_log->add_info( iv_msg  = |Object { ls_item-obj_type } { ls_item-obj_name } deleted|
                                is_item = ls_item ).
            ENDIF.

          CATCH Lcx_abapgit_exception INTO lx_error.
            IF ii_log IS BOUND.
              ii_log->add_exception( ix_exc  = lx_error
                                     is_item = ls_item ).
              ii_log->add_error( iv_msg  = |Deletion of object { ls_item-obj_name } failed|
                                 is_item = ls_item ).
            ENDIF.
        ENDTRY.

      ENDLOOP.

      " Exit if done or nothing else was deleted
      IF lines( lt_tadir ) = 0 OR lines( lt_deleted ) = 0.
        EXIT.
      ENDIF.
    ENDDO.

    Lcl_abapgit_default_transport=>get_instance( )->reset( ).

    IF lx_error IS BOUND AND lines( lt_tadir ) > 0.
      Lcx_abapgit_exception=>raise( 'Error during uninstall. Check the log.' ).
    ENDIF.

    li_progress->off( ).

  ENDMETHOD.
  METHOD delete_object.

    DATA: li_obj TYPE REF TO Lif_abapgit_object.

    " Nothing to do for unsupported objects
    IF is_type_supported( is_item-obj_type ) = abap_false.
      RETURN.
    ENDIF.

    li_obj = create_object( is_item ).
    li_obj->delete( iv_package   = iv_package
                    iv_transport = iv_transport ).

  ENDMETHOD.
  METHOD deserialize.

    DATA: ls_item     TYPE Lif_abapgit_definitions=>ty_item,
          li_obj      TYPE REF TO Lif_abapgit_object,
          lt_remote   TYPE Lif_abapgit_git_definitions=>ty_files_tt,
          lv_package  TYPE devclass,
          lo_files    TYPE REF TO Lcl_abapgit_objects_files,
          ls_metadata TYPE Lif_abapgit_definitions=>ty_metadata,
          lo_xml      TYPE REF TO Lif_abapgit_xml_input,
          lt_results  TYPE Lif_abapgit_definitions=>ty_results_tt,
          li_progress TYPE REF TO Lif_abapgit_progress,
          lv_path     TYPE string,
          lt_items    TYPE Lif_abapgit_definitions=>ty_items_tt,
          lt_steps_id TYPE Lif_abapgit_definitions=>ty_deserialization_step_tt,
          lt_steps    TYPE Lif_abapgit_objects=>ty_step_data_tt,
          lx_exc      TYPE REF TO Lcx_abapgit_exception.
    DATA lo_folder_logic TYPE REF TO Lcl_abapgit_folder_logic.
    DATA lo_i18n_params TYPE REF TO Lcl_abapgit_i18n_params.
    DATA lo_timer TYPE REF TO Lcl_abapgit_timer.
    DATA lo_abap_language_vers TYPE REF TO Lcl_abapgit_abap_language_vers.

    FIELD-SYMBOLS: <ls_result>  TYPE Lif_abapgit_definitions=>ty_result,
                   <lv_step_id> TYPE LINE OF Lif_abapgit_definitions=>ty_deserialization_step_tt,
                   <ls_step>    TYPE LINE OF Lif_abapgit_objects=>ty_step_data_tt,
                   <ls_deser>   TYPE LINE OF Lif_abapgit_objects=>ty_deserialization_tt.

    lt_steps = get_deserialize_steps( ).

    lv_package = io_repo->get_package( ).

    IF is_checks-transport-required = abap_true.
      Lcl_abapgit_default_transport=>get_instance( )->set( is_checks-transport-transport ).
    ENDIF.

    Lcl_abapgit_objects_activation=>clear( ).

    lt_remote = io_repo->get_files_remote( iv_ignore_files = abap_true ).

    lt_results = Lcl_abapgit_file_deserialize=>get_results(
      io_repo = io_repo
      ii_log = ii_log ).

    IF lt_results IS INITIAL.
      RETURN.
    ENDIF.

    Lcl_abapgit_objects_check=>checks_adjust(
      EXPORTING
        io_repo    = io_repo
        is_checks  = is_checks
      CHANGING
        ct_results = lt_results ).

    li_progress = Lcl_abapgit_progress=>get_instance( lines( lt_results ) ).

    lt_items = map_results_to_items( lt_results ).

    lo_timer = Lcl_abapgit_timer=>create(
      iv_text  = 'Deserialize:'
      iv_count = lines( lt_items ) )->start( ).

    Lcl_abapgit_factory=>get_cts_api( )->confirm_transport_messages( ).

    check_objects_locked( lt_items ).

    lo_i18n_params = Lcl_abapgit_i18n_params=>new( is_params = determine_i18n_params(
      io_dot                = io_repo->get_dot_abapgit( )
      iv_main_language_only = io_repo->get_local_settings( )-main_language_only ) ).

    IF lines( lt_items ) = 1.
      ii_log->add_info( |>>> Deserializing 1 object| ).
    ELSE.
      ii_log->add_info( |>>> Deserializing { lines( lt_items ) } objects| ).
    ENDIF.

    CREATE OBJECT lo_abap_language_vers
      EXPORTING
        io_dot_abapgit = io_repo->get_dot_abapgit( ).

    lo_folder_logic = Lcl_abapgit_folder_logic=>get_instance( ).
    LOOP AT lt_results ASSIGNING <ls_result>.
      li_progress->show( iv_current = sy-tabix
                         iv_text    = |Prepare Deserialize: { <ls_result>-obj_type } { <ls_result>-obj_name }| ).

      CLEAR ls_item.
      CLEAR: lv_path, lv_package.

      ls_item-obj_type = <ls_result>-obj_type.
      ls_item-obj_name = <ls_result>-obj_name.

      "error handling & logging added
      TRY.
          IF ls_item-obj_type <> 'NSPC'.
            " If package does not exist yet, it will be created with this call
            lv_package = lo_folder_logic->path_to_package(
              iv_top  = io_repo->get_package( )
              io_dot  = io_repo->get_dot_abapgit( )
              iv_path = <ls_result>-path ).

            check_main_package(
              iv_package  = lv_package
              iv_obj_type = ls_item-obj_type ).
          ENDIF.

          IF ls_item-obj_type = 'DEVC'.
            " Packages have the same filename across different folders. The path needs to be supplied
            " to find the correct file.
            lv_path = <ls_result>-path.
          ENDIF.

          ls_item-devclass = lv_package.
          ls_item-abap_language_version = lo_abap_language_vers->get_abap_language_vers_by_objt(
                                                                    iv_object_type = ls_item-obj_type
                                                                    iv_package = lv_package ).

          IF <ls_result>-packmove = abap_true.
            " Move object to new package
            change_package_assignments( is_item = ls_item
                                        ii_log  = ii_log ).
            " No other changes required
            CONTINUE.
          ENDIF.

          " Create or update object
          CREATE OBJECT lo_files
            EXPORTING
              is_item = ls_item
              iv_path = lv_path.

          lo_files->set_files( lt_remote ).

          IF lo_files->is_json_metadata( ) = abap_false.
            "analyze XML in order to instantiate the proper serializer
            lo_xml = lo_files->read_xml( ).
            ls_metadata = lo_xml->get_metadata( ).
          ELSE.
            " there's no XML and metadata for JSON format
            CLEAR: lo_xml, ls_metadata.
          ENDIF.

          li_obj = create_object(
            is_item        = ls_item
            is_metadata    = ls_metadata
            io_i18n_params = lo_i18n_params ).

          compare_remote_to_local(
            ii_object = li_obj
            it_remote = lt_remote
            is_result = <ls_result>
            ii_log    = ii_log ).

          li_obj->mo_files = lo_files.

          "get required steps for deserialize the object
          lt_steps_id = li_obj->get_deserialize_steps( ).

          LOOP AT lt_steps_id ASSIGNING <lv_step_id>.
            READ TABLE lt_steps WITH KEY step_id = <lv_step_id> ASSIGNING <ls_step>.
            ASSERT sy-subrc = 0.
            IF <lv_step_id> = Lif_abapgit_object=>gc_step_id-ddic AND
               Lcl_abapgit_objects_activation=>is_ddic_type( ls_item-obj_type ) = abap_false.
              " DDIC only for DDIC objects
              Lcx_abapgit_exception=>raise( |Step { <lv_step_id> } is only for DDIC objects| ).
            ENDIF.
            APPEND INITIAL LINE TO <ls_step>-objects ASSIGNING <ls_deser>.
            <ls_deser>-item    = ls_item.
            <ls_deser>-obj     = li_obj.
            <ls_deser>-xml     = lo_xml.
            <ls_deser>-package = lv_package.
          ENDLOOP.

          " LXE, TODO refactor and move below activation
          IF lo_i18n_params->is_lxe_applicable( ) = abap_true.
            Lcl_abapgit_factory=>get_lxe_texts( )->deserialize(
              iv_object_type = ls_item-obj_type
              iv_object_name = ls_item-obj_name
              io_i18n_params = lo_i18n_params
              ii_xml         = lo_xml
              io_files       = lo_files ).
          ENDIF.


        CATCH Lcx_abapgit_exception INTO lx_exc.
          ii_log->add_exception( ix_exc = lx_exc
                                 is_item = ls_item ).
          ii_log->add_error( iv_msg = |Import of object { ls_item-obj_name } failed|
                             is_item = ls_item ).
          "object should not be part of any deserialization step
          CONTINUE.
      ENDTRY.

    ENDLOOP.

    li_progress->off( ).

    "run deserialize for all steps and its objects
    deserialize_steps(
      EXPORTING
        it_steps     = lt_steps
        ii_log       = ii_log
        iv_transport = is_checks-transport-transport
      CHANGING
        ct_files     = rt_accessed_files ).

    " TODO: LXE translations (objects has been activated by now)

    update_package_tree( io_repo->get_package( ) ).

    Lcl_abapgit_default_transport=>get_instance( )->reset( ).

    lo_timer->end( abap_true ).

  ENDMETHOD.
  METHOD deserialize_checks.

    rs_checks = Lcl_abapgit_objects_check=>deserialize_checks( io_repo ).

  ENDMETHOD.
  METHOD determine_i18n_params.

    " TODO: unify with ZCL_ABAPGIT_SERIALIZE=>DETERMINE_I18N_PARAMS, same code

    IF io_dot IS BOUND.
      rs_i18n_params-main_language         = io_dot->get_main_language( ).
      rs_i18n_params-use_lxe               = io_dot->use_lxe( ).
      rs_i18n_params-main_language_only    = iv_main_language_only.
      rs_i18n_params-translation_languages = Lcl_abapgit_lxe_texts=>get_translation_languages(
        iv_main_language  = io_dot->get_main_language( )
        it_i18n_languages = io_dot->get_i18n_languages( ) ).
    ENDIF.

    IF rs_i18n_params-main_language IS INITIAL.
      rs_i18n_params-main_language = sy-langu.
    ENDIF.

  ENDMETHOD.
  METHOD exists.

    DATA: li_obj TYPE REF TO Lif_abapgit_object.

    " Might be called for objects without tadir entry
    IF is_item IS INITIAL.
      RETURN.
    ENDIF.

    " For unsupported objects, assume object exists
    IF is_type_supported( is_item-obj_type ) = abap_false.
      rv_bool = abap_true.
      RETURN.
    ENDIF.

    TRY.
        li_obj = create_object( is_item ).
        rv_bool = li_obj->exists( ).
      CATCH Lcx_abapgit_exception.
        " Ignore errors and assume the object exists
        rv_bool = abap_true.
    ENDTRY.

  ENDMETHOD.
  METHOD get_deserialize_steps.
    FIELD-SYMBOLS: <ls_step> TYPE LINE OF Lif_abapgit_objects=>ty_step_data_tt.

    APPEND INITIAL LINE TO rt_steps ASSIGNING <ls_step>.
    <ls_step>-step_id      = Lif_abapgit_object=>gc_step_id-early.
    <ls_step>-descr        = 'Pre-process Objects'.
    <ls_step>-syntax_check = abap_false.
    <ls_step>-order        = 1.

    APPEND INITIAL LINE TO rt_steps ASSIGNING <ls_step>.
    <ls_step>-step_id      = Lif_abapgit_object=>gc_step_id-ddic.
    <ls_step>-descr        = 'Deserialize DDIC Objects'.
    <ls_step>-syntax_check = abap_false.
    <ls_step>-order        = 2.

    APPEND INITIAL LINE TO rt_steps ASSIGNING <ls_step>.
    <ls_step>-step_id      = Lif_abapgit_object=>gc_step_id-abap.
    <ls_step>-descr        = 'Deserialize non-DDIC Objects'.
    <ls_step>-syntax_check = abap_false.
    <ls_step>-order        = 3.

    APPEND INITIAL LINE TO rt_steps ASSIGNING <ls_step>.
    <ls_step>-step_id      = Lif_abapgit_object=>gc_step_id-late.
    <ls_step>-descr        = 'Post-process Objects'.
    <ls_step>-syntax_check = abap_true.
    <ls_step>-order        = 4.

    SORT rt_steps BY order. " ensure correct processing order
  ENDMETHOD.
  METHOD is_active.

    DATA: li_obj TYPE REF TO Lif_abapgit_object.

    " For unsupported objects, assume active state
    IF is_type_supported( is_item-obj_type ) = abap_false.
      rv_active = abap_true.
      RETURN.
    ENDIF.

    TRY.
        li_obj = create_object( is_item ).
        rv_active = li_obj->is_active( ).
      CATCH cx_sy_dyn_call_illegal_method
            cx_sy_ref_is_initial
            Lcx_abapgit_exception.
        " Ignore errors and assume active state
        rv_active = abap_true.
    ENDTRY.

  ENDMETHOD.
  METHOD is_supported.

    TRY.
        create_object(
          is_item        = is_item
          iv_native_only = iv_native_only ).
        rv_bool = abap_true.
      CATCH Lcx_abapgit_exception.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.
  METHOD is_type_supported.

    DATA: ls_item               TYPE Lif_abapgit_definitions=>ty_item,
          ls_supported_obj_type TYPE ty_supported_types.

    FIELD-SYMBOLS <ls_supported_obj_type> TYPE ty_supported_types.

    IF iv_obj_type IS INITIAL.
      " empty object type should never exist
      RETURN.
    ENDIF.

    READ TABLE gt_supported_obj_types
      ASSIGNING <ls_supported_obj_type>
      WITH KEY obj_type = iv_obj_type.

    IF sy-subrc <> 0.

      ls_item-obj_type = iv_obj_type.

      ls_supported_obj_type-obj_type  = iv_obj_type.
      ls_supported_obj_type-supported = is_supported( ls_item ).

      INSERT ls_supported_obj_type INTO TABLE gt_supported_obj_types.

      rv_bool = ls_supported_obj_type-supported.
      RETURN.

    ENDIF.

    rv_bool = <ls_supported_obj_type>-supported.

  ENDMETHOD.
  METHOD jump.

    DATA: li_obj  TYPE REF TO Lif_abapgit_object,
          lv_exit TYPE abap_bool.

    " Nothing to do for unsupported objects
    IF is_type_supported( is_item-obj_type ) = abap_false.
      Lcx_abapgit_exception=>raise( |Object type { is_item-obj_type } is not supported by this system| ).
    ENDIF.

    " Nothing to do if object does not exist
    li_obj = create_object( is_item ).

    IF li_obj->exists( ) = abap_false.
      Lcx_abapgit_exception=>raise( |Object { is_item-obj_type } { is_item-obj_name } doesn't exist| ).
    ENDIF.

    " First priority object-specific handler
    lv_exit = li_obj->jump( get_extra_from_filename( iv_filename ) ).

    IF lv_exit = abap_false.
      " Open object in new window with generic jumper
      lv_exit = Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump(
        is_item        = is_item
        is_sub_item    = is_sub_item
        iv_line_number = iv_line_number
        iv_new_window  = iv_new_window ).
    ENDIF.

    IF lv_exit = abap_false.
      Lcx_abapgit_exception=>raise( |Jump to { is_item-obj_type } { is_item-obj_name } not possible| ).
    ENDIF.

  ENDMETHOD.
  METHOD map_results_to_items.

    DATA: ls_item LIKE LINE OF rt_items.
    FIELD-SYMBOLS: <ls_result> TYPE Lif_abapgit_definitions=>ty_result.

    LOOP AT it_results ASSIGNING <ls_result>.

      ls_item-devclass = <ls_result>-package.
      ls_item-obj_type = <ls_result>-obj_type.
      ls_item-obj_name = <ls_result>-obj_name.
      INSERT ls_item INTO TABLE rt_items.

    ENDLOOP.

  ENDMETHOD.
  METHOD map_tadir_to_items.

    DATA: ls_item LIKE LINE OF rt_items.
    FIELD-SYMBOLS: <ls_tadir> TYPE Lif_abapgit_definitions=>ty_tadir.

    LOOP AT it_tadir ASSIGNING <ls_tadir>.

      ls_item-devclass = <ls_tadir>-devclass.
      ls_item-obj_type = <ls_tadir>-object.
      ls_item-obj_name = <ls_tadir>-obj_name.
      INSERT ls_item INTO TABLE rt_items.

    ENDLOOP.

  ENDMETHOD.
  METHOD serialize.

    DATA: li_obj   TYPE REF TO Lif_abapgit_object,
          lx_error TYPE REF TO Lcx_abapgit_exception,
          li_xml   TYPE REF TO Lif_abapgit_xml_output,
          lo_files TYPE REF TO Lcl_abapgit_objects_files.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF rs_files_and_item-files.

    IF is_type_supported( is_item-obj_type ) = abap_false.
      Lcx_abapgit_exception=>raise( |Object type ignored, not supported: {
        is_item-obj_type }-{
        is_item-obj_name }| ).
    ENDIF.

    li_obj = create_object(
      is_item        = is_item
      io_i18n_params = io_i18n_params ).

    CREATE OBJECT lo_files EXPORTING is_item = is_item.
    li_obj->mo_files = lo_files. " TODO move into create_object

    CREATE OBJECT li_xml TYPE Lcl_abapgit_xml_output.

    rs_files_and_item-item = is_item.

    TRY.
        li_obj->serialize( li_xml ).
      CATCH Lcx_abapgit_exception INTO lx_error.
        rs_files_and_item-item-inactive = boolc( li_obj->is_active( ) = abap_false ).
        RAISE EXCEPTION lx_error.
    ENDTRY.

    IF io_i18n_params->is_lxe_applicable( ) = abap_true.
      Lcl_abapgit_factory=>get_lxe_texts( )->serialize(
        iv_object_type = is_item-obj_type
        iv_object_name = is_item-obj_name
        io_i18n_params = io_i18n_params
        io_files       = lo_files
        ii_xml         = li_xml ).
    ENDIF.

    IF lo_files->is_json_metadata( ) = abap_false.
      lo_files->add_xml(
        ii_xml      = li_xml
        is_metadata = li_obj->get_metadata( ) ).
    ENDIF.

    rs_files_and_item-files = lo_files->get_files( ).

    check_duplicates( rs_files_and_item-files ).

    rs_files_and_item-item-inactive = boolc( li_obj->is_active( ) = abap_false ).

    LOOP AT rs_files_and_item-files ASSIGNING <ls_file>.
      <ls_file>-sha1 = Lcl_abapgit_hash=>sha1_blob( <ls_file>-data ).
    ENDLOOP.

  ENDMETHOD.
  METHOD supported_list.

    DATA lt_objects            TYPE STANDARD TABLE OF ko100.
    DATA ls_item               TYPE Lif_abapgit_definitions=>ty_item.
    DATA ls_supported_obj_type TYPE ty_supported_types.
    DATA lt_types              TYPE Lif_abapgit_exit=>ty_object_types.
    DATA lv_type               LIKE LINE OF lt_types.
    DATA li_exit               TYPE REF TO Lif_abapgit_exit.

    FIELD-SYMBOLS <ls_object> LIKE LINE OF lt_objects.
    FIELD-SYMBOLS <ls_supported_obj_type> TYPE ty_supported_types.

    IF gv_supported_obj_types_loaded = abap_true.
      LOOP AT gt_supported_obj_types ASSIGNING <ls_supported_obj_type> WHERE supported = abap_true.
        INSERT <ls_supported_obj_type>-obj_type INTO TABLE rt_types.
      ENDLOOP.
      RETURN.
    ENDIF.

    " delete content because it might be filled already by method IS_TYPE_SUPPORTED
    CLEAR gt_supported_obj_types.

    CALL FUNCTION 'TR_OBJECT_TABLE'
      TABLES
        wt_object_text = lt_objects
      EXCEPTIONS
        OTHERS         = 1 ##FM_SUBRC_OK.

    LOOP AT lt_objects ASSIGNING <ls_object> WHERE pgmid = 'R3TR'.
      INSERT <ls_object>-object INTO TABLE lt_types.
    ENDLOOP.

    li_exit = Lcl_abapgit_exit=>get_instance( ).
    li_exit->change_supported_object_types( CHANGING ct_types = lt_types ).

    LOOP AT lt_types INTO lv_type.
      ls_item-obj_type = lv_type.

      ls_supported_obj_type-obj_type  = lv_type.
      ls_supported_obj_type-supported = is_supported( ls_item ).

      INSERT ls_supported_obj_type INTO TABLE gt_supported_obj_types.

      IF ls_supported_obj_type-supported = abap_true.
        INSERT ls_supported_obj_type-obj_type INTO TABLE rt_types.
      ENDIF.
    ENDLOOP.

    gv_supported_obj_types_loaded = abap_true.

  ENDMETHOD.
  METHOD update_package_tree.

    DATA: lt_packages TYPE Lif_abapgit_sap_package=>ty_devclass_tt,
          lv_package  LIKE LINE OF lt_packages,
          lv_tree     TYPE dirtree-tname.


    lt_packages = Lcl_abapgit_factory=>get_sap_package( iv_package )->list_subpackages( ).
    APPEND iv_package TO lt_packages.

    LOOP AT lt_packages INTO lv_package.
* update package tree for SE80
      lv_tree = 'EU_' && lv_package.
      CALL FUNCTION 'WB_TREE_ACTUALIZE'
        EXPORTING
          tree_name              = lv_tree
          without_crossreference = abap_true
          with_tcode_index       = abap_true.
    ENDLOOP.

  ENDMETHOD.
  METHOD deserialize_steps.

    FIELD-SYMBOLS <ls_step> LIKE LINE OF it_steps.

    LOOP AT it_steps ASSIGNING <ls_step>.
      deserialize_step(
        EXPORTING
          is_step      = <ls_step>
          ii_log       = ii_log
          iv_transport = iv_transport
        CHANGING
          ct_files     = ct_files ).
    ENDLOOP.

    SORT ct_files BY path ASCENDING filename ASCENDING.
    DELETE ADJACENT DUPLICATES FROM ct_files. " Just in case

  ENDMETHOD.
  METHOD get_extra_from_filename.

    IF iv_filename IS NOT INITIAL.
      FIND REGEX '\..*\.([\-a-z0-9_%]*)\.' IN iv_filename SUBMATCHES rv_extra.
      IF sy-subrc = 0.
        rv_extra = cl_http_utility=>unescape_url( rv_extra ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECTS implementation

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
        AND object     = lv_object.     "#EC CI_GENBUFF "#EC CI_NOORDER

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
  METHOD Lif_abapgit_object~serialize.

    mi_longtexts->serialize(
        iv_longtext_name = c_name
        iv_object_name = ms_item-obj_name
        iv_longtext_id = c_id
        io_i18n_params = mo_i18n_params
        ii_xml         = io_xml ).

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
         AND object = mv_doc_object.    "#EC CI_GENBUFF "#EC CI_NOORDER

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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
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
      AND ddlanguage <> mv_language
      ORDER BY langu.                                     "#EC CI_SUBRC

    SELECT DISTINCT ddlanguage AS langu APPENDING TABLE lt_i18n_langs
      FROM dd07v
      WHERE domname = lv_name
      AND ddlanguage IN lt_language_filter
      AND ddlanguage <> mv_language
      ORDER BY langu.                                     "#EC CI_SUBRC

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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_DOMA implementation

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
      AND masterlang = abap_true.                       "#EC CI_NOORDER

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
           AND object = mv_doc_object.  "#EC CI_GENBUFF "#EC CI_NOORDER

    rv_bool = boolc( lv_count > 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
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
  METHOD Lif_abapgit_object~serialize.

    Lcl_abapgit_factory=>get_longtexts( )->serialize(
      iv_object_name = mv_doc_object
      iv_longtext_id = c_id
      io_i18n_params = mo_i18n_params
      ii_xml         = io_xml ).

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
endclass. "ZCL_ABAPGIT_OBJECT_DSYS implementation

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
      AND ddlanguage <> mv_language
      ORDER BY langu.                                     "#EC CI_SUBRC

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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_DTEL implementation

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
      AND name = ms_item-obj_name
      ORDER BY application_id.

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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_FDT0 implementation

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

    SELECT DISTINCT pgmid object obj_name
      INTO CORRESPONDING FIELDS OF TABLE lt_e071_filter
      FROM e071
      WHERE trkorr IN it_r_trkorr
      ORDER BY pgmid object obj_name.
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

*>>>>>>> ZCL_ABAPGIT_OBJECT_INTF <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_intf=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_intf=======ccimp.
CLASS SHRITEFUH64VYIPO5IWUYB3KW4KSFY DEFINITION.
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
