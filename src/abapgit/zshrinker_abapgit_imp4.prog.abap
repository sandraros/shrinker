********************************************************************************
*
* LICENSE and NOTICE
*
* See include program ZSHRINKER_ABAPGIT_LICENSE
*
********************************************************************************
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
CLASS SHRITEFUH64VYIPO5I47WOOA56CASM DEFINITION FINAL.
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

CLASS SHRITEFUH64VYIPO5I47WOOA56CASM IMPLEMENTATION.

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

CLASS SHRITEFUH64VYIPO5I47WOOA56EASM DEFINITION FINAL.
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

CLASS SHRITEFUH64VYIPO5I47WOOA56EASM IMPLEMENTATION.

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
      lo_transport_zipper TYPE REF TO SHRITEFUH64VYIPO5I47WOOA56EASM,
      lx_except           TYPE REF TO cx_root,
      lv_folder           TYPE string,
      lv_text             TYPE string.

    TRY.

        lt_trkorr = SHRITEFUH64VYIPO5I47WOOA56CASM=>select_tr_requests( ).

        IF lt_trkorr[] IS NOT INITIAL.

          lv_folder = SHRITEFUH64VYIPO5I47WOOA56CASM=>f4_folder( ).

          IF lv_folder IS INITIAL.
* Empty folder
            Lcx_abapgit_exception=>raise( 'Empty destination folder' ).
          ENDIF.

* Instantiate transport zipper object that will also create the timestamped output folder
          CREATE OBJECT lo_transport_zipper TYPE SHRITEFUH64VYIPO5I47WOOA56EASM
            EXPORTING
              iv_folder = lv_folder.

* Generate the local zip files from the given list of transport requests
          lo_transport_zipper->generate_files(
            it_trkorr = lt_trkorr
            ig_logic  = Lcl_abapgit_ui_factory=>get_popups( )->popup_folder_logic( ) ).

* Open output folder if user asked it
          SHRITEFUH64VYIPO5I47WOOA56CASM=>open_folder_frontend( lo_transport_zipper->get_folder( ) ).

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
*CLASS SHRITEFUH64VYIPO5I47WOOA566ASM DEFINITION DEFERRED.
*CLASS zcl_abapgit_xml_output DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA566ASM.


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
*CLASS SHRITEFUH64VYIPO5I47WOOA57MASM DEFINITION DEFERRED.
*CLASS SHRITEFUH64VYIPO5I47WOOA57NASM DEFINITION DEFERRED.
*CLASS zcx_abapgit_exception DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA57MASM SHRITEFUH64VYIPO5I47WOOA57NASM.













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




*CLASS zcl_abapgit_gui_page_patch DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5XPASM.





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
*CLASS SHRITEFUH64VYIPO5I47WOOA5ZPASM DEFINITION DEFERRED.
*CLASS zcl_abapgit_objects_activation DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5ZPASM.



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

*CLASS zcl_abapgit_objects_program DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5ZWASM.


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

*CLASS zcl_abapgit_object_common_aff DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5ZXASM.

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
CLASS SHRITEFUH64VYIPO5I47WOOA52JASM DEFINITION
  INHERITING FROM cl_workflow_general_task_def
  CREATE PUBLIC
  FINAL.
  PUBLIC SECTION.

    CLASS-METHODS set_objid IMPORTING iv_objid TYPE hrobject-objid
                                      io_task  TYPE REF TO cl_workflow_general_task_def.

    CLASS-METHODS set_container_id IMPORTING iv_id   TYPE guid_32
                                             io_task TYPE REF TO cl_workflow_general_task_def. "#EC NEEDED
ENDCLASS.

CLASS SHRITEFUH64VYIPO5I47WOOA52JASM IMPLEMENTATION.

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


CLASS SHRITEFUH64VYIPO5I47WOOA52LASM DEFINITION
  CREATE PUBLIC
  FINAL.

  PUBLIC SECTION.

    INTERFACES SHRITEFUH64VYIPO5I47WOOA52IASM.

    CLASS-METHODS load IMPORTING iv_objid         TYPE hrobject-objid
                       RETURNING VALUE(ri_result) TYPE REF TO SHRITEFUH64VYIPO5I47WOOA52IASM
                       RAISING   Lcx_abapgit_exception.

    CLASS-METHODS create IMPORTING iv_objid         TYPE hrobject-objid
                                   is_task_data     TYPE SHRITEFUH64VYIPO5I47WOOA52IASM=>ty_task_data
                         RETURNING VALUE(ri_result) TYPE REF TO SHRITEFUH64VYIPO5I47WOOA52IASM
                         RAISING   Lcx_abapgit_exception.


  PRIVATE SECTION.
    CONSTANTS c_subty_task_description TYPE hr_s_subty VALUE '0120'.

    DATA mo_taskdef TYPE REF TO cl_workflow_task_ts.
    DATA ms_task TYPE SHRITEFUH64VYIPO5I47WOOA52IASM=>ty_task_data.

    DATA: mv_objid                      TYPE hrobjid.

    METHODS supply_instance RAISING Lcx_abapgit_exception.
    METHODS check_subrc_for IMPORTING iv_call TYPE clike OPTIONAL
                            RAISING   Lcx_abapgit_exception.

ENDCLASS.


CLASS SHRITEFUH64VYIPO5I47WOOA52LASM IMPLEMENTATION.

  METHOD load.

    DATA lo_taskdef TYPE REF TO SHRITEFUH64VYIPO5I47WOOA52LASM.

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

  METHOD SHRITEFUH64VYIPO5I47WOOA52IASM~clear_origin_data.

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

  METHOD SHRITEFUH64VYIPO5I47WOOA52IASM~get_definition.
    rs_result = me->ms_task.
  ENDMETHOD.

  METHOD SHRITEFUH64VYIPO5I47WOOA52IASM~get_container.
    ri_result = mo_taskdef->container.
  ENDMETHOD.

  METHOD SHRITEFUH64VYIPO5I47WOOA52IASM~get_user_container.

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
    DATA lo_task TYPE REF TO SHRITEFUH64VYIPO5I47WOOA52LASM.

    CREATE OBJECT lo_task TYPE SHRITEFUH64VYIPO5I47WOOA52LASM.
    lo_task->mv_objid = iv_objid.
    lo_task->ms_task = is_task_data.
    ri_result = lo_task.

  ENDMETHOD.


  METHOD SHRITEFUH64VYIPO5I47WOOA52IASM~import_container.

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

  METHOD SHRITEFUH64VYIPO5I47WOOA52IASM~create_task.

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

    SHRITEFUH64VYIPO5I47WOOA52JASM=>set_objid( iv_objid = mv_objid
                                           io_task  = mo_taskdef ).

    SHRITEFUH64VYIPO5I47WOOA52JASM=>set_container_id( iv_id    = |TS{ mv_objid }|
                                            io_task  = mo_taskdef ).

  ENDMETHOD.

  METHOD SHRITEFUH64VYIPO5I47WOOA52IASM~change_start_events.

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


  METHOD SHRITEFUH64VYIPO5I47WOOA52IASM~save.

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


  METHOD SHRITEFUH64VYIPO5I47WOOA52IASM~change_wi_text.

    mo_taskdef->change_wi_text(
      EXPORTING
        new_wi_text        = ms_task-wi_text
      EXCEPTIONS
        no_changes_allowed = 1
        OTHERS             = 2 ).                         "#EC SUBRC_OK

    check_subrc_for( `CHANGE_WI_TEXT` ).

  ENDMETHOD.


  METHOD SHRITEFUH64VYIPO5I47WOOA52IASM~change_method.

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


  METHOD SHRITEFUH64VYIPO5I47WOOA52IASM~change_terminating_events.

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


  METHOD SHRITEFUH64VYIPO5I47WOOA52IASM~change_text.

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

    DATA: ls_task       TYPE SHRITEFUH64VYIPO5I47WOOA52IASM=>ty_task_data,
          lv_xml_string TYPE xstring,
          li_task       TYPE REF TO SHRITEFUH64VYIPO5I47WOOA52IASM.

    io_xml->read( EXPORTING iv_name = 'PDTS'
      CHANGING cg_data = ls_task ).

    li_task = SHRITEFUH64VYIPO5I47WOOA52LASM=>create(
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

    DATA li_task TYPE REF TO SHRITEFUH64VYIPO5I47WOOA52IASM.

    li_task = SHRITEFUH64VYIPO5I47WOOA52LASM=>load( mv_objid ).
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
CLASS SHRITEFUH64VYIPO5I47WOOA5RAASM DEFINITION.

  PUBLIC SECTION.
    INTERFACES Lif_abapgit_environment.

    DATA mv_is_cloud TYPE abap_bool.

    METHODS set_cloud
      IMPORTING
        iv_is_cloud TYPE abap_bool.

ENDCLASS.

CLASS SHRITEFUH64VYIPO5I47WOOA5RAASM IMPLEMENTATION.

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
CLASS SHRITEFUH64VYIPO5I47WOOA5RCASM DEFINITION.

  PUBLIC SECTION.
    INTERFACES Lif_abapgit_persist_settings.

    DATA mo_settings TYPE REF TO Lcl_abapgit_settings.

    METHODS constructor.

ENDCLASS.

CLASS SHRITEFUH64VYIPO5I47WOOA5RCASM IMPLEMENTATION.

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
*CLASS SHRITEFUH64VYIPO5I47WOOA5UBASM DEFINITION DEFERRED.
*CLASS zcl_abapgit_data_deserializer DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5UBASM.




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
*CLASS SHRITEFUH64VYIPO5I47WOOA5UDASM DEFINITION DEFERRED.
*CLASS zcl_abapgit_data_serializer DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5UDASM.




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
CLASS SHRITEFUH64VYIPO5I47WOOA5UFASM DEFINITION.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_data_supporter.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5I47WOOA5UFASM IMPLEMENTATION.
  METHOD Lif_abapgit_data_supporter~is_object_supported.

    IF iv_type = Lif_abapgit_data_config=>c_data_type-tabu AND iv_name = 'T005'.
      rv_supported = abap_true.
    ENDIF.

  ENDMETHOD.

ENDCLASS.


*CLASS zcl_abapgit_data_supporter DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5UHASM.


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



*CLASS zcl_abapgit_dependencies DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5UMASM.


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
*CLASS SHRITEFUH64VYIPO5I47WOOA5UPASM DEFINITION DEFERRED.
*CLASS zcl_abapgit_dot_abapgit DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5UPASM.



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
*CLASS SHRITEFUH64VYIPO5I47WOOA5URASM DEFINITION DEFERRED.
*CLASS zcl_abapgit_environment DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5URASM.




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

*CLASS SHRITEFUH64VYIPO5I47WOOA5Q6ASM DEFINITION DEFERRED.
*CLASS zcl_abapgit_exit DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5Q6ASM.

" The class name SHRITEFUH64VYIPO5I47WOOA5Q6ASM is hardcoded in zcl_abapgit_exit=>is_running_in_test_context



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
        p_name         = |\\PROGRAM={ sy-repid }\\CLASS=SHRITEFUH64VYIPO5I47WOOA5Q6ASM|
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
*  LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5WNASM
*                SHRITEFUH64VYIPO5I47WOOA5WQASM.









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
*CLASS SHRITEFUH64VYIPO5I47WOOA5XBASM DEFINITION DEFERRED.
*CLASS zcl_abapgit_gui_page_data DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5I47WOOA5XBASM.




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

CLASS SHRITEFUH64VYIPO5I47WOOA5XIASM DEFINITION.
  PUBLIC SECTION.
    METHODS clear
      RETURNING
        VALUE(ro_stack) TYPE REF TO SHRITEFUH64VYIPO5I47WOOA5XIASM.

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

CLASS SHRITEFUH64VYIPO5I47WOOA5XIASM IMPLEMENTATION.
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

CLASS SHRITEFUH64VYIPO5I47WOOA5XKASM DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_object_filter.

    METHODS constructor
      IMPORTING
        it_filter TYPE Lif_abapgit_definitions=>ty_tadir_tt.

  PRIVATE SECTION.
    DATA mt_filter TYPE Lif_abapgit_definitions=>ty_tadir_tt.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5I47WOOA5XKASM IMPLEMENTATION.
  METHOD constructor.
    mt_filter = it_filter.
  ENDMETHOD.

  METHOD Lif_abapgit_object_filter~get_filter.
    rt_filter = mt_filter.
  ENDMETHOD.
ENDCLASS.

***************************************************

CLASS SHRITEFUH64VYIPO5I47WOOA5XMASM DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS get_information
      RETURNING
        VALUE(rt_features) TYPE SHRITEFUH64VYIPO5I47WOOA5XHASM
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
        VALUE(rs_data) TYPE SHRITEFUH64VYIPO5I47WOOA5XGASM-repo.

    CLASS-METHODS map_files_to_objects
      IMPORTING
        it_files                  TYPE SHRITEFUH64VYIPO5I47WOOA5XFASM
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
        ct_features      TYPE SHRITEFUH64VYIPO5I47WOOA5XHASM
      RAISING
        Lcx_abapgit_exception.

    CLASS-METHODS try_matching_transports
      IMPORTING
        ii_repo          TYPE REF TO Lif_abapgit_repo
        it_main_expanded TYPE Lif_abapgit_git_definitions=>ty_expanded_tt
      CHANGING
        ct_features      TYPE SHRITEFUH64VYIPO5I47WOOA5XHASM
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
        cs_feature       TYPE SHRITEFUH64VYIPO5I47WOOA5XGASM
      RAISING
        Lcx_abapgit_exception.

    CLASS-METHODS find_up_to_date
      IMPORTING
        iv_url      TYPE string
        it_branches TYPE Lif_abapgit_git_definitions=>ty_git_branch_list_tt
      CHANGING
        ct_features TYPE SHRITEFUH64VYIPO5I47WOOA5XHASM
      RAISING
        Lcx_abapgit_exception.

    CLASS-METHODS find_prs
      IMPORTING
        iv_url      TYPE string
      CHANGING
        ct_features TYPE SHRITEFUH64VYIPO5I47WOOA5XHASM
      RAISING
        Lcx_abapgit_exception.

    CLASS-METHODS add_local_status
      IMPORTING
        io_online   TYPE REF TO Lcl_abapgit_repo_online
      CHANGING
        ct_features TYPE SHRITEFUH64VYIPO5I47WOOA5XHASM
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
        VALUE(rt_files) TYPE SHRITEFUH64VYIPO5I47WOOA5XFASM.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5I47WOOA5XMASM IMPLEMENTATION.

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
    DATA lo_filter       TYPE REF TO SHRITEFUH64VYIPO5I47WOOA5XKASM.
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
    DATA lo_visit   TYPE REF TO SHRITEFUH64VYIPO5I47WOOA5XIASM.
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
    DATA lo_filter TYPE REF TO SHRITEFUH64VYIPO5I47WOOA5XKASM.
    DATA lt_filter TYPE Lif_abapgit_definitions=>ty_tadir_tt.

    FIELD-SYMBOLS <ls_branch>       LIKE LINE OF ct_features.
    FIELD-SYMBOLS <ls_local>        LIKE LINE OF lt_local.
    FIELD-SYMBOLS <ls_changed_file> TYPE SHRITEFUH64VYIPO5I47WOOA5XDASM.
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
    DATA lo_filter  TYPE REF TO SHRITEFUH64VYIPO5I47WOOA5XKASM.
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
      mt_features = SHRITEFUH64VYIPO5I47WOOA5XMASM=>get_information( ).
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
CLASS SHRITEFUH64VYIPO5I47WOOA5XRASM DEFINITION FINAL.
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
        VALUE(ro_me) TYPE REF TO SHRITEFUH64VYIPO5I47WOOA5XRASM.

ENDCLASS.

CLASS SHRITEFUH64VYIPO5I47WOOA5XRASM IMPLEMENTATION.

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

    DATA lo_tab_scheme TYPE REF TO SHRITEFUH64VYIPO5I47WOOA5XRASM.

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
