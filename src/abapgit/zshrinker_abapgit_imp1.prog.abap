********************************************************************************
*
* LICENSE and NOTICE
*
* See include program ZSHRINKER_DEMO_ABAPGIT_LICENSE
*
********************************************************************************
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

    IF gi_http_agent IS BOUND.
      ri_http_agent = gi_http_agent.
    ELSE.
      ri_http_agent = Lcl_abapgit_http_agent=>create( ).
    ENDIF.

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
  METHOD get_sap_report.

    IF gi_sap_report IS NOT BOUND.
      CREATE OBJECT gi_sap_report TYPE Lcl_abapgit_sap_report.
    ENDIF.

    ri_report = gi_sap_report.

  ENDMETHOD.
  METHOD get_stage_logic.

    IF gi_stage_logic IS INITIAL.
      CREATE OBJECT gi_stage_logic
        TYPE Lcl_abapgit_stage_logic.
    ENDIF.

    ri_logic = gi_stage_logic.

  ENDMETHOD.
  METHOD get_tadir.

    IF gi_tadir IS INITIAL.
      CREATE OBJECT gi_tadir TYPE Lcl_abapgit_tadir.
    ENDIF.

    ri_tadir = gi_tadir.

  ENDMETHOD.
  METHOD get_function_module.

    IF gi_function_module IS INITIAL.
      CREATE OBJECT gi_function_module TYPE Lcl_abapgit_function_module.
    ENDIF.

    ri_function_module = gi_function_module.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_FACTORY implementation

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
  METHOD set_sap_report.
    Lcl_abapgit_factory=>gi_sap_report = ii_report.
  ENDMETHOD.
  METHOD set_stage_logic.

    Lcl_abapgit_factory=>gi_stage_logic = ii_logic.

  ENDMETHOD.
  METHOD set_tadir.
    Lcl_abapgit_factory=>gi_tadir = ii_tadir.
  ENDMETHOD.
  METHOD set_function_module.

    Lcl_abapgit_factory=>gi_function_module = ii_function_module.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_INJECTOR implementation

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
  METHOD run.

    " Migrate STDTEXT to TABLE
    Lcl_abapgit_persist_migrate=>run( ).

    " Create ZIF_APACK_MANIFEST interface
    Lcl_abapgit_apack_migration=>run( ).

    " Migrate checksums from repo metadata to separate DB object
    Lcl_abapgit_repo_cs_migration=>run( ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_MIGRATIONS implementation

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
*CLASS SHRIS5ZPAUXVKEPN5HWETLLASV6BTU DEFINITION DEFERRED.
*CLASS SHRIS5ZPAUXVKEPN5HWETLLASV7BTU DEFINITION DEFERRED.
*CLASS zcx_abapgit_exception DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLASV6BTU SHRIS5ZPAUXVKEPN5HWETLLASV7BTU.













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

    DATA: lt_met_status TYPE ty_dependency_statuses.

    lt_met_status = get_dependencies_met_status( it_dependencies ).

    show_dependencies_popup( lt_met_status ).

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
            OR seometarel~refclsname = Lif_abapgit_apack_definitions=>c_apack_interface_sap ).

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
          ls_manifest_implementation TYPE ty_s_manifest_declaration.

    IF mv_is_cached IS INITIAL AND mv_package_name IS NOT INITIAL.
      SELECT SINGLE seometarel~clsname tadir~devclass FROM seometarel "#EC CI_NOORDER
         INNER JOIN tadir ON seometarel~clsname = tadir~obj_name "#EC CI_BUFFJOIN
         INTO ls_manifest_implementation
         WHERE tadir~pgmid = 'R3TR' AND
               tadir~object = 'CLAS' AND
               seometarel~version = '1' AND
               seometarel~refclsname = Lif_abapgit_apack_definitions=>c_apack_interface_cust AND
               tadir~devclass = mv_package_name.
      IF ls_manifest_implementation IS INITIAL.
        SELECT SINGLE seometarel~clsname tadir~devclass FROM seometarel "#EC CI_NOORDER
           INNER JOIN tadir ON seometarel~clsname = tadir~obj_name "#EC CI_BUFFJOIN
           INTO ls_manifest_implementation
           WHERE tadir~pgmid = 'R3TR' AND
                 tadir~object = 'CLAS' AND
                 seometarel~version = '1' AND
                 seometarel~refclsname = Lif_abapgit_apack_definitions=>c_apack_interface_sap AND
                 tadir~devclass = mv_package_name.
      ENDIF.
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
endclass. "ZCL_ABAPGIT_BACKGROUND implementation

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

*>>>>>>> ZCL_ABAPGIT_TRANSPORT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_transport=========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_transport=========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_TRANSPORT implementation.
*"* method's implementations
*include methods.
  METHOD add_all_objects_to_trans_req.

    DATA:
      ls_request      TYPE trwbo_request_header,
      lt_e071         TYPE tr_objects,
      lv_text         TYPE string,
      lv_answer       TYPE c LENGTH 1,
      lv_lock_objects TYPE trparflag,
      lt_log          TYPE sprot_u_tab.

    lv_answer = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
                    iv_titlebar              = `Lock objects?`
                    iv_text_question         = `Shall all objects be locked in the transport request?`
                    iv_display_cancel_button = abap_true ).

    CASE lv_answer.
      WHEN '1'.
        lv_lock_objects = abap_true.
      WHEN '2'.
        lv_lock_objects = abap_false.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    lt_e071 = collect_all_objects( iv_key ).

    " We used TR_REQUEST_CHOICE before, but it issues its error log with
    " write lists which are not compatible with abapGit.
    " There we user TRINT_REQUEST_CHOICE which returns the error log
    " and display the log ourselve.
    CALL FUNCTION 'TRINT_REQUEST_CHOICE'
      EXPORTING
        iv_request_types     = 'FTCOK'
        iv_lock_objects      = lv_lock_objects
        iv_with_error_log    = abap_false
      IMPORTING
        es_request           = ls_request
        et_log               = lt_log
      TABLES
        it_e071              = lt_e071
      EXCEPTIONS
        invalid_request      = 1
        invalid_request_type = 2
        user_not_owner       = 3
        no_objects_appended  = 4
        enqueue_error        = 5
        cancelled_by_user    = 6
        recursive_call       = 7
        OTHERS               = 8.
    IF sy-subrc = 0.
      lv_text = |Objects successfully added to { ls_request-trkorr }|.
      MESSAGE lv_text TYPE 'S'.
      RETURN.
    ENDIF.

    IF lines( lt_log ) > 0.
      show_log(
          it_log   = lt_log
          iv_title = `Error log` ).
    ELSE.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD collect_all_objects.

    DATA:
      lt_objects     TYPE scts_tadir,
      lt_objects_all LIKE lt_objects,
      ls_e071        LIKE LINE OF rt_objects,
      lo_repo        TYPE REF TO Lcl_abapgit_repo,
      lv_package     TYPE Lif_abapgit_persistence=>ty_repo-package,
      lt_packages    TYPE Lif_abapgit_sap_package=>ty_devclass_tt.

    FIELD-SYMBOLS:
      <lv_package> TYPE devclass,
      <ls_object>  TYPE tadir.

    lo_repo    ?= Lcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
    lv_package  = lo_repo->get_package( ).
    lt_packages = Lcl_abapgit_factory=>get_sap_package( lv_package )->list_subpackages( ).
    INSERT lv_package INTO TABLE lt_packages.

    LOOP AT lt_packages ASSIGNING <lv_package>.

      CLEAR: lt_objects.

      CALL FUNCTION 'TRINT_SELECT_OBJECTS'
        EXPORTING
          iv_devclass       = <lv_package>
          iv_via_selscreen  = abap_false
        IMPORTING
          et_objects_tadir  = lt_objects
        EXCEPTIONS
          cancelled_by_user = 1
          invalid_input     = 2
          OTHERS            = 3.

      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      INSERT LINES OF lt_objects INTO TABLE lt_objects_all.

    ENDLOOP.

    IF lines( lt_objects_all ) = 0.
      Lcx_abapgit_exception=>raise( |No objects found| ).
    ENDIF.

    LOOP AT lt_objects_all ASSIGNING <ls_object>.

      CLEAR: ls_e071.

      MOVE-CORRESPONDING <ls_object> TO ls_e071.
      INSERT ls_e071 INTO TABLE rt_objects.

    ENDLOOP.

  ENDMETHOD.
  METHOD find_top_package.
* assumption: all objects in transport share a common super package

    DATA: lt_obj   TYPE Lif_abapgit_sap_package=>ty_devclass_tt,
          lt_super TYPE Lif_abapgit_sap_package=>ty_devclass_tt,
          lv_super LIKE LINE OF lt_super,
          lv_index TYPE i.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF it_tadir.


    READ TABLE it_tadir INDEX 1 ASSIGNING <ls_tadir>.
    ASSERT sy-subrc = 0.
    lt_super = Lcl_abapgit_factory=>get_sap_package( <ls_tadir>-devclass )->list_superpackages( ).

    LOOP AT it_tadir ASSIGNING <ls_tadir>.
      lt_obj = Lcl_abapgit_factory=>get_sap_package( <ls_tadir>-devclass )->list_superpackages( ).

* filter out possibilities from lt_super
      LOOP AT lt_super INTO lv_super.
        lv_index = sy-tabix.
        READ TABLE lt_obj FROM lv_super TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          DELETE lt_super INDEX lv_index.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    READ TABLE lt_super INDEX lines( lt_super ) INTO rv_package.
  ENDMETHOD.
  METHOD read_requests.
    DATA lt_requests LIKE rt_requests.
    FIELD-SYMBOLS <ls_trkorr> LIKE LINE OF it_trkorr.

    LOOP AT it_trkorr ASSIGNING <ls_trkorr>.
      CALL FUNCTION 'TR_READ_REQUEST_WITH_TASKS'
        EXPORTING
          iv_trkorr     = <ls_trkorr>-trkorr
        IMPORTING
          et_requests   = lt_requests
        EXCEPTIONS
          invalid_input = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      APPEND LINES OF lt_requests TO rt_requests.
    ENDLOOP.
  ENDMETHOD.
  METHOD resolve.
    DATA: lv_object    TYPE tadir-object,
          lv_obj_name  TYPE tadir-obj_name,
          ls_tadir     TYPE Lif_abapgit_definitions=>ty_tadir,
          lv_result    TYPE trpari-s_checked,
          ls_tadir_sap TYPE tadir.

    FIELD-SYMBOLS: <ls_request> LIKE LINE OF it_requests,
                   <ls_object>  LIKE LINE OF <ls_request>-objects.


    LOOP AT it_requests ASSIGNING <ls_request>.
      LOOP AT <ls_request>-objects ASSIGNING <ls_object>.
        " VARX, see https://github.com/abapGit/abapGit/issues/3107
        IF <ls_object>-pgmid = 'LIMU' AND <ls_object>-object <> 'VARX'.
          CALL FUNCTION 'TR_CHECK_TYPE'
            EXPORTING
              wi_e071   = <ls_object>
            IMPORTING
              we_tadir  = ls_tadir_sap
              pe_result = lv_result.
          IF lv_result NA 'TL' OR ls_tadir_sap IS INITIAL.
            Lcx_abapgit_exception=>raise( 'error from TR_CHECK_TYPE' ).
          ENDIF.
          lv_object   = ls_tadir_sap-object.
          lv_obj_name = ls_tadir_sap-obj_name.
        ELSE.
          lv_object   = <ls_object>-object.
          lv_obj_name = <ls_object>-obj_name.
        ENDIF.

        ls_tadir = Lcl_abapgit_factory=>get_tadir( )->read_single(
          iv_object   = lv_object
          iv_obj_name = lv_obj_name ).

        IF ls_tadir-delflag IS INITIAL OR iv_deleted_objects = abap_true.
          APPEND ls_tadir TO rt_tadir.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    SORT rt_tadir BY object ASCENDING obj_name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rt_tadir COMPARING object obj_name.
    DELETE rt_tadir WHERE table_line IS INITIAL.
  ENDMETHOD.
  METHOD show_log.

    DATA: li_log     TYPE REF TO Lif_abapgit_log,
          lv_message TYPE string.
    FIELD-SYMBOLS: <ls_log> TYPE sprot_u.

    CREATE OBJECT li_log TYPE Lcl_abapgit_log
      EXPORTING
        iv_title = iv_title.

    LOOP AT it_log ASSIGNING <ls_log>.

      MESSAGE ID <ls_log>-ag TYPE <ls_log>-severity NUMBER <ls_log>-msgnr
       WITH <ls_log>-var1 <ls_log>-var2 <ls_log>-var3 <ls_log>-var4
       INTO lv_message.

      li_log->add(
          iv_msg  = lv_message
          iv_type = <ls_log>-severity ).

    ENDLOOP.

    Lcl_abapgit_log_viewer=>show_log( li_log ).

  ENDMETHOD.
  METHOD to_tadir.
    DATA: lt_requests TYPE trwbo_requests.


    IF lines( it_transport_headers ) = 0.
      RETURN.
    ENDIF.

    lt_requests = read_requests( it_transport_headers ).
    rt_tadir = resolve(
      it_requests        = lt_requests
      iv_deleted_objects = iv_deleted_objects ).

  ENDMETHOD.
  METHOD zip.

    DATA: lt_requests       TYPE trwbo_requests,
          lt_tadir          TYPE Lif_abapgit_definitions=>ty_tadir_tt,
          lv_package        TYPE devclass,
          lo_dot_abapgit    TYPE REF TO Lcl_abapgit_dot_abapgit,
          ls_local_settings TYPE Lif_abapgit_persistence=>ty_repo-local_settings,
          lt_trkorr         TYPE trwbo_request_headers.


    IF is_trkorr IS SUPPLIED.
      APPEND is_trkorr TO lt_trkorr.
    ELSE.
      lt_trkorr = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_select_transports( ).
    ENDIF.

    IF lines( lt_trkorr ) = 0.
      RETURN.
    ENDIF.

    lt_requests = read_requests( lt_trkorr ).
    lt_tadir = resolve( lt_requests ).
    IF lines( lt_tadir ) = 0.
      Lcx_abapgit_exception=>raise( 'empty transport' ).
    ENDIF.

    lv_package = find_top_package( lt_tadir ).
    IF lv_package IS INITIAL.
      Lcx_abapgit_exception=>raise( 'error finding super package' ).
    ENDIF.

    lo_dot_abapgit = Lcl_abapgit_dot_abapgit=>build_default( ).
    IF iv_logic IS SUPPLIED AND iv_logic IS NOT INITIAL.
      lo_dot_abapgit->set_folder_logic( iv_logic ).
    ELSE.
      lo_dot_abapgit->set_folder_logic( Lcl_abapgit_ui_factory=>get_popups( )->popup_folder_logic( ) ).
    ENDIF.

    rv_xstr = Lcl_abapgit_zip=>export(
      iv_package        = lv_package
      io_dot_abapgit    = lo_dot_abapgit
      is_local_settings = ls_local_settings
      it_filter         = lt_tadir
      iv_show_log       = iv_show_log_popup ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_TRANSPORT implementation

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

*>>>>>>> ZCL_ABAPGIT_TRANSPORT_MASS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_transport_mass====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_transport_mass====ccimp.
CLASS SHRIS5ZPAUXVKEPN5HWETLLASWQBTU DEFINITION FINAL.
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

CLASS SHRIS5ZPAUXVKEPN5HWETLLASWQBTU IMPLEMENTATION.

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

CLASS SHRIS5ZPAUXVKEPN5HWETLLASWSBTU DEFINITION FINAL.
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

CLASS SHRIS5ZPAUXVKEPN5HWETLLASWSBTU IMPLEMENTATION.

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
      lo_transport_zipper TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLASWSBTU,
      lx_except           TYPE REF TO cx_root,
      lv_folder           TYPE string,
      lv_text             TYPE string.

    TRY.

        lt_trkorr = SHRIS5ZPAUXVKEPN5HWETLLASWQBTU=>select_tr_requests( ).

        IF lt_trkorr[] IS NOT INITIAL.

          lv_folder = SHRIS5ZPAUXVKEPN5HWETLLASWQBTU=>f4_folder( ).

          IF lv_folder IS INITIAL.
* Empty folder
            Lcx_abapgit_exception=>raise( 'Empty destination folder' ).
          ENDIF.

* Instantiate transport zipper object that will also create the timestamped output folder
          CREATE OBJECT lo_transport_zipper TYPE SHRIS5ZPAUXVKEPN5HWETLLASWSBTU
            EXPORTING
              iv_folder = lv_folder.

* Generate the local zip files from the given list of transport requests
          lo_transport_zipper->generate_files(
            it_trkorr = lt_trkorr
            ig_logic  = Lcl_abapgit_ui_factory=>get_popups( )->popup_folder_logic( ) ).

* Open output folder if user asked it
          SHRIS5ZPAUXVKEPN5HWETLLASWQBTU=>open_folder_frontend( lo_transport_zipper->get_folder( ) ).

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
*CLASS SHRIS5ZPAUXVKEPN5HWETLLASWYBTU DEFINITION DEFERRED.
*CLASS zcl_abapgit_data_deserializer DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLASWYBTU.




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
      SELECT * FROM (iv_name) APPENDING TABLE <lg_tab> WHERE (lv_where).
    ENDLOOP.
    IF lines( it_where ) = 0.
      SELECT * FROM (iv_name) INTO TABLE <lg_tab>.
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
  METHOD is_table_allowed_to_edit.

    " Is the object supported (by default or based on exit)?
    rv_allowed_to_edit = Lcl_abapgit_data_factory=>get_supporter( )->is_object_supported(
      iv_type = is_result-type
      iv_name = is_result-name ).

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
endclass. "ZCL_ABAPGIT_DATA_DESERIALIZER implementation

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
  METHOD get_config.
    CREATE OBJECT ri_config TYPE Lcl_abapgit_data_config.
  ENDMETHOD.
  METHOD get_supporter.

    IF gi_supporter IS INITIAL.
      CREATE OBJECT gi_supporter TYPE Lcl_abapgit_data_supporter.
    ENDIF.

    ri_supporter = gi_supporter.

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
  METHOD set_deserializer.
    Lcl_abapgit_data_factory=>gi_deserializer = ii_deserializer.
  ENDMETHOD.
  METHOD set_serializer.
    Lcl_abapgit_data_factory=>gi_serializer = ii_serializer.
  ENDMETHOD.
  METHOD set_supporter.
    Lcl_abapgit_data_factory=>gi_supporter = ii_supporter.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_DATA_INJECTOR implementation

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
*CLASS SHRIS5ZPAUXVKEPN5HWETLLASW2BTU DEFINITION DEFERRED.
*CLASS zcl_abapgit_data_serializer DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLASW2BTU.




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
          SELECT * FROM (iv_name) APPENDING TABLE <lg_tab> WHERE (lv_where).
        ENDLOOP.
        IF lines( it_where ) = 0.
          SELECT * FROM (iv_name) INTO TABLE <lg_tab>.
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
  METHOD does_table_exist.

    " This is slow but ensures that the table actually exists and is not just buffered by RTTI
    " If we just rely on RTTI, uninstalling and reinstalling a table in the same session will lead to dumps
    TRY.
        build_table_itab( iv_name ).
        rv_exists = abap_true.
      CATCH Lcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

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
endclass. "ZCL_ABAPGIT_DATA_UTILS implementation

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
CLASS SHRIS5ZPAUXVKEPN5HWETLLASW6BTU DEFINITION.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_data_supporter.
ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLASW6BTU IMPLEMENTATION.
  METHOD Lif_abapgit_data_supporter~is_object_supported.

    IF iv_type = Lif_abapgit_data_config=>c_data_type-tabu AND iv_name = 'T005'.
      rv_supported = abap_true.
    ENDIF.

  ENDMETHOD.

ENDCLASS.


*CLASS zcl_abapgit_data_supporter DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLASXABTU.


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
        AND dd02l~contflag = 'C'. "Only customizing tables

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

    TRY.
        CREATE OBJECT gi_exit TYPE (lv_class_name).
      CATCH cx_sy_create_object_error ##NO_HANDLER.
    ENDTRY.

    CREATE OBJECT gi_global_exit TYPE Lcl_abapgit_exit. " this class

    ri_exit = gi_global_exit.

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
  METHOD Lif_abapgit_exit~change_rfc_server_group.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_rfc_server_group( CHANGING cv_group = cv_group ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_EXIT implementation

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
*CLASS SHRIS5ZPAUXVKEPN5HWETLLASXFBTU DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_branch_list DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLASXFBTU.




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
    FIELD-SYMBOLS <ls_branch> LIKE LINE OF mt_branches.

    LOOP AT mt_branches ASSIGNING <ls_branch>.
      IF <ls_branch>-type = Lif_abapgit_git_definitions=>c_git_branch_type-branch.
        APPEND <ls_branch> TO rt_branches.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_display_name.
    rv_display_name = iv_branch_name.

    IF rv_display_name CP Lif_abapgit_git_definitions=>c_git_branch-heads.
      REPLACE FIRST OCCURRENCE OF Lif_abapgit_git_definitions=>c_git_branch-heads_prefix IN rv_display_name WITH ''.
    ELSEIF rv_display_name CP Lif_abapgit_git_definitions=>c_git_branch-tags.
      rv_display_name = Lcl_abapgit_git_tag=>remove_tag_prefix( Lcl_abapgit_git_tag=>remove_peel( rv_display_name ) ).
    ENDIF.

  ENDMETHOD.
  METHOD get_head_symref.
    rv_head_symref = mv_head_symref.
  ENDMETHOD.
  METHOD get_tags_only.
    FIELD-SYMBOLS <ls_branch> LIKE LINE OF mt_branches.

    LOOP AT mt_branches ASSIGNING <ls_branch>
        WHERE type = Lif_abapgit_git_definitions=>c_git_branch_type-lightweight_tag
        OR type = Lif_abapgit_git_definitions=>c_git_branch_type-annotated_tag.
      APPEND <ls_branch> TO rt_tags.
    ENDLOOP.

  ENDMETHOD.
  METHOD get_type.

    FIELD-SYMBOLS: <lv_result> TYPE LINE OF string_table.

    rv_type = Lif_abapgit_git_definitions=>c_git_branch_type-other.

    IF iv_branch_name CP Lif_abapgit_git_definitions=>c_git_branch-heads OR
       iv_branch_name = Lif_abapgit_git_definitions=>c_head_name.
      rv_type = Lif_abapgit_git_definitions=>c_git_branch_type-branch.

    ELSEIF iv_branch_name CP Lif_abapgit_git_definitions=>c_git_branch-tags.

      READ TABLE it_result ASSIGNING <lv_result>
                           INDEX iv_current_row_index + 1.
      IF sy-subrc = 0 AND <lv_result> CP '*' && Lcl_abapgit_git_tag=>add_peel( iv_branch_name ).
        rv_type = Lif_abapgit_git_definitions=>c_git_branch_type-annotated_tag.
      ELSE.
        rv_type = Lif_abapgit_git_definitions=>c_git_branch_type-lightweight_tag.
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD normalize_branch_name.

    rv_name = iv_branch_name. " Force convert to string
    REPLACE ALL OCCURRENCES OF ` ` IN rv_name WITH '-'. " Disallow space in branch name

  ENDMETHOD.
  METHOD parse_branch_list.

    DATA: lt_result            TYPE TABLE OF string,
          lv_hash              TYPE Lif_abapgit_git_definitions=>ty_sha1,
          lv_name              TYPE string,
          lv_head_params       TYPE string,
          lv_char              TYPE c,
          lv_data              LIKE LINE OF lt_result,
          lv_current_row_index TYPE syst-tabix.

    FIELD-SYMBOLS: <ls_branch> LIKE LINE OF et_list.

    CLEAR: et_list, ev_head_symref.

    lv_data = skip_first_pkt( iv_data ).
    SPLIT lv_data AT cl_abap_char_utilities=>newline INTO TABLE lt_result.

    LOOP AT lt_result INTO lv_data.
      lv_current_row_index = sy-tabix.

      IF sy-tabix = 1 AND strlen( lv_data ) > 12 AND lv_data(4) = '0000' AND lv_data+8(3) = 'ERR'.
        lv_name = lv_data+8.
        Lcx_abapgit_exception=>raise( lv_name ).
      ELSEIF sy-tabix = 1 AND strlen( lv_data ) > 49.
        lv_hash = lv_data+8.
        lv_name = lv_data+49.
        lv_char = Lcl_abapgit_git_utils=>get_null( ).

        SPLIT lv_name AT lv_char INTO lv_name lv_head_params.
        ev_head_symref = parse_head_params( lv_head_params ).
        IF ev_head_symref IS INITIAL AND lv_name CS 'refs/heads/'.
          ev_head_symref = lv_name.
        ENDIF.
      ELSEIF sy-tabix > 1 AND strlen( lv_data ) > 45.
        lv_hash = lv_data+4.
        lv_name = lv_data+45.
      ELSEIF sy-tabix = 1 AND strlen( lv_data ) = 8 AND lv_data(8) = '00000000'.
        Lcx_abapgit_exception=>raise( 'No branches, create branch manually by adding file' ).
      ELSE.
        CONTINUE.
      ENDIF.

      ASSERT lv_name IS NOT INITIAL.

      APPEND INITIAL LINE TO et_list ASSIGNING <ls_branch>.
      <ls_branch>-sha1         = lv_hash.
      <ls_branch>-name         = lv_name.
      <ls_branch>-display_name = get_display_name( lv_name ).
      <ls_branch>-type         = get_type( iv_branch_name       = lv_name
                                           it_result            = lt_result
                                           iv_current_row_index = lv_current_row_index ).
      IF <ls_branch>-name = Lif_abapgit_git_definitions=>c_head_name OR <ls_branch>-name = ev_head_symref.
        <ls_branch>-is_head = abap_true.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD parse_head_params.

    DATA: ls_match    TYPE match_result,
          ls_submatch LIKE LINE OF ls_match-submatches.

    FIND FIRST OCCURRENCE OF REGEX '\ssymref=HEAD:([^\s]+)' IN iv_data RESULTS ls_match.
    READ TABLE ls_match-submatches INTO ls_submatch INDEX 1.
    IF sy-subrc IS INITIAL.
      rv_head_symref = iv_data+ls_submatch-offset(ls_submatch-length).
    ENDIF.

  ENDMETHOD.
  METHOD skip_first_pkt.

    DATA: lv_hex    TYPE x LENGTH 1,
          lv_length TYPE i.

* channel
    ASSERT iv_data(2) = '00'.

    lv_hex = to_upper( iv_data+2(2) ).
    lv_length = lv_hex.

    rv_data = iv_data+lv_length.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GIT_BRANCH_LIST implementation

*>>>>>>> ZCL_ABAPGIT_GIT_COMMIT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_git_commit========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_git_commit========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_git_commit========ccau.
*CLASS SHRIS5ZPAUXVKEPN5HWETLLASXHBTU DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_commit DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLASXHBTU.





*CLASS SHRIS5ZPAUXVKEPN5HWETLLASXJBTU DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_commit DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLASXJBTU.



*CLASS SHRIS5ZPAUXVKEPN5HWETLLASXLBTU DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_commit DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLASXLBTU.



class LCL_ABAPGIT_GIT_COMMIT implementation.
*"* method's implementations
*include methods.
  METHOD clear_missing_parents.

    "Part of #4719 to handle cut commit sequences, todo

    FIELD-SYMBOLS: <ls_commit> TYPE Lif_abapgit_git_definitions=>ty_commit.

    LOOP AT ct_commits ASSIGNING <ls_commit>.

      IF is_missing( it_commits = ct_commits
                     iv_sha1  = <ls_commit>-parent1 ) = abap_true.
        CLEAR <ls_commit>-parent1.
      ENDIF.

      IF is_missing( it_commits = ct_commits
                     iv_sha1  = <ls_commit>-parent2 ) = abap_true.
        CLEAR <ls_commit>-parent2.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD get_1st_child_commit.

    DATA: lt_1stchild_commits TYPE Lif_abapgit_git_definitions=>ty_commit_tt,
          ls_parent           LIKE LINE OF it_commit_sha1s,
          lt_commit_sha1s     LIKE it_commit_sha1s.

    FIELD-SYMBOLS: <ls_child_commit> TYPE Lif_abapgit_git_definitions=>ty_commit.

    CLEAR: es_1st_commit.

* get all reachable next commits
    lt_commit_sha1s = it_commit_sha1s.
    LOOP AT ct_commits ASSIGNING <ls_child_commit> WHERE parent1 IN lt_commit_sha1s
                                                      OR parent2 IN lt_commit_sha1s.
      INSERT <ls_child_commit> INTO TABLE lt_1stchild_commits.
    ENDLOOP.

* return oldest one
    SORT lt_1stchild_commits BY time ASCENDING.
    READ TABLE lt_1stchild_commits INTO es_1st_commit INDEX 1.

* remove from available commits
    DELETE ct_commits WHERE sha1 = es_1st_commit-sha1.

* set relevant parent commit sha1s
    IF lines( lt_1stchild_commits ) = 1.
      CLEAR et_commit_sha1s.
    ELSE.
      et_commit_sha1s = it_commit_sha1s.
    ENDIF.

    ls_parent-sign   = 'I'.
    ls_parent-option = 'EQ'.
    ls_parent-low    = es_1st_commit-sha1.
    INSERT ls_parent INTO TABLE et_commit_sha1s.

  ENDMETHOD.
  METHOD get_by_branch.

    DATA: li_progress TYPE REF TO Lif_abapgit_progress,
          lt_objects  TYPE Lif_abapgit_definitions=>ty_objects_tt.

    li_progress = Lcl_abapgit_progress=>get_instance( 1 ).

    li_progress->show(
      iv_current = 1
      iv_text    = |Get git commits { iv_repo_url }| ).

    Lcl_abapgit_git_transport=>upload_pack_by_branch(
      EXPORTING
        iv_url          = iv_repo_url
        iv_branch_name  = iv_branch_name
        iv_deepen_level = iv_deepen_level
      IMPORTING
        ev_branch       = rs_pull_result-commit
        et_objects      = lt_objects ).

    DELETE lt_objects WHERE type <> Lif_abapgit_git_definitions=>c_type-commit.

    rs_pull_result-commits = parse_commits( lt_objects ).

    IF iv_sorted = abap_true.
      sort_commits( CHANGING ct_commits = rs_pull_result-commits ).
    ENDIF.

  ENDMETHOD.
  METHOD get_by_commit.

    DATA: li_progress TYPE REF TO Lif_abapgit_progress,
          lt_objects  TYPE Lif_abapgit_definitions=>ty_objects_tt.

    li_progress = Lcl_abapgit_progress=>get_instance( 1 ).

    li_progress->show(
      iv_current = 1
      iv_text    = |Get git commits { iv_repo_url }| ).

    Lcl_abapgit_git_transport=>upload_pack_by_commit(
      EXPORTING
        iv_url          = iv_repo_url
        iv_deepen_level = iv_deepen_level
        iv_hash         = iv_commit_hash
      IMPORTING
        et_objects      = lt_objects ).

    DELETE lt_objects WHERE type <> Lif_abapgit_git_definitions=>c_type-commit.

    rt_commits = parse_commits( lt_objects ).
    sort_commits( CHANGING ct_commits = rt_commits ).

  ENDMETHOD.
  METHOD is_missing.

    IF iv_sha1 IS NOT INITIAL.

      READ TABLE it_commits
        TRANSPORTING NO FIELDS
        WITH KEY sha1 = iv_sha1.
      rv_result = boolc( sy-subrc <> 0 ).

    ENDIF.

  ENDMETHOD.
  METHOD parse_commits.

    DATA: ls_commit TYPE Lif_abapgit_git_definitions=>ty_commit,
          lt_body   TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
          ls_raw    TYPE Lcl_abapgit_git_pack=>ty_commit.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF it_objects,
                   <lv_body>   TYPE string.


    LOOP AT it_objects ASSIGNING <ls_object> USING KEY type
        WHERE type = Lif_abapgit_git_definitions=>c_type-commit.
      ls_raw = Lcl_abapgit_git_pack=>decode_commit( <ls_object>-data ).

      CLEAR ls_commit.
      ls_commit-sha1 = <ls_object>-sha1.
      ls_commit-parent1 = ls_raw-parent.
      ls_commit-parent2 = ls_raw-parent2.

      SPLIT ls_raw-body AT cl_abap_char_utilities=>newline INTO TABLE lt_body.

      READ TABLE lt_body WITH KEY table_line = ' -----END PGP SIGNATURE-----' TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        DELETE lt_body TO sy-tabix.
        DELETE lt_body TO 2.
      ENDIF.

      READ TABLE lt_body INDEX 1 INTO ls_commit-message.  "#EC CI_SUBRC
      " The second line is always empty. Therefore we omit it.
      LOOP AT lt_body ASSIGNING <lv_body>
                      FROM 3.
        INSERT <lv_body> INTO TABLE ls_commit-body.
      ENDLOOP.

      extract_author_data(
        EXPORTING
          iv_author = ls_raw-author
        IMPORTING
          ev_author = ls_commit-author
          ev_email  = ls_commit-email
          ev_time   = ls_commit-time ).

      APPEND ls_commit TO rt_commits.

    ENDLOOP.

  ENDMETHOD.
  METHOD reverse_sort_order.

    DATA: lt_commits           TYPE Lif_abapgit_git_definitions=>ty_commit_tt.
    FIELD-SYMBOLS: <ls_commit> TYPE Lif_abapgit_git_definitions=>ty_commit.

    LOOP AT ct_commits ASSIGNING <ls_commit>.
      INSERT <ls_commit> INTO lt_commits INDEX 1.
    ENDLOOP.
    ct_commits = lt_commits.
    FREE lt_commits.

  ENDMETHOD.
  METHOD sort_commits.

    DATA: lt_sorted_commits TYPE Lif_abapgit_git_definitions=>ty_commit_tt,
          ls_next_commit    TYPE Lif_abapgit_git_definitions=>ty_commit,
          lt_parents        TYPE ty_sha1_range,
          ls_parent         LIKE LINE OF lt_parents.

    FIELD-SYMBOLS: <ls_initial_commit> TYPE Lif_abapgit_git_definitions=>ty_commit.

    " find initial commit
    READ TABLE ct_commits ASSIGNING <ls_initial_commit> WITH KEY parent1 = space.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Cannot find initial commit. Too many commits. Action not possible.| ).
    ENDIF.

    ls_parent-sign   = 'I'.
    ls_parent-option = 'EQ'.
    ls_parent-low    = <ls_initial_commit>-sha1.
    INSERT ls_parent INTO TABLE lt_parents.

    " first commit
    INSERT <ls_initial_commit> INTO TABLE lt_sorted_commits.

    " remove from available commits
    DELETE ct_commits WHERE sha1 = <ls_initial_commit>-sha1.

    DO.
      get_1st_child_commit( EXPORTING it_commit_sha1s = lt_parents
                            IMPORTING et_commit_sha1s = lt_parents
                                      es_1st_commit   = ls_next_commit
                            CHANGING  ct_commits      = ct_commits ).
      IF ls_next_commit IS INITIAL.
        EXIT. "DO
      ENDIF.
      INSERT ls_next_commit INTO TABLE lt_sorted_commits.
    ENDDO.

    ct_commits = lt_sorted_commits.

  ENDMETHOD.
  METHOD extract_author_data.

    " unix time stamps are in same time zone, so ignore the zone
    FIND REGEX Lif_abapgit_definitions=>c_author_regex IN iv_author
      SUBMATCHES
      ev_author
      ev_email
      ev_time.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error author regex value='{ iv_author }'| ).
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GIT_COMMIT implementation

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

CLASS SHRIS5ZPAUXVKEPN5HWETLLASXNBTU IMPLEMENTATION.

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





*CLASS SHRIS5ZPAUXVKEPN5HWETLLASXTBTU DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_pack DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLASXTBTU.



*CLASS SHRIS5ZPAUXVKEPN5HWETLLASXVBTU DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_pack DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLASXVBTU.









*CLASS SHRIS5ZPAUXVKEPN5HWETLLASX3BTU DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_pack DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLASX3BTU.



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
          lo_stream TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLASXNBTU,
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
*CLASS SHRIS5ZPAUXVKEPN5HWETLLASX5BTU DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_porcelain DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLASX5BTU.



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
  METHOD delete_branch.

    Lcl_abapgit_git_transport=>receive_pack(
      iv_url         = iv_url
      iv_old         = is_branch-sha1
      iv_new         = c_zero
      iv_branch_name = is_branch-name ).

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
  METHOD delete_lightweight_tag.

    Lcl_abapgit_git_transport=>receive_pack(
      iv_url         = iv_url
      iv_old         = is_tag-sha1
      iv_new         = c_zero
      iv_branch_name = is_tag-name ).

  ENDMETHOD.
  METHOD empty_packfile.

    " For avoiding "client MUST send an empty packfile" error
    " https://github.com/git/git/blob/master/Documentation/gitprotocol-pack.txt#L595-L599

    DATA lt_objects TYPE Lif_abapgit_definitions=>ty_objects_tt.

    rv_pack = Lcl_abapgit_git_pack=>encode( lt_objects ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GIT_PORCELAIN implementation

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
  METHOD add_tag_prefix.

    rv_text = Lif_abapgit_git_definitions=>c_git_branch-tags_prefix && iv_text.

  ENDMETHOD.
  METHOD remove_tag_prefix.

    rv_text = iv_text.

    REPLACE FIRST OCCURRENCE OF Lif_abapgit_git_definitions=>c_git_branch-tags_prefix
            IN rv_text
            WITH ''.

  ENDMETHOD.
  METHOD add_peel.

    rv_text = iv_text && Lif_abapgit_git_definitions=>c_git_branch-peel.

  ENDMETHOD.
  METHOD remove_peel.

    rv_text = iv_text.

    REPLACE Lif_abapgit_git_definitions=>c_git_branch-peel IN rv_text WITH ''.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GIT_TAG implementation

*>>>>>>> ZCL_ABAPGIT_GIT_TRANSPORT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_git_transport=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_git_transport=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_git_transport=====ccau.
*CLASS SHRIS5ZPAUXVKEPN5HWETLLASX7BTU DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_transport DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLASX7BTU.



class LCL_ABAPGIT_GIT_TRANSPORT implementation.
*"* method's implementations
*include methods.
  METHOD branches.

    DATA: lo_client TYPE REF TO Lcl_abapgit_http_client.


    branch_list(
      EXPORTING
        iv_url         = iv_url
        iv_service     = c_service-upload
      IMPORTING
        eo_client      = lo_client
        eo_branch_list = ro_branch_list ).

    lo_client->close( ).

  ENDMETHOD.
  METHOD branch_list.

    CONSTANTS lc_content_regex TYPE string VALUE '^[0-9a-f]{4}#'.
    CONSTANTS lc_content_type  TYPE string VALUE 'application/x-git-<service>-pack-advertisement'.

    DATA lv_data                  TYPE string.
    DATA lv_expected_content_type TYPE string.

    eo_client = Lcl_abapgit_http=>create_by_url(
      iv_url     = iv_url
      iv_service = iv_service ).

    lv_expected_content_type = lc_content_type.
    REPLACE '<service>' IN lv_expected_content_type WITH iv_service.

    eo_client->check_smart_response(
        iv_expected_content_type = lv_expected_content_type
        iv_content_regex         = lc_content_regex ).

    lv_data = eo_client->get_cdata( ).

    CREATE OBJECT eo_branch_list
      EXPORTING
        iv_data = lv_data.

  ENDMETHOD.
  METHOD check_report_status.

    DATA:
      lv_string        TYPE string,
      lv_error         TYPE string,
      lv_unpack_status TYPE string,
      lv_unpack_code   TYPE string,
      lv_unpack_text   TYPE string,
      lv_commnd_status TYPE string,
      lv_commnd_code   TYPE string,
      lv_commnd_text   TYPE string.

    " Based on https://git-scm.com/docs/pack-protocol/2.2.3#_report_status
    lv_string = iv_string.

    IF lv_string = ''.
      lv_error = 'Unexpected empty reply'.
    ELSEIF strlen( lv_string ) < 4.
      lv_error = 'Missing pkt length for unpack status'.
    ELSE.
      lv_string = lv_string+4.
      SPLIT lv_string AT cl_abap_char_utilities=>newline INTO lv_unpack_status lv_string.
      SPLIT lv_unpack_status AT space INTO lv_unpack_text lv_unpack_code.

      IF lv_unpack_text <> 'unpack'.
        lv_error = 'Unexpected unpack status'.
      ELSEIF lv_unpack_code <> 'ok'.
        lv_error = |Unpack not ok ({ lv_unpack_code })|.
      ELSEIF lv_string = ''.
        lv_error = 'Unexpected command status'.
      ELSEIF strlen( lv_string ) < 4.
        lv_error = 'Missing pkt length for command status'.
      ELSE.
        lv_string = lv_string+4.
        SPLIT lv_string AT cl_abap_char_utilities=>newline INTO lv_commnd_status lv_string.
        SPLIT lv_commnd_status AT space INTO lv_commnd_code lv_commnd_text.

        IF lv_commnd_code <> 'ok'. "=ng
          " Some pre-defined error messages
          IF lv_commnd_text CP '*pre-receive hook declined*'.
            lv_error = 'Pre-receive hook declined'.
          ELSEIF lv_commnd_text CP '*protected branch hook declined*'.
            lv_error = 'Protected branch hook declined'.
          ELSEIF lv_commnd_text CP '*push declined due to email privacy*'.
            lv_error = 'Push declined due to email privacy'.
          ELSEIF lv_commnd_text CP '*funny refname*'.
            lv_error = 'Funny refname'.
          ELSEIF lv_commnd_text CP '*failed to update ref*'.
            lv_error = 'Failed to update ref'.
          ELSEIF lv_commnd_text CP '*missing necessary objects*'.
            lv_error = 'Missing necessary objects'.
          ELSEIF lv_commnd_text CP '*refusing to delete the current branch*'.
            lv_error = 'Branch delete not allowed'.
          ELSEIF lv_commnd_text CP '*cannot lock ref*reference already exists*'.
            lv_error = 'Branch already exists'.
          ELSEIF lv_commnd_text CP '*cannot lock ref*but expected*'.
            lv_error = 'Branch cannot be locked'.
          ELSEIF lv_commnd_text CP '*invalid committer*'.
            lv_error = 'Invalid committer'.
          ELSE.
            " Otherwise return full error message
            lv_error = lv_commnd_text.
          ENDIF.
        ELSEIF strlen( lv_string ) < 4.
          lv_error = 'Missing flush-pkt'.
        ELSEIF lv_string <> '0000' AND lv_string <> '00000000'.
          " We update only one reference at a time so this should be the end
          lv_error = 'Unexpected end of status (flush-pkt)'.
        ENDIF.
      ENDIF.
    ENDIF.

    IF lv_error IS NOT INITIAL.
      Lcx_abapgit_exception=>raise( |Git protocol error: { lv_error }| ).
    ENDIF.

  ENDMETHOD.
  METHOD find_branch.

    branch_list(
      EXPORTING
        iv_url          = iv_url
        iv_service      = iv_service
      IMPORTING
        eo_client       = eo_client
        eo_branch_list  = eo_branch_list ).

    IF ev_branch IS SUPPLIED.
      ev_branch = eo_branch_list->find_by_name( iv_branch_name )-sha1.
    ENDIF.

  ENDMETHOD.
  METHOD parse.

    CONSTANTS: lc_band1 TYPE x VALUE '01'.

    DATA: lv_len      TYPE i,
          lv_contents TYPE xstring,
          lv_pack     TYPE xstring.


    WHILE xstrlen( cv_data ) >= 4.
      lv_len = Lcl_abapgit_git_utils=>length_utf8_hex( cv_data ).

      IF lv_len > xstrlen( cv_data ).
        Lcx_abapgit_exception=>raise( 'parse, string length too large' ).
      ENDIF.

      lv_contents = cv_data(lv_len).
      IF lv_len = 0.
        cv_data = cv_data+4.
        CONTINUE.
      ELSE.
        cv_data = cv_data+lv_len.
      ENDIF.

      lv_contents = lv_contents+4.

      IF xstrlen( lv_contents ) > 1 AND lv_contents(1) = lc_band1.
        CONCATENATE lv_pack lv_contents+1 INTO lv_pack IN BYTE MODE.
      ENDIF.

    ENDWHILE.

    ev_pack = lv_pack.

  ENDMETHOD.
  METHOD receive_pack.

    DATA: lo_client   TYPE REF TO Lcl_abapgit_http_client,
          lv_cmd_pkt  TYPE string,
          lv_line     TYPE string,
          lv_tmp      TYPE xstring,
          lv_xstring  TYPE xstring,
          lv_string   TYPE string,
          lv_cap_list TYPE string,
          lv_buffer   TYPE string.


    find_branch(
      EXPORTING
        iv_url         = iv_url
        iv_service     = c_service-receive
        iv_branch_name = iv_branch_name
      IMPORTING
        eo_client      = lo_client ).

    lo_client->set_headers(
      iv_url     = iv_url
      iv_service = c_service-receive ).

    lv_cap_list = 'report-status'.

    lv_line = iv_old &&
              ` ` &&
              iv_new &&
              ` ` &&
              iv_branch_name &&
              Lcl_abapgit_git_utils=>get_null( ) &&
              ` ` &&
              lv_cap_list &&
              cl_abap_char_utilities=>newline.
    lv_cmd_pkt = Lcl_abapgit_git_utils=>pkt_string( lv_line ).

    lv_buffer = lv_cmd_pkt && '0000'.
    lv_tmp = Lcl_abapgit_convert=>string_to_xstring_utf8( lv_buffer ).

    CONCATENATE lv_tmp iv_pack INTO lv_xstring IN BYTE MODE.

    lv_xstring = lo_client->send_receive_close( lv_xstring ).

    lv_string = Lcl_abapgit_convert=>xstring_to_string_utf8( lv_xstring ).

    check_report_status( lv_string ).

  ENDMETHOD.
  METHOD upload_pack.

    DATA: lv_capa    TYPE string,
          lv_line    TYPE string,
          lv_buffer  TYPE string,
          lv_xstring TYPE xstring,
          lv_pack    TYPE xstring.

    FIELD-SYMBOLS: <lv_hash> LIKE LINE OF it_hashes.


    io_client->set_headers( iv_url     = iv_url
                            iv_service = c_service-upload ).

    LOOP AT it_hashes FROM 1 ASSIGNING <lv_hash>.
      IF sy-tabix = 1.
        lv_capa = 'side-band-64k no-progress multi_ack'.
        lv_line = 'want' && ` ` && <lv_hash>
          && ` ` && lv_capa && cl_abap_char_utilities=>newline.
      ELSE.
        lv_line = 'want' && ` ` && <lv_hash>
          && cl_abap_char_utilities=>newline.
      ENDIF.
      lv_buffer = lv_buffer && Lcl_abapgit_git_utils=>pkt_string( lv_line ).
    ENDLOOP.

    IF iv_deepen_level > 0.
      lv_buffer = lv_buffer && Lcl_abapgit_git_utils=>pkt_string( |deepen { iv_deepen_level }| &&
        cl_abap_char_utilities=>newline ).
    ENDIF.

    lv_buffer = lv_buffer
             && '0000'
             && '0009done' && cl_abap_char_utilities=>newline.

    lv_xstring = io_client->send_receive_close( Lcl_abapgit_convert=>string_to_xstring_utf8( lv_buffer ) ).

    parse( IMPORTING ev_pack = lv_pack
           CHANGING  cv_data = lv_xstring ).

    IF lv_pack IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Response could not be parsed - empty pack returned.' ).
    ENDIF.

    rt_objects = Lcl_abapgit_git_pack=>decode( lv_pack ).

  ENDMETHOD.
  METHOD upload_pack_by_branch.

    DATA: lo_client TYPE REF TO Lcl_abapgit_http_client,
          lt_hashes TYPE Lif_abapgit_git_definitions=>ty_sha1_tt.

    FIELD-SYMBOLS: <ls_branch> LIKE LINE OF it_branches.


    CLEAR: et_objects,
           ev_branch.

    find_branch(
      EXPORTING
        iv_url         = iv_url
        iv_service     = c_service-upload
        iv_branch_name = iv_branch_name
      IMPORTING
        eo_client      = lo_client
        ev_branch      = ev_branch ).

    IF it_branches IS INITIAL.
      APPEND ev_branch TO lt_hashes.
    ELSE.
      LOOP AT it_branches ASSIGNING <ls_branch>.
        APPEND <ls_branch>-sha1 TO lt_hashes.
      ENDLOOP.
    ENDIF.

    et_objects = upload_pack( io_client       = lo_client
                              iv_url          = iv_url
                              iv_deepen_level = iv_deepen_level
                              it_hashes       = lt_hashes ).

  ENDMETHOD.
  METHOD upload_pack_by_commit.

    DATA: lo_client TYPE REF TO Lcl_abapgit_http_client,
          lt_hashes TYPE Lif_abapgit_git_definitions=>ty_sha1_tt.


    CLEAR: et_objects,
           ev_commit.

    APPEND iv_hash TO lt_hashes.
    ev_commit = iv_hash.

    lo_client = Lcl_abapgit_http=>create_by_url(
      iv_url     = iv_url
      iv_service = c_service-upload ).

    et_objects = upload_pack( io_client       = lo_client
                              iv_url          = iv_url
                              iv_deepen_level = iv_deepen_level
                              it_hashes       = lt_hashes ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GIT_TRANSPORT implementation

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
*CLASS SHRIS5ZPAUXVKEPN5HWETLLASYHBTU DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_url DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLASYHBTU.




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

*>>>>>>> ZCL_ABAPGIT_HTTP <<<<<<<*

*"* macro definitions
*include zcl_abapgit_http==============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_http==============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_HTTP implementation.
*"* method's implementations
*include methods.
  METHOD acquire_login_details.

    DATA: lv_default_user TYPE string,
          lv_user         TYPE string,
          lv_pass         TYPE string,
          lo_digest       TYPE REF TO Lcl_abapgit_http_digest.


    lv_default_user = Lcl_abapgit_persistence_user=>get_instance( )->get_repo_login( iv_url ).
    lv_user         = lv_default_user.

    Lcl_abapgit_password_dialog=>popup(
      EXPORTING
        iv_repo_url     = iv_url
      CHANGING
        cv_user         = lv_user
        cv_pass         = lv_pass ).

    IF lv_user IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Unauthorized access. Check your credentials' ).
    ENDIF.

    IF lv_user <> lv_default_user.
      Lcl_abapgit_persistence_user=>get_instance( )->set_repo_login(
        iv_url   = iv_url
        iv_login = lv_user ).
    ENDIF.

    rv_scheme = ii_client->response->get_header_field( 'www-authenticate' ).
    FIND REGEX '^(\w+)' IN rv_scheme SUBMATCHES rv_scheme.

    CASE rv_scheme.
      WHEN c_scheme-digest.
* https://en.wikipedia.org/wiki/Digest_access_authentication
* e.g. used by https://www.gerritcodereview.com/
        CREATE OBJECT lo_digest
          EXPORTING
            ii_client   = ii_client
            iv_username = lv_user
            iv_password = lv_pass.
        lo_digest->run( ii_client ).
        io_client->set_digest( lo_digest ).
      WHEN OTHERS.
* https://en.wikipedia.org/wiki/Basic_access_authentication
        ii_client->authenticate(
          username = lv_user
          password = lv_pass ).
    ENDCASE.

  ENDMETHOD.
  METHOD check_auth_requested.

    DATA: lv_code TYPE i.

    ii_client->response->get_status( IMPORTING code = lv_code ).
    IF lv_code = 401.
      rv_auth_requested = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD create_by_url.

    DATA: lv_uri                 TYPE string,
          lv_scheme              TYPE string,
          lv_authorization       TYPE string,
          li_client              TYPE REF TO if_http_client,
          ls_header              LIKE LINE OF it_headers,
          lo_proxy_configuration TYPE REF TO Lcl_abapgit_proxy_config,
          lv_text                TYPE string.


    CREATE OBJECT lo_proxy_configuration.

    li_client = Lcl_abapgit_exit=>get_instance( )->create_http_client( iv_url ).

    IF li_client IS NOT BOUND.

      cl_http_client=>create_by_url(
        EXPORTING
          url                = Lcl_abapgit_url=>host( iv_url )
          ssl_id             = Lcl_abapgit_exit=>get_instance( )->get_ssl_id( )
          proxy_host         = lo_proxy_configuration->get_proxy_url( iv_url )
          proxy_service      = lo_proxy_configuration->get_proxy_port( iv_url )
        IMPORTING
          client             = li_client
        EXCEPTIONS
          argument_not_found = 1
          plugin_not_active  = 2
          internal_error     = 3
          OTHERS             = 4 ).
      IF sy-subrc <> 0.
        CASE sy-subrc.
          WHEN 1.
            " make sure:
            " a) SSL is setup properly in STRUST
            lv_text = 'HTTPS ARGUMENT_NOT_FOUND | STRUST/SSL Setup correct?'.
          WHEN OTHERS.
            lv_text = 'While creating HTTP Client'.

        ENDCASE.
        Lcx_abapgit_exception=>raise( lv_text ).
      ENDIF.

    ENDIF.

    IF lo_proxy_configuration->get_proxy_authentication( iv_url ) = abap_true.
      Lcl_abapgit_proxy_auth=>run( li_client ).
    ENDIF.

    CREATE OBJECT ro_client
      EXPORTING
        ii_client = li_client.

    IF is_local_system( iv_url ) = abap_true.
      li_client->send_sap_logon_ticket( ).
    ENDIF.

    li_client->request->set_cdata( '' ).
    li_client->request->set_header_field(
        name  = '~request_method'
        value = 'GET' ).
    li_client->request->set_header_field(
        name  = 'user-agent'
        value = get_agent( ) ).
    lv_uri = Lcl_abapgit_url=>path_name( iv_url ) &&
             '/info/refs?service=git-' &&
             iv_service &&
             '-pack'.
    li_client->request->set_header_field(
        name  = '~request_uri'
        value = lv_uri ).

    LOOP AT it_headers INTO ls_header.
      li_client->request->set_header_field(
        name  = ls_header-key
        value = ls_header-value ).
    ENDLOOP.

    " Disable internal auth dialog (due to its unclarity)
    li_client->propertytype_logon_popup = if_http_client=>co_disabled.

    lv_authorization = Lcl_abapgit_login_manager=>load( iv_url ).
    IF lv_authorization IS NOT INITIAL.
      li_client->request->set_header_field(
        name  = 'authorization'
        value = lv_authorization ).
      li_client->propertytype_logon_popup = li_client->co_disabled.
    ENDIF.

    Lcl_abapgit_exit=>get_instance( )->http_client(
      iv_url    = iv_url
      ii_client = li_client ).

    ro_client->send_receive( ).
    IF check_auth_requested( li_client ) = abap_true.
      lv_scheme = acquire_login_details( ii_client = li_client
                                         io_client = ro_client
                                         iv_url    = iv_url ).
      ro_client->send_receive( ).
    ENDIF.
    ro_client->check_http_200( ).

    IF lv_scheme <> c_scheme-digest.
      Lcl_abapgit_login_manager=>save(
        iv_uri           = iv_url
        iv_authorization = li_client->request->get_header_field( 'authorization' ) ).
    ENDIF.

  ENDMETHOD.
  METHOD get_agent.

* bitbucket require agent prefix = "git/"
* also see https://github.com/abapGit/abapGit/issues/1432
    rv_agent = |git/2.0 (abapGit { Lif_abapgit_version=>c_abap_version })|.

  ENDMETHOD.
  METHOD is_local_system.

    DATA: lv_host TYPE string,
          lt_list TYPE Lif_abapgit_definitions=>ty_string_tt,
          li_exit TYPE REF TO Lif_abapgit_exit.


    cl_http_server=>get_location( IMPORTING host = lv_host ).
    APPEND lv_host TO lt_list.

    APPEND 'localhost' TO lt_list.

    li_exit = Lcl_abapgit_exit=>get_instance( ).
    li_exit->change_local_host( CHANGING ct_hosts = lt_list ).

    FIND REGEX 'https?://([^/^:]*)' IN iv_url SUBMATCHES lv_host.

    READ TABLE lt_list WITH KEY table_line = lv_host TRANSPORTING NO FIELDS.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_HTTP implementation

*>>>>>>> ZCL_ABAPGIT_HTTP_AGENT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_http_agent========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_http_agent========ccimp.
CLASS SHRIS5ZPAUXVKEPN5HWETLLASYRBTU DEFINITION FINAL.
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

CLASS SHRIS5ZPAUXVKEPN5HWETLLASYRBTU IMPLEMENTATION.

  METHOD create.
    DATA lo_response TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLASYRBTU.
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

    ri_response = SHRIS5ZPAUXVKEPN5HWETLLASYRBTU=>create( li_client ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_HTTP_AGENT implementation

*>>>>>>> ZCL_ABAPGIT_HTTP_CLIENT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_http_client=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_http_client=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_HTTP_CLIENT implementation.
*"* method's implementations
*include methods.
  METHOD check_http_200.

    DATA: lv_code TYPE i,
          lv_text TYPE string.

    mi_client->response->get_status( IMPORTING code = lv_code ).
    CASE lv_code.
      WHEN 200.
        RETURN. " Success, OK
      WHEN 302.
        Lcx_abapgit_exception=>raise( 'Resource access temporarily redirected (HTTP 302). Check the URL' ).
      WHEN 401.
        Lcx_abapgit_exception=>raise( 'Unauthorized access to resource (HTTP 401). Check your credentials' ).
      WHEN 403.
        Lcx_abapgit_exception=>raise( 'Access to resource forbidden (HTTP 403)' ).
      WHEN 404.
        Lcx_abapgit_exception=>raise( 'Resource not found (HTTP 404). Check the URL' ).
      WHEN 407.
        Lcx_abapgit_exception=>raise( 'Proxy authentication required (HTTP 407). Check your credentials' ).
      WHEN 408.
        Lcx_abapgit_exception=>raise( 'Request timeout (HTTP 408)' ).
      WHEN 415.
        Lcx_abapgit_exception=>raise( 'Unsupported media type (HTTP 415)' ).
      WHEN 422.
        Lcx_abapgit_exception=>raise( 'Unprocessable entity (HTTP 422). Check, if URL has to end with ".git"' ).
      WHEN OTHERS.
        lv_text = mi_client->response->get_cdata( ).
        Lcx_abapgit_exception=>raise( |(HTTP { lv_code }) { lv_text }| ).
    ENDCASE.

  ENDMETHOD.
  METHOD check_smart_response.

    DATA: lv_content_type TYPE string.
    DATA: lv_data         TYPE string.

    IF iv_expected_content_type IS NOT INITIAL.
      lv_content_type = mi_client->response->get_content_type( ).
      IF lv_content_type <> iv_expected_content_type.
        Lcx_abapgit_exception=>raise( 'Wrong Content-Type sent by server - no fallback to the dumb protocol!' ).
      ENDIF.
    ENDIF.

    IF iv_content_regex IS NOT INITIAL.
      lv_data = mi_client->response->get_cdata( ).
      FIND REGEX iv_content_regex IN lv_data.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'Wrong Content sent by server' ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD close.
    mi_client->close( ).
  ENDMETHOD.
  METHOD constructor.
    mi_client = ii_client.
  ENDMETHOD.
  METHOD get_cdata.
    rv_value = mi_client->response->get_cdata( ).
  ENDMETHOD.
  METHOD send_receive.

    DATA: lv_text    TYPE string,
          lv_code    TYPE i,
          lv_message TYPE string.

    mi_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).

    IF sy-subrc = 0.
      mi_client->receive(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4 ).
    ENDIF.

    IF sy-subrc <> 0.
      " in case of HTTP_COMMUNICATION_FAILURE
      " make sure:
      " a) SSL is setup properly in STRUST
      " b) no firewalls
      " check trace file in transaction SMICM

      mi_client->get_last_error(
        IMPORTING
          code    = lv_code
          message = lv_message ).

      lv_text = |HTTP error { lv_code } occured: { lv_message }|.

      Lcx_abapgit_exception=>raise( lv_text ).
    ENDIF.

  ENDMETHOD.
  METHOD send_receive_close.

* do not use set_cdata as it modifies the Content-Type header field
    mi_client->request->set_data( iv_data ).
    send_receive( ).
    check_http_200( ).
    rv_data = mi_client->response->get_data( ).
    mi_client->close( ).

  ENDMETHOD.
  METHOD set_digest.
    mo_digest = io_digest.
  ENDMETHOD.
  METHOD set_headers.

    DATA: lv_value TYPE string.


    mi_client->request->set_header_field(
        name  = '~request_method'
        value = 'POST' ).

    lv_value = Lcl_abapgit_url=>path_name( iv_url ) &&
      '/git-' &&
      iv_service &&
      '-pack'.
    mi_client->request->set_header_field(
        name  = '~request_uri'
        value = lv_value ).

    lv_value = 'application/x-git-'
                  && iv_service && '-pack-request'.
    mi_client->request->set_header_field(
        name  = 'Content-Type'
        value = lv_value ).

    lv_value = 'application/x-git-'
                  && iv_service && '-pack-result'.
    mi_client->request->set_header_field(
        name  = 'Accept'
        value = lv_value ).

    IF mo_digest IS BOUND.
      mo_digest->run( mi_client ).
    ENDIF.

  ENDMETHOD.
  METHOD set_header.
    mi_client->request->set_header_field(
      name  = iv_key
      value = iv_value ).
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_HTTP_CLIENT implementation

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
  METHOD Lif_abapgit_code_inspector~list_global_variants.

    SELECT scichkv_hd~checkvname AS name
      scichkv_tx~text AS description
      INTO TABLE rt_list
      FROM scichkv_hd
      LEFT OUTER JOIN scichkv_tx
      ON scichkv_hd~checkvid = scichkv_tx~checkvid
      AND scichkv_hd~ciuser  = scichkv_tx~ciuser
      AND scichkv_tx~language = sy-langu
      WHERE scichkv_hd~ciuser = space.

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

INTERFACE SHRIS5ZPAUXVKEPN5HWETLLASYXBTU.

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

CLASS SHRIS5ZPAUXVKEPN5HWETLLASYYBTU DEFINITION FINAL.
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

CLASS SHRIS5ZPAUXVKEPN5HWETLLASYYBTU IMPLEMENTATION.

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

CLASS SHRIS5ZPAUXVKEPN5HWETLLASY2BTU DEFINITION FINAL.
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

CLASS SHRIS5ZPAUXVKEPN5HWETLLASY2BTU IMPLEMENTATION.

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
    lo_reader = cl_sxml_string_reader=>create( SHRIS5ZPAUXVKEPN5HWETLLASYYBTU=>string_to_xstring_utf8( iv_json ) ).

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

CLASS SHRIS5ZPAUXVKEPN5HWETLLASY4BTU DEFINITION FINAL CREATE PRIVATE.
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

CLASS SHRIS5ZPAUXVKEPN5HWETLLASY4BTU IMPLEMENTATION.

  METHOD class_constructor.
    gv_comma_with_lf = ',' && cl_abap_char_utilities=>newline.
  ENDMETHOD.

  METHOD stringify.

    DATA lo TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLASY4BTU.
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

CLASS SHRIS5ZPAUXVKEPN5HWETLLASY6BTU DEFINITION FINAL.
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
        type_kind         LIKE SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>any,
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

CLASS SHRIS5ZPAUXVKEPN5HWETLLASY6BTU IMPLEMENTATION.

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
    IF is_parent_type-type_kind = SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>table.
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
        WHEN SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>table.
          lo_tdescr ?= is_parent_type-dd.
          rs_node_type-dd = lo_tdescr->get_table_line_type( ).

        WHEN SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>struct_flat OR SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>struct_deep.
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
      IF rs_node_type-type_kind = SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>table.
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
      WHEN SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>table.
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

      WHEN SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>struct_flat OR SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>struct_deep.
        ASSIGN i_container_ref->* TO <parent_struc>.
        ASSERT sy-subrc = 0.
    ENDCASE.

    TRY.

      " array_index because stringified index goes in wrong order [1, 10, 2 ...]
        LOOP AT mr_nodes->* ASSIGNING <n> USING KEY array_index WHERE path = iv_path.

        " Get or create type cache record
          IF is_parent_type-type_kind <> SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>table OR ls_node_type-type_kind IS INITIAL.
          " table records are the same, no need to refetch twice

            ls_node_type = get_node_type(
            is_node        = <n>
            is_parent_type = is_parent_type ).

            IF mv_corresponding = abap_true AND ls_node_type IS INITIAL.
              CONTINUE.
            ENDIF.

          ENDIF.

        " Validate node type
          IF ls_node_type-type_kind = SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>data_ref OR
           ls_node_type-type_kind = SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>object_ref.
          " TODO maybe in future
            Lcx_abapgit_ajson_error=>raise( 'Cannot assign to ref' ).
          ENDIF.

        " Find target field reference
          CASE is_parent_type-type_kind.
            WHEN SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>table.
              IF NOT ls_node_type-target_field_name CO '0123456789'.
              " Does not affect anything actually but for integrity
                Lcx_abapgit_ajson_error=>raise( 'Need index to access tables' ).
              ENDIF.

              IF is_parent_type-tab_item_buf IS NOT BOUND. " Indirect hint that table was srt/hsh, see get_node_type
                APPEND INITIAL LINE TO <parent_stdtab> REFERENCE INTO lr_target_field.
                ASSERT sy-subrc = 0.
              ENDIF.

            WHEN SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>struct_flat OR SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>struct_deep.
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
              IF ls_node_type-type_kind <> SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>struct_flat AND
               ls_node_type-type_kind <> SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>struct_deep.
                Lcx_abapgit_ajson_error=>raise( 'Expected structure' ).
              ENDIF.
              any_to_abap(
              iv_path         = <n>-path && <n>-name && '/'
              is_parent_type  = ls_node_type
              i_container_ref = lr_target_field ).

            WHEN Lif_abapgit_ajson_types=>node_type-array.
              IF NOT ls_node_type-type_kind = SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>table.
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

    IF is_node_type-type_kind CA SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>deep_targets.
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
        IF is_node_type-type_kind = SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>date AND is_node-value IS NOT INITIAL.
          <container> = to_date( is_node-value ).
        ELSEIF is_node_type-type_kind = SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>packed AND is_node-value IS NOT INITIAL.
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

CLASS SHRIS5ZPAUXVKEPN5HWETLLASZABTU DEFINITION FINAL.
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

CLASS SHRIS5ZPAUXVKEPN5HWETLLASZABTU IMPLEMENTATION.

  METHOD class_constructor.

    DATA lo_dummy TYPE REF TO Lcl_abapgit_ajson.
    DATA lo_type TYPE REF TO cl_abap_refdescr.
    lo_type ?= cl_abap_typedescr=>describe_by_data( lo_dummy ).
    gv_ajson_absolute_type_name = lo_type->get_referenced_type( )->absolute_name.

  ENDMETHOD.

  METHOD convert.

    DATA lo_type TYPE REF TO cl_abap_typedescr.
    DATA lo_converter TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLASZABTU.

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

        IF io_type->type_kind = SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>data_ref OR iv_data IS INITIAL.
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

        ELSEIF io_type->type_kind = SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>object_ref
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
    ELSEIF io_type->type_kind CO SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>texts OR
           io_type->type_kind CO SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>binary OR
           io_type->type_kind CO SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>enum.
      ls_node-type = Lif_abapgit_ajson_types=>node_type-string.
      ls_node-value = |{ iv_data }|.
    ELSEIF io_type->type_kind = SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>date.
      ls_node-type = Lif_abapgit_ajson_types=>node_type-string.
      IF mv_format_datetime = abap_true.
        ls_node-value = format_date( iv_data ).
      ELSE.
        ls_node-value = |{ iv_data }|.
      ENDIF.
    ELSEIF io_type->type_kind = SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>time.
      ls_node-type = Lif_abapgit_ajson_types=>node_type-string.
      IF mv_format_datetime = abap_true.
        ls_node-value = format_time( iv_data ).
      ELSE.
        ls_node-value = |{ iv_data }|.
      ENDIF.
    ELSEIF io_type->type_kind CO SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>numeric.
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
    DATA lo_converter TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLASZABTU.

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
    IF io_type->type_kind CO SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>texts OR
       io_type->type_kind CO SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>date OR
       io_type->type_kind CO SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>time.
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
    ELSEIF io_type->type_kind CO SHRIS5ZPAUXVKEPN5HWETLLASYXBTU=>numeric.
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

INTERFACE SHRIS5ZPAUXVKEPN5HWETLLASZCBTU.
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

CLASS SHRIS5ZPAUXVKEPN5HWETLLASZDBTU DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES SHRIS5ZPAUXVKEPN5HWETLLASZCBTU.
    CLASS-METHODS new
      IMPORTING
        ii_filter TYPE REF TO Lif_abapgit_ajson_filter
      RETURNING
        VALUE(ro_instance) TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLASZDBTU.
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

CLASS SHRIS5ZPAUXVKEPN5HWETLLASZDBTU IMPLEMENTATION.

  METHOD new.
    CREATE OBJECT ro_instance EXPORTING ii_filter = ii_filter.
  ENDMETHOD.

  METHOD constructor.
    ASSERT ii_filter IS BOUND.
    mi_filter = ii_filter.
  ENDMETHOD.

  METHOD SHRIS5ZPAUXVKEPN5HWETLLASZCBTU~run.

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

CLASS SHRIS5ZPAUXVKEPN5HWETLLASZFBTU DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES SHRIS5ZPAUXVKEPN5HWETLLASZCBTU.
    CLASS-METHODS new
      IMPORTING
        ii_mapper TYPE REF TO Lif_abapgit_ajson_mapping
      RETURNING
        VALUE(ro_instance) TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLASZFBTU.
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

CLASS SHRIS5ZPAUXVKEPN5HWETLLASZFBTU IMPLEMENTATION.

  METHOD new.
    CREATE OBJECT ro_instance EXPORTING ii_mapper = ii_mapper.
  ENDMETHOD.

  METHOD constructor.
    ASSERT ii_mapper IS BOUND.
    mi_mapper = ii_mapper.
  ENDMETHOD.

  METHOD SHRIS5ZPAUXVKEPN5HWETLLASZCBTU~run.

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

CLASS SHRIS5ZPAUXVKEPN5HWETLLASZHBTU DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES SHRIS5ZPAUXVKEPN5HWETLLASZCBTU.
    CLASS-METHODS new
      RETURNING
        VALUE(ro_instance) TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLASZHBTU.
    METHODS add
      IMPORTING
        ii_mutator TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLASZCBTU
      RETURNING
        VALUE(ro_self) TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLASZHBTU.

  PRIVATE SECTION.
    DATA mt_queue TYPE STANDARD TABLE OF REF TO SHRIS5ZPAUXVKEPN5HWETLLASZCBTU.

ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLASZHBTU IMPLEMENTATION.

  METHOD add.
    IF ii_mutator IS BOUND.
      APPEND ii_mutator TO mt_queue.
    ENDIF.
    ro_self = me.
  ENDMETHOD.

  METHOD new.
    CREATE OBJECT ro_instance.
  ENDMETHOD.

  METHOD SHRIS5ZPAUXVKEPN5HWETLLASZCBTU~run.

    DATA li_mutator TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLASZCBTU.
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
CLASS SHRIS5ZPAUXVKEPN5HWETLLASZJBTU DEFINITION FINAL.
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

CLASS SHRIS5ZPAUXVKEPN5HWETLLASZJBTU IMPLEMENTATION.
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


*CLASS zcl_abapgit_ajson DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLASZPBTU.


**********************************************************************
* READER
**********************************************************************


*CLASS zcl_abapgit_ajson DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLASZQBTU.



**********************************************************************
* JSON TO ABAP
**********************************************************************


*CLASS zcl_abapgit_ajson DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLASZRBTU.


**********************************************************************
* WRITER
**********************************************************************


*CLASS zcl_abapgit_ajson DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLASZSBTU.



**********************************************************************
* INTEGRATED
**********************************************************************


**********************************************************************
* ABAP TO JSON
**********************************************************************

*CLASS zcl_abapgit_ajson DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLASZVBTU.


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

    DATA lo_mutator_queue TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLASZHBTU.

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
        lo_mutator_queue->add( SHRIS5ZPAUXVKEPN5HWETLLASZFBTU=>new( ii_mapper ) ).
      ENDIF.
      IF ii_filter IS BOUND.
        lo_mutator_queue->add( SHRIS5ZPAUXVKEPN5HWETLLASZDBTU=>new( ii_filter ) ).
      ENDIF.
      lo_mutator_queue->SHRIS5ZPAUXVKEPN5HWETLLASZCBTU~run(
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
    ls_path_name = SHRIS5ZPAUXVKEPN5HWETLLASYYBTU=>split_path( iv_path ).

    READ TABLE mt_json_tree
      ASSIGNING <item>
      WITH KEY
        path = ls_path_name-path
        name = ls_path_name-name.
    IF sy-subrc = 0.
      GET REFERENCE OF <item> INTO rv_item.
    ENDIF.

  ENDMETHOD.
  METHOD parse.

    DATA lo_parser TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLASY2BTU.

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
            ls_new_node-index = SHRIS5ZPAUXVKEPN5HWETLLASYYBTU=>validate_array_index(
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

    lv_normalized_path = SHRIS5ZPAUXVKEPN5HWETLLASYYBTU=>normalize_path( iv_path ).
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
  METHOD Lif_abapgit_ajson~delete.

    read_only_watchdog( ).

    DATA ls_split_path TYPE Lif_abapgit_ajson_types=>ty_path_name.
    ls_split_path = SHRIS5ZPAUXVKEPN5HWETLLASYYBTU=>split_path( iv_path ).

    delete_subtree(
      iv_path = ls_split_path-path
      iv_name = ls_split_path-name ).

    ri_json = me.

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~exists.
    rv_exists = boolc( get_item( iv_path ) IS NOT INITIAL ).
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

    DATA lo_to_abap TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLASY6BTU.
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
  METHOD Lif_abapgit_ajson~members.

    DATA lv_normalized_path TYPE string.
    FIELD-SYMBOLS <item> LIKE LINE OF mt_json_tree.

    lv_normalized_path = SHRIS5ZPAUXVKEPN5HWETLLASYYBTU=>normalize_path( iv_path ).

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
    ls_new_path-path = SHRIS5ZPAUXVKEPN5HWETLLASYYBTU=>normalize_path( iv_path ).
    ls_new_path-name = |{ lv_new_index }|.

    lt_new_nodes = SHRIS5ZPAUXVKEPN5HWETLLASZABTU=>convert(
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

    ls_split_path = SHRIS5ZPAUXVKEPN5HWETLLASYYBTU=>split_path( iv_path ).
    IF ls_split_path IS INITIAL. " Assign root, exceptional processing
      IF iv_node_type IS NOT INITIAL.
        mt_json_tree = SHRIS5ZPAUXVKEPN5HWETLLASZABTU=>insert_with_type(
          is_opts            = ms_opts
          iv_data            = iv_val
          iv_type            = iv_node_type
          is_prefix          = ls_split_path
          ii_custom_mapping  = mi_custom_mapping ).
      ELSE.
        mt_json_tree = SHRIS5ZPAUXVKEPN5HWETLLASZABTU=>convert(
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
      lv_array_index = SHRIS5ZPAUXVKEPN5HWETLLASYYBTU=>validate_array_index(
        iv_path  = ls_split_path-path
        iv_index = ls_split_path-name ).
    ELSEIF lr_parent->type = Lif_abapgit_ajson_types=>node_type-object
      AND lv_item_order = 0 AND ms_opts-keep_item_order = abap_true.
      lv_item_order = lr_parent->children + 1.
    ENDIF.

    IF iv_node_type IS NOT INITIAL.
      lt_new_nodes = SHRIS5ZPAUXVKEPN5HWETLLASZABTU=>insert_with_type(
        is_opts            = ms_opts
        iv_item_order      = lv_item_order
        iv_data            = iv_val
        iv_type            = iv_node_type
        iv_array_index     = lv_array_index
        is_prefix          = ls_split_path
        ii_custom_mapping  = mi_custom_mapping ).
    ELSE.
      lt_new_nodes = SHRIS5ZPAUXVKEPN5HWETLLASZABTU=>convert(
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
    lv_val = SHRIS5ZPAUXVKEPN5HWETLLASZABTU=>format_date( iv_val ).

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
    lv_timestamp_iso = SHRIS5ZPAUXVKEPN5HWETLLASZABTU=>format_timestamp( iv_val ).

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
    lv_normalized_path = SHRIS5ZPAUXVKEPN5HWETLLASYYBTU=>normalize_path( iv_path ).
    lv_path_len        = strlen( lv_normalized_path ).
    ls_path_parts      = SHRIS5ZPAUXVKEPN5HWETLLASYYBTU=>split_path( lv_normalized_path ).

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

    rv_json = SHRIS5ZPAUXVKEPN5HWETLLASY4BTU=>stringify(
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

    ls_split_path = SHRIS5ZPAUXVKEPN5HWETLLASYYBTU=>split_path( iv_path ).
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

    DATA lo_to_abap TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLASY6BTU.

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
  METHOD new.
    CREATE OBJECT ro_instance
      EXPORTING
        iv_to_abap_corresponding_only = iv_to_abap_corresponding_only
        iv_format_datetime = iv_format_datetime
        iv_keep_item_order = iv_keep_item_order.
  ENDMETHOD.
  METHOD Lif_abapgit_ajson~clone.
    ri_json = create_from( me ).
  ENDMETHOD.
  METHOD Lif_abapgit_ajson~filter.
    ri_json = create_from(
      ii_source_json = me
      ii_filter      = ii_filter ).
  ENDMETHOD.
  METHOD Lif_abapgit_ajson~map.
    ri_json = create_from(
      ii_source_json = me
      ii_mapper      = ii_mapper ).
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
  METHOD Lif_abapgit_ajson~to_abap_corresponding_only.
    ms_opts-to_abap_corresponding_only = iv_enable.
    ri_json = me.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_AJSON implementation

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

CLASS SHRIS5ZPAUXVKEPN5HWETLLASZ4BTU DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_ajson_filter.
ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLASZ4BTU IMPLEMENTATION.
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

CLASS SHRIS5ZPAUXVKEPN5HWETLLASZ6BTU DEFINITION FINAL.
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

CLASS SHRIS5ZPAUXVKEPN5HWETLLASZ6BTU IMPLEMENTATION.

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

CLASS SHRIS5ZPAUXVKEPN5HWETLLAS2ABTU DEFINITION FINAL.
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

CLASS SHRIS5ZPAUXVKEPN5HWETLLAS2ABTU IMPLEMENTATION.

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
    CREATE OBJECT ri_filter TYPE SHRIS5ZPAUXVKEPN5HWETLLAS2ABTU
      EXPORTING
        it_filters = it_filters.
  ENDMETHOD.
  METHOD create_empty_filter.
    CREATE OBJECT ri_filter TYPE SHRIS5ZPAUXVKEPN5HWETLLASZ4BTU.
  ENDMETHOD.
  METHOD create_path_filter.
    CREATE OBJECT ri_filter TYPE SHRIS5ZPAUXVKEPN5HWETLLASZ6BTU
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
CLASS SHRIS5ZPAUXVKEPN5HWETLLAS2EBTU IMPLEMENTATION. "DEPRECATED


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

CLASS SHRIS5ZPAUXVKEPN5HWETLLAS2FBTU IMPLEMENTATION.

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

CLASS SHRIS5ZPAUXVKEPN5HWETLLAS2GBTU IMPLEMENTATION.


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


CLASS SHRIS5ZPAUXVKEPN5HWETLLAS2HBTU IMPLEMENTATION.


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


CLASS SHRIS5ZPAUXVKEPN5HWETLLAS2IBTU IMPLEMENTATION. "DEPRECATED


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

CLASS SHRIS5ZPAUXVKEPN5HWETLLAS2JBTU IMPLEMENTATION.

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

CLASS SHRIS5ZPAUXVKEPN5HWETLLAS2KBTU IMPLEMENTATION.

  METHOD Lif_abapgit_ajson_mapping~rename_node.

    REPLACE ALL OCCURRENCES OF REGEX `([a-z])([A-Z])` IN cv_name WITH `$1_$2`.
    cv_name = to_lower( cv_name ).

  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~to_abap.

  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~to_json.

  ENDMETHOD.

ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLAS2LBTU IMPLEMENTATION.

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

    CREATE OBJECT ri_mapping TYPE SHRIS5ZPAUXVKEPN5HWETLLAS2IBTU
      EXPORTING
        it_mapping_fields   = it_mapping_fields
        iv_first_json_upper = iv_first_json_upper.

  ENDMETHOD.
  METHOD create_field_mapping.

    CREATE OBJECT ri_mapping TYPE SHRIS5ZPAUXVKEPN5HWETLLAS2EBTU
      EXPORTING
        it_mapping_fields = it_mapping_fields.

  ENDMETHOD.
  METHOD create_lower_case.

    CREATE OBJECT ri_mapping TYPE SHRIS5ZPAUXVKEPN5HWETLLAS2HBTU
      EXPORTING
        it_mapping_fields = it_mapping_fields.

  ENDMETHOD.
  METHOD create_upper_case.

    CREATE OBJECT ri_mapping TYPE SHRIS5ZPAUXVKEPN5HWETLLAS2GBTU
      EXPORTING
        it_mapping_fields = it_mapping_fields.

  ENDMETHOD.
  METHOD create_compound_mapper.

    DATA lt_queue TYPE Lif_abapgit_ajson_mapping=>ty_table_of.

    APPEND ii_mapper1 TO lt_queue.
    APPEND ii_mapper2 TO lt_queue.
    APPEND ii_mapper3 TO lt_queue.
    APPEND LINES OF it_more TO lt_queue.
    DELETE lt_queue WHERE table_line IS INITIAL.

    CREATE OBJECT ri_mapping TYPE SHRIS5ZPAUXVKEPN5HWETLLAS2JBTU
      EXPORTING
        it_queue = lt_queue.

  ENDMETHOD.
  METHOD create_rename.

    CREATE OBJECT ri_mapping TYPE SHRIS5ZPAUXVKEPN5HWETLLAS2FBTU
      EXPORTING
        it_rename_map = it_rename_map
        iv_rename_by = iv_rename_by.

  ENDMETHOD.
  METHOD create_to_camel_case.

    CREATE OBJECT ri_mapping TYPE SHRIS5ZPAUXVKEPN5HWETLLAS2LBTU
      EXPORTING
        iv_first_json_upper = iv_first_json_upper.

  ENDMETHOD.
  METHOD create_to_snake_case.

    CREATE OBJECT ri_mapping TYPE SHRIS5ZPAUXVKEPN5HWETLLAS2KBTU.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_AJSON_MAPPING implementation

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

CLASS SHRIS5ZPAUXVKEPN5HWETLLAS24BTU DEFINITION FINAL.
  PUBLIC SECTION.

    DATA mt_nodes TYPE Lif_abapgit_ajson_types=>ty_nodes_tt READ-ONLY.

    METHODS add
      IMPORTING
        iv_str TYPE string.
    METHODS sorted
      RETURNING
        VALUE(rt_nodes) TYPE Lif_abapgit_ajson_types=>ty_nodes_ts.

ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLAS24BTU IMPLEMENTATION.
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
*       CLASS SHRIS5ZPAUXVKEPN5HWETLLAS3GBTU DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS SHRIS5ZPAUXVKEPN5HWETLLAS3GBTU IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*




*CLASS zcl_abapgit_objects DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLAS3KBTU.


class LCL_ABAPGIT_OBJECTS implementation.
*"* method's implementations
*include methods.
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
  METHOD deserialize_objects.

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
  METHOD deserialize_steps.

    FIELD-SYMBOLS <ls_step> LIKE LINE OF it_steps.

    LOOP AT it_steps ASSIGNING <ls_step>.
      deserialize_objects(
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
  METHOD get_extra_from_filename.

    IF iv_filename IS NOT INITIAL.
      FIND REGEX '\..*\.([\-a-z0-9_%]*)\.' IN iv_filename SUBMATCHES rv_extra.
      IF sy-subrc = 0.
        rv_extra = cl_http_utility=>unescape_url( rv_extra ).
      ENDIF.
    ENDIF.

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
endclass. "ZCL_ABAPGIT_OBJECTS implementation

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
  METHOD Lif_abapgit_object~get_deserialize_order.
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
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    CALL METHOD mo_plugin->('WRAP_SERIALIZE')
      EXPORTING
        io_xml = io_xml.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECTS_BRIDGE implementation

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
*CLASS SHRIS5ZPAUXVKEPN5HWETLLAS3LBTU DEFINITION DEFERRED.
*CLASS zcl_abapgit_objects_generic DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLAS3LBTU.



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

*CLASS zcl_abapgit_objects_program DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLAS3NBTU.


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

*>>>>>>> ZCL_ABAPGIT_OBJECTS_SUPER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_objects_super=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_objects_super=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECTS_SUPER implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    ms_item = is_item.
    ASSERT NOT ms_item IS INITIAL.
    mv_language = iv_language.
    ASSERT NOT mv_language IS INITIAL.
  ENDMETHOD.
  METHOD corr_insert.

    DATA: lv_object       TYPE trobj_name,
          lv_object_class TYPE tadir-object.

    IF ig_object_class IS NOT INITIAL.
      lv_object_class = ig_object_class.
      IF ig_object_class = 'DICT'.
        CONCATENATE ms_item-obj_type ms_item-obj_name INTO lv_object.
      ELSE.
        lv_object = ms_item-obj_name.
      ENDIF.
    ELSE.
      lv_object_class = ms_item-obj_type.
      lv_object       = ms_item-obj_name.
    ENDIF.

    Lcl_abapgit_factory=>get_cts_api( )->insert_transport_object(
      iv_object   = lv_object_class
      iv_obj_name = lv_object
      iv_package  = iv_package
      iv_language = mv_language ).

  ENDMETHOD.
  METHOD delete_ddic.

    DATA: lv_objname TYPE rsedd0-ddobjname,
          lv_objtype TYPE rsedd0-ddobjtype.

    lv_objname = ms_item-obj_name.
    lv_objtype = iv_objtype.

    TRY.
        CALL FUNCTION 'RS_DD_DELETE_OBJ'
          EXPORTING
            no_ask               = iv_no_ask
            objname              = lv_objname
            objtype              = lv_objtype
            no_ask_delete_append = iv_no_ask_delete_append
          EXCEPTIONS
            not_executed         = 1
            object_not_found     = 2
            object_not_specified = 3
            permission_failure   = 4
            dialog_needed        = 5
            OTHERS               = 6.
      CATCH cx_sy_dyn_call_param_not_found.
        TRY.
            " try to force deletion for APPENDs
            CALL FUNCTION 'RS_DD_DELETE_OBJ'
              EXPORTING
                no_ask               = iv_no_ask
                objname              = lv_objname
                objtype              = lv_objtype
                aie_force_deletion   = iv_no_ask_delete_append
              EXCEPTIONS
                not_executed         = 1
                object_not_found     = 2
                object_not_specified = 3
                permission_failure   = 4
                dialog_needed        = 5
                OTHERS               = 6.
          CATCH cx_sy_dyn_call_param_not_found.
            " no_ask_delete_append and aie_force_deletion not available in lower releases
            CALL FUNCTION 'RS_DD_DELETE_OBJ'
              EXPORTING
                no_ask               = iv_no_ask
                objname              = lv_objname
                objtype              = lv_objtype
              EXCEPTIONS
                not_executed         = 1
                object_not_found     = 2
                object_not_specified = 3
                permission_failure   = 4
                dialog_needed        = 5
                OTHERS               = 6.
        ENDTRY.
    ENDTRY.

    IF sy-subrc = 5.
      Lcx_abapgit_exception=>raise( |Object { ms_item-obj_type } { ms_item-obj_name
                                    } has dependencies and must be deleted manually| ).
    ELSEIF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error deleting { ms_item-obj_type } { ms_item-obj_name }| ).
    ENDIF.

  ENDMETHOD.
  METHOD delete_longtexts.

    Lcl_abapgit_factory=>get_longtexts( )->delete(
      iv_longtext_id = iv_longtext_id
      iv_object_name = ms_item-obj_name ).

  ENDMETHOD.
  METHOD deserialize_longtexts.

    Lcl_abapgit_factory=>get_longtexts( )->deserialize(
      ii_xml           = ii_xml
      iv_longtext_name = iv_longtext_name
      iv_object_name   = ms_item-obj_name
      iv_longtext_id   = iv_longtext_id
      iv_main_language = mv_language ).

  ENDMETHOD.
  METHOD exists_a_lock_entry_for.

    DATA: lt_lock_entries TYPE STANDARD TABLE OF seqg3.
    DATA: lv_argument TYPE seqg3-garg.

    IF iv_prefix IS INITIAL.
      lv_argument = iv_argument.
    ELSE.
      lv_argument = |{ iv_prefix  }{ iv_argument }|.
      OVERLAY lv_argument WITH '                                          '.
      lv_argument = lv_argument && '*'.
    ENDIF.

    CALL FUNCTION 'ENQUEUE_READ'
      EXPORTING
        guname                = '*'
        garg                  = lv_argument
      TABLES
        enq                   = lt_lock_entries
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        OTHERS                = 3.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    READ TABLE lt_lock_entries TRANSPORTING NO FIELDS
                               WITH KEY gobj = iv_lock_object.
    IF sy-subrc = 0.
      rv_exists_a_lock_entry = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD get_metadata.

    DATA: lv_class TYPE string.

    lv_class = cl_abap_classdescr=>describe_by_object_ref( me )->get_relative_name( ).

    REPLACE FIRST OCCURRENCE OF 'LCL_ABAPGIT' IN lv_class WITH 'LCL'.

    rs_metadata-class = lv_class.
    rs_metadata-version = 'v1.0.0'.

  ENDMETHOD.
  METHOD is_active.

    rv_active = Lcl_abapgit_objects_activation=>is_active( ms_item ).

  ENDMETHOD.
  METHOD serialize_longtexts.

    Lcl_abapgit_factory=>get_longtexts( )->serialize(
      iv_object_name   = ms_item-obj_name
      iv_longtext_name = iv_longtext_name
      iv_longtext_id   = iv_longtext_id
      it_dokil         = it_dokil
      io_i18n_params   = mo_i18n_params
      ii_xml           = ii_xml ).

  ENDMETHOD.
  METHOD set_default_package.

    " In certain cases we need to set the package via ABAP memory
    " because we can't supply it via the APIs.
    "
    " Set default package, see function module RS_CORR_INSERT FORM get_current_devclass.
    "
    " We use ABAP memory instead the SET parameter because it is
    " more reliable. SET parameter doesn't work when multiple objects
    " are deserialized which uses the ABAP memory mechanism.
    " We don't need to reset the memory as it is done in above mentioned form routine.

    EXPORT current_devclass FROM iv_package TO MEMORY ID 'EUK'.

  ENDMETHOD.
  METHOD set_default_transport.

    " In certain cases we need to set the transport via ABAP memory
    " because we can't supply it via the APIs.
    "
    " See function module RS_CORR_INSERT

    EXPORT tasknr FROM iv_transport TO MEMORY ID 'EUT'.

  ENDMETHOD.
  METHOD tadir_delete.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_delete_tadir_entry          = abap_true
        wi_tadir_pgmid                 = 'R3TR'
        wi_tadir_object                = ms_item-obj_type
        wi_tadir_obj_name              = ms_item-obj_name
        wi_test_modus                  = abap_false
      EXCEPTIONS
        tadir_entry_not_existing       = 1
        tadir_entry_ill_type           = 2
        no_systemname                  = 3
        no_systemtype                  = 4
        original_system_conflict       = 5
        object_reserved_for_devclass   = 6
        object_exists_global           = 7
        object_exists_local            = 8
        object_is_distributed          = 9
        obj_specification_not_unique   = 10
        no_authorization_to_delete     = 11
        devclass_not_existing          = 12
        simultanious_set_remove_repair = 13
        order_missing                  = 14
        no_modification_of_head_syst   = 15
        pgmid_object_not_allowed       = 16
        masterlanguage_not_specified   = 17
        devclass_not_specified         = 18
        specify_owner_unique           = 19
        loc_priv_objs_no_repair        = 20
        gtadir_not_reached             = 21
        object_locked_for_order        = 22
        change_of_class_not_allowed    = 23
        no_change_from_sap_to_tmp      = 24
        OTHERS                         = 25.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD tadir_insert.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_test_modus                  = abap_false
        wi_tadir_pgmid                 = 'R3TR'
        wi_tadir_object                = ms_item-obj_type
        wi_tadir_obj_name              = ms_item-obj_name
        wi_tadir_author                = sy-uname
        wi_tadir_devclass              = iv_package
        wi_tadir_masterlang            = mv_language
        iv_delflag                     = abap_false
      EXCEPTIONS
        tadir_entry_not_existing       = 1
        tadir_entry_ill_type           = 2
        no_systemname                  = 3
        no_systemtype                  = 4
        original_system_conflict       = 5
        object_reserved_for_devclass   = 6
        object_exists_global           = 7
        object_exists_local            = 8
        object_is_distributed          = 9
        obj_specification_not_unique   = 10
        no_authorization_to_delete     = 11
        devclass_not_existing          = 12
        simultanious_set_remove_repair = 13
        order_missing                  = 14
        no_modification_of_head_syst   = 15
        pgmid_object_not_allowed       = 16
        masterlanguage_not_specified   = 17
        devclass_not_specified         = 18
        specify_owner_unique           = 19
        loc_priv_objs_no_repair        = 20
        gtadir_not_reached             = 21
        object_locked_for_order        = 22
        change_of_class_not_allowed    = 23
        no_change_from_sap_to_tmp      = 24
        OTHERS                         = 25.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD clear_abap_language_version.

    " Used during serializing of objects
    IF ms_item-abap_language_version <> Lcl_abapgit_abap_language_vers=>c_any_abap_language_version.
      " ABAP language is defined in repo setting so there's no need to serialize it
      CLEAR cv_abap_language_version.
    ENDIF.

  ENDMETHOD.
  METHOD set_abap_language_version.

    " Used during deserializing of objects
    IF ms_item-abap_language_version <> Lcl_abapgit_abap_language_vers=>c_any_abap_language_version.
      " ABAP language is defined in repo setting so set it accordingly
      cv_abap_language_version = ms_item-abap_language_version.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECTS_SUPER implementation

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
endclass. "ZCL_ABAPGIT_OBJECT_ACID implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_AIFC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_aifc=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_aifc=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_AIFC implementation.
*"* method's implementations
*include methods.
  METHOD authorization_check.
    DATA: lx_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.
    DATA: lx_root TYPE REF TO cx_root.

    rv_success = abap_false.
    TRY.
        CALL METHOD mo_abapgit_util->('/AIF/IF_ABAPGIT_AIFC_UTIL~AUTHORIZATION_CHECK')
          RECEIVING
            rv_success = rv_success.

      CATCH cx_sy_dyn_call_error INTO lx_dyn_call_error.
        Lcx_abapgit_exception=>raise( iv_text = 'AIFC not supported'
                                      ix_previous = lx_dyn_call_error ).
      CATCH cx_root INTO lx_root.
        Lcx_abapgit_exception=>raise_with_text( lx_root ).
    ENDTRY.
  ENDMETHOD.
  METHOD clear_client.
    DATA:
      BEGIN OF ls_data_to_clear,
        mandt  TYPE sy-mandt,
        client TYPE sy-mandt,
      END OF ls_data_to_clear.

    FIELD-SYMBOLS:
      <ls_data> TYPE any.

    LOOP AT ct_data ASSIGNING <ls_data>.
      MOVE-CORRESPONDING ls_data_to_clear TO <ls_data>.
    ENDLOOP.
  ENDMETHOD.
  METHOD compress_interface.
    DATA: lx_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.
    DATA: lx_root TYPE REF TO cx_root.

    TRY.
        CALL METHOD mo_abapgit_util->('/AIF/IF_ABAPGIT_AIFC_UTIL~COMPRESS_INTERFACE')
          EXPORTING
            is_ifkeys  = is_ifkeys
          RECEIVING
            rv_success = rv_success.

      CATCH cx_sy_dyn_call_error INTO lx_dyn_call_error.
        Lcx_abapgit_exception=>raise( iv_text = 'AIFC not supported'
                                      ix_previous = lx_dyn_call_error ).
      CATCH cx_root INTO lx_root.
        Lcx_abapgit_exception=>raise_with_text( lx_root ).
    ENDTRY.
  ENDMETHOD.
  METHOD constructor.
    DATA: lx_exc_ref TYPE REF TO cx_sy_dyn_call_error.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    ms_icd_data_key = is_item-obj_name.

    TRY.
        CALL METHOD ('/AIF/CL_ABAPGIT_AIFC_UTIL')=>('GET_INSTANCE')
          RECEIVING
            rr_abapgit_aifc_util = mo_abapgit_util.

      CATCH cx_sy_dyn_call_error INTO lx_exc_ref.
        Lcx_abapgit_exception=>raise( 'AIFC not supported' ).
    ENDTRY.
  ENDMETHOD.
  METHOD execute_checks.
    DATA ls_ifkeys TYPE ty_aif_key_s.

    DATA lr_tabledescr TYPE REF TO cl_abap_tabledescr.
    DATA lr_structdescr TYPE REF TO cl_abap_structdescr.
    DATA lr_table TYPE REF TO data.
    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <ls_table> TYPE any.
    FIELD-SYMBOLS: <lv_value> TYPE any.

    DATA: lx_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.
    DATA: lx_root TYPE REF TO cx_root.

    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( p_name = '/AIF/T_FINF' ).
    lr_tabledescr = cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).

    CREATE DATA lr_table TYPE HANDLE lr_tabledescr.
    ASSIGN lr_table->* TO <lt_table>.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Fieldsymbol not assigned' ).
    ENDIF.

    TRY.
        io_xml->read( EXPORTING
                    iv_name = '/AIF/T_FINF'
                  CHANGING
                    cg_data = <lt_table> ).

        READ TABLE <lt_table> ASSIGNING <ls_table> INDEX 1.
        IF sy-subrc = 0.
          ASSIGN COMPONENT 'NS' OF STRUCTURE <ls_table> TO <lv_value>.
          IF sy-subrc = 0.
            ls_ifkeys-ns = <lv_value>.
          ENDIF.

          ASSIGN COMPONENT 'IFNAME' OF STRUCTURE <ls_table> TO <lv_value>.
          IF sy-subrc = 0.
            ls_ifkeys-ifname = <lv_value>.
          ENDIF.

          ASSIGN COMPONENT 'IFVERSION' OF STRUCTURE <ls_table> TO <lv_value>.
          IF sy-subrc = 0.
            ls_ifkeys-ifver = <lv_value>.
          ENDIF.

          CALL METHOD mo_abapgit_util->('/AIF/IF_ABAPGIT_AIFC_UTIL~EXECUTE_CHECKS')
            EXPORTING
              is_ifkeys  = ls_ifkeys
              is_finf    = <ls_table>
            RECEIVING
              rv_success = rv_success.
        ENDIF.

      CATCH cx_sy_dyn_call_error INTO lx_dyn_call_error.
        Lcx_abapgit_exception=>raise( iv_text = 'AIFC not supported'
                                      ix_previous = lx_dyn_call_error ).
      CATCH cx_root INTO lx_root.
        Lcx_abapgit_exception=>raise_with_text( lx_root ).
    ENDTRY.
  ENDMETHOD.
  METHOD get_content_compress.
    DATA: lx_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.
    DATA: lx_root TYPE REF TO cx_root.
    DATA: lo_log TYPE REF TO object.

    TRY.
        CREATE OBJECT lo_log TYPE ('/AIF/CL_ABAPGIT_BAL_LOG')
          EXPORTING ir_git_log = io_log
                    is_item = ms_item.

        CALL METHOD mo_abapgit_util->('/AIF/IF_ABAPGIT_AIFC_UTIL~INITIALIZE_CONTENT_COMPRESS')
          EXPORTING
            ir_bal     = lo_log
            is_ifkey   = is_ifkeys
            iv_package = iv_package
            iv_depl_id = ms_icd_data_key-depl_scenario.

      CATCH cx_sy_dyn_call_error INTO lx_dyn_call_error.
        Lcx_abapgit_exception=>raise( iv_text = 'AIFC not supported'
                                      ix_previous = lx_dyn_call_error ).
      CATCH cx_root INTO lx_root.
        Lcx_abapgit_exception=>raise_with_text( lx_root ).
    ENDTRY.
  ENDMETHOD.
  METHOD handle_table_data.
    DATA: lx_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.
    DATA: lx_root TYPE REF TO cx_root.

    TRY.
        CALL METHOD mo_abapgit_util->('/AIF/IF_ABAPGIT_AIFC_UTIL~HANDLE_TABLE_DATA')
          EXPORTING
            iv_tabname = iv_tabname
            it_data    = it_data.

      CATCH cx_sy_dyn_call_error INTO lx_dyn_call_error.
        Lcx_abapgit_exception=>raise( iv_text = 'AIFC not supported'
                                      ix_previous = lx_dyn_call_error ).
      CATCH cx_root INTO lx_root.
        Lcx_abapgit_exception=>raise_with_text( lx_root ).
    ENDTRY.
  ENDMETHOD.
  METHOD validate_interface.
    DATA: lx_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.
    DATA: lx_root TYPE REF TO cx_root.

    rv_success = abap_false.
    TRY.
        CALL METHOD mo_abapgit_util->('/AIF/IF_ABAPGIT_AIFC_UTIL~VALIDATE_INTERFACE')
          EXPORTING
            is_ifkeys  = is_ifkeys
          RECEIVING
            rv_success = rv_success.

      CATCH cx_sy_dyn_call_error INTO lx_dyn_call_error.
        Lcx_abapgit_exception=>raise( iv_text = 'AIFC not supported'
                                      ix_previous = lx_dyn_call_error ).
      CATCH cx_root INTO lx_root.
        Lcx_abapgit_exception=>raise_with_text( lx_root ).
    ENDTRY.
  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    DATA lx_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.

    DATA ls_icd_data_key TYPE ty_icd_data_key.
    ls_icd_data_key-depl_scenario = ms_icd_data_key-depl_scenario.
    ls_icd_data_key-ns = ms_icd_data_key-ns.
    ls_icd_data_key-ifname = ms_icd_data_key-ifname.
    ls_icd_data_key-ifver2 = ms_icd_data_key-ifver2.

    TRY.
        CALL METHOD mo_abapgit_util->('/AIF/IF_ABAPGIT_AIFC_UTIL~CHANGED_BY')
          EXPORTING
            is_key  = ls_icd_data_key
          RECEIVING
            rv_user = rv_user.

      CATCH cx_sy_dyn_call_error INTO lx_dyn_call_error.
        Lcx_abapgit_exception=>raise( iv_text = 'AIFC not supported'
                                      ix_previous = lx_dyn_call_error ).
    ENDTRY.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.
    Lcx_abapgit_exception=>raise( 'Delete not supported.' ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.
    DATA: lx_root TYPE REF TO cx_root.
    DATA: lt_content TYPE ty_content_t.

    DATA lr_tabledescr TYPE REF TO cl_abap_tabledescr.
    DATA lr_structdescr TYPE REF TO cl_abap_structdescr.
    DATA lr_table TYPE REF TO data.
    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <ls_table> TYPE any.

    DATA ls_ifkey TYPE ty_aif_key_s.
    DATA lr_content TYPE REF TO ty_content_s.

    DATA lx_abap_not_a_table TYPE REF TO cx_abap_not_a_table.

    DATA lv_tablename TYPE string.
    FIELD-SYMBOLS: <lv_value> TYPE any.

    IF iv_step <> Lif_abapgit_object=>gc_step_id-abap.
      RETURN.
    ENDIF.

    TRY.
        IF execute_checks( io_xml ) = abap_false.
          Lcx_abapgit_exception=>raise( 'AIF interface checks failed' ).
        ENDIF.

        io_xml->read( EXPORTING
                        iv_name = `Content_table`
                      CHANGING
                        cg_data = lt_content ).


        LOOP AT lt_content REFERENCE INTO lr_content.
          TRY.
              lv_tablename = cl_abap_dyn_prg=>check_table_name_str( val = lr_content->tabname
                                                                    packages = '' ).
            CATCH cx_abap_not_a_table INTO lx_abap_not_a_table.
              Lcx_abapgit_exception=>raise_with_text( lx_abap_not_a_table ).
            CATCH cx_abap_not_in_package.
              "thats fine
          ENDTRY.

          CLEAR lr_tabledescr.
          lr_structdescr ?= cl_abap_typedescr=>describe_by_name( p_name = lr_content->tabname ).
          lr_tabledescr = cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).

          CREATE DATA lr_table TYPE HANDLE lr_tabledescr.
          ASSIGN lr_table->* TO <lt_table>.
          IF sy-subrc <> 0.
            Lcx_abapgit_exception=>raise( 'Fieldsymbol not assigned' ).
          ENDIF.

          io_xml->read( EXPORTING
                          iv_name = lr_content->tabname
                        CHANGING
                          cg_data = <lt_table> ).

          handle_table_data( iv_tabname = lr_content->tabname
                             it_data = <lt_table> ).

          IF lr_content->tabname = '/AIF/T_FINF'.
            READ TABLE <lt_table> ASSIGNING <ls_table> INDEX 1.

            ASSIGN COMPONENT 'NS' OF STRUCTURE <ls_table> TO <lv_value>.
            IF <lv_value> IS ASSIGNED.
              ls_ifkey-ns = <lv_value>.
              UNASSIGN <lv_value>.
            ENDIF.

            ASSIGN COMPONENT 'IFNAME' OF STRUCTURE <ls_table> TO <lv_value>.
            IF <lv_value> IS ASSIGNED.
              ls_ifkey-ifname = <lv_value>.
              UNASSIGN <lv_value>.
            ENDIF.

            ASSIGN COMPONENT 'IFVERSION' OF STRUCTURE <ls_table> TO <lv_value>.
            IF <lv_value> IS ASSIGNED.
              ls_ifkey-ifver = <lv_value>.
              UNASSIGN <lv_value>.
            ENDIF.
          ENDIF.

        ENDLOOP.

        IF ls_ifkey IS INITIAL.
          RETURN.
        ENDIF.

        get_content_compress( io_log = ii_log
                              is_ifkeys = ls_ifkey
                              iv_package = iv_package ).


        IF authorization_check( ) = abap_false.
          RETURN.
        ENDIF.

        IF validate_interface( ls_ifkey ) = abap_false.
          RETURN.
        ENDIF.

        IF compress_interface( ls_ifkey ) = abap_false.
          RETURN.
        ENDIF.

      CATCH cx_root INTO lx_root.
        ii_log->add_exception( ix_exc = lx_root
                               is_item = ms_item ).
    ENDTRY.
  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.
    DATA: lx_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.

    DATA ls_icd_data_key TYPE ty_icd_data_key.

    ls_icd_data_key-depl_scenario = ms_icd_data_key-depl_scenario.
    ls_icd_data_key-ns = ms_icd_data_key-ns.
    ls_icd_data_key-ifname = ms_icd_data_key-ifname.
    ls_icd_data_key-ifver2 = ms_icd_data_key-ifver2.

    rv_bool = abap_false.

    TRY.
        CALL METHOD mo_abapgit_util->('/AIF/IF_ABAPGIT_AIFC_UTIL~EXISTS')
          EXPORTING
            is_key  = ls_icd_data_key
          RECEIVING
            rv_bool = rv_bool.

      CATCH cx_sy_dyn_call_error INTO lx_dyn_call_error.
        Lcx_abapgit_exception=>raise( iv_text = 'AIFC not supported'
                                      ix_previous = lx_dyn_call_error ).
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
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = abap_false.
    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.
    rv_active = abap_true.
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    TYPES: ty_rsparamsl_255_t TYPE STANDARD TABLE OF rsparamsl_255 WITH NON-UNIQUE DEFAULT KEY.

    DATA lv_report TYPE progname VALUE '/AIF/CONTENT_DISPLAY'.
    DATA lt_params TYPE ty_rsparamsl_255_t.
    DATA ls_param LIKE LINE OF lt_params.

    ls_param-selname = 'P_DEPL'.
    ls_param-kind = 'P'.
    ls_param-sign = 'I'.
    ls_param-option = 'EQ'.
    ls_param-low = ms_icd_data_key-depl_scenario.
    APPEND ls_param TO lt_params.

    SUBMIT (lv_report) WITH SELECTION-TABLE lt_params AND RETURN.

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.
    DATA: lx_root TYPE REF TO cx_root.
    DATA: lx_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.

    DATA ls_icd_data_key TYPE ty_icd_data_key.
    DATA lt_ifdata TYPE ty_table_data_t.

    DATA lr_data TYPE REF TO data.
    FIELD-SYMBOLS <ls_data> TYPE any.

    DATA lt_content TYPE ty_content_t.
    DATA ls_content TYPE ty_content_s.
    DATA lr_ifdata TYPE REF TO ty_table_data_s.
    FIELD-SYMBOLS <lt_table> TYPE ANY TABLE.

    TRY.

        ASSIGN lr_data TO <ls_data>.
        IF NOT <ls_data> IS ASSIGNED.
          RETURN.
        ENDIF.

        ls_icd_data_key-depl_scenario = ms_icd_data_key-depl_scenario.
        ls_icd_data_key-ns = ms_icd_data_key-ns.
        ls_icd_data_key-ifname = ms_icd_data_key-ifname.
        ls_icd_data_key-ifver2 = ms_icd_data_key-ifver2.

        TRY.
            CALL METHOD mo_abapgit_util->('/AIF/IF_ABAPGIT_AIFC_UTIL~GET_IF_DATA')
              EXPORTING
                is_key    = ls_icd_data_key
              RECEIVING
                rt_ifdata = lt_ifdata.

          CATCH cx_sy_dyn_call_error INTO lx_dyn_call_error.
            Lcx_abapgit_exception=>raise( iv_text = 'AIFC not supported'
                                          ix_previous = lx_dyn_call_error ).
        ENDTRY.

        LOOP AT lt_ifdata REFERENCE INTO lr_ifdata.

          UNASSIGN <lt_table>.
          ASSIGN lr_ifdata->table_data->* TO <lt_table>.
          IF <lt_table> IS NOT ASSIGNED.
            CONTINUE.
          ENDIF.

          clear_client( CHANGING ct_data = <lt_table> ).

          io_xml->add( iv_name = lr_ifdata->tabname
                       ig_data = <lt_table> ).

          ls_content-tabname = lr_ifdata->tabname.
          APPEND ls_content TO lt_content.

        ENDLOOP.

        io_xml->add( iv_name = `Content_table`
                     ig_data = lt_content ).

      CATCH cx_root INTO lx_root.
        Lcx_abapgit_exception=>raise( iv_text = 'Serialize not possible'
                                      ix_previous = lx_dyn_call_error ).
    ENDTRY.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_AIFC implementation

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
endclass. "ZCL_ABAPGIT_OBJECT_AMSD implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_AREA <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_area=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_area=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_AREA implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lv_user TYPE string.

    SELECT SINGLE tstpnm FROM ('RSDAREA') INTO lv_user.

    rv_user = lv_user.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA:
      lr_area         TYPE REF TO object.

    CREATE OBJECT lr_area TYPE ('CL_NEW_AWB_AREA').

    CALL METHOD lr_area->('IF_RSAWBN_FOLDER_TREE~DELETE_NODE')
      EXPORTING
        i_nodename    = ms_item-obj_name
        i_with_dialog = ''.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error while deleting AREA: { ms_item-obj_name }| ).
    ENDIF.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA:
      lv_nodename   TYPE c LENGTH 40,
      lv_parentname TYPE c LENGTH 40,
      lv_txtsh      TYPE c LENGTH 20,
      lv_txtlg      TYPE c LENGTH 60,
      lr_area       TYPE REF TO object.

    io_xml->read( EXPORTING iv_name = 'NODENAME'
                  CHANGING cg_data = lv_nodename ).

    io_xml->read( EXPORTING iv_name = 'PARENTNAME'
                  CHANGING  cg_data = lv_parentname ).

    io_xml->read( EXPORTING iv_name = 'TXTSH'
                  CHANGING  cg_data = lv_txtsh ).

    io_xml->read( EXPORTING iv_name = 'TXTLG'
                  CHANGING  cg_data = lv_txtlg ).

    CREATE OBJECT lr_area TYPE ('CL_NEW_AWB_AREA').

    CALL METHOD lr_area->('IF_RSAWBN_FOLDER_TREE~CREATE_NODE')
      EXPORTING
        i_parentname = lv_parentname
        i_nodename   = lv_nodename
        i_txtsh      = lv_txtsh
        i_txtlg      = lv_txtlg.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error while creating AREA: { ms_item-obj_name }| ).
    ENDIF.

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA:
      lr_area     TYPE REF TO object,
      lr_tab_tree TYPE REF TO data,
      lr_str_tee  TYPE REF TO data.

    FIELD-SYMBOLS:
      <lt_tree> TYPE STANDARD TABLE,
      <ls_tree> TYPE any.

    CREATE OBJECT lr_area TYPE ('CL_NEW_AWB_AREA').

    CREATE DATA lr_tab_tree TYPE STANDARD TABLE OF ('RSAWBN_S_TREEORG').
    ASSIGN lr_tab_tree->* TO <lt_tree>.

    CREATE DATA lr_str_tee TYPE STANDARD TABLE OF ('RSAWBN_S_TREEORG').
    ASSIGN lr_str_tee->* TO <ls_tree>.

    CALL METHOD lr_area->('IF_RSAWBN_FOLDER_TREE~GET_TREE')
      EXPORTING
        i_objvers = ''
        i_langu   = ''
      IMPORTING
        e_t_tree  = <lt_tree>.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error while read AREA tree| ).
    ENDIF.

    READ TABLE <lt_tree> WITH KEY ('NODENAME') = ms_item-obj_name ASSIGNING <ls_tree>.

    IF sy-subrc = 0.
      rv_bool = abap_true.
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

    DATA:
      lr_area     TYPE REF TO object,
      lr_tab_tree TYPE REF TO data,
      lr_str_tee  TYPE REF TO data.

    FIELD-SYMBOLS:
      <lt_tree> TYPE STANDARD TABLE,
      <ls_tree> TYPE any.

    CREATE OBJECT lr_area TYPE ('CL_NEW_AWB_AREA').

    CREATE DATA lr_tab_tree TYPE STANDARD TABLE OF ('RSAWBN_S_TREEORG').
    ASSIGN lr_tab_tree->* TO <lt_tree>.

    CREATE DATA lr_str_tee TYPE STANDARD TABLE OF ('RSAWBN_S_TREEORG').
    ASSIGN lr_str_tee->* TO <ls_tree>.

    CALL METHOD lr_area->('IF_RSAWBN_FOLDER_TREE~GET_TREE')
      EXPORTING
        i_objvers = 'A'
        i_langu   = mv_language
      IMPORTING
        e_t_tree  = <lt_tree>.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error while read AREA tree| ).
    ENDIF.

    READ TABLE <lt_tree> WITH KEY ('NODENAME') = ms_item-obj_name ASSIGNING <ls_tree>.

    IF sy-subrc = 0.
      rv_active = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = exists_a_lock_entry_for( 'ERSDAREA' ).
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

    DATA:
      lr_area     TYPE REF TO object,
      lr_tab_tree TYPE REF TO data,
      lr_str_tee  TYPE REF TO data,
      lr_rsdareat TYPE REF TO data,
      lv_select   TYPE string.

    FIELD-SYMBOLS:
      <lt_tree>       TYPE STANDARD TABLE,
      <ls_tree>       TYPE any,
      <lv_parentname> TYPE any,
      <ls_rsdareat>   TYPE any,
      <lv_txtlg>      TYPE any,
      <lv_txtsh>      TYPE any.

    CREATE OBJECT lr_area TYPE ('CL_NEW_AWB_AREA').

    CREATE DATA lr_tab_tree TYPE STANDARD TABLE OF ('RSAWBN_S_TREEORG').
    ASSIGN lr_tab_tree->* TO <lt_tree>.

    CREATE DATA lr_str_tee TYPE STANDARD TABLE OF ('RSAWBN_S_TREEORG').
    ASSIGN lr_str_tee->* TO <ls_tree>.

    CREATE DATA lr_rsdareat TYPE ('RSDAREAT').
    ASSIGN lr_rsdareat->* TO <ls_rsdareat>.

    CALL METHOD lr_area->('IF_RSAWBN_FOLDER_TREE~GET_TREE')
      EXPORTING
        i_objvers = 'A'
        i_langu   = mv_language
      IMPORTING
        e_t_tree  = <lt_tree>.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error while read AREA tree| ).
    ENDIF.

    READ TABLE <lt_tree> WITH KEY ('NODENAME') = ms_item-obj_name ASSIGNING <ls_tree>.

    lv_select = |INFOAREA = '{ ms_item-obj_name }'|.

    SELECT SINGLE * FROM ('RSDAREAT')
    INTO <ls_rsdareat>
    WHERE infoarea = ms_item-obj_name.

    ASSIGN COMPONENT 'TXTSH' OF STRUCTURE <ls_rsdareat> TO <lv_txtsh>.
    ASSIGN COMPONENT 'TXTLG' OF STRUCTURE <ls_rsdareat> TO <lv_txtlg>.


    ASSIGN COMPONENT 'PARENTNAME' OF STRUCTURE <ls_tree> TO <lv_parentname>.

    io_xml->add( iv_name = 'NODENAME'
                 ig_data = ms_item-obj_name ).

    io_xml->add( iv_name = 'PARENTNAME'
                 ig_data = <lv_parentname> ).

    io_xml->add( iv_name = 'TXTSH'
                 ig_data = <lv_txtsh> ).

    io_xml->add( iv_name = 'TXTLG'
                 ig_data = <lv_txtlg> ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_AREA implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ASFC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_asfc=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_asfc=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ASFC implementation.
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
endclass. "ZCL_ABAPGIT_OBJECT_ASFC implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_AUTH <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_auth=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_auth=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_AUTH implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_fieldname = ms_item-obj_name.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
* looks like "changed by user" is not stored in the database
    rv_user = c_user_unknown.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    " there is a bug in SAP standard, the TADIR entries are not deleted
    " when the AUTH object is deleted in transaction SU20

    " FM SUSR_AUTF_DELETE_FIELD calls the UI, therefore we reimplement its logic

    DATA:
      lt_objlst TYPE susr_t_xuobject,
      lo_auth   TYPE REF TO cl_auth_tools.

    " authority check
    CREATE OBJECT lo_auth.
    IF lo_auth->authority_check_suso( actvt     = '06'
                                      fieldname = mv_fieldname ) <> 0.
      MESSAGE e463(01) WITH mv_fieldname INTO Lcx_abapgit_exception=>null.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    " if field is used check
    lt_objlst = lo_auth->suso_where_used_afield( mv_fieldname ).
    IF lt_objlst IS NOT INITIAL.
      MESSAGE i453(01) WITH mv_fieldname INTO Lcx_abapgit_exception=>null.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    " collect fieldname into a transport task
    IF lo_auth->add_afield_to_trkorr( mv_fieldname ) <> 0.
      "no transport -> no deletion
      MESSAGE e507(0m) INTO Lcx_abapgit_exception=>null.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    DELETE FROM authx WHERE fieldname = mv_fieldname.
    IF sy-subrc <> 0.
      MESSAGE e507(0m) INTO Lcx_abapgit_exception=>null.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.
* see include LSAUT_FIELDF02

    DATA: ls_authx TYPE authx,
          lo_auth  TYPE REF TO cl_auth_tools.


    io_xml->read( EXPORTING iv_name = 'AUTHX'
                  CHANGING cg_data = ls_authx ).

    tadir_insert( iv_package ).

    CREATE OBJECT lo_auth.

    IF lo_auth->add_afield_to_trkorr( ls_authx-fieldname ) <> 0.
      Lcx_abapgit_exception=>raise( 'Error deserializing AUTH' ).
    ENDIF.

    MODIFY authx FROM ls_authx.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Error deserializing AUTH' ).
    ENDIF.

    CALL FUNCTION 'DB_COMMIT'.
    lo_auth->set_authfld_info_from_db( ls_authx-fieldname ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    SELECT SINGLE fieldname FROM authx
      INTO mv_fieldname
      WHERE fieldname = ms_item-obj_name.               "#EC CI_GENBUFF
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
    IF Lcl_abapgit_factory=>get_function_module( )->function_exists( 'SU20_MAINTAIN_SNGL' ) = abap_true.
      " this function module does not exist in 740
      CALL FUNCTION 'SU20_MAINTAIN_SNGL'
        EXPORTING
          id_field    = mv_fieldname
          id_wbo_mode = abap_false.
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

    DATA: ls_authx TYPE authx.


    SELECT SINGLE * FROM authx INTO ls_authx
      WHERE fieldname = ms_item-obj_name.               "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    io_xml->add( iv_name = 'AUTHX'
                 ig_data = ls_authx ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_AUTH implementation

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
endclass. "ZCL_ABAPGIT_OBJECT_AVAR implementation

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

    rv_is_locked = exists_a_lock_entry_for(
      iv_lock_object = 'CLS_ENQUEUE_STRU'
      iv_argument    = |{ ms_item-obj_name }| ).

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
endclass. "ZCL_ABAPGIT_OBJECT_AVAS implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_BDEF <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_bdef=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_bdef=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_BDEF implementation.
*"* method's implementations
*include methods.
  METHOD clear_field.

    FIELD-SYMBOLS: <lv_value> TYPE data.

    ASSIGN COMPONENT iv_fieldname OF STRUCTURE cs_metadata
           TO <lv_value>.
    ASSERT sy-subrc = 0.

    CLEAR: <lv_value>.

  ENDMETHOD.
  METHOD clear_fields.

    FIELD-SYMBOLS: <lv_links> TYPE ANY TABLE.
    FIELD-SYMBOLS: <lv_value> TYPE data.
    FIELD-SYMBOLS <ls_item> TYPE any.

    clear_field(
      EXPORTING
        iv_fieldname          = 'VERSION'
      CHANGING
        cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'CREATED_AT'
      CHANGING
        cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'CREATED_BY'
      CHANGING
        cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'CHANGED_AT'
      CHANGING
        cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'CHANGED_BY'
      CHANGING
        cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'RESPONSIBLE'
      CHANGING
        cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'PACKAGE_REF'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'CONTAINER_REF'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'MASTER_SYSTEM'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'MAIN_OBJECT-CHANGED_AT'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'MAIN_OBJECT-CHANGED_BY'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'MAIN_OBJECT-CREATED_AT'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'MAIN_OBJECT-CREATED_BY'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'MAIN_OBJECT-RESPONSIBLE'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'MAIN_OBJECT-PACKAGE_REF'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'MAIN_OBJECT-CONTAINER_REF'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'MAIN_OBJECT-MASTER_SYSTEM'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'SYNTAX_CONFIGURATION'
      CHANGING
      cs_metadata = cs_metadata ).

    ASSIGN COMPONENT 'LINKS' OF STRUCTURE cs_metadata TO <lv_links>.
    ASSERT sy-subrc = 0.

    LOOP AT <lv_links> ASSIGNING <ls_item>.
      ASSIGN COMPONENT 'COMMON_ATTRIBUTES' OF STRUCTURE <ls_item> TO <lv_value>.
      ASSERT sy-subrc = 0.
      CLEAR: <lv_value>.
    ENDLOOP.

  ENDMETHOD.
  METHOD constructor.

    super->constructor(
        is_item     = is_item
        iv_language = iv_language ).

    mv_behaviour_definition_key = ms_item-obj_name.

    TRY.
        CREATE DATA mr_behaviour_definition TYPE ('CL_BLUE_SOURCE_OBJECT_DATA=>TY_OBJECT_DATA').
        CREATE OBJECT mi_persistence TYPE ('CL_BDEF_OBJECT_PERSIST').

      CATCH cx_sy_create_error.
        Lcx_abapgit_exception=>raise( |BDEF not supported by your NW release| ).
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

    CREATE DATA lr_data TYPE ('CL_BLUE_SOURCE_OBJECT_DATA=>TY_OBJECT_DATA').
    ASSIGN lr_data->* TO <lg_data>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'METADATA' OF STRUCTURE <lg_data> TO <lv_metadata_node>.
    ASSERT sy-subrc = 0.

    CREATE DATA lr_metadata  TYPE ('CL_BLUE_SOURCE_OBJECT_DATA=>TY_OBJECT_DATA-METADATA').
    ASSIGN lr_metadata->* TO <ls_metadata>.
    ASSERT sy-subrc = 0.

    io_xml->read(
      EXPORTING
        iv_name = 'BDEF'
      CHANGING
        cg_data = <ls_metadata> ).

    <lv_metadata_node> = <ls_metadata>.

    ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <lg_data> TO <lv_source>.
    ASSERT sy-subrc = 0.

    <lv_source> = Lif_abapgit_object~mo_files->read_string( 'asbdef' ).

    CREATE OBJECT ro_object_data TYPE ('CL_BLUE_SOURCE_OBJECT_DATA').

    ro_object_data->set_data( p_data = <lg_data> ).

  ENDMETHOD.
  METHOD get_wb_object_operator.

    DATA:
      ls_object_type TYPE wbobjtype,
      lx_error       TYPE REF TO cx_root.

    IF mi_wb_object_operator IS BOUND.
      ri_wb_object_operator = mi_wb_object_operator.
    ENDIF.

    ls_object_type-objtype_tr = 'BDEF'.
    ls_object_type-subtype_wb = 'BDO'.

    TRY.
        CALL METHOD ('CL_WB_OBJECT_OPERATOR')=>('CREATE_INSTANCE')
          EXPORTING
            object_type = ls_object_type
            object_key  = mv_behaviour_definition_key
          RECEIVING
            result      = mi_wb_object_operator.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    ri_wb_object_operator = mi_wb_object_operator.

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

    CREATE OBJECT lo_object_data TYPE ('CL_BLUE_SOURCE_OBJECT_DATA').
    lo_object_data = io_object_data.

    CREATE DATA lr_new TYPE ('CL_BLUE_SOURCE_OBJECT_DATA=>TY_OBJECT_DATA').
    ASSIGN lr_new->* TO <ls_new>.
    ASSERT sy-subrc = 0.

    CREATE DATA lr_old TYPE ('CL_BLUE_SOURCE_OBJECT_DATA=>TY_OBJECT_DATA').
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

    CREATE OBJECT ro_object_data_merged TYPE ('CL_BLUE_SOURCE_OBJECT_DATA').

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
      lo_object_data_merged TYPE REF TO if_wb_object_data_model,
      lo_wb_object_operator TYPE REF TO object,
      lx_error              TYPE REF TO cx_root,
      lr_wbobjtype          TYPE REF TO data,
      lr_category           TYPE REF TO data.

    FIELD-SYMBOLS:
      <ls_wbobjtype> TYPE any,
      <lv_category>  TYPE any,
      <lv_field>     TYPE any.

    TRY.

        lo_object_data = get_object_data( io_xml ).

        CREATE DATA lr_wbobjtype TYPE ('WBOBJTYPE').
        ASSIGN lr_wbobjtype->* TO <ls_wbobjtype>.
        ASSIGN COMPONENT 'OBJTYPE_TR' OF STRUCTURE <ls_wbobjtype> TO <lv_field>.
        <lv_field> = 'BDEF'.
        ASSIGN COMPONENT 'SUBTYPE_WB' OF STRUCTURE <ls_wbobjtype> TO <lv_field>.
        <lv_field> = 'BDO'.

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
                  data_selection    = 'AL' " if_wb_object_data_selection_co=>c_all_data
                  version           = 'I'
                  package           = iv_package
                  transport_request = iv_transport.
            WHEN '2'. "if_wb_adt_plugin_resource_co=>co_sfs_res_category_compound_s.
              CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~CREATE')
                EXPORTING
                  io_object_data    = lo_object_data
                  data_selection    = 'P' " if_wb_object_data_selection_co=>c_properties
                  version           = 'I'
                  package           = iv_package
                  transport_request = iv_transport.

              CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
                EXPORTING
                  io_object_data    = lo_object_data
                  data_selection    = 'D' "if_wb_object_data_selection_co=>c_data_content
                  version           = 'I'
                  transport_request = iv_transport.
            WHEN OTHERS.
          ENDCASE.
        ELSE.
          lo_object_data_merged = merge_object_data( lo_object_data ).
          CASE <lv_category>.
            WHEN '1'. "if_wb_adt_plugin_resource_co=>co_sfs_res_category_atomic.
              CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
                EXPORTING
                  io_object_data    = lo_object_data_merged
                  data_selection    = 'AL' "if_wb_object_data_selection_co=>c_all_data
                  version           = 'I'
                  transport_request = iv_transport.
            WHEN '2'. "if_wb_adt_plugin_resource_co=>co_sfs_res_category_compound_s.
              CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
                EXPORTING
                  io_object_data    = lo_object_data_merged
                  data_selection    = 'P' "if_wb_object_data_selection_co=>c_properties
                  version           = 'I'
                  transport_request = iv_transport.
              CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
                EXPORTING
                  io_object_data    = lo_object_data_merged
                  data_selection    = 'D' "if_wb_object_data_selection_co=>c_data_content
                  version           = 'I'
                  transport_request = iv_transport.
            WHEN OTHERS.
          ENDCASE.
        ENDIF.

        corr_insert( iv_package ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    TRY.
        mi_persistence->get(
            p_object_key              = mv_behaviour_definition_key
            p_version                 = 'A'
            p_existence_check_only    = abap_true ).
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
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESDIC'
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
      li_object_data_model  TYPE REF TO if_wb_object_data_model,
      li_wb_object_operator TYPE REF TO object,
      lx_error              TYPE REF TO cx_root,
      lv_source             TYPE string.

    FIELD-SYMBOLS:
      <ls_behaviour_definition> TYPE any,
      <lv_metadata>             TYPE any,
      <lv_source>               TYPE string.

    ASSIGN mr_behaviour_definition->* TO <ls_behaviour_definition>.
    ASSERT sy-subrc = 0.

    li_wb_object_operator = get_wb_object_operator( ).

    TRY.
        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
          EXPORTING
            version        = 'A'
          IMPORTING
            data           = <ls_behaviour_definition>
            eo_object_data = li_object_data_model.

        ASSIGN COMPONENT 'METADATA' OF STRUCTURE <ls_behaviour_definition> TO <lv_metadata>.
        ASSERT sy-subrc = 0.
        clear_fields( CHANGING cs_metadata = <lv_metadata> ).

        ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <ls_behaviour_definition> TO <lv_source>.
        ASSERT sy-subrc = 0.
        lv_source = <lv_source>.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    io_xml->add(
        iv_name = 'BDEF'
        ig_data = <lv_metadata> ).

    Lif_abapgit_object~mo_files->add_string(
        iv_ext    = 'asbdef'
        iv_string = lv_source ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_BDEF implementation

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
endclass. "ZCL_ABAPGIT_OBJECT_CHAR implementation

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
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
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
endclass. "ZCL_ABAPGIT_OBJECT_CHDO implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_CLAS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_clas=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_clas=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_CLAS implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    CREATE OBJECT mi_object_oriented_object_fct TYPE Lcl_abapgit_oo_class.

    mv_classpool_name = cl_oo_classname_service=>get_classpool_name( |{ is_item-obj_name }| ).

  ENDMETHOD.
  METHOD deserialize_abap.

    DATA: ls_vseoclass             TYPE vseoclass,
          lt_source                TYPE seop_source_string,
          lt_local_definitions     TYPE seop_source_string,
          lt_local_implementations TYPE seop_source_string,
          lt_local_macros          TYPE seop_source_string,
          lt_test_classes          TYPE seop_source_string,
          lt_descriptions          TYPE Lif_abapgit_oo_object_fnc=>ty_seocompotx_tt,
          lt_descriptions_sub      TYPE Lif_abapgit_oo_object_fnc=>ty_seosubcotx_tt,
          ls_class_key             TYPE seoclskey,
          lt_attributes            TYPE Lif_abapgit_definitions=>ty_obj_attribute_tt.


    lt_source = Lif_abapgit_object~mo_files->read_abap( ).

    lt_local_definitions = Lif_abapgit_object~mo_files->read_abap(
      iv_extra = Lif_abapgit_oo_object_fnc=>c_parts-locals_def
      iv_error = abap_false ).

    lt_local_implementations = Lif_abapgit_object~mo_files->read_abap(
      iv_extra = Lif_abapgit_oo_object_fnc=>c_parts-locals_imp
      iv_error = abap_false ).

    lt_local_macros = Lif_abapgit_object~mo_files->read_abap(
      iv_extra = Lif_abapgit_oo_object_fnc=>c_parts-macros
      iv_error = abap_false ).

    lt_test_classes = Lif_abapgit_object~mo_files->read_abap(
      iv_extra = Lif_abapgit_oo_object_fnc=>c_parts-testclasses
      iv_error = abap_false ).

    ls_class_key-clsname = ms_item-obj_name.

    ii_xml->read( EXPORTING iv_name = 'VSEOCLASS'
                  CHANGING  cg_data = ls_vseoclass ).

    set_abap_language_version( CHANGING cv_abap_language_version = ls_vseoclass-unicode ).

    ii_xml->read( EXPORTING iv_name = 'ATTRIBUTES'
                  CHANGING  cg_data = lt_attributes ).

    " Remove code for test classes if they have been deleted
    IF ls_vseoclass-with_unit_tests = abap_false.
      CLEAR lt_test_classes.
    ENDIF.

    mi_object_oriented_object_fct->create(
      EXPORTING
        iv_check      = abap_true
        iv_package    = iv_package
        it_attributes = lt_attributes
      CHANGING
        cg_properties = ls_vseoclass ).

    mi_object_oriented_object_fct->generate_locals(
      is_key                   = ls_class_key
      iv_package               = iv_package
      iv_version               = ls_vseoclass-unicode
      it_local_definitions     = lt_local_definitions
      it_local_implementations = lt_local_implementations
      it_local_macros          = lt_local_macros
      it_local_test_classes    = lt_test_classes ).

    repo_apack_replacement( CHANGING ct_source = lt_source ).

    mi_object_oriented_object_fct->deserialize_source(
      is_key     = ls_class_key
      iv_package = iv_package
      iv_version = ls_vseoclass-unicode
      it_source  = lt_source ).

    ii_xml->read( EXPORTING iv_name = 'DESCRIPTIONS'
                  CHANGING cg_data = lt_descriptions ).

    mi_object_oriented_object_fct->update_descriptions(
      is_key          = ls_class_key
      it_descriptions = lt_descriptions ).

    ii_xml->read( EXPORTING iv_name = 'DESCRIPTIONS_SUB'
                  CHANGING cg_data = lt_descriptions_sub ).

    mi_object_oriented_object_fct->update_descriptions_sub(
      is_key          = ls_class_key
      it_descriptions = lt_descriptions_sub ).

    mi_object_oriented_object_fct->add_to_activation_list( ms_item ).

  ENDMETHOD.
  METHOD deserialize_docu.

    DATA: lt_lines      TYPE tlinetab,
          lv_object     TYPE dokhl-object,
          lt_i18n_lines TYPE Lif_abapgit_lang_definitions=>ty_i18n_lines,
          ls_i18n_lines TYPE Lif_abapgit_lang_definitions=>ty_i18n_line.

    ii_xml->read( EXPORTING iv_name = 'LINES'
                  CHANGING cg_data = lt_lines ).

    lv_object = ms_item-obj_name.

    IF lines( lt_lines ) = 0.
      mi_object_oriented_object_fct->delete_documentation(
        iv_id          = c_longtext_id-class
        iv_object_name = lv_object
        iv_language    = mv_language ).
      RETURN.
    ENDIF.

    mi_object_oriented_object_fct->create_documentation(
      it_lines       = lt_lines
      iv_id          = c_longtext_id-class
      iv_object_name = lv_object
      iv_language    = mv_language ).

    ii_xml->read( EXPORTING iv_name = 'I18N_LINES'
                  CHANGING cg_data = lt_i18n_lines ).

    LOOP AT lt_i18n_lines INTO ls_i18n_lines.
      mi_object_oriented_object_fct->create_documentation(
        it_lines         = ls_i18n_lines-lines
        iv_id            = c_longtext_id-class
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

    deserialize_longtexts(
      ii_xml           = ii_xml
      iv_longtext_name = c_longtext_name-types
      iv_longtext_id   = c_longtext_id-types ).

  ENDMETHOD.
  METHOD deserialize_pre_ddic.

    DATA: ls_vseoclass TYPE vseoclass.

    ii_xml->read( EXPORTING iv_name = 'VSEOCLASS'
                  CHANGING  cg_data = ls_vseoclass ).

    set_abap_language_version( CHANGING cv_abap_language_version = ls_vseoclass-unicode ).

    mi_object_oriented_object_fct->create(
      EXPORTING
        iv_check      = abap_false
        iv_package    = iv_package
      CHANGING
        cg_properties = ls_vseoclass ).

  ENDMETHOD.
  METHOD deserialize_sotr.
    "OTR stands for Online Text Repository
    mi_object_oriented_object_fct->create_sotr(
      iv_object_name = ms_item-obj_name
      iv_package     = iv_package
      ii_xml         = ii_xml ).
  ENDMETHOD.
  METHOD deserialize_tpool.

    DATA: lv_clsname   TYPE seoclsname,
          lt_tpool_ext TYPE Lif_abapgit_definitions=>ty_tpool_tt,
          lt_tpool     TYPE textpool_table.

    ii_xml->read( EXPORTING iv_name = 'TPOOL'
                  CHANGING cg_data = lt_tpool_ext ).
    lt_tpool = read_tpool( lt_tpool_ext ).

    IF lines( lt_tpool ) = 0.
      RETURN.
    ENDIF.

    lv_clsname = ms_item-obj_name.

    mi_object_oriented_object_fct->insert_text_pool(
      iv_class_name = lv_clsname
      it_text_pool  = lt_tpool
      iv_language   = mv_language ).

  ENDMETHOD.
  METHOD deserialize_tpool_i18n.

    DATA: lv_clsname    TYPE seoclsname,
          lt_tpool      TYPE textpool_table,
          lt_i18n_tpool TYPE Lif_abapgit_lang_definitions=>ty_i18n_tpools,
          ls_i18n_tpool TYPE Lif_abapgit_lang_definitions=>ty_i18n_tpool.

    lv_clsname = ms_item-obj_name.

    ii_xml->read( EXPORTING iv_name = 'I18N_TPOOL'
                  CHANGING  cg_data = lt_i18n_tpool ).

    mo_i18n_params->trim_saplang_keyed_table(
      EXPORTING
        iv_lang_field_name = 'LANGUAGE'
      CHANGING
        ct_tab = lt_i18n_tpool ).

    LOOP AT lt_i18n_tpool INTO ls_i18n_tpool.
      lt_tpool = read_tpool( ls_i18n_tpool-textpool ).
      mi_object_oriented_object_fct->insert_text_pool(
        iv_class_name = lv_clsname
        it_text_pool  = lt_tpool
        iv_language   = ls_i18n_tpool-language
        iv_state      = 'A' ).
    ENDLOOP.

  ENDMETHOD.
  METHOD interface_replacement.

    DATA lv_tabix TYPE sy-tabix.

    FIELD-SYMBOLS <lv_source> LIKE LINE OF ct_source.

    FIND REGEX '^\s*INTERFACES(:| )\s*' && iv_from_interface && '\s*.' IN TABLE ct_source MATCH LINE lv_tabix.
    IF sy-subrc = 0.
      READ TABLE ct_source ASSIGNING <lv_source> INDEX lv_tabix.
      ASSERT sy-subrc = 0.

      REPLACE FIRST OCCURRENCE OF iv_from_interface IN <lv_source>
                             WITH iv_to_interface IGNORING CASE.

      REPLACE ALL OCCURRENCES OF iv_from_interface && '~descriptor' IN TABLE ct_source
                            WITH iv_to_interface && '~descriptor' IGNORING CASE.
      REPLACE ALL OCCURRENCES OF iv_from_interface && '=>' IN TABLE ct_source
                            WITH iv_to_interface && '=>' IGNORING CASE.
      REPLACE ALL OCCURRENCES OF iv_from_interface && '->' IN TABLE ct_source
                            WITH iv_to_interface && '->' IGNORING CASE.
    ENDIF.

  ENDMETHOD.
  METHOD is_class_locked.

    DATA: lv_argument TYPE seqg3-garg.

    lv_argument = ms_item-obj_name.
    OVERLAY lv_argument WITH '=============================='.
    lv_argument = lv_argument && '*'.

    rv_is_class_locked = exists_a_lock_entry_for( iv_lock_object = 'ESEOCLASS'
                                                  iv_argument    = lv_argument ).

  ENDMETHOD.
  METHOD repo_apack_replacement.

    DATA lv_apack TYPE seoclsname.

    " Check if SAP-version of APACK manifest exists
    SELECT SINGLE clsname INTO lv_apack
      FROM seoclass
      WHERE clsname = Lif_abapgit_apack_definitions=>c_apack_interface_sap.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    " If not, replace with abapGit version
    interface_replacement(
      EXPORTING
        iv_from_interface = to_lower( Lif_abapgit_apack_definitions=>c_apack_interface_sap )
        iv_to_interface   = to_lower( Lif_abapgit_apack_definitions=>c_apack_interface_cust )
      CHANGING
        ct_source         = ct_source ).

  ENDMETHOD.
  METHOD serialize_attr.

    DATA: lt_attributes TYPE Lif_abapgit_definitions=>ty_obj_attribute_tt.

    lt_attributes = mi_object_oriented_object_fct->read_attributes( iv_clsname ).
    IF lines( lt_attributes ) = 0.
      RETURN.
    ENDIF.

    ii_xml->add( iv_name = 'ATTRIBUTES'
                 ig_data = lt_attributes ).

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

    IF lines( lt_descriptions ) = 0.
      RETURN.
    ENDIF.
    " Remove technical languages
    lt_language_filter = mo_i18n_params->build_language_filter( ).
    DELETE lt_descriptions WHERE NOT langu IN lt_language_filter AND langu <> mv_language.

    ii_xml->add( iv_name = 'DESCRIPTIONS'
                 ig_data = lt_descriptions ).

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

    IF lines( lt_descriptions ) = 0.
      RETURN.
    ENDIF.
    " Remove technical languages
    lt_language_filter = mo_i18n_params->build_language_filter( ).
    DELETE lt_descriptions WHERE NOT langu IN lt_language_filter AND langu <> mv_language.

    ii_xml->add( iv_name = 'DESCRIPTIONS_SUB'
                 ig_data = lt_descriptions ).

  ENDMETHOD.
  METHOD serialize_docu.

    DATA: lt_lines      TYPE tlinetab,
          lv_object     TYPE dokhl-object,
          lv_langu      TYPE sy-langu,
          lt_i18n_lines TYPE Lif_abapgit_lang_definitions=>ty_i18n_lines,
          ls_i18n_lines TYPE Lif_abapgit_lang_definitions=>ty_i18n_line.

    lv_object = iv_clsname.

    lt_lines = mi_object_oriented_object_fct->read_documentation(
      iv_id          = c_longtext_id-class
      iv_object_name = lv_object
      iv_language    = mv_language ).
    IF lines( lt_lines ) > 0.
      ii_xml->add( iv_name = 'LINES'
                   ig_data = lt_lines ).
    ENDIF.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      RETURN.
    ENDIF.

    LOOP AT it_langu_additional INTO lv_langu.

      lt_lines = mi_object_oriented_object_fct->read_documentation(
        iv_id          = c_longtext_id-class
        iv_object_name = lv_object
        iv_language    = lv_langu ).

      IF lines( lt_lines ) > 0.
        CLEAR ls_i18n_lines.
        ls_i18n_lines-language = lv_langu.
        ls_i18n_lines-lines    = lt_lines.
        INSERT ls_i18n_lines INTO TABLE lt_i18n_lines.
      ENDIF.

    ENDLOOP.

    IF lines( lt_i18n_lines ) > 0.
      ii_xml->add( iv_name = 'I18N_LINES'
                   ig_data = lt_i18n_lines ).
    ENDIF.

    serialize_longtexts(
      ii_xml           = ii_xml
      iv_longtext_name = c_longtext_name-attributes
      iv_longtext_id   = c_longtext_id-attributes ).

    serialize_longtexts(
      ii_xml           = ii_xml
      iv_longtext_name = c_longtext_name-methods
      iv_longtext_id   = c_longtext_id-methods ).

    serialize_longtexts(
      ii_xml           = ii_xml
      iv_longtext_name = c_longtext_name-events
      iv_longtext_id   = c_longtext_id-events ).

    serialize_longtexts(
      ii_xml           = ii_xml
      iv_longtext_name = c_longtext_name-types
      iv_longtext_id   = c_longtext_id-types ).

  ENDMETHOD.
  METHOD serialize_sotr.
    mi_object_oriented_object_fct->read_sotr(
      iv_object_name = ms_item-obj_name
      io_i18n_params = mo_i18n_params
      ii_xml         = ii_xml ).
  ENDMETHOD.
  METHOD serialize_tpool.

    DATA lt_tpool TYPE textpool_table.

    lt_tpool = mi_object_oriented_object_fct->read_text_pool(
      iv_class_name = iv_clsname
      iv_language   = mv_language ).
    ii_xml->add( iv_name = 'TPOOL'
                 ig_data = add_tpool( lt_tpool ) ).

    rt_tpool = lt_tpool.

  ENDMETHOD.
  METHOD serialize_tpool_i18n.

    DATA: lt_tpool      TYPE textpool_table,
          lv_index      TYPE i,
          lv_langu      TYPE sy-langu,
          lt_i18n_tpool TYPE Lif_abapgit_lang_definitions=>ty_i18n_tpools,
          ls_i18n_tpool TYPE Lif_abapgit_lang_definitions=>ty_i18n_tpool.

    FIELD-SYMBOLS <ls_tpool> LIKE LINE OF it_tpool_main.

    DATA lt_tpool_main LIKE SORTED TABLE OF <ls_tpool> WITH UNIQUE KEY id key.

    IF mo_i18n_params->ms_params-main_language_only = abap_true OR lines( it_tpool_main ) = 0.
      RETURN.
    ENDIF.

    " Copy single records to be able to catch duplicate key error
    LOOP AT it_tpool_main ASSIGNING <ls_tpool>.
      INSERT <ls_tpool> INTO TABLE lt_tpool_main.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Inconsistent textpool in { ms_item-obj_type } { ms_item-obj_name }| ).
      ENDIF.
    ENDLOOP.

    LOOP AT it_langu_additional INTO lv_langu.

      lt_tpool = mi_object_oriented_object_fct->read_text_pool(
        iv_class_name = iv_clsname
        iv_language   = lv_langu ).

      LOOP AT lt_tpool ASSIGNING <ls_tpool>.
        lv_index = sy-tabix.
        READ TABLE lt_tpool_main WITH KEY id = <ls_tpool>-id key = <ls_tpool>-key
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          DELETE lt_tpool INDEX lv_index.
        ENDIF.
      ENDLOOP.

      IF lines( lt_tpool ) > 0.
        CLEAR ls_i18n_tpool.
        ls_i18n_tpool-language = lv_langu.
        ls_i18n_tpool-textpool = add_tpool( lt_tpool ).
        INSERT ls_i18n_tpool INTO TABLE lt_i18n_tpool.
      ENDIF.

    ENDLOOP.

    IF lines( lt_i18n_tpool ) > 0.
      ii_xml->add( iv_name = 'I18N_TPOOL'
                   ig_data = lt_i18n_tpool ).
    ENDIF.

  ENDMETHOD.
  METHOD serialize_xml.

    DATA: ls_vseoclass        TYPE vseoclass,
          lt_tpool            TYPE textpool_table,
          ls_clskey           TYPE seoclskey,
          lt_langu_additional TYPE Lif_abapgit_lang_definitions=>ty_langus,
          lt_language_filter  TYPE Lif_abapgit_environment=>ty_system_language_filter.

    ls_clskey-clsname = ms_item-obj_name.

    "If class was deserialized with a previous versions of abapGit and current language was different
    "from main language at this time, this call would return SY-LANGU as main language. To fix
    "these objects, set SY-LANGU to main language temporarily.
    Lcl_abapgit_language=>set_current_language( mv_language ).

    TRY.
        ls_vseoclass = mi_object_oriented_object_fct->get_class_properties( ls_clskey ).

        clear_abap_language_version( CHANGING cv_abap_language_version = ls_vseoclass-unicode ).

      CLEANUP.
        Lcl_abapgit_language=>restore_login_language( ).

    ENDTRY.

    Lcl_abapgit_language=>restore_login_language( ).

    IF mv_skip_testclass = abap_true.
      CLEAR ls_vseoclass-with_unit_tests.
    ENDIF.

    " Table d010tinf stores info. on languages in which program is maintained
    " Select all active translations of program texts
    " Skip main language - it was already serialized
    lt_language_filter = mo_i18n_params->build_language_filter( ).

    SELECT DISTINCT language
      INTO TABLE lt_langu_additional
      FROM d010tinf
      WHERE r3state  = 'A'
        AND prog     = mv_classpool_name
        AND language IN lt_language_filter
        AND language <> mv_language
      ORDER BY language.

    ii_xml->add( iv_name = 'VSEOCLASS'
                 ig_data = ls_vseoclass ).

    lt_tpool = serialize_tpool(
      ii_xml     = ii_xml
      iv_clsname = ls_clskey-clsname ).

    IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
      serialize_tpool_i18n(
        ii_xml              = ii_xml
        it_langu_additional = lt_langu_additional
        it_tpool_main       = lt_tpool
        iv_clsname          = ls_clskey-clsname ).
    ENDIF.

    IF ls_vseoclass-category = seoc_category_exception.
      serialize_sotr( ii_xml ).
    ENDIF.

    SELECT DISTINCT langu
      INTO TABLE lt_langu_additional
      FROM dokhl
      WHERE id     = 'CL'
        AND object = ls_clskey-clsname
        AND langu IN lt_language_filter
        AND langu <> mv_language
      ORDER BY langu.

    serialize_docu( ii_xml              = ii_xml
                    iv_clsname          = ls_clskey-clsname
                    it_langu_additional = lt_langu_additional ).

    serialize_descr( ii_xml     = ii_xml
                     iv_clsname = ls_clskey-clsname ).

    serialize_descr_sub( ii_xml     = ii_xml
                         iv_clsname = ls_clskey-clsname ).

    serialize_attr( ii_xml     = ii_xml
                    iv_clsname = ls_clskey-clsname ).

  ENDMETHOD.
  METHOD source_apack_replacement.

    DATA lv_clsname TYPE seoclsname.

    " Check if abapGit version of APACK manifest is used
    SELECT SINGLE clsname INTO lv_clsname
      FROM seometarel
      WHERE clsname    = ms_item-obj_name
        AND refclsname = Lif_abapgit_apack_definitions=>c_apack_interface_cust
        AND version    = '1'.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " If yes, replace with SAP-version
    interface_replacement(
      EXPORTING
        iv_from_interface = to_lower( Lif_abapgit_apack_definitions=>c_apack_interface_cust )
        iv_to_interface   = to_lower( Lif_abapgit_apack_definitions=>c_apack_interface_sap )
      CHANGING
        ct_source         = ct_source ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    TYPES: BEGIN OF ty_reposrc,
             unam  TYPE reposrc-unam,
             udat  TYPE reposrc-udat,
             utime TYPE reposrc-utime,
           END OF ty_reposrc.

    DATA: lt_reposrc  TYPE STANDARD TABLE OF ty_reposrc,
          ls_reposrc  LIKE LINE OF lt_reposrc,
          lv_include  TYPE syrepid,
          lt_includes TYPE STANDARD TABLE OF syrepid.

    CASE iv_extra.
      WHEN Lif_abapgit_oo_object_fnc=>c_parts-locals_def.
        lv_include = cl_oo_classname_service=>get_ccdef_name( |{ ms_item-obj_name }| ).
        INSERT lv_include INTO TABLE lt_includes.
      WHEN Lif_abapgit_oo_object_fnc=>c_parts-locals_imp.
        lv_include = cl_oo_classname_service=>get_ccimp_name( |{ ms_item-obj_name }| ).
        INSERT lv_include INTO TABLE lt_includes.
      WHEN Lif_abapgit_oo_object_fnc=>c_parts-macros.
        lv_include = cl_oo_classname_service=>get_ccmac_name( |{ ms_item-obj_name }| ).
        INSERT lv_include INTO TABLE lt_includes.
      WHEN Lif_abapgit_oo_object_fnc=>c_parts-testclasses.
        lv_include = cl_oo_classname_service=>get_ccau_name( |{ ms_item-obj_name }| ).
        INSERT lv_include INTO TABLE lt_includes.
      WHEN OTHERS.
        lt_includes = mi_object_oriented_object_fct->get_includes( ms_item-obj_name ).
    ENDCASE.

    ASSERT lines( lt_includes ) > 0.

    SELECT unam udat utime FROM reposrc
      INTO TABLE lt_reposrc
      FOR ALL ENTRIES IN lt_includes
      WHERE progname = lt_includes-table_line
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
    DATA: ls_clskey TYPE seoclskey.
    ls_clskey-clsname = ms_item-obj_name.

    corr_insert( iv_package ).

    mi_object_oriented_object_fct->delete( ls_clskey ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    IF iv_step = Lif_abapgit_object=>gc_step_id-abap.

      deserialize_abap( ii_xml     = io_xml
                        iv_package = iv_package ).

      deserialize_tpool( io_xml ).

      IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
        deserialize_tpool_i18n( io_xml ).
      ENDIF.

      deserialize_sotr( ii_xml     = io_xml
                        iv_package = iv_package ).

      deserialize_docu( io_xml ).

    ELSEIF iv_step = Lif_abapgit_object=>gc_step_id-early.

      " If class does not exist, create it
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

    DATA ls_class_key TYPE seoclskey.

    ls_class_key-clsname = ms_item-obj_name.

    rv_bool = mi_object_oriented_object_fct->exists( ls_class_key ).

    " Skip classes generated by DDLS (SADL)
    IF rv_bool = abap_true AND
      mi_object_oriented_object_fct->read_superclass( ls_class_key-clsname ) = 'CL_SADL_GTK_EXPOSURE_MPC'.
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

    IF is_class_locked( ) = abap_true OR is_text_locked( mv_classpool_name ) = abap_true.
      rv_is_locked = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA ls_item TYPE Lif_abapgit_definitions=>ty_item.

    ls_item-obj_type = 'PROG'.

    CASE iv_extra.
      WHEN Lif_abapgit_oo_object_fnc=>c_parts-locals_def.
        ls_item-obj_name = cl_oo_classname_service=>get_ccdef_name( |{ ms_item-obj_name }| ).
      WHEN Lif_abapgit_oo_object_fnc=>c_parts-locals_imp.
        ls_item-obj_name = cl_oo_classname_service=>get_ccimp_name( |{ ms_item-obj_name }| ).
      WHEN Lif_abapgit_oo_object_fnc=>c_parts-macros.
        ls_item-obj_name = cl_oo_classname_service=>get_ccmac_name( |{ ms_item-obj_name }| ).
      WHEN Lif_abapgit_oo_object_fnc=>c_parts-testclasses.
        ls_item-obj_name = cl_oo_classname_service=>get_ccau_name( |{ ms_item-obj_name }| ).
    ENDCASE.

    IF ls_item-obj_name IS NOT INITIAL.
      rv_exit = Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump( ls_item ).
    ENDIF.

    " Otherwise covered by ZCL_ABAPGIT_OBJECTS=>JUMP

  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lt_source    TYPE seop_source_string,
          ls_class_key TYPE seoclskey.

    ls_class_key-clsname = ms_item-obj_name.

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

    lt_source = mi_object_oriented_object_fct->serialize_abap( ls_class_key ).

    source_apack_replacement( CHANGING ct_source = lt_source ).

    Lif_abapgit_object~mo_files->add_abap( lt_source ).

    lt_source = mi_object_oriented_object_fct->serialize_abap(
      is_class_key = ls_class_key
      iv_type      = seop_ext_class_locals_def ).
    IF lines( lt_source ) > 0.
      Lif_abapgit_object~mo_files->add_abap(
        iv_extra = Lif_abapgit_oo_object_fnc=>c_parts-locals_def
        it_abap  = lt_source ).
    ENDIF.

    lt_source = mi_object_oriented_object_fct->serialize_abap(
      is_class_key = ls_class_key
      iv_type      = seop_ext_class_locals_imp ).
    IF lines( lt_source ) > 0.
      Lif_abapgit_object~mo_files->add_abap(
        iv_extra = Lif_abapgit_oo_object_fnc=>c_parts-locals_imp
        it_abap  = lt_source ).
    ENDIF.

    lt_source = mi_object_oriented_object_fct->serialize_abap(
      is_class_key            = ls_class_key
      iv_type                 = seop_ext_class_testclasses ).

    mv_skip_testclass = mi_object_oriented_object_fct->get_skip_test_classes( ).
    IF lines( lt_source ) > 0 AND mv_skip_testclass = abap_false.
      Lif_abapgit_object~mo_files->add_abap(
        iv_extra = Lif_abapgit_oo_object_fnc=>c_parts-testclasses
        it_abap  = lt_source ).
    ENDIF.

    lt_source = mi_object_oriented_object_fct->serialize_abap(
      is_class_key = ls_class_key
      iv_type      = seop_ext_class_macros ).
    IF lines( lt_source ) > 0.
      Lif_abapgit_object~mo_files->add_abap(
        iv_extra = Lif_abapgit_oo_object_fnc=>c_parts-macros
        it_abap  = lt_source ).
    ENDIF.

    serialize_xml( io_xml ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_CLAS implementation

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
endclass. "ZCL_ABAPGIT_OBJECT_CMOD implementation

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
endclass. "ZCL_ABAPGIT_OBJECT_CMPT implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_CUS0 <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_cus0=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_cus0=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_CUS0 implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( is_item = is_item
                        iv_language = iv_language ).

    mv_img_activity = ms_item-obj_name.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA ls_header TYPE ty_img_activity-header.

    CALL FUNCTION 'S_CUS_IMG_ACTIVITY_READ'
      EXPORTING
        img_activity        = mv_img_activity
      IMPORTING
        img_activity_header = ls_header.

    rv_user = ls_header-luser.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: ls_message TYPE hier_mess.

    CALL FUNCTION 'S_CUS_IMG_ACTIVITY_DELETE'
      EXPORTING
        img_activity = mv_img_activity
      IMPORTING
        message      = ls_message.

    IF ls_message-msgty <> 'S'.
      Lcx_abapgit_exception=>raise( |error from delete CUS0 { mv_img_activity } S_CUS_IMG_ACTIVITY_DELETE| ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_img_activity TYPE ty_img_activity,
          ls_text         LIKE LINE OF ls_img_activity-texts.

    io_xml->read(
      EXPORTING
        iv_name = 'CUS0'
      CHANGING
        cg_data = ls_img_activity ).

    READ TABLE ls_img_activity-texts INTO ls_text
                                     WITH KEY spras = mv_language.

    CALL FUNCTION 'S_CUS_IMG_ACTIVITY_SAVE'
      EXPORTING
        img_activity  = ls_img_activity-header-activity
        i_docu        = ls_img_activity-header-docu_id
        i_attributes  = ls_img_activity-header-attributes
        i_activity    = ls_img_activity-header-c_activity
        i_description = ls_text
        i_tcode       = ls_img_activity-header-tcode.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: ls_message TYPE hier_mess.

    CALL FUNCTION 'S_CUS_IMG_ACTIVITY_EXISTS'
      EXPORTING
        img_activity = mv_img_activity
      IMPORTING
        message      = ls_message.

    rv_bool = boolc( ls_message IS INITIAL ).

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

    rv_is_locked = abap_false.

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    DATA: lv_img_activity TYPE cus_img_ac.

    lv_img_activity = mv_img_activity.

    CALL FUNCTION 'S_CUS_IMG_ACTIVITY_MAINTAIN'
      EXPORTING
        i_display    = abap_true
      CHANGING
        img_activity = lv_img_activity.

    rv_exit = abap_true.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_img_activity TYPE ty_img_activity.

    CALL FUNCTION 'S_CUS_IMG_ACTIVITY_READ'
      EXPORTING
        img_activity        = mv_img_activity
      IMPORTING
        img_activity_header = ls_img_activity-header
      TABLES
        img_activity_texts  = ls_img_activity-texts.

    CLEAR: ls_img_activity-header-fuser,
           ls_img_activity-header-fdate,
           ls_img_activity-header-ftime,
           ls_img_activity-header-luser,
           ls_img_activity-header-ldate,
           ls_img_activity-header-ltime.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      DELETE ls_img_activity-texts WHERE spras <> mv_language.
    ENDIF.

    SORT ls_img_activity-texts.

    io_xml->add( iv_name = 'CUS0'
                 ig_data = ls_img_activity ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_CUS0 implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_CUS1 <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_cus1=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_cus1=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_CUS1 implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( is_item = is_item
                        iv_language = iv_language ).

    mv_customizing_activity = ms_item-obj_name.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA ls_header TYPE ty_customzing_activity-activity_header.

    CALL FUNCTION 'S_CUS_ACTIVITY_READ'
      EXPORTING
        activity        = mv_customizing_activity
      IMPORTING
        activity_header = ls_header.

    rv_user = ls_header-luser.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: ls_message TYPE hier_mess.

    CALL FUNCTION 'S_CUS_ACTIVITY_DELETE'
      EXPORTING
        activity = mv_customizing_activity
      IMPORTING
        message  = ls_message.

    IF ls_message-msgty <> 'S'.
      Lcx_abapgit_exception=>raise( |error from delete CUS1 { mv_customizing_activity } S_CUS_ACTIVITY_DELETE| ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_customzing_activity TYPE ty_customzing_activity,
          ls_message             TYPE hier_mess.

    io_xml->read(
      EXPORTING
        iv_name = 'CUS1'
      CHANGING
        cg_data = ls_customzing_activity ).

    CALL FUNCTION 'S_CUS_ACTIVITY_SAVE'
      EXPORTING
        activity                     = ls_customzing_activity-activity_header-act_id
        activity_type                = ls_customzing_activity-activity_header-act_type
        tcode                        = ls_customzing_activity-activity_header-tcode
        customer_exit                = ls_customzing_activity-activity_customer_exit-exit_name
        customer_exit_enhancement    = ls_customzing_activity-activity_customer_exit-enhancement
        customer_exit_implementation = ls_customzing_activity-activity_customer_exit-impl_name
      IMPORTING
        message                      = ls_message
      TABLES
        activity_title               = ls_customzing_activity-activity_title
        objects                      = ls_customzing_activity-objects
        objects_texts                = ls_customzing_activity-objects_title.

    IF ls_message-msgty <> 'S'.
      Lcx_abapgit_exception=>raise( |error from deserialize CUS1 { mv_customizing_activity } S_CUS_ACTIVITY_SAVE| ).
    ENDIF.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    CALL FUNCTION 'S_CUS_ACTIVITY_EXIST'
      EXPORTING
        activity            = mv_customizing_activity
      EXCEPTIONS
        activity_exists_not = 1
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
    rv_active = abap_true.
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    DATA: lt_bdc_data TYPE STANDARD TABLE OF bdcdata.
    FIELD-SYMBOLS: <ls_bdc_data> TYPE bdcdata.

    APPEND INITIAL LINE TO lt_bdc_data ASSIGNING <ls_bdc_data>.
    <ls_bdc_data>-program = 'SAPLS_CUS_ACTIVITY'.
    <ls_bdc_data>-dynpro = '0200'.
    <ls_bdc_data>-dynbegin = 'X'.

    APPEND INITIAL LINE TO lt_bdc_data ASSIGNING <ls_bdc_data>.
    <ls_bdc_data>-fnam = 'CUS_ACTH-ACT_ID'.
    <ls_bdc_data>-fval = mv_customizing_activity.

    APPEND INITIAL LINE TO lt_bdc_data ASSIGNING <ls_bdc_data>.
    <ls_bdc_data>-fnam = 'BDC_OKCODE'.
    <ls_bdc_data>-fval = '=ACT_DISP'.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'S_CUS_ACTIVITY'
      it_bdcdata = lt_bdc_data ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_customzing_activity TYPE ty_customzing_activity.

    CALL FUNCTION 'S_CUS_ACTIVITY_READ'
      EXPORTING
        activity               = mv_customizing_activity
      IMPORTING
        activity_header        = ls_customzing_activity-activity_header
        activity_customer_exit = ls_customzing_activity-activity_customer_exit
      TABLES
        activity_title         = ls_customzing_activity-activity_title
        objects                = ls_customzing_activity-objects
        objects_title          = ls_customzing_activity-objects_title.

    CLEAR: ls_customzing_activity-activity_header-fdatetime,
           ls_customzing_activity-activity_header-fuser,
           ls_customzing_activity-activity_header-ldatetime,
           ls_customzing_activity-activity_header-luser.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      DELETE ls_customzing_activity-activity_title WHERE spras <> mv_language.
    ENDIF.

    SORT ls_customzing_activity-activity_title.
    SORT ls_customzing_activity-objects.
    SORT ls_customzing_activity-objects_title.

    io_xml->add( iv_name = 'CUS1'
                 ig_data = ls_customzing_activity ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_CUS1 implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_CUS2 <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_cus2=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_cus2=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_CUS2 implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( is_item = is_item
                        iv_language = iv_language ).

    mv_img_attribute = ms_item-obj_name.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA ls_header TYPE ty_customizing_attribute-header.

    CALL FUNCTION 'S_CUS_ATTRIBUTES_READ'
      EXPORTING
        img_attribute    = mv_img_attribute
      IMPORTING
        attribute_header = ls_header.

    rv_user = ls_header-luser.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: ls_message TYPE hier_mess.

    CALL FUNCTION 'S_CUS_ATTRIBUTES_DELETE'
      EXPORTING
        img_attribute = mv_img_attribute
      IMPORTING
        message       = ls_message.

    IF ls_message-msgty <> 'S'.
      Lcx_abapgit_exception=>raise( |error from delete CUS2 { mv_img_attribute } S_CUS_ATTRIBUTES_DELETE| ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_customizing_attribute TYPE ty_customizing_attribute,
          ls_message               TYPE hier_mess.

    io_xml->read(
      EXPORTING
        iv_name = 'CUS2'
      CHANGING
        cg_data = ls_customizing_attribute ).

    CALL FUNCTION 'S_CUS_ATTRIBUTES_SAVE'
      EXPORTING
        img_attribute         = ls_customizing_attribute-header
      IMPORTING
        message               = ls_message
      TABLES
        attributes_title      = ls_customizing_attribute-titles
        attributes_countries  = ls_customizing_attribute-countries
        attributes_components = ls_customizing_attribute-components.

    IF ls_message-msgty <> 'S'.
      Lcx_abapgit_exception=>raise( |error from deserialize CUS2 { mv_img_attribute } S_CUS_ATTRIBUTES_SAVE| ).
    ENDIF.

    corr_insert( iv_package ).

    tadir_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    CALL FUNCTION 'S_CUS_ATTRIBUTES_EXIST'
      EXPORTING
        img_attribute         = mv_img_attribute
      EXCEPTIONS
        attributes_exists_not = 1
        OTHERS                = 2.

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

    DATA: ls_customizing_attribute TYPE ty_customizing_attribute.

    CALL FUNCTION 'S_CUS_ATTRIBUTES_READ'
      EXPORTING
        img_attribute                 = mv_img_attribute
      IMPORTING
        attribute_header              = ls_customizing_attribute-header
      TABLES
        attribute_title               = ls_customizing_attribute-titles
        attribute_countries           = ls_customizing_attribute-countries
        attribute_components          = ls_customizing_attribute-components
        attribute_components_variants = ls_customizing_attribute-components_variants.

    CLEAR: ls_customizing_attribute-header-fdatetime,
           ls_customizing_attribute-header-fuser,
           ls_customizing_attribute-header-ldatetime,
           ls_customizing_attribute-header-luser.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      DELETE ls_customizing_attribute-titles WHERE spras <> mv_language.
    ENDIF.

    io_xml->add( iv_name = 'CUS2'
                 ig_data = ls_customizing_attribute ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_CUS2 implementation

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

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'E_ACMDCLSRC'
                                            iv_argument    = |{ ms_item-obj_name }| ).

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
