********************************************************************************
*
* LICENSE and NOTICE
*
* See include program ZSHRINKER_DEMO_ABAPGIT_LICENSE
*
********************************************************************************
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
endclass. "ZCL_ABAPGIT_GUI_CHUNK_LIB implementation

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
  METHOD Lif_abapgit_gui_modal~is_modal.
    rv_yes = boolc( ms_control-show_as_modal = abap_true ).
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
endclass. "ZCL_ABAPGIT_GUI_PAGE implementation

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
  METHOD get_child.
    ri_child = mi_child.
  ENDMETHOD.
  METHOD render_content.

    IF mi_child IS BOUND.
      ri_html = mi_child->render( ).
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_HOC implementation

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
  METHOD file_encode.

    DATA lt_fields TYPE tihttpnvp.


    add_field( EXPORTING iv_name = 'KEY'
                         ig_field = iv_key CHANGING ct_field = lt_fields ).
    add_field( EXPORTING iv_name = 'PATH'
                         ig_field = ig_file CHANGING ct_field = lt_fields ).
    add_field( EXPORTING iv_name = 'FILENAME'
                         ig_field = ig_file CHANGING ct_field = lt_fields ).

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

    CASE is_cmd-cmd_type.
      WHEN Lif_abapgit_html_form=>c_cmd_type-link.

        ii_html->add_a(
          iv_txt   = is_cmd-label
          iv_act   = is_cmd-action
          iv_class = 'dialog-commands' ).

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

*>>>>>>> ZCL_ABAPGIT_HTML_TABLE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_html_table========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_html_table========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_html_table========ccau.


class LCL_ABAPGIT_HTML_TABLE implementation.
*"* method's implementations
*include methods.
  METHOD create.
    ASSERT ii_renderer IS BOUND.
    CREATE OBJECT ro_instance.
    ro_instance->mi_renderer = ii_renderer.
  ENDMETHOD.
  METHOD define_column.

    FIELD-SYMBOLS <ls_c> LIKE LINE OF mt_columns.

    ASSERT iv_column_id IS NOT INITIAL.
    ro_self = me.

    APPEND INITIAL LINE TO mt_columns ASSIGNING <ls_c>.
    <ls_c>-column_id    = iv_column_id.
    <ls_c>-column_title = iv_column_title.
    <ls_c>-from_field   = to_upper( iv_from_field ).

  ENDMETHOD.
  METHOD render.

    DATA lv_attrs TYPE string.

    IF iv_id IS NOT INITIAL.
      lv_attrs = lv_attrs && | id="{ iv_id }"|.
    ENDIF.

    IF iv_css_class IS NOT INITIAL.
      lv_attrs = lv_attrs && | class="{ iv_css_class }"|.
    ENDIF.

    CREATE OBJECT mi_html TYPE Lcl_abapgit_html.
    ri_html = mi_html.

    mi_html->add( |<table{ lv_attrs }>| ).
    render_thead( ).
    render_tbody( it_data ).
    mi_html->add( '</table>' ).

  ENDMETHOD.
  METHOD render_row.

    DATA ls_render TYPE Lif_abapgit_html_table=>ty_cell_render.
    DATA lv_dummy TYPE string.
    FIELD-SYMBOLS <ls_col> LIKE LINE OF mt_columns.
    FIELD-SYMBOLS <lv_val> TYPE any.

    LOOP AT mt_columns ASSIGNING <ls_col>.
      IF <ls_col>-from_field IS NOT INITIAL AND <ls_col>-from_field <> '-'.
        ASSIGN COMPONENT <ls_col>-from_field OF STRUCTURE is_row TO <lv_val>.
        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise( |html_table: cannot assign field [{ <ls_col>-from_field }]| ).
        ENDIF.
      ELSEIF <ls_col>-from_field <> '-'.
        <ls_col>-from_field = to_upper( <ls_col>-column_id ). " Try column_id
        ASSIGN COMPONENT <ls_col>-from_field OF STRUCTURE is_row TO <lv_val>.
        IF sy-subrc <> 0.
          <ls_col>-from_field = '-'. " Don't try assignments anymore
          ASSIGN lv_dummy TO <lv_val>.
        ENDIF.
      ELSE.
        ASSIGN lv_dummy TO <lv_val>.
      ENDIF.
      ls_render = mi_renderer->render_cell(
        iv_row_index = iv_row_index
        is_row       = is_row
        iv_column_id = <ls_col>-column_id
        iv_value     = <lv_val> ).
      mi_html->td(
        iv_content = ls_render-content
        ii_content = ls_render-html
        iv_class   = ls_render-css_class ).
    ENDLOOP.

  ENDMETHOD.
  METHOD render_tbody.

    DATA ls_row_attrs TYPE Lif_abapgit_html_table=>ty_row_attrs.
    DATA lv_row_attrs TYPE string.
    DATA lv_index TYPE i.

    FIELD-SYMBOLS <ls_i> TYPE any.

    mi_html->add( '<tbody>' ).

    LOOP AT it_data ASSIGNING <ls_i>.
      lv_index = sy-tabix.
      ls_row_attrs = mi_renderer->get_row_attrs(
        iv_row_index = lv_index
        is_row       = <ls_i> ).
      CLEAR lv_row_attrs.
      IF ls_row_attrs-css_class IS NOT INITIAL.
        lv_row_attrs = lv_row_attrs && | class="{ ls_row_attrs-css_class }"|.
      ENDIF.
      mi_html->add( |<tr{ lv_row_attrs }>| ).
      render_row(
        iv_row_index = lv_index
        is_row       = <ls_i> ).
      mi_html->add( '</tr>' ).
    ENDLOOP.

    mi_html->add( '</tbody>' ).

  ENDMETHOD.
  METHOD render_thead.

    FIELD-SYMBOLS <ls_col> LIKE LINE OF mt_columns.

    mi_html->add( '<thead>' ).
    mi_html->add( '<tr>' ).

    LOOP AT mt_columns ASSIGNING <ls_col>.
      mi_html->th( <ls_col>-column_title ).
    ENDLOOP.

    mi_html->add( '</tr>' ).
    mi_html->add( '</thead>' ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_HTML_TABLE implementation

*>>>>>>> ZCL_ABAPGIT_HTML_TOOLBAR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_html_toolbar======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_html_toolbar======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_HTML_TOOLBAR implementation.
*"* method's implementations
*include methods.
  METHOD add.
    DATA ls_item TYPE ty_item.

    ASSERT iv_typ = Lif_abapgit_html=>c_action_type-separator  " sep doesn't have action
      OR iv_typ = Lif_abapgit_html=>c_action_type-onclick      " click may have no action (assigned in JS)
      OR iv_typ = Lif_abapgit_html=>c_action_type-dummy        " dummy may have no action
      OR iv_act IS INITIAL AND io_sub IS NOT INITIAL
      OR iv_act IS NOT INITIAL AND io_sub IS INITIAL. " Only one supplied

    ASSERT NOT ( iv_chk <> abap_undefined AND io_sub IS NOT INITIAL ).

    ls_item-txt   = iv_txt.
    ls_item-act   = iv_act.
    ls_item-ico   = iv_ico.
    ls_item-sub   = io_sub.
    ls_item-opt   = iv_opt.
    ls_item-typ   = iv_typ.
    ls_item-cur   = iv_cur.
    ls_item-chk   = iv_chk.
    ls_item-aux   = iv_aux.
    ls_item-id    = iv_id.
    ls_item-title = iv_title.
    ls_item-class = iv_class.
    ls_item-li_class = iv_li_class.

    APPEND ls_item TO mt_items.

    ro_self = me.

  ENDMETHOD.
  METHOD constructor.
    mv_id = iv_id.
  ENDMETHOD.
  METHOD count_items.
    rv_count = lines( mt_items ).
  ENDMETHOD.
  METHOD create.
    CREATE OBJECT ro_instance
      EXPORTING
        iv_id = iv_id.
  ENDMETHOD.
  METHOD render.

    DATA: lv_class TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    lv_class = 'nav-container'.
    IF iv_right = abap_true.
      lv_class = lv_class && ' float-right'.
    ENDIF.

    ri_html->add( |<div class="{ lv_class }">| ).
    ri_html->add( render_items( iv_sort = iv_sort ) ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
  METHOD render_as_droplist.

    DATA: lv_class TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    lv_class = 'nav-container'.
    IF iv_right = abap_true.
      lv_class = lv_class && ' float-right'.
    ENDIF.
    IF iv_corner = abap_true.
      lv_class = lv_class && ' corner'.
    ENDIF.

    ri_html->add( |<div class="{ lv_class }">| ).
    ri_html->add( '<ul><li>' ).
    ri_html->add_a( iv_txt = iv_label
                    iv_typ = Lif_abapgit_html=>c_action_type-sapevent
                    iv_act = iv_action ).
    ri_html->add( '<div class="minizone"></div>' ).
    ri_html->add( render_items( iv_sort = iv_sort ) ).
    ri_html->add( '</li></ul>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
  METHOD render_items.

    DATA: lv_class       TYPE string,
          lv_class_value TYPE string,
          lv_icon        TYPE string,
          lv_id          TYPE string,
          lv_check       TYPE string,
          lv_aux         TYPE string,
          lv_has_icons   TYPE abap_bool.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF mt_items.


    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    IF iv_sort = abap_true.
      SORT mt_items BY txt ASCENDING AS TEXT.
    ENDIF.

    " Check has icons or check boxes
    LOOP AT mt_items ASSIGNING <ls_item> WHERE ico IS NOT INITIAL OR chk <> abap_undefined.
      lv_has_icons = abap_true.
      lv_class     = ' class="with-icons"'.
      EXIT.
    ENDLOOP.

    IF mv_id IS NOT INITIAL.
      lv_id = | id="{ mv_id }"|.
    ENDIF.

    ri_html->add( |<ul{ lv_id }{ lv_class }>| ).

    " Render items
    LOOP AT mt_items ASSIGNING <ls_item>.
      CLEAR: lv_class, lv_class_value, lv_icon.

      IF <ls_item>-typ = Lif_abapgit_html=>c_action_type-separator.
        ri_html->add( |<li class="separator">{ <ls_item>-txt }</li>| ).
        CONTINUE.
      ENDIF.

      IF lv_has_icons = abap_true.
        IF <ls_item>-chk = abap_true.
          lv_icon  = ri_html->icon( 'check/blue' ).
          lv_check = ' data-check="X"'.
        ELSEIF <ls_item>-chk = abap_false.
          lv_icon = ri_html->icon( 'check/grey' ).
          lv_check = ' data-check=""'.
        ELSE. " abap_undefined -> not a check box
          lv_icon = ri_html->icon( <ls_item>-ico ).
        ENDIF.
      ENDIF.


      IF <ls_item>-cur = abap_true.
        IF <ls_item>-li_class IS INITIAL.
          lv_class_value = 'current-menu-item'.
        ELSE.
          lv_class_value = |current-menu-item { <ls_item>-li_class }|.
        ENDIF.
      ELSE.
        lv_class_value = <ls_item>-li_class.
      ENDIF.
      IF lv_class_value IS NOT INITIAL.
        lv_class = | class="{ lv_class_value }"|.
      ENDIF.
      IF <ls_item>-aux IS NOT INITIAL.
        lv_aux = | data-aux="{ <ls_item>-aux }"|.
      ENDIF.

      ri_html->add( |<li{ lv_class }{ lv_check }{ lv_aux }>| ).

      IF <ls_item>-sub IS INITIAL.
        ri_html->add_a( iv_txt   = lv_icon && <ls_item>-txt
                        iv_typ   = <ls_item>-typ
                        iv_act   = <ls_item>-act
                        iv_id    = <ls_item>-id
                        iv_opt   = <ls_item>-opt
                        iv_title = <ls_item>-title
                        iv_class = <ls_item>-class ).
      ELSE.
        ri_html->add_a( iv_txt   = lv_icon && <ls_item>-txt
                        iv_typ   = Lif_abapgit_html=>c_action_type-dummy
                        iv_act   = ''
                        iv_id    = <ls_item>-id
                        iv_opt   = <ls_item>-opt
                        iv_title = <ls_item>-title
                        iv_class = <ls_item>-class ).
        ri_html->add( <ls_item>-sub->render_items( iv_sort ) ).
      ENDIF.
      ri_html->add( '</li>' ).

    ENDLOOP.

    ri_html->add( '</ul>' ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_HTML_TOOLBAR implementation

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

*>>>>>>> ZCL_ABAPGIT_GUI_MENUS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_menus=========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_menus=========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_MENUS implementation.
*"* method's implementations
*include methods.
  METHOD advanced.

    CREATE OBJECT ro_menu EXPORTING iv_id = 'toolbar-advanced'.

    ro_menu->add(
      iv_txt = 'Database Utility'
      iv_act = Lif_abapgit_definitions=>c_action-go_db
    )->add(
      iv_txt = 'Package to ZIP'
      iv_act = Lif_abapgit_definitions=>c_action-zip_package
    )->add(
      iv_txt = 'Transport to ZIP'
      iv_act = Lif_abapgit_definitions=>c_action-zip_transport
    )->add(
      iv_txt = 'Object to Files'
      iv_act = Lif_abapgit_definitions=>c_action-zip_object
    )->add(
      iv_txt = 'Debug Info'
      iv_act = Lif_abapgit_definitions=>c_action-go_debuginfo ).

    IF Lcl_abapgit_ui_factory=>get_frontend_services( )->is_sapgui_for_windows( ) = abap_true.
      ro_menu->add(
        iv_txt = 'Open IE DevTools'
        iv_act = Lif_abapgit_definitions=>c_action-ie_devtools ).
    ENDIF.

  ENDMETHOD.
  METHOD back.

    CREATE OBJECT ro_menu EXPORTING iv_id = 'toolbar-back'.

    ro_menu->add(
      iv_txt = 'Back'
      iv_act = Lif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.
  METHOD experimental.

    IF Lcl_abapgit_persist_factory=>get_settings( )->read( )->get_experimental_features( ) IS NOT INITIAL.
      io_menu->add(
        iv_txt = Lcl_abapgit_gui_buttons=>experimental( )
        iv_act = Lif_abapgit_definitions=>c_action-go_settings ).
    ENDIF.

  ENDMETHOD.
  METHOD help.

    CREATE OBJECT ro_menu EXPORTING iv_id = 'toolbar-help'.

    ro_menu->add(
      iv_txt = 'Tutorial'
      iv_act = Lif_abapgit_definitions=>c_action-go_tutorial
    )->add(
      iv_txt = 'Documentation'
      iv_act = Lif_abapgit_definitions=>c_action-documentation
    )->add(
      iv_txt = 'Explore'
      iv_act = Lif_abapgit_definitions=>c_action-go_explore
    )->add(
      iv_txt = 'Changelog'
      iv_act = Lif_abapgit_definitions=>c_action-changelog
    )->add(
      iv_txt = 'Hotkeys'
      iv_act = Lif_abapgit_definitions=>c_action-show_hotkeys ).

  ENDMETHOD.
  METHOD repo_settings.

    CREATE OBJECT ro_menu EXPORTING iv_id = 'toolbar-repo-settings'.

    ro_menu->add(
      iv_txt = 'Repository'
      iv_act = |{ Lif_abapgit_definitions=>c_action-repo_settings }?key={ iv_key }|
      iv_cur = boolc( iv_act = Lif_abapgit_definitions=>c_action-repo_settings )
    )->add(
      iv_txt = 'Local'
      iv_act = |{ Lif_abapgit_definitions=>c_action-repo_local_settings }?key={ iv_key }|
      iv_cur = boolc( iv_act = Lif_abapgit_definitions=>c_action-repo_local_settings )
    )->add(
      iv_txt = 'Remote'
      iv_act = |{ Lif_abapgit_definitions=>c_action-repo_remote_settings }?key={ iv_key }|
      iv_cur = boolc( iv_act = Lif_abapgit_definitions=>c_action-repo_remote_settings )
    )->add(
      iv_txt = 'Background'
      iv_act = |{ Lif_abapgit_definitions=>c_action-repo_background }?key={ iv_key }|
      iv_cur = boolc( iv_act = Lif_abapgit_definitions=>c_action-repo_background )
    )->add(
      iv_txt = 'Stats'
      iv_act = |{ Lif_abapgit_definitions=>c_action-repo_infos }?key={ iv_key }|
      iv_cur = boolc( iv_act = Lif_abapgit_definitions=>c_action-repo_infos ) ).

    Lcl_abapgit_exit=>get_instance( )->enhance_repo_toolbar(
      io_menu = ro_menu
      iv_key  = iv_key
      iv_act  = iv_act ).

  ENDMETHOD.
  METHOD settings.

    CREATE OBJECT ro_menu EXPORTING iv_id = 'toolbar-settings'.

    ro_menu->add(
      iv_txt = 'Global'
      iv_act = Lif_abapgit_definitions=>c_action-go_settings
      iv_cur = boolc( iv_act = Lif_abapgit_definitions=>c_action-go_settings )
    )->add(
      iv_txt = 'Personal'
      iv_act = Lif_abapgit_definitions=>c_action-go_settings_personal
      iv_cur = boolc( iv_act = Lif_abapgit_definitions=>c_action-go_settings_personal ) ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_MENUS implementation

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
*CLASS SHRIS5ZPAUXVKEPN5HWETLLATBUBTU DEFINITION DEFERRED.
*CLASS zcl_abapgit_gui_page_data DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLATBUBTU.




class LCL_ABAPGIT_GUI_PAGE_DATA implementation.
*"* method's implementations
*include methods.
  METHOD add_via_transport.

    DATA lt_trkorr  TYPE trwbo_request_headers.
    DATA ls_trkorr  LIKE LINE OF lt_trkorr.
    DATA ls_request TYPE Lif_abapgit_cts_api=>ty_transport_data.
    DATA ls_key     LIKE LINE OF ls_request-keys.
    DATA lv_where   TYPE string.
    DATA ls_config  TYPE Lif_abapgit_data_config=>ty_config.


    lt_trkorr = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_select_transports( ).
    IF lines( lt_trkorr ) <> 1.
      RETURN.
    ENDIF.

    READ TABLE lt_trkorr INDEX 1 INTO ls_trkorr.
    ASSERT sy-subrc = 0.

    ls_request = Lcl_abapgit_factory=>get_cts_api( )->read( ls_trkorr-trkorr ).

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
endclass. "ZCL_ABAPGIT_GUI_PAGE_DATA implementation

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




*CLASS zcl_abapgit_gui_page_patch DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLATBXBTU.





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

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_TUTORIAL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_tutorial=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_tutorial=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_TUTORIAL implementation.
*"* method's implementations
*include methods.
  METHOD build_main_menu.

    CREATE OBJECT ro_menu EXPORTING iv_id = 'toolbar-main'.

    ro_menu->add(
      iv_txt = Lcl_abapgit_gui_buttons=>repo_list( )
      iv_act = Lif_abapgit_definitions=>c_action-abapgit_home
    )->add(
      iv_txt = Lcl_abapgit_gui_buttons=>new_online( )
      iv_act = Lif_abapgit_definitions=>c_action-repo_newonline
    )->add(
      iv_txt = Lcl_abapgit_gui_buttons=>new_offline( )
      iv_act = Lif_abapgit_definitions=>c_action-repo_newoffline
    )->add(
      iv_txt = Lcl_abapgit_gui_buttons=>help( )
      io_sub = Lcl_abapgit_gui_menus=>help( ) ).

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_tutorial.

    CREATE OBJECT lo_component.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Tutorial'
      io_page_menu       = build_main_menu( )
      ii_child_component = lo_component ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<div class="tutorial">' ).

    ri_html->add( '<h1>Tutorial</h1>' ).
    ri_html->add( '<hr>' ).

    ri_html->add( '<h2>Online repositories</h2>' ).
    ri_html->add( '<p><ul>' ).

    ri_html->add( `<li>To clone a remote repository (e.g. from github) click ` ).
    ri_html->add_a( iv_txt = Lcl_abapgit_gui_buttons=>new_online( )
                    iv_act = Lif_abapgit_definitions=>c_action-repo_newonline ).
    ri_html->add( ' from the top menu. This will link a remote repository with a package on your system.</li>' ).
    ri_html->add( '<li>Use the pull button to retrieve and activate the remote objects.</li>' ).
    ri_html->add( '<li>If the remote repository is updated,' ).
    ri_html->add( ' you will see the changes and can pull to apply the updates.</li>' ).

    ri_html->add( '</ul></p>' ).

    ri_html->add( '<h2>Offline repositories</h2>' ).
    ri_html->add( '<p><ul>' ).

    ri_html->add( `<li>To add a package as an offline repository, click ` ).
    ri_html->add_a( iv_txt = Lcl_abapgit_gui_buttons=>new_offline( )
                    iv_act = Lif_abapgit_definitions=>c_action-repo_newoffline ).
    ri_html->add( ' from the top menu.' ).
    ri_html->add( '<li>abapGit will start tracking changes for the package ' ).
    ri_html->add( 'without linking it to an online git repository.</li>' ).
    ri_html->add( '<li>You can link the package later or just export the package content as a ZIP file.</li>' ).

    ri_html->add( '</ul></p>' ).

    ri_html->add( '</ul></p>' ).

    ri_html->add( '<h2>Repository list and favorites</h2>' ).
    ri_html->add( '<p><ul>' ).
    ri_html->add( |<li>To favorite a repository, use the {
                  ri_html->icon( 'star/darkgrey' ) } icon in the repository list.</li>| ).
    ri_html->add( |<li>To go to a repository, click on the repository name.</li>| ).
    ri_html->add( |<li>To go back to your favorites, use the| ).
    ri_html->add_a(
      iv_txt = Lcl_abapgit_gui_buttons=>repo_list( )
      iv_act = Lif_abapgit_definitions=>c_action-abapgit_home ).
    ri_html->add( |</li>| ).

    ri_html->add( `<li>` ).
    ri_html->add_a( iv_txt = 'Explore'
                    iv_act = Lif_abapgit_definitions=>c_action-go_explore ).
    ri_html->add( ' to find projects using abapGit</li>' ).


    ri_html->add( '</ul></p>' ).
    ri_html->add( '</div>' ).


  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_TUTORIAL implementation

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
      iv_name        = c_id-url
      iv_required    = abap_true
      iv_label       = 'Repository Name'
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
      lo_zip      TYPE REF TO cl_abap_zip,
      lv_zip      TYPE xstring,
      lv_path     TYPE string,
      lv_filename TYPE string,
      li_fe_serv  TYPE REF TO Lif_abapgit_frontend_services.

    FIELD-SYMBOLS:
      <ls_data> LIKE LINE OF lt_data.

    lt_data = Lcl_abapgit_persistence_db=>get_instance( )->list( ).

    CREATE OBJECT lo_zip.

    LOOP AT lt_data ASSIGNING <ls_data>.
      CONCATENATE <ls_data>-type '_' <ls_data>-value '.xml' INTO lv_filename.
      lo_zip->add(
        name    = lv_filename
        content = Lcl_abapgit_convert=>string_to_xstring_utf8( <ls_data>-data_str ) ).
    ENDLOOP.

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

    LOOP AT lo_zip->files ASSIGNING <ls_zipfile>.
      CLEAR ls_data.
      lv_filename = <ls_zipfile>-name.
      REPLACE '.xml' IN lv_filename WITH ''.
      SPLIT lv_filename AT '_' INTO ls_data-type ls_data-value.

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

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_REPO_OVER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_repo_overccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_repo_overccimp.
CLASS SHRIS5ZPAUXVKEPN5HWETLLATBZBTU DEFINITION FINAL.
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
        VALUE(ro_me) TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLATBZBTU.

ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLATBZBTU IMPLEMENTATION.

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

    DATA lo_tab_scheme TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLATBZBTU.

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
      lv_lock           TYPE string.

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

    ii_html->td(
      ii_html->a(
        iv_txt = is_repo-name
        iv_act = |{ c_action-select }?key={ is_repo-key }| ) && lv_lock ).

    " Labels
    IF mt_all_labels IS NOT INITIAL.
      ii_html->td(
        iv_content = Lcl_abapgit_gui_chunk_lib=>render_label_list(
          it_labels = is_repo-labels
          io_label_colors = mo_label_colors )
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
  METHOD save_settings.
    Lcl_abapgit_persistence_user=>get_instance( )->set_list_settings( ms_list_settings ).
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
endclass. "ZCL_ABAPGIT_GUI_PAGE_REPO_OVER implementation

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
  METHOD build_origlang_code.

    IF is_item-origlang IS NOT INITIAL AND is_item-origlang <> mo_repo->get_dot_abapgit( )->get_main_language( ).
      rv_html_code = Lcl_abapgit_html=>icon(
        iv_name  = 'language-solid/grey'
        iv_hint  = |Original language: { is_item-origlang }|
        iv_class = 'cursor-pointer' ).
    ENDIF.

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

    lt_tadir = Lcl_abapgit_factory=>get_tadir( )->read(
      iv_package            = mo_repo->get_package( )
      iv_only_local_objects = abap_true ).

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

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_RUN_BCKG <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_run_bckg=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_run_bckg=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_RUN_BCKG implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( ).

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_run_bckg.

    CREATE OBJECT lo_component.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Background Run'
      io_page_menu       = Lcl_abapgit_gui_menus=>back( )
      ii_child_component = lo_component ).

  ENDMETHOD.
  METHOD run.

    DATA: lx_error TYPE REF TO Lcx_abapgit_exception,
          lv_text  TYPE string,
          lv_line  TYPE i VALUE 1.


    TRY.
        Lcl_abapgit_background=>run( ).

        DO.
          READ LINE lv_line LINE VALUE INTO lv_text.
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.
          APPEND lv_text TO mt_text.
          lv_line = lv_line + 1.
        ENDDO.
      CATCH Lcx_abapgit_exception INTO lx_error.
        APPEND lx_error->get_text( ) TO mt_text.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.
    rs_handled-state = Lcl_abapgit_gui=>c_event_state-go_back.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    DATA: lv_text LIKE LINE OF mt_text.

    register_handlers( ).

    run( ).

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<div id="toc">' ).
    LOOP AT mt_text INTO lv_text.
      ri_html->add( '<pre>' && lv_text && '</pre><br>' ).
    ENDLOOP.
    ri_html->add( '</div>' ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_RUN_BCKG implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_SETT_BCKG <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_sett_bckgccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_sett_bckgccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_SETT_BCKG implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( ).
    CREATE OBJECT mo_form_data.
    mo_repo = io_repo.
    mo_form = get_form_schema( ).
    mo_form_data = read_settings( ).

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_sett_bckg.

    CREATE OBJECT lo_component
      EXPORTING
        io_repo = io_repo.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Background Mode'
      io_page_menu       = Lcl_abapgit_gui_menus=>repo_settings(
                             iv_key = io_repo->get_key( )
                             iv_act = Lif_abapgit_definitions=>c_action-repo_background )
      ii_child_component = lo_component ).

  ENDMETHOD.
  METHOD get_form_schema.

    DATA:
      lt_methods TYPE Lcl_abapgit_background=>ty_methods,
      ls_method  LIKE LINE OF lt_methods,
      lv_hint    TYPE string.

    lt_methods = Lcl_abapgit_background=>list_methods( ).

    ro_form = Lcl_abapgit_html_form=>create(
                iv_form_id   = 'repo-background-form'
                iv_help_page = 'https://docs.abapgit.org/settings-background-mode.html' ).

    ro_form->start_group(
      iv_name          = c_id-mode_selection
      iv_label         = 'Mode'
    )->radio(
      iv_name          = c_id-method
      iv_default_value = ''
      iv_label         = 'Selection'
      iv_hint          = 'Define the action that will be executed in background mode'
    )->option(
      iv_label         = 'Do Nothing'
      iv_value         = '' ).

    LOOP AT lt_methods INTO ls_method.
      ro_form->option(
        iv_label       = ls_method-description
        iv_value       = ls_method-class ).
    ENDLOOP.

    ro_form->table(
      iv_name        = c_id-settings
      iv_hint        = 'Settings required for selected background action'
      iv_label       = 'Additional Settings'
    )->column(
      iv_label       = 'Key'
      iv_width       = '50%'
      iv_readonly    = abap_true
    )->column(
      iv_label       = 'Value'
      iv_width       = '50%' ).

    lv_hint = 'Password will be saved in clear text!'.

    ro_form->start_group(
      iv_name        = c_id-authentication
      iv_label       = 'HTTP Authentication (Optional)'
      iv_hint        = lv_hint
    )->text(
      iv_name        = c_id-username
      iv_label       = 'Username'
      iv_hint        = lv_hint
    )->text(
      iv_name        = c_id-password
      iv_label       = 'Password'
      iv_hint        = lv_hint
      iv_placeholder = lv_hint ).

    ro_form->command(
      iv_label       = 'Save Settings'
      iv_cmd_type    = Lif_abapgit_html_form=>c_cmd_type-input_main
      iv_action      = c_event-save
    )->command(
      iv_label       = 'Run Background Logic'
      iv_action      = Lif_abapgit_definitions=>c_action-go_background_run
    )->command(
      iv_label       = 'Back'
      iv_action      = Lif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.
  METHOD read_persist.

    DATA lo_per TYPE REF TO Lcl_abapgit_persist_background.

    CREATE OBJECT lo_per.

    TRY.
        rs_persist = lo_per->get_by_key( mo_repo->get_key( ) ).
      CATCH Lcx_abapgit_not_found.
        CLEAR rs_persist.
    ENDTRY.

  ENDMETHOD.
  METHOD read_settings.

    DATA:
      ls_per      TYPE Lcl_abapgit_persist_background=>ty_background,
      lv_row      TYPE i,
      lv_val      TYPE string,
      lt_settings LIKE ls_per-settings,
      ls_settings LIKE LINE OF ls_per-settings.

    ls_per = read_persist( ).
    CREATE OBJECT ro_form_data.

    " Mode Selection
    ro_form_data->set(
      iv_key = c_id-method
      iv_val = ls_per-method ).

    " Mode Settings
    IF ls_per-method IS NOT INITIAL.

      lt_settings = ls_per-settings.

      " skip invalid values, from old background logic
      IF ls_per-method <> 'push' AND ls_per-method <> 'pull' AND ls_per-method <> 'nothing'.
        CALL METHOD (ls_per-method)=>Lif_abapgit_background~get_settings
          CHANGING
            ct_settings = lt_settings.
      ENDIF.

      LOOP AT lt_settings INTO ls_settings.
        lv_row = lv_row + 1.
        DO 3 TIMES.
          CASE sy-index.
            WHEN 1.
              lv_val = ls_settings-key.
            WHEN 2.
              lv_val = ls_settings-value.
          ENDCASE.
          ro_form_data->set(
            iv_key = |{ c_id-settings }-{ lv_row }-{ sy-index }|
            iv_val = lv_val ).
        ENDDO.
      ENDLOOP.

    ENDIF.

    mv_settings_count = lv_row.

    ro_form_data->set(
      iv_key = |{ c_id-settings }-{ Lif_abapgit_html_form=>c_rows }|
      iv_val = |{ mv_settings_count }| ).

    " Authentication
    ro_form_data->set(
      iv_key = c_id-username
      iv_val = ls_per-username ).
    ro_form_data->set(
      iv_key = c_id-password
      iv_val = ls_per-password ).

  ENDMETHOD.
  METHOD save_settings.

    DATA:
      lo_persistence TYPE REF TO Lcl_abapgit_persist_background,
      ls_per         TYPE Lcl_abapgit_persist_background=>ty_background,
      lt_settings    LIKE ls_per-settings.

    FIELD-SYMBOLS:
      <ls_settings> LIKE LINE OF ls_per-settings.

    ls_per-key = mo_repo->get_key( ).

    " Mode Selection
    ls_per-method = mo_form_data->get( c_id-method ).

    " Mode Settings
    IF ls_per-method IS NOT INITIAL.

      lt_settings = ls_per-settings.

      " skip invalid values, from old background logic
      IF ls_per-method <> 'push' AND ls_per-method <> 'pull' AND ls_per-method <> 'nothing'.
        CALL METHOD (ls_per-method)=>Lif_abapgit_background~get_settings
          CHANGING
            ct_settings = lt_settings.
      ENDIF.

      LOOP AT lt_settings ASSIGNING <ls_settings>.
        <ls_settings>-value = mo_form_data->get( |{ c_id-settings }-{ sy-tabix }-2| ).
      ENDLOOP.

      ls_per-settings = lt_settings.

    ENDIF.

    " Authentication
    ls_per-username = mo_form_data->get( c_id-username ).
    ls_per-password = mo_form_data->get( c_id-password ).

    CREATE OBJECT lo_persistence.

    IF ls_per-method IS INITIAL.
      lo_persistence->delete( ls_per-key ).
    ELSE.
      lo_persistence->modify( ls_per ).
    ENDIF.

    COMMIT WORK AND WAIT.

    MESSAGE 'Settings succesfully saved' TYPE 'S'.

    mo_form_data = read_settings( ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    mo_form_data->merge( Lcl_abapgit_html_form_utils=>create( mo_form )->normalize( ii_event->form_data( ) ) ).

    CASE ii_event->mv_action.
      WHEN Lif_abapgit_definitions=>c_action-go_back.
        rs_handled-state = Lcl_abapgit_html_form_utils=>create( mo_form )->exit(
          io_form_data    = mo_form_data
          io_compare_with = read_settings( ) ).

      WHEN c_event-save.
        save_settings( ).
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
      iv_form_class = 'w800px'
      io_values     = mo_form_data ) ).

    ri_html->add( `</div>` ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_SETT_BCKG implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_SETT_GLOB <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_sett_globccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_sett_globccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_SETT_GLOB implementation.
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

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_sett_glob.

    CREATE OBJECT lo_component.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Global Settings'
      io_page_menu       = Lcl_abapgit_gui_menus=>settings( Lif_abapgit_definitions=>c_action-go_settings )
      ii_child_component = lo_component ).

  ENDMETHOD.
  METHOD get_form_schema.

    ro_form = Lcl_abapgit_html_form=>create(
      iv_form_id   = 'global-setting-form'
      iv_help_page = 'https://docs.abapgit.org/guide-settings-global.html' ).

    ro_form->start_group(
      iv_name        = c_id-proxy_settings
      iv_label       = 'Proxy Settings'
    )->text(
      iv_name        = c_id-proxy_url
      iv_label       = 'Proxy Host'
      iv_hint        = 'Hostname or IP of proxy required to access the Internet (do not enter http://)'
      iv_placeholder = 'Hostname or IP without http://'
    )->number(
      iv_name        = c_id-proxy_port
      iv_label       = 'Proxy Port'
      iv_hint        = 'Port of proxy required to access the Internet'
      iv_min         = 0
      iv_max         = 65535
    )->checkbox(
      iv_name        = c_id-proxy_auth
      iv_label       = 'Proxy Authentication'
      iv_hint        = 'Check, if proxy requires you to login'
    )->textarea(
      iv_name        = c_id-proxy_bypass
      iv_label       = 'Proxy Bypass'
      iv_hint        = 'List of hosts/domains for which to bypass using proxy'
    )->start_group(
      iv_name        = c_id-commit_settings
      iv_label       = 'Commit Message Settings'
    )->number(
      iv_name        = c_id-commitmsg_comment_length
      iv_required    = abap_true
      iv_label       = 'Maximum Length of Comment'
      iv_hint        = |At least { Lcl_abapgit_settings=>c_commitmsg_comment_length_dft } characters|
      iv_min         = Lcl_abapgit_settings=>c_commitmsg_comment_length_dft
    )->text(
      iv_name        = c_id-commitmsg_comment_deflt
      iv_label       = 'Default Text For Comment'
      iv_hint        = 'You can use $OBJECT or $FILE to include the number of objects/files'
    )->number(
      iv_name        = c_id-commitmsg_body_size
      iv_required    = abap_true
      iv_label       = 'Maximum Line Size of Body'
      iv_hint        = |At least { Lcl_abapgit_settings=>c_commitmsg_body_size_dft } characters|
      iv_min         = Lcl_abapgit_settings=>c_commitmsg_body_size_dft
    )->checkbox(
      iv_name        = c_id-commitmsg_hide_author
      iv_label       = 'Hide Author Fields' ).

    IF Lcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_false.
      ro_form->start_group(
        iv_name        = c_id-devint_settings
        iv_label       = 'Development Internal Settings'
      )->checkbox(
        iv_name        = c_id-run_critical_tests
        iv_label       = 'Enable Critical Unit Tests'
      )->text(
        iv_name        = c_id-experimental_features
        iv_label       = 'Experimental Features'
        iv_hint        = 'Set to "X" to enable all features or add feature values as a comma-separated list' ).
    ENDIF.

    ro_form->command(
      iv_label       = 'Save Settings'
      iv_cmd_type    = Lif_abapgit_html_form=>c_cmd_type-input_main
      iv_action      = c_event-save
    )->command(
      iv_label       = 'Back'
      iv_action      = Lif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.
  METHOD read_proxy_bypass.

    DATA:
      lt_proxy_bypass TYPE Lif_abapgit_definitions=>ty_range_proxy_bypass_url,
      ls_proxy_bypass LIKE LINE OF lt_proxy_bypass,
      lv_val          TYPE string.

    lt_proxy_bypass = io_settings->get_proxy_bypass( ).
    LOOP AT lt_proxy_bypass INTO ls_proxy_bypass.
      lv_val = lv_val && ls_proxy_bypass-low && cl_abap_char_utilities=>newline.
    ENDLOOP.

    io_form_data->set(
      iv_key = c_id-proxy_bypass
      iv_val = lv_val ).

  ENDMETHOD.
  METHOD read_settings.

    " Get settings from DB
    mo_settings = Lcl_abapgit_persist_factory=>get_settings( )->read( ).
    CREATE OBJECT ro_form_data.

    " Proxy
    ro_form_data->set(
      iv_key = c_id-proxy_url
      iv_val = mo_settings->get_proxy_url( ) ).
    ro_form_data->set(
      iv_key = c_id-proxy_port
      iv_val = mo_settings->get_proxy_port( ) ).
    ro_form_data->set(
      iv_key = c_id-proxy_auth
      iv_val = boolc( mo_settings->get_proxy_authentication( ) = abap_true ) ) ##TYPE.

    read_proxy_bypass(
      io_settings = mo_settings
      io_form_data = ro_form_data ).

    " Commit Message
    ro_form_data->set(
      iv_key = c_id-commitmsg_comment_length
      iv_val = |{ mo_settings->get_commitmsg_comment_length( ) }| ).
    ro_form_data->set(
      iv_key = c_id-commitmsg_comment_deflt
      iv_val = mo_settings->get_commitmsg_comment_default( ) ).
    ro_form_data->set(
      iv_key = c_id-commitmsg_body_size
      iv_val = |{ mo_settings->get_commitmsg_body_size( ) }| ).
    ro_form_data->set(
      iv_key = c_id-commitmsg_hide_author
      iv_val = boolc( mo_settings->get_commitmsg_hide_author( ) = abap_true ) ) ##TYPE.

    " Dev Internal
    IF Lcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_false.
      ro_form_data->set(
        iv_key = c_id-run_critical_tests
        iv_val = boolc( mo_settings->get_run_critical_tests( ) = abap_true ) ) ##TYPE.
      ro_form_data->set(
        iv_key = c_id-experimental_features
        iv_val = mo_settings->get_experimental_features( ) ).
    ENDIF.

  ENDMETHOD.
  METHOD save_proxy_bypass.

    DATA:
      lt_textarea     TYPE TABLE OF string,
      lt_proxy_bypass TYPE Lif_abapgit_definitions=>ty_range_proxy_bypass_url,
      ls_proxy_bypass LIKE LINE OF lt_proxy_bypass.

    lt_textarea = Lcl_abapgit_convert=>split_string( mo_form_data->get( c_id-proxy_bypass ) ).

    ls_proxy_bypass-sign = 'I'.
    LOOP AT lt_textarea INTO ls_proxy_bypass-low WHERE table_line IS NOT INITIAL.
      IF ls_proxy_bypass-low CA '*+'.
        ls_proxy_bypass-option = 'CP'.
      ELSE.
        ls_proxy_bypass-option = 'EQ'.
      ENDIF.
      APPEND ls_proxy_bypass TO lt_proxy_bypass.
    ENDLOOP.

    mo_settings->set_proxy_bypass( lt_proxy_bypass ).

  ENDMETHOD.
  METHOD save_settings.

    DATA:
      li_persistence TYPE REF TO Lif_abapgit_persist_settings,
      lv_value       TYPE i.

    " Proxy
    mo_settings->set_proxy_url( mo_form_data->get( c_id-proxy_url ) ).
    mo_settings->set_proxy_port( mo_form_data->get( c_id-proxy_port ) ).
    mo_settings->set_proxy_authentication( boolc( mo_form_data->get( c_id-proxy_auth ) = abap_true ) ).

    save_proxy_bypass( ).

    " Commit Message
    lv_value = mo_form_data->get( c_id-commitmsg_comment_length ).
    mo_settings->set_commitmsg_comment_length( lv_value ).
    mo_settings->set_commitmsg_comment_default( mo_form_data->get( c_id-commitmsg_comment_deflt ) ).
    lv_value = mo_form_data->get( c_id-commitmsg_body_size ).
    mo_settings->set_commitmsg_body_size( lv_value ).
    mo_settings->set_commitmsg_hide_author( boolc( mo_form_data->get( c_id-commitmsg_hide_author ) = abap_true ) ).

    " Dev Internal
    IF Lcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_false.
      mo_settings->set_run_critical_tests( boolc( mo_form_data->get( c_id-run_critical_tests ) = abap_true ) ).
      mo_settings->set_experimental_features( mo_form_data->get( c_id-experimental_features ) ).
    ENDIF.

    " Store in DB
    li_persistence = Lcl_abapgit_persist_factory=>get_settings( ).
    li_persistence->modify( mo_settings ).

    COMMIT WORK AND WAIT.

    MESSAGE 'Settings succesfully saved' TYPE 'S'.

    mo_form_data = read_settings( ).

  ENDMETHOD.
  METHOD validate_form.

    ro_validation_log = Lcl_abapgit_html_form_utils=>create( mo_form )->validate( io_form_data ).

    IF io_form_data->get( c_id-proxy_url ) IS NOT INITIAL AND io_form_data->get( c_id-proxy_port ) IS INITIAL OR
       io_form_data->get( c_id-proxy_url ) IS INITIAL AND io_form_data->get( c_id-proxy_port ) IS NOT INITIAL.
      ro_validation_log->set(
        iv_key = c_id-proxy_url
        iv_val = |If you specify a proxy, you have to specify host and port| ).
    ENDIF.

    IF ( io_form_data->get( c_id-proxy_url ) IS INITIAL OR io_form_data->get( c_id-proxy_port ) IS INITIAL ) AND
       io_form_data->get( c_id-proxy_auth ) = abap_true.
      ro_validation_log->set(
        iv_key = c_id-proxy_auth
        iv_val = |To turn on authentication, you have to specify host and port| ).
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
endclass. "ZCL_ABAPGIT_GUI_PAGE_SETT_GLOB implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_SETT_INFO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_sett_infoccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_sett_infoccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_SETT_INFO implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( ).
    CREATE OBJECT mo_form_data.
    mo_repo = io_repo.
    mo_form = get_form_schema( ).

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_sett_info.

    CREATE OBJECT lo_component
      EXPORTING
        io_repo = io_repo.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Repository Stats'
      io_page_menu       = Lcl_abapgit_gui_menus=>repo_settings(
                             iv_key = io_repo->get_key( )
                             iv_act = Lif_abapgit_definitions=>c_action-repo_infos )
      ii_child_component = lo_component ).

  ENDMETHOD.
  METHOD format_size.

    DATA:
      lv_size TYPE p LENGTH 16 DECIMALS 2.

    IF iv_size > 1024 * 1024 * 1024.
      lv_size = iv_size / 1024 / 1024 / 1024.
      rv_size = |{ lv_size } GB|.
    ELSEIF iv_size > 1024 * 1024.
      lv_size = iv_size / 1024 / 1024.
      rv_size = |{ lv_size } MB|.
    ELSEIF iv_size > 1024.
      lv_size = iv_size / 1024.
      rv_size = |{ lv_size } KB|.
    ELSE.
      rv_size = |{ iv_size } Bytes|.
    ENDIF.

  ENDMETHOD.
  METHOD format_timestamp.

    DATA lv_short TYPE timestamp.

    IF iv_timestamp IS INITIAL.
      rv_timestamp = 'n/a'.
      RETURN.
    ENDIF.

    cl_abap_tstmp=>move(
      EXPORTING tstmp_src = iv_timestamp
      IMPORTING tstmp_tgt = lv_short ).

    rv_timestamp = |{ lv_short TIMESTAMP = ISO }|.

  ENDMETHOD.
  METHOD format_user.

    DATA lv_title TYPE string.

    IF iv_username IS INITIAL.
      rv_user = 'n/a'.
      RETURN.
    ENDIF.

    IF iv_username <> Lcl_abapgit_objects_super=>c_user_unknown.
      lv_title = Lcl_abapgit_user_record=>get_title( iv_username ).
    ENDIF.

    rv_user = iv_username.
    IF lv_title IS NOT INITIAL.
      rv_user = |{ rv_user } ({ lv_title })|.
    ENDIF.

  ENDMETHOD.
  METHOD get_form_schema.

    DATA lv_label TYPE string.

    ro_form = Lcl_abapgit_html_form=>create(
                iv_form_id   = 'repo-infos-form'
                iv_help_page = 'https://docs.abapgit.org/settings-stats.html' ).

    IF mo_repo->is_offline( ) = abap_true.
      lv_label = 'ZIP File'.
    ELSE.
      lv_label = 'Remote'.
    ENDIF.

    ro_form->start_group(
      iv_name        = c_id-info
      iv_label       = 'Stats'
    )->text(
      iv_name        = c_id-created_by
      iv_label       = 'Created By'
      iv_readonly    = abap_true
    )->text(
      iv_name        = c_id-created_at
      iv_label       = 'Created At'
      iv_readonly    = abap_true
    )->text(
      iv_name        = c_id-deserialized_by
      iv_label       = 'Last Deserialized By'
      iv_readonly    = abap_true
    )->text(
      iv_name        = c_id-deserialized_at
      iv_label       = 'Last Deserialized At'
      iv_readonly    = abap_true
    )->table(
      iv_name        = c_id-stats_table
      iv_label       = 'Statistics'
    )->column(
      iv_label       = 'Measure'
      iv_width       = '50%'
      iv_readonly    = abap_true
    )->column(
      iv_label       = 'Local'
      iv_width       = '25%'
      iv_readonly    = abap_true
    )->column(
      iv_label       = lv_label
      iv_width       = '25%'
      iv_readonly    = abap_true
    )->command(
      iv_label       = 'Back'
      iv_action      = Lif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.
  METHOD read_settings.

    DATA:
      ls_repo  TYPE Lif_abapgit_persistence=>ty_repo,
      ls_stats TYPE ty_stats,
      lv_row   TYPE i,
      lv_int   TYPE i,
      lv_val   TYPE string.

    " Get infos from DB
    TRY.
        ls_repo = Lcl_abapgit_persist_factory=>get_repo( )->read( mo_repo->get_key( ) ).
      CATCH Lcx_abapgit_not_found.
        Lcx_abapgit_exception=>raise( |Repo not found, key { mo_repo->get_key( ) }| ).
    ENDTRY.

    read_stats( ).

    " Infos
    mo_form_data->set(
      iv_key = c_id-created_by
      iv_val = format_user( ls_repo-created_by ) ).
    mo_form_data->set(
      iv_key = c_id-created_at
      iv_val = format_timestamp( ls_repo-created_at ) ).
    mo_form_data->set(
      iv_key = c_id-deserialized_by
      iv_val = format_user( ls_repo-deserialized_by ) ).
    mo_form_data->set(
      iv_key = c_id-deserialized_at
      iv_val = format_timestamp( ls_repo-deserialized_at ) ).

    LOOP AT mt_stats INTO ls_stats.
      lv_row = sy-tabix.
      DO 3 TIMES.
        CASE sy-index.
          WHEN 1.
            lv_val = ls_stats-measure.
          WHEN 2.
            lv_val = ls_stats-local.
          WHEN 3.
            lv_val = ls_stats-remote.
        ENDCASE.

        IF ls_stats-measure CS 'Size' AND sy-index BETWEEN 2 AND 3.
          lv_int = lv_val.
          lv_val = format_size( lv_int ).
        ENDIF.

        mo_form_data->set(
          iv_key = |{ c_id-stats_table }-{ lv_row }-{ sy-index }|
          iv_val = lv_val ).
      ENDDO.
    ENDLOOP.

    mo_form_data->set(
      iv_key = |{ c_id-stats_table }-{ Lif_abapgit_html_form=>c_rows }|
      iv_val = |{ lv_row }| ).

  ENDMETHOD.
  METHOD read_stats.

    DATA:
      lt_local        TYPE Lif_abapgit_definitions=>ty_files_item_tt,
      lt_remote       TYPE Lif_abapgit_git_definitions=>ty_files_tt,
      lt_local_items  TYPE Lif_abapgit_definitions=>ty_items_tt,
      lt_remote_items TYPE Lif_abapgit_definitions=>ty_items_tt.

    CLEAR mt_stats.

    read_stats_files(
      IMPORTING
        et_local  = lt_local
        et_remote = lt_remote ).

    read_stats_state( ).

    read_stats_size_lines_sloc(
      EXPORTING
        it_local        = lt_local
        it_remote       = lt_remote
      IMPORTING
        et_local_items  = lt_local_items
        et_remote_items = lt_remote_items ).

    read_stats_objects(
      CHANGING
        ct_local_items  = lt_local_items
        ct_remote_items = lt_remote_items ).

  ENDMETHOD.
  METHOD read_stats_file.

    TYPES ty_char255 TYPE c LENGTH 255.

    DATA:
      lv_code TYPE string,
      lt_code TYPE STANDARD TABLE OF ty_char255 WITH DEFAULT KEY.

    FIELD-SYMBOLS:
      <ls_code> LIKE LINE OF lt_code.

    rs_info-size = xstrlen( is_file-data ).

    IF is_file-filename CP '*.abap'.
      TRY.
          lv_code = Lcl_abapgit_convert=>xstring_to_string_utf8( is_file-data ).
        CATCH Lcx_abapgit_exception ##NO_HANDLER.
      ENDTRY.

      SPLIT lv_code AT cl_abap_char_utilities=>newline INTO TABLE lt_code.

      rs_info-line = lines( lt_code ).

      LOOP AT lt_code ASSIGNING <ls_code> WHERE table_line IS NOT INITIAL AND table_line(1) <> '*'.
        SHIFT <ls_code> LEFT DELETING LEADING space.
        IF <ls_code>(1) <> '"'.
          rs_info-sloc = rs_info-sloc + 1.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD read_stats_files.

    DATA ls_stats TYPE ty_stats.
    DATA lt_remote_wo_ignored TYPE Lif_abapgit_git_definitions=>ty_files_tt.

    et_local = mo_repo->get_files_local( ).

    ls_stats-measure = 'Number of Files'.
    ls_stats-local   = lines( et_local ).

    IF mo_repo->has_remote_source( ) = abap_true.
      et_remote = mo_repo->get_files_remote( ).
      ls_stats-remote = lines( et_remote ).
      lt_remote_wo_ignored = mo_repo->get_files_remote( iv_ignore_files = abap_true ).
    ENDIF.

    APPEND ls_stats TO mt_stats.

    IF et_remote IS NOT INITIAL.
      CLEAR ls_stats.
      ls_stats-measure = 'Number of Ignored Files'.
      ls_stats-remote = lines( et_remote ) - lines( lt_remote_wo_ignored ).
      APPEND ls_stats TO mt_stats.
    ENDIF.

  ENDMETHOD.
  METHOD read_stats_objects.

    DATA:
      ls_stats           TYPE ty_stats,
      ls_item            TYPE Lif_abapgit_definitions=>ty_item,
      lt_supported_types TYPE Lcl_abapgit_objects=>ty_types_tt.

    ls_stats-measure = 'Number of Objects'.

    DELETE ct_local_items WHERE obj_type IS INITIAL OR obj_name IS INITIAL.
    ls_stats-local = lines( ct_local_items ).

    DELETE ct_remote_items WHERE obj_type IS INITIAL OR obj_name IS INITIAL.
    ls_stats-remote = lines( ct_remote_items ).

    APPEND ls_stats TO mt_stats.

    CLEAR ls_stats.
    ls_stats-measure = 'Number of Unsupported Objects'.
    ls_stats-local   = lines( mo_repo->get_unsupported_objects_local( ) ).

    lt_supported_types = Lcl_abapgit_objects=>supported_list( ).

    LOOP AT ct_remote_items INTO ls_item.
      READ TABLE lt_supported_types WITH KEY table_line = ls_item-obj_type TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        ls_stats-remote = ls_stats-remote + 1.
      ENDIF.
    ENDLOOP.

    APPEND ls_stats TO mt_stats.

  ENDMETHOD.
  METHOD read_stats_size_lines_sloc.

    DATA:
      ls_stats       TYPE ty_stats,
      lv_ignored     TYPE abap_bool,
      ls_info_file   TYPE ty_infos,
      ls_info_local  TYPE ty_infos,
      ls_info_remote TYPE ty_infos,
      ls_item        TYPE Lif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS:
      <ls_local>  LIKE LINE OF it_local,
      <ls_remote> LIKE LINE OF it_remote.

    LOOP AT it_local ASSIGNING <ls_local>.
      ls_info_file = read_stats_file( <ls_local>-file ).

      ls_info_local-size = ls_info_local-size + ls_info_file-size.
      ls_info_local-line = ls_info_local-line + ls_info_file-line.
      ls_info_local-sloc = ls_info_local-sloc + ls_info_file-sloc.

      COLLECT <ls_local>-item INTO et_local_items.
    ENDLOOP.

    IF mo_repo->has_remote_source( ) = abap_true.
      LOOP AT it_remote ASSIGNING <ls_remote> WHERE filename IS NOT INITIAL.
        lv_ignored = mo_repo->get_dot_abapgit( )->is_ignored(
                       iv_filename = <ls_remote>-filename
                       iv_path     = <ls_remote>-path ).

        IF lv_ignored = abap_false.
          ls_info_file = read_stats_file( <ls_remote> ).

          ls_info_remote-size = ls_info_remote-size + ls_info_file-size.
          ls_info_remote-line = ls_info_remote-line + ls_info_file-line.
          ls_info_remote-sloc = ls_info_remote-sloc + ls_info_file-sloc.

          TRY.
              Lcl_abapgit_filename_logic=>file_to_object(
                EXPORTING
                  iv_filename = <ls_remote>-filename
                  iv_path     = <ls_remote>-path
                  iv_devclass = mo_repo->get_package( )
                  io_dot      = mo_repo->get_dot_abapgit( )
                IMPORTING
                  es_item     = ls_item ).
              COLLECT ls_item INTO et_remote_items.
            CATCH Lcx_abapgit_exception ##NO_HANDLER.
          ENDTRY.
        ENDIF.

      ENDLOOP.
    ENDIF.

    ls_stats-measure = 'Size of Files'.
    ls_stats-local   = ls_info_local-size.
    ls_stats-remote  = ls_info_remote-size.
    APPEND ls_stats TO mt_stats.
    ls_stats-measure = 'Lines in ABAP Files'.
    ls_stats-local   = ls_info_local-line.
    ls_stats-remote  = ls_info_remote-line.
    APPEND ls_stats TO mt_stats.
    ls_stats-measure = 'Lines of Code in ABAP Files'.
    ls_stats-local   = ls_info_local-sloc.
    ls_stats-remote  = ls_info_remote-sloc.
    APPEND ls_stats TO mt_stats.

  ENDMETHOD.
  METHOD read_stats_state.

    DATA:
      lt_results TYPE Lif_abapgit_definitions=>ty_results_tt,
      lv_state   TYPE c LENGTH 1,
      ls_stats   TYPE ty_stats.

    FIELD-SYMBOLS:
      <ls_result> LIKE LINE OF lt_results.

    lt_results = Lcl_abapgit_repo_status=>calculate( mo_repo ).

    DO 3 TIMES.
      CLEAR ls_stats.

      CASE sy-index.
        WHEN 1.
          ls_stats-measure = 'Number of Modified Files'.
          lv_state = Lif_abapgit_definitions=>c_state-modified.
        WHEN 2.
          ls_stats-measure = 'Number of Added Files'.
          lv_state = Lif_abapgit_definitions=>c_state-added.
        WHEN 3.
          ls_stats-measure = 'Number of Deleted Files'.
          lv_state = Lif_abapgit_definitions=>c_state-deleted.
      ENDCASE.

      LOOP AT lt_results ASSIGNING <ls_result>.
        IF <ls_result>-lstate = lv_state.
          ls_stats-local = ls_stats-local + 1.
        ENDIF.
        IF <ls_result>-rstate = lv_state AND mo_repo->has_remote_source( ) = abap_true.
          ls_stats-remote = ls_stats-remote + 1.
        ENDIF.
      ENDLOOP.

      APPEND ls_stats TO mt_stats.
    ENDDO.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    IF ii_event->mv_action = Lif_abapgit_definitions=>c_action-go_back.
      rs_handled-state = Lcl_abapgit_gui=>c_event_state-go_back_to_bookmark.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    register_handlers( ).

    read_settings( ).

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( `<div class="repo">` ).

    ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_repo_top(
                    io_repo               = mo_repo
                    iv_show_commit        = abap_false
                    iv_interactive_branch = abap_true ) ).

    ri_html->add( mo_form->render( mo_form_data ) ).

    ri_html->add( `</div>` ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_SETT_INFO implementation

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

    IF Lcl_abapgit_feature=>is_enabled( 'FLOW' ) = abap_true.
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
  METHOD is_customizing_included.

    DATA lt_files TYPE Lif_abapgit_definitions=>ty_files_item_tt.

    lt_files = mo_repo->get_files_local( ).

    READ TABLE lt_files TRANSPORTING NO FIELDS
      WITH KEY item-obj_type = Lif_abapgit_data_config=>c_data_type-tabu. "todo
    IF sy-subrc = 0.
      rv_result = abap_true.
    ENDIF.

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
    APPEND ` <code>color</code> part can be either a css style (see below) or <code>#fg/bg</code> pair,`
      TO lt_fragments.
    APPEND ` where <code>fg</code> and <code>bg</code> are RGB color codes (3 or 6 long).` TO lt_fragments.
    APPEND ` You can also specify just <code>fg</code> or <code>bg</code>` TO lt_fragments.
    APPEND ` (defaults will be used for missing parts).` TO lt_fragments.
    APPEND ` E.g. <code>utils:brown, work:#ff0000/880000, client X:#ddd, client Y:#/333</code>` TO lt_fragments.
    APPEND `<br>Available CSS styles:` TO lt_fragments.
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
    APPEND `orrange` TO lt_labels.
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
      `<p style="margin-top: 0.3em">see also <code>rl-*</code> styles in common.css (styles, forgotten here)</p>`
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
      lv_button      TYPE string,
      lv_label       TYPE string,
      lv_icon        TYPE string,
      lv_hint        TYPE string,
      lv_placeholder TYPE string,
      lv_offline     TYPE abap_bool,
      lv_head_type   TYPE ty_head_type.

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
      lv_button      = 'Switch to Online'.
      lv_icon        = 'plug/darkgrey'.
      lv_label       = 'Repository Name'.
    ELSE.
      lv_button      = 'Switch to Offline'.
      lv_icon        = 'cloud-upload-alt/darkgrey'.
      lv_label       = 'Git Repository URL'.
      lv_hint        = 'URL of original repository'.
      lv_placeholder = 'https://github.com/...git'.
    ENDIF.

    ro_form->start_group(
      iv_name  = c_id-general
      iv_label = 'General'
      iv_hint  = 'Change the general type and origin of the repository'
    )->text(
      iv_name        = c_id-repo_type
      iv_label       = |Type of Repository: { Lcl_abapgit_html=>icon( lv_icon ) }|
      iv_readonly    = abap_true
    )->hidden( c_id-offline
    )->text(
      iv_name        = c_id-url
      iv_condense    = abap_true
      iv_label       = lv_label
      iv_hint        = lv_hint
      iv_placeholder = lv_placeholder ).

    IF lv_offline = abap_false.

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

    rs_settings-url = io_form_data->get( c_id-url ).
    rs_settings-offline = io_form_data->get( c_id-offline ).

    IF rs_settings-offline = abap_false.
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

    DATA: lo_repo_online  TYPE REF TO Lcl_abapgit_repo_online,
          lo_repo_offline TYPE REF TO Lcl_abapgit_repo_offline,
          lv_branch       TYPE ty_remote_settings-branch.

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
      lo_repo_offline ?= io_repo.

      rs_settings-url = lo_repo_offline->get_name( ).
      rs_settings-offline = abap_true.
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
  METHOD initialize_form_data.

    DATA:
      lv_type TYPE string,
      lv_head TYPE string.

    CREATE OBJECT ro_form_data.

    IF ms_settings_snapshot-offline = abap_true.
      lv_type = 'Offline repository'.
    ELSE.
      lv_type = 'Online repository'.
    ENDIF.

    ro_form_data->set(
      iv_key = c_id-offline
      iv_val = ms_settings_snapshot-offline ).
    ro_form_data->set(
      iv_key = c_id-repo_type
      iv_val = lv_type ).
    ro_form_data->set(
      iv_key = c_id-url
      iv_val = ms_settings_snapshot-url ).

    IF ms_settings_snapshot-offline = abap_false.
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
  METHOD save_settings.

    DATA:
      lo_repo_online  TYPE REF TO Lcl_abapgit_repo_online,
      lo_repo_offline TYPE REF TO Lcl_abapgit_repo_offline,
      ls_settings_new TYPE ty_remote_settings.

    ls_settings_new = get_remote_settings_from_form( mo_form_data ).

    " Switch online / offline
    IF ls_settings_new-offline <> ms_settings_snapshot-offline.
      " Remember key, switch, retrieve new instance (todo, refactor #2244)
      mo_repo->switch_repo_type( ls_settings_new-offline ).
      mo_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->get( mo_repo->get_key( ) ).
    ENDIF.

    IF mo_repo->is_offline( ) = abap_true.
      " Offline: Save repo name
      lo_repo_offline ?= mo_repo.
      lo_repo_offline->set_name( ls_settings_new-url ).
    ELSE.
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
      IF lv_url CP 'http*'.
        lv_url = Lcl_abapgit_url=>name( lv_url ).
        mo_form_data->set(
          iv_key = c_id-url
          iv_val = lv_url ).
      ENDIF.

    ELSE.
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

    ro_form->text(
      iv_name        = c_id-version_constant
      iv_label       = 'Version Constant'
      iv_placeholder = 'ZVERSION_CLASS=>VERSION_CONSTANT'
    )->text(
      iv_name        = c_id-version_value
      iv_label       = 'Version Value'
      iv_readonly    = abap_true ).

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

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_SYNTAX <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_syntax===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_syntax===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_SYNTAX implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    super->constructor( ).
    mo_repo = io_repo.
    run_syntax_check( ).
  ENDMETHOD.
  METHOD run_syntax_check.

    DATA: li_syntax_check TYPE REF TO Lif_abapgit_code_inspector.

    li_syntax_check = Lcl_abapgit_factory=>get_code_inspector( mo_repo->get_package( ) ).

    TRY.
        mt_result = li_syntax_check->run( c_variant ).
      CATCH Lcx_abapgit_exception.
        " Variant SYNTAX_CHECK does not exist in 702
        mt_result = li_syntax_check->run( 'VERI_' && c_variant ).
    ENDTRY.

    mv_summary = li_syntax_check->get_summary( ).

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_syntax.

    CREATE OBJECT lo_component
      EXPORTING
        io_repo = io_repo.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Syntax Check'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_actions-rerun.

        run_syntax_check( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

      WHEN OTHERS.
        rs_handled = on_event( ii_event ).
    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    ls_hotkey_action-ui_component = 'Syntax Check'.

    ls_hotkey_action-description = |Re-Run|.
    ls_hotkey_action-action = c_actions-rerun.
    ls_hotkey_action-hotkey = |r|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_menu_provider~get_menu.

    ro_toolbar = build_base_menu( ).

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

    ri_html->add( '<div class="toc">' ).

    ri_html->add( render_variant(
      iv_variant = c_variant
      iv_summary = mv_summary ) ).

    IF lines( mt_result ) = 0.
      ri_html->add( '<div class="dummydiv success">' ).
      ri_html->add( ri_html->icon( 'check' ) ).
      ri_html->add( 'No syntax errors' ).
      ri_html->add( '</div>' ).
    ELSE.
      render_result( ii_html   = ri_html
                     it_result = mt_result ).
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_SYNTAX implementation

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

CLASS SHRIS5ZPAUXVKEPN5HWETLLATB3BTU DEFINITION.
  PUBLIC SECTION.
    METHODS clear
      RETURNING
        VALUE(ro_stack) TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLATB3BTU.

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

CLASS SHRIS5ZPAUXVKEPN5HWETLLATB3BTU IMPLEMENTATION.
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

CLASS SHRIS5ZPAUXVKEPN5HWETLLATB5BTU DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_object_filter.

    METHODS constructor
      IMPORTING
        it_filter TYPE Lif_abapgit_definitions=>ty_tadir_tt.

  PRIVATE SECTION.
    DATA mt_filter TYPE Lif_abapgit_definitions=>ty_tadir_tt.
ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLATB5BTU IMPLEMENTATION.
  METHOD constructor.
    mt_filter = it_filter.
  ENDMETHOD.

  METHOD Lif_abapgit_object_filter~get_filter.
    rt_filter = mt_filter.
  ENDMETHOD.
ENDCLASS.

***************************************************

CLASS SHRIS5ZPAUXVKEPN5HWETLLATB7BTU DEFINITION FINAL.
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_path_name,
        path        TYPE string,
        name        TYPE string,
        remote_sha1 TYPE Lif_abapgit_git_definitions=>ty_sha1,
        local_sha1  TYPE Lif_abapgit_git_definitions=>ty_sha1,
      END OF ty_path_name.
    TYPES:
      ty_path_name_tt TYPE HASHED TABLE OF ty_path_name WITH UNIQUE KEY path name.

    TYPES: BEGIN OF ty_feature,
             repo_name       TYPE string,
             package         TYPE devclass,
             BEGIN OF branch,
               display_name TYPE string,
               sha1         TYPE Lif_abapgit_git_definitions=>ty_sha1,
               up_to_date   TYPE abap_bool,
             END OF branch,
             BEGIN OF pr,
               title TYPE string,
               url   TYPE string,
               draft TYPE abap_bool,
             END OF pr,
             BEGIN OF transport,
               trkorr TYPE trkorr,
               title  TYPE string,
             END OF transport,
             changed_files   TYPE ty_path_name_tt,
             changed_objects TYPE Lif_abapgit_definitions=>ty_items_ts,
           END OF ty_feature.
    TYPES ty_features TYPE STANDARD TABLE OF ty_feature WITH DEFAULT KEY.

    CLASS-METHODS get_information
      RETURNING
        VALUE(rt_features) TYPE ty_features
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

    CLASS-METHODS map_files_to_objects
      IMPORTING
        it_files                  TYPE ty_path_name_tt
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
        ct_features      TYPE ty_features
      RAISING
        Lcx_abapgit_exception.

    CLASS-METHODS try_matching_transports
      IMPORTING
        ii_repo          TYPE REF TO Lif_abapgit_repo
        it_main_expanded TYPE Lif_abapgit_git_definitions=>ty_expanded_tt
      CHANGING
        ct_features      TYPE ty_features
        ct_transports    TYPE ty_transports_tt
      RAISING
        Lcx_abapgit_exception.

    CLASS-METHODS find_up_to_date
      IMPORTING
        iv_url      TYPE string
        it_branches TYPE Lif_abapgit_git_definitions=>ty_git_branch_list_tt
      CHANGING
        ct_features TYPE ty_features
      RAISING
        Lcx_abapgit_exception.

    CLASS-METHODS find_prs
      IMPORTING
        iv_url      TYPE string
      CHANGING
        ct_features TYPE ty_features
      RAISING
        Lcx_abapgit_exception.

    CLASS-METHODS add_local_status
      IMPORTING
        io_online   TYPE REF TO Lcl_abapgit_repo_online
      CHANGING
        ct_features TYPE ty_features
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
        VALUE(rt_files) TYPE ty_path_name_tt.
ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLATB7BTU IMPLEMENTATION.

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
        ls_result-repo_name = li_favorite->get_name( ).
        ls_result-package = li_favorite->get_package( ).
        ls_result-branch-display_name = ls_branch-display_name.
        ls_result-branch-sha1 = ls_branch-sha1.
        INSERT ls_result INTO TABLE lt_features.
      ENDLOOP.

      find_changed_files_all(
        EXPORTING
          io_online   = lo_online
          it_branches = lt_branches
        IMPORTING
          et_main_expanded = lt_main_expanded
        CHANGING
          ct_features = lt_features ).

      try_matching_transports(
        EXPORTING
          ii_repo       = li_favorite
          it_main_expanded = lt_main_expanded
        CHANGING
          ct_transports = lt_transports
          ct_features   = lt_features ).

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

      INSERT LINES OF lt_features INTO TABLE rt_features.
    ENDLOOP.

  ENDMETHOD.

  METHOD try_matching_transports.

    DATA lt_trkorr   LIKE ct_transports.
    DATA ls_trkorr   LIKE LINE OF lt_trkorr.
    DATA ls_result   LIKE LINE OF ct_features.
    DATA lt_packages TYPE Lif_abapgit_sap_package=>ty_devclass_tt.
    DATA lv_package  LIKE LINE OF lt_packages.
    DATA lv_found    TYPE abap_bool.
    DATA ls_changed  LIKE LINE OF ls_result-changed_objects.
    DATA lo_filter   TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLATB5BTU.
    DATA lt_filter   TYPE Lif_abapgit_definitions=>ty_tadir_tt.
    DATA lt_local  TYPE Lif_abapgit_definitions=>ty_files_item_tt.
    DATA ls_changed_file LIKE LINE OF ls_result-changed_files.

    FIELD-SYMBOLS <ls_feature>   LIKE LINE OF ct_features.
    FIELD-SYMBOLS <ls_transport> LIKE LINE OF ct_transports.
    FIELD-SYMBOLS <ls_local>     LIKE LINE OF lt_local.
    FIELD-SYMBOLS <ls_filter>    LIKE LINE OF lt_filter.
    FIELD-SYMBOLS <ls_changed>   LIKE LINE OF <ls_feature>-changed_objects.
    FIELD-SYMBOLS <ls_main_expanded> LIKE LINE OF it_main_expanded.


    SORT ct_transports BY object obj_name.

    LOOP AT ct_features ASSIGNING <ls_feature>.
      LOOP AT <ls_feature>-changed_objects ASSIGNING <ls_changed>.
        READ TABLE ct_transports ASSIGNING <ls_transport>
          WITH KEY object = <ls_changed>-obj_type obj_name = <ls_changed>-obj_name BINARY SEARCH.
        IF sy-subrc = 0.
          <ls_feature>-transport-trkorr = <ls_transport>-trkorr.
          <ls_feature>-transport-title = <ls_transport>-title.

          DELETE ct_transports WHERE trkorr = <ls_transport>-trkorr.
* todo, fill changed objects/files?
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

* unmatched transports
    lt_trkorr = ct_transports.
    SORT lt_trkorr BY trkorr.
    DELETE ADJACENT DUPLICATES FROM lt_trkorr COMPARING trkorr.

    lt_packages = Lcl_abapgit_factory=>get_sap_package( ii_repo->get_package( ) )->list_subpackages( ).

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
      CLEAR lt_filter.
      ls_result-repo_name = ii_repo->get_name( ).
      ls_result-package = ii_repo->get_package( ).
      ls_result-transport-trkorr = <ls_transport>-trkorr.
      ls_result-transport-title = <ls_transport>-title.
      LOOP AT ct_transports ASSIGNING <ls_transport> WHERE trkorr = ls_trkorr-trkorr.
        ls_changed-obj_type = <ls_transport>-object.
        ls_changed-obj_name = <ls_transport>-obj_name.
        INSERT ls_changed INTO TABLE ls_result-changed_objects.

        APPEND INITIAL LINE TO lt_filter ASSIGNING <ls_filter>.
        <ls_filter>-object = <ls_transport>-object.
        <ls_filter>-obj_name = <ls_transport>-obj_name.
      ENDLOOP.

      CREATE OBJECT lo_filter EXPORTING it_filter = lt_filter.
      lt_local = ii_repo->get_files_local_filtered( lo_filter ).
      LOOP AT lt_local ASSIGNING <ls_local> WHERE file-filename <> Lif_abapgit_definitions=>c_dot_abapgit.
        ls_changed_file-path       = <ls_local>-file-path.
        ls_changed_file-name       = <ls_local>-file-filename.
        ls_changed_file-local_sha1 = <ls_local>-file-sha1.

        READ TABLE it_main_expanded ASSIGNING <ls_main_expanded>
          WITH TABLE KEY path_name COMPONENTS
          path = ls_changed_file-path
          name = ls_changed_file-name.
        IF sy-subrc = 0.
          ls_changed_file-remote_sha1 = <ls_main_expanded>-sha1.
        ENDIF.

        INSERT ls_changed_file INTO TABLE ls_result-changed_files.
      ENDLOOP.

      INSERT ls_result INTO TABLE ct_features.
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
    DATA lo_visit   TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLATB3BTU.
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
    DATA lo_filter TYPE REF TO SHRIS5ZPAUXVKEPN5HWETLLATB5BTU.
    DATA lt_filter TYPE Lif_abapgit_definitions=>ty_tadir_tt.

    FIELD-SYMBOLS <ls_branch>       LIKE LINE OF ct_features.
    FIELD-SYMBOLS <ls_local>        LIKE LINE OF lt_local.
    FIELD-SYMBOLS <ls_changed_file> TYPE ty_path_name.
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
          WITH KEY file-filename = <ls_changed_file>-name
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
          iv_filename = <ls_file>-name
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
      ls_path_name-remote_sha1 = <ls_expanded2>-sha1.
      INSERT ls_path_name INTO TABLE rt_files.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

class LCL_ABAPGIT_GUI_PAGE_FLOW implementation.
*"* method's implementations
*include methods.
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

    IF ii_event->mv_action = c_action-refresh.
      rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
    ENDIF.

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
    DATA lt_features   TYPE SHRIS5ZPAUXVKEPN5HWETLLATB7BTU=>ty_features.
    DATA ls_feature    LIKE LINE OF lt_features.
    DATA ls_path_name  LIKE LINE OF ls_feature-changed_files.
    DATA ls_item       LIKE LINE OF ls_feature-changed_objects.
    DATA lv_status     TYPE string.
    DATA lv_full_match TYPE abap_bool.
    DATA li_table      TYPE REF TO Lif_abapgit_html.


    register_handlers( ).
    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.
    ri_html->add( '<div class="repo-overview">' ).

    lt_features = SHRIS5ZPAUXVKEPN5HWETLLATB7BTU=>get_information( ).
    LOOP AT lt_features INTO ls_feature.
      IF lines( ls_feature-changed_files ) = 0.
* no changes, eg. only files outside of starting folder changed
        CONTINUE.
      ENDIF.

      ri_html->add( '<b><font size="+2">' && ls_feature-repo_name ).
      IF ls_feature-branch IS NOT INITIAL.
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

      IF ls_feature-branch IS INITIAL.
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

      CREATE OBJECT li_table TYPE Lcl_abapgit_html.
      lv_full_match = abap_true.

      li_table->add( |<table>| ).
      li_table->add( |<tr><td><u>Filename</u></td><td><u>Remote SHA1</u></td>| &&
                    |<td><u>Local SHA1</u></td><td></td></tr>| ).
      LOOP AT ls_feature-changed_files INTO ls_path_name.

        IF ls_path_name-remote_sha1 = ls_path_name-local_sha1.
          lv_status = 'Match'.
        ELSE.
          lv_full_match = abap_false.
          lv_status = 'Diff'.
        ENDIF.
        li_table->add( |<tr><td><tt>{ ls_path_name-path }{ ls_path_name-name }</tt></td><td>{
          ls_path_name-remote_sha1(7) }</td><td>{
          ls_path_name-local_sha1(7) }</td><td>{ lv_status }</td></tr>| ).
      ENDLOOP.
      li_table->add( |</table>| ).
      LOOP AT ls_feature-changed_objects INTO ls_item.
        li_table->add( |<tt>{ ls_item-obj_type } { ls_item-obj_name }</tt><br>| ).
      ENDLOOP.

      IF lv_full_match = abap_true.
        ri_html->add( |Full Match<br>| ).
      ELSE.
        ri_html->add( li_table ).
      ENDIF.

      ri_html->add( '<br>' ).
    ENDLOOP.

    ri_html->add( '</div>' ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_FLOW implementation

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

*>>>>>>> ZCL_ABAPGIT_POPUP_PULL_REQUEST <<<<<<<*

*"* macro definitions
*include zcl_abapgit_popup_pull_requestccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_popup_pull_requestccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_POPUP_PULL_REQUEST implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    mv_repo_url = iv_url.
  ENDMETHOD.
  METHOD create.

    CREATE OBJECT ri_popup TYPE Lcl_abapgit_popup_pull_request
      EXPORTING
        iv_url = iv_url.

  ENDMETHOD.
  METHOD fetch_pull_request_list.

    rt_pulls = Lcl_abapgit_pr_enumerator=>new( mv_repo_url )->get_pulls( ).

    IF lines( rt_pulls ) = 0.
      Lcx_abapgit_exception=>raise( 'No pull requests found' ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_render_item~render.

    FIELD-SYMBOLS <ls_pr> TYPE Lif_abapgit_pr_enum_provider=>ty_pull_request.

    ASSIGN iv_item TO <ls_pr>.
    ASSERT sy-subrc = 0.

    ri_html = Lcl_abapgit_html=>create( |<b>{ <ls_pr>-number }</b> - { <ls_pr>-title } @{ <ls_pr>-user }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_html_popup~create_picklist.

    CREATE OBJECT ro_picklist
      EXPORTING
        iv_title         = 'Choose Pull Request'
        it_list          = fetch_pull_request_list( )
        ii_item_renderer = me.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_POPUP_PULL_REQUEST implementation

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

*>>>>>>> ZCL_ABAPGIT_SERVICES_ABAPGIT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_services_abapgit==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_services_abapgit==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_SERVICES_ABAPGIT implementation.
*"* method's implementations
*include methods.
  METHOD check_sapgui.

    CONSTANTS:
      lc_hide_sapgui_hint TYPE string VALUE '2'.

    DATA:
      lv_answer           TYPE char1,
      ls_settings         TYPE Lif_abapgit_definitions=>ty_s_user_settings,
      li_user_persistence TYPE REF TO Lif_abapgit_persist_user.

    li_user_persistence = Lcl_abapgit_persistence_user=>get_instance( ).

    ls_settings = li_user_persistence->get_settings( ).

    IF ls_settings-hide_sapgui_hint = abap_true.
      RETURN.
    ENDIF.

    IF Lcl_abapgit_ui_factory=>get_frontend_services( )->is_sapgui_for_java( ) = abap_false.
      RETURN.
    ENDIF.

    lv_answer = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
                    iv_titlebar              = 'Not supported SAPGUI'
                    iv_text_question         = 'SAPGUI for Java is not supported! There might be some issues.'
                    iv_text_button_1         = 'Got it'
                    iv_icon_button_1         = |{ icon_okay }|
                    iv_text_button_2         = 'Hide'
                    iv_icon_button_2         = |{ icon_set_state }|
                    iv_display_cancel_button = abap_false ).

    IF lv_answer = lc_hide_sapgui_hint.
      ls_settings-hide_sapgui_hint = abap_true.
      li_user_persistence->set_settings( ls_settings ).
    ENDIF.

  ENDMETHOD.
  METHOD get_abapgit_tcode.
    CONSTANTS: lc_report_tcode_hex TYPE x VALUE '80'.
    DATA: lt_tcodes TYPE STANDARD TABLE OF tcode.

    SELECT tcode
      FROM tstc
      INTO TABLE lt_tcodes
      WHERE pgmna = sy-cprog
        AND cinfo = lc_report_tcode_hex.

    IF lines( lt_tcodes ) > 0.
      READ TABLE lt_tcodes INDEX 1 INTO rv_tcode.
    ENDIF.
  ENDMETHOD.
  METHOD get_package_from_adt.

    DATA: ls_item    TYPE Lif_abapgit_definitions=>ty_item,
          lr_context TYPE REF TO data,
          lt_fields  TYPE tihttpnvp.


    FIELD-SYMBOLS: <lg_context>    TYPE any,
                   <lv_parameters> TYPE string,
                   <ls_field>      LIKE LINE OF lt_fields.

    ls_item-obj_type = 'CLAS'.
    ls_item-obj_name = 'CL_ADT_GUI_INTEGRATION_CONTEXT'.

    IF Lcl_abapgit_objects=>exists( ls_item ) = abap_false.
      " ADT is not supported in this NW release
      RETURN.
    ENDIF.

    TRY.
        CREATE DATA lr_context TYPE ('CL_ADT_GUI_INTEGRATION_CONTEXT=>TY_CONTEXT_INFO').

        ASSIGN lr_context->* TO <lg_context>.
        ASSERT sy-subrc = 0.

        CALL METHOD ('CL_ADT_GUI_INTEGRATION_CONTEXT')=>read_context
          RECEIVING
            result = <lg_context>.

        ASSIGN COMPONENT 'PARAMETERS'
               OF STRUCTURE <lg_context>
               TO <lv_parameters>.
        ASSERT sy-subrc = 0.

        lt_fields = cl_http_utility=>string_to_fields( cl_http_utility=>unescape_url( <lv_parameters> ) ).

        READ TABLE lt_fields ASSIGNING <ls_field>
                             WITH KEY name = 'p_package_name'.
        IF sy-subrc = 0.
          rv_package = <ls_field>-value.

          " We want to open the repo just once. Therefore we delete the parameters
          " and initialize the ADT context.
          CLEAR <lv_parameters>.
          CALL METHOD ('CL_ADT_GUI_INTEGRATION_CONTEXT')=>initialize_instance
            EXPORTING
              context_info = <lg_context>.

        ENDIF.

      CATCH cx_root.
        " Some problems with dynamic ADT access.
        " Let's ignore it for now and fail silently
    ENDTRY.

  ENDMETHOD.
  METHOD is_installed.

    SELECT SINGLE devclass FROM tadir INTO rv_devclass
      WHERE pgmid = 'R3TR'
      AND object = 'CLAS'
      AND obj_name = c_abapgit_class.

  ENDMETHOD.
  METHOD open_abapgit_changelog.
    open_url_in_browser( |{ c_abapgit_repo }{ c_changelog_path }| ).
  ENDMETHOD.
  METHOD open_abapgit_homepage.
    open_url_in_browser( |{ c_abapgit_homepage }/{ iv_page }| ).
  ENDMETHOD.
  METHOD open_abapgit_wikipage.
    open_url_in_browser( |{ c_abapgit_wikipage }/{ iv_page }| ).
  ENDMETHOD.
  METHOD open_dotabap_homepage.
    open_url_in_browser( c_dotabap_homepage ).
  ENDMETHOD.
  METHOD open_url_in_browser.
    DATA lx_error TYPE REF TO Lcx_abapgit_exception.

    TRY.
        Lcl_abapgit_ui_factory=>get_frontend_services( )->execute( iv_document = iv_url ).
      CATCH Lcx_abapgit_exception INTO lx_error.
        Lcx_abapgit_exception=>raise( iv_text     = 'Opening page in external browser failed.'
                                      ix_previous = lx_error ).
    ENDTRY.
  ENDMETHOD.
  METHOD prepare_gui_startup.

    DATA: lv_repo_key    TYPE Lif_abapgit_persistence=>ty_value,
          lv_package     TYPE devclass,
          lv_package_adt TYPE devclass.

    check_sapgui( ).

    IF Lcl_abapgit_persist_factory=>get_settings( )->read( )->get_show_default_repo( ) = abap_false.
      " Don't show the last seen repo at startup
      Lcl_abapgit_persistence_user=>get_instance( )->set_repo_show( || ).
    ENDIF.

    " We have three special cases for gui startup
    "   - open a specific repo by repo key
    "   - open a specific repo by package name
    "   - open a specific repo by package name provided by ADT
    " These overrule the last shown repo

    GET PARAMETER ID Lif_abapgit_definitions=>c_spagpa_param_repo_key FIELD lv_repo_key.
    GET PARAMETER ID Lif_abapgit_definitions=>c_spagpa_param_package  FIELD lv_package.
    lv_package_adt = get_package_from_adt( ).

    IF lv_repo_key IS NOT INITIAL.

      SET PARAMETER ID Lif_abapgit_definitions=>c_spagpa_param_repo_key FIELD ''.
      Lcl_abapgit_persistence_user=>get_instance( )->set_repo_show( lv_repo_key ).

    ELSEIF lv_package IS NOT INITIAL.

      SET PARAMETER ID Lif_abapgit_definitions=>c_spagpa_param_package FIELD ''.
      set_start_repo_from_package( lv_package ).

    ELSEIF lv_package_adt IS NOT INITIAL.

      set_start_repo_from_package( lv_package_adt ).

    ENDIF.

  ENDMETHOD.
  METHOD set_start_repo_from_package.

    DATA: lo_repo          TYPE REF TO Lcl_abapgit_repo,
          lt_r_package     TYPE RANGE OF devclass,
          ls_r_package     LIKE LINE OF lt_r_package,
          lt_superpackages TYPE Lif_abapgit_sap_package=>ty_devclass_tt,
          li_package       TYPE REF TO Lif_abapgit_sap_package,
          lt_repo_list     TYPE Lif_abapgit_repo_srv=>ty_repo_list.

    FIELD-SYMBOLS: <lo_repo>         TYPE LINE OF Lif_abapgit_repo_srv=>ty_repo_list,
                   <lv_superpackage> LIKE LINE OF lt_superpackages.

    li_package = Lcl_abapgit_factory=>get_sap_package( iv_package ).

    IF li_package->exists( ) = abap_false.
      RETURN.
    ENDIF.

    ls_r_package-sign   = 'I'.
    ls_r_package-option = 'EQ'.
    ls_r_package-low    = iv_package.
    INSERT ls_r_package INTO TABLE lt_r_package.

    " Also consider superpackages. E.g. when some open $abapgit_ui, abapGit repo
    " should be found via package $abapgit
    lt_superpackages = li_package->list_superpackages( ).
    LOOP AT lt_superpackages ASSIGNING <lv_superpackage>.
      ls_r_package-low = <lv_superpackage>.
      INSERT ls_r_package INTO TABLE lt_r_package.
    ENDLOOP.

    lt_repo_list = Lcl_abapgit_repo_srv=>get_instance( )->list( ).

    LOOP AT lt_repo_list ASSIGNING <lo_repo>.

      IF <lo_repo>->get_package( ) IN lt_r_package.
        lo_repo ?= <lo_repo>.
        EXIT.
      ENDIF.

    ENDLOOP.

    IF lo_repo IS BOUND.
      Lcl_abapgit_persistence_user=>get_instance( )->set_repo_show( lo_repo->get_key( ) ).
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SERVICES_ABAPGIT implementation

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

*>>>>>>> ZCL_ABAPGIT_SERVICES_REPO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_services_repo=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_services_repo=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_services_repo=====ccau.











class LCL_ABAPGIT_SERVICES_REPO implementation.
*"* method's implementations
*include methods.
  METHOD activate_objects.

    DATA:
      lo_repo       TYPE REF TO Lcl_abapgit_repo,
      lo_browser    TYPE REF TO Lcl_abapgit_repo_content_list,
      lt_repo_items TYPE Lif_abapgit_definitions=>ty_repo_item_tt,
      lv_count      TYPE i,
      lv_message    TYPE string.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF lt_repo_items.

    lo_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    CREATE OBJECT lo_browser
      EXPORTING
        io_repo = lo_repo.

    lt_repo_items = lo_browser->list( '/' ).

    ri_log = lo_repo->create_new_log( 'Activation Log' ).

    " Add all inactive objects to activation queue
    Lcl_abapgit_objects_activation=>clear( ).

    LOOP AT lt_repo_items ASSIGNING <ls_item> WHERE inactive = abap_true.
      Lcl_abapgit_objects_activation=>add(
        iv_type = <ls_item>-obj_type
        iv_name = <ls_item>-obj_name ).
      lv_count = lv_count + 1.
    ENDLOOP.

    IF lv_count = 0.
      MESSAGE 'No inactive objects found' TYPE 'S'.
      RETURN.
    ENDIF.

    " Activate DDIC + non-DDIC
    Lcl_abapgit_objects_activation=>activate(
      iv_ddic = abap_true
      ii_log  = ri_log ).

    Lcl_abapgit_objects_activation=>activate(
      iv_ddic = abap_false
      ii_log  = ri_log ).

    IF ri_log->get_status( ) <> Lif_abapgit_log=>c_status-error.
      lv_message = |Successfully activated { lv_count } objects|.
      MESSAGE lv_message TYPE 'S'.
    ENDIF.

    lo_repo->refresh( iv_drop_log = abap_false ).

  ENDMETHOD.
  METHOD check_and_create_package.

    IF Lcl_abapgit_factory=>get_sap_package( iv_package )->exists( ) = abap_false.
      " Check if any package is included in remote
      READ TABLE it_remote TRANSPORTING NO FIELDS
        WITH KEY file
        COMPONENTS filename = Lcl_abapgit_filename_logic=>c_package_file.
      IF sy-subrc <> 0.
        " If not, prompt to create it
        create_package( iv_package ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD check_for_restart.

    CONSTANTS:
      lc_abapgit_prog TYPE progname VALUE `ZABAPGIT`.

    DATA lo_repo_online TYPE REF TO Lcl_abapgit_repo_online.

    IF io_repo->is_offline( ) = abap_true.
      RETURN.
    ENDIF.

    lo_repo_online ?= io_repo.

    " If abapGit was used to update itself, then restart to avoid LOAD_PROGRAM_&_MISMATCH dumps
    " because abapGit code was changed at runtime
    IF Lcl_abapgit_ui_factory=>get_frontend_services( )->gui_is_available( ) = abap_true AND
       Lcl_abapgit_url=>is_abapgit_repo( lo_repo_online->get_url( ) ) = abap_true AND
       sy-batch = abap_false AND
       sy-cprog = lc_abapgit_prog.

      IF Lcl_abapgit_persist_factory=>get_settings( )->read( )->get_show_default_repo( ) = abap_false.
        MESSAGE 'abapGit was updated and will restart itself' TYPE 'I'.
      ENDIF.

      SUBMIT (sy-cprog).

    ENDIF.

  ENDMETHOD.
  METHOD check_package.

    DATA:
      li_repo     TYPE REF TO Lif_abapgit_repo,
      li_repo_srv TYPE REF TO Lif_abapgit_repo_srv,
      lv_reason   TYPE string.

    " make sure package is not already in use for a different repository
    " 702: chaining calls with exp&imp parameters causes syntax error
    li_repo_srv = Lcl_abapgit_repo_srv=>get_instance( ).
    li_repo_srv->get_repo_from_package(
      EXPORTING
        iv_package    = is_repo_params-package
        iv_ign_subpkg = is_repo_params-ignore_subpackages
      IMPORTING
        ei_repo    = li_repo
        ev_reason  = lv_reason ).

    IF li_repo IS BOUND.
      Lcx_abapgit_exception=>raise( lv_reason ).
    ENDIF.

  ENDMETHOD.
  METHOD create_package.

    DATA ls_package_data TYPE scompkdtln.
    DATA lv_create       TYPE abap_bool.
    DATA li_popup        TYPE REF TO Lif_abapgit_popups.

    ls_package_data-devclass = condense( to_upper( iv_prefill_package ) ).

    raise_error_if_package_exists( ls_package_data-devclass ).

    li_popup = Lcl_abapgit_ui_factory=>get_popups( ).

    li_popup->popup_to_create_package(
      IMPORTING
        es_package_data = ls_package_data
        ev_create       = lv_create ).

    IF lv_create = abap_true.
      Lcl_abapgit_factory=>get_sap_package( ls_package_data-devclass )->create( ls_package_data ).
      rv_package = ls_package_data-devclass.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDMETHOD.
  METHOD delete_unnecessary_objects.

    DATA:
      ls_checks TYPE Lif_abapgit_definitions=>ty_delete_checks,
      ls_tadir  TYPE Lif_abapgit_definitions=>ty_tadir,
      lt_tadir  TYPE Lif_abapgit_definitions=>ty_tadir_tt.

    FIELD-SYMBOLS <ls_overwrite> LIKE LINE OF is_checks-overwrite.

    " get confirmed deletions
    LOOP AT is_checks-overwrite ASSIGNING <ls_overwrite>
      WHERE ( action = Lif_abapgit_objects=>c_deserialize_action-delete
      OR action = Lif_abapgit_objects=>c_deserialize_action-delete_add )
      AND decision = Lif_abapgit_definitions=>c_yes.

      ls_tadir-pgmid    = 'R3TR'.
      ls_tadir-object   = <ls_overwrite>-obj_type.
      ls_tadir-obj_name = <ls_overwrite>-obj_name.
      ls_tadir-devclass = <ls_overwrite>-devclass.
      INSERT ls_tadir INTO TABLE lt_tadir.

    ENDLOOP.

    " todo, check if object type supports deletion of parts to avoid deleting complete object

    " delete objects
    IF lines( lt_tadir ) > 0.
      ls_checks-transport = is_checks-transport.

      Lcl_abapgit_objects=>delete( it_tadir  = lt_tadir
                                   is_checks = ls_checks
                                   ii_log    = ii_log ).

      io_repo->refresh( iv_drop_log = abap_false ).
    ENDIF.

  ENDMETHOD.
  METHOD gui_deserialize.

    DATA:
      lv_msg    TYPE string,
      ls_checks TYPE Lif_abapgit_definitions=>ty_deserialize_checks,
      li_log    TYPE REF TO Lif_abapgit_log.

    " find troublesome objects
    ls_checks = io_repo->deserialize_checks( ).

    IF ls_checks-overwrite IS INITIAL.
      Lcx_abapgit_exception=>raise(
        'There is nothing to pull. The local state completely matches the remote repository.' ).
    ENDIF.

    " let the user decide what to do
    TRY.
        popup_decisions(
          EXPORTING
            io_repo   = io_repo
          CHANGING
            cs_checks = ls_checks ).

      CATCH Lcx_abapgit_cancel.
        RETURN.
    ENDTRY.

    li_log = io_repo->create_new_log( 'Pull Log' ).

    " pass decisions to delete
    delete_unnecessary_objects(
      io_repo   = io_repo
      is_checks = ls_checks
      ii_log    = li_log ).

    " pass decisions to deserialize
    io_repo->deserialize(
      is_checks = ls_checks
      ii_log    = li_log ).

    IF li_log->get_status( ) = Lif_abapgit_log=>c_status-ok.
      lv_msg = |Repository { io_repo->get_name( ) } successfully pulled for package { io_repo->get_package( ) }|.
      MESSAGE lv_msg TYPE 'S'.
    ENDIF.

    check_for_restart( io_repo ).

  ENDMETHOD.
  METHOD new_offline.

    check_package( is_repo_params ).

    " create new repo and add to favorites
    ro_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->new_offline(
      iv_url            = is_repo_params-url
      iv_package        = is_repo_params-package
      iv_folder_logic   = is_repo_params-folder_logic
      iv_labels         = is_repo_params-labels
      iv_ign_subpkg     = is_repo_params-ignore_subpackages
      iv_main_lang_only = is_repo_params-main_lang_only
      iv_abap_lang_vers = is_repo_params-abap_lang_vers ).

    check_and_create_package(
      iv_package = is_repo_params-package
      it_remote  = ro_repo->get_files_remote( ) ).

    " Make sure there're no leftovers from previous repos
    ro_repo->Lif_abapgit_repo~checksums( )->rebuild( ).

    toggle_favorite( ro_repo->get_key( ) ).

    " Set default repo for user
    Lcl_abapgit_persistence_user=>get_instance( )->set_repo_show( ro_repo->get_key( ) ).

    COMMIT WORK AND WAIT.


  ENDMETHOD.
  METHOD new_online.

    check_package( is_repo_params ).

    ro_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->new_online(
      iv_url            = is_repo_params-url
      iv_branch_name    = is_repo_params-branch_name
      iv_package        = is_repo_params-package
      iv_display_name   = is_repo_params-display_name
      iv_folder_logic   = is_repo_params-folder_logic
      iv_labels         = is_repo_params-labels
      iv_ign_subpkg     = is_repo_params-ignore_subpackages
      iv_main_lang_only = is_repo_params-main_lang_only
      iv_abap_lang_vers = is_repo_params-abap_lang_vers ).

    check_and_create_package(
      iv_package = is_repo_params-package
      it_remote  = ro_repo->get_files_remote( ) ).

    " Make sure there're no leftovers from previous repos
    ro_repo->Lif_abapgit_repo~checksums( )->rebuild( ).

    toggle_favorite( ro_repo->get_key( ) ).

    " Set default repo for user
    Lcl_abapgit_persistence_user=>get_instance( )->set_repo_show( ro_repo->get_key( ) ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.
  METHOD popup_decisions.

    DATA:
      lt_decision     TYPE Lif_abapgit_definitions=>ty_overwrite_tt,
      lt_requirements TYPE Lif_abapgit_dot_abapgit=>ty_requirement_tt,
      lt_dependencies TYPE Lif_abapgit_apack_definitions=>ty_dependencies.

    FIELD-SYMBOLS:
      <ls_overwrite> LIKE LINE OF cs_checks-overwrite,
      <ls_decision>  LIKE LINE OF lt_decision.

    lt_decision = cs_checks-overwrite.

    " If there's a new namespace, it has to be pulled before all other objects
    READ TABLE lt_decision ASSIGNING <ls_decision> WITH KEY obj_type = 'NSPC'.
    IF sy-subrc = 0 AND <ls_decision>-action = Lif_abapgit_objects=>c_deserialize_action-add.
      <ls_decision>-decision = Lif_abapgit_definitions=>c_yes.
    ELSE.
      " Set all new objects to YES
      LOOP AT lt_decision ASSIGNING <ls_decision> WHERE action = Lif_abapgit_objects=>c_deserialize_action-add.
        <ls_decision>-decision = Lif_abapgit_definitions=>c_yes.
      ENDLOOP.
    ENDIF.

    " Ask user what to do
    popup_overwrite( CHANGING ct_overwrite = lt_decision ).
    popup_package_overwrite( CHANGING ct_overwrite = cs_checks-warning_package ).

    IF cs_checks-requirements-met = Lif_abapgit_definitions=>c_no.
      lt_requirements = io_repo->get_dot_abapgit( )->get_data( )-requirements.
      Lcl_abapgit_requirement_helper=>requirements_popup( lt_requirements ).
      cs_checks-requirements-decision = Lif_abapgit_definitions=>c_yes.
    ENDIF.

    IF cs_checks-dependencies-met = Lif_abapgit_definitions=>c_no.
      lt_dependencies = io_repo->get_dot_apack( )->get_manifest_descriptor( )-dependencies.
      Lcl_abapgit_apack_helper=>dependencies_popup( lt_dependencies ).
    ENDIF.

    IF cs_checks-transport-required = abap_true AND cs_checks-transport-transport IS INITIAL.
      cs_checks-transport-transport =
        Lcl_abapgit_ui_factory=>get_popups( )->popup_transport_request( cs_checks-transport-type ).
    ENDIF.

    " Update decisions
    LOOP AT cs_checks-overwrite ASSIGNING <ls_overwrite>.
      READ TABLE lt_decision ASSIGNING <ls_decision> WITH KEY object_type_and_name COMPONENTS
        obj_type = <ls_overwrite>-obj_type
        obj_name = <ls_overwrite>-obj_name.
      ASSERT sy-subrc = 0.
      <ls_overwrite>-decision = <ls_decision>-decision.
    ENDLOOP.

  ENDMETHOD.
  METHOD popup_overwrite.

    DATA: lt_columns  TYPE Lif_abapgit_popups=>ty_alv_column_tt,
          lt_selected LIKE ct_overwrite,
          li_popups   TYPE REF TO Lif_abapgit_popups.
    DATA lt_preselected_rows TYPE Lif_abapgit_popups=>ty_rows.

    FIELD-SYMBOLS: <ls_overwrite> LIKE LINE OF ct_overwrite,
                   <ls_column>    TYPE Lif_abapgit_popups=>ty_alv_column.


    IF lines( ct_overwrite ) = 0.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'OBJ_TYPE'.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'OBJ_NAME'.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'DEVCLASS'.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'STATE'.
    <ls_column>-text = 'State'.
    <ls_column>-length = 3.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'ICON'.
    <ls_column>-text = 'Action'.
    <ls_column>-show_icon = abap_true.
    <ls_column>-length = 5.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'TEXT'.
    <ls_column>-text = 'Description'.

    LOOP AT ct_overwrite ASSIGNING <ls_overwrite> WHERE decision = Lif_abapgit_definitions=>c_yes.
      INSERT sy-tabix INTO TABLE lt_preselected_rows.
    ENDLOOP.

    li_popups = Lcl_abapgit_ui_factory=>get_popups( ).
    li_popups->popup_to_select_from_list(
      EXPORTING
        it_list               = ct_overwrite
        iv_header_text        = |The following objects are different between local and remote repository.|
                             && | Select the objects which should be brought in line with the remote version.|
        iv_select_column_text = 'Change?'
        it_columns_to_display = lt_columns
        it_preselected_rows   = lt_preselected_rows
      IMPORTING
        et_list               = lt_selected ).

    LOOP AT ct_overwrite ASSIGNING <ls_overwrite>.
      READ TABLE lt_selected WITH TABLE KEY object_type_and_name
                             COMPONENTS obj_type = <ls_overwrite>-obj_type
                                        obj_name = <ls_overwrite>-obj_name
                             TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        <ls_overwrite>-decision = Lif_abapgit_definitions=>c_yes.
      ELSE.
        <ls_overwrite>-decision = Lif_abapgit_definitions=>c_no.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD popup_package_overwrite.

    DATA: lt_columns  TYPE Lif_abapgit_popups=>ty_alv_column_tt,
          lt_selected LIKE ct_overwrite,
          li_popups   TYPE REF TO Lif_abapgit_popups.

    FIELD-SYMBOLS: <ls_overwrite> LIKE LINE OF ct_overwrite,
                   <ls_column>    TYPE Lif_abapgit_popups=>ty_alv_column.

    IF lines( ct_overwrite ) = 0.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'OBJ_TYPE'.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'OBJ_NAME'.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'DEVCLASS'.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'STATE'.
    <ls_column>-text = 'State'.
    <ls_column>-length = 3.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'ICON'.
    <ls_column>-text = 'Action'.
    <ls_column>-show_icon = abap_true.
    <ls_column>-length = 5.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'TEXT'.
    <ls_column>-text = 'Description'.

    li_popups = Lcl_abapgit_ui_factory=>get_popups( ).
    li_popups->popup_to_select_from_list(
      EXPORTING
        it_list               = ct_overwrite
        iv_header_text        = |The following objects have been created in other packages.|
                             && | Select the objects which should be overwritten.|
        iv_select_column_text = |Overwrite?|
        it_columns_to_display = lt_columns
      IMPORTING
        et_list               = lt_selected ).

    LOOP AT ct_overwrite ASSIGNING <ls_overwrite>.

      READ TABLE lt_selected WITH TABLE KEY object_type_and_name
                             COMPONENTS obj_type = <ls_overwrite>-obj_type
                                        obj_name = <ls_overwrite>-obj_name
                             TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        <ls_overwrite>-decision = Lif_abapgit_definitions=>c_yes.
      ELSE.
        <ls_overwrite>-decision = Lif_abapgit_definitions=>c_no.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD purge.

    DATA: lt_tadir     TYPE Lif_abapgit_definitions=>ty_tadir_tt,
          lv_answer    TYPE c LENGTH 1,
          lo_repo      TYPE REF TO Lcl_abapgit_repo,
          lv_package   TYPE devclass,
          lv_title     TYPE c LENGTH 20,
          lv_question  TYPE c LENGTH 150,
          ls_checks    TYPE Lif_abapgit_definitions=>ty_delete_checks,
          lv_repo_name TYPE string,
          lv_message   TYPE string.


    lo_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
    lv_repo_name = lo_repo->get_name( ).

    lv_package = lo_repo->get_package( ).
    lt_tadir   = Lcl_abapgit_factory=>get_tadir( )->read( lv_package ).

    IF lines( lt_tadir ) > 0.

      lv_question = |This will DELETE all objects in package { lv_package
        } including subpackages ({ lines( lt_tadir ) } objects) from the system|.

      IF iv_keep_repo = abap_true.
        lv_title = 'Remove Objects'.
        lv_question = lv_question && ', but keep the reference to the repository'.
      ELSE.
        lv_title = 'Uninstall'.
        lv_question = lv_question && ' and remove the reference to the repository'.
      ENDIF.

      lv_answer = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
        iv_titlebar              = lv_title
        iv_text_question         = lv_question
        iv_text_button_1         = 'Delete'
        iv_icon_button_1         = 'ICON_DELETE'
        iv_text_button_2         = 'Cancel'
        iv_icon_button_2         = 'ICON_CANCEL'
        iv_default_button        = '2'
        iv_popup_type            = 'ICON_MESSAGE_WARNING'
        iv_display_cancel_button = abap_false ).

      IF lv_answer = '2'.
        RAISE EXCEPTION TYPE Lcx_abapgit_cancel.
      ENDIF.

    ENDIF.

    ls_checks = lo_repo->delete_checks( ).
    IF ls_checks-transport-required = abap_true.
      ls_checks-transport-transport = Lcl_abapgit_ui_factory=>get_popups(
                                        )->popup_transport_request( ls_checks-transport-type ).
    ENDIF.

    ri_log = Lcl_abapgit_repo_srv=>get_instance( )->purge(
      ii_repo      = lo_repo
      is_checks    = ls_checks
      iv_keep_repo = iv_keep_repo ).

    COMMIT WORK.

    IF ri_log IS BOUND AND ri_log->get_status( ) = Lif_abapgit_log=>c_status-error.
      Lcl_abapgit_log_viewer=>show_log( ri_log ).
      RETURN.
    ENDIF.

    lv_message = |Repository { lv_repo_name } successfully uninstalled from Package { lv_package }. |.
    MESSAGE lv_message TYPE 'S'.

  ENDMETHOD.
  METHOD raise_error_if_package_exists.

    IF iv_devclass IS INITIAL.
      RETURN.
    ENDIF.

    IF Lcl_abapgit_factory=>get_sap_package( iv_devclass )->exists( ) = abap_true.
      Lcx_abapgit_exception=>raise( |Package { iv_devclass } already exists| ).
    ENDIF.

  ENDMETHOD.
  METHOD refresh.

    Lcl_abapgit_repo_srv=>get_instance( )->get( iv_key )->refresh( ).

  ENDMETHOD.
  METHOD refresh_local_checksums.

    DATA: lv_answer   TYPE c,
          lv_question TYPE string,
          lo_repo     TYPE REF TO Lcl_abapgit_repo.


    IF Lcl_abapgit_auth=>is_allowed( Lif_abapgit_auth=>c_authorization-update_local_checksum ) = abap_false.
      Lcx_abapgit_exception=>raise( 'Not authorized' ).
    ENDIF.

    lo_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    lv_question = 'This will rebuild and overwrite local repo checksums.'.

    IF lo_repo->is_offline( ) = abap_false.
      lv_question = lv_question
                && ' The logic: if local and remote file differs then:'
                && ' if remote branch is ahead then assume changes are remote,'
                && ' else (branches are equal) assume changes are local.'
                && ' This will lead to incorrect state for files changed on both sides.'
                && ' Please make sure you don''t have ones like that.'.
    ENDIF.

    lv_answer = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
      iv_titlebar              = 'Warning'
      iv_text_question         = lv_question
      iv_text_button_1         = 'OK'
      iv_icon_button_1         = 'ICON_DELETE'
      iv_text_button_2         = 'Cancel'
      iv_icon_button_2         = 'ICON_CANCEL'
      iv_default_button        = '2'
      iv_display_cancel_button = abap_false ).

    IF lv_answer = '2'.
      RAISE EXCEPTION TYPE Lcx_abapgit_cancel.
    ENDIF.

    lo_repo->Lif_abapgit_repo~checksums( )->rebuild( ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.
  METHOD remove.

    DATA: lv_answer    TYPE c LENGTH 1,
          li_repo      TYPE REF TO Lif_abapgit_repo,
          lv_package   TYPE devclass,
          lv_question  TYPE c LENGTH 200,
          lv_repo_name TYPE string,
          lv_message   TYPE string.


    li_repo      = Lcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
    lv_repo_name = li_repo->get_name( ).
    lv_package   = li_repo->get_package( ).
    lv_question  = |This will remove the repository reference to the package { lv_package
      }. All objects will safely remain in the system.|.

    lv_answer = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
      iv_titlebar              = 'Remove Repository'
      iv_text_question         = lv_question
      iv_text_button_1         = 'Remove'
      iv_icon_button_1         = 'ICON_WF_UNLINK'
      iv_text_button_2         = 'Cancel'
      iv_icon_button_2         = 'ICON_CANCEL'
      iv_default_button        = '2'
      iv_display_cancel_button = abap_false ).

    IF lv_answer = '2'.
      RAISE EXCEPTION TYPE Lcx_abapgit_cancel.
    ENDIF.

    Lcl_abapgit_repo_srv=>get_instance( )->delete( li_repo ).

    COMMIT WORK.

    lv_message = |Reference to repository { lv_repo_name } successfully removed from Package { lv_package }. |.
    MESSAGE lv_message TYPE 'S'.

  ENDMETHOD.
  METHOD toggle_favorite.

    Lcl_abapgit_persistence_user=>get_instance( )->toggle_favorite( iv_key ).

  ENDMETHOD.
  METHOD transport_to_branch.

    DATA:
      lo_repository          TYPE REF TO Lcl_abapgit_repo_online,
      lo_transport_to_branch TYPE REF TO Lcl_abapgit_transport_2_branch,
      lt_transport_headers   TYPE trwbo_request_headers,
      lt_transport_objects   TYPE Lif_abapgit_definitions=>ty_tadir_tt,
      ls_transport_to_branch TYPE Lif_abapgit_definitions=>ty_transport_to_branch.


    IF Lcl_abapgit_auth=>is_allowed( Lif_abapgit_auth=>c_authorization-transport_to_branch ) = abap_false.
      Lcx_abapgit_exception=>raise( 'Not authorized' ).
    ENDIF.

    lo_repository ?= Lcl_abapgit_repo_srv=>get_instance( )->get( iv_repository_key ).

    lt_transport_headers = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_select_transports( ).
    " Also include deleted objects that are included in transport
    lt_transport_objects = Lcl_abapgit_transport=>to_tadir(
      it_transport_headers = lt_transport_headers
      iv_deleted_objects   = abap_true ).
    IF lt_transport_objects IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Canceled or List of objects is empty ' ).
    ENDIF.

    ls_transport_to_branch = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_create_transp_branch(
      lt_transport_headers ).

    CREATE OBJECT lo_transport_to_branch.
    lo_transport_to_branch->create(
      io_repository          = lo_repository
      is_transport_to_branch = ls_transport_to_branch
      it_transport_objects   = lt_transport_objects ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SERVICES_REPO implementation

*>>>>>>> ZCL_ABAPGIT_CONVERT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_convert===========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_convert===========ccimp.
CLASS SHRIS5ZPAUXVKEPN5HWETLLATCHBTU DEFINITION.
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

CLASS SHRIS5ZPAUXVKEPN5HWETLLATCHBTU IMPLEMENTATION.
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

CLASS SHRIS5ZPAUXVKEPN5HWETLLATCJBTU DEFINITION.
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

CLASS SHRIS5ZPAUXVKEPN5HWETLLATCJBTU IMPLEMENTATION.
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
*       CLASS SHRIS5ZPAUXVKEPN5HWETLLATCLBTU DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS SHRIS5ZPAUXVKEPN5HWETLLATCLBTU IMPLEMENTATION
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

    rv_xstring = SHRIS5ZPAUXVKEPN5HWETLLATCJBTU=>convert( iv_string ).

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
  METHOD xstring_remove_bom.

    rv_xstr = iv_xstr.

    " cl_abap_conv_in_ce does not handle BOM in non-Unicode systems, so we remove it
    IF cl_abap_char_utilities=>charsize = 1 AND xstrlen( rv_xstr ) > 3
        AND rv_xstr(3) = cl_abap_char_utilities=>byte_order_mark_utf8.

      rv_xstr = rv_xstr+3.

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

    rv_string = SHRIS5ZPAUXVKEPN5HWETLLATCHBTU=>convert(
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
endclass. "ZCL_ABAPGIT_CONVERT implementation

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
*CLASS SHRIS5ZPAUXVKEPN5HWETLLATCPBTU DEFINITION DEFERRED.
*CLASS zcl_abapgit_environment DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLATCPBTU.




class LCL_ABAPGIT_ENVIRONMENT implementation.
*"* method's implementations
*include methods.
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

CLASS SHRIS5ZPAUXVKEPN5HWETLLATCVBTU DEFINITION FINAL.
  PUBLIC SECTION.
    DATA mt_buffer TYPE string_table.
    METHODS add
      IMPORTING
        iv_str TYPE string.
ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLATCVBTU IMPLEMENTATION.
  METHOD add.
    APPEND iv_str TO mt_buffer.
  ENDMETHOD.
ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLATCXBTU DEFINITION FINAL.
  PUBLIC SECTION.
    DATA mt_log_entries TYPE Lcl_abapgit_news=>ty_logs.
    METHODS add
      IMPORTING
        iv_str TYPE string.
ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLATCXBTU IMPLEMENTATION.
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

*CLASS SHRIS5ZPAUXVKEPN5HWETLLATCZBTU DEFINITION DEFERRED.
*CLASS zcl_abapgit_news DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLATCZBTU.

*----------------------------------------------------------------------*
*       CLASS SHRIS5ZPAUXVKEPN5HWETLLATCZBTU DEFINITION
*----------------------------------------------------------------------*
* Definition of test class for news announcement
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS SHRIS5ZPAUXVKEPN5HWETLLATCZBTU IMPLEMENTATION
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
CLASS SHRIS5ZPAUXVKEPN5HWETLLATC5BTU DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS get_sap_basis_component
      RETURNING
        VALUE(rs_result) TYPE cvers_sdu
      RAISING
        Lcx_abapgit_exception.

ENDCLASS.


CLASS SHRIS5ZPAUXVKEPN5HWETLLATC5BTU IMPLEMENTATION.


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
  METHOD merge.

    FIELD-SYMBOLS <ls_entry> LIKE LINE OF mt_entries.

    LOOP AT io_string_map->mt_entries ASSIGNING <ls_entry>.
      set(
        iv_key = <ls_entry>-k
        iv_val = <ls_entry>-v ).
    ENDLOOP.

    ro_instance = me.

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
endclass. "ZCL_ABAPGIT_STRING_MAP implementation

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
*CLASS SHRIS5ZPAUXVKEPN5HWETLLATDJBTU DEFINITION DEFERRED.

*CLASS zcl_abapgit_user_record DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLATDJBTU.




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
CLASS SHRIS5ZPAUXVKEPN5HWETLLATDNBTU DEFINITION.

  PUBLIC SECTION.
    INTERFACES Lif_abapgit_environment.

    DATA mv_is_cloud TYPE abap_bool.

    METHODS set_cloud
      IMPORTING
        iv_is_cloud TYPE abap_bool.

ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLATDNBTU IMPLEMENTATION.

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

ENDCLASS.

* Helper to toggle experimental features
CLASS SHRIS5ZPAUXVKEPN5HWETLLATDPBTU DEFINITION.

  PUBLIC SECTION.
    INTERFACES Lif_abapgit_persist_settings.

    DATA mo_settings TYPE REF TO Lcl_abapgit_settings.

    METHODS constructor.

ENDCLASS.

CLASS SHRIS5ZPAUXVKEPN5HWETLLATDPBTU IMPLEMENTATION.

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

*>>>>>>> ZCL_ABAPGIT_REPO_LABELS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_repo_labels=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_repo_labels=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_repo_labels=======ccau.


class LCL_ABAPGIT_REPO_LABELS implementation.
*"* method's implementations
*include methods.
  METHOD class_constructor.
    gv_regex = |^[{ c_allowed_chars }]*$|. " Must start with -
  ENDMETHOD.
  METHOD normalize.

    DATA lt_labels TYPE string_table.
    DATA lt_normalized TYPE string_table.
    FIELD-SYMBOLS <lv_lab> LIKE LINE OF lt_labels.

    lt_labels = split( iv_labels ).

    LOOP AT lt_labels ASSIGNING <lv_lab>.
      FIND REGEX gv_regex IN <lv_lab>.
      IF sy-subrc = 0.
        APPEND <lv_lab> TO lt_normalized.
      ENDIF.
    ENDLOOP.

    SORT lt_normalized.
    DELETE ADJACENT DUPLICATES FROM lt_normalized.

    rv_labels = concat_lines_of(
      table = lt_normalized
      sep = `, ` ).

  ENDMETHOD.
  METHOD normalize_colors.

    DATA lt_colors TYPE ty_label_colors.
    DATA lt_normalized TYPE ty_label_colors.
    DATA lt_pairs TYPE string_table.
    DATA lv_pair TYPE string.
    FIELD-SYMBOLS <ls_c> LIKE LINE OF lt_colors.

    lt_colors = split_colors( iv_config ).

    LOOP AT lt_colors ASSIGNING <ls_c>.
      TRY.
          validate_one_label_color( <ls_c> ).
          APPEND <ls_c> TO lt_normalized.
        CATCH Lcx_abapgit_exception.
      ENDTRY.
    ENDLOOP.

    SORT lt_normalized BY label.
    DELETE ADJACENT DUPLICATES FROM lt_normalized COMPARING label.

    LOOP AT lt_normalized ASSIGNING <ls_c>.
      lv_pair = <ls_c>-label && `:` && <ls_c>-color.
      APPEND lv_pair TO lt_pairs.
    ENDLOOP.

    rv_config = concat_lines_of(
      table = lt_pairs
      sep = `, ` ).

  ENDMETHOD.
  METHOD parse_color.

    DATA lv_tmp TYPE string.

    IF iv_color IS INITIAL.
      RETURN.
    ENDIF.

    IF iv_color+0(1) = '#'.
      lv_tmp  = iv_color+1.
      SPLIT lv_tmp AT '/' INTO rs_parsed-fg rs_parsed-bg.
    ELSE.
      rs_parsed-cls = iv_color.
    ENDIF.

  ENDMETHOD.
  METHOD split.

    FIELD-SYMBOLS <lv_lab> LIKE LINE OF rt_labels.

    SPLIT iv_labels AT ',' INTO TABLE rt_labels.
    LOOP AT rt_labels ASSIGNING <lv_lab>.
      CONDENSE <lv_lab>.
    ENDLOOP.
    DELETE rt_labels WHERE table_line IS INITIAL.

  ENDMETHOD.
  METHOD split_colors.

    DATA lt_pairs TYPE string_table.
    DATA lv_clean_config LIKE iv_config.
    DATA ls_c LIKE LINE OF rt_label_colors.
    FIELD-SYMBOLS <lv_pair> LIKE LINE OF lt_pairs.

    lv_clean_config = replace(
      val = iv_config
      sub = cl_abap_char_utilities=>newline
      with = ` ` ). " text area ends with LF

    SPLIT lv_clean_config AT ',' INTO TABLE lt_pairs.
    LOOP AT lt_pairs ASSIGNING <lv_pair>.
      CONDENSE <lv_pair>.
      IF <lv_pair> IS NOT INITIAL.
        SPLIT <lv_pair> AT ':' INTO ls_c-label ls_c-color.
        CONDENSE ls_c-label.
        CONDENSE ls_c-color.
        APPEND ls_c TO rt_label_colors.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD split_colors_into_map.

    DATA lt_colors TYPE ty_label_colors.
    FIELD-SYMBOLS <ls_c> LIKE LINE OF lt_colors.

    lt_colors = split_colors( iv_config ).

    ro_map = Lcl_abapgit_string_map=>create( ).
    LOOP AT lt_colors ASSIGNING <ls_c>.
      TRY.
          ro_map->set(
            iv_key = <ls_c>-label
            iv_val = <ls_c>-color ).
        CATCH Lcx_abapgit_exception.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.
  METHOD validate.

    DATA lt_labels TYPE string_table.
    FIELD-SYMBOLS <lv_lab> LIKE LINE OF lt_labels.

    lt_labels = split( iv_labels ).

    LOOP AT lt_labels ASSIGNING <lv_lab>.
      FIND REGEX gv_regex IN <lv_lab>.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Disallowed chars in label #{ sy-tabix }| ).
      ENDIF.
      " TODO: maybe also limit length ?
    ENDLOOP.

  ENDMETHOD.
  METHOD validate_colors.

    DATA lt_colors TYPE ty_label_colors.
    FIELD-SYMBOLS <ls_c> LIKE LINE OF lt_colors.

    lt_colors = split_colors( iv_config ).

    LOOP AT lt_colors ASSIGNING <ls_c>.
      validate_one_label_color(
        is_lc    = <ls_c>
        iv_index = sy-tabix ).
    ENDLOOP.

  ENDMETHOD.
  METHOD validate_one_label_color.

    DATA ls_parsed_color TYPE ty_color.

    IF is_lc-label IS INITIAL.
      Lcx_abapgit_exception=>raise( |Label is empty in pair #{ iv_index }| ).
    ENDIF.

    IF is_lc-color IS INITIAL.
      Lcx_abapgit_exception=>raise( |Color is empty in pair #{ iv_index }| ).
    ENDIF.

    FIND REGEX gv_regex IN is_lc-label.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Disallowed chars in label in pair #{ iv_index }| ).
    ENDIF.

    ls_parsed_color = parse_color( is_lc-color ).
    IF ls_parsed_color-cls IS NOT INITIAL.
      FIND REGEX '^[-_A-Za-z]+$' IN ls_parsed_color-cls.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Disallowed chars in color in pair #{ iv_index }| ).
      ENDIF.
    ENDIF.
    IF ls_parsed_color-fg IS NOT INITIAL.
      validate_rgb_color( ls_parsed_color-fg ).
    ENDIF.
    IF ls_parsed_color-bg IS NOT INITIAL.
      validate_rgb_color( ls_parsed_color-bg ).
    ENDIF.

  ENDMETHOD.
  METHOD validate_rgb_color.

    DATA lv_len TYPE i.

    IF iv_color IS NOT INITIAL.
      FIND REGEX '^[0-9A-Fa-f]+$' IN iv_color.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Disallowed chars in color in pair #{ iv_index }| ).
      ENDIF.
      lv_len = strlen( iv_color ).
      IF NOT ( lv_len = 3 OR lv_len = 6 ).
        Lcx_abapgit_exception=>raise( |Icorrect color in pair #{ iv_index }| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_REPO_LABELS implementation

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
*CLASS SHRIS5ZPAUXVKEPN5HWETLLATDZBTU DEFINITION DEFERRED.
*CLASS zcl_abapgit_version DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLATDZBTU.



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
*CLASS SHRIS5ZPAUXVKEPN5HWETLLATEBBTU DEFINITION DEFERRED.
*CLASS zcl_abapgit_xml_output DEFINITION LOCAL FRIENDS SHRIS5ZPAUXVKEPN5HWETLLATEBBTU.


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

