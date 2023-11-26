********************************************************************************
*
* LICENSE and NOTICE
*
* See include program ZSHRINKER_ABAPGIT_LICENSE
*
********************************************************************************
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

*>>>>>>> ZCL_ABAPGIT_POPUPS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_popups============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_popups============ccimp.
CLASS SHRITEFUH64VYIPN5I4UHL45BMTR4A DEFINITION FINAL.
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

CLASS SHRITEFUH64VYIPN5I4UHL45BMTR4A IMPLEMENTATION.

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
  METHOD Lif_abapgit_popups~popup_to_select_transport.

    CALL FUNCTION 'TR_F4_REQUESTS'
      IMPORTING
        ev_selected_request = rv_trkorr.

  ENDMETHOD.
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

    DATA ls_data TYPE scompkdtln.

    IF Lcl_abapgit_factory=>get_function_module( )->function_exists( 'PB_POPUP_PACKAGE_CREATE' ) = abap_false.
* looks like the function module used does not exist on all
* versions since 702, so show an error
      Lcx_abapgit_exception=>raise( 'Your system does not support automatic creation of packages.' &&
        'Please, create the package manually.' ).
    ENDIF.

    CALL FUNCTION 'PB_POPUP_PACKAGE_CREATE'
      CHANGING
        p_object_data    = ls_data
      EXCEPTIONS
        action_cancelled = 1.
    ev_create = boolc( sy-subrc = 0 ).
    MOVE-CORRESPONDING ls_data TO es_package_data.
  ENDMETHOD.
  METHOD Lif_abapgit_popups~popup_to_create_transp_branch.
    DATA: lt_fields             TYPE TABLE OF sval,
          lv_transports_as_text TYPE string,
          lv_desc_as_text       TYPE string.
    DATA: lv_branch_name        TYPE spo_value.
    DATA: lv_commit_text        TYPE spo_value.

    CLEAR: rs_transport_branch-branch_name, rs_transport_branch-commit_text.

    lv_transports_as_text = iv_trkorr.
    lv_desc_as_text = Lcl_abapgit_factory=>get_cts_api( )->read_description( iv_trkorr ).

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

    DATA lo_popup TYPE REF TO SHRITEFUH64VYIPN5I4UHL45BMTR4A.

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
  METHOD get_transport_layer.

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
          ls_package TYPE scompkdtln.


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

    MOVE-CORRESPONDING is_package TO ls_package.

    " Set software component to 'HOME' if none is set at this point.
    " Otherwise SOFTWARE_COMPONENT_INVALID will be raised.
    IF ls_package-dlvunit IS INITIAL.
      ls_package-dlvunit = 'HOME'.
    ENDIF.

    " For transportable packages, get default transport and layer
    IF ls_package-devclass(1) <> '$' AND ls_package-pdevclass IS INITIAL.
      ls_package-pdevclass = get_transport_layer( ).
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
          ls_child  TYPE Lif_abapgit_sap_package=>ty_create.


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

    DATA: ls_package TYPE Lif_abapgit_sap_package=>ty_create.


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
  METHOD check_package_exists.

    IF Lcl_abapgit_factory=>get_sap_package( iv_package )->exists( ) = abap_false.
      " Check if any package is included in remote
      READ TABLE it_remote TRANSPORTING NO FIELDS
        WITH KEY file
        COMPONENTS filename = Lcl_abapgit_filename_logic=>c_package_file.
      IF sy-subrc <> 0.
        " If not, give error
        Lcx_abapgit_exception=>raise(
          iv_text     = |Package { iv_package } does not exist and there's no package included in the repository|
          iv_longtext = 'Either select an existing package, create a new one, or add a package to the repository' ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD popup_objects_overwrite.

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

    DATA ls_package_data TYPE Lif_abapgit_sap_package=>ty_create.
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

    DATA lx_error TYPE REF TO Lcx_abapgit_exception.

    check_package( is_repo_params ).

    " create new repo and add to favorites
    ro_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->new_offline(
      iv_name           = is_repo_params-name
      iv_package        = is_repo_params-package
      iv_folder_logic   = is_repo_params-folder_logic
      iv_labels         = is_repo_params-labels
      iv_ign_subpkg     = is_repo_params-ignore_subpackages
      iv_main_lang_only = is_repo_params-main_lang_only
      iv_abap_lang_vers = is_repo_params-abap_lang_vers ).

    TRY.
        check_package_exists(
          iv_package = is_repo_params-package
          it_remote  = ro_repo->get_files_remote( ) ).
      CATCH Lcx_abapgit_exception INTO lx_error.
        Lcl_abapgit_repo_srv=>get_instance( )->delete( ro_repo ).
        COMMIT WORK.
        RAISE EXCEPTION lx_error.
    ENDTRY.

    " Make sure there're no leftovers from previous repos
    ro_repo->Lif_abapgit_repo~checksums( )->rebuild( ).

    toggle_favorite( ro_repo->get_key( ) ).

    " Set default repo for user
    Lcl_abapgit_persistence_user=>get_instance( )->set_repo_show( ro_repo->get_key( ) ).

    COMMIT WORK AND WAIT.


  ENDMETHOD.
  METHOD new_online.

    DATA lx_error TYPE REF TO Lcx_abapgit_exception.

    check_package( is_repo_params ).

    ro_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->new_online(
      iv_url            = is_repo_params-url
      iv_branch_name    = is_repo_params-branch_name
      iv_name           = is_repo_params-name
      iv_package        = is_repo_params-package
      iv_display_name   = is_repo_params-display_name
      iv_folder_logic   = is_repo_params-folder_logic
      iv_labels         = is_repo_params-labels
      iv_ign_subpkg     = is_repo_params-ignore_subpackages
      iv_main_lang_only = is_repo_params-main_lang_only
      iv_abap_lang_vers = is_repo_params-abap_lang_vers ).

    TRY.
        check_package_exists(
          iv_package = is_repo_params-package
          it_remote  = ro_repo->get_files_remote( ) ).
      CATCH Lcx_abapgit_exception INTO lx_error.
        Lcl_abapgit_repo_srv=>get_instance( )->delete( ro_repo ).
        COMMIT WORK.
        RAISE EXCEPTION lx_error.
    ENDTRY.

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
    IF cs_checks-requirements-met = Lif_abapgit_definitions=>c_no.
      lt_requirements = io_repo->get_dot_abapgit( )->get_data( )-requirements.
      Lcl_abapgit_requirement_helper=>requirements_popup( lt_requirements ).
      cs_checks-requirements-decision = Lif_abapgit_definitions=>c_yes.
    ENDIF.

    IF cs_checks-dependencies-met = Lif_abapgit_definitions=>c_no.
      lt_dependencies = io_repo->get_dot_apack( )->get_manifest_descriptor( )-dependencies.
      Lcl_abapgit_apack_helper=>dependencies_popup( lt_dependencies ).
      cs_checks-dependencies-decision = Lif_abapgit_definitions=>c_yes.
    ENDIF.

    popup_objects_overwrite( CHANGING ct_overwrite = lt_decision ).
    popup_package_overwrite( CHANGING ct_overwrite = cs_checks-warning_package ).

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
    lt_tadir   = lo_repo->get_tadir_objects( ).

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
      lt_transport_objects   TYPE Lif_abapgit_definitions=>ty_tadir_tt,
      lv_trkorr              TYPE trkorr,
      ls_transport_to_branch TYPE Lif_abapgit_definitions=>ty_transport_to_branch.


    IF Lcl_abapgit_auth=>is_allowed( Lif_abapgit_auth=>c_authorization-transport_to_branch ) = abap_false.
      Lcx_abapgit_exception=>raise( 'Not authorized' ).
    ENDIF.

    lo_repository ?= Lcl_abapgit_repo_srv=>get_instance( )->get( iv_repository_key ).

    lv_trkorr = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_select_transport( ).
    " Also include deleted objects that are included in transport
    lt_transport_objects = Lcl_abapgit_transport=>to_tadir(
      iv_trkorr          = lv_trkorr
      iv_deleted_objects = abap_true ).
    IF lt_transport_objects IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Canceled or List of objects is empty ' ).
    ENDIF.

    ls_transport_to_branch = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_create_transp_branch( lv_trkorr ).

    CREATE OBJECT lo_transport_to_branch.
    lo_transport_to_branch->create(
      io_repository          = lo_repository
      is_transport_to_branch = ls_transport_to_branch
      it_transport_objects   = lt_transport_objects ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SERVICES_REPO implementation

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

CLASS SHRITEFUH64VYIPN5I4UHL45BKMR4A DEFINITION FINAL.
  PUBLIC SECTION.
    DATA mt_buffer TYPE string_table.
    METHODS add
      IMPORTING
        iv_str TYPE string.
ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BKMR4A IMPLEMENTATION.
  METHOD add.
    APPEND iv_str TO mt_buffer.
  ENDMETHOD.
ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BKOR4A DEFINITION FINAL.
  PUBLIC SECTION.
    DATA mt_log_entries TYPE Lcl_abapgit_news=>ty_logs.
    METHODS add
      IMPORTING
        iv_str TYPE string.
ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BKOR4A IMPLEMENTATION.
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

*CLASS SHRITEFUH64VYIPN5I4UHL45BKQR4A DEFINITION DEFERRED.
*CLASS zcl_abapgit_news DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BKQR4A.

*----------------------------------------------------------------------*
*       CLASS SHRITEFUH64VYIPN5I4UHL45BKQR4A DEFINITION
*----------------------------------------------------------------------*
* Definition of test class for news announcement
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS SHRITEFUH64VYIPN5I4UHL45BKQR4A IMPLEMENTATION
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
*CLASS SHRITEFUH64VYIPN5I4UHL45BQAR4A DEFINITION DEFERRED.
*CLASS zcl_abapgit_version DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BQAR4A.



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
*CLASS SHRITEFUH64VYIPN5I4UHL45BGTR4A DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_branch_list DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BGTR4A.




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
*CLASS SHRITEFUH64VYIPN5I4UHL45BGVR4A DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_commit DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BGVR4A.





*CLASS SHRITEFUH64VYIPN5I4UHL45BGXR4A DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_commit DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BGXR4A.



*CLASS SHRITEFUH64VYIPN5I4UHL45BGZR4A DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_commit DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BGZR4A.



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
endclass. "ZCL_ABAPGIT_GIT_COMMIT implementation

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
*CLASS SHRITEFUH64VYIPN5I4UHL45BHPR4A DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_transport DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BHPR4A.



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



*CLASS zcl_abapgit_gui_event DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BIGR4A.


class LCL_ABAPGIT_GUI_EVENT implementation.
*"* method's implementations
*include methods.
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
  METHOD class_constructor.

    CONSTANTS lc_nbsp TYPE xstring VALUE 'C2A0'. " &nbsp;

    TRY.
        gv_non_breaking_space = Lcl_abapgit_convert=>xstring_to_string_utf8( lc_nbsp ).
      CATCH Lcx_abapgit_exception.
        ASSERT 0 = 1.
    ENDTRY.

  ENDMETHOD.
  METHOD field_keys_to_upper.

    FIELD-SYMBOLS <ls_field> LIKE LINE OF ct_fields.

    LOOP AT ct_fields ASSIGNING <ls_field>.
      <ls_field>-name = to_upper( <ls_field>-name ).
    ENDLOOP.

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
  METHOD new.
    CREATE OBJECT ro_instance
      EXPORTING
        ii_gui_services = ii_gui_services
        iv_action       = iv_action
        iv_getdata      = iv_getdata
        it_postdata     = it_postdata.
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
endclass. "ZCL_ABAPGIT_GUI_EVENT implementation

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



*CLASS SHRITEFUH64VYIPN5I4UHL45BIJR4A DEFINITION DEFERRED.
*CLASS zcl_abapgit_gui_html_processor DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BIJR4A.



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
CLASS SHRITEFUH64VYIPN5I4UHL45BJFR4A DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_gui_renderable.
ENDCLASS.
CLASS SHRITEFUH64VYIPN5I4UHL45BJFR4A IMPLEMENTATION.
  METHOD Lif_abapgit_gui_renderable~render.
    ri_html = Lcl_abapgit_html=>create( 'Hello' ).
  ENDMETHOD.
ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BJHR4A DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_gui_renderable.
ENDCLASS.
CLASS SHRITEFUH64VYIPN5I4UHL45BJHR4A IMPLEMENTATION.
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

*>>>>>>> ZCL_ABAPGIT_JSON_HANDLER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_json_handler======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_json_handler======ccimp.
CLASS SHRITEFUH64VYIPN5I4UHL45BKER4A DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_ajson_filter.
    TYPES:
      BEGIN OF ty_path_value_pair,
        path  TYPE string,
        value TYPE string,
      END OF ty_path_value_pair,
      ty_skip_paths TYPE STANDARD TABLE OF ty_path_value_pair WITH KEY path.

    METHODS constructor
      IMPORTING iv_skip_paths TYPE ty_skip_paths OPTIONAL
      RAISING   Lcx_abapgit_ajson_error.
  PRIVATE SECTION.
    DATA mt_skip_paths TYPE ty_skip_paths.
ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BKER4A IMPLEMENTATION.

  METHOD Lif_abapgit_ajson_filter~keep_node.

    DATA lv_path TYPE string.

    lv_path = is_node-path && is_node-name.

    READ TABLE mt_skip_paths WITH KEY path = lv_path value = is_node-value TRANSPORTING NO FIELDS.
    IF boolc( sy-subrc = 0 ) = abap_true
      AND iv_visit = Lif_abapgit_ajson_filter=>visit_type-value.
      rv_keep = abap_false.
      RETURN.
    ELSE.
      READ TABLE mt_skip_paths WITH KEY path = lv_path TRANSPORTING NO FIELDS.
      IF boolc( sy-subrc = 0 ) = abap_true
        AND iv_visit = Lif_abapgit_ajson_filter=>visit_type-value.
        rv_keep = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    IF is_node-type = 'bool' AND is_node-value = 'false' AND iv_visit = Lif_abapgit_ajson_filter=>visit_type-value.
      rv_keep = abap_false.
      RETURN.
    ENDIF.

    " AFF: if INTEGER type is initial (0) then is will be skipped
    "      However, if type is $required, it should be serialized.
    IF NOT ( ( iv_visit = Lif_abapgit_ajson_filter=>visit_type-value AND is_node-value IS NOT INITIAL ) OR
         ( iv_visit <> Lif_abapgit_ajson_filter=>visit_type-value AND is_node-children > 0 ) ).
      rv_keep = abap_false.
      RETURN.
    ENDIF.

    rv_keep = abap_true.

  ENDMETHOD.

  METHOD constructor.
    " extract annotations and build table for values to be skipped ( path/name | value )
    DATA lo_abap_language_pair TYPE ty_path_value_pair.
    lo_abap_language_pair-path = `/header/abapLanguageVersion`.
    lo_abap_language_pair-value = 'standard'.

    APPEND lo_abap_language_pair TO mt_skip_paths.

    IF iv_skip_paths IS NOT INITIAL.
      APPEND LINES OF iv_skip_paths TO mt_skip_paths.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

class LCL_ABAPGIT_JSON_HANDLER implementation.
*"* method's implementations
*include methods.
  METHOD deserialize.
    DATA lv_json    TYPE string.
    DATA lo_ajson   TYPE REF TO Lif_abapgit_ajson.

    CLEAR ev_data.

    lv_json = Lcl_abapgit_convert=>xstring_to_string_utf8( iv_content ).

    lo_ajson = Lcl_abapgit_ajson=>parse( lv_json
      )->map( Lcl_abapgit_ajson_mapping=>create_to_snake_case( ) ).

    map2abap_original_language( CHANGING co_ajson = lo_ajson ).
    set_defaults( EXPORTING it_defaults = iv_defaults
                  CHANGING  co_ajson    = lo_ajson ).
    map2abap_abap_language_version( CHANGING co_ajson = lo_ajson ).
    map2abap_custom_enum( EXPORTING it_enum_mappings = iv_enum_mappings
                          CHANGING co_ajson          = lo_ajson ).

    lo_ajson->to_abap( IMPORTING ev_container = ev_data ).

  ENDMETHOD.
  METHOD map2abap_abap_language_version.
    DATA:
      lv_enum_abap TYPE string,
      lv_enum_json TYPE string.


    lv_enum_json = co_ajson->get_string( '/header/abap_language_version' ).
    IF lv_enum_json = Lif_abapgit_dot_abapgit=>c_abap_language_version-standard.
      lv_enum_abap = Lif_abapgit_aff_types_v1=>co_abap_language_version_src-standard.
    ELSEIF lv_enum_json = Lif_abapgit_dot_abapgit=>c_abap_language_version-cloud_development.
      lv_enum_abap = Lif_abapgit_aff_types_v1=>co_abap_language_version-cloud_development.
    ELSEIF lv_enum_json = Lif_abapgit_dot_abapgit=>c_abap_language_version-key_user.
      lv_enum_abap = Lif_abapgit_aff_types_v1=>co_abap_language_version-key_user.
    ENDIF.

    co_ajson->set_string( iv_path = '/header/abap_language_version'
                          iv_val  = lv_enum_abap ).
  ENDMETHOD.
  METHOD map2abap_custom_enum.
    DATA:
      lv_enum_json    TYPE string,
      ls_enum_mapping TYPE ty_enum_mapping,
      ls_mapping      TYPE ty_json_abap_mapping.


    LOOP AT it_enum_mappings INTO ls_enum_mapping.
      lv_enum_json = co_ajson->get_string( ls_enum_mapping-path ).
      READ TABLE ls_enum_mapping-mappings WITH KEY json = lv_enum_json INTO ls_mapping.
      IF sy-subrc = 0.
        co_ajson->set_string( iv_path = ls_enum_mapping-path
                              iv_val  = ls_mapping-abap ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD map2abap_original_language.
    DATA:
      lv_iso_language      TYPE laiso,
      lv_original_language TYPE sy-langu.


    lv_iso_language = co_ajson->get_string( '/header/original_language' ).

    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
      EXPORTING
        input  = lv_iso_language
      IMPORTING
        output = lv_original_language.

    co_ajson->set_string( iv_path = '/header/original_language'
                          iv_val  = lv_original_language ).
  ENDMETHOD.
  METHOD map2json_abap_language_version.
    DATA:
      lv_enum_abap TYPE string,
      lv_enum_json TYPE string.


    lv_enum_abap = co_ajson->get_string( '/header/abapLanguageVersion' ).
    IF lv_enum_abap = Lif_abapgit_aff_types_v1=>co_abap_language_version_src-standard
      OR lv_enum_abap = Lif_abapgit_aff_types_v1=>co_abap_language_version-standard.
      lv_enum_json = Lif_abapgit_dot_abapgit=>c_abap_language_version-standard.
    ELSEIF lv_enum_abap = Lif_abapgit_aff_types_v1=>co_abap_language_version-cloud_development.
      lv_enum_json = Lif_abapgit_dot_abapgit=>c_abap_language_version-cloud_development.
    ELSEIF lv_enum_abap = Lif_abapgit_aff_types_v1=>co_abap_language_version-key_user.
      lv_enum_json = Lif_abapgit_dot_abapgit=>c_abap_language_version-key_user.
    ENDIF.

    co_ajson->set_string( iv_path = '/header/abapLanguageVersion'
                          iv_val  = lv_enum_json ).
  ENDMETHOD.
  METHOD map2json_custom_enum.
    DATA:
      lv_enum_abap    TYPE string,
      ls_enum_mapping TYPE ty_enum_mapping,
      ls_mapping      TYPE ty_json_abap_mapping.


    LOOP AT it_enum_mappings INTO ls_enum_mapping.
      lv_enum_abap = co_ajson->get_string( ls_enum_mapping-path ).
      READ TABLE ls_enum_mapping-mappings WITH KEY abap = lv_enum_abap INTO ls_mapping.
      IF sy-subrc = 0.
        co_ajson->set_string( iv_path = ls_enum_mapping-path
                              iv_val  = ls_mapping-json ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD map2json_original_language.
    DATA:
      lv_iso_language      TYPE laiso,
      lv_original_language TYPE sy-langu.


    lv_original_language = co_ajson->get_string( '/header/originalLanguage' ).

    lv_iso_language = Lcl_abapgit_convert=>conversion_exit_isola_output( lv_original_language ).

    TRANSLATE lv_iso_language TO LOWER CASE.
    co_ajson->set_string( iv_path = '/header/originalLanguage'
                          iv_val  = lv_iso_language ).
  ENDMETHOD.
  METHOD serialize.
    DATA: lt_st_source TYPE abap_trans_srcbind_tab,
          lv_json      TYPE string,
          lo_ajson     TYPE REF TO Lif_abapgit_ajson,
          lo_filter    TYPE REF TO SHRITEFUH64VYIPN5I4UHL45BKER4A.

    FIELD-SYMBOLS: <lg_source> LIKE LINE OF lt_st_source.

    APPEND INITIAL LINE TO lt_st_source ASSIGNING <lg_source>.
    GET REFERENCE OF iv_data INTO <lg_source>-value.

    lo_ajson = Lcl_abapgit_ajson=>new( iv_keep_item_order = abap_true
      )->set( iv_path = '/'
              iv_val  = iv_data
      )->map( Lcl_abapgit_ajson_mapping=>create_to_camel_case( ) ).

    map2json_original_language( CHANGING co_ajson = lo_ajson ).
    map2json_abap_language_version( CHANGING co_ajson = lo_ajson ).
    map2json_custom_enum( EXPORTING it_enum_mappings = iv_enum_mappings
                          CHANGING co_ajson          = lo_ajson ).

    CREATE OBJECT lo_filter EXPORTING iv_skip_paths = iv_skip_paths.

    " files end with an empty line (EOF)
    lv_json = lo_ajson->clone( )->filter( lo_filter )->stringify( 2 ) && cl_abap_char_utilities=>newline.

    rv_result = Lcl_abapgit_convert=>string_to_xstring_utf8( lv_json ).
  ENDMETHOD.
  METHOD set_defaults.
    DATA:
      lv_enum_json TYPE string,
      ls_default   TYPE ty_path_value_pair.


    LOOP AT it_defaults INTO ls_default.
      lv_enum_json = co_ajson->get_string( ls_default-path ).
      IF lv_enum_json = ``.
        co_ajson->set_string( iv_path = ls_default-path
                              iv_val  = ls_default-value ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_JSON_HANDLER implementation

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

*CLASS zcl_abapgit_lxe_texts DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BKLR4A.


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

*>>>>>>> ZCL_ABAPGIT_OBJECTS_CHECK <<<<<<<*

*"* macro definitions
*include zcl_abapgit_objects_check=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_objects_check=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_objects_check=====ccau.

*CLASS zcl_abapgit_objects_check DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BK3R4A.


class LCL_ABAPGIT_OBJECTS_CHECK implementation.
*"* method's implementations
*include methods.
  METHOD checks_adjust.

    warning_overwrite_adjust(
      EXPORTING
        it_overwrite = is_checks-overwrite
      CHANGING
        ct_results   = ct_results ).

    warning_package_adjust(
      EXPORTING
        io_repo      = io_repo
        it_overwrite = is_checks-warning_package
      CHANGING
        ct_results   = ct_results ).

  ENDMETHOD.
  METHOD check_multiple_files.

    DATA:
      lv_msg      TYPE string,
      lv_lstate   TYPE c LENGTH 2,
      lv_rstate   TYPE c LENGTH 2,
      lt_res_sort LIKE it_results,
      ls_result   LIKE LINE OF it_results.

    FIELD-SYMBOLS <ls_result> LIKE LINE OF it_results.

    lt_res_sort = it_results.
    SORT lt_res_sort BY filename ASCENDING.

    " Prevent pulling if there is more than one file with the same name
    LOOP AT lt_res_sort ASSIGNING <ls_result>
      WHERE obj_type <> 'DEVC' AND packmove = abap_false AND filename IS NOT INITIAL.
      " Changing package and object at the same time is ok (state: Add + Delete)
      CONCATENATE <ls_result>-lstate ls_result-lstate INTO lv_lstate RESPECTING BLANKS.
      CONCATENATE <ls_result>-rstate ls_result-rstate INTO lv_rstate RESPECTING BLANKS.
      IF <ls_result>-filename = ls_result-filename AND
        lv_lstate <> 'AD' AND lv_lstate <> 'DA' AND lv_rstate <> 'AD' AND lv_rstate <> 'DA'.
        lv_msg = |Pull not possible since there are multiple files with same filename, { <ls_result>-filename }.|
          && | Keep one of the files and delete the other in the repository.|.
        Lcx_abapgit_exception=>raise( lv_msg ).
      ENDIF.
      MOVE-CORRESPONDING <ls_result> TO ls_result.
    ENDLOOP.

  ENDMETHOD.
  METHOD class_constructor.

    gi_exit = Lcl_abapgit_exit=>get_instance( ).

  ENDMETHOD.
  METHOD deserialize_checks.

    DATA: lt_results TYPE Lif_abapgit_definitions=>ty_results_tt,
          li_package TYPE REF TO Lif_abapgit_sap_package.

    " get unfiltered status to evaluate properly which warnings are required
    lt_results = Lcl_abapgit_repo_status=>calculate( io_repo ).

    check_multiple_files( lt_results ).

    rs_checks-overwrite = warning_overwrite_find( lt_results ).

    rs_checks-warning_package = warning_package_find(
      io_repo    = io_repo
      it_results = lt_results ).

    IF lines( lt_results ) > 0.
      li_package = Lcl_abapgit_factory=>get_sap_package( io_repo->get_package( ) ).
      rs_checks-transport-required = li_package->are_changes_recorded_in_tr_req( ).
      IF NOT rs_checks-transport-required IS INITIAL.
        rs_checks-transport-type = li_package->get_transport_type( ).
        rs_checks-transport-transport = determine_transport_request(
                                            io_repo           = io_repo
                                            iv_transport_type = rs_checks-transport-type ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD determine_transport_request.

    " Use transport from repo settings if maintained, or determine via user exit.
    " If transport keeps empty here, it'll requested later via popup.
    rv_transport_request = io_repo->get_local_settings( )-transport_request.

    gi_exit->determine_transport_request(
      EXPORTING
        io_repo              = io_repo
        iv_transport_type    = iv_transport_type
      CHANGING
        cv_transport_request = rv_transport_request ).

  ENDMETHOD.
  METHOD warning_overwrite_adjust.

    DATA: lt_overwrite LIKE it_overwrite,
          ls_overwrite LIKE LINE OF lt_overwrite.

    FIELD-SYMBOLS: <ls_overwrite> LIKE LINE OF lt_overwrite.


* make sure to get the current status, as something might have changed in the meanwhile
    lt_overwrite = warning_overwrite_find( ct_results ).

    LOOP AT lt_overwrite ASSIGNING <ls_overwrite>.

      READ TABLE it_overwrite INTO ls_overwrite
                              WITH TABLE KEY object_type_and_name
                              COMPONENTS obj_type = <ls_overwrite>-obj_type
                                         obj_name = <ls_overwrite>-obj_name.
      IF sy-subrc <> 0 OR ls_overwrite-decision IS INITIAL.
        Lcx_abapgit_exception=>raise( |Overwrite { <ls_overwrite>-obj_type } {
          <ls_overwrite>-obj_name } undecided| ).
      ENDIF.

      IF ls_overwrite-decision = Lif_abapgit_definitions=>c_no.
        DELETE ct_results WHERE
          obj_type = <ls_overwrite>-obj_type AND
          obj_name = <ls_overwrite>-obj_name.
        ASSERT sy-subrc = 0.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD warning_overwrite_find.

    DATA:
      ls_item    TYPE Lif_abapgit_definitions=>ty_item,
      lv_status  TYPE c LENGTH 2,
      lt_changes TYPE STANDARD TABLE OF Lif_abapgit_definitions=>ty_overwrite WITH DEFAULT KEY.

    FIELD-SYMBOLS:
      <ls_result>  LIKE LINE OF it_results,
      <ls_changes> LIKE LINE OF lt_changes.

    " collect all actions for object that have been changed
    LOOP AT it_results ASSIGNING <ls_result> WHERE NOT obj_type IS INITIAL.

      APPEND INITIAL LINE TO lt_changes ASSIGNING <ls_changes>.
      MOVE-CORRESPONDING <ls_result> TO <ls_changes>.
      <ls_changes>-devclass = <ls_result>-package.
      MOVE-CORRESPONDING <ls_changes> TO ls_item.

      IF <ls_result>-packmove = abap_true.
        <ls_changes>-action = Lif_abapgit_objects=>c_deserialize_action-packmove.
        <ls_changes>-icon   = icon_package_standard.
        <ls_changes>-text   = 'Change package assignment'.
      ELSEIF Lcl_abapgit_objects=>is_supported( ls_item ) = abap_false
        AND ls_item-obj_type <> Lif_abapgit_data_config=>c_data_type-tabu.
        <ls_changes>-action = Lif_abapgit_objects=>c_deserialize_action-no_support.
        <ls_changes>-icon   = icon_no_status.
        <ls_changes>-text   = 'Object type not supported'.
      ELSE.
        CONCATENATE <ls_result>-lstate <ls_result>-rstate INTO lv_status RESPECTING BLANKS.
        <ls_changes>-state = lv_status.
        REPLACE ALL OCCURRENCES OF ` ` IN <ls_changes>-state WITH '_'.

        CASE lv_status.
          WHEN '  '. " no changes
            <ls_changes>-action = Lif_abapgit_objects=>c_deserialize_action-none.
          WHEN ' A' OR 'D ' OR 'DM'. " added remotely or deleted locally
            <ls_changes>-action = Lif_abapgit_objects=>c_deserialize_action-add.
            <ls_changes>-icon   = icon_create.
            <ls_changes>-text   = 'Add local object'.
          WHEN 'A ' OR ' D' OR 'MD'. " added locally or deleted remotely
            <ls_changes>-action = Lif_abapgit_objects=>c_deserialize_action-delete.
            <ls_changes>-icon   = icon_delete.
            <ls_changes>-text   = 'Delete local object'.
          WHEN 'M ' OR 'MM'. " modified locally
            <ls_changes>-action = Lif_abapgit_objects=>c_deserialize_action-overwrite.
            <ls_changes>-icon   = icon_change.
            <ls_changes>-text   = 'Overwrite local object'.
          WHEN ' M'. " modified only remotely
            <ls_changes>-action = Lif_abapgit_objects=>c_deserialize_action-update.
            <ls_changes>-icon   = icon_change.
            <ls_changes>-text   = 'Update local object'.
          WHEN OTHERS.
            ASSERT 0 = 1.
        ENDCASE.
      ENDIF.

    ENDLOOP.

    " Remove duplicate actions
    SORT lt_changes.
    DELETE ADJACENT DUPLICATES FROM lt_changes.

    " Check if deletions are for complete object or just a part
    LOOP AT lt_changes ASSIGNING <ls_changes> WHERE action = Lif_abapgit_objects=>c_deserialize_action-delete.

      LOOP AT lt_changes TRANSPORTING NO FIELDS
        WHERE obj_type = <ls_changes>-obj_type AND obj_name = <ls_changes>-obj_name
          AND action <> Lif_abapgit_objects=>c_deserialize_action-delete.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        " There's some other action, so object will be recreated after deletion
        <ls_changes>-action = Lif_abapgit_objects=>c_deserialize_action-delete_add.
        <ls_changes>-icon   = icon_adopt.
        <ls_changes>-text   = 'Delete and recreate local object'.
      ENDIF.

    ENDLOOP.

    DELETE lt_changes WHERE action = Lif_abapgit_objects=>c_deserialize_action-none.

    " If there are multiple changes in an object, keep highest priority action
    SORT lt_changes BY obj_type obj_name action DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_changes COMPARING obj_type obj_name.

    rt_overwrite = lt_changes.

  ENDMETHOD.
  METHOD warning_package_adjust.

    DATA: lt_overwrite LIKE it_overwrite,
          ls_overwrite LIKE LINE OF lt_overwrite.

    FIELD-SYMBOLS: <ls_overwrite> LIKE LINE OF lt_overwrite.


* make sure to get the current status, as something might have changed in the meanwhile
    lt_overwrite = warning_package_find(
      it_results   = ct_results
      io_repo      = io_repo ).

    LOOP AT lt_overwrite ASSIGNING <ls_overwrite>.

      READ TABLE it_overwrite INTO ls_overwrite
                              WITH TABLE KEY object_type_and_name
                              COMPONENTS obj_type = <ls_overwrite>-obj_type
                                         obj_name = <ls_overwrite>-obj_name.
      IF sy-subrc <> 0 OR ls_overwrite-decision IS INITIAL.
        Lcx_abapgit_exception=>raise( |Overwrite of package { <ls_overwrite>-obj_type } {
          <ls_overwrite>-obj_name } undecided| ).
      ENDIF.

      IF ls_overwrite-decision = Lif_abapgit_definitions=>c_no.
        DELETE ct_results WHERE
          obj_type = <ls_overwrite>-obj_type AND
          obj_name = <ls_overwrite>-obj_name.
        ASSERT sy-subrc = 0.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD warning_package_find.

    DATA: lv_package          TYPE devclass,
          lt_overwrite_unique TYPE HASHED TABLE OF Lif_abapgit_definitions=>ty_overwrite
                                  WITH UNIQUE KEY obj_type obj_name devclass,
          ls_overwrite        LIKE LINE OF rt_overwrite,
          ls_tadir            TYPE Lif_abapgit_definitions=>ty_tadir.

    DATA: lo_folder_logic TYPE REF TO Lcl_abapgit_folder_logic.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF it_results.

    lo_folder_logic = Lcl_abapgit_folder_logic=>get_instance( ).
    LOOP AT it_results ASSIGNING <ls_result> WHERE match IS INITIAL AND packmove IS INITIAL.

      lv_package = lo_folder_logic->path_to_package(
        iv_top  = io_repo->get_package( )
        io_dot  = io_repo->get_dot_abapgit( )
        iv_path = <ls_result>-path
        iv_create_if_not_exists = abap_false ).

      ls_tadir = Lcl_abapgit_factory=>get_tadir( )->read_single(
        iv_object   = <ls_result>-obj_type
        iv_obj_name = <ls_result>-obj_name ).

      IF NOT ls_tadir IS INITIAL AND ls_tadir-devclass <> lv_package.
* overwriting object from different package than expected
        CLEAR ls_overwrite.
        CONCATENATE <ls_result>-lstate <ls_result>-rstate INTO ls_overwrite-state RESPECTING BLANKS.
        REPLACE ALL OCCURRENCES OF ` ` IN ls_overwrite-state WITH '_'.
        ls_overwrite-obj_type = <ls_result>-obj_type.
        ls_overwrite-obj_name = <ls_result>-obj_name.
        ls_overwrite-devclass = ls_tadir-devclass.
        ls_overwrite-action   = Lif_abapgit_objects=>c_deserialize_action-overwrite.
        ls_overwrite-icon     = icon_change.
        ls_overwrite-text     = 'Overwrite local object'.
        INSERT ls_overwrite INTO TABLE lt_overwrite_unique.
      ENDIF.

    ENDLOOP.

    rt_overwrite = lt_overwrite_unique.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECTS_CHECK implementation

*>>>>>>> ZCL_ABAPGIT_OBJECTS_FACTORY <<<<<<<*

*"* macro definitions
*include zcl_abapgit_objects_factory===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_objects_factory===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECTS_FACTORY implementation.
*"* method's implementations
*include methods.
  METHOD get_gui_jumper.

    IF gi_gui_jumper IS INITIAL.
      CREATE OBJECT gi_gui_jumper TYPE Lcl_abapgit_gui_jumper.
    ENDIF.

    ri_gui_jumper = gi_gui_jumper.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECTS_FACTORY implementation

*>>>>>>> ZCL_ABAPGIT_OBJECTS_INJECTOR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_objects_injector==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_objects_injector==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECTS_INJECTOR implementation.
*"* method's implementations
*include methods.
  METHOD set_gui_jumper.

    Lcl_abapgit_objects_factory=>gi_gui_jumper = ii_gui_jumper.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECTS_INJECTOR implementation

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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_AIFC implementation

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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_AUTH implementation

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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_BDEF implementation

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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_CLAS implementation

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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_CUS2 implementation

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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_DIAL implementation

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
*CLASS SHRITEFUH64VYIPN5I4UHL45BLCR4A DEFINITION DEFERRED.

*CLASS zcl_abapgit_object_ecatt_super DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BLCR4A.




class LCL_ABAPGIT_OBJECT_ECATT_SUPER implementation.
*"* method's implementations
*include methods.
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

*>>>>>>> ZCL_ABAPGIT_OBJECT_EEEC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_eeec=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_eeec=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_EEEC implementation.
*"* method's implementations
*include methods.
  METHOD get_object_handler.

    DATA lx_error TYPE REF TO cx_root.

    ro_object_handler = super->get_object_handler( ).

    IF ro_object_handler IS NOT BOUND.
      TRY.
          CREATE OBJECT ro_object_handler TYPE ('/IWXBE/CL_EEEC_AFF_OBJECTHANDL').
        CATCH cx_root INTO lx_error.
          Lcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                        ix_previous = lx_error ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lr_data             TYPE REF TO data,
          lo_registry_adapter TYPE REF TO object,
          lv_object_key       TYPE seu_objkey,
          lx_error            TYPE REF TO cx_root.

    FIELD-SYMBOLS: <ls_consumer>   TYPE any,
                   <lv_changed_by> TYPE any.

    TRY.
        CREATE OBJECT lo_registry_adapter TYPE ('/IWXBE/CL_EEEC_REG_ADAPTER').
        CREATE DATA lr_data TYPE ('/IWXBE/IF_REGISTRY_TYPES=>TY_S_CONSUMER').
        ASSIGN lr_data->* TO <ls_consumer>.

        lv_object_key = ms_item-obj_name.

        TRY.
            CALL METHOD lo_registry_adapter->('/IWXBE/IF_EEEC_REG_ADAPTER_WB~GET_METADATA')
              EXPORTING
                iv_object_key = lv_object_key
                iv_state      = 'I'
              RECEIVING
                rs_consumer   = <ls_consumer>.

          CATCH cx_root.
            CALL METHOD lo_registry_adapter->('/IWXBE/IF_EEEC_REG_ADAPTER_WB~GET_METADATA')
              EXPORTING
                iv_object_key = lv_object_key
                iv_state      = 'A'
              RECEIVING
                rs_consumer   = <ls_consumer>.
        ENDTRY.

        ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <ls_consumer> TO <lv_changed_by>.
        rv_user = <lv_changed_by>.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                      ix_previous = lx_error ).
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_EEEC implementation

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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
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
CLASS SHRITEFUH64VYIPN5I4UHL45BMCR4A IMPLEMENTATION.

  METHOD constructor.

    mi_interface = ii_interface.

  ENDMETHOD.

  METHOD SHRITEFUH64VYIPN5I4UHL45BMBR4A~get_elements.

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

  METHOD SHRITEFUH64VYIPN5I4UHL45BMBR4A~set_elements_changeable.

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

  METHOD SHRITEFUH64VYIPN5I4UHL45BMBR4A~save_elements.

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

  METHOD SHRITEFUH64VYIPN5I4UHL45BMBR4A~get_all_attributes.

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

  METHOD SHRITEFUH64VYIPN5I4UHL45BMBR4A~set_changeable.

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

  METHOD SHRITEFUH64VYIPN5I4UHL45BMBR4A~delete.

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

  METHOD SHRITEFUH64VYIPN5I4UHL45BMBR4A~save.

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

  METHOD SHRITEFUH64VYIPN5I4UHL45BMBR4A~remove_elements.

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

  METHOD SHRITEFUH64VYIPN5I4UHL45BMBR4A~add_elements.

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

  METHOD SHRITEFUH64VYIPN5I4UHL45BMBR4A~set_all_attributes.

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

  METHOD SHRITEFUH64VYIPN5I4UHL45BMBR4A~get_changeable.

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
