********************************************************************************
*
* LICENSE and NOTICE
*
* See include program ZSHRINKER_ABAPGIT_LICENSE
*
********************************************************************************

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
*CLASS SHRITEFUH64VYIPN5I4UHL45BILR4A DEFINITION DEFERRED.
*CLASS zcl_abapgit_gui_page_data DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BILR4A.




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

CLASS SHRITEFUH64VYIPN5I4UHL45BISR4A DEFINITION.
  PUBLIC SECTION.
    METHODS clear
      RETURNING
        VALUE(ro_stack) TYPE REF TO SHRITEFUH64VYIPN5I4UHL45BISR4A.

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

CLASS SHRITEFUH64VYIPN5I4UHL45BISR4A IMPLEMENTATION.
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

CLASS SHRITEFUH64VYIPN5I4UHL45BIUR4A DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_object_filter.

    METHODS constructor
      IMPORTING
        it_filter TYPE Lif_abapgit_definitions=>ty_tadir_tt.

  PRIVATE SECTION.
    DATA mt_filter TYPE Lif_abapgit_definitions=>ty_tadir_tt.
ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BIUR4A IMPLEMENTATION.
  METHOD constructor.
    mt_filter = it_filter.
  ENDMETHOD.

  METHOD Lif_abapgit_object_filter~get_filter.
    rt_filter = mt_filter.
  ENDMETHOD.
ENDCLASS.

***************************************************

CLASS SHRITEFUH64VYIPN5I4UHL45BIWR4A DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS get_information
      RETURNING
        VALUE(rt_features) TYPE SHRITEFUH64VYIPN5I4UHL45BIRR4A
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
        VALUE(rs_data) TYPE SHRITEFUH64VYIPN5I4UHL45BIQR4A-repo.

    CLASS-METHODS map_files_to_objects
      IMPORTING
        it_files                  TYPE SHRITEFUH64VYIPN5I4UHL45BIPR4A
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
        ct_features      TYPE SHRITEFUH64VYIPN5I4UHL45BIRR4A
      RAISING
        Lcx_abapgit_exception.

    CLASS-METHODS try_matching_transports
      IMPORTING
        ii_repo          TYPE REF TO Lif_abapgit_repo
        it_main_expanded TYPE Lif_abapgit_git_definitions=>ty_expanded_tt
      CHANGING
        ct_features      TYPE SHRITEFUH64VYIPN5I4UHL45BIRR4A
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
        cs_feature       TYPE SHRITEFUH64VYIPN5I4UHL45BIQR4A
      RAISING
        Lcx_abapgit_exception.

    CLASS-METHODS find_up_to_date
      IMPORTING
        iv_url      TYPE string
        it_branches TYPE Lif_abapgit_git_definitions=>ty_git_branch_list_tt
      CHANGING
        ct_features TYPE SHRITEFUH64VYIPN5I4UHL45BIRR4A
      RAISING
        Lcx_abapgit_exception.

    CLASS-METHODS find_prs
      IMPORTING
        iv_url      TYPE string
      CHANGING
        ct_features TYPE SHRITEFUH64VYIPN5I4UHL45BIRR4A
      RAISING
        Lcx_abapgit_exception.

    CLASS-METHODS add_local_status
      IMPORTING
        io_online   TYPE REF TO Lcl_abapgit_repo_online
      CHANGING
        ct_features TYPE SHRITEFUH64VYIPN5I4UHL45BIRR4A
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
        VALUE(rt_files) TYPE SHRITEFUH64VYIPN5I4UHL45BIPR4A.
ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BIWR4A IMPLEMENTATION.

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
    DATA lo_filter       TYPE REF TO SHRITEFUH64VYIPN5I4UHL45BIUR4A.
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
    DATA lo_visit   TYPE REF TO SHRITEFUH64VYIPN5I4UHL45BISR4A.
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
    DATA lo_filter TYPE REF TO SHRITEFUH64VYIPN5I4UHL45BIUR4A.
    DATA lt_filter TYPE Lif_abapgit_definitions=>ty_tadir_tt.

    FIELD-SYMBOLS <ls_branch>       LIKE LINE OF ct_features.
    FIELD-SYMBOLS <ls_local>        LIKE LINE OF lt_local.
    FIELD-SYMBOLS <ls_changed_file> TYPE SHRITEFUH64VYIPN5I4UHL45BINR4A.
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
    DATA lo_filter  TYPE REF TO SHRITEFUH64VYIPN5I4UHL45BIUR4A.
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
      mt_features = SHRITEFUH64VYIPN5I4UHL45BIWR4A=>get_information( ).
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
CLASS SHRITEFUH64VYIPN5I4UHL45BI3R4A DEFINITION FINAL.
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
        VALUE(ro_me) TYPE REF TO SHRITEFUH64VYIPN5I4UHL45BI3R4A.

ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BI3R4A IMPLEMENTATION.

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

    DATA lo_tab_scheme TYPE REF TO SHRITEFUH64VYIPN5I4UHL45BI3R4A.

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

*CLASS zcl_abapgit_longtexts DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BKKR4A.


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
*       CLASS SHRITEFUH64VYIPN5I4UHL45BKUR4A DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS SHRITEFUH64VYIPN5I4UHL45BKUR4A IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*




*CLASS zcl_abapgit_objects DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BKYR4A.


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
CLASS SHRITEFUH64VYIPN5I4UHL45BLGR4A DEFINITION.
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
      ty_compontents     TYPE SORTED TABLE OF ty_component WITH UNIQUE DEFAULT KEY,
      ty_sub_compontents TYPE SORTED TABLE OF ty_sub_component WITH UNIQUE DEFAULT KEY.

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


CLASS SHRITEFUH64VYIPN5I4UHL45BLGR4A IMPLEMENTATION.

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
      WHERE component~clsname = iv_clif_name
      ORDER BY component~cmpname.          "#EC CI_BUFFJOIN

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

CLASS SHRITEFUH64VYIPN5I4UHL45BLIR4A DEFINITION.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_aff_type_mapping.
  PRIVATE SECTION.
    METHODS set_abapgit_descriptions
      IMPORTING is_clsname          TYPE seoclsname
                is_intf_aff         TYPE Lif_abapgit_aff_intf_v1=>ty_main
      EXPORTING et_descriptions     TYPE Lif_abapgit_oo_object_fnc=>ty_seocompotx_tt
                et_descriptions_sub TYPE Lif_abapgit_oo_object_fnc=>ty_seosubcotx_tt.
ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BLIR4A IMPLEMENTATION.

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
    ls_data_aff-descriptions = SHRITEFUH64VYIPN5I4UHL45BLGR4A=>get_descriptions_compo_subco(
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


CLASS SHRITEFUH64VYIPN5I4UHL45BLKR4A DEFINITION.
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

CLASS SHRITEFUH64VYIPN5I4UHL45BLKR4A IMPLEMENTATION.

  METHOD serialize.
    DATA:
      ls_data_aff      TYPE Lif_abapgit_aff_intf_v1=>ty_main,
      lx_exception     TYPE REF TO cx_root,
      lo_aff_handler   TYPE REF TO Lcl_abapgit_json_handler,
      lo_aff_mapper    TYPE REF TO Lif_abapgit_aff_type_mapping,
      lt_enum_mappings TYPE Lcl_abapgit_json_handler=>ty_enum_mappings,
      lt_paths_to_skip TYPE Lcl_abapgit_json_handler=>ty_skip_paths.


    CREATE OBJECT lo_aff_mapper TYPE SHRITEFUH64VYIPN5I4UHL45BLIR4A.
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
*CLASS SHRITEFUH64VYIPN5I4UHL45BLMR4A DEFINITION DEFERRED.
*CLASS zcl_abapgit_object_intf DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BLMR4A.









class LCL_ABAPGIT_OBJECT_INTF implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    DATA li_aff_registry TYPE REF TO Lif_abapgit_aff_registry.

    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).
    mi_object_oriented_object_fct = Lcl_abapgit_oo_factory=>make( ms_item-obj_type ).

    CREATE OBJECT li_aff_registry TYPE Lcl_abapgit_aff_registry.

    mv_aff_enabled = li_aff_registry->is_supported_object_type( 'INTF' ).

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
    ls_intf_aff = SHRITEFUH64VYIPN5I4UHL45BLKR4A=>deserialize( lv_json_data ).

    CREATE OBJECT lo_aff_mapper TYPE SHRITEFUH64VYIPN5I4UHL45BLIR4A.
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
      lv_serialized_data = SHRITEFUH64VYIPN5I4UHL45BLKR4A=>serialize( ls_intf ).
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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_INTF implementation

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
      WHERE technical_name = ms_item-obj_name
      ORDER BY PRIMARY KEY.
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
endclass. "ZCL_ABAPGIT_OBJECT_IWMO implementation

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
      WHERE technical_name = ms_item-obj_name
      ORDER BY PRIMARY KEY.
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
      WHERE technical_name = ms_item-obj_name
      ORDER BY PRIMARY KEY.
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

    SUBMIT /iwbep/r_dst_vocan_register
      WITH ip_aname = ms_item-obj_name
      WITH ip_avers = ms_item-obj_name+32(4)
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
endclass. "ZCL_ABAPGIT_OBJECT_IWVB implementation

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
      AND sprsl <> mv_language
      ORDER BY langu.                    "#EC CI_BYPASS "#EC CI_GENBUFF

    SORT lt_i18n_langs ASCENDING.

    IF lines( lt_i18n_langs ) > 0.

      SELECT * FROM t100t INTO CORRESPONDING FIELDS OF TABLE lt_t100t
        WHERE sprsl IN lt_language_filter
        AND sprsl <> mv_language
        AND arbgb = lv_msg_id
        ORDER BY PRIMARY KEY.                           "#EC CI_GENBUFF

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
    DATA: ls_t100a      TYPE t100a,
          lv_frozen     TYPE abap_bool,
          lv_message_id TYPE arbgb.

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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_MSAG implementation

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
      WHERE namespace = ms_item-obj_name AND spras <> mv_language
      ORDER BY langu.                                     "#EC CI_SUBRC

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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_NSPC implementation

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
      WHERE icf_name = is_icfservice-icf_name
      ORDER BY PRIMARY KEY.
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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
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
endclass. "ZCL_ABAPGIT_OBJECT_SICF implementation

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
      WHERE paket = ms_item-obj_name.                   "#EC CI_NOORDER
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
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SOTS implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SSFO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ssfo=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ssfo=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SSFO implementation.
*"* method's implementations
*include methods.
  METHOD sort_texts.

    DATA: li_node      TYPE REF TO if_ixml_node,
          li_item      TYPE REF TO if_ixml_node,
          li_field     TYPE REF TO if_ixml_node,
          li_item_list TYPE REF TO if_ixml_node_list,
          li_iterator  TYPE REF TO if_ixml_node_iterator,
          li_items     TYPE REF TO if_ixml_node_iterator,
          lv_index     TYPE i,
          ls_item      TYPE stxfobjt,
          lt_items     TYPE STANDARD TABLE OF stxfobjt.

    FIELD-SYMBOLS <lv_field> TYPE any.

    li_iterator = ii_xml_doc->create_iterator( ).
    li_node = li_iterator->get_next( ).
    WHILE NOT li_node IS INITIAL.
      IF li_node->get_name( ) = 'T_CAPTION'.

        " Read all records for T_CAPTION
        CLEAR lt_items.
        li_item_list = li_node->get_children( ).
        li_items = li_item_list->create_iterator( ).
        DO.
          li_item = li_items->get_next( ).
          IF li_item IS INITIAL.
            EXIT.
          ENDIF.
          CLEAR ls_item.
          li_field = li_item->get_first_child( ).
          WHILE NOT li_field IS INITIAL.
            ASSIGN COMPONENT li_field->get_name( ) OF STRUCTURE ls_item TO <lv_field>.
            ASSERT sy-subrc = 0.
            <lv_field> = li_field->get_value( ).
            li_field = li_field->get_next( ).
          ENDWHILE.
          INSERT ls_item INTO TABLE lt_items.
        ENDDO.

        SORT lt_items.

        " Write all records back after sorting
        lv_index = 1.
        li_items = li_item_list->create_iterator( ).
        DO.
          li_item = li_items->get_next( ).
          IF li_item IS INITIAL.
            EXIT.
          ENDIF.
          READ TABLE lt_items INTO ls_item INDEX lv_index.
          li_field = li_item->get_first_child( ).
          WHILE NOT li_field IS INITIAL.
            ASSIGN COMPONENT li_field->get_name( ) OF STRUCTURE ls_item TO <lv_field>.
            ASSERT sy-subrc = 0.
            li_field->set_value( |{ <lv_field> }| ).
            li_field = li_field->get_next( ).
          ENDWHILE.
          lv_index = lv_index + 1.
        ENDDO.

      ENDIF.
      li_node = li_iterator->get_next( ).
    ENDWHILE.

  ENDMETHOD.
  METHOD code_item_section_handling.
    CONSTANTS: lc_node_item TYPE string VALUE 'item'.
    CONSTANTS: lc_node_text TYPE string VALUE '#text'.

    IF iv_name IN get_range_node_codes( ).
      cv_within_code_section = abap_true.
    ENDIF.

    IF cv_within_code_section = abap_true.
      IF iv_name = lc_node_item.
        TRY.
            ei_code_item_element ?= ii_node.
            RETURN.
          CATCH cx_sy_move_cast_error ##NO_HANDLER.
        ENDTRY.

      ELSEIF iv_name NOT IN get_range_node_codes( ) AND
             iv_name <> lc_node_text.
        cv_within_code_section = abap_false.
      ENDIF.
    ENDIF.

    RAISE EXCEPTION TYPE Lcx_abapgit_exception.

  ENDMETHOD.
  METHOD fix_ids.

    " makes sure ID and IDREF values are the same values for each serialization run
    " the standard code has a counter that keeps increasing values.
    "
    " It is important that IDs and IDREFs which are the same before the fix
    " are also the same after the fix.

    TYPES:
      BEGIN OF ty_id_mapping,
        old TYPE string,
        new TYPE string,
      END OF ty_id_mapping,
      ty_id_mappings TYPE HASHED TABLE OF ty_id_mapping
                          WITH UNIQUE KEY old.

    DATA: lv_name       TYPE string,
          li_idref      TYPE REF TO if_ixml_node,
          li_node       TYPE REF TO if_ixml_node,
          li_attr       TYPE REF TO if_ixml_named_node_map,
          li_iterator   TYPE REF TO if_ixml_node_iterator,
          lt_id_mapping TYPE ty_id_mappings,
          ls_id_mapping LIKE LINE OF lt_id_mapping.

    li_iterator = ii_xml_doc->create_iterator( ).
    li_node = li_iterator->get_next( ).
    WHILE NOT li_node IS INITIAL.
      lv_name = li_node->get_name( ).
      IF lv_name = 'NODE' OR lv_name = 'WINDOW'.
        li_idref = li_node->get_attributes( )->get_named_item( 'IDREF' ).
        IF li_idref IS BOUND.

          ls_id_mapping-old = li_idref->get_value( ).
          READ TABLE lt_id_mapping WITH KEY old = ls_id_mapping-old
                                   INTO ls_id_mapping.
          IF sy-subrc <> 0.
            lv_name = lines( lt_id_mapping ) + 1.
            ls_id_mapping-new = condense( lv_name ).
            INSERT ls_id_mapping INTO TABLE lt_id_mapping.
          ENDIF.

          li_idref->set_value( |{ ls_id_mapping-new }| ).
        ENDIF.
      ENDIF.
      li_node = li_iterator->get_next( ).
    ENDWHILE.

    li_iterator = ii_xml_doc->create_iterator( ).
    li_node = li_iterator->get_next( ).
    WHILE NOT li_node IS INITIAL.
      lv_name = li_node->get_name( ).
      IF lv_name = 'NODE' OR lv_name = 'WINDOW'.
        li_idref = li_node->get_attributes( )->get_named_item( 'ID' ).
        IF li_idref IS BOUND.

          ls_id_mapping-old = li_idref->get_value( ).
          READ TABLE lt_id_mapping WITH KEY old = ls_id_mapping-old
                                   INTO ls_id_mapping.
          IF sy-subrc = 0.
            li_idref->set_value( |{ ls_id_mapping-new }| ).
          ELSE.
            li_attr = li_node->get_attributes( ).
            li_attr->remove_named_item( 'ID' ).
          ENDIF.

        ENDIF.
      ENDIF.
      li_node = li_iterator->get_next( ).
    ENDWHILE.

  ENDMETHOD.
  METHOD get_range_node_codes.

    DATA: ls_range_node_code TYPE LINE OF ty_string_range.

    IF gt_range_node_codes IS INITIAL.
      ls_range_node_code-sign   = 'I'.
      ls_range_node_code-option = 'EQ'.
      ls_range_node_code-low    = 'CODE'.
      INSERT ls_range_node_code INTO TABLE gt_range_node_codes.
      ls_range_node_code-low    = 'GTYPES'.
      INSERT ls_range_node_code INTO TABLE gt_range_node_codes.
      ls_range_node_code-low    = 'GCODING'.
      INSERT ls_range_node_code INTO TABLE gt_range_node_codes.
      ls_range_node_code-low    = 'FCODING'.
      INSERT ls_range_node_code INTO TABLE gt_range_node_codes.
    ENDIF.

    rt_range_node_codes = gt_range_node_codes.

  ENDMETHOD.
  METHOD handle_attrib_leading_spaces.

    DATA li_element        TYPE REF TO if_ixml_element.
    DATA lv_leading_spaces TYPE string.
    DATA lv_coding_line    TYPE string.

    TRY.
        code_item_section_handling( EXPORTING iv_name                = iv_name
                                              ii_node                = ii_node
                                    IMPORTING ei_code_item_element   = li_element
                                    CHANGING  cv_within_code_section = cv_within_code_section ).

* for downwards compatibility, this code can be removed sometime in the future
        lv_leading_spaces = li_element->get_attribute_ns( c_attrib_abapgit_leadig_spaces ).

        lv_coding_line = li_element->get_value( ).
        IF strlen( lv_coding_line ) >= 1 AND lv_coding_line(1) <> | |.
          SHIFT lv_coding_line RIGHT BY lv_leading_spaces PLACES.
          li_element->set_value( lv_coding_line ).
        ENDIF.
      CATCH Lcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE lastuser FROM stxfadm INTO rv_user
      WHERE formname = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_formname TYPE tdsfname.

    lv_formname = ms_item-obj_name.

    CALL FUNCTION 'FB_DELETE_FORM'
      EXPORTING
        i_formname            = lv_formname
        i_with_dialog         = abap_false
        i_with_confirm_dialog = abap_false
      EXCEPTIONS
        no_form               = 1
        OTHERS                = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.
* see function module FB_UPLOAD_FORM

    DATA: li_node                TYPE REF TO if_ixml_node,
          lv_formname            TYPE tdsfname,
          lv_name                TYPE string,
          li_iterator            TYPE REF TO if_ixml_node_iterator,
          lo_sf                  TYPE REF TO cl_ssf_fb_smart_form,
          lo_res                 TYPE REF TO cl_ssf_fb_smart_form,
          lx_error               TYPE REF TO cx_ssf_fb,
          lv_text                TYPE string,
          lv_within_code_section TYPE abap_bool.

    CREATE OBJECT lo_sf.

* set "created by" and "changed by" to current user
    li_iterator = io_xml->get_raw( )->get_root_element( )->create_iterator( ).
    li_node = li_iterator->get_next( ).
    WHILE NOT li_node IS INITIAL.
      lv_name = li_node->get_name( ).
      CASE lv_name.
        WHEN 'LASTDATE'.
          li_node->set_value( sy-datum(4) && '-' && sy-datum+4(2) && '-' && sy-datum+6(2) ).
        WHEN 'LASTTIME'.
          li_node->set_value( sy-uzeit(2) && ':' && sy-uzeit+2(2) && ':' && sy-uzeit+4(2) ).
        WHEN 'FIRSTUSER' OR 'LASTUSER'.
          li_node->set_value( sy-uname && '' ).

      ENDCASE.

      handle_attrib_leading_spaces( EXPORTING iv_name                = lv_name
                                              ii_node                = li_node
                                    CHANGING  cv_within_code_section = lv_within_code_section ).

      li_node = li_iterator->get_next( ).
    ENDWHILE.

    tadir_insert( iv_package ).

    lv_formname = ms_item-obj_name.

    TRY.
        lo_sf->enqueue( suppress_corr_check = space
                        master_language     = mv_language
                        mode                = 'INSERT'
                        formname            = lv_formname ).

        lo_sf->xml_upload( EXPORTING dom      = io_xml->get_raw( )->get_root_element( )
                                     formname = lv_formname
                                     language = mv_language
                           CHANGING  sform    = lo_res ).

        lo_res->store( im_formname = lo_res->header-formname
                       im_language = mv_language
                       im_active   = abap_true ).

        lo_sf->dequeue( lv_formname ).

      CATCH cx_ssf_fb INTO lx_error.
        lv_text = lx_error->get_text( ).
        Lcx_abapgit_exception=>raise( |{ ms_item-obj_type } { ms_item-obj_name }: { lv_text } | ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_formname TYPE stxfadm-formname.

    SELECT SINGLE formname FROM stxfadm INTO lv_formname
      WHERE formname = ms_item-obj_name.
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

    DATA: lv_ssfo_formname TYPE tdsfname.
    DATA lv_inactive TYPE abap_bool.

    lv_ssfo_formname = ms_item-obj_name.

    CALL FUNCTION 'SSF_STATUS_INFO'
      EXPORTING
        i_formname = lv_ssfo_formname
      IMPORTING
        o_inactive = lv_inactive.

    rv_active = boolc( lv_inactive = abap_false ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'E_SMFORM'
                                            iv_argument    = |{ ms_item-obj_name }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: lt_bdcdata  TYPE TABLE OF bdcdata,
          lv_formtype TYPE stxfadm-formtype.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPMSSFO'.
    <ls_bdcdata>-dynpro   = '0100'.
    <ls_bdcdata>-dynbegin = abap_true.

    SELECT SINGLE formtype FROM stxfadm INTO lv_formtype
           WHERE formname = ms_item-obj_name.

    IF lv_formtype = cssf_formtype_text.

      APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
      <ls_bdcdata>-fnam = 'RB_TX'.
      <ls_bdcdata>-fval = abap_true.

      APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
      <ls_bdcdata>-fnam = 'BDC_OKCODE'.
      <ls_bdcdata>-fval = '=RB'.

      APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
      <ls_bdcdata>-program  = 'SAPMSSFO'.
      <ls_bdcdata>-dynpro   = '0100'.
      <ls_bdcdata>-dynbegin = abap_true.

      APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
      <ls_bdcdata>-fnam = 'SSFSCREEN-TNAME'.
      <ls_bdcdata>-fval = ms_item-obj_name.

    ELSE.

      APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
      <ls_bdcdata>-fnam = 'SSFSCREEN-FNAME'.
      <ls_bdcdata>-fval = ms_item-obj_name.

    ENDIF.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=DISPLAY'.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SMARTFORMS'
      it_bdcdata = lt_bdcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.
* see function module FB_DOWNLOAD_FORM

    DATA: lo_sf       TYPE REF TO cl_ssf_fb_smart_form,
          lv_name     TYPE string,
          li_node     TYPE REF TO if_ixml_node,
          li_element  TYPE REF TO if_ixml_element,
          li_iterator TYPE REF TO if_ixml_node_iterator,
          lv_formname TYPE tdsfname,
          li_ixml     TYPE REF TO if_ixml,
          li_xml_doc  TYPE REF TO if_ixml_document.

    li_ixml = cl_ixml=>create( ).
    li_xml_doc = li_ixml->create_document( ).

    CREATE OBJECT lo_sf.
    lv_formname = ms_item-obj_name. " convert type
    TRY.
        lo_sf->load( im_formname = lv_formname
                     im_language = '' ).
      CATCH cx_ssf_fb.
* the smartform is not present in system, or other error occured
        RETURN.
    ENDTRY.

    lo_sf->xml_download( EXPORTING parent   = li_xml_doc
                         CHANGING  document = li_xml_doc ).

    li_iterator = li_xml_doc->create_iterator( ).
    li_node = li_iterator->get_next( ).
    WHILE NOT li_node IS INITIAL.

      lv_name = li_node->get_name( ).
      IF lv_name = 'DEVCLASS'
          OR lv_name = 'LASTDATE'
          OR lv_name = 'LASTTIME'.
        li_node->set_value( '' ).
      ENDIF.
      IF lv_name = 'FIRSTUSER'
          OR lv_name = 'LASTUSER'.
        li_node->set_value( 'DUMMY' ).
      ENDIF.
      li_node = li_iterator->get_next( ).
    ENDWHILE.

    fix_ids( li_xml_doc ).

    sort_texts( li_xml_doc ).

    li_element = li_xml_doc->get_root_element( ).
    li_element->set_attribute(
      name      = 'sf'
      namespace = 'xmlns'
      value     = 'urn:sap-com:SmartForms:2000:internal-structure' ).
    li_element->set_attribute(
      name  = 'xmlns'
      value = 'urn:sap-com:sdixml-ifr:2000' ).

    io_xml->set_raw( li_xml_doc->get_root_element( ) ).

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
endclass. "ZCL_ABAPGIT_OBJECT_SSFO implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SUSO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_suso=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_suso=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SUSO implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_objectname = ms_item-obj_name.

  ENDMETHOD.
  METHOD delete_documentation.

    DATA:
      lv_docu_obj TYPE dokhl-object,
      lv_dummy    TYPE sy-langu.

    lv_docu_obj  = mv_objectname.

    SELECT SINGLE langu
           FROM dokil INTO lv_dummy
           WHERE id   = 'UO'                            "#EC CI_GENBUFF
           AND object = lv_docu_obj.                    "#EC CI_NOORDER

    IF sy-subrc = 0.

      CALL FUNCTION 'DOKU_DELETE_ALL'
        EXPORTING
          doku_id                        = 'UO'
          doku_object                    = lv_docu_obj
          suppress_transport             = space
        EXCEPTIONS
          header_without_text            = 1
          index_without_header           = 2
          no_authority_for_devclass_xxxx = 3
          no_docu_found                  = 4
          object_is_already_enqueued     = 5
          object_is_enqueued_by_corr     = 6
          techn_enqueue_problem          = 7
          user_break                     = 8
          OTHERS                         = 9.

      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD pre_check.

    CONSTANTS:
      lc_act_delete TYPE activ_auth VALUE '06'.

    DATA:
      lv_act_head            TYPE activ_auth,
      lo_suso                TYPE REF TO object,
      lv_failed              TYPE abap_bool,
      lv_suso_collect_in_cts TYPE i,
      ls_clskey              TYPE seoclskey.

    " Downport: CL_SUSO_GEN doesn't exist in 702
    ls_clskey-clsname = |CL_SUSO_GEN|.

    CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
      EXPORTING
        clskey        = ls_clskey
      EXCEPTIONS
        not_specified = 1
        not_existing  = 2
        is_interface  = 3
        no_text       = 4
        inconsistent  = 5
        OTHERS        = 6.

    IF sy-subrc = 0.

      " so these check are not executed in 702

      CREATE OBJECT lo_suso
        TYPE
          ('CL_SUSO_GEN').

      CALL METHOD lo_suso->('SUSO_LOAD_FROM_DB')
        EXPORTING
          id_object = mv_objectname
        RECEIVING
          ed_failed = lv_failed.

      IF lv_failed = abap_true.
        " Object & does not exist; choose an existing object
        MESSAGE s111(01) WITH mv_objectname INTO Lcx_abapgit_exception=>null.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      CALL METHOD lo_suso->('GET_SUSO_EDIT_MODE')
        EXPORTING
          id_object     = mv_objectname
          id_planed_act = lc_act_delete
        IMPORTING
          ed_mode_head  = lv_act_head.

      IF lv_act_head <> lc_act_delete.
        Lcx_abapgit_exception=>raise( |SUSO { mv_objectname }: Delete not allowed. Check where-used in SU21| ).
      ENDIF.

      CALL METHOD lo_suso->('SUSO_COLLECT_IN_CTS')
        EXPORTING
          id_object = mv_objectname
        RECEIVING
          ed_result = lv_suso_collect_in_cts.

      IF lv_suso_collect_in_cts IS NOT INITIAL.
        Lcx_abapgit_exception=>raise( |SUSO { mv_objectname }: Cannot delete| ).
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD regenerate_sap_all.

    DATA: ls_e071  TYPE e071,
          lt_e071  TYPE STANDARD TABLE OF e071,
          lt_e071k TYPE STANDARD TABLE OF e071k.

    ls_e071-pgmid = 'R3TR'.
    ls_e071-object = ms_item-obj_type.
    ls_e071-obj_name = ms_item-obj_name.
    INSERT ls_e071 INTO TABLE lt_e071.

    CALL FUNCTION 'PRGN_AFTER_IMP_SUSO_SAP_ALL'
      EXPORTING
        iv_tarclient  = '000'
        iv_is_upgrade = space
      TABLES
        tt_e071       = lt_e071
        tt_e071k      = lt_e071k.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    SELECT SINGLE modifier FROM tobjvor INTO rv_user
      WHERE objct = mv_objectname.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    " FM SUSR_DELETE_OBJECT calls the UI. Therefore we reimplement it here.
    " As the class CL_SUSO_GEN isn't present in 702, we call dynamically and
    " skip the pre checks on 702 system. That seems ok.

    pre_check( ).

    delete_documentation( ).

    DELETE FROM tobj  WHERE objct  = mv_objectname.
    DELETE FROM tobjt WHERE object = mv_objectname.
    DELETE FROM tactz WHERE brobj  = mv_objectname.

    CALL FUNCTION 'SUPV_DELETE_OBJECT_ASSIGNMENTS'
      EXPORTING
        object_name  = mv_objectname
        all_releases = abap_true.

    CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
      EXPORTING
        object    = mv_objectname
        type      = 'SUSO'
        operation = 'DELETE'.

    regenerate_sap_all( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.
* see function group SUSA

    DATA: lv_objectname TYPE trobj_name,
          ls_tobj       TYPE tobj,
          ls_tobjt      TYPE tobjt,
          ls_tobjvorflg TYPE tobjvorflg,
          lt_tactz      TYPE TABLE OF tactz,
          lt_tobjvordat TYPE TABLE OF tobjvordat,
          lt_tobjvor    TYPE TABLE OF tobjvor.


    ASSERT NOT ms_item-obj_name IS INITIAL.

    io_xml->read( EXPORTING iv_name = 'TOBJ'
                  CHANGING cg_data = ls_tobj ).
    ls_tobj-bname = sy-uname.
    io_xml->read( EXPORTING iv_name = 'TOBJT'
                  CHANGING cg_data = ls_tobjt ).
    io_xml->read( EXPORTING iv_name = 'TOBJVORFLG'
                  CHANGING cg_data = ls_tobjvorflg ).
    io_xml->read( EXPORTING iv_name = 'TACTZ'
                  CHANGING  cg_data = lt_tactz ).
    io_xml->read( EXPORTING iv_name = 'TOBJVORDAT'
                  CHANGING  cg_data = lt_tobjvordat ).
    io_xml->read( EXPORTING iv_name = 'TOBJVOR'
                  CHANGING  cg_data = lt_tobjvor ).

    tadir_insert( iv_package ).

    lv_objectname = mv_objectname.

    CALL FUNCTION 'SUSR_COMMEDITCHECK'
      EXPORTING
        objectname      = lv_objectname
        transobjecttype = 'O'.

    MODIFY tobj FROM ls_tobj.                             "#EC CI_SUBRC
    MODIFY tobjt FROM ls_tobjt.                           "#EC CI_SUBRC
    MODIFY tobjvorflg FROM ls_tobjvorflg.                 "#EC CI_SUBRC
    DELETE FROM tactz WHERE brobj = ms_item-obj_name.     "#EC CI_SUBRC
    INSERT tactz FROM TABLE lt_tactz.                     "#EC CI_SUBRC
    DELETE FROM tobjvordat WHERE objct = ms_item-obj_name. "#EC CI_SUBRC
    INSERT tobjvordat FROM TABLE lt_tobjvordat.           "#EC CI_SUBRC
    DELETE FROM tobjvor WHERE objct = ms_item-obj_name.   "#EC CI_SUBRC
    INSERT tobjvor FROM TABLE lt_tobjvor.                 "#EC CI_SUBRC

    deserialize_longtexts( ii_xml         = io_xml
                           iv_longtext_id = c_longtext_id_suso ).

    regenerate_sap_all( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_objct TYPE tobj-objct.


    SELECT SINGLE objct FROM tobj INTO lv_objct
      WHERE objct = ms_item-obj_name.

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

    CALL FUNCTION 'SUSR_SHOW_OBJECT'
      EXPORTING
        object = mv_objectname.

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_tobj       TYPE tobj,
          ls_tobjt      TYPE tobjt,
          ls_tobjvorflg TYPE tobjvorflg,
          lt_tactz      TYPE TABLE OF tactz,
          lt_tobjvordat TYPE TABLE OF tobjvordat,
          lt_tobjvor    TYPE TABLE OF tobjvor.


    SELECT SINGLE * FROM tobj INTO ls_tobj
      WHERE objct = ms_item-obj_name.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    CLEAR ls_tobj-bname.

    SELECT SINGLE * FROM tobjt INTO ls_tobjt
      WHERE object = ms_item-obj_name
      AND langu = mv_language.                          "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'TOBJT no english description'
        && ' for object (' && ms_item-obj_name && ')' ).
    ENDIF.

    SELECT SINGLE * FROM tobjvorflg INTO ls_tobjvorflg
      WHERE objct = ms_item-obj_name.                     "#EC CI_SUBRC

    SELECT * FROM tactz INTO TABLE lt_tactz
      WHERE brobj = ms_item-obj_name
      ORDER BY PRIMARY KEY.               "#EC CI_SUBRC "#EC CI_GENBUFF

    SELECT * FROM tobjvordat INTO TABLE lt_tobjvordat
      WHERE objct = ms_item-obj_name
      ORDER BY PRIMARY KEY.               "#EC CI_SUBRC "#EC CI_GENBUFF

    SELECT * FROM tobjvor INTO TABLE lt_tobjvor
      WHERE objct = ms_item-obj_name
      ORDER BY PRIMARY KEY.                               "#EC CI_SUBRC

    io_xml->add( iv_name = 'TOBJ'
                 ig_data = ls_tobj ).
    io_xml->add( iv_name = 'TOBJT'
                 ig_data = ls_tobjt ).
    io_xml->add( iv_name = 'TOBJVORFLG'
                 ig_data = ls_tobjvorflg ).
    io_xml->add( ig_data = lt_tactz
                 iv_name = 'TACTZ' ).
    io_xml->add( ig_data = lt_tobjvordat
                 iv_name = 'TOBJVORDAT' ).
    io_xml->add( ig_data = lt_tobjvor
                 iv_name = 'TOBJVOR' ).

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_suso ).

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
endclass. "ZCL_ABAPGIT_OBJECT_SUSO implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_TABL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_tabl=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_tabl=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_object_tabl=======ccau.


class LCL_ABAPGIT_OBJECT_TABL implementation.
*"* method's implementations
*include methods.
  METHOD clear_dd03p_fields.

    CONSTANTS lc_comptype_dataelement TYPE comptype VALUE 'E'.

    DATA: lv_masklen TYPE c LENGTH 4.

    FIELD-SYMBOLS: <ls_dd03p> TYPE dd03p.

* remove nested structures
    DELETE ct_dd03p WHERE depth <> '00'.
* remove fields from .INCLUDEs
    DELETE ct_dd03p WHERE adminfield <> '0'.

    LOOP AT ct_dd03p ASSIGNING <ls_dd03p> WHERE NOT rollname IS INITIAL.

      clear_dd03p_fields_common( CHANGING cs_dd03p = <ls_dd03p> ).

      lv_masklen = <ls_dd03p>-masklen.
      IF lv_masklen = '' OR NOT lv_masklen CO '0123456789'.
* make sure the field contains valid data, or the XML will dump
        CLEAR <ls_dd03p>-masklen.
      ENDIF.

      IF <ls_dd03p>-comptype = lc_comptype_dataelement.
        clear_dd03p_fields_dataelement( CHANGING cs_dd03p = <ls_dd03p> ).
      ENDIF.

      IF <ls_dd03p>-shlporigin = 'D'.
* search help from domain
        CLEAR: <ls_dd03p>-shlpfield,
               <ls_dd03p>-shlpname.
      ENDIF.

* XML output assumes correct field content
      IF <ls_dd03p>-routputlen = '      '.
        CLEAR <ls_dd03p>-routputlen.
      ENDIF.

    ENDLOOP.

    " Clear position to avoid issues with include structures that contain different number of fields
    LOOP AT ct_dd03p ASSIGNING <ls_dd03p>.
      CLEAR: <ls_dd03p>-position, <ls_dd03p>-tabname, <ls_dd03p>-ddlanguage.
    ENDLOOP.

  ENDMETHOD.
  METHOD clear_dd03p_fields_common.

    CLEAR: cs_dd03p-ddlanguage,
           cs_dd03p-dtelmaster,
           cs_dd03p-logflag,
           cs_dd03p-ddtext,
           cs_dd03p-reservedte,
           cs_dd03p-reptext,
           cs_dd03p-scrtext_s,
           cs_dd03p-scrtext_m,
           cs_dd03p-scrtext_l.

  ENDMETHOD.
  METHOD clear_dd03p_fields_dataelement.

* type specified via data element
    CLEAR: cs_dd03p-domname,
           cs_dd03p-inttype,
           cs_dd03p-intlen,
           cs_dd03p-mask,
           cs_dd03p-memoryid,
           cs_dd03p-headlen,
           cs_dd03p-scrlen1,
           cs_dd03p-scrlen2,
           cs_dd03p-scrlen3,
           cs_dd03p-datatype,
           cs_dd03p-leng,
           cs_dd03p-outputlen,
           cs_dd03p-deffdname,
           cs_dd03p-convexit,
           cs_dd03p-entitytab,
           cs_dd03p-dommaster,
           cs_dd03p-domname3l,
           cs_dd03p-decimals,
           cs_dd03p-lowercase,
           cs_dd03p-signflag.

  ENDMETHOD.
  METHOD delete_extras.

    DELETE FROM tddat WHERE tabname = iv_tabname.

  ENDMETHOD.
  METHOD delete_idoc_segment.

    DATA lv_segment_type        TYPE edilsegtyp.
    DATA lv_result              LIKE sy-subrc.

    IF is_idoc_segment( ) = abap_false.
      rv_deleted = abap_false.
      RETURN. "previous XML version or no IDoc segment
    ENDIF.

    rv_deleted = abap_true.
    lv_segment_type = ms_item-obj_name.

    CALL FUNCTION 'SEGMENT_DELETE'
      EXPORTING
        segmenttyp = lv_segment_type
      IMPORTING
        result     = lv_result
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0 OR lv_result <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.
  ENDMETHOD.
  METHOD deserialize_idoc_segment.

    DATA lv_result              LIKE sy-subrc.
    DATA lt_segment_definitions TYPE ty_segment_definitions.
    DATA lv_package             TYPE devclass.
    DATA lv_uname               TYPE sy-uname.
    DATA lv_transport           TYPE trkorr.
    DATA ls_edisdef             TYPE edisdef.
    DATA ls_segment_definition  TYPE ty_segment_definition.
    FIELD-SYMBOLS <ls_segment_definition> TYPE ty_segment_definition.

    rv_deserialized = abap_false.

    TRY.

        io_xml->read( EXPORTING iv_name = c_s_dataname-segment_definition
                      CHANGING  cg_data = lt_segment_definitions ).

      CATCH Lcx_abapgit_exception.
        RETURN. "previous XML version or no IDoc segment
    ENDTRY.

    IF lines( lt_segment_definitions ) = 0.
      RETURN. "no IDoc segment
    ENDIF.

    rv_deserialized = abap_true.

    lv_package = iv_package.
    lv_transport = iv_transport.

    LOOP AT lt_segment_definitions ASSIGNING <ls_segment_definition>.
      ls_segment_definition = <ls_segment_definition>.
      <ls_segment_definition>-segmentheader-presp = sy-uname.
      <ls_segment_definition>-segmentheader-pwork = sy-uname.

      CALL FUNCTION 'SEGMENT_READ'
        EXPORTING
          segmenttyp = <ls_segment_definition>-segmentdefinition-segtyp
        IMPORTING
          result     = lv_result
        EXCEPTIONS
          OTHERS     = 1.
      IF sy-subrc <> 0 OR lv_result <> 0.
        CALL FUNCTION 'SEGMENT_CREATE'
          IMPORTING
            segmentdefinition = <ls_segment_definition>-segmentdefinition
          TABLES
            segmentstructure  = <ls_segment_definition>-segmentstructures
          CHANGING
            segmentheader     = <ls_segment_definition>-segmentheader
            devclass          = lv_package
          EXCEPTIONS
            OTHERS            = 1.
      ELSE.

        CALL FUNCTION 'SEGMENT_MODIFY'
          CHANGING
            segmentheader = <ls_segment_definition>-segmentheader
            devclass      = lv_package
          EXCEPTIONS
            OTHERS        = 1.
        IF sy-subrc = 0.
          CALL FUNCTION 'SEGMENTDEFINITION_MODIFY'
            TABLES
              segmentstructure  = <ls_segment_definition>-segmentstructures
            CHANGING
              segmentdefinition = <ls_segment_definition>-segmentdefinition
            EXCEPTIONS
              OTHERS            = 1.
        ENDIF.
      ENDIF.

      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      IF ls_segment_definition-segmentdefinition-closed = abap_true.
        IF lv_transport IS NOT INITIAL.
          CALL FUNCTION 'SEGMENTDEFINITION_CLOSE'
            EXPORTING
              segmenttyp = ls_segment_definition-segmentdefinition-segtyp
            CHANGING
              order      = lv_transport
            EXCEPTIONS
              OTHERS     = 1.
          IF sy-subrc <> 0.
            Lcx_abapgit_exception=>raise_t100( ).
          ENDIF.
        ENDIF.

        " SEGMENTDEFINITION_CLOSE saves current release but it should be same as in repo
        SELECT SINGLE * FROM edisdef INTO ls_edisdef
          WHERE segtyp  = ls_segment_definition-segmentdefinition-segtyp
            AND version = ls_segment_definition-segmentdefinition-version.
        ls_edisdef-released = ls_segment_definition-segmentdefinition-released.
        ls_edisdef-applrel  = ls_segment_definition-segmentdefinition-applrel.
        ls_edisdef-closed   = ls_segment_definition-segmentdefinition-closed.
        UPDATE edisdef FROM ls_edisdef.
        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise( |Error updating IDOC segment {
            <ls_segment_definition>-segmentdefinition-segtyp }| ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    lv_uname = sy-uname.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_test_modus       = abap_false
        wi_tadir_pgmid      = 'R3TR'
        wi_tadir_object     = ms_item-obj_type
        wi_tadir_obj_name   = ms_item-obj_name
        wi_tadir_author     = lv_uname
        wi_tadir_devclass   = iv_package
        wi_tadir_masterlang = mv_language
        iv_set_edtflag      = abap_true
        iv_delflag          = abap_false
      EXCEPTIONS
        OTHERS              = 1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.
  ENDMETHOD.
  METHOD deserialize_indexes.

    DATA:
      lv_name      TYPE ddobjname,
      lv_subrc     TYPE sy-subrc,
      lt_dd12v     TYPE dd12vtab,
      lt_dd12v_db  TYPE dd12vtab,
      ls_dd12v     LIKE LINE OF lt_dd12v,
      lt_dd17v     TYPE dd17vtab,
      ls_dd17v     LIKE LINE OF lt_dd17v,
      lt_secondary LIKE lt_dd17v.

    io_xml->read( EXPORTING iv_name = 'DD12V'
                  CHANGING cg_data = lt_dd12v ).
    io_xml->read( EXPORTING iv_name = 'DD17V'
                  CHANGING cg_data = lt_dd17v ).

    lv_name = ms_item-obj_name.

    " Get existing indexes and drop the ones that are not included in remote
    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = lv_name
        langu         = mv_language
      TABLES
        dd12v_tab     = lt_dd12v_db
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from DDIF_TABL_GET' ).
    ENDIF.

    LOOP AT lt_dd12v_db INTO ls_dd12v.
      READ TABLE lt_dd12v TRANSPORTING NO FIELDS WITH KEY
        sqltab    = ls_dd12v-sqltab
        indexname = ls_dd12v-indexname.
      IF sy-subrc <> 0.
        CALL FUNCTION 'DD_INDX_DEL'
          EXPORTING
            sqltab    = ls_dd12v-sqltab
            indexname = ls_dd12v-indexname
            del_state = 'M'     "all states
          IMPORTING
            rc        = lv_subrc.
        IF lv_subrc <> 0.
          Lcx_abapgit_exception=>raise( |Error deleting index { ls_dd12v-sqltab }~{ ls_dd12v-indexname }| ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    " Create new or update existing indexes
    LOOP AT lt_dd12v INTO ls_dd12v.

      CLEAR lt_secondary.
      LOOP AT lt_dd17v INTO ls_dd17v
          WHERE sqltab = ls_dd12v-sqltab AND indexname = ls_dd12v-indexname.
        APPEND ls_dd17v TO lt_secondary.
      ENDLOOP.

      CALL FUNCTION 'DDIF_INDX_PUT'
        EXPORTING
          name              = ls_dd12v-sqltab
          id                = ls_dd12v-indexname
          dd12v_wa          = ls_dd12v
        TABLES
          dd17v_tab         = lt_secondary
        EXCEPTIONS
          indx_not_found    = 1
          name_inconsistent = 2
          indx_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      " Secondary indexes are automatically activated as part of R3TR TABL
      " So there's no need to add them to activation queue
    ENDLOOP.

  ENDMETHOD.
  METHOD deserialize_texts.

    DATA: lv_name       TYPE ddobjname,
          ls_dd02v_tmp  TYPE dd02v,
          lt_i18n_langs TYPE TABLE OF langu,
          lt_dd02_texts TYPE ty_dd02_texts.

    FIELD-SYMBOLS: <lv_lang>      LIKE LINE OF lt_i18n_langs,
                   <ls_dd02_text> LIKE LINE OF lt_dd02_texts.

    lv_name = ms_item-obj_name.

    ii_xml->read( EXPORTING iv_name = 'I18N_LANGS'
                  CHANGING  cg_data = lt_i18n_langs ).

    ii_xml->read( EXPORTING iv_name = 'DD02_TEXTS'
                  CHANGING  cg_data = lt_dd02_texts ).

    mo_i18n_params->trim_saplang_list( CHANGING ct_sap_langs = lt_i18n_langs ).

    SORT lt_i18n_langs.
    SORT lt_dd02_texts BY ddlanguage. " Optimization

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.

      " Table description
      ls_dd02v_tmp = is_dd02v.
      READ TABLE lt_dd02_texts ASSIGNING <ls_dd02_text> WITH KEY ddlanguage = <lv_lang>.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |DD02_TEXTS cannot find lang { <lv_lang> } in XML| ).
      ENDIF.
      MOVE-CORRESPONDING <ls_dd02_text> TO ls_dd02v_tmp.
      CALL FUNCTION 'DDIF_TABL_PUT'
        EXPORTING
          name              = lv_name
          dd02v_wa          = ls_dd02v_tmp
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
    ENDLOOP.

  ENDMETHOD.
  METHOD is_db_table_category.

    " values from domain TABCLASS
    rv_is_db_table_type = boolc( iv_tabclass = 'TRANSP'
                              OR iv_tabclass = 'CLUSTER'
                              OR iv_tabclass = 'POOL' ).

  ENDMETHOD.
  METHOD is_idoc_segment.

    DATA lv_segment_type TYPE edilsegtyp.

    lv_segment_type = ms_item-obj_name.

    SELECT SINGLE segtyp
           FROM edisegment
           INTO lv_segment_type
           WHERE segtyp = lv_segment_type.
    rv_is_idoc_segment = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD read_extras.

    SELECT SINGLE * FROM tddat INTO rs_tabl_extras-tddat WHERE tabname = iv_tabname.

  ENDMETHOD.
  METHOD serialize_idoc_segment.

    DATA lv_segment_type        TYPE edilsegtyp.
    DATA lv_result              LIKE sy-subrc.
    DATA lv_devclass            TYPE devclass.
    DATA lt_segmentdefinitions  TYPE STANDARD TABLE OF edisegmdef.
    DATA ls_segment_definition  TYPE ty_segment_definition.
    DATA lt_segment_definitions TYPE ty_segment_definitions.
    FIELD-SYMBOLS: <ls_segemtndefinition> TYPE edisegmdef.

    IF is_idoc_segment( ) = abap_false.
      RETURN.
    ENDIF.

    lv_segment_type = ms_item-obj_name.
    CALL FUNCTION 'SEGMENT_READ'
      EXPORTING
        segmenttyp        = lv_segment_type
      IMPORTING
        result            = lv_result
      TABLES
        segmentdefinition = lt_segmentdefinitions
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc <> 0 OR lv_result <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    LOOP AT lt_segmentdefinitions ASSIGNING <ls_segemtndefinition>.
      CLEAR ls_segment_definition.
      CALL FUNCTION 'SEGMENTDEFINITION_READ'
        EXPORTING
          segmenttyp           = <ls_segemtndefinition>-segtyp
        IMPORTING
          result               = lv_result
          devclass             = lv_devclass
          segmentheader        = ls_segment_definition-segmentheader
          segmentdefinition    = ls_segment_definition-segmentdefinition
        TABLES
          segmentstructure     = ls_segment_definition-segmentstructures
        CHANGING
          version              = <ls_segemtndefinition>-version
        EXCEPTIONS
          no_authority         = 1
          segment_not_existing = 2
          OTHERS               = 3.
      IF sy-subrc <> 0 OR lv_result <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      Lcl_abapgit_object_idoc=>clear_idoc_segement_fields(
                                 CHANGING cg_structure = ls_segment_definition-segmentdefinition ).
      Lcl_abapgit_object_idoc=>clear_idoc_segement_fields(
                                 CHANGING cg_structure = ls_segment_definition-segmentheader ).

      APPEND ls_segment_definition TO lt_segment_definitions.
    ENDLOOP.

    io_xml->add( iv_name = c_s_dataname-segment_definition
                 ig_data = lt_segment_definitions ).

  ENDMETHOD.
  METHOD serialize_texts.

    DATA: lv_name            TYPE ddobjname,
          lv_index           TYPE i,
          ls_dd02v           TYPE dd02v,
          lt_dd02_texts      TYPE ty_dd02_texts,
          lt_i18n_langs      TYPE TABLE OF langu,
          lt_language_filter TYPE Lif_abapgit_environment=>ty_system_language_filter.

    FIELD-SYMBOLS: <lv_lang>      LIKE LINE OF lt_i18n_langs,
                   <ls_dd02_text> LIKE LINE OF lt_dd02_texts.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      RETURN.
    ENDIF.

    lv_name = ms_item-obj_name.

    " Collect additional languages, skip main lang - it was serialized already
    lt_language_filter = mo_i18n_params->build_language_filter( ).

    SELECT DISTINCT ddlanguage AS langu INTO TABLE lt_i18n_langs
      FROM dd02v
      WHERE tabname = lv_name
      AND ddlanguage IN lt_language_filter
      AND ddlanguage <> mv_language
      ORDER BY langu.                                     "#EC CI_SUBRC

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.
      lv_index = sy-tabix.
      CALL FUNCTION 'DDIF_TABL_GET'
        EXPORTING
          name          = lv_name
          langu         = <lv_lang>
        IMPORTING
          dd02v_wa      = ls_dd02v
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.
      IF sy-subrc <> 0 OR ls_dd02v-ddlanguage IS INITIAL.
        DELETE lt_i18n_langs INDEX lv_index. " Don't save this lang
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO lt_dd02_texts ASSIGNING <ls_dd02_text>.
      MOVE-CORRESPONDING ls_dd02v TO <ls_dd02_text>.

    ENDLOOP.

    SORT lt_i18n_langs ASCENDING.
    SORT lt_dd02_texts BY ddlanguage ASCENDING.

    IF lines( lt_i18n_langs ) > 0.
      ii_xml->add( iv_name = 'I18N_LANGS'
                   ig_data = lt_i18n_langs ).

      ii_xml->add( iv_name = 'DD02_TEXTS'
                   ig_data = lt_dd02_texts ).
    ENDIF.

  ENDMETHOD.
  METHOD update_extras.

    IF is_tabl_extras-tddat IS INITIAL.
      delete_extras( iv_tabname ).
    ELSE.
      MODIFY tddat FROM is_tabl_extras-tddat.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    TYPES: BEGIN OF ty_data,
             as4user TYPE dd02l-as4user,
             as4date TYPE dd02l-as4date,
             as4time TYPE dd02l-as4time,
           END OF ty_data.

    DATA: lt_data TYPE STANDARD TABLE OF ty_data WITH DEFAULT KEY,
          ls_data LIKE LINE OF lt_data.


    SELECT as4user as4date as4time
      FROM dd02l INTO TABLE lt_data
      WHERE tabname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'
      ORDER BY PRIMARY KEY.

    SELECT as4user as4date as4time
      APPENDING TABLE lt_data
      FROM dd09l
      WHERE tabname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'
      ORDER BY PRIMARY KEY.

    SELECT as4user as4date as4time
      APPENDING TABLE lt_data
      FROM dd12l
      WHERE sqltab = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'
      ORDER BY PRIMARY KEY.

    SORT lt_data BY as4date DESCENDING as4time DESCENDING.

    READ TABLE lt_data INDEX 1 INTO ls_data.
    IF sy-subrc = 0.
      rv_user = ls_data-as4user.
    ELSE.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_objname TYPE rsedd0-ddobjname,
          lv_no_ask  TYPE abap_bool,
          lv_subrc   TYPE sy-subrc,
          BEGIN OF ls_dd02l,
            tabname  TYPE dd02l-tabname,
            tabclass TYPE dd02l-tabclass,
            sqltab   TYPE dd02l-sqltab,
          END OF ls_dd02l.

    IF Lif_abapgit_object~exists( ) = abap_false.
      " Proxies e.g. delete on its own, nothing todo here then.
      RETURN.
    ENDIF.

    lv_objname = ms_item-obj_name.

    IF delete_idoc_segment( ) = abap_false.

      lv_no_ask = abap_true.
      SELECT SINGLE tabname tabclass sqltab FROM dd02l
        INTO CORRESPONDING FIELDS OF ls_dd02l
        WHERE tabname = ms_item-obj_name
        AND as4local = 'A'
        AND as4vers = '0000'.
      IF sy-subrc = 0 AND is_db_table_category( ls_dd02l-tabclass ) = abap_true.

        CALL FUNCTION 'DD_EXISTS_DATA'
          EXPORTING
            reftab          = ls_dd02l-sqltab
            tabclass        = ls_dd02l-tabclass
            tabname         = ls_dd02l-tabname
          IMPORTING
            subrc           = lv_subrc
          EXCEPTIONS
            missing_reftab  = 1
            sql_error       = 2
            buffer_overflow = 3
            unknown_error   = 4
            OTHERS          = 5.

        IF sy-subrc = 0 AND lv_subrc = 0.
          lv_no_ask = abap_false.
        ENDIF.

      ENDIF.

      delete_ddic( iv_objtype = 'T'
                   iv_no_ask  = lv_no_ask ).

      delete_longtexts( c_longtext_id_tabl ).

      delete_extras( lv_objname ).

    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_name   TYPE ddobjname,
          ls_dd02v  TYPE dd02v,
          ls_dd09l  TYPE dd09l,
          lt_dd03p  TYPE TABLE OF dd03p,
          lt_dd05m  TYPE TABLE OF dd05m,
          lt_dd08v  TYPE TABLE OF dd08v,
          lt_dd35v  TYPE TABLE OF dd35v,
          lt_dd36m  TYPE dd36mttyp,
          ls_extras TYPE ty_tabl_extras.

    FIELD-SYMBOLS: <ls_dd03p>      TYPE dd03p,
                   <ls_dd05m>      TYPE dd05m,
                   <ls_dd08v>      TYPE dd08v,
                   <ls_dd35v>      TYPE dd35v,
                   <ls_dd36m>      TYPE dd36m,
                   <lg_roworcolst> TYPE any.

    lv_name = ms_item-obj_name. " type conversion

    IF deserialize_idoc_segment( io_xml     = io_xml
                                 iv_transport = iv_transport
                                 iv_package = iv_package ) = abap_false.

      io_xml->read( EXPORTING iv_name = 'DD02V'
                    CHANGING cg_data = ls_dd02v ).
      io_xml->read( EXPORTING iv_name = 'DD09L'
                    CHANGING cg_data = ls_dd09l ).
      io_xml->read( EXPORTING iv_name  = 'DD03P_TABLE'
                    CHANGING cg_data = lt_dd03p ).
      ASSIGN COMPONENT 'ROWORCOLST' OF STRUCTURE ls_dd09l TO <lg_roworcolst>.
      IF sy-subrc = 0 AND <lg_roworcolst> IS INITIAL.
        <lg_roworcolst> = 'C'. "Reverse fix from serialize
      ENDIF.

      " Number fields sequentially and fill table name
      LOOP AT lt_dd03p ASSIGNING <ls_dd03p>.
        <ls_dd03p>-position   = sy-tabix.
        <ls_dd03p>-tabname    = lv_name.
        <ls_dd03p>-ddlanguage = mv_language.
      ENDLOOP.

      io_xml->read( EXPORTING iv_name = 'DD05M_TABLE'
                    CHANGING cg_data = lt_dd05m ).
      io_xml->read( EXPORTING iv_name = 'DD08V_TABLE'
                    CHANGING cg_data = lt_dd08v ).
      io_xml->read( EXPORTING iv_name = 'DD35V_TALE'
                    CHANGING cg_data = lt_dd35v ).
      io_xml->read( EXPORTING iv_name = 'DD36M'
                    CHANGING cg_data = lt_dd36m ).

      LOOP AT lt_dd05m ASSIGNING <ls_dd05m>.
        <ls_dd05m>-tabname = lv_name.
      ENDLOOP.
      LOOP AT lt_dd08v ASSIGNING <ls_dd08v>.
        <ls_dd08v>-tabname = lv_name.
        <ls_dd08v>-ddlanguage = mv_language.
      ENDLOOP.
      LOOP AT lt_dd35v ASSIGNING <ls_dd35v>.
        <ls_dd35v>-tabname = lv_name.
      ENDLOOP.
      LOOP AT lt_dd36m ASSIGNING <ls_dd36m>.
        <ls_dd36m>-tabname = lv_name.
      ENDLOOP.

      corr_insert( iv_package = iv_package
                   ig_object_class = 'DICT' ).

      CALL FUNCTION 'DD_TABL_EXPAND'
        EXPORTING
          dd02v_wa          = ls_dd02v
        TABLES
          dd03p_tab         = lt_dd03p
          dd05m_tab         = lt_dd05m
          dd08v_tab         = lt_dd08v
          dd35v_tab         = lt_dd35v
          dd36m_tab         = lt_dd36m
        EXCEPTIONS
          illegal_parameter = 1
          OTHERS            = 2.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      CALL FUNCTION 'DDIF_TABL_PUT'
        EXPORTING
          name              = lv_name
          dd02v_wa          = ls_dd02v
          dd09l_wa          = ls_dd09l
        TABLES
          dd03p_tab         = lt_dd03p
          dd05m_tab         = lt_dd05m
          dd08v_tab         = lt_dd08v
          dd35v_tab         = lt_dd35v
          dd36m_tab         = lt_dd36m
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

      Lcl_abapgit_objects_activation=>add_item( ms_item ).

      deserialize_indexes( io_xml ).

      IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
        deserialize_texts(
          ii_xml   = io_xml
          is_dd02v = ls_dd02v ).
      ENDIF.

      deserialize_longtexts( ii_xml         = io_xml
                             iv_longtext_id = c_longtext_id_tabl ).

      io_xml->read( EXPORTING iv_name = c_s_dataname-tabl_extras
                    CHANGING cg_data = ls_extras ).
      update_extras( iv_tabname     = lv_name
                     is_tabl_extras = ls_extras ).

    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_tabname TYPE dd02l-tabname.

    lv_tabname = ms_item-obj_name.

    " Check nametab because it's fast
    CALL FUNCTION 'DD_GET_NAMETAB_HEADER'
      EXPORTING
        tabname   = lv_tabname
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      " Check for new, inactive, or modified versions that might not be in nametab
      SELECT SINGLE tabname FROM dd02l INTO lv_tabname
        WHERE tabname = lv_tabname.                     "#EC CI_NOORDER
    ENDIF.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.

    DATA: li_local_version_output TYPE REF TO Lif_abapgit_xml_output,
          li_local_version_input  TYPE REF TO Lif_abapgit_xml_input.


    CREATE OBJECT li_local_version_output TYPE Lcl_abapgit_xml_output.

    Lif_abapgit_object~serialize( li_local_version_output ).

    CREATE OBJECT li_local_version_input
      TYPE Lcl_abapgit_xml_input
      EXPORTING
        iv_xml = li_local_version_output->render( ).

    CREATE OBJECT ri_comparator TYPE Lcl_abapgit_object_tabl_compar
      EXPORTING
        ii_local = li_local_version_input.

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

    DATA: lv_name   TYPE ddobjname,
          lv_state  TYPE ddgotstate,
          ls_dd02v  TYPE dd02v,
          ls_dd09l  TYPE dd09l,
          lt_dd03p  TYPE ty_dd03p_tt,
          lt_dd05m  TYPE TABLE OF dd05m,
          lt_dd08v  TYPE TABLE OF dd08v,
          lt_dd12v  TYPE dd12vtab,
          lt_dd17v  TYPE dd17vtab,
          lt_dd35v  TYPE TABLE OF dd35v,
          lv_index  LIKE sy-index,
          lt_dd36m  TYPE dd36mttyp,
          ls_extras TYPE ty_tabl_extras.

    FIELD-SYMBOLS: <ls_dd12v>      LIKE LINE OF lt_dd12v,
                   <ls_dd05m>      LIKE LINE OF lt_dd05m,
                   <ls_dd08v>      LIKE LINE OF lt_dd08v,
                   <ls_dd35v>      LIKE LINE OF lt_dd35v,
                   <ls_dd36m>      LIKE LINE OF lt_dd36m,
                   <lg_roworcolst> TYPE any.

    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = lv_name
        langu         = mv_language
      IMPORTING
        gotstate      = lv_state
        dd02v_wa      = ls_dd02v
        dd09l_wa      = ls_dd09l
      TABLES
        dd03p_tab     = lt_dd03p
        dd05m_tab     = lt_dd05m
        dd08v_tab     = lt_dd08v
        dd12v_tab     = lt_dd12v
        dd17v_tab     = lt_dd17v
        dd35v_tab     = lt_dd35v
        dd36m_tab     = lt_dd36m
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from DDIF_TABL_GET' ).
    ENDIF.

    " Check if any active version was returned
    IF lv_state <> 'A'.
      RETURN.
    ENDIF.

    CLEAR: ls_dd02v-as4user,
           ls_dd02v-as4date,
           ls_dd02v-as4time.

* reset numeric field, so XML does not crash
    IF ls_dd02v-prozpuff = ''.
      CLEAR ls_dd02v-prozpuff.
    ENDIF.
    IF ls_dd02v-datmin = ''.
      CLEAR ls_dd02v-datmin.
    ENDIF.
    IF ls_dd02v-datmax = ''.
      CLEAR ls_dd02v-datmax.
    ENDIF.
    IF ls_dd02v-datavg = ''.
      CLEAR ls_dd02v-datavg.
    ENDIF.

    CLEAR: ls_dd09l-as4user,
           ls_dd09l-as4date,
           ls_dd09l-as4time.

    ASSIGN COMPONENT 'ROWORCOLST' OF STRUCTURE ls_dd09l TO <lg_roworcolst>.
    IF sy-subrc = 0 AND <lg_roworcolst> = 'C'.
      CLEAR <lg_roworcolst>. "To avoid diff errors. This field doesn't exists in all releases
    ENDIF.


    LOOP AT lt_dd12v ASSIGNING <ls_dd12v>.
      CLEAR: <ls_dd12v>-as4user,
             <ls_dd12v>-as4date,
             <ls_dd12v>-as4time,
             <ls_dd12v>-dbindex.
    ENDLOOP.

    clear_dd03p_fields( CHANGING ct_dd03p = lt_dd03p ).

* remove foreign keys inherited from .INCLUDEs
    DELETE lt_dd08v WHERE noinherit = 'N'.
    LOOP AT lt_dd05m ASSIGNING <ls_dd05m>.
      CLEAR <ls_dd05m>-tabname.
      CLEAR <ls_dd05m>-leng.
      lv_index = sy-tabix.
      READ TABLE lt_dd08v WITH KEY fieldname = <ls_dd05m>-fieldname TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        DELETE lt_dd05m INDEX lv_index.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_dd08v ASSIGNING <ls_dd08v>.
      CLEAR: <ls_dd08v>-tabname, <ls_dd08v>-ddlanguage.
    ENDLOOP.
    LOOP AT lt_dd35v ASSIGNING <ls_dd35v>.
      CLEAR <ls_dd35v>-tabname.
    ENDLOOP.

* remove inherited search helps
    DELETE lt_dd35v WHERE shlpinher = abap_true.
    LOOP AT lt_dd36m ASSIGNING <ls_dd36m>.
      CLEAR <ls_dd36m>-tabname.
      lv_index = sy-tabix.
      READ TABLE lt_dd35v WITH KEY fieldname = <ls_dd36m>-fieldname TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        DELETE lt_dd36m INDEX lv_index.
      ENDIF.
    ENDLOOP.

    io_xml->add( iv_name = 'DD02V'
                 ig_data = ls_dd02v ).
    IF NOT ls_dd09l IS INITIAL.
      io_xml->add( iv_name = 'DD09L'
                   ig_data = ls_dd09l ).
    ENDIF.
    io_xml->add( iv_name = 'DD03P_TABLE'
                 ig_data = lt_dd03p ).
    io_xml->add( iv_name = 'DD05M_TABLE'
                 ig_data = lt_dd05m ).
    io_xml->add( iv_name = 'DD08V_TABLE'
                 ig_data = lt_dd08v ).
    io_xml->add( iv_name = 'DD12V'
                 ig_data = lt_dd12v ).
    io_xml->add( iv_name = 'DD17V'
                 ig_data = lt_dd17v ).
    io_xml->add( iv_name = 'DD35V_TALE'
                 ig_data = lt_dd35v ).
    io_xml->add( iv_name = 'DD36M'
                 ig_data = lt_dd36m ).

    IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
      serialize_texts( io_xml ).
    ENDIF.

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_tabl ).

    serialize_idoc_segment( io_xml ).

    ls_extras = read_extras( lv_name ).
    io_xml->add( iv_name = c_s_dataname-tabl_extras
                 ig_data = ls_extras ).

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
endclass. "ZCL_ABAPGIT_OBJECT_TABL implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_TRAN <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_tran=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_tran=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_object_tran=======ccau.

*CLASS zcl_abapgit_object_tran DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BMGR4A.


class LCL_ABAPGIT_OBJECT_TRAN implementation.
*"* method's implementations
*include methods.
  METHOD add_data.

    DATA: ls_bcdata LIKE LINE OF mt_bcdata.

    ls_bcdata-fnam = iv_fnam.
    ls_bcdata-fval = iv_fval.
    APPEND ls_bcdata TO mt_bcdata.

  ENDMETHOD.
  METHOD call_se93.

    DATA: lt_message TYPE STANDARD TABLE OF bdcmsgcoll.
    DATA lv_msg TYPE string.

    FIELD-SYMBOLS: <ls_message> TYPE bdcmsgcoll.


    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      EXPORTING
        tcode     = 'SE93'
        mode_val  = 'N'
      TABLES
        using_tab = mt_bcdata
        mess_tab  = lt_message
      EXCEPTIONS
        OTHERS    = 1.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error deserializing { ms_item-obj_type } { ms_item-obj_name }| ).
    ENDIF.

    LOOP AT lt_message ASSIGNING <ls_message> WHERE msgtyp CA 'EAX'.
      MESSAGE ID <ls_message>-msgid
        TYPE <ls_message>-msgtyp
        NUMBER <ls_message>-msgnr
        WITH <ls_message>-msgv1 <ls_message>-msgv2 <ls_message>-msgv3 <ls_message>-msgv4
        INTO lv_msg.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDLOOP.

  ENDMETHOD.
  METHOD clear_functiongroup_globals.
    TYPES ty_param_vari TYPE abap_bool.

    DATA lt_error_list TYPE STANDARD TABLE OF rsmp_check WITH DEFAULT KEY.
    FIELD-SYMBOLS <lv_param_vari> TYPE ty_param_vari.

    " only way to clear global fields in function group
    CALL FUNCTION 'RS_TRANSACTION_INCONSISTENCIES'
      EXPORTING
        transaction_code = 'ZTHISTCODENEVEREXIST'
      TABLES
        error_list       = lt_error_list
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      "Expected - fine

      " but there is no other way to clear this field
      ASSIGN ('(SAPLSEUK)PARAM_VARI') TO <lv_param_vari>.
      IF sy-subrc = 0.
        CLEAR <lv_param_vari>.
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD deserialize_oo_transaction.

    " You should remember that we don't use batch input just for fun,
    " but because FM RPY_TRANSACTION_INSERT doesn't support OO transactions.

    DATA: ls_bcdata  TYPE bdcdata.


    CLEAR mt_bcdata.

    ls_bcdata-program  = 'SAPLSEUK'.
    ls_bcdata-dynpro   = '0390'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO mt_bcdata.

    add_data( iv_fnam = 'TSTC-TCODE'
              iv_fval = is_tstc-tcode ).

    IF Lif_abapgit_object~exists( ) = abap_true.

      add_data( iv_fnam = 'BDC_OKCODE'
                iv_fval = '=CHNG' ).

    ELSE.

      add_data( iv_fnam = 'BDC_OKCODE'
                iv_fval = '=ADD' ).

    ENDIF.

    ls_bcdata-program  = 'SAPLSEUK'.
    ls_bcdata-dynpro   = '0300'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO mt_bcdata.

    add_data( iv_fnam     = 'TSTCT-TTEXT'
              iv_fval     = is_tstct-ttext ).

    add_data( iv_fnam     = 'RSSTCD-S_CLASS'
              iv_fval     = 'X' ).

    add_data( iv_fnam = 'BDC_OKCODE'
              iv_fval = '=ENTR' ).

    ls_bcdata-program  = 'SAPLSEUK'.
    ls_bcdata-dynpro   = '0360'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO mt_bcdata.

    add_data( iv_fnam     = 'RSSTCD-S_TRFRAME'
              iv_fval     = is_rsstcd-s_trframe ).

    add_data( iv_fnam     = 'RSSTCD-S_UPDTASK'
              iv_fval     = is_rsstcd-s_updtask ).

    add_data( iv_fnam = 'BDC_OKCODE'
              iv_fval = '=TR_FRAMEWORK' ).

    ls_bcdata-program  = 'SAPLSEUK'.
    ls_bcdata-dynpro   = '0360'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO mt_bcdata.

    add_data( iv_fnam     = 'RSSTCD-CLASSNAME'
              iv_fval     = is_rsstcd-classname ).

    add_data( iv_fnam     = 'RSSTCD-METHOD'
              iv_fval     = is_rsstcd-method ).

    IF is_rsstcd-s_local IS NOT INITIAL.
      add_data( iv_fnam     = 'RSSTCD-S_LOCAL'
                iv_fval     = is_rsstcd-s_local ).
    ENDIF.

    IF is_rsstcd-s_updlok IS NOT INITIAL.
      add_data( iv_fnam     = 'RSSTCD-S_UPDLOK'
                iv_fval     = is_rsstcd-s_updlok ).
    ENDIF.

    add_data( iv_fnam     = 'TSTC-PGMNA'
              iv_fval     = is_tstc-pgmna ).

    IF is_tstcc-s_webgui = '2'.

      add_data( iv_fnam     = 'G_IAC_EWT'
                iv_fval     = abap_true ).

      add_data( iv_fnam = 'BDC_OKCODE'
                iv_fval = 'MAKE_PROFI' ).

      ls_bcdata-program  = 'SAPLSEUK'.
      ls_bcdata-dynpro   = '0360'.
      ls_bcdata-dynbegin = 'X'.
      APPEND ls_bcdata TO mt_bcdata.

    ELSEIF is_tstcc-s_webgui IS NOT INITIAL.

      add_data( iv_fnam     = 'TSTCC-S_WEBGUI'
                iv_fval     = is_tstcc-s_webgui ).

    ENDIF.

    IF is_tstcc-s_pervas IS NOT INITIAL.
      add_data( iv_fnam     = 'TSTCC-S_PERVAS'
                iv_fval     = is_tstcc-s_pervas ).
    ENDIF.

    IF is_tstcc-s_service IS NOT INITIAL.
      add_data( iv_fnam     = 'TSTCC-S_SERVICE'
                iv_fval     = is_tstcc-s_service ).
    ENDIF.

    IF is_tstcc-s_platin IS NOT INITIAL.
      add_data( iv_fnam     = 'TSTCC-S_PLATIN'
                iv_fval     = is_tstcc-s_platin ).
    ENDIF.

    IF is_tstcc-s_win32 IS NOT INITIAL.
      add_data( iv_fnam     = 'TSTCC-S_WIN32'
                iv_fval     = is_tstcc-s_win32 ).
    ENDIF.

    add_data( iv_fnam = 'BDC_OKCODE'
              iv_fval = '=WB_SAVE' ).

    ls_bcdata-program  = 'SAPLSTRD'.
    ls_bcdata-dynpro   = '0100'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO mt_bcdata.

    add_data( iv_fnam     = 'KO007-L_DEVCLASS'
              iv_fval     = iv_package ).

    add_data( iv_fnam = 'BDC_OKCODE'
              iv_fval = '=ADD' ).

    ls_bcdata-program  = 'BDC_OKCODE'.
    ls_bcdata-dynpro   = '0360'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO mt_bcdata.

    add_data( iv_fnam = 'BDC_OKCODE'
              iv_fval = '=WB_BACK' ).

    ls_bcdata-program  = 'BDC_OKCODE'.
    ls_bcdata-dynpro   = '0360'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO mt_bcdata.

    add_data( iv_fnam = 'BDC_OKCODE'
              iv_fval = '=WB_BACK' ).

    call_se93( ).

  ENDMETHOD.
  METHOD deserialize_texts.

    DATA lt_tpool_i18n TYPE TABLE OF tstct.

    FIELD-SYMBOLS <ls_tpool> LIKE LINE OF lt_tpool_i18n.

    " Read XML-files data
    ii_xml->read( EXPORTING iv_name = 'I18N_TPOOL'
                  CHANGING  cg_data = lt_tpool_i18n ).

    mo_i18n_params->trim_saplang_keyed_table(
      EXPORTING
        iv_lang_field_name = 'SPRSL'
      CHANGING
        ct_tab = lt_tpool_i18n ).

    " Force t-code name (security reasons)
    LOOP AT lt_tpool_i18n ASSIGNING <ls_tpool>.
      <ls_tpool>-tcode = ms_item-obj_name.
    ENDLOOP.

    IF lines( lt_tpool_i18n ) > 0.
      MODIFY tstct FROM TABLE lt_tpool_i18n.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'Update of t-code translations failed' ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD is_variant_transaction.

    rv_variant_transaction = boolc( is_tstcp-param(1) = '@' ).

  ENDMETHOD.
  METHOD save_authorizations.

    CONSTANTS: lc_hex_chk TYPE x VALUE '04'.
    DATA: ls_transaction TYPE tstc.

    transaction_read( EXPORTING iv_transaction = iv_transaction
                      IMPORTING es_transaction = ls_transaction ).

    DELETE FROM tstca WHERE tcode = iv_transaction.

    IF ls_transaction IS NOT INITIAL.
      INSERT tstca FROM TABLE it_authorizations.
      ls_transaction-cinfo = ls_transaction-cinfo + lc_hex_chk.
      UPDATE tstc SET cinfo = ls_transaction-cinfo WHERE tcode = ls_transaction-tcode.
    ENDIF.

  ENDMETHOD.
  METHOD serialize_texts.

    DATA lt_tpool_i18n TYPE TABLE OF tstct.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      RETURN.
    ENDIF.

    " Skip main language - it was already serialized
    " Don't serialize t-code itself
    SELECT sprsl ttext
      INTO CORRESPONDING FIELDS OF TABLE lt_tpool_i18n
      FROM tstct
      WHERE sprsl <> mv_language
      AND   tcode = ms_item-obj_name
      ORDER BY sprsl ##TOO_MANY_ITAB_FIELDS.            "#EC CI_GENBUFF

    mo_i18n_params->trim_saplang_keyed_table(
      EXPORTING
        iv_lang_field_name = 'SPRSL'
      CHANGING
        ct_tab = lt_tpool_i18n ).

    IF lines( lt_tpool_i18n ) > 0.
      SORT lt_tpool_i18n BY sprsl ASCENDING.
      ii_xml->add( iv_name = 'I18N_TPOOL'
                   ig_data = lt_tpool_i18n ).
    ENDIF.

  ENDMETHOD.
  METHOD set_oo_parameters.

    DATA: ls_param LIKE LINE OF it_rsparam.

    IF cs_rsstcd-call_tcode = c_oo_tcode.
      cs_rsstcd-s_trframe = c_true.
      LOOP AT it_rsparam INTO ls_param.
        CASE ls_param-field.
          WHEN c_oo_frclass.
            cs_rsstcd-classname = ls_param-value.
          WHEN c_oo_frmethod.
            cs_rsstcd-method   = ls_param-value.
          WHEN c_oo_frupdtask.
            IF ls_param-value = c_oo_synchron.
              cs_rsstcd-s_upddir  = c_true.
              cs_rsstcd-s_updtask = c_false.
              cs_rsstcd-s_updlok  = c_false.
            ELSEIF ls_param-value = c_oo_asynchron.
              cs_rsstcd-s_upddir  = c_false.
              cs_rsstcd-s_updtask = c_true.
              cs_rsstcd-s_updlok  = c_false.
            ELSE.
              cs_rsstcd-s_upddir  = c_false.
              cs_rsstcd-s_updtask = c_false.
              cs_rsstcd-s_updlok  = c_true.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD shift_param.

    DATA: ls_param  LIKE LINE OF ct_rsparam,
          lv_fdpos  TYPE sy-fdpos,
          lv_length TYPE i.

    FIELD-SYMBOLS <lg_f> TYPE any.


    DO 254 TIMES.
      IF cs_tstcp-param = space.
        EXIT.
      ENDIF.
      CLEAR ls_param.
      IF cs_tstcp-param CA '='.
        CHECK sy-fdpos <> 0.
        ASSIGN cs_tstcp-param(sy-fdpos) TO <lg_f>.
        ls_param-field = <lg_f>.
        IF ls_param-field(1) = space.
          SHIFT ls_param-field.
        ENDIF.
        lv_fdpos = sy-fdpos + 1.
        SHIFT cs_tstcp-param BY lv_fdpos PLACES.
        IF cs_tstcp-param CA ';'.
          IF sy-fdpos <> 0.
            ASSIGN cs_tstcp-param(sy-fdpos) TO <lg_f>.
            ls_param-value = <lg_f>.
            IF ls_param-value(1) = space.
              SHIFT ls_param-value.
            ENDIF.
          ENDIF.
          lv_fdpos = sy-fdpos + 1.
          SHIFT cs_tstcp-param BY lv_fdpos PLACES.
          APPEND ls_param TO ct_rsparam.
        ELSE.
          lv_length = strlen( cs_tstcp-param ).
          CHECK lv_length > 0.
          ASSIGN cs_tstcp-param(lv_length) TO <lg_f>.
          ls_param-value = <lg_f>.
          IF ls_param-value(1) = space.
            SHIFT ls_param-value.
          ENDIF.
          lv_length = lv_length + 1.
          SHIFT cs_tstcp-param BY lv_length PLACES.
          APPEND ls_param TO ct_rsparam.
        ENDIF.
      ENDIF.
    ENDDO.

  ENDMETHOD.
  METHOD split_parameters.
* see subroutine split_parameters in include LSEUKF01

    DATA: lv_off       TYPE i,
          lv_fdpos     TYPE sy-fdpos,
          lv_param_beg TYPE i.


    CLEAR cs_rsstcd-s_vari.

    IF cs_tstcp-param(1) = '\'.             " OO-Transaktion ohne FR
      split_parameters_comp( EXPORTING ig_type = c_oo_program
                                       ig_param = cs_tstcp-param
                             CHANGING  cg_value = cs_tstc-pgmna ).
      split_parameters_comp( EXPORTING ig_type = c_oo_class
                                       ig_param = cs_tstcp-param
                             CHANGING  cg_value = cs_rsstcd-classname ).
      split_parameters_comp( EXPORTING ig_type = c_oo_method
                                       ig_param = cs_tstcp-param
                             CHANGING  cg_value = cs_rsstcd-method ).

      IF NOT cs_tstc-pgmna IS INITIAL.
        cs_rsstcd-s_local = c_true.
      ENDIF.
      RETURN.
    ELSEIF cs_tstcp-param(1) = '@'.         " Transaktionsvariante
      cs_rsstcd-s_vari = c_true.
      IF cs_tstcp-param(2) = '@@'.
        cs_rsstcd-s_ind_vari = c_true.
        lv_off = 2.
      ELSE.
        CLEAR cs_rsstcd-s_ind_vari.
        lv_off = 1.
      ENDIF.
      IF cs_tstcp-param CA ' '.
      ENDIF.
      lv_fdpos = sy-fdpos - lv_off.
      IF lv_fdpos > 0.
        cs_rsstcd-call_tcode = cs_tstcp-param+lv_off(sy-fdpos).
        lv_fdpos = lv_fdpos + 1 + lv_off.
        cs_rsstcd-variant = cs_tstcp-param+lv_fdpos.
      ENDIF.
    ELSEIF cs_tstcp-param(1) = '/'.
      cs_rsstcd-st_tcode = c_true.
      cs_rsstcd-st_prog  = space.
      IF cs_tstcp-param+1(1) = '*'.
        cs_rsstcd-st_skip_1 = c_true.
      ELSE.
        CLEAR cs_rsstcd-st_skip_1.
      ENDIF.
      IF cs_tstcp-param CA ' '.
      ENDIF.
      lv_param_beg = sy-fdpos + 1.
      lv_fdpos = sy-fdpos - 2.
      IF lv_fdpos > 0.
        cs_rsstcd-call_tcode = cs_tstcp-param+2(lv_fdpos).
      ENDIF.
      SHIFT cs_tstcp-param BY lv_param_beg PLACES.
    ELSE.
      cs_rsstcd-st_tcode = space.
      cs_rsstcd-st_prog  = c_true.
    ENDIF.

    shift_param(
      CHANGING ct_rsparam = ct_rsparam
               cs_tstcp   = cs_tstcp ).

    set_oo_parameters(
      EXPORTING it_rsparam = ct_rsparam
      CHANGING cs_rsstcd = cs_rsstcd ).

  ENDMETHOD.
  METHOD split_parameters_comp.
    DATA: lv_off TYPE i.

    IF ig_param CS ig_type.
      lv_off = sy-fdpos + strlen( ig_type ).
      cg_value = ig_param+lv_off.
      IF cg_value CA '\'.
        CLEAR cg_value+sy-fdpos.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD transaction_read.

    DATA: lt_tcodes   TYPE TABLE OF tstc,
          lt_gui_attr TYPE TABLE OF tstcc.

    CLEAR: es_transaction, es_gui_attr.

    CALL FUNCTION 'RPY_TRANSACTION_READ'
      EXPORTING
        transaction      = iv_transaction
      TABLES
        tcodes           = lt_tcodes
        gui_attributes   = lt_gui_attr
      EXCEPTIONS
        permission_error = 1
        cancelled        = 2
        not_found        = 3
        object_not_found = 4
        OTHERS           = 5.
    IF sy-subrc = 4 OR sy-subrc = 3.
      RETURN.
    ELSEIF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    READ TABLE lt_tcodes INDEX 1 INTO es_transaction.
    ASSERT sy-subrc = 0.
    READ TABLE lt_gui_attr INDEX 1 INTO es_gui_attr.
    ASSERT sy-subrc = 0.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    " Changed-by-user is not stored in transaction metadata
    " Instead, use owner of last transport or object directory

    DATA lv_transport TYPE trkorr.

    lv_transport = Lcl_abapgit_factory=>get_cts_api( )->get_transport_for_object( ms_item ).

    IF lv_transport IS NOT INITIAL.
      SELECT SINGLE as4user FROM e070 INTO rv_user WHERE trkorr = lv_transport.
    ELSE.
      SELECT SINGLE author FROM tadir INTO rv_user
        WHERE pgmid = 'R3TR' AND object = ms_item-obj_type AND obj_name = ms_item-obj_name.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_transaction TYPE tstc-tcode.


    lv_transaction = ms_item-obj_name.

    CALL FUNCTION 'RPY_TRANSACTION_DELETE'
      EXPORTING
        transaction      = lv_transaction
      EXCEPTIONS
        not_excecuted    = 1
        object_not_found = 0
        OTHERS           = 3.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    CONSTANTS: lc_hex_tra TYPE x VALUE '00',
*               lc_hex_men TYPE x VALUE '01',
               lc_hex_par TYPE x VALUE '02',
               lc_hex_rep TYPE x VALUE '80',
*               lc_hex_rpv TYPE x VALUE '10',
               lc_hex_obj TYPE x VALUE '08'.

    DATA: lv_dynpro       TYPE d020s-dnum,
          ls_tstc         TYPE tstc,
          lv_type         TYPE rglif-docutype,
          ls_tstct        TYPE tstct,
          ls_tstcc        TYPE tstcc,
          ls_tstcp        TYPE tstcp,
          lt_tstca        TYPE ty_tstca,
          lt_param_values TYPE ty_param_values,
          ls_rsstcd       TYPE rsstcd.


    IF Lif_abapgit_object~exists( ) = abap_true.
      Lif_abapgit_object~delete( iv_package   = iv_package
                                 iv_transport = iv_transport ).
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'TSTC'
                  CHANGING cg_data = ls_tstc ).
    io_xml->read( EXPORTING iv_name = 'TSTCC'
                  CHANGING cg_data = ls_tstcc ).
    io_xml->read( EXPORTING iv_name = 'TSTCT'
                  CHANGING cg_data = ls_tstct ).
    io_xml->read( EXPORTING iv_name = 'TSTCP'
                  CHANGING cg_data = ls_tstcp ).
    io_xml->read( EXPORTING iv_name = 'AUTHORIZATIONS'
                  CHANGING cg_data = lt_tstca ).

    lv_dynpro = ls_tstc-dypno.

    IF ls_tstc-cinfo O lc_hex_rep.
      lv_type = c_variant_type-report.
    ELSEIF ls_tstc-cinfo O lc_hex_obj.
      lv_type = c_variant_type-object.
    ELSEIF ls_tstc-cinfo O lc_hex_par.
      IF is_variant_transaction( ls_tstcp ) = abap_true.
        lv_type = c_variant_type-variant.
      ELSE.
        lv_type = c_variant_type-parameters.
      ENDIF.
    ELSEIF ls_tstc-cinfo O lc_hex_tra.
      lv_type = c_variant_type-dialog.
    ELSE.
      Lcx_abapgit_exception=>raise( 'Transaction, unknown CINFO' ).
    ENDIF.

    IF ls_tstcp IS NOT INITIAL.
      split_parameters( CHANGING ct_rsparam = lt_param_values
                                 cs_rsstcd  = ls_rsstcd
                                 cs_tstcp   = ls_tstcp
                                 cs_tstc    = ls_tstc ).
    ENDIF.

    CASE lv_type.
      WHEN c_variant_type-object.

        deserialize_oo_transaction( iv_package      = iv_package
                                    is_tstc         = ls_tstc
                                    is_tstcc        = ls_tstcc
                                    is_tstct        = ls_tstct
                                    is_rsstcd       = ls_rsstcd ).

      WHEN OTHERS.

        clear_functiongroup_globals( ).

        corr_insert( iv_package ).

        CALL FUNCTION 'RPY_TRANSACTION_INSERT'
          EXPORTING
            transaction             = ls_tstc-tcode
            program                 = ls_tstc-pgmna
            dynpro                  = lv_dynpro
            language                = mv_language
            development_class       = iv_package
            transaction_type        = lv_type
            shorttext               = ls_tstct-ttext
            called_transaction      = ls_rsstcd-call_tcode
            called_transaction_skip = ls_rsstcd-st_skip_1
            variant                 = ls_rsstcd-variant
            cl_independend          = ls_rsstcd-s_ind_vari
            html_enabled            = ls_tstcc-s_webgui
            java_enabled            = ls_tstcc-s_platin
            wingui_enabled          = ls_tstcc-s_win32
            suppress_corr_insert    = abap_true
          TABLES
            param_values            = lt_param_values
          EXCEPTIONS
            cancelled               = 1
            already_exist           = 2
            permission_error        = 3
            name_not_allowed        = 4
            name_conflict           = 5
            illegal_type            = 6
            object_inconsistent     = 7
            db_access_error         = 8
            OTHERS                  = 9.
        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise_t100( ).
        ENDIF.

    ENDCASE.

    IF lt_tstca IS NOT INITIAL.
      save_authorizations( iv_transaction    = ls_tstc-tcode
                           it_authorizations = lt_tstca ).
    ENDIF.

    IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
      deserialize_texts( io_xml ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_tcode TYPE tstc-tcode.


    SELECT SINGLE tcode FROM tstc INTO lv_tcode
      WHERE tcode = ms_item-obj_name.                   "#EC CI_GENBUFF
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
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'EEUDB'
                                            iv_argument    = ms_item-obj_name
                                            iv_prefix      = 'TN' ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.


    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPLSEUK'.
    <ls_bdcdata>-dynpro   = '0390'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=SHOW'.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'TSTC-TCODE'.
    <ls_bdcdata>-fval = ms_item-obj_name.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode      = 'SE93'
      it_bdcdata    = lt_bdcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_transaction TYPE tstc-tcode,
          ls_tcode       TYPE tstc,
          ls_tstct       TYPE tstct,
          ls_tstcp       TYPE tstcp,
          lt_tstca       TYPE ty_tstca,
          ls_gui_attr    TYPE tstcc.


    lv_transaction = ms_item-obj_name.

    transaction_read( EXPORTING iv_transaction = lv_transaction
                      IMPORTING es_transaction = ls_tcode
                                es_gui_attr    = ls_gui_attr ).
    IF ls_tcode IS INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM tstct INTO ls_tstct
      WHERE sprsl = mv_language
      AND tcode = lv_transaction.         "#EC CI_SUBRC "#EC CI_GENBUFF

    SELECT SINGLE * FROM tstcp INTO ls_tstcp
      WHERE tcode = lv_transaction.       "#EC CI_SUBRC "#EC CI_GENBUFF

    SELECT * FROM tstca INTO TABLE lt_tstca
      WHERE tcode = lv_transaction
      ORDER BY PRIMARY KEY.
    IF sy-subrc <> 0.
      CLEAR: lt_tstca.
    ENDIF.

    io_xml->add( iv_name = 'TSTC'
                 ig_data = ls_tcode ).
    io_xml->add( iv_name = 'TSTCC'
                 ig_data = ls_gui_attr ).
    io_xml->add( iv_name = 'TSTCT'
                 ig_data = ls_tstct ).
    IF ls_tstcp IS NOT INITIAL.
      io_xml->add( iv_name = 'TSTCP'
                   ig_data = ls_tstcp ).
    ENDIF.
    io_xml->add( iv_name = 'AUTHORIZATIONS'
                 ig_data = lt_tstca ).

    IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
      serialize_texts( io_xml ).
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
endclass. "ZCL_ABAPGIT_OBJECT_TRAN implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_TYPE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_type=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_type=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_TYPE implementation.
*"* method's implementations
*include methods.
  METHOD create.

    DATA: lv_progname  TYPE reposrc-progname,
          lv_typegroup TYPE rsedd0-typegroup.


    lv_typegroup = ms_item-obj_name.

    CALL FUNCTION 'RS_DD_TYGR_INSERT_SOURCES'
      EXPORTING
        typegroupname        = lv_typegroup
        ddtext               = iv_ddtext
        corrnum              = ''
        devclass             = iv_devclass
      TABLES
        source               = it_source
      EXCEPTIONS
        already_exists       = 1
        not_executed         = 2
        permission_failure   = 3
        object_not_specified = 4
        illegal_name         = 5
        OTHERS               = 6.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CONCATENATE c_prefix lv_typegroup INTO lv_progname.
    UPDATE progdir SET uccheck = Lif_abapgit_aff_types_v1=>co_abap_language_version_src-standard
      WHERE name = lv_progname.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error setting uccheck' ).
    ENDIF.

  ENDMETHOD.
  METHOD read.

    DATA: lv_typdname  TYPE rsedd0-typegroup,
          lt_psmodisrc TYPE TABLE OF smodisrc,
          lt_psmodilog TYPE TABLE OF smodilog,
          lt_ptrdir    TYPE TABLE OF trdir.


    SELECT SINGLE ddtext FROM ddtypet
      INTO ev_ddtext
      WHERE typegroup  = ms_item-obj_name
        AND ddlanguage = mv_language.

    lv_typdname = ms_item-obj_name.

    " Get active version, ignore errors if not found
    CALL FUNCTION 'TYPD_GET_OBJECT'
      EXPORTING
        typdname          = lv_typdname
      TABLES
        psmodisrc         = lt_psmodisrc
        psmodilog         = lt_psmodilog
        psource           = et_source
        ptrdir            = lt_ptrdir
      EXCEPTIONS
        version_not_found = 1
        reps_not_exist    = 2
        OTHERS            = 3 ##FM_SUBRC_OK.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    DATA lv_prog TYPE progname.

    CONCATENATE '%_C' ms_item-obj_name INTO lv_prog.

    SELECT SINGLE unam FROM reposrc INTO rv_user
      WHERE progname = lv_prog AND r3state = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    delete_ddic( 'G' ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_ddtext    TYPE ddtypet-ddtext,
          lt_source    TYPE abaptxt255_tab,
          lv_progname  TYPE reposrc-progname,
          lv_typegroup TYPE rsedd0-typegroup.


    lv_typegroup = ms_item-obj_name.


    io_xml->read( EXPORTING iv_name = 'DDTEXT'
                  CHANGING cg_data = lv_ddtext ).

    lt_source = Lif_abapgit_object~mo_files->read_abap( ).

    IF Lif_abapgit_object~exists( ) = abap_false.
      create( iv_ddtext   = lv_ddtext
              it_source   = lt_source
              iv_devclass = iv_package ).
    ELSE.
      CONCATENATE c_prefix lv_typegroup INTO lv_progname.

      Lcl_abapgit_factory=>get_sap_report( )->insert_report(
        iv_name    = lv_progname
        iv_package = iv_package
        iv_version = Lif_abapgit_aff_types_v1=>co_abap_language_version_src-standard
        it_source  = lt_source ).
    ENDIF.

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_progname TYPE progname,
          lv_state    TYPE r3state.

    lv_progname = |%_C{ ms_item-obj_name }|.
    SELECT SINGLE state
      FROM progdir
      INTO lv_state
      WHERE name = lv_progname.                         "#EC CI_NOORDER
    IF lv_state IS NOT INITIAL.
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
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECT=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_ddtext TYPE ddtypet-ddtext,
          lt_source TYPE abaptxt255_tab.


    read( IMPORTING ev_ddtext = lv_ddtext
                    et_source = lt_source ).

    IF lt_source IS INITIAL.
      RETURN.
    ENDIF.

    io_xml->add( iv_name = 'DDTEXT'
                 ig_data = lv_ddtext ).

    Lif_abapgit_object~mo_files->add_abap( lt_source ).

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
endclass. "ZCL_ABAPGIT_OBJECT_TYPE implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_UENO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ueno=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ueno=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_UENO implementation.
*"* method's implementations
*include methods.
  METHOD build_text_name.

    TYPES BEGIN OF ty_text_name.
    TYPES id TYPE c LENGTH 4.
    TYPES entity TYPE c LENGTH 26.
    TYPES modifier TYPE c LENGTH 2.
    TYPES END OF ty_text_name.

    DATA ls_text_name TYPE ty_text_name.

    ls_text_name-id = iv_id.
    ls_text_name-entity = mv_entity_id.
    ls_text_name-modifier = 'A%'.

    rv_result = ls_text_name.

  ENDMETHOD.
  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_entity_id = is_item-obj_name.

  ENDMETHOD.
  METHOD delete_docu_uen.

    DATA lt_dm02l TYPE STANDARD TABLE OF dm02l WITH DEFAULT KEY.
    DATA ls_dm02l TYPE dm02l.

    SELECT *
      FROM dm02l
      INTO TABLE lt_dm02l
      WHERE entid = mv_entity_id
      ORDER BY PRIMARY KEY.

    LOOP AT lt_dm02l INTO ls_dm02l.

      CALL FUNCTION 'SDU_DOCU_DELETE'
        EXPORTING
          key1     = ls_dm02l-entid
          key2     = ls_dm02l-as4local
          key3     = '00'
          langu    = mv_language
          obj_id   = 'UENC' "Entity Comments
        EXCEPTIONS
          ret_code = 0.

      CALL FUNCTION 'SDU_DOCU_DELETE'
        EXPORTING
          key1     = ls_dm02l-entid
          key2     = ls_dm02l-as4local
          key3     = '00'
          langu    = mv_language
          obj_id   = 'UEND' "Entity Definition
        EXCEPTIONS
          ret_code = 0.

      CALL FUNCTION 'SDU_DOCU_DELETE'
        EXPORTING
          key1     = ls_dm02l-entid
          key2     = ls_dm02l-as4local
          key3     = '00'
          langu    = mv_language
          obj_id   = 'UENE' "Entity Example
        EXCEPTIONS
          ret_code = 0.

    ENDLOOP.

  ENDMETHOD.
  METHOD delete_docu_url.

    DATA lt_dm42s TYPE STANDARD TABLE OF dm42s WITH DEFAULT KEY.
    DATA ls_dm42s LIKE LINE OF lt_dm42s.

    SELECT *
      FROM dm42s
      INTO TABLE lt_dm42s
      WHERE entidto = mv_entity_id
      ORDER BY PRIMARY KEY.

    LOOP AT lt_dm42s INTO ls_dm42s.

      CALL FUNCTION 'SDU_DOCU_DELETE'
        EXPORTING
          langu    = mv_language
          obj_id   = 'URL1'
          key1     = ls_dm42s-entidto
          key2     = ls_dm42s-as4local
          key3     = ls_dm42s-entidfrom
          key4     = ls_dm42s-ebrolnr
        EXCEPTIONS
          ret_code = 0.

      CALL FUNCTION 'SDU_DOCU_DELETE'
        EXPORTING
          langu    = mv_language
          obj_id   = 'URL2'
          key1     = ls_dm42s-entidto
          key2     = ls_dm42s-as4local
          key3     = ls_dm42s-entidfrom
          key4     = ls_dm42s-ebrolnr
        EXCEPTIONS
          ret_code = 0.

      CALL FUNCTION 'SDU_DOCU_DELETE'
        EXPORTING
          langu    = mv_language
          obj_id   = 'URLC'
          key1     = ls_dm42s-entidto
          key2     = ls_dm42s-as4local
          key3     = ls_dm42s-entidfrom
          key4     = ls_dm42s-ebrolnr
        EXCEPTIONS
          ret_code = 0.

    ENDLOOP.


  ENDMETHOD.
  METHOD delete_docu_usp.

    DATA lt_dm45l TYPE STANDARD TABLE OF dm45l WITH DEFAULT KEY.
    DATA ls_dm45l LIKE LINE OF lt_dm45l.

    SELECT *
      FROM dm45l
      INTO TABLE lt_dm45l
      WHERE entid = ms_item-obj_name
      ORDER BY PRIMARY KEY.

    LOOP AT lt_dm45l INTO ls_dm45l.

      CALL FUNCTION 'SDU_DOCU_DELETE'
        EXPORTING
          langu    = mv_language
          obj_id   = 'USPD'
          key1     = ls_dm45l-entid
          key2     = ls_dm45l-as4local
          key3     = ls_dm45l-spezid
        EXCEPTIONS
          ret_code = 0.

    ENDLOOP.


  ENDMETHOD.
  METHOD deserialize_docu_uen.

    DATA lt_docu TYPE ty_docu_lines.

    io_xml->read( EXPORTING iv_name = 'DOCU_UENC'
                 CHANGING cg_data = lt_docu ).
    deserialize_docu_xxxx( lt_docu ).


    CLEAR lt_docu.
    io_xml->read( EXPORTING iv_name = 'DOCU_UEND'
                 CHANGING cg_data = lt_docu ).
    deserialize_docu_xxxx( lt_docu ).


    CLEAR lt_docu.
    io_xml->read( EXPORTING iv_name = 'DOCU_UENE'
                 CHANGING cg_data = lt_docu ).
    deserialize_docu_xxxx( lt_docu ).

  ENDMETHOD.
  METHOD deserialize_docu_url.

    DATA lt_docu TYPE ty_docu_lines.

    io_xml->read( EXPORTING iv_name = 'DOCU_URL1'
                 CHANGING cg_data = lt_docu ).

    deserialize_docu_xxxx( lt_docu ).

    CLEAR lt_docu.
    io_xml->read( EXPORTING iv_name = 'DOCU_URL2'
                 CHANGING cg_data = lt_docu ).

    deserialize_docu_xxxx( lt_docu ).

    CLEAR lt_docu.
    io_xml->read( EXPORTING iv_name = 'DOCU_URLC'
                 CHANGING cg_data = lt_docu ).

    deserialize_docu_xxxx( lt_docu ).

  ENDMETHOD.
  METHOD deserialize_docu_usp.

    DATA lt_docu TYPE ty_docu_lines.

    io_xml->read( EXPORTING iv_name = 'DOCU_USPD'
                 CHANGING cg_data = lt_docu ).

    deserialize_docu_xxxx( lt_docu ).

  ENDMETHOD.
  METHOD deserialize_docu_xxxx.

    DATA ls_docu LIKE LINE OF it_docu.
    DATA lv_objname TYPE lxeobjname.
    DATA lv_change_flag TYPE char1.
    DATA lv_error_status  TYPE lxestatprc.

    LOOP AT it_docu INTO ls_docu.

      ls_docu-header-tdfuser = sy-uname.
      ls_docu-header-tdfdate = sy-datum.
      ls_docu-header-tdftime = sy-uzeit.
      ls_docu-header-tdfreles = sy-saprl.

      ls_docu-header-tdluser = sy-uname.
      ls_docu-header-tdldate = sy-datum.
      ls_docu-header-tdltime = sy-uzeit.
      ls_docu-header-tdlreles = sy-saprl.

      lv_objname = ls_docu-header-tdname.

      CALL FUNCTION 'LXE_OBJ_DOKU_PUT_XSTRING'
        EXPORTING
          slang       = mv_language
          tlang       = ls_docu-language
          objtype     = ls_docu-header-tdid
          objname     = lv_objname
          header      = ls_docu-header
          content     = ls_docu-content
        IMPORTING
          change_flag = lv_change_flag
          pstatus     = lv_error_status.

    ENDLOOP.


  ENDMETHOD.
  METHOD get_field_rules.

    DATA:
      lt_fields    TYPE TABLE OF string,
      lv_fields    TYPE string,
      lv_table     TYPE tabname,
      lv_field     TYPE string,
      lv_rule      TYPE string,
      lv_rule_iter TYPE string,
      lv_fill_rule TYPE Lif_abapgit_field_rules=>ty_fill_rule,
      lv_prefix    TYPE fieldname,
      lv_suffix    TYPE fieldname.

    ro_result = Lcl_abapgit_field_rules=>create( ).

    " Many tables and fields with date,time,user so we encode them
    APPEND 'DM02L,FL,DTU' TO lt_fields.
    APPEND 'DM02T,L,DTU' TO lt_fields.
    APPEND 'DM03S,FL,DTU' TO lt_fields.
    APPEND 'DM25L,FL,DTU' TO lt_fields.
    APPEND 'DM26L,FL,DTU' TO lt_fields.
    APPEND 'DM42S,FL,DTU' TO lt_fields.
    APPEND 'DM42T,L,DTU' TO lt_fields.
    APPEND 'DM43T,L,DU' TO lt_fields.
    APPEND 'DM45L,FL,DTU' TO lt_fields.
    APPEND 'DM45T,L,DTU' TO lt_fields.
    APPEND 'DM46S,FL,DTU' TO lt_fields.

    LOOP AT lt_fields INTO lv_fields.
      SPLIT lv_fields AT ',' INTO lv_table lv_field lv_rule_iter.

      DO strlen( lv_field ) TIMES.
        CASE lv_field(1).
          WHEN 'F'.
            lv_prefix = 'FST'.
          WHEN 'L'.
            lv_prefix = 'LST'.
        ENDCASE.

        lv_rule = lv_rule_iter.
        DO strlen( lv_rule ) TIMES.
          CASE lv_rule(1).
            WHEN 'D'.
              lv_suffix    = 'DATE'.
              lv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-date.
            WHEN 'T'.
              lv_suffix    = 'TIME'.
              lv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-time.
            WHEN 'U'.
              lv_suffix    = 'USER'.
              lv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user.
          ENDCASE.

          ro_result->add(
            iv_table     = lv_table
            iv_field     = lv_prefix && lv_suffix
            iv_fill_rule = lv_fill_rule ).

          SHIFT lv_rule LEFT.
        ENDDO.

        SHIFT lv_field LEFT.
      ENDDO.

    ENDLOOP.

  ENDMETHOD.
  METHOD get_generic.

    CREATE OBJECT ro_generic
      EXPORTING
        io_field_rules = get_field_rules( )
        is_item        = ms_item
        iv_language    = mv_language.

  ENDMETHOD.
  METHOD is_name_permitted.

    " It is unlikely that a serialized entity will have a name that is not permitted. However
    " there may be reservations in TRESE which could prohibit the entity name.
    " So to be safe, we check. Tx SD11 does this check.

    CALL FUNCTION 'SDU_SAA_CHECK'
      EXPORTING
        obj_name   = ms_item-obj_name
        obj_type   = ms_item-obj_type
      EXCEPTIONS
        wrong_type = 1.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD serialize_docu_uen.

    DATA lt_docu            TYPE ty_docu_lines.

    lt_docu = serialize_docu_xxxx( 'UENC' ).

    io_xml->add( iv_name = 'DOCU_UENC'
                 ig_data = lt_docu ).


    lt_docu = serialize_docu_xxxx( 'UEND' ).

    io_xml->add( iv_name = 'DOCU_UEND'
                 ig_data = lt_docu ).

    lt_docu = serialize_docu_xxxx( 'UENE' ).

    io_xml->add( iv_name = 'DOCU_UENE'
                 ig_data = lt_docu ).
  ENDMETHOD.
  METHOD serialize_docu_url.


    DATA lt_docu            TYPE ty_docu_lines.

    lt_docu = serialize_docu_xxxx( 'URL1' ).
    io_xml->add( iv_name = 'DOCU_URL1'
                 ig_data = lt_docu ).


    lt_docu = serialize_docu_xxxx( 'URL2' ).
    io_xml->add( iv_name = 'DOCU_URL2'
                 ig_data = lt_docu ).

    lt_docu = serialize_docu_xxxx( 'URLC' ).
    io_xml->add( iv_name = 'DOCU_URLC'
                 ig_data = lt_docu ).

  ENDMETHOD.
  METHOD serialize_docu_usp.

    DATA lt_docu            TYPE ty_docu_lines.

    lt_docu = serialize_docu_xxxx( 'USPD' ).

    io_xml->add( iv_name = 'DOCU_USPD'
                 ig_data = lt_docu ).


  ENDMETHOD.
  METHOD serialize_docu_xxxx.

    DATA ls_docu            TYPE ty_docu.
    DATA ls_dokvl           TYPE dokvl.
    DATA lt_dokvl           TYPE STANDARD TABLE OF dokvl.
    DATA lv_error_status    TYPE lxestatprc.
    DATA lv_objname         TYPE lxeobjname.


    ls_dokvl-object = build_text_name( iv_id ).

    SELECT id object langu
      FROM dokvl
      INTO CORRESPONDING FIELDS OF TABLE lt_dokvl
      WHERE id = c_text_object_type
      AND   object LIKE ls_dokvl-object
      ORDER BY PRIMARY KEY ##TOO_MANY_ITAB_FIELDS.

    LOOP AT lt_dokvl INTO ls_dokvl.

      ls_docu-language = ls_dokvl-langu.
      lv_objname = ls_dokvl-object.

      " You are reminded that this function gets the most recent version of the texts.
      CALL FUNCTION 'LXE_OBJ_DOKU_GET_XSTRING'
        EXPORTING
          lang    = ls_docu-language
          objtype = c_text_object_type
          objname = lv_objname
        IMPORTING
          header  = ls_docu-header
          content = ls_docu-content
          itf     = ls_docu-itf
          pstatus = lv_error_status.

      CHECK lv_error_status = 'S'. "Success

      " Administrative information is not
      CLEAR ls_docu-header-tdfuser.
      CLEAR ls_docu-header-tdfdate.
      CLEAR ls_docu-header-tdftime.
      CLEAR ls_docu-header-tdfreles.

      CLEAR ls_docu-header-tdluser.
      CLEAR ls_docu-header-tdldate.
      CLEAR ls_docu-header-tdltime.
      CLEAR ls_docu-header-tdlreles.

      APPEND ls_docu TO rt_result.

    ENDLOOP.


  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE lstuser INTO rv_user
      FROM dm02l
      WHERE entid = mv_entity_id
      AND as4local = c_active_state.

    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    " The deletion of the documentation occurs before the deletion of
    " the associated tables - otherwise we don't know what
    " documentation needs deletion
    delete_docu_uen( ).
    delete_docu_url( ).
    delete_docu_usp( ).

    " the deletion of the tables of the entity
    get_generic( )->delete( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    " Is the entity type name compliant with naming conventions?
    " Entity Type have their own conventions.
    is_name_permitted( ).

    get_generic( )->deserialize(
      iv_package = iv_package
      io_xml     = io_xml ).

    deserialize_docu_uen( io_xml ).
    deserialize_docu_url( io_xml ).
    deserialize_docu_usp( io_xml ).

    " You are reminded that entity types are not relevant for activation.

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

    rv_is_locked = exists_a_lock_entry_for(
      iv_lock_object = 'ESDUM'
      iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    " The function module listed below do not open a new window - so we revert to BDC.
    "    CALL FUNCTION 'SDU_MODEL_SHOW'
    "    CALL FUNCTION 'RS_TOOL_ACCESS'

    DATA lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPMUD00'.
    <ls_bdcdata>-dynpro   = '0100'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=SHOW'.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'RSUD3-ENTI'.
    <ls_bdcdata>-fval = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'RSUD3-OBJ_KEY'.
    <ls_bdcdata>-fval = ms_item-obj_name.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SD11'
      it_bdcdata = lt_bdcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    get_generic( )->serialize( io_xml ).

    serialize_docu_uen( io_xml ).
    serialize_docu_url( io_xml ).
    serialize_docu_usp( io_xml ).

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
endclass. "ZCL_ABAPGIT_OBJECT_UENO implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_VIEW <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_view=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_view=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_VIEW implementation.
*"* method's implementations
*include methods.
  METHOD deserialize_texts.

    DATA:
      lv_name       TYPE ddobjname,
      lt_i18n_langs TYPE TABLE OF langu,
      lt_dd25_texts TYPE ty_dd25_texts,
      ls_dd25v_tmp  TYPE dd25v.

    FIELD-SYMBOLS:
      <lv_lang>      TYPE langu,
      <ls_dd25_text> LIKE LINE OF lt_dd25_texts.

    lv_name = ms_item-obj_name.

    ii_xml->read( EXPORTING iv_name = 'I18N_LANGS'
                  CHANGING  cg_data = lt_i18n_langs ).

    ii_xml->read( EXPORTING iv_name = 'DD25_TEXTS'
                  CHANGING  cg_data = lt_dd25_texts ).

    mo_i18n_params->trim_saplang_list( CHANGING ct_sap_langs = lt_i18n_langs ).

    SORT lt_i18n_langs.
    SORT lt_dd25_texts BY ddlanguage.

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.

      " View description
      ls_dd25v_tmp = is_dd25v.
      READ TABLE lt_dd25_texts ASSIGNING <ls_dd25_text> WITH KEY ddlanguage = <lv_lang>.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |DD25_TEXTS cannot find lang { <lv_lang> } in XML| ).
      ENDIF.
      MOVE-CORRESPONDING <ls_dd25_text> TO ls_dd25v_tmp.
      CALL FUNCTION 'DDIF_VIEW_PUT'
        EXPORTING
          name              = lv_name
          dd25v_wa          = ls_dd25v_tmp
        EXCEPTIONS
          view_not_found    = 1
          name_inconsistent = 2
          view_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD read_view.

    DATA: lv_name TYPE ddobjname.

    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_VIEW_GET'
      EXPORTING
        name          = lv_name
        state         = 'A'
        langu         = iv_language
      IMPORTING
        gotstate      = ev_state
        dd25v_wa      = es_dd25v
        dd09l_wa      = es_dd09l
      TABLES
        dd26v_tab     = et_dd26v
        dd27p_tab     = et_dd27p
        dd28j_tab     = et_dd28j
        dd28v_tab     = et_dd28v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD serialize_texts.

    DATA:
      lv_index           TYPE i,
      ls_dd25v           TYPE dd25v,
      lt_dd25_texts      TYPE ty_dd25_texts,
      lt_i18n_langs      TYPE TABLE OF langu,
      lt_language_filter TYPE Lif_abapgit_environment=>ty_system_language_filter.

    FIELD-SYMBOLS:
      <lv_lang>      LIKE LINE OF lt_i18n_langs,
      <ls_dd25_text> LIKE LINE OF lt_dd25_texts.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      RETURN.
    ENDIF.

    " Collect additional languages, skip main lang - it was serialized already
    lt_language_filter = mo_i18n_params->build_language_filter( ).

    SELECT DISTINCT ddlanguage AS langu INTO TABLE lt_i18n_langs
      FROM dd25v
      WHERE viewname = ms_item-obj_name
      AND ddlanguage IN lt_language_filter
      AND ddlanguage <> mv_language
      ORDER BY langu.                                     "#EC CI_SUBRC

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.
      lv_index = sy-tabix.
      CLEAR: ls_dd25v.

      TRY.
          read_view(
            EXPORTING
              iv_language = <lv_lang>
            IMPORTING
              es_dd25v    = ls_dd25v ).

        CATCH Lcx_abapgit_exception.
          CONTINUE.
      ENDTRY.

      IF ls_dd25v-ddlanguage IS INITIAL.
        DELETE lt_i18n_langs INDEX lv_index. " Don't save this lang
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO lt_dd25_texts ASSIGNING <ls_dd25_text>.
      MOVE-CORRESPONDING ls_dd25v TO <ls_dd25_text>.

    ENDLOOP.

    SORT lt_i18n_langs ASCENDING.
    SORT lt_dd25_texts BY ddlanguage ASCENDING.

    IF lines( lt_i18n_langs ) > 0.
      ii_xml->add( iv_name = 'I18N_LANGS'
                   ig_data = lt_i18n_langs ).

      ii_xml->add( iv_name = 'DD25_TEXTS'
                   ig_data = lt_dd25_texts ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE as4user FROM dd25l INTO rv_user
      WHERE viewname = ms_item-obj_name
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

    delete_ddic( 'V' ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_name  TYPE ddobjname,
          ls_dd25v TYPE dd25v,
          ls_dd09l TYPE dd09l,
          lt_dd26v TYPE TABLE OF dd26v,
          lt_dd27p TYPE TABLE OF dd27p,
          lt_dd28j TYPE TABLE OF dd28j,
          lt_dd28v TYPE TABLE OF dd28v.

    FIELD-SYMBOLS: <ls_dd27p> LIKE LINE OF lt_dd27p.

    io_xml->read( EXPORTING iv_name = 'DD25V'
                  CHANGING cg_data = ls_dd25v ).
    io_xml->read( EXPORTING iv_name = 'DD09L'
                  CHANGING cg_data = ls_dd09l ).
    io_xml->read( EXPORTING iv_name = 'DD26V_TABLE'
                  CHANGING cg_data = lt_dd26v ).
    io_xml->read( EXPORTING iv_name = 'DD27P_TABLE'
                  CHANGING cg_data = lt_dd27p ).
    io_xml->read( EXPORTING iv_name = 'DD28J_TABLE'
                  CHANGING cg_data = lt_dd28j ).
    io_xml->read( EXPORTING iv_name = 'DD28V_TABLE'
                  CHANGING cg_data = lt_dd28v ).

    lv_name = ms_item-obj_name. " type conversion

    LOOP AT lt_dd27p ASSIGNING <ls_dd27p>.
      <ls_dd27p>-objpos = sy-tabix.
      <ls_dd27p>-viewname = lv_name.
      " rollname seems to be mandatory in the API, but is typically not defined in the VIEW
      SELECT SINGLE rollname FROM dd03l INTO <ls_dd27p>-rollname
        WHERE tabname = <ls_dd27p>-tabname
        AND fieldname = <ls_dd27p>-fieldname.
      IF <ls_dd27p>-rollnamevi IS INITIAL.
        <ls_dd27p>-rollnamevi = <ls_dd27p>-rollname.
      ENDIF.
    ENDLOOP.

    corr_insert( iv_package = iv_package
                 ig_object_class = 'DICT' ).

    CALL FUNCTION 'DDIF_VIEW_PUT'
      EXPORTING
        name              = lv_name
        dd25v_wa          = ls_dd25v
        dd09l_wa          = ls_dd09l
      TABLES
        dd26v_tab         = lt_dd26v
        dd27p_tab         = lt_dd27p
        dd28j_tab         = lt_dd28j
        dd28v_tab         = lt_dd28v
      EXCEPTIONS
        view_not_found    = 1
        name_inconsistent = 2
        view_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
      deserialize_texts(
        ii_xml   = io_xml
        is_dd25v = ls_dd25v ).
    ENDIF.

    deserialize_longtexts( ii_xml         = io_xml
                           iv_longtext_id = c_longtext_id_view ).

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_viewname TYPE dd25l-viewname,
          lv_ddl_view TYPE abap_bool.

    SELECT SINGLE viewname FROM dd25l INTO lv_viewname
      WHERE viewname = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

    IF rv_bool = abap_true.
      TRY.
          CALL METHOD ('CL_DD_DDL_UTILITIES')=>('CHECK_FOR_DDL_VIEW')
            EXPORTING
              objname     = lv_viewname
            RECEIVING
              is_ddl_view = lv_ddl_view.

          IF lv_ddl_view = abap_true.
            rv_bool = abap_false.
          ENDIF.
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.
    ENDIF.

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

    DATA: ls_dd25v TYPE dd25v,
          lv_state TYPE ddgotstate,
          ls_dd09l TYPE dd09l,
          lt_dd26v TYPE ty_dd26v,
          lt_dd27p TYPE ty_dd27p,
          lt_dd28j TYPE ty_dd28j,
          lt_dd28v TYPE ty_dd28v.

    FIELD-SYMBOLS: <ls_dd27p> LIKE LINE OF lt_dd27p.

    read_view(
      EXPORTING
        iv_language = mv_language
      IMPORTING
        ev_state    = lv_state
        es_dd25v    = ls_dd25v
        es_dd09l    = ls_dd09l
        et_dd26v    = lt_dd26v
        et_dd27p    = lt_dd27p
        et_dd28j    = lt_dd28j
        et_dd28v    = lt_dd28v ).

    IF ls_dd25v IS INITIAL OR lv_state <> 'A'.
      RETURN.
    ENDIF.

    CLEAR: ls_dd25v-as4user,
           ls_dd25v-as4date,
           ls_dd25v-as4time.

    CLEAR: ls_dd09l-as4user,
           ls_dd09l-as4date,
           ls_dd09l-as4time.

    LOOP AT lt_dd27p ASSIGNING <ls_dd27p>.
      CLEAR: <ls_dd27p>-ddtext,
             <ls_dd27p>-reptext,
             <ls_dd27p>-scrtext_s,
             <ls_dd27p>-scrtext_m,
             <ls_dd27p>-scrtext_l,
             <ls_dd27p>-outputlen,
             <ls_dd27p>-decimals,
             <ls_dd27p>-lowercase,
             <ls_dd27p>-convexit,
             <ls_dd27p>-signflag,
             <ls_dd27p>-flength,
             <ls_dd27p>-domname,
             <ls_dd27p>-datatype,
             <ls_dd27p>-entitytab,
             <ls_dd27p>-inttype,
             <ls_dd27p>-intlen,
             <ls_dd27p>-headlen,
             <ls_dd27p>-scrlen1,
             <ls_dd27p>-scrlen2,
             <ls_dd27p>-scrlen3,
             <ls_dd27p>-memoryid.
      IF <ls_dd27p>-rollchange = abap_false.
        CLEAR <ls_dd27p>-rollnamevi.
      ENDIF.
      CLEAR <ls_dd27p>-ddlanguage.
      CLEAR <ls_dd27p>-rollname.
      CLEAR <ls_dd27p>-viewname.
      CLEAR <ls_dd27p>-objpos.
    ENDLOOP.

    io_xml->add( iv_name = 'DD25V'
                 ig_data = ls_dd25v ).
    io_xml->add( iv_name = 'DD09L'
                 ig_data = ls_dd09l ).
    io_xml->add( ig_data = lt_dd26v
                 iv_name = 'DD26V_TABLE' ).
    io_xml->add( ig_data = lt_dd27p
                 iv_name = 'DD27P_TABLE' ).
    io_xml->add( ig_data = lt_dd28j
                 iv_name = 'DD28J_TABLE' ).
    io_xml->add( ig_data = lt_dd28v
                 iv_name = 'DD28V_TABLE' ).

    IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
      serialize_texts( io_xml ).
    ENDIF.

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_view ).

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
endclass. "ZCL_ABAPGIT_OBJECT_VIEW implementation

*>>>>>>> ZCL_ABAPGIT_PERSIST_MIGRATE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_persist_migrate===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_persist_migrate===ccimp.
CLASS SHRITEFUH64VYIPN5I4UHL45BMNR4A DEFINITION INHERITING FROM Lcl_abapgit_objects_program FINAL.
  PUBLIC SECTION.
    CLASS-METHODS new
      RETURNING
        VALUE(ro_instance) TYPE REF TO SHRITEFUH64VYIPN5I4UHL45BMNR4A.
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

CLASS SHRITEFUH64VYIPN5I4UHL45BMNR4A IMPLEMENTATION.

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

CLASS SHRITEFUH64VYIPN5I4UHL45BMPR4A DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS get
      RETURNING
        VALUE(rs_cua) TYPE Lcl_abapgit_objects_program=>ty_cua ##NEEDED.
ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BMPR4A IMPLEMENTATION.
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

    ls_cua = SHRITEFUH64VYIPN5I4UHL45BMPR4A=>get( ).

    IF ls_cua IS INITIAL. " Full version or something wrong with abapmerged version
      RETURN.
    ENDIF.

    TRY.
        SHRITEFUH64VYIPN5I4UHL45BMNR4A=>new( )->put_own_cua( ls_cua ).
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
        ls_own_cua = SHRITEFUH64VYIPN5I4UHL45BMNR4A=>new( )->get_own_cua( ).
      CATCH Lcx_abapgit_exception.
    ENDTRY.

    IF ls_own_cua IS INITIAL.
      rv_exists = abap_false.
      RETURN.
    ENDIF.

    ls_new_cua = SHRITEFUH64VYIPN5I4UHL45BMPR4A=>get( ).
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
      WHERE tabname = Lcl_abapgit_persistence_db=>c_tabname. "#EC CI_NOORDER
    rv_exists = boolc( sy-subrc = 0 ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_PERSIST_MIGRATE implementation

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

    SORT rt_pulls DESCENDING BY number.

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
  METHOD has_remote_source.
    rv_yes = boolc( lines( mt_remote ) > 0 ).
  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_tadir_objects.

    rt_tadir = Lcl_abapgit_factory=>get_tadir( )->read(
      iv_package            = get_package( )
      iv_ignore_subpackages = get_local_settings( )-ignore_subpackages
      iv_only_local_objects = get_local_settings( )-only_local_objects
      io_dot                = get_dot_abapgit( ) ).

  ENDMETHOD.
  METHOD bind_listener.
    mi_listener = ii_listener.
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

    lt_tadir = get_tadir_objects( ).

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

    lt_tadir = get_tadir_objects( ).

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

    IF is_checks-dependencies-met = Lif_abapgit_definitions=>c_no AND is_checks-dependencies-decision IS INITIAL.
      Lcx_abapgit_exception=>raise( 'APACK dependencies not met and undecided' ).
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

    " Local display name has priority over official name
    rv_name = ms_data-local_settings-display_name.
    IF rv_name IS INITIAL.
      rv_name = ms_data-dot_abapgit-name.
    ENDIF.

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
CLASS SHRITEFUH64VYIPN5I4UHL45BMYR4A DEFINITION
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



CLASS SHRITEFUH64VYIPN5I4UHL45BMYR4A IMPLEMENTATION.


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

CLASS SHRITEFUH64VYIPN5I4UHL45BM2R4A DEFINITION
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

CLASS SHRITEFUH64VYIPN5I4UHL45BM2R4A IMPLEMENTATION.

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

CLASS SHRITEFUH64VYIPN5I4UHL45BM7R4A DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_repo.
    INTERFACES Lif_abapgit_repo_srv.
    DATA mt_local_files TYPE Lif_abapgit_definitions=>ty_files_item_tt.
    DATA mt_remote_files TYPE Lif_abapgit_git_definitions=>ty_files_tt.
ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BM7R4A IMPLEMENTATION.

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
  METHOD Lif_abapgit_repo~has_remote_source.
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
  METHOD Lif_abapgit_repo~get_tadir_objects.
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

CLASS SHRITEFUH64VYIPN5I4UHL45BNBR4A DEFINITION FINAL.
  PUBLIC SECTION.
    DATA mt_tab TYPE Lif_abapgit_definitions=>ty_files_item_tt.
    METHODS add IMPORTING iv_str TYPE string.
ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BNBR4A IMPLEMENTATION.
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

CLASS SHRITEFUH64VYIPN5I4UHL45BNDR4A DEFINITION FINAL.
  PUBLIC SECTION.
    DATA mt_tab TYPE Lif_abapgit_git_definitions=>ty_files_tt.
    METHODS add IMPORTING iv_str TYPE string.
ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BNDR4A IMPLEMENTATION.
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

CLASS SHRITEFUH64VYIPN5I4UHL45BNFR4A DEFINITION FINAL.
  PUBLIC SECTION.
    DATA mt_tab TYPE Lif_abapgit_git_definitions=>ty_file_signatures_tt.
    METHODS add IMPORTING iv_str TYPE string.
ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BNFR4A IMPLEMENTATION.
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

    lv_cs_blob = SHRITEFUH64VYIPN5I4UHL45BMYR4A=>serialize( it_checksums ).
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
      rt_checksums = SHRITEFUH64VYIPN5I4UHL45BMYR4A=>deserialize( lv_cs_blob ).
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

    lt_checksums = SHRITEFUH64VYIPN5I4UHL45BM2R4A=>calculate_updated(
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
  METHOD build_repo_items.

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

    FIELD-SYMBOLS <ls_repo_item> LIKE LINE OF rt_repo_items.

    mi_log->clear( ).

    rt_repo_items = build_repo_items( ).
    check_repo_size( ).

    IF mo_repo->has_remote_source( ) = abap_false.
      " If there's no remote source, ignore the item state
      LOOP AT rt_repo_items ASSIGNING <ls_repo_item>.
        CLEAR: <ls_repo_item>-changes, <ls_repo_item>-lstate, <ls_repo_item>-rstate.
      ENDLOOP.
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
      SPLIT lv_tmp AT '/' INTO rs_parsed-fg rs_parsed-bg rs_parsed-border.
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
    IF ls_parsed_color-border IS NOT INITIAL.
      validate_rgb_color( ls_parsed_color-border ).
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
        Lcx_abapgit_exception=>raise( |Incorrect color in pair #{ iv_index }| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_REPO_LABELS implementation

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

    IF iv_name IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Missing name for repository' ).
    ENDIF.

    " Repo Settings
    lo_dot_abapgit = Lcl_abapgit_dot_abapgit=>build_default( ).
    lo_dot_abapgit->set_folder_logic( iv_folder_logic ).
    lo_dot_abapgit->set_name( iv_name ).
    lo_dot_abapgit->set_abap_language_version( iv_abap_lang_vers ).

    lv_key = Lcl_abapgit_persist_factory=>get_repo( )->add(
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
    lo_dot_abapgit->set_name( iv_name ).
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

    lt_tadir = lo_repo->get_tadir_objects( ).

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
CLASS SHRITEFUH64VYIPN5I4UHL45BNMR4A DEFINITION FINAL.
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

CLASS SHRITEFUH64VYIPN5I4UHL45BNMR4A IMPLEMENTATION.

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
*CLASS SHRITEFUH64VYIPN5I4UHL45BNOR4A DEFINITION DEFERRED.
*CLASS zcl_abapgit_repo_status DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BNOR4A.





CLASS SHRITEFUH64VYIPN5I4UHL45BNSR4A DEFINITION.

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

CLASS SHRITEFUH64VYIPN5I4UHL45BNSR4A IMPLEMENTATION.

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

*CLASS SHRITEFUH64VYIPN5I4UHL45BNUR4A DEFINITION DEFERRED.
*CLASS zcl_abapgit_repo_status DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BNUR4A.



*CLASS SHRITEFUH64VYIPN5I4UHL45BNWR4A DEFINITION DEFERRED.
*CLASS zcl_abapgit_repo_status DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BNWR4A.



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
    DATA lo_consistency_checks TYPE REF TO SHRITEFUH64VYIPN5I4UHL45BNMR4A.

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

*>>>>>>> ZCL_ABAPGIT_SERIALIZE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_serialize=========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_serialize=========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_serialize=========ccau.
*CLASS SHRITEFUH64VYIPN5I4UHL45BOER4A DEFINITION DEFERRED.
*CLASS zcl_abapgit_serialize DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BOER4A.

























class LCL_ABAPGIT_SERIALIZE implementation.
*"* method's implementations
*include methods.
  METHOD is_parallelization_possible.

    rv_result = boolc( Lcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_false
                   AND Lcl_abapgit_persist_factory=>get_settings( )->read( )->get_parallel_proc_disabled( ) = abap_false
                   AND mv_group IS NOT INITIAL
                   " The function module below should always exist here as is_merged evaluated to false above.
                   " It does however not exist in the transpiled version which then causes unit tests to fail.
                   " Therefore the check needs to stay.
                   AND Lcl_abapgit_factory=>get_function_module(
                                         )->function_exists( 'Z_ABAPGIT_SERIALIZE_PARALLEL' ) = abap_true ).

  ENDMETHOD.
  METHOD add_apack.

    DATA ls_apack_file TYPE Lif_abapgit_git_definitions=>ty_file.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF ct_files.


    ls_apack_file = Lcl_abapgit_apack_helper=>to_file( iv_package ).
    IF ls_apack_file IS NOT INITIAL.
      APPEND INITIAL LINE TO ct_files ASSIGNING <ls_file>.
      <ls_file>-file = ls_apack_file.
    ENDIF.

  ENDMETHOD.
  METHOD add_data.

    DATA lt_files TYPE Lif_abapgit_git_definitions=>ty_files_tt.
    DATA ls_file LIKE LINE OF lt_files.

    FIELD-SYMBOLS <ls_return> LIKE LINE OF ct_files.

    IF ii_data_config IS INITIAL.
      RETURN.
    ENDIF.

    lt_files = ii_data_config->to_json( ).
    LOOP AT lt_files INTO ls_file.
      APPEND INITIAL LINE TO ct_files ASSIGNING <ls_return>.
      <ls_return>-file = ls_file.

      " Derive object from config filename (namespace + escaping)
      Lcl_abapgit_filename_logic=>file_to_object(
        EXPORTING
          iv_filename = <ls_return>-file-filename
          iv_path     = <ls_return>-file-path
          io_dot      = mo_dot_abapgit
        IMPORTING
          es_item     = <ls_return>-item ).

      <ls_return>-item-obj_type = Lif_abapgit_data_config=>c_data_type-tabu. " todo
    ENDLOOP.

    lt_files = Lcl_abapgit_data_factory=>get_serializer( )->serialize( ii_data_config ).
    LOOP AT lt_files INTO ls_file.
      APPEND INITIAL LINE TO ct_files ASSIGNING <ls_return>.
      <ls_return>-file = ls_file.

      " Derive object from data filename (namespace + escaping)
      Lcl_abapgit_filename_logic=>file_to_object(
        EXPORTING
          iv_filename = <ls_return>-file-filename
          iv_path     = <ls_return>-file-path
          io_dot      = mo_dot_abapgit
        IMPORTING
          es_item     = <ls_return>-item ).
    ENDLOOP.

  ENDMETHOD.
  METHOD add_dot_abapgit.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF ct_files.

    APPEND INITIAL LINE TO ct_files ASSIGNING <ls_file>.
    <ls_file>-file = mo_dot_abapgit->to_file( ).

  ENDMETHOD.
  METHOD add_objects.

    DATA: lo_filter TYPE REF TO Lcl_abapgit_repo_filter,
          lv_force  TYPE abap_bool,
          lt_found  LIKE ct_files,
          lt_tadir  TYPE Lif_abapgit_definitions=>ty_tadir_tt.

    lt_tadir = Lcl_abapgit_factory=>get_tadir( )->read(
      iv_package            = iv_package
      iv_ignore_subpackages = ms_local_settings-ignore_subpackages
      iv_only_local_objects = ms_local_settings-only_local_objects
      io_dot                = mo_dot_abapgit
      ii_log                = ii_log
      it_filter             = it_filter ).

    CREATE OBJECT lo_filter.

    lo_filter->apply( EXPORTING it_filter = it_filter
                      CHANGING  ct_tadir  = lt_tadir ).

* if there are less than 10 objects run in single thread
* this helps a lot when debugging, plus performance gain
* with low number of objects does not matter much
    lv_force = boolc( lines( lt_tadir ) < 10 ).

    lt_found = serialize(
      iv_package          = iv_package
      it_tadir            = lt_tadir
      ii_log              = ii_log
      iv_force_sequential = lv_force ).
    APPEND LINES OF lt_found TO ct_files.

  ENDMETHOD.
  METHOD add_to_return.

    FIELD-SYMBOLS: <ls_file>   LIKE LINE OF is_file_item-files,
                   <ls_return> LIKE LINE OF mt_files.


    LOOP AT is_file_item-files ASSIGNING <ls_file>.
      APPEND INITIAL LINE TO mt_files ASSIGNING <ls_return>.
      <ls_return>-file = <ls_file>.
      <ls_return>-file-path = iv_path.
      <ls_return>-item = is_file_item-item.
    ENDLOOP.

  ENDMETHOD.
  METHOD constructor.

    DATA li_exit TYPE REF TO Lif_abapgit_exit.

    mv_group = 'parallel_generators'.
    li_exit = Lcl_abapgit_exit=>get_instance( ).
    li_exit->change_rfc_server_group( CHANGING cv_group = mv_group ).

    mo_dot_abapgit = io_dot_abapgit.
    ms_local_settings = is_local_settings.

    ms_i18n_params = determine_i18n_params(
      io_dot = io_dot_abapgit
      iv_main_language_only = is_local_settings-main_language_only ).

    CREATE OBJECT mo_abap_language_version
      EXPORTING
        io_dot_abapgit = mo_dot_abapgit.

  ENDMETHOD.
  METHOD files_local.

* serializes objects, including .abapgit.xml, apack, and takes into account local settings

    add_dot_abapgit( CHANGING ct_files = rt_files ).

    add_apack(
      EXPORTING
        iv_package = iv_package
      CHANGING
        ct_files   = rt_files ).

    add_data(
      EXPORTING
        ii_data_config = ii_data_config
      CHANGING
        ct_files       = rt_files ).

    add_objects(
      EXPORTING
        iv_package = iv_package
        ii_log     = ii_log
        it_filter  = it_filter
      CHANGING
        ct_files   = rt_files ).

  ENDMETHOD.
  METHOD filter_ignored_objects.

    DATA:
      ls_ignored_count TYPE ty_unsupported_count,
      lt_ignored_count TYPE ty_unsupported_count_tt,
      lo_folder_logic  TYPE REF TO Lcl_abapgit_folder_logic,
      ls_item          TYPE Lif_abapgit_definitions=>ty_item,
      lv_path          TYPE string,
      lv_filename      TYPE string.

    FIELD-SYMBOLS:
      <ls_tadir>         LIKE LINE OF ct_tadir,
      <ls_ignored_count> TYPE ty_unsupported_count.

    " Ignore logic requires .abapGit.xml
    IF mo_dot_abapgit IS INITIAL OR iv_package IS INITIAL OR mi_log IS INITIAL.
      RETURN.
    ENDIF.

    lo_folder_logic = Lcl_abapgit_folder_logic=>get_instance( ).

    LOOP AT ct_tadir ASSIGNING <ls_tadir>.
      CLEAR: ls_ignored_count.

      ls_item-obj_type = <ls_tadir>-object.
      ls_item-obj_name = <ls_tadir>-obj_name.

      IF <ls_tadir>-devclass IS NOT INITIAL.
        lv_path = lo_folder_logic->package_to_path(
          iv_top     = iv_package
          io_dot     = mo_dot_abapgit
          iv_package = <ls_tadir>-devclass ).
      ELSE.
        lv_path = mo_dot_abapgit->get_starting_folder( ).
      ENDIF.

      lv_filename = Lcl_abapgit_filename_logic=>object_to_file(
        is_item  = ls_item
        iv_ext   = '*' ).

      IF mo_dot_abapgit->is_ignored(
        iv_path     = lv_path
        iv_filename = lv_filename ) = abap_false.
        CONTINUE.
      ENDIF.

      READ TABLE lt_ignored_count ASSIGNING <ls_ignored_count> WITH TABLE KEY obj_type = <ls_tadir>-object.
      IF sy-subrc <> 0.
        ls_ignored_count-obj_type = <ls_tadir>-object.
        ls_ignored_count-count    = 1.
        ls_ignored_count-obj_name = <ls_tadir>-obj_name.
        INSERT ls_ignored_count INTO TABLE lt_ignored_count ASSIGNING <ls_ignored_count>.
      ELSE.
        CLEAR: <ls_ignored_count>-obj_name.
        <ls_ignored_count>-count = <ls_ignored_count>-count + 1.
      ENDIF.
      " init object so we can remove these entries afterward
      CLEAR <ls_tadir>-object.
    ENDLOOP.
    IF lt_ignored_count IS INITIAL.
      RETURN.
    ENDIF.

    " remove ignored objects
    DELETE ct_tadir WHERE object IS INITIAL.

    LOOP AT lt_ignored_count ASSIGNING <ls_ignored_count>.
      IF <ls_ignored_count>-count = 1.
        mi_log->add_warning( |Object { <ls_ignored_count>-obj_type } { <ls_ignored_count>-obj_name } ignored| ).
      ELSE.
        mi_log->add_warning( |Object type { <ls_ignored_count>-obj_type } with | &&
                             |{ <ls_ignored_count>-count } objects ignored| ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD filter_unsupported_objects.

    DATA: ls_unsupported_count TYPE ty_unsupported_count,
          lt_supported_types   TYPE Lcl_abapgit_objects=>ty_types_tt,
          lt_unsupported_count TYPE ty_unsupported_count_tt.

    FIELD-SYMBOLS: <ls_tadir>             LIKE LINE OF ct_tadir,
                   <ls_unsupported_count> TYPE ty_unsupported_count.

    lt_supported_types = Lcl_abapgit_objects=>supported_list( ).
    LOOP AT ct_tadir ASSIGNING <ls_tadir>.
      CLEAR: ls_unsupported_count.
      READ TABLE lt_supported_types WITH KEY table_line = <ls_tadir>-object TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      READ TABLE lt_unsupported_count ASSIGNING <ls_unsupported_count>
                                      WITH TABLE KEY obj_type = <ls_tadir>-object.
      IF sy-subrc <> 0.
        ls_unsupported_count-obj_type = <ls_tadir>-object.
        ls_unsupported_count-count    = 1.
        ls_unsupported_count-obj_name = <ls_tadir>-obj_name.
        INSERT ls_unsupported_count INTO TABLE lt_unsupported_count ASSIGNING <ls_unsupported_count>.
      ELSE.
        CLEAR: <ls_unsupported_count>-obj_name.
        <ls_unsupported_count>-count = <ls_unsupported_count>-count + 1.
      ENDIF.
      CLEAR: <ls_tadir>-object.
    ENDLOOP.
    IF lt_unsupported_count IS INITIAL.
      RETURN.
    ENDIF.

    DELETE ct_tadir WHERE object IS INITIAL.
    IF mi_log IS BOUND.
      LOOP AT lt_unsupported_count ASSIGNING <ls_unsupported_count>.
        IF <ls_unsupported_count>-count = 1.
          mi_log->add_error( |Object type { <ls_unsupported_count>-obj_type } not supported, | &&
                             |{ <ls_unsupported_count>-obj_name } ignored| ).
        ELSE.
          mi_log->add_error( |Object type { <ls_unsupported_count>-obj_type } not supported, | &&
                             |{ <ls_unsupported_count>-count } objects ignored| ).
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD on_end_of_task.

* this method will be called from the parallel processing, thus it must be public

    DATA: lv_result    TYPE xstring,
          lv_path      TYPE string,
          lv_mess      TYPE c LENGTH 200,
          ls_file_item TYPE Lif_abapgit_objects=>ty_serialization.


    RECEIVE RESULTS FROM FUNCTION 'Z_ABAPGIT_SERIALIZE_PARALLEL'
      IMPORTING
        ev_result             = lv_result
        ev_path               = lv_path
      EXCEPTIONS
        error                 = 1
        system_failure        = 2 MESSAGE lv_mess
        communication_failure = 3 MESSAGE lv_mess
        OTHERS = 4.
    IF sy-subrc <> 0.
      IF NOT mi_log IS INITIAL.
        IF NOT lv_mess IS INITIAL.
          mi_log->add_error( lv_mess ).
        ELSE.
          mi_log->add_error( |{ sy-msgv1 }{ sy-msgv2 }{ sy-msgv3 }{ sy-msgv3 }| ).
        ENDIF.
      ENDIF.
    ELSE.
      IMPORT data = ls_file_item FROM DATA BUFFER lv_result. "#EC CI_SUBRC
      ASSERT sy-subrc = 0.
      add_to_return( is_file_item = ls_file_item
                     iv_path      = lv_path ).
    ENDIF.

    mv_free = mv_free + 1.

  ENDMETHOD.
  METHOD run_parallel.

    DATA: lv_msg  TYPE c LENGTH 100,
          lv_task TYPE c LENGTH 32,
          lv_free LIKE mv_free.
    DATA lv_abap_language_version TYPE Lif_abapgit_aff_types_v1=>ty_abap_language_version.

    ASSERT mv_free > 0.

    lv_abap_language_version = mo_abap_language_version->get_repo_abap_language_version( ).

    DO.
      lv_task = |{ iv_task }-{ sy-index }|.
      CALL FUNCTION 'Z_ABAPGIT_SERIALIZE_PARALLEL'
        STARTING NEW TASK lv_task
        DESTINATION IN GROUP mv_group
        CALLING on_end_of_task ON END OF TASK
        EXPORTING
          iv_obj_type           = is_tadir-object
          iv_obj_name           = is_tadir-obj_name
          iv_devclass           = is_tadir-devclass
          iv_path               = is_tadir-path
          iv_srcsystem          = is_tadir-srcsystem
          iv_abap_language_vers = lv_abap_language_version
          iv_language           = ms_i18n_params-main_language
          iv_main_language_only = ms_i18n_params-main_language_only
          it_translation_langs  = ms_i18n_params-translation_languages
          iv_use_lxe            = ms_i18n_params-use_lxe
        EXCEPTIONS
          system_failure        = 1 MESSAGE lv_msg
          communication_failure = 2 MESSAGE lv_msg
          resource_failure      = 3
          OTHERS                = 4.
      IF sy-subrc = 3.
        lv_free = mv_free.
        WAIT UNTIL mv_free <> lv_free UP TO 1 SECONDS.
        CONTINUE.
      ELSEIF sy-subrc <> 0.
        ASSERT lv_msg = '' AND 0 = 1.
      ENDIF.
      EXIT.
    ENDDO.

    mv_free = mv_free - 1.

  ENDMETHOD.
  METHOD run_sequential.

    DATA: lx_error     TYPE REF TO Lcx_abapgit_exception,
          ls_file_item TYPE Lif_abapgit_objects=>ty_serialization.

    ls_file_item-item-obj_type  = is_tadir-object.
    ls_file_item-item-obj_name  = is_tadir-obj_name.
    ls_file_item-item-devclass  = is_tadir-devclass.
    ls_file_item-item-srcsystem = is_tadir-srcsystem.
    ls_file_item-item-abap_language_version = mo_abap_language_version->get_repo_abap_language_version( ).

    TRY.
        ls_file_item = Lcl_abapgit_objects=>serialize(
          is_item        = ls_file_item-item
          io_i18n_params = Lcl_abapgit_i18n_params=>new( is_params = ms_i18n_params ) ).

        add_to_return( is_file_item = ls_file_item
                       iv_path      = is_tadir-path ).
      CATCH Lcx_abapgit_exception INTO lx_error.
        IF NOT mi_log IS INITIAL.
          mi_log->add_exception(
              ix_exc  = lx_error
              is_item = ls_file_item-item ).
        ENDIF.
    ENDTRY.

  ENDMETHOD.
  METHOD serialize.

* serializes only objects

    DATA: lv_max      TYPE i,
          lv_count    TYPE i,
          li_progress TYPE REF TO Lif_abapgit_progress,
          li_exit     TYPE REF TO Lif_abapgit_exit,
          lo_timer    TYPE REF TO Lcl_abapgit_timer,
          lt_tadir    TYPE Lif_abapgit_definitions=>ty_tadir_tt.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF it_tadir.


    CLEAR mt_files.

    lv_max = determine_max_processes( iv_force_sequential = iv_force_sequential
                                      iv_package          = iv_package ).
    mv_free = lv_max.
    mi_log = ii_log.

    lt_tadir = it_tadir.
    filter_unsupported_objects( CHANGING ct_tadir = lt_tadir ).

    filter_ignored_objects(
      EXPORTING
        iv_package = iv_package
      CHANGING
        ct_tadir   = lt_tadir ).

    lv_count = lines( lt_tadir ).

    li_progress = Lcl_abapgit_progress=>get_instance( lv_count ).

    lo_timer = Lcl_abapgit_timer=>create(
      iv_text  = 'Serialize:'
      iv_count = lv_count )->start( ).

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.

      li_progress->show(
        iv_current = sy-tabix
        iv_text    = |Serialize { <ls_tadir>-obj_name }, { lv_max } threads| ).

      IF lv_max = 1.
        run_sequential( <ls_tadir> ).
      ELSE.
        run_parallel(
          is_tadir = <ls_tadir>
          iv_task  = |{ sy-tabix }| ).
        WAIT UNTIL mv_free > 0 UP TO 120 SECONDS.
      ENDIF.
    ENDLOOP.

    li_progress->off( ).

    WAIT UNTIL mv_free = lv_max UP TO 120 SECONDS.
    rt_files = mt_files.
    FREE mt_files.

*   Call postprocessing
    li_exit = Lcl_abapgit_exit=>get_instance( ).

    li_exit->serialize_postprocess(
      EXPORTING
        iv_package = iv_package
        ii_log     = ii_log
      CHANGING
        ct_files   = rt_files ).

    lo_timer->end( abap_true ).

  ENDMETHOD.
  METHOD determine_i18n_params.

    " TODO: unify with ZCL_ABAPGIT_OBJECTS=>DETERMINE_I18N_PARAMS, same code

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
  METHOD determine_max_processes.
    DATA: li_exit TYPE REF TO Lif_abapgit_exit.

    IF iv_force_sequential = abap_true.
      rv_processes = 1.
      RETURN.
    ENDIF.

    IF gv_max_processes IS INITIAL AND is_parallelization_possible( ) = abap_true.

      gv_max_processes = Lcl_abapgit_factory=>get_environment( )->init_parallel_processing( mv_group ).

      IF gv_max_processes > 1.
        gv_max_processes = gv_max_processes - 1.
      ENDIF.

      IF gv_max_processes > 32.
        " https://en.wikipedia.org/wiki/Amdahl%27s_law
        gv_max_processes = 32.
      ENDIF.

    ENDIF.

    IF gv_max_processes IS INITIAL.
      " fallback to running sequentially.
      gv_max_processes = 1.
    ENDIF.

    rv_processes = gv_max_processes.

    ASSERT rv_processes >= 1.

    li_exit = Lcl_abapgit_exit=>get_instance( ).
    li_exit->change_max_parallel_processes(
      EXPORTING
        iv_package       = iv_package
      CHANGING
        cv_max_processes = rv_processes ).

    ASSERT rv_processes >= 1. " check exit above

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SERIALIZE implementation

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
        AND cinfo = lc_report_tcode_hex
      ORDER BY tcode.

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

    SELECT * FROM sotr_head INTO TABLE lt_sotr_head WHERE paket = iv_package ORDER BY PRIMARY KEY.

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
    SELECT * FROM sotr_head INTO TABLE lt_sotr_head WHERE paket = iv_package ORDER BY PRIMARY KEY.
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
    FIELD-SYMBOLS <lv_trkorr> LIKE LINE OF it_trkorr.

    LOOP AT it_trkorr ASSIGNING <lv_trkorr>.
      CALL FUNCTION 'TR_READ_REQUEST_WITH_TASKS'
        EXPORTING
          iv_trkorr     = <lv_trkorr>
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
    DATA lt_requests TYPE trwbo_requests.
    DATA lt_trkorr   TYPE ty_trkorr_tt.


    IF iv_trkorr IS INITIAL.
      RETURN.
    ENDIF.

    INSERT iv_trkorr INTO TABLE lt_trkorr.

    lt_requests = read_requests( lt_trkorr ).
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
          lt_trkorr         TYPE ty_trkorr_tt,
          lv_trkorr         TYPE trkorr.


    IF is_trkorr IS SUPPLIED.
      APPEND is_trkorr-trkorr TO lt_trkorr.
    ELSE.
      lv_trkorr = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_select_transport( ).
      IF lv_trkorr IS NOT INITIAL.
        APPEND lv_trkorr TO lt_trkorr.
      ENDIF.
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

*>>>>>>> ZCL_ABAPGIT_UI_INJECTOR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ui_injector=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ui_injector=======ccimp.
CLASS SHRITEFUH64VYIPN5I4UHL45BPSR4A DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_gui_services.
    CLASS-METHODS create
      RETURNING
        VALUE(ro_instance) TYPE REF TO SHRITEFUH64VYIPN5I4UHL45BPSR4A.
ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BPSR4A IMPLEMENTATION.
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

    ri_gui_services = SHRITEFUH64VYIPN5I4UHL45BPSR4A=>create( ).

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
          ls_package              TYPE Lif_abapgit_sap_package=>ty_create,
          lv_new                  TYPE string,
          lv_path                 TYPE string,
          lv_absolute_name        TYPE string,
          lv_folder_logic         TYPE string,
          lt_unique_package_names TYPE HASHED TABLE OF devclass WITH UNIQUE KEY table_line.

    lv_length = strlen( io_dot->get_starting_folder( ) ).
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
