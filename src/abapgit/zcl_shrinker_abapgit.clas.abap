CLASS zcl_shrinker_abapgit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA package_path TYPE string READ-ONLY.

    CLASS-METHODS create
      IMPORTING
        package       TYPE devclass
        user_exit     TYPE REF TO zif_shrinker_abapgit_user_exit OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_shrinker_abapgit
      RAISING
        zcx_shrinker.

    METHODS deserialize
      RAISING
        zcx_shrinker.

    METHODS file_download
      IMPORTING
        !iv_path TYPE string
        !iv_xstr TYPE xstring
      RAISING
        zcx_shrinker .

    METHODS get_zip
      RETURNING
        VALUE(result) TYPE REF TO cl_abap_zip.

    METHODS serialize
      RAISING
        zcx_shrinker.

    METHODS zip_replace
      IMPORTING
        file_path TYPE string
        content   TYPE xstring
      RAISING
        zcx_shrinker.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA zip TYPE REF TO cl_abap_zip.
    DATA git_repository TYPE REF TO lif_abapgit_repo.
    DATA dot_abapgit TYPE REF TO lcl_abapgit_dot_abapgit.
    DATA user_exit TYPE REF TO zif_shrinker_abapgit_user_exit.

    METHODS handle_exception
      IMPORTING
        exception TYPE REF TO cx_root
      RAISING
        zcx_shrinker.

ENDCLASS.



CLASS zcl_shrinker_abapgit IMPLEMENTATION.

  METHOD create.

    TRY.

        result = NEW zcl_shrinker_abapgit( ).

        lcl_abapgit_repo_srv=>get_instance( )->get_repo_from_package(
          EXPORTING
            iv_package = package
          IMPORTING
            ei_repo    = result->git_repository ).

        result->dot_abapgit = result->git_repository->get_dot_abapgit( ).
        result->package_path = lcl_abapgit_folder_logic=>get_instance(
                            )->package_to_path( iv_top     = result->git_repository->ms_data-package
                                                io_dot     = result->dot_abapgit
                                                iv_package = package ).
        result->user_exit = user_exit.

      CATCH lcx_abapgit_exception INTO DATA(error).
        RAISE EXCEPTION TYPE zcx_shrinker EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD deserialize.

    TRY.

        DATA(zip_xstring) = zip->save( ).
        DATA(files_remote) = lcl_abapgit_zip=>load( zip_xstring ).

        CAST lcl_abapgit_repo( git_repository )->set_files_remote( files_remote ).

        " Credit: method GUI_DESERIALIZE of lcl_abapgit_SERVICES_REPO
        " Note that the source code units are compared in the method CALCULATE of lcl_abapgit_REPO_STATUS.
        DATA(ls_checks) = git_repository->deserialize_checks( ).

        LOOP AT ls_checks-overwrite REFERENCE INTO DATA(check_overwrite).

          CASE check_overwrite->action.
            WHEN lif_abapgit_objects=>c_deserialize_action-add
               OR lif_abapgit_objects=>c_deserialize_action-update
               OR lif_abapgit_objects=>c_deserialize_action-overwrite.
              " ACTION (credit: structured constant lif_abapgit_OBJECTS=>C_DESERIALIZE_ACTION and
              " method WARNING_OVERWRITE_FIND of lcl_abapgit_OBJECTS_CHECK):
              "   1:
              "   2: update local object
              "   3: overwrite local object
              "   4: delete local object
              IF user_exit IS BOUND
                AND user_exit->is_to_be_deserialized( object   = check_overwrite->obj_type
                                                      obj_name = check_overwrite->obj_name ).
                check_overwrite->decision = lif_abapgit_definitions=>c_yes.
              ELSE.
                check_overwrite->decision = lif_abapgit_definitions=>c_no.
              ENDIF.
            WHEN OTHERS.
              check_overwrite->decision = lif_abapgit_definitions=>c_no.
          ENDCASE.
        ENDLOOP.

        DATA(li_log) = CAST lif_abapgit_log( NEW lcl_abapgit_log( ) ).
        " NB: REPO->DESERIALIZE does a COMMIT WORK.
        git_repository->deserialize( is_checks = ls_checks
                                     ii_log    = li_log ).

        IF li_log->get_log_level( ) = li_log->c_log_level-error.
          lcl_abapgit_log_viewer=>show_log( li_log ).
        ENDIF.

      CATCH cx_root INTO DATA(error).
        handle_exception( error ).
    ENDTRY.

  ENDMETHOD.


  METHOD file_download.

    TRY.

        DATA(li_fe_serv) = lcl_abapgit_ui_factory=>get_frontend_services( ).

        li_fe_serv->file_download(
          iv_path = iv_path
          iv_xstr = iv_xstr ).

      CATCH lcx_abapgit_exception INTO DATA(error).
        RAISE EXCEPTION TYPE zcx_shrinker EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD get_zip.

    DATA(zip_xstring) = zip->save( ).

    result = NEW cl_abap_zip( ).
    result->load( zip_xstring ).

  ENDMETHOD.


  METHOD handle_exception.

    TRY.
        DATA(abapgit_error) = CAST lcx_abapgit_exception( exception ).
        DATA(triggering_location) = REF #( abapgit_error->mt_callstack[ 1 ] OPTIONAL ).
        RAISE EXCEPTION TYPE zcx_shrinker
            EXPORTING
                previous = exception
                text     = |Error raised by program { triggering_location->mainprogram }, include { triggering_location->include
                           }, line { triggering_location->line }, { triggering_location->blocktype
                           } { triggering_location->blockname }|.
      CATCH cx_sy_move_cast_error.
        RAISE EXCEPTION TYPE zcx_shrinker EXPORTING previous = exception.
    ENDTRY.
  ENDMETHOD.


  METHOD serialize.

    TRY.

        "==========================================
        " Serialize abap2xlsx files
        "==========================================
        git_repository->refresh( ).

        DATA(files_local) = git_repository->get_files_local( ).

        "==========================================
        " Create a ZIP file of the repository
        " Credit: method ZIP_SERVICES of lcl_abapgit_GUI_ROUTER.
        "==========================================
        DATA(zip_xstring) = lcl_abapgit_zip=>encode_files( files_local ).

*        data(li_fe_serv) = lcl_abapgit_ui_factory=>get_frontend_services( ).
*        li_fe_serv->file_download(
*          iv_path = p_zip
*          iv_xstr = xstring ).

        "==========================================
        " Instantiate a ZIP instance, whose content will be modified
        "==========================================

        zip = NEW cl_abap_zip( ).
        zip->load(
          EXPORTING
            zip             = zip_xstring
          EXCEPTIONS
            zip_parse_error = 1
            OTHERS          = 2 ).
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_shrinker.
        ENDIF.

      CATCH lcx_abapgit_exception INTO DATA(error).
        RAISE EXCEPTION TYPE zcx_shrinker EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD zip_replace.

    IF zip IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_shrinker EXPORTING text = 'Serialize must be called first'.
    ENDIF.

    IF line_exists( zip->files[ name = file_path ] ).
      zip->delete( EXPORTING  name            = file_path
                   EXCEPTIONS zip_index_error = 1
                              OTHERS          = 2 ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_shrinker EXPORTING text = 'Unexpected error, file exists but cannot be deleted'.
      ENDIF.
    ENDIF.

    zip->add( name    = file_path
              content = content ).

  ENDMETHOD.

ENDCLASS.
