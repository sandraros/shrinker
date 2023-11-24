*&---------------------------------------------------------------------*
*& Report zshrinker_create_obj_abapgit
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zshrinker_create_obj_abapgit.


TYPES ty_program_name TYPE c LENGTH 30.


SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_devc VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_devc TYPE devclass DEFAULT '$SHRINKER_ABAPGIT'.
SELECTION-SCREEN END OF LINE.

" Program description prefix (37 out of 70 characters / RS38M-REPTI / Last 33 characters reserved)
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_repti VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_repti TYPE c LENGTH 37 LOWER CASE DEFAULT 'Shrinked version of abapGit'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_def VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_def TYPE ty_program_name DEFAULT 'ZSHRINKER_ABAPGIT_DEF'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_nb_imp VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_nb_imp TYPE i DEFAULT 5.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_imp VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_imp TYPE ty_program_name DEFAULT 'ZSHRINKER_ABAPGIT_IMP'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_standa VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_standa TYPE ty_program_name DEFAULT 'ZSHRINKER_ABAPGIT_STANDALONE'.
SELECTION-SCREEN END OF LINE.


CLASS lcl_app DEFINITION DEFERRED.
DATA app TYPE REF TO lcl_app.


LOAD-OF-PROGRAM.
  CALL METHOD lcl_app=>('CREATE')
    RECEIVING
      result = app.


INITIALIZATION.
  t_devc = 'Package assigned to the created include programs'(t01).
  t_repti = 'Prefix of program descriptions'(t06).
  t_def = 'Include for class and interface definitions'(t02).
  t_nb_imp = 'Number of includes for class implementations'(t05).
  t_imp = 'Prefix of includes for class implementations'(t03).
  t_standa = 'Main abapGit executable program (~ZABAPGIT_STANDALONE)'(t04).


START-OF-SELECTION.
  TRY.
      CALL METHOD app->('START_OF_SELECTION').
    CATCH cx_root INTO DATA(error).
      MESSAGE error TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.


CLASS lcl_app DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-METHODS create
      RETURNING
        VALUE(result) TYPE REF TO lcl_app.

    METHODS start_of_selection.

  PRIVATE SECTION.

ENDCLASS.


CLASS lcl_app IMPLEMENTATION.

  METHOD create.

    result = NEW lcl_app( ).

  ENDMETHOD.


  METHOD start_of_selection.

    TYPES ty_suffix TYPE c LENGTH 37.

    zcl_shrinker_utils=>create_empty_prog_batch_input(
                    program_name        = p_def
                    program_type        = 'I'
                    program_description = p_repti && '-Class/Interface pool definitions'
                    package_name        = p_devc ).

    DO p_nb_imp TIMES.

      DATA(include_name_suffix) = |{ sy-index }|.

      zcl_shrinker_utils=>create_empty_prog_batch_input(
                      program_name        = p_imp && include_name_suffix
                      program_type        = 'I'
                      program_description = p_repti && |-Class implementations part { include_name_suffix }/{ p_nb_imp }|
                      package_name        = p_devc ).

    ENDDO.

    IF p_standa IS NOT INITIAL.
      zcl_shrinker_utils=>create_empty_prog_batch_input(
                      program_name        = p_standa
                      program_type        = '1'
                      program_description = p_repti && `-Approx. program ZABAPGIT_STANDALONE`
                      package_name        = p_devc ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
