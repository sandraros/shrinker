CLASS zcl_shrinker_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES ty_program_name TYPE c LENGTH 30.

    CLASS-METHODS abap_message_for_exception
      IMPORTING
        exception    TYPE REF TO cx_root
        type         TYPE symsgty DEFAULT 'E'
        display_like TYPE symsgty OPTIONAL.

    CLASS-METHODS abap_message_i_for_exception
      IMPORTING
        exception TYPE REF TO cx_root.

    CLASS-METHODS create_empty_prog_batch_input
      IMPORTING
        program_name        TYPE ty_program_name
        program_type        TYPE trdir-subc
        program_description TYPE rs38m-repti
        package_name        TYPE devclass.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_shrinker_utils IMPLEMENTATION.

  METHOD abap_message_for_exception.

    DATA(error_2) = exception.
    DATA(error_texts) = VALUE string_table( ).
    WHILE error_2 IS BOUND.
      INSERT error_2->get_text( ) INTO TABLE error_texts.
      error_2 = error_2->previous.
    ENDWHILE.
    DATA(display_like_2) = COND #( WHEN display_like IS NOT INITIAL THEN type ELSE display_like ).

    MESSAGE concat_lines_of( table = error_texts sep = ` ; ` ) TYPE 'I' DISPLAY LIKE display_like_2.

  ENDMETHOD.


  METHOD abap_message_i_for_exception.

    abap_message_for_exception( exception    = exception
                                type         = 'I'
                                display_like = 'E' ).

  ENDMETHOD.


  METHOD create_empty_prog_batch_input.

    TYPES tt_bdcdata TYPE STANDARD TABLE OF bdcdata WITH EMPTY KEY.
    DATA(bdcdata) = VALUE tt_bdcdata(
        ( program = 'SAPLWBABAP' dynpro = '0100' dynbegin = 'X' )
            ( fnam = 'RS38M-PROGRAMM ' fval = program_name )
            ( fnam = 'RS38M-FUNC_EDIT' fval = 'X' )
            ( fnam = 'BDC_OKCODE'      fval = '=NEW' )
        ( program = 'SAPLSEDTATTR' dynpro = '0200' dynbegin = 'X' )
            ( fnam = 'RS38M-REPTI' fval = program_description )
            ( fnam = 'TRDIR-SUBC'  fval = program_type )
            ( fnam = 'BDC_OKCODE'  fval = '=DROPDOWN_ENTER_SC' ) " Click on button to select program type
        ( program = 'SAPLSEDTATTR' dynpro = '0200' dynbegin = 'X' )
            ( LINES OF COND #( WHEN program_type = '1' THEN VALUE #(
                ( fnam = 'TRDIR-UCCHECK' fval = 'X' ) " ABAP Version/Unicode check. X = classic ABAP program Unicode-enabled.
                ( fnam = 'TRDIR-FIXPT'   fval = 'X' ) ) ) )" Fixed decimal point numbers. X = Yes.
            ( fnam = 'BDC_OKCODE' fval = '=CONT' )
        ( program = 'SAPLSTRD' dynpro = '0100' dynbegin = 'X' )
            ( fnam = 'KO007-L_DEVCLASS' fval = package_name ) " Package name
            ( fnam = 'BDC_OKCODE' fval = '=ADD' )
        ( program = 'SAPLS38E' dynpro = '0400' dynbegin = 'X' )
            ( fnam = 'BDC_OKCODE' fval = '/EWB_BACK' )
        ( program = 'SAPLWBABAP' dynpro = '0100' dynbegin = 'X' )
            ( fnam = 'BDC_OKCODE' fval = '=BACK' )
            ).

    DATA(ctu_params) = VALUE ctu_params( dismode = 'E' defsize = 'X' ).

    CALL TRANSACTION 'SE38'
        USING bdcdata
        OPTIONS FROM ctu_params.

  ENDMETHOD.

ENDCLASS.
