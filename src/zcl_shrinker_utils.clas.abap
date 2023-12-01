CLASS zcl_shrinker_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES ty_program_name TYPE c LENGTH 30.
    TYPES ty_extension_code TYPE c LENGTH 10.
    TYPES:
      BEGIN OF ty_program_name_parts,
        main           TYPE ty_program_name,
        extension_code TYPE ty_extension_code,
      END OF ty_program_name_parts.
    TYPES:
      BEGIN OF ty_tadir_object,
        object   TYPE trobjtype,
        obj_name TYPE sobj_name,
      END OF ty_tadir_object.

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

    CLASS-METHODS get_class_include_name
      IMPORTING
        class_name     TYPE seoclsname
        extension_code TYPE ty_extension_code
      RETURNING
        VALUE(result)  TYPE string.

    CLASS-METHODS get_equivalent_method_includes
      IMPORTING
        class_pool_name TYPE seoclsname
      RETURNING
        VALUE(result)   TYPE string_table.

    CLASS-METHODS get_main_program_name
      IMPORTING
        object        TYPE trobjtype
        obj_name      TYPE sobj_name
      RETURNING
        VALUE(result) TYPE syrepid.

    CLASS-METHODS get_program_as_tadir_object
      IMPORTING
        main_program_name TYPE syrepid
      RETURNING
        VALUE(result)     TYPE ty_tadir_object.

    "! <p class="shorttext synchronized" lang="en"></p>Utility method same as SPLIT ABAP statement but a POSIX Regular
    "! Expression can be used instead of a simple text (AT word in SPLIT). Example: <br/>
    "! <strong>ASSERT SPLIT_AT_REGEX( val = &#124;A\nB\r\nC&#124; regex = &#124;\r\n\&#124;\n&#124; )<br/>
    "! = VALUE string_table( ( `A` ) ( `B` ) ( `C` ) ).</strong>
    "!
    "! @parameter val | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter regex | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter max_splits | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS split_at_regex
      IMPORTING
        val           TYPE csequence
        regex         TYPE csequence
        max_splits    TYPE i DEFAULT 0
      RETURNING
        VALUE(result) TYPE string_table .

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS convert_methodindx_to_extensio
      IMPORTING
        !methodindx   TYPE tmdir-methodindx
      RETURNING
        VALUE(result) TYPE ty_extension_code .

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


  METHOD convert_methodindx_to_extensio.
    CONSTANTS base TYPE c LENGTH 36 VALUE '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'.

    result = ''.
    DATA(methodindx_2) = methodindx.
    DO 3 TIMES.
      DATA(indx) = CONV i( methodindx_2 ) MOD 36.
      result = base+indx(1) && result.
      methodindx_2 = methodindx_2 DIV 36.
    ENDDO.
    result = 'CM' && result.
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


  METHOD get_class_include_name.

    result = |{ class_name WIDTH = 30 PAD = '=' }| && extension_code.

  ENDMETHOD.


  METHOD get_equivalent_method_includes.

    SELECT tmdir~classname, tmdir~methodindx, tmdir~methodname
        FROM tmdir
        WHERE tmdir~classname = @class_pool_name
          AND tmdir~methodname <> ''
        INTO TABLE @DATA(class_methods).

    result = VALUE #(
            FOR <method> IN class_methods
            ( |INCLUDE { zcl_shrinker_utils=>get_class_include_name(
                           class_name     = <method>-classname
                           extension_code = convert_methodindx_to_extensio( <method>-methodindx ) )
                       }. "{ <method>-methodname }| ) ).

  ENDMETHOD.


  METHOD get_main_program_name.

    result  = COND #(
      WHEN ( object = 'FUGR' OR object = 'FUGS' )
               AND obj_name CP '/*' THEN
           '/' && replace( val  = substring( val = obj_name
                                             off = 1 )
                           sub  = '/'
                           with = '/SAPL' )
      WHEN object = 'FUGR' OR object = 'FUGS' THEN
           'SAPL' && obj_name
      WHEN object = 'CLAS' THEN
           get_class_include_name( class_name     = CONV #( obj_name )
                                   extension_code = 'CP' )
      WHEN object = 'INTF' THEN
           |{ obj_name WIDTH = 30 PAD = '=' }|
           && 'IP'
      WHEN object = 'CNTX' THEN
           'CONTEXT_X_' && obj_name
      ELSE " PROG
           obj_name ).

  ENDMETHOD.


  METHOD get_program_as_tadir_object.

    DATA(program_name_parts) = CONV ty_program_name_parts( main_program_name ).
    TRANSLATE program_name_parts-main USING '= '.

    result  = COND #(
      WHEN program_name_parts-extension_code = 'CP' THEN
           VALUE #( object   = 'CLAS'
                    obj_name = program_name_parts-main )
      WHEN program_name_parts-extension_code = 'IP' THEN
           VALUE #( object   = 'INTF'
                    obj_name = program_name_parts-main )
      WHEN program_name_parts-main CP 'CONTEXT_X_*' THEN
           VALUE #( object   = 'CNTX'
                    obj_name = substring( val = program_name_parts-main
                                          off = 10 ) )
      WHEN program_name_parts-main CP 'SAPL*' THEN
           VALUE #( object   = 'FUGR'
                    obj_name = substring( val = program_name_parts-main
                                          off = 4 ) )
      WHEN program_name_parts-main CP '/*/SAPL*' THEN
           VALUE #( object   = 'FUGR'
                    obj_name = CONV sobj_name( LET offset_second_slash = 1 + find( val = substring( val = program_name_parts-main
                                                                                                    off = 1 )
                                                                                   sub = '/' )
                                               IN substring( val = program_name_parts-main
                                                             off = 0
                                                             len = offset_second_slash + 1 )
                                               && substring( val = program_name_parts-main
                                                             off = offset_second_slash + 5 ) ) )
      ELSE
           VALUE #( object   = 'PROG'
                    obj_name = program_name_parts-main ) ).

  ENDMETHOD.


  METHOD split_at_regex.

    FIND ALL OCCURRENCES OF REGEX regex IN val RESULTS DATA(matches) ##REGEX_POSIX.

    " 0 match means 1 segment (split 'ab' at ':' -> 0 match and result is 'ab')
    IF matches IS INITIAL.
      result = VALUE #( ( CONV #( val ) ) ).
    ELSE.
      " 1 match means 2 segments (split 'a:b' at ':' -> 1 match and result is 'a' and 'b'),
      " 2 matches means 3 segments,
      " etc.
      IF max_splits >= 1 AND lines( matches ) >= max_splits.
        DELETE matches FROM max_splits.
      ENDIF.
      DATA(offset) = 0.
      LOOP AT matches ASSIGNING FIELD-SYMBOL(<match>).
        DATA(length) = <match>-offset - offset.
        APPEND substring( val = val off = offset len = length ) TO result.
        offset = <match>-offset + <match>-length.
      ENDLOOP.
      APPEND substring( val = val off = offset len = strlen( val ) - offset ) TO result.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
