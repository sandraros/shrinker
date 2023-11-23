CLASS zcl_shrinker_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS abap_message_for_exception
      IMPORTING
        exception    TYPE REF TO cx_root
        type         TYPE symsgty DEFAULT 'E'
        display_like TYPE symsgty OPTIONAL.

    CLASS-METHODS abap_message_i_for_exception
      IMPORTING
        exception TYPE REF TO cx_root.

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

ENDCLASS.
