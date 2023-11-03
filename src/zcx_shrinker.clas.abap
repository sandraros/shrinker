CLASS zcx_shrinker DEFINITION
  INHERITING FROM cx_static_check
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS zcx_shrinker TYPE sotr_conc VALUE '1D4817BF20AF1EEE9BA04AEA659C5911'.

    METHODS constructor
      IMPORTING
        text                    TYPE clike OPTIONAL
        msgv1                   TYPE clike OPTIONAL
        msgv2                   TYPE clike OPTIONAL
        msgv3                   TYPE clike OPTIONAL
        msgv4                   TYPE clike OPTIONAL
        textid                  LIKE textid DEFAULT zcx_shrinker
        previous                TYPE REF TO cx_root OPTIONAL
        substitute_placeholders TYPE abap_bool DEFAULT abap_true.

    METHODS get_text REDEFINITION.

    METHODS get_longtext REDEFINITION.

  PRIVATE SECTION.

    DATA text TYPE string.
    DATA msgv1 TYPE string.
    DATA msgv2 TYPE string.
    DATA msgv3 TYPE string.
    DATA msgv4 TYPE string.

ENDCLASS.


CLASS zcx_shrinker IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( textid   = textid
                        previous = previous ).
    me->text = text.
    me->msgv1 = msgv1.
    me->msgv2 = msgv2.
    me->msgv3 = msgv3.
    me->msgv4 = msgv4.
    IF me->text IS NOT INITIAL.
      IF substitute_placeholders = abap_true.
        me->text = replace( val = me->text sub = '&1' with = msgv1 ).
        me->text = replace( val = me->text sub = '&2' with = msgv2 ).
        me->text = replace( val = me->text sub = '&3' with = msgv3 ).
        me->text = replace( val = me->text sub = '&4' with = msgv4 ).
      ENDIF.
    ELSE.
      me->text = 'General error, please contact the support.'(001).
    ENDIF.
  ENDMETHOD.


  METHOD get_text.
    result = text.
  ENDMETHOD.


  METHOD get_longtext.
    result = get_text( ).
  ENDMETHOD.

ENDCLASS.

