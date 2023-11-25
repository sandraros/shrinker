CLASS zcl_shrinker_abap_scan DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES ty_abap_source_code TYPE zif_shrinker_abap_code_adapter=>ty_abap_source_code .
    TYPES:
      BEGIN OF ty_syntax_check,
        itab  TYPE string_table,
        mess  TYPE string,
        lin   TYPE i,
        wrd   TYPE string,
        prog  TYPE syrepid,
        dir   TYPE trdir,
        incl  TYPE string,
        off   TYPE i,
        mid   TYPE trmsg_key,
        subrc TYPE sysubrc,
      END OF ty_syntax_check .
    TYPES:
      BEGIN OF ty_get_next_lines_of_statement,
        first_line_index TYPE i,
        last_line_index  TYPE i,
        "! Offset of whole_text where the statement starts
        offset           TYPE i,
        "! Length of whole_text corresponding to the statement
        length           TYPE i,
        "! Each line feed character is replaced with one space. May contain characters \r\n which each indicate one linefeed position.
        whole_text       TYPE string,
      END OF ty_get_next_lines_of_statement .

    "! Get the ABAP statement located at a position (line and column) in a given ABAP source code.
    CLASS-METHODS get_abap_statement_at_cursor
      IMPORTING
        !it_source      TYPE zif_shrinker_abap_code_adapter=>ty_abap_source_code
        VALUE(i_linenr) TYPE numeric
        VALUE(i_offset) TYPE numeric DEFAULT 0
      RETURNING
        VALUE(result)   TYPE zif_shrinker_abap_scan=>ty_scan_result .
    "! Get the ABAP statement located at a line of a given ABAP source code.
    CLASS-METHODS get_whole_abap_statement
      IMPORTING
        !line_index       TYPE i
        !abap_source_code TYPE ty_abap_source_code
      RETURNING
        VALUE(result)     TYPE ty_get_next_lines_of_statement .
    CLASS-METHODS syntax_check
      IMPORTING
        !abap_source_code TYPE ty_abap_source_code
      RETURNING
        VALUE(result)     TYPE ty_syntax_check
      RAISING
        zcx_shrinker .
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_SHRINKER_ABAP_SCAN IMPLEMENTATION.


  METHOD get_abap_statement_at_cursor.

    result = lcl_abap_statement_at_cursor=>get( it_source = it_source
                                                i_linenr  = i_linenr
                                                i_offset  = i_offset ).

  ENDMETHOD.


  METHOD get_whole_abap_statement.

    DATA(abap_statement) = get_abap_statement_at_cursor( it_source = abap_source_code
                                                         i_linenr  = line_index ).
    IF abap_statement-stokes IS INITIAL.
      result = VALUE #(
          first_line_index = line_index
          last_line_index  = line_index
          whole_text       = '' ).
    ELSE.
      result = VALUE #(
          LET tabix_last_stokes = lines( abap_statement-stokes )
              whole_text = concat_lines_of(
                            sep   = |\r\n|
                            table = VALUE string_table(
                                ( LINES OF abap_source_code
                                  FROM abap_statement-stokes[ 1 ]-row
                                  TO abap_statement-stokes[ tabix_last_stokes ]-row ) ) )
          IN
          first_line_index = abap_statement-stokes[ 1 ]-row
          last_line_index  = abap_statement-stokes[ tabix_last_stokes ]-row
          whole_text       = whole_text
          offset           = abap_statement-stokes[ 1 ]-col
          length           = strlen( whole_text ) - abap_statement-stokes[ 1 ]-col ).
    ENDIF.

  ENDMETHOD.


  METHOD syntax_check.

    DATA(synt) = VALUE ty_syntax_check( dir = VALUE #( name = '$$DUMMY' subc = '1' fixpt = 'X' uccheck = 'X' ) ).
*X   VARCL *S   DBAPL *D$  DBNA
    SYNTAX-CHECK FOR abap_source_code MESSAGE synt-mess LINE synt-lin WORD synt-wrd DIRECTORY ENTRY synt-dir INCLUDE synt-incl OFFSET synt-off MESSAGE-ID synt-mid.
    synt-subrc = sy-subrc.
    " SYNTAX-CHECK FOR itab MESSAGE mess LINE lin WORD wrd
    "                  [PROGRAM prog] [DIRECTORY ENTRY dir]
    "                  [WITH CURRENT SWITCHSTATES]
    " ... [INCLUDE incl]
    "     [OFFSET off]
    "     [MESSAGE-ID mid] ...

    result = synt.

  ENDMETHOD.
ENDCLASS.
