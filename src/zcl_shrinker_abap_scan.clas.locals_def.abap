*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS ltc_raw_scan_lines_around DEFINITION DEFERRED.
CLASS ltc_scan_abap_statement DEFINITION DEFERRED.
CLASS ltc_scan_abap_parse_line DEFINITION DEFERRED.


*----------------------------------------------------------------------*
*       CLASS lcl_program_load DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_program_load DEFINITION.
  PUBLIC SECTION.
    TYPES ty_t_rng_prog TYPE RANGE OF repoload-progname.
    TYPES ty_t_rng_udat TYPE RANGE OF repoload-udat.
    TYPES ty_t_rng_sdat TYPE RANGE OF repoload-sdat.
    TYPES : BEGIN OF ty_s_load_status,
              progname    TYPE repoload-progname,
              sdat        TYPE repoload-sdat, "last time regen was requested
              stime       TYPE repoload-stime,
              udat        TYPE repoload-udat, "last regen time
              utime       TYPE repoload-utime,
              stat        TYPE string,
              gen_result  TYPE sysubrc,
              gen_message TYPE string,
            END OF ty_s_load_status.
    TYPES ty_t_load_status TYPE TABLE OF ty_s_load_status WITH DEFAULT KEY.
    DATA ku_program TYPE string READ-ONLY.
    DATA ku_date    TYPE sy-datum READ-ONLY.
    DATA ku_time    TYPE sy-uzeit READ-ONLY.

    "!
    "! @parameter it_rng_prog | Range of main programs (including classes, etc.)
    "! @parameter it_rng_udat | Last DDIC modification date
    "! @parameter it_rng_sdat | Last generation date
    "! @parameter i_nevergen | <ul>
    "! <li>True = include the programs which have never been generated</li>
    "! <li>False = exclude the programs which have never been generated</li>
    "! </ul>
    METHODS select_prog
      IMPORTING
        it_rng_prog TYPE ty_t_rng_prog OPTIONAL
        it_rng_udat TYPE ty_t_rng_udat OPTIONAL
        it_rng_sdat TYPE ty_t_rng_sdat OPTIONAL
        i_nevergen  TYPE flag DEFAULT abap_true.

    METHODS get_progs
      RETURNING
        VALUE(et_load_status) TYPE ty_t_load_status.

    METHODS get_number_of_progs
      RETURNING
        VALUE(e_number) TYPE i.

    "!
    "! @parameter nevergen | <ul>
    "! <li>false : remove the programs which have never been generated</li>
    "! </ul>
    "! @parameter toberege | <ul>
    "! <li>false : remove the programs which have to be generated</li>
    "! </ul>
    "! @parameter generatd | <ul>
    "! <li>false : remove the programs which don't need to be regenerated</li>
    "! </ul>
    METHODS filter_prog
      IMPORTING
        nevergen TYPE flag DEFAULT abap_true
        toberege TYPE flag DEFAULT abap_true
        generatd TYPE flag DEFAULT abap_false.

    METHODS gen_progs
      IMPORTING
        i_commit_frequency TYPE i.

    METHODS invalidate
      IMPORTING
        i_test TYPE flag.

    EVENTS program_generated
      EXPORTING
        VALUE(progname) TYPE progname
        VALUE(subrc)    TYPE sysubrc
        VALUE(counter)  TYPE i
        VALUE(total)    TYPE i.

  PRIVATE SECTION.

    DATA kit_load_status TYPE ty_t_load_status.

ENDCLASS.


CLASS lcl_abap_statement_at_cursor DEFINITION
    FRIENDS ltc_raw_scan_lines_around
            ltc_scan_abap_statement
            ltc_scan_abap_parse_line.

  PUBLIC SECTION.

    "! Return the statement at cursor position. If the cursor is before the colon of a chained
    "! statement, it will return several statements (all the ones of the chained statement).
    CLASS-METHODS get
      IMPORTING
        it_source       TYPE zcl_shrinker_abap_scan=>ty_abap_source_code
        VALUE(i_linenr) TYPE numeric
        VALUE(i_offset) TYPE numeric DEFAULT 0
      RETURNING
        VALUE(result)   TYPE zcl_shrinker_abap_scan=>ty_scan_result.

  PRIVATE SECTION.

    TYPES:
      "! Scan information about one "pseudo-statement" (line scan)
      BEGIN OF ts_pseudo_sstmnt,
        "! Index of the first token of this statement in the token table (NB: the first token in the token table has index 1).
        "! It may be zero if the pseudo-statement has no token at all (empty line, dot alone, etc.)
        from       TYPE stmnt_from,
        "! Index of the last token of this statement in the token table (NB: the first token in the token table has index 1)
        to         TYPE stmnt_to,
        "! Statement number in the LEVEL source unit (NB: the first statement in the LEVEL source unit has index 1)
        number     TYPE stmnt_nr,
        "! Row of the chained statement colon in the LEVEL source unit (>= 1 if TERMINATOR = ',' / 1 = first row, otherwise 0)
        colonrow   TYPE stmnt_crow,
        "! Row of terminator in the LEVEL source unit (>= 1 if TERMINATOR <> SPACE / 1 = first row, otherwise 0)
        trow       TYPE stmnt_trow,
        "! Column of the chained statement colon (>= 0 if TERMINATOR = ',' - 0 = first column, otherwise 0)
        coloncol   TYPE stmnt_ccol,
        "! Column of terminator (>= 0 if TERMINATOR <> SPACE - 0 = first column, otherwise 0)
        tcol       TYPE stmnt_tcol,
        "! Number of tokens before the colon (with chain statements >= 1, otherwise 0)
        prefixlen  TYPE stmnt_plen,
        "! Terminator character (period if not a chained statement, comma if it's a chained statement)
        "! or empty for native SQL statements and internal macro definitions
        terminator TYPE stmnt_term,
        zz_1st_row TYPE i,
        zz_1st_col TYPE i,
      END OF ts_pseudo_sstmnt.
    TYPES:
      "! A pseudo-token may also contain comma, dot and colon.
      BEGIN OF ty_pseudo_token,
        str  TYPE string,
        row  TYPE token_row,
        col  TYPE token_col,
        "! The possible values are defined in the structured constant C_PSEUDO_TOKEN.
        type TYPE token_type,
      END OF ty_pseudo_token.
    TYPES ty_pseudo_tokens TYPE STANDARD TABLE OF ty_pseudo_token WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_line_scan,
        linenr        TYPE i,
        pseudo_tokens TYPE ty_pseudo_tokens,
      END OF ty_line_scan.
    TYPES ty_ref_line_scan TYPE REF TO ty_line_scan.
    TYPES:
      BEGIN OF ty_parsed_line_statement,
        parsed_linenr   TYPE i,
        ref_parsed_line TYPE ty_ref_line_scan,
        tabix_sstmnt    TYPE sytabix,
      END OF ty_parsed_line_statement.
    TYPES:
      BEGIN OF ty_raw_scan_lines_around,
        pseudo_tokens TYPE ty_pseudo_tokens,
      END OF ty_raw_scan_lines_around.

    CONSTANTS type LIKE zcl_shrinker_abap_scan=>type VALUE zcl_shrinker_abap_scan=>type.

    CLASS-METHODS parse_line
      IMPORTING
        i_line        TYPE csequence
        i_linenr      TYPE numeric DEFAULT 0
      RETURNING
        VALUE(result) TYPE ty_line_scan.

    "!
    "! @parameter it_source | X
    "! @parameter i_linenr | X
    "! @parameter i_offset | X
    "! @parameter result | NB: because RESULT contains references to self-contained data, to avoid these references to be FREED, it was required to:
    "!                          <ul>
    "!                          <li>EITHER define it as a data reference, i.e. create it via CREATE DATA,</li>
    "!                          <li>OR not pass it by value, i.e. use EXPORTING instead of RETURNING.</li>
    "!                          </ul>
    CLASS-METHODS raw_scan_lines_around
      IMPORTING
        it_source       TYPE zcl_shrinker_abap_scan=>ty_abap_source_code
        VALUE(i_linenr) TYPE numeric
        VALUE(i_offset) TYPE numeric DEFAULT 0
      EXPORTING
        result          TYPE ty_raw_scan_lines_around.

    "! Input is raw lines around the cursor, output is only one statement or several if cursor is before the colon ":" of a chained statement.
    "! @parameter raw_scan_lines_around | Raw lines, may contain several statements
    "! @parameter i_linenr | Cursor row
    "! @parameter i_offset | Cursor column
    "! @parameter result | In general only one statement is returned
    CLASS-METHODS rework_raw_scan_lines
      IMPORTING
        raw_scan_lines_around TYPE ty_raw_scan_lines_around
        i_linenr              TYPE numeric
        i_offset              TYPE numeric
        remove_comments       TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(result)         TYPE zcl_shrinker_abap_scan=>ty_scan_result.

ENDCLASS.


class lcl_uuid definition.

  PUBLIC SECTION.


    CLASS-DATA uuid_generator TYPE REF TO if_system_uuid.


  class-methods class_constructor.

  class-methods get_c26
  RETURNING
  VALUE(result) type sysuuid_c26.

  class-methods get_x16
  RETURNING
  VALUE(result) type sysuuid_x16.

endclass.
