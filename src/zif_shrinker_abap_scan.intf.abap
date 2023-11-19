interface ZIF_SHRINKER_ABAP_SCAN
  public .

    CONSTANTS:
      BEGIN OF c_pseudo_token,
        colon TYPE token_type VALUE ':',
        comma TYPE token_type VALUE ',',
        dot   TYPE token_type VALUE '.',
        std   LIKE scan_token_type VALUE scan_token_type,
      END OF c_pseudo_token.
    CONSTANTS:
      BEGIN OF type,
        level        LIKE scan_level_type VALUE scan_level_type,
        struc        LIKE scan_struc_type VALUE scan_struc_type,
        struc_stmnt  LIKE scan_struc_stmnt_type VALUE scan_struc_stmnt_type,
        stmnt        LIKE scan_stmnt_type VALUE scan_stmnt_type,
        token        LIKE scan_token_type VALUE scan_token_type,
        pseudo_token LIKE c_pseudo_token VALUE c_pseudo_token,
      END OF type.
    TYPES:
      BEGIN OF ty_slevel,
        depth TYPE level_dpth,
        level TYPE level_levl,
        stmnt TYPE level_stmt,
        from  TYPE level_from,
        to    TYPE level_to,
        name  TYPE level_name,
        type  TYPE level_type,
      END OF ty_slevel.
    TYPES:
      BEGIN OF ty_sstruc,
        type       TYPE stru_type,
        stmnt_type TYPE stru_type,
        key_start  TYPE stru_keyw,
        key_end    TYPE stru_keyw,
        stmnt_from TYPE stru_from1,
        stmnt_to   TYPE stru_to1,
        struc_from TYPE stru_from2,
        struc_to   TYPE stru_to2,
        back       TYPE stru_back,
      END OF ty_sstruc.
    TYPES:
      "! Scan information about one statement
      BEGIN OF ty_sstmnt,
        "! Index of the "LEVEL" source unit which contains this statement
        level      TYPE stmnt_levl,
        "! Index of the "STRUC" block which contains this statement
        struc      TYPE stmnt_stru,
        "! Index of the first token of this statement in the token table (NB: the first token in the token table has index 1)
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
        "! The possible values are defined in the structured constant SCAN_STMNT_TYPE.
        type       TYPE stmnt_type,
        "! Terminator character (period if not a chained statement, comma if it's a chained statement)
        "! or empty for native SQL statements and internal macro definitions
        terminator TYPE stmnt_term,
        "! Index in the enhancement table of type SENHMT, if the statement was enhanced or originates completely from
        "! an enhancement implementation. If addition ENHANCEMENTS INTO itab is not specified, this value is always 0.
        enhmt      TYPE i,
      END OF ty_sstmnt.
    TYPES:
      BEGIN OF ty_stokes,
        str  TYPE string,
        row  TYPE token_row,
        col  TYPE token_col,
        "! The possible values are defined in the structured constant SCAN_TOKEN_TYPE.
        type TYPE token_type,
      END OF ty_stokes.

    TYPES ty_ut_slevel TYPE STANDARD TABLE OF ty_slevel WITH EMPTY KEY.
    TYPES ty_ut_sstruc TYPE STANDARD TABLE OF ty_sstruc WITH EMPTY KEY.
    TYPES ty_ut_stokes TYPE STANDARD TABLE OF ty_stokes WITH EMPTY KEY.
    TYPES ty_ut_sstmnt TYPE STANDARD TABLE OF ty_sstmnt WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_scan_result,
        slevel TYPE ty_ut_slevel,
        sstruc TYPE ty_ut_sstruc,
        sstmnt TYPE ty_ut_sstmnt,
        stokes TYPE ty_ut_stokes,
      END OF ty_scan_result.

endinterface.
