********************************************************************************
*
* LICENSE and NOTICE
*
* See include program ZSHRINKER_LICENSE
*
********************************************************************************
CLASS LCL_SHRINKER DEFINITION DEFERRED.
CLASS LCX_SHRINKER DEFINITION DEFERRED.
INTERFACE LIF_SHRINKER_ABAP_CODE_ADAPTER DEFERRED.
*class-pool .
*"* class pool for class ZCX_SHRINKER

*"* local type definitions
*include zcx_shrinker==================ccdef.
*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section


*"* class ZCX_SHRINKER definition
*"* public declarations
*include zcx_shrinker==================cu.
CLASS Lcx_shrinker DEFINITION
  INHERITING FROM cx_static_check
*public
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS Lcx_shrinker TYPE sotr_conc VALUE '1D4817BF20AF1EEE9BA04AEA659C5911'.

    METHODS constructor
      IMPORTING
        text                    TYPE clike OPTIONAL
        msgv1                   TYPE clike OPTIONAL
        msgv2                   TYPE clike OPTIONAL
        msgv3                   TYPE clike OPTIONAL
        msgv4                   TYPE clike OPTIONAL
        textid                  LIKE textid DEFAULT Lcx_shrinker
        previous                TYPE REF TO cx_root OPTIONAL
        substitute_placeholders TYPE abap_bool DEFAULT abap_true.

    METHODS get_text REDEFINITION.

    METHODS get_longtext REDEFINITION.

*"* protected declarations
*include zcx_shrinker==================co.

*"* private declarations
*include zcx_shrinker==================ci.
  PRIVATE SECTION.

    DATA text TYPE string.
    DATA msgv1 TYPE string.
    DATA msgv2 TYPE string.
    DATA msgv3 TYPE string.
    DATA msgv4 TYPE string.

endclass. "ZCX_SHRINKER definition
*"* class pool for interface ZIF_SHRINKER_ABAP_CODE_ADAPTER

INTERFACE Lif_shrinker_abap_code_adapter
   .

  TYPES ty_abap_source_code TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  TYPES ty_extension_code TYPE c LENGTH 10.
  TYPES:
    BEGIN OF ty_oo_include,
      include_name     TYPE syrepid,
      extension_code   TYPE ty_extension_code,
      method_name      TYPE seocpdname,
      abap_source_code TYPE ty_abap_source_code,
    END OF ty_oo_include.
  TYPES ty_oo_includes TYPE STANDARD TABLE OF ty_oo_include WITH EMPTY KEY.
  TYPES:
    BEGIN OF ty_interface,
      name     TYPE seoclsname,
      includes TYPE ty_oo_includes,
    END OF ty_interface.
  TYPES ty_interfaces TYPE STANDARD TABLE OF ty_interface WITH EMPTY KEY.
  TYPES:
    BEGIN OF ty_class,
      name      TYPE seoclassdf-clsname,
      clsccincl TYPE seoclassdf-clsccincl,
      includes  TYPE ty_oo_includes,
    END OF ty_class.
  TYPES ty_classes TYPE STANDARD TABLE OF ty_class WITH EMPTY KEY.
  TYPES:
    BEGIN OF ty_read_class_interface_includ,
      classes    TYPE ty_classes,
      interfaces TYPE ty_interfaces,
    END OF ty_read_class_interface_includ.
  TYPES:
    BEGIN OF ty_source_unit,
      name             TYPE syrepid,
      abap_source_code TYPE ty_abap_source_code,
    END OF ty_source_unit.
  TYPES ty_source_units TYPE STANDARD TABLE OF ty_source_unit WITH EMPTY KEY.

  METHODS adapt_source_code
    CHANGING
      classes_interfaces TYPE ty_read_class_interface_includ OPTIONAL
      other_source_units TYPE ty_source_units OPTIONAL.

ENDINTERFACE.
      " Don't remove now the lines CLASS ... DEFINITION DEFERRED because they may
    "   1. all lines "CLASS ... DEFINITION DEFERRED" will be moved at the very top, above
    "║ CLASS ltc_2 DEFINITION DEFERRED.
    "║ CLASS ltc_1 DEFINITION DEFERRED.
    "║ CLASS ltc_2 DEFINITION DEFERRED.
    "║ CLASS ltc_1 DEFINITION DEFERRED.
    " "Components of classes declared using "CLASS ZCL_EXCEL_GRAPH DEFINITION DEFERRED" can only be accessed after the class is defined (CLASS ZCL_EXCEL_GRAPH DEFINITION)."
    "! <li>CLASS lcl_xxx DEFINITION DEFERRED.</li>
*class-pool .
*"* class pool for class ZCL_SHRINKER

*"* local type definitions
*include zcl_shrinker==================ccdef.
*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

*CLASS SHRIS5ZPAUXVKEPN5HWER7BEX2UBRY DEFINITION DEFERRED.
*CLASS SHRIS5ZPAUXVKEPN5HWER7BEX25BRY DEFINITION DEFERRED.
*CLASS SHRIS5ZPAUXVKEPN5HWER7BEX24BRY DEFINITION DEFERRED.


*----------------------------------------------------------------------*
*       CLASS SHRIS5ZPAUXVKEPN5HWER7BEX2WBRY DEFINITION
*----------------------------------------------------------------------*
CLASS SHRIS5ZPAUXVKEPN5HWER7BEX2WBRY DEFINITION.
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


CLASS SHRIS5ZPAUXVKEPN5HWER7BEX2VBRY DEFINITION


            .

  PUBLIC SECTION.

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
    "! Return the statement at cursor position. If the cursor is before the colon of a chained
    "! statement, it will return several statements (all the ones of the chained statement).
    CLASS-METHODS get
      IMPORTING
        it_source       TYPE Lif_shrinker_abap_code_adapter=>ty_abap_source_code
        VALUE(i_linenr) TYPE numeric
        VALUE(i_offset) TYPE numeric DEFAULT 0
      RETURNING
        VALUE(result)   TYPE ty_scan_result.

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
        it_source       TYPE Lif_shrinker_abap_code_adapter=>ty_abap_source_code
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
        VALUE(result)         TYPE ty_scan_result.

ENDCLASS.

*"* class ZCL_SHRINKER definition
*"* public declarations
*include zcl_shrinker==================cu.
CLASS Lcl_shrinker DEFINITION
*public
  FINAL
  CREATE PRIVATE


.

  PUBLIC SECTION.

    TYPES ty_package_range TYPE RANGE OF devclass.
    TYPES ty_range_of_obj_name TYPE RANGE OF tadir-obj_name.
    TYPES ty_range_of_obj_type TYPE RANGE OF tadir-object.
    TYPES ty_abap_source_code TYPE Lif_shrinker_abap_code_adapter=>ty_abap_source_code.
    TYPES:
      BEGIN OF ty_object,
        pgmid    TYPE tadir-pgmid,
        object   TYPE tadir-object,
        obj_name TYPE tadir-obj_name,
      END OF ty_object.
    TYPES ty_objects TYPE STANDARD TABLE OF ty_object WITH EMPTY KEY.
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
      END OF ty_syntax_check.
    TYPES:
      BEGIN OF ty_main_result,
        def_abap_source_code TYPE ty_abap_source_code,
        imp_abap_source_code TYPE ty_abap_source_code,
      END OF ty_main_result.
    TYPES:
      BEGIN OF ty_cc_renaming,
        "! Class pool name
        class_name  TYPE seoclsname,
        "! Old member name (local class, type, etc.) as a REGEX POSIX find expression
        posix_regex TYPE string,
        "! New member name as a REGEX POSIX replace expression
        with        TYPE string,
      END OF ty_cc_renaming.
    TYPES ty_cc_renamings TYPE STANDARD TABLE OF ty_cc_renaming WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_obj_renaming,
        "! Text to find expressed as a POSIX Regular Expression
        posix_regex             TYPE string,
        "! Replacement text expressed as a POSIX Regular Expression
        with                    TYPE string,
        "! Apply on whole ABAP word
        start_of_abap_word      TYPE abap_bool,
        "! replace posix_regex if it's in the place of "interfacename" in "object->interfacename~member"
        "! or "class=>interfacename~member"
        member_interface_prefix TYPE abap_bool,
      END OF ty_obj_renaming.
    TYPES ty_obj_renamings TYPE STANDARD TABLE OF ty_obj_renaming WITH EMPTY KEY.
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
      END OF ty_get_next_lines_of_statement.

    CLASS-METHODS create
      IMPORTING
        customizer    TYPE REF TO Lif_shrinker_abap_code_adapter OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO Lcl_shrinker.

    METHODS get_abap_for_program
      IMPORTING
        program_name        TYPE syrepid
        global_replacements TYPE ty_obj_renamings OPTIONAL
      RETURNING
        VALUE(result)       TYPE ty_abap_source_code
      RAISING
        Lcx_shrinker.

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter package_range | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter range_of_obj_name | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter global_replacements | <p class="shorttext synchronized" lang="en"></p> Do any text replacement e.g. to change
    "! the prefix of shrinked object names from ZCL to LCL, use posix_regex = '\&lt;Z(\w+)' with = 'L$1'.
    "! @parameter class_replacements | <p class="shorttext synchronized" lang="en"></p> Renamings in CCDEF, CCIMP, CCMAC and CCAU includes.
    "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
    METHODS get_one_abap_code
      IMPORTING
        package_range       TYPE ty_package_range OPTIONAL
        objects             TYPE ty_objects OPTIONAL
        range_of_obj_name   TYPE ty_range_of_obj_name OPTIONAL
        range_of_obj_type   TYPE ty_range_of_obj_type OPTIONAL
        global_replacements TYPE ty_obj_renamings OPTIONAL
        class_replacements  TYPE ty_cc_renamings OPTIONAL
        omit_test_classes   TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(result)       TYPE ty_main_result
      RAISING
        Lcx_shrinker.

    "! Get the ABAP statement located at a line of a given ABAP source code.
    CLASS-METHODS get_whole_abap_statement
      IMPORTING
        line_index       TYPE i
        abap_source_code TYPE ty_abap_source_code
      RETURNING
        VALUE(result)    TYPE ty_get_next_lines_of_statement.

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

    CLASS-METHODS syntax_check
      IMPORTING
        abap_source_code TYPE ty_abap_source_code
      RETURNING
        VALUE(result)    TYPE ty_syntax_check
      RAISING
        Lcx_shrinker.

*"* protected declarations
*include zcl_shrinker==================co.

*"* private declarations
*include zcl_shrinker==================ci.
  PRIVATE SECTION.
    TYPES ty_range_of_object_types TYPE RANGE OF tadir-object.
    TYPES:
      BEGIN OF ty_ddic_elementary_type,
        datatype TYPE dd04l-datatype,
        leng     TYPE dd04l-leng,
        decimals TYPE dd04l-decimals,
      END OF ty_ddic_elementary_type.
    TYPES:
      BEGIN OF ty_ddic_data_element,
        rollname TYPE dd04l-rollname,
        domname  TYPE dd04l-domname,
        reftype  TYPE dd04l-reftype.
        INCLUDE TYPE ty_ddic_elementary_type AS elem_info.
      TYPES: END OF ty_ddic_data_element.
    TYPES ty_ddic_data_elements TYPE STANDARD TABLE OF ty_ddic_data_element WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_ddic_structure,
        tabname  TYPE dd02l-tabname,
        tabclass TYPE dd02l-tabclass,
      END OF ty_ddic_structure.
    TYPES ty_ddic_structures TYPE STANDARD TABLE OF ty_ddic_structure WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_ddic_structure_component,
        tabname   TYPE dd03l-tabname,
        fieldname TYPE dd03l-fieldname,
        rollname  TYPE dd03l-rollname,
        precfield TYPE dd03l-precfield,
        comptype  TYPE dd03l-comptype,
        reftype   TYPE dd03l-reftype.
        INCLUDE TYPE ty_ddic_elementary_type AS elem_info.
      TYPES: END OF ty_ddic_structure_component.
    TYPES ty_ddic_structure_components TYPE STANDARD TABLE OF ty_ddic_structure_component WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_ddic_table_type,
        typename   TYPE dd40l-typename,
        rowkind    TYPE dd40l-rowkind,
        reftype    TYPE dd40l-reftype,
        rowtype    TYPE dd40l-rowtype,
        keydef     TYPE dd40l-keydef,
        keykind    TYPE dd40l-keykind,
        ttypkind   TYPE dd40l-ttypkind,
        accessmode TYPE dd40l-accessmode.
        INCLUDE TYPE ty_ddic_elementary_type AS elem_info.
      TYPES: END OF ty_ddic_table_type.
    TYPES ty_ddic_table_types TYPE STANDARD TABLE OF ty_ddic_table_type WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_ddic_table_type_sec_key,
        typename     TYPE dd43l-typename,
        seckeyname   TYPE dd43l-seckeyname,
        seckeyunique TYPE dd43l-seckeyunique,
        accessmode   TYPE dd43l-accessmode,
        kind         TYPE dd43l-kind,
      END OF ty_ddic_table_type_sec_key.
    TYPES ty_ddic_table_type_sec_keys TYPE STANDARD TABLE OF ty_ddic_table_type_sec_key WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_ddic_table_key_component,
        typename   TYPE dd42s-typename,
        seckeyname TYPE dd42s-seckeyname,
        keyfdpos   TYPE dd42s-keyfdpos,
        keyfield   TYPE dd42s-keyfield,
      END OF ty_ddic_table_key_component.
    TYPES ty_ddic_table_key_components TYPE STANDARD TABLE OF ty_ddic_table_key_component WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_ddic_all,
        data_elements             TYPE ty_ddic_data_elements,
        structures                TYPE ty_ddic_structures,
        structure_components      TYPE ty_ddic_structure_components,
        table_types               TYPE ty_ddic_table_types,
        table_type_key_components TYPE ty_ddic_table_key_components,
        table_type_sec_keys       TYPE ty_ddic_table_type_sec_keys,
      END OF ty_ddic_all.
    TYPES ty_extension_code TYPE Lif_shrinker_abap_code_adapter=>ty_extension_code.
    TYPES ty_oo_include TYPE Lif_shrinker_abap_code_adapter=>ty_oo_include.
    TYPES ty_class TYPE Lif_shrinker_abap_code_adapter=>ty_class.
    TYPES ty_interface TYPE Lif_shrinker_abap_code_adapter=>ty_interface.
    TYPES ty_read_class_interface_includ TYPE Lif_shrinker_abap_code_adapter=>ty_read_class_interface_includ.
    TYPES:
      BEGIN OF ty_oo_relationship,
        clsname    TYPE seometarel-clsname,
        reltype    TYPE seometarel-reltype,
        refclsname TYPE seometarel-refclsname,
      END OF ty_oo_relationship.
    TYPES ty_oo_relationships TYPE STANDARD TABLE OF ty_oo_relationship WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_class_description,
        name      TYPE seoclassdf-clsname,
        "! <ul>
        "! <li>'0': class pool</li>
        "! <li>'1': interface pool</li>
        "! </ul>
        clstype   TYPE seoclass-clstype,
        "! <ul>
        "! <li>' ': old structure (CL include)</li>
        "! <li>'X': new structure (CCDEF, CCMAC, CCIMP and CCAU includes)</li>
        "! </ul>
        clsccincl TYPE seoclassdf-clsccincl,
        category  TYPE seoclassdf-category,
      END OF ty_class_description.
    TYPES ty_class_descriptions TYPE STANDARD TABLE OF ty_class_description WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_method,
        classname  TYPE tmdir-classname,
        methodindx TYPE tmdir-methodindx,
        methodname TYPE tmdir-methodname,
      END OF ty_method.
    TYPES ty_methods TYPE STANDARD TABLE OF ty_method WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_used_exception_class,
        using_object_type TYPE tadir-object,
        using_object_name TYPE tadir-obj_name,
        used_object_type  TYPE tadir-object,
        used_object_name  TYPE tadir-obj_name,
      END OF ty_used_exception_class.
    TYPES ty_used_exception_classes TYPE STANDARD TABLE OF ty_used_exception_class WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_miscellaneous,
        all_objects            TYPE ty_objects,
        oo_relationships       TYPE ty_oo_relationships,
        class_descriptions     TYPE ty_class_descriptions,
        used_exception_classes TYPE ty_used_exception_classes,
        class_methods          TYPE ty_methods,
      END OF ty_miscellaneous.
    TYPES:
      BEGIN OF ty_abap_source_code_4_class,
        name           TYPE seoclsname,
        definition     TYPE ty_abap_source_code,
        implementation TYPE ty_abap_source_code,
      END OF ty_abap_source_code_4_class.
    TYPES ty_abap_source_code_4_classes TYPE STANDARD TABLE OF ty_abap_source_code_4_class WITH EMPTY KEY.
    TYPES: BEGIN OF ty_class_pool_local_symbol,
             class_name   TYPE string,
             abap_keyword TYPE string,
             symbol_name  TYPE string,
             replacement  TYPE string,
           END OF ty_class_pool_local_symbol,
           ty_class_pool_local_symbols TYPE HASHED TABLE OF ty_class_pool_local_symbol WITH UNIQUE KEY class_name abap_keyword symbol_name.

    DATA log TYPE string_table.
    DATA package_range TYPE ty_package_range.
    DATA objects TYPE ty_objects.
    DATA range_of_obj_name TYPE ty_range_of_obj_name.
    DATA range_of_obj_type TYPE ty_range_of_obj_type.
    DATA cc_renamings TYPE ty_cc_renamings.
    DATA replacements TYPE ty_obj_renamings.
    DATA def_include_name TYPE string.
    DATA imp_include_name TYPE string.
    DATA range_all_objects TYPE Lcl_shrinker=>ty_range_of_obj_name.
    DATA class_pool_local_symbols TYPE ty_class_pool_local_symbols.
    DATA customizer TYPE REF TO Lif_shrinker_abap_code_adapter.
    DATA program_name TYPE syrepid.

    METHODS convert_methodindx_to_extensio
      IMPORTING
        methodindx    TYPE tmdir-methodindx
      RETURNING
        VALUE(result) TYPE ty_extension_code.

    METHODS get_abap_for_class_pool
      IMPORTING
        class2        TYPE ty_class
      RETURNING
        VALUE(result) TYPE ty_abap_source_code_4_class.

    METHODS get_abap_for_data_element
      IMPORTING
        data_element  TYPE ty_ddic_data_element
      RETURNING
        VALUE(result) TYPE ty_abap_source_code.

    METHODS get_abap_for_data_elements
      IMPORTING
        data_elements TYPE ty_ddic_data_elements
      RETURNING
        VALUE(result) TYPE ty_abap_source_code.

    METHODS get_abap_for_ddic_elem_info
      IMPORTING
        type_name     TYPE csequence
        ref_to        TYPE abap_bool DEFAULT abap_false
        elem_info     TYPE ty_ddic_elementary_type
      RETURNING
        VALUE(result) TYPE ty_abap_source_code.

    METHODS get_abap_for_interface_pool
      IMPORTING
        interface     TYPE ty_interface
      RETURNING
        VALUE(result) TYPE ty_abap_source_code.

    METHODS get_abap_for_structure
      IMPORTING
        structure            TYPE ty_ddic_structure
        structure_components TYPE ty_ddic_structure_components
      RETURNING
        VALUE(result)        TYPE ty_abap_source_code.

    METHODS get_abap_for_structures
      IMPORTING
        structures           TYPE ty_ddic_structures
        structure_components TYPE ty_ddic_structure_components
      RETURNING
        VALUE(result)        TYPE ty_abap_source_code.

    METHODS get_abap_for_table_type
      IMPORTING
        table_type     TYPE ty_ddic_table_type
        key_components TYPE ty_ddic_table_key_components
        sec_keys       TYPE ty_ddic_table_type_sec_keys
      RETURNING
        VALUE(result)  TYPE ty_abap_source_code.

    METHODS get_abap_for_table_types
      IMPORTING
        table_types    TYPE ty_ddic_table_types
        key_components TYPE ty_ddic_table_key_components
        sec_keys       TYPE ty_ddic_table_type_sec_keys
      RETURNING
        VALUE(result)  TYPE ty_abap_source_code.

    METHODS get_class_include_name
      IMPORTING
        class_name    TYPE seoclsname
        extension     TYPE csequence
      RETURNING
        VALUE(result) TYPE include.

    CLASS-METHODS get_random_replacement_name
      RETURNING
        VALUE(result) TYPE string.

    METHODS get_table_type_key_components
      IMPORTING
        typename       TYPE dd43l-typename
        keyname        TYPE dd43l-seckeyname
        key_components TYPE ty_ddic_table_key_components
      RETURNING
        VALUE(result)  TYPE string.

    METHODS on_program_generated
        FOR EVENT program_generated OF SHRIS5ZPAUXVKEPN5HWER7BEX2WBRY
      IMPORTING
        progname.

    "! READ REPORT of all includes
    METHODS read_class_interface_includes
      IMPORTING
        classes       TYPE ty_class_descriptions
        class_methods TYPE ty_methods
        interfaces    TYPE ty_objects
        renamings     TYPE ty_cc_renamings
      RETURNING
        VALUE(result) TYPE ty_read_class_interface_includ.

    METHODS read_report
      IMPORTING
        include_name  TYPE include
      RETURNING
        VALUE(result) TYPE ty_abap_source_code.

    METHODS read_report_class_include
      IMPORTING
        class_name    TYPE seoclsname
        extension     TYPE csequence
        method_name   TYPE seocpdname OPTIONAL
        cc_renamings  TYPE ty_cc_renamings OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_oo_include.

    METHODS replace_texts
      IMPORTING
        replacements     TYPE ty_obj_renamings
      CHANGING
        abap_source_code TYPE ty_abap_source_code.

    METHODS select_miscellaneous
      IMPORTING
        package_range     TYPE ty_package_range
        objects           TYPE ty_objects
        range_of_obj_name TYPE ty_range_of_obj_name
        range_of_obj_type TYPE ty_range_of_obj_type
      RETURNING
        VALUE(result)     TYPE ty_miscellaneous.

    METHODS select_ddic_objects
      IMPORTING
        package_range     TYPE ty_package_range
        range_of_obj_name TYPE ty_range_of_obj_name
      RETURNING
        VALUE(result)     TYPE ty_ddic_all.
    METHODS remove_test_classes
      CHANGING
        abap_source_code TYPE ty_abap_source_code_4_classes.
    TYPES:
      BEGIN OF ty_scanned_class,
        name                   TYPE seoclsname,
        is_definition          TYPE abap_bool,
        cu_co_ci               TYPE abap_bool,
        is_test_class          TYPE abap_bool,
        is_deferred            TYPE abap_bool,
        statement_tokens       TYPE SHRIS5ZPAUXVKEPN5HWER7BEX2VBRY=>ty_ut_stokes,
        "! True if FRIENDS, LOCAL FRIENDS or GLOBAL FRIENDS
        any_friends            TYPE abap_bool,
        "! 'LOCAL', 'GLOBAL' or empty (word before FRIENDS)
        local_global_friends   TYPE string,
        local_friends          TYPE abap_bool,
        "! Position of first token in FRIENDS, LOCAL FRIENDS or GLOBAL FRIENDS
        tabix_local_or_friends TYPE i,
        "! Position after FRIENDS
        tabix_first_friend     TYPE i,
        start_line             TYPE i,
        end_line               TYPE i,
      END OF ty_scanned_class.
    "! Unique must be by name, is_definition, is_deferred, local_friends because of all these possibilities which may exist in the same source code at the same time
    "! (NB: a same class cannot have both is_deferred = 'X' and local_friends = 'X' at the same time):
    "! <ul>
    "! <li>CLASS zcl_xxx DEFINITION.</li>
    "! <li>CLASS zcl_xxx DEFINITION LOCAL FRIENDS xxxxxxxx.</li>
    "! <li>CLASS zcl_xxx IMPLEMENTATION.</li>
    "! </ul>
    "! and
    "! <ul>
*    "! <li>CLASS lcl_xxx DEFINITION DEFERRED.</li>
    "! <li>CLASS lcl_xxx DEFINITION.</li>
    "! <li>CLASS zcl_xxx IMPLEMENTATION.</li>
    "! </ul>
    TYPES ty_scanned_classes TYPE HASHED TABLE OF ty_scanned_class WITH UNIQUE KEY name is_definition is_deferred local_friends.
    METHODS get_class_endclass_positions
      IMPORTING
        itab     TYPE ty_abap_source_code
        cu_co_ci TYPE abap_bool
      CHANGING
        classes  TYPE ty_scanned_classes.

endclass. "ZCL_SHRINKER definition
