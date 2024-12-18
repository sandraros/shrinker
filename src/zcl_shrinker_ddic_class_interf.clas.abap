CLASS zcl_shrinker_ddic_class_interf DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES ty_package_range TYPE RANGE OF devclass.
    TYPES ty_range_of_obj_name TYPE RANGE OF tadir-obj_name.
    TYPES ty_range_of_obj_type TYPE RANGE OF tadir-object.
    TYPES ty_abap_source_code TYPE zif_shrinker_abap_code_adapter=>ty_abap_source_code.
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
    TYPES:
      BEGIN OF ty_object_copy,
        "! AUTH - Authorization fields
        "! SUSO - Authorization objects
        "! TABL - Tables and Structures
        "! CLAS - Classes
        "! INTF - Interfaces
        object          TYPE trobjtype,
        source_obj_name TYPE sobj_name,
        target_obj_name TYPE sobj_name,
      END OF ty_object_copy.
    TYPES ty_object_copies TYPE STANDARD TABLE OF ty_object_copy WITH EMPTY KEY.


    CLASS-METHODS create
      IMPORTING
        customizer    TYPE REF TO zif_shrinker_abap_code_adapter OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_shrinker_ddic_class_interf.

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
        zcx_shrinker.


  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      ty_range_of_object_types TYPE RANGE OF tadir-object .
    TYPES:
      BEGIN OF ty_ddic_elementary_type,
        datatype TYPE dd04l-datatype,
        leng     TYPE dd04l-leng,
        decimals TYPE dd04l-decimals,
      END OF ty_ddic_elementary_type .
    TYPES:
      BEGIN OF ty_ddic_data_element,
        rollname TYPE dd04l-rollname,
        domname  TYPE dd04l-domname,
        reftype  TYPE dd04l-reftype.
        INCLUDE TYPE ty_ddic_elementary_type AS elem_info.
    TYPES: END OF ty_ddic_data_element .
    TYPES:
      ty_ddic_data_elements TYPE STANDARD TABLE OF ty_ddic_data_element WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_ddic_structure,
        tabname  TYPE dd02l-tabname,
        tabclass TYPE dd02l-tabclass,
      END OF ty_ddic_structure .
    TYPES:
      ty_ddic_structures TYPE STANDARD TABLE OF ty_ddic_structure WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_ddic_structure_component,
        tabname   TYPE dd03l-tabname,
        position  TYPE dd03l-position,
        fieldname TYPE dd03l-fieldname,
        rollname  TYPE dd03l-rollname,
        precfield TYPE dd03l-precfield,
        comptype  TYPE dd03l-comptype,
        reftype   TYPE dd03l-reftype.
        INCLUDE TYPE ty_ddic_elementary_type AS elem_info.
    TYPES: END OF ty_ddic_structure_component .
    TYPES:
      ty_ddic_structure_components TYPE STANDARD TABLE OF ty_ddic_structure_component WITH EMPTY KEY .
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
    TYPES: END OF ty_ddic_table_type .
    TYPES:
      ty_ddic_table_types TYPE STANDARD TABLE OF ty_ddic_table_type WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_ddic_table_type_sec_key,
        typename     TYPE dd43l-typename,
        seckeyname   TYPE dd43l-seckeyname,
        seckeyunique TYPE dd43l-seckeyunique,
        accessmode   TYPE dd43l-accessmode,
        kind         TYPE dd43l-kind,
      END OF ty_ddic_table_type_sec_key .
    TYPES:
      ty_ddic_table_type_sec_keys TYPE STANDARD TABLE OF ty_ddic_table_type_sec_key WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_ddic_table_key_component,
        typename   TYPE dd42s-typename,
        seckeyname TYPE dd42s-seckeyname,
        keyfdpos   TYPE dd42s-keyfdpos,
        keyfield   TYPE dd42s-keyfield,
      END OF ty_ddic_table_key_component .
    TYPES:
      ty_ddic_table_key_components TYPE STANDARD TABLE OF ty_ddic_table_key_component WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_ddic_all,
        data_elements             TYPE ty_ddic_data_elements,
        structures                TYPE ty_ddic_structures,
        structure_components      TYPE ty_ddic_structure_components,
        table_types               TYPE ty_ddic_table_types,
        table_type_key_components TYPE ty_ddic_table_key_components,
        table_type_sec_keys       TYPE ty_ddic_table_type_sec_keys,
      END OF ty_ddic_all .
    TYPES ty_extension_code TYPE zif_shrinker_abap_code_adapter=>ty_extension_code .
    TYPES ty_oo_include TYPE zif_shrinker_abap_code_adapter=>ty_oo_include .
    TYPES ty_class TYPE zif_shrinker_abap_code_adapter=>ty_class .
    TYPES ty_interface TYPE zif_shrinker_abap_code_adapter=>ty_interface .
    TYPES ty_read_class_interface_includ TYPE zif_shrinker_abap_code_adapter=>ty_read_class_interface_includ .
    TYPES:
      BEGIN OF ty_oo_relationship,
        clsname    TYPE seometarel-clsname,
        reltype    TYPE seometarel-reltype,
        refclsname TYPE seometarel-refclsname,
      END OF ty_oo_relationship .
    TYPES:
      ty_oo_relationships TYPE STANDARD TABLE OF ty_oo_relationship WITH EMPTY KEY .
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
      END OF ty_class_description .
    TYPES:
      ty_class_descriptions TYPE STANDARD TABLE OF ty_class_description WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_method,
        classname  TYPE tmdir-classname,
        methodindx TYPE tmdir-methodindx,
        methodname TYPE tmdir-methodname,
      END OF ty_method .
    TYPES:
      ty_methods TYPE STANDARD TABLE OF ty_method WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_used_class,
        using_object_type TYPE tadir-object,
        using_object_name TYPE tadir-obj_name,
        used_object_type  TYPE tadir-object,
        used_object_name  TYPE tadir-obj_name,
      END OF ty_used_class .
    TYPES:
      ty_used_classes TYPE STANDARD TABLE OF ty_used_class WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_miscellaneous,
        all_objects        TYPE ty_objects,
        oo_relationships   TYPE ty_oo_relationships,
        class_descriptions TYPE ty_class_descriptions,
        used_classes       TYPE ty_used_classes,
        class_methods      TYPE ty_methods,
      END OF ty_miscellaneous .
    TYPES:
      BEGIN OF ty_abap_source_code_4_class,
        name           TYPE seoclsname,
        definition     TYPE ty_abap_source_code,
        implementation TYPE ty_abap_source_code,
      END OF ty_abap_source_code_4_class .
    TYPES:
      ty_abap_source_code_4_classes TYPE STANDARD TABLE OF ty_abap_source_code_4_class WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_class_pool_local_symbol,
        class_name   TYPE string,
        abap_keyword TYPE string,
        symbol_name  TYPE string,
        replacement  TYPE string,
      END OF ty_class_pool_local_symbol .
    TYPES:
      ty_class_pool_local_symbols TYPE HASHED TABLE OF ty_class_pool_local_symbol WITH UNIQUE KEY class_name abap_keyword symbol_name .
    TYPES:
      BEGIN OF ty_scanned_class,
        name                   TYPE seoclsname,
        is_definition          TYPE abap_bool,
        cu_co_ci               TYPE abap_bool,
        is_test_class          TYPE abap_bool,
        is_deferred            TYPE abap_bool,
        statement_tokens       TYPE zcl_shrinker_abap_scan=>ty_ut_stokes,
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
      END OF ty_scanned_class .
    TYPES:
      "! Unique must be by name, is_definition, is_deferred, local_friends because of all these possibilities which may exist in the same source code at the same time
      "! (NB: a same class cannot have both is_deferred = 'X' and local_friends = 'X' at the same time):
      "! <ul>
      "! <li>CLASS zcl_xxx DEFINITION.</li>
      "! <li>CLASS zcl_xxx DEFINITION LOCAL FRIENDS xxxxxxxx.</li>
      "! <li>CLASS zcl_xxx IMPLEMENTATION.</li>
      "! </ul>
      "! and
      "! <ul>
      "! <li>CLASS lcl_xxx DEFINITION DEFERRED.</li>
      "! <li>CLASS lcl_xxx DEFINITION.</li>
      "! <li>CLASS zcl_xxx IMPLEMENTATION.</li>
      "! </ul>
      ty_scanned_classes TYPE HASHED TABLE OF ty_scanned_class WITH UNIQUE KEY name is_definition is_deferred local_friends.


    DATA log TYPE string_table .
    DATA package_range TYPE ty_package_range .
    DATA objects TYPE ty_objects .
    DATA range_of_obj_name TYPE ty_range_of_obj_name .
    DATA range_of_obj_type TYPE ty_range_of_obj_type .
    DATA cc_renamings TYPE ty_cc_renamings .
    DATA replacements TYPE ty_obj_renamings .
    DATA def_include_name TYPE string .
    DATA imp_include_name TYPE string .
    DATA range_all_objects TYPE zcl_shrinker_ddic_class_interf=>ty_range_of_obj_name .
    DATA class_pool_local_symbols TYPE ty_class_pool_local_symbols .
    DATA customizer TYPE REF TO zif_shrinker_abap_code_adapter .
    DATA program_name TYPE syrepid .


    METHODS convert_methodindx_to_extensio
      IMPORTING
        !methodindx   TYPE tmdir-methodindx
      RETURNING
        VALUE(result) TYPE ty_extension_code .

    METHODS get_abap_for_class_pool
      IMPORTING
        !class2       TYPE ty_class
      RETURNING
        VALUE(result) TYPE ty_abap_source_code_4_class .

    METHODS get_abap_for_data_element
      IMPORTING
        !data_element TYPE ty_ddic_data_element
      RETURNING
        VALUE(result) TYPE ty_abap_source_code .

    METHODS get_abap_for_data_elements
      IMPORTING
        !data_elements TYPE ty_ddic_data_elements
      RETURNING
        VALUE(result)  TYPE ty_abap_source_code .

    METHODS get_abap_for_ddic_elem_info
      IMPORTING
        !type_name    TYPE csequence
        !ref_to       TYPE abap_bool DEFAULT abap_false
        !elem_info    TYPE ty_ddic_elementary_type
      RETURNING
        VALUE(result) TYPE ty_abap_source_code .

    METHODS get_abap_for_interface_pool
      IMPORTING
        !interface    TYPE ty_interface
      RETURNING
        VALUE(result) TYPE ty_abap_source_code .

    METHODS get_abap_for_structure
      IMPORTING
        !structure            TYPE ty_ddic_structure
        !structure_components TYPE ty_ddic_structure_components
      RETURNING
        VALUE(result)         TYPE ty_abap_source_code .

    METHODS get_abap_for_structures
      IMPORTING
        !structures           TYPE ty_ddic_structures
        !structure_components TYPE ty_ddic_structure_components
      RETURNING
        VALUE(result)         TYPE ty_abap_source_code .

    METHODS get_abap_for_table_type
      IMPORTING
        !table_type     TYPE ty_ddic_table_type
        !key_components TYPE ty_ddic_table_key_components
        !sec_keys       TYPE ty_ddic_table_type_sec_keys
      RETURNING
        VALUE(result)   TYPE ty_abap_source_code .

    METHODS get_abap_for_table_types
      IMPORTING
        !table_types    TYPE ty_ddic_table_types
        !key_components TYPE ty_ddic_table_key_components
        !sec_keys       TYPE ty_ddic_table_type_sec_keys
      RETURNING
        VALUE(result)   TYPE ty_abap_source_code .

    METHODS get_class_endclass_positions
      IMPORTING
        !itab     TYPE ty_abap_source_code
        !cu_co_ci TYPE abap_bool
      CHANGING
        !classes  TYPE ty_scanned_classes .

    METHODS get_class_include_name
      IMPORTING
        !class_name   TYPE seoclsname
        !extension    TYPE csequence
      RETURNING
        VALUE(result) TYPE include .

    CLASS-METHODS get_random_replacement_name
      RETURNING
        VALUE(result) TYPE string .

    METHODS get_table_type_key_components
      IMPORTING
        !typename       TYPE dd43l-typename
        !keyname        TYPE dd43l-seckeyname
        !key_components TYPE ty_ddic_table_key_components
      RETURNING
        VALUE(result)   TYPE string .

    METHODS on_program_generated
      FOR EVENT program_generated OF lcl_program_load
      IMPORTING
        !progname .

    "! READ REPORT of all includes
    METHODS read_class_interface_includes
      IMPORTING
        !classes       TYPE ty_class_descriptions
        !class_methods TYPE ty_methods
        !interfaces    TYPE ty_objects
        !renamings     TYPE ty_cc_renamings
      RETURNING
        VALUE(result)  TYPE ty_read_class_interface_includ .

    METHODS read_report
      IMPORTING
        !include_name TYPE include
      RETURNING
        VALUE(result) TYPE ty_abap_source_code .

    METHODS read_report_class_include
      IMPORTING
        !class_name   TYPE seoclsname
        !extension    TYPE csequence
        !method_name  TYPE seocpdname OPTIONAL
        !cc_renamings TYPE ty_cc_renamings OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_oo_include .

    METHODS remove_test_classes
      CHANGING
        !abap_source_code TYPE ty_abap_source_code_4_classes .

    METHODS replace_includes
      IMPORTING
        program_name TYPE syrepid
      CHANGING
        source_units TYPE zif_shrinker_abap_code_adapter=>ty_source_units.

    METHODS replace_texts
      IMPORTING
        !replacements     TYPE ty_obj_renamings
      CHANGING
        !abap_source_code TYPE ty_abap_source_code .

    METHODS select_ddic_objects
      IMPORTING
        !package_range     TYPE ty_package_range
        !range_of_obj_name TYPE ty_range_of_obj_name
        !range_of_obj_type TYPE ty_range_of_obj_type
      RETURNING
        VALUE(result)      TYPE ty_ddic_all .

    METHODS select_miscellaneous
      IMPORTING
        !package_range     TYPE ty_package_range
        !objects           TYPE ty_objects
        !range_of_obj_name TYPE ty_range_of_obj_name
        !range_of_obj_type TYPE ty_range_of_obj_type
      RETURNING
        VALUE(result)      TYPE ty_miscellaneous
      RAISING
        zcx_shrinker .

    METHODS select_used_classes
      IMPORTING
        package_range     TYPE ty_package_range
        range_of_obj_name TYPE ty_range_of_obj_name
        range_of_obj_type TYPE ty_range_of_obj_type
        all_objects       TYPE ty_objects
      RETURNING
        VALUE(result)     TYPE ty_used_classes
      RAISING
        zcx_shrinker.

ENDCLASS.



CLASS zcl_shrinker_ddic_class_interf IMPLEMENTATION.


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


  METHOD create.

    result = NEW zcl_shrinker_ddic_class_interf( ).
    result->customizer = customizer.

  ENDMETHOD.


  METHOD get_abap_for_class_pool.
    TYPES: BEGIN OF ty_find_replace_item,
             from TYPE string,
             to   TYPE string_table,
           END OF ty_find_replace_item.
    TYPES ty_find_replace_items TYPE STANDARD TABLE OF ty_find_replace_item WITH EMPTY KEY.
    TYPES ty_extension_codes TYPE STANDARD TABLE OF ty_extension_code WITH EMPTY KEY.
    TYPES ty_ref_includes TYPE STANDARD TABLE OF REF TO ty_oo_include WITH EMPTY KEY.

    DATA(class) = class2.

    IF class-name = 'ZCL_EXCEL_READER_2007'.
      ASSERT 1 = 1. " debug helper
    ENDIF.

    "=====================================
    " Convert to lower case
    "=====================================
    DATA(class_pool_abap_source_code) = REF #( class-includes[ extension_code = 'CP' ]-abap_source_code ).
    FIND ALL OCCURRENCES OF REGEX '^ *(?:class-pool|include)[^\.]*\. *$'
                            IN TABLE class_pool_abap_source_code->*
                            IGNORING CASE
                            RESULTS DATA(matches) ##REGEX_POSIX.
    LOOP AT matches REFERENCE INTO DATA(match).
      DATA(line) = REF #( class_pool_abap_source_code->*[ match->line ] ).
      TRANSLATE line->* TO LOWER CASE.
      CONDENSE line->*.
    ENDLOOP.

    class_pool_abap_source_code = REF #( class-includes[ extension_code = 'CU' ]-abap_source_code ).
    FIND ALL OCCURRENCES OF REGEX '^ *public *$'
                            IN TABLE class_pool_abap_source_code->*
                            IGNORING CASE
                            RESULTS matches ##REGEX_POSIX.
    LOOP AT matches REFERENCE INTO match.
      line = REF #( class_pool_abap_source_code->*[ match->line ] ).
      TRANSLATE line->* TO LOWER CASE.
      CONDENSE line->*.
    ENDLOOP.


    "=================================================
    " Rename all symbols declared in CCDEF, CCMAC, CCIMP and CCAU includes,
    " because the same names may be used for other local code in other class pools,
    " which would lead to syntax errors because one same name is used in the same program
    " to describe different objects.
    "   - TYPES XXX (except the ones between TYPES BEGIN OF and END OF)
    "   - TYPES BEGIN OF XXX
    "   - CONSTANTS XXX (except the ones between CONSTANTS BEGIN OF and END OF)
    "   - CONSTANTS BEGIN OF XXX
    "   - CLASS XXX DEFINITION
    "   - INTERFACE XXX
    " XXX may be used in CI and CM### includes, so XXX must be renamed there also.
    " SOLUTION: assign a new unique name which will be a 25-characters base 36 GUID preceded with "SHRIN"
    "           e.g. SHRIN46Y0UXXVUGVEYH0K3GH55TMOB
    "=================================================

    " Row  CLASS_NAME            ABAP_KEYWORD  SYMBOL_NAME
    " ======================================================================
    " 1    ZCL_ABAPGIT_INJECTOR  CLASS         LTCL_ABAPGIT_TADIR_MOCK
    " 2    ZCL_ABAPGIT_INJECTOR  CLASS         LTCL_NO_DEPENDENCY_INJECTION
    " 3    ZCL_ABAPGIT_INJECTOR  CLASS         LTCL_SIMPLE_DEPENDENCY_INJECT
    " 4    ZCL_ABAPGIT_SETTINGS  CLASS         LTCL_SETTINGS

    DATA(local_class_pool_local_symbols) = VALUE ty_class_pool_local_symbols( ).
    LOOP AT VALUE ty_extension_codes(
                ( 'CCDEF' )
                ( 'CCMAC' )
                ( 'CCIMP' )
                ( 'CCAU' ) )
        REFERENCE INTO DATA(extension_code).
      DATA(cc_include_abap_source_code) = REF #( class-includes[ extension_code = extension_code->* ]-abap_source_code ).
      FIND ALL OCCURRENCES OF REGEX '\<TYPES\>|\<CONSTANTS\>|\<ENDCLASS\>|\<ENDINTERFACE\>|\<CLASS\>|\<INTERFACE\>'
                              IN TABLE cc_include_abap_source_code->*
                              IGNORING CASE
                              RESULTS matches ##REGEX_POSIX.


      DATA(inside_class) = abap_false.

      LOOP AT matches REFERENCE INTO match.

        DATA(abap_line) = REF #( cc_include_abap_source_code->*[ match->line ] ).
        DATA(abap_keyword) = to_upper( abap_line->*+match->offset(match->length) ).
        DATA(abap_statement) = zcl_shrinker_abap_scan=>get_abap_statement_at_cursor(
                                  it_source = cc_include_abap_source_code->*
                                  i_linenr  = match->line ).

        IF abap_statement-stokes IS NOT INITIAL.

          IF inside_class = abap_false.

            DATA(symbol_name) = to_upper( SWITCH string( abap_statement-stokes[ 1 ]-str
              WHEN 'TYPES' OR 'CONSTANTS' THEN COND #( WHEN abap_statement-stokes[ 2 ]-str = 'BEGIN' AND abap_statement-stokes[ 3 ]-str = 'OF' THEN
                                                          abap_statement-stokes[ 4 ]-str
                                                       WHEN abap_statement-stokes[ 2 ]-str = 'END' AND abap_statement-stokes[ 3 ]-str = 'OF' THEN
                                                          ''
                                                       ELSE
                                                          abap_statement-stokes[ 2 ]-str )
              WHEN 'CLASS' OR 'INTERFACE' THEN COND #( WHEN abap_statement-stokes[ 2 ]-str <> class-name THEN abap_statement-stokes[ 2 ]-str ) ) ).

            IF symbol_name IS NOT INITIAL.
              INSERT VALUE #(
                      class_name   = class-name
                      abap_keyword = abap_keyword
                      symbol_name  = symbol_name
                      replacement  = get_random_replacement_name( )
                  ) INTO TABLE local_class_pool_local_symbols.
            ENDIF.

          ENDIF.

          CASE abap_statement-stokes[ 1 ]-str.
            WHEN 'CLASS' OR 'INTERFACE'.
              inside_class = abap_true.
            WHEN 'ENDCLASS' OR 'ENDINTERFACE'.
              inside_class = abap_false.
          ENDCASE.

        ENDIF.
      ENDLOOP.
    ENDLOOP.

    INSERT LINES OF local_class_pool_local_symbols INTO TABLE class_pool_local_symbols.


    "=====================================
    " GLOBAL FRIENDS -> FRIENDS
    "=====================================
    FIND ALL OCCURRENCES OF REGEX 'global +friends'
                            IN TABLE class_pool_abap_source_code->*
                            IGNORING CASE
                            RESULTS matches ##REGEX_POSIX.
    LOOP AT matches REFERENCE INTO match.
      line = REF #( class_pool_abap_source_code->*[ match->line ] ).
      REPLACE SECTION OFFSET match->offset LENGTH match->length OF line->* WITH 'friends'.
    ENDLOOP.


    "=====================================
    " ???
    "=====================================
    DATA(class_abap_source_code) = class-includes[ extension_code = 'CP' ]-abap_source_code.

    DATA(ref_includes) = VALUE ty_ref_includes(
        FOR <extension_code> IN VALUE ty_extension_codes(
                ( 'CCDEF' )
                ( 'CU' )
                ( 'CO' )
                ( 'CI' )
                ( 'CCMAC' )
                ( 'CCIMP' )
                ( 'CCAU' ) )
        LET ref_include = REF #( class-includes[ extension_code = <extension_code> ] OPTIONAL )
        IN
        ( LINES OF COND #( WHEN ref_include IS BOUND THEN VALUE #( ( ref_include ) ) ) ) ).

    DATA(find_replace_items) = VALUE ty_find_replace_items(
            ( LINES OF VALUE #(
              FOR <ref_include> IN ref_includes
              ( from = |INCLUDE { <ref_include>->include_name }.|
                to   = <ref_include>->abap_source_code ) ) )
            ( from = |class-pool .|
              to   = VALUE #( ) )
            ( from = |PUBLIC| " word part of CLASS DEFINITION to declare a global class (part of a class pool).
              to   = VALUE #( ) )
            ( from = |INCLUDE METHODS.|
              to   = VALUE #( FOR <class_pool_abap_include> IN class-includes
                              WHERE ( extension_code CP 'CM*' )
                              ( LINES OF <class_pool_abap_include>-abap_source_code ) ) ) ).


    LOOP AT find_replace_items REFERENCE INTO DATA(find_replace_item).
      DATA(line_index) = line_index( class_abap_source_code[ table_line = to_lower( find_replace_item->from ) ] ).
      IF line_index <> 0.
        DATA(replaced_abap_line) = class_abap_source_code[ line_index ].
        DELETE class_abap_source_code INDEX line_index.
        INSERT LINES OF find_replace_item->to INTO class_abap_source_code INDEX line_index.
        INSERT |*{ replaced_abap_line }| INTO class_abap_source_code INDEX line_index.
      ELSE.
        log = VALUE #( BASE log ( |Line not found in class { class-name }. Line: { find_replace_item->from }| ) ).
      ENDIF.
    ENDLOOP.


    "=======================================================================
    "=======================================================================

    LOOP AT local_class_pool_local_symbols REFERENCE INTO DATA(local_class_pool_local_symbol).
      REPLACE ALL OCCURRENCES OF
              REGEX |\\<{ local_class_pool_local_symbol->symbol_name }\\>| ##REGEX_POSIX
              IN TABLE class_abap_source_code
              WITH local_class_pool_local_symbol->replacement
              IGNORING CASE.
    ENDLOOP.


    "=======================================================================
    " Remove PUBLIC from the global class definition (CLASS z... DEFINITION ... PUBLIC ...)
    " but not the one of CREATE PUBLIC
    "=======================================================================
    DATA(regex) = |^\\s*class\\s+{ class-name }\\s+definition|.
    FIND ALL OCCURRENCES OF
            REGEX regex ##REGEX_POSIX
            IN TABLE class_abap_source_code
            IGNORING CASE
            RESULTS DATA(class_definition_matches).

    IF sy-subrc = 0.
      DATA(class_definition_statement) = zcl_shrinker_abap_scan=>get_whole_abap_statement(
                              line_index       = class_definition_matches[ 1 ]-line
                              abap_source_code = class_abap_source_code ).

      REPLACE REGEX '(?!create)(\<\w+\>\s+)public' ##REGEX_POSIX
            IN SECTION OFFSET class_definition_statement-offset
            LENGTH class_definition_statement-length
            OF class_definition_statement-whole_text
            WITH '$1'
            IGNORING CASE.
      IF sy-subrc = 0.

        SPLIT class_definition_statement-whole_text AT |\r\n| INTO TABLE DATA(class_definition_source_code).

        DELETE class_abap_source_code
                FROM class_definition_statement-first_line_index
                TO   class_definition_statement-last_line_index.

        INSERT LINES OF class_definition_source_code
                INTO class_abap_source_code
                INDEX class_definition_statement-first_line_index.

      ENDIF.
    ENDIF.

    " Special case for local test classes of class pools:
    "========================================
    "║ CLASS zcl... DEFINITION ... PUBLIC ...
    "║ ...
    "║ CLASS ltc_1 DEFINITION DEFERRED.
    "║ CLASS ltc_2 DEFINITION DEFERRED.
    "║ CLASS zcl... DEFINITION LOCAL FRIENDS ltc_1 ltc_2 ...
    "║ ...
    "========================================
    " should be turned into local classes with only one CLASS ... DEFINITION:
    "========================================
    "║ PROGRAM.
    "║ CLASS ltc_1 DEFINITION DEFERRED.
    "║ CLASS ltc_2 DEFINITION DEFERRED.
    "║ CLASS zcl... DEFINITION ... FRIENDS ltc_1 ltc_2 ...
    "║ ...
    "========================================
    " To simplify:
    "   1. all lines "CLASS ... DEFINITION DEFERRED" will be moved at the very top, above
    "      CLASS zcl... DEFINITION ... PUBLIC ...
    "      (note that "INTERFACE ... DEFERRED" is also possible).
    "   2. all classes/interfaces after "local friends" will be moved after "friends" of the
    "      first "CLASS zcl... DEFINITION ...", if "friends" is not present, it will be added before the terminal dot.
    "   3. the second CLASS zcl... DEFINITION LOCAL FRIENDS ... is removed

    IF lines( class_definition_matches ) >= 2.

      IF lines( class_definition_matches ) = 3.
        ASSERT 1 = 1. " debug helper
      ENDIF.

      FIND ALL OCCURRENCES OF
              REGEX 'class\s+(\S+)\s+definition\s+deferred'
              IN TABLE class_abap_source_code
              IGNORING CASE
              RESULTS matches ##REGEX_POSIX.

      DATA(deferred_lines) = VALUE string_table( ).
      DATA(deferred_classes) = VALUE string_table( ).
      SORT matches BY line DESCENDING.
      LOOP AT matches REFERENCE INTO match.
        line = REF #( class_abap_source_code[ match->line ] ).
        INSERT line->* INTO TABLE deferred_lines.
        DATA(submatch) = REF #( match->submatches[ 1 ] ).
        INSERT to_upper( line->*+submatch->offset(submatch->length) ) INTO TABLE deferred_classes.
        line->* = '*' && line->*.
      ENDLOOP.

      "=====================================
      " extract the list of LOCAL FRIENDS
      "=====================================
      DATA(local_friends_including_dot) = ``.
      LOOP AT class_definition_matches
            FROM 2
            REFERENCE INTO DATA(class_definition_match).

        DATA(class_local_friends_statement) = zcl_shrinker_abap_scan=>get_whole_abap_statement(
                                line_index       = class_definition_match->line
                                abap_source_code = class_abap_source_code ).

        CONSTANTS: BEGIN OF case,
                     insensitive TYPE abap_bool VALUE abap_false,
                     sensitive   TYPE abap_bool VALUE abap_true,
                   END OF case.

        REPLACE ALL OCCURRENCES OF |\r\n| IN class_local_friends_statement-whole_text WITH ` `.
        CONDENSE class_local_friends_statement-whole_text.

        " Get all the local friends, including the trailing dot.
        local_friends_including_dot = local_friends_including_dot
                           && substring_after( val  = class_local_friends_statement-whole_text
                                               sub  = `local friends `
                                               case = case-insensitive ).

        LOOP AT class_abap_source_code REFERENCE INTO line
            FROM class_local_friends_statement-first_line_index
            TO   class_local_friends_statement-last_line_index.
          line->* = '*' && line->*.
        ENDLOOP.

        DATA(local_friends) = zcl_shrinker_utils=>split_at_regex(
                                val   = to_upper( replace( val = local_friends_including_dot sub = '.' with = ` ` occ = 0 ) )
                                regex = ` +` ).
        DELETE local_friends WHERE table_line = ``.
        LOOP AT local_friends REFERENCE INTO DATA(local_friend).
          IF NOT line_exists( deferred_classes[ table_line = local_friend->* ] ).
            INSERT local_friend->* INTO TABLE deferred_classes.
            INSERT |CLASS { local_friend->* } DEFINITION DEFERRED.| INTO TABLE deferred_lines.
          ENDIF.
        ENDLOOP.

      ENDLOOP.

      DATA(local_friends_excluding_dots) = replace( val  = local_friends_including_dot
                                                    sub  = `.`
                                                    with = ` `
                                                    occ  = 0 ).

      DATA(friends) = substring_after( val  = class_definition_statement-whole_text
                                       sub  = ` friends `
                                       case = case-insensitive ).

      line = REF #( class_abap_source_code[ class_definition_statement-last_line_index ] ).
      REPLACE '.' IN line->* WITH ''.

      DATA(abap_local_friends) = VALUE string_table(
            ( |{ COND #( WHEN NOT contains( val = class_definition_statement-whole_text
                                            sub = ` friends ` )
                         THEN `FRIENDS ` ) }| )
            ( LINES OF zcl_shrinker_utils=>split_at_regex(
                            val   = condense( local_friends_excluding_dots )
                            regex = ` ` ) )
            ( `.` ) ).

      INSERT LINES OF abap_local_friends
            INTO class_abap_source_code
            INDEX class_definition_statement-last_line_index + 1.

      INSERT LINES OF deferred_lines INTO class_abap_source_code INDEX 1.

    ENDIF.

    "=====================================
    " Split the lines into DEFINITION and IMPLEMENTATION parts.
    "=====================================
    FIND FIRST OCCURRENCE OF
            REGEX '^endclass\..+'
            IN TABLE class_abap_source_code
            IGNORING CASE
            MATCH LINE DATA(endclass_line) ##REGEX_POSIX.
    IF sy-subrc <> 0.
      log = VALUE #( BASE log ( |ENDCLASS not found in class { class-name }.| ) ).
      RETURN.
    ENDIF.

    result = VALUE #(
            name           = class-name
            definition     = VALUE ty_abap_source_code(
                            FOR <abap_line> IN class_abap_source_code
                            FROM 1
                            TO endclass_line
                            ( <abap_line> ) )
            implementation = VALUE ty_abap_source_code(
                            ( |*>>>>>>> { class-name } <<<<<<<*| )
                            ( LINES OF VALUE #(
                            FOR <abap_line> IN class_abap_source_code
                            FROM endclass_line + 1
                            ( <abap_line> ) ) ) ) ).

  ENDMETHOD.


  METHOD get_abap_for_data_element.

    result = get_abap_for_ddic_elem_info( type_name = data_element-rollname
                                          ref_to    = xsdbool( data_element-reftype IS NOT INITIAL )
                                          elem_info = data_element-elem_info ).

  ENDMETHOD.


  METHOD get_abap_for_data_elements.

    result = VALUE #(
        FOR <data_element> IN data_elements
        ( LINES OF get_abap_for_data_element( <data_element> ) ) ).

  ENDMETHOD.


  METHOD get_abap_for_ddic_elem_info.

    TYPES: BEGIN OF ty_type_equivalence_abap_ddic,
             ddic TYPE datatype_d,
             abap TYPE c LENGTH 10,
             " abap_internal type abap_typekind,
           END OF ty_type_equivalence_abap_ddic.
    TYPES tt_type_equivalence_abap_ddic TYPE STANDARD TABLE OF ty_type_equivalence_abap_ddic WITH EMPTY KEY.

    DATA(type_equivalence_abap_ddic) = VALUE tt_type_equivalence_abap_ddic(
        ( ddic = 'ACCP' abap = 'N'          )
        ( ddic = 'CHAR' abap = 'C'          )
        ( ddic = 'CLNT' abap = 'C'          )
        ( ddic = 'CUKY' abap = 'C'          )
        ( ddic = 'CURR' abap = 'P'          )
        ( ddic = 'DATS' abap = 'D'          )
        ( ddic = 'DEC ' abap = 'P'          )
        ( ddic = 'D16R' abap = 'decfloat16' )
        ( ddic = 'D16S' abap = 'decfloat16' )
        ( ddic = 'D34R' abap = 'decfloat34' )
        ( ddic = 'D34S' abap = 'decfloat34' )
        ( ddic = 'FLTP' abap = 'F'          )
        ( ddic = 'INT1' abap = 'INT1'       )
        ( ddic = 'INT2' abap = 'INT2'       )
        ( ddic = 'INT4' abap = 'I'          )
        ( ddic = 'LANG' abap = 'C'          )
        ( ddic = 'LCHR' abap = 'C'          )
        ( ddic = 'LRAW' abap = 'X'          )
        ( ddic = 'NUMC' abap = 'N'          )
        ( ddic = 'PREC' abap = 'INT2'       )
        ( ddic = 'QUAN' abap = 'P'          )
        ( ddic = 'RAW ' abap = 'X'          )
        ( ddic = 'RSTR' abap = 'xstring'    )
        ( ddic = 'SSTR' abap = 'string'     )
        ( ddic = 'STRG' abap = 'string'     )
        ( ddic = 'TIMS' abap = 'T'          )
        ( ddic = 'UNIT' abap = 'C'          ) ).

    DATA(abap_type) = VALUE #( type_equivalence_abap_ddic[ ddic = elem_info-datatype ]-abap OPTIONAL ).
    IF abap_type IS INITIAL.
      ASSERT 1 = 1. " debug helper
    ENDIF.

    DATA(length) = SWITCH string( abap_type
            WHEN 'C' OR 'N' OR 'X' THEN |{ CONV i( elem_info-leng ) }| " LENG is type N -> remove leading zeroes and no trailing spaces
            WHEN 'P' THEN |{ 1 + round( val = ( elem_info-leng - 1 ) / 2 dec = 0 ) }|
            ELSE '' ).

    DATA(decimals) = COND string( WHEN abap_type = 'P' THEN |{ CONV i( elem_info-decimals ) }| ). " DECIMALS is type N -> remove leading zeroes and no trailing spaces

    result = VALUE #(
            ( |TYPES { type_name } TYPE { COND #( WHEN ref_to = abap_true THEN |REF TO | )
                                        && abap_type
                                        && COND #( WHEN length IS NOT INITIAL THEN | LENGTH { length }| )
                                        && COND #( WHEN decimals IS NOT INITIAL THEN | DECIMALS { decimals }| ) }.| ) ).

  ENDMETHOD.


  METHOD get_abap_for_interface_pool.
    TYPES: BEGIN OF ty_find_replace_item,
             from TYPE string,
             to   TYPE string_table,
           END OF ty_find_replace_item.
    TYPES ty_find_replace_items TYPE STANDARD TABLE OF ty_find_replace_item WITH EMPTY KEY.
    TYPES ty_extension_codes TYPE STANDARD TABLE OF ty_extension_code WITH EMPTY KEY.
    TYPES ty_ref_includes TYPE STANDARD TABLE OF REF TO ty_oo_include WITH EMPTY KEY.

    DATA(interface_abap_source_code) = interface-includes[ extension_code = 'IP' ]-abap_source_code.

    DATA(ref_includes) = VALUE ty_ref_includes(
        FOR <extension_code> IN VALUE ty_extension_codes(
                ( 'IU' ) )
        LET ref_include = REF #( interface-includes[ extension_code = <extension_code> ] OPTIONAL )
        IN
        ( LINES OF COND #( WHEN ref_include IS BOUND THEN VALUE #( ( ref_include ) ) ) ) ).

    DATA(find_replace_items) = VALUE ty_find_replace_items(
            ( from = |interface-pool.|
              to   = VALUE #( ) )
            ( LINES OF VALUE #(
              FOR <ref_include> IN ref_includes
              ( from = |  INCLUDE { <ref_include>->include_name }.|
                to   = <ref_include>->abap_source_code ) ) ) ).

    LOOP AT find_replace_items REFERENCE INTO DATA(find_replace_item).
      FIND REGEX find_replace_item->from ##REGEX_POSIX
            IN TABLE interface_abap_source_code
            IGNORING CASE
            MATCH LINE DATA(line_index).
      IF sy-subrc = 0.
        DELETE interface_abap_source_code INDEX line_index.
        INSERT LINES OF find_replace_item->to INTO interface_abap_source_code INDEX line_index.
      ELSE.
        log = VALUE #( BASE log ( |Line not found in class { interface-name }. Line: { find_replace_item->from }| ) ).
      ENDIF.
    ENDLOOP.

    DATA(regex) = `^\s*interface\s+`.
    FIND ALL OCCURRENCES OF
            REGEX regex
            IN TABLE interface_abap_source_code
            IGNORING CASE
            RESULTS DATA(matches) ##REGEX_POSIX.

    LOOP AT matches REFERENCE INTO DATA(match).

      DATA(interface_statement) = zcl_shrinker_abap_scan=>get_whole_abap_statement(
                              line_index       = match->line
                              abap_source_code = interface_abap_source_code ).
      LOOP AT interface_abap_source_code
          REFERENCE INTO DATA(abap_line)
          FROM interface_statement-first_line_index
          TO interface_statement-last_line_index.
        REPLACE REGEX `\<public\>` IN abap_line->* WITH `` IGNORING CASE ##REGEX_POSIX.
      ENDLOOP.

    ENDLOOP..

    result = interface_abap_source_code.

  ENDMETHOD.


  METHOD get_abap_for_structure.

    result = VALUE #(
        ( |TYPES BEGIN OF { structure-tabname }.| )
        ( LINES OF VALUE #(
            FOR <component> IN structure_components
            WHERE ( tabname = structure-tabname )
            ( LINES OF COND #( WHEN <component>-fieldname(1) = '.' THEN
                                    VALUE #( ( |INCLUDE TYPE { <component>-precfield }.| ) )
                                WHEN <component>-rollname IS NOT INITIAL THEN
                                    VALUE #( ( |TYPES { <component>-fieldname } TYPE { COND #( WHEN <component>-reftype IS NOT INITIAL THEN |REF TO | )
                                                                                        && <component>-rollname }.| ) )
                                ELSE
                                    get_abap_for_ddic_elem_info( type_name = <component>-fieldname
                                                                 ref_to    = xsdbool( <component>-reftype IS NOT INITIAL )
                                                                 elem_info = <component>-elem_info ) ) ) ) )
        ( |TYPES END OF { structure-tabname }.| ) ).

  ENDMETHOD.


  METHOD get_abap_for_structures.

    result = VALUE #(
        FOR <structure> IN structures
        ( LINES OF get_abap_for_structure( structure            = <structure>
                                           structure_components = structure_components ) ) ).

  ENDMETHOD.


  METHOD get_abap_for_table_type.

    result = VALUE #(
        LET auxiliary_type_name = COND #( WHEN table_type-rowkind IS INITIAL THEN | OF | )
            auxiliary_type_abap_line = COND #( WHEN auxiliary_type_name IS NOT INITIAL THEN
                                            get_abap_for_ddic_elem_info( type_name = auxiliary_type_name
                                                                         ref_to    = xsdbool( table_type-reftype IS NOT INITIAL )
                                                                         elem_info = table_type-elem_info ) )
            last_secondary_key_name = REDUCE string( INIT t = ``
                                                     FOR <sec_key> IN sec_keys
                                                     WHERE ( typename = table_type-typename )
                                                     NEXT t = <sec_key>-seckeyname )
        IN
        ( LINES OF COND #( WHEN auxiliary_type_abap_line IS NOT INITIAL THEN auxiliary_type_abap_line ) )
        ( |TYPES { table_type-typename
            } TYPE { SWITCH #( table_type-accessmode
                        WHEN 'T' THEN `STANDARD`
                        WHEN 'S' THEN `SORTED`
                        WHEN 'H' THEN `HASHED`
                        WHEN 'A' THEN `ANY`
                        WHEN 'I' THEN `INDEX` )
            } TABLE OF { COND #( WHEN auxiliary_type_name IS NOT INITIAL THEN
                            auxiliary_type_name
                        ELSE
                            table_type-rowtype )
            }{ COND #( WHEN table_type-keykind <> 'G' THEN | WITH { SWITCH #( table_type-keykind WHEN 'U' THEN `UNIQUE` ELSE `NON-UNIQUE` )
                } { SWITCH #( table_type-keydef WHEN 'D' THEN |DEFAULT KEY|
                                                WHEN 'T' THEN |KEY table_line|
                                                WHEN 'K' THEN |KEY { get_table_type_key_components( typename       = table_type-typename
                                                                                                    keyname        = ''
                                                                                                    key_components = key_components ) }|
                           )
                }| " end of WITH
                )
            }{ COND #( WHEN last_secondary_key_name IS INITIAL THEN '.' ) }| )
        ( LINES OF COND #( WHEN last_secondary_key_name IS NOT INITIAL THEN VALUE #(
            FOR <sec_key> IN sec_keys
            WHERE ( typename = table_type-typename )
            ( | WITH { SWITCH #( <sec_key>-kind WHEN 'U' THEN `UNIQUE` ELSE `NON-UNIQUE` )
                    }{ SWITCH #( <sec_key>-accessmode
                        WHEN 'T' THEN `STANDARD`
                        WHEN 'S' THEN `SORTED`
                        WHEN 'H' THEN `HASHED`
                        WHEN 'A' THEN `ANY`
                        WHEN 'I' THEN `INDEX` )
                    } KEY { <sec_key>-seckeyname
                    } COMPONENTS { get_table_type_key_components( typename       = table_type-typename
                                                                  keyname        = <sec_key>-seckeyname
                                                                  key_components = key_components )
                    }{ COND #( WHEN <sec_key>-seckeyname = last_secondary_key_name THEN '.' ) }| ) ) ) ) ).

  ENDMETHOD.


  METHOD get_abap_for_table_types.

    result = VALUE #(
        FOR <table_type> IN table_types
        ( LINES OF get_abap_for_table_type(
                     table_type     = <table_type>
                     key_components = key_components
                     sec_keys       = sec_keys ) ) ).

  ENDMETHOD.


  METHOD get_class_endclass_positions.
    DATA current_class TYPE ty_scanned_class.

    FIND ALL OCCURRENCES OF
        REGEX 'definition|implementation|endclass' ##REGEX_POSIX
        IN TABLE itab
        IGNORING CASE
        RESULTS DATA(matches).

    LOOP AT matches REFERENCE INTO DATA(match).
      " skip lines corresponding to comments (line which starts with an asterisk)
      CHECK itab[ match->line ] NP '#**'.

      DATA(abap_statement) = zcl_shrinker_abap_scan=>get_abap_statement_at_cursor(
                                  it_source = itab
                                  i_linenr  = match->line ).
      CHECK lines( abap_statement-stokes ) > 0.

      CASE abap_statement-stokes[ 1 ]-str.
        WHEN 'CLASS'.
          current_class = VALUE #( LET aux_tabix_friends          = line_index( abap_statement-stokes[ str = 'FRIENDS' ] )
                                       aux_local_global_friends   = COND #( LET word_before_friends = COND #(
                                                                                    WHEN aux_tabix_friends > 1
                                                                                    THEN abap_statement-stokes[ aux_tabix_friends - 1 ]-str )
                                                                            IN
                                                                            WHEN SWITCH #( word_before_friends WHEN 'LOCAL' OR 'GLOBAL' THEN abap_true ) = abap_true
                                                                            THEN word_before_friends )
                                       aux_tabix_local_or_friends = COND i( WHEN aux_tabix_friends > 0
                                                                            AND abap_statement-stokes[ aux_tabix_friends - 1 ]-str = 'LOCAL'
                                                                            THEN
                                                                            aux_tabix_friends - 1
                                                                            ELSE
                                                                            aux_tabix_friends )
                                   IN
                                   name                   = abap_statement-stokes[ 2 ]-str
                                   cu_co_ci               = cu_co_ci
                                   is_definition          = xsdbool( abap_statement-stokes[ 3 ]-str = 'DEFINITION' )
                                   is_test_class          = xsdbool( line_index( abap_statement-stokes[ str = 'FOR' ] )
                                                                    = line_index( abap_statement-stokes[ str = 'TESTING' ] ) - 1 )
                                   is_deferred            = xsdbool( line_exists( abap_statement-stokes[ str = 'DEFERRED' ] ) )
                                   statement_tokens       = abap_statement-stokes
                                   any_friends            = xsdbool( aux_tabix_friends > 0 )
                                   local_global_friends   = aux_local_global_friends
                                   local_friends          = xsdbool( aux_local_global_friends = 'LOCAL' )
                                   tabix_local_or_friends = aux_tabix_local_or_friends
                                   tabix_first_friend     = COND #( WHEN aux_tabix_friends > 0 THEN aux_tabix_friends + 1 )
                                   start_line             = abap_statement-stokes[ 1 ]-row ).
          IF current_class-is_deferred = abap_true
                OR current_class-local_friends = abap_true.
            current_class-end_line = abap_statement-stokes[ lines( abap_statement-stokes ) ]-row.
            INSERT current_class INTO TABLE classes.
            current_class = VALUE #( ).
          ENDIF.
        WHEN 'ENDCLASS'.
          IF current_class IS NOT INITIAL.
            current_class-end_line = abap_statement-stokes[ 1 ]-row.
            INSERT current_class INTO TABLE classes.
            current_class = VALUE #( ).
          ENDIF.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_class_include_name.

    TYPES: BEGIN OF ty_class_and_extension,
             class_name TYPE c LENGTH 30,
             extension  TYPE ty_extension_code,
           END OF ty_class_and_extension.

    result = translate( val  = CONV string( VALUE ty_class_and_extension(
                                 class_name = class_name
                                 extension  = extension ) )
                        from = ` `
                        to   = '=' ).

  ENDMETHOD.


  METHOD get_one_abap_code.

    TRY.

        me->package_range     = package_range.
        me->objects           = objects.
        me->range_of_obj_name = range_of_obj_name.
        me->range_of_obj_type = range_of_obj_type.
        me->cc_renamings      = class_replacements.
        me->replacements      = global_replacements.

        DATA(miscellaneous) = select_miscellaneous( package_range     = package_range
                                                    objects           = objects
                                                    range_of_obj_type = range_of_obj_type
                                                    range_of_obj_name = range_of_obj_name ).

        range_all_objects = VALUE ty_range_of_obj_name(
                FOR <object> IN miscellaneous-all_objects
                WHERE ( obj_name IN range_of_obj_name )
                ( sign   = 'I'
                  option = 'EQ'
                  low    = <object>-obj_name ) ).

        DATA(classes_interfaces) = read_class_interface_includes(
                    classes       = miscellaneous-class_descriptions
                    class_methods = miscellaneous-class_methods
                    interfaces    = VALUE #(
                                    FOR <object> IN miscellaneous-all_objects
                                    WHERE ( object = 'INTF' )
                                    ( <object> ) )
                    renamings     = cc_renamings ).

        LOOP AT classes_interfaces-classes REFERENCE INTO DATA(class).

          DATA(class_cu_include) = REF #( class->includes[ extension_code = 'CU' ] ).
          FIND ALL OCCURRENCES OF 'FRIENDS' IN TABLE class_cu_include->abap_source_code RESULTS DATA(matches).
          LOOP AT matches REFERENCE INTO DATA(match).
            DATA(abap_statement) = zcl_shrinker_abap_scan=>get_abap_statement_at_cursor(
                                      it_source = class_cu_include->abap_source_code
                                      i_linenr  = match->line
                                      i_offset  = match->offset ).
            IF lines( abap_statement-stokes ) >= 1
                AND abap_statement-stokes[ 1 ]-str = 'CLASS'.
              DATA(tabix_friends) = line_index( abap_statement-stokes[ str = 'FRIENDS' ] ).
              IF tabix_friends >= 2
                    AND abap_statement-stokes[ tabix_friends - 1 ]-str = 'GLOBAL'.
                DATA(at_least_one_friend_remains) = abap_false.
                DATA(tokens_to_remove) = VALUE zcl_shrinker_abap_scan=>ty_ut_stokes( ).
                LOOP AT abap_statement-stokes REFERENCE INTO DATA(token)
                    FROM tabix_friends + 1.
                  IF line_exists( miscellaneous-class_descriptions[ name     = token->str
                                                                    category = '05' ] ).
                    " Remove the friend if it's a class pool FOR TESTING (05)
                    APPEND token->* TO tokens_to_remove.
                  ELSE.
                    at_least_one_friend_remains = abap_true.
                  ENDIF.
                ENDLOOP.
                IF at_least_one_friend_remains = abap_false.
                  " Remove GLOBAL
                  token = REF #( abap_statement-stokes[ tabix_friends - 1 ] ).
                  APPEND token->* TO tokens_to_remove.
                  " Remove FRIENDS
                  token = REF #( abap_statement-stokes[ tabix_friends ] ).
                  APPEND token->* TO tokens_to_remove.
                ENDIF.
                SORT tokens_to_remove BY row ASCENDING col DESCENDING.
                LOOP AT tokens_to_remove REFERENCE INTO token.
                  DATA(abap_line) = REF #( class_cu_include->abap_source_code[ token->row ] OPTIONAL ).
                  IF abap_line IS BOUND.
                    REPLACE SECTION
                          OFFSET token->col
                          LENGTH strlen( token->str )
                          OF abap_line->*
                          WITH space.
                  ENDIF.
                ENDLOOP.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDLOOP.

        IF customizer IS BOUND.
          customizer->adapt_source_code( CHANGING classes_interfaces = classes_interfaces ).
        ENDIF.

        DATA(deferred) = VALUE ty_abap_source_code(
            FOR <clas_intf> IN miscellaneous-class_descriptions
            WHERE ( category <> '05' ) " exclude class pools FOR TESTING
            ( SWITCH #( <clas_intf>-clstype WHEN '0' THEN |CLASS { <clas_intf>-name } DEFINITION DEFERRED.|
                                            WHEN '1' THEN |INTERFACE { <clas_intf>-name } DEFERRED.| ) ) ).

        DATA(ddic) = select_ddic_objects( package_range     = package_range
                                          range_of_obj_name = range_of_obj_name
                                          range_of_obj_type = range_of_obj_type ).

        TYPES: BEGIN OF ty_dependency,
                 object     TYPE ty_object,
                 "! Object which is needed by OBJECT i.e. REF_OBJECT must be defined before OBJECT.
                 ref_object TYPE ty_object,
               END OF ty_dependency.
        TYPES ty_dependencies TYPE STANDARD TABLE OF ty_dependency WITH EMPTY KEY.

        DATA(dependencies) = VALUE ty_dependencies( ).
        LOOP AT ddic-data_elements REFERENCE INTO DATA(data_element).
          IF data_element->domname IS NOT INITIAL
              AND data_element->domname <> data_element->rollname
              AND data_element->domname <> 'OBJECT'
              AND data_element->domname <> 'DATA'.
            INSERT VALUE #( object     = VALUE #( pgmid    = 'R3TR'
                                                  object   = 'DTEL'
                                                  obj_name = data_element->rollname )
                            ref_object = VALUE #( pgmid    = 'R3TR'
                                                  object   = SWITCH #( data_element->reftype
                                                            WHEN 'C' THEN 'CLAS'
                                                            WHEN 'E' THEN 'DTEL'
                                                            WHEN 'I' THEN 'INTF'
                                                            WHEN 'L' THEN 'TTYP'
                                                            WHEN 'S' THEN 'TABL'
                                                            ELSE          'DOMA' )
                                                  obj_name = data_element->domname )
                          ) INTO TABLE dependencies.
          ENDIF.
        ENDLOOP.

        LOOP AT ddic-structure_components REFERENCE INTO DATA(structure_component).
          IF structure_component->fieldname(1) = '.'.
            INSERT VALUE #( object     = VALUE #( pgmid    = 'R3TR'
                                                  object   = 'TABL'
                                                  obj_name = structure_component->tabname )
                            ref_object = VALUE #( pgmid    = 'R3TR'
                                                  object   = 'TABL'
                                                  obj_name = structure_component->precfield )
                          ) INTO TABLE dependencies.
          ELSEIF structure_component->rollname IS NOT INITIAL
                AND structure_component->reftype <> 'B'
                AND structure_component->reftype <> 'D'
                AND structure_component->rollname <> 'OBJECT'.
            INSERT VALUE #( object     = VALUE #( pgmid    = 'R3TR'
                                                  object   = 'TABL'
                                                  obj_name = structure_component->tabname )
                            ref_object = VALUE #( pgmid    = 'R3TR'
                                                  object   = SWITCH #( structure_component->comptype
                                                            WHEN 'E' THEN 'DTEL'
                                                            WHEN 'L' THEN 'TTYP'
                                                            WHEN 'R' THEN SWITCH #( structure_component->reftype
                                                                    WHEN 'C' THEN 'CLAS'
                                                                    WHEN 'E' THEN 'DTEL'
                                                                    WHEN 'I' THEN 'INTF'
                                                                    WHEN 'L' THEN 'TTYP'
                                                                    WHEN 'S' THEN 'TABL'
                                                                    ELSE          '?' )
                                                            WHEN 'S' THEN 'TABL'
                                                            ELSE          '?' )
                                                  obj_name = structure_component->rollname )
                          ) INTO TABLE dependencies.
          ENDIF.
        ENDLOOP.

        LOOP AT ddic-table_types REFERENCE INTO DATA(table_type).
          IF table_type->rowtype IS NOT INITIAL.
            INSERT VALUE #( object     = VALUE #( pgmid    = 'R3TR'
                                                  object   = 'TTYP'
                                                  obj_name = table_type->typename )
                            ref_object = VALUE #( pgmid    = 'R3TR'
                                                  object   = SWITCH #( table_type->rowkind
                                                            WHEN 'E' THEN 'DTEL'
                                                            WHEN 'L' THEN 'TTYP'
                                                            WHEN 'R' THEN SWITCH #( table_type->reftype
                                                                    WHEN 'C' THEN 'CLAS'
                                                                    WHEN 'E' THEN 'DTEL'
                                                                    WHEN 'I' THEN 'INTF'
                                                                    WHEN 'L' THEN 'TTYP'
                                                                    ELSE          '?' )
                                                            WHEN 'S' THEN COND #( WHEN table_type->ttypkind IS INITIAL
                                                                                THEN 'TABL'   " pas un range
                                                                                ELSE 'DTEL' ) " range
                                                            ELSE          '?' )
                                                  obj_name = table_type->rowtype )
                          ) INTO TABLE dependencies.
          ENDIF.
        ENDLOOP.

        LOOP AT miscellaneous-oo_relationships REFERENCE INTO DATA(oo_relationship).
          INSERT VALUE #( object     = VALUE #( pgmid    = 'R3TR'
                                                object   = SWITCH #( oo_relationship->reltype
                                                              WHEN '0' THEN 'INTF'
                                                              ELSE          'CLAS' )
                                                obj_name = oo_relationship->clsname )
                          ref_object = VALUE #( pgmid    = 'R3TR'
                                                object   = SWITCH #( oo_relationship->reltype
                                                              WHEN '2' THEN 'CLAS'
                                                              ELSE          'INTF' )
                                                obj_name = oo_relationship->refclsname )
                        ) INTO TABLE dependencies.
        ENDLOOP.

        LOOP AT miscellaneous-used_classes REFERENCE INTO DATA(used_class).
          INSERT VALUE #( object     = VALUE #( pgmid    = 'R3TR'
                                                object   = used_class->using_object_type
                                                obj_name = used_class->using_object_name )
                          ref_object = VALUE #( pgmid    = 'R3TR'
                                                object   = used_class->used_object_type
                                                obj_name = used_class->used_object_name )
                        ) INTO TABLE dependencies.
        ENDLOOP.

        SORT dependencies BY table_line.
        DELETE ADJACENT DUPLICATES FROM dependencies COMPARING table_line.

        TYPES ty_ref_objects TYPE STANDARD TABLE OF ty_object WITH EMPTY KEY.
        TYPES: BEGIN OF ty_dependency_2,
                 object      TYPE ty_object,
                 "! Object which is needed by OBJECT i.e. REF_OBJECT must be defined before OBJECT.
                 ref_objects TYPE ty_objects,
               END OF ty_dependency_2.
        TYPES ty_dependencies_2 TYPE STANDARD TABLE OF ty_dependency_2 WITH EMPTY KEY.

        DATA(dependencies_2) = VALUE ty_dependencies_2(
                ( LINES OF VALUE #(
                    FOR GROUPS <group_object> OF <dependency> IN dependencies
                    GROUP BY <dependency>-object
                    ( object      = <group_object>
                      ref_objects = VALUE #(
                                    FOR <dependency_bis> IN GROUP <group_object>
                                    ( LINES OF COND #( WHEN line_exists( miscellaneous-all_objects[ table_line = <dependency_bis>-ref_object ] )
                                                       THEN VALUE #( ( <dependency_bis>-ref_object ) ) ) ) ) ) ) )
                ( LINES OF VALUE #(
                    FOR <object> IN miscellaneous-all_objects
                    ( LINES OF COND #( WHEN NOT line_exists( dependencies[ object = <object> ] ) THEN VALUE #( ( object = <object> ) ) ) ) ) ) ).

        SORT dependencies_2 BY table_line.

        " All dictionary objects will be positioned before classes and interfaces because
        " the object types will be defined as DEFERRED before the TYPES equivalences of dictionary objects.

        TYPES ty_range_objects TYPE RANGE OF trobjtype.

        DATA(objects_in_order) = VALUE ty_objects( ).
        DATA(range_ddic_objects) = VALUE ty_range_objects(
                sign   = 'I'
                option = 'EQ'
                ( low = 'DOMA' )
                ( low = 'DTEL' )
                ( low = 'TABL' )
                ( low = 'TTYP' ) ).

        LOOP AT dependencies_2 REFERENCE INTO DATA(dependency_2)
              WHERE object-object IN range_ddic_objects.
          DELETE dependency_2->ref_objects
              WHERE object = 'CLAS'
                 OR object = 'INTF'.
        ENDLOOP.

        DATA(range_objects) = range_ddic_objects.

        DO.
          DATA(dependencies_implying_a_move) = VALUE ty_dependencies( ).
          LOOP AT dependencies_2 REFERENCE INTO dependency_2
                WHERE object-object IN range_objects
                  AND ref_objects IS INITIAL.
            INSERT dependency_2->object INTO TABLE objects_in_order.
            LOOP AT dependencies_2 REFERENCE INTO DATA(dependency_2_bis).
              DELETE dependency_2_bis->ref_objects WHERE table_line = dependency_2->object.
            ENDLOOP.
            DELETE dependencies_2 USING KEY loop_key.
          ENDLOOP.
          IF sy-subrc <> 0 OR dependencies_2 IS INITIAL.
            IF range_objects IS NOT INITIAL.
              range_objects = VALUE #( ).
            ELSE.
              EXIT.
            ENDIF.
          ENDIF.
        ENDDO.

        IF dependencies_2 IS NOT INITIAL.
          " circular reference error (e.g. object A requires object B which requires A)
          RAISE EXCEPTION TYPE zcx_shrinker EXPORTING text = 'Circular reference while resolving order in DDIC objects'(001).
        ENDIF.


        DATA(abap_source_code_4_classes) = VALUE ty_abap_source_code_4_classes(
                    FOR <class> IN classes_interfaces-classes
                    ( get_abap_for_class_pool( <class> ) ) ).

        IF omit_test_classes = abap_true.
          remove_test_classes( CHANGING abap_source_code = abap_source_code_4_classes ).
        ENDIF.

        " The object names may exist in TADIR but the objects may not exist, so need to add conditions.
        result-def_abap_source_code = VALUE ty_abap_source_code(
            ( LINES OF deferred )
            ( LINES OF VALUE #(
                  FOR <object> IN objects_in_order
                  ( LINES OF SWITCH #( <object>-object
                                       WHEN 'DTEL' THEN
                                         LET aux_data_element = REF #( ddic-data_elements[
                                                                           rollname = <object>-obj_name ] OPTIONAL )
                                         IN COND #( WHEN aux_data_element IS BOUND
                                                    THEN get_abap_for_data_element( aux_data_element->* ) )
                                       WHEN 'CLAS' THEN
                                         LET aux_class = REF #( abap_source_code_4_classes[ name = <object>-obj_name ] OPTIONAL )
                                         IN COND #( WHEN aux_class IS BOUND THEN aux_class->definition )
                                       WHEN 'INTF' THEN
                                         LET aux_interface = REF #( classes_interfaces-interfaces[
                                                                        name = <object>-obj_name ] OPTIONAL )
                                         IN COND #( WHEN aux_interface IS BOUND
                                                    THEN get_abap_for_interface_pool( aux_interface->* ) )
                                       WHEN 'TABL' THEN
                                         LET aux_ddic_structure = REF #( ddic-structures[ tabname = <object>-obj_name ] OPTIONAL )
                                         IN COND #( WHEN aux_ddic_structure IS BOUND
                                                    THEN get_abap_for_structure(
                                                            structure            = aux_ddic_structure->*
                                                            structure_components = ddic-structure_components ) )
                                       WHEN 'TTYP' THEN
                                         LET aux_ddic_table_type = REF #( ddic-table_types[
                                                                              typename = <object>-obj_name ] OPTIONAL )
                                         IN COND #( WHEN aux_ddic_table_type IS BOUND
                                                    THEN get_abap_for_table_type(
                                                            table_type     = aux_ddic_table_type->*
                                                            key_components = ddic-table_type_key_components
                                                            sec_keys       = ddic-table_type_sec_keys ) ) ) ) ) ) ).


        replace_texts( EXPORTING replacements     = replacements
                       CHANGING  abap_source_code = result-def_abap_source_code ).

        " The object names may exist in TADIR but the objects may not exist, so need to add conditions.
        result-imp_abap_source_code = VALUE ty_abap_source_code(
            FOR <object> IN miscellaneous-all_objects
            WHERE ( object = 'CLAS' )
            ( LINES OF COND #( LET aux_class = REF #( abap_source_code_4_classes[ name = <object>-obj_name ] OPTIONAL ) IN
                               WHEN aux_class IS BOUND
                               THEN aux_class->implementation ) ) ).

        replace_texts( EXPORTING replacements     = replacements
                       CHANGING  abap_source_code = result-imp_abap_source_code ).

      CATCH cx_root INTO DATA(error).
        RAISE EXCEPTION TYPE zcx_shrinker EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD get_random_replacement_name.

    result = 'SHRI' && lcl_uuid=>get_c26( ).

  ENDMETHOD.


  METHOD get_table_type_key_components.

    result = concat_lines_of( sep   = ` `
                              table = REDUCE string_table(
                                        INIT a = VALUE string_table( )
                                        FOR <key_component> IN key_components
                                        WHERE ( typename   = typename
                                            AND seckeyname = keyname )
                                        NEXT a = VALUE #( BASE a ( |{ <key_component>-keyfield }| ) ) ) ).

  ENDMETHOD.


  METHOD on_program_generated.

    "=================================================
    " Recalculate WBCROSSGT
    "=================================================

    DATA(wb_crossreference) = NEW cl_wb_crossreference( p_name    = progname
                                                        p_include = '' ).
    wb_crossreference->index_actualize( IMPORTING p_error = DATA(error) ).

  ENDMETHOD.


  METHOD read_class_interface_includes.

    result-classes = VALUE #(
        FOR <class> IN classes
        WHERE ( clstype = '0' ) " class pools (not interface pools)
        LET aux_renamings = VALUE ty_cc_renamings(
            FOR <renaming> IN renamings
            WHERE ( class_name = <class>-name )
            ( <renaming> ) )
        IN
        ( name      = <class>-name
          clsccincl = <class>-clsccincl
          includes  = VALUE #(
                ( read_report_class_include( class_name = <class>-name
                                             extension  = 'CCAU'
                                             cc_renamings  = aux_renamings ) )
                ( read_report_class_include( class_name = <class>-name
                                             extension  = 'CCDEF'
                                             cc_renamings  = aux_renamings ) )
                ( read_report_class_include( class_name = <class>-name
                                             extension  = 'CCIMP'
                                             cc_renamings  = aux_renamings ) )
                ( read_report_class_include( class_name = <class>-name
                                             extension  = 'CCMAC'
                                             cc_renamings  = aux_renamings ) )
                ( read_report_class_include( class_name = <class>-name
                                             extension  = 'CI' ) )
                ( read_report_class_include( class_name = <class>-name
                                             extension  = 'CL' ) )
                " Methods = CM### extensions
                ( LINES OF VALUE #(
                    FOR <method> IN class_methods
                    WHERE ( classname = <class>-name )
                    ( read_report_class_include( class_name  = <class>-name
                                                 extension   = convert_methodindx_to_extensio( <method>-methodindx )
                                                 method_name = <method>-methodname ) ) ) )
                ( read_report_class_include( class_name = <class>-name
                                             extension  = 'CO' ) )
                ( read_report_class_include( class_name = <class>-name
                                             extension  = 'CP' ) )
                ( read_report_class_include( class_name = <class>-name
                                             extension  = 'CU' ) ) ) ) ).

    result-interfaces = VALUE #(
        FOR <object> IN interfaces
        LET intf_name = CONV seoclsname( <object>-obj_name )
        IN
        ( name     = <object>-obj_name
          includes = VALUE #(
                ( read_report_class_include( class_name = intf_name
                                             extension  = 'IP' ) )
                ( read_report_class_include( class_name = intf_name
                                             extension  = 'IU' ) ) ) ) ).

  ENDMETHOD.


  METHOD read_report.

    READ REPORT include_name INTO result.

  ENDMETHOD.


  METHOD read_report_class_include.

    result = VALUE #(
        LET include_name = get_class_include_name( class_name = class_name
                                                   extension  = extension )
        IN
        include_name     = include_name
        extension_code   = extension
        method_name      = method_name
        abap_source_code = read_report( include_name ) ).

    LOOP AT cc_renamings REFERENCE INTO DATA(renaming).
      REPLACE ALL OCCURRENCES OF
            REGEX |\\<{ renaming->posix_regex }\\>| ##REGEX_POSIX
            IN TABLE result-abap_source_code
            WITH renaming->with
            IGNORING CASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD remove_test_classes.
    DATA class_2 TYPE REF TO ty_scanned_class.
    DATA: abap_line TYPE REF TO string,
          friend    TYPE REF TO zcl_shrinker_abap_scan=>ty_stokes.

    LOOP AT abap_source_code REFERENCE INTO DATA(class).

      DATA(classes) = VALUE ty_scanned_classes( ).

      get_class_endclass_positions( EXPORTING itab = class->definition
                                              cu_co_ci = abap_true
                                    CHANGING  classes = classes ).

      get_class_endclass_positions( EXPORTING itab = class->implementation
                                              cu_co_ci = abap_false
                                    CHANGING  classes = classes ).

      "====================================
      " get the names of test classes
      "====================================
      TYPES ty_class_names TYPE HASHED TABLE OF seoclsname WITH UNIQUE KEY table_line.
      DATA(test_class_names) = VALUE ty_class_names( ).

      " Don't remove now the lines CLASS ... DEFINITION DEFERRED because they may
      " refer to test classes although there is no FOR TESTING. If it's a test
      " class, the DEFERRED line will have to be removed too.
      " Same remark for LOCAL FRIENDS.
      LOOP AT classes REFERENCE INTO class_2
            WHERE is_test_class = abap_true.
        INSERT class_2->name INTO TABLE test_class_names.
      ENDLOOP.

      "====================================
      " keep only classes to delete (not test classes)
      "====================================
      DATA(test_classes) = classes.
      LOOP AT test_classes REFERENCE INTO DATA(test_class).
        IF NOT line_exists( test_class_names[ table_line = test_class->name ] ).
          DELETE test_classes
                WHERE name          = test_class->name
                  AND is_definition = test_class->is_definition
                  AND is_deferred   = test_class->is_deferred
                  AND local_friends = test_class->local_friends.
        ENDIF.
      ENDLOOP.

      "====================================
      " Replace test classes from local friends (CLASS ... DEFINITION ... LOCAL FRIENDS lcl_1 ... ltc_1 ltc_2 ...)
      "   or friends (CLASS ... DEFINITION ... FRIENDS lcl_1 ... ltc_1 ltc_2 ...)
      "   with spaces.
      " Remove whole class definition if all friends after "LOCAL FRIENDS" have been removed (case it concerns class pool, the main class pool definition remains intact).
      " Remove only "FRIENDS" if all friends have been removed (case class is local class).
      "====================================
      LOOP AT classes REFERENCE INTO class_2
          WHERE any_friends = abap_true.

        TYPES ty_ref_abap_source_code TYPE REF TO ty_abap_source_code.
        DATA(ref_abap_source_code) = COND ty_ref_abap_source_code(
                                        WHEN class_2->cu_co_ci = abap_true THEN
                                            REF #( class->definition )
                                        ELSE
                                            REF #( class->implementation ) ).

        DATA(friends_to_remove) = VALUE zcl_shrinker_abap_scan=>ty_ut_stokes( ).
        LOOP AT class_2->statement_tokens REFERENCE INTO friend
            FROM class_2->tabix_first_friend.
          IF line_exists( test_classes[ name = friend->str ] ).
            INSERT friend->* INTO TABLE friends_to_remove.
          ENDIF.
        ENDLOOP.

        IF friends_to_remove IS NOT INITIAL.
          DATA(total_number_friends) = lines( class_2->statement_tokens ) - class_2->tabix_first_friend + 1.
          IF lines( friends_to_remove ) = total_number_friends.
            "===================
            " Remove all friend test classes
            "===================
            IF class_2->statement_tokens[ class_2->tabix_local_or_friends ]-str = 'LOCAL'.
              " 1) If statement is CLASS ... LOCAL FRIENDS then comment out whole CLASS ... DEFINITION LOCAL FRIENDS...
              LOOP AT ref_abap_source_code->* REFERENCE INTO abap_line
                  FROM class_2->statement_tokens[ class_2->tabix_first_friend ]-row
                  TO class_2->statement_tokens[ lines( class_2->statement_tokens ) ]-row.
                abap_line->* = '*' && abap_line->*.
              ENDLOOP.
            ELSE.
              " 2) Else it's CLASS ... FRIENDS ... just remove FRIENDS and all classes after FRIENDS.

              " Remove "FRIENDS"
              INSERT class_2->statement_tokens[ class_2->tabix_local_or_friends ]
                    INTO TABLE friends_to_remove.

              " What is import is COL DESCENDING in case two tokens are on same line, first remove the rightmost word
              " (ROW could have been DESCENDING instead of ASCENDING)
              SORT friends_to_remove BY row ASCENDING col DESCENDING.

              " Remove test classes
              LOOP AT friends_to_remove REFERENCE INTO friend.
                abap_line = REF #( ref_abap_source_code->*[ friend->row ] OPTIONAL ).
                CHECK abap_line IS BOUND.
                REPLACE SECTION
                        OFFSET friend->col
                        LENGTH strlen( friend->str )
                        OF abap_line->*
                        WITH space.
              ENDLOOP.
            ENDIF.

          ELSE.
            "===================
            " Remove not all friends, remove only test classes
            "===================

            " What is import is COL DESCENDING in case two tokens are on same line, first remove the rightmost word
            " (ROW could have been DESCENDING instead of ASCENDING)
            SORT friends_to_remove BY row ASCENDING col DESCENDING.

            LOOP AT friends_to_remove REFERENCE INTO friend.
              abap_line = REF #( ref_abap_source_code->*[ friend->row ] OPTIONAL ).
              CHECK abap_line IS BOUND.
              REPLACE SECTION
                      OFFSET friend->col
                      LENGTH strlen( friend->str )
                      OF abap_line->*
                      WITH space.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDLOOP.

      "====================================
      " delete ABAP lines corresponding to test classes
      " (definition deferred, definition, implementation)
      "====================================

      SORT test_classes BY cu_co_ci DESCENDING start_line DESCENDING.
      LOOP AT test_classes REFERENCE INTO class_2.

        IF class_2->cu_co_ci = abap_true.
          DELETE class->definition
                FROM class_2->start_line
                TO   class_2->end_line.
        ELSE.
          DELETE class->implementation
                FROM class_2->start_line
                TO   class_2->end_line.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD replace_includes.

    DATA(source_unit) = REF #( source_units[ name = program_name ] ).

    SORT source_unit->include_statements BY row DESCENDING.

    LOOP AT source_unit->include_statements REFERENCE INTO DATA(include_statement).

      replace_includes( EXPORTING program_name = include_statement->name
                        CHANGING  source_units = source_units ).

      DATA(line_index) = include_statement->stokes[ 1 ]-row.
      DATA(source_unit_2) = REF #( source_units[ name = include_statement->stokes[ 2 ]-str ] OPTIONAL ).
      IF source_unit_2 IS NOT BOUND.
        source_unit->abap_source_code[ line_index ] = '*' && source_unit->abap_source_code[ line_index ].
      ELSE.
        DATA(replaced_abap_line) = source_unit->abap_source_code[ line_index ].
        DELETE source_unit->abap_source_code INDEX line_index.
        INSERT LINES OF source_unit_2->abap_source_code INTO source_unit->abap_source_code INDEX line_index.
        INSERT |*{ replaced_abap_line }| INTO source_unit->abap_source_code INDEX line_index.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD replace_texts.

    LOOP AT replacements REFERENCE INTO DATA(local_obj_renaming).

      FIND ALL OCCURRENCES OF
            REGEX local_obj_renaming->posix_regex ##REGEX_POSIX
            IN TABLE abap_source_code
            IGNORING CASE
            RESULTS DATA(matches).

      SORT matches BY line ASCENDING offset DESCENDING.

      LOOP AT matches REFERENCE INTO DATA(match).
        DATA(line) = REF #( abap_source_code[ match->line ] ).
        DATA(old_name) = substring( val = line->* off = match->offset len = match->length ).

        IF old_name CP 'zcl_abapgit_data_injector'.
          ASSERT 1 = 1. " debug helper
        ENDIF.

        IF line->* CS '>' AND line->* CS '~'.
          ASSERT 1 = 1. " debug helper
        ENDIF.

        DATA(do_replace) = abap_false.
        IF local_obj_renaming->start_of_abap_word = abap_false
            AND local_obj_renaming->member_interface_prefix = abap_false.
          do_replace = abap_true.
        ELSE.
          DATA(abap_statement) = zcl_shrinker_abap_scan=>get_abap_statement_at_cursor(
                                    it_source = abap_source_code
                                    i_linenr  = match->line ).
          IF local_obj_renaming->start_of_abap_word = abap_true
                AND line_exists( abap_statement-stokes[ row = match->line
                                                        col = match->offset ] ).
            do_replace = abap_true.
          ENDIF.
          IF do_replace = abap_false
                AND local_obj_renaming->member_interface_prefix = abap_true.
            DATA(tabix) = 0.
            LOOP AT abap_statement-stokes REFERENCE INTO DATA(token)
                WHERE row = match->line.
              IF token->col > match->offset.
                EXIT.
              ENDIF.
              tabix = sy-tabix.
            ENDLOOP.
            IF tabix > 0.
              token = REF #( abap_statement-stokes[ tabix ] ).
              DATA(token_offset) = match->offset - token->col - 2.
              IF token_offset >= 0
                    AND strlen( token->str ) >= token_offset + 2.
                CASE token->str+token_offset(2).
                  WHEN '->' OR '=>'.
                    do_replace = abap_true.
                ENDCASE.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        IF do_replace = abap_true.
          DATA(new_name) = replace( val   = old_name
                                    regex = local_obj_renaming->posix_regex ##REGEX_POSIX
                                    with  = local_obj_renaming->with
                                    case  = abap_false ).
          REPLACE SECTION OFFSET match->offset LENGTH match->length OF line->* WITH new_name.
        ENDIF.

        ASSERT 1 = 1. " debug helper
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD select_ddic_objects.

    SELECT dd04l~rollname, dd04l~domname, dd04l~reftype, dd04l~datatype, dd04l~leng, dd04l~decimals
            FROM dd04l
            INNER JOIN tadir
              ON tadir~obj_name = dd04l~rollname
            WHERE tadir~object = 'DTEL'
              AND tadir~devclass IN @package_range
              AND tadir~object   IN @range_of_obj_type
              AND tadir~obj_name IN @range_of_obj_name
              AND dd04l~as4local = 'A'
            INTO TABLE @result-data_elements.

    SELECT dd02l~tabname, dd02l~tabclass
            FROM dd02l
            INNER JOIN tadir
              ON tadir~obj_name = dd02l~tabname
            WHERE tadir~object = 'TABL'
              AND tadir~devclass IN @package_range
              AND tadir~object   IN @range_of_obj_type
              AND tadir~obj_name IN @range_of_obj_name
              AND dd02l~as4local = 'A'
            INTO TABLE @result-structures.

    SELECT dd03l~tabname, dd03l~position, dd03l~fieldname, dd03l~rollname, dd03l~precfield, dd03l~comptype, dd03l~reftype, dd03l~datatype, dd03l~leng, dd03l~decimals
            FROM dd03l
            INNER JOIN tadir
              ON tadir~obj_name = dd03l~tabname
            WHERE tadir~object = 'TABL'
              AND tadir~devclass IN @package_range
              AND tadir~object   IN @range_of_obj_type
              AND tadir~obj_name IN @range_of_obj_name
              AND dd03l~as4local = 'A'
              AND dd03l~adminfield = '0' " skip components added via ".INCLUDE structure"
              AND dd03l~depth = 0        " skip components added via "component TYPE structure"
            INTO TABLE @result-structure_components.
    SORT result-structure_components BY tabname position.

    SELECT dd40l~typename, dd40l~rowkind, dd40l~reftype, dd40l~rowtype, dd40l~keydef, dd40l~keykind, dd40l~ttypkind, dd40l~accessmode, dd40l~datatype, dd40l~leng, dd40l~decimals
            FROM dd40l
            INNER JOIN tadir
              ON tadir~obj_name = dd40l~typename
            WHERE tadir~object = 'TTYP'
              AND tadir~devclass IN @package_range
              AND tadir~object   IN @range_of_obj_type
              AND tadir~obj_name IN @range_of_obj_name
              AND dd40l~as4local = 'A'
            INTO TABLE @result-table_types.

    SELECT dd42s~typename, dd42s~seckeyname, dd42s~keyfdpos, dd42s~keyfield
            FROM dd42s
            INNER JOIN tadir
              ON tadir~obj_name = dd42s~typename
            WHERE tadir~object = 'TTYP'
              AND tadir~devclass IN @package_range
              AND tadir~object   IN @range_of_obj_type
              AND tadir~obj_name IN @range_of_obj_name
              AND dd42s~as4local = 'A'
            INTO TABLE @result-table_type_key_components.
    SORT result-table_type_key_components BY typename seckeyname keyfdpos.

    SELECT dd43l~typename, dd43l~seckeyname, dd43l~seckeyunique, dd43l~accessmode, dd43l~kind
            FROM dd43l
            INNER JOIN tadir
              ON tadir~obj_name = dd43l~typename
            WHERE tadir~object = 'TTYP'
              AND tadir~devclass IN @package_range
              AND tadir~object   IN @range_of_obj_type
              AND tadir~obj_name IN @range_of_obj_name
              AND dd43l~as4local = 'A'
            INTO TABLE @result-table_type_sec_keys.

  ENDMETHOD.


  METHOD select_miscellaneous.

    IF objects IS INITIAL.
      SELECT pgmid, object, obj_name
          FROM tadir
          WHERE devclass IN @package_range
            AND object NOT IN ('DOMA')
            AND object IN @range_of_obj_type
            AND obj_name IN @range_of_obj_name
          INTO TABLE @result-all_objects.
    ELSE.
      SELECT pgmid, object, obj_name
          FROM tadir
          FOR ALL ENTRIES IN @objects
          WHERE pgmid = @objects-pgmid
            AND object = @objects-object
            AND obj_name = @objects-obj_name
            AND devclass IN @package_range
            AND object NOT IN ('DOMA')
            AND object IN @range_of_obj_type
            AND obj_name IN @range_of_obj_name
          INTO TABLE @result-all_objects.
    ENDIF.

    " CLSNAME               RELTYPE  REFCLSNAME
    " ====================  =======  ====================
    " ZCL_EXCEL             1        ZIF_EXCEL_BOOK_PROPERTIES
    " ZCL_EXCEL_GRAPH_BARS  2        ZCL_EXCEL_GRAPH
    " ZCX_EXCEL             2        CX_STATIC_CHECK


    " At statement "CLASS zcl_excel_graph_bars DEFINITION INHERITING FROM zcl_excel_graph", this syntax error MESSAGEG)F:
    " "Components of classes declared using "CLASS ZCL_EXCEL_GRAPH DEFINITION DEFERRED" can only be accessed after the class is defined (CLASS ZCL_EXCEL_GRAPH DEFINITION)."

    SELECT seometarel~clsname, seometarel~reltype, seometarel~refclsname
            FROM seometarel
            INNER JOIN tadir
              ON tadir~obj_name = seometarel~clsname
            WHERE tadir~object   IN ('CLAS','INTF')
              AND tadir~object   IN @range_of_obj_type
              AND tadir~devclass IN @package_range
              AND tadir~obj_name IN @range_of_obj_name
            INTO TABLE @result-oo_relationships.


    result-used_classes = select_used_classes(
                            package_range     = package_range
                            range_of_obj_name = range_of_obj_name
                            range_of_obj_type = range_of_obj_type
                            all_objects       = result-all_objects ).


    " NAME                  CLSCCINCL
    " ====================  =========
    " ZCL_EXCEL             X
    " ZCL_EXCEL_AUTOFILTER  X
    SELECT seoclassdf~clsname, seoclass~clstype, seoclassdf~clsccincl, seoclassdf~category
        FROM seoclassdf
            INNER JOIN seoclass
              ON seoclass~clsname = seoclassdf~clsname
            INNER JOIN tadir
              ON tadir~obj_name = seoclassdf~clsname
        WHERE tadir~object   IN ('CLAS','INTF')
          AND tadir~object   IN @range_of_obj_type
          AND tadir~devclass IN @package_range
          AND tadir~obj_name IN @range_of_obj_name
        INTO TABLE @result-class_descriptions.


    " CLASSNAME  METHODINDX  METHODNAME
    " =========  ==========  =======================
    " ZCL_EXCEL  00000
    " ZCL_EXCEL  00001       ADD_NEW_AUTOFILTER
    " ZCL_EXCEL  00002       ADD_NEW_COMMENT
    SELECT tmdir~classname, tmdir~methodindx, tmdir~methodname
        FROM tmdir
            INNER JOIN tadir
              ON tadir~obj_name = tmdir~classname
        WHERE tadir~object   IN ('CLAS','INTF')
          AND tadir~object   IN @range_of_obj_type
          AND tadir~devclass IN @package_range
          AND tadir~obj_name IN @range_of_obj_name
        INTO TABLE @result-class_methods.

  ENDMETHOD.


  METHOD select_used_classes.

    TYPES:
      BEGIN OF ty_tadir_main_program,
        pgmid             TYPE tadir-pgmid,
        object            TYPE tadir-object,
        obj_name          TYPE tadir-obj_name,
        devclass          TYPE tadir-devclass,
        main_program_name TYPE syrepid,
      END OF ty_tadir_main_program.
    TYPES ty_tadir_main_programs TYPE STANDARD TABLE OF ty_tadir_main_program WITH EMPTY KEY.

*    TYPES:
*      ty_using_object_type TYPE zshrinker_gtt_mp-object,
*      ty_using_object_name TYPE zshrinker_gtt_mp-obj_name,
*      ty_used_object_type  TYPE c LENGTH 4,
*      ty_used_object_name  TYPE zshrinker_gtt_ec-exception_class_name.
*    TYPES main_program_name TYPE syrepid.
*    TYPES object TYPE tadir-object.
*    TYPES obj_name TYPE tadir-obj_name.
*    TYPES pgmid TYPE tadir-pgmid.
*    TYPES devclass TYPE tadir-devclass.
*    DATA ty_tadir_main_programs TYPE ty_tadir_main_programs.

    " get_used_classes
    "=================================================
    " Make sure D010INC and WBCROSSGT are up-to-date
    "=================================================
    DATA(tadir_main_programs) = VALUE ty_tadir_main_programs( ).
    SELECT tadir~pgmid,
           tadir~object,
           tadir~obj_name,
           tadir~devclass
*           CAST( CASE
*           WHEN ( tadir~object = 'FUGR' OR tadir~object = 'FUGS' )
*                AND left( tadir~obj_name, 1 ) = '/'
*                THEN concat( '/', replace( substring( tadir~obj_name, 2, 39 ), '/', '/SAPL' ) )
*           WHEN ( tadir~object = 'FUGR' OR tadir~object = 'FUGS' )
*                AND left( tadir~obj_name, 1 ) <> '/'
*                THEN concat( 'SAPL', tadir~obj_name )
*           WHEN tadir~object = 'CLAS'
*                THEN concat( rpad( tadir~obj_name, 30, '=' ), 'CP' )
*           WHEN tadir~object = 'INTF'
*                THEN concat( rpad( tadir~obj_name, 30, '=' ), 'IP' )
*           WHEN tadir~object = 'CNTX'
*                THEN concat( 'CONTEXT_X_', tadir~obj_name )
*           ELSE " PROG
*                tadir~obj_name
*           END AS CHAR( 40 ) ) AS main_program_name
        FROM tadir
        WHERE tadir~devclass IN @package_range
          AND tadir~object   IN ('CLAS','CNTX','FUGR','FUGS','INTF','PROG')
          AND tadir~object   IN @range_of_obj_type
          AND tadir~obj_name IN @range_of_obj_name
        INTO TABLE @tadir_main_programs.

    LOOP AT tadir_main_programs REFERENCE INTO DATA(tadir_main_program).
      DATA(object) = tadir_main_program->object.
      DATA(obj_name) = tadir_main_program->obj_name.
      DATA main_program_name TYPE sy-repid.

      main_program_name = zcl_shrinker_utils=>get_main_program_name( object = object
                                                                     obj_name = obj_name ).

      tadir_main_program->main_program_name = main_program_name.
    ENDLOOP.

    TYPES ty_table_zshrinker_gtt_mp TYPE STANDARD TABLE OF zshrinker_gtt_mp WITH EMPTY KEY.
    TYPES ty_table_zshrinker_gtt_in TYPE STANDARD TABLE OF zshrinker_gtt_in WITH EMPTY KEY.

    DATA(uuid) = lcl_uuid=>get_x16( ).
    CALL FUNCTION 'ENQUEUE_EZSHRINKER_UUID'
      EXPORTING
*       mode_zshrinker_gtt_ec = 'E'              " Lock mode for table ZSHRINKER_GTT_EC
        uuid           = uuid
*       x_uuid         = space            " Fill argument 01 with initial value?
*       _scope         = '2'
*       _wait          = space
*       _collect       = ' '              " Initially only collect lock
      EXCEPTIONS
        foreign_lock   = 1                " Object already locked
        system_failure = 2                " Internal error from enqueue server
        OTHERS         = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_shrinker EXPORTING text = 'UUID &1 cannot be locked (RC &2)' msgv1 = |{ uuid }| msgv2 = |{ sy-subrc }|.
*   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    DATA(table_zshrinker_gtt_mp) = VALUE ty_table_zshrinker_gtt_mp(
                            FOR <tadir_main_program> IN tadir_main_programs
                            ( uuid              = uuid
                              pgmid             = <tadir_main_program>-pgmid
                              object            = <tadir_main_program>-object
                              obj_name          = <tadir_main_program>-obj_name
                              devclass          = <tadir_main_program>-devclass
                              main_program_name = <tadir_main_program>-main_program_name ) ).
    INSERT zshrinker_gtt_mp FROM TABLE @table_zshrinker_gtt_mp.


    DATA(table_zshrinker_gtt_in) = VALUE ty_table_zshrinker_gtt_in(
                            FOR <tadir_main_program> IN tadir_main_programs
                            WHERE ( object = 'CLAS'
                                 OR object = 'INTF' )
                            FOR <suffix> IN SWITCH string_table( <tadir_main_program>-object
                                WHEN 'CLAS' THEN VALUE #(
                                    ( `CU` ) ( `CI` ) ( `CO` ) ( `CCIMP` ) ( `CCDEF` ) ( `CCAU` ) ( `CCMAC` ) )
                                WHEN 'INTF' THEN VALUE #(
                                    ( `IU` ) ) )
                            ( uuid              = uuid
                              pgmid             = <tadir_main_program>-pgmid
                              object            = <tadir_main_program>-object
                              obj_name          = <tadir_main_program>-obj_name
                              include           = |{ <tadir_main_program>-obj_name WIDTH = 30 PAD = '=' }{ <suffix> }| ) ).
    INSERT zshrinker_gtt_in FROM TABLE @table_zshrinker_gtt_in.


    DATA(lo_prog) = NEW lcl_program_load( ).

    lo_prog->select_prog( it_rng_prog = VALUE #( FOR <tadir_main_program> IN tadir_main_programs
                                                 ( sign   = 'I'
                                                   option = 'EQ'
                                                   low    = <tadir_main_program>-main_program_name ) ) ).

    lo_prog->filter_prog( generatd = abap_false ).

    " Used to regenerate WBCROSSGT
    SET HANDLER on_program_generated FOR lo_prog.

    lo_prog->gen_progs( i_commit_frequency = 10 ).


    " The definition of exception classes must be positioned before any class definition containing RAISING of this exception class,
    " because the compiler checks that the mentioned class is an exception class, otherwise syntax error:
    " The class "&1" was not derived from either "CX_STATIC_CHECK" or "CX_DYNAMIC_CHECK".
    "
    " TADIR
    " OBJECT  OBJ_NAME
    " ======  ====================
    " CLAS    ZCL_EXCEL
    " CLAS    ZCX_EXCEL
    " INTF    ZIF_EXCEL_WRITER



    " WBCROSSGT contains a few groups of relationships:
    "
    " 1) The ones due to "INTERFACES ...", here "INTERFACES zif_excel_book_properties" in ZCL_EXCEL:
    "    OTYPE  NAME                       INCLUDE
    "    =====  =========================  ==================================
    "    TY     ZIF_EXCEL_BOOK_PROPERTIES  ZCL_EXCEL=====================CU
    "
    " 1) The ones due to "METHODS ... IMPORTING ... TYPE ... DEFAULT otherclassinterface=>attribute",
    "    here "METHODS add_new_drawing IMPORTING ... DEFAULT zcl_excel_drawing=>type_image" for ZCL_EXCEL:
    "    OTYPE  NAME               INCLUDE
    "    =====  =================  ==================================
    "    TY     ZCL_EXCEL_DRAWING  ZCL_EXCEL=====================CU
    "
    " ERROR "Direct access to components of the global class ZCL_EXCEL_DRAWING is not possible. The statement CLASS ZCL_EXCEL_DRAWING DEFINITION LOAD is missing."
    " In ZCL_EXCEL:
    "   METHODS add_new_drawing
    "     IMPORTING
    "       !ip_type TYPE zexcel_drawing_type DEFAULT zcl_excel_drawing=>type_image " <=== "CLASS zcl_excel_drawing DEFINITION"... should be located above
    "    OTYPE  NAME                                        INCLUDE
    "    =====  =========================================   ==================================
    "    DA     ZCL_EXCEL_DRAWING\DA:TYPE_IMAGE             ZCL_EXCEL=====================CU

    "   METHODS xx
    "     IMPORTING
    "       xx TYPE ZIF_ABAPGIT_AJSON_FILTER=>TY_FILTER_TAB
    "    OTYPE  NAME                                        INCLUDE
    "    =====  =========================================   ==================================
    "    TY     ZIF_ABAPGIT_AJSON_FILTER\TY:TY_FILTER_TAB   ZCL_ABAPGIT_AJSON_FILTER_LIB==CCAU


    " The following SELECT will result in, which must be interpreted as declare first ZCX_EXCEL,
    " then ZCL_EXCEL and ZIF_EXCEL_CONVERTER in any order:
    "    USING_OBJECT_TYPE  USING_OBJECT_NAME    USED_OBJECT_TYPE  USED_OBJECT_NAME
    "    ===========================================================================
    "    CLAS               ZCL_EXCEL            CLAS              ZCX_EXCEL
    "    CLAS               ZCX_EXCEL            CLAS              CX_STATIC_CHECK
    "    CLAS               ZCX_EXCEL            CLAS              ZCX_EXCEL             "<==== meaningless link to be removed
    "    INTF               ZIF_EXCEL_CONVERTER  CLAS              ZCX_EXCEL
    "
    " 2) The ones due to "INHERITING FROM cx_static_check" (or any other exception class), here for ZCX_EXCEL:
    "    (wbcrossgt_cx below)
    "    OTYPE  NAME       INCLUDE
    "    =====  =========  ==================================
    "    TY     CX_ROOT    ZCX_EXCEL=====================CU
*    WITH
*    +exception_class AS (
    SELECT seoclassdf~clsname AS name
        FROM seoclassdf
        INNER JOIN tadir
            ON tadir~pgmid  = 'R3TR'
           AND tadir~object = 'CLAS'
           AND tadir~obj_name = seoclassdf~clsname
        WHERE seoclassdf~category = '40'
          AND tadir~devclass IN @package_range
          AND tadir~object   IN @range_of_obj_type
          AND tadir~obj_name IN @range_of_obj_name
        INTO TABLE @DATA(table_seoclassdf_ec).

    TYPES ty_table_zshrinker_gtt_ec TYPE STANDARD TABLE OF zshrinker_gtt_ec WITH EMPTY KEY.
*    TYPES: BEGIN OF ty_used_class,
*             using_object_type TYPE zshrinker_gtt_mp-object,
*             using_object_name TYPE zshrinker_gtt_mp-obj_name,
*             used_object_type  TYPE c LENGTH 4,
*             used_object_name  TYPE zshrinker_gtt_ec-exception_class_name,
*           END OF ty_used_class.
*    TYPES ty_used_classes TYPE STANDARD TABLE OF ty_used_class WITH EMPTY KEY.

    DATA(table_zshrinker_gtt_ec) = VALUE ty_table_zshrinker_gtt_ec(
                            FOR <seoclassdf_ec> IN table_seoclassdf_ec
                            ( uuid                 = uuid
                              exception_class_name = <seoclassdf_ec>-name ) ).
    INSERT zshrinker_gtt_ec FROM TABLE @table_zshrinker_gtt_ec.
*    ),
*    " TADIR              D010INC-MASTER
*    "==================  ==================================
*    " R3TR PROG XXXXX    XXXXX
*    " R3TR FUGR XXXXX    SAPLXXXXX
*    " R3TR FUGR /XX/XXX  /XX/SAPLXXX
*    " R3TR CLAS XXXXX    XXXXX=========================CP
**    +tadir_master AS (
*    SELECT tadir~pgmid,
*           tadir~object,
*           tadir~obj_name,
*           tadir~devclass,
*           CASE
*           WHEN ( tadir~object = 'FUGR' OR tadir~object = 'FUGS' )
*                AND left( tadir~obj_name, 1 ) = '/'
*                THEN concat( '/', replace( substring( tadir~obj_name, 2, 39 ), '/', '/SAPL' ) )
*           WHEN ( tadir~object = 'FUGR' OR tadir~object = 'FUGS' )
*                AND left( tadir~obj_name, 1 ) <> '/'
*                THEN concat( 'SAPL', tadir~obj_name )
*           WHEN tadir~object = 'CLAS'
*                THEN concat( rpad( tadir~obj_name, 30, '=' ), 'CP' )
*           WHEN tadir~object = 'INTF'
*                THEN concat( rpad( tadir~obj_name, 30, '=' ), 'IP' )
*           WHEN tadir~object = 'CNTX'
*                THEN concat( 'CONTEXT_X_', tadir~obj_name )
*           ELSE " PROG
*                tadir~obj_name
*           END AS master
*        FROM tadir
*        WHERE tadir~devclass IN @package_range
*          AND tadir~object   IN ('CLAS','CNTX','FUGR','FUGS','INTF','PROG')
*          AND tadir~object   IN @range_of_obj_type
*          AND tadir~obj_name IN @range_of_obj_name
*    )

    " 1) The ones due to "METHODS ... RAISING ...", here for ZCL_EXCEL and ZIF_EXCEL_WRITER which are using ZCX_EXCEL:
    "    (wbcrossgt below)
    "    OTYPE  NAME       INCLUDE
    "    =====  =========  ==================================
    "    TY     ZCX_EXCEL  ZCL_EXCEL=====================CU
    "    TY     ZCX_EXCEL  ZIF_EXCEL_WRITER==============IU
    "    TY     ZCX_EXCEL  ZCL_EXCEL_WRITER_CSV==========CI
*    SELECT DISTINCT
*            +tadir_master~object   AS using_object_type,
*            +tadir_master~obj_name AS using_object_name,
*            'CLAS'                 AS used_object_type,
*            +exception_class~name  AS used_object_name
*        FROM wbcrossgt
*        INNER JOIN +exception_class
*            ON +exception_class~name = wbcrossgt~name
*        INNER JOIN d010inc
*            ON d010inc~include = wbcrossgt~include
*        INNER JOIN +tadir_master
*            ON +tadir_master~master = d010inc~master
*        WHERE wbcrossgt~otype        =  'TY'
*          AND +tadir_master~devclass IN @package_range
*          AND +tadir_master~object   IN @range_of_obj_type
*          AND +tadir_master~obj_name IN @range_of_obj_name
*          AND +tadir_master~obj_name <> +exception_class~name " remove meaningless link CLAS ZCX_EXCEL ZCX_EXCEL
*    UNION
    DATA(used_classes) = VALUE ty_used_classes( ).

    SELECT DISTINCT
            zshrinker_gtt_mp~object               AS using_object_type,
            zshrinker_gtt_mp~obj_name             AS using_object_name,
            'CLAS'                                AS used_object_type,
            zshrinker_gtt_ec~exception_class_name AS used_object_name
        FROM wbcrossgt
        INNER JOIN zshrinker_gtt_ec
            ON zshrinker_gtt_ec~exception_class_name = wbcrossgt~name
        INNER JOIN d010inc
            ON d010inc~include = wbcrossgt~include
        INNER JOIN zshrinker_gtt_mp
            ON zshrinker_gtt_mp~main_program_name = d010inc~master
        WHERE wbcrossgt~otype           =  'TY'
          AND zshrinker_gtt_mp~devclass IN @package_range
          AND zshrinker_gtt_mp~object   IN @range_of_obj_type
          AND zshrinker_gtt_mp~obj_name IN @range_of_obj_name
          AND zshrinker_gtt_mp~obj_name <> zshrinker_gtt_ec~exception_class_name " remove meaningless link CLAS ZCX_EXCEL ZCX_EXCEL
        APPENDING TABLE @used_classes.

    " WBCROSSGT:
    "    OTYPE  NAME                             INCLUDE
    "    =====  ===============================  ==================================
    "    DA     ZCL_EXCEL_DRAWING\DA:TYPE_IMAGE  ZCL_EXCEL=====================CU
    SELECT DISTINCT
            zshrinker_gtt_in~object   AS using_object_type,
            zshrinker_gtt_in~obj_name AS using_object_name,
            'CLIF'                    AS used_object_type, " CLAS or INTF ?
            wbcrossgt~name            AS used_object_name
        FROM wbcrossgt
            INNER JOIN zshrinker_gtt_in
                ON zshrinker_gtt_in~include = wbcrossgt~include
        WHERE zshrinker_gtt_in~uuid   =    @uuid
          AND zshrinker_gtt_in~object IN   ('CLAS','INTF')
          AND wbcrossgt~name          LIKE '%\%'
        INTO TABLE @DATA(wbcrossgt_lines).

    LOOP AT wbcrossgt_lines REFERENCE INTO DATA(wbcrossgt_line).
      wbcrossgt_line->used_object_name = substring_before( val = wbcrossgt_line->used_object_name
                                                       sub = '\' ).
      IF wbcrossgt_line->using_object_name = wbcrossgt_line->used_object_name.
        DELETE wbcrossgt_lines USING KEY loop_key.
      ELSEIF line_exists( all_objects[ pgmid    = 'R3TR'
                                       object   = 'CLAS'
                                       obj_name = wbcrossgt_line->used_object_name ] ).
        wbcrossgt_line->used_object_type = 'CLAS'.
      ELSEIF line_exists( all_objects[ pgmid    = 'R3TR'
                                       object   = 'INTF'
                                       obj_name = wbcrossgt_line->used_object_name ] ).
        wbcrossgt_line->used_object_type = 'INTF'.
      ELSE.
        DELETE wbcrossgt_lines USING KEY loop_key.
      ENDIF.

      IF wbcrossgt_line IS BOUND.
        INSERT VALUE #( using_object_type = wbcrossgt_line->using_object_type
                        using_object_name = wbcrossgt_line->using_object_name
                        used_object_type  = wbcrossgt_line->used_object_type
                        used_object_name  = wbcrossgt_line->used_object_name )
              INTO TABLE used_classes.
      ENDIF.
    ENDLOOP.

*    SELECT DISTINCT
*            tadir~object   AS using_object_type,
*            tadir~obj_name AS using_object_name,
*            'CLIF'         AS used_object_type, " CLAS or INTF ?
*            wbcrossgt~name AS used_object_name  " DA     ZCL_EXCEL_DRAWING\DA:TYPE_IMAGE   ZCL_EXCEL=====================CU
*        FROM wbcrossgt
*            CROSS JOIN tadir
*        WHERE ( concat( rpad( tadir~obj_name, 30, '=' ), 'CU'    ) = wbcrossgt~include
*             OR concat( rpad( tadir~obj_name, 30, '=' ), 'CI'    ) = wbcrossgt~include
*             OR concat( rpad( tadir~obj_name, 30, '=' ), 'CO'    ) = wbcrossgt~include
*             OR concat( rpad( tadir~obj_name, 30, '=' ), 'CCIMP' ) = wbcrossgt~include
*             OR concat( rpad( tadir~obj_name, 30, '=' ), 'CCDEF' ) = wbcrossgt~include
*             OR concat( rpad( tadir~obj_name, 30, '=' ), 'CCAU'  ) = wbcrossgt~include
*             OR concat( rpad( tadir~obj_name, 30, '=' ), 'CCMAC' ) = wbcrossgt~include
*             OR concat( rpad( tadir~obj_name, 30, '=' ), 'IU'    ) = wbcrossgt~include )
*          AND tadir~object   =  'CLAS'
*          AND tadir~object   IN @range_of_obj_type
*          AND tadir~devclass IN @package_range
*          AND tadir~obj_name IN @range_of_obj_name
*          AND wbcrossgt~name LIKE '%\%'
*    UNION
*    SELECT DISTINCT
*            tadir~object   AS using_object_type,
*            tadir~obj_name AS using_object_name,
*            'CLIF'         AS used_object_type, " CLAS or INTF ?
*            wbcrossgt~name AS used_object_name  " DA     ZIF_WWWWWW\DA:TYPE_IMAGE   ZIF_XXXXX=====================IU
*        FROM wbcrossgt
*            CROSS JOIN tadir
*        WHERE concat( rpad( tadir~obj_name, 30, '=' ), 'IU' ) = wbcrossgt~include
*          AND tadir~object   =  'INTF'
*          AND tadir~object   IN @range_of_obj_type
*          AND tadir~devclass IN @package_range
*          AND tadir~obj_name IN @range_of_obj_name
*          AND wbcrossgt~name LIKE '%\%'
*    INTO TABLE @result-used_classes.

    SORT used_classes BY table_line.
    DELETE ADJACENT DUPLICATES FROM used_classes COMPARING table_line.


    CALL FUNCTION 'DEQUEUE_EZSHRINKER_UUID'
      EXPORTING
        uuid = uuid.

    DELETE FROM zshrinker_gtt_ec WHERE uuid = @uuid.
    DELETE FROM zshrinker_gtt_mp WHERE uuid = @uuid.
    DELETE FROM zshrinker_gtt_in WHERE uuid = @uuid.

    " At this point, used_classES contains:
    "
    " USING_OBJECT_TYPE  USING_OBJECT_NAME     USED_OBJECT_TYPE  USED_OBJECT_NAME
    " =================  ====================  ================  ===============================
    " CLAS               ZCL_EXCEL             CLAS              ZCX_EXCEL
    " INTF               ZIF_EXCEL_CONVERTER   CLAS              ZCX_EXCEL
    " CLAS               ZCL_EXCEL             CLIF              ZCL_EXCEL_DRAWING\DA:TYPE_IMAGE
    " CLAS               ZCL_EXCEL_AUTOFILTER  CLIF              ZCL_EXCEL_AUTOFILTER\TY:TT_FILTERS
    " CLAS               ZCL_EXCEL_AUTOFILTER  CLIF              ZCL_EXCEL_AUTOFILTER\TY:TS_FILTER
    " INTF               ZIF_EXCEL_READER      CLIF              SY\DA:BATCH
    " INTF               ZIF_EXCEL_READER      CLIF              SYST\TY:BATCH

*    " Replace CLIF with either CLAS or INTF.
*    LOOP AT used_classes REFERENCE INTO DATA(using_used_object_type)
*        WHERE used_object_type = 'CLIF'.
*      DATA(used_object_name) = using_used_object_type->used_object_name. " Save value before changed (Debug helper)
*      using_used_object_type->used_object_name = substring_before( val = using_used_object_type->used_object_name
*                                                                   sub = '\' ).
*      IF using_used_object_type->using_object_name = using_used_object_type->used_object_name.
*        DELETE used_classes USING KEY loop_key.
*      ELSEIF line_exists( all_objects[ pgmid    = 'R3TR'
*                                       object   = 'CLAS'
*                                       obj_name = using_used_object_type->used_object_name ] ).
*        using_used_object_type->used_object_type = 'CLAS'.
*      ELSEIF line_exists( all_objects[ pgmid    = 'R3TR'
*                                       object   = 'INTF'
*                                       obj_name = using_used_object_type->used_object_name ] ).
*        using_used_object_type->used_object_type = 'INTF'.
*      ELSE.
*        DELETE used_classes USING KEY loop_key.
*      ENDIF.
*    ENDLOOP.
*
*    SORT used_classes BY table_line.
*    DELETE ADJACENT DUPLICATES FROM used_classes COMPARING table_line.


    result = used_classes.

  ENDMETHOD.

ENDCLASS.
