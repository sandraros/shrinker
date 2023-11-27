CLASS zcl_shrinker_any_program DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES ty_abap_source_code TYPE zif_shrinker_abap_code_adapter=>ty_abap_source_code.
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


    CLASS-METHODS create
      IMPORTING
        customizer    TYPE REF TO zif_shrinker_abap_code_adapter OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_shrinker_any_program.

    METHODS get_abap_for_main_program
      IMPORTING
        main_program_name   TYPE syrepid
        global_replacements TYPE ty_obj_renamings OPTIONAL
      RETURNING
        VALUE(result)       TYPE ty_abap_source_code
      RAISING
        zcx_shrinker.

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter object | <p class="shorttext synchronized" lang="en">PROG, FUGR, CLAS or INTF</p>
    "! @parameter obj_name | <p class="shorttext synchronized" lang="en">Name of program, function group or class/interface pool</p>
    "! @parameter global_replacements | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
    "! @raising zcx_shrinker | <p class="shorttext synchronized" lang="en"></p>
    METHODS get_abap_for_program
      IMPORTING
        object              TYPE trobjtype
        obj_name            TYPE sobj_name
        global_replacements TYPE ty_obj_renamings OPTIONAL
      RETURNING
        VALUE(result)       TYPE ty_abap_source_code
      RAISING
        zcx_shrinker.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA replacements TYPE ty_obj_renamings .
    DATA customizer TYPE REF TO zif_shrinker_abap_code_adapter .
*    DATA program_name TYPE syrepid .



    METHODS read_includes
      CHANGING
        source_units TYPE zif_shrinker_abap_code_adapter=>ty_source_units
      RAISING
        zcx_shrinker.

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

ENDCLASS.



CLASS zcl_shrinker_any_program IMPLEMENTATION.


  METHOD create.

    result = NEW zcl_shrinker_any_program( ).
    result->customizer = customizer.

  ENDMETHOD.


  METHOD get_abap_for_main_program.

    TRY.

        me->replacements = global_replacements.

        SELECT COUNT(*)
            UP TO 1 ROWS
            FROM trdir
            WHERE name = @main_program_name.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_shrinker EXPORTING text = 'Program &1 does not exist'(002) msgv1 = main_program_name.
        ENDIF.

        DATA(source_units) = VALUE zif_shrinker_abap_code_adapter=>ty_source_units( ).

        INSERT VALUE #(
                name = main_program_name )
            INTO TABLE source_units
            REFERENCE INTO DATA(source_unit).

        READ REPORT main_program_name INTO source_unit->abap_source_code.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_shrinker EXPORTING text = 'Program &1 does not exist'(002) msgv1 = main_program_name.
        ENDIF.


        read_includes( CHANGING source_units = source_units ).

        IF customizer IS BOUND.

          customizer->adapt_source_code_before_rep_i( CHANGING source_units = source_units ).

          read_includes( CHANGING source_units = source_units ).

        ENDIF.

        replace_includes( EXPORTING program_name = main_program_name
                          CHANGING  source_units = source_units ).

        IF customizer IS BOUND.
          customizer->adapt_source_code( CHANGING other_source_units = source_units ).
        ENDIF.

        replace_texts( EXPORTING replacements     = replacements
                       CHANGING  abap_source_code = result ).


        result = source_units[ name = main_program_name ]-abap_source_code.


      CATCH zcx_shrinker INTO DATA(error_2).
        RAISE EXCEPTION error_2.
      CATCH cx_root INTO DATA(error).
        RAISE EXCEPTION TYPE zcx_shrinker EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD get_abap_for_program.

    DATA(main_program_name) = zcl_shrinker_utils=>get_main_program_name( object   = object
                                                                         obj_name = obj_name ).

    result = get_abap_for_main_program( main_program_name   = main_program_name
                                        global_replacements = global_replacements ).

  ENDMETHOD.


  METHOD read_includes.

    LOOP AT source_units REFERENCE INTO DATA(source_unit).

      source_unit->include_statements = VALUE #( ).

      FIND ALL OCCURRENCES
          OF 'INCLUDE'
          IN TABLE source_unit->abap_source_code
          RESULTS DATA(matches).

      LOOP AT matches REFERENCE INTO DATA(match).
        DATA(abap_statement) = zcl_shrinker_abap_scan=>get_abap_statement_at_cursor(
                                      it_source = source_unit->abap_source_code
                                      i_linenr  = match->line
                                      i_offset  = match->offset ).
        IF abap_statement-stokes IS NOT INITIAL
              AND abap_statement-stokes[ 1 ]-str = 'INCLUDE'.
          INSERT VALUE #(
                  row      = abap_statement-stokes[ 1 ]-row
                  stokes   = abap_statement-stokes
                  name     = abap_statement-stokes[ 2 ]-str
                  if_found = xsdbool( lines( abap_statement-stokes ) >= 4
                                  AND abap_statement-stokes[ 3 ]-str = 'IF'
                                  AND abap_statement-stokes[ 4 ]-str = 'FOUND' )
              ) INTO TABLE source_unit->include_statements.
        ENDIF.
      ENDLOOP.

      LOOP AT source_unit->include_statements REFERENCE INTO DATA(include_statement).

        IF NOT line_exists( source_units[ name = include_statement->name ] ).
          INSERT VALUE #(
                  name = include_statement->name )
              INTO TABLE source_units
              REFERENCE INTO DATA(source_unit_2).

          READ REPORT include_statement->name INTO source_unit_2->abap_source_code.
          IF sy-subrc <> 0
              AND include_statement->if_found = abap_false.
            RAISE EXCEPTION TYPE zcx_shrinker EXPORTING text = 'Program &1 does not exist'(002) msgv1 = include_statement->name.
          ENDIF.
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

ENDCLASS.
