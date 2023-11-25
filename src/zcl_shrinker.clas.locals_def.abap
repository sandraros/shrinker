*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

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
