*"* use this source file for your ABAP unit test classes

CLASS ltc_replace_texts DEFINITION DEFERRED.

CLASS zcl_shrinker_ddic_class_interf DEFINITION LOCAL FRIENDS
    ltc_replace_texts.


CLASS ltc_regex DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA: regex TYPE string.
    METHODS test FOR TESTING RAISING cx_static_check.
    METHODS setup.
ENDCLASS.


CLASS ltc_replace_texts DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS member_interface_prefix FOR TESTING RAISING cx_static_check.
    METHODS start_of_abap_word FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_regex IMPLEMENTATION.

  METHOD setup.
    regex = `(['``])((?=[^\\1]|\1\1).*)\1|[^\(,\)'``]+|\(|,|\)|.`.
  ENDMETHOD.


  METHOD test.
    FIND ALL OCCURRENCES OF
        REGEX regex ##regex_posix
        IN `(zif_abapgit_apack_definitions=>c_apack_interface_cust)=>('CO_INTERFACE_VERSION')`
        RESULTS DATA(matches).
    " cl_abap_unit_assert=>assert_equals( ACT = ? EXP = ? MSG = ? ).
  ENDMETHOD.
ENDCLASS.


CLASS ltc_replace_texts IMPLEMENTATION.

  METHOD member_interface_prefix.
    DATA(abap_source_code) = VALUE zcl_shrinker_ddic_class_interf=>ty_abap_source_code(
            ( ` lo_cut ?= lo_cut->zif_abapgit_ajson~slice( '/issues' ).` ) ).
    DATA(cut) = zcl_shrinker_ddic_class_interf=>create( ).
    cut->replace_texts(
        EXPORTING
            replacements     = VALUE #( ( posix_regex = '\<Z(.._ABAPGIT\w*)' with = 'L$1' member_interface_prefix = abap_true ) )
        CHANGING
            abap_source_code = abap_source_code ).
    cl_abap_unit_assert=>assert_equals(
        act = abap_source_code
        exp = VALUE zcl_shrinker_ddic_class_interf=>ty_abap_source_code(
            ( ` lo_cut ?= lo_cut->Lif_abapgit_ajson~slice( '/issues' ).` ) ) ).
  ENDMETHOD.


  METHOD start_of_abap_word.
    DATA(abap_source_code) = VALUE zcl_shrinker_ddic_class_interf=>ty_abap_source_code(
            ( `zif_abapgit_ajson=>member.` ) ).
    DATA(cut) = zcl_shrinker_ddic_class_interf=>create( ).
    cut->replace_texts(
        EXPORTING
            replacements     = VALUE #( ( posix_regex = '\<Z(.._ABAPGIT\w*)' with = 'L$1' start_of_abap_word = abap_true ) )
        CHANGING
            abap_source_code = abap_source_code ).
    cl_abap_unit_assert=>assert_equals(
        act = abap_source_code
        exp = VALUE zcl_shrinker_ddic_class_interf=>ty_abap_source_code(
            ( `Lif_abapgit_ajson=>member.` ) ) ).
  ENDMETHOD.

ENDCLASS.
