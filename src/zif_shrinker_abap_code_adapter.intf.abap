INTERFACE zif_shrinker_abap_code_adapter
  PUBLIC .

  TYPES ty_abap_source_code TYPE zcl_shrinker_abap_scan=>ty_abap_source_code.
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
    BEGIN OF ty_include_statement,
      row      TYPE i,
      stokes   TYPE zcl_shrinker_abap_scan=>ty_ut_stokes,
      name     TYPE syrepid,
      if_found TYPE abap_bool,
    END OF ty_include_statement.
  TYPES ty_include_statements TYPE STANDARD TABLE OF ty_include_statement WITH EMPTY KEY.
  TYPES:
    BEGIN OF ty_source_unit,
      name               TYPE syrepid,
      abap_source_code   TYPE ty_abap_source_code,
      include_statements TYPE ty_include_statements,
    END OF ty_source_unit.
  TYPES ty_source_units TYPE STANDARD TABLE OF ty_source_unit WITH EMPTY KEY.


  METHODS adapt_source_code
    CHANGING
      classes_interfaces TYPE ty_read_class_interface_includ OPTIONAL
      other_source_units TYPE ty_source_units OPTIONAL.

  METHODS adapt_source_code_before_rep_i
    CHANGING
      source_units TYPE ty_source_units.

ENDINTERFACE.
