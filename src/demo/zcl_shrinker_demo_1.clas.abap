CLASS zcl_shrinker_demo_1 DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .
  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_structure_with_include.
        INCLUDE TYPE bapiret2.
        INCLUDE STRUCTURE sy.
    TYPES:
      END OF ty_structure_with_include.
    METHODS dummy_method_1.
    METHODS dummy_method_2.
ENDCLASS.
CLASS zcl_shrinker_demo_1 IMPLEMENTATION.
  METHOD dummy_method_1.
  ENDMETHOD.
  METHOD dummy_method_2.
  ENDMETHOD.
ENDCLASS.
