*"* use this source file for your ABAP unit test classes
*CLASS ltc_main DEFINITION
*      FOR TESTING
*      DURATION SHORT
*      RISK LEVEL HARMLESS.
*  PRIVATE SECTION.
*    METHODS test FOR TESTING.
**    class-methods class_setup.
**    class-methods class_teardown.
**    methods setup.
**    methods teardown.
*ENDCLASS.
*CLASS ltc_main IMPLEMENTATION.
*  METHOD test.
*    DATA(shrinker_any_program) = zcl_shrinker_any_program=>create( ).
*    DATA(shrinked) = shrinker_any_program->get_abap_for_main_program(
*                        zcl_shrinker_utils=>get_main_program_name( object   = 'CLAS'
*                                                                   obj_name = 'ZCL_SHRINKER_DEMO_1' ) ).
*    " cl_abap_unit_assert=>assert_equals( ACT = ? EXP = ? MSG = ? ).
*  ENDMETHOD.
*ENDCLASS.
