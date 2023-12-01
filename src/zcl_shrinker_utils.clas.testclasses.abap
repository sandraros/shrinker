*"* use this source file for your ABAP unit test classes

CLASS ltc_get_main_program_name DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.

    METHODS class_pool FOR TESTING RAISING cx_static_check.
    METHODS context FOR TESTING RAISING cx_static_check.
    METHODS function_group FOR TESTING RAISING cx_static_check.
    METHODS function_group_customer_ns FOR TESTING RAISING cx_static_check.
    METHODS program FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_get_program_as_tadir_obj DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS class_pool FOR TESTING RAISING cx_static_check.
    METHODS context FOR TESTING RAISING cx_static_check.
    METHODS function_group FOR TESTING RAISING cx_static_check.
    METHODS function_group_customer_ns FOR TESTING RAISING cx_static_check.
    METHODS program FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_get_main_program_name IMPLEMENTATION.

  METHOD class_pool.
    cl_abap_unit_assert=>assert_equals(
        act = zcl_shrinker_utils=>get_main_program_name( object   = 'CLAS'
                                                         obj_name = 'ZXXX' )
        exp = CONV syrepid( 'ZXXX==========================CP' ) ).
  ENDMETHOD.


  METHOD context.
    cl_abap_unit_assert=>assert_equals(
        act = zcl_shrinker_utils=>get_main_program_name( object   = 'CNTX'
                                                         obj_name = 'ZCONTEXT' )
        exp = CONV syrepid( 'CONTEXT_X_ZCONTEXT' ) ).
  ENDMETHOD.


  METHOD function_group.
    cl_abap_unit_assert=>assert_equals(
        act = zcl_shrinker_utils=>get_main_program_name( object   = 'FUGR'
                                                         obj_name = 'XXX' )
        exp = CONV syrepid( 'SAPLXXX' ) ).
  ENDMETHOD.


  METHOD function_group_customer_ns.
    cl_abap_unit_assert=>assert_equals(
        act = zcl_shrinker_utils=>get_main_program_name( object   = 'FUGR'
                                                         obj_name = '/CUSTOMER/XXX' )
        exp = CONV syrepid( '/CUSTOMER/SAPLXXX' ) ).
  ENDMETHOD.


  METHOD program.
    cl_abap_unit_assert=>assert_equals(
        act = zcl_shrinker_utils=>get_main_program_name( object   = 'PROG'
                                                         obj_name = 'ZXXX' )
        exp = CONV syrepid( 'ZXXX' ) ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_get_program_as_tadir_obj IMPLEMENTATION.

  METHOD class_pool.
    cl_abap_unit_assert=>assert_equals(
        act = zcl_shrinker_utils=>get_program_as_tadir_object( 'ZXXX==========================CP' )
        exp = VALUE zcl_shrinker_utils=>ty_tadir_object( object   = 'CLAS'
                                                         obj_name = 'ZXXX' ) ).
  ENDMETHOD.


  METHOD context.
    cl_abap_unit_assert=>assert_equals(
        act = zcl_shrinker_utils=>get_program_as_tadir_object( 'CONTEXT_X_ZCONTEXT' )
        exp = VALUE zcl_shrinker_utils=>ty_tadir_object( object   = 'CNTX'
                                                         obj_name = 'ZCONTEXT' ) ).
  ENDMETHOD.


  METHOD function_group.
    cl_abap_unit_assert=>assert_equals(
        act = zcl_shrinker_utils=>get_program_as_tadir_object( 'SAPLXXX' )
        exp = VALUE zcl_shrinker_utils=>ty_tadir_object( object   = 'FUGR'
                                                         obj_name = 'XXX' ) ).
  ENDMETHOD.


  METHOD function_group_customer_ns.
    cl_abap_unit_assert=>assert_equals(
        act = zcl_shrinker_utils=>get_program_as_tadir_object( '/CUSTOMER/SAPLXXX' )
        exp = VALUE zcl_shrinker_utils=>ty_tadir_object( object   = 'FUGR'
                                                         obj_name = '/CUSTOMER/XXX' ) ).
  ENDMETHOD.


  METHOD program.
    cl_abap_unit_assert=>assert_equals(
        act = zcl_shrinker_utils=>get_program_as_tadir_object( 'ZXXX' )
        exp = VALUE zcl_shrinker_utils=>ty_tadir_object( object   = 'PROG'
                                                         obj_name = 'ZXXX' ) ).
  ENDMETHOD.

ENDCLASS.
