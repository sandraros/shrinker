********************************************************************************
*
* LICENSE and NOTICE
*
* See include program ZSHRINKER_ABAPGIT_LICENSE
*
********************************************************************************

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~set_integer.

    ri_json = me.

    Lif_abapgit_ajson~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = iv_val ).

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~set_null.

    ri_json = me.

    DATA lv_null_ref TYPE REF TO data.
    Lif_abapgit_ajson~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_null_ref ).

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~set_string.

    ri_json = me.

    DATA lv_val TYPE string.
    lv_val = iv_val.
    Lif_abapgit_ajson~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_val ).

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~set_timestamp.

    ri_json = me.

    DATA lv_timestamp_iso TYPE string.
    lv_timestamp_iso = SHRITEFUH64VYIPN5I4UHL45BC3R4A=>format_timestamp( iv_val ).

    Lif_abapgit_ajson~set(
      iv_ignore_empty = abap_false
      iv_path = iv_path
      iv_val  = lv_timestamp_iso ).

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~slice.

    DATA lo_section         TYPE REF TO Lcl_abapgit_ajson.
    DATA ls_item            LIKE LINE OF mt_json_tree.
    DATA lv_normalized_path TYPE string.
    DATA ls_path_parts      TYPE Lif_abapgit_ajson_types=>ty_path_name.
    DATA lv_path_len        TYPE i.
    DATA lv_path_pattern    TYPE string.

    CREATE OBJECT lo_section.
    lv_normalized_path = SHRITEFUH64VYIPN5I4UHL45BCTR4A=>normalize_path( iv_path ).
    lv_path_len        = strlen( lv_normalized_path ).
    ls_path_parts      = SHRITEFUH64VYIPN5I4UHL45BCTR4A=>split_path( lv_normalized_path ).

    READ TABLE mt_json_tree INTO ls_item
      WITH KEY path = ls_path_parts-path name = ls_path_parts-name.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CLEAR: ls_item-path, ls_item-name, ls_item-order. " this becomes a new root
    INSERT ls_item INTO TABLE lo_section->mt_json_tree.

    lv_path_pattern = lv_normalized_path && `*`.

    LOOP AT mt_json_tree INTO ls_item WHERE path CP lv_path_pattern.

      ls_item-path = substring( val = ls_item-path
                                off = lv_path_len - 1 ). " less closing '/'
      INSERT ls_item INTO TABLE lo_section->mt_json_tree.

    ENDLOOP.

    ri_json = lo_section.

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~stringify.

    rv_json = SHRITEFUH64VYIPN5I4UHL45BCXR4A=>stringify(
      it_json_tree       = mt_json_tree
      iv_keep_item_order = ms_opts-keep_item_order
      iv_indent          = iv_indent ).

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~touch_array.

    DATA lr_node TYPE REF TO Lif_abapgit_ajson_types=>ty_node.
    DATA ls_deleted_node TYPE Lif_abapgit_ajson_types=>ty_node.
    DATA ls_new_node LIKE LINE OF mt_json_tree.
    DATA ls_split_path TYPE Lif_abapgit_ajson_types=>ty_path_name.

    read_only_watchdog( ).

    ls_split_path = SHRITEFUH64VYIPN5I4UHL45BCTR4A=>split_path( iv_path ).
    IF ls_split_path IS INITIAL. " Assign root, exceptional processing
      ls_new_node-path = ls_split_path-path.
      ls_new_node-name = ls_split_path-name.
      ls_new_node-type = Lif_abapgit_ajson_types=>node_type-array.
      INSERT ls_new_node INTO TABLE mt_json_tree.
      RETURN.
    ENDIF.

    IF iv_clear = abap_true.
      ls_deleted_node = delete_subtree(
        iv_path = ls_split_path-path
        iv_name = ls_split_path-name ).
    ELSE.
      lr_node = get_item( iv_path ).
    ENDIF.

    IF lr_node IS INITIAL. " Or node was cleared

      DATA lr_parent TYPE REF TO Lif_abapgit_ajson_types=>ty_node.
      lr_parent = prove_path_exists( ls_split_path-path ).
      ASSERT lr_parent IS NOT INITIAL.

      lr_parent->children = lr_parent->children + 1.

      ls_new_node-path = ls_split_path-path.
      ls_new_node-name = ls_split_path-name.
      ls_new_node-type = Lif_abapgit_ajson_types=>node_type-array.

      IF ms_opts-keep_item_order = abap_true AND ls_deleted_node IS NOT INITIAL.
        ls_new_node-order = ls_deleted_node-order.
      ENDIF.

      INSERT ls_new_node INTO TABLE mt_json_tree.

    ELSEIF lr_node->type <> Lif_abapgit_ajson_types=>node_type-array.
      Lcx_abapgit_ajson_error=>raise( |Path [{ iv_path }] already used and is not array| ).
    ENDIF.

    ri_json = me.

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~to_abap.

    DATA lo_to_abap TYPE REF TO SHRITEFUH64VYIPN5I4UHL45BCZR4A.

    CLEAR ev_container.
    CREATE OBJECT lo_to_abap
      EXPORTING
        iv_corresponding  = boolc( iv_corresponding = abap_true OR ms_opts-to_abap_corresponding_only = abap_true )
        ii_custom_mapping = mi_custom_mapping.

    lo_to_abap->to_abap(
      EXPORTING
        it_nodes    = Lif_abapgit_ajson~mt_json_tree
      CHANGING
        c_container = ev_container ).

  ENDMETHOD.
  METHOD Lif_abapgit_ajson~to_abap_corresponding_only.
    ms_opts-to_abap_corresponding_only = iv_enable.
    ri_json = me.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_AJSON implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_CHDO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_chdo=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_chdo=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_CHDO implementation.
*"* method's implementations
*include methods.
  METHOD after_import.

    DATA: lt_cts_object_entry TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY,
          ls_cts_object_entry LIKE LINE OF lt_cts_object_entry,
          lt_errormsg         TYPE STANDARD TABLE OF sprot_u WITH DEFAULT KEY.

    ls_cts_object_entry-pgmid    = 'R3TR'.
    ls_cts_object_entry-object   = ms_item-obj_type.
    ls_cts_object_entry-obj_name = ms_item-obj_name.
    INSERT ls_cts_object_entry INTO TABLE lt_cts_object_entry.

    CALL FUNCTION 'AFTER_IMP_CHDO'
      EXPORTING
        iv_tarclient  = sy-mandt
        iv_is_upgrade = abap_false
      TABLES
        tt_e071       = lt_cts_object_entry
        tt_errormsg   = lt_errormsg.

    LOOP AT lt_errormsg TRANSPORTING NO FIELDS WHERE severity = 'E' OR severity = 'A'.
      EXIT.
    ENDLOOP.

    IF sy-subrc = 0.
      Lcx_abapgit_exception=>raise( 'Error from AFTER_IMP_CHDO' ).
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_object = is_item-obj_name.

  ENDMETHOD.
  METHOD delete_tadir_cdnames.

    DATA: lv_obj_name TYPE sobj_name.

    IF is_cdnames-repnamec IS NOT INITIAL.
      lv_obj_name = is_cdnames-repnamec.
      CALL FUNCTION 'TR_TADIR_INTERFACE'
        EXPORTING
          wi_delete_tadir_entry    = abap_true
          wi_tadir_pgmid           = 'R3TR'
          wi_tadir_object          = 'PROG'
          wi_tadir_obj_name        = lv_obj_name
          wi_test_modus            = abap_false
        EXCEPTIONS
          tadir_entry_not_existing = 1
          OTHERS                   = 2.
      IF sy-subrc > 1.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

    IF is_cdnames-repnamet IS NOT INITIAL.
      lv_obj_name = is_cdnames-repnamet.
      CALL FUNCTION 'TR_TADIR_INTERFACE'
        EXPORTING
          wi_delete_tadir_entry    = abap_true
          wi_tadir_pgmid           = 'R3TR'
          wi_tadir_object          = 'PROG'
          wi_tadir_obj_name        = lv_obj_name
          wi_test_modus            = abap_false
        EXCEPTIONS
          tadir_entry_not_existing = 1
          OTHERS                   = 2.
      IF sy-subrc > 1.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

    IF is_cdnames-repnamefix IS NOT INITIAL.
      lv_obj_name = is_cdnames-repnamefix.
      CALL FUNCTION 'TR_TADIR_INTERFACE'
        EXPORTING
          wi_delete_tadir_entry    = abap_true
          wi_tadir_pgmid           = 'R3TR'
          wi_tadir_object          = 'PROG'
          wi_tadir_obj_name        = lv_obj_name
          wi_test_modus            = abap_false
        EXCEPTIONS
          tadir_entry_not_existing = 1
          OTHERS                   = 2.
      IF sy-subrc > 1.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

    IF is_cdnames-repnamevar IS NOT INITIAL.
      lv_obj_name = is_cdnames-repnamevar.
      CALL FUNCTION 'TR_TADIR_INTERFACE'
        EXPORTING
          wi_delete_tadir_entry    = abap_true
          wi_tadir_pgmid           = 'R3TR'
          wi_tadir_object          = 'PROG'
          wi_tadir_obj_name        = lv_obj_name
          wi_test_modus            = abap_false
        EXCEPTIONS
          tadir_entry_not_existing = 1
          OTHERS                   = 2.
      IF sy-subrc > 1.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

    IF is_cdnames-fgrp IS NOT INITIAL.
      lv_obj_name = is_cdnames-fgrp.
      CALL FUNCTION 'TR_TADIR_INTERFACE'
        EXPORTING
          wi_delete_tadir_entry    = abap_true
          wi_tadir_pgmid           = 'R3TR'
          wi_tadir_object          = 'FUGR'
          wi_tadir_obj_name        = lv_obj_name
          wi_test_modus            = abap_false
        EXCEPTIONS
          tadir_entry_not_existing = 1
          OTHERS                   = 2.
      IF sy-subrc > 1.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD delete_tadir_tabl.

    DATA: lv_obj_name TYPE sobj_name.

    IF is_tcdrs-tabname IS NOT INITIAL.
      lv_obj_name = is_tcdrs-tabname.
      CALL FUNCTION 'TR_TADIR_INTERFACE'
        EXPORTING
          wi_delete_tadir_entry    = abap_true
          wi_tadir_pgmid           = 'R3TR'
          wi_tadir_object          = 'TABL'
          wi_tadir_obj_name        = lv_obj_name
          wi_test_modus            = abap_false
        EXCEPTIONS
          tadir_entry_not_existing = 1
          OTHERS                   = 2.
      IF sy-subrc > 1.
        Lcx_abapgit_exception=>raise( |Error from TR_TADIR_INTERFACE (subrc={ sy-subrc } ).| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE updname INTO rv_user
      FROM tcdrp
      WHERE object = mv_object.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lt_cdnames TYPE STANDARD TABLE OF cdnames,
          ls_cdnames TYPE cdnames,
          lt_tcdrs   TYPE STANDARD TABLE OF tcdrs,
          ls_tcdrs   TYPE tcdrs,
          lv_msg     TYPE symsgv.

    CALL FUNCTION 'CDNAMES_GET'
      EXPORTING
        iv_object        = mv_object
      TABLES
        it_tcdrs         = lt_tcdrs
        it_names         = lt_cdnames
      EXCEPTIONS
        object_space     = 1
        object_not_found = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'CHDO_DELETE'
      EXPORTING
        iv_object        = mv_object
        iv_with_tadir    = abap_true
      EXCEPTIONS
        object_is_space  = 1
        object_not_found = 2
        other_error      = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      lv_msg = mv_object.
      Lcx_abapgit_exception=>raise_t100( iv_msgid = 'CD'
                                         iv_msgno = '869'
                                         iv_msgv1 = lv_msg ).
    ENDIF.

    LOOP AT lt_cdnames INTO ls_cdnames.
      delete_tadir_cdnames( ls_cdnames ).
    ENDLOOP.

    LOOP AT lt_tcdrs INTO ls_tcdrs.
      delete_tadir_tabl( ls_tcdrs ).
    ENDLOOP.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_change_object TYPE ty_change_document.
    FIELD-SYMBOLS: <ls_report_generated> LIKE LINE OF ls_change_object-reports_generated.

    io_xml->read( EXPORTING iv_name = 'CHDO'
                  CHANGING  cg_data = ls_change_object ).

    DELETE FROM tcdobs  WHERE object = mv_object.
    DELETE FROM tcdobts WHERE object = mv_object.
    DELETE FROM tcdrps  WHERE object = mv_object.

    LOOP AT ls_change_object-reports_generated ASSIGNING <ls_report_generated>.
      <ls_report_generated>-devclass = iv_package.
    ENDLOOP.

    INSERT tcdobs  FROM TABLE ls_change_object-objects.
    INSERT tcdobts FROM TABLE ls_change_object-objects_text.
    INSERT tcdrps  FROM TABLE ls_change_object-reports_generated.

    tadir_insert( iv_package ).

    after_import( ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    SELECT COUNT(*)
      FROM tcdrp
      WHERE object = mv_object.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: lt_bdcdata TYPE STANDARD TABLE OF bdcdata,
          ls_bdcdata LIKE LINE OF lt_bdcdata.

    CLEAR: ls_bdcdata.
    ls_bdcdata-program  = 'SAPMSCDO_NEW'.
    ls_bdcdata-dynpro   = '0100'.
    ls_bdcdata-dynbegin = abap_true.
    APPEND ls_bdcdata TO lt_bdcdata.

    CLEAR: ls_bdcdata.
    ls_bdcdata-fnam = 'TCDOB-OBJECT'.
    ls_bdcdata-fval = mv_object.
    APPEND ls_bdcdata TO lt_bdcdata.

    CLEAR: ls_bdcdata.
    ls_bdcdata-fnam = 'BDC_OKCODE'.
    ls_bdcdata-fval = '=DISP'.
    APPEND ls_bdcdata TO lt_bdcdata.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SCDO'
      it_bdcdata = lt_bdcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_change_object TYPE ty_change_document,
          lt_tcdrp         TYPE STANDARD TABLE OF tcdrp,
          lt_tcdob         TYPE STANDARD TABLE OF tcdob,
          lt_tcdobt        TYPE STANDARD TABLE OF tcdobt,
          BEGIN OF ls_nulldatetime, " hack ro reset fields when they exist without syntax errors when they don't
            udate TYPE sy-datum,
            utime TYPE sy-uzeit,
          END OF ls_nulldatetime.

    FIELD-SYMBOLS: <ls_reports_generated> LIKE LINE OF ls_change_object-reports_generated,
                   <ls_objects>           LIKE LINE OF ls_change_object-objects,
                   <ls_objects_text>      LIKE LINE OF ls_change_object-objects_text.

    CALL FUNCTION 'CDNAMES_GET'
      EXPORTING
        iv_object        = mv_object
      TABLES
        it_tcdrp         = lt_tcdrp
        it_tcdob         = lt_tcdob
        it_tcdobt        = lt_tcdobt
      EXCEPTIONS
        object_space     = 1
        object_not_found = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    ls_change_object-reports_generated = lt_tcdrp.
    ls_change_object-objects           = lt_tcdob.
    ls_change_object-objects_text      = lt_tcdobt.

    " At import, when CHDO is generated date & time change, so always detects changes for this fields
    LOOP AT ls_change_object-reports_generated ASSIGNING <ls_reports_generated>.
      CLEAR: <ls_reports_generated>-datum, <ls_reports_generated>-uzeit,
             <ls_reports_generated>-author, <ls_reports_generated>-updname,
             <ls_reports_generated>-devclass.
    ENDLOOP.

    LOOP AT ls_change_object-objects ASSIGNING <ls_objects>.
      MOVE-CORRESPONDING ls_nulldatetime TO <ls_objects>. " reset date and time
    ENDLOOP.

    LOOP AT ls_change_object-objects_text ASSIGNING <ls_objects_text>.
      MOVE-CORRESPONDING ls_nulldatetime TO <ls_objects_text>. " reset date and time
    ENDLOOP.

    io_xml->add( iv_name = 'CHDO'
                 ig_data = ls_change_object ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_CHDO implementation

*>>>>>>> ZCL_ABAPGIT_AJSON_FILTER_LIB <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ajson_filter_lib==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ajson_filter_lib==ccimp.
**********************************************************************
*  FILTER EMPTY VALUES
**********************************************************************

CLASS SHRITEFUH64VYIPN5I4UHL45BDXR4A DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_ajson_filter.
ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BDXR4A IMPLEMENTATION.
  METHOD Lif_abapgit_ajson_filter~keep_node.

    rv_keep = boolc(
      ( iv_visit = Lif_abapgit_ajson_filter=>visit_type-value AND is_node-value IS NOT INITIAL ) OR
      ( iv_visit <> Lif_abapgit_ajson_filter=>visit_type-value AND is_node-children > 0 ) ).
    " children = 0 on open for initially empty nodes and on close for filtered ones

  ENDMETHOD.
ENDCLASS.

**********************************************************************
*  FILTER PREDEFINED PATHS
**********************************************************************

CLASS SHRITEFUH64VYIPN5I4UHL45BDZR4A DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_ajson_filter.
    METHODS constructor
      IMPORTING
        it_skip_paths TYPE string_table OPTIONAL
        iv_skip_paths TYPE string OPTIONAL
        iv_pattern_search TYPE abap_bool
      RAISING
        Lcx_abapgit_ajson_error.
  PRIVATE SECTION.
    DATA mt_skip_paths TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
    DATA mv_pattern_search TYPE abap_bool.
ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BDZR4A IMPLEMENTATION.

  METHOD Lif_abapgit_ajson_filter~keep_node.

    DATA lv_full_path TYPE string.
    FIELD-SYMBOLS <p> LIKE LINE OF mt_skip_paths.

    lv_full_path = is_node-path && is_node-name.

    IF mv_pattern_search = abap_true.
      rv_keep = abap_true.
      LOOP AT mt_skip_paths ASSIGNING <p>.
        IF lv_full_path CP <p>.
          rv_keep = abap_false.
          EXIT.
        ENDIF.
      ENDLOOP.
    ELSE.
      READ TABLE mt_skip_paths WITH KEY table_line = lv_full_path TRANSPORTING NO FIELDS.
      rv_keep = boolc( sy-subrc <> 0 ).
    ENDIF.

  ENDMETHOD.

  METHOD constructor.

    DATA lv_s TYPE string.
    DATA lt_tab TYPE string_table.
    FIELD-SYMBOLS <s> TYPE string.

    IF boolc( iv_skip_paths IS INITIAL ) = boolc( it_skip_paths IS INITIAL ). " XOR
      Lcx_abapgit_ajson_error=>raise( 'no filter path specified' ).
    ENDIF.

    LOOP AT it_skip_paths INTO lv_s.
      lv_s = to_lower( lv_s ).
      APPEND lv_s TO lt_tab.
    ENDLOOP.

    IF iv_skip_paths IS NOT INITIAL.
      SPLIT iv_skip_paths AT ',' INTO TABLE lt_tab.
      LOOP AT lt_tab ASSIGNING <s>.
        IF <s> IS INITIAL.
          DELETE lt_tab INDEX sy-tabix.
          CONTINUE.
        ENDIF.
        <s> = condense( to_lower( <s> ) ).
      ENDLOOP.
    ENDIF.

    SORT lt_tab BY table_line.
    DELETE ADJACENT DUPLICATES FROM lt_tab.

    mt_skip_paths = lt_tab.
    mv_pattern_search = iv_pattern_search.

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* MULTI FILTER
**********************************************************************

CLASS SHRITEFUH64VYIPN5I4UHL45BD3R4A DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_ajson_filter.
    METHODS constructor
      IMPORTING
        it_filters TYPE Lif_abapgit_ajson_filter=>ty_filter_tab
      RAISING
        Lcx_abapgit_ajson_error.
  PRIVATE SECTION.
    DATA mt_filters TYPE Lif_abapgit_ajson_filter=>ty_filter_tab.
ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BD3R4A IMPLEMENTATION.

  METHOD Lif_abapgit_ajson_filter~keep_node.

    DATA li_filter LIKE LINE OF mt_filters.

    rv_keep = abap_true.
    LOOP AT mt_filters INTO li_filter.
      rv_keep = li_filter->keep_node(
        is_node  = is_node
        iv_visit = iv_visit ).
      IF rv_keep = abap_false.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD constructor.

    DATA li_filter LIKE LINE OF it_filters.

    LOOP AT it_filters INTO li_filter WHERE table_line IS BOUND.
      APPEND li_filter TO mt_filters.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

*"* test class
*include zcl_abapgit_ajson_filter_lib==ccau.



class LCL_ABAPGIT_AJSON_FILTER_LIB implementation.
*"* method's implementations
*include methods.
  METHOD create_and_filter.
    CREATE OBJECT ri_filter TYPE SHRITEFUH64VYIPN5I4UHL45BD3R4A
      EXPORTING
        it_filters = it_filters.
  ENDMETHOD.
  METHOD create_empty_filter.
    CREATE OBJECT ri_filter TYPE SHRITEFUH64VYIPN5I4UHL45BDXR4A.
  ENDMETHOD.
  METHOD create_path_filter.
    CREATE OBJECT ri_filter TYPE SHRITEFUH64VYIPN5I4UHL45BDZR4A
      EXPORTING
        iv_pattern_search = iv_pattern_search
        it_skip_paths = it_skip_paths
        iv_skip_paths = iv_skip_paths.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_AJSON_FILTER_LIB implementation

*>>>>>>> ZCL_ABAPGIT_AJSON_MAPPING <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ajson_mapping=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ajson_mapping=====ccimp.
CLASS SHRITEFUH64VYIPN5I4UHL45BD7R4A IMPLEMENTATION. "DEPRECATED


  METHOD constructor.

    DATA ls_mapping_field LIKE LINE OF mt_mapping_fields.

    LOOP AT it_mapping_fields INTO ls_mapping_field.
      ls_mapping_field-abap = to_upper( ls_mapping_field-abap ).
      INSERT ls_mapping_field INTO TABLE mt_mapping_fields.
    ENDLOOP.

  ENDMETHOD.


  METHOD Lif_abapgit_ajson_mapping~to_abap.

    DATA ls_mapping_field LIKE LINE OF mt_mapping_fields.

    READ TABLE mt_mapping_fields INTO ls_mapping_field
      WITH KEY json COMPONENTS json = iv_name.
    IF sy-subrc = 0.
      rv_result = ls_mapping_field-abap.
    ENDIF.

  ENDMETHOD.


  METHOD Lif_abapgit_ajson_mapping~to_json.

    DATA lv_field TYPE string.
    DATA ls_mapping_field LIKE LINE OF mt_mapping_fields.

    lv_field = to_upper( iv_name ).

    READ TABLE mt_mapping_fields INTO ls_mapping_field
      WITH KEY abap COMPONENTS abap = lv_field.
    IF sy-subrc = 0.
      rv_result = ls_mapping_field-json.
    ENDIF.

  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~rename_node.

  ENDMETHOD.

ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BEAR4A IMPLEMENTATION.

  METHOD constructor.
    mt_rename_map = it_rename_map.
    mv_rename_by = iv_rename_by.
  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~to_abap.
  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~to_json.
  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~rename_node.

    DATA lv_full_path TYPE string.
    DATA lv_pair_found TYPE abap_bool.
    FIELD-SYMBOLS <r> LIKE LINE OF mt_rename_map.

    CASE mv_rename_by.
      WHEN Lcl_abapgit_ajson_mapping=>rename_by-attr_name.
        READ TABLE mt_rename_map ASSIGNING <r> WITH TABLE KEY by_name COMPONENTS from = cv_name.
        lv_pair_found = boolc( sy-subrc = 0 ).
      WHEN Lcl_abapgit_ajson_mapping=>rename_by-full_path.
        lv_full_path = is_node-path && cv_name.
        READ TABLE mt_rename_map ASSIGNING <r> WITH TABLE KEY by_name COMPONENTS from = lv_full_path.
        lv_pair_found = boolc( sy-subrc = 0 ).
      WHEN Lcl_abapgit_ajson_mapping=>rename_by-pattern.
        lv_full_path = is_node-path && cv_name.
        LOOP AT mt_rename_map ASSIGNING <r>.
          IF lv_full_path CP <r>-from.
            lv_pair_found = abap_true.
            EXIT.
          ENDIF.
        ENDLOOP.
      WHEN OTHERS.
        lv_pair_found = abap_false. " No rename
    ENDCASE.

    IF lv_pair_found = abap_true.
      cv_name = <r>-to.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BEBR4A IMPLEMENTATION.


  METHOD constructor.

    mi_mapping_fields = Lcl_abapgit_ajson_mapping=>create_field_mapping( it_mapping_fields ).

  ENDMETHOD.


  METHOD Lif_abapgit_ajson_mapping~to_abap.

    rv_result = mi_mapping_fields->to_abap( iv_path = iv_path
                                            iv_name = iv_name ).

  ENDMETHOD.


  METHOD Lif_abapgit_ajson_mapping~to_json.

    rv_result = mi_mapping_fields->to_json( iv_path = iv_path
                                            iv_name = iv_name ).

    IF rv_result IS NOT INITIAL. " Mapping found
      RETURN.
    ENDIF.

    rv_result = to_upper( iv_name ).

  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~rename_node.

    cv_name = to_upper( cv_name ).

  ENDMETHOD.

ENDCLASS.


CLASS SHRITEFUH64VYIPN5I4UHL45BECR4A IMPLEMENTATION.


  METHOD constructor.

    mi_mapping_fields = Lcl_abapgit_ajson_mapping=>create_field_mapping( it_mapping_fields ).

  ENDMETHOD.


  METHOD Lif_abapgit_ajson_mapping~to_abap.

    rv_result = mi_mapping_fields->to_abap( iv_path = iv_path
                                            iv_name = iv_name ).

  ENDMETHOD.


  METHOD Lif_abapgit_ajson_mapping~to_json.

    rv_result = mi_mapping_fields->to_json( iv_path = iv_path
                                            iv_name = iv_name ).

    IF rv_result IS NOT INITIAL. " Mapping found
      RETURN.
    ENDIF.

    rv_result = to_lower( iv_name ).

  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~rename_node.

    cv_name = to_lower( cv_name ).

  ENDMETHOD.

ENDCLASS.


CLASS SHRITEFUH64VYIPN5I4UHL45BEDR4A IMPLEMENTATION. "DEPRECATED


  METHOD constructor.

    mi_mapping_fields   = Lcl_abapgit_ajson_mapping=>create_field_mapping( it_mapping_fields ).
    mv_first_json_upper = iv_first_json_upper.

  ENDMETHOD.


  METHOD Lif_abapgit_ajson_mapping~to_abap.

    rv_result = mi_mapping_fields->to_abap( iv_path = iv_path
                                            iv_name = iv_name ).

    IF rv_result IS NOT INITIAL. " Mapping found
      RETURN.
    ENDIF.

    rv_result = iv_name.

    REPLACE ALL OCCURRENCES OF REGEX `([a-z])([A-Z])` IN rv_result WITH `$1_$2`.

  ENDMETHOD.


  METHOD Lif_abapgit_ajson_mapping~to_json.

    TYPES ty_token TYPE c LENGTH 255.
    DATA lt_tokens TYPE STANDARD TABLE OF ty_token.
    DATA lv_from TYPE i.
    FIELD-SYMBOLS <token> LIKE LINE OF lt_tokens.

    rv_result = mi_mapping_fields->to_json( iv_path = iv_path
                                            iv_name = iv_name ).

    IF rv_result IS NOT INITIAL. " Mapping found
      RETURN.
    ENDIF.

    rv_result = iv_name.

    REPLACE ALL OCCURRENCES OF `__` IN rv_result WITH `*`.

    TRANSLATE rv_result TO LOWER CASE.
    TRANSLATE rv_result USING `/_:_~_`.

    IF mv_first_json_upper = abap_true.
      lv_from = 1.
    ELSE.
      lv_from = 2.
    ENDIF.

    SPLIT rv_result AT `_` INTO TABLE lt_tokens.
    LOOP AT lt_tokens ASSIGNING <token> FROM lv_from.
      TRANSLATE <token>(1) TO UPPER CASE.
    ENDLOOP.

    CONCATENATE LINES OF lt_tokens INTO rv_result.
    REPLACE ALL OCCURRENCES OF `*` IN rv_result WITH `_`.

  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~rename_node.

  ENDMETHOD.

ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BEER4A IMPLEMENTATION.

  METHOD constructor.
    mt_queue = it_queue.
  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~rename_node.

    DATA ls_node LIKE is_node.
    DATA li_mapper LIKE LINE OF mt_queue.

    ls_node = is_node.

    LOOP AT mt_queue INTO li_mapper.
      li_mapper->rename_node(
        EXPORTING
          is_node = ls_node
        CHANGING
          cv_name = cv_name ).
      ls_node-name = cv_name.
    ENDLOOP.

  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~to_abap.

  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~to_json.

  ENDMETHOD.

ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BEFR4A IMPLEMENTATION.

  METHOD Lif_abapgit_ajson_mapping~rename_node.

    REPLACE ALL OCCURRENCES OF REGEX `([a-z])([A-Z])` IN cv_name WITH `$1_$2`.
    cv_name = to_lower( cv_name ).

  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~to_abap.

  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~to_json.

  ENDMETHOD.

ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BEGR4A IMPLEMENTATION.

  METHOD constructor.
    mv_first_json_upper = iv_first_json_upper.
  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~rename_node.

    TYPES lty_token TYPE c LENGTH 255.
    CONSTANTS lc_forced_underscore_marker TYPE c LENGTH 1 VALUE cl_abap_char_utilities=>horizontal_tab.

    DATA lt_tokens TYPE STANDARD TABLE OF lty_token.
    DATA lv_from TYPE i.
    FIELD-SYMBOLS <token> LIKE LINE OF lt_tokens.

    IF mv_first_json_upper = abap_true.
      lv_from = 1.
    ELSE.
      lv_from = 2.
    ENDIF.
    REPLACE ALL OCCURRENCES OF `__` IN cv_name WITH lc_forced_underscore_marker. " Force underscore

    SPLIT cv_name AT `_` INTO TABLE lt_tokens.
    DELETE lt_tokens WHERE table_line IS INITIAL.
    LOOP AT lt_tokens ASSIGNING <token> FROM lv_from.
      TRANSLATE <token>+0(1) TO UPPER CASE.
    ENDLOOP.

    CONCATENATE LINES OF lt_tokens INTO cv_name.
    REPLACE ALL OCCURRENCES OF lc_forced_underscore_marker IN cv_name WITH `_`.

  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~to_abap.

  ENDMETHOD.

  METHOD Lif_abapgit_ajson_mapping~to_json.

  ENDMETHOD.

ENDCLASS.

*"* test class
*include zcl_abapgit_ajson_mapping=====ccau.


















class LCL_ABAPGIT_AJSON_MAPPING implementation.
*"* method's implementations
*include methods.
  METHOD create_camel_case.

    CREATE OBJECT ri_mapping TYPE SHRITEFUH64VYIPN5I4UHL45BEDR4A
      EXPORTING
        it_mapping_fields   = it_mapping_fields
        iv_first_json_upper = iv_first_json_upper.

  ENDMETHOD.
  METHOD create_compound_mapper.

    DATA lt_queue TYPE Lif_abapgit_ajson_mapping=>ty_table_of.

    APPEND ii_mapper1 TO lt_queue.
    APPEND ii_mapper2 TO lt_queue.
    APPEND ii_mapper3 TO lt_queue.
    APPEND LINES OF it_more TO lt_queue.
    DELETE lt_queue WHERE table_line IS INITIAL.

    CREATE OBJECT ri_mapping TYPE SHRITEFUH64VYIPN5I4UHL45BEER4A
      EXPORTING
        it_queue = lt_queue.

  ENDMETHOD.
  METHOD create_field_mapping.

    CREATE OBJECT ri_mapping TYPE SHRITEFUH64VYIPN5I4UHL45BD7R4A
      EXPORTING
        it_mapping_fields = it_mapping_fields.

  ENDMETHOD.
  METHOD create_lower_case.

    CREATE OBJECT ri_mapping TYPE SHRITEFUH64VYIPN5I4UHL45BECR4A
      EXPORTING
        it_mapping_fields = it_mapping_fields.

  ENDMETHOD.
  METHOD create_rename.

    CREATE OBJECT ri_mapping TYPE SHRITEFUH64VYIPN5I4UHL45BEAR4A
      EXPORTING
        it_rename_map = it_rename_map
        iv_rename_by = iv_rename_by.

  ENDMETHOD.
  METHOD create_to_camel_case.

    CREATE OBJECT ri_mapping TYPE SHRITEFUH64VYIPN5I4UHL45BEGR4A
      EXPORTING
        iv_first_json_upper = iv_first_json_upper.

  ENDMETHOD.
  METHOD create_to_snake_case.

    CREATE OBJECT ri_mapping TYPE SHRITEFUH64VYIPN5I4UHL45BEFR4A.

  ENDMETHOD.
  METHOD create_upper_case.

    CREATE OBJECT ri_mapping TYPE SHRITEFUH64VYIPN5I4UHL45BEBR4A
      EXPORTING
        it_mapping_fields = it_mapping_fields.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_AJSON_MAPPING implementation

*>>>>>>> ZCL_ABAPGIT_APACK_MIGRATION <<<<<<<*

*"* macro definitions
*include zcl_abapgit_apack_migration===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_apack_migration===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_APACK_MIGRATION implementation.
*"* method's implementations
*include methods.
  METHOD add_interface_source.
    DATA: lo_factory     TYPE REF TO object,
          lo_source      TYPE REF TO object,
          lt_source_code TYPE Lif_abapgit_definitions=>ty_string_tt.

    "Buffer needs to be refreshed,
    "otherwise standard SAP CLIF_SOURCE reorder methods alphabetically
    CALL FUNCTION 'SEO_BUFFER_INIT'.
    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        cifkey  = is_clskey
        version = seoc_version_inactive.

    TRY.
        CALL METHOD ('CL_OO_FACTORY')=>('CREATE_INSTANCE')
          RECEIVING
            result = lo_factory.

        CALL METHOD lo_factory->('CREATE_CLIF_SOURCE')
          EXPORTING
            clif_name = is_clskey-clsname
          RECEIVING
            result    = lo_source.

        TRY.
            CALL METHOD lo_source->('IF_OO_CLIF_SOURCE~LOCK').
          CATCH cx_oo_access_permission.
            Lcx_abapgit_exception=>raise( 'source_new, access permission exception' ).
        ENDTRY.

        lt_source_code = get_interface_source( ).

        CALL METHOD lo_source->('IF_OO_CLIF_SOURCE~SET_SOURCE')
          EXPORTING
            source = lt_source_code.

        CALL METHOD lo_source->('IF_OO_CLIF_SOURCE~SAVE').
        CALL METHOD lo_source->('IF_OO_CLIF_SOURCE~UNLOCK').

      CATCH cx_sy_dyn_call_error.
        add_interface_source_classic( is_clskey ).
    ENDTRY.

  ENDMETHOD.
  METHOD add_interface_source_classic.
    DATA: lo_source      TYPE REF TO object,
          lt_source_code TYPE Lif_abapgit_definitions=>ty_string_tt.

    CREATE OBJECT lo_source TYPE ('CL_OO_SOURCE')
      EXPORTING
        clskey             = is_clskey
      EXCEPTIONS
        class_not_existing = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from CL_OO_SOURCE' ).
    ENDIF.

    TRY.
        CALL METHOD lo_source->('ACCESS_PERMISSION')
          EXPORTING
            access_mode = seok_access_modify.
        lt_source_code = get_interface_source( ).
        CALL METHOD lo_source->('SET_SOURCE')
          EXPORTING
            i_source = lt_source_code.
        CALL METHOD lo_source->('SAVE').
        CALL METHOD lo_source->('ACCESS_PERMISSION')
          EXPORTING
            access_mode = seok_access_free.
      CATCH cx_oo_access_permission.
        Lcx_abapgit_exception=>raise( 'permission error' ).
      CATCH cx_oo_source_save_failure.
        Lcx_abapgit_exception=>raise( 'save failure' ).
    ENDTRY.
  ENDMETHOD.
  METHOD add_intf_source_and_activate.

    DATA: ls_clskey           TYPE seoclskey,
          ls_inactive_object  TYPE dwinactiv,
          lt_inactive_objects TYPE TABLE OF dwinactiv.

    ls_clskey-clsname = Lif_abapgit_apack_definitions=>c_apack_interface_cust.

    add_interface_source( ls_clskey ).

    ls_inactive_object-object   = 'INTF'.
    ls_inactive_object-obj_name = Lif_abapgit_apack_definitions=>c_apack_interface_cust.
    INSERT ls_inactive_object INTO TABLE lt_inactive_objects.

    CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
      TABLES
        objects                = lt_inactive_objects
      EXCEPTIONS
        excecution_error       = 1
        cancelled              = 2
        insert_into_corr_error = 3
        OTHERS                 = 4.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD create_interface.

    DATA: ls_interface_properties TYPE vseointerf.

    ls_interface_properties-clsname  = Lif_abapgit_apack_definitions=>c_apack_interface_cust.
    ls_interface_properties-version  = '1'.
    ls_interface_properties-langu    = 'E'.
    ls_interface_properties-descript = 'APACK: Manifest interface'.
    ls_interface_properties-exposure = '2'.
    ls_interface_properties-state    = '1'.
    ls_interface_properties-unicode  = abap_true.

    TRY.
        CALL FUNCTION 'SEO_INTERFACE_CREATE_COMPLETE'
          EXPORTING
            devclass        = '$TMP'
            suppress_dialog = abap_true " Parameter missing in 702
          CHANGING
            interface       = ls_interface_properties
          EXCEPTIONS
            existing        = 1
            is_class        = 2
            db_error        = 3
            component_error = 4
            no_access       = 5
            other           = 6
            OTHERS          = 7.
      CATCH cx_sy_dyn_call_param_not_found.
        CALL FUNCTION 'SEO_INTERFACE_CREATE_COMPLETE'
          EXPORTING
            devclass        = '$TMP'
          CHANGING
            interface       = ls_interface_properties
          EXCEPTIONS
            existing        = 1
            is_class        = 2
            db_error        = 3
            component_error = 4
            no_access       = 5
            other           = 6
            OTHERS          = 7.
    ENDTRY.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    add_intf_source_and_activate( ).

  ENDMETHOD.
  METHOD get_interface_source.

    INSERT `INTERFACE zif_apack_manifest PUBLIC.` INTO TABLE rt_source.
    INSERT `` INTO TABLE rt_source.
    INSERT `  TYPES: BEGIN OF ty_dependency,` INTO TABLE rt_source.
    INSERT `           group_id       TYPE string,` INTO TABLE rt_source.
    INSERT `           artifact_id    TYPE string,` INTO TABLE rt_source.
    INSERT `           version        TYPE string,` INTO TABLE rt_source.
    INSERT `           git_url        TYPE string,` INTO TABLE rt_source.
    INSERT `           target_package TYPE devclass,` INTO TABLE rt_source.
    INSERT `         END OF ty_dependency,` INTO TABLE rt_source.
    INSERT `         ty_dependencies    TYPE STANDARD TABLE OF ty_dependency` INTO TABLE rt_source.
    INSERT `                            WITH NON-UNIQUE DEFAULT KEY,` INTO TABLE rt_source.
    INSERT `         ty_repository_type TYPE string,` INTO TABLE rt_source.
    INSERT `         BEGIN OF ty_descriptor,` INTO TABLE rt_source.
    INSERT `           group_id        TYPE string,` INTO TABLE rt_source.
    INSERT `           artifact_id     TYPE string,` INTO TABLE rt_source.
    INSERT `           version         TYPE string,` INTO TABLE rt_source.
    INSERT `           repository_type TYPE ty_repository_type,` INTO TABLE rt_source.
    INSERT `           git_url         TYPE string,` INTO TABLE rt_source.
    INSERT `           dependencies    TYPE ty_dependencies,` INTO TABLE rt_source.
    INSERT `         END OF ty_descriptor.` INTO TABLE rt_source.
    INSERT `` INTO TABLE rt_source.
    INSERT `  CONSTANTS: co_file_name         TYPE string VALUE '.apack-manifest.xml',` INTO TABLE rt_source.
    INSERT `             co_abap_git          TYPE ty_repository_type VALUE 'abapGit',` INTO TABLE rt_source.
    INSERT `             co_interface_version TYPE i VALUE 1.` INTO TABLE rt_source.
    INSERT `` INTO TABLE rt_source.
    INSERT `  DATA: descriptor TYPE ty_descriptor READ-ONLY.` INTO TABLE rt_source.
    INSERT `` INTO TABLE rt_source.
    INSERT `ENDINTERFACE.` INTO TABLE rt_source.

  ENDMETHOD.
  METHOD interface_exists.

    DATA: lv_interface_name TYPE seoclsname.

    SELECT SINGLE clsname FROM seoclass INTO lv_interface_name
      WHERE clsname = Lif_abapgit_apack_definitions=>c_apack_interface_cust.
    rv_interface_exists = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD interface_valid.

    FIELD-SYMBOLS: <lv_interface_vers> TYPE i.

    ASSIGN (Lif_abapgit_apack_definitions=>c_apack_interface_cust)=>('CO_INTERFACE_VERSION') TO <lv_interface_vers>.
    rv_interface_valid = boolc( <lv_interface_vers> IS ASSIGNED
      AND <lv_interface_vers> >= c_apack_interface_version ).

  ENDMETHOD.
  METHOD perform_migration.

    IF interface_exists( ) = abap_false.
      create_interface( ).
    ELSEIF interface_valid( ) = abap_false.
      add_intf_source_and_activate( ).
    ENDIF.

  ENDMETHOD.
  METHOD run.

    DATA: lo_apack_migration TYPE REF TO Lcl_abapgit_apack_migration.

    CREATE OBJECT lo_apack_migration.
    lo_apack_migration->perform_migration( ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_APACK_MIGRATION implementation

*>>>>>>> ZCL_ABAPGIT_APACK_WRITER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_apack_writer======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_apack_writer======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_apack_writer======ccau.
*"* use this source file for your ABAP unit test classes


class LCL_ABAPGIT_APACK_WRITER implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    ms_manifest_descriptor = is_apack_manifest_descriptor.
  ENDMETHOD.
  METHOD create_instance.
    CREATE OBJECT ro_manifest_writer EXPORTING is_apack_manifest_descriptor = is_apack_manifest_descriptor.
  ENDMETHOD.
  METHOD serialize.

    DATA: ls_manifest_descriptor LIKE ms_manifest_descriptor.
    FIELD-SYMBOLS: <ls_dependency> LIKE LINE OF ls_manifest_descriptor-dependencies.

    " Setting repository type automatically to 'abapGit' as there is no other one right now
    ms_manifest_descriptor-repository_type = Lif_abapgit_apack_definitions=>c_repository_type_abapgit.

    ls_manifest_descriptor = ms_manifest_descriptor.
    CLEAR: ls_manifest_descriptor-sem_version.

    LOOP AT ls_manifest_descriptor-dependencies ASSIGNING <ls_dependency>.
      CLEAR: <ls_dependency>-sem_version.
    ENDLOOP.

    CALL TRANSFORMATION id
      OPTIONS initial_components = 'suppress'
      SOURCE data = ls_manifest_descriptor
      RESULT XML rv_xml.

    rv_xml = Lcl_abapgit_xml_pretty=>print( rv_xml ).

    REPLACE FIRST OCCURRENCE
      OF REGEX '<\?xml version="1\.0" encoding="[\w-]+"\?>'
      IN rv_xml
      WITH '<?xml version="1.0" encoding="utf-8"?>'.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_APACK_WRITER implementation

*>>>>>>> ZCL_ABAPGIT_BACKGROUND_PULL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_background_pull===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_background_pull===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_BACKGROUND_PULL implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_background~get_description.

    rv_description = 'Automatic pull'.

  ENDMETHOD.
  METHOD Lif_abapgit_background~get_settings.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_background~run.

    DATA: ls_checks TYPE Lif_abapgit_definitions=>ty_deserialize_checks.

    FIELD-SYMBOLS: <ls_overwrite> LIKE LINE OF ls_checks-overwrite.


    ls_checks = io_repo->deserialize_checks( ).

    LOOP AT ls_checks-overwrite ASSIGNING <ls_overwrite>.
      <ls_overwrite>-decision = Lif_abapgit_definitions=>c_yes.
    ENDLOOP.

    io_repo->deserialize( is_checks = ls_checks
                          ii_log    = ii_log ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_BACKGROUND_PULL implementation

*>>>>>>> ZCL_ABAPGIT_BACKGROUND_PUSH_FI <<<<<<<*

*"* macro definitions
*include zcl_abapgit_background_push_ficcmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_background_push_ficcimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_BACKGROUND_PUSH_FI implementation.
*"* method's implementations
*include methods.
  METHOD build_comment.

    DATA: lt_objects TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
          lv_str     TYPE string.

    FIELD-SYMBOLS: <ls_local> LIKE LINE OF is_files-local.


    LOOP AT is_files-local ASSIGNING <ls_local>.
      lv_str = |{ <ls_local>-item-obj_type } { <ls_local>-item-obj_name }|.
      APPEND lv_str TO lt_objects.
    ENDLOOP.

    SORT lt_objects AS TEXT.
    DELETE ADJACENT DUPLICATES FROM lt_objects.

    IF lines( lt_objects ) = 1.
      rv_comment = |BG: { lv_str }|.
    ELSE.
      rv_comment = 'BG: Multiple objects'.
      LOOP AT lt_objects INTO lv_str.
        CONCATENATE rv_comment cl_abap_char_utilities=>newline lv_str INTO rv_comment.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD push_fixed.

    DATA: ls_comment TYPE Lif_abapgit_git_definitions=>ty_comment,
          ls_files   TYPE Lif_abapgit_definitions=>ty_stage_files,
          lo_stage   TYPE REF TO Lcl_abapgit_stage.

    FIELD-SYMBOLS: <ls_local>  LIKE LINE OF ls_files-local,
                   <ls_remote> LIKE LINE OF ls_files-remote.


    ls_files = Lcl_abapgit_factory=>get_stage_logic( )->get( io_repo ).
    ASSERT lines( ls_files-local ) > 0
        OR lines( ls_files-remote ) > 0.

    CREATE OBJECT lo_stage.

    LOOP AT ls_files-local ASSIGNING <ls_local>.
      mi_log->add_info( |stage: { <ls_local>-file-path } { <ls_local>-file-filename }| ).
      lo_stage->add( iv_path     = <ls_local>-file-path
                     iv_filename = <ls_local>-file-filename
                     iv_data     = <ls_local>-file-data ).
    ENDLOOP.

    LOOP AT ls_files-remote ASSIGNING <ls_remote>.

      mi_log->add_info( |removed: { <ls_remote>-path } { <ls_remote>-filename }| ).

      lo_stage->rm( iv_path     = <ls_remote>-path
                    iv_filename = <ls_remote>-filename ).

    ENDLOOP.

    ls_comment-committer-name  = iv_name.
    ls_comment-committer-email = iv_email.
    ls_comment-comment         = build_comment( ls_files ).

    io_repo->push( is_comment = ls_comment
                   io_stage   = lo_stage ).

  ENDMETHOD.
  METHOD Lif_abapgit_background~get_description.

    rv_description = 'Automatic push, fixed author'.

  ENDMETHOD.
  METHOD Lif_abapgit_background~get_settings.

    DATA: ls_setting LIKE LINE OF ct_settings.


    READ TABLE ct_settings WITH KEY key = c_settings-name INTO ls_setting.
    IF sy-subrc <> 0.
      ls_setting-key = c_settings-name.
      ls_setting-value = 'foobar'.
      APPEND ls_setting TO ct_settings.
    ENDIF.

    READ TABLE ct_settings WITH KEY key = c_settings-email INTO ls_setting.
    IF sy-subrc <> 0.
      ls_setting-key = c_settings-email.
      ls_setting-value = 'foobar@localhost'.
      APPEND ls_setting TO ct_settings.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_background~run.

    DATA: ls_files   TYPE Lif_abapgit_definitions=>ty_stage_files,
          ls_setting LIKE LINE OF it_settings,
          lv_name    TYPE string,
          lv_email   TYPE string.

    mi_log = ii_log.
    ls_files = Lcl_abapgit_factory=>get_stage_logic( )->get( io_repo ).

    IF lines( ls_files-local ) = 0 AND lines( ls_files-remote ) = 0.
      ii_log->add_info( 'Nothing to stage' ).
      RETURN.
    ENDIF.

    READ TABLE it_settings WITH KEY key = c_settings-name INTO ls_setting. "#EC CI_SUBRC
    lv_name = ls_setting-value.

    READ TABLE it_settings WITH KEY key = c_settings-email INTO ls_setting. "#EC CI_SUBRC
    lv_email = ls_setting-value.

    push_fixed(
      io_repo  = io_repo
      iv_name  = lv_name
      iv_email = lv_email ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_BACKGROUND_PUSH_FI implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_GSMP <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_gsmp=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_gsmp=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_GSMP implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    DATA lv_name TYPE c LENGTH 180.
    DATA lv_user  TYPE string.
    DATA lx_root TYPE REF TO cx_root.


    TRY.
        lv_name = ms_item-obj_name.

        SELECT SINGLE changed_by INTO lv_user
          FROM ('GSM_MD_PRV_W')
          WHERE provider_id = lv_name AND version = 'I'.

        IF lv_user IS INITIAL.
          SELECT SINGLE changed_by INTO lv_user
            FROM ('GSM_MD_PRV_W')
            WHERE provider_id = lv_name AND version = 'A'.
        ENDIF.

        rv_user = lv_user.
      CATCH cx_root INTO lx_root.
        Lcx_abapgit_exception=>raise( iv_text     = lx_root->get_text( )
                                     ix_previous = lx_root ).
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_GSMP implementation

*>>>>>>> ZCL_ABAPGIT_DEFAULT_TRANSPORT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_default_transport=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_default_transport=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_DEFAULT_TRANSPORT implementation.
*"* method's implementations
*include methods.
  METHOD clear.

    CALL FUNCTION 'TR_TASK_RESET'
      EXPORTING
        iv_username      = is_default_task-username
        iv_order         = is_default_task-ordernum
        iv_task          = is_default_task-tasknum
        iv_dialog        = abap_false
      EXCEPTIONS
        invalid_username = 1
        invalid_order    = 2
        invalid_task     = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    store( ).

  ENDMETHOD.
  METHOD get.

    DATA: lt_e070use TYPE STANDARD TABLE OF e070use.

    CALL FUNCTION 'TR_TASK_GET'
      TABLES
        tt_e070use       = lt_e070use
      EXCEPTIONS
        invalid_username = 1
        invalid_category = 2
        invalid_client   = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    READ TABLE lt_e070use INTO rs_default_task
                          INDEX 1.

  ENDMETHOD.
  METHOD get_instance.

    IF go_instance IS NOT BOUND.
      CREATE OBJECT go_instance.
    ENDIF.

    ro_instance = go_instance.

  ENDMETHOD.
  METHOD reset.

    DATA: ls_default_task TYPE e070use.

    IF mv_is_set_by_abapgit = abap_false.
      " if the default transport request task isn't set
      " by us there is nothing to do.
      RETURN.
    ENDIF.

    CLEAR mv_is_set_by_abapgit.

    ls_default_task = get( ).

    IF ls_default_task IS NOT INITIAL.

      clear( ls_default_task ).

    ENDIF.

    restore( ).

  ENDMETHOD.
  METHOD restore.

    IF ms_save IS INITIAL.
      " There wasn't a default transport request before
      " so we needn't restore anything.
      RETURN.
    ENDIF.

    CALL FUNCTION 'TR_TASK_SET'
      EXPORTING
        iv_order          = ms_save-ordernum
        iv_task           = ms_save-tasknum
      EXCEPTIONS
        invalid_username  = 1
        invalid_category  = 2
        invalid_client    = 3
        invalid_validdays = 4
        invalid_order     = 5
        invalid_task      = 6
        OTHERS            = 7.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD set.

    " checks whether object changes of the package are rerorded in transport
    " requests. If true then we set the default task, so that no annoying
    " transport request popups are shown while deserializing.

    IF mv_is_set_by_abapgit = abap_true.
      " the default transport request task is already set by us
      " -> no reason to do it again.
      RETURN.
    ENDIF.

    IF iv_transport IS INITIAL.
      Lcx_abapgit_exception=>raise( |No transport request was supplied| ).
    ENDIF.

    set_internal( iv_transport ).

    mv_is_set_by_abapgit = abap_true.

  ENDMETHOD.
  METHOD set_internal.

    CALL FUNCTION 'TR_TASK_SET'
      EXPORTING
        iv_order          = iv_transport
        iv_validdays      = 1
      EXCEPTIONS
        invalid_username  = 1
        invalid_category  = 2
        invalid_client    = 3
        invalid_validdays = 4
        invalid_order     = 5
        invalid_task      = 6
        OTHERS            = 7.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD store.

    ms_save = get( ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_DEFAULT_TRANSPORT implementation

*>>>>>>> ZCL_ABAPGIT_ECATT_CONFIG_UPL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ecatt_config_upl==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ecatt_config_upl==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ECATT_CONFIG_UPL implementation.
*"* method's implementations
*include methods.
  METHOD upload_data_from_stream.

    " Downport
    template_over_all = Lcl_abapgit_ecatt_helper=>upload_data_from_stream( mv_external_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_ecatt_upload~set_stream_for_upload.

    " downport from CL_ABAPGIT_ECATT_DATA_UPLOAD SET_STREAM_FOR_UPLOAD
    mv_external_xml = iv_xml.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ECATT_CONFIG_UPL implementation

*>>>>>>> ZCL_ABAPGIT_ECATT_SCRIPT_UPL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ecatt_script_upl==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ecatt_script_upl==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ECATT_SCRIPT_UPL implementation.
*"* method's implementations
*include methods.
  METHOD upload_data_from_stream.

    " Downport
    template_over_all = Lcl_abapgit_ecatt_helper=>upload_data_from_stream( mv_external_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_ecatt_upload~set_stream_for_upload.

    " downport from CL_ABAPGIT_ECATT_DATA_UPLOAD SET_STREAM_FOR_UPLOAD
    mv_external_xml = iv_xml.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ECATT_SCRIPT_UPL implementation

*>>>>>>> ZCL_ABAPGIT_USER_RECORD <<<<<<<*

*"* macro definitions
*include zcl_abapgit_user_record=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_user_record=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_user_record=======ccau.
*CLASS SHRITEFUH64VYIPN5I4UHL45BP4R4A DEFINITION DEFERRED.

*CLASS zcl_abapgit_user_record DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BP4R4A.




class LCL_ABAPGIT_USER_RECORD implementation.
*"* method's implementations
*include methods.
  METHOD check_user_exists.

    DATA lt_return  TYPE STANDARD TABLE OF bapiret2 WITH DEFAULT KEY.
    DATA ls_address TYPE bapiaddr3.
    DATA lt_smtp    TYPE TABLE OF bapiadsmtp.
    DATA ls_smtp    LIKE LINE OF lt_smtp.

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = iv_user
      IMPORTING
        address  = ls_address
      TABLES
        return   = lt_return
        addsmtp  = lt_smtp.
    LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type CA 'EA'.
      Lcx_abapgit_exception=>raise( |User: { iv_user } not found| ).
    ENDLOOP.

    ev_fullname = ls_address-fullname.

    " Choose the first email from SU01
    SORT lt_smtp BY consnumber ASCENDING.

    LOOP AT lt_smtp INTO ls_smtp.
      ev_email = ls_smtp-e_mail.
      EXIT.
    ENDLOOP.

  ENDMETHOD.
  METHOD constructor.

    DATA ls_user TYPE ty_user.

    " Get user details
    TRY.
        check_user_exists(
          EXPORTING
            iv_user     = iv_user
          IMPORTING
            ev_fullname = ms_user-name
            ev_email    = ms_user-email ).
      CATCH Lcx_abapgit_exception.
        " Could not find user, try to get from other clients
        get_user_dtls_from_other_clnt( iv_user ).
    ENDTRY.

    " If the user has been found add it to the list
    IF ms_user-name IS NOT INITIAL AND ms_user-email IS NOT INITIAL.
      ls_user-user = iv_user.
      ls_user-o_user = me.
      INSERT ls_user INTO TABLE gt_user.
    ENDIF.

  ENDMETHOD.
  METHOD get_email.

    rv_email = ms_user-email.

  ENDMETHOD.
  METHOD get_instance.

    FIELD-SYMBOLS <ls_user> TYPE ty_user.

    READ TABLE gt_user ASSIGNING <ls_user> WITH TABLE KEY user = iv_user.
    IF sy-subrc = 0.
      ro_user = <ls_user>-o_user.
    ELSE.
      CREATE OBJECT ro_user
        EXPORTING
          iv_user = iv_user.
    ENDIF.

  ENDMETHOD.
  METHOD get_name.

    rv_name = ms_user-name.

  ENDMETHOD.
  METHOD get_user_dtls_from_other_clnt.

    CONSTANTS lc_cc_category TYPE string VALUE 'C'.
    TYPES ty_dev_clients TYPE SORTED TABLE OF sy-mandt WITH UNIQUE KEY table_line.
    DATA lt_dev_clients TYPE ty_dev_clients.
    FIELD-SYMBOLS <lv_dev_client> LIKE LINE OF lt_dev_clients.

    " Could not find the user, try other development clients
    SELECT mandt FROM t000 INTO TABLE lt_dev_clients
        WHERE cccategory = lc_cc_category AND mandt <> sy-mandt
        ORDER BY PRIMARY KEY.

    LOOP AT lt_dev_clients ASSIGNING <lv_dev_client>.
      SELECT SINGLE p~name_text a~smtp_addr INTO (ms_user-name, ms_user-email)
          FROM usr21 AS u
          INNER JOIN adrp AS p ON p~persnumber = u~persnumber
                              AND p~client     = u~mandt
          INNER JOIN adr6 AS a ON a~persnumber = u~persnumber
                              AND a~addrnumber = u~addrnumber
                              AND a~client     = u~mandt
          CLIENT SPECIFIED
          WHERE u~mandt      = <lv_dev_client>
            AND u~bname      = iv_user
            AND p~date_from <= sy-datum
            AND p~date_to   >= sy-datum
            AND a~date_from <= sy-datum.
      IF sy-subrc = 0.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD reset.
    CLEAR gt_user.
  ENDMETHOD.
  METHOD get_title.
* the queried username might not exist, so this method is static

    DATA ls_user_address TYPE addr3_val.

    CALL FUNCTION 'SUSR_USER_ADDRESS_READ'
      EXPORTING
        user_name              = iv_username
      IMPORTING
        user_address           = ls_user_address
      EXCEPTIONS
        user_address_not_found = 1
        OTHERS                 = 2.
    IF sy-subrc = 0.
      rv_title = ls_user_address-name_text.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_USER_RECORD implementation

*>>>>>>> ZCX_ABAPGIT_CANCEL <<<<<<<*

*"* macro definitions
*include zcx_abapgit_cancel============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcx_abapgit_cancel============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCX_ABAPGIT_CANCEL implementation.
*"* method's implementations
*include methods.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor(
      previous = previous
      log      = log
      msgv1    = msgv1
      msgv2    = msgv2
      msgv3    = msgv3
      msgv4    = msgv4
      longtext = longtext ).

    CLEAR me->textid.

    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
endclass. "ZCX_ABAPGIT_CANCEL implementation

*>>>>>>> ZCL_ABAPGIT_XML_PRETTY <<<<<<<*

*"* macro definitions
*include zcl_abapgit_xml_pretty========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_xml_pretty========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_xml_pretty========ccau.




class LCL_ABAPGIT_XML_PRETTY implementation.
*"* method's implementations
*include methods.
  METHOD print.

    DATA: li_ixml           TYPE REF TO if_ixml,
          li_xml_doc        TYPE REF TO if_ixml_document,
          li_stream_factory TYPE REF TO if_ixml_stream_factory,
          li_istream        TYPE REF TO if_ixml_istream,
          li_parser         TYPE REF TO if_ixml_parser,
          lv_xstring        TYPE xstring,
          li_encoding       TYPE REF TO if_ixml_encoding,
          li_ostream        TYPE REF TO if_ixml_ostream,
          li_renderer       TYPE REF TO if_ixml_renderer.


    ASSERT NOT iv_xml IS INITIAL.

    li_ixml    = cl_ixml=>create( ).
    li_xml_doc = li_ixml->create_document( ).

    li_stream_factory = li_ixml->create_stream_factory( ).
    li_istream        = li_stream_factory->create_istream_xstring(
      Lcl_abapgit_convert=>string_to_xstring_utf8( iv_xml ) ).
    li_parser         = li_ixml->create_parser( stream_factory = li_stream_factory
                                                istream        = li_istream
                                                document       = li_xml_doc ).
    li_parser->set_normalizing( abap_true ).
    IF li_parser->parse( ) <> 0.
      IF iv_ignore_errors = abap_true.
        rv_xml = iv_xml.
        RETURN.
      ELSE.
        Lcx_abapgit_exception=>raise( 'error parsing xml' ).
      ENDIF.
    ENDIF.


    li_ostream  = li_stream_factory->create_ostream_xstring( lv_xstring ).

    li_encoding = li_ixml->create_encoding(
      character_set = 'utf-8'
      byte_order    = if_ixml_encoding=>co_big_endian ).

    li_xml_doc->set_encoding( li_encoding ).
    li_renderer = li_ixml->create_renderer( ostream  = li_ostream
                                            document = li_xml_doc ).

    li_renderer->set_normalizing( boolc( iv_unpretty = abap_false ) ).

    li_renderer->render( ).

    rv_xml = Lcl_abapgit_convert=>xstring_to_string_utf8_bom( lv_xstring ).
    REPLACE FIRST OCCURRENCE OF 'utf-8' IN rv_xml WITH 'utf-16'.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_XML_PRETTY implementation

*>>>>>>> ZCX_ABAPGIT_NOT_FOUND <<<<<<<*

*"* macro definitions
*include zcx_abapgit_not_found=========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcx_abapgit_not_found=========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCX_ABAPGIT_NOT_FOUND implementation.
*"* method's implementations
*include methods.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor(
      textid   = textid
      previous = previous ).
  ENDMETHOD.
endclass. "ZCX_ABAPGIT_NOT_FOUND implementation

*>>>>>>> ZCL_ABAPGIT_GIT_ADD_PATCH <<<<<<<*

*"* macro definitions
*include zcl_abapgit_git_add_patch=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_git_add_patch=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_git_add_patch=====ccau.
*"* use this source file for your ABAP unit test classes




class LCL_ABAPGIT_GIT_ADD_PATCH implementation.
*"* method's implementations
*include methods.
  METHOD calculate_patch.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF mt_diff.

    LOOP AT mt_diff ASSIGNING <ls_diff>.

      CASE <ls_diff>-result.
        WHEN Lif_abapgit_definitions=>c_diff-unchanged.

          INSERT <ls_diff>-old INTO TABLE rt_patch.

        WHEN Lif_abapgit_definitions=>c_diff-insert.

          IF <ls_diff>-patch_flag = abap_true.
            INSERT <ls_diff>-new INTO TABLE rt_patch.
          ENDIF.

        WHEN Lif_abapgit_definitions=>c_diff-delete.

          IF <ls_diff>-patch_flag = abap_false.
            INSERT <ls_diff>-old INTO TABLE rt_patch.
          ENDIF.

        WHEN Lif_abapgit_definitions=>c_diff-update.

          IF <ls_diff>-patch_flag = abap_true.
            INSERT <ls_diff>-new INTO TABLE rt_patch.
          ELSE.
            INSERT <ls_diff>-old INTO TABLE rt_patch.
          ENDIF.

        WHEN OTHERS.

          Lcx_abapgit_exception=>raise( |Unknown result| ).

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.
  METHOD constructor.

    mt_diff = it_diff.

  ENDMETHOD.
  METHOD get_patch.

    IF mt_patch IS INITIAL.
      mt_patch = calculate_patch( ).
    ENDIF.

    rt_patch = mt_patch.

  ENDMETHOD.
  METHOD get_patch_binary.

    DATA: lv_string TYPE string.

    IF mt_patch IS INITIAL.
      mt_patch = calculate_patch( ).
    ENDIF.

    CONCATENATE LINES OF mt_patch INTO lv_string SEPARATED BY cl_abap_char_utilities=>newline.
    lv_string = lv_string && cl_abap_char_utilities=>newline.

    rv_patch_binary = Lcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GIT_ADD_PATCH implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_DRUL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_drul=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_drul=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_DRUL implementation.
*"* method's implementations
*include methods.
  METHOD clear_field.

    FIELD-SYMBOLS: <lv_value> TYPE data.

    ASSIGN COMPONENT iv_fieldname OF STRUCTURE cs_dependency_rule
           TO <lv_value>.
    ASSERT sy-subrc = 0.

    CLEAR: <lv_value>.

  ENDMETHOD.
  METHOD clear_fields.

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CREATED_AT'
      CHANGING
        cs_dependency_rule = cs_dependency_rule ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CREATED_BY'
      CHANGING
        cs_dependency_rule = cs_dependency_rule ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CHANGED_AT'
      CHANGING
        cs_dependency_rule = cs_dependency_rule ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CHANGED_BY'
      CHANGING
        cs_dependency_rule = cs_dependency_rule ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-MASTER_LANGUAGE'
      CHANGING
        cs_dependency_rule = cs_dependency_rule ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-RESPONSIBLE'
      CHANGING
        cs_dependency_rule = cs_dependency_rule ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-PACKAGE_REF'
      CHANGING
        cs_dependency_rule = cs_dependency_rule ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'CONTENT-SOURCE'
      CHANGING
        cs_dependency_rule = cs_dependency_rule ).

  ENDMETHOD.
  METHOD constructor.

    super->constructor(
        is_item     = is_item
        iv_language = iv_language ).

    mv_dependency_rule_key = ms_item-obj_name.

    TRY.
        CREATE DATA mr_dependency_rule TYPE ('CL_BLUE_SOURCE_OBJECT_DATA=>TY_OBJECT_DATA').
        CREATE OBJECT mi_persistence TYPE ('CL_DRUL_WB_OBJECT_PERSIST').

      CATCH cx_sy_create_error.
        Lcx_abapgit_exception=>raise( |DRUL not supported by your NW release| ).
    ENDTRY.

  ENDMETHOD.
  METHOD fill_metadata_from_db.

    DATA:
      li_wb_object_operator          TYPE REF TO object,
      lr_dependency_rule_old         TYPE REF TO data,
      lv_drul_object_data_clas_exist TYPE c LENGTH 1.

    FIELD-SYMBOLS:
      <ls_dependency_rule_old> TYPE any,
      <lv_created_at>          TYPE xsddatetime_z,
      <lv_created_by>          TYPE syuname,
      <lv_created_at_old>      TYPE xsddatetime_z,
      <lv_created_by_old>      TYPE syuname.

    li_wb_object_operator = get_wb_object_operator( ).

    CREATE DATA lr_dependency_rule_old TYPE ('CL_BLUE_SOURCE_OBJECT_DATA2=>TY_OBJECT_DATA').
    CALL FUNCTION 'CHECK_EXIST_CLAS'
      EXPORTING
        name            = 'CL_DRUL_WB_OBJECT_DATA'
      IMPORTING
        exist           = lv_drul_object_data_clas_exist
      EXCEPTIONS
        tr_invalid_type = 1
        OTHERS          = 2.

    IF sy-subrc = 0 AND lv_drul_object_data_clas_exist = abap_true.
      CREATE DATA lr_dependency_rule_old TYPE ('CL_DRUL_WB_OBJECT_DATA=>TY_OBJECT_DATA').
    ELSE.
      CREATE DATA lr_dependency_rule_old TYPE ('CL_BLUE_SOURCE_OBJECT_DATA=>TY_OBJECT_DATA').
    ENDIF.
    ASSIGN lr_dependency_rule_old->* TO <ls_dependency_rule_old>.
    ASSERT sy-subrc = 0.

    CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
      IMPORTING
        data = <ls_dependency_rule_old>.

    ASSIGN COMPONENT 'METADATA-CREATED_BY' OF STRUCTURE cs_dependency_rule
           TO <lv_created_by>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'METADATA-CREATED_AT' OF STRUCTURE cs_dependency_rule
           TO <lv_created_at>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'METADATA-CREATED_BY' OF STRUCTURE <ls_dependency_rule_old>
           TO <lv_created_by_old>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'METADATA-CREATED_AT' OF STRUCTURE <ls_dependency_rule_old>
           TO <lv_created_at_old>.
    ASSERT sy-subrc = 0.

    <lv_created_at> = <lv_created_at_old>.
    <lv_created_by> = <lv_created_by_old>.

  ENDMETHOD.
  METHOD get_wb_object_operator.

    DATA:
      ls_object_type TYPE wbobjtype,
      lx_error       TYPE REF TO cx_root.

    IF mi_wb_object_operator IS BOUND.
      ri_wb_object_operator = mi_wb_object_operator.
    ENDIF.

    ls_object_type-objtype_tr = 'DRUL'.
    ls_object_type-subtype_wb = 'DRL'.

    TRY.
        CALL METHOD ('CL_WB_OBJECT_OPERATOR')=>('CREATE_INSTANCE')
          EXPORTING
            object_type = ls_object_type
            object_key  = mv_dependency_rule_key
          RECEIVING
            result      = mi_wb_object_operator.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    ri_wb_object_operator = mi_wb_object_operator.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA:
      li_wb_object_operator TYPE REF TO object,
      li_object_data_model  TYPE REF TO if_wb_object_data_model,
      lx_error              TYPE REF TO cx_root.

    TRY.
        li_wb_object_operator = get_wb_object_operator( ).

        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
          IMPORTING
            eo_object_data = li_object_data_model.

        rv_user = li_object_data_model->get_changed_by( ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA:
      lx_error              TYPE REF TO cx_root,
      li_wb_object_operator TYPE REF TO object.

    li_wb_object_operator = get_wb_object_operator( ).

    TRY.
        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~DELETE')
          EXPORTING
            transport_request = iv_transport.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA:
      li_object_data_model           TYPE REF TO if_wb_object_data_model,
      li_wb_object_operator          TYPE REF TO object,
      lx_error                       TYPE REF TO cx_root,
      lv_drul_object_data_clas_exist TYPE c LENGTH 1.

    FIELD-SYMBOLS:
      <ls_dependency_rule> TYPE any,
      <lv_source>          TYPE data.

    ASSIGN mr_dependency_rule->* TO <ls_dependency_rule>.
    ASSERT sy-subrc = 0.

    io_xml->read(
      EXPORTING
        iv_name = 'DRUL'
      CHANGING
        cg_data = <ls_dependency_rule> ).

    li_wb_object_operator = get_wb_object_operator( ).

    TRY.
        CALL FUNCTION 'CHECK_EXIST_CLAS'
          EXPORTING
            name            = 'CL_DRUL_WB_OBJECT_DATA'
          IMPORTING
            exist           = lv_drul_object_data_clas_exist
          EXCEPTIONS
            tr_invalid_type = 1
            OTHERS          = 2.

        IF sy-subrc = 0 AND lv_drul_object_data_clas_exist = abap_true.
          CREATE OBJECT li_object_data_model TYPE ('CL_DRUL_WB_OBJECT_DATA').
        ELSE.
          CREATE OBJECT li_object_data_model TYPE ('CL_BLUE_SOURCE_OBJECT_DATA').
        ENDIF.

        ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <ls_dependency_rule>
               TO <lv_source>.
        ASSERT sy-subrc = 0.

        <lv_source> = Lif_abapgit_object~mo_files->read_string( 'asdrul' ).

        tadir_insert( iv_package ).

        IF Lif_abapgit_object~exists( ) = abap_true.

          " We need to populate created_at, created_by, because otherwise update  is not possible
          fill_metadata_from_db( CHANGING cs_dependency_rule = <ls_dependency_rule> ).
          li_object_data_model->set_data( <ls_dependency_rule> ).

          CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
            EXPORTING
              io_object_data    = li_object_data_model
              transport_request = iv_transport.

        ELSE.

          li_object_data_model->set_data( <ls_dependency_rule> ).

          CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~CREATE')
            EXPORTING
              io_object_data    = li_object_data_model
              data_selection    = 'P' " if_wb_object_data_selection_co=>c_properties
              package           = iv_package
              transport_request = iv_transport.

          CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
            EXPORTING
              io_object_data    = li_object_data_model
              data_selection    = 'D' " if_wb_object_data_selection_co=>c_data_content
              transport_request = iv_transport.

        ENDIF.

        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~ACTIVATE').

        corr_insert( iv_package ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    TRY.
        mi_persistence->get(
            p_object_key           = mv_dependency_rule_key
            p_version              = 'A'
            p_existence_check_only = abap_true ).
        rv_bool = abap_true.

      CATCH cx_swb_exception.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-ddic TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESWB_EO'
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA:
      li_wb_object_operator TYPE REF TO object,
      li_object_data_model  TYPE REF TO if_wb_object_data_model,
      lx_error              TYPE REF TO cx_root,
      lv_source             TYPE string.

    FIELD-SYMBOLS:
      <ls_dependency_rule> TYPE any,
      <lv_source>          TYPE string.

    ASSIGN mr_dependency_rule->* TO <ls_dependency_rule>.
    ASSERT sy-subrc = 0.

    li_wb_object_operator = get_wb_object_operator( ).

    TRY.
        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
          EXPORTING
            version        = 'A'
          IMPORTING
            data           = <ls_dependency_rule>
            eo_object_data = li_object_data_model.

        ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <ls_dependency_rule>
               TO <lv_source>.
        ASSERT sy-subrc = 0.

        lv_source = <lv_source>.

        clear_fields( CHANGING cs_dependency_rule = <ls_dependency_rule> ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    io_xml->add(
        iv_name = 'DRUL'
        ig_data = <ls_dependency_rule> ).

    Lif_abapgit_object~mo_files->add_string(
        iv_ext    = 'asdrul'
        iv_string = lv_source ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_DRUL implementation

*>>>>>>> ZCL_ABAPGIT_GIT_URL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_git_url===========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_git_url===========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_git_url===========ccau.
*CLASS SHRITEFUH64VYIPN5I4UHL45BHRR4A DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_url DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BHRR4A.




class LCL_ABAPGIT_GIT_URL implementation.
*"* method's implementations
*include methods.
  METHOD get_commit_display_url.

    DATA li_exit TYPE REF TO Lif_abapgit_exit.

    rv_url = get_default_commit_display_url(
      iv_repo_url = io_repo->get_url( )
      iv_hash     = io_repo->get_current_remote( ) ).

    li_exit = Lcl_abapgit_exit=>get_instance( ).
    li_exit->adjust_display_commit_url(
      EXPORTING
        iv_repo_url    = io_repo->get_url( )
        iv_repo_name   = io_repo->get_name( )
        iv_repo_key    = io_repo->get_key( )
        iv_commit_hash = io_repo->get_current_remote( )
      CHANGING
        cv_display_url = rv_url ).

    IF rv_url IS INITIAL.
      Lcx_abapgit_exception=>raise( |provider not yet supported| ).
    ENDIF.

  ENDMETHOD.
  METHOD get_default_commit_display_url.

    DATA ls_result TYPE match_result.
    FIELD-SYMBOLS <ls_provider_match> TYPE submatch_result.

    rv_commit_url = iv_repo_url.

    FIND REGEX '^http(?:s)?:\/\/(?:www\.)?(github\.com|bitbucket\.org|gitlab\.com)\/'
      IN rv_commit_url
      RESULTS ls_result.
    IF sy-subrc = 0.
      READ TABLE ls_result-submatches INDEX 1 ASSIGNING <ls_provider_match>.
      CASE rv_commit_url+<ls_provider_match>-offset(<ls_provider_match>-length).
        WHEN 'github.com'.
          REPLACE REGEX '\.git$' IN rv_commit_url WITH space.
          rv_commit_url = rv_commit_url && |/commit/| && iv_hash.
        WHEN 'bitbucket.org'.
          REPLACE REGEX '\.git$' IN rv_commit_url WITH space.
          rv_commit_url = rv_commit_url && |/commits/| && iv_hash.
        WHEN 'gitlab.com'.
          REPLACE REGEX '\.git$' IN rv_commit_url WITH space.
          rv_commit_url = rv_commit_url && |/-/commit/| && iv_hash.
      ENDCASE.
    ENDIF.

  ENDMETHOD.
  METHOD validate_url.

    DATA lv_provider TYPE string.

    lv_provider = Lcl_abapgit_url=>host( to_lower( iv_url ) ).

    " Provider-specific check for URLs that don't work
    IF lv_provider CS 'gitlab.com'.
      FIND REGEX '\.git$' IN iv_url IGNORING CASE.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'Repo URL for GitLab must end in ".git"' ).
      ENDIF.
    ELSEIF lv_provider CS 'dev.azure.com'.
      FIND REGEX '\.git$' IN iv_url IGNORING CASE.
      IF sy-subrc = 0.
        Lcx_abapgit_exception=>raise( 'Repo URL for Azure DevOps must not end in ".git"' ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GIT_URL implementation

*>>>>>>> ZCL_ABAPGIT_GIT_UTILS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_git_utils=========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_git_utils=========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_git_utils=========ccau.


class LCL_ABAPGIT_GIT_UTILS implementation.
*"* method's implementations
*include methods.
  METHOD get_null.

* must be length 4, or it gives a syntax error on lower versions
    DATA: lv_x TYPE x LENGTH 4 VALUE '00000000'.
    FIELD-SYMBOLS <lv_y> TYPE c.

    ASSIGN lv_x TO <lv_y> CASTING.
    rv_c = <lv_y>.

  ENDMETHOD.
  METHOD length_utf8_hex.

    DATA: lv_xstring TYPE xstring,
          lv_char4   TYPE c LENGTH 4,
          lv_x       TYPE x LENGTH 2.

    IF xstrlen( iv_data ) < 4.
      Lcx_abapgit_exception=>raise( 'error converting to hex, LENGTH_UTF8_HEX' ).
    ENDIF.

    lv_xstring = iv_data(4).

    lv_char4 = Lcl_abapgit_convert=>xstring_to_string_utf8(
      iv_data   = lv_xstring
      iv_length = 4 ).

    TRANSLATE lv_char4 TO UPPER CASE.
    lv_x = lv_char4.
    rv_len = lv_x.

  ENDMETHOD.
  METHOD pkt_string.

    DATA: lv_x   TYPE x,
          lv_len TYPE i.


    lv_len = strlen( iv_string ).

    IF lv_len >= 255.
      Lcx_abapgit_exception=>raise( 'PKT, todo' ).
    ENDIF.

    lv_x = lv_len + 4.

    rv_pkt = '00' && lv_x && iv_string.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GIT_UTILS implementation

*>>>>>>> ZCL_ABAPGIT_ADT_LINK <<<<<<<*

*"* macro definitions
*include zcl_abapgit_adt_link==========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_adt_link==========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ADT_LINK implementation.
*"* method's implementations
*include methods.
  METHOD link_transport.
* call to CL_CTS_ADT_TM_URI_BUILDER=>CREATE_ADT_URI replaced with logic that works on all systems,
    rv_link = |adt://{ sy-sysid }/sap/bc/adt/cts/transportrequests/{ iv_transport }|.
  ENDMETHOD.
  METHOD generate.

    DATA: lv_adt_link       TYPE string.
    DATA: lo_adt_uri_mapper TYPE REF TO object.
    DATA: lo_adt_objref     TYPE REF TO object.
    DATA: lo_adt_sub_objref TYPE REF TO object.
    DATA: lv_program        TYPE progname.
    DATA: lv_include        TYPE progname.

    FIELD-SYMBOLS: <lv_uri> TYPE string.

    get_adt_objects_and_names(
      EXPORTING
        iv_obj_name       = iv_obj_name
        iv_obj_type       = iv_obj_type
      IMPORTING
        eo_adt_uri_mapper = lo_adt_uri_mapper
        eo_adt_objectref  = lo_adt_objref
        ev_program        = lv_program
        ev_include        = lv_include ).

    TRY.
        IF iv_sub_obj_name IS NOT INITIAL.

          IF ( lv_program <> iv_obj_name AND lv_include IS INITIAL ) OR
             ( lv_program = lv_include AND iv_sub_obj_name IS NOT INITIAL ).
            lv_include = iv_sub_obj_name.
          ENDIF.

          CALL METHOD lo_adt_uri_mapper->('IF_ADT_URI_MAPPER~MAP_INCLUDE_TO_OBJREF')
            EXPORTING
              program     = lv_program
              include     = lv_include
              line        = iv_line_number
              line_offset = 0
              end_line    = iv_line_number
              end_offset  = 1
            RECEIVING
              result      = lo_adt_sub_objref.
          IF lo_adt_sub_objref IS NOT INITIAL.
            lo_adt_objref = lo_adt_sub_objref.
          ENDIF.

        ENDIF.

        ASSIGN ('LO_ADT_OBJREF->REF_DATA-URI') TO <lv_uri>.
        ASSERT sy-subrc = 0.

        CONCATENATE 'adt://' sy-sysid <lv_uri> INTO lv_adt_link.

        rv_result = lv_adt_link.

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( 'ADT Jump Error' ).
    ENDTRY.

  ENDMETHOD.
  METHOD get_adt_objects_and_names.

    DATA lv_obj_type       TYPE trobjtype.
    DATA lv_obj_name       TYPE trobj_name.
    DATA lo_object         TYPE REF TO cl_wb_object.
    DATA lo_adt            TYPE REF TO object.

    FIELD-SYMBOLS <lv_uri> TYPE string.

    lv_obj_name = iv_obj_name.
    lv_obj_type = iv_obj_type.

    TRY.
        cl_wb_object=>create_from_transport_key(
          EXPORTING
            p_object    = lv_obj_type
            p_obj_name  = lv_obj_name
          RECEIVING
            p_wb_object = lo_object
          EXCEPTIONS
            OTHERS      = 1 ).
        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise( 'ADT Jump Error' ).
        ENDIF.

        CALL METHOD ('CL_ADT_TOOLS_CORE_FACTORY')=>('GET_INSTANCE')
          RECEIVING
            result = lo_adt.

        IF is_adt_jump_possible( io_object = lo_object
                                 io_adt    = lo_adt ) = abap_false.
          Lcx_abapgit_exception=>raise( 'ADT Jump Error' ).
        ENDIF.

        CALL METHOD lo_adt->('IF_ADT_TOOLS_CORE_FACTORY~GET_URI_MAPPER')
          RECEIVING
            result = eo_adt_uri_mapper.

        CALL METHOD eo_adt_uri_mapper->('IF_ADT_URI_MAPPER~MAP_WB_OBJECT_TO_OBJREF')
          EXPORTING
            wb_object = lo_object
          RECEIVING
            result    = eo_adt_objectref.

        ASSIGN ('EO_ADT_OBJECTREF->REF_DATA-URI') TO <lv_uri>.
        ASSERT sy-subrc = 0.

        CALL METHOD eo_adt_uri_mapper->('IF_ADT_URI_MAPPER~MAP_OBJREF_TO_INCLUDE')
          EXPORTING
            uri     = <lv_uri>
          IMPORTING
            program = ev_program
            include = ev_include.

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( 'ADT Jump Error' ).
    ENDTRY.

  ENDMETHOD.
  METHOD is_adt_jump_possible.

    DATA: lo_wb_request         TYPE REF TO cl_wb_request,
          lo_adt_uri_mapper_vit TYPE REF TO object,
          lv_vit_wb_request     TYPE abap_bool.

    cl_wb_request=>create_from_object_ref(
      EXPORTING
        p_wb_object       = io_object
      RECEIVING
        p_wb_request      = lo_wb_request
      EXCEPTIONS
        illegal_operation = 1
        cancelled         = 2
        OTHERS            = 3 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'ADT Jump Error' ).
    ENDIF.

    TRY.
        CALL METHOD io_adt->('IF_ADT_TOOLS_CORE_FACTORY~GET_URI_MAPPER_VIT')
          RECEIVING
            result = lo_adt_uri_mapper_vit.

        CALL METHOD lo_adt_uri_mapper_vit->('IF_ADT_URI_MAPPER_VIT~IS_VIT_WB_REQUEST')
          EXPORTING
            wb_request = lo_wb_request
          RECEIVING
            result     = lv_vit_wb_request.

        rv_is_adt_jump_possible = boolc( NOT lv_vit_wb_request = abap_true ).

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( 'ADT Jump Error' ).
    ENDTRY.

  ENDMETHOD.
  METHOD jump.

    DATA lv_adt_link TYPE string.

    TRY.
        lv_adt_link = generate(
          iv_obj_name     = iv_obj_name
          iv_obj_type     = iv_obj_type
          iv_sub_obj_name = iv_sub_obj_name
          iv_line_number  = iv_line_number ).

        Lcl_abapgit_ui_factory=>get_frontend_services( )->execute( iv_document = lv_adt_link ).

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( 'ADT Jump Error' ).
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ADT_LINK implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ENHO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_enho=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_enho=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ENHO implementation.
*"* method's implementations
*include methods.
  METHOD factory.

    CASE iv_tool.
      WHEN cl_enh_tool_badi_impl=>tooltype.
        CREATE OBJECT ri_enho TYPE Lcl_abapgit_object_enho_badi
          EXPORTING
            is_item = ms_item.
      WHEN cl_enh_tool_hook_impl=>tooltype.
        CREATE OBJECT ri_enho TYPE Lcl_abapgit_object_enho_hook
          EXPORTING
            is_item  = ms_item
            io_files = Lif_abapgit_object~mo_files.
      WHEN cl_enh_tool_class=>tooltype.
        CREATE OBJECT ri_enho TYPE Lcl_abapgit_object_enho_class
          EXPORTING
            is_item  = ms_item
            io_files = Lif_abapgit_object~mo_files.
      WHEN cl_enh_tool_intf=>tooltype.
        CREATE OBJECT ri_enho TYPE Lcl_abapgit_object_enho_intf
          EXPORTING
            is_item  = ms_item
            io_files = Lif_abapgit_object~mo_files.
      WHEN cl_wdr_cfg_enhancement=>tooltype.
        CREATE OBJECT ri_enho TYPE Lcl_abapgit_object_enho_wdyc
          EXPORTING
            is_item = ms_item.
      WHEN 'FUGRENH'.
        CREATE OBJECT ri_enho TYPE Lcl_abapgit_object_enho_fugr
          EXPORTING
            is_item  = ms_item
            io_files = Lif_abapgit_object~mo_files.
      WHEN 'WDYENH'.
        CREATE OBJECT ri_enho TYPE Lcl_abapgit_object_enho_wdyn
          EXPORTING
            is_item = ms_item.
      WHEN OTHERS.
        Lcx_abapgit_exception=>raise( |Unsupported ENHO type { iv_tool }| ).
    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lv_enh_id   TYPE enhname,
          lt_log      TYPE enh_log_it,
          li_log_obj  TYPE REF TO if_enh_log,
          ls_enhlog   TYPE enhlog,
          lv_lines    TYPE i,
          lt_enhlog   TYPE STANDARD TABLE OF enhlog WITH DEFAULT KEY,
          li_enh_tool TYPE REF TO if_enh_tool.


    lv_enh_id = ms_item-obj_name.
    TRY.
        li_enh_tool = cl_enh_factory=>get_enhancement(
          enhancement_id   = lv_enh_id
          run_dark         = abap_true
          bypassing_buffer = abap_true ).
      CATCH cx_enh_root.
        rv_user = c_user_unknown.
        RETURN.
    ENDTRY.

    lt_log = li_enh_tool->get_log( ).

    LOOP AT lt_log INTO li_log_obj.
      ls_enhlog = li_log_obj->get_enhlog( ).
      APPEND ls_enhlog TO lt_enhlog.
    ENDLOOP.

    lv_lines = lines( lt_enhlog ).
    READ TABLE lt_enhlog INTO ls_enhlog INDEX lv_lines.
    IF sy-subrc = 0.
      rv_user = ls_enhlog-loguser.
    ELSE.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_enh_id     TYPE enhname,
          li_enh_object TYPE REF TO if_enh_object,
          lx_enh_root   TYPE REF TO cx_enh_root,
          lv_corrnum    TYPE trkorr.

    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lv_corrnum = iv_transport.

    lv_enh_id = ms_item-obj_name.
    TRY.
        li_enh_object = cl_enh_factory=>get_enhancement(
          enhancement_id = lv_enh_id
          run_dark       = abap_true
          lock           = abap_true ).
        li_enh_object->delete(
          EXPORTING
            nevertheless_delete = abap_true
            run_dark            = abap_true
          CHANGING
            trkorr              = lv_corrnum ).
        li_enh_object->unlock( ).
      CATCH cx_enh_root INTO lx_enh_root.
        Lcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_tool TYPE enhtooltype,
          li_enho TYPE REF TO Lif_abapgit_object_enho.

    IF Lif_abapgit_object~exists( ) = abap_true.
      Lif_abapgit_object~delete( iv_package   = iv_package
                                 iv_transport = iv_transport ).
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'TOOL'
                  CHANGING cg_data = lv_tool ).

    li_enho = factory( lv_tool ).

    li_enho->deserialize( ii_xml     = io_xml
                          iv_package = iv_package ).

    Lcl_abapgit_sotr_handler=>create_sotr(
      iv_package = iv_package
      io_xml     = io_xml ).

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_enh_id TYPE enhname.


    lv_enh_id = ms_item-obj_name.
    TRY.
        cl_enh_factory=>get_enhancement(
          enhancement_id   = lv_enh_id
          run_dark         = abap_true
          bypassing_buffer = abap_true ).
        rv_bool = abap_true.
      CATCH cx_enh_root.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    DATA: lv_object TYPE seqg3-garg.

    lv_object = |{ ms_item-obj_type }{ ms_item-obj_name }|.
    OVERLAY lv_object WITH '                                          '.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'E_ENHANCE'
                                            iv_argument    = lv_object ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_enh_id   TYPE enhname,
          li_enho     TYPE REF TO Lif_abapgit_object_enho,
          li_enh_tool TYPE REF TO if_enh_tool,
          lx_enh_root TYPE REF TO cx_enh_root.

    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lv_enh_id = ms_item-obj_name.
    TRY.
        li_enh_tool = cl_enh_factory=>get_enhancement(
          enhancement_id   = lv_enh_id
          run_dark         = abap_true
          bypassing_buffer = abap_true ).
      CATCH cx_enh_root INTO lx_enh_root.
        Lcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

    li_enho = factory( li_enh_tool->get_tool( ) ).

    li_enho->serialize( ii_xml      = io_xml
                        ii_enh_tool = li_enh_tool ).

    Lcl_abapgit_sotr_handler=>read_sotr(
      iv_pgmid    = 'R3TR'
      iv_object   = ms_item-obj_type
      iv_obj_name = ms_item-obj_name
      io_i18n_params = mo_i18n_params
      io_xml      = io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENHO implementation

*>>>>>>> ZCL_ABAPGIT_FIELD_RULES <<<<<<<*

*"* macro definitions
*include zcl_abapgit_field_rules=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_field_rules=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_FIELD_RULES implementation.
*"* method's implementations
*include methods.
  METHOD create.
    CREATE OBJECT ro_result TYPE Lcl_abapgit_field_rules.
  ENDMETHOD.
  METHOD fill_value.
    CASE iv_rule.
      WHEN Lif_abapgit_field_rules=>c_fill_rule-date.
        cv_value = sy-datum.
      WHEN Lif_abapgit_field_rules=>c_fill_rule-time.
        cv_value = sy-uzeit.
      WHEN Lif_abapgit_field_rules=>c_fill_rule-timestamp.
        GET TIME STAMP FIELD cv_value.
      WHEN Lif_abapgit_field_rules=>c_fill_rule-user.
        cv_value = sy-uname.
      WHEN Lif_abapgit_field_rules=>c_fill_rule-client.
        cv_value = sy-mandt.
      WHEN Lif_abapgit_field_rules=>c_fill_rule-package.
        cv_value = iv_package.
    ENDCASE.
  ENDMETHOD.
  METHOD Lif_abapgit_field_rules~add.
    DATA ls_item TYPE ty_item.

    ls_item-tabname   = iv_table.
    ls_item-fieldname = iv_field.
    ls_item-fill_rule = iv_fill_rule.
    INSERT ls_item INTO TABLE mt_item.

    ro_self = me.
  ENDMETHOD.
  METHOD Lif_abapgit_field_rules~apply_clear_logic.
    DATA ls_item TYPE ty_item.

    FIELD-SYMBOLS <ls_data> TYPE any.
    FIELD-SYMBOLS <lv_value> TYPE any.

    IF mt_item IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT ct_data ASSIGNING <ls_data>.
      LOOP AT mt_item INTO ls_item WHERE tabname = iv_table.
        ASSIGN COMPONENT ls_item-fieldname OF STRUCTURE <ls_data> TO <lv_value>.
        IF sy-subrc = 0.
          CLEAR <lv_value>.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
  METHOD Lif_abapgit_field_rules~apply_fill_logic.
    DATA ls_item TYPE ty_item.

    FIELD-SYMBOLS <ls_data> TYPE any.
    FIELD-SYMBOLS <lv_value> TYPE any.

    IF mt_item IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT ct_data ASSIGNING <ls_data>.
      LOOP AT mt_item INTO ls_item WHERE tabname = iv_table.
        ASSIGN COMPONENT ls_item-fieldname OF STRUCTURE <ls_data> TO <lv_value>.
        IF sy-subrc = 0.
          fill_value(
            EXPORTING
              iv_rule    = ls_item-fill_rule
              iv_package = iv_package
            CHANGING
              cv_value   = <lv_value> ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_FIELD_RULES implementation

*>>>>>>> ZCL_ABAPGIT_GUI_CSS_PROCESSOR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_css_processor=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_css_processor=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_gui_css_processor=ccau.






class LCL_ABAPGIT_GUI_CSS_PROCESSOR implementation.
*"* method's implementations
*include methods.
  METHOD add_file.
    APPEND iv_url TO mt_files.
  ENDMETHOD.
  METHOD constructor.
    mi_asset_manager = ii_asset_manager.
  ENDMETHOD.
  METHOD get_css_vars_in_string.
    CONSTANTS: lc_root_pattern     TYPE string VALUE `:root\s*\{([^\}]*)\}`,
               lc_variable_pattern TYPE string VALUE `\-\-([\w\d-]+)\s*:\s*([^\n\r;]*);`.
    DATA: lv_root     TYPE string,
          lo_matcher  TYPE REF TO cl_abap_matcher,
          lo_regex    TYPE REF TO cl_abap_regex,
          ls_variable LIKE LINE OF rt_variables.

    " Only the :root element may define variables for now

    FIND FIRST OCCURRENCE OF REGEX lc_root_pattern IN iv_string SUBMATCHES lv_root.
    IF sy-subrc = 0 AND lv_root IS NOT INITIAL.
      CREATE OBJECT lo_regex
        EXPORTING
          pattern = lc_variable_pattern.
      lo_matcher = lo_regex->create_matcher( text = lv_root ).
      WHILE lo_matcher->find_next( ) = abap_true.
        ls_variable-name = lo_matcher->get_submatch( 1 ).
        ls_variable-value = lo_matcher->get_submatch( 2 ).
        INSERT ls_variable INTO TABLE rt_variables.
        IF sy-subrc <> 0.
          MODIFY TABLE rt_variables FROM ls_variable.
        ENDIF.
      ENDWHILE.
    ENDIF.
  ENDMETHOD.
  METHOD process.
    DATA:
          lt_contents         TYPE STANDARD TABLE OF string,
          lv_content          TYPE string,
          lt_css_variables    TYPE ty_css_vars,
          lt_css_vars_in_file TYPE ty_css_vars.
    FIELD-SYMBOLS: <lv_url>          TYPE string,
                   <ls_css_variable> LIKE LINE OF lt_css_vars_in_file,
                   <lv_content>      LIKE LINE OF lt_contents.

    " 1. Determine all variables and their values. Later definitions overwrite previous ones.
    LOOP AT mt_files ASSIGNING <lv_url>.
      lv_content = mi_asset_manager->get_text_asset(
        iv_url = <lv_url>
        iv_assert_subtype = 'css' ).

      lt_css_vars_in_file = get_css_vars_in_string( lv_content ).

      LOOP AT lt_css_vars_in_file ASSIGNING <ls_css_variable>.
        INSERT <ls_css_variable> INTO TABLE lt_css_variables.
        IF sy-subrc <> 0.
          MODIFY TABLE lt_css_variables FROM <ls_css_variable>.
        ENDIF.
      ENDLOOP.

      APPEND lv_content TO lt_contents.
    ENDLOOP.

    " 2. Replace all variable usages in variables
    LOOP AT lt_css_variables ASSIGNING <ls_css_variable> WHERE value CS 'var(--'.
      resolve_var_recursively( EXPORTING iv_variable_name = <ls_css_variable>-name
                               CHANGING  ct_variables     = lt_css_variables ).
    ENDLOOP.

    " 3. Replace all other variable usages by inlining the values.
    LOOP AT lt_contents ASSIGNING <lv_content>.
      LOOP AT lt_css_variables ASSIGNING <ls_css_variable>.
        REPLACE ALL OCCURRENCES OF |var(--{ <ls_css_variable>-name })|
                IN <lv_content>
                WITH <ls_css_variable>-value.
      ENDLOOP.
    ENDLOOP.

    rv_result = concat_lines_of( table = lt_contents
                                 sep = cl_abap_char_utilities=>newline ).
  ENDMETHOD.
  METHOD resolve_var_recursively.
    CONSTANTS: lc_variable_usage_pattern TYPE string VALUE `var\(\-\-([^\)]*)\)`.
    DATA: lv_variable_name  TYPE string.
    FIELD-SYMBOLS: <ls_variable>       LIKE LINE OF ct_variables,
                   <ls_other_variable> LIKE LINE OF ct_variables.

    READ TABLE ct_variables WITH TABLE KEY name = iv_variable_name ASSIGNING <ls_variable>.
    IF sy-subrc = 0.
      DO.
        FIND FIRST OCCURRENCE OF REGEX lc_variable_usage_pattern
             IN <ls_variable>-value
             SUBMATCHES lv_variable_name.
        IF sy-subrc = 0.
          resolve_var_recursively( EXPORTING iv_variable_name = lv_variable_name
                                   CHANGING  ct_variables     = ct_variables ).
          READ TABLE ct_variables WITH TABLE KEY name = lv_variable_name ASSIGNING <ls_other_variable>.
          REPLACE FIRST OCCURRENCE OF |var(--{ lv_variable_name })|
                  IN <ls_variable>-value
                  WITH <ls_other_variable>-value.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
    ELSE.
      Lcx_abapgit_exception=>raise( |CSS variable { iv_variable_name } not resolveable| ).
    ENDIF.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_CSS_PROCESSOR implementation

*>>>>>>> ZCL_ABAPGIT_AUTH <<<<<<<*

*"* macro definitions
*include zcl_abapgit_auth==============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_auth==============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_auth==============ccau.




class LCL_ABAPGIT_AUTH implementation.
*"* method's implementations
*include methods.
  METHOD is_allowed.

    DATA: li_auth TYPE REF TO Lif_abapgit_auth.

    TRY.
        CREATE OBJECT li_auth TYPE ('LCL_ABAPGIT_AUTH_EXIT').
        rv_allowed = li_auth->is_allowed( iv_authorization = iv_authorization
                                          iv_param         = iv_param ).
      CATCH cx_sy_create_object_error.
        rv_allowed = abap_true.
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_AUTH implementation

*>>>>>>> ZCL_ABAPGIT_BACKGROUND_PUSH_AU <<<<<<<*

*"* macro definitions
*include zcl_abapgit_background_push_auccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_background_push_auccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_BACKGROUND_PUSH_AU implementation.
*"* method's implementations
*include methods.
  METHOD build_comment.

    DATA: lt_objects TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
          lv_str     TYPE string.

    FIELD-SYMBOLS: <ls_local> LIKE LINE OF is_files-local.


    LOOP AT is_files-local ASSIGNING <ls_local>.
      lv_str = |{ <ls_local>-item-obj_type } { <ls_local>-item-obj_name }|.
      APPEND lv_str TO lt_objects.
    ENDLOOP.

    SORT lt_objects AS TEXT.
    DELETE ADJACENT DUPLICATES FROM lt_objects.

    IF lines( lt_objects ) = 1.
      rv_comment = |BG: { lv_str }|.
    ELSE.
      rv_comment = 'BG: Multiple objects'.
      LOOP AT lt_objects INTO lv_str.
        CONCATENATE rv_comment cl_abap_char_utilities=>newline lv_str INTO rv_comment.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD determine_user_details.

    DATA: lo_user_record TYPE REF TO Lcl_abapgit_user_record.


    lo_user_record = Lcl_abapgit_user_record=>get_instance( iv_changed_by ).
    rs_user-name = lo_user_record->get_name( ).
    rs_user-email = lo_user_record->get_email( ).

*   If no email, fall back to localhost/default email
    IF rs_user-email IS INITIAL.
      rs_user-email = |{ iv_changed_by }@localhost|.
    ENDIF.

*   If no full name maintained, just use changed by user name
    IF rs_user-name IS INITIAL.
      rs_user-name  = iv_changed_by.
    ENDIF.

  ENDMETHOD.
  METHOD push_auto.

    TYPES: BEGIN OF ty_changed,
             filename   TYPE string,
             path       TYPE string,
             changed_by TYPE syuname,
           END OF ty_changed.

    DATA: ls_comment    TYPE Lif_abapgit_git_definitions=>ty_comment,
          ls_files      TYPE Lif_abapgit_definitions=>ty_stage_files,
          lt_changed    TYPE STANDARD TABLE OF ty_changed WITH DEFAULT KEY,
          lt_users      TYPE STANDARD TABLE OF syuname WITH DEFAULT KEY,
          ls_user_files LIKE ls_files,
          lv_changed_by LIKE LINE OF lt_users,
          lo_stage      TYPE REF TO Lcl_abapgit_stage.

    FIELD-SYMBOLS: <ls_changed> LIKE LINE OF lt_changed,
                   <ls_remote>  LIKE LINE OF ls_files-remote,
                   <ls_local>   LIKE LINE OF ls_files-local.


    ls_files = Lcl_abapgit_factory=>get_stage_logic( )->get( io_repo ).

    LOOP AT ls_files-local ASSIGNING <ls_local>.
      lv_changed_by = Lcl_abapgit_objects=>changed_by(
        is_item     = <ls_local>-item
        iv_filename = <ls_local>-file-filename ).
      APPEND lv_changed_by TO lt_users.
      APPEND INITIAL LINE TO lt_changed ASSIGNING <ls_changed>.
      <ls_changed>-changed_by = lv_changed_by.
      <ls_changed>-filename   = <ls_local>-file-filename.
      <ls_changed>-path       = <ls_local>-file-path.
    ENDLOOP.

    SORT lt_users ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_users.

    LOOP AT lt_users INTO lv_changed_by.
      CLEAR: ls_comment.

*     Fill user details
      ls_comment-committer = determine_user_details( lv_changed_by ).

      CREATE OBJECT lo_stage.

      CLEAR ls_user_files.

      LOOP AT ls_files-local ASSIGNING <ls_local>.
        READ TABLE lt_changed WITH KEY
          path = <ls_local>-file-path
          filename = <ls_local>-file-filename
          changed_by = lv_changed_by
          TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          mi_log->add_info( |stage: {
            ls_comment-committer-name } {
            <ls_local>-file-path } {
            <ls_local>-file-filename }| ).

          lo_stage->add( iv_path     = <ls_local>-file-path
                         iv_filename = <ls_local>-file-filename
                         iv_data     = <ls_local>-file-data ).

          APPEND <ls_local> TO ls_user_files-local.

          LOOP AT ls_files-remote ASSIGNING <ls_remote>
              USING KEY file
              WHERE filename = <ls_local>-file-filename
              AND path <> <ls_local>-file-path
              AND filename <> 'package.devc.xml'.
            mi_log->add_info( |rm: { <ls_remote>-path } { <ls_remote>-filename }| ).

* rm old file when object has moved
            lo_stage->rm(
              iv_path     = <ls_remote>-path
              iv_filename = <ls_remote>-filename ).
            EXIT. " assumption: only one file
          ENDLOOP.
        ENDIF.
      ENDLOOP.

      ls_comment-comment = build_comment( ls_user_files ).

      io_repo->push( is_comment = ls_comment
                     io_stage   = lo_stage ).
    ENDLOOP.

    IF lines( ls_files-remote ) > 0.
      push_deletions( io_repo  = io_repo
                      is_files = ls_files ).
    ENDIF.

  ENDMETHOD.
  METHOD push_deletions.

    DATA: lo_stage   TYPE REF TO Lcl_abapgit_stage,
          ls_comment TYPE Lif_abapgit_git_definitions=>ty_comment.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF is_files-remote.

    ASSERT lines( is_files-remote ) > 0.

    CREATE OBJECT lo_stage.

    ls_comment-comment = 'BG: Deletion'.

    LOOP AT is_files-remote ASSIGNING <ls_remote>.

      mi_log->add_info( |removed: { <ls_remote>-path } { <ls_remote>-filename }| ).

      lo_stage->rm( iv_path     = <ls_remote>-path
                    iv_filename = <ls_remote>-filename ).

      CONCATENATE ls_comment-comment cl_abap_char_utilities=>newline <ls_remote>-filename
        INTO ls_comment-comment.

    ENDLOOP.

    ls_comment-committer-name  = 'Deletion'.
    ls_comment-committer-email = 'deletion@localhost'.

    io_repo->push( is_comment = ls_comment
                   io_stage   = lo_stage ).

  ENDMETHOD.
  METHOD Lif_abapgit_background~get_description.

    rv_description = 'Automatic push, auto author'.

  ENDMETHOD.
  METHOD Lif_abapgit_background~get_settings.

    RETURN.

  ENDMETHOD.
  METHOD Lif_abapgit_background~run.

    DATA: ls_files TYPE Lif_abapgit_definitions=>ty_stage_files.

    mi_log = ii_log.
    ls_files = Lcl_abapgit_factory=>get_stage_logic( )->get( io_repo ).

    IF lines( ls_files-local ) = 0 AND lines( ls_files-remote ) = 0.
      ii_log->add_info( 'Nothing to stage' ).
      RETURN.
    ENDIF.

    push_auto( io_repo ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_BACKGROUND_PUSH_AU implementation

*>>>>>>> ZCL_ABAPGIT_GUI_COMPONENT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_component=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_component=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_COMPONENT implementation.
*"* method's implementations
*include methods.
  METHOD gui_services.
    IF mi_gui_services IS NOT BOUND.
      mi_gui_services = Lcl_abapgit_ui_factory=>get_gui_services( ).
    ENDIF.
    ri_gui_services = mi_gui_services.
  ENDMETHOD.
  METHOD register_deferred_script.
    gui_services( )->get_html_parts( )->add_part(
      iv_collection = c_html_parts-scripts
      ii_part       = ii_part ).
  ENDMETHOD.
  METHOD register_event_handler.

    DATA li_event_handler TYPE REF TO Lif_abapgit_gui_event_handler.

    IF ii_event_handler IS BOUND.
      li_event_handler = ii_event_handler.
    ELSE.
      TRY.
          li_event_handler ?= me.
        CATCH cx_root.
          RETURN.
      ENDTRY.
    ENDIF.

    gui_services( )->register_event_handler( li_event_handler ).

  ENDMETHOD.
  METHOD register_handlers.
    register_event_handler( ).
    register_hotkeys( ).
  ENDMETHOD.
  METHOD register_hotkeys.

    DATA li_hotkey_provider TYPE REF TO Lif_abapgit_gui_hotkeys.

    IF ii_hotkey_provider IS BOUND.
      li_hotkey_provider = ii_hotkey_provider.
    ELSE.
      TRY.
          li_hotkey_provider ?= me.
        CATCH cx_root.
          RETURN.
      ENDTRY.
    ENDIF.

    gui_services( )->get_hotkeys_ctl( )->register_hotkeys( li_hotkey_provider->get_hotkey_actions( ) ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_COMPONENT implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ENHS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_enhs=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_enhs=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ENHS implementation.
*"* method's implementations
*include methods.
  METHOD factory.

    CASE iv_tool.
      WHEN cl_enh_tool_badi_def=>tooltype.
        CREATE OBJECT ri_enho TYPE Lcl_abapgit_object_enhs_badi_d.
      WHEN cl_enh_tool_hook_def=>tool_type.
        CREATE OBJECT ri_enho TYPE Lcl_abapgit_object_enhs_hook_d.
      WHEN OTHERS.
        Lcx_abapgit_exception=>raise( |ENHS: Unsupported tool { iv_tool }| ).
    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lv_spot_name TYPE enhspotname,
          li_spot_ref  TYPE REF TO if_enh_spot_tool.

    lv_spot_name = ms_item-obj_name.

    TRY.
        li_spot_ref = cl_enh_factory=>get_enhancement_spot( spot_name = lv_spot_name
                                                            run_dark  = abap_true ).
        li_spot_ref->get_attributes( IMPORTING changedby = rv_user ).

      CATCH cx_enh_root.
        rv_user = c_user_unknown.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_spot_name  TYPE enhspotname,
          lx_enh_root   TYPE REF TO cx_enh_root,
          li_enh_object TYPE REF TO if_enh_object.

    lv_spot_name  = ms_item-obj_name.

    TRY.
        li_enh_object ?= cl_enh_factory=>get_enhancement_spot( spot_name = lv_spot_name
                                                               run_dark  = abap_true
                                                               lock      = abap_true ).

        li_enh_object->delete( nevertheless_delete = abap_true
                               run_dark            = abap_true ).

        li_enh_object->unlock( ).

      CATCH cx_enh_root INTO lx_enh_root.
        Lcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_parent    TYPE enhspotcompositename,
          lv_spot_name TYPE enhspotname,
          lv_tool      TYPE enhspottooltype,
          lv_package   LIKE iv_package,
          lx_enh_root  TYPE REF TO cx_enh_root,
          li_spot_ref  TYPE REF TO if_enh_spot_tool,
          li_enhs      TYPE REF TO Lif_abapgit_object_enhs.

    IF Lif_abapgit_object~exists( ) = abap_true.
      Lif_abapgit_object~delete( iv_package   = iv_package
                                 iv_transport = iv_transport ).
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'TOOL'
                  CHANGING  cg_data = lv_tool ).

    lv_spot_name = ms_item-obj_name.
    lv_package   = iv_package.

    TRY.
        cl_enh_factory=>create_enhancement_spot(
          EXPORTING
            spot_name      = lv_spot_name
            tooltype       = lv_tool
            dark           = abap_false
            compositename  = lv_parent
          IMPORTING
            spot           = li_spot_ref
          CHANGING
            devclass       = lv_package ).

      CATCH cx_enh_root INTO lx_enh_root.
        Lcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

    li_enhs = factory( lv_tool ).

    li_enhs->deserialize( ii_xml           = io_xml
                          iv_package       = iv_package
                          ii_enh_spot_tool = li_spot_ref ).

    Lcl_abapgit_sotr_handler=>create_sotr(
      iv_package = iv_package
      io_xml     = io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_spot_name TYPE enhspotname,
          li_spot_ref  TYPE REF TO if_enh_spot_tool.

    lv_spot_name = ms_item-obj_name.

    TRY.
        li_spot_ref = cl_enh_factory=>get_enhancement_spot( spot_name = lv_spot_name
                                                            run_dark  = abap_true ).

        rv_bool = abap_true.

      CATCH cx_enh_root.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_spot_name TYPE enhspotname,
          li_spot_ref  TYPE REF TO if_enh_spot_tool,
          li_enhs      TYPE REF TO Lif_abapgit_object_enhs,
          lx_enh_root  TYPE REF TO cx_enh_root.

    lv_spot_name = ms_item-obj_name.

    TRY.
        li_spot_ref = cl_enh_factory=>get_enhancement_spot( spot_name = lv_spot_name
                                                            run_dark  = abap_true ).

      CATCH cx_enh_root INTO lx_enh_root.
        Lcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

    li_enhs = factory( li_spot_ref->get_tool( ) ).

    li_enhs->serialize( ii_xml           = io_xml
                        ii_enh_spot_tool = li_spot_ref ).

    Lcl_abapgit_sotr_handler=>read_sotr(
      iv_pgmid    = 'R3TR'
      iv_object   = ms_item-obj_type
      iv_obj_name = ms_item-obj_name
      io_i18n_params = mo_i18n_params
      io_xml      = io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENHS implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ENSC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ensc=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ensc=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ENSC implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lv_spot_name TYPE enhspotcompositename,
          li_spot_ref  TYPE REF TO if_enh_spot_composite,
          lo_spot_ref  TYPE REF TO cl_enh_spot_composite.

    lv_spot_name = ms_item-obj_name.

    TRY.
        li_spot_ref = cl_enh_factory=>get_enhancement_spot_comp(
          lock     = ''
          run_dark = abap_true
          name     = lv_spot_name ).

        lo_spot_ref ?= li_spot_ref.

        lo_spot_ref->if_enh_spot_composite~get_change_attributes( IMPORTING changedby = rv_user ).
      CATCH cx_root.
        rv_user = c_user_unknown.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.
    DATA: lv_spot_name TYPE enhspotcompositename,
          lv_message   TYPE string,
          lx_root      TYPE REF TO cx_root,
          li_spot_ref  TYPE REF TO if_enh_spot_composite.

    lv_spot_name = ms_item-obj_name.

    TRY.
        li_spot_ref = cl_enh_factory=>get_enhancement_spot_comp(
          lock     = abap_true
          run_dark = abap_true
          name     = lv_spot_name ).

        IF li_spot_ref IS BOUND.
          li_spot_ref->if_enh_object~delete(
            nevertheless_delete = abap_true
            run_dark            = abap_true ).
        ENDIF.
        li_spot_ref->if_enh_object~unlock( ).
      CATCH cx_enh_root INTO lx_root.
        lv_message = `Error occured while deleting ENSC: `
          && lx_root->get_text( ).
        Lcx_abapgit_exception=>raise( lv_message ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_spot_name  TYPE enhspotcompositename,
          lv_message    TYPE string,
          lv_enh_shtext TYPE string,
          lv_enh_spot   TYPE enhspotname,
          lt_enh_spots  TYPE enhspotname_it,
          lt_comp_spots TYPE enhspotname_it,
          lx_root       TYPE REF TO cx_root,
          lv_package    LIKE iv_package,
          li_spot_ref   TYPE REF TO if_enh_spot_composite,
          lo_spot_ref   TYPE REF TO cl_enh_spot_composite.


    lv_spot_name = ms_item-obj_name.

    io_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING  cg_data = lv_enh_shtext ).
    io_xml->read( EXPORTING iv_name = 'ENH_SPOTS'     "Enhancement spots
                  CHANGING  cg_data = lt_enh_spots ).
    io_xml->read( EXPORTING iv_name = 'COMP_ENH_SPOTS' "Composite enhancement spots
                  CHANGING  cg_data = lt_comp_spots ).

    IF Lif_abapgit_object~exists( ) = abap_true.
      Lif_abapgit_object~delete( iv_package   = iv_package
                                 iv_transport = iv_transport ).
    ENDIF.

    lv_package = iv_package.

    TRY.
        cl_enh_factory=>create_enhancement_spot_comp(
          EXPORTING
            name      = lv_spot_name
            run_dark  = abap_true
          IMPORTING
            composite = li_spot_ref
          CHANGING
            devclass  = lv_package ).

        lo_spot_ref ?= li_spot_ref.

        lo_spot_ref->if_enh_object_docu~set_shorttext( lv_enh_shtext ).
        "Add subsequent enhancement spots
        LOOP AT lt_enh_spots INTO lv_enh_spot.
          lo_spot_ref->if_enh_spot_composite~add_enh_spot_child( lv_enh_spot ).
        ENDLOOP.
        "Add subsequent composite enhancement spots
        LOOP AT lt_comp_spots INTO lv_enh_spot.
          lo_spot_ref->if_enh_spot_composite~add_composite_child( lv_enh_spot ).
        ENDLOOP.

        lo_spot_ref->if_enh_object~save( ).
        lo_spot_ref->if_enh_object~activate( ).
        lo_spot_ref->if_enh_object~unlock( ).

        Lcl_abapgit_sotr_handler=>create_sotr(
          iv_package = iv_package
          io_xml     = io_xml ).

      CATCH cx_enh_root INTO lx_root.
        lv_message = `Error occured while deserializing ENSC: `
          && lx_root->get_text( ).
        Lcx_abapgit_exception=>raise( lv_message ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_spot_name TYPE enhspotcompositename.


    lv_spot_name = ms_item-obj_name.

    TRY.
        cl_enh_factory=>get_enhancement_spot_comp(
          lock     = ''
          run_dark = abap_true
          name     = lv_spot_name ).
        rv_bool = abap_true.
      CATCH cx_enh_root.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_spot_name  TYPE enhspotcompositename,
          lv_message    TYPE string,
          lv_enh_shtext TYPE string,
          lt_enh_spots  TYPE enhspotname_it,
          lt_comp_spots TYPE enhspotname_it,
          lx_root       TYPE REF TO cx_root,
          li_spot_ref   TYPE REF TO if_enh_spot_composite,
          lo_spot_ref   TYPE REF TO cl_enh_spot_composite.


    lv_spot_name = ms_item-obj_name.

    TRY.
        li_spot_ref = cl_enh_factory=>get_enhancement_spot_comp(
          lock     = ''
          run_dark = abap_true
          name     = lv_spot_name ).

        lo_spot_ref ?= li_spot_ref.

        lv_enh_shtext = li_spot_ref->if_enh_object_docu~get_shorttext( ).
        "find subsequent enhancement spots
        lt_enh_spots = lo_spot_ref->if_enh_spot_composite~get_enh_spot_childs( ).
        "find subsequent composite enhancement spots
        lt_comp_spots = lo_spot_ref->if_enh_spot_composite~get_composite_childs( ).

        io_xml->add( ig_data = lv_enh_shtext
                     iv_name = 'SHORTTEXT' ).
        io_xml->add( ig_data = lt_enh_spots
                     iv_name = 'ENH_SPOTS' ).         "Enhancement spots
        io_xml->add( ig_data = lt_comp_spots
                     iv_name = 'COMP_ENH_SPOTS' ).    "Composite enhancement spots

        Lcl_abapgit_sotr_handler=>read_sotr(
          iv_pgmid    = 'R3TR'
          iv_object   = ms_item-obj_type
          iv_obj_name = ms_item-obj_name
          io_i18n_params = mo_i18n_params
          io_xml      = io_xml ).

      CATCH cx_enh_root INTO lx_root.
        lv_message = `Error occured while serializing ENSC: `
          && lx_root->get_text( ).
        Lcx_abapgit_exception=>raise( lv_message ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENSC implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_FORM <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_form=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_form=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_FORM implementation.
*"* method's implementations
*include methods.
  METHOD build_extra_from_header.

    DATA lv_tdspras TYPE laiso.

    lv_tdspras = Lcl_abapgit_convert=>conversion_exit_isola_output( is_header-tdspras ).

    rv_result = c_objectname_tdlines && '_' && lv_tdspras.

  ENDMETHOD.
  METHOD build_extra_from_header_old.
    rv_result = c_objectname_tdlines && '_' && is_header-tdspras.
  ENDMETHOD.
  METHOD compress_lines.

    DATA lv_string TYPE string.
    DATA li_xml TYPE REF TO Lif_abapgit_xml_output.

    CREATE OBJECT li_xml TYPE Lcl_abapgit_xml_output.
    li_xml->add( iv_name = c_objectname_tdlines
                 ig_data = it_lines ).
    lv_string = li_xml->render( ).
    IF lv_string IS NOT INITIAL.
      Lif_abapgit_object~mo_files->add_string( iv_extra  =
                    build_extra_from_header( is_form_data-form_header )
                            iv_ext    = c_extension_xml
                            iv_string = lv_string ).
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_form_name = ms_item-obj_name.

  ENDMETHOD.
  METHOD extract_tdlines.

    DATA lv_string TYPE string.
    DATA li_xml TYPE REF TO Lif_abapgit_xml_input.

    TRY.
        lv_string = Lif_abapgit_object~mo_files->read_string( iv_extra =
                                   build_extra_from_header( is_form_data-form_header )
                                           iv_ext   = c_extension_xml ).
      CATCH Lcx_abapgit_exception.

        lv_string = Lif_abapgit_object~mo_files->read_string( iv_extra =
                               build_extra_from_header_old( is_form_data-form_header )
                                           iv_ext   = c_extension_xml ).

    ENDTRY.

    CREATE OBJECT li_xml TYPE Lcl_abapgit_xml_input EXPORTING iv_xml = lv_string.
    li_xml->read( EXPORTING iv_name = c_objectname_tdlines
                  CHANGING  cg_data = rt_lines ).

  ENDMETHOD.
  METHOD find_form.

    DATA: lv_text_name TYPE thead-tdname.

    lv_text_name = iv_object_name.

    CALL FUNCTION 'SELECT_TEXT'
      EXPORTING
        database_only = abap_true
        id            = 'TXT'
        language      = '*'
        name          = lv_text_name
        object        = c_objectname_form
      TABLES
        selections    = rt_text_header
      EXCEPTIONS
        OTHERS        = 1 ##FM_SUBRC_OK.  "#EC CI_SUBRC

  ENDMETHOD.
  METHOD get_last_changes.

    DATA: lv_form_name         TYPE thead-tdform.

    CLEAR rs_last_changed.

    lv_form_name = iv_form_name.

    CALL FUNCTION 'READ_FORM'
      EXPORTING
        form             = lv_form_name
        read_only_header = abap_true
      IMPORTING
        form_header      = rs_last_changed.

  ENDMETHOD.
  METHOD order_check_and_insert.

    DATA: lv_order TYPE e071k-trkorr.

    CALL FUNCTION 'SAPSCRIPT_ORDER_CHECK'
      EXPORTING
        objecttype           = ms_item-obj_type
        form                 = mv_form_name
      EXCEPTIONS
        invalid_input        = 1
        object_locked        = 2
        object_not_available = 3
        OTHERS               = 4.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'SAPSCRIPT_ORDER_INSERT'
      EXPORTING
        objecttype     = ms_item-obj_type
        form           = mv_form_name
        masterlang     = mv_language
      CHANGING
        order          = lv_order
      EXCEPTIONS
        invalid_input  = 1
        order_canceled = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: ls_last_changed TYPE ty_s_form_header.

    ls_last_changed = get_last_changes( ms_item-obj_name ).

    IF ls_last_changed-tdluser IS NOT INITIAL.
      rv_user = ls_last_changed-tdluser.
    ELSE.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    CALL FUNCTION 'DELETE_FORM'
      EXPORTING
        form     = mv_form_name
        language = '*'.

    order_check_and_insert( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lt_form_data            TYPE ty_t_form_data.
    DATA: lt_lines TYPE ty_t_lines.
    FIELD-SYMBOLS: <ls_form_data> TYPE LINE OF ty_t_form_data.

    io_xml->read( EXPORTING iv_name = c_objectname_form
                  CHANGING  cg_data = lt_form_data ).

    LOOP AT lt_form_data ASSIGNING <ls_form_data>.

      lt_lines = extract_tdlines( <ls_form_data> ).

      _save_form( EXPORTING it_lines     = lt_lines
                  CHANGING  cs_form_data = <ls_form_data> ).

    ENDLOOP.

    CALL FUNCTION 'SAPSCRIPT_DELETE_LOAD'
      EXPORTING
        delete = abap_true
        form   = '*'
        write  = space.

    tadir_insert( iv_package ).

    order_check_and_insert( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    CALL FUNCTION 'READ_FORM'
      EXPORTING
        form             = mv_form_name
        read_only_header = abap_true
      IMPORTING
        found            = rv_bool.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    DATA: lv_object TYPE seqg3-garg.

    " example lock entry
    "'001FORM      ZTEST_SAPSCRIPT                                                       TXT'
    lv_object = |{ sy-mandt }{ ms_item-obj_type }      { ms_item-obj_name }|.
    OVERLAY lv_object WITH '                                                                                   '.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESSFORM'
                                            iv_argument    = lv_object ).


  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPMSSCF'.
    <ls_bdcdata>-dynpro   = '1102'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=SHOW'.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'RSSCF-TDFORM'.
    <ls_bdcdata>-fval = ms_item-obj_name.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SE71'
      it_bdcdata = lt_bdcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lt_form_data              TYPE ty_t_form_data.
    DATA: ls_form_data              TYPE ty_s_form_data.
    DATA: lt_text_header            TYPE ty_t_text_header.
    DATA: lt_lines                  TYPE ty_t_lines.
    DATA: lv_form_found             TYPE abap_bool.
    FIELD-SYMBOLS: <ls_text_header> LIKE LINE OF lt_text_header.

    lt_text_header = find_form( ms_item-obj_name ).

    LOOP AT lt_text_header ASSIGNING <ls_text_header>.
      CLEAR lt_lines.
      CLEAR ls_form_data.

      _read_form( EXPORTING is_text_header = <ls_text_header>
                  IMPORTING ev_form_found = lv_form_found
                            es_form_data  = ls_form_data
                            et_lines      = lt_lines ).

      IF lv_form_found = abap_true.

        _clear_changed_fields( CHANGING cs_form_data = ls_form_data ).

        compress_lines( is_form_data = ls_form_data
                        it_lines     = lt_lines ).

        INSERT ls_form_data INTO TABLE lt_form_data.

      ENDIF.

    ENDLOOP.

    IF lt_form_data IS NOT INITIAL.

      io_xml->add( iv_name = c_objectname_form
                   ig_data = lt_form_data ).

    ENDIF.

  ENDMETHOD.
  METHOD _clear_changed_fields.

    CLEAR: cs_form_data-form_header-tdfuser,
           cs_form_data-form_header-tdfdate,
           cs_form_data-form_header-tdftime,
           cs_form_data-form_header-tdfreles,
           cs_form_data-form_header-tdluser,
           cs_form_data-form_header-tdldate,
           cs_form_data-form_header-tdltime,
           cs_form_data-form_header-tdlreles.
    CLEAR: cs_form_data-text_header-tdfuser,
           cs_form_data-text_header-tdfdate,
           cs_form_data-text_header-tdftime,
           cs_form_data-text_header-tdfreles,
           cs_form_data-text_header-tdluser,
           cs_form_data-text_header-tdldate,
           cs_form_data-text_header-tdltime,
           cs_form_data-text_header-tdlreles.

  ENDMETHOD.
  METHOD _read_form.

    CLEAR es_form_data.

    CALL FUNCTION 'READ_FORM'
      EXPORTING
        form         = is_text_header-tdform
        language     = is_text_header-tdspras
        status       = ' '
      IMPORTING
        form_header  = es_form_data-form_header
        found        = ev_form_found
        header       = es_form_data-text_header
        olanguage    = es_form_data-orig_language
      TABLES
        form_lines   = et_lines
        pages        = es_form_data-pages
        page_windows = es_form_data-page_windows
        paragraphs   = es_form_data-paragraphs
        strings      = es_form_data-strings
        tabs         = es_form_data-tabs
        windows      = es_form_data-windows.

    _sort_tdlines_by_windows( CHANGING ct_form_windows  = es_form_data-windows
                                       ct_lines         = et_lines ).

    es_form_data-form_header-tdversion = '00001'.
    es_form_data-text_header-tdversion = '00001'.

  ENDMETHOD.
  METHOD _save_form.

    CALL FUNCTION 'SAVE_FORM'
      EXPORTING
        form_header  = cs_form_data-form_header
      TABLES
        form_lines   = it_lines
        pages        = cs_form_data-pages
        page_windows = cs_form_data-page_windows
        paragraphs   = cs_form_data-paragraphs
        strings      = cs_form_data-strings
        tabs         = cs_form_data-tabs
        windows      = cs_form_data-windows.

    CALL FUNCTION 'SAPSCRIPT_CHANGE_OLANGUAGE'
      EXPORTING
        forced    = abap_true
        name      = cs_form_data-text_header-tdname
        object    = cs_form_data-text_header-tdobject
        olanguage = cs_form_data-orig_language
      EXCEPTIONS
        OTHERS    = 1 ##FM_SUBRC_OK.                                                   "#EC CI_SUBRC

  ENDMETHOD.
  METHOD _sort_tdlines_by_windows.
    DATA lt_lines        TYPE ty_t_lines.
    DATA ls_lines        LIKE LINE OF lt_lines.
    DATA ls_form_windows LIKE LINE OF ct_form_windows.
    DATA lv_elt_windows  TYPE tdformat VALUE '/W'.
    DATA lv_firstloop    TYPE abap_bool.

    lt_lines = ct_lines.
    CLEAR ct_lines.

    SORT ct_form_windows BY tdwindow.

    LOOP AT ct_form_windows INTO ls_form_windows.
      lv_firstloop = abap_true.
      READ TABLE lt_lines INTO ls_lines WITH KEY tdformat = lv_elt_windows
                                                 tdline   = ls_form_windows-tdwindow.
      IF sy-subrc <> 0.
        CONTINUE. " current loop
      ENDIF.
      LOOP AT lt_lines INTO ls_lines FROM sy-tabix.
        IF lv_firstloop = abap_false AND
           ls_lines-tdformat = lv_elt_windows.
          EXIT.
        ENDIF.
        APPEND ls_lines TO ct_lines.
        lv_firstloop = abap_false.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_FORM implementation

*>>>>>>> ZCL_ABAPGIT_DATA_CONFIG <<<<<<<*

*"* macro definitions
*include zcl_abapgit_data_config=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_data_config=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_data_config=======ccau.



class LCL_ABAPGIT_DATA_CONFIG implementation.
*"* method's implementations
*include methods.
  METHOD dump.

    DATA lo_ajson TYPE REF TO Lcl_abapgit_ajson.
    DATA lx_ajson TYPE REF TO Lcx_abapgit_ajson_error.

    TRY.
        lo_ajson = Lcl_abapgit_ajson=>create_empty( ).
        lo_ajson->Lif_abapgit_ajson~set(
          iv_path = '/'
          iv_val  = is_config ).
        rv_json = Lcl_abapgit_convert=>string_to_xstring_utf8( lo_ajson->stringify( 2 ) ).
      CATCH Lcx_abapgit_ajson_error INTO lx_ajson.
        Lcx_abapgit_exception=>raise( lx_ajson->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_data_config~add_config.

    DATA lv_where TYPE string.

    FIELD-SYMBOLS <ls_config> LIKE LINE OF mt_config.

    ASSERT is_config-type IS NOT INITIAL.
    ASSERT is_config-name IS NOT INITIAL.
    ASSERT is_config-name = to_upper( is_config-name ).

    INSERT is_config INTO TABLE mt_config.
    IF sy-subrc <> 0.
* append to existing
      READ TABLE mt_config ASSIGNING <ls_config> WITH KEY type = is_config-type name = is_config-name.
      ASSERT sy-subrc = 0.
      LOOP AT is_config-where INTO lv_where.
        READ TABLE <ls_config>-where TRANSPORTING NO FIELDS WITH KEY table_line = lv_where.
        IF sy-subrc <> 0.
          INSERT lv_where INTO TABLE <ls_config>-where.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_data_config~from_json.

    DATA ls_file LIKE LINE OF it_files.
    DATA ls_config TYPE Lif_abapgit_data_config=>ty_config.
    DATA lo_ajson TYPE REF TO Lcl_abapgit_ajson.
    DATA lx_ajson TYPE REF TO Lcx_abapgit_ajson_error.

    CLEAR mt_config.
    LOOP AT it_files INTO ls_file
        USING KEY file_path
        WHERE path = Lif_abapgit_data_config=>c_default_path
        AND filename CP |*.{ Lif_abapgit_data_config=>c_config }.{ Lif_abapgit_data_config=>c_default_format }|.
      TRY.
          lo_ajson = Lcl_abapgit_ajson=>parse( Lcl_abapgit_convert=>xstring_to_string_utf8( ls_file-data ) ).
          lo_ajson->Lif_abapgit_ajson~to_abap( IMPORTING ev_container = ls_config ).
        CATCH Lcx_abapgit_ajson_error INTO lx_ajson.
          Lcx_abapgit_exception=>raise( lx_ajson->get_text( ) ).
      ENDTRY.

      Lif_abapgit_data_config~add_config( ls_config ).
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_data_config~get_configs.
    rt_configs = mt_config.
  ENDMETHOD.
  METHOD Lif_abapgit_data_config~remove_config.

    ASSERT is_config-type IS NOT INITIAL.
    ASSERT is_config-name IS NOT INITIAL.
    ASSERT is_config-name = to_upper( is_config-name ).

    DELETE mt_config WHERE name = is_config-name AND type = is_config-type.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Not found' ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_data_config~to_json.

    DATA ls_config LIKE LINE OF mt_config.
    DATA ls_file LIKE LINE OF rt_files.

    ls_file-path = Lif_abapgit_data_config=>c_default_path.

    LOOP AT mt_config INTO ls_config.
      ls_file-data = dump( ls_config ).
      ls_file-sha1 = Lcl_abapgit_hash=>sha1_blob( ls_file-data ).
      ls_config-type = Lif_abapgit_data_config=>c_config.
      ls_file-filename = Lcl_abapgit_data_utils=>build_data_filename( ls_config ).
      APPEND ls_file TO rt_files.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_data_config~update_config.

    Lif_abapgit_data_config~remove_config( is_config ).
    Lif_abapgit_data_config~add_config( is_config ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_DATA_CONFIG implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_IDOC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_idoc=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_idoc=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_IDOC implementation.
*"* method's implementations
*include methods.
  METHOD clear_idoc_segement_field.

    FIELD-SYMBOLS <lg_any_field> TYPE any.

    ASSIGN COMPONENT iv_fieldname OF STRUCTURE cg_structure TO <lg_any_field>.
    IF sy-subrc = 0.
      CLEAR <lg_any_field>.
    ENDIF.

  ENDMETHOD.
  METHOD clear_idoc_segement_fields.

    clear_idoc_segement_field( EXPORTING iv_fieldname = 'DEVC'
                               CHANGING  cg_structure = cg_structure ).
    clear_idoc_segement_field( EXPORTING iv_fieldname = 'PLAST'
                               CHANGING  cg_structure = cg_structure ).
    clear_idoc_segement_field( EXPORTING iv_fieldname = 'PWORK'
                               CHANGING  cg_structure = cg_structure ).
    clear_idoc_segement_field( EXPORTING iv_fieldname = 'PRESP'
                               CHANGING  cg_structure = cg_structure ).
    clear_idoc_segement_field( EXPORTING iv_fieldname = 'CREDATE'
                               CHANGING  cg_structure = cg_structure ).
    clear_idoc_segement_field( EXPORTING iv_fieldname = 'CRETIME'
                               CHANGING  cg_structure = cg_structure ).
    clear_idoc_segement_field( EXPORTING iv_fieldname = 'LDATE'
                               CHANGING  cg_structure = cg_structure ).
    clear_idoc_segement_field( EXPORTING iv_fieldname = 'LTIME'
                               CHANGING  cg_structure = cg_structure ).
  ENDMETHOD.
  METHOD constructor.

    super->constructor( is_item = is_item
                        iv_language = iv_language ).

    mv_idoctyp = ms_item-obj_name.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: ls_attributes TYPE edi_iapi01.

    CALL FUNCTION 'IDOCTYPE_READ'
      EXPORTING
        pi_idoctyp       = mv_idoctyp
      IMPORTING
        pe_attributes    = ls_attributes
      EXCEPTIONS
        object_not_found = 1
        db_error         = 2
        no_authority     = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    rv_user = ls_attributes-plast.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    CALL FUNCTION 'IDOCTYPE_DELETE'
      EXPORTING
        pi_idoctyp          = mv_idoctyp
      EXCEPTIONS
        object_not_found    = 1
        lock_error          = 2
        action_not_possible = 3
        transport_error     = 4
        db_error            = 5
        no_authority        = 6
        OTHERS              = 7.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_idoc       TYPE ty_idoc,
          lv_transport  TYPE trkorr,
          ls_edbas      TYPE edbas,
          ls_attributes TYPE edi_iapi05.

    io_xml->read(
      EXPORTING
        iv_name = 'IDOC'
      CHANGING
        cg_data = ls_idoc ).

    MOVE-CORRESPONDING ls_idoc-attributes TO ls_attributes.

    IF Lif_abapgit_object~exists( ) = abap_false.
      CALL FUNCTION 'IDOCTYPE_CREATE'
        EXPORTING
          pi_idoctyp       = mv_idoctyp
          pi_devclass      = iv_package
          pi_attributes    = ls_attributes
        TABLES
          pt_syntax        = ls_idoc-t_syntax
        EXCEPTIONS
          object_not_found = 1
          object_exists    = 2
          syntax_error     = 3
          segment_error    = 4
          transport_error  = 5
          db_error         = 6
          no_authority     = 7
          OTHERS           = 8.
    ELSE.
      IF is_closed( ) = abap_true.
        CALL FUNCTION 'IDOCTYPE_UNCLOSE'
          EXPORTING
            pi_idoctyp          = mv_idoctyp
          EXCEPTIONS
            object_not_found    = 1
            action_not_possible = 2
            db_error            = 3
            no_authority        = 4
            OTHERS              = 5.
        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise_t100( ).
        ENDIF.
      ENDIF.

      CALL FUNCTION 'IDOCTYPE_UPDATE'
        EXPORTING
          pi_idoctyp       = mv_idoctyp
          pi_attributes    = ls_attributes
        TABLES
          pt_syntax        = ls_idoc-t_syntax
        EXCEPTIONS
          object_not_found = 1
          object_exists    = 2
          syntax_error     = 3
          segment_error    = 4
          transport_error  = 5
          db_error         = 6
          no_authority     = 7
          OTHERS           = 8.
    ENDIF.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF ls_idoc-attributes-closed = abap_true.
      IF iv_transport IS NOT INITIAL.
        lv_transport = iv_transport.

        CALL FUNCTION 'IDOCTYPE_CLOSE'
          EXPORTING
            pi_idoctyp          = mv_idoctyp
          CHANGING
            pc_order            = lv_transport
          EXCEPTIONS
            object_not_found    = 1
            action_not_possible = 2
            db_error            = 3
            no_authority        = 4
            OTHERS              = 5.
        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise_t100( ).
        ENDIF.
      ENDIF.

      " IDOCTYPE_CLOSE saves current release but it should be same as in repo
      SELECT SINGLE * FROM edbas INTO ls_edbas WHERE idoctyp = mv_idoctyp.
      ls_edbas-released = ls_idoc-attributes-released.
      ls_edbas-applrel  = ls_idoc-attributes-applrel.
      ls_edbas-closed   = ls_idoc-attributes-closed.
      UPDATE edbas FROM ls_edbas.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Error updating IDOC { mv_idoctyp }| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    CALL FUNCTION 'IDOCTYPE_EXISTENCE_CHECK'
      EXPORTING
        pi_idoctyp       = mv_idoctyp
      EXCEPTIONS
        object_not_found = 1
        db_error         = 2
        OTHERS           = 3.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPMSED5'.
    <ls_bdcdata>-dynpro   = '0010'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'SED5STRUC-OBJECT'.
    <ls_bdcdata>-fval = ms_item-obj_name.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'SED5STRUC-SELECT_ORG'.
    <ls_bdcdata>-fval = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=DISP'.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'WE30'
      it_bdcdata = lt_bdcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_idoc TYPE ty_idoc.

    CALL FUNCTION 'IDOCTYPE_READ'
      EXPORTING
        pi_idoctyp       = mv_idoctyp
      IMPORTING
        pe_attributes    = ls_idoc-attributes
      TABLES
        pt_syntax        = ls_idoc-t_syntax
      EXCEPTIONS
        object_not_found = 1
        db_error         = 2
        no_authority     = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    clear_idoc_segement_fields( CHANGING cg_structure = ls_idoc-attributes ).

    io_xml->add( iv_name = 'IDOC'
                 ig_data = ls_idoc ).

  ENDMETHOD.
  METHOD is_closed.

    DATA ls_idoc TYPE ty_idoc.

    CALL FUNCTION 'IDOCTYPE_READ'
      EXPORTING
        pi_idoctyp       = mv_idoctyp
      IMPORTING
        pe_attributes    = ls_idoc-attributes
      EXCEPTIONS
        object_not_found = 1
        db_error         = 2
        no_authority     = 3
        OTHERS           = 4.
    rv_closed = boolc( sy-subrc = 0 AND ls_idoc-attributes-closed = abap_true ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_IDOC implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_IEXT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_iext=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_iext=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_IEXT implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_extension = ms_item-obj_name.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: ls_attributes TYPE edi_iapi01.

    CALL FUNCTION 'EXTTYPE_READ'
      EXPORTING
        pi_cimtyp     = mv_extension
      IMPORTING
        pe_attributes = ls_attributes
      EXCEPTIONS
        OTHERS        = 1.

    rv_user = ls_attributes-plast.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    CALL FUNCTION 'EXTTYPE_DELETE'
      EXPORTING
        pi_cimtyp = mv_extension
      EXCEPTIONS
        OTHERS    = 1.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_extension  TYPE ty_extention,
          ls_attributes TYPE edi_iapi05.

    io_xml->read( EXPORTING iv_name = c_dataname_iext
                  CHANGING  cg_data = ls_extension ).

    MOVE-CORRESPONDING ls_extension-attributes TO ls_attributes.
    ls_attributes-presp = sy-uname.
    ls_attributes-pwork = ls_attributes-presp.

    IF Lif_abapgit_object~exists( ) = abap_true.
      CALL FUNCTION 'EXTTYPE_UPDATE'
        EXPORTING
          pi_cimtyp     = mv_extension
          pi_attributes = ls_attributes
        TABLES
          pt_syntax     = ls_extension-t_syntax
        EXCEPTIONS
          OTHERS        = 1.
    ELSE.
      CALL FUNCTION 'EXTTYPE_CREATE'
        EXPORTING
          pi_cimtyp     = mv_extension
          pi_devclass   = iv_package
          pi_attributes = ls_attributes
        TABLES
          pt_syntax     = ls_extension-t_syntax
        EXCEPTIONS
          OTHERS        = 1.
    ENDIF.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.


  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    CALL FUNCTION 'EXTTYPE_READ'
      EXPORTING
        pi_cimtyp = mv_extension
      EXCEPTIONS
        OTHERS    = 1.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPMSED5'.
    <ls_bdcdata>-dynpro   = '0010'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'SED5STRUC-OBJECT'.
    <ls_bdcdata>-fval = ms_item-obj_name.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'SED5STRUC-SELECT_EXT'.
    <ls_bdcdata>-fval = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=DISP'.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'WE30'
      it_bdcdata = lt_bdcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA ls_extension           TYPE ty_extention.

    CALL FUNCTION 'EXTTYPE_READ'
      EXPORTING
        pi_cimtyp     = mv_extension
      IMPORTING
        pe_attributes = ls_extension-attributes
      TABLES
        pt_syntax     = ls_extension-t_syntax
      EXCEPTIONS
        OTHERS        = 1.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    Lcl_abapgit_object_idoc=>clear_idoc_segement_fields( CHANGING cg_structure = ls_extension-attributes ).

    io_xml->add( iv_name = c_dataname_iext
                 ig_data = ls_extension ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_IEXT implementation

*>>>>>>> ZCL_ABAPGIT_FILENAME_LOGIC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_filename_logic====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_filename_logic====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_filename_logic====ccau.
*CLASS SHRITEFUH64VYIPN5I4UHL45BF7R4A DEFINITION DEFERRED.
*CLASS zcl_abapgit_filename_logic DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BF7R4A.

CLASS SHRITEFUH64VYIPN5I4UHL45BGAR4A DEFINITION.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_persist_settings.
ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BGAR4A IMPLEMENTATION.
  METHOD Lif_abapgit_persist_settings~modify.
    RETURN.
  ENDMETHOD.

  METHOD Lif_abapgit_persist_settings~read.
    CREATE OBJECT ro_settings.
  ENDMETHOD.
ENDCLASS.



class LCL_ABAPGIT_FILENAME_LOGIC implementation.
*"* method's implementations
*include methods.
  METHOD detect_obj_definition.

    ev_is_xml  = boolc( iv_ext = to_upper( c_package_file-extension ) AND strlen( iv_type ) = 4 ).
    ev_is_json = boolc( iv_ext = to_upper( c_json_file-extension ) AND strlen( iv_type ) = 4 ).

  ENDMETHOD.
  METHOD file_to_object.

    DATA:
      lv_name TYPE string,
      lv_type TYPE string,
      lv_ext  TYPE string.

    " Guess object type and name
    SPLIT to_upper( iv_filename ) AT '.' INTO lv_name lv_type lv_ext.

    " Handle namespaces
    REPLACE ALL OCCURRENCES OF '#' IN lv_name WITH '/'.
    REPLACE ALL OCCURRENCES OF '#' IN lv_type WITH '/'.
    REPLACE ALL OCCURRENCES OF '#' IN lv_ext WITH '/'.

    " Assume AFF namespace convention
    CREATE OBJECT go_aff_registry TYPE Lcl_abapgit_aff_registry.

    IF go_aff_registry->is_supported_object_type( |{ lv_type }| ) = abap_true.
      REPLACE ALL OCCURRENCES OF '(' IN lv_name WITH '/'.
      REPLACE ALL OCCURRENCES OF ')' IN lv_name WITH '/'.
    ENDIF.

    " Get original object name
    lv_name = name_unescape( lv_name ).

    CLEAR es_item.
    es_item-obj_type = lv_type.
    es_item-obj_name = lv_name.

    " Get mapping specific to object type
    map_filename_to_object(
      EXPORTING
        iv_filename = iv_filename
        iv_path     = iv_path
        io_dot      = io_dot
        iv_package  = iv_devclass
      CHANGING
        cs_item     = es_item ).

    detect_obj_definition(
      EXPORTING
        iv_ext     = lv_ext
        iv_type    = lv_type
      IMPORTING
        ev_is_xml  = ev_is_xml
        ev_is_json = ev_is_json ).

  ENDMETHOD.
  METHOD is_obj_definition_file.

    DATA:
      lv_xml  TYPE abap_bool,
      lv_json TYPE abap_bool,
      lv_name TYPE string,
      lv_type TYPE string,
      lv_ext  TYPE string.

    SPLIT to_upper( iv_filename ) AT '.' INTO lv_name lv_type lv_ext.

    detect_obj_definition(
      EXPORTING
        iv_ext     = lv_ext
        iv_type    = lv_type
      IMPORTING
        ev_is_xml  = lv_xml
        ev_is_json = lv_json ).

    rv_yes = boolc( lv_json = abap_true OR lv_xml = abap_true ).

  ENDMETHOD.
  METHOD object_to_file.

    DATA lv_obj_name TYPE string.
    DATA lv_nb_of_slash TYPE string.

    " Get escaped object name
    lv_obj_name = to_lower( name_escape( is_item-obj_name ) ).

    IF iv_extra IS INITIAL.
      CONCATENATE lv_obj_name '.' is_item-obj_type INTO rv_filename.
    ELSE.
      CONCATENATE lv_obj_name '.' is_item-obj_type '.' iv_extra INTO rv_filename.
    ENDIF.

    IF iv_ext IS NOT INITIAL.
      CONCATENATE rv_filename '.' iv_ext INTO rv_filename.
    ENDIF.

    " Get mapping specific to object type
    TRY.
        map_object_to_filename(
          EXPORTING
            is_item     = is_item
          CHANGING
            cv_filename = rv_filename ).
      CATCH Lcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

    " Handle namespaces
    CREATE OBJECT go_aff_registry TYPE Lcl_abapgit_aff_registry.

    IF go_aff_registry->is_supported_object_type( is_item-obj_type ) = abap_true.
      FIND ALL OCCURRENCES OF `/` IN rv_filename MATCH COUNT lv_nb_of_slash.
      IF lv_nb_of_slash = 2.
        REPLACE FIRST OCCURRENCE OF `/` IN rv_filename WITH `(`.
        REPLACE `/` IN rv_filename WITH `)`.
      ENDIF.
    ELSE.
      REPLACE ALL OCCURRENCES OF '/' IN rv_filename WITH '#'.
    ENDIF.

    TRANSLATE rv_filename TO LOWER CASE.

  ENDMETHOD.
  METHOD map_filename_to_object.

    DATA lv_class TYPE seoclsname.

    " TODO: Add check for supported object types to avoid calls to non-existing classes
    " zcl_abapgit_objects=>is_type_supported( is_item-obj_type )
    " This will trigger class constructor of zcl_abapgit_objects_bridge reading table seometarel
    " which is currently not supported by abaplint test runner

    TRY.
        lv_class = 'LCL_ABAPGIT_OBJECT_' && cs_item-obj_type.

        CALL METHOD (lv_class)=>('LIF_ABAPGIT_OBJECT~MAP_FILENAME_TO_OBJECT')
          EXPORTING
            iv_filename = iv_filename
            iv_path     = iv_path
            io_dot      = io_dot
            iv_package  = iv_package
          CHANGING
            cs_item     = cs_item.
      CATCH cx_sy_dyn_call_illegal_class ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.
  METHOD map_object_to_filename.

    DATA lv_class TYPE seoclsname.

    " TODO: Add check for supported object types to avoid calls to non-existing classes
    " zcl_abapgit_objects=>is_type_supported( is_item-obj_type )
    " This will trigger class constructor of zcl_abapgit_objects_bridge reading table seometarel
    " which is currently not supported by abaplint test runner

    TRY.
        lv_class = 'LCL_ABAPGIT_OBJECT_' && is_item-obj_type.

        CALL METHOD (lv_class)=>('LIF_ABAPGIT_OBJECT~MAP_OBJECT_TO_FILENAME')
          EXPORTING
            is_item     = is_item
          CHANGING
            cv_filename = cv_filename.
      CATCH cx_sy_dyn_call_illegal_class ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.
  METHOD name_escape.
    " Some characters in object names cause problems when identifying the object later
    " -> we escape these characters here
    " cl_http_utility=>escape_url doesn't do dots but escapes slash which we use for namespaces
    " -> we escape just some selected characters
    rv_name = iv_name.
    REPLACE ALL OCCURRENCES OF `#` IN rv_name WITH '%23'.
    REPLACE ALL OCCURRENCES OF `%` IN rv_name WITH '%25'.
    REPLACE ALL OCCURRENCES OF `.` IN rv_name WITH '%2e'.
    REPLACE ALL OCCURRENCES OF `<` IN rv_name WITH '%3c'.
    REPLACE ALL OCCURRENCES OF `=` IN rv_name WITH '%3d'.
    REPLACE ALL OCCURRENCES OF `>` IN rv_name WITH '%3e'.
    REPLACE ALL OCCURRENCES OF `?` IN rv_name WITH '%3f'.
  ENDMETHOD.
  METHOD name_unescape.
    " Replace all %xy with encoded character
    rv_name = cl_http_utility=>unescape_url( iv_name ).
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_FILENAME_LOGIC implementation

*>>>>>>> ZCL_ABAPGIT_HTML_VIEWER_GUI <<<<<<<*

*"* macro definitions
*include zcl_abapgit_html_viewer_gui===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_html_viewer_gui===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_HTML_VIEWER_GUI implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    DATA: lt_events TYPE cntl_simple_events,
          ls_event  LIKE LINE OF lt_events.

    CREATE OBJECT mo_html_viewer
      EXPORTING
        query_table_disabled = iv_disable_query_table
        parent               = io_container.

    ls_event-eventid    = Lif_abapgit_html_viewer=>c_id_sapevent.
    ls_event-appl_event = abap_true.
    APPEND ls_event TO lt_events.

    mo_html_viewer->set_registered_events( lt_events ).
    SET HANDLER on_event FOR mo_html_viewer.

  ENDMETHOD.
  METHOD on_event.

    RAISE EVENT Lif_abapgit_html_viewer~sapevent
      EXPORTING
        action      = action
        frame       = frame
        getdata     = getdata
        postdata    = postdata
        query_table = query_table.

  ENDMETHOD.
  METHOD Lif_abapgit_html_viewer~back.

    mo_html_viewer->go_back( ).

  ENDMETHOD.
  METHOD Lif_abapgit_html_viewer~close_document.

    mo_html_viewer->close_document( ).

  ENDMETHOD.
  METHOD Lif_abapgit_html_viewer~free.

    mo_html_viewer->free( ).

  ENDMETHOD.
  METHOD Lif_abapgit_html_viewer~get_url.

    DATA lv_url TYPE c LENGTH 250.
    mo_html_viewer->get_current_url( IMPORTING url = lv_url ).
    cl_gui_cfw=>flush( ).
    rv_url = lv_url.

  ENDMETHOD.
  METHOD Lif_abapgit_html_viewer~load_data.

    DATA lv_url TYPE c LENGTH 250.
    DATA lv_assigned TYPE c LENGTH 250.

    ASSERT strlen( iv_url ) <= 250.
    lv_url = iv_url.
    mo_html_viewer->load_data(
      EXPORTING
        url                    = lv_url
        type                   = iv_type
        subtype                = iv_subtype
        size                   = iv_size
      IMPORTING
        assigned_url           = lv_assigned
      CHANGING
        data_table             = ct_data_table
      EXCEPTIONS
        dp_invalid_parameter   = 1
        dp_error_general       = 2
        cntl_error             = 3
        " html_syntax_notcorrect = 4  " not in lower releases
        OTHERS                 = 5 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Error loading data for HTML viewer' ).
    ENDIF.
    ev_assigned_url = lv_assigned.

  ENDMETHOD.
  METHOD Lif_abapgit_html_viewer~set_focus.
    cl_gui_control=>set_focus(
      EXPORTING
        control           = mo_html_viewer
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error in: cl_gui_control=>set_focus - SUBRC = { sy-subrc }| ).
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_html_viewer~set_registered_events.

    mo_html_viewer->set_registered_events(
      EXPORTING
        events                    = it_events
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3
        OTHERS                    = 4 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Error registering events for HTML viewer' ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_html_viewer~set_visiblity.
    DATA: lv_visible TYPE c LENGTH 1.

    IF iv_visible = abap_true.
      lv_visible = cl_gui_container=>visible_true.
    ELSE.
      lv_visible = cl_gui_container=>visible_false.
    ENDIF.

    mo_html_viewer->set_visible( lv_visible ).
  ENDMETHOD.
  METHOD Lif_abapgit_html_viewer~show_url.

    DATA lv_url TYPE c LENGTH 250.
    lv_url = iv_url.
    mo_html_viewer->show_url(
      EXPORTING
        url                    = lv_url
      EXCEPTIONS
        cntl_error             = 1
        cnht_error_not_allowed = 2
        cnht_error_parameter   = 3
        dp_error_general       = 4
        OTHERS                 = 5 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Error showing URL in HTML viewer' ).
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_HTML_VIEWER_GUI implementation

*>>>>>>> ZCL_ABAPGIT_PASSWORD_DIALOG <<<<<<<*

*"* macro definitions
*include zcl_abapgit_password_dialog===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_password_dialog===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_PASSWORD_DIALOG implementation.
*"* method's implementations
*include methods.
  METHOD popup.

    DATA: lx_error TYPE REF TO cx_sy_dyn_call_illegal_form.

    IF Lcl_abapgit_ui_factory=>get_frontend_services( )->gui_is_available( ) = abap_true.

      TRY.
          PERFORM password_popup
            IN PROGRAM (sy-cprog)
            USING iv_repo_url
            CHANGING cv_user cv_pass.
        CATCH cx_sy_dyn_call_illegal_form INTO lx_error.
          " abapGit was called via API and either wrong or no username/password
          " was supplied. It's not possible to call abapGit password popup in
          " this case.
          " See https://docs.abapgit.org/development/api.html#online-repository
          " on how to provide username/password
          Lcx_abapgit_exception=>raise_with_text( lx_error ).
      ENDTRY.
    ELSE.
      "Extract user credentials from the environment...
      "Class ZCL_ABAPGIT_DEFAULT_AUTH_INFO is part of https://github.com/abapGit/ADT_Backend.
      "It stores the credentials of a private repository as long as the session exists.
      "Usually this class should belong to abapGit core and a refactoring is recommended.
      "As a temporary solution - and to avoid a DYNPRO_SEND_IN_BACKGROUND dump - a generic
      "call of the getter methods for username and password is implemented by PR#2635.
      TRY.
          CALL METHOD ('LCL_ABAPGIT_DEFAULT_AUTH_INFO')=>('GET_USER')
            RECEIVING
              rv_user = cv_user.
        CATCH cx_root.
          RETURN.
      ENDTRY.
      TRY.
          CALL METHOD ('LCL_ABAPGIT_DEFAULT_AUTH_INFO')=>('GET_PASSWORD')
            RECEIVING
              rv_password = cv_pass.
        CATCH cx_root.
          "check if old version with typo in method name exists
          TRY.
              CALL METHOD ('LCL_ABAPGIT_DEFAULT_AUTH_INFO')=>('GET_PASSOWORD')
                RECEIVING
                  rv_password = cv_pass.
            CATCH cx_root.
              RETURN.
          ENDTRY.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_PASSWORD_DIALOG implementation

*>>>>>>> ZCL_ABAPGIT_GUI_ASSET_MANAGER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_asset_manager=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_asset_manager=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_gui_asset_manager=ccau.


class LCL_ABAPGIT_GUI_ASSET_MANAGER implementation.
*"* method's implementations
*include methods.
  METHOD create.
    CREATE OBJECT ri_asset_manager TYPE Lcl_abapgit_gui_asset_manager.
  ENDMETHOD.
  METHOD get_mime_asset.

    DATA: ls_key    TYPE wwwdatatab,
          lv_size_c TYPE wwwparams-value,
          lv_size   TYPE i,
          lt_w3mime TYPE STANDARD TABLE OF w3mime,
          ls_w3mime LIKE LINE OF lt_w3mime.

    ls_key-relid = 'MI'.
    ls_key-objid = iv_mime_name.

    " Get exact file size
    CALL FUNCTION 'WWWPARAMS_READ'
      EXPORTING
        relid            = ls_key-relid
        objid            = ls_key-objid
        name             = 'filesize'
      IMPORTING
        value            = lv_size_c
      EXCEPTIONS
        entry_not_exists = 1.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_size = lv_size_c.

    " Get binary data
    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key               = ls_key
      TABLES
        mime              = lt_w3mime
      EXCEPTIONS
        wrong_object_type = 1
        import_error      = 2.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_w3mime INTO ls_w3mime.
      CONCATENATE rv_xdata ls_w3mime-line INTO rv_xdata IN BYTE MODE.
    ENDLOOP.
    rv_xdata = rv_xdata(lv_size).

  ENDMETHOD.
  METHOD load_asset.

    MOVE-CORRESPONDING is_asset_entry TO rs_asset.
    IF rs_asset-content IS INITIAL AND is_asset_entry-mime_name IS NOT INITIAL.
      " inline content has the priority
      rs_asset-content = get_mime_asset( is_asset_entry-mime_name ).
    ENDIF.
    IF rs_asset-content IS INITIAL.
      Lcx_abapgit_exception=>raise( |failed to load GUI asset: { is_asset_entry-url }| ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_asset_manager~get_all_assets.

    FIELD-SYMBOLS <ls_a> LIKE LINE OF mt_asset_register.

    LOOP AT mt_asset_register ASSIGNING <ls_a>.
      APPEND load_asset( <ls_a> ) TO rt_assets.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_asset_manager~get_asset.

    FIELD-SYMBOLS <ls_a> LIKE LINE OF mt_asset_register.

    READ TABLE mt_asset_register WITH KEY url = iv_url ASSIGNING <ls_a>.
    IF <ls_a> IS NOT ASSIGNED.
      Lcx_abapgit_exception=>raise( |Cannot find GUI asset: { iv_url }| ).
    ENDIF.
    rs_asset = load_asset( <ls_a> ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_asset_manager~get_text_asset.

    DATA ls_asset TYPE Lif_abapgit_gui_asset_manager~ty_web_asset.
    ls_asset = Lif_abapgit_gui_asset_manager~get_asset( iv_url ).

    IF ls_asset-type <> 'text'.
      Lcx_abapgit_exception=>raise( |Not a text asset: { iv_url }| ).
    ENDIF.

    IF iv_assert_subtype IS NOT INITIAL AND ls_asset-subtype <> iv_assert_subtype.
      Lcx_abapgit_exception=>raise( |Wrong subtype ({ iv_assert_subtype }): { iv_url }| ).
    ENDIF.

    rv_asset = Lcl_abapgit_convert=>xstring_to_string_utf8( ls_asset-content ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_asset_manager~register_asset.

    DATA ls_asset LIKE LINE OF mt_asset_register.

    SPLIT iv_type AT '/' INTO ls_asset-type ls_asset-subtype.
    ls_asset-url          = iv_url.
    ls_asset-mime_name    = iv_mime_name.
    ls_asset-is_cacheable = iv_cachable.
    IF iv_base64 IS NOT INITIAL.
      ls_asset-content = Lcl_abapgit_convert=>base64_to_xstring( iv_base64 ).
    ELSEIF iv_inline IS NOT INITIAL.
      ls_asset-content = Lcl_abapgit_convert=>string_to_xstring( iv_inline ).
    ENDIF.

    DELETE mt_asset_register WHERE url = iv_url.
    " TODO: Maybe forbid averwriting cachable assets as they were probably already cached ... agrueable
    APPEND ls_asset TO mt_asset_register.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_ASSET_MANAGER implementation

*>>>>>>> ZCL_ABAPGIT_GUI_IN_PAGE_MODAL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_in_page_modal=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_in_page_modal=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_IN_PAGE_MODAL implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    ms_attrs-width  = iv_width.
    ms_attrs-height = iv_height.
    mi_child        = ii_child.

  ENDMETHOD.
  METHOD create.
    CREATE OBJECT ro_wrap
      EXPORTING
        ii_child  = ii_child
        iv_width  = iv_width
        iv_height = iv_height.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    DATA lo_style TYPE REF TO Lcl_abapgit_string_buffer.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.
    CREATE OBJECT lo_style.

    IF ms_attrs-width IS NOT INITIAL.
      lo_style->add( |width:{ ms_attrs-width }px;| ).
    ENDIF.
    IF ms_attrs-height IS NOT INITIAL.
      lo_style->add( |height:{ ms_attrs-height }px;| ).
    ENDIF.

    ri_html->add( |<div class="modal" style="{ lo_style->join_w_space_and_flush( ) }">| ).
    ri_html->add( |<div class="modal-guts">| ).
    ri_html->add( mi_child->render( ) ).
    ri_html->add( |</div>| ).
    ri_html->add( |</div>| ).
    ri_html->add( |<div class="modal-overlay"></div>| ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_IN_PAGE_MODAL implementation

*>>>>>>> ZCL_ABAPGIT_GUI_UTILS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_utils=========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_utils=========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_gui_utils=========ccau.
CLASS SHRITEFUH64VYIPN5I4UHL45BI5R4A DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_gui_renderable.
ENDCLASS.
CLASS SHRITEFUH64VYIPN5I4UHL45BI5R4A IMPLEMENTATION.
  METHOD Lif_abapgit_gui_renderable~render.
  ENDMETHOD.
ENDCLASS.
CLASS SHRITEFUH64VYIPN5I4UHL45BI7R4A DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_gui_event_handler.
ENDCLASS.
CLASS SHRITEFUH64VYIPN5I4UHL45BI7R4A IMPLEMENTATION.
  METHOD Lif_abapgit_gui_event_handler~on_event.
  ENDMETHOD.
ENDCLASS.




class LCL_ABAPGIT_GUI_UTILS implementation.
*"* method's implementations
*include methods.
  METHOD is_event_handler.
    DATA li_event_handler TYPE REF TO Lif_abapgit_gui_event_handler.
    TRY.
        li_event_handler ?= io_obj.
        rv_yes = abap_true.
      CATCH cx_sy_move_cast_error.
        rv_yes = abap_false.
    ENDTRY.
  ENDMETHOD.
  METHOD is_renderable.
    DATA li_renderable TYPE REF TO Lif_abapgit_gui_renderable.
    TRY.
        li_renderable ?= io_obj.
        rv_yes = abap_true.
      CATCH cx_sy_move_cast_error.
        rv_yes = abap_false.
    ENDTRY.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_UTILS implementation

*>>>>>>> ZCL_ABAPGIT_LOGIN_MANAGER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_login_manager=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_login_manager=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_login_manager=====ccau.


class LCL_ABAPGIT_LOGIN_MANAGER implementation.
*"* method's implementations
*include methods.
  METHOD append.

    FIELD-SYMBOLS: <ls_auth> LIKE LINE OF gt_auth.

    READ TABLE gt_auth WITH KEY uri = Lcl_abapgit_url=>host( iv_uri )
      TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO gt_auth ASSIGNING <ls_auth>.
      <ls_auth>-uri           = Lcl_abapgit_url=>host( iv_uri ).
      <ls_auth>-authorization = iv_auth.
    ENDIF.

  ENDMETHOD.
  METHOD clear.

    CLEAR gt_auth.

  ENDMETHOD.
  METHOD get.

    DATA ls_auth LIKE LINE OF gt_auth.

    READ TABLE gt_auth INTO ls_auth WITH KEY uri = Lcl_abapgit_url=>host( iv_uri ).
    IF sy-subrc = 0.
      rv_auth = ls_auth-authorization.
    ENDIF.

  ENDMETHOD.
  METHOD load.

    DATA ls_auth LIKE LINE OF gt_auth.

    READ TABLE gt_auth INTO ls_auth WITH KEY uri = Lcl_abapgit_url=>host( iv_uri ).
    IF sy-subrc = 0.
      rv_authorization = ls_auth-authorization.
    ENDIF.

  ENDMETHOD.
  METHOD save.

    IF NOT iv_authorization IS INITIAL.
      append( iv_uri  = iv_uri
              iv_auth = iv_authorization ).
    ENDIF.

  ENDMETHOD.
  METHOD set.

    DATA: lv_concat TYPE string.

    ASSERT NOT iv_uri IS INITIAL.

    IF iv_username IS INITIAL OR iv_password IS INITIAL.
      RETURN.
    ENDIF.

    CONCATENATE iv_username ':' iv_password INTO lv_concat.

    rv_auth = cl_http_utility=>encode_base64( lv_concat ).

    CONCATENATE 'Basic' rv_auth INTO rv_auth
      SEPARATED BY space.

    append( iv_uri  = iv_uri
            iv_auth = rv_auth ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_LOGIN_MANAGER implementation

*>>>>>>> ZCL_ABAPGIT_LOG_VIEWER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_log_viewer========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_log_viewer========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_LOG_VIEWER implementation.
*"* method's implementations
*include methods.
  METHOD calculate_cell_type.

    FIELD-SYMBOLS: <ls_log> LIKE LINE OF gt_log.
    DATA: ls_cell_type LIKE LINE OF <ls_log>-cell_type.

    LOOP AT gt_log ASSIGNING <ls_log>.

      IF <ls_log>-longtext IS NOT INITIAL.
        ls_cell_type-columnname = `LONGTEXT`.
        ls_cell_type-value      = if_salv_c_cell_type=>hotspot.
        INSERT ls_cell_type INTO TABLE <ls_log>-cell_type.
      ENDIF.

      IF <ls_log>-t100 IS NOT INITIAL.
        ls_cell_type-columnname = `T100`.
        ls_cell_type-value      = if_salv_c_cell_type=>hotspot.
        INSERT ls_cell_type INTO TABLE <ls_log>-cell_type.
      ENDIF.

      IF <ls_log>-source IS NOT INITIAL.
        ls_cell_type-columnname = `SOURCE`.
        ls_cell_type-value      = if_salv_c_cell_type=>hotspot.
        INSERT ls_cell_type INTO TABLE <ls_log>-cell_type.
      ENDIF.

      IF <ls_log>-callstack IS NOT INITIAL.
        ls_cell_type-columnname = `CALLSTACK`.
        ls_cell_type-value      = if_salv_c_cell_type=>hotspot.
        INSERT ls_cell_type INTO TABLE <ls_log>-cell_type.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD dispatch.

    CASE iv_column.
      WHEN `LONGTEXT`.

        show_longtext( is_log ).

      WHEN `T100`.

        goto_t100_message( is_log ).

      WHEN `SOURCE`.

        goto_source( is_log ).

      WHEN `CALLSTACK`.

        goto_callstack( is_log ).

    ENDCASE.

  ENDMETHOD.
  METHOD get_exception_viewer.

    DATA:
      lx_abapgit TYPE REF TO Lcx_abapgit_exception.

    ASSERT is_log-exception IS BOUND.
    lx_abapgit ?= is_log-exception.

    CREATE OBJECT ro_exception_viewer
      EXPORTING
        ix_error = lx_abapgit.

  ENDMETHOD.
  METHOD goto_callstack.

    get_exception_viewer( is_log )->show_callstack( ).

  ENDMETHOD.
  METHOD goto_source.

    get_exception_viewer( is_log )->goto_source( ).

  ENDMETHOD.
  METHOD goto_t100_message.

    get_exception_viewer( is_log )->goto_message( ).

  ENDMETHOD.
  METHOD on_link_click.

    DATA: lx_error TYPE REF TO Lcx_abapgit_exception.
    FIELD-SYMBOLS: <ls_log> TYPE ty_log_out.

    IF row IS INITIAL
    OR column IS INITIAL.
      RETURN.
    ENDIF.

    READ TABLE gt_log ASSIGNING <ls_log>
                      INDEX row.
    ASSERT sy-subrc = 0.

    TRY.
        dispatch(
            is_log    = <ls_log>
            iv_column = column ).

      CATCH Lcx_abapgit_exception INTO lx_error.
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.
  METHOD prepare_log_for_display.

    DATA: lt_message      TYPE Lif_abapgit_log=>ty_log_outs,
          lr_message      TYPE REF TO Lif_abapgit_log=>ty_log_out,
          ls_log          TYPE ty_log_out,
          li_t100_message TYPE REF TO if_t100_message,
          lx_abapgit      TYPE REF TO Lcx_abapgit_exception.

    lt_message = ii_log->get_messages( ).

    LOOP AT lt_message REFERENCE INTO lr_message.

      CLEAR: ls_log.

      ls_log-msg = lr_message->text.
      ls_log-exception = lr_message->exception.

      CASE lr_message->type.
        WHEN 'E' OR 'A' OR 'X'.
          ls_log-type = icon_led_red.
        WHEN 'W'.
          ls_log-type = icon_led_yellow.
        WHEN 'I' OR 'S'.
          ls_log-type = icon_led_green.
        WHEN OTHERS.
          ls_log-type = icon_led_inactive.
      ENDCASE.

      IF lr_message->exception IS BOUND.

        TRY.
            li_t100_message ?= lr_message->exception.

            IF li_t100_message->t100key IS NOT INITIAL.
              ls_log-t100 = icon_message_information.
            ENDIF.

          CATCH cx_sy_move_cast_error ##NO_HANDLER.
        ENDTRY.

        TRY.
            lx_abapgit ?= lr_message->exception.

            IF lx_abapgit->mt_callstack IS NOT INITIAL.
              ls_log-longtext  = icon_system_help.
              ls_log-callstack = icon_stack.
              ls_log-source    = icon_abap.
            ENDIF.

          CATCH cx_sy_move_cast_error ##NO_HANDLER.
        ENDTRY.

      ENDIF.

      ls_log-obj_type = lr_message->obj_type.
      ls_log-obj_name = lr_message->obj_name.

      INSERT ls_log INTO TABLE rt_log_out.

    ENDLOOP.

  ENDMETHOD.
  METHOD show_log.

    DATA: lr_log         TYPE REF TO ty_log_out,
          lo_alv         TYPE REF TO cl_salv_table,
          lx_error       TYPE REF TO cx_salv_error,
          lo_form_header TYPE REF TO cl_salv_form_header_info,
          lo_columns     TYPE REF TO cl_salv_columns_table,
          lo_column      TYPE REF TO cl_salv_column,
          lo_functions   TYPE REF TO cl_salv_functions_list,
          ls_position    TYPE Lif_abapgit_popups=>ty_popup_position,
          lv_add_obj_col TYPE abap_bool,
          lo_event       TYPE REF TO cl_salv_events_table.

    gt_log = prepare_log_for_display( ii_log ).

    "check if log contains any object info
    LOOP AT gt_log REFERENCE INTO lr_log.
      IF lr_log->obj_type IS NOT INITIAL OR lr_log->obj_name IS NOT INITIAL.
        lv_add_obj_col = abap_true.
      ENDIF.
    ENDLOOP.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = gt_log ).

        lo_functions = lo_alv->get_functions( ).
        lo_functions->set_all( ).

        lo_alv->get_display_settings( )->set_list_header( |abapGit Log Viewer| ).

        lo_columns = lo_alv->get_columns( ).

        lo_columns->set_optimize( ).
        lo_columns->set_cell_type_column( |CELL_TYPE| ).

        calculate_cell_type( ).

        lo_column = lo_columns->get_column( |TYPE| ).
        lo_column->set_medium_text( |Type| ).

        lo_column = lo_columns->get_column( |MSG| ).
        lo_column->set_medium_text( |Message| ).

        lo_column = lo_columns->get_column( |LONGTEXT| ).
        lo_column->set_medium_text( |Longtext| ).

        lo_column = lo_columns->get_column( |T100| ).
        lo_column->set_medium_text( |Goto message| ).

        lo_column = lo_columns->get_column( |SOURCE| ).
        lo_column->set_medium_text( |Goto source| ).

        lo_column = lo_columns->get_column( |CALLSTACK| ).
        lo_column->set_medium_text( |Show callstack| ).

        IF lv_add_obj_col = abap_true.
          lo_column = lo_columns->get_column( |OBJ_TYPE| ).
          lo_column->set_medium_text( |Object Type| ).

          lo_column = lo_columns->get_column( |OBJ_NAME| ).
          lo_column->set_medium_text( |Object Name| ).
        ELSE.
          "hide object columns
          lo_column = lo_columns->get_column( |OBJ_TYPE| ).
          lo_column->set_technical( abap_true ).

          lo_column = lo_columns->get_column( |OBJ_NAME| ).
          lo_column->set_technical( abap_true ).
        ENDIF.

        "hide empty columns
        LOOP AT gt_log TRANSPORTING NO FIELDS WHERE t100 IS NOT INITIAL.
          EXIT.
        ENDLOOP.
        IF sy-subrc <> 0.
          lo_column = lo_columns->get_column( |T100| ).
          lo_column->set_technical( abap_true ).
        ENDIF.

        LOOP AT gt_log TRANSPORTING NO FIELDS WHERE source IS NOT INITIAL.
          EXIT.
        ENDLOOP.
        IF sy-subrc <> 0.
          lo_column = lo_columns->get_column( |SOURCE| ).
          lo_column->set_technical( abap_true ).
        ENDIF.

        LOOP AT gt_log TRANSPORTING NO FIELDS WHERE longtext IS NOT INITIAL.
          EXIT.
        ENDLOOP.
        IF sy-subrc <> 0.
          lo_column = lo_columns->get_column( |LONGTEXT| ).
          lo_column->set_technical( abap_true ).
        ENDIF.

        LOOP AT gt_log TRANSPORTING NO FIELDS WHERE callstack IS NOT INITIAL.
          EXIT.
        ENDLOOP.
        IF sy-subrc <> 0.
          lo_column = lo_columns->get_column( |CALLSTACK| ).
          lo_column->set_technical( abap_true ).
        ENDIF.

        ls_position = Lcl_abapgit_popups=>center(
          iv_width  = 125
          iv_height = 20 ).

        lo_alv->set_screen_popup( start_column = ls_position-start_column
                                  end_column   = ls_position-end_column
                                  start_line   = ls_position-start_row
                                  end_line     = ls_position-end_row ).

        CREATE OBJECT lo_form_header
          EXPORTING
            text = ii_log->get_title( ).

        lo_alv->set_top_of_list( lo_form_header ).

        lo_event = lo_alv->get_event( ).

        SET HANDLER on_link_click FOR lo_event.

        lo_alv->display( ).

      CATCH cx_salv_error INTO lx_error.
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.
  METHOD show_longtext.

    DATA: lx_abapgit TYPE REF TO Lcx_abapgit_exception.

    DATA: lv_docu_object TYPE dokhl-object,
          lt_dummy1      TYPE TABLE OF dselc,
          lt_dummy2      TYPE TABLE OF dval,
          ls_help_info   TYPE help_info.

    IF is_log-exception IS NOT BOUND.
      RETURN.
    ENDIF.

    TRY.
        lx_abapgit ?= is_log-exception.
      CATCH cx_sy_move_cast_error.
        RETURN.
    ENDTRY.

    lv_docu_object   = lx_abapgit->if_t100_message~t100key-msgid.
    lv_docu_object+2 = lx_abapgit->if_t100_message~t100key-msgno.

    ls_help_info-call       = 'D'.
    ls_help_info-spras      = sy-langu.
    ls_help_info-messageid  = lx_abapgit->if_t100_message~t100key-msgid.
    ls_help_info-messagenr  = lx_abapgit->if_t100_message~t100key-msgno.
    ls_help_info-message    = is_log-msg.
    ls_help_info-title      = 'Longtext'.
    ls_help_info-docuid     = 'NA'.
    ls_help_info-docuobject = lv_docu_object.
    ls_help_info-msgv1      = lx_abapgit->msgv1.
    ls_help_info-msgv2      = lx_abapgit->msgv2.
    ls_help_info-msgv3      = lx_abapgit->msgv3.
    ls_help_info-msgv4      = lx_abapgit->msgv4.

    CALL FUNCTION 'HELP_START'
      EXPORTING
        help_infos   = ls_help_info
      TABLES
        dynpselect   = lt_dummy1
        dynpvaluetab = lt_dummy2
      EXCEPTIONS
        OTHERS       = 1.

    IF sy-subrc IS NOT INITIAL.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD to_html.

    DATA: lt_message TYPE Lif_abapgit_log=>ty_log_outs,
          lr_message TYPE REF TO Lif_abapgit_log=>ty_log_out,
          lv_class   TYPE string,
          lv_icon    TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    IF ii_log->count( ) = 0.
      RETURN.
    ENDIF.

    lt_message = ii_log->get_messages( ).

    LOOP AT lt_message REFERENCE INTO lr_message.
      CASE lr_message->type.
        WHEN 'W'.
          lv_icon  = 'attention'.
          lv_class = 'warning'.
        WHEN 'E'.
          lv_icon  = 'error'.
          lv_class = 'error'.
        WHEN OTHERS. " ??? unexpected
          lv_icon  = 'error'.
          lv_class = 'error'.
      ENDCASE.

      ri_html->add( |<span class="{ lv_class }">| ).
      ri_html->add_icon( lv_icon ).
      ri_html->add( lr_message->text ).
      ri_html->add( '</span>' ).
    ENDLOOP.

  ENDMETHOD.
  METHOD write_log.

    DATA: lt_message TYPE Lif_abapgit_log=>ty_log_outs,
          lr_message TYPE REF TO Lif_abapgit_log=>ty_log_out,
          lv_text    TYPE string.

    lt_message = ii_log->get_messages( ).

    LOOP AT lt_message REFERENCE INTO lr_message.
      IF lr_message->obj_name IS NOT INITIAL AND lr_message->obj_type IS NOT INITIAL.
        lv_text = |{ lr_message->type }: { lr_message->text } ({ lr_message->obj_type }/{ lr_message->obj_name })|.
      ELSE.
        lv_text = |{ lr_message->type }: { lr_message->text }|.
      ENDIF.
      WRITE: / lv_text.
    ENDLOOP.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_LOG_VIEWER implementation

*>>>>>>> ZCL_ABAPGIT_HTML_PARTS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_html_parts========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_html_parts========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_html_parts========ccau.


class LCL_ABAPGIT_HTML_PARTS implementation.
*"* method's implementations
*include methods.
  METHOD add_part.

    DATA lr_collection TYPE REF TO ty_named_collection.
    lr_collection = get_collection(
      iv_collection = iv_collection
      iv_create_if_missing = abap_true ).
    APPEND ii_part TO lr_collection->pile.

  ENDMETHOD.
  METHOD clear.
    CLEAR mt_part_collections.
  ENDMETHOD.
  METHOD get_collection.

    READ TABLE mt_part_collections REFERENCE INTO rr_collection WITH KEY name = iv_collection.
    IF sy-subrc <> 0 AND iv_create_if_missing = abap_true.
      APPEND INITIAL LINE TO mt_part_collections REFERENCE INTO rr_collection.
      rr_collection->name = iv_collection.
    ENDIF.

  ENDMETHOD.
  METHOD get_collection_names.

    FIELD-SYMBOLS <ls_coll> LIKE LINE OF mt_part_collections.
    LOOP AT mt_part_collections ASSIGNING <ls_coll>.
      APPEND <ls_coll>-name TO rt_list.
    ENDLOOP.

  ENDMETHOD.
  METHOD get_collection_size.

    DATA lr_collection TYPE REF TO ty_named_collection.
    lr_collection = get_collection( iv_collection ).
    IF lr_collection IS BOUND.
      rv_size = lines( lr_collection->pile ).
    ENDIF.

  ENDMETHOD.
  METHOD get_parts.

    DATA lr_collection TYPE REF TO ty_named_collection.
    lr_collection = get_collection( iv_collection ).
    IF lr_collection IS BOUND.
      rt_parts = lr_collection->pile.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_HTML_PARTS implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_IWOM <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_iwom=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_iwom=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_IWOM implementation.
*"* method's implementations
*include methods.
  METHOD get_field_rules.
    ro_result = Lcl_abapgit_field_rules=>create( ).
    ro_result->add(
      iv_table     = '/IWFND/I_MED_OHD'
      iv_field     = 'CREATED_BY'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWFND/I_MED_OHD'
      iv_field     = 'CREATED_TIMESTMP'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-timestamp
    )->add(
      iv_table     = '/IWFND/I_MED_OHD'
      iv_field     = 'CHANGED_BY'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWFND/I_MED_OHD'
      iv_field     = 'CHANGED_TIMESTMP'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-timestamp ).
  ENDMETHOD.
  METHOD get_generic.

    CREATE OBJECT ro_generic
      EXPORTING
        io_field_rules = get_field_rules( )
        is_item        = ms_item
        iv_language    = mv_language.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE changed_by FROM ('/IWFND/I_MED_OHD') INTO rv_user
      WHERE model_identifier = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    get_generic( )->delete( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    get_generic( )->deserialize(
      iv_package = iv_package
      io_xml     = io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    rv_bool = get_generic( )->exists( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    get_generic( )->serialize( io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_IWOM implementation

*>>>>>>> ZCL_ABAPGIT_HTTP_AGENT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_http_agent========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_http_agent========ccimp.
CLASS SHRITEFUH64VYIPN5I4UHL45BJXR4A DEFINITION FINAL.
  PUBLIC SECTION.

    INTERFACES Lif_abapgit_http_response.

    CLASS-METHODS create
      IMPORTING
        ii_client          TYPE REF TO if_http_client
      RETURNING
        VALUE(ri_response) TYPE REF TO Lif_abapgit_http_response.

  PRIVATE SECTION.
    DATA mi_client TYPE REF TO if_http_client.
    DATA mi_response TYPE REF TO if_http_response.
ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BJXR4A IMPLEMENTATION.

  METHOD create.
    DATA lo_response TYPE REF TO SHRITEFUH64VYIPN5I4UHL45BJXR4A.
    CREATE OBJECT lo_response.
    lo_response->mi_client   = ii_client.
    lo_response->mi_response = ii_client->response.
    ri_response ?= lo_response.
  ENDMETHOD.

  METHOD Lif_abapgit_http_response~close.
    mi_client->close( ).
  ENDMETHOD.

  METHOD Lif_abapgit_http_response~is_ok.
    DATA lv_code TYPE i.
    lv_code = Lif_abapgit_http_response~code( ).
    rv_yes = boolc( lv_code >= 200 AND lv_code < 300 ).
  ENDMETHOD.

  METHOD Lif_abapgit_http_response~data.
    rv_data = mi_response->get_data( ).
  ENDMETHOD.

  METHOD Lif_abapgit_http_response~cdata.
    rv_data = mi_response->get_cdata( ).
  ENDMETHOD.

  METHOD Lif_abapgit_http_response~code.
    DATA lv_msg TYPE string ##NEEDED.
    mi_response->get_status(
      IMPORTING
        reason = lv_msg " for debug
        code   = rv_code ).
  ENDMETHOD.

  METHOD Lif_abapgit_http_response~json.

    ri_json = Lcl_abapgit_ajson=>parse( Lif_abapgit_http_response~cdata( ) ).

  ENDMETHOD.

  METHOD Lif_abapgit_http_response~error.
    rv_message = mi_response->get_cdata( ).
  ENDMETHOD.

  METHOD Lif_abapgit_http_response~headers.

    DATA lt_headers TYPE tihttpnvp.
    FIELD-SYMBOLS <ls_h> LIKE LINE OF lt_headers.

    CREATE OBJECT ro_headers.

    mi_response->get_header_fields( CHANGING fields = lt_headers ).
    LOOP AT lt_headers ASSIGNING <ls_h>.
      ro_headers->set(
        iv_key = <ls_h>-name
        iv_val = <ls_h>-value ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

class LCL_ABAPGIT_HTTP_AGENT implementation.
*"* method's implementations
*include methods.
  METHOD attach_payload.

    DATA lo_type TYPE REF TO cl_abap_typedescr.
    lo_type = cl_abap_typedescr=>describe_by_data( iv_payload ).

    IF lo_type->type_kind = cl_abap_typedescr=>typekind_xstring.
      ii_request->set_data( iv_payload ).

    ELSEIF lo_type->type_kind = cl_abap_typedescr=>typekind_string.
      ii_request->set_cdata( iv_payload ).

    ELSE.
      Lcx_abapgit_exception=>raise( |Unexpected payload type { lo_type->absolute_name }| ).
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    CREATE OBJECT mo_global_headers.

  ENDMETHOD.
  METHOD create.

    CREATE OBJECT ri_instance TYPE Lcl_abapgit_http_agent.

  ENDMETHOD.
  METHOD Lif_abapgit_http_agent~global_headers.

    ro_global_headers = mo_global_headers.

  ENDMETHOD.
  METHOD Lif_abapgit_http_agent~request.

    DATA li_client TYPE REF TO if_http_client.
    DATA lo_proxy_configuration TYPE REF TO Lcl_abapgit_proxy_config.
    DATA lv_code TYPE i.
    DATA lv_message TYPE string.
    FIELD-SYMBOLS <ls_entry> LIKE LINE OF io_query->mt_entries.

    CREATE OBJECT lo_proxy_configuration.

    cl_http_client=>create_by_url(
      EXPORTING
        url           = iv_url
        ssl_id        = Lcl_abapgit_exit=>get_instance( )->get_ssl_id( )
        proxy_host    = lo_proxy_configuration->get_proxy_url( iv_url )
        proxy_service = lo_proxy_configuration->get_proxy_port( iv_url )
      IMPORTING
        client = li_client ).

    li_client->request->set_version( if_http_request=>co_protocol_version_1_1 ).
    li_client->request->set_method( iv_method ).

    IF io_query IS BOUND.
      LOOP AT io_query->mt_entries ASSIGNING <ls_entry>.
        li_client->request->set_form_field(
          name  = <ls_entry>-k
          value = <ls_entry>-v ).
      ENDLOOP.
    ENDIF.

    LOOP AT mo_global_headers->mt_entries ASSIGNING <ls_entry>.
      li_client->request->set_header_field(
        name  = to_lower( <ls_entry>-k )
        value = <ls_entry>-v ).
    ENDLOOP.

    IF io_headers IS BOUND.
      LOOP AT io_headers->mt_entries ASSIGNING <ls_entry>.
        li_client->request->set_header_field(
          name  = to_lower( <ls_entry>-k )
          value = <ls_entry>-v ).
      ENDLOOP.
    ENDIF.

    IF iv_method = Lif_abapgit_http_agent=>c_methods-post
      OR iv_method = Lif_abapgit_http_agent=>c_methods-put
      OR iv_method = Lif_abapgit_http_agent=>c_methods-patch.
      attach_payload(
        ii_request = li_client->request
        iv_payload = iv_payload ).
    ENDIF.

    li_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).
    IF sy-subrc = 0.
      li_client->receive(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4 ).
    ENDIF.

    IF sy-subrc <> 0.
      li_client->get_last_error(
        IMPORTING
          code    = lv_code
          message = lv_message ).
      Lcx_abapgit_exception=>raise( |HTTP error: [{ lv_code }] { lv_message }| ).
    ENDIF.

    ri_response = SHRITEFUH64VYIPN5I4UHL45BJXR4A=>create( li_client ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_HTTP_AGENT implementation

*>>>>>>> ZCL_ABAPGIT_HTTP_DIGEST <<<<<<<*

*"* macro definitions
*include zcl_abapgit_http_digest=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_http_digest=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_HTTP_DIGEST implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    parse( ii_client ).

    mv_ha1 = md5( |{ iv_username }:{ mv_realm }:{ iv_password }| ).

    mv_username = iv_username.

  ENDMETHOD.
  METHOD hash.

    DATA: lv_ha2 TYPE string.


    lv_ha2 = md5( |{ iv_method }:{ iv_uri }| ).

    ASSERT NOT iv_cnonse IS INITIAL.

    rv_response = md5( |{ mv_ha1 }:{ iv_nonce }:{ gv_nc }:{ iv_cnonse }:{ iv_qop }:{ lv_ha2 }| ).

  ENDMETHOD.
  METHOD md5.

    DATA lv_xstr TYPE xstring.
    DATA lv_hash TYPE xstring.
    DATA lv_empty TYPE xstring.

    lv_xstr = Lcl_abapgit_convert=>string_to_xstring_utf8( iv_data ).

    TRY.
        cl_abap_hmac=>calculate_hmac_for_raw(
          EXPORTING
            if_algorithm   = 'MD5'
            if_key         = lv_empty
            if_data        = lv_xstr
          IMPORTING
            ef_hmacxstring = lv_hash ).
      CATCH cx_abap_message_digest.
        Lcx_abapgit_exception=>raise( 'error calculating md5' ).
    ENDTRY.

    rv_hash = lv_hash.
    TRANSLATE rv_hash TO LOWER CASE.

  ENDMETHOD.
  METHOD parse.

    DATA: lv_value TYPE string.


    lv_value = ii_client->response->get_header_field( 'www-authenticate' ).

    FIND REGEX 'realm="([\w ]+)"' IN lv_value SUBMATCHES mv_realm.
    FIND REGEX 'qop="(\w+)"' IN lv_value SUBMATCHES mv_qop.
    FIND REGEX 'nonce="([\w=/+\$]+)"' IN lv_value SUBMATCHES mv_nonce.

  ENDMETHOD.
  METHOD run.

    DATA: lv_response TYPE string,
          lv_method   TYPE string,
          lv_cnonce   TYPE string,
          lv_uri      TYPE string,
          lv_auth     TYPE string.


    ASSERT NOT mv_nonce IS INITIAL.

    lv_method = ii_client->request->get_header_field( '~request_method' ).
    lv_uri = ii_client->request->get_header_field( '~request_uri' ).

    CALL FUNCTION 'GENERAL_GET_RANDOM_STRING'
      EXPORTING
        number_chars  = 24
      IMPORTING
        random_string = lv_cnonce.

    lv_response = hash(
      iv_qop    = mv_qop
      iv_nonce  = mv_nonce
      iv_uri    = lv_uri
      iv_method = lv_method
      iv_cnonse = lv_cnonce ).

* client response
    lv_auth = |Digest username="{ mv_username
      }", realm="{ mv_realm
      }", nonce="{ mv_nonce
      }", uri="{ lv_uri
      }", qop={ mv_qop
      }, nc={ gv_nc
      }, cnonce="{ lv_cnonce
      }", response="{ lv_response }"|.

    ii_client->request->set_header_field(
      name  = 'Authorization'
      value = lv_auth ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_HTTP_DIGEST implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_IWPR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_iwpr=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_iwpr=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_IWPR implementation.
*"* method's implementations
*include methods.
  METHOD get_field_rules.
    ro_result = Lcl_abapgit_field_rules=>create( ).
    ro_result->add(
      iv_table     = '/IWBEP/I_SBD_GA'
      iv_field     = 'CREATION_USER_ID'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWBEP/I_SBD_GA'
      iv_field     = 'CREATION_TIME'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-timestamp
    )->add(
      iv_table     = '/IWBEP/I_SBD_GA'
      iv_field     = 'LAST_CHG_USER_ID'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWBEP/I_SBD_GA'
      iv_field     = 'LAST_CHG_TIME'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-timestamp ).
  ENDMETHOD.
  METHOD get_generic.

    CREATE OBJECT ro_generic
      EXPORTING
        io_field_rules = get_field_rules( )
        is_item        = ms_item
        iv_language    = mv_language.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE last_chg_user_id FROM ('/IWBEP/I_SBD_PR') INTO rv_user
      WHERE project = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    get_generic( )->delete( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    get_generic( )->deserialize(
      iv_package = iv_package
      io_xml     = io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    rv_bool = get_generic( )->exists( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    SUBMIT /iwbep/r_sbui_service_builder
      WITH i_prname = ms_item-obj_name
      AND RETURN.

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    get_generic( )->serialize( io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_IWPR implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_IWSG <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_iwsg=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_iwsg=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_IWSG implementation.
*"* method's implementations
*include methods.
  METHOD get_field_rules.

    ro_result = Lcl_abapgit_field_rules=>create( ).
    ro_result->add(
      iv_table     = '/IWFND/I_MED_SRH'
      iv_field     = 'CREATED_BY'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWFND/I_MED_SRH'
      iv_field     = 'CREATED_TIMESTMP'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-timestamp
    )->add(
      iv_table     = '/IWFND/I_MED_SRH'
      iv_field     = 'CHANGED_BY'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWFND/I_MED_SRH'
      iv_field     = 'CHANGED_TIMESTMP'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-timestamp ).

  ENDMETHOD.
  METHOD get_generic.

    CREATE OBJECT ro_generic
      EXPORTING
        io_field_rules = get_field_rules( )
        is_item        = ms_item
        iv_language    = mv_language.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE changed_by FROM ('/IWFND/I_MED_SRH') INTO rv_user
      WHERE srv_identifier = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    get_generic( )->delete( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    get_generic( )->deserialize(
      iv_package = iv_package
      io_xml     = io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    rv_bool = get_generic( )->exists( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    get_generic( )->serialize( io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_IWSG implementation

*>>>>>>> ZCL_ABAPGIT_LANGUAGE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_language==========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_language==========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_LANGUAGE implementation.
*"* method's implementations
*include methods.
  METHOD class_constructor.

    DATA lv_dummy TYPE string.

    GET LOCALE LANGUAGE gv_login_language COUNTRY lv_dummy MODIFIER lv_dummy.

  ENDMETHOD.
  METHOD restore_login_language.

    SET LOCALE LANGUAGE gv_login_language.

  ENDMETHOD.
  METHOD set_current_language.

    SET LOCALE LANGUAGE iv_language.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_LANGUAGE implementation

*>>>>>>> ZCL_ABAPGIT_LOG <<<<<<<*

*"* macro definitions
*include zcl_abapgit_log===============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_log===============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_log===============ccau.




class LCL_ABAPGIT_LOG implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    Lif_abapgit_log~set_title( iv_title ).

  ENDMETHOD.
  METHOD from_exception.

    CREATE OBJECT ro_log.

    IF io_x IS BOUND.
      ro_log->Lif_abapgit_log~add_exception( io_x ).
    ENDIF.

  ENDMETHOD.
  METHOD get_messages_status.

    DATA lr_msg TYPE REF TO Lif_abapgit_log=>ty_msg.
    rv_status = 'S'.
    LOOP AT it_msg REFERENCE INTO lr_msg.
      CASE lr_msg->type.
        WHEN 'E' OR 'A' OR 'X'.
          rv_status = 'E'. "not okay
          EXIT.
        WHEN 'W'.
          rv_status = 'W'. "maybe
          CONTINUE.
        WHEN 'S' OR 'I'.
          IF rv_status <> 'W'.
            rv_status = 'S'. "okay
          ENDIF.
          CONTINUE.
        WHEN OTHERS. "unknown
          CONTINUE.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_log~add.

    FIELD-SYMBOLS: <ls_log> LIKE LINE OF mt_log.

    APPEND INITIAL LINE TO mt_log ASSIGNING <ls_log>.
    <ls_log>-msg-text  = iv_msg.
    <ls_log>-msg-type  = iv_type.
    <ls_log>-item      = is_item.
    <ls_log>-exception = ix_exc.

    CASE iv_type.
      WHEN 'E' OR 'A' OR 'X'.
        <ls_log>-msg-level = Lif_abapgit_log=>c_log_level-error.
      WHEN 'W'.
        <ls_log>-msg-level = Lif_abapgit_log=>c_log_level-warning.
      WHEN 'S' OR 'I'.
        <ls_log>-msg-level = Lif_abapgit_log=>c_log_level-info.
      WHEN OTHERS. "unknown
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_log~add_error.

    Lif_abapgit_log~add(
      iv_msg  = iv_msg
      iv_type = 'E'
      is_item = is_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_log~add_exception.

    DATA lx_exc TYPE REF TO cx_root.
    DATA lv_msg TYPE string.
    lx_exc = ix_exc.
    DO.
      lv_msg = lx_exc->get_text( ).
      Lif_abapgit_log~add( iv_msg  = lv_msg
                           iv_type = 'E'
                           is_item = is_item
                           ix_exc  = lx_exc ).
      IF lx_exc->previous IS BOUND.
        lx_exc = lx_exc->previous.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD.
  METHOD Lif_abapgit_log~add_info.

    Lif_abapgit_log~add(
      iv_msg  = iv_msg
      iv_type = 'I'
      is_item = is_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_log~add_success.

    Lif_abapgit_log~add(
      iv_msg  = iv_msg
      iv_type = 'S'
      is_item = is_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_log~add_warning.

    Lif_abapgit_log~add(
      iv_msg  = iv_msg
      iv_type = 'W'
      is_item = is_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_log~clear.
    CLEAR mt_log.
  ENDMETHOD.
  METHOD Lif_abapgit_log~clone.

    DATA lo_log TYPE REF TO Lcl_abapgit_log.

    CREATE OBJECT lo_log EXPORTING iv_title = mv_title.
    lo_log->mt_log = mt_log.
    ri_log = lo_log.

  ENDMETHOD.
  METHOD Lif_abapgit_log~count.
    rv_count = lines( mt_log ).
  ENDMETHOD.
  METHOD Lif_abapgit_log~get_item_status.

    DATA lr_log         TYPE REF TO ty_log.
    DATA ls_msg         TYPE Lif_abapgit_log=>ty_msg.
    DATA ls_item_status TYPE Lif_abapgit_log=>ty_item_status_out.
    DATA lr_item_status TYPE REF TO Lif_abapgit_log=>ty_item_status_out.

    "collect all message for all objects
    LOOP AT mt_log REFERENCE INTO lr_log.
      CLEAR ls_item_status.
      ls_item_status-item = lr_log->item.
      READ TABLE rt_item_status REFERENCE INTO lr_item_status
           WITH KEY item-obj_type = ls_item_status-item-obj_type
                    item-obj_name = ls_item_status-item-obj_name.
      IF sy-subrc <> 0.
        INSERT ls_item_status INTO TABLE rt_item_status.
        GET REFERENCE OF ls_item_status INTO lr_item_status.
      ENDIF.
      CLEAR ls_msg.
      ls_msg-type = lr_log->msg-type.
      ls_msg-text = lr_log->msg-text.
      INSERT ls_msg INTO TABLE lr_item_status->messages.
    ENDLOOP.

    "determine object status from object messages
    LOOP AT rt_item_status REFERENCE INTO lr_item_status.
      lr_item_status->status = get_messages_status( lr_item_status->messages ).
      IF lr_item_status->messages IS INITIAL.
        CLEAR ls_msg.
        ls_msg-type = 'I'.
        ls_msg-text = 'No message'.
        INSERT ls_msg INTO TABLE lr_item_status->messages.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_log~get_log_level.

    FIELD-SYMBOLS <ls_log> LIKE LINE OF mt_log.

    rv_level = Lif_abapgit_log=>c_log_level-empty.

    LOOP AT mt_log ASSIGNING <ls_log>.
      IF <ls_log>-msg-level = Lif_abapgit_log=>c_log_level-error.
        rv_level = Lif_abapgit_log=>c_log_level-error.
        EXIT.
      ELSEIF <ls_log>-msg-level > rv_level.
        rv_level = <ls_log>-msg-level.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_log~get_messages.
    DATA ls_msg TYPE Lif_abapgit_log~ty_log_out.
    FIELD-SYMBOLS <ls_log> TYPE ty_log.
    LOOP AT mt_log ASSIGNING <ls_log>.
      ls_msg-type      = <ls_log>-msg-type.
      ls_msg-text      = <ls_log>-msg-text.
      ls_msg-obj_type  = <ls_log>-item-obj_type.
      ls_msg-obj_name  = <ls_log>-item-obj_name.
      ls_msg-exception = <ls_log>-exception.
      APPEND ls_msg TO rt_msg.
    ENDLOOP.
    DELETE ADJACENT DUPLICATES FROM rt_msg.
  ENDMETHOD.
  METHOD Lif_abapgit_log~get_status.

    DATA lr_log TYPE REF TO ty_log.
    rv_status = Lif_abapgit_log=>c_status-ok.
    LOOP AT mt_log REFERENCE INTO lr_log.
      CASE lr_log->msg-type.
        WHEN 'E' OR 'A' OR 'X'.
          rv_status = Lif_abapgit_log=>c_status-error.
          EXIT.
        WHEN 'W'.
          rv_status = Lif_abapgit_log=>c_status-warning.
          CONTINUE.
        WHEN 'S' OR 'I'.
          IF rv_status <> Lif_abapgit_log=>c_status-warning.
            rv_status = Lif_abapgit_log=>c_status-ok.
          ENDIF.
          CONTINUE.
        WHEN OTHERS. "unknown
          ASSERT 0 = 1.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_log~get_title.
    rv_title = mv_title.
    IF rv_title IS INITIAL.
      rv_title = 'Log'.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_log~merge_with.

    DATA lo_log TYPE REF TO Lcl_abapgit_log.
    DATA lt_log_temp LIKE lo_log->mt_log.

    IF ii_log IS BOUND.
      lo_log ?= ii_log.
      IF iv_min_level > 0.
        lt_log_temp = lo_log->mt_log.
        DELETE lt_log_temp WHERE msg-level < iv_min_level.
        APPEND LINES OF lt_log_temp TO mt_log.
      ELSE.
        APPEND LINES OF lo_log->mt_log TO mt_log.
      ENDIF.
    ENDIF.

    ri_log = me.

  ENDMETHOD.
  METHOD Lif_abapgit_log~set_title.
    mv_title = iv_title.
    ri_log = me.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_LOG implementation

*>>>>>>> ZCL_ABAPGIT_MERGE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_merge=============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_merge=============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_MERGE implementation.
*"* method's implementations
*include methods.
  METHOD all_files.

    APPEND LINES OF ms_merge-stree TO rt_files.
    APPEND LINES OF ms_merge-ttree TO rt_files.
    APPEND LINES OF ms_merge-ctree TO rt_files.
    SORT rt_files BY path DESCENDING name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rt_files COMPARING path name.

  ENDMETHOD.
  METHOD calculate_result.

    DATA: lt_files        TYPE Lif_abapgit_git_definitions=>ty_expanded_tt,
          lv_found_source TYPE abap_bool,
          lv_found_target TYPE abap_bool,
          lv_found_common TYPE abap_bool.

    FIELD-SYMBOLS: <ls_source>   LIKE LINE OF lt_files,
                   <ls_target>   LIKE LINE OF lt_files,
                   <ls_common>   LIKE LINE OF lt_files,
                   <ls_file>     LIKE LINE OF lt_files,
                   <ls_result>   LIKE LINE OF ms_merge-result,
                   <ls_object>   LIKE LINE OF mt_objects,
                   <ls_conflict> LIKE LINE OF mt_conflicts.

    lt_files = all_files( ).

    CREATE OBJECT ms_merge-stage
      EXPORTING
        iv_merge_source = ms_merge-source-sha1.

    LOOP AT lt_files ASSIGNING <ls_file>.

      UNASSIGN <ls_source>.
      UNASSIGN <ls_target>.
      UNASSIGN <ls_common>.

      READ TABLE ms_merge-stree ASSIGNING <ls_source>
        WITH KEY path_name
        COMPONENTS path = <ls_file>-path name = <ls_file>-name. "#EC CI_SUBRC
      READ TABLE ms_merge-ttree ASSIGNING <ls_target>
        WITH KEY path_name
        COMPONENTS path = <ls_file>-path name = <ls_file>-name. "#EC CI_SUBRC
      READ TABLE ms_merge-ctree ASSIGNING <ls_common>
        WITH KEY path_name
        COMPONENTS path = <ls_file>-path name = <ls_file>-name. "#EC CI_SUBRC

      lv_found_source = boolc( <ls_source> IS ASSIGNED ).
      lv_found_target = boolc( <ls_target> IS ASSIGNED ).
      lv_found_common = boolc( <ls_common> IS ASSIGNED ).

      IF lv_found_source = abap_false
          AND lv_found_target = abap_false.
* deleted in source and target, skip
        CONTINUE.
      ELSEIF lv_found_source = abap_false
          AND lv_found_common = abap_true
          AND <ls_target>-sha1 = <ls_common>-sha1.
* deleted in source, skip
        ms_merge-stage->rm( iv_path     = <ls_file>-path
                            iv_filename = <ls_file>-name ).
        CONTINUE.
      ELSEIF lv_found_target = abap_false
          AND lv_found_common = abap_true
          AND <ls_source>-sha1 = <ls_common>-sha1.
* deleted in target, skip
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO ms_merge-result ASSIGNING <ls_result>.
      <ls_result>-path = <ls_file>-path.
      <ls_result>-name = <ls_file>-name.

      IF lv_found_target = abap_false.
* added in source
        READ TABLE mt_objects ASSIGNING <ls_object>
          WITH KEY type COMPONENTS
            type = Lif_abapgit_git_definitions=>c_type-blob
            sha1 = <ls_source>-sha1.
        ASSERT sy-subrc = 0.

        ms_merge-stage->add( iv_path     = <ls_file>-path
                             iv_filename = <ls_file>-name
                             iv_data     = <ls_object>-data ).
        <ls_result>-sha1 = <ls_source>-sha1.
        CONTINUE.
      ELSEIF lv_found_source = abap_false.
* added in target
        <ls_result>-sha1 = <ls_target>-sha1.
      ELSEIF lv_found_common = abap_false
          AND <ls_target>-sha1 = <ls_source>-sha1.
* added in source and target
        <ls_result>-sha1 = <ls_source>-sha1.
      ELSEIF lv_found_common = abap_false
          AND <ls_target>-sha1 <> <ls_source>-sha1.

        INSERT INITIAL LINE INTO TABLE mt_conflicts ASSIGNING <ls_conflict>.
        <ls_conflict>-path = <ls_file>-path.
        <ls_conflict>-filename = <ls_file>-name.
        <ls_conflict>-source_sha1 = <ls_source>-sha1.
        READ TABLE mt_objects ASSIGNING <ls_object>
          WITH KEY type COMPONENTS
            type = Lif_abapgit_git_definitions=>c_type-blob
            sha1 = <ls_source>-sha1.
        <ls_conflict>-source_data = <ls_object>-data.

        <ls_conflict>-target_sha1 = <ls_target>-sha1.
        READ TABLE mt_objects ASSIGNING <ls_object>
          WITH KEY type COMPONENTS
            type = Lif_abapgit_git_definitions=>c_type-blob
            sha1 = <ls_target>-sha1.
        <ls_conflict>-target_data = <ls_object>-data.

* added in source and target, but different, merge conflict must be resolved
        ms_merge-conflict = |{ <ls_file>-name } merge conflict|.
        CONTINUE.
      ENDIF.

      IF lv_found_source = abap_false
          OR lv_found_target = abap_false
          OR lv_found_common = abap_false.
        ms_merge-conflict = |{ <ls_file>-name } merge conflict, not found anywhere|.
        CONTINUE.
      ENDIF.

      IF <ls_target>-sha1 = <ls_source>-sha1.
* target and source match
        <ls_result>-sha1 = <ls_source>-sha1.
      ELSEIF <ls_target>-sha1 = <ls_common>-sha1.
* changed in source
        READ TABLE mt_objects ASSIGNING <ls_object>
          WITH KEY type COMPONENTS
            type = Lif_abapgit_git_definitions=>c_type-blob
            sha1 = <ls_source>-sha1.
        ASSERT sy-subrc = 0.

        ms_merge-stage->add( iv_path     = <ls_file>-path
                             iv_filename = <ls_file>-name
                             iv_data     = <ls_object>-data ).
        <ls_result>-sha1 = <ls_source>-sha1.
      ELSEIF <ls_source>-sha1 = <ls_common>-sha1.
* changed in target
        <ls_result>-sha1 = <ls_target>-sha1.
      ELSE.
* changed in source and target, conflict
* conflict must be resolved before merge
        INSERT INITIAL LINE INTO TABLE mt_conflicts ASSIGNING <ls_conflict>.
        <ls_conflict>-path = <ls_file>-path.
        <ls_conflict>-filename = <ls_file>-name.
        <ls_conflict>-source_sha1 = <ls_source>-sha1.
        READ TABLE mt_objects ASSIGNING <ls_object>
          WITH KEY type COMPONENTS
            type = Lif_abapgit_git_definitions=>c_type-blob
            sha1 = <ls_source>-sha1.
        <ls_conflict>-source_data = <ls_object>-data.

        <ls_conflict>-target_sha1 = <ls_target>-sha1.
        READ TABLE mt_objects ASSIGNING <ls_object>
          WITH KEY type COMPONENTS
            type = Lif_abapgit_git_definitions=>c_type-blob
            sha1 = <ls_target>-sha1.
        <ls_conflict>-target_data = <ls_object>-data.

        ms_merge-conflict = |{ <ls_file>-name } merge conflict, changed in source and target branch|.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD constructor.

    IF iv_source_branch = io_repo->get_selected_branch( ).
      Lcx_abapgit_exception=>raise( 'source = target' ).
    ENDIF.

    mo_repo = io_repo.
    mv_source_branch = iv_source_branch.

  ENDMETHOD.
  METHOD fetch_git.

    DATA: lo_branch_list TYPE REF TO Lcl_abapgit_git_branch_list,
          lt_upload      TYPE Lif_abapgit_git_definitions=>ty_git_branch_list_tt.

    lo_branch_list = Lcl_abapgit_git_transport=>branches( ms_merge-repo->get_url( ) ).

    ms_merge-source = lo_branch_list->find_by_name(
      Lcl_abapgit_git_branch_list=>complete_heads_branch_name( mv_source_branch ) ).

    ms_merge-target = lo_branch_list->find_by_name(
      Lcl_abapgit_git_branch_list=>complete_heads_branch_name( mo_repo->get_selected_branch( ) ) ).

    APPEND ms_merge-source TO lt_upload.
    APPEND ms_merge-target TO lt_upload.

    Lcl_abapgit_git_transport=>upload_pack_by_branch(
      EXPORTING
        iv_url          = ms_merge-repo->get_url( )
        iv_branch_name  = ms_merge-repo->get_selected_branch( )
        iv_deepen_level = 0
        it_branches     = lt_upload
      IMPORTING
        et_objects      = rt_objects ).

  ENDMETHOD.
  METHOD find_ancestors.

    DATA: ls_commit TYPE Lcl_abapgit_git_pack=>ty_commit,
          lt_visit  TYPE ty_visit_tt,
          lv_commit LIKE LINE OF lt_visit.

    FIELD-SYMBOLS: <ls_ancestor> LIKE LINE OF rt_ancestors,
                   <ls_object>   LIKE LINE OF mt_objects.


    APPEND iv_commit TO lt_visit.

    LOOP AT lt_visit INTO lv_commit.
      READ TABLE mt_objects ASSIGNING <ls_object>
        WITH KEY type COMPONENTS
          type = Lif_abapgit_git_definitions=>c_type-commit
          sha1 = lv_commit.
      ASSERT sy-subrc = 0.

      ls_commit = Lcl_abapgit_git_pack=>decode_commit( <ls_object>-data ).

      visit( EXPORTING iv_parent = ls_commit-parent
             CHANGING ct_visit   = lt_visit ).
      visit( EXPORTING iv_parent = ls_commit-parent2
             CHANGING ct_visit   = lt_visit ).

      APPEND INITIAL LINE TO rt_ancestors ASSIGNING <ls_ancestor>.
      <ls_ancestor>-commit = lv_commit.
      <ls_ancestor>-tree = ls_commit-tree.
      <ls_ancestor>-body = ls_commit-body.
      <ls_ancestor>-time = ls_commit-author.

      "Strip Author entry of all but the time component
      REPLACE ALL OCCURRENCES OF REGEX '[a-zA-Z<>@.-]*' IN <ls_ancestor>-time WITH ''.
      CONDENSE <ls_ancestor>-time.
    ENDLOOP.

    SORT rt_ancestors BY time DESCENDING.

  ENDMETHOD.
  METHOD find_first_common.

    FIELD-SYMBOLS: <ls_list1> LIKE LINE OF it_list1,
                   <ls_list2> LIKE LINE OF it_list2.

    LOOP AT it_list1 ASSIGNING <ls_list1>.
      LOOP AT it_list2 ASSIGNING <ls_list2>.
        IF <ls_list1>-tree = <ls_list2>-tree.
          rs_common = <ls_list1>.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    Lcx_abapgit_exception=>raise( 'error finding common ancestor' ).

  ENDMETHOD.
  METHOD visit.

    IF NOT iv_parent IS INITIAL.
      READ TABLE ct_visit FROM iv_parent TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND iv_parent TO ct_visit.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_merge~get_conflicts.

    rt_conflicts = mt_conflicts.

  ENDMETHOD.
  METHOD Lif_abapgit_merge~get_result.

    rs_merge = ms_merge.

  ENDMETHOD.
  METHOD Lif_abapgit_merge~get_source_branch.

    rv_source_branch = mv_source_branch.

  ENDMETHOD.
  METHOD Lif_abapgit_merge~has_conflicts.

    rv_conflicts_exists = boolc( lines( mt_conflicts ) > 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_merge~resolve_conflict.

    FIELD-SYMBOLS: <ls_conflict> TYPE Lif_abapgit_merge=>ty_merge_conflict,
                   <ls_result>   LIKE LINE OF ms_merge-result.

    IF is_conflict-result_sha1 IS NOT INITIAL
        AND is_conflict-result_data IS NOT INITIAL.
      READ TABLE mt_conflicts ASSIGNING <ls_conflict> WITH KEY path = is_conflict-path
                                                               filename = is_conflict-filename.
      IF sy-subrc = 0.
        READ TABLE ms_merge-result ASSIGNING <ls_result>
          WITH KEY path_name
          COMPONENTS path = is_conflict-path name = is_conflict-filename.
        IF sy-subrc = 0.
          <ls_result>-sha1 = is_conflict-result_sha1.

          ms_merge-stage->add( iv_path     = <ls_conflict>-path
                               iv_filename = <ls_conflict>-filename
                               iv_data     = is_conflict-result_data ).

          DELETE mt_conflicts WHERE path     = is_conflict-path
                                AND filename = is_conflict-filename.
        ENDIF.

        READ TABLE ms_merge-result ASSIGNING <ls_result> WITH KEY sha1 = space.
        IF sy-subrc = 0.
          ms_merge-conflict = |{ <ls_result>-name } merge conflict, changed in source and target branch|.
        ELSE.
          CLEAR ms_merge-conflict.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_merge~run.

    DATA: lt_asource TYPE ty_ancestor_tt,
          lt_atarget TYPE ty_ancestor_tt.

    CLEAR: ms_merge, mt_objects, mt_conflicts.

    ms_merge-repo = mo_repo.
    mt_objects = fetch_git( ).

    lt_asource = find_ancestors( ms_merge-source-sha1 ).
    lt_atarget = find_ancestors( ms_merge-target-sha1 ).

    ms_merge-common = find_first_common( it_list1 = lt_asource
                                         it_list2 = lt_atarget ).

    ms_merge-stree = Lcl_abapgit_git_porcelain=>full_tree(
      it_objects = mt_objects
      iv_parent  = ms_merge-source-sha1 ).
    ms_merge-ttree = Lcl_abapgit_git_porcelain=>full_tree(
      it_objects = mt_objects
      iv_parent  = ms_merge-target-sha1 ).
    ms_merge-ctree = Lcl_abapgit_git_porcelain=>full_tree(
      it_objects = mt_objects
      iv_parent  = ms_merge-common-commit ).

    calculate_result( ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_MERGE implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_HOC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_hoc======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_hoc======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_HOC implementation.
*"* method's implementations
*include methods.
  METHOD create.

    DATA lo_page TYPE REF TO Lcl_abapgit_gui_page_hoc.
    DATA ls_control TYPE Lcl_abapgit_gui_page=>ty_control.

    ls_control-page_title          = iv_page_title.
    ls_control-page_layout         = iv_page_layout.
    ls_control-page_menu           = io_page_menu.
    ls_control-page_menu_provider  = ii_page_menu_provider.
    ls_control-page_title_provider = ii_page_title_provider.
    ls_control-extra_css_url       = iv_extra_css_url.
    ls_control-extra_js_url        = iv_extra_js_url.
    ls_control-show_as_modal       = iv_show_as_modal.

    CREATE OBJECT lo_page
      EXPORTING
        ii_child_component = ii_child_component
        is_control         = ls_control.

    ri_page_wrap = lo_page.

  ENDMETHOD.
  METHOD get_child.
    ri_child = mi_child.
  ENDMETHOD.
  METHOD render_content.

    IF mi_child IS BOUND.
      ri_html = mi_child->render( ).
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    super->constructor( ).

    mi_child = ii_child_component.
    ms_control = is_control.

    IF ms_control-show_as_modal = abap_false.
      ms_control-show_as_modal = detect_modal( ).
    ENDIF.

    IF ms_control-page_menu_provider IS NOT BOUND.
      ms_control-page_menu_provider = detect_menu_provider( ).
    ENDIF.

    IF ms_control-page_title_provider IS NOT BOUND.
      ms_control-page_title_provider = detect_title_provider( ).
    ENDIF.

  ENDMETHOD.
  METHOD detect_menu_provider.
    TRY.
        ri_ref ?= mi_child.
      CATCH cx_sy_move_cast_error.
    ENDTRY.
  ENDMETHOD.
  METHOD detect_modal.

    DATA li_modal TYPE REF TO Lif_abapgit_gui_modal.

    TRY.
        li_modal ?= mi_child.
        rv_is_modal = li_modal->is_modal( ).
      CATCH cx_sy_move_cast_error.
    ENDTRY.

  ENDMETHOD.
  METHOD detect_title_provider.
    TRY.
        ri_ref ?= mi_child.
      CATCH cx_sy_move_cast_error.
    ENDTRY.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_HOC implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_NROB <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_nrob=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_nrob=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_NROB implementation.
*"* method's implementations
*include methods.
  METHOD delete_intervals.

    DATA: lv_error    TYPE c LENGTH 1,
          ls_error    TYPE inrer,
          lt_list     TYPE STANDARD TABLE OF inriv WITH DEFAULT KEY,
          lt_error_iv TYPE STANDARD TABLE OF inriv WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF lt_list.


    CALL FUNCTION 'NUMBER_RANGE_INTERVAL_LIST'
      EXPORTING
        object                     = iv_object
      TABLES
        interval                   = lt_list
      EXCEPTIONS
        nr_range_nr1_not_found     = 1
        nr_range_nr1_not_intern    = 2
        nr_range_nr2_must_be_space = 3
        nr_range_nr2_not_extern    = 4
        nr_range_nr2_not_found     = 5
        object_not_found           = 6
        subobject_must_be_space    = 7
        subobject_not_found        = 8
        OTHERS                     = 9.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF lines( lt_list ) = 0.
      RETURN.
    ENDIF.

    LOOP AT lt_list ASSIGNING <ls_list>.
      CLEAR <ls_list>-nrlevel.
      <ls_list>-procind = 'D'.
    ENDLOOP.

    CALL FUNCTION 'NUMBER_RANGE_INTERVAL_UPDATE'
      EXPORTING
        object           = iv_object
      IMPORTING
        error            = ls_error
        error_occured    = lv_error
      TABLES
        error_iv         = lt_error_iv
        interval         = lt_list
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0 OR lv_error = abap_true.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'NUMBER_RANGE_UPDATE_CLOSE'
      EXPORTING
        object                 = iv_object
      EXCEPTIONS
        no_changes_made        = 1
        object_not_initialized = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lv_objectid TYPE cdhdr-objectid,
          lt_cdhdr    TYPE cdhdr_tab.

    FIELD-SYMBOLS: <ls_cdhdr> LIKE LINE OF lt_cdhdr.


    lv_objectid = ms_item-obj_name.

    CALL FUNCTION 'CHANGEDOCUMENT_READ_HEADERS'
      EXPORTING
        objectclass                = 'NRKROBJ'
        objectid                   = lv_objectid
      TABLES
        i_cdhdr                    = lt_cdhdr
      EXCEPTIONS
        no_position_found          = 1
        wrong_access_to_archive    = 2
        time_zone_conversion_error = 3
        OTHERS                     = 4.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
      RETURN.
    ENDIF.

    SORT lt_cdhdr BY udate DESCENDING utime DESCENDING.

    READ TABLE lt_cdhdr INDEX 1 ASSIGNING <ls_cdhdr>.
    ASSERT sy-subrc = 0.

    rv_user = <ls_cdhdr>-username.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_object TYPE tnro-object.


    lv_object = ms_item-obj_name.

    delete_intervals( lv_object ).

    CALL FUNCTION 'NUMBER_RANGE_OBJECT_DELETE'
      EXPORTING
        language           = mv_language
        object             = lv_object
      EXCEPTIONS
        delete_not_allowed = 1
        object_not_found   = 2
        wrong_indicator    = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lt_errors     TYPE TABLE OF inoer,
          ls_attributes TYPE tnro,
          ls_text       TYPE tnrot.

    FIELD-SYMBOLS <lv_any> TYPE any.

    io_xml->read( EXPORTING iv_name = 'ATTRIBUTES'
                  CHANGING cg_data = ls_attributes ).
    io_xml->read( EXPORTING iv_name = 'TEXT'
                  CHANGING cg_data = ls_text ).

    ASSIGN COMPONENT 'CHANGED_AT' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      GET TIME STAMP FIELD <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-uname.
    ENDIF.
    ASSIGN COMPONENT 'ENAME' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-uname.
    ENDIF.
    ASSIGN COMPONENT 'EDATE' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-datum.
    ENDIF.
    ASSIGN COMPONENT 'ETIME' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-uzeit.
    ENDIF.

    ASSIGN COMPONENT 'UNAME' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-uname.
    ENDIF.
    ASSIGN COMPONENT 'UDATE' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-datum.
    ENDIF.
    ASSIGN COMPONENT 'UTIME' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-uzeit.
    ENDIF.
    ASSIGN COMPONENT 'ENAME' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-uname.
    ENDIF.
    ASSIGN COMPONENT 'EDATE' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-datum.
    ENDIF.
    ASSIGN COMPONENT 'ETIME' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      <lv_any> = sy-uzeit.
    ENDIF.

    CALL FUNCTION 'NUMBER_RANGE_OBJECT_UPDATE'
      EXPORTING
        indicator                 = 'I'
        object_attributes         = ls_attributes
        object_text               = ls_text
      TABLES
        errors                    = lt_errors
      EXCEPTIONS
        object_already_exists     = 1
        object_attributes_missing = 2
        object_not_found          = 3
        object_text_missing       = 4
        wrong_indicator           = 5
        OTHERS                    = 6.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    tadir_insert( iv_package ).
    corr_insert( iv_package ).

    CALL FUNCTION 'NUMBER_RANGE_OBJECT_CLOSE'
      EXPORTING
        object                 = ls_attributes-object
      EXCEPTIONS
        object_not_initialized = 1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_object TYPE tnro-object.


    SELECT SINGLE object FROM tnro INTO lv_object
      WHERE object = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-late TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: ls_bcdata TYPE bdcdata,
          lt_bcdata TYPE STANDARD TABLE OF bdcdata.

    ls_bcdata-program  = 'SAPMSNRO'.
    ls_bcdata-dynpro   = '0150'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam     = 'NRIV-OBJECT'.
    ls_bcdata-fval     = ms_item-obj_name.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=DISP'.
    APPEND ls_bcdata TO lt_bcdata.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SNRO'
      it_bdcdata = lt_bcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_object     TYPE tnro-object,
          ls_attributes TYPE tnro,
          ls_text       TYPE tnrot.

    FIELD-SYMBOLS <lv_any> TYPE any.


    lv_object = ms_item-obj_name.

    CALL FUNCTION 'NUMBER_RANGE_OBJECT_READ'
      EXPORTING
        language          = mv_language
        object            = lv_object
      IMPORTING
        object_attributes = ls_attributes
        object_text       = ls_text
      EXCEPTIONS
        object_not_found  = 1
        OTHERS            = 2.
    IF sy-subrc = 1.
      RETURN.
    ELSEIF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    ASSIGN COMPONENT 'CHANGED_AT' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'ENAME' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'EDATE' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'ETIME' OF STRUCTURE ls_attributes TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.

    ASSIGN COMPONENT 'UNAME' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'UDATE' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'UTIME' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'ENAME' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'EDATE' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.
    ASSIGN COMPONENT 'ETIME' OF STRUCTURE ls_text TO <lv_any>.
    IF sy-subrc = 0.
      CLEAR <lv_any>.
    ENDIF.

    io_xml->add( iv_name = 'ATTRIBUTES'
                 ig_data = ls_attributes ).
    io_xml->add( iv_name = 'TEXT'
                 ig_data = ls_text ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_NROB implementation

*>>>>>>> ZCL_ABAPGIT_OBJECTS_CI_TESTS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_objects_ci_tests==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_objects_ci_tests==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations



*>>>>>>> ZCL_ABAPGIT_OBJECTS_FILES <<<<<<<*

*"* macro definitions
*include zcl_abapgit_objects_files=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_objects_files=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_objects_files=====ccau.


class LCL_ABAPGIT_OBJECTS_FILES implementation.
*"* method's implementations
*include methods.
  METHOD add.
    APPEND is_file TO mt_files.
  ENDMETHOD.
  METHOD add_abap.

    DATA: lv_source TYPE string,
          ls_file   TYPE Lif_abapgit_git_definitions=>ty_file.


    CONCATENATE LINES OF it_abap INTO lv_source SEPARATED BY cl_abap_char_utilities=>newline.
* when editing files via eg. GitHub web interface it adds a newline at end of file
    lv_source = lv_source && cl_abap_char_utilities=>newline.

    ls_file-path = '/'.
    ls_file-filename = Lcl_abapgit_filename_logic=>object_to_file(
      is_item  = ms_item
      iv_extra = iv_extra
      iv_ext   = 'abap' ).
    ls_file-data = Lcl_abapgit_convert=>string_to_xstring_utf8( lv_source ).

    APPEND ls_file TO mt_files.

  ENDMETHOD.
  METHOD add_raw.

    DATA: ls_file TYPE Lif_abapgit_git_definitions=>ty_file.

    ls_file-path     = '/'.
    ls_file-data     = iv_data.
    ls_file-filename = Lcl_abapgit_filename_logic=>object_to_file(
      is_item  = ms_item
      iv_extra = iv_extra
      iv_ext   = iv_ext ).

    APPEND ls_file TO mt_files.

  ENDMETHOD.
  METHOD add_string.

    DATA: ls_file TYPE Lif_abapgit_git_definitions=>ty_file.


    ls_file-path = '/'.
    ls_file-filename = Lcl_abapgit_filename_logic=>object_to_file(
      is_item  = ms_item
      iv_extra = iv_extra
      iv_ext   = iv_ext ).
    ls_file-data = Lcl_abapgit_convert=>string_to_xstring_utf8( iv_string ).

    APPEND ls_file TO mt_files.

  ENDMETHOD.
  METHOD add_xml.

    DATA: lv_xml  TYPE string,
          ls_file TYPE Lif_abapgit_git_definitions=>ty_file.

    lv_xml = ii_xml->render( iv_normalize = iv_normalize
                             is_metadata = is_metadata ).
    ls_file-path = '/'.

    ls_file-filename = Lcl_abapgit_filename_logic=>object_to_file(
      is_item  = ms_item
      iv_extra = iv_extra
      iv_ext   = 'xml' ).

    REPLACE FIRST OCCURRENCE
      OF REGEX '<\?xml version="1\.0" encoding="[\w-]+"\?>'
      IN lv_xml
      WITH '<?xml version="1.0" encoding="utf-8"?>'.
    ASSERT sy-subrc = 0.

    ls_file-data = Lcl_abapgit_convert=>string_to_xstring_utf8_bom( lv_xml ).

    APPEND ls_file TO mt_files.
  ENDMETHOD.
  METHOD constructor.
    ms_item = is_item.
    mv_path = iv_path.
  ENDMETHOD.
  METHOD contains_file.
    DATA: lv_filename TYPE string.

    lv_filename = Lcl_abapgit_filename_logic=>object_to_file(
      is_item  = ms_item
      iv_extra = iv_extra
      iv_ext   = iv_ext ).

    IF mv_path IS NOT INITIAL.
      READ TABLE mt_files TRANSPORTING NO FIELDS
          WITH KEY file_path
          COMPONENTS path     = mv_path
                     filename = lv_filename.
    ELSE.
      READ TABLE mt_files TRANSPORTING NO FIELDS
          WITH KEY file
          COMPONENTS filename = lv_filename.
    ENDIF.

    IF sy-subrc = 0.
      rv_present = abap_true.
    ENDIF.
  ENDMETHOD.
  METHOD get_accessed_files.
    rt_files = mt_accessed_files.
  ENDMETHOD.
  METHOD get_files.
    rt_files = mt_files.
  ENDMETHOD.
  METHOD get_file_pattern.
    rv_pattern = Lcl_abapgit_filename_logic=>object_to_file(
      is_item  = ms_item
      iv_ext   = '*' ).
    " Escape special characters for use with 'covers pattern' (CP)
    REPLACE ALL OCCURRENCES OF '#' IN rv_pattern WITH '##'.
    REPLACE ALL OCCURRENCES OF '+' IN rv_pattern WITH '#+'.
  ENDMETHOD.
  METHOD is_json_metadata.

    DATA lv_pattern TYPE string.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF mt_files.

    lv_pattern = |*.{ to_lower( ms_item-obj_type ) }.json|.

    LOOP AT mt_files ASSIGNING <ls_file> WHERE filename CP lv_pattern.
      rv_result = abap_true.
      EXIT.
    ENDLOOP.

  ENDMETHOD.
  METHOD read_abap.

    DATA: lv_filename TYPE string,
          lv_data     TYPE xstring,
          lv_abap     TYPE string.


    lv_filename = Lcl_abapgit_filename_logic=>object_to_file(
      is_item  = ms_item
      iv_extra = iv_extra
      iv_ext   = 'abap' ).

    lv_data = read_file( iv_filename = lv_filename
                         iv_error    = iv_error ).

    IF lv_data IS INITIAL. " Post-handling of iv_error = false
      RETURN.
    ENDIF.

    lv_abap = Lcl_abapgit_convert=>xstring_to_string_utf8( lv_data ).

    SPLIT lv_abap AT cl_abap_char_utilities=>newline INTO TABLE rt_abap.

  ENDMETHOD.
  METHOD read_file.

    FIELD-SYMBOLS <ls_file>     LIKE LINE OF mt_files.

    IF mv_path IS NOT INITIAL.
      READ TABLE mt_files ASSIGNING <ls_file>
          WITH KEY file_path
          COMPONENTS path     = mv_path
                     filename = iv_filename.
    ELSE.
      READ TABLE mt_files ASSIGNING <ls_file>
          WITH KEY file
          COMPONENTS filename = iv_filename.
    ENDIF.

    IF sy-subrc <> 0.
      IF iv_error = abap_true.
        Lcx_abapgit_exception=>raise( |File not found: { iv_filename }| ).
      ELSE.
        RETURN.
      ENDIF.
    ENDIF.

    " Update access table
    mark_accessed(
      iv_path = <ls_file>-path
      iv_file = <ls_file>-filename
      iv_sha1 = <ls_file>-sha1 ).

    rv_data = <ls_file>-data.

  ENDMETHOD.
  METHOD read_raw.

    DATA: lv_filename TYPE string.

    lv_filename = Lcl_abapgit_filename_logic=>object_to_file(
      is_item  = ms_item
      iv_extra = iv_extra
      iv_ext   = iv_ext ).

    rv_data = read_file( lv_filename ).

  ENDMETHOD.
  METHOD read_string.

    DATA: lv_filename TYPE string,
          lv_data     TYPE xstring.

    lv_filename = Lcl_abapgit_filename_logic=>object_to_file(
      is_item  = ms_item
      iv_extra = iv_extra
      iv_ext   = iv_ext ).

    lv_data = read_file( lv_filename ).

    rv_string = Lcl_abapgit_convert=>xstring_to_string_utf8( lv_data ).

  ENDMETHOD.
  METHOD read_xml.

    DATA: lv_filename TYPE string,
          lv_data     TYPE xstring,
          lv_xml      TYPE string.

    lv_filename = Lcl_abapgit_filename_logic=>object_to_file(
      is_item  = ms_item
      iv_extra = iv_extra
      iv_ext   = 'xml' ).

    lv_data = read_file( lv_filename ).

    lv_xml = Lcl_abapgit_convert=>xstring_to_string_utf8( lv_data ).

    CREATE OBJECT ri_xml
      TYPE Lcl_abapgit_xml_input
      EXPORTING
        iv_xml      = lv_xml
        iv_filename = lv_filename.

  ENDMETHOD.
  METHOD set_files.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF it_files.

    CLEAR mt_files.

    " Set only files matching the pattern for this object
    " If a path has been defined in the constructor, then the path has to match, too
    LOOP AT it_files ASSIGNING <ls_file> WHERE filename CP get_file_pattern( ).
      IF mv_path IS INITIAL.
        INSERT <ls_file> INTO TABLE mt_files.
      ELSEIF mv_path = <ls_file>-path.
        INSERT <ls_file> INTO TABLE mt_files.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD add_i18n_file.

    DATA ls_file TYPE Lif_abapgit_git_definitions=>ty_file.

    ls_file-data = ii_i18n_file->render( ).
    IF ls_file-data IS INITIAL.
      RETURN. " Don't add empty files
    ENDIF.

    ls_file-path     = '/'.
    ls_file-filename = Lcl_abapgit_filename_logic=>object_to_file(
      is_item  = ms_item
      iv_extra = |i18n.{ ii_i18n_file->lang( ) }|
      iv_ext   = ii_i18n_file->ext( ) ).

    APPEND ls_file TO mt_files.

  ENDMETHOD.
  METHOD mark_accessed.

    FIELD-SYMBOLS <ls_accessed> LIKE LINE OF mt_accessed_files.

    READ TABLE mt_accessed_files TRANSPORTING NO FIELDS
      WITH KEY path = iv_path filename = iv_file.
    IF sy-subrc > 0. " Not found ? -> Add
      APPEND INITIAL LINE TO mt_accessed_files ASSIGNING <ls_accessed>.
      <ls_accessed>-path     = iv_path.
      <ls_accessed>-filename = iv_file.
      <ls_accessed>-sha1     = iv_sha1.
    ENDIF.

  ENDMETHOD.
  METHOD read_i18n_files.

    DATA lv_lang TYPE laiso.
    DATA lv_ext TYPE string.
    DATA lo_po TYPE REF TO Lcl_abapgit_po_file.
    FIELD-SYMBOLS <ls_file> LIKE LINE OF mt_files.

    LOOP AT mt_files ASSIGNING <ls_file>.

      " TODO: Maybe this should be in zcl_abapgit_filename_logic
      FIND FIRST OCCURRENCE OF REGEX 'i18n\.([^.]{2})\.([^.]+)$' IN <ls_file>-filename SUBMATCHES lv_lang lv_ext.
      CHECK sy-subrc = 0.

      CASE lv_ext.
        WHEN 'po'.
          CREATE OBJECT lo_po EXPORTING iv_lang = lv_lang.
          lo_po->parse( <ls_file>-data ).
          APPEND lo_po TO rt_i18n_files.
        WHEN OTHERS.
          CONTINUE. " Unsupported i18n file type
      ENDCASE.

      mark_accessed(
        iv_path = <ls_file>-path
        iv_file = <ls_file>-filename
        iv_sha1 = <ls_file>-sha1 ).

    ENDLOOP.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECTS_FILES implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SHI3 <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_shi3=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_shi3=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SHI3 implementation.
*"* method's implementations
*include methods.
  METHOD clear_fields.

    FIELD-SYMBOLS <ls_node> LIKE LINE OF ct_nodes.

    CLEAR: cs_head-luser, cs_head-ldate, cs_head-ltime.
    CLEAR: cs_head-fuser, cs_head-fdate, cs_head-ftime.
    CLEAR: cs_head-frelease, cs_head-lrelease.
    CLEAR: cs_head-responsibl.

    LOOP AT ct_nodes ASSIGNING <ls_node>.
      CLEAR: <ls_node>-luser, <ls_node>-ldate, <ls_node>-ltime.
      CLEAR: <ls_node>-fuser, <ls_node>-fdate, <ls_node>-ftime.
      CLEAR: <ls_node>-frelease, <ls_node>-lrelease.
    ENDLOOP.

  ENDMETHOD.
  METHOD constructor.
    super->constructor( is_item = is_item
                        iv_language = iv_language ).
    mv_tree_id = ms_item-obj_name.
  ENDMETHOD.
  METHOD delete_tree_structure.
    CALL FUNCTION 'STREE_EXTERNAL_DELETE'
      EXPORTING
        structure_id          = iv_structure_id
        no_confirmation_popup = abap_true.
  ENDMETHOD.
  METHOD has_authorization.

    AUTHORITY-CHECK OBJECT 'S_DEVELOP'
      ID 'DEVCLASS'  FIELD iv_devclass
      ID 'OBJTYPE'   FIELD 'MENU'
      ID 'OBJNAME'   FIELD iv_structure_id
      ID 'P_GROUP'   DUMMY
      ID 'ACTVT'     FIELD iv_activity.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( iv_msgid = 'S#'
                                         iv_msgno = '203' ).
    ENDIF.
  ENDMETHOD.
  METHOD insert_transport.

    DATA:
      ls_msg     TYPE hier_mess,
      ls_object  TYPE e071,
      lt_objects TYPE TABLE OF e071,
      lt_keys    TYPE TABLE OF e071k,
      ls_ko200   TYPE ko200,
      lt_ko200   TYPE TABLE OF ko200.

    " This function shows a popup so get objects and keys and insert
    " them into transport below
    CALL FUNCTION 'STREE_INSERT_ALL_IN_TRANSPORT'
      EXPORTING
        structure_id               = mv_tree_id
        iv_return_objects_and_keys = abap_true
      IMPORTING
        message                    = ls_msg
      TABLES
        et_objects                 = lt_objects
        et_keys                    = lt_keys.
    IF ls_msg-msgty = 'E'.
      MESSAGE ID ls_msg-msgid TYPE ls_msg-msgty NUMBER ls_msg-msgno
        WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4 INTO Lcx_abapgit_exception=>null.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    LOOP AT lt_objects INTO ls_object.
      MOVE-CORRESPONDING ls_object TO ls_ko200.
      INSERT ls_ko200 INTO TABLE lt_ko200.
    ENDLOOP.

    CALL FUNCTION 'TR_RECORD_OBJ_CHANGE_TO_REQ'
      EXPORTING
        iv_request = iv_transport
        it_objects = lt_ko200
        it_keys    = lt_keys
      EXCEPTIONS
        cancel     = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD is_used.

    DATA: lt_used_in_structures TYPE STANDARD TABLE OF ttree WITH DEFAULT KEY.

    CALL FUNCTION 'STREE_GET_STRUCTURE_USAGE'
      EXPORTING
        structure_id       = iv_structure_id
      TABLES
        used_in_structures = lt_used_in_structures.

    IF lt_used_in_structures IS NOT INITIAL.
      Lcx_abapgit_exception=>raise( |IMG structure ID { iv_structure_id } is still used| ).
    ENDIF.

  ENDMETHOD.
  METHOD jump_sbach04.
    DATA: ls_message      TYPE hier_mess,
          lv_structure_id TYPE hier_treeg.

    lv_structure_id = ms_item-obj_name.

    CALL FUNCTION 'STREE_EXTERNAL_EDIT'
      EXPORTING
        structure_id   = lv_structure_id
        language       = mv_language
        edit_structure = abap_false
        no_commit_work = abap_false
        activity       = 'D'
      IMPORTING
        message        = ls_message.
    IF ls_message IS NOT INITIAL.
      Lcx_abapgit_exception=>raise_t100(
        iv_msgid = ls_message-msgid
        iv_msgno = ls_message-msgno
        iv_msgv1 = ls_message-msgv1
        iv_msgv2 = ls_message-msgv2
        iv_msgv3 = ls_message-msgv3
        iv_msgv4 = ls_message-msgv4 ).
    ENDIF.
  ENDMETHOD.
  METHOD jump_se43.

    DATA: lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPLBMEN'.
    <ls_bdcdata>-dynpro   = '0200'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=SHOW'.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BMENUNAME-ID'.
    <ls_bdcdata>-fval = ms_item-obj_name.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SE43'
      it_bdcdata = lt_bdcdata ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: ls_head TYPE ttree.

    CALL FUNCTION 'STREE_STRUCTURE_READ'
      EXPORTING
        structure_id     = mv_tree_id
      IMPORTING
        structure_header = ls_head.

    rv_user = ls_head-luser.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    CONSTANTS lc_activity_delete_06 TYPE activ_auth VALUE '06'.

    TRY.
        IF Lif_abapgit_object~exists( ) = abap_false.
          RETURN.
        ENDIF.
      CATCH Lcx_abapgit_exception.
        RETURN.
    ENDTRY.

    has_authorization( iv_structure_id = mv_tree_id
                       iv_devclass     = ms_item-devclass
                       iv_activity     = lc_activity_delete_06 ).

    is_used( mv_tree_id ).

    delete_tree_structure( mv_tree_id ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_msg    TYPE hier_mess,
          ls_head   TYPE ttree,
          ls_ttree  TYPE ttree,
          lt_titles TYPE TABLE OF ttreet,
          lt_nodes  TYPE TABLE OF hier_iface,
          lt_texts  TYPE TABLE OF hier_texts,
          lt_refs   TYPE TABLE OF hier_ref.

    io_xml->read( EXPORTING iv_name = 'TREE_HEAD'
                  CHANGING  cg_data = ls_head ).
    io_xml->read( EXPORTING iv_name = 'TREE_TITLES'
                  CHANGING  cg_data = lt_titles ).
    io_xml->read( EXPORTING iv_name = 'TREE_NODES'
                  CHANGING  cg_data = lt_nodes ).
    io_xml->read( EXPORTING iv_name = 'TREE_REFS'
                  CHANGING  cg_data = lt_refs ).
    io_xml->read( EXPORTING iv_name = 'TREE_TEXTS'
                  CHANGING  cg_data = lt_texts ).

    mo_i18n_params->trim_saplang_keyed_table(
      EXPORTING
        iv_lang_field_name = 'SPRAS'
        iv_keep_master_lang = abap_true
      CHANGING
        ct_tab = lt_titles ).
    mo_i18n_params->trim_saplang_keyed_table(
      EXPORTING
        iv_lang_field_name = 'SPRAS'
        iv_keep_master_lang = abap_true
      CHANGING
        ct_tab = lt_texts ).

    IF Lif_abapgit_object~exists( ) = abap_true.
      delete_tree_structure( mv_tree_id ).
    ENDIF.

    CALL FUNCTION 'STREE_HIERARCHY_SAVE'
      EXPORTING
        structure_id             = mv_tree_id
        structure_type           = ls_head-type
        structure_description    = space
        structure_masterlanguage = mv_language
        structure_responsible    = sy-uname
        structure_buffermode     = ls_head-buffermode
        development_class        = iv_package
      IMPORTING
        message                  = ls_msg
      TABLES
        list_of_nodes            = lt_nodes
        list_of_references       = lt_refs
        list_of_texts            = lt_texts
        structure_descriptions   = lt_titles
      EXCEPTIONS
        no_nodes_given           = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ELSEIF ls_msg-msgty = 'E'.
      MESSAGE ID ls_msg-msgid TYPE ls_msg-msgty NUMBER ls_msg-msgno
        WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4 INTO Lcx_abapgit_exception=>null.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    " Set buffer mode for menus (see function BMENU_CREATE_TREE)
    SELECT SINGLE * FROM ttree INTO ls_ttree
      WHERE type = 'BMENU' AND id = mv_tree_id.
    IF sy-subrc = 0.
      ls_ttree-buffermode = ls_head-buffermode.
      ls_ttree-buffervar  = ls_head-buffervar.
      MODIFY ttree FROM ls_ttree.
    ENDIF.

    IF Lcl_abapgit_factory=>get_sap_package( iv_package )->are_changes_recorded_in_tr_req( ) = abap_true.
      " Add necessary SHI6, SHI7, and TABU entries to transport (SAP Note 455542)
      insert_transport( iv_transport ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: ls_msg    TYPE hier_mess,
          ls_header TYPE ttree,
          ls_tadir  TYPE tadir.

    " Ignore buffer and get state from DB
    CALL FUNCTION 'STREE_STRUCTURE_EXIST'
      EXPORTING
        structure_id         = mv_tree_id
        read_from_database   = abap_true
        do_not_read_devclass = abap_false
      IMPORTING
        message              = ls_msg
        structure_header     = ls_header
        structure_tadir      = ls_tadir.

    rv_bool = boolc( ls_header-id IS NOT INITIAL ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: ls_head TYPE ttree.

    CALL FUNCTION 'STREE_STRUCTURE_READ'
      EXPORTING
        structure_id     = mv_tree_id
      IMPORTING
        structure_header = ls_head.

    CASE ls_head-type.
      WHEN 'BMENU'.
        jump_se43( ).
        rv_exit = abap_true.
      WHEN 'GHIER'.
        jump_sbach04( ).
        rv_exit = abap_true.
    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_msg           TYPE hier_mess,
          ls_head          TYPE ttree,
          lt_titles        TYPE TABLE OF ttreet,
          lt_nodes         TYPE TABLE OF hier_iface,
          lt_texts         TYPE TABLE OF hier_texts,
          lt_refs          TYPE TABLE OF hier_ref,
          lv_all_languages TYPE abap_bool.


    CALL FUNCTION 'STREE_STRUCTURE_READ'
      EXPORTING
        structure_id     = mv_tree_id
      IMPORTING
        message          = ls_msg
        structure_header = ls_head
      TABLES
        description      = lt_titles.

    IF mo_i18n_params->ms_params-main_language_only = abap_true OR mo_i18n_params->is_lxe_applicable( ) = abap_true.
      lv_all_languages = abap_false.
      DELETE lt_titles WHERE spras <> mv_language.
    ELSE.
      lv_all_languages = abap_true.
      mo_i18n_params->trim_saplang_keyed_table(
        EXPORTING
            iv_lang_field_name = 'SPRAS'
            iv_keep_master_lang = abap_true
          CHANGING
            ct_tab = lt_titles ).
    ENDIF.

    CALL FUNCTION 'STREE_HIERARCHY_READ'
      EXPORTING
        structure_id       = mv_tree_id
        read_also_texts    = abap_true
        all_languages      = lv_all_languages
        language           = mv_language
      IMPORTING
        message            = ls_msg
      TABLES
        list_of_nodes      = lt_nodes
        list_of_references = lt_refs
        list_of_texts      = lt_texts.

    clear_fields( CHANGING cs_head  = ls_head
                           ct_nodes = lt_nodes ).

    SORT lt_titles BY id.
    DELETE ADJACENT DUPLICATES FROM lt_titles COMPARING spras id.

    SORT lt_texts BY spras.
    DELETE ADJACENT DUPLICATES FROM lt_texts COMPARING spras node_id.

    mo_i18n_params->trim_saplang_keyed_table(
      EXPORTING
        iv_lang_field_name = 'SPRAS'
        iv_keep_master_lang = abap_true
      CHANGING
        ct_tab = lt_texts ).

    io_xml->add( iv_name = 'TREE_HEAD'
                 ig_data = ls_head ).
    io_xml->add( iv_name = 'TREE_TITLES'
                 ig_data = lt_titles ).
    io_xml->add( iv_name = 'TREE_NODES'
                 ig_data = lt_nodes ).
    io_xml->add( iv_name = 'TREE_REFS'
                 ig_data = lt_refs ).
    io_xml->add( iv_name = 'TREE_TEXTS'
                 ig_data = lt_texts ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SHI3 implementation

*>>>>>>> ZCL_ABAPGIT_OBJECTS_GENERIC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_objects_generic===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_objects_generic===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_objects_generic===ccau.
*CLASS SHRITEFUH64VYIPN5I4UHL45BK6R4A DEFINITION DEFERRED.
*CLASS zcl_abapgit_objects_generic DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BK6R4A.



class LCL_ABAPGIT_OBJECTS_GENERIC implementation.
*"* method's implementations
*include methods.
  METHOD after_import.

    DATA: lt_cts_object_entry TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY,
          ls_cts_object_entry LIKE LINE OF lt_cts_object_entry,
          lt_cts_key          TYPE STANDARD TABLE OF e071k WITH DEFAULT KEY.

    FIELD-SYMBOLS <ls_object_method> LIKE LINE OF mt_object_method.


    ls_cts_object_entry-pgmid    = 'R3TR'.
    ls_cts_object_entry-object   = ms_item-obj_type.
    ls_cts_object_entry-obj_name = ms_item-obj_name.
    INSERT ls_cts_object_entry INTO TABLE lt_cts_object_entry.

    READ TABLE mt_object_method ASSIGNING <ls_object_method>
      WITH KEY
        objectname = ms_item-obj_type
        objecttype = 'L'
        method = 'AFTER_IMP'.
    IF sy-subrc = 0.
* client is actually optional for most AIM, but let's supply it and hope
* that those client-independent-ones just ignore it
      CALL FUNCTION <ls_object_method>-methodname
        EXPORTING
          iv_tarclient  = sy-mandt
          iv_is_upgrade = abap_false
        TABLES
          tt_e071       = lt_cts_object_entry
          tt_e071k      = lt_cts_key.
    ENDIF.

  ENDMETHOD.
  METHOD apply_clear_logic.
    IF mo_field_rules IS BOUND.
      mo_field_rules->apply_clear_logic( EXPORTING iv_table = |{ iv_table }|
                                         CHANGING  ct_data  = ct_data ).
    ENDIF.
  ENDMETHOD.
  METHOD apply_fill_logic.
    IF mo_field_rules IS BOUND.
      mo_field_rules->apply_fill_logic(
        EXPORTING
          iv_table   = |{ iv_table }|
          iv_package = iv_package
        CHANGING
          ct_data    = ct_data ).
    ENDIF.
  ENDMETHOD.
  METHOD before_export.

    DATA: lt_cts_object_entry TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY,
          ls_cts_object_entry LIKE LINE OF lt_cts_object_entry,
          lt_cts_key          TYPE STANDARD TABLE OF e071k WITH DEFAULT KEY,
          lv_client           TYPE trclient.

    FIELD-SYMBOLS <ls_object_method> LIKE LINE OF mt_object_method.


    READ TABLE mt_object_method ASSIGNING <ls_object_method>
      WITH KEY
        objectname = ms_item-obj_type
        objecttype = 'L'
        method     = 'BEFORE_EXP'.
    IF sy-subrc = 0.
      lv_client = sy-mandt.

      ls_cts_object_entry-pgmid    = 'R3TR'.
      ls_cts_object_entry-object   = ms_item-obj_type.
      ls_cts_object_entry-obj_name = ms_item-obj_name.
      INSERT ls_cts_object_entry INTO TABLE lt_cts_object_entry.

      CALL FUNCTION <ls_object_method>-methodname
        EXPORTING
          iv_client = lv_client
        TABLES
          tt_e071   = lt_cts_object_entry
          tt_e071k  = lt_cts_key.
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    CONSTANTS lc_logical_transport_object TYPE c LENGTH 1 VALUE 'L'.


    SELECT SINGLE * FROM objh INTO ms_object_header
      WHERE objectname = is_item-obj_type
      AND objecttype = lc_logical_transport_object.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Not found in OBJH, or not supported' ).
    ENDIF.

    " object tables
    SELECT * FROM objsl INTO CORRESPONDING FIELDS OF TABLE mt_object_table
      WHERE objectname = is_item-obj_type
      AND objecttype = lc_logical_transport_object
      AND tobject = 'TABU'
      ORDER BY PRIMARY KEY.
    IF mt_object_table IS INITIAL.
      Lcx_abapgit_exception=>raise( |Obviously corrupted object-type { is_item-obj_type }: No tables defined| ).
    ENDIF.

    " remove duplicate table/table-key entries
    " same table with different keys is ok
    SORT mt_object_table BY tobj_name tobjkey.
    DELETE ADJACENT DUPLICATES FROM mt_object_table COMPARING tobj_name tobjkey.

    " object methods
    SELECT * FROM objm INTO TABLE mt_object_method
      WHERE objectname = is_item-obj_type
      AND objecttype = lc_logical_transport_object
      ORDER BY PRIMARY KEY.

    ms_item = is_item.
    mv_language = iv_language.
    mo_field_rules = io_field_rules.

  ENDMETHOD.
  METHOD corr_insert.

* this will also insert into TADIR
    Lcl_abapgit_factory=>get_cts_api( )->insert_transport_object(
      iv_object   = ms_item-obj_type
      iv_obj_name = ms_item-obj_name
      iv_package  = iv_package
      iv_language = mv_language ).

  ENDMETHOD.
  METHOD delete.

    DATA: lv_where   TYPE string,
          lv_primary TYPE objsl-tobj_name.

    FIELD-SYMBOLS <ls_table> LIKE LINE OF mt_object_table.


    lv_primary = get_primary_table( ).

    LOOP AT mt_object_table ASSIGNING <ls_table>.
      lv_where = get_where_clause( <ls_table>-tobj_name ).
      ASSERT NOT lv_where IS INITIAL.

      DELETE FROM (<ls_table>-tobj_name) WHERE (lv_where).

      IF <ls_table>-tobj_name = lv_primary.
        ASSERT sy-dbcnt <= 1. "Just to be on the very safe side
      ENDIF.
    ENDLOOP.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD deserialize.

    validate( io_xml ).

    delete( iv_package ).

    deserialize_data(
      io_xml     = io_xml
      iv_package = iv_package ).

    after_import( ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD deserialize_data.

    DATA: lr_ref TYPE REF TO data.

    FIELD-SYMBOLS: <lt_data>  TYPE STANDARD TABLE,
                   <ls_table> LIKE LINE OF mt_object_table.


    LOOP AT mt_object_table ASSIGNING <ls_table>.

      CREATE DATA lr_ref TYPE STANDARD TABLE OF (<ls_table>-tobj_name).
      ASSIGN lr_ref->* TO <lt_data>.

      io_xml->read(
        EXPORTING
          iv_name = <ls_table>-tobj_name
        CHANGING
          cg_data = <lt_data> ).
      apply_fill_logic(
        EXPORTING
          iv_table   = <ls_table>-tobj_name
          iv_package = iv_package
        CHANGING
          ct_data    = <lt_data> ).

      INSERT (<ls_table>-tobj_name) FROM TABLE <lt_data>.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Error inserting data, { <ls_table>-tobj_name }| ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD distribute_name_to_components.

    DATA: lt_key_component_uncovered  LIKE it_key_component,
          ls_key_component_uncovered  LIKE LINE OF lt_key_component_uncovered,
          ls_objkey_sub               LIKE cs_objkey,
          lv_objkey_sub_pos           TYPE i,
          lv_remaining_length         TYPE i,
          lv_count_components_covered LIKE ls_objkey_sub-num.

    DATA lv_len LIKE ls_key_component_uncovered-leng.


    lt_key_component_uncovered = it_key_component.
    ls_objkey_sub-num = cs_objkey-num.
    lv_objkey_sub_pos = 0.

*    we want to fill the atribute values which are not covered by explicit key components yet
    lv_count_components_covered = ls_objkey_sub-num - 1.
    DO lv_count_components_covered TIMES.
      DELETE lt_key_component_uncovered INDEX 1.
    ENDDO.

    LOOP AT lt_key_component_uncovered INTO ls_key_component_uncovered.
      CLEAR ls_objkey_sub-value.

*      Some datatype used in the key might exceed the total remaining characters length (e. g. SICF)
      TRY.
          lv_remaining_length = strlen( |{ substring( val = cs_objkey-value
                                                      off = lv_objkey_sub_pos ) }| ).
        CATCH cx_sy_range_out_of_bounds.
          lv_remaining_length = 0.
          RETURN. ">>>>>>>>>>>>>>>>>>>>>>>>>>>
      ENDTRY.
      IF ls_key_component_uncovered-leng <= lv_remaining_length.
        lv_len = ls_key_component_uncovered-leng.
      ELSE.
        lv_len = lv_remaining_length.
      ENDIF.

      ls_objkey_sub-value = |{ substring( val = cs_objkey-value
                                          off = lv_objkey_sub_pos
                                          len = lv_len ) }|.
      ls_objkey_sub-num = cv_non_value_pos.

      INSERT ls_objkey_sub INTO TABLE ct_objkey.

      lv_objkey_sub_pos = lv_objkey_sub_pos + ls_key_component_uncovered-leng.
      cv_non_value_pos = cv_non_value_pos + 1.
      CLEAR ls_objkey_sub.

      IF lv_objkey_sub_pos = strlen( cs_objkey-value ).
        cs_objkey-num = cv_non_value_pos.
        EXIT. "end splitting - all characters captured
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD exists.

    DATA: lv_where_clause TYPE string,
          lv_primary      TYPE objsl-tobj_name,
          lr_table_line   TYPE REF TO data.

    FIELD-SYMBOLS: <lg_table_line> TYPE any.


    lv_primary = get_primary_table( ).

    lv_where_clause = get_where_clause( lv_primary ).

    CREATE DATA lr_table_line TYPE (lv_primary).
    ASSIGN lr_table_line->* TO <lg_table_line>.

    SELECT SINGLE * FROM (lv_primary) INTO <lg_table_line> WHERE (lv_where_clause).
    rv_bool = boolc( sy-dbcnt > 0 ).

  ENDMETHOD.
  METHOD get_key_fields.

    DATA: lv_table TYPE ddobjname.


    lv_table = iv_table.

    CALL FUNCTION 'DDIF_NAMETAB_GET'
      EXPORTING
        tabname   = lv_table
      TABLES
        dfies_tab = rt_keys
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    DELETE rt_keys WHERE keyflag = abap_false.

  ENDMETHOD.
  METHOD get_primary_table.

    DATA: ls_object_table LIKE LINE OF mt_object_table.
    DATA: lt_object_table LIKE mt_object_table.

    " There might be several tables marked as "primary"
    " Sort by DB key so we get first one in the list
    lt_object_table = mt_object_table.
    SORT lt_object_table.

    READ TABLE lt_object_table INTO ls_object_table WITH KEY prim_table = abap_true.
    IF sy-subrc <> 0.
      " Fallback. For some objects, no primary table is explicitly flagged
      " Then, the one with only one key field shall be chosen
      READ TABLE lt_object_table INTO ls_object_table WITH KEY tobjkey = '/&'. "#EC CI_SUBRC
    ENDIF.
    IF ls_object_table IS INITIAL.
      Lcx_abapgit_exception=>raise( |Object { ms_item-obj_type } has got no defined primary table| ).
    ENDIF.

    rv_table = ls_object_table-tobj_name.

  ENDMETHOD.
  METHOD get_where_clause.

    DATA: lv_objkey_pos      TYPE i,
          lv_next_objkey_pos TYPE i,
          lv_value_pos       TYPE i,
          lv_objkey_length   TYPE i,
          lt_objkey          TYPE ty_t_objkey,
          ls_objkey          LIKE LINE OF lt_objkey,
          lv_non_value_pos   TYPE numc3,
          lt_key_fields      TYPE ddfields.

    DATA: lv_is_asterix      TYPE abap_bool,
          lv_where_statement TYPE string,
          lv_key_pos         TYPE i,
          lv_value128        TYPE string.

    FIELD-SYMBOLS <ls_object_table> LIKE LINE OF mt_object_table.

    FIELD-SYMBOLS <ls_table_field> LIKE LINE OF lt_key_fields.


    READ TABLE mt_object_table ASSIGNING <ls_object_table> WITH KEY tobj_name = iv_tobj_name.
    ASSERT sy-subrc = 0.

    lt_key_fields = get_key_fields( iv_tobj_name ).

*   analyze the object key and compose the key (table)
    CLEAR lt_objkey.
    CLEAR ls_objkey.
    lv_objkey_pos = 0.
    lv_non_value_pos = 1.
    lv_value_pos = 0.
    lv_objkey_length = strlen( <ls_object_table>-tobjkey ).

    WHILE lv_objkey_pos <= lv_objkey_length.
      ls_objkey-num = lv_non_value_pos.
*     command
      IF <ls_object_table>-tobjkey+lv_objkey_pos(1) = '/'.
        IF NOT ls_objkey-value IS INITIAL.
*        We reached the end of a key-definition.
*        this key part may address multiple fields.
*        E. g. six characters may address one boolean field and a five-digit version field.
*        Thus, we need to analyze the remaining key components which have not been covered yet.
          split_value_to_keys(
            EXPORTING
              it_key_component = lt_key_fields
            CHANGING
              ct_objkey        = lt_objkey
              cs_objkey        = ls_objkey
              cv_non_value_pos = lv_non_value_pos ).
        ENDIF.
        lv_next_objkey_pos = lv_objkey_pos + 1.
*       '*' means all further key values
        IF <ls_object_table>-tobjkey+lv_next_objkey_pos(1) = '*'.
          ls_objkey-value = '*'.
          INSERT ls_objkey INTO TABLE lt_objkey.
          CLEAR ls_objkey.
          lv_non_value_pos = lv_non_value_pos + 1.
          lv_objkey_pos = lv_objkey_pos + 1.
*       object name
        ELSEIF <ls_object_table>-tobjkey+lv_next_objkey_pos(1) = '&'.
          ls_objkey-value = ms_item-obj_name.
*    The object name might comprise multiple key components (e. g. WDCC)
*    This string needs to be split
          distribute_name_to_components(
            EXPORTING
              it_key_component = lt_key_fields
            CHANGING
              ct_objkey        = lt_objkey
              cs_objkey        = ls_objkey
              cv_non_value_pos = lv_non_value_pos ).
          CLEAR ls_objkey.
          lv_objkey_pos = lv_objkey_pos + 1.
*       language
        ELSEIF <ls_object_table>-tobjkey+lv_next_objkey_pos(1) = 'L'.
          ls_objkey-value = mv_language.
          INSERT ls_objkey INTO TABLE lt_objkey.
          CLEAR ls_objkey.
          lv_non_value_pos = lv_non_value_pos + 1.
          lv_objkey_pos = lv_objkey_pos + 1.
*       Client
        ELSEIF <ls_object_table>-tobjkey+lv_next_objkey_pos(1) = 'C'.
          ls_objkey-value = sy-mandt.
          INSERT ls_objkey INTO TABLE lt_objkey.
          CLEAR ls_objkey.
          lv_non_value_pos = lv_non_value_pos + 1.
          lv_objkey_pos = lv_objkey_pos + 1.
        ENDIF.
        lv_value_pos = 0.
*     value
      ELSE.
        ls_objkey-value+lv_value_pos(1) = <ls_object_table>-tobjkey+lv_objkey_pos(1).
        lv_value_pos = lv_value_pos + 1.
      ENDIF.

      lv_objkey_pos = lv_objkey_pos + 1.
    ENDWHILE.

*    Similarly to that, fixed values might be supplied in the object key which actually make up key components
    IF NOT ls_objkey-value IS INITIAL.
      split_value_to_keys(
        EXPORTING
          it_key_component = lt_key_fields
        CHANGING
          ct_objkey        = lt_objkey
          cs_objkey        = ls_objkey
          cv_non_value_pos = lv_non_value_pos ).
    ENDIF.

*   compose the where clause
    lv_is_asterix = abap_false.
    lv_key_pos = 1.

    LOOP AT lt_key_fields ASSIGNING <ls_table_field>.
      READ TABLE lt_objkey INTO ls_objkey
        WITH TABLE KEY num = lv_key_pos.
      IF sy-subrc <> 0 OR <ls_table_field>-fieldname = 'LANGU'.
        CLEAR ls_objkey.
        lv_key_pos = lv_key_pos + 1.
        CONTINUE.
      ENDIF.
      IF ls_objkey-value = '*'.
        lv_is_asterix = abap_true.
      ENDIF.
      IF lv_is_asterix = abap_true.
        CONTINUE.
      ENDIF.
      IF NOT lv_where_statement IS INITIAL.
        CONCATENATE lv_where_statement 'AND' INTO lv_where_statement
          SEPARATED BY space.
      ENDIF.
      lv_value128 = cl_abap_dyn_prg=>quote( ls_objkey-value ).
      CONCATENATE lv_where_statement <ls_table_field>-fieldname '='
        lv_value128 INTO lv_where_statement SEPARATED BY space.
      lv_key_pos = lv_key_pos + 1.
    ENDLOOP.

    rv_where = condense( lv_where_statement ).

  ENDMETHOD.
  METHOD serialize.

    before_export( ).

    serialize_data( io_xml ).

  ENDMETHOD.
  METHOD serialize_data.

    DATA: lr_ref   TYPE REF TO data,
          lv_where TYPE string.

    FIELD-SYMBOLS: <lt_data>         TYPE STANDARD TABLE,
                   <ls_object_table> LIKE LINE OF mt_object_table.


    LOOP AT mt_object_table ASSIGNING <ls_object_table>.

      CREATE DATA lr_ref TYPE STANDARD TABLE OF (<ls_object_table>-tobj_name).
      ASSIGN lr_ref->* TO <lt_data>.

      lv_where = get_where_clause( <ls_object_table>-tobj_name ).

      SELECT * FROM (<ls_object_table>-tobj_name)
        INTO TABLE <lt_data>
        WHERE (lv_where)
        ORDER BY PRIMARY KEY.

      apply_clear_logic( EXPORTING iv_table = <ls_object_table>-tobj_name
                         CHANGING  ct_data  = <lt_data> ).

      io_xml->add(
        iv_name = <ls_object_table>-tobj_name
        ig_data = <lt_data> ).

    ENDLOOP.

  ENDMETHOD.
  METHOD split_value_to_keys.

    DATA: lt_key_component_uncovered LIKE it_key_component,
          ls_dummy                   LIKE LINE OF ct_objkey,
          ls_key_component_uncovered LIKE LINE OF lt_key_component_uncovered,
          ls_objkey_sub              LIKE cs_objkey,
          lv_objkey_sub_pos          TYPE i.


    lt_key_component_uncovered = it_key_component.

*    we want to fill the atribute values which are not covered by explicit key components yet
    LOOP AT ct_objkey INTO ls_dummy.
      DELETE lt_key_component_uncovered INDEX 1.
    ENDLOOP.

    ls_objkey_sub-num = cs_objkey-num.
    lv_objkey_sub_pos = 0.
    LOOP AT lt_key_component_uncovered INTO ls_key_component_uncovered.
      CLEAR ls_objkey_sub-value.
      ls_objkey_sub-value = cs_objkey-value+lv_objkey_sub_pos(ls_key_component_uncovered-leng).
      ls_objkey_sub-num = cv_non_value_pos.

      INSERT ls_objkey_sub INTO TABLE ct_objkey.

      lv_objkey_sub_pos = lv_objkey_sub_pos + ls_key_component_uncovered-leng.
      cv_non_value_pos = cv_non_value_pos + 1.
      CLEAR ls_objkey_sub.

      IF lv_objkey_sub_pos = strlen( cs_objkey-value ).
        cs_objkey-num = cv_non_value_pos.
        EXIT. "end splitting - all characters captured
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD validate.

    DATA: lv_where   TYPE string,
          lv_primary TYPE objsl-tobj_name,
          lr_ref     TYPE REF TO data.

    FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE.


    lv_primary = get_primary_table( ).

    CREATE DATA lr_ref TYPE STANDARD TABLE OF (lv_primary).
    ASSIGN lr_ref->* TO <lt_data>.

    io_xml->read(
      EXPORTING
        iv_name = lv_primary
      CHANGING
        cg_data = <lt_data> ).

    IF lines( <lt_data> ) = 0.
      Lcx_abapgit_exception=>raise( |Primary table { lv_primary } not found in imported container| ).
    ELSEIF lines( <lt_data> ) <> 1.
      Lcx_abapgit_exception=>raise( |Primary table { lv_primary } contains more than one instance!| ).
    ENDIF.

    lv_where = get_where_clause( lv_primary ).

*  validate that max one local instance was affected by the import
    SELECT COUNT(*) FROM (lv_primary) WHERE (lv_where).
    IF sy-dbcnt > 1.
      Lcx_abapgit_exception=>raise( |More than one instance exists locally in primary table { lv_primary }| ).
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECTS_GENERIC implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PICKLIST <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_picklist======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_picklist======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PICKLIST implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    FIELD-SYMBOLS <lt_tab> TYPE STANDARD TABLE.

    super->constructor( ).

    " copy contents of table to local scope
    CREATE DATA mr_list LIKE it_list.
    ASSIGN mr_list->* TO <lt_tab>.
    APPEND LINES OF it_list TO <lt_tab>.

    mv_attr_name = to_upper( iv_attr_name ).
    mi_item_renderer = ii_item_renderer.
    mv_in_page = iv_in_page.
    mv_id      = iv_id.
    mv_title   = iv_title.

    IF mi_item_renderer IS NOT BOUND AND mv_attr_name IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Renderer or attr name required' ).
    ENDIF.

    CREATE OBJECT mo_form_data.
    CREATE OBJECT mo_validation_log.
    mo_form = get_form_schema( ).
    mo_form_util = Lcl_abapgit_html_form_utils=>create( mo_form ).

  ENDMETHOD.
  METHOD get_form_schema.

    FIELD-SYMBOLS <lt_list> TYPE ANY TABLE.
    FIELD-SYMBOLS <lv_val> TYPE any.
    FIELD-SYMBOLS <ls_row> TYPE any.
    DATA lv_index TYPE i.
    DATA lv_label TYPE string.

    ro_form = Lcl_abapgit_html_form=>create( ).

    ro_form->radio(
      iv_name     = c_radio_name
      iv_label    = mv_title ).

    ASSIGN mr_list->* TO <lt_list>.
    LOOP AT <lt_list> ASSIGNING <ls_row>.
      lv_index = sy-tabix.

      IF mv_attr_name IS NOT INITIAL.
        ASSIGN COMPONENT mv_attr_name OF STRUCTURE <ls_row> TO <lv_val>.
        ASSERT sy-subrc = 0.
        lv_label = <lv_val>.
      ELSEIF mi_item_renderer IS BOUND.
        lv_label = mi_item_renderer->render(
          iv_item  = <ls_row>
          iv_index = lv_index )->render( ).
      ENDIF.

      ro_form->option(
        iv_label = lv_label
        iv_value = |{ lv_index }| ).

    ENDLOOP.

    ro_form->command(
      iv_label    = 'Choose'
      iv_cmd_type = Lif_abapgit_html_form=>c_cmd_type-input_main
      iv_action   = c_event-choose
    )->command(
      iv_label    = 'Back'
      iv_action   = c_event-back ).

  ENDMETHOD.
  METHOD get_result_idx.
    rv_index = mv_selected.
  ENDMETHOD.
  METHOD get_result_item.

    FIELD-SYMBOLS <lt_tab> TYPE STANDARD TABLE.

    CLEAR cs_selected.

    IF mv_selected > 0.
      ASSIGN mr_list->* TO <lt_tab>.
      READ TABLE <lt_tab> INDEX mv_selected INTO cs_selected.
      ASSERT sy-subrc = 0.
    ENDIF.

  ENDMETHOD.
  METHOD id.
    rv_id = mv_id.
  ENDMETHOD.
  METHOD is_fulfilled.
    rv_yes = mv_fulfilled.
  ENDMETHOD.
  METHOD is_in_page.
    rv_yes = mv_in_page.
  ENDMETHOD.
  METHOD return_state.
    IF mv_in_page = abap_true.
      rv_state = Lcl_abapgit_gui=>c_event_state-re_render.
    ELSE.
      rv_state = Lcl_abapgit_gui=>c_event_state-go_back.
    ENDIF.
  ENDMETHOD.
  METHOD set_id.
    mv_id = iv_id.
    ro_me = me.
  ENDMETHOD.
  METHOD set_in_page.
    mv_in_page = iv_in_page.
    ro_me = me.
  ENDMETHOD.
  METHOD was_cancelled.
    rv_yes = mv_cancelled.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    mo_form_data = mo_form_util->normalize( ii_event->form_data( ) ).
    mo_validation_log->clear( ).

    CASE ii_event->mv_action.
      WHEN c_event-back OR Lif_abapgit_definitions=>c_action-go_back.
        " Handle go_back as a "graceful back" - implicit cancel by F3/ESC
        mv_fulfilled = abap_true.
        mv_cancelled = abap_true.
        rs_handled-state = return_state( ).
      WHEN c_event-choose.
        mv_selected = mo_form_data->get( c_radio_name ).
        IF mv_selected = 0.
          mo_validation_log->set(
            iv_key = c_radio_name
            iv_val = 'You have to select one item' ).
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
        ELSE.
          mv_fulfilled = abap_true.
          rs_handled-state = return_state( ).
        ENDIF.
    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_page_title~get_page_title.
    rv_title = mv_title.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    ri_html = Lcl_abapgit_html=>create( mo_form->render(
      io_values         = mo_form_data
      io_validation_log = mo_validation_log ) ).
    register_handlers( ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PICKLIST implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SOD1 <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sod1=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sod1=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SOD1 implementation.
*"* method's implementations
*include methods.
  METHOD clear_content_fields.

    FIELD-SYMBOLS <ls_content_data> TYPE any.

    ASSIGN COMPONENT 'CONTENT' OF STRUCTURE cs_data TO <ls_content_data>.

    clear_field(
      EXPORTING
        iv_fieldname = 'CHANGE_USER'
      CHANGING
        cs_metadata  = <ls_content_data> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CHANGE_TIMESTAMP'
      CHANGING
        cs_metadata  = <ls_content_data> ).

  ENDMETHOD.
  METHOD clear_field.

    FIELD-SYMBOLS: <lv_value> TYPE data.

    ASSIGN COMPONENT iv_fieldname OF STRUCTURE cs_metadata TO <lv_value>.
    IF sy-subrc = 0.
      CLEAR: <lv_value>.
    ENDIF.

  ENDMETHOD.
  METHOD clear_metadata_fields.

    FIELD-SYMBOLS <ls_metadata> TYPE any.

    ASSIGN COMPONENT 'METADATA' OF STRUCTURE cs_data TO <ls_metadata>.

    clear_field(
      EXPORTING
        iv_fieldname = 'VERSION'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CREATED_AT'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CREATED_BY'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CHANGED_AT'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CHANGED_BY'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'RESPONSIBLE'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'PACKAGE_REF'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'MASTER_SYSTEM'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'DT_UUID'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'ABAP_LANGU_VERSION'
      CHANGING
        cs_metadata  = <ls_metadata> ).
    clear_field(
      EXPORTING
        iv_fieldname = 'ABAP_LANGU_VERSION'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'LINKS'
      CHANGING
        cs_metadata  = <ls_metadata> ).

  ENDMETHOD.
  METHOD constructor.

    DATA lo_data_model TYPE REF TO object.

    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).

    TRY.
        CREATE OBJECT lo_data_model TYPE (c_data_model_class_name).
      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |Object type { is_item-obj_type } is not supported by this system| ).
    ENDTRY.

  ENDMETHOD.
  METHOD create_wb_object_operator.

    DATA lx_error TYPE REF TO cx_root.

    TRY.

        CALL METHOD ('CL_WB_OBJECT_OPERATOR_FACTORY')=>('CREATE_OBJECT_OPERATOR')
          EXPORTING
            object_type       = is_object_type
            object_key        = iv_object_key
            transport_request = iv_transport_request
            do_commits        = iv_do_commits
            run_in_test_mode  = iv_run_in_test_mode
          RECEIVING
            result            = ro_wb_object_operator.

      CATCH cx_root INTO lx_error.

        Lcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

  ENDMETHOD.
  METHOD get_wb_object_operator.

    DATA lx_error TYPE REF TO cx_root.

    TRY.

        CALL METHOD ('CL_WB_OBJECT_OPERATOR_FACTORY')=>('GET_OBJECT_OPERATOR')
          EXPORTING
            object_type       = is_object_type
            object_key        = iv_object_key
            transport_request = iv_transport_request
          RECEIVING
            result            = ro_wb_object_operator.

      CATCH cx_root INTO lx_error.

        Lcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lo_data_model  TYPE REF TO if_wb_object_data_model,
          lo_factory     TYPE REF TO object,
          ls_object_type TYPE wbobjtype,
          lv_object_key  TYPE seu_objkey,
          lx_error       TYPE REF TO cx_root.

    TRY.

        ls_object_type-objtype_tr = ms_item-obj_type.
        lv_object_key             = ms_item-obj_name.

        lo_factory = create_wb_object_operator( is_object_type = ls_object_type
                                                iv_object_key  = lv_object_key ).

        CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~READ')
          IMPORTING
            eo_object_data = lo_data_model.

        rv_user = lo_data_model->get_changed_by( ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: ls_object_type TYPE wbobjtype,
          lv_object_key  TYPE seu_objkey,
          lo_factory     TYPE REF TO object,
          lx_error       TYPE REF TO cx_root.

    ls_object_type-objtype_tr = ms_item-obj_type.
    lv_object_key             = ms_item-obj_name.

    TRY.

        lo_factory = get_wb_object_operator( is_object_type       = ls_object_type
                                             iv_object_key        = lv_object_key
                                             iv_transport_request = iv_transport ).

        CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~DELETE').

      CATCH cx_root INTO lx_error.

        Lcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lo_factory               TYPE REF TO object,
          lo_data_model            TYPE REF TO if_wb_object_data_model,
          lv_data_type_name        TYPE string,
          ls_data                  TYPE REF TO data,
          ls_object_type           TYPE wbobjtype,
          lv_object_key            TYPE seu_objkey,
          lv_transport_request     TYPE trkorr,
          lo_logger                TYPE REF TO cl_wb_checklist,
          lx_create_error          TYPE REF TO cx_root,
          lx_error                 TYPE REF TO cx_root,
          lt_msgs                  TYPE TABLE OF string,
          lt_error_msgs_create     TYPE swbme_error_tab,
          ls_error_msg_create      LIKE LINE OF lt_error_msgs_create,
          lv_error_msg             TYPE string,
          lv_abap_language_version TYPE c LENGTH 1. " abap_language_version


    FIELD-SYMBOLS <ls_data> TYPE any.

    CREATE OBJECT lo_data_model TYPE (c_data_model_class_name).

    " if_wb_object_data_selection_co=>c_all_data
    CALL METHOD lo_data_model->('GET_DATATYPE_NAME')
      EXPORTING
        p_data_selection = 'AL'
      RECEIVING
        result           = lv_data_type_name.

    CREATE DATA ls_data TYPE (lv_data_type_name).
    ASSIGN ls_data->* TO <ls_data>.

    io_xml->read(
      EXPORTING
        iv_name = c_xml_transformation_name
      CHANGING
        cg_data = <ls_data> ).

    CALL METHOD lo_data_model->('SET_SELECTED_DATA')
      EXPORTING
        p_data_selection = 'AL' " if_wb_object_data_selection_co=>c_all_data
        p_data           = <ls_data>.

    TRY.

        ls_object_type-objtype_tr = ms_item-obj_type.
        lv_object_key             = ms_item-obj_name.

        lo_factory = get_wb_object_operator( is_object_type = ls_object_type
                                             iv_object_key  = lv_object_key ).

        lv_transport_request = Lcl_abapgit_default_transport=>get_instance( )->get( )-ordernum.

        IF Lif_abapgit_object~exists( ) = abap_true.

          CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~UPDATE')
            EXPORTING
              io_object_data    = lo_data_model
              version           = 'A'
              transport_request = lv_transport_request.

        ELSE.

          TRY.

              CALL METHOD lo_data_model->('GET_ABAP_LANGUAGE_VERSION')
                RECEIVING
                  result = lv_abap_language_version.

              CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~CREATE')
                EXPORTING
                  io_object_data        = lo_data_model
                  version               = 'A'
                  package               = iv_package
                  abap_language_version = lv_abap_language_version
                  transport_request     = lv_transport_request
                IMPORTING
                  logger                = lo_logger.

            CATCH cx_root INTO lx_create_error.

              " Check for error messages from Workbench API to provide more error infos to user
              lo_logger->get_error_messages( IMPORTING p_error_tab = lt_error_msgs_create ).

              IF lt_error_msgs_create IS NOT INITIAL.

                LOOP AT lt_error_msgs_create INTO ls_error_msg_create.

                  APPEND LINES OF ls_error_msg_create-mtext TO lt_msgs.

                ENDLOOP.

                CONCATENATE LINES OF lt_msgs INTO lv_error_msg SEPARATED BY '; '.
                Lcx_abapgit_exception=>raise( iv_text     = lv_error_msg
                                              ix_previous = lx_create_error ).

              ELSE.

                Lcx_abapgit_exception=>raise_with_text( lx_create_error ).

              ENDIF.

          ENDTRY.

        ENDIF.

      CATCH cx_root INTO lx_error.

        Lcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lo_factory     TYPE REF TO object,
          ls_object_type TYPE wbobjtype,
          lv_object_key  TYPE seu_objkey,
          lx_error       TYPE REF TO cx_root.

    TRY.

        ls_object_type-objtype_tr = ms_item-obj_type.
        lv_object_key             = ms_item-obj_name.

        lo_factory = get_wb_object_operator( is_object_type = ls_object_type
                                             iv_object_key  = lv_object_key ).

        CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~CHECK_EXISTENCE')
          RECEIVING
            r_result = rv_bool.

      CATCH cx_root INTO lx_error.

        Lcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESWB_EO'
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }*| ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lo_data_model     TYPE REF TO if_wb_object_data_model,
          lv_data_type_name TYPE string,
          lo_factory        TYPE REF TO object,
          ls_object_type    TYPE wbobjtype,
          lv_object_key     TYPE seu_objkey,
          lx_error          TYPE REF TO cx_root.

    DATA ls_data TYPE REF TO data.
    FIELD-SYMBOLS <ls_data> TYPE any.

    TRY.

        ls_object_type-objtype_tr = ms_item-obj_type.
        lv_object_key             = ms_item-obj_name.

        lo_factory = create_wb_object_operator( is_object_type = ls_object_type
                                                iv_object_key  = lv_object_key ).

        CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~READ')
          IMPORTING
            eo_object_data = lo_data_model.

        " if_wb_object_data_selection_co=>c_all_data
        CALL METHOD lo_data_model->('GET_DATATYPE_NAME')
          EXPORTING
            p_data_selection = 'AL'
          RECEIVING
            result           = lv_data_type_name.

        CREATE DATA ls_data TYPE (lv_data_type_name).
        ASSIGN ls_data->* TO <ls_data>.

        CALL METHOD lo_data_model->('GET_SELECTED_DATA')
          EXPORTING
            p_data_selection = 'AL' " if_wb_object_data_selection_co=>c_all_data
          IMPORTING
            p_data           = <ls_data>.

        clear_metadata_fields( CHANGING cs_data = <ls_data> ).
        clear_content_fields( CHANGING cs_data = <ls_data> ).
        clear_field( EXPORTING iv_fieldname = 'PLUGIN_CONFIG'
                     CHANGING  cs_metadata  = <ls_data> ).

        io_xml->add( iv_name = c_xml_transformation_name
                     ig_data = <ls_data> ).

      CATCH cx_root INTO lx_error.

        Lcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SOD1 implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SOD2 <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sod2=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sod2=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SOD2 implementation.
*"* method's implementations
*include methods.
  METHOD clear_content_fields.

    FIELD-SYMBOLS <ls_content_data> TYPE any.

    ASSIGN COMPONENT 'CONTENT' OF STRUCTURE cs_data TO <ls_content_data>.

    clear_field(
      EXPORTING
        iv_fieldname = 'CHANGE_USER'
      CHANGING
        cs_metadata  = <ls_content_data> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CHANGE_TIMESTAMP'
      CHANGING
        cs_metadata  = <ls_content_data> ).

  ENDMETHOD.
  METHOD clear_field.

    FIELD-SYMBOLS: <lv_value> TYPE data.

    ASSIGN COMPONENT iv_fieldname OF STRUCTURE cs_metadata TO <lv_value>.
    IF sy-subrc = 0.
      CLEAR: <lv_value>.
    ENDIF.

  ENDMETHOD.
  METHOD clear_metadata_fields.

    FIELD-SYMBOLS <ls_metadata> TYPE any.

    ASSIGN COMPONENT 'METADATA' OF STRUCTURE cs_data TO <ls_metadata>.

    clear_field(
      EXPORTING
        iv_fieldname = 'VERSION'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CREATED_AT'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CREATED_BY'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CHANGED_AT'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CHANGED_BY'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'RESPONSIBLE'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'PACKAGE_REF'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'MASTER_SYSTEM'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'DT_UUID'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'ABAP_LANGU_VERSION'
      CHANGING
        cs_metadata  = <ls_metadata> ).
    clear_field(
      EXPORTING
        iv_fieldname = 'ABAP_LANGU_VERSION'
      CHANGING
        cs_metadata  = <ls_metadata> ).

    clear_field(
      EXPORTING
        iv_fieldname = 'LINKS'
      CHANGING
        cs_metadata  = <ls_metadata> ).

  ENDMETHOD.
  METHOD constructor.

    DATA lo_data_model TYPE REF TO object.

    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).

    TRY.
        CREATE OBJECT lo_data_model TYPE (c_data_model_class_name).
      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |Object type { is_item-obj_type } is not supported by this system| ).
    ENDTRY.

  ENDMETHOD.
  METHOD create_wb_object_operator.

    DATA lx_error TYPE REF TO cx_root.

    TRY.

        CALL METHOD ('CL_WB_OBJECT_OPERATOR_FACTORY')=>('CREATE_OBJECT_OPERATOR')
          EXPORTING
            object_type       = is_object_type
            object_key        = iv_object_key
            transport_request = iv_transport_request
            do_commits        = iv_do_commits
            run_in_test_mode  = iv_run_in_test_mode
          RECEIVING
            result            = ro_wb_object_operator.

      CATCH cx_root INTO lx_error.

        Lcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

  ENDMETHOD.
  METHOD get_wb_object_operator.

    DATA lx_error TYPE REF TO cx_root.

    TRY.

        CALL METHOD ('CL_WB_OBJECT_OPERATOR_FACTORY')=>('GET_OBJECT_OPERATOR')
          EXPORTING
            object_type       = is_object_type
            object_key        = iv_object_key
            transport_request = iv_transport_request
          RECEIVING
            result            = ro_wb_object_operator.

      CATCH cx_root INTO lx_error.

        Lcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lo_data_model  TYPE REF TO if_wb_object_data_model,
          lo_factory     TYPE REF TO object,
          ls_object_type TYPE wbobjtype,
          lv_object_key  TYPE seu_objkey,
          lx_error       TYPE REF TO cx_root.

    TRY.

        ls_object_type-objtype_tr = ms_item-obj_type.
        lv_object_key             = ms_item-obj_name.

        lo_factory = create_wb_object_operator( is_object_type = ls_object_type
                                                iv_object_key  = lv_object_key ).

        CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~READ')
          IMPORTING
            eo_object_data = lo_data_model.

        rv_user = lo_data_model->get_changed_by( ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: ls_object_type TYPE wbobjtype,
          lv_object_key  TYPE seu_objkey,
          lo_factory     TYPE REF TO object,
          lx_error       TYPE REF TO cx_root.

    ls_object_type-objtype_tr = ms_item-obj_type.
    lv_object_key             = ms_item-obj_name.

    TRY.

        lo_factory = get_wb_object_operator( is_object_type       = ls_object_type
                                             iv_object_key        = lv_object_key
                                             iv_transport_request = iv_transport ).

        CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~DELETE').

      CATCH cx_root INTO lx_error.

        Lcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lo_factory               TYPE REF TO object,
          lo_data_model            TYPE REF TO if_wb_object_data_model,
          lv_data_type_name        TYPE string,
          ls_data                  TYPE REF TO data,
          ls_object_type           TYPE wbobjtype,
          lv_object_key            TYPE seu_objkey,
          lv_transport_request     TYPE trkorr,
          lo_logger                TYPE REF TO cl_wb_checklist,
          lx_create_error          TYPE REF TO cx_root,
          lx_error                 TYPE REF TO cx_root,
          lt_msgs                  TYPE TABLE OF string,
          lt_error_msgs_create     TYPE swbme_error_tab,
          ls_error_msg_create      LIKE LINE OF lt_error_msgs_create,
          lv_error_msg             TYPE string,
          lv_abap_language_version TYPE c LENGTH 1. " abap_language_version

    FIELD-SYMBOLS <ls_data> TYPE any.

    CREATE OBJECT lo_data_model TYPE (c_data_model_class_name).

    " if_wb_object_data_selection_co=>c_all_data
    CALL METHOD lo_data_model->('GET_DATATYPE_NAME')
      EXPORTING
        p_data_selection = 'AL'
      RECEIVING
        result           = lv_data_type_name.

    CREATE DATA ls_data TYPE (lv_data_type_name).
    ASSIGN ls_data->* TO <ls_data>.

    io_xml->read(
      EXPORTING
        iv_name = c_xml_transformation_name
      CHANGING
        cg_data = <ls_data> ).

    CALL METHOD lo_data_model->('SET_SELECTED_DATA')
      EXPORTING
        p_data_selection = 'AL' " if_wb_object_data_selection_co=>c_all_data
        p_data           = <ls_data>.

    TRY.

        ls_object_type-objtype_tr = ms_item-obj_type.
        lv_object_key             = ms_item-obj_name.

        lo_factory = get_wb_object_operator( is_object_type = ls_object_type
                                             iv_object_key  = lv_object_key ).

        lv_transport_request = Lcl_abapgit_default_transport=>get_instance( )->get( )-ordernum.

        IF Lif_abapgit_object~exists( ) = abap_true.

          CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~UPDATE')
            EXPORTING
              io_object_data    = lo_data_model
              version           = 'A'
              transport_request = lv_transport_request.

        ELSE.

          TRY.

              CALL METHOD lo_data_model->('GET_ABAP_LANGUAGE_VERSION')
                RECEIVING
                  result = lv_abap_language_version.

              CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~CREATE')
                EXPORTING
                  io_object_data        = lo_data_model
                  version               = 'A'
                  package               = iv_package
                  abap_language_version = lv_abap_language_version
                  transport_request     = lv_transport_request
                IMPORTING
                  logger                = lo_logger.

            CATCH cx_root INTO lx_create_error.

              " Check for error messages from Workbench API to provide more error infos to user
              lo_logger->get_error_messages( IMPORTING p_error_tab = lt_error_msgs_create ).

              IF lt_error_msgs_create IS NOT INITIAL.

                LOOP AT lt_error_msgs_create INTO ls_error_msg_create.

                  APPEND LINES OF ls_error_msg_create-mtext TO lt_msgs.

                ENDLOOP.

                CONCATENATE LINES OF lt_msgs INTO lv_error_msg SEPARATED BY '; '.
                Lcx_abapgit_exception=>raise( iv_text     = lv_error_msg
                                              ix_previous = lx_create_error ).

              ELSE.

                Lcx_abapgit_exception=>raise_with_text( lx_create_error ).

              ENDIF.

          ENDTRY.

        ENDIF.

      CATCH cx_root INTO lx_error.

        Lcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lo_factory     TYPE REF TO object,
          ls_object_type TYPE wbobjtype,
          lv_object_key  TYPE seu_objkey,
          lx_error       TYPE REF TO cx_root.

    TRY.

        ls_object_type-objtype_tr = ms_item-obj_type.
        lv_object_key             = ms_item-obj_name.

        lo_factory = get_wb_object_operator( is_object_type = ls_object_type
                                             iv_object_key  = lv_object_key ).

        CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~CHECK_EXISTENCE')
          RECEIVING
            r_result = rv_bool.

      CATCH cx_root INTO lx_error.

        Lcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESWB_EO'
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }*| ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lo_data_model     TYPE REF TO if_wb_object_data_model,
          lv_data_type_name TYPE string,
          lo_factory        TYPE REF TO object,
          ls_object_type    TYPE wbobjtype,
          lv_object_key     TYPE seu_objkey,
          lx_error          TYPE REF TO cx_root.

    DATA ls_data TYPE REF TO data.
    FIELD-SYMBOLS <ls_data> TYPE any.

    TRY.

        ls_object_type-objtype_tr = ms_item-obj_type.
        lv_object_key             = ms_item-obj_name.

        lo_factory = create_wb_object_operator( is_object_type = ls_object_type
                                                iv_object_key  = lv_object_key ).

        CALL METHOD lo_factory->('IF_WB_OBJECT_OPERATOR~READ')
          IMPORTING
            eo_object_data = lo_data_model.

        " if_wb_object_data_selection_co=>c_all_data
        CALL METHOD lo_data_model->('GET_DATATYPE_NAME')
          EXPORTING
            p_data_selection = 'AL'
          RECEIVING
            result           = lv_data_type_name.

        CREATE DATA ls_data TYPE (lv_data_type_name).
        ASSIGN ls_data->* TO <ls_data>.

        CALL METHOD lo_data_model->('GET_SELECTED_DATA')
          EXPORTING
            p_data_selection = 'AL' " if_wb_object_data_selection_co=>c_all_data
          IMPORTING
            p_data           = <ls_data>.

        clear_metadata_fields( CHANGING cs_data = <ls_data> ).
        clear_content_fields( CHANGING cs_data = <ls_data> ).
        clear_field( EXPORTING iv_fieldname = 'PLUGIN_CONFIG'
                     CHANGING  cs_metadata  = <ls_data> ).

        io_xml->add( iv_name = c_xml_transformation_name
                     ig_data = <ls_data> ).

      CATCH cx_root INTO lx_error.

        Lcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SOD2 implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_CHKC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_chkc=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_chkc=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_CHKC implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lr_data        TYPE REF TO data,
          lo_chkc_db_api TYPE REF TO object,
          lv_name        TYPE c LENGTH 30,
          lx_error       TYPE REF TO cx_root.

    FIELD-SYMBOLS: <ls_chkc_header> TYPE any,
                   <ls_chkc_user>   TYPE any.

    TRY.
        CREATE OBJECT lo_chkc_db_api TYPE ('CL_CHKC_DB_API').
        CREATE DATA lr_data TYPE ('CL_CHKC_DB_API=>TY_HEADER').
        ASSIGN lr_data->* TO <ls_chkc_header>.

        lv_name = ms_item-obj_name.

        CALL METHOD lo_chkc_db_api->('GET_HEADER')
          EXPORTING
            name    = lv_name
            version = 'I'
          RECEIVING
            header  = <ls_chkc_header>.

        IF <ls_chkc_header> IS INITIAL.
          CALL METHOD lo_chkc_db_api->('GET_HEADER')
            EXPORTING
              name    = lv_name
              version = 'A'
            RECEIVING
              header  = <ls_chkc_header>.
        ENDIF.

        ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <ls_chkc_header> TO <ls_chkc_user>.
        rv_user = <ls_chkc_user>.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_CHKC implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_CHKO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_chko=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_chko=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_CHKO implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lr_data        TYPE REF TO data,
          lo_chko_db_api TYPE REF TO object,
          lv_name        TYPE c LENGTH 30,
          lx_error       TYPE REF TO cx_root.

    FIELD-SYMBOLS: <ls_chko_header> TYPE any,
                   <ls_chko_user>   TYPE any.


    TRY.
        CREATE OBJECT lo_chko_db_api TYPE ('CL_CHKO_DB_API').
        CREATE DATA lr_data TYPE ('CL_CHKO_DB_API=>TY_HEADER').
        ASSIGN lr_data->* TO <ls_chko_header>.

        lv_name = ms_item-obj_name.

        CALL METHOD lo_chko_db_api->('GET_HEADER')
          EXPORTING
            name    = lv_name
            version = 'I'
          RECEIVING
            header  = <ls_chko_header>.

        IF <ls_chko_header> IS INITIAL.
          CALL METHOD lo_chko_db_api->('GET_HEADER')
            EXPORTING
              name    = lv_name
              version = 'A'
            RECEIVING
              header  = <ls_chko_header>.
        ENDIF.

        ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <ls_chko_header> TO <ls_chko_user>.
        rv_user = <ls_chko_user>.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_CHKO implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_CHKV <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_chkv=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_chkv=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_CHKV implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lr_data        TYPE REF TO data,
          lo_chkv_db_api TYPE REF TO object,
          lv_name        TYPE c LENGTH 180,
          lx_error       TYPE REF TO cx_root.

    FIELD-SYMBOLS: <ls_chkv_header> TYPE any,
                   <ls_chkv_user>   TYPE any.


    TRY.
        CREATE OBJECT lo_chkv_db_api TYPE ('CL_CHKV_DB_API').
        CREATE DATA lr_data TYPE ('CL_CHKV_DB_API=>TY_HEADER').
        ASSIGN lr_data->* TO <ls_chkv_header>.

        lv_name = ms_item-obj_name.

        CALL METHOD lo_chkv_db_api->('GET_HEADER')
          EXPORTING
            object_key = lv_name
            version    = 'I'
          RECEIVING
            header  = <ls_chkv_header>.

        IF <ls_chkv_header> IS INITIAL.
          CALL METHOD lo_chkv_db_api->('GET_HEADER')
            EXPORTING
              object_key = lv_name
              version    = 'A'
            RECEIVING
              header  = <ls_chkv_header>.
        ENDIF.

        ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <ls_chkv_header> TO <ls_chkv_user>.
        rv_user = <ls_chkv_user>.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_CHKV implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SSST <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ssst=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ssst=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SSST implementation.
*"* method's implementations
*include methods.
  METHOD validate_font.

    DATA: lv_tdfamily TYPE tfo01-tdfamily.


    SELECT SINGLE tdfamily FROM tfo01 INTO lv_tdfamily
      WHERE tdfamily = iv_tdfamily.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Font family not found' ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE lastuser FROM stxsadm INTO rv_user
      WHERE stylename = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_stylename TYPE tdssname.


    lv_stylename = ms_item-obj_name.

    CALL FUNCTION 'SSF_DELETE_STYLE'
      EXPORTING
        i_stylename           = lv_stylename
        i_with_dialog         = abap_false
        i_with_confirm_dialog = abap_false
      EXCEPTIONS
        no_name               = 1
        no_style              = 2
        style_locked          = 3
        cancelled             = 4
        no_access_permission  = 5
        illegal_language      = 6
        OTHERS                = 7.
    IF sy-subrc <> 0 AND sy-subrc <> 2.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.
* see fm SSF_UPLOAD_STYLE

    DATA: ls_header     TYPE ssfcats,
          ls_new_header TYPE ssfcats,
          lt_paragraphs TYPE TABLE OF ssfparas,
          lt_strings    TYPE TABLE OF ssfstrings,
          lt_tabstops   TYPE TABLE OF stxstab.

    FIELD-SYMBOLS: <lv_spras> TYPE spras.


    io_xml->read( EXPORTING iv_name = 'HEADER'
                  CHANGING cg_data = ls_header ).
    io_xml->read( EXPORTING iv_name = 'SSFPARAS'
                  CHANGING cg_data = lt_paragraphs ).
    io_xml->read( EXPORTING iv_name = 'SSFSTRINGS'
                  CHANGING cg_data = lt_strings ).
    io_xml->read( EXPORTING iv_name = 'STXSTAB'
                  CHANGING cg_data = lt_tabstops ).

    validate_font( ls_header-tdfamily ).

    CALL FUNCTION 'SSF_READ_STYLE' "Just load FG
      EXPORTING
        i_style_name        = ls_header-stylename
        i_style_active_flag = 'A'
      EXCEPTIONS
        OTHERS              = 0.

    set_default_package( iv_package ).
    ASSIGN ('(SAPLSTXBS)MASTER_LANGUAGE') TO <lv_spras>.
    IF sy-subrc = 0.
      <lv_spras> = ls_header-masterlang.
    ENDIF.

    tadir_insert( iv_package ).

    CALL FUNCTION 'SSF_SAVE_STYLE'
      EXPORTING
        i_header     = ls_header
      IMPORTING
        e_header     = ls_new_header
      TABLES
        i_paragraphs = lt_paragraphs
        i_strings    = lt_strings
        i_tabstops   = lt_tabstops.

    IF ls_new_header IS NOT INITIAL.

      CALL FUNCTION 'SSF_ACTIVATE_STYLE'
        EXPORTING
          i_stylename          = ls_header-stylename
          redirect_error_msg   = abap_true " otherwise warnings write list output
        EXCEPTIONS
          no_name              = 1
          no_style             = 2
          cancelled            = 3
          no_access_permission = 4
          illegal_language     = 5
          OTHERS               = 6.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_stylename TYPE stxsadm-stylename.

    SELECT SINGLE stylename
      FROM stxshead INTO lv_stylename
      WHERE active    = c_style_active
        AND stylename = ms_item-obj_name
        AND vari      = ''.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'E_SMSTYLE'
                                            iv_argument    = |{ ms_item-obj_name }| ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: ls_bcdata TYPE bdcdata,
          lt_bcdata TYPE STANDARD TABLE OF bdcdata.

    ls_bcdata-program  = 'SAPMSSFS'.
    ls_bcdata-dynpro   = '0100'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam     = 'SSFSCREENS-SNAME'.
    ls_bcdata-fval     = ms_item-obj_name.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=DISPLAY'.
    APPEND ls_bcdata TO lt_bcdata.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SMARTSTYLES'
      it_bdcdata = lt_bcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.
* see fm SSF_DOWNLOAD_STYLE

    DATA: lv_style_name TYPE tdssname,
          ls_header     TYPE ssfcats,
          lt_paragraphs TYPE TABLE OF ssfparas,
          lt_strings    TYPE TABLE OF ssfstrings,
          lt_tabstops   TYPE TABLE OF stxstab.


    lv_style_name = ms_item-obj_name.

    CALL FUNCTION 'SSF_READ_STYLE'
      EXPORTING
        i_style_name             = lv_style_name
        i_style_active_flag      = c_style_active
        i_style_variant          = '%MAIN'
        i_style_language         = mv_language
      IMPORTING
        e_header                 = ls_header
      TABLES
        e_paragraphs             = lt_paragraphs
        e_strings                = lt_strings
        e_tabstops               = lt_tabstops
      EXCEPTIONS
        no_name                  = 1
        no_style                 = 2
        active_style_not_found   = 3
        inactive_style_not_found = 4
        no_variant               = 5
        no_main_variant          = 6
        cancelled                = 7
        no_access_permission     = 8
        OTHERS                   = 9.
    IF sy-subrc = 2.
      RETURN.
    ELSEIF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CLEAR ls_header-version.
    CLEAR ls_header-firstuser.
    CLEAR ls_header-firstdate.
    CLEAR ls_header-firsttime.
    CLEAR ls_header-lastuser.
    CLEAR ls_header-lastdate.
    CLEAR ls_header-lasttime.

    io_xml->add( iv_name = 'HEADER'
                 ig_data = ls_header ).
    io_xml->add( ig_data = lt_paragraphs
                 iv_name = 'SSFPARAS' ).
    io_xml->add( ig_data = lt_strings
                 iv_name = 'SSFSTRINGS' ).
    io_xml->add( ig_data = lt_tabstops
                 iv_name = 'STXSTAB' ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SSST implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_APIS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_apis=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_apis=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_APIS implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    DATA lr_data TYPE REF TO data.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    TRY.
        CREATE DATA lr_data TYPE (c_model).
      CATCH cx_sy_create_error.
        Lcx_abapgit_exception=>raise( |APIS not supported by your NW release| ).
    ENDTRY.

  ENDMETHOD.
  METHOD initialize.

    IF mo_handler IS NOT BOUND.
      CREATE OBJECT mo_handler TYPE ('CL_ARS_API_ABAPGIT')
        EXPORTING
          iv_api_object_name = ms_item-obj_name.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    initialize( ).

    TRY.
        CALL METHOD mo_handler->('IF_ARS_API_ABAPGIT~GET_CHANGED_BY')
          RECEIVING
            rv_changed_by = rv_user.
      CATCH cx_root.
        rv_user = c_user_unknown.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

* IF_ARS_API_ABAPGIT~DELETE_API_STATE dumps and checks fail, even tho I as a developer can delete it

    DATA lo_db   TYPE REF TO object.
    DATA lr_data TYPE REF TO data.
    FIELD-SYMBOLS <ls_api_key> TYPE any.


    CREATE DATA lr_data TYPE ('IF_ARS_STATE_DB_ACCESS=>TY_S_API_KEY').
    ASSIGN lr_data->* TO <ls_api_key>.
    <ls_api_key> = ms_item-obj_name.
    ASSERT <ls_api_key> IS NOT INITIAL.

    CALL METHOD ('CL_ARS_STATE_DB_ACCESS')=>('GET_INSTANCE')
      RECEIVING
        ro_state_db_access = lo_db.

    CALL METHOD lo_db->('IF_ARS_STATE_DB_ACCESS~DELETE')
      EXPORTING
        is_api_key = <ls_api_key>.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

* IF_ARS_API_ABAPGIT~SAVE_API_STATE dumps in some package checks

    DATA lr_data              TYPE REF TO data.
    DATA lo_db                TYPE REF TO object.
    FIELD-SYMBOLS <ls_data>   TYPE any.
    FIELD-SYMBOLS <lt_data_states> TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_header> TYPE any.
    FIELD-SYMBOLS <lt_states> TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_state>  TYPE any.
    FIELD-SYMBOLS <ls_row>    TYPE any.
    FIELD-SYMBOLS <lv_simple> TYPE simple.


    CREATE DATA lr_data TYPE (c_model).
    ASSIGN lr_data->* TO <ls_data>.
    CREATE DATA lr_data TYPE ('IF_ARS_STATE_DB_ACCESS=>TY_S_HEADER').
    ASSIGN lr_data->* TO <ls_header>.
    CREATE DATA lr_data TYPE ('IF_ARS_STATE_DB_ACCESS=>TY_T_STATE').
    ASSIGN lr_data->* TO <lt_states>.

    io_xml->read(
      EXPORTING
        iv_name = 'APIS'
      CHANGING
        cg_data = <ls_data> ).

    MOVE-CORRESPONDING <ls_data> TO <ls_header>.

    ASSIGN COMPONENT 'API_STATES' OF STRUCTURE <ls_data> TO <lt_data_states>.
    ASSERT sy-subrc = 0.

* the state table is sorted,
    LOOP AT <lt_data_states> ASSIGNING <ls_state>.
      CREATE DATA lr_data TYPE ('IF_ARS_STATE_DB_ACCESS=>TY_S_STATE').
      ASSIGN lr_data->* TO <ls_row>.
      MOVE-CORRESPONDING <ls_state> TO <ls_row>.
      MOVE-CORRESPONDING <ls_header> TO <ls_row>.

      ASSIGN COMPONENT 'SOFTWARE_RELEASE_NAME' OF STRUCTURE <ls_row> TO <lv_simple>.
      ASSERT sy-subrc = 0.
      <lv_simple> = '1908'.
      ASSIGN COMPONENT 'CREATED_AT' OF STRUCTURE <ls_row> TO <lv_simple>.
      ASSERT sy-subrc = 0.
      <lv_simple> = sy-datum.
      ASSIGN COMPONENT 'CREATED_BY' OF STRUCTURE <ls_row> TO <lv_simple>.
      ASSERT sy-subrc = 0.
      <lv_simple> = sy-uname.
      ASSIGN COMPONENT 'LAST_CHANGED_AT' OF STRUCTURE <ls_row> TO <lv_simple>.
      ASSERT sy-subrc = 0.
      <lv_simple> = sy-datum.
      ASSIGN COMPONENT 'LAST_CHANGED_BY' OF STRUCTURE <ls_row> TO <lv_simple>.
      ASSERT sy-subrc = 0.
      <lv_simple> = sy-uname.

      INSERT <ls_row> INTO TABLE <lt_states>.
    ENDLOOP.

    CALL METHOD ('CL_ARS_STATE_DB_ACCESS')=>('GET_INSTANCE')
      RECEIVING
        ro_state_db_access = lo_db.

    CALL METHOD lo_db->('IF_ARS_STATE_DB_ACCESS~SAVE')
      EXPORTING
        is_header         = <ls_header>
        it_release_states = <lt_states>.

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    TRY.
        initialize( ).
        CALL METHOD mo_handler->('IF_ARS_API_ABAPGIT~CHECK_EXISTS')
          RECEIVING
            rv_api_exists = rv_bool.
      CATCH cx_root.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-late TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-version = 'v2.0.0'.
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = Lif_abapgit_object~exists( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
* looks like there is no enqueue lock
* E_ARS_API ?
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " todo
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA lr_data TYPE REF TO data.
    FIELD-SYMBOLS <ls_data> TYPE any.

    CREATE DATA lr_data TYPE (c_model).
    ASSIGN lr_data->* TO <ls_data>.

    initialize( ).

    CALL METHOD mo_handler->('IF_ARS_API_ABAPGIT~GET_API_STATE')
      RECEIVING
        rs_apis_object = <ls_data>.

    io_xml->add( iv_name = 'APIS'
                 ig_data = <ls_data> ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_APIS implementation

*>>>>>>> ZCL_ABAPGIT_ITEM_GRAPH <<<<<<<*

*"* macro definitions
*include zcl_abapgit_item_graph========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_item_graph========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_item_graph========ccau.



class LCL_ABAPGIT_ITEM_GRAPH implementation.
*"* method's implementations
*include methods.
  METHOD add_edge.
    DATA ls_edge LIKE LINE OF mt_edges.
    ASSERT is_from IS NOT INITIAL.
    ASSERT is_to IS NOT INITIAL.
    ls_edge-from = is_from.
    ls_edge-to   = is_to.
    APPEND ls_edge TO mt_edges.
  ENDMETHOD.
  METHOD constructor.
    INSERT LINES OF it_items INTO TABLE mt_vertices.
  ENDMETHOD.
  METHOD get_next.
* find a vertex with no inbound edges, if it does not exist pick anything

    DATA ls_vertex LIKE LINE OF mt_vertices.
    DATA lv_index  TYPE i.

    LOOP AT mt_vertices INTO ls_vertex.
      lv_index = sy-tabix.
      READ TABLE mt_edges WITH KEY sec_key COMPONENTS
        to-obj_type = ls_vertex-obj_type
        to-obj_name = ls_vertex-obj_name
        TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        remove_vertex( lv_index ).
        rs_item = ls_vertex.
        RETURN.
      ENDIF.
    ENDLOOP.

    IF mv_warning = abap_false.
* only issue the warning once per graph
      ii_log->add_warning( |Cycle detected in item graph| ).
      mv_warning = abap_true.
    ENDIF.

    READ TABLE mt_vertices INTO rs_item INDEX 1.
    ASSERT sy-subrc = 0.
    remove_vertex( 1 ).

  ENDMETHOD.
  METHOD has_vertices.
    rv_bool = boolc( lines( mt_vertices ) > 0 ).
  ENDMETHOD.
  METHOD remove_vertex.
    DATA ls_vertex LIKE LINE OF mt_vertices.

    READ TABLE mt_vertices INDEX iv_index INTO ls_vertex.
    ASSERT sy-subrc = 0.

    DELETE mt_vertices INDEX iv_index.
    DELETE mt_edges WHERE
      from-obj_type = ls_vertex-obj_type AND
      from-obj_name = ls_vertex-obj_name.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ITEM_GRAPH implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_STYL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_styl=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_styl=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_STYL implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    DATA: ls_style TYPE ty_style,
          lv_name  TYPE itcda-tdstyle.


    lv_name = ms_item-obj_name.

    CALL FUNCTION 'READ_STYLE'
      EXPORTING
        style        = lv_name
      IMPORTING
        style_header = ls_style-header
      TABLES
        paragraphs   = ls_style-paragraphs
        strings      = ls_style-strings
        tabs         = ls_style-tabs.

    rv_user = ls_style-header-tdluser.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_style TYPE itcda-tdstyle.


    lv_style = ms_item-obj_name.

    CALL FUNCTION 'DELETE_STYLE'
      EXPORTING
        style    = lv_style
        language = '*'.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_style TYPE ty_style.


    io_xml->read( EXPORTING iv_name = 'STYLE'
                  CHANGING cg_data = ls_style ).

    CALL FUNCTION 'SAVE_STYLE'
      EXPORTING
        style_header = ls_style-header
      TABLES
        paragraphs   = ls_style-paragraphs
        strings      = ls_style-strings
        tabs         = ls_style-tabs.

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: ls_style TYPE ty_style,
          lv_name  TYPE itcda-tdstyle,
          lv_found TYPE abap_bool.


    lv_name = ms_item-obj_name.

    CALL FUNCTION 'READ_STYLE'
      EXPORTING
        style      = lv_name
      IMPORTING
        found      = lv_found
      TABLES
        paragraphs = ls_style-paragraphs
        strings    = ls_style-strings
        tabs       = ls_style-tabs.

    rv_bool = boolc( lv_found = abap_true ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: ls_bcdata TYPE bdcdata,
          lt_bcdata TYPE STANDARD TABLE OF bdcdata.

    ls_bcdata-program  = 'SAPMSSCS'.
    ls_bcdata-dynpro   = '1100'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam     = 'RSSCS-TDSTYLE'.
    ls_bcdata-fval     = ms_item-obj_name.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam     = 'RSSCS-TDSPRAS'.
    ls_bcdata-fval     = mv_language.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam     = 'RSSCS-TDHEADEROB'.
    ls_bcdata-fval     = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=SHOW'.
    APPEND ls_bcdata TO lt_bcdata.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SE72'
      it_bdcdata = lt_bcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_style TYPE ty_style,
          lv_name  TYPE itcda-tdstyle.


    lv_name = ms_item-obj_name.

    CALL FUNCTION 'READ_STYLE'
      EXPORTING
        style        = lv_name
      IMPORTING
        style_header = ls_style-header
      TABLES
        paragraphs   = ls_style-paragraphs
        strings      = ls_style-strings
        tabs         = ls_style-tabs.

    CLEAR: ls_style-header-tdfuser,
           ls_style-header-tdfdate,
           ls_style-header-tdftime,
           ls_style-header-tdfreles,
           ls_style-header-tdluser,
           ls_style-header-tdldate,
           ls_style-header-tdltime,
           ls_style-header-tdlreles.

    io_xml->add( iv_name = 'STYLE'
                 ig_data = ls_style ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_STYL implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_CMPT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_cmpt=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_cmpt=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_CMPT implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    TRY.
        CALL METHOD ('CL_CMP_TEMPLATE')=>('S_GET_DB_ACCESS')
          RECEIVING
            r_ref_db_access = mo_cmp_db.

      CATCH cx_root.
    ENDTRY.

    mv_name = ms_item-obj_name.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lo_cmp_template TYPE REF TO object.

    TRY.
        CALL METHOD ('CL_CMP_TEMPLATE')=>('S_CREATE_FROM_DB')
          EXPORTING
            i_name         = mv_name
            i_version      = 'A'
          RECEIVING
            r_ref_template = lo_cmp_template.

        CALL METHOD lo_cmp_template->('IF_CMP_TEMPLATE_EDIT~GET_CHANGE_USER')
          RECEIVING
            r_user = rv_user.

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( 'CMPT not supported' ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_deleted TYPE abap_bool.

    TRY.
        CALL METHOD mo_cmp_db->('IF_CMP_TEMPLATE_DB~DELETE_TEMPLATE')
          EXPORTING
            i_name        = mv_name
            i_version     = 'A'
            i_flg_header  = abap_true
            i_flg_lines   = abap_true
          RECEIVING
            r_flg_deleted = lv_deleted.

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( 'CMPT not supported' ).
    ENDTRY.

    IF lv_deleted = abap_false.
      Lcx_abapgit_exception=>raise( |Error deleting CMPT { ms_item-obj_name }| ).
    ENDIF.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lr_template TYPE REF TO data.
    FIELD-SYMBOLS: <lg_template> TYPE any,
                   <lg_header>   TYPE any,
                   <lg_field>    TYPE any.

    TRY.
        CREATE DATA lr_template TYPE ('IF_CMP_TEMPLATE_DB=>TYP_TEMPLATE').
        ASSIGN lr_template->* TO <lg_template>.

        io_xml->read(
          EXPORTING
            iv_name = 'CMPT'
          CHANGING
            cg_data = <lg_template> ).

        ASSIGN COMPONENT 'STR_HEADER' OF STRUCTURE <lg_template> TO <lg_header>.
        IF sy-subrc = 0.
          ASSIGN COMPONENT 'NAME' OF STRUCTURE <lg_header> TO <lg_field>.
          IF sy-subrc = 0.
            <lg_field> = ms_item-obj_name.
          ENDIF.
          ASSIGN COMPONENT 'VERSION' OF STRUCTURE <lg_header> TO <lg_field>.
          IF sy-subrc = 0.
            <lg_field> = 'A'.
          ENDIF.
        ENDIF.

        CALL METHOD mo_cmp_db->('IF_CMP_TEMPLATE_DB~SAVE_TEMPLATE')
          EXPORTING
            i_template_db = <lg_template>
            i_flg_header  = abap_true
            i_flg_lines   = abap_true.

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( 'CMPT not supported' ).
    ENDTRY.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    TRY.
        CALL METHOD ('CL_CMP_TEMPLATE')=>('S_TEMPLATE_EXISTS')
          EXPORTING
            i_name       = mv_name
            i_version    = 'A'
          RECEIVING
            r_flg_exists = rv_bool.
        IF rv_bool = abap_false.
          CALL METHOD ('CL_CMP_TEMPLATE')=>('S_TEMPLATE_EXISTS')
            EXPORTING
              i_name       = mv_name
              i_version    = 'I'
            RECEIVING
              r_flg_exists = rv_bool.
        ENDIF.
      CATCH cx_root.
        Lcx_abapgit_exception=>raise( 'CMPT not supported' ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lr_template TYPE REF TO data.
    FIELD-SYMBOLS: <lg_template> TYPE any,
                   <lg_header>   TYPE any,
                   <lg_field>    TYPE any.

    TRY.
        CREATE DATA lr_template TYPE ('IF_CMP_TEMPLATE_DB=>TYP_TEMPLATE').
        ASSIGN lr_template->* TO <lg_template>.

        CALL METHOD mo_cmp_db->('IF_CMP_TEMPLATE_DB~READ_TEMPLATE')
          EXPORTING
            i_name     = |{ ms_item-obj_name }|
            i_version  = 'A'
          RECEIVING
            r_template = <lg_template>.

        ASSIGN COMPONENT 'STR_HEADER' OF STRUCTURE <lg_template> TO <lg_header>.
        IF sy-subrc = 0.
          ASSIGN COMPONENT 'NAME' OF STRUCTURE <lg_header> TO <lg_field>.
          IF sy-subrc = 0.
            CLEAR <lg_field>.
          ENDIF.
          ASSIGN COMPONENT 'VERSION' OF STRUCTURE <lg_header> TO <lg_field>.
          IF sy-subrc = 0.
            CLEAR <lg_field>.
          ENDIF.
          ASSIGN COMPONENT 'CHANGED_ON' OF STRUCTURE <lg_header> TO <lg_field>.
          IF sy-subrc = 0.
            CLEAR <lg_field>.
          ENDIF.
          ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <lg_header> TO <lg_field>.
          IF sy-subrc = 0.
            CLEAR <lg_field>.
          ENDIF.
          ASSIGN COMPONENT 'CHANGED_TS' OF STRUCTURE <lg_header> TO <lg_field>.
          IF sy-subrc = 0.
            CLEAR <lg_field>.
          ENDIF.
        ENDIF.

        io_xml->add( iv_name = 'CMPT'
                     ig_data = <lg_template> ).

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( 'CMPT not supported' ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_CMPT implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ACID <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_acid=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_acid=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ACID implementation.
*"* method's implementations
*include methods.
  METHOD create_object.

    DATA: lv_name TYPE aab_id_name.


    lv_name = ms_item-obj_name.

    CREATE OBJECT ro_aab
      EXPORTING
        im_name          = lv_name
      EXCEPTIONS
        name_not_allowed = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
* looks like "changed by user" is not stored in the database
    rv_user = c_user_unknown.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lo_aab TYPE REF TO cl_aab_id.


    lo_aab = create_object( ).
    lo_aab->enqueue(
      EXCEPTIONS
        foreign_lock = 1
        system_error = 2
        cts_error    = 3
        OTHERS       = 4 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.
    lo_aab->delete(
      EXCEPTIONS
        prop_error       = 1
        propt_error      = 2
        act_error        = 3
        cts_error        = 4
        cts_devclass     = 5
        id_not_found     = 6
        no_authorization = 7
        id_still_used    = 8
        where_used_error = 9
        OTHERS           = 10 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.
    lo_aab->dequeue( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_description TYPE aab_id_descript,
          lo_aab         TYPE REF TO cl_aab_id.


    io_xml->read( EXPORTING iv_name = 'DESCRIPTION'
                  CHANGING  cg_data = lv_description ).

    lo_aab = create_object( ).

    lo_aab->enqueue(
      EXCEPTIONS
        foreign_lock = 1
        system_error = 2
        cts_error    = 3
        OTHERS       = 4 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    lo_aab->set_descript(
      EXPORTING
        im_descript      = lv_description
      EXCEPTIONS
        no_authorization = 1
        OTHERS           = 2 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    tadir_insert( iv_package ).

    lo_aab->save(
      EXCEPTIONS
        no_descript_specified = 1
        no_changes_found      = 2
        prop_error            = 3
        propt_error           = 4
        act_error             = 5
        cts_error             = 6
        sync_attributes_error = 7
        action_canceled       = 8
        OTHERS                = 9 ).
    IF sy-subrc >= 3.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    lo_aab->dequeue( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_state TYPE abap_bool,
          lo_aab   TYPE REF TO cl_aab_id.


    lo_aab = create_object( ).

    lo_aab->get_state( IMPORTING ex_state = lv_state ).
    rv_bool = boolc( lv_state = abap_true ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lo_aab         TYPE REF TO cl_aab_id,
          lv_description TYPE aab_id_descript.


    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lo_aab = create_object( ).

    lo_aab->get_descript(
      IMPORTING ex_descript = lv_description
      EXCEPTIONS no_description_found = 1 ).

    io_xml->add( iv_name = 'DESCRIPTION'
                 ig_data = lv_description ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ACID implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_AMSD <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_amsd=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_amsd=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_AMSD implementation.
*"* method's implementations
*include methods.
  METHOD clear_field.

    FIELD-SYMBOLS: <lv_value> TYPE data.

    ASSIGN COMPONENT iv_fieldname OF STRUCTURE cs_logical_db_schema
           TO <lv_value>.
    ASSERT sy-subrc = 0.

    CLEAR: <lv_value>.

  ENDMETHOD.
  METHOD clear_fields.

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CREATED_AT'
      CHANGING
        cs_logical_db_schema = cs_logical_db_schema ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CREATED_BY'
      CHANGING
        cs_logical_db_schema = cs_logical_db_schema ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CHANGED_AT'
      CHANGING
        cs_logical_db_schema = cs_logical_db_schema ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CHANGED_BY'
      CHANGING
        cs_logical_db_schema = cs_logical_db_schema ).

  ENDMETHOD.
  METHOD constructor.

    super->constructor(
        is_item     = is_item
        iv_language = iv_language ).

    mv_logical_db_schema_key = ms_item-obj_name.

    TRY.
        CREATE DATA mr_logical_db_schema TYPE ('CL_AMDP_SCHEMA_OBJECT_DATA=>TY_OBJECT_DATA').
        CREATE OBJECT mi_persistence TYPE ('CL_AMDP_SCHEMA_OBJECT_PERSIST').

      CATCH cx_sy_create_error.
        Lcx_abapgit_exception=>raise( |AMSD not supported by your NW release| ).
    ENDTRY.

  ENDMETHOD.
  METHOD fill_metadata_from_db.

    DATA:
      li_wb_object_operator    TYPE REF TO object,
      lr_logical_db_schema_old TYPE REF TO data.

    FIELD-SYMBOLS:
      <ls_logical_db_schema_old> TYPE any,
      <lv_created_at>            TYPE xsddatetime_z,
      <lv_created_by>            TYPE syuname,
      <lv_created_at_old>        TYPE xsddatetime_z,
      <lv_created_by_old>        TYPE syuname.

    li_wb_object_operator = get_wb_object_operator( ).

    CREATE DATA lr_logical_db_schema_old TYPE ('CL_AMDP_SCHEMA_OBJECT_DATA=>TY_OBJECT_DATA').
    ASSIGN lr_logical_db_schema_old->* TO <ls_logical_db_schema_old>.
    ASSERT sy-subrc = 0.

    CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
      IMPORTING
        data = <ls_logical_db_schema_old>.

    ASSIGN COMPONENT 'METADATA-CREATED_BY' OF STRUCTURE cs_logical_db_schema
           TO <lv_created_by>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'METADATA-CREATED_AT' OF STRUCTURE cs_logical_db_schema
           TO <lv_created_at>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'METADATA-CREATED_BY' OF STRUCTURE <ls_logical_db_schema_old>
           TO <lv_created_by_old>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'METADATA-CREATED_AT' OF STRUCTURE <ls_logical_db_schema_old>
           TO <lv_created_at_old>.
    ASSERT sy-subrc = 0.

    <lv_created_at> = <lv_created_at_old>.
    <lv_created_by> = <lv_created_by_old>.

  ENDMETHOD.
  METHOD get_wb_object_operator.

    DATA:
      ls_object_type TYPE wbobjtype,
      lx_error       TYPE REF TO cx_root.

    IF mi_wb_object_operator IS BOUND.
      ri_wb_object_operator = mi_wb_object_operator.
    ENDIF.

    ls_object_type-objtype_tr = 'AMSD'.
    ls_object_type-subtype_wb = 'TYP'.

    TRY.
        CALL METHOD ('CL_WB_OBJECT_OPERATOR')=>('CREATE_INSTANCE')
          EXPORTING
            object_type = ls_object_type
            object_key  = mv_logical_db_schema_key
          RECEIVING
            result      = mi_wb_object_operator.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    ri_wb_object_operator = mi_wb_object_operator.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA:
      li_wb_object_operator TYPE REF TO object,
      li_object_data_model  TYPE REF TO if_wb_object_data_model,
      lx_error              TYPE REF TO cx_root.

    TRY.
        li_wb_object_operator = get_wb_object_operator( ).

        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
          IMPORTING
            eo_object_data = li_object_data_model.

        rv_user = li_object_data_model->get_changed_by( ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA:
      li_wb_object_operator TYPE REF TO object,
      lx_error              TYPE REF TO cx_root.

    li_wb_object_operator = get_wb_object_operator( ).

    TRY.
        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~DELETE')
          EXPORTING
            transport_request = iv_transport.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA:
      li_object_data_model  TYPE REF TO if_wb_object_data_model,
      li_wb_object_operator TYPE REF TO object,
      lx_error              TYPE REF TO cx_root.

    FIELD-SYMBOLS:
      <ls_logical_db_schema> TYPE any.

    ASSIGN mr_logical_db_schema->* TO <ls_logical_db_schema>.
    ASSERT sy-subrc = 0.

    io_xml->read(
      EXPORTING
        iv_name = 'AMSD'
      CHANGING
        cg_data = <ls_logical_db_schema> ).

    li_wb_object_operator = get_wb_object_operator( ).

    TRY.
        CREATE OBJECT li_object_data_model TYPE ('CL_AMDP_SCHEMA_OBJECT_DATA').

        tadir_insert( iv_package ).

        IF Lif_abapgit_object~exists( ) = abap_true.

          " We need to populate created_at, created_by, because otherwise update  is not possible
          fill_metadata_from_db( CHANGING cs_logical_db_schema = <ls_logical_db_schema> ).
          li_object_data_model->set_data( <ls_logical_db_schema> ).

          CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
            EXPORTING
              io_object_data    = li_object_data_model
              transport_request = iv_transport.

        ELSE.

          li_object_data_model->set_data( <ls_logical_db_schema> ).

          CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~CREATE')
            EXPORTING
              io_object_data    = li_object_data_model
              data_selection    = 'P' " if_wb_object_data_selection_co=>c_properties
              package           = iv_package
              transport_request = iv_transport.

          CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
            EXPORTING
              io_object_data    = li_object_data_model
              data_selection    = 'D' " if_wb_object_data_selection_co=>c_data_content
              transport_request = iv_transport.

        ENDIF.

        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~ACTIVATE').

        corr_insert( iv_package ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    TRY.
        mi_persistence->get(
            p_object_key           = mv_logical_db_schema_key
            p_version              = 'A'
            p_existence_check_only = abap_true ).
        rv_bool = abap_true.

      CATCH cx_swb_exception.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESWB_EO'
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA:
      li_object_data_model  TYPE REF TO if_wb_object_data_model,
      lx_error              TYPE REF TO cx_root,
      li_wb_object_operator TYPE REF TO object.

    FIELD-SYMBOLS:
      <ls_logical_db_schema> TYPE any.

    ASSIGN mr_logical_db_schema->* TO <ls_logical_db_schema>.
    ASSERT sy-subrc = 0.

    li_wb_object_operator = get_wb_object_operator( ).

    TRY.
        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
          EXPORTING
            version        = 'A'
          IMPORTING
            data           = <ls_logical_db_schema>
            eo_object_data = li_object_data_model.

        clear_fields( CHANGING cs_logical_db_schema = <ls_logical_db_schema> ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    io_xml->add(
        iv_name = 'AMSD'
        ig_data = <ls_logical_db_schema> ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_AMSD implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_VCLS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_vcls=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_vcls=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_VCLS implementation.
*"* method's implementations
*include methods.
  METHOD is_locked.

    DATA:
      ls_rstable_key TYPE rstable, " Lock argument for table RSTABLE
      lv_argument    TYPE eqegraarg.

    " Set Values for generic table lock
    ls_rstable_key-tabname = iv_tabname.
    ls_rstable_key-varkey  = iv_argument.

    " include all sub keys
    lv_argument = ls_rstable_key.
    lv_argument = lv_argument && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object         = 'E_TABLEE'
                                            iv_argument            = lv_argument ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    SELECT SINGLE author FROM vcldir INTO rv_user
      WHERE vclname = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.
* Do the same as in VIEWCLUSTER_SAVE_DEFINITION
    DATA: lv_vclname TYPE vcl_name.


    lv_vclname = ms_item-obj_name.

    DELETE FROM vcldir WHERE vclname = lv_vclname.        "#EC CI_SUBRC
    DELETE FROM vcldirt WHERE vclname = lv_vclname. "#EC CI_NOFIRST "#EC CI_SUBRC
    DELETE FROM vclstruc WHERE vclname = lv_vclname.      "#EC CI_SUBRC
    DELETE FROM vclstruct WHERE vclname = lv_vclname. "#EC CI_NOFIRST "#EC CI_SUBRC
    DELETE FROM vclstrudep WHERE vclname = lv_vclname.    "#EC CI_SUBRC
    DELETE FROM vclmf WHERE vclname = lv_vclname.         "#EC CI_SUBRC

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_vcldir_entry TYPE v_vcldir,
          lt_vclstruc     TYPE TABLE OF v_vclstruc,
          lt_vclstrudep   TYPE TABLE OF v_vclstdep,
          lt_vclmf        TYPE TABLE OF v_vclmf,
          lv_objectname   TYPE ob_object.


    io_xml->read( EXPORTING iv_name = 'VCLDIR'
                  CHANGING cg_data = ls_vcldir_entry ).
    io_xml->read( EXPORTING iv_name = 'VLCSTRUC_TAB'
                  CHANGING cg_data = lt_vclstruc ).
    io_xml->read( EXPORTING iv_name = 'VCLSTRUDEP_TAB'
                  CHANGING cg_data = lt_vclstrudep ).
    io_xml->read( EXPORTING iv_name = 'lt_vclstrudep'
                  CHANGING cg_data = lt_vclmf ).

    ls_vcldir_entry-author = sy-uname.
    ls_vcldir_entry-changedate = sy-datum.

    CALL FUNCTION 'VIEWCLUSTER_SAVE_DEFINITION'
      EXPORTING
        vcldir_entry   = ls_vcldir_entry
      TABLES
        vclstruc_tab   = lt_vclstruc
        vclstrudep_tab = lt_vclstrudep
        vclmf_tab      = lt_vclmf.

    corr_insert( iv_package ).

    lv_objectname = ls_vcldir_entry-vclname.

    CALL FUNCTION 'OBJ_GENERATE'
      EXPORTING
        iv_objectname         = lv_objectname
        iv_objecttype         = c_cluster_type
        iv_maint_mode         = c_mode_insert
        iv_devclass           = iv_package
      EXCEPTIONS
        illegal_call          = 1
        object_not_found      = 2
        generate_error        = 3
        transport_error       = 4
        object_enqueue_failed = 5
        OTHERS                = 6.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA lv_changedate TYPE vcldir-changedate.

    SELECT SINGLE changedate INTO lv_changedate FROM vcldir
      WHERE vclname = ms_item-obj_name.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.

    DATA lv_changedate TYPE vcldir-changedate.

    SELECT SINGLE changedate INTO lv_changedate FROM vcldir
      WHERE vclname = ms_item-obj_name.

* see logic in function module VIEWCLUSTER_GET_DEFINITION
    rv_active = boolc( lv_changedate IS NOT INITIAL ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    DATA:
      lv_argument       TYPE seqg3-garg,
      lv_argument_langu TYPE seqg3-garg.

    lv_argument       = ms_item-obj_name.
    lv_argument_langu = |@{ ms_item-obj_name }|.

    "Check all relevant maintein tabeles for view clusters
    IF is_locked( iv_tabname = 'VCLDIR'
                  iv_argument = lv_argument ) = abap_true
        OR is_locked( iv_tabname = 'VCLDIRT'
                      iv_argument = lv_argument_langu ) = abap_true
        OR is_locked( iv_tabname = 'VCLSTRUC'
                      iv_argument = lv_argument )       = abap_true
        OR is_locked( iv_tabname = 'VCLSTRUCT'
                      iv_argument = lv_argument_langu ) = abap_true
        OR is_locked( iv_tabname = 'VCLMF'
                      iv_argument = lv_argument )       = abap_true.

      rv_is_locked = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: ls_bcdata TYPE bdcdata,
          lt_bcdata TYPE STANDARD TABLE OF bdcdata.

    ls_bcdata-program  = 'SAPMSVIM'.
    ls_bcdata-dynpro   = '0050'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam     = 'VIMDYNFLDS-VIEWNAME'.
    ls_bcdata-fval     = ms_item-obj_name.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam     = 'VIMDYNFLDS-STRUCT_MNT'.
    ls_bcdata-fval     = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=CLUS'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-program  = 'SAPMSVIM'.
    ls_bcdata-dynpro   = '0052 '.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam     = 'VIMDYNFLDS-VCLNAME'.
    ls_bcdata-fval     = ms_item-obj_name.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=CLSH'.
    APPEND ls_bcdata TO lt_bcdata.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SE54'
      it_bdcdata = lt_bcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_vclname      TYPE vcl_name,
          ls_vcldir_entry TYPE v_vcldir,
          lt_vclstruc     TYPE TABLE OF v_vclstruc,
          lt_vclstrudep   TYPE TABLE OF v_vclstdep,
          lt_vclmf        TYPE TABLE OF v_vclmf.


    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lv_vclname = ms_item-obj_name.

    CALL FUNCTION 'VIEWCLUSTER_GET_DEFINITION'
      EXPORTING
        vclname                = lv_vclname
      IMPORTING
        vcldir_entry           = ls_vcldir_entry
      TABLES
        vclstruc_tab           = lt_vclstruc
        vclstrudep_tab         = lt_vclstrudep
        vclmf_tab              = lt_vclmf
      EXCEPTIONS
        viewcluster_not_found  = 1
        incomplete_viewcluster = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    SORT lt_vclstrudep BY vclname object objfield.

    CLEAR ls_vcldir_entry-author.
    CLEAR ls_vcldir_entry-changedate.

    io_xml->add( iv_name = 'VCLDIR'
                 ig_data = ls_vcldir_entry ).
    io_xml->add( iv_name = 'VLCSTRUC_TAB'
                 ig_data = lt_vclstruc ).
    io_xml->add( iv_name = 'VCLSTRUDEP_TAB'
                 ig_data = lt_vclstrudep ).
    io_xml->add( iv_name = 'VCLMF_TAB'
                 ig_data = lt_vclmf ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_VCLS implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ECAT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ecat=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ecat=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ECAT implementation.
*"* method's implementations
*include methods.
  METHOD get_download.

    CREATE OBJECT ro_download TYPE Lcl_abapgit_ecatt_script_downl.

  ENDMETHOD.
  METHOD get_lock_object.

    rv_lock_object = 'E_ECATT'.

  ENDMETHOD.
  METHOD get_object_type.

    rv_object_type = cl_apl_ecatt_const=>obj_type_test_script.

  ENDMETHOD.
  METHOD get_upload.

    CREATE OBJECT ro_upload TYPE Lcl_abapgit_ecatt_script_upl.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ECAT implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ECSD <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ecsd=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ecsd=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ECSD implementation.
*"* method's implementations
*include methods.
  METHOD get_download.

    CREATE OBJECT ro_download TYPE Lcl_abapgit_ecatt_system_downl.

  ENDMETHOD.
  METHOD get_lock_object.

    rv_lock_object = 'E_ECATT_SD'.

  ENDMETHOD.
  METHOD get_object_type.

    rv_object_type = cl_apl_ecatt_const=>obj_type_system_data.

  ENDMETHOD.
  METHOD get_upload.

    CREATE OBJECT ro_upload TYPE Lcl_abapgit_ecatt_system_upl.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ECSD implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ECSP <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ecsp=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ecsp=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ECSP implementation.
*"* method's implementations
*include methods.
  METHOD get_download.

    CREATE OBJECT ro_download TYPE Lcl_abapgit_ecatt_sp_download.

  ENDMETHOD.
  METHOD get_lock_object.

    rv_lock_object = 'E_ECATT_SP'.

  ENDMETHOD.
  METHOD get_object_type.

* constant missing in 702, cl_apl_ecatt_const=>obj_type_start_profile
    rv_object_type = 'ECSP'.

  ENDMETHOD.
  METHOD get_upload.

    CREATE OBJECT ro_upload TYPE Lcl_abapgit_ecatt_sp_upload.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ECSP implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ECTC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ectc=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ectc=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ECTC implementation.
*"* method's implementations
*include methods.
  METHOD get_download.

    CREATE OBJECT ro_download TYPE Lcl_abapgit_ecatt_config_downl.

  ENDMETHOD.
  METHOD get_lock_object.

    rv_lock_object = 'E_ECATT_TC'.

  ENDMETHOD.
  METHOD get_object_type.

    rv_object_type = cl_apl_ecatt_const=>obj_type_test_config.

  ENDMETHOD.
  METHOD get_upload.

    CREATE OBJECT ro_upload TYPE Lcl_abapgit_ecatt_config_upl.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ECTC implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ECTD <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ectd=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ectd=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ECTD implementation.
*"* method's implementations
*include methods.
  METHOD get_download.

    CREATE OBJECT ro_download TYPE Lcl_abapgit_ecatt_data_downl.

  ENDMETHOD.
  METHOD get_lock_object.

    rv_lock_object = 'E_ECATT_TD'.

  ENDMETHOD.
  METHOD get_object_type.

    rv_object_type = cl_apl_ecatt_const=>obj_type_test_data.

  ENDMETHOD.
  METHOD get_upload.

    CREATE OBJECT ro_upload TYPE Lcl_abapgit_ecatt_data_upload.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ECTD implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ECVO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ecvo=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ecvo=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ECVO implementation.
*"* method's implementations
*include methods.
  METHOD get_download.

    CREATE OBJECT ro_download TYPE Lcl_abapgit_ecatt_val_obj_down.

  ENDMETHOD.
  METHOD get_lock_object.

    rv_lock_object = 'E_ECATT_TD'.

  ENDMETHOD.
  METHOD get_object_type.

* constant missing in 702, cl_apl_ecatt_const=>obj_type_ecatt_vo
    rv_object_type = 'ECVO'.

  ENDMETHOD.
  METHOD get_upload.

    CREATE OBJECT ro_upload TYPE Lcl_abapgit_ecatt_val_obj_upl.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ECVO implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ENHO_BADI <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_enho_badi==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_enho_badi==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ENHO_BADI implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    ms_item = is_item.
  ENDMETHOD.
  METHOD Lif_abapgit_object_enho~deserialize.

    DATA: lv_spot_name TYPE enhspotname,
          lv_shorttext TYPE string,
          lv_enhname   TYPE enhname,
          lo_badi      TYPE REF TO cl_enh_tool_badi_impl,
          li_tool      TYPE REF TO if_enh_tool,
          lv_package   TYPE devclass,
          lt_impl      TYPE enh_badi_impl_data_it,
          lx_enh_root  TYPE REF TO cx_enh_root.

    FIELD-SYMBOLS: <ls_impl> LIKE LINE OF lt_impl.

    ii_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING cg_data  = lv_shorttext ).
    ii_xml->read( EXPORTING iv_name = 'SPOT_NAME'
                  CHANGING cg_data  = lv_spot_name ).
    ii_xml->read( EXPORTING iv_name = 'IMPL'
                  CHANGING cg_data  = lt_impl ).

    lv_enhname = ms_item-obj_name.
    lv_package = iv_package.
    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname     = lv_enhname
            enhtype     = cl_abstract_enh_tool_redef=>credefinition
            enhtooltype = cl_enh_tool_badi_impl=>tooltype
          IMPORTING
            enhancement = li_tool
          CHANGING
            devclass    = lv_package ).
        lo_badi ?= li_tool.

        lo_badi->set_spot_name( lv_spot_name ).
        lo_badi->if_enh_object_docu~set_shorttext( lv_shorttext ).
        LOOP AT lt_impl ASSIGNING <ls_impl>.
          lo_badi->add_implementation( <ls_impl> ).
        ENDLOOP.
        lo_badi->if_enh_object~save( run_dark = abap_true ).
        lo_badi->if_enh_object~unlock( ).
      CATCH cx_enh_root INTO lx_enh_root.
        TRY.
            lo_badi->if_enh_object~unlock( ).
          CATCH cx_sy_ref_is_initial cx_enh_mod_not_allowed ##NO_HANDLER.
        ENDTRY.
        Lcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object_enho~serialize.

    DATA: lo_badi_impl TYPE REF TO cl_enh_tool_badi_impl,
          lv_spot_name TYPE enhspotname,
          lv_shorttext TYPE string,
          lt_impl      TYPE enh_badi_impl_data_it.

    FIELD-SYMBOLS: <ls_impl>   LIKE LINE OF lt_impl,
                   <ls_values> LIKE LINE OF <ls_impl>-filter_values,
                   <ls_filter> LIKE LINE OF <ls_impl>-filters.


    lo_badi_impl ?= ii_enh_tool.

    lv_shorttext = lo_badi_impl->if_enh_object_docu~get_shorttext( ).
    lv_spot_name = lo_badi_impl->get_spot_name( ).
    lt_impl      = lo_badi_impl->get_implementations( ).

    LOOP AT lt_impl ASSIGNING <ls_impl>.
* make sure the XML serialization does not dump, field type = N
      LOOP AT <ls_impl>-filter_values ASSIGNING <ls_values>.
        IF <ls_values>-filter_numeric_value1 CA space.
          CLEAR <ls_values>-filter_numeric_value1.
        ENDIF.
      ENDLOOP.
      LOOP AT <ls_impl>-filters ASSIGNING <ls_filter>.
        IF <ls_filter>-filter_numeric_value1 CA space.
          CLEAR <ls_filter>-filter_numeric_value1.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    ii_xml->add( iv_name = 'TOOL'
                 ig_data = ii_enh_tool->get_tool( ) ).
    ii_xml->add( ig_data = lv_shorttext
                 iv_name = 'SHORTTEXT' ).
    ii_xml->add( iv_name = 'SPOT_NAME'
                 ig_data = lv_spot_name ).
    ii_xml->add( iv_name = 'IMPL'
                 ig_data = lt_impl ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENHO_BADI implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_AVAR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_avar=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_avar=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_AVAR implementation.
*"* method's implementations
*include methods.
  METHOD create_object.

    DATA: lv_name TYPE aab_var_name.

    lv_name = ms_item-obj_name.

    CREATE OBJECT ro_aab_var
      EXPORTING
        im_name          = lv_name
        im_local         = ''
      EXCEPTIONS
        name_not_allowed = 1
        user_not_valid   = 2
        no_authorization = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lo_aab TYPE REF TO cl_aab_variant.

    lo_aab = create_object( ).
    lo_aab->get_author( IMPORTING ex_author = rv_user ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lo_aab TYPE REF TO cl_aab_variant.

    lo_aab = create_object( ).
    lo_aab->enqueue( ).
    lo_aab->delete(
      EXCEPTIONS
        var_not_found    = 1
        prop_error       = 2
        propt_error      = 3
        var_id_error     = 4
        no_authorization = 5
        cts_error        = 6
        cts_devclass     = 7
        OTHERS           = 8 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error deleting AVAR { ms_item-obj_name }| ).
    ENDIF.
    lo_aab->dequeue( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_possible    TYPE abap_bool,
          lv_description TYPE aab_var_descript,
          ls_is          TYPE aab_var_obj_act,
          lt_ids         TYPE aab_var_obj_act_tab,
          lo_aab         TYPE REF TO cl_aab_variant.

    " AVAR can only be created in transportable packages
    lv_possible = Lcl_abapgit_factory=>get_sap_package( iv_package )->are_changes_recorded_in_tr_req( ).
    IF lv_possible = abap_false.
      Lcx_abapgit_exception=>raise( |Global activation variants require a transportable package| ).
    ENDIF.

    " Create AVAR with description and object (id) list
    io_xml->read( EXPORTING iv_name = 'DESCRIPTION'
                  CHANGING  cg_data = lv_description ).

    io_xml->read( EXPORTING iv_name = 'IDS'
                  CHANGING  cg_data = lt_ids ).

    lo_aab = create_object( ).
    lo_aab->enqueue( ).
    lo_aab->set_descript(
      EXPORTING
        im_descript      = lv_description
      EXCEPTIONS
        no_authorization = 1 ).
    IF sy-subrc <> 0.
      lo_aab->dequeue( ).
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    LOOP AT lt_ids INTO ls_is.
      lo_aab->set_id(
        EXPORTING
          im_name              = ls_is-name
          im_object            = ls_is-object
          im_actmode           = ls_is-actmode
        EXCEPTIONS
          no_authorization     = 1
          id_not_exists        = 2
          id_not_transportable = 3
          OTHERS               = 4 ).
      IF sy-subrc <> 0.
        lo_aab->dequeue( ).
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDLOOP.

    tadir_insert( iv_package ).

    lo_aab->save(
      EXCEPTIONS
        no_descript_specified = 1
        prop_error            = 2
        propt_error           = 3
        var_id_error          = 4
        no_changes_found      = 5
        cts_error             = 6 ).
    IF sy-subrc <> 0.
      lo_aab->dequeue( ).
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.
    lo_aab->dequeue( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_state TYPE abap_bool,
          lo_aab   TYPE REF TO cl_aab_variant.

    lo_aab = create_object( ).

    lo_aab->get_state( IMPORTING ex_state = lv_state ).
    rv_bool = boolc( lv_state = abap_true ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lo_aab         TYPE REF TO cl_aab_variant,
          lt_ids         TYPE aab_var_obj_act_tab,
          lv_description TYPE aab_var_descript.

    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lo_aab = create_object( ).

    lo_aab->get_descript(
      IMPORTING
        ex_descript = lv_description
      EXCEPTIONS
        no_descript_found = 1 ).
    IF sy-subrc = 0.
      io_xml->add( iv_name = 'DESCRIPTION'
                   ig_data = lv_description ).
    ENDIF.

    lo_aab->get_ids( IMPORTING ex_ids = lt_ids ).

    io_xml->add( iv_name = 'IDS'
                 ig_data = lt_ids ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_AVAR implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ENHO_CLIF <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_enho_clif==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_enho_clif==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ENHO_CLIF implementation.
*"* method's implementations
*include methods.
  METHOD deserialize.

    DATA: lt_tab_attributes TYPE enhclasstabattrib,
          lt_tab_types      TYPE enhtype_tab,
          lt_tab_methods    TYPE enhnewmeth_tab,
          ls_type_line      TYPE vseotype,
          ls_header         TYPE vseomethod,
          ls_param          TYPE vseomepara,
          ls_exc            TYPE vseoexcep,
          lt_tab_eventdata  TYPE enhevent_tab,
          ls_event_line     TYPE vseoevent,
          ls_event_param    TYPE vseoeparam.

    FIELD-SYMBOLS: <ls_type>        LIKE LINE OF lt_tab_types,
                   <ls_method>      LIKE LINE OF lt_tab_methods,
                   <ls_param>       LIKE LINE OF <ls_method>-meth_param,
                   <ls_event>       LIKE LINE OF lt_tab_eventdata,
                   <ls_exc>         LIKE LINE OF <ls_method>-meth_exc,
                   <ls_event_param> LIKE LINE OF <ls_event>-event_param.


    io_xml->read( EXPORTING iv_name = 'TAB_ATTRIBUTES'
                  CHANGING cg_data = lt_tab_attributes ).
    io_xml->read( EXPORTING iv_name = 'TAB_TYPES'
                  CHANGING cg_data = lt_tab_types ).
    io_xml->read( EXPORTING iv_name = 'TAB_METHODS'
                  CHANGING cg_data = lt_tab_methods ).
    io_xml->read( EXPORTING iv_name = 'TAB_EVENTDATA'
                  CHANGING cg_data = lt_tab_eventdata ).

    LOOP AT lt_tab_types ASSIGNING <ls_type>.
      MOVE-CORRESPONDING <ls_type> TO ls_type_line.
      TRY.
          io_clif->add_change_enha_type( type_line = ls_type_line ).
        CATCH cx_enh_mod_not_allowed
        cx_enh_is_not_enhanceable.
          " TODO
      ENDTRY.
    ENDLOOP.

    io_clif->set_enhattributes( lt_tab_attributes ).

* SAP standard SET_ENH_NEW_METHOS does not work

    LOOP AT lt_tab_methods ASSIGNING <ls_method>.

      MOVE-CORRESPONDING <ls_method>-meth_header TO ls_header.

      io_clif->add_change_new_enh_method(
        methkey       = <ls_method>-methkey
        method_header = ls_header ).

* parameters
      LOOP AT <ls_method>-meth_param ASSIGNING <ls_param>.
        MOVE-CORRESPONDING <ls_param> TO ls_param.
        io_clif->add_change_enh_methparam(
          methname   = <ls_method>-methkey-cmpname
          param_line = ls_param ).
      ENDLOOP.

* exceptions
      LOOP AT <ls_method>-meth_exc ASSIGNING <ls_exc>.
        MOVE-CORRESPONDING <ls_exc> TO ls_exc.
        io_clif->add_change_enh_methexc(
          methname    = <ls_method>-methkey-cmpname
          except_line = ls_exc ).
      ENDLOOP.

    ENDLOOP.

    " events are renumbered based on
    LOOP AT lt_tab_eventdata ASSIGNING <ls_event>.

      MOVE-CORRESPONDING <ls_event>-event_header TO ls_event_line.

      io_clif->add_change_enha_event(
        event_key  = <ls_event>-eventkey
        event_line = ls_event_line ).

* parameters
      LOOP AT <ls_event>-event_param ASSIGNING <ls_event_param>.
        MOVE-CORRESPONDING <ls_event_param> TO ls_event_param.
        io_clif->add_change_enh_eventparam(
          eventname   = <ls_event>-eventkey-cmpname
          event_param = ls_event_param ).
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.
  METHOD serialize.

    DATA: lt_tab_attributes TYPE enhclasstabattrib,
          lt_tab_types      TYPE enhtype_tab,
          lt_tab_methods    TYPE enhnewmeth_tab,
          lt_tab_eventdata  TYPE enhevent_tab,
          lv_editorder      TYPE i.

    FIELD-SYMBOLS: <ls_attr>        LIKE LINE OF lt_tab_attributes,
                   <ls_type>        LIKE LINE OF lt_tab_types,
                   <ls_meth>        LIKE LINE OF lt_tab_methods,
                   <ls_param>       LIKE LINE OF <ls_meth>-meth_param,
                   <ls_exc>         LIKE LINE OF <ls_meth>-meth_exc,
                   <ls_event>       LIKE LINE OF lt_tab_eventdata,
                   <ls_event_param> LIKE LINE OF <ls_event>-event_param.


    io_clif->get_enhattributes( IMPORTING tab_attributes = lt_tab_attributes ).

    io_clif->get_enhatypes( IMPORTING tab_types = lt_tab_types ).

    io_clif->get_enh_new_methodes( IMPORTING tab_methodes = lt_tab_methods ).

    io_clif->get_enhevents( IMPORTING tab_eventdata = lt_tab_eventdata ).

    LOOP AT lt_tab_attributes ASSIGNING <ls_attr>.
      CLEAR: <ls_attr>-author,
             <ls_attr>-createdon,
             <ls_attr>-changedby,
             <ls_attr>-changedon,
             <ls_attr>-descript_id.
    ENDLOOP.

    LOOP AT lt_tab_types ASSIGNING <ls_type>.
      CLEAR: <ls_type>-author,
             <ls_type>-createdon,
             <ls_type>-changedby,
             <ls_type>-changedon,
             <ls_type>-descript_id.
    ENDLOOP.

    lv_editorder = 0.
    SORT lt_tab_methods BY meth_header-editorder.
    LOOP AT lt_tab_methods ASSIGNING <ls_meth>.
      CLEAR: <ls_meth>-meth_header-author,
             <ls_meth>-meth_header-createdon,
             <ls_meth>-meth_header-changedby,
             <ls_meth>-meth_header-changedon,
             <ls_meth>-meth_header-descript_id.
      lv_editorder = lv_editorder + 1.
      <ls_meth>-meth_header-editorder = lv_editorder.
      LOOP AT <ls_meth>-meth_param ASSIGNING <ls_param>.
        CLEAR: <ls_param>-author,
               <ls_param>-createdon,
               <ls_param>-changedby,
               <ls_param>-changedon,
               <ls_param>-descript_id.
      ENDLOOP.
      LOOP AT <ls_meth>-meth_exc ASSIGNING <ls_exc>.
        CLEAR: <ls_exc>-author,
               <ls_exc>-createdon,
               <ls_exc>-changedby,
               <ls_exc>-changedon,
               <ls_exc>-descript_id.
      ENDLOOP.
    ENDLOOP.

    LOOP AT lt_tab_eventdata ASSIGNING <ls_event>.
      CLEAR: <ls_event>-event_header-author,
             <ls_event>-event_header-createdon,
             <ls_event>-event_header-changedby,
             <ls_event>-event_header-changedon,
             <ls_event>-event_header-descript_id.
      LOOP AT <ls_event>-event_param ASSIGNING <ls_event_param>.
        CLEAR: <ls_event_param>-author,
               <ls_event_param>-createdon,
               <ls_event_param>-changedby,
               <ls_event_param>-changedon,
               <ls_event_param>-descript_id.
      ENDLOOP.
    ENDLOOP.

    io_xml->add( iv_name = 'TAB_ATTRIBUTES'
                 ig_data = lt_tab_attributes ).
    io_xml->add( iv_name = 'TAB_TYPES'
                 ig_data = lt_tab_types ).
    io_xml->add( iv_name = 'TAB_METHODS'
                 ig_data = lt_tab_methods ).
    io_xml->add( iv_name = 'TAB_EVENTDATA'
                 ig_data = lt_tab_eventdata ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENHO_CLIF implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ENHO_FUGR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_enho_fugr==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_enho_fugr==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ENHO_FUGR implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    ms_item = is_item.
    mo_files = io_files.
  ENDMETHOD.
  METHOD Lif_abapgit_object_enho~deserialize.

    DATA: lo_fugrdata  TYPE REF TO cl_enh_tool_fugr,
          ls_enha_data TYPE enhfugrdata,
          li_tool      TYPE REF TO if_enh_tool,
          lv_tool      TYPE enhtooltype,
          lv_package   TYPE devclass,
          lx_enh_root  TYPE REF TO cx_enh_root.

    FIELD-SYMBOLS: <ls_fuba> TYPE enhfugrfuncdata.

    ii_xml->read(
      EXPORTING
        iv_name = 'TOOL'
      CHANGING
        cg_data = lv_tool ).

    ii_xml->read(
      EXPORTING
        iv_name = 'FUGRDATA'
      CHANGING
        cg_data = ls_enha_data ).

    lv_package = iv_package.

    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname     = |{ ms_item-obj_name }|
            enhtype     = ''
            enhtooltype = lv_tool
          IMPORTING
            enhancement = li_tool
          CHANGING
            devclass    = lv_package ).

        lo_fugrdata ?= li_tool.

        lo_fugrdata->set_fugr( ls_enha_data-fugr ).

        LOOP AT ls_enha_data-enh_fubas ASSIGNING <ls_fuba>.

          lo_fugrdata->set_func_data( func_name     = <ls_fuba>-fuba
                                      func_enhadata = <ls_fuba> ).

        ENDLOOP.

        lo_fugrdata->if_enh_object~save( run_dark = abap_true ).
        lo_fugrdata->if_enh_object~unlock( ).
      CATCH cx_enh_root INTO lx_enh_root.
        TRY.
            lo_fugrdata->if_enh_object~unlock( ).
          CATCH cx_sy_ref_is_initial cx_enh_mod_not_allowed ##NO_HANDLER.
        ENDTRY.
        Lcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object_enho~serialize.

    DATA: lo_fugrdata  TYPE REF TO cl_enh_tool_fugr,
          lv_fugr_name TYPE rs38l-area,
          ls_enha_data TYPE enhfugrdata.

    FIELD-SYMBOLS: <ls_docuobj> TYPE enhfugrparamdocu.


    lo_fugrdata ?= ii_enh_tool.

    lo_fugrdata->get_fugr( IMPORTING fugr_name = lv_fugr_name ).

    TRY.
        lo_fugrdata->get_all_data_for_fugr(
          EXPORTING
            fugr_name = lv_fugr_name
          IMPORTING
            enha_data = ls_enha_data ).

        LOOP AT ls_enha_data-docuobjs ASSIGNING <ls_docuobj>.
          CLEAR: <ls_docuobj>-shorttext,
                 <ls_docuobj>-longtext.
        ENDLOOP.

      CATCH cx_enh_not_found.
        Lcx_abapgit_exception=>raise( |error deserializing ENHO fugrdata { ms_item-obj_name }| ).
    ENDTRY.

    ii_xml->add( iv_name = 'TOOL'
                 ig_data = lo_fugrdata->if_enh_tool~get_tool( ) ).

    ii_xml->add( iv_name = 'FUGRDATA'
                 ig_data = ls_enha_data ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENHO_FUGR implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ENHO_HOOK <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_enho_hook==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_enho_hook==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ENHO_HOOK implementation.
*"* method's implementations
*include methods.
  METHOD add_sources.

    DATA lv_source TYPE string.
    DATA ls_file LIKE LINE OF ct_files.

    FIELD-SYMBOLS <ls_enhancement> LIKE LINE OF ct_enhancements.

    LOOP AT ct_enhancements ASSIGNING <ls_enhancement>.
      " Use hash as filename since full_name is very long
      CLEAR ls_file.
      ls_file-name = <ls_enhancement>-full_name.
      ls_file-file = substring(
        val = Lcl_abapgit_hash=>sha1_string( <ls_enhancement>-full_name )
        len = 8 ).
      INSERT ls_file INTO TABLE ct_files.

      " Add full name as comment and put code between enhancement statements
      lv_source = c_enhancement.
      REPLACE '*' IN lv_source WITH ms_item-obj_name.
      INSERT lv_source INTO <ls_enhancement>-source INDEX 1.

      lv_source = |"Name: { <ls_enhancement>-full_name }|.
      INSERT lv_source INTO <ls_enhancement>-source INDEX 1.

      APPEND c_endenhancement TO <ls_enhancement>-source.

      mo_files->add_abap( iv_extra = ls_file-file
                          it_abap  = <ls_enhancement>-source ).

      CLEAR <ls_enhancement>-source.
    ENDLOOP.

  ENDMETHOD.
  METHOD constructor.
    ms_item = is_item.
    mo_files = io_files.
  ENDMETHOD.
  METHOD hook_impl_deserialize.

    FIELD-SYMBOLS: <ls_impl>   LIKE LINE OF ct_impl,
                   <lv_line>   TYPE string,
                   <lv_space>  TYPE i,
                   <ls_spaces> LIKE LINE OF it_spaces.


    LOOP AT ct_impl ASSIGNING <ls_impl>.
      READ TABLE it_spaces ASSIGNING <ls_spaces> WITH KEY full_name = <ls_impl>-full_name.
      IF sy-subrc = 0.
        LOOP AT <ls_impl>-source ASSIGNING <lv_line>.
          READ TABLE <ls_spaces>-spaces ASSIGNING <lv_space> INDEX sy-tabix.
          IF sy-subrc = 0 AND <lv_space> > 0.
            DO <lv_space> TIMES.
              CONCATENATE space <lv_line> INTO <lv_line> RESPECTING BLANKS.
            ENDDO.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD read_sources.

    DATA lv_source TYPE string.
    DATA ls_file LIKE LINE OF ct_files.
    DATA lv_from TYPE i.
    DATA lv_to TYPE i.

    FIELD-SYMBOLS <ls_enhancement> LIKE LINE OF ct_enhancements.

    LOOP AT ct_enhancements ASSIGNING <ls_enhancement>.
      READ TABLE ct_files INTO ls_file WITH TABLE KEY name = <ls_enhancement>-full_name.
      IF sy-subrc = 0.
        <ls_enhancement>-source = mo_files->read_abap( iv_extra = ls_file-file ).
        " Get code between enhancement statements
        LOOP AT <ls_enhancement>-source INTO lv_source.
          IF lv_source CP c_enhancement.
            lv_from = sy-tabix.
          ENDIF.
          IF lv_source CP c_endenhancement.
            lv_to = sy-tabix.
          ENDIF.
        ENDLOOP.
        DELETE <ls_enhancement>-source FROM lv_to.
        DELETE <ls_enhancement>-source TO lv_from.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_object_enho~deserialize.

    DATA: lv_shorttext       TYPE string,
          lo_hook_impl       TYPE REF TO cl_enh_tool_hook_impl,
          li_tool            TYPE REF TO if_enh_tool,
          lv_enhname         TYPE enhname,
          lv_package         TYPE devclass,
          ls_original_object TYPE enh_hook_admin,
          lt_spaces          TYPE ty_spaces_tt,
          lt_files           TYPE ty_files,
          lt_enhancements    TYPE enh_hook_impl_it,
          lx_enh_root        TYPE REF TO cx_enh_root.

    FIELD-SYMBOLS: <ls_enhancement> LIKE LINE OF lt_enhancements.


    ii_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING cg_data  = lv_shorttext ).
    ii_xml->read( EXPORTING iv_name = 'ORIGINAL_OBJECT'
                  CHANGING cg_data  = ls_original_object ).
    ii_xml->read( EXPORTING iv_name = 'ENHANCEMENTS'
                  CHANGING cg_data  = lt_enhancements ).
    ii_xml->read( EXPORTING iv_name = 'FILES'
                  CHANGING cg_data  = lt_files ).
    ii_xml->read( EXPORTING iv_name = 'SPACES'
                  CHANGING cg_data  = lt_spaces ).

    " todo: kept for compatibility, remove after grace period #3680
    hook_impl_deserialize( EXPORTING it_spaces = lt_spaces
                           CHANGING ct_impl    = lt_enhancements ).

    read_sources( CHANGING ct_enhancements = lt_enhancements
                           ct_files        = lt_files ).

    lv_enhname = ms_item-obj_name.
    lv_package = iv_package.
    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname     = lv_enhname
            enhtype     = cl_abstract_enh_tool_redef=>credefinition
            enhtooltype = cl_enh_tool_hook_impl=>tooltype
          IMPORTING
            enhancement = li_tool
          CHANGING
            devclass    = lv_package ).
        lo_hook_impl ?= li_tool.

        lo_hook_impl->if_enh_object_docu~set_shorttext( lv_shorttext ).
        lo_hook_impl->set_original_object(
            pgmid       = ls_original_object-pgmid
            obj_name    = ls_original_object-org_obj_name
            obj_type    = ls_original_object-org_obj_type
            program     = ls_original_object-programname
            main_type   = ls_original_object-org_main_type
            main_name   = ls_original_object-org_main_name ).
        lo_hook_impl->set_include_bound( ls_original_object-include_bound ).

        LOOP AT lt_enhancements ASSIGNING <ls_enhancement>.
          lo_hook_impl->add_hook_impl(
              overwrite        = <ls_enhancement>-overwrite
              method           = <ls_enhancement>-method
              enhmode          = <ls_enhancement>-enhmode
              full_name        = <ls_enhancement>-full_name
              source           = <ls_enhancement>-source
              spot             = <ls_enhancement>-spotname
              parent_full_name = <ls_enhancement>-parent_full_name ).
        ENDLOOP.
        lo_hook_impl->if_enh_object~save( run_dark = abap_true ).
        lo_hook_impl->if_enh_object~unlock( ).
      CATCH cx_enh_root INTO lx_enh_root.
        TRY.
            lo_hook_impl->if_enh_object~unlock( ).
          CATCH cx_sy_ref_is_initial cx_enh_mod_not_allowed ##NO_HANDLER.
        ENDTRY.
        Lcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object_enho~serialize.

    DATA: lv_shorttext       TYPE string,
          lo_hook_impl       TYPE REF TO cl_enh_tool_hook_impl,
          ls_original_object TYPE enh_hook_admin,
          lt_spaces          TYPE ty_spaces_tt,
          lt_files           TYPE ty_files,
          lt_enhancements    TYPE enh_hook_impl_it.

    FIELD-SYMBOLS: <ls_enhancement> LIKE LINE OF lt_enhancements.

    lo_hook_impl ?= ii_enh_tool.

    lv_shorttext = lo_hook_impl->if_enh_object_docu~get_shorttext( ).
    lo_hook_impl->get_original_object(
      IMPORTING
        pgmid     = ls_original_object-pgmid
        obj_name  = ls_original_object-org_obj_name
        obj_type  = ls_original_object-org_obj_type
        main_type = ls_original_object-org_main_type
        main_name = ls_original_object-org_main_name
        program   = ls_original_object-programname ).
    ls_original_object-include_bound = lo_hook_impl->get_include_bound( ).
    lt_enhancements = lo_hook_impl->get_hook_impls( ).

    LOOP AT lt_enhancements ASSIGNING <ls_enhancement>.
      CLEAR: <ls_enhancement>-extid,
             <ls_enhancement>-id.
    ENDLOOP.

    add_sources( CHANGING ct_enhancements = lt_enhancements
                          ct_files        = lt_files ).

    ii_xml->add( iv_name = 'TOOL'
                 ig_data = ii_enh_tool->get_tool( ) ).
    ii_xml->add( iv_name = 'SHORTTEXT'
                 ig_data = lv_shorttext ).
    ii_xml->add( iv_name = 'ORIGINAL_OBJECT'
                 ig_data = ls_original_object ).
    ii_xml->add( iv_name = 'ENHANCEMENTS'
                 ig_data = lt_enhancements ).
    ii_xml->add( iv_name = 'FILES'
                 ig_data = lt_files ).
    ii_xml->add( iv_name = 'SPACES'
                 ig_data = lt_spaces ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENHO_HOOK implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ENHO_INTF <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_enho_intf==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_enho_intf==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ENHO_INTF implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    ms_item  = is_item.
    mo_files = io_files.
  ENDMETHOD.
  METHOD Lif_abapgit_object_enho~deserialize.

    DATA: lo_enh_intf  TYPE REF TO cl_enh_tool_intf,
          li_tool      TYPE REF TO if_enh_tool,
          lv_shorttext TYPE string,
          lv_class     TYPE seoclsname,
          lv_enhname   TYPE enhname,
          lv_package   TYPE devclass,
          lx_enh_root  TYPE REF TO cx_enh_root.

    ii_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING cg_data  = lv_shorttext ).
    ii_xml->read( EXPORTING iv_name = 'CLASS'
                  CHANGING cg_data  = lv_class ).

    lv_enhname = ms_item-obj_name.
    lv_package = iv_package.
    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname     = lv_enhname
            enhtype     = ''
            enhtooltype = cl_enh_tool_intf=>tooltype
          IMPORTING
            enhancement = li_tool
          CHANGING
            devclass    = lv_package ).
        lo_enh_intf ?= li_tool.

        lo_enh_intf->if_enh_object_docu~set_shorttext( lv_shorttext ).
        lo_enh_intf->set_class( lv_class ).

        Lcl_abapgit_object_enho_clif=>deserialize(
          io_xml  = ii_xml
          io_clif = lo_enh_intf ).

        lo_enh_intf->if_enh_object~save( run_dark = abap_true ).
        lo_enh_intf->if_enh_object~unlock( ).
      CATCH cx_enh_root INTO lx_enh_root.
        TRY.
            lo_enh_intf->if_enh_object~unlock( ).
          CATCH cx_sy_ref_is_initial cx_enh_mod_not_allowed ##NO_HANDLER.
        ENDTRY.
        Lcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object_enho~serialize.

    DATA: lo_enh_intf  TYPE REF TO cl_enh_tool_intf,
          lv_class     TYPE seoclsname,
          lv_shorttext TYPE string.


    lo_enh_intf ?= ii_enh_tool.

    lv_shorttext = lo_enh_intf->if_enh_object_docu~get_shorttext( ).
    lo_enh_intf->get_class( IMPORTING class_name = lv_class ).

    ii_xml->add( iv_name = 'TOOL'
                 ig_data = ii_enh_tool->get_tool( ) ).
    ii_xml->add( ig_data = lv_shorttext
                 iv_name = 'SHORTTEXT' ).
    ii_xml->add( iv_name = 'CLASS'
                 ig_data = lv_class ).

    Lcl_abapgit_object_enho_clif=>serialize(
      io_xml  = ii_xml
      io_clif = lo_enh_intf ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENHO_INTF implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ENHO_WDYC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_enho_wdyc==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_enho_wdyc==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ENHO_WDYC implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    ms_item = is_item.
  ENDMETHOD.
  METHOD Lif_abapgit_object_enho~deserialize.

    DATA: lv_enhname TYPE enhname,
          lo_wdyconf TYPE REF TO cl_wdr_cfg_enhancement,
          li_tool    TYPE REF TO if_enh_tool,
          ls_obj     TYPE wdy_config_key,
          lv_xml     TYPE string,
          lt_data    TYPE wdy_cfg_expl_data_tab,
          lv_package TYPE devclass.

    ii_xml->read( EXPORTING iv_name = 'ORIGINAL_OBJECT'
                  CHANGING  cg_data = ls_obj ).

    ii_xml->read( EXPORTING iv_name = 'ENHANCEMENT_DATA'
                  CHANGING  cg_data = lv_xml ).

    lv_enhname = ms_item-obj_name.
    lv_package = iv_package.
    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname     = lv_enhname
            enhtype     = ''
            enhtooltype = cl_wdr_cfg_enhancement=>tooltype
          IMPORTING
            enhancement = li_tool
          CHANGING
            devclass    = lv_package ).

        lo_wdyconf ?= li_tool.

        CALL METHOD cl_wdr_cfg_persistence_utils=>('COMP_XML_TO_TABLES')
          EXPORTING
            xml_content   = lv_xml
          IMPORTING
            expl_data_tab = lt_data.

* only works on new ABAP versions, parameters differ between versions
        CALL METHOD lo_wdyconf->('SET_ENHANCEMENT_DATA')
          EXPORTING
            p_enh_data = lt_data.

        lo_wdyconf->if_enh_object~save( run_dark = abap_true ).
        lo_wdyconf->if_enh_object~unlock( ).
      CATCH cx_enh_root cx_static_check.
        TRY.
            lo_wdyconf->if_enh_object~unlock( ).
          CATCH cx_sy_ref_is_initial cx_enh_mod_not_allowed ##NO_HANDLER.
        ENDTRY.
        Lcx_abapgit_exception=>raise( 'error deserializing ENHO wdyconf' ).
    ENDTRY.
  ENDMETHOD.
  METHOD Lif_abapgit_object_enho~serialize.

    DATA: lo_wdyconf  TYPE REF TO cl_wdr_cfg_enhancement,
          lt_data     TYPE wdy_cfg_expl_data_tab,
          ls_outline  TYPE wdy_cfg_outline_data,
          ls_obj      TYPE wdy_config_key,
          li_document TYPE REF TO if_ixml_document,
          li_element  TYPE REF TO if_ixml_element.


    lo_wdyconf ?= ii_enh_tool.

    ls_obj = lo_wdyconf->get_original_object( ).
    ii_xml->add( iv_name = 'TOOL'
                 ig_data = ii_enh_tool->get_tool( ) ).
    ii_xml->add( iv_name = 'ORIGINAL_OBJECT'
                 ig_data = ls_obj ).

* only works on new ABAP versions, parameters differ between versions
    CALL METHOD lo_wdyconf->('GET_ENHANCEMENT_DATA')
      EXPORTING
        p_scope    = 1
      IMPORTING
        p_enh_data = lt_data.

    CALL METHOD cl_wdr_cfg_persistence_utils=>('COMP_TABLES_TO_XML')
      EXPORTING
        outline_data  = ls_outline
        expl_data_tab = lt_data
      IMPORTING
        element       = li_element
      CHANGING
        document      = li_document.

    ii_xml->add_xml( iv_name = 'ENHANCEMENT_DATA'
                     ii_xml = li_element ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENHO_WDYC implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ENHO_WDYN <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_enho_wdyn==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_enho_wdyn==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ENHO_WDYN implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    ms_item = is_item.
  ENDMETHOD.
  METHOD Lif_abapgit_object_enho~deserialize.

    DATA: ls_enh_data  TYPE enhwdyn,
          li_tool      TYPE REF TO if_enh_tool,
          lo_wdyn      TYPE REF TO cl_enh_tool_wdy,
          lv_tool_type TYPE enhtooltype,
          lv_package   TYPE devclass.

    FIELD-SYMBOLS: <ls_controller_data> TYPE enhwdyc,
                   <ls_view_data>       TYPE enhwdyv.


    ii_xml->read(
      EXPORTING
        iv_name = 'TOOL'
      CHANGING
        cg_data = lv_tool_type ).

    ii_xml->read(
      EXPORTING
        iv_name = 'COMPONENT_DATA'
      CHANGING
        cg_data = ls_enh_data ).

    lv_package = iv_package.

    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname     = |{ ms_item-obj_name }|
            enhtype     = ''
            enhtooltype = lv_tool_type
          IMPORTING
            enhancement = li_tool
          CHANGING
            devclass    = lv_package ).

        lo_wdyn ?= li_tool.

        lo_wdyn->initialize( ls_enh_data-component_name ).

        lo_wdyn->set_component_data( ls_enh_data-component_data ).

        LOOP AT ls_enh_data-controller_data ASSIGNING <ls_controller_data>.

          lo_wdyn->set_controller_data( p_controller_name = <ls_controller_data>-controller_name
                                        p_enh_data        = <ls_controller_data> ).

        ENDLOOP.

        LOOP AT ls_enh_data-view_data ASSIGNING <ls_view_data>.

          lo_wdyn->set_view_data( p_view_name = <ls_view_data>-view_name
                                  p_enh_data  = <ls_view_data> ).

        ENDLOOP.

        lo_wdyn->if_enh_object~save( run_dark = abap_true ).
        lo_wdyn->if_enh_object~unlock( ).

      CATCH cx_root.
        TRY.
            lo_wdyn->if_enh_object~unlock( ).
          CATCH cx_sy_ref_is_initial cx_enh_mod_not_allowed ##NO_HANDLER.
        ENDTRY.
        Lcx_abapgit_exception=>raise( |error deserializing ENHO wdyn { ms_item-obj_name }| ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object_enho~serialize.

    DATA: lo_wdyn           TYPE REF TO cl_enh_tool_wdy,
          lv_component_name TYPE wdy_component_name,
          ls_enh_data       TYPE enhwdyn.


    lo_wdyn ?= ii_enh_tool.
    lv_component_name = lo_wdyn->get_component_name( ).

    TRY.
        lo_wdyn->get_all_data_for_comp(
          EXPORTING
            p_component_name = lv_component_name
          IMPORTING
            p_enh_data       = ls_enh_data ).

        ii_xml->add( iv_name = 'TOOL'
                     ig_data = ii_enh_tool->get_tool( ) ).

        ii_xml->add( iv_name = 'COMPONENT_DATA'
                     ig_data = ls_enh_data ).

      CATCH cx_enh_not_found.
        Lcx_abapgit_exception=>raise( |error serializing ENHO wdyn { ms_item-obj_name }| ).
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENHO_WDYN implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_AVAS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_avas=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_avas=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_AVAS implementation.
*"* method's implementations
*include methods.
  METHOD insert_assignments.

    DATA: lt_assignment TYPE STANDARD TABLE OF cls_assignment,
          ls_assignment LIKE LINE OF lt_assignment,
          ls_value      LIKE LINE OF is_avas-values.


    LOOP AT is_avas-values INTO ls_value.
      CLEAR ls_assignment.
      ls_assignment-guid        = is_avas-header-guid.
      ls_assignment-value       = ls_value-value.
      ls_assignment-trobjtype   = is_avas-header-object-trobjtype.
      ls_assignment-sobj_name   = is_avas-header-object-sobj_name.
      ls_assignment-object_type = is_avas-header-object-object_type.
      ls_assignment-sub_key     = is_avas-header-object-sub_key.
      ls_assignment-attribute   = is_avas-header-attribute.
      ls_assignment-set_by      = sy-uname.
      ls_assignment-changed_on  = sy-datum.
      ls_assignment-remark      = ls_value-remark.
      APPEND ls_assignment TO lt_assignment.
    ENDLOOP.

    DELETE FROM cls_assignment WHERE guid = is_avas-header-guid.

    INSERT cls_assignment FROM TABLE lt_assignment.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error inserting into CLS_ASSIGNMENT| ).
    ENDIF.

  ENDMETHOD.
  METHOD instantiate.

    DATA: lv_id  TYPE guid_32,
          lx_err TYPE REF TO cx_root.

    lv_id = ms_item-obj_name.

    TRY.
        CREATE OBJECT ro_avas
          EXPORTING
            im_assignment_id = lv_id.
      CATCH cx_pak_wb_object_locked INTO lx_err.
        Lcx_abapgit_exception=>raise( |AVAS { lv_id }: locked: { lx_err->get_longtext( ) }| ).
      CATCH cx_pak_not_authorized INTO lx_err.
        Lcx_abapgit_exception=>raise( |AVAS { lv_id }: not authorized: { lx_err->get_longtext( ) }| ).
      CATCH cx_pak_invalid_state INTO lx_err.
        Lcx_abapgit_exception=>raise( |AVAS { lv_id }: invalid state: { lx_err->get_longtext( ) }| ).
      CATCH cx_pak_invalid_data INTO lx_err.
        Lcx_abapgit_exception=>raise( |AVAS { lv_id }: invalid data: { lx_err->get_longtext( ) }| ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lo_avas TYPE REF TO cl_cls_attr_value_assignment.


    lo_avas = instantiate( ).

    lo_avas->if_pak_wb_object~get_last_changed( IMPORTING ex_changed_by = rv_user ).

    IF rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lo_avas TYPE REF TO cl_cls_attr_value_assignment.


    lo_avas = instantiate( ).

    TRY.
        lo_avas->if_cls_attr_value_assignment~lock_and_refresh( im_allow_popups = abap_false ).
      CATCH cx_pak_invalid_state
          cx_pak_invalid_data
          cx_pak_not_authorized
          cx_pak_wb_object_locked.
        Lcx_abapgit_exception=>raise( |AVAS error| ).
    ENDTRY.

    lo_avas->if_pak_wb_object~delete( ).

    lo_avas->if_pak_wb_object~save( ).

    lo_avas->if_pak_wb_object_internal~unlock( ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_avas TYPE ty_avas.


    io_xml->read( EXPORTING iv_name = 'AVAS'
                  CHANGING cg_data = ls_avas ).

* The AVAS API cannot be used in this case, as it will always create a new GUID

    ASSERT NOT ls_avas-header-guid IS INITIAL.

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

    insert_assignments( ls_avas ).
* todo, how does links work?

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_guid TYPE cls_assignment-guid.

    SELECT SINGLE guid FROM cls_assignment INTO lv_guid
      WHERE guid = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.

    rs_metadata = get_metadata( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for(
      iv_lock_object = 'CLS_ENQUEUE_STRU'
      iv_argument    = |{ ms_item-obj_name }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lo_avas TYPE REF TO cl_cls_attr_value_assignment,
          ls_avas TYPE ty_avas.

    FIELD-SYMBOLS: <ls_value> LIKE LINE OF ls_avas-values,
                   <ls_link>  LIKE LINE OF ls_avas-links.


    lo_avas = instantiate( ).

    ls_avas-header-guid      = lo_avas->if_cls_attr_value_assignment~get_guid( ).
    ls_avas-header-attribute = lo_avas->if_cls_attr_value_assignment~get_attribute( ).
    ls_avas-header-object    = lo_avas->if_cls_attr_value_assignment~get_object( ).

    lo_avas->if_cls_attr_value_assignment~get_values( IMPORTING ex_values = ls_avas-values ).

    lo_avas->if_cls_attr_value_assignment~get_links( IMPORTING ex_links = ls_avas-links ).

    LOOP AT ls_avas-values ASSIGNING <ls_value>.
      CLEAR: <ls_value>-set_by, <ls_value>-changed_on.
    ENDLOOP.

    LOOP AT ls_avas-links ASSIGNING <ls_link>.
      CLEAR: <ls_link>-set_by, <ls_link>-changed_on.
    ENDLOOP.

    io_xml->add(
      iv_name = 'AVAS'
      ig_data = ls_avas ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_AVAS implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ENHS_BADI_D <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_enhs_badi_dccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_enhs_badi_dccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ENHS_BADI_D implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object_enhs~deserialize.

    DATA: lv_parent          TYPE enhspotcompositename,
          lt_enh_badi        TYPE enh_badi_data_it,
          lo_badidef_tool    TYPE REF TO cl_enh_tool_badi_def,
          lv_enh_shorttext   TYPE string,
          li_enh_object      TYPE REF TO if_enh_object,
          li_enh_object_docu TYPE REF TO if_enh_object_docu,
          lx_enh_root        TYPE REF TO cx_enh_root.

    FIELD-SYMBOLS: <ls_enh_badi> LIKE LINE OF lt_enh_badi.

    ii_xml->read( EXPORTING iv_name = 'PARENT_COMP'
                  CHANGING  cg_data = lv_parent ).

    ii_xml->read( EXPORTING iv_name = 'BADI_DATA'
                  CHANGING  cg_data = lt_enh_badi ).

    ii_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING  cg_data = lv_enh_shorttext ).

    li_enh_object ?= ii_enh_spot_tool.
    li_enh_object_docu ?= ii_enh_spot_tool.

    TRY.
        li_enh_object_docu->set_shorttext( lv_enh_shorttext ).

        lo_badidef_tool ?= ii_enh_spot_tool.

        LOOP AT lt_enh_badi ASSIGNING <ls_enh_badi>.
          lo_badidef_tool->add_badi_def( <ls_enh_badi> ).
        ENDLOOP.

        li_enh_object->save( ).
        li_enh_object->activate( ).
        li_enh_object->unlock( ).

      CATCH cx_enh_root INTO lx_enh_root.
        Lcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object_enhs~serialize.

    DATA: lv_spot_name       TYPE enhspotname,
          lv_parent          TYPE enhspotcompositename,
          lt_enh_badi        TYPE enh_badi_data_it,
          lo_badidef_tool    TYPE REF TO cl_enh_tool_badi_def,
          lv_enh_shorttext   TYPE string,
          li_enh_object_docu TYPE REF TO if_enh_object_docu.

    lo_badidef_tool ?= ii_enh_spot_tool.

    li_enh_object_docu ?= ii_enh_spot_tool.
    lv_enh_shorttext = li_enh_object_docu->get_shorttext( ).

    "get parent = composite enhs (ENHC)
    lv_parent = cl_r3standard_persistence=>enh_find_parent_composite( lv_spot_name ).
    "get subsequent BADI definitions
    lt_enh_badi = lo_badidef_tool->get_badi_defs( ).

    ii_xml->add( ig_data = ii_enh_spot_tool->get_tool( )
                 iv_name = 'TOOL' ).

    ii_xml->add( ig_data = lv_enh_shorttext
                 iv_name = 'SHORTTEXT' ).

    ii_xml->add( ig_data = lv_parent
                 iv_name = 'PARENT_COMP' ).

    ii_xml->add( ig_data = lt_enh_badi
                 iv_name = 'BADI_DATA' ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENHS_BADI_D implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ENHS_HOOK_D <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_enhs_hook_dccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_enhs_hook_dccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ENHS_HOOK_D implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object_enhs~deserialize.

    DATA: lv_enh_shorttext       TYPE string,
          ls_enh_hook_definition TYPE enh_hook_def,
          ls_hook_definition     TYPE ty_hook_defifnition,
          li_enh_object          TYPE REF TO if_enh_object,
          li_enh_object_docu     TYPE REF TO if_enh_object_docu,
          lo_hookdef_tool        TYPE REF TO cl_enh_tool_hook_def,
          lx_enh_root            TYPE REF TO cx_enh_root.

    FIELD-SYMBOLS: <ls_hook_definition> TYPE enh_hook_def_ext.

    ii_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING  cg_data = lv_enh_shorttext ).

    ii_xml->read( EXPORTING iv_name = 'BADI_DATA'
                  CHANGING  cg_data = ls_hook_definition ).

    li_enh_object ?= ii_enh_spot_tool.
    li_enh_object_docu ?= ii_enh_spot_tool.

    TRY.
        li_enh_object_docu->set_shorttext( lv_enh_shorttext ).

        lo_hookdef_tool ?= ii_enh_spot_tool.

        lo_hookdef_tool->set_original_object( pgmid     = ls_hook_definition-pgmid
                                              obj_name  = ls_hook_definition-obj_name
                                              obj_type  = ls_hook_definition-obj_type
                                              program   = ls_hook_definition-program
                                              main_type = ls_hook_definition-main_type
                                              main_name = ls_hook_definition-main_name ).

        LOOP AT ls_hook_definition-def_hooks ASSIGNING <ls_hook_definition>.
          MOVE-CORRESPONDING <ls_hook_definition> TO ls_enh_hook_definition.
          lo_hookdef_tool->add_hook_def( ls_enh_hook_definition ).
        ENDLOOP.

        li_enh_object->save( ).
        li_enh_object->activate( ).
        li_enh_object->unlock( ).

      CATCH cx_enh_root INTO lx_enh_root.
        Lcx_abapgit_exception=>raise_with_text( lx_enh_root ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object_enhs~serialize.

    DATA: lo_hookdef_tool    TYPE REF TO cl_enh_tool_hook_def,
          lv_enh_shorttext   TYPE string,
          li_enh_object_docu TYPE REF TO if_enh_object_docu,
          ls_hook_definition TYPE ty_hook_defifnition.

    lo_hookdef_tool ?= ii_enh_spot_tool.

    li_enh_object_docu ?= ii_enh_spot_tool.
    lv_enh_shorttext = li_enh_object_docu->get_shorttext( ).

    ls_hook_definition-def_hooks = lo_hookdef_tool->get_hook_defs( ).

    lo_hookdef_tool->get_original_object(
      IMPORTING
        pgmid     = ls_hook_definition-pgmid
        obj_name  = ls_hook_definition-obj_name
        obj_type  = ls_hook_definition-obj_type
        main_type = ls_hook_definition-main_type
        main_name = ls_hook_definition-main_name
        program   = ls_hook_definition-program ).

    ii_xml->add( ig_data = ii_enh_spot_tool->get_tool( )
                 iv_name = 'TOOL' ).

    ii_xml->add( ig_data = lv_enh_shorttext
                 iv_name = 'SHORTTEXT' ).

    ii_xml->add( ig_data = ls_hook_definition
                 iv_name = 'BADI_DATA' ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ENHS_HOOK_D implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_WAPA <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_wapa=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_wapa=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_WAPA implementation.
*"* method's implementations
*include methods.
  METHOD create_new_application.

    DATA: ls_item   LIKE ms_item,
          lv_objkey TYPE seu_objkey.

    cl_o2_api_application=>create_new(
      EXPORTING
        p_application_data      = is_attributes
        p_nodes                 = it_nodes
        p_navgraph              = it_navgraph
      IMPORTING
        p_application           = ro_bsp
      EXCEPTIONS
        object_already_existing = 1
        object_just_created     = 2
        not_authorized          = 3
        undefined_name          = 4
        author_not_existing     = 5
        action_cancelled        = 6
        error_occured           = 7
        invalid_parameter       = 8 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |WAPA - error from create_new: { sy-subrc }| ).
    ENDIF.

    ro_bsp->save( ).

    ro_bsp->set_changeable(
      p_changeable           = abap_false
      p_complete_application = abap_true ).

    ls_item-obj_type = 'WAPD'.
    ls_item-obj_name = ms_item-obj_name.
    Lcl_abapgit_objects_activation=>add_item( ls_item ).

    lv_objkey = ls_item-obj_name.
* todo, hmm, the WAPD is not added to the worklist during activation
    cl_o2_api_application=>activate( lv_objkey ).


  ENDMETHOD.
  METHOD create_new_page.

    cl_o2_api_pages=>create_new_page(
      EXPORTING
        p_pageattrs = is_page_attributes
      IMPORTING
        p_page      = ro_page
      EXCEPTIONS
        object_already_exists = 1
        invalid_name          = 2
        error_occured         = 3
        o2appl_not_existing   = 4
        OTHERS                = 5 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error { sy-subrc } from CL_O2_API_PAGES=>CREATE_NEW_PAGE| ).
    ENDIF.

  ENDMETHOD.
  METHOD delete_superfluous_pages.

    DATA: ls_pagekey TYPE o2pagkey.
    FIELD-SYMBOLS: <ls_local_page> LIKE LINE OF it_local_pages.

    " delete local pages which doesn't exists remotely
    LOOP AT it_local_pages ASSIGNING <ls_local_page>.

      READ TABLE it_remote_pages WITH KEY attributes-pagekey = <ls_local_page>-pagekey
                               TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        " page exists locally but not remotely -> delete

        ls_pagekey-applname = <ls_local_page>-applname.
        ls_pagekey-pagekey = <ls_local_page>-pagekey.

        cl_o2_page=>delete_page_for_application(
          EXPORTING
            p_pagekey           = ls_pagekey
          EXCEPTIONS
            object_not_existing = 1
            error_occured       = 2 ).

        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise( |Error { sy-subrc } from CL_O2_PAGE=>DELETE_PAGE_FOR_APPLICATION| ).
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD get_page_content.

    DATA: lt_content TYPE o2pageline_table,
          lv_string  TYPE string.

    io_page->get_page(
      IMPORTING
        p_content = lt_content
      EXCEPTIONS
        invalid_call = 1
        page_deleted = 2
        OTHERS       = 3 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |WAPA - error from get_page_content| ).
    ENDIF.

    CONCATENATE LINES OF lt_content INTO lv_string SEPARATED BY cl_abap_char_utilities=>newline RESPECTING BLANKS.

    rv_content = Lcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).

  ENDMETHOD.
  METHOD read_page.

    DATA: lv_name    TYPE o2applname,
          ls_pagekey TYPE o2pagkey,
          lv_content TYPE xstring,
          lv_extra   TYPE string,
          lv_ext     TYPE string,
          lo_page    TYPE REF TO cl_o2_api_pages.


    lv_name = ms_item-obj_name.

    ls_pagekey-applname = lv_name.
    ls_pagekey-pagekey = is_page-pagekey.

    cl_o2_api_pages=>load(
      EXPORTING
        p_pagekey = ls_pagekey
      IMPORTING
        p_page    = lo_page ).

    lo_page->get_attrs( IMPORTING p_attrs = rs_page-attributes ).

    IF rs_page-attributes-pagetype <> so2_controller.

      lo_page->get_event_handlers(
        IMPORTING
          p_ev_handler = rs_page-event_handlers
        EXCEPTIONS
          page_deleted = 1
          invalid_call = 2 ).
      ASSERT sy-subrc = 0.

      lo_page->get_parameters(
        IMPORTING
          p_parameters = rs_page-parameters
        EXCEPTIONS
          page_deleted = 1
          invalid_call = 2
          OTHERS       = 3 ).
      ASSERT sy-subrc = 0.

      lo_page->get_type_source(
        IMPORTING
          p_source     = rs_page-types
        EXCEPTIONS
          page_deleted = 1
          invalid_call = 2
          OTHERS       = 3 ).
      ASSERT sy-subrc = 0.

      lv_content = get_page_content( lo_page ).
      SPLIT is_page-pagename AT '.' INTO lv_extra lv_ext.
      REPLACE ALL OCCURRENCES OF '/' IN lv_ext WITH '_-'.
      REPLACE ALL OCCURRENCES OF '/' IN lv_extra WITH '_-'.
      IF iv_no_files_add = abap_false.
        Lif_abapgit_object~mo_files->add_raw(
          iv_extra = lv_extra
          iv_ext   = lv_ext
          iv_data  = lv_content ).
      ENDIF.

      CLEAR: rs_page-attributes-implclass.

    ENDIF.

    CLEAR: rs_page-attributes-author,
           rs_page-attributes-createdon,
           rs_page-attributes-changedby,
           rs_page-attributes-changedon,
           rs_page-attributes-changetime,
           rs_page-attributes-gendate,
           rs_page-attributes-gentime,
           rs_page-attributes-devclass.

  ENDMETHOD.
  METHOD to_page_content.

    DATA: lv_string TYPE string.


    lv_string = Lcl_abapgit_convert=>xstring_to_string_utf8( iv_content ).

    SPLIT lv_string AT cl_abap_char_utilities=>newline INTO TABLE rt_content.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lv_name   TYPE o2applname,
          lt_pages  TYPE STANDARD TABLE OF o2pagdir WITH DEFAULT KEY,
          ls_latest LIKE LINE OF lt_pages.


    lv_name = ms_item-obj_name.

    SELECT * FROM o2pagdir INTO TABLE lt_pages WHERE applname = lv_name
      ORDER BY changedon DESCENDING changetime DESCENDING.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
      RETURN.
    ENDIF.

    READ TABLE lt_pages INDEX 1 INTO ls_latest.
    ASSERT sy-subrc = 0.

    rv_user = ls_latest-changedby.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_name        TYPE o2applname,
          lo_bsp         TYPE REF TO cl_o2_api_application,
          ls_pagekey     TYPE o2pagkey,
          lv_object      TYPE seu_objkey,
          lt_pages       TYPE o2pagelist,
          lt_local_mimes TYPE o2pagename_table.

    FIELD-SYMBOLS: <ls_page>       LIKE LINE OF lt_pages,
                   <ls_local_mime> TYPE o2pagename.

    lv_name = ms_item-obj_name.

    cl_o2_api_application=>load(
      EXPORTING
        p_application_name  = lv_name
      IMPORTING
        p_application       = lo_bsp
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3 ).
    ASSERT sy-subrc = 0.

    lo_bsp->set_changeable(
      p_changeable           = abap_true
      p_complete_application = abap_true ).

    cl_o2_api_pages=>get_all_pages(
      EXPORTING
        p_applname = lv_name
        p_version  = c_active
      IMPORTING
        p_pages    = lt_pages ).

    LOOP AT lt_pages ASSIGNING <ls_page>.
      CLEAR ls_pagekey.
      ls_pagekey-applname = lv_name.
      ls_pagekey-pagekey  = <ls_page>-pagekey.

      cl_o2_page=>delete_page_for_application(
        EXPORTING
          p_pagekey           = ls_pagekey
        EXCEPTIONS
          object_not_existing = 1
          error_occured       = 2 ).
      ASSERT sy-subrc = 0.
    ENDLOOP.

    lo_bsp->get_local_mimes(
      IMPORTING
        p_local_mimes  = lt_local_mimes
      EXCEPTIONS
        object_invalid = 1
        object_deleted = 2
        error_occured  = 3
        OTHERS         = 4 ).

    LOOP AT lt_local_mimes ASSIGNING <ls_local_mime>.
      CLEAR ls_pagekey.
      ls_pagekey-applname = <ls_local_mime>-applname.
      ls_pagekey-pagekey  = <ls_local_mime>-pagekey.

      cl_o2_page=>delete_page_for_application(
        EXPORTING
          p_pagekey           = ls_pagekey
        EXCEPTIONS
          object_not_existing = 1
          error_occured       = 2 ).
      ASSERT sy-subrc = 0.
    ENDLOOP.

    lo_bsp->delete(
      EXCEPTIONS
        object_not_empty      = 1
        object_not_changeable = 2
        object_invalid        = 3
        action_cancelled      = 4
        permission_failure    = 5
        error_occured         = 6
        OTHERS                = 7 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |WAPA - error from delete: { sy-subrc }| ).
    ENDIF.

* release lock
    lv_object = lv_name.
    cl_o2_api_application=>call_access_permission(
      p_mode                 = 'FREE'
      p_object               = lv_object
      p_complete_application = abap_true ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lo_bsp            TYPE REF TO cl_o2_api_application,
          ls_attributes     TYPE o2applattr,
          lt_nodes          TYPE o2applnode_table,
          lt_navgraph       TYPE o2applgrap_table,
          lv_obj_name       TYPE string,
          lv_extra          TYPE string,
          lv_ext            TYPE string,
          lo_page           TYPE REF TO cl_o2_api_pages,
          lt_pages_info     TYPE ty_pages_tt,
          ls_pagekey        TYPE o2pagkey,
          ls_local_page     TYPE ty_page,
          lt_remote_content TYPE o2pageline_table,
          lt_local_content  TYPE o2pageline_table,
          lt_local_pages    TYPE o2pagelist.

    FIELD-SYMBOLS: <ls_remote_page> LIKE LINE OF lt_pages_info.

    io_xml->read( EXPORTING iv_name = 'ATTRIBUTES'
                  CHANGING cg_data = ls_attributes ).
    io_xml->read( EXPORTING iv_name = 'NAVGRAPH'
                  CHANGING cg_data = lt_navgraph ).
    io_xml->read( EXPORTING iv_name = 'PAGES'
                  CHANGING cg_data = lt_pages_info ).

    ls_attributes-devclass = iv_package.

    cl_o2_api_application=>load(
      EXPORTING
        p_application_name  = ls_attributes-applname    " Application Name
      IMPORTING
        p_application       = lo_bsp    " Instance Created
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3
        OTHERS              = 4 ).

    CASE sy-subrc.
      WHEN 0.

        cl_o2_api_pages=>get_all_pages(
          EXPORTING
            p_applname = ls_attributes-applname
            p_version  = c_active
          IMPORTING
            p_pages    = lt_local_pages ).

      WHEN 1.

        lo_bsp = create_new_application( is_attributes = ls_attributes
                                         it_nodes      = lt_nodes
                                         it_navgraph   = lt_navgraph ).

      WHEN OTHERS.

        Lcx_abapgit_exception=>raise( |Error { sy-subrc } from CL_O2_API_APPLICATION=>LOAD| ).

    ENDCASE.

    LOOP AT lt_pages_info ASSIGNING <ls_remote_page>.

      ls_pagekey-applname = <ls_remote_page>-attributes-applname.
      ls_pagekey-pagekey = <ls_remote_page>-attributes-pagekey.

      cl_o2_api_pages=>load(
        EXPORTING
          p_pagekey             = ls_pagekey
        IMPORTING
          p_page                = lo_page
        EXCEPTIONS
          object_not_existing   = 1
          version_not_existing  = 2
          OTHERS                = 3 ).

      CASE sy-subrc.
        WHEN 0.

          ls_local_page = read_page( is_page = <ls_remote_page>-attributes
                                     iv_no_files_add = abap_true ).

        WHEN 1.

          lo_page = create_new_page( <ls_remote_page>-attributes ).

        WHEN 2.

          " Do nothing...

        WHEN OTHERS.

          Lcx_abapgit_exception=>raise( |Error { sy-subrc } from CL_O2_API_PAGES=>LOAD| ).

      ENDCASE.

      SPLIT <ls_remote_page>-attributes-pagename AT '.' INTO lv_extra lv_ext.
      REPLACE ALL OCCURRENCES OF '/' IN lv_extra WITH '_-'.
      REPLACE ALL OCCURRENCES OF '/' IN lv_ext WITH '_-'.

      lt_remote_content = to_page_content( Lif_abapgit_object~mo_files->read_raw( iv_extra = lv_extra
                                                                                  iv_ext   = lv_ext ) ).
      lt_local_content = to_page_content( get_page_content( lo_page ) ).

      IF ls_local_page = <ls_remote_page> AND lt_local_content = lt_remote_content.
        " no changes -> nothing to do
        CONTINUE.
      ENDIF.

      IF <ls_remote_page>-attributes-pagetype <> so2_controller.

        lo_page->set_page( lt_remote_content ).

        lo_page->set_event_handlers( <ls_remote_page>-event_handlers ).
        lo_page->set_parameters( <ls_remote_page>-parameters ).
        lo_page->set_type_source( <ls_remote_page>-types ).

      ENDIF.

      lo_page->save( p_with_all_texts = abap_true ).

      lv_obj_name = cl_wb_object_type=>get_concatenated_key_from_id(
        p_key_component1 = <ls_remote_page>-attributes-applname
        p_key_component2 = <ls_remote_page>-attributes-pagekey
        p_external_id    = 'WG ' ).

      Lcl_abapgit_objects_activation=>add( iv_type = 'WAPP'
                                           iv_name = lv_obj_name ).

    ENDLOOP.

    delete_superfluous_pages( it_local_pages  = lt_local_pages
                              it_remote_pages = lt_pages_info ).

    Lcl_abapgit_sotr_handler=>create_sotr(
      iv_package = iv_package
      io_xml     = io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_name TYPE o2applname.


    lv_name = ms_item-obj_name.

    cl_o2_api_application=>load(
      EXPORTING
        p_application_name  = lv_name
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3 ).
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_name       TYPE o2applname,
          ls_attributes TYPE o2applattr,
          lt_navgraph   TYPE o2applgrap_table,
          lt_pages      TYPE o2pagelist,
          lt_pages_info TYPE ty_pages_tt,
          lo_bsp        TYPE REF TO cl_o2_api_application.

    FIELD-SYMBOLS: <ls_page> LIKE LINE OF lt_pages.

    lv_name = ms_item-obj_name.

    cl_o2_api_application=>load(
      EXPORTING
        p_application_name  = lv_name
      IMPORTING
        p_application       = lo_bsp
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lo_bsp->get_attributes(
      EXPORTING
        p_version    = c_active
      IMPORTING
        p_attributes = ls_attributes ).

    CLEAR: ls_attributes-author,
           ls_attributes-createdon,
           ls_attributes-changedby,
           ls_attributes-changedon,
           ls_attributes-devclass.

    io_xml->add( iv_name = 'ATTRIBUTES'
                 ig_data = ls_attributes ).

    lo_bsp->get_navgraph(
      EXPORTING
        p_version  = c_active
      IMPORTING
        p_navgraph = lt_navgraph ).

    io_xml->add( iv_name = 'NAVGRAPH'
                 ig_data = lt_navgraph ).

    cl_o2_api_pages=>get_all_pages(
      EXPORTING
        p_applname = lv_name
        p_version  = c_active
      IMPORTING
        p_pages    = lt_pages ).

    LOOP AT lt_pages ASSIGNING <ls_page>.
      APPEND read_page( <ls_page> ) TO lt_pages_info.
    ENDLOOP.

    io_xml->add( iv_name = 'PAGES'
                 ig_data = lt_pages_info ).

    Lcl_abapgit_sotr_handler=>read_sotr(
      iv_pgmid    = 'LIMU'
      iv_object   = 'WAPP'
      iv_obj_name = ms_item-obj_name
      io_i18n_params = mo_i18n_params
      io_xml      = io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_WAPA implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_WDYA <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_wdya=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_wdya=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_WDYA implementation.
*"* method's implementations
*include methods.
  METHOD read.

    DATA: li_app  TYPE REF TO if_wdy_md_application,
          li_map  TYPE REF TO if_object_map,
          lo_prop TYPE REF TO cl_wdy_md_application_property,
          ls_prop LIKE LINE OF et_properties,
          lv_name TYPE wdy_application_name.


    CLEAR es_app.
    CLEAR et_properties.

    lv_name = ms_item-obj_name.
    TRY.
        li_app = cl_wdy_md_application=>get_object_by_key(
                   name    = lv_name
                   version = 'A' ).
      CATCH cx_wdy_md_not_existing.
        RETURN.
      CATCH cx_wdy_md_permission_failure.
        Lcx_abapgit_exception=>raise( 'WDYA, permission failure' ).
    ENDTRY.

    li_app->if_wdy_md_object~get_definition( IMPORTING definition = es_app ).
    CLEAR: es_app-author,
           es_app-createdon,
           es_app-changedby,
           es_app-changedon.

    li_map = li_app->get_properties( ).
    DO li_map->size( ) TIMES.
      lo_prop ?= li_map->get_by_position( sy-index ).
      lo_prop->get_definition( IMPORTING definition = ls_prop ).
      APPEND ls_prop TO et_properties.
    ENDDO.

  ENDMETHOD.
  METHOD save.

    DATA: li_prop TYPE REF TO if_wdy_md_application_property,
          lo_app  TYPE REF TO cl_wdy_md_application.

    FIELD-SYMBOLS: <ls_property> LIKE LINE OF it_properties.


    TRY.
        CREATE OBJECT lo_app
          EXPORTING
            name       = is_app-application_name
            definition = is_app
            devclass   = iv_package.

        LOOP AT it_properties ASSIGNING <ls_property>.
          li_prop = lo_app->if_wdy_md_application~create_property( <ls_property>-name ).
          li_prop->set_value( <ls_property>-value ).
        ENDLOOP.

        tadir_insert( iv_package ).

        lo_app->if_wdy_md_lockable_object~save_to_database( ).
      CATCH cx_wdy_md_exception.
        Lcx_abapgit_exception=>raise( 'error saving WDYA' ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: li_app  TYPE REF TO if_wdy_md_application,
          ls_app  TYPE wdy_application,
          lv_name TYPE wdy_application_name.


    lv_name = ms_item-obj_name.
    TRY.
        li_app = cl_wdy_md_application=>get_object_by_key(
                   name    = lv_name
                   version = 'A' ).

        li_app->if_wdy_md_object~get_definition( IMPORTING definition = ls_app ).

        IF ls_app-changedby IS INITIAL.
          rv_user = ls_app-author.
        ELSE.
          rv_user = ls_app-changedby.
        ENDIF.
      CATCH cx_root.
        rv_user = c_user_unknown.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: li_app    TYPE REF TO if_wdy_md_application,
          lv_objkey TYPE wdy_wb_appl_name,
          lv_type   TYPE seu_type,
          lv_name   TYPE wdy_application_name.


    lv_name = ms_item-obj_name.
    TRY.
        li_app = cl_wdy_md_application=>get_object_by_key(
                   name    = lv_name
                   version = 'A' ).
        li_app->if_wdy_md_object~delete( ).
        li_app->if_wdy_md_lockable_object~save_to_database( ).

* method save_to_database calls function module TR_TADIR_INTERFACE
* with test mode = X, so it does not delete the TADIR entry.
* Instead the standard code uses RS_TREE_OBJECT_PLACEMENT to delete
* the TADIR entry
        lv_objkey = ms_item-obj_name.
        CONCATENATE 'O' swbm_c_type_wdy_application INTO lv_type.
        CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
          EXPORTING
            object    = lv_objkey
            type      = lv_type
            operation = 'DELETE'.

      CATCH cx_wdy_md_not_existing.
        RETURN.
      CATCH cx_wdy_md_exception.
        Lcx_abapgit_exception=>raise( 'WDYA, error deleting' ).
    ENDTRY.

    delete_longtexts( c_longtext_id_wdya ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_app        TYPE wdy_application,
          lt_properties TYPE wdy_app_property_table.

    io_xml->read( EXPORTING iv_name = 'APP'
                  CHANGING cg_data = ls_app ).
    io_xml->read( EXPORTING iv_name = 'PROPERTIES'
                  CHANGING cg_data = lt_properties ).

    save( is_app        = ls_app
          it_properties = lt_properties
          iv_package    = iv_package ).

    Lcl_abapgit_sotr_handler=>create_sotr(
      iv_package = iv_package
      io_xml     = io_xml ).

    deserialize_longtexts( ii_xml         = io_xml
                           iv_longtext_id = c_longtext_id_wdya ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_name TYPE wdy_application_name.


    lv_name = ms_item-obj_name.

    TRY.
        cl_wdy_md_application=>get_object_by_key(
          name    = lv_name
          version = 'A' ).
        rv_bool = abap_true.
      CATCH cx_wdy_md_not_existing.
        rv_bool = abap_false.
      CATCH cx_wdy_md_permission_failure.
        Lcx_abapgit_exception=>raise( 'WDYA, permission failure' ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_app        TYPE wdy_application,
          lt_properties TYPE wdy_app_property_table.

    read( IMPORTING es_app        = ls_app
                    et_properties = lt_properties ).

    io_xml->add( iv_name = 'APP'
                 ig_data = ls_app ).
    io_xml->add( iv_name = 'PROPERTIES'
                 ig_data = lt_properties ).

    Lcl_abapgit_sotr_handler=>read_sotr(
      iv_pgmid    = 'R3TR'
      iv_object   = ms_item-obj_type
      iv_obj_name = ms_item-obj_name
      io_i18n_params = mo_i18n_params
      io_xml      = io_xml ).

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_wdya ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_WDYA implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_CMOD <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_cmod=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_cmod=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_CMOD implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE anam FROM modattr INTO rv_user WHERE name = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA lv_name TYPE modact-name.

    lv_name = ms_item-obj_name.

    CALL FUNCTION 'MOD_KUN_ACTIVATE'
      EXPORTING
        activate           = abap_false
        deactivate         = abap_true
        modname            = lv_name
      EXCEPTIONS
        call_error         = 1
        generate_error     = 2
        modattr_status     = 3
        mod_active         = 4
        mod_enqueued       = 5
        not_activated      = 6
        no_modification    = 7
        permission_failure = 8
        OTHERS             = 9.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'MOD_KUN_DELETE'
      EXPORTING
        modname            = lv_name
        screen             = abap_false
      EXCEPTIONS
        attr_enqueued      = 1
        mod_active         = 2
        mod_enqueued       = 3
        text_enqueued      = 4
        permission_failure = 5
        OTHERS             = 6.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_name    TYPE modact-name,
          lt_modact  TYPE TABLE OF modact,
          lt_modtext TYPE TABLE OF modtext,
          lt_modattr TYPE TABLE OF modattr.

    lv_name = ms_item-obj_name.

    DELETE FROM modact WHERE name = lv_name.
    DELETE FROM modtext WHERE name = lv_name.
    DELETE FROM modattr WHERE name = lv_name.

    io_xml->read( EXPORTING iv_name = 'MODACT'
                  CHANGING  cg_data = lt_modact ).

    io_xml->read( EXPORTING iv_name = 'MODTEXT'
                  CHANGING  cg_data = lt_modtext ).

    io_xml->read( EXPORTING iv_name = 'MODATTR'
                  CHANGING  cg_data = lt_modattr ).

    INSERT modact FROM TABLE lt_modact.
    INSERT modtext FROM TABLE lt_modtext.
    INSERT modattr FROM TABLE lt_modattr.

    tadir_insert( iv_package ).

    CALL FUNCTION 'MOD_KUN_ACTIVATE'
      EXPORTING
        activate           = abap_true
        deactivate         = abap_false
        modname            = lv_name
      EXCEPTIONS
        call_error         = 1
        generate_error     = 2
        modattr_status     = 3
        mod_active         = 4
        mod_enqueued       = 5
        not_activated      = 6
        no_modification    = 7
        permission_failure = 8
        OTHERS             = 9.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_name TYPE modact-name.

    SELECT SINGLE name FROM modact INTO lv_name WHERE name = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lt_modact  TYPE TABLE OF modact,
          lt_modtext TYPE TABLE OF modtext,
          lt_modattr TYPE TABLE OF modattr.

    FIELD-SYMBOLS: <ls_modattr> TYPE modattr.

    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    SELECT * FROM modact INTO TABLE lt_modact WHERE name = ms_item-obj_name
      ORDER BY PRIMARY KEY.
    IF sy-subrc = 0.
      io_xml->add( iv_name = 'MODACT'
                   ig_data = lt_modact ).
    ENDIF.

    SELECT * FROM modtext INTO TABLE lt_modtext WHERE name = ms_item-obj_name AND sprsl = mv_language
      ORDER BY PRIMARY KEY.
    IF sy-subrc = 0.
      io_xml->add( iv_name = 'MODTEXT'
                   ig_data = lt_modtext ).
    ENDIF.

    SELECT * FROM modattr INTO TABLE lt_modattr WHERE name = ms_item-obj_name
      ORDER BY PRIMARY KEY.
    IF sy-subrc = 0.
      LOOP AT lt_modattr ASSIGNING <ls_modattr>.
        CLEAR:
          <ls_modattr>-cnam, <ls_modattr>-cdat,
          <ls_modattr>-unam, <ls_modattr>-udat,
          <ls_modattr>-anam, <ls_modattr>-adat,
          <ls_modattr>-fnam, <ls_modattr>-fdat.
      ENDLOOP.

      io_xml->add( iv_name = 'MODATTR'
                   ig_data = lt_modattr ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_CMOD implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_WDYN <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_wdyn=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_wdyn=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_WDYN implementation.
*"* method's implementations
*include methods.
  METHOD add_fm_exception.

    DATA: ls_exception LIKE LINE OF ct_exception.

    ls_exception-name = iv_name.
    ls_exception-value = iv_value.

    INSERT ls_exception INTO TABLE ct_exception.

  ENDMETHOD.
  METHOD add_fm_param_exporting.

    DATA: ls_param LIKE LINE OF ct_param.

    ls_param-kind = abap_func_exporting.
    ls_param-name = iv_name.
    GET REFERENCE OF ig_value INTO ls_param-value.

    INSERT ls_param INTO TABLE ct_param.

  ENDMETHOD.
  METHOD add_fm_param_tables.

    DATA: ls_param LIKE LINE OF ct_param.

    ls_param-kind = abap_func_tables.
    ls_param-name = iv_name.
    GET REFERENCE OF ct_value INTO ls_param-value.

    INSERT ls_param INTO TABLE ct_param.

  ENDMETHOD.
  METHOD add_with_inactive_parts.

    DATA:
      lv_obj_name TYPE trobj_name,
      lv_object   TYPE trobjtype,
      lt_objects  TYPE dwinactiv_tab.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF lt_objects.

    lv_obj_name = ms_item-obj_name.
    lv_object = ms_item-obj_type.

    CALL FUNCTION 'RS_INACTIVE_OBJECTS_IN_OBJECT'
      EXPORTING
        obj_name         = lv_obj_name
        object           = lv_object
      TABLES
        inactive_objects = lt_objects
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    LOOP AT lt_objects ASSIGNING <ls_object>.
      Lcl_abapgit_objects_activation=>add( iv_type = <ls_object>-object
                                           iv_name = <ls_object>-obj_name ).
    ENDLOOP.

  ENDMETHOD.
  METHOD delta_controller.

    DATA: li_controller TYPE REF TO if_wdy_md_controller,
          lx_error      TYPE REF TO cx_wdy_md_exception,
          lv_found      TYPE abap_bool,
          ls_key        TYPE wdy_md_controller_key,
          ls_obj_new    TYPE svrs2_versionable_object,
          ls_obj_old    TYPE svrs2_versionable_object.

    FIELD-SYMBOLS: <ls_component>            LIKE LINE OF mt_components,
                   <ls_source>               LIKE LINE OF mt_sources,
                   <lt_ctrl_exceptions>      TYPE ANY TABLE,
                   <lt_ctrl_exception_texts> TYPE ANY TABLE,
                   <lt_excp>                 TYPE ANY TABLE,
                   <lt_excpt>                TYPE ANY TABLE.


    ls_key-component_name = is_controller-definition-component_name.
    ls_key-controller_name = is_controller-definition-controller_name.

    lv_found = cl_wdy_md_controller=>check_existency(
          component_name  = ls_key-component_name
          controller_name = ls_key-controller_name ).
    IF lv_found = abap_false.
      TRY.
          li_controller ?= cl_wdy_md_controller=>create_complete(
                component_name  = ls_key-component_name
                controller_name = ls_key-controller_name
                controller_type = is_controller-definition-controller_type ).
          li_controller->save_to_database( ).
          li_controller->unlock( ).
        CATCH cx_wdy_md_exception INTO lx_error.
          Lcx_abapgit_exception=>raise( |Error creating dummy controller: { lx_error->get_text( ) }| ).
      ENDTRY.
    ENDIF.

    ls_obj_new-objtype = wdyn_limu_component_controller.
    ls_obj_new-objname = ls_key.

    ls_obj_old-objtype = wdyn_limu_component_controller.
    ls_obj_old-objname = ls_key.

    APPEND is_controller-definition TO ls_obj_old-wdyc-defin.

    LOOP AT mt_components ASSIGNING <ls_component>
        WHERE component_name = ls_key-component_name
        AND controller_name = ls_key-controller_name.
      APPEND <ls_component> TO ls_obj_old-wdyc-ccomp.
    ENDLOOP.
    LOOP AT mt_sources ASSIGNING <ls_source>
        WHERE component_name = ls_key-component_name
        AND controller_name = ls_key-controller_name.
      APPEND <ls_source> TO ls_obj_old-wdyc-ccoms.
    ENDLOOP.

    ls_obj_old-wdyc-descr = is_controller-descriptions.
    ls_obj_old-wdyc-cusag = is_controller-controller_usages.
    ls_obj_old-wdyc-ccomt = is_controller-controller_component_texts.
    ls_obj_old-wdyc-cpara = is_controller-controller_parameters.
    ls_obj_old-wdyc-cpart = is_controller-controller_parameter_texts.
    ls_obj_old-wdyc-cnode = is_controller-context_nodes.
    ls_obj_old-wdyc-cattr = is_controller-context_attributes.
    ls_obj_old-wdyc-cmapp = is_controller-context_mappings.
*   Version 702 doesn't have these two attributes so we
*   use them dynamically for downward compatibility
    ASSIGN COMPONENT 'CONTROLLER_EXCEPTIONS' OF STRUCTURE is_controller
      TO <lt_ctrl_exceptions>.
    IF sy-subrc = 0.
      ASSIGN COMPONENT 'EXCP' OF STRUCTURE ls_obj_old-wdyc TO <lt_excp>.
      IF sy-subrc = 0.
        <lt_excp> = <lt_ctrl_exceptions>.
      ENDIF.
    ENDIF.
    ASSIGN COMPONENT 'CONTROLLER_EXCEPTIONS_TEXTS' OF STRUCTURE is_controller
      TO <lt_ctrl_exception_texts>.
    IF sy-subrc = 0.
      ASSIGN COMPONENT 'EXCPT' OF STRUCTURE ls_obj_old-wdyc TO <lt_excpt>.
      IF sy-subrc = 0.
        <lt_excpt> = <lt_ctrl_exception_texts>.
      ENDIF.
    ENDIF.
    ls_obj_old-wdyc-fgrps = is_controller-fieldgroups.

    CALL FUNCTION 'SVRS_MAKE_OBJECT_DELTA'
      EXPORTING
        obj_old              = ls_obj_new
        obj_new              = ls_obj_old
      CHANGING
        delta                = rs_delta
      EXCEPTIONS
        inconsistent_objects = 1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from SVRS_MAKE_OBJECT_DELTA' ).
    ENDIF.

  ENDMETHOD.
  METHOD delta_definition.

    DATA: ls_key       TYPE wdy_md_component_key,
          lv_found     TYPE abap_bool,
          ls_obj_new   TYPE svrs2_versionable_object,
          li_component TYPE REF TO if_wdy_md_component,
          lx_error     TYPE REF TO cx_wdy_md_exception,
          ls_obj_old   TYPE svrs2_versionable_object.


    ls_key-component_name = is_definition-definition-component_name.

    lv_found = cl_wdy_md_component=>check_existency( ls_key-component_name ).
    IF lv_found = abap_false.
      TRY.
          cl_wdy_md_component=>create_complete(
            EXPORTING
              name      = ls_key-component_name
            IMPORTING
              component = li_component
            CHANGING
              devclass  = iv_package ).
          li_component->save_to_database( ).
          li_component->unlock( ).
        CATCH cx_wdy_md_exception INTO lx_error.
          Lcx_abapgit_exception=>raise( |Error creating dummy component: { lx_error->get_text( ) }| ).
      ENDTRY.
    ENDIF.

    ls_obj_new-objtype = wdyn_limu_component_definition.
    ls_obj_new-objname = ls_key-component_name.

    ls_obj_old-objtype = wdyn_limu_component_definition.
    ls_obj_old-objname = ls_key-component_name.

    APPEND is_definition-definition TO ls_obj_old-wdyd-defin.
    ls_obj_old-wdyd-descr = is_definition-descriptions.
    ls_obj_old-wdyd-cusag = is_definition-component_usages.
    ls_obj_old-wdyd-intrf = is_definition-interface_implementings.
    ls_obj_old-wdyd-libra = is_definition-library_usages.
    ls_obj_old-wdyd-ctuse = is_definition-ext_ctlr_usages.
    ls_obj_old-wdyd-ctmap = is_definition-ext_ctx_mappings.

    CALL FUNCTION 'SVRS_MAKE_OBJECT_DELTA'
      EXPORTING
        obj_old              = ls_obj_new
        obj_new              = ls_obj_old
      CHANGING
        delta                = rs_delta
      EXCEPTIONS
        inconsistent_objects = 1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from SVRS_MAKE_OBJECT_DELTA' ).
    ENDIF.

  ENDMETHOD.
  METHOD delta_view.

    DATA: ls_key     TYPE wdy_md_view_key,
          ls_obj_new TYPE svrs2_versionable_object,
          ls_obj_old TYPE svrs2_versionable_object,
          lv_found   TYPE abap_bool,
          lx_error   TYPE REF TO cx_wdy_md_exception,
          li_view    TYPE REF TO if_wdy_md_abstract_view.

    FIELD-SYMBOLS: <ls_def> LIKE LINE OF ls_obj_old-wdyv-defin.


    ls_key-component_name = is_view-definition-component_name.
    ls_key-view_name      = is_view-definition-view_name.

    lv_found = cl_wdy_md_abstract_view=>check_existency(
                 component_name = ls_key-component_name
                 name           = ls_key-view_name ).
    IF lv_found = abap_false.
      TRY.
          li_view = cl_wdy_md_abstract_view=>create(
                      component_name = is_view-definition-component_name
                      view_name      = is_view-definition-view_name
                      type           = is_view-definition-type ).
          li_view->save_to_database( ).
          li_view->unlock( ).
        CATCH cx_wdy_md_exception INTO lx_error.
          Lcx_abapgit_exception=>raise( |Error creating dummy view: { lx_error->get_text( ) }| ).
      ENDTRY.
    ENDIF.

    ls_obj_new-objtype = wdyn_limu_component_view.
    ls_obj_new-objname = ls_key.

    ls_obj_old-objtype = wdyn_limu_component_view.
    ls_obj_old-objname = ls_key.

    APPEND INITIAL LINE TO ls_obj_old-wdyv-defin ASSIGNING <ls_def>.
    MOVE-CORRESPONDING is_view-definition TO <ls_def>.

    ls_obj_old-wdyv-descr = is_view-descriptions.
    ls_obj_old-wdyv-vcont = is_view-view_containers.
    ls_obj_old-wdyv-vcntt = is_view-view_container_texts.
    ls_obj_old-wdyv-ibplg = is_view-iobound_plugs.
    ls_obj_old-wdyv-ibplt = is_view-iobound_plug_texts.
    ls_obj_old-wdyv-plpar = is_view-plug_parameters.
    ls_obj_old-wdyv-plprt = is_view-plug_parameter_texts.
    ls_obj_old-wdyv-uiele = is_view-ui_elements.
    ls_obj_old-wdyv-uicon = is_view-ui_context_bindings.
    ls_obj_old-wdyv-uievt = is_view-ui_event_bindings.
    ls_obj_old-wdyv-uiddc = is_view-ui_ddic_bindings.
    ls_obj_old-wdyv-uiprp = is_view-ui_properties.
    ls_obj_old-wdyv-navil = is_view-navigation_links.
    ls_obj_old-wdyv-navit = is_view-navigation_target_refs.
    ls_obj_old-wdyv-vshno = is_view-vsh_nodes.
    ls_obj_old-wdyv-vshpl = is_view-vsh_placeholders.
    ls_obj_old-wdyv-views = is_view-viewset_properties.

    CALL FUNCTION 'SVRS_MAKE_OBJECT_DELTA'
      EXPORTING
        obj_old              = ls_obj_new
        obj_new              = ls_obj_old
      CHANGING
        delta                = rs_delta
      EXCEPTIONS
        inconsistent_objects = 1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from SVRS_MAKE_OBJECT_DELTA' ).
    ENDIF.

  ENDMETHOD.
  METHOD get_limu_objects.

    DATA: lv_name TYPE wdy_component_name.


    lv_name = ms_item-obj_name.
    CALL FUNCTION 'WDYN_GET_LIMU_OBJECTS'
      EXPORTING
        component_name = lv_name
      IMPORTING
        limu_objects   = rt_objects.

  ENDMETHOD.
  METHOD read.

    DATA: lt_objects        TYPE wdy_md_transport_keys,
          ls_controller_key TYPE wdy_md_controller_key,
          ls_component_key  TYPE wdy_md_component_key,
          ls_view_key       TYPE wdy_md_view_key.

    FIELD-SYMBOLS: <ls_object>               LIKE LINE OF lt_objects,
                   <ls_meta>                 LIKE LINE OF rs_component-ctlr_metadata,
                   <ls_view>                 LIKE LINE OF rs_component-view_metadata,
                   <lt_ctrl_exceptions>      TYPE ANY TABLE,
                   <lt_ctrl_exception_texts> TYPE ANY TABLE.

    CLEAR mt_components.
    CLEAR mt_sources.

    lt_objects = get_limu_objects( ).

    LOOP AT lt_objects ASSIGNING <ls_object>.
      CASE <ls_object>-sub_type.
        WHEN wdyn_limu_component_controller.
          ls_controller_key = <ls_object>-sub_name.
          APPEND read_controller( ls_controller_key ) TO rs_component-ctlr_metadata.
        WHEN wdyn_limu_component_definition.
          ls_component_key = <ls_object>-sub_name.
          rs_component-comp_metadata = read_definition( ls_component_key ).
        WHEN wdyn_limu_component_view.
          ls_view_key = <ls_object>-sub_name.
          APPEND read_view( ls_view_key ) TO rs_component-view_metadata.
        WHEN OTHERS.
          ASSERT 0 = 1.
      ENDCASE.
    ENDLOOP.

    SORT rs_component-ctlr_metadata BY
      definition-component_name ASCENDING
      definition-controller_name ASCENDING.

    LOOP AT rs_component-ctlr_metadata ASSIGNING <ls_meta>.
      SORT <ls_meta>-descriptions.
      SORT <ls_meta>-controller_usages.
      SORT <ls_meta>-controller_components.
      SORT <ls_meta>-controller_component_texts.
      SORT <ls_meta>-controller_parameters.
      SORT <ls_meta>-controller_parameter_texts.
      SORT <ls_meta>-context_nodes.
      SORT <ls_meta>-context_attributes.
      SORT <ls_meta>-context_mappings.
      SORT <ls_meta>-fieldgroups.
*     Version 702 doesn't have these two attributes so we
*     use them dynamically for downward compatibility
      ASSIGN COMPONENT 'CONTROLLER_EXCEPTIONS' OF STRUCTURE <ls_meta> TO <lt_ctrl_exceptions>.
      IF sy-subrc = 0.
        SORT <lt_ctrl_exceptions>.
      ENDIF.
      ASSIGN COMPONENT 'CONTROLLER_EXCEPTION_TEXTS' OF STRUCTURE <ls_meta> TO <lt_ctrl_exception_texts>.
      IF sy-subrc = 0.
        SORT <lt_ctrl_exception_texts>.
      ENDIF.
    ENDLOOP.

    SORT rs_component-view_metadata BY
      definition-component_name ASCENDING
      definition-view_name ASCENDING.

    LOOP AT rs_component-view_metadata ASSIGNING <ls_view>.
      SORT <ls_view>-descriptions.
      SORT <ls_view>-view_containers.
      SORT <ls_view>-view_container_texts.
      SORT <ls_view>-iobound_plugs.
      SORT <ls_view>-iobound_plug_texts.
      SORT <ls_view>-plug_parameters.
      SORT <ls_view>-plug_parameter_texts.
      SORT <ls_view>-ui_elements.
      SORT <ls_view>-ui_context_bindings.
      SORT <ls_view>-ui_event_bindings.
      SORT <ls_view>-ui_ddic_bindings.
      SORT <ls_view>-ui_properties.
      SORT <ls_view>-navigation_links.
      SORT <ls_view>-navigation_target_refs.
      SORT <ls_view>-vsh_nodes.
      SORT <ls_view>-vsh_placeholders.
      SORT <ls_view>-viewset_properties.
    ENDLOOP.

    SORT mt_components BY
      component_name ASCENDING
      controller_name ASCENDING
      cmpname ASCENDING.

    SORT mt_sources BY
      component_name ASCENDING
      controller_name ASCENDING
      cmpname ASCENDING
      line_number ASCENDING.

  ENDMETHOD.
  METHOD read_controller.

    DATA: lt_components   TYPE TABLE OF wdy_ctlr_compo_vrs,
          lt_sources      TYPE TABLE OF wdy_ctlr_compo_source_vrs,
          lt_definition   TYPE TABLE OF wdy_controller,
          lt_psmodilog    TYPE TABLE OF smodilog,
          lt_psmodisrc    TYPE TABLE OF smodisrc,
          lt_fm_param     TYPE abap_func_parmbind_tab,
          lt_fm_exception TYPE abap_func_excpbind_tab.

    FIELD-SYMBOLS: <lt_ctrl_exceptions>      TYPE ANY TABLE,
                   <lt_ctrl_exception_texts> TYPE ANY TABLE.

*   Calling FM dynamically because version 702 has less parameters

*   FM parameters
    add_fm_param_exporting( EXPORTING iv_name     = 'CONTROLLER_KEY'
                                      ig_value    = is_key
                            CHANGING  ct_param = lt_fm_param ).
    add_fm_param_exporting( EXPORTING iv_name     = 'GET_ALL_TRANSLATIONS'
                                      ig_value    = abap_false
                            CHANGING  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'DEFINITION'
                         CHANGING  ct_value = lt_definition
                                   ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'DESCRIPTIONS'
                         CHANGING ct_value = rs_controller-descriptions
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'CONTROLLER_USAGES'
                         CHANGING ct_value = rs_controller-controller_usages
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'CONTROLLER_COMPONENTS'
                         CHANGING ct_value = lt_components
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'CONTROLLER_COMPONENT_SOURCES'
                         CHANGING ct_value = lt_sources
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'CONTROLLER_COMPONENT_TEXTS'
                         CHANGING ct_value = rs_controller-controller_component_texts
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'CONTROLLER_PARAMETERS'
                         CHANGING ct_value = rs_controller-controller_parameters
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'CONTROLLER_PARAMETER_TEXTS'
                         CHANGING ct_value = rs_controller-controller_parameter_texts
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'CONTEXT_NODES'
                         CHANGING ct_value = rs_controller-context_nodes
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'CONTEXT_ATTRIBUTES'
                         CHANGING ct_value = rs_controller-context_attributes
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'CONTEXT_MAPPINGS'
                         CHANGING ct_value = rs_controller-context_mappings
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'FIELDGROUPS'
                         CHANGING ct_value = rs_controller-fieldgroups
                                  ct_param = lt_fm_param ).
*   Version 702 doesn't have these two attributes so we
*   use them dynamically for downward compatibility
    ASSIGN COMPONENT 'CONTROLLER_EXCEPTIONS' OF STRUCTURE rs_controller TO <lt_ctrl_exceptions>.
    IF sy-subrc = 0.
      add_fm_param_tables( EXPORTING iv_name = 'CONTROLLER_EXCEPTIONS'
                           CHANGING ct_value = <lt_ctrl_exceptions>
                                    ct_param = lt_fm_param ).
    ENDIF.
    ASSIGN COMPONENT 'CONTROLLER_EXCEPTION_TEXTS' OF STRUCTURE rs_controller TO <lt_ctrl_exception_texts>.
    IF sy-subrc = 0.
      add_fm_param_tables( EXPORTING iv_name = 'CONTROLLER_EXCEPTION_TEXTS'
                           CHANGING ct_value = <lt_ctrl_exception_texts>
                                    ct_param = lt_fm_param ).
    ENDIF.
    add_fm_param_tables( EXPORTING iv_name = 'PSMODILOG'
                         CHANGING ct_value = lt_psmodilog
                                  ct_param = lt_fm_param ).
    add_fm_param_tables( EXPORTING iv_name = 'PSMODISRC'
                         CHANGING ct_value = lt_psmodisrc
                                  ct_param = lt_fm_param ).

*   FM exceptions
    add_fm_exception( EXPORTING iv_name = 'NOT_EXISTING'
                                iv_value = 1
                      CHANGING ct_exception = lt_fm_exception ).
    add_fm_exception( EXPORTING iv_name = 'OTHERS'
                                iv_value = 2
                      CHANGING ct_exception = lt_fm_exception ).

    CALL FUNCTION 'WDYC_GET_OBJECT'
      PARAMETER-TABLE
      lt_fm_param
      EXCEPTION-TABLE
      lt_fm_exception.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from WDYC_GET_OBJECT' ).
    ENDIF.

    APPEND LINES OF lt_components TO mt_components.
    APPEND LINES OF lt_sources TO mt_sources.

    READ TABLE lt_definition INDEX 1 INTO rs_controller-definition.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'WDYC, definition not found' ).
    ENDIF.

    CLEAR: rs_controller-definition-author,
           rs_controller-definition-createdon,
           rs_controller-definition-changedby,
           rs_controller-definition-changedon.

  ENDMETHOD.
  METHOD read_definition.

    DATA: lt_definition TYPE TABLE OF wdy_component,
          lt_psmodilog  TYPE TABLE OF smodilog,
          lt_psmodisrc  TYPE TABLE OF smodisrc.


    CALL FUNCTION 'WDYD_GET_OBJECT'
      EXPORTING
        component_key           = is_key
        get_all_translations    = abap_false
      TABLES
        definition              = lt_definition
        descriptions            = rs_definition-descriptions
        component_usages        = rs_definition-component_usages
        interface_implementings = rs_definition-interface_implementings
        library_usages          = rs_definition-library_usages
        ext_ctlr_usages         = rs_definition-ext_ctlr_usages
        ext_ctx_mappings        = rs_definition-ext_ctx_mappings
        psmodilog               = lt_psmodilog " not optional in all versions
        psmodisrc               = lt_psmodisrc " not optional in all versions
      EXCEPTIONS
        not_existing            = 1
        OTHERS                  = 2.
    IF sy-subrc = 1.
      RETURN.
    ELSEIF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from WDYD_GET_OBJECT' ).
    ENDIF.

    READ TABLE lt_definition INDEX 1 INTO rs_definition-definition.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'WDYD, definition not found' ).
    ENDIF.

    CLEAR: rs_definition-definition-author,
           rs_definition-definition-createdon,
           rs_definition-definition-changedby,
           rs_definition-definition-changedon,
           rs_definition-definition-gendate,
           rs_definition-definition-gentime.

  ENDMETHOD.
  METHOD read_view.

    DATA: lt_definition TYPE TABLE OF wdy_view_vrs,
          lt_psmodilog  TYPE TABLE OF smodilog,
          lt_psmodisrc  TYPE TABLE OF smodisrc.

    FIELD-SYMBOLS: <ls_definition> LIKE LINE OF lt_definition.


    CALL FUNCTION 'WDYV_GET_OBJECT'
      EXPORTING
        view_key               = is_key
        get_all_translations   = abap_false
      TABLES
        definition             = lt_definition
        descriptions           = rs_view-descriptions
        view_containers        = rs_view-view_containers
        view_container_texts   = rs_view-view_container_texts
        iobound_plugs          = rs_view-iobound_plugs
        iobound_plug_texts     = rs_view-iobound_plug_texts
        plug_parameters        = rs_view-plug_parameters
        plug_parameter_texts   = rs_view-plug_parameter_texts
        ui_elements            = rs_view-ui_elements
        ui_context_bindings    = rs_view-ui_context_bindings
        ui_event_bindings      = rs_view-ui_event_bindings
        ui_ddic_bindings       = rs_view-ui_ddic_bindings
        ui_properties          = rs_view-ui_properties
        navigation_links       = rs_view-navigation_links
        navigation_target_refs = rs_view-navigation_target_refs
        vsh_nodes              = rs_view-vsh_nodes
        vsh_placeholders       = rs_view-vsh_placeholders
        viewset_properties     = rs_view-viewset_properties
        psmodilog              = lt_psmodilog
        psmodisrc              = lt_psmodisrc
      EXCEPTIONS
        not_existing           = 1
        OTHERS                 = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from WDYV_GET_OBJECT' ).
    ENDIF.

    READ TABLE lt_definition INDEX 1 ASSIGNING <ls_definition>.
    ASSERT sy-subrc = 0.
    MOVE-CORRESPONDING <ls_definition> TO rs_view-definition.

    CLEAR: rs_view-definition-author,
           rs_view-definition-createdon,
           rs_view-definition-changedby,
           rs_view-definition-changedon.

  ENDMETHOD.
  METHOD recover_controller.

    DATA: ls_key    TYPE wdy_controller_key,
          lv_corrnr TYPE trkorr,
          lx_error  TYPE REF TO cx_wdy_md_exception,
          ls_delta  TYPE svrs2_xversionable_object.


    ls_delta = delta_controller( is_controller ).
    ls_key-component_name  = is_controller-definition-component_name.
    ls_key-controller_name = is_controller-definition-controller_name.

    TRY.
        cl_wdy_md_controller=>recover_version(
          EXPORTING
            controller_key = ls_key
            delta          = ls_delta-wdyc
          CHANGING
            corrnr         = lv_corrnr ).
      CATCH cx_wdy_md_exception INTO lx_error.
        Lcx_abapgit_exception=>raise( |Error recovering version of controller: { lx_error->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.
  METHOD recover_definition.

    DATA: ls_key    TYPE wdy_md_component_key,
          lv_corrnr TYPE trkorr,
          lx_error  TYPE REF TO cx_wdy_md_exception,
          ls_delta  TYPE svrs2_xversionable_object.


    ls_delta = delta_definition(
      is_definition = is_definition
      iv_package    = iv_package ).

    ls_key-component_name = is_definition-definition-component_name.

    TRY.
        cl_wdy_md_component=>recover_version(
          EXPORTING
            component_key = ls_key
            delta         = ls_delta-wdyd
          CHANGING
            corrnr        = lv_corrnr ).
      CATCH cx_wdy_md_exception INTO lx_error.
        Lcx_abapgit_exception=>raise( |Error recovering version of component: { lx_error->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.
  METHOD recover_view.

    DATA: ls_key    TYPE wdy_md_view_key,
          lv_corrnr TYPE trkorr,
          lx_error  TYPE REF TO cx_wdy_md_exception,
          ls_delta  TYPE svrs2_xversionable_object.


    ls_delta = delta_view( is_view ).
    ls_key-component_name = is_view-definition-component_name.
    ls_key-view_name      = is_view-definition-view_name.

    TRY.
        cl_wdy_md_abstract_view=>recover_version(
          EXPORTING
            view_key = ls_key
            delta    = ls_delta-wdyv
          CHANGING
            corrnr   = lv_corrnr ).
      CATCH cx_wdy_md_exception INTO lx_error.
        Lcx_abapgit_exception=>raise( |Error recovering version of abstract view: { lx_error->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    SELECT SINGLE changedby FROM wdy_component INTO rv_user
      WHERE component_name = ms_item-obj_name AND version = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lo_component   TYPE REF TO cl_wdy_wb_component,
          lo_request     TYPE REF TO cl_wb_request,
          li_state       TYPE REF TO if_wb_program_state,
          lv_object_name TYPE seu_objkey.


    CREATE OBJECT lo_component.

    lv_object_name = ms_item-obj_name.
    CREATE OBJECT lo_request
      EXPORTING
        p_object_type = 'YC'
        p_object_name = lv_object_name
        p_operation   = swbm_c_op_delete_no_dialog.

    lo_component->if_wb_program~process_wb_request(
      p_wb_request       = lo_request
      p_wb_program_state = li_state ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_component   TYPE wdy_component_metadata,
          ls_description TYPE wdy_ext_ctx_map.

    FIELD-SYMBOLS: <ls_view>       LIKE LINE OF ls_component-view_metadata,
                   <ls_controller> LIKE LINE OF ls_component-ctlr_metadata.

    io_xml->read( EXPORTING iv_name = 'COMPONENT'
                  CHANGING cg_data = ls_component ).
    io_xml->read( EXPORTING iv_name  = 'COMPONENTS'
                  CHANGING cg_data = mt_components ).
    io_xml->read( EXPORTING iv_name  = 'SOURCES'
                  CHANGING cg_data = mt_sources ).

    ls_component-comp_metadata-definition-author = sy-uname.
    ls_component-comp_metadata-definition-createdon = sy-datum.
    recover_definition( is_definition = ls_component-comp_metadata
                        iv_package    = iv_package ).

    LOOP AT ls_component-ctlr_metadata ASSIGNING <ls_controller>.
      <ls_controller>-definition-author = sy-uname.
      <ls_controller>-definition-createdon = sy-datum.
      recover_controller( <ls_controller> ).
    ENDLOOP.
    LOOP AT ls_component-view_metadata ASSIGNING <ls_view>.
      <ls_view>-definition-author = sy-uname.
      <ls_view>-definition-createdon = sy-datum.
      recover_view( <ls_view> ).
    ENDLOOP.

    READ TABLE ls_component-comp_metadata-descriptions INTO ls_description INDEX 1.
    IF sy-subrc = 0.
      Lcl_abapgit_sotr_handler=>create_sotr(
        iv_package = iv_package
        io_xml     = io_xml ).
    ENDIF.

    add_with_inactive_parts( ).

    deserialize_longtexts(
      ii_xml         = io_xml
      iv_longtext_id = c_longtext_id_wd ).

    deserialize_longtexts(
      ii_xml           = io_xml
      iv_longtext_id   = c_longtext_id_wc
      iv_longtext_name = c_longtext_name_wc ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_component_name TYPE wdy_component-component_name.


    SELECT SINGLE component_name FROM wdy_component
      INTO lv_component_name
      WHERE component_name = ms_item-obj_name.          "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_component   TYPE wdy_component_metadata,
          ls_comp        TYPE wdy_ctlr_compo_vrs,
          lv_object      TYPE dokil-object,
          lt_object      TYPE STANDARD TABLE OF dokil-object WITH DEFAULT KEY,
          lt_dokil       TYPE STANDARD TABLE OF dokil WITH DEFAULT KEY,
          ls_description TYPE wdy_ext_ctx_map.

    ls_component = read( ).

    io_xml->add( iv_name = 'COMPONENT'
                 ig_data = ls_component ).
    io_xml->add( ig_data = mt_components
                 iv_name = 'COMPONENTS' ).
    io_xml->add( ig_data = mt_sources
                 iv_name = 'SOURCES' ).

    READ TABLE ls_component-comp_metadata-descriptions INTO ls_description INDEX 1.
    IF sy-subrc = 0.
      Lcl_abapgit_sotr_handler=>read_sotr(
        iv_pgmid    = 'LIMU'
        iv_object   = 'WDYV'
        iv_obj_name = ms_item-obj_name
        io_i18n_params = mo_i18n_params
        io_xml      = io_xml ).
    ENDIF.

    serialize_longtexts(
      ii_xml         = io_xml
      iv_longtext_id = c_longtext_id_wd ).

    LOOP AT mt_components INTO ls_comp.
      lv_object    = ls_comp-component_name.
      lv_object+30 = ls_comp-controller_name.
      COLLECT lv_object INTO lt_object.
    ENDLOOP.

    IF lt_object IS NOT INITIAL.
      IF mo_i18n_params->ms_params-main_language_only = abap_true.
        SELECT * FROM dokil INTO TABLE lt_dokil
          FOR ALL ENTRIES IN lt_object
          WHERE id = c_longtext_id_wc AND object = lt_object-table_line AND masterlang = abap_true
          ORDER BY PRIMARY KEY.
      ELSE.
        SELECT * FROM dokil INTO TABLE lt_dokil
          FOR ALL ENTRIES IN lt_object
          WHERE id = c_longtext_id_wc AND object = lt_object-table_line
          ORDER BY PRIMARY KEY.
      ENDIF.

      serialize_longtexts(
        ii_xml           = io_xml
        it_dokil         = lt_dokil
        iv_longtext_id   = c_longtext_id_wc
        iv_longtext_name = c_longtext_name_wc ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_WDYN implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_WEBI <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_webi=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_webi=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_WEBI implementation.
*"* method's implementations
*include methods.
  METHOD handle_endpoint.

    DATA: ls_endpoint LIKE LINE OF is_webi-pvependpoint,
          li_endpoint TYPE REF TO if_ws_md_vif_endpoint_ref.

    FIELD-SYMBOLS: <ls_function> LIKE LINE OF is_webi-pvepfunction.


    READ TABLE is_webi-pvependpoint INDEX 1 INTO ls_endpoint.
    ASSERT sy-subrc = 0.

    IF mi_vi->has_endpoint_reference( sews_c_vif_version-all ) = abap_true.
      RETURN.
    ENDIF.

    li_endpoint = mi_vi->create_endpoint_reference(
      endpoint_type          = ls_endpoint-endpointtype
      service_def_startpoint = ls_endpoint-def_start_pt
      auto_generated         = ls_endpoint-auto_generated
      i_is_srvv              = ls_endpoint-is_srvv ).

    IF ls_endpoint-endpointtype = 'BAPI'.
* it looks like some special handling is needed when calling
* set_data, and looking at the cluster data LS_ENDPOINT-CLUSTD
      Lcx_abapgit_exception=>raise( 'todo, WEBI BAPI' ).
    ENDIF.

* field ls_endpoint-endpointname does not exist in 702
    READ TABLE is_webi-pvepfunction INDEX 1 ASSIGNING <ls_function>.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |WEBI { ms_item-obj_name }: couldn't detect endpoint name| ).
    ENDIF.

    li_endpoint->set_data(
      data_version = '1'
      data         = <ls_function>-function ).

  ENDMETHOD.
  METHOD handle_function.



    DATA: li_parameter TYPE REF TO if_ws_md_vif_param,
          li_soap      TYPE REF TO if_ws_md_soap_ext_func,
          li_fault     TYPE REF TO if_ws_md_vif_fault,
          li_function  TYPE REF TO if_ws_md_vif_func.

    FIELD-SYMBOLS: <ls_function>  LIKE LINE OF is_webi-pvepfunction,
                   <ls_soap>      LIKE LINE OF is_webi-pvepfuncsoapext,
                   <ls_fault>     LIKE LINE OF is_webi-pvepfault,
                   <ls_parameter> LIKE LINE OF is_webi-pvepparameter.


    LOOP AT is_webi-pvepfunction ASSIGNING <ls_function>.

      IF mi_vi->has_function( funcname = <ls_function>-function
                              version  = sews_c_vif_version-active ) = abap_true.
        CONTINUE.
      ENDIF.

      IF mi_vi->has_function( funcname = <ls_function>-function
                              version  = sews_c_vif_version-inactive ) = abap_true.

        li_function = mi_vi->get_function( funcname = <ls_function>-function
                                           version  = sews_c_vif_version-inactive ).

      ELSE.

        li_function = mi_vi->create_function( funcname    = <ls_function>-function
                                              mapped_name = <ls_function>-mappedname ).

      ENDIF.

      li_function->set_is_exposed( <ls_function>-is_exposed ).

      LOOP AT is_webi-pvepparameter ASSIGNING <ls_parameter>
          WHERE function = <ls_function>-function.

        li_parameter = handle_single_parameter( iv_name           = <ls_parameter>-vepparam
                                                ii_function       = li_function
                                                iv_parameter_type = <ls_parameter>-vepparamtype ).

        li_parameter->set_name_mapped_to( <ls_parameter>-mappedname ).
        li_parameter->set_is_exposed( <ls_parameter>-is_exposed ).
        li_parameter->set_is_optional( <ls_parameter>-is_optional ).
        li_parameter->set_default_value( <ls_parameter>-default_value ).
        li_parameter->set_initial( <ls_parameter>-is_initial ).
        li_parameter->set_type( <ls_parameter>-typename ).
      ENDLOOP.

      LOOP AT is_webi-pvepfuncsoapext ASSIGNING <ls_soap>
          WHERE function = <ls_function>-function.
        IF li_function->has_soap_extension_function( 'I' ) = abap_true.
          li_function->delete_soap_extension_function( ).
        ENDIF.
        li_soap = li_function->create_soap_extension_function( ).
        li_soap->set_soap_request_name( <ls_soap>-requestname ).
        li_soap->set_soap_response_name( <ls_soap>-responsename ).
        li_soap->set_namespace( <ls_soap>-namespace ).
      ENDLOOP.

      LOOP AT is_webi-pvepfault ASSIGNING <ls_fault>
          WHERE function = <ls_function>-function.
        li_fault = li_function->create_fault( <ls_fault>-fault ).
        li_fault->set_name_mapped_to( <ls_fault>-mappedname ).
        li_fault->set_detail( <ls_fault>-detail ).
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.
  METHOD handle_single_parameter.
    CONSTANTS:
      BEGIN OF lc_parameter_type,
        import TYPE vepparamtype VALUE 'I',
        export TYPE vepparamtype VALUE 'O',
      END OF lc_parameter_type.

    CASE iv_parameter_type.
      WHEN lc_parameter_type-import.
        ri_parameter = ii_function->get_incoming_parameter( parameter_name  = iv_name
                                                            version         = 'I' ).
        IF ri_parameter IS BOUND.
          ii_function->delete_incoming_parameter( ri_parameter ).
        ENDIF.
        ri_parameter = ii_function->create_incoming_parameter( iv_name ).

      WHEN lc_parameter_type-export.

        ri_parameter = ii_function->get_outgoing_parameter( parameter_name  = iv_name
                                                            version         = 'I' ).
        IF ri_parameter IS BOUND.
          ii_function->delete_outgoing_parameter( parameter = ri_parameter ).
        ENDIF.

        ri_parameter = ii_function->create_outgoing_parameter( iv_name ).

      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.
  METHOD handle_soap.

    DATA: li_soap TYPE REF TO if_ws_md_soap_ext_virtinfc,
          ls_soap LIKE LINE OF is_webi-pvepvisoapext.


    READ TABLE is_webi-pvepvisoapext INDEX 1 INTO ls_soap.
    ASSERT sy-subrc = 0.

    IF mi_vi->has_soap_extension_virtinfc( sews_c_vif_version-active ) = abap_true.
      RETURN.
    ENDIF.

    IF mi_vi->has_soap_extension_virtinfc( sews_c_vif_version-inactive ) = abap_true.
      li_soap = mi_vi->get_soap_extension_virtinfc( sews_c_vif_version-inactive ).
    ELSE.
      li_soap = mi_vi->create_soap_extension_virtinfc( ls_soap-soap_appl_uri ).
    ENDIF.

    li_soap->set_namespace( ls_soap-namespace ).

  ENDMETHOD.
  METHOD handle_types.

    DATA: lv_index TYPE i,
          li_soap  TYPE REF TO if_ws_md_soap_extension_type,
          li_struc TYPE REF TO if_ws_md_vif_struc_type,
          li_field TYPE REF TO if_ws_md_vif_field,
          li_table TYPE REF TO if_ws_md_vif_table_type,
          li_elem  TYPE REF TO if_ws_md_vif_elem_type.

    FIELD-SYMBOLS: <ls_elem>  LIKE LINE OF is_webi-pvepelemtype,
                   <ls_table> LIKE LINE OF is_webi-pveptabletype,
                   <ls_soap>  LIKE LINE OF is_webi-pveptypesoapext,
                   <ls_struc> LIKE LINE OF is_webi-pvepstrutype.


    LOOP AT is_webi-pvepelemtype ASSIGNING <ls_elem>.
      li_elem = mi_vi->create_type_as_elementary( <ls_elem>-typename ).
      li_elem->set_built_in_type( <ls_elem>-build_in_type ).
      li_elem->set_decimals( <ls_elem>-decimals ).
      li_elem->set_kind( <ls_elem>-kind ).
      li_elem->set_length( <ls_elem>-length ).
      li_elem->set_signed( <ls_elem>-signed ).
      li_elem->set_abaptype( <ls_elem>-abaptype ).

      IF li_elem->if_ws_md_vif_type~has_soap_extension_type( sews_c_vif_version-all ) = abap_false.
        READ TABLE is_webi-pveptypesoapext ASSIGNING <ls_soap>
          WITH KEY typename = <ls_elem>-typename.
        IF sy-subrc = 0.
          li_soap = li_elem->if_ws_md_vif_type~create_soap_extension_type( ).
          li_soap->set_namespace( <ls_soap>-namespace ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT is_webi-pvepstrutype ASSIGNING <ls_struc>.
      lv_index = sy-tabix.

      li_struc = mi_vi->create_type_as_structure( <ls_struc>-typename ).

      IF li_struc->has_field( field_pos = <ls_struc>-fieldpos
          version = sews_c_vif_version-active ) = abap_true.
        CONTINUE.
      ENDIF.

      li_field = li_struc->create_field(
        field_name = <ls_struc>-fieldname
        fieldpos = <ls_struc>-fieldpos ).
      li_field->set_type( mi_vi->get_type( typename = <ls_struc>-typeref
                                           version  = sews_c_vif_version-inactive ) ).

      IF lv_index = 1
          AND li_struc->if_ws_md_vif_type~has_soap_extension_type(
          sews_c_vif_version-all ) = abap_false.
        READ TABLE is_webi-pveptypesoapext ASSIGNING <ls_soap>
          WITH KEY typename = <ls_struc>-typename.
        IF sy-subrc = 0.
          li_soap = li_struc->if_ws_md_vif_type~create_soap_extension_type( ).
          li_soap->set_namespace( <ls_soap>-namespace ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT is_webi-pveptabletype ASSIGNING <ls_table>.
      li_table = mi_vi->create_type_as_table( <ls_table>-typename ).
      li_table->set_line_type( mi_vi->get_type( typename = <ls_table>-typeref
                                                version  = sews_c_vif_version-inactive ) ).

      IF li_table->if_ws_md_vif_type~has_soap_extension_type( sews_c_vif_version-all ) = abap_false.
        READ TABLE is_webi-pveptypesoapext ASSIGNING <ls_soap>
          WITH KEY typename = <ls_table>-typename.
        IF sy-subrc = 0.
          li_soap = li_table->if_ws_md_vif_type~create_soap_extension_type( ).
          li_soap->set_namespace( <ls_soap>-namespace ).
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD sort.
    SORT cs_webi-pvepheader BY vepname version.
    SORT cs_webi-pvepfunction BY vepname version function.
    SORT cs_webi-pvepfault BY vepname version function fault.
    SORT cs_webi-pvepparameter BY vepname version function vepparam vepparamtype.
    SORT cs_webi-pveptype BY vepname version typename.
    SORT cs_webi-pvepelemtype BY vepname version typename.
    SORT cs_webi-pveptabletype BY vepname version typename.
    SORT cs_webi-pvepstrutype BY vepname version typename fieldpos.
    SORT cs_webi-pveptypesoapext BY vepname version typename.
    SORT cs_webi-pvepeletypsoap BY vepname version typename assign_type assign_data1 assign_data2.
    SORT cs_webi-pveptabtypsoap BY vepname version typename.
    SORT cs_webi-pvepfuncsoapext BY vepname version function.
    SORT cs_webi-pvepfieldref BY vepname version function vepparam vepparamtype strucid fieldname.
    SORT cs_webi-pvependpoint BY relid vepname version sortfield.
    SORT cs_webi-pvepvisoapext BY vepname version.
    SORT cs_webi-pvepparasoapext BY vepname version function vepparam vepparamtype.
    SORT cs_webi-pwsheader BY wsname version.
    SORT cs_webi-pwssoapprop BY wsname version feature soapapp funcref propnum.
  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE changedby FROM vepheader INTO rv_user
      WHERE vepname = ms_item-obj_name AND version = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_name TYPE vepname,
          lo_vif  TYPE REF TO cl_ws_md_vif_root.


    lv_name = ms_item-obj_name.

    CREATE OBJECT lo_vif.
    TRY.
        lo_vif->if_ws_md_vif_root~delete_virtual_interface( lv_name ).
      CATCH cx_ws_md_exception.
        Lcx_abapgit_exception=>raise( 'error deleting WEBI' ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_webi     TYPE ty_webi,
          lv_name     TYPE vepname,
          ls_header   LIKE LINE OF ls_webi-pvepheader,
          lx_root     TYPE REF TO cx_root,
          lv_exists   TYPE abap_bool,
          li_root     TYPE REF TO if_ws_md_vif_root,
          ls_endpoint LIKE LINE OF ls_webi-pvependpoint.

    io_xml->read( EXPORTING iv_name = 'WEBI'
                  CHANGING  cg_data = ls_webi ).

    lv_name = ms_item-obj_name.

    READ TABLE ls_webi-pvependpoint INDEX 1 INTO ls_endpoint.
    ASSERT sy-subrc = 0.
    IF ls_endpoint-auto_generated = abap_true.
      " handled by SPRX.
      RETURN.
    ENDIF.

    READ TABLE ls_webi-pvepheader INDEX 1 INTO ls_header.
    ASSERT sy-subrc = 0.

    lv_exists = cl_ws_md_vif_root=>check_existence_by_vif_name(
      name      = lv_name
      i_version = sews_c_vif_version-all ).

    li_root = cl_ws_md_factory=>get_vif_root( ).
    TRY.
        IF lv_exists = abap_false.
          mi_vi = li_root->create_virtual_interface(
            name    = lv_name
            nameext = ls_header-vepnameext ).
        ELSE.
          mi_vi = li_root->get_virtual_interface( lv_name ).
          mi_vi->if_ws_md_lockable_object~lock( ).
        ENDIF.

        mi_vi->set_short_text( ls_webi-veptext ).

        handle_endpoint( ls_webi ).
        handle_types( ls_webi ).
        handle_function( ls_webi ).
        handle_soap( ls_webi ).

        tadir_insert( iv_package ).

        mi_vi->if_ws_md_lockable_object~save( ).
        mi_vi->if_ws_md_lockable_object~unlock( ).
      CATCH cx_ws_md_exception INTO lx_root.
        TRY.
            mi_vi->if_ws_md_lockable_object~unlock( ).
          CATCH cx_ws_md_exception ##NO_HANDLER.
        ENDTRY.
        Lcx_abapgit_exception=>raise_with_text( lx_root ).
    ENDTRY.

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

    Lcl_abapgit_sotr_handler=>create_sotr(
      iv_package = iv_package
      io_xml     = io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_name TYPE vepname.
    DATA lv_generated TYPE abap_bool.

    lv_name = ms_item-obj_name.

    " Check if service is generated by proxy
    SELECT SINGLE auto_generated FROM vependpoint INTO lv_generated
      WHERE vepname = lv_name AND version = sews_c_vif_version-active.
    IF sy-subrc = 0 AND lv_generated = abap_true.
      RETURN.
    ENDIF.

    rv_bool = cl_ws_md_vif_root=>check_existence_by_vif_name(
      name      = lv_name
      i_version = sews_c_vif_version-all ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_webi    TYPE ty_webi,
          lx_error   TYPE REF TO cx_ws_md_exception,
          lt_modilog TYPE STANDARD TABLE OF smodilog WITH DEFAULT KEY,
          li_vi      TYPE REF TO if_ws_md_vif,
          lv_name    TYPE vepname.

    FIELD-SYMBOLS: <ls_vepheader>   LIKE LINE OF ls_webi-pvepheader,
                   <ls_vependpoint> LIKE LINE OF ls_webi-pvependpoint,
                   <ls_wsheader>    TYPE wsheader.

    CALL FUNCTION 'WEBI_GET_OBJECT'
      EXPORTING
        webiname          = ms_item-obj_name
      TABLES
        psmodilog         = lt_modilog
        pvepheader        = ls_webi-pvepheader
        pvepfunction      = ls_webi-pvepfunction
        pvepfault         = ls_webi-pvepfault
        pvepparameter     = ls_webi-pvepparameter
        pveptype          = ls_webi-pveptype
        pvepelemtype      = ls_webi-pvepelemtype
        pveptabletype     = ls_webi-pveptabletype
        pvepstrutype      = ls_webi-pvepstrutype
        pveptypesoapext   = ls_webi-pveptypesoapext
        pvepeletypsoap    = ls_webi-pvepeletypsoap
        pveptabtypsoap    = ls_webi-pveptabtypsoap
        pvepfuncsoapext   = ls_webi-pvepfuncsoapext
        pvepfieldref      = ls_webi-pvepfieldref
        pvependpoint      = ls_webi-pvependpoint
        pvepvisoapext     = ls_webi-pvepvisoapext
        pvepparasoapext   = ls_webi-pvepparasoapext
        pwsheader         = ls_webi-pwsheader
        pwssoapprop       = ls_webi-pwssoapprop
      EXCEPTIONS
        version_not_found = 1
        webi_not_exist    = 2
        OTHERS            = 3.
    IF sy-subrc = 1.
      " no active version
      RETURN.
    ELSEIF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    sort( CHANGING cs_webi = ls_webi ).

    lv_name = ms_item-obj_name.
    TRY.
        li_vi = cl_ws_md_factory=>get_vif_root( )->get_virtual_interface( lv_name ).
        ls_webi-veptext = li_vi->get_short_text( sews_c_vif_version-active ).
      CATCH cx_ws_md_exception INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    LOOP AT ls_webi-pvepheader ASSIGNING <ls_vepheader>.
      CLEAR <ls_vepheader>-author.
      CLEAR <ls_vepheader>-createdon.
      CLEAR <ls_vepheader>-changedby.
      CLEAR <ls_vepheader>-changedon.
      CLEAR <ls_vepheader>-ctime.
      CLEAR <ls_vepheader>-text_id.
      CLEAR <ls_vepheader>-utime.
      CLEAR <ls_vepheader>-wsint_version.
    ENDLOOP.

    LOOP AT ls_webi-pvependpoint ASSIGNING <ls_vependpoint>.
      CLEAR <ls_vependpoint>-clustd.
    ENDLOOP.

    LOOP AT ls_webi-pwsheader ASSIGNING <ls_wsheader>.

      CLEAR:
        <ls_wsheader>-author,
        <ls_wsheader>-createdon,
        <ls_wsheader>-changedby,
        <ls_wsheader>-changedon,
        <ls_wsheader>-ctime,
        <ls_wsheader>-utime.

    ENDLOOP.

    io_xml->add( iv_name = 'WEBI'
                 ig_data = ls_webi ).

    Lcl_abapgit_sotr_handler=>read_sotr(
      iv_pgmid    = 'R3TR'
      iv_object   = ms_item-obj_type
      iv_obj_name = ms_item-obj_name
      io_i18n_params = mo_i18n_params
      io_xml      = io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_WEBI implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_DCLS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_dcls=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_dcls=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_DCLS implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.
    DATA: lr_data  TYPE REF TO data,
          lo_dcl   TYPE REF TO object,
          lx_error TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_data>  TYPE any,
                   <lg_field> TYPE any.

    CREATE DATA lr_data TYPE ('ACM_S_DCLSRC').
    ASSIGN lr_data->* TO <lg_data>.

    TRY.
        CALL METHOD ('CL_ACM_DCL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            ro_handler = lo_dcl.

        CALL METHOD lo_dcl->('READ')
          EXPORTING
            iv_dclname = ms_item-obj_name
          IMPORTING
            es_dclsrc  = <lg_data>.

        ASSIGN COMPONENT 'AS4USER' OF STRUCTURE <lg_data> TO <lg_field>.
        IF sy-subrc = 0.
          rv_user = <lg_field>.
        ELSE.
          rv_user = c_user_unknown.
        ENDIF.
      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lo_dcl   TYPE REF TO object,
          lx_error TYPE REF TO cx_root.

    TRY.
        CALL METHOD ('CL_ACM_DCL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            ro_handler = lo_dcl.

        CALL METHOD lo_dcl->('DELETE')
          EXPORTING
            iv_dclname = ms_item-obj_name.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lr_data  TYPE REF TO data,
          lo_dcl   TYPE REF TO object,
          lx_error TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_data>  TYPE any,
                   <lg_field> TYPE any.


    CREATE DATA lr_data TYPE ('ACM_S_DCLSRC').
    ASSIGN lr_data->* TO <lg_data>.

    io_xml->read(
      EXPORTING
        iv_name = 'DCLS'
      CHANGING
        cg_data = <lg_data> ).

    ASSIGN COMPONENT 'SOURCE' OF STRUCTURE <lg_data> TO <lg_field>.
    ASSERT sy-subrc = 0.
    <lg_field> = Lif_abapgit_object~mo_files->read_string( 'asdcls' ).

    TRY.
        tadir_insert( iv_package ).

        CALL METHOD ('CL_ACM_DCL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            ro_handler = lo_dcl.

        CALL METHOD lo_dcl->('SAVE')
          EXPORTING
            iv_dclname     = ms_item-obj_name
            iv_put_state   = 'I'
            is_dclsrc      = <lg_data>
            iv_devclass    = iv_package
            iv_access_mode = 'INSERT'.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lo_dcl   TYPE REF TO object,
          lx_error TYPE REF TO cx_root.

    TRY.
        CALL METHOD ('CL_ACM_DCL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            ro_handler = lo_dcl.

        CALL METHOD lo_dcl->('CHECK_EXISTENCE')
          EXPORTING
            iv_objectname = ms_item-obj_name
          RECEIVING
            rv_exists     = rv_bool.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-late TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'E_ACMDCLSRC'
                                            iv_argument    = |{ ms_item-obj_name }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_ADT_LINK=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lr_data  TYPE REF TO data,
          lo_dcl   TYPE REF TO object,
          lx_error TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_data>  TYPE any,
                   <lg_field> TYPE any.


    CREATE DATA lr_data TYPE ('ACM_S_DCLSRC').
    ASSIGN lr_data->* TO <lg_data>.

    TRY.
        CALL METHOD ('CL_ACM_DCL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            ro_handler = lo_dcl.

        CALL METHOD lo_dcl->('READ')
          EXPORTING
            iv_dclname = ms_item-obj_name
          IMPORTING
            es_dclsrc  = <lg_data>.

        ASSIGN COMPONENT 'AS4USER' OF STRUCTURE <lg_data> TO <lg_field>.
        ASSERT sy-subrc = 0.
        CLEAR <lg_field>.

        ASSIGN COMPONENT 'AS4DATE' OF STRUCTURE <lg_data> TO <lg_field>.
        ASSERT sy-subrc = 0.
        CLEAR <lg_field>.

        ASSIGN COMPONENT 'AS4TIME' OF STRUCTURE <lg_data> TO <lg_field>.
        ASSERT sy-subrc = 0.
        CLEAR <lg_field>.

        ASSIGN COMPONENT 'CREATED_BY' OF STRUCTURE <lg_data> TO <lg_field>.
        ASSERT sy-subrc = 0.
        CLEAR <lg_field>.

        ASSIGN COMPONENT 'CREATED_DATE' OF STRUCTURE <lg_data> TO <lg_field>.
        ASSERT sy-subrc = 0.
        CLEAR <lg_field>.

        ASSIGN COMPONENT 'AS4LOCAL' OF STRUCTURE <lg_data> TO <lg_field>.
        ASSERT sy-subrc = 0.
        CLEAR <lg_field>.

        ASSIGN COMPONENT 'ABAP_LANGUAGE_VERSION' OF STRUCTURE <lg_data> TO <lg_field>.
        IF sy-subrc = 0.
          CLEAR <lg_field>.
        ENDIF.

        ASSIGN COMPONENT 'SOURCE' OF STRUCTURE <lg_data> TO <lg_field>.
        ASSERT sy-subrc = 0.

        Lif_abapgit_object~mo_files->add_string(
          iv_ext    = 'asdcls'
          iv_string = <lg_field> ).

        CLEAR <lg_field>.

        io_xml->add( iv_name = 'DCLS'
                     ig_data = <lg_data> ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_DCLS implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_DDLS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ddls=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ddls=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_DDLS implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    DATA lo_ddl TYPE REF TO object.

    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).

    TRY.
        CALL METHOD ('CL_DD_DDL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            handler = lo_ddl.
      CATCH cx_root.
        Lcx_abapgit_exception=>raise( 'Object type DDLS is not supported by this system' ).
    ENDTRY.

  ENDMETHOD.
  METHOD format_source_before_serialize.

    DATA:
      lv_len       TYPE i,
      lv_lastchar1 TYPE c,
      lv_lastchar2 TYPE c.

    " New line included in 751+ by CL_DD_DDL_HANDLER=>ADD_BASEOBJS_INFO_TO_DDLS
    " Change for 750-

    lv_len = strlen( cv_string ) - 1.
    IF lv_len < 0.
      RETURN.
    ENDIF.
    lv_lastchar1 = cv_string+lv_len(1).

    lv_len = strlen( cv_string ) - 2.
    IF lv_len < 0.
      RETURN.
    ENDIF.
    lv_lastchar2 = cv_string+lv_len(1).

    " only add a line break, if the last character is unequal to cr_lf and newline !
    IF lv_lastchar1 <> cl_abap_char_utilities=>cr_lf AND lv_lastchar1 <> cl_abap_char_utilities=>newline AND
        lv_lastchar1 <> space OR
        ( lv_lastchar1 = space AND
          ( lv_lastchar2 <> cl_abap_char_utilities=>cr_lf AND lv_lastchar2 <> cl_abap_char_utilities=>newline ) ).
      cv_string = |{ cv_string }{ cl_abap_char_utilities=>cr_lf }|.
    ENDIF.

  ENDMETHOD.
  METHOD is_baseinfo_supported.

    DATA:
      lr_data_baseinfo TYPE REF TO data,
      lx_error         TYPE REF TO cx_root.

    TRY.
        CREATE DATA lr_data_baseinfo TYPE ('IF_DD_DDL_TYPES=>TY_S_BASEINFO_STRING_SAVE').
        rv_supported = abap_true.
      CATCH cx_root INTO lx_error.
        rv_supported = abap_false.
    ENDTRY.

  ENDMETHOD.
  METHOD open_adt_stob.

    DATA: lr_data  TYPE REF TO data,
          lo_ddl   TYPE REF TO object,
          lx_error TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lt_ddnames>     TYPE STANDARD TABLE.
    FIELD-SYMBOLS: <lt_entity_view> TYPE STANDARD TABLE.
    FIELD-SYMBOLS: <lg_ddnames>     TYPE any.
    FIELD-SYMBOLS: <lg_entity_view> TYPE any.
    FIELD-SYMBOLS: <lg_ddname>      TYPE any.
    FIELD-SYMBOLS: <lg_ddlname>     TYPE any.


    TRY.
        CREATE DATA lr_data TYPE ('IF_DD_DDL_TYPES=>TY_T_DDOBJ').
        ASSIGN lr_data->* TO <lt_ddnames>.

        CREATE DATA lr_data LIKE LINE OF <lt_ddnames>.
        ASSIGN lr_data->* TO <lg_ddnames>.

        CREATE DATA lr_data TYPE ('IF_DD_DDL_TYPES=>TY_T_ENTITY_OF_VIEW').
        ASSIGN lr_data->* TO <lt_entity_view>.

        CREATE DATA lr_data LIKE LINE OF <lt_entity_view>.
        ASSIGN lr_data->* TO <lg_entity_view>.

        CLEAR <lt_ddnames>.
        ASSIGN COMPONENT 'NAME' OF STRUCTURE <lg_ddnames> TO <lg_ddname>.
        <lg_ddname> = iv_ddls_name.
        INSERT <lg_ddnames> INTO TABLE <lt_ddnames>.

        CALL METHOD ('CL_DD_DDL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            handler = lo_ddl.

        CALL METHOD lo_ddl->('IF_DD_DDL_HANDLER~GET_VIEWNAME_FROM_ENTITYNAME')
          EXPORTING
            ddnames        = <lt_ddnames>
          IMPORTING
            view_of_entity = <lt_entity_view>.

        READ TABLE <lt_entity_view> ASSIGNING <lg_entity_view> INDEX 1.
        IF sy-subrc = 0.
          ASSIGN COMPONENT 'DDLNAME' OF STRUCTURE <lg_entity_view> TO <lg_ddlname>.

          Lcl_abapgit_adt_link=>jump( iv_obj_name = <lg_ddlname>
                                      iv_obj_type = 'DDLS' ).

        ENDIF.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD read_baseinfo.

    TRY.
        rv_baseinfo_string = Lif_abapgit_object~mo_files->read_string( 'baseinfo' ).

      CATCH Lcx_abapgit_exception.
        " File not found. That's ok, as the object could have been created in a
        " system where baseinfo wasn't supported.
        RETURN.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lo_ddl   TYPE REF TO object,
          lr_data  TYPE REF TO data,
          lx_error TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_data>  TYPE any,
                   <lg_field> TYPE any.


    TRY.
        CREATE DATA lr_data TYPE ('DDDDLSRCV').
        ASSIGN lr_data->* TO <lg_data>.

        CALL METHOD ('CL_DD_DDL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            handler = lo_ddl.

        CALL METHOD lo_ddl->('IF_DD_DDL_HANDLER~READ')
          EXPORTING
            name         = ms_item-obj_name
            get_state    = 'A'
          IMPORTING
            ddddlsrcv_wa = <lg_data>.

        ASSIGN COMPONENT 'AS4USER' OF STRUCTURE <lg_data> TO <lg_field>.
        IF sy-subrc = 0.
          rv_user = <lg_field>.
        ENDIF.
      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    IF rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA:
      lt_deltab TYPE TABLE OF dcdeltb,
      ls_deltab TYPE dcdeltb,
      lt_gentab TYPE TABLE OF dcgentb,
      lv_rc     TYPE sy-subrc.

    " CL_DD_DDL_HANDLER->DELETE does not work for CDS views that reference other views
    " To drop any views regardless of reference, we use delnoref = false
    ls_deltab-objtyp  = 'DDLS'.
    ls_deltab-objname = ms_item-obj_name.
    APPEND ls_deltab TO lt_deltab.

    CALL FUNCTION 'DD_MASS_ACT_C3'
      EXPORTING
        ddmode         = 'O'
        inactive       = abap_true
        write_log      = abap_false
        delall         = abap_true
        delnoref       = abap_false
        prid           = -1
      IMPORTING
        act_rc         = lv_rc
      TABLES
        gentab         = lt_gentab
        deltab         = lt_deltab
      EXCEPTIONS
        access_failure = 1
        no_objects     = 2
        locked         = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA:
      lo_ddl           TYPE REF TO object,
      lr_data          TYPE REF TO data,
      lr_data_baseinfo TYPE REF TO data,
      lx_error         TYPE REF TO cx_root.

    FIELD-SYMBOLS:
      <lg_data>             TYPE any,
      <lg_data_baseinfo>    TYPE any,
      <lg_source>           TYPE any,
      <lg_baseinfo_string>  TYPE any,
      <lg_baseinfo_ddlname> TYPE any.

    TRY.
        CREATE DATA lr_data TYPE ('DDDDLSRCV').
        ASSIGN lr_data->* TO <lg_data>.

        io_xml->read( EXPORTING iv_name = 'DDLS'
                      CHANGING cg_data  = <lg_data> ).

        ASSIGN COMPONENT 'SOURCE' OF STRUCTURE <lg_data> TO <lg_source>.
        ASSERT sy-subrc = 0.
        <lg_source> = Lif_abapgit_object~mo_files->read_string( 'asddls' ).

        CALL METHOD ('CL_DD_DDL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            handler = lo_ddl.

        IF is_baseinfo_supported( ) = abap_true.
          CREATE DATA lr_data_baseinfo TYPE ('IF_DD_DDL_TYPES=>TY_S_BASEINFO_STRING_SAVE').
          ASSIGN lr_data_baseinfo->* TO <lg_data_baseinfo>.

          ASSIGN COMPONENT 'BASEINFO_STRING' OF STRUCTURE <lg_data_baseinfo> TO <lg_baseinfo_string>.
          ASSERT sy-subrc = 0.

          <lg_baseinfo_string> = read_baseinfo( ).

          ASSIGN COMPONENT 'DDLNAME' OF STRUCTURE <lg_data_baseinfo> TO <lg_baseinfo_ddlname>.
          ASSERT sy-subrc = 0.
          <lg_baseinfo_ddlname> = ms_item-obj_name.

          CALL METHOD lo_ddl->('IF_DD_DDL_HANDLER~SAVE')
            EXPORTING
              name            = ms_item-obj_name
              put_state       = 'N'
              ddddlsrcv_wa    = <lg_data>
              baseinfo_string = <lg_data_baseinfo>.
        ELSE.
          CALL METHOD lo_ddl->('IF_DD_DDL_HANDLER~SAVE')
            EXPORTING
              name         = ms_item-obj_name
              put_state    = 'N'
              ddddlsrcv_wa = <lg_data>.
        ENDIF.

        CALL METHOD lo_ddl->('IF_DD_DDL_HANDLER~WRITE_TADIR')
          EXPORTING
            objectname = ms_item-obj_name
            devclass   = iv_package
            prid       = 0.

        corr_insert( iv_package ).

      CATCH cx_root INTO lx_error.
        IF lo_ddl IS NOT INITIAL.
          " Attempt clean-up but catch error if it doesn't work
          TRY.
              CALL METHOD lo_ddl->('IF_DD_DDL_HANDLER~DELETE')
                EXPORTING
                  name = ms_item-obj_name
                  prid = 0.
            CATCH cx_root ##NO_HANDLER.
          ENDTRY.
        ENDIF.

        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_state TYPE objstate,
          lo_ddl   TYPE REF TO object.

    TRY.
        CALL METHOD ('CL_DD_DDL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            handler = lo_ddl.

        CALL METHOD lo_ddl->('IF_DD_DDL_HANDLER~READ')
          EXPORTING
            name      = ms_item-obj_name
          IMPORTING
            got_state = lv_state.
        rv_bool = boolc( NOT lv_state IS INITIAL ).
      CATCH cx_root.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-ddic TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESDICT'
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: lv_typename   TYPE typename.
    DATA: lv_ddtypekind TYPE ddtypekind.

    lv_typename = ms_item-obj_name.

    CALL FUNCTION 'DDIF_TYPEINFO_GET'
      EXPORTING
        typename = lv_typename
      IMPORTING
        typekind = lv_ddtypekind.

    IF lv_ddtypekind = 'STOB'.
      open_adt_stob( ms_item-obj_name ).
      rv_exit = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lo_ddl           TYPE REF TO object,
          lr_data          TYPE REF TO data,
          lr_data_baseinfo TYPE REF TO data,
          lt_clr_comps     TYPE STANDARD TABLE OF fieldname WITH DEFAULT KEY,
          lx_error         TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_data>          TYPE any,
                   <lg_field>         TYPE any,
                   <lv_comp>          LIKE LINE OF lt_clr_comps,
                   <lt_data_baseinfo> TYPE ANY TABLE,
                   <lg_data_baseinfo> TYPE any,
                   <lg_ddlname>       TYPE any,
                   <lg_as4local>      TYPE any.


    TRY.
        CREATE DATA lr_data TYPE ('DDDDLSRCV').
        ASSIGN lr_data->* TO <lg_data>.

        CALL METHOD ('CL_DD_DDL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            handler = lo_ddl.

        IF is_baseinfo_supported( ) = abap_true.
          CREATE DATA lr_data_baseinfo TYPE ('IF_DD_DDL_TYPES=>TY_T_BASEINFO_STRING').
          ASSIGN lr_data_baseinfo->* TO <lt_data_baseinfo>.
          ASSIGN lr_data_baseinfo->* TO <lg_data_baseinfo>.

          CALL METHOD lo_ddl->('IF_DD_DDL_HANDLER~READ')
            EXPORTING
              name            = ms_item-obj_name
              get_state       = 'A'
            IMPORTING
              ddddlsrcv_wa    = <lg_data>
              baseinfo_string = <lt_data_baseinfo>.

          LOOP AT <lt_data_baseinfo> ASSIGNING <lg_data_baseinfo>.
            ASSIGN COMPONENT 'DDLNAME' OF STRUCTURE <lg_data_baseinfo> TO <lg_ddlname>.
            ASSERT sy-subrc = 0.

            ASSIGN COMPONENT 'AS4LOCAL' OF STRUCTURE <lg_data_baseinfo> TO <lg_as4local>.
            ASSERT sy-subrc = 0.

            IF <lg_ddlname> = ms_item-obj_name AND <lg_as4local> = 'A'.
              ASSIGN COMPONENT 'BASEINFO_STRING' OF STRUCTURE <lg_data_baseinfo> TO <lg_field>.
              ASSERT sy-subrc = 0.
              Lif_abapgit_object~mo_files->add_string(
                iv_ext    = 'baseinfo'
                iv_string = <lg_field> ).
              EXIT.
            ENDIF.
          ENDLOOP.
        ELSE.
          CALL METHOD lo_ddl->('IF_DD_DDL_HANDLER~READ')
            EXPORTING
              name         = ms_item-obj_name
              get_state    = 'A'
            IMPORTING
              ddddlsrcv_wa = <lg_data>.
        ENDIF.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    APPEND 'AS4USER'               TO lt_clr_comps.
    APPEND 'AS4DATE'               TO lt_clr_comps.
    APPEND 'AS4TIME'               TO lt_clr_comps.
    APPEND 'ACTFLAG'               TO lt_clr_comps.
    APPEND 'CHGFLAG'               TO lt_clr_comps.
    APPEND 'ABAP_LANGUAGE_VERSION' TO lt_clr_comps.
    APPEND 'ABAP_LANGU_VERSION'    TO lt_clr_comps.

    LOOP AT lt_clr_comps ASSIGNING <lv_comp>.
      ASSIGN COMPONENT <lv_comp> OF STRUCTURE <lg_data> TO <lg_field>.
      IF sy-subrc = 0.
        CLEAR <lg_field>.
      ENDIF.
    ENDLOOP.

    ASSIGN COMPONENT 'SOURCE' OF STRUCTURE <lg_data> TO <lg_field>.
    ASSERT sy-subrc = 0.

    format_source_before_serialize( CHANGING cv_string = <lg_field> ).

    Lif_abapgit_object~mo_files->add_string(
      iv_ext    = 'asddls'
      iv_string = <lg_field> ).

    CLEAR <lg_field>.

    io_xml->add( iv_name = 'DDLS'
                 ig_data = <lg_data> ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_DDLS implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_DDLX <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ddlx=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ddlx=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_DDLX implementation.
*"* method's implementations
*include methods.
  METHOD clear_field.

    FIELD-SYMBOLS: <lg_field> TYPE data.

    ASSIGN COMPONENT iv_fieldname
           OF STRUCTURE cg_metadata
           TO <lg_field>.
    IF sy-subrc = 0.
      CLEAR: <lg_field>.
    ENDIF.

  ENDMETHOD.
  METHOD clear_fields.

    FIELD-SYMBOLS: <lg_metadata> TYPE any.

    ASSIGN COMPONENT 'METADATA'
           OF STRUCTURE cg_data
           TO <lg_metadata>.
    ASSERT sy-subrc = 0.

    clear_field( EXPORTING iv_fieldname = 'CHANGED_AT'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'CHANGED_BY'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'CREATED_AT'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'CREATED_BY'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'RESPONSIBLE'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'PACKAGE_REF-NAME'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'CONTAINER_REF-PACKAGE_NAME'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'VERSION'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'RESPONSIBLE'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'MASTER_SYSTEM'
                 CHANGING  cg_metadata  = <lg_metadata> ).

    clear_field( EXPORTING iv_fieldname = 'ABAP_LANGUAGE_VERSION'
                 CHANGING  cg_metadata  = <lg_metadata> ).
    clear_field( EXPORTING iv_fieldname = 'ABAP_LANGU_VERSION'
                 CHANGING  cg_metadata  = <lg_metadata> ).

  ENDMETHOD.
  METHOD get_persistence.

    DATA: lx_error TYPE REF TO cx_root.

    TRY.
        IF mi_persistence IS NOT BOUND.

          CREATE OBJECT mi_persistence
                 TYPE ('CL_DDLX_ADT_OBJECT_PERSIST').

        ENDIF.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    ri_persistence = mi_persistence.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA:
      lv_object_key  TYPE seu_objkey,
      li_data_model  TYPE REF TO if_wb_object_data_model,
      li_persistence TYPE REF TO if_wb_object_persist,
      lr_data        TYPE REF TO data.

    FIELD-SYMBOLS:
      <lg_data>       TYPE any,
      <lg_changed_by> TYPE data.

    lv_object_key = ms_item-obj_name.

    TRY.
        CREATE DATA lr_data
          TYPE ('CL_DDLX_WB_OBJECT_DATA=>TY_OBJECT_DATA').
        ASSIGN lr_data->* TO <lg_data>.

        CREATE OBJECT li_data_model
          TYPE ('CL_DDLX_WB_OBJECT_DATA').

        li_persistence = get_persistence( ).

        li_persistence->get(
          EXPORTING
            p_object_key  = lv_object_key
            p_version     = swbm_version_active
          CHANGING
            p_object_data = li_data_model ).
      CATCH cx_root.
        rv_user = c_user_unknown.
        RETURN.
    ENDTRY.

    li_data_model->get_data( IMPORTING p_data = <lg_data> ).

    ASSIGN COMPONENT 'METADATA-CHANGED_BY' OF STRUCTURE <lg_data> TO <lg_changed_by>.
    ASSERT sy-subrc = 0.
    rv_user = <lg_changed_by>.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_object_key TYPE seu_objkey,
          lx_error      TYPE REF TO cx_root.


    lv_object_key = ms_item-obj_name.

    TRY.

        get_persistence( )->delete( p_object_key = lv_object_key
                                    p_version    = swbm_version_active ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: li_data_model TYPE REF TO if_wb_object_data_model,
          lr_data       TYPE REF TO data,
          lx_error      TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_data>    TYPE any,
                   <lg_source>  TYPE data,
                   <lg_version> TYPE data,
                   <lg_package> TYPE data.

    TRY.
        CREATE DATA lr_data
          TYPE ('CL_DDLX_WB_OBJECT_DATA=>TY_OBJECT_DATA').
        ASSIGN lr_data->* TO <lg_data>.

        io_xml->read(
          EXPORTING
            iv_name = 'DDLX'
          CHANGING
            cg_data = <lg_data> ).

        ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <lg_data> TO <lg_source>.
        ASSERT sy-subrc = 0.

        TRY.
            " If the file doesn't exist that's ok, because previously
            " the source code was stored in the xml. We are downward compatible.
            <lg_source> = Lif_abapgit_object~mo_files->read_string( 'asddlxs' ).
          CATCH Lcx_abapgit_exception ##NO_HANDLER.
        ENDTRY.

        CREATE OBJECT li_data_model
          TYPE ('CL_DDLX_WB_OBJECT_DATA').

        ASSIGN COMPONENT 'METADATA-VERSION' OF STRUCTURE <lg_data> TO <lg_version>.
        ASSERT sy-subrc = 0.

        " We have to always save as inactive. Standard activation below activates then
        " and also creates transport request entry if necessary
        <lg_version> = 'inactive'.

        "package needed to be able to determine ABAP language version
        ASSIGN COMPONENT 'METADATA-PACKAGE_REF-NAME' OF STRUCTURE <lg_data> TO <lg_package>.
        IF <lg_package> IS ASSIGNED.
          <lg_package> = iv_package.
        ENDIF.

        li_data_model->set_data( <lg_data> ).

        get_persistence( )->save( li_data_model ).

        tadir_insert( iv_package ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_object_key TYPE seu_objkey.

    lv_object_key = ms_item-obj_name.

    rv_bool = abap_true.

    TRY.
        get_persistence( )->get( p_object_key           = lv_object_key
                                 p_version              = swbm_version_active
                                 p_existence_check_only = abap_true ).

      CATCH cx_swb_exception.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESWB_EO'
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_ADT_LINK=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_object_key  TYPE seu_objkey,
          li_data_model  TYPE REF TO if_wb_object_data_model,
          li_persistence TYPE REF TO if_wb_object_persist,
          lr_data        TYPE REF TO data,
          lx_error       TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_data>  TYPE any,
                   <lg_field> TYPE data.

    lv_object_key = ms_item-obj_name.

    TRY.
        CREATE DATA lr_data
          TYPE ('CL_DDLX_WB_OBJECT_DATA=>TY_OBJECT_DATA').
        ASSIGN lr_data->* TO <lg_data>.

        CREATE OBJECT li_data_model
          TYPE ('CL_DDLX_WB_OBJECT_DATA').

        li_persistence = get_persistence( ).

        IF Lcl_abapgit_factory=>get_environment( )->compare_with_inactive( ) = abap_true.
          "Retrieve inactive version
          li_persistence->get(
            EXPORTING
              p_object_key  = lv_object_key
              p_version     = swbm_version_inactive
            CHANGING
              p_object_data = li_data_model ).
          IF li_data_model->get_object_name( ) IS INITIAL.
            "Fallback: retrieve active version
            li_persistence->get(
              EXPORTING
                p_object_key  = lv_object_key
                p_version     = swbm_version_active
              CHANGING
                p_object_data = li_data_model ).
          ENDIF.
        ELSE.
          "Retrieve active version
          li_persistence->get(
            EXPORTING
              p_object_key  = lv_object_key
              p_version     = swbm_version_active
            CHANGING
              p_object_data = li_data_model ).
        ENDIF.

        li_data_model->get_data( IMPORTING p_data = <lg_data> ).

        clear_fields( CHANGING cg_data = <lg_data> ).

        ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <lg_data> TO <lg_field>.
        ASSERT sy-subrc = 0.

        Lif_abapgit_object~mo_files->add_string(
          iv_ext    = 'asddlxs'
          iv_string = <lg_field> ).

        CLEAR <lg_field>.

        io_xml->add( iv_name = 'DDLX'
                     ig_data = <lg_data> ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_DDLX implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_EVTB <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_evtb=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_evtb=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_EVTB implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lv_user  TYPE string,
          lx_error TYPE REF TO cx_root.

    TRY.

        SELECT SINGLE changed_by INTO lv_user
            FROM (c_table_name)
            WHERE evtb_name = ms_item-obj_name AND version = 'I'.

        IF lv_user IS INITIAL.
          SELECT SINGLE changed_by INTO lv_user
            FROM (c_table_name)
            WHERE evtb_name = ms_item-obj_name AND version = 'A'.
        ENDIF.

        rv_user = lv_user.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_EVTB implementation

*>>>>>>> ZCL_ABAPGIT_ABAP_LANGUAGE_VERS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_abap_language_versccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_abap_language_versccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_abap_language_versccau.
* Helper to toggle (cloud) enviroment
CLASS SHRITEFUH64VYIPN5I4UHL45BCKR4A DEFINITION.

  PUBLIC SECTION.
    INTERFACES Lif_abapgit_environment.

    DATA mv_is_cloud TYPE abap_bool.

    METHODS set_cloud
      IMPORTING
        iv_is_cloud TYPE abap_bool.

ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BCKR4A IMPLEMENTATION.

  METHOD set_cloud.
    mv_is_cloud = iv_is_cloud.
  ENDMETHOD.

  METHOD Lif_abapgit_environment~is_sap_cloud_platform.
    rv_result = mv_is_cloud.
  ENDMETHOD.

  METHOD Lif_abapgit_environment~compare_with_inactive.
  ENDMETHOD.
  METHOD Lif_abapgit_environment~get_basis_release.
  ENDMETHOD.
  METHOD Lif_abapgit_environment~get_system_language_filter.
  ENDMETHOD.
  METHOD Lif_abapgit_environment~is_merged.
  ENDMETHOD.
  METHOD Lif_abapgit_environment~is_repo_object_changes_allowed.
  ENDMETHOD.
  METHOD Lif_abapgit_environment~is_restart_required.
  ENDMETHOD.
  METHOD Lif_abapgit_environment~is_sap_object_allowed.
  ENDMETHOD.
  METHOD Lif_abapgit_environment~is_variant_maintenance.
  ENDMETHOD.
  METHOD Lif_abapgit_environment~init_parallel_processing.
  ENDMETHOD.

ENDCLASS.

* Helper to toggle experimental features
CLASS SHRITEFUH64VYIPN5I4UHL45BCMR4A DEFINITION.

  PUBLIC SECTION.
    INTERFACES Lif_abapgit_persist_settings.

    DATA mo_settings TYPE REF TO Lcl_abapgit_settings.

    METHODS constructor.

ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BCMR4A IMPLEMENTATION.

  METHOD constructor.
    CREATE OBJECT mo_settings.
  ENDMETHOD.

  METHOD Lif_abapgit_persist_settings~modify.
  ENDMETHOD.

  METHOD Lif_abapgit_persist_settings~read.
    ro_settings = mo_settings.
  ENDMETHOD.

ENDCLASS.

* Test cases


class LCL_ABAPGIT_ABAP_LANGUAGE_VERS implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    mo_dot_abapgit = io_dot_abapgit.

    IF Lcl_abapgit_feature=>is_enabled( c_feature_flag ) = abap_false.
      mv_has_abap_language_vers = abap_false.
    ELSEIF get_abap_language_vers_by_repo( ) = Lif_abapgit_dot_abapgit=>c_abap_language_version-undefined.
      mv_has_abap_language_vers = abap_false.
    ELSE.
      mv_has_abap_language_vers = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD get_abap_language_vers_by_devc.

    DATA lv_class TYPE string.
    DATA lv_abap_lang_version_devc TYPE string.
    DATA lo_abap_language_version_cfg TYPE REF TO object.

    lv_class = 'CL_ABAP_LANGUAGE_VERSION_CFG'.

    TRY.

        CALL METHOD (lv_class)=>('GET_INSTANCE')
          RECEIVING
            ro_instance = lo_abap_language_version_cfg.

        " For non-existing packages, GET_PACKAGE_DEFAULT_VERSION returns "standard"
        " but we want to return "undefined" in this case to allow any new packages
        IF Lcl_abapgit_factory=>get_sap_package( iv_package )->exists( ) = abap_true.
          CALL METHOD lo_abap_language_version_cfg->('IF_ABAP_LANGUAGE_VERSION_CFG~GET_PACKAGE_DEFAULT_VERSION')
            EXPORTING
              iv_package_name             = iv_package
            RECEIVING
              rv_default_language_version = lv_abap_lang_version_devc.
        ELSE.
          lv_abap_lang_version_devc = '-'.
        ENDIF.

        CASE lv_abap_lang_version_devc.
          WHEN Lif_abapgit_aff_types_v1=>co_abap_language_version-standard.
            rv_abap_language_version = Lif_abapgit_dot_abapgit=>c_abap_language_version-standard.
          WHEN Lif_abapgit_aff_types_v1=>co_abap_language_version-key_user.
            rv_abap_language_version = Lif_abapgit_dot_abapgit=>c_abap_language_version-key_user.
          WHEN Lif_abapgit_aff_types_v1=>co_abap_language_version-cloud_development.
            rv_abap_language_version = Lif_abapgit_dot_abapgit=>c_abap_language_version-cloud_development.
          WHEN OTHERS.
            rv_abap_language_version = Lif_abapgit_dot_abapgit=>c_abap_language_version-undefined.
        ENDCASE.
      CATCH cx_root.
        rv_abap_language_version = Lif_abapgit_dot_abapgit=>c_abap_language_version-undefined.
    ENDTRY.

  ENDMETHOD.
  METHOD get_abap_language_vers_by_objt.

    DATA lv_class TYPE string.
    DATA lo_abap_language_version TYPE REF TO object.

    IF mv_has_abap_language_vers = abap_false.
      rv_allowed_abap_langu_version = c_any_abap_language_version.
      RETURN. ">>>
    ENDIF.

    lv_class = 'CL_ABAP_LANGUAGE_VERSION'.

    TRY.

        CALL METHOD (lv_class)=>('GET_INSTANCE')
          RECEIVING
            ro_version_handler = lo_abap_language_version.

        CALL METHOD lo_abap_language_version->('IF_ABAP_LANGUAGE_VERSION~GET_DEFAULT_VERSION')
          EXPORTING
            iv_object_type     = iv_object_type
            iv_package         = iv_package
          RECEIVING
            rv_default_version = rv_allowed_abap_langu_version.

      CATCH cx_root.
        rv_allowed_abap_langu_version = get_default_abap_language_vers( iv_object_type ).
    ENDTRY.

  ENDMETHOD.
  METHOD get_abap_language_vers_by_repo.
    rv_abap_language_version = mo_dot_abapgit->get_abap_language_version( ).
    IF rv_abap_language_version IS INITIAL.
      rv_abap_language_version = Lif_abapgit_dot_abapgit=>c_abap_language_version-undefined.
    ENDIF.
  ENDMETHOD.
  METHOD get_default_abap_language_vers.

    IF Lcl_abapgit_factory=>get_environment( )->is_sap_cloud_platform( ) = abap_true.
      " On BTP, default to ABAP for Cloud Development
      rv_abap_language_version = Lif_abapgit_aff_types_v1=>co_abap_language_version_cloud-cloud_development.
    ELSE.
      " Differentiate between source code object and non-source code objects
      CASE iv_object_type.
        WHEN 'BDEF' OR 'CLAS' OR 'FUGR' OR 'FUGS' OR 'INTF' OR 'PROG' OR 'TYPE'.
          rv_abap_language_version = Lif_abapgit_aff_types_v1=>co_abap_language_version_src-standard.
        WHEN OTHERS.
          rv_abap_language_version = Lif_abapgit_aff_types_v1=>co_abap_language_version-standard.
      ENDCASE.
    ENDIF.

  ENDMETHOD.
  METHOD get_repo_abap_language_version.

    DATA lv_abap_language_version TYPE string.

    IF mv_has_abap_language_vers = abap_true.
      lv_abap_language_version = mo_dot_abapgit->get_abap_language_version( ).
    ENDIF.

    CASE lv_abap_language_version.
      WHEN Lif_abapgit_dot_abapgit=>c_abap_language_version-standard.
        rv_abap_language_version = Lif_abapgit_aff_types_v1=>co_abap_language_version_src-standard.
      WHEN Lif_abapgit_dot_abapgit=>c_abap_language_version-key_user.
        rv_abap_language_version = Lif_abapgit_aff_types_v1=>co_abap_language_version_src-key_user.
      WHEN Lif_abapgit_dot_abapgit=>c_abap_language_version-cloud_development.
        rv_abap_language_version = Lif_abapgit_aff_types_v1=>co_abap_language_version_src-cloud_development.
      WHEN OTHERS. " undefined or feature off
        rv_abap_language_version = c_any_abap_language_version.
    ENDCASE.

  ENDMETHOD.
  METHOD is_import_allowed.

    DATA lv_package_version TYPE string.

    lv_package_version = get_abap_language_vers_by_devc( iv_package ).

    CASE get_abap_language_vers_by_repo( ).
      WHEN Lif_abapgit_dot_abapgit=>c_abap_language_version-undefined.
        rv_allowed = abap_true.
      WHEN OTHERS.
        IF get_abap_language_vers_by_repo( ) = lv_package_version.
          " allow packages that match repo setting
          rv_allowed = abap_true.
        ELSEIF lv_package_version = Lif_abapgit_dot_abapgit=>c_abap_language_version-undefined.
          " always allow new packages
          rv_allowed = abap_true.
        ELSE.
          rv_allowed = abap_false.
        ENDIF.
    ENDCASE.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ABAP_LANGUAGE_VERS implementation

*>>>>>>> ZCL_ABAPGIT_AFF_REGISTRY <<<<<<<*

*"* macro definitions
*include zcl_abapgit_aff_registry======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_aff_registry======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_aff_registry======ccau.
"! @testing zcl_abapgit_filename_logic



class LCL_ABAPGIT_AFF_REGISTRY implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    mv_aff_enabled = Lcl_abapgit_feature=>is_enabled( c_aff_feature ).
  ENDMETHOD.
  METHOD initialize_registry_table.
    register( 'CHKC' ).
    register( 'CHKO' ).
    register( 'CHKV' ).
    register( 'EVTB' ).
    register( 'EEEC' ).
    register( 'GSMP' ).
    register( iv_obj_type     = 'INTF'
              iv_experimental = abap_true ).
    register( 'SMBC' ).
    register( 'NONT' ).
    register( 'RONT' ).
  ENDMETHOD.
  METHOD register.
    DATA ls_registry_entry TYPE ty_registry_entry.

    ls_registry_entry-obj_type = iv_obj_type.
    ls_registry_entry-experimental = iv_experimental.
    INSERT ls_registry_entry INTO TABLE gt_registry.
  ENDMETHOD.
  METHOD Lif_abapgit_aff_registry~is_supported_object_type.

    DATA ls_registry_entry TYPE ty_registry_entry.

    IF gt_registry IS INITIAL.
      initialize_registry_table( ).
    ENDIF.

    READ TABLE gt_registry WITH TABLE KEY obj_type = iv_obj_type INTO ls_registry_entry.
    IF sy-subrc = 0 AND ls_registry_entry-experimental = abap_false.
      rv_result = abap_true.
    ELSEIF sy-subrc = 0 AND mv_aff_enabled = abap_true.
      rv_result = abap_true.
    ELSE.
      rv_result = abap_false.
    ENDIF.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_AFF_REGISTRY implementation

*>>>>>>> ZCL_ABAPGIT_APACK_HELPER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_apack_helper======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_apack_helper======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_APACK_HELPER implementation.
*"* method's implementations
*include methods.
  METHOD are_dependencies_met.

    DATA: lt_dependencies_status TYPE ty_dependency_statuses.

    IF it_dependencies IS INITIAL.
      rv_status = Lif_abapgit_definitions=>c_yes.
      RETURN.
    ENDIF.

    lt_dependencies_status = get_dependencies_met_status( it_dependencies ).

    LOOP AT lt_dependencies_status TRANSPORTING NO FIELDS WHERE met <> Lif_abapgit_definitions=>c_yes.
      EXIT.
    ENDLOOP.

    IF sy-subrc = 0.
      rv_status = Lif_abapgit_definitions=>c_no.
    ELSE.
      rv_status = Lif_abapgit_definitions=>c_yes.
    ENDIF.

  ENDMETHOD.
  METHOD dependencies_popup.

    DATA: lt_met_status TYPE ty_dependency_statuses,
          lv_answer     TYPE c LENGTH 1.

    lt_met_status = get_dependencies_met_status( it_dependencies ).

    show_dependencies_popup( lt_met_status ).

    lv_answer = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
      iv_titlebar      = 'Warning'
      iv_text_question = 'The project has unmet dependencies. Do you want to continue?' ).

    IF lv_answer <> '1'.
      Lcx_abapgit_exception=>raise( 'Cancelling because of unmet dependencies.' ).
    ENDIF.

  ENDMETHOD.
  METHOD get_color_table.

    DATA:
      lo_functional_settings TYPE REF TO cl_salv_functional_settings,
      lo_hyperlinks          TYPE REF TO cl_salv_hyperlinks,
      lt_color_negative      TYPE lvc_t_scol,
      lt_color_normal        TYPE lvc_t_scol,
      lt_color_positive      TYPE lvc_t_scol,
      ls_color               TYPE lvc_s_scol,
      lv_handle              TYPE i,
      ls_hyperlink           TYPE salv_s_int4_column,
      lv_hyperlink           TYPE service_rl.

    FIELD-SYMBOLS:
      <ls_line>       TYPE ty_color_line,
      <ls_dependency> LIKE LINE OF it_dependencies.

    CLEAR: ls_color.
    ls_color-color-col = col_negative.
    APPEND ls_color TO lt_color_negative.

    CLEAR: ls_color.
    ls_color-color-col = col_normal.
    APPEND ls_color TO lt_color_normal.

    CLEAR: ls_color.
    ls_color-color-col = col_positive.
    APPEND ls_color TO lt_color_positive.

    lo_functional_settings = io_alv->get_functional_settings( ).
    lo_hyperlinks = lo_functional_settings->get_hyperlinks( ).

    CLEAR: lv_handle, ls_color.
    LOOP AT it_dependencies ASSIGNING <ls_dependency>.
      lv_handle = lv_handle + 1.

      APPEND INITIAL LINE TO ct_color_table ASSIGNING <ls_line>.
      MOVE-CORRESPONDING <ls_dependency> TO <ls_line>.

      CASE <ls_line>-met.
        WHEN Lif_abapgit_definitions=>c_yes.
          <ls_line>-color     = lt_color_positive.
          <ls_line>-exception = '3'.
        WHEN Lif_abapgit_definitions=>c_partial.
          <ls_line>-color     = lt_color_normal.
          <ls_line>-exception = '2'.
        WHEN Lif_abapgit_definitions=>c_no.
          <ls_line>-color     = lt_color_negative.
          <ls_line>-exception = '1'.
      ENDCASE.

      CLEAR: ls_hyperlink.
      ls_hyperlink-columnname = 'GIT_URL'.
      ls_hyperlink-value      = lv_handle.
      APPEND ls_hyperlink TO <ls_line>-t_hyperlink.

      lv_hyperlink = <ls_line>-git_url.
      lo_hyperlinks->add_hyperlink( handle    = lv_handle
                                    hyperlink = lv_hyperlink ).

    ENDLOOP.

  ENDMETHOD.
  METHOD get_dependencies_met_status.

    DATA: lt_installed_packages TYPE Lif_abapgit_apack_definitions=>ty_descriptors,
          ls_installed_package  TYPE Lif_abapgit_apack_definitions=>ty_descriptor,
          ls_dependecy          TYPE Lif_abapgit_apack_definitions=>ty_dependency,
          ls_dependecy_popup    TYPE ty_dependency_status.

    IF it_dependencies IS INITIAL.
      RETURN.
    ENDIF.

    lt_installed_packages = get_installed_packages( ).

    LOOP AT it_dependencies INTO ls_dependecy.
      CLEAR: ls_dependecy_popup.

      MOVE-CORRESPONDING ls_dependecy TO ls_dependecy_popup.

      READ TABLE lt_installed_packages INTO ls_installed_package
        WITH KEY group_id    = ls_dependecy-group_id
                 artifact_id = ls_dependecy-artifact_id.
      IF sy-subrc <> 0.
        ls_dependecy_popup-met = Lif_abapgit_definitions=>c_no.
      ELSE.
        TRY.
            Lcl_abapgit_version=>check_dependant_version( is_current   = ls_installed_package-sem_version
                                                          is_dependant = ls_dependecy-sem_version ).
            ls_dependecy_popup-met = Lif_abapgit_definitions=>c_yes.
          CATCH Lcx_abapgit_exception.
            ls_dependecy_popup-met = Lif_abapgit_definitions=>c_partial.
        ENDTRY.
      ENDIF.

      INSERT ls_dependecy_popup INTO TABLE rt_status.

    ENDLOOP.

  ENDMETHOD.
  METHOD get_installed_packages.

    DATA: lo_apack_reader            TYPE REF TO Lcl_abapgit_apack_reader,
          lt_manifest_implementation TYPE ty_manifest_declarations,
          ls_manifest_implementation TYPE ty_manifest_declaration,
          lo_manifest_provider       TYPE REF TO object,
          ls_descriptor              TYPE Lif_abapgit_apack_definitions=>ty_descriptor.

    SELECT seometarel~clsname tadir~devclass FROM seometarel "#EC CI_NOORDER
       INNER JOIN tadir ON seometarel~clsname = tadir~obj_name "#EC CI_BUFFJOIN
       INTO TABLE lt_manifest_implementation
       WHERE tadir~pgmid = 'R3TR'
         AND tadir~object = 'CLAS'
         AND seometarel~version = '1'
         AND ( seometarel~refclsname = Lif_abapgit_apack_definitions=>c_apack_interface_cust
            OR seometarel~refclsname = Lif_abapgit_apack_definitions=>c_apack_interface_sap )
      ORDER BY clsname devclass.

    LOOP AT lt_manifest_implementation INTO ls_manifest_implementation.
      CLEAR: lo_manifest_provider, lo_apack_reader.

      TRY.
          CREATE OBJECT lo_manifest_provider TYPE (ls_manifest_implementation-clsname).
        CATCH cx_sy_create_object_error.
          CLEAR: lo_manifest_provider.
      ENDTRY.

      IF lo_manifest_provider IS NOT BOUND.
        CONTINUE.
      ENDIF.

      lo_apack_reader = Lcl_abapgit_apack_reader=>create_instance( ls_manifest_implementation-devclass ).
      lo_apack_reader->copy_manifest_descriptor( lo_manifest_provider ).
      ls_descriptor = lo_apack_reader->get_manifest_descriptor( ).

      IF ls_descriptor IS NOT INITIAL.
        INSERT ls_descriptor INTO TABLE rt_packages.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD show_dependencies_popup.

    DATA: lo_alv          TYPE REF TO cl_salv_table,
          lo_column       TYPE REF TO cl_salv_column,
          lo_column_table TYPE REF TO cl_salv_column_table,
          lo_columns      TYPE REF TO cl_salv_columns_table,
          lt_columns      TYPE salv_t_column_ref,
          ls_column       LIKE LINE OF lt_columns,
          lt_color_table  TYPE ty_color_tab,
          ls_position     TYPE Lif_abapgit_popups=>ty_popup_position,
          lx_ex           TYPE REF TO cx_root.

    IF it_dependencies IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table  = lo_alv
                                CHANGING  t_table       = lt_color_table ).

        lo_columns = lo_alv->get_columns( ).
        lt_columns = lo_columns->get( ).
        LOOP AT lt_columns INTO ls_column WHERE columnname CP 'SEM_VERSION-*'.
          ls_column-r_column->set_technical( ).
        ENDLOOP.

        lo_column = lo_columns->get_column( 'MET' ).
        lo_column->set_technical( ).

        lo_column = lo_columns->get_column( 'GROUP_ID' ).
        lo_column->set_short_text( 'Org/ProjId' ).

        lo_columns->set_color_column( 'COLOR' ).
        lo_columns->set_exception_column( 'EXCEPTION' ).
        lo_columns->set_hyperlink_entry_column( 'T_HYPERLINK' ).
        lo_columns->set_optimize( ).

        lo_column = lo_columns->get_column( 'GROUP_ID' ).
        lo_column->set_short_text( 'Org/ProjId' ).

        lo_column = lo_columns->get_column( 'ARTIFACT_ID' ).
        lo_column->set_short_text( 'Proj. Name' ).

        lo_column = lo_columns->get_column( 'GIT_URL' ).
        lo_column->set_short_text( 'Git URL' ).

        lo_column_table ?= lo_column.
        lo_column_table->set_cell_type( if_salv_c_cell_type=>link ).

        lo_column = lo_columns->get_column( 'VERSION' ).
        lo_column->set_short_text( 'Version' ).

        lo_column = lo_columns->get_column( 'TARGET_PACKAGE' ).
        lo_column->set_technical( ).

        get_color_table(
          EXPORTING
            io_alv          = lo_alv
            it_dependencies = it_dependencies
          CHANGING
            ct_color_table  = lt_color_table ).

        ls_position = Lcl_abapgit_popups=>center(
          iv_width  = 90
          iv_height = 10 ).

        lo_alv->set_screen_popup( start_column = ls_position-start_column
                                  end_column   = ls_position-end_column
                                  start_line   = ls_position-start_row
                                  end_line     = ls_position-end_row ).

        lo_alv->get_display_settings( )->set_list_header( 'APACK dependencies' ).
        lo_alv->display( ).

      CATCH cx_salv_msg cx_salv_not_found cx_salv_data_error cx_salv_existing INTO lx_ex.
        Lcx_abapgit_exception=>raise( lx_ex->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
  METHOD to_file.

    DATA: lo_manifest_reader TYPE REF TO Lcl_abapgit_apack_reader,
          ls_descriptor      TYPE Lif_abapgit_apack_definitions=>ty_descriptor,
          lo_manifest_writer TYPE REF TO Lcl_abapgit_apack_writer.

    lo_manifest_reader = Lcl_abapgit_apack_reader=>create_instance( iv_package ).
    IF lo_manifest_reader->has_manifest( ) = abap_true.
      ls_descriptor = lo_manifest_reader->get_manifest_descriptor( ).
      lo_manifest_writer = Lcl_abapgit_apack_writer=>create_instance( ls_descriptor ).
      rs_file-path     = Lif_abapgit_definitions=>c_root_dir.
      rs_file-filename = Lif_abapgit_apack_definitions=>c_dot_apack_manifest.
      rs_file-data     = Lcl_abapgit_convert=>string_to_xstring_utf8( lo_manifest_writer->serialize( ) ).
      rs_file-sha1     = Lcl_abapgit_hash=>sha1_blob( rs_file-data ).
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_APACK_HELPER implementation

*>>>>>>> ZCL_ABAPGIT_APACK_READER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_apack_reader======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_apack_reader======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_apack_reader======ccau.
*"* use this source file for your ABAP unit test classes


class LCL_ABAPGIT_APACK_READER implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    mv_package_name = iv_package_name.
  ENDMETHOD.
  METHOD copy_manifest_descriptor.

    DATA: ls_my_manifest_wo_deps TYPE Lif_abapgit_apack_definitions=>ty_descriptor_wo_dependencies,
          ls_my_dependency       TYPE Lif_abapgit_apack_definitions=>ty_dependency,
          ls_descriptor          TYPE Lif_abapgit_apack_definitions=>ty_descriptor,
          lv_descriptor_cust     TYPE string,
          lv_descriptor_sap      TYPE string.

    FIELD-SYMBOLS: <lg_descriptor>   TYPE any,
                   <lt_dependencies> TYPE ANY TABLE,
                   <lg_dependency>   TYPE any.

    lv_descriptor_cust = Lif_abapgit_apack_definitions=>c_apack_interface_cust && '~DESCRIPTOR'.
    lv_descriptor_sap  = Lif_abapgit_apack_definitions=>c_apack_interface_sap && '~DESCRIPTOR'.

    ASSIGN io_manifest_provider->(lv_descriptor_cust) TO <lg_descriptor>.
    IF <lg_descriptor> IS NOT ASSIGNED.
      ASSIGN io_manifest_provider->(lv_descriptor_sap) TO <lg_descriptor>.
    ENDIF.
    IF <lg_descriptor> IS ASSIGNED.
      " A little more complex than a normal MOVE-CORRSPONDING
      " to avoid dumps in case of future updates to the dependencies table structure
      ASSIGN COMPONENT 'DEPENDENCIES' OF STRUCTURE <lg_descriptor> TO <lt_dependencies>.
      IF <lt_dependencies> IS ASSIGNED.
        LOOP AT <lt_dependencies> ASSIGNING <lg_dependency>.
          MOVE-CORRESPONDING <lg_dependency> TO ls_my_dependency.
          INSERT ls_my_dependency INTO TABLE ls_descriptor-dependencies.
        ENDLOOP.
        MOVE-CORRESPONDING <lg_descriptor> TO ls_my_manifest_wo_deps.
        MOVE-CORRESPONDING ls_my_manifest_wo_deps TO ls_descriptor.
      ENDIF.
    ENDIF.

    set_manifest_descriptor( ls_descriptor ).

  ENDMETHOD.
  METHOD create_instance.
    CREATE OBJECT ro_manifest_reader
      EXPORTING
        iv_package_name = iv_package_name.
  ENDMETHOD.
  METHOD deserialize.

    DATA: lv_xml  TYPE string,
          ls_data TYPE Lif_abapgit_apack_definitions=>ty_descriptor.

    lv_xml = Lcl_abapgit_convert=>xstring_to_string_utf8( iv_xstr ).

    ls_data = from_xml( lv_xml ).

    ro_manifest_reader = create_instance( iv_package_name ).
    ro_manifest_reader->set_manifest_descriptor( ls_data ).

  ENDMETHOD.
  METHOD format_version.

    FIELD-SYMBOLS: <ls_dependency> TYPE Lif_abapgit_apack_definitions=>ty_dependency.

    TRANSLATE ms_cached_descriptor-version TO LOWER CASE.
    ms_cached_descriptor-sem_version = Lcl_abapgit_version=>conv_str_to_version( ms_cached_descriptor-version ).

    LOOP AT ms_cached_descriptor-dependencies ASSIGNING <ls_dependency>.
      <ls_dependency>-sem_version = Lcl_abapgit_version=>conv_str_to_version( <ls_dependency>-version ).
    ENDLOOP.

  ENDMETHOD.
  METHOD from_xml.

    DATA: lv_xml TYPE string.

    lv_xml = iv_xml.

    CALL TRANSFORMATION id
      OPTIONS value_handling = 'accept_data_loss'
      SOURCE XML lv_xml
      RESULT data = rs_data.

  ENDMETHOD.
  METHOD get_manifest_descriptor.

    DATA: lo_manifest_provider       TYPE REF TO object,
          lv_package                 TYPE devclass,
          lt_packages                TYPE Lif_abapgit_sap_package=>ty_devclass_tt,
          ls_manifest_implementation TYPE ty_s_manifest_declaration,
          lt_manifest_implementation TYPE STANDARD TABLE OF ty_s_manifest_declaration WITH DEFAULT KEY.
    DATA lt_refclsname TYPE RANGE OF abap_classname.
    DATA ls_refclsname LIKE LINE OF lt_refclsname.


    IF mv_package_name IS NOT INITIAL.
      lt_packages = Lcl_abapgit_factory=>get_sap_package( mv_package_name )->list_subpackages( ).
      INSERT mv_package_name INTO TABLE lt_packages.
    ENDIF.

    IF mv_is_cached IS INITIAL AND lt_packages IS NOT INITIAL.
      ls_refclsname-sign = 'I'.
      ls_refclsname-option = 'EQ'.
      ls_refclsname-low = Lif_abapgit_apack_definitions=>c_apack_interface_cust.
      INSERT ls_refclsname INTO TABLE lt_refclsname.

      ls_refclsname-sign = 'I'.
      ls_refclsname-option = 'EQ'.
      ls_refclsname-low = Lif_abapgit_apack_definitions=>c_apack_interface_sap.
      INSERT ls_refclsname INTO TABLE lt_refclsname.

      IF mv_package_name CA '/'.
        ls_refclsname-sign = 'I'.
        ls_refclsname-option = 'CP'.
        ls_refclsname-low = Lif_abapgit_apack_definitions=>c_apack_interface_nspc.
        INSERT ls_refclsname INTO TABLE lt_refclsname.
      ENDIF.

      " Find all classes that implement customer or SAP version of APACK interface
      SELECT seometarel~clsname tadir~devclass FROM seometarel "#EC CI_NOORDER
         INNER JOIN tadir ON seometarel~clsname = tadir~obj_name "#EC CI_BUFFJOIN
         INTO TABLE lt_manifest_implementation
         WHERE tadir~pgmid = 'R3TR' AND
               tadir~object = 'CLAS' AND
               seometarel~version = '1' AND
               seometarel~refclsname IN lt_refclsname
         ORDER BY clsname devclass.

      LOOP AT lt_packages INTO lv_package.
        READ TABLE lt_manifest_implementation INTO ls_manifest_implementation WITH KEY devclass = lv_package.
        IF sy-subrc = 0.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF ls_manifest_implementation IS NOT INITIAL.
        TRY.
            CREATE OBJECT lo_manifest_provider TYPE (ls_manifest_implementation-clsname).
          CATCH cx_sy_create_object_error.
            CLEAR: rs_manifest_descriptor.
        ENDTRY.
        IF lo_manifest_provider IS BOUND.
          copy_manifest_descriptor( lo_manifest_provider ).
        ENDIF.
      ENDIF.

      mv_is_cached = abap_true.

    ENDIF.

    rs_manifest_descriptor = ms_cached_descriptor.
  ENDMETHOD.
  METHOD has_manifest.

    DATA: ls_returned_manifest TYPE Lif_abapgit_apack_definitions=>ty_descriptor.

    ls_returned_manifest = get_manifest_descriptor( ).

    rv_has_manifest = abap_false.
    IF ls_returned_manifest IS NOT INITIAL.
      rv_has_manifest = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD set_manifest_descriptor.
    mv_is_cached = abap_true.
    ms_cached_descriptor = is_manifest_descriptor.
    format_version( ).
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_APACK_READER implementation

*>>>>>>> ZCL_ABAPGIT_CODE_INSPECTOR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_code_inspector====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_code_inspector====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_CODE_INSPECTOR implementation.
*"* method's implementations
*include methods.
  METHOD cleanup.

    IF mo_inspection IS BOUND.

      mo_inspection->delete(
        EXCEPTIONS
          locked              = 1
          error_in_enqueue    = 2
          not_authorized      = 3
          exceptn_appl_exists = 4
          OTHERS              = 5 ).

      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Couldn't delete inspection. Subrc = { sy-subrc }| ).
      ENDIF.

    ENDIF.

    io_set->delete(
      EXCEPTIONS
        exists_in_insp   = 1
        locked           = 2
        error_in_enqueue = 3
        not_authorized   = 4
        exists_in_objs   = 5
        OTHERS           = 6 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Couldn't delete objectset. Subrc = { sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    IF iv_package IS INITIAL.
      Lcx_abapgit_exception=>raise( |Please supply package| ).
    ENDIF.

    mv_package = iv_package.

    " We create the inspection and objectset with dummy names.
    " Because we want to persist them so we can run it in parallel.
    " Both are deleted afterwards.
    mv_name = |{ sy-uname }_{ sy-datum }_{ sy-uzeit }|.
    mv_run_mode = decide_run_mode( ).

  ENDMETHOD.
  METHOD create_inspection.

    cl_ci_inspection=>create(
      EXPORTING
        p_user           = sy-uname
        p_name           = mv_name
      RECEIVING
        p_ref            = ro_inspection
      EXCEPTIONS
        locked           = 1
        error_in_enqueue = 2
        not_authorized   = 3
        OTHERS           = 4 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Failed to create inspection. Subrc = { sy-subrc }| ).
    ENDIF.

    ro_inspection->set(
      p_chkv = io_variant
      p_objs = io_set ).

    ro_inspection->save(
      EXCEPTIONS
        missing_information = 1
        insp_no_name        = 2
        not_enqueued        = 3
        OTHERS              = 4 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Failed to save inspection. Subrc = { sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD create_objectset.

    DATA: lt_objs       TYPE scit_objs,
          ls_obj        TYPE scir_objs,
          lt_objs_check TYPE scit_objs,
          ls_item       TYPE Lif_abapgit_definitions=>ty_item,
          lt_packages   TYPE Lif_abapgit_sap_package=>ty_devclass_tt.

    lt_packages = Lcl_abapgit_factory=>get_sap_package( mv_package )->list_subpackages( ).
    INSERT mv_package INTO TABLE lt_packages.

    SELECT object AS objtype obj_name AS objname
      FROM tadir
      INTO CORRESPONDING FIELDS OF TABLE lt_objs
      FOR ALL ENTRIES IN lt_packages
      WHERE devclass = lt_packages-table_line
      AND delflag = abap_false
      AND pgmid = 'R3TR' ##TOO_MANY_ITAB_FIELDS.        "#EC CI_GENBUFF

    LOOP AT lt_objs INTO ls_obj.

      IF skip_object( ls_obj ) = abap_true.
        CONTINUE.
      ENDIF.

      ls_item-obj_type = ls_obj-objtype.
      ls_item-obj_name = ls_obj-objname.

      IF Lcl_abapgit_objects=>exists( ls_item ) = abap_false.
        CONTINUE.
      ENDIF.

      INSERT ls_obj INTO TABLE lt_objs_check.

    ENDLOOP.

    ro_set = cl_ci_objectset=>save_from_list(
      p_name    = mv_name
      p_objects = lt_objs_check ).

  ENDMETHOD.
  METHOD create_variant.

    IF iv_variant IS INITIAL.
      Lcx_abapgit_exception=>raise( |No check variant supplied.| ).
    ENDIF.

    cl_ci_checkvariant=>get_ref(
      EXPORTING
        p_user                   = ''
        p_name                   = iv_variant
      RECEIVING
        p_ref                    = ro_variant
      EXCEPTIONS
        chkv_not_exists          = 1
        missing_parameter        = 2
        OTHERS                   = 3 ).

    CASE sy-subrc.
      WHEN 1.
        Lcx_abapgit_exception=>raise( |Check variant { iv_variant } doesn't exist| ).
      WHEN 2.
        Lcx_abapgit_exception=>raise( |Parameter missing for check variant { iv_variant }| ).
    ENDCASE.

  ENDMETHOD.
  METHOD decide_run_mode.

    DATA lo_settings TYPE REF TO Lcl_abapgit_settings.
    lo_settings = Lcl_abapgit_persist_factory=>get_settings( )->read( ).

    IF sy-batch = abap_true.
      " We have to disable parallelization in batch because of lock errors.
      rv_run_mode = co_run_mode-run_via_rfc.
    ELSEIF lo_settings->get_parallel_proc_disabled( ) = abap_false.
      rv_run_mode = co_run_mode-run_loc_parallel.
    ELSE.
      rv_run_mode = co_run_mode-run_via_rfc.
    ENDIF.

  ENDMETHOD.
  METHOD filter_inspection.

    " Remove findings in LSVIM* includes which are part of generated maintenance screens
    DELETE ct_list WHERE sobjtype = 'PROG' AND sobjname CP 'LSVIM*'.

  ENDMETHOD.
  METHOD run_inspection.

    io_inspection->run(
      EXPORTING
        p_howtorun            = mv_run_mode
      EXCEPTIONS
        invalid_check_version = 1
        OTHERS                = 2 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Code inspector run failed. Subrc = { sy-subrc }| ).
    ENDIF.

    io_inspection->plain_list( IMPORTING p_list = rt_list ).

    filter_inspection( CHANGING ct_list = rt_list ).

    SORT rt_list BY objtype objname test code sobjtype sobjname line col.

    DELETE ADJACENT DUPLICATES FROM rt_list.

  ENDMETHOD.
  METHOD skip_object.

    DATA ls_program_type TYPE subc.

    CASE is_obj-objtype.
      WHEN 'PROG'.

        SELECT SINGLE subc
          INTO ls_program_type
          FROM trdir
          WHERE name = is_obj-objname.

        rv_skip = boolc( ls_program_type = 'I' ). " Include program.

      WHEN OTHERS.
        rv_skip = abap_false.

    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_code_inspector~get_summary.
    rv_summary = mv_summary.
  ENDMETHOD.
  METHOD Lif_abapgit_code_inspector~is_successful.

    rv_success = mv_success.

  ENDMETHOD.
  METHOD Lif_abapgit_code_inspector~run.

    DATA: lo_set     TYPE REF TO cl_ci_objectset,
          lo_variant TYPE REF TO cl_ci_checkvariant,
          lv_count   TYPE i,
          lt_list    TYPE scit_alvlist,
          ls_list    LIKE LINE OF lt_list,
          ls_result  LIKE LINE OF rt_list,
          lo_timer   TYPE REF TO Lcl_abapgit_timer,
          lx_error   TYPE REF TO Lcx_abapgit_exception.

    TRY.
        lo_set = create_objectset( ).

        lv_count = lines( lo_set->iobjlst-objects ).
        IF lv_count = 0.
          " no objects, nothing to check
          RETURN.
        ENDIF.

        lo_timer = Lcl_abapgit_timer=>create( iv_count = lv_count )->start( ).

        lo_variant = create_variant( iv_variant ).

        mo_inspection = create_inspection(
          io_set     = lo_set
          io_variant = lo_variant ).

        lt_list = run_inspection( mo_inspection ).

        cleanup( lo_set ).

        LOOP AT lt_list INTO ls_list.
          MOVE-CORRESPONDING ls_list TO ls_result.
          INSERT ls_result INTO TABLE rt_list.
        ENDLOOP.

        IF iv_save = abap_true.
          READ TABLE rt_list TRANSPORTING NO FIELDS WITH KEY kind = 'E'.
          mv_success = boolc( sy-subrc <> 0 ).
        ENDIF.

      CATCH Lcx_abapgit_exception INTO lx_error.

        " ensure cleanup
        cleanup( lo_set ).
        Lcx_abapgit_exception=>raise_with_text( lx_error ).

    ENDTRY.

    mv_summary = lo_timer->end( ).

  ENDMETHOD.
  METHOD Lif_abapgit_code_inspector~list_global_variants.

    SELECT scichkv_hd~checkvname AS name
      scichkv_tx~text AS description
      INTO TABLE rt_list
      FROM scichkv_hd
      LEFT OUTER JOIN scichkv_tx
      ON scichkv_hd~checkvid = scichkv_tx~checkvid
      AND scichkv_hd~ciuser  = scichkv_tx~ciuser
      AND scichkv_tx~language = sy-langu
      WHERE scichkv_hd~ciuser = space
      ORDER BY name.

  ENDMETHOD.
  METHOD Lif_abapgit_code_inspector~validate_check_variant.

    cl_ci_checkvariant=>get_ref(
      EXPORTING
        p_user                   = ''
        p_name                   = iv_check_variant_name
      EXCEPTIONS
        chkv_not_exists          = 1
        missing_parameter        = 2
        OTHERS                   = 3 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |No valid check variant { iv_check_variant_name  }| ).
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_CODE_INSPECTOR implementation

*>>>>>>> ZCL_ABAPGIT_CTS_API <<<<<<<*

*"* macro definitions
*include zcl_abapgit_cts_api===========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_cts_api===========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_CTS_API implementation.
*"* method's implementations
*include methods.
  METHOD get_current_transport_for_obj.
    DATA: lv_object_lockable   TYPE abap_bool,
          lv_locked            TYPE abap_bool,
          lv_transport_request TYPE trkorr,
          lv_task              TYPE trkorr,
          lv_tr_object_name    TYPE trobj_name.

    lv_tr_object_name = iv_object_name.

    CALL FUNCTION 'TR_CHECK_OBJECT_LOCK'
      EXPORTING
        wi_pgmid             = iv_program_id
        wi_object            = iv_object_type
        wi_objname           = lv_tr_object_name
      IMPORTING
        we_lockable_object   = lv_object_lockable
        we_locked            = lv_locked
        we_lock_order        = lv_transport_request
        we_lock_task         = lv_task
      EXCEPTIONS
        empty_key            = 1
        no_systemname        = 2
        no_systemtype        = 3
        unallowed_lock_order = 4
        OTHERS               = 5.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF lv_locked = abap_false.
      Lcx_abapgit_exception=>raise( |Object { iv_program_id }-{ iv_object_type }-{ iv_object_name } is not locked| ).
    ENDIF.

    IF lv_object_lockable = abap_false.
      Lcx_abapgit_exception=>raise( |Object type { iv_program_id }-{ iv_object_type } not lockable| ).
    ENDIF.

    rv_transport = lv_transport_request.

  ENDMETHOD.
  METHOD get_current_transport_from_db.

    " This method is used for objects that are included in transports but not locked
    " for example, namespaces (NSPC)
    SELECT SINGLE a~trkorr FROM e070 AS a JOIN e071 AS b ON a~trkorr = b~trkorr
      INTO rv_transport
      WHERE ( a~trstatus = 'D' OR a~trstatus = 'L' )
        AND a~trfunction <> 'G'
        AND b~pgmid = iv_program_id AND b~object = iv_object_type AND b~obj_name = iv_object_name.

  ENDMETHOD.
  METHOD is_object_locked_in_transport.
    DATA: ls_object_key        TYPE e071,
          lv_type_check_result TYPE c LENGTH 1,
          ls_lock_key          TYPE tlock_int,
          lv_lock_flag         TYPE c LENGTH 1.

    ls_object_key-pgmid = iv_program_id.
    ls_object_key-object = iv_object_type.
    ls_object_key-obj_name = iv_object_name.

    CALL FUNCTION 'TR_CHECK_TYPE'
      EXPORTING
        wi_e071     = ls_object_key
      IMPORTING
        pe_result   = lv_type_check_result
        we_lock_key = ls_lock_key.

    IF lv_type_check_result <> 'L'.
      Lcx_abapgit_exception=>raise( |Object type { iv_program_id }-{ iv_object_type } not lockable| ).
    ENDIF.

    CALL FUNCTION 'TRINT_CHECK_LOCKS'
      EXPORTING
        wi_lock_key = ls_lock_key
      IMPORTING
        we_lockflag = lv_lock_flag
      EXCEPTIONS
        empty_key   = 1
        OTHERS      = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    rv_locked = boolc( lv_lock_flag <> space ).
  ENDMETHOD.
  METHOD is_object_type_lockable.
    DATA: ls_object_key        TYPE e071,
          lv_type_check_result TYPE c LENGTH 1.

    ls_object_key-pgmid = iv_program_id.
    ls_object_key-object = iv_object_type.
    ls_object_key-obj_name = '_'. " Dummy value #2071

    CALL FUNCTION 'TR_CHECK_TYPE'
      EXPORTING
        wi_e071   = ls_object_key
      IMPORTING
        pe_result = lv_type_check_result.

    rv_lockable = boolc( lv_type_check_result = 'L' ).
  ENDMETHOD.
  METHOD is_object_type_transportable.
    DATA: ls_object_key        TYPE e071,
          lv_type_check_result TYPE c LENGTH 1.

    ls_object_key-pgmid = iv_program_id.
    ls_object_key-object = iv_object_type.
    ls_object_key-obj_name = '_'. " Dummy value #2071

    CALL FUNCTION 'TR_CHECK_TYPE'
      EXPORTING
        wi_e071   = ls_object_key
      IMPORTING
        pe_result = lv_type_check_result.

    rv_transportable = boolc( lv_type_check_result CA 'RTL' ).
  ENDMETHOD.
  METHOD Lif_abapgit_cts_api~get_r3tr_obj_for_limu_obj.

    CLEAR ev_object.
    CLEAR ev_obj_name.

    IF iv_object = 'MESS'.
      ev_object = 'MSAG'.
      ev_obj_name = substring( val = iv_obj_name
                               len = strlen( iv_obj_name ) - 3 ).
      RETURN.
    ENDIF.

    CALL FUNCTION 'GET_R3TR_OBJECT_FROM_LIMU_OBJ'
      EXPORTING
        p_limu_objtype = iv_object
        p_limu_objname = iv_obj_name
      IMPORTING
        p_r3tr_objtype = ev_object
        p_r3tr_objname = ev_obj_name
      EXCEPTIONS
        no_mapping     = 1
        OTHERS         = 2.
    IF sy-subrc <> 0 OR ev_obj_name IS INITIAL.
      Lcx_abapgit_exception=>raise( |No R3TR Object found for { iv_object } { iv_obj_name }| ).
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_cts_api~get_transports_for_list.

    DATA lv_request TYPE trkorr.
    DATA lt_tlock TYPE SORTED TABLE OF tlock WITH NON-UNIQUE KEY object hikey.
    DATA ls_object_key TYPE e071.
    DATA lv_type_check_result TYPE c LENGTH 1.
    DATA ls_lock_key TYPE tlock_int.
    DATA ls_transport LIKE LINE OF rt_transports.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF it_items.
    FIELD-SYMBOLS <ls_tlock> LIKE LINE OF lt_tlock.

* Workarounds to improve performance, note that IT_ITEMS might
* contain 1000s of rows, see standard logic in function module
* TR_CHECK_OBJECT_LOCK

* avoid database lookups in TLOCK for each item,
    SELECT * FROM tlock INTO TABLE lt_tlock.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT it_items ASSIGNING <ls_item>.
      CLEAR lv_request.

      ls_object_key-pgmid = 'R3TR'.
      ls_object_key-object = <ls_item>-obj_type.
      ls_object_key-obj_name = <ls_item>-obj_name.

      CALL FUNCTION 'TR_CHECK_TYPE'
        EXPORTING
          wi_e071     = ls_object_key
        IMPORTING
          we_lock_key = ls_lock_key
          pe_result   = lv_type_check_result.

      IF lv_type_check_result = 'L'.
        LOOP AT lt_tlock ASSIGNING <ls_tlock>
            WHERE object = ls_lock_key-obj
            AND hikey >= ls_lock_key-low
            AND lokey <= ls_lock_key-hi.                  "#EC PORTABLE
          lv_request = <ls_tlock>-trkorr.
          EXIT.
        ENDLOOP.
      ELSEIF is_object_type_transportable( <ls_item>-obj_type ) = abap_true.
        lv_request = get_current_transport_from_db(
          iv_object_type = <ls_item>-obj_type
          iv_object_name = <ls_item>-obj_name ).
      ENDIF.

      IF lv_request IS NOT INITIAL.
        ls_transport-obj_type = <ls_item>-obj_type.
        ls_transport-obj_name = <ls_item>-obj_name.
        ls_transport-trkorr = lv_request.
        INSERT ls_transport INTO TABLE rt_transports.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_cts_api~get_transport_for_object.

    IF is_item-obj_type IS NOT INITIAL AND is_item-obj_name IS NOT INITIAL.

      IF is_object_type_lockable( is_item-obj_type ) = abap_true AND
         is_object_locked_in_transport(
           iv_object_type = is_item-obj_type
           iv_object_name = is_item-obj_name ) = abap_true.

        rv_transport = get_current_transport_for_obj(
          iv_object_type = is_item-obj_type
          iv_object_name = is_item-obj_name ).

      ELSEIF is_object_type_transportable( is_item-obj_type ) = abap_true.

        rv_transport = get_current_transport_from_db(
          iv_object_type = is_item-obj_type
          iv_object_name = is_item-obj_name ).

      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_cts_api~is_chrec_possible_for_package.
    IF iv_package IS NOT INITIAL.
      rv_possible = Lcl_abapgit_factory=>get_sap_package( iv_package )->are_changes_recorded_in_tr_req( ).
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_cts_api~read_description.

    SELECT SINGLE as4text FROM e07t
      INTO rv_description
      WHERE trkorr = iv_trkorr
      AND langu = sy-langu ##SUBRC_OK.

  ENDMETHOD.
  METHOD Lif_abapgit_cts_api~read_user.

    SELECT SINGLE as4user FROM e070 INTO rv_uname
      WHERE trkorr = iv_trkorr ##SUBRC_OK.

  ENDMETHOD.
  METHOD Lif_abapgit_cts_api~confirm_transport_messages.

    TYPES: BEGIN OF ty_s_message,
             id TYPE symsgid,
             ty TYPE symsgty,
             no TYPE symsgno,
             v1 TYPE symsgv,
             v2 TYPE symsgv,
             v3 TYPE symsgv,
             v4 TYPE symsgv,
           END OF ty_s_message.

    DATA ls_message TYPE ty_s_message.

    FIELD-SYMBOLS: <lt_confirmed_messages> TYPE STANDARD TABLE.

    IF mv_confirm_transp_msgs_called = abap_true.
      RETURN.
    ENDIF.

    " remember the call to avoid duplicates in GT_CONFIRMED_MESSAGES
    mv_confirm_transp_msgs_called = abap_true.


    " Auto-confirm certain messages (requires SAP Note 1609940)
    PERFORM dummy IN PROGRAM saplstrd IF FOUND.  "load function group STRD once into memory

    ASSIGN ('(SAPLSTRD)GT_CONFIRMED_MESSAGES') TO <lt_confirmed_messages>.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Object can only be created in package of namespace
    ls_message-id = 'TR'.
    ls_message-no = '007'.
    INSERT ls_message INTO TABLE <lt_confirmed_messages>.

    " Original system set to "SAP"
    ls_message-id = 'TR'.
    ls_message-no = '013'.
    INSERT ls_message INTO TABLE <lt_confirmed_messages>.

    " Make repairs in foreign namespaces only if they are urgent
    ls_message-id = 'TR'.
    ls_message-no = '852'.
    INSERT ls_message INTO TABLE <lt_confirmed_messages>.

    " Make repairs in foreign namespaces only if they are urgent
    ls_message-id = 'TK'.
    ls_message-no = '016'.
    INSERT ls_message INTO TABLE <lt_confirmed_messages>.

    rv_messages_confirmed = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_cts_api~create_transport_entries.

    DATA lt_tables      TYPE tredt_objects.
    DATA lt_table_keys  TYPE STANDARD TABLE OF e071k.
    DATA lv_with_dialog TYPE abap_bool.

    cl_table_utilities_brf=>create_transport_entries(
      EXPORTING
        it_table_ins = it_table_ins
        it_table_upd = it_table_upd
        it_table_del = it_table_del
        iv_tabname   = iv_tabname
      CHANGING
        ct_e071      = lt_tables
        ct_e071k     = lt_table_keys ).

    " cl_table_utilities_brf=>write_transport_entries does not allow passing a request

    CALL FUNCTION 'TR_OBJECTS_CHECK'
      TABLES
        wt_ko200                = lt_tables
      EXCEPTIONS
        cancel_edit_other_error = 1
        show_only_other_error   = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF iv_transport IS INITIAL.
      lv_with_dialog = abap_true.
    ENDIF.

    CALL FUNCTION 'TRINT_OBJECTS_CHECK_AND_INSERT'
      EXPORTING
        iv_order       = iv_transport
        iv_with_dialog = lv_with_dialog
      CHANGING
        ct_ko200       = lt_tables
        ct_e071k       = lt_table_keys
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_cts_api~insert_transport_object.

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = iv_obj_name
        object_class        = iv_object
        devclass            = iv_package
        master_language     = iv_language
        mode                = iv_mode
        global_lock         = abap_true
        suppress_dialog     = abap_true
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_cts_api~list_open_requests_by_user.

    TYPES: BEGIN OF ty_e070,
             trkorr     TYPE e070-trkorr,
             trfunction TYPE e070-trfunction,
             strkorr    TYPE e070-strkorr,
           END OF ty_e070.
    DATA lt_e070 TYPE STANDARD TABLE OF ty_e070 WITH DEFAULT KEY.

* find all tasks first
    SELECT trkorr trfunction strkorr
      FROM e070 INTO TABLE lt_e070
      WHERE as4user = sy-uname
      AND trstatus = Lif_abapgit_cts_api=>c_transport_status-modifiable
      AND strkorr <> ''
      ORDER BY PRIMARY KEY.

    IF lines( lt_e070 ) > 0.
      SELECT trkorr FROM e070
        INTO TABLE rt_trkorr
        FOR ALL ENTRIES IN lt_e070
        WHERE trkorr = lt_e070-strkorr
        AND trfunction = Lif_abapgit_cts_api=>c_transport_type-wb_request.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_cts_api~read.

    DATA ls_request TYPE trwbo_request.
    DATA ls_key     LIKE LINE OF ls_request-keys.

    FIELD-SYMBOLS <ls_key> LIKE LINE OF rs_request-keys.


    ls_request-h-trkorr = iv_trkorr.

    CALL FUNCTION 'TRINT_READ_REQUEST'
      EXPORTING
        iv_read_e070       = abap_true
        iv_read_e07t       = abap_true
        iv_read_e070c      = abap_true
        iv_read_e070m      = abap_true
        iv_read_objs_keys  = abap_true
        iv_read_objs       = abap_true
        iv_read_attributes = abap_true
      CHANGING
        cs_request         = ls_request
      EXCEPTIONS
        error_occured      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

* move to output structure
    rs_request-trstatus = ls_request-h-trstatus.
    LOOP AT ls_request-keys INTO ls_key.
      APPEND INITIAL LINE TO rs_request-keys ASSIGNING <ls_key>.
      MOVE-CORRESPONDING ls_key TO <ls_key>.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_cts_api~validate_transport_request.

    CONSTANTS:
      BEGIN OF c_tr_status,
        modifiable           TYPE trstatus VALUE 'D',
        modifiable_protected TYPE trstatus VALUE 'L',
      END OF c_tr_status.

    DATA ls_request TYPE Lif_abapgit_cts_api=>ty_transport_data.

    ls_request = Lif_abapgit_cts_api~read( iv_transport_request ).

    IF ls_request-trstatus <> c_tr_status-modifiable
        AND ls_request-trstatus <> c_tr_status-modifiable_protected.
      " Task/request &1 has already been released
      MESSAGE e064(tk) WITH iv_transport_request INTO Lcx_abapgit_exception=>null.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_cts_api~list_r3tr_by_request.

    TYPES: BEGIN OF ty_contents,
             trkorr   TYPE e071-trkorr,
             as4pos   TYPE e071-as4pos,
             pgmid    TYPE e071-pgmid,
             object   TYPE e071-object,
             obj_name TYPE e071-obj_name,
           END OF ty_contents.

    DATA lt_tasks    TYPE STANDARD TABLE OF trkorr WITH DEFAULT KEY.
    DATA lt_contents TYPE STANDARD TABLE OF ty_contents WITH DEFAULT KEY.
    DATA ls_contents LIKE LINE OF lt_contents.
    DATA ls_list     LIKE LINE OF rt_list.


    SELECT trkorr FROM e070 INTO TABLE lt_tasks
      WHERE strkorr = iv_request
      ORDER BY PRIMARY KEY.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT trkorr as4pos pgmid object obj_name FROM e071
      INTO TABLE lt_contents
      FOR ALL ENTRIES IN lt_tasks
      WHERE trkorr = lt_tasks-table_line
      ORDER BY PRIMARY KEY.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_contents INTO ls_contents.
      CASE ls_contents-pgmid.
        WHEN 'R3TR'.
          ls_list-object = ls_contents-object.
          ls_list-obj_name = ls_contents-obj_name.
          INSERT ls_list INTO TABLE rt_list.
        WHEN 'LIMU'.
          TRY.
              Lif_abapgit_cts_api~get_r3tr_obj_for_limu_obj(
                EXPORTING
                  iv_object   = ls_contents-object
                  iv_obj_name = ls_contents-obj_name
                IMPORTING
                  ev_object   = ls_list-object
                  ev_obj_name = ls_list-obj_name ).
              INSERT ls_list INTO TABLE rt_list.
            CATCH Lcx_abapgit_exception.
          ENDTRY.
      ENDCASE.
    ENDLOOP.

    SORT rt_list BY object obj_name.
    DELETE ADJACENT DUPLICATES FROM rt_list COMPARING object obj_name.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_CTS_API implementation

*>>>>>>> ZCL_ABAPGIT_DATA_DESERIALIZER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_data_deserializer=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_data_deserializer=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_data_deserializer=ccau.
*CLASS SHRITEFUH64VYIPN5I4UHL45BFLR4A DEFINITION DEFERRED.
*CLASS zcl_abapgit_data_deserializer DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BFLR4A.




class LCL_ABAPGIT_DATA_DESERIALIZER implementation.
*"* method's implementations
*include methods.
  METHOD convert_json_to_itab.

    DATA lo_ajson TYPE REF TO Lcl_abapgit_ajson.
    DATA lx_ajson TYPE REF TO Lcx_abapgit_ajson_error.

    FIELD-SYMBOLS <lg_tab> TYPE ANY TABLE.

    ASSIGN ir_data->* TO <lg_tab>.

    TRY.
        lo_ajson = Lcl_abapgit_ajson=>parse( Lcl_abapgit_convert=>xstring_to_string_utf8( is_file-data ) ).
        lo_ajson->Lif_abapgit_ajson~to_abap( IMPORTING ev_container = <lg_tab> ).
      CATCH Lcx_abapgit_ajson_error INTO lx_ajson.
        Lcx_abapgit_exception=>raise( lx_ajson->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
  METHOD preview_database_changes.

* method currently distinguishes between records be deleted and inserted (comparison of complete record)

    FIELD-SYMBOLS <lg_old> TYPE ANY TABLE.
    FIELD-SYMBOLS <lg_new> TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_del> TYPE any.
    FIELD-SYMBOLS <ls_ins> TYPE any.
    FIELD-SYMBOLS <lg_del> TYPE ANY TABLE.
    FIELD-SYMBOLS <lg_ins> TYPE ANY TABLE.
    FIELD-SYMBOLS <lg_upd> TYPE ANY TABLE.

    ASSIGN ir_db_data->* TO <lg_old>.
    ASSIGN ir_lc_data->* TO <lg_new>.

    rs_result-type = Lif_abapgit_data_config=>c_data_type-tabu.
    rs_result-name = iv_name.
    rs_result-deletes = Lcl_abapgit_data_utils=>build_table_itab( iv_name ).
    rs_result-inserts = Lcl_abapgit_data_utils=>build_table_itab( iv_name ).
    rs_result-updates = Lcl_abapgit_data_utils=>build_table_itab( iv_name ).
    ASSIGN rs_result-deletes->* TO <lg_del>.
    ASSIGN rs_result-inserts->* TO <lg_ins>.
    ASSIGN rs_result-updates->* TO <lg_upd>.

    <lg_del> = <lg_old>.
    <lg_ins> = <lg_new>.

    " Remove identical records
    LOOP AT <lg_del> ASSIGNING <ls_del>.
      READ TABLE <lg_ins> ASSIGNING <ls_ins> FROM <ls_del>.
      IF sy-subrc = 0.
        IF <ls_del> <> <ls_ins>.
          " Identical key but not identical component values
          INSERT <ls_ins> INTO TABLE <lg_upd>.
        ENDIF.
        DELETE TABLE <lg_del> FROM <ls_del>.
        DELETE TABLE <lg_ins> FROM <ls_ins>.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD read_database_table.

    DATA lv_where LIKE LINE OF it_where.

    FIELD-SYMBOLS <lg_tab> TYPE ANY TABLE.

    rr_data = Lcl_abapgit_data_utils=>build_table_itab( iv_name ).
    ASSIGN rr_data->* TO <lg_tab>.

    LOOP AT it_where INTO lv_where.
      SELECT * FROM (iv_name) APPENDING TABLE <lg_tab> WHERE (lv_where) ORDER BY PRIMARY KEY.
    ENDLOOP.
    IF lines( it_where ) = 0.
      SELECT * FROM (iv_name) INTO TABLE <lg_tab> ORDER BY PRIMARY KEY.
    ENDIF.

  ENDMETHOD.
  METHOD write_database_table.

    FIELD-SYMBOLS <lg_del> TYPE ANY TABLE.
    FIELD-SYMBOLS <lg_ins> TYPE ANY TABLE.
    FIELD-SYMBOLS <lg_upd> TYPE ANY TABLE.

    IF Lcl_abapgit_data_utils=>does_table_exist( iv_name ) = abap_false.
      Lcx_abapgit_exception=>raise( |Table { iv_name } not found for data deserialization| ).
    ENDIF.

    ASSIGN ir_del->* TO <lg_del>.
    ASSIGN ir_ins->* TO <lg_ins>.
    ASSIGN ir_upd->* TO <lg_upd>.

    IF lines( <lg_del> ) > 0.
      DELETE (iv_name) FROM TABLE <lg_del>.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Error deleting { lines( <lg_del> ) } records from table { iv_name }| ).
      ENDIF.
    ENDIF.

    IF lines( <lg_ins> ) > 0.
      INSERT (iv_name) FROM TABLE <lg_ins>.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Error inserting { lines( <lg_ins> ) } records into table { iv_name }| ).
      ENDIF.
    ENDIF.

    IF lines( <lg_upd> ) > 0.
      UPDATE (iv_name) FROM TABLE <lg_upd>.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Error updating { lines( <lg_upd> ) } records into table { iv_name }| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_data_deserializer~actualize.

* this method updates the database

    DATA ls_result  LIKE LINE OF it_result.
    DATA li_cts_api TYPE REF TO Lif_abapgit_cts_api.

    FIELD-SYMBOLS:
      <lt_ins> TYPE ANY TABLE,
      <lt_del> TYPE ANY TABLE,
      <lt_upd> TYPE ANY TABLE.

    LOOP AT it_result INTO ls_result.
      ASSERT ls_result-type = Lif_abapgit_data_config=>c_data_type-tabu. " todo
      ASSERT ls_result-name IS NOT INITIAL.

      " Did the user flagged this object for update?
      READ TABLE is_checks-overwrite TRANSPORTING NO FIELDS
        WITH KEY object_type_and_name
        COMPONENTS
          obj_type = ls_result-type
          obj_name = ls_result-name
          decision = Lif_abapgit_definitions=>c_yes.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF is_table_allowed_to_edit( ls_result ) = abap_false.
        Lcx_abapgit_exception=>raise( |Table { ls_result-name } not supported for updating data| ).
      ENDIF.

      write_database_table(
        iv_name = ls_result-name
        ir_del  = ls_result-deletes
        ir_ins  = ls_result-inserts
        ir_upd  = ls_result-updates ).

      ASSIGN ls_result-inserts->* TO <lt_ins>.
      ASSIGN ls_result-deletes->* TO <lt_del>.
      ASSIGN ls_result-updates->* TO <lt_upd>.

      IF Lcl_abapgit_data_utils=>is_customizing_table( ls_result-name ) = abap_true.
        IF li_cts_api IS INITIAL.
          li_cts_api = Lcl_abapgit_factory=>get_cts_api( ).
        ENDIF.

        li_cts_api->create_transport_entries(
          iv_transport = is_checks-customizing-transport
          it_table_ins = <lt_ins>
          it_table_upd = <lt_upd>
          it_table_del = <lt_del>
          iv_tabname   = |{ ls_result-name }| ).
      ENDIF.

      INSERT ls_result-file INTO TABLE rt_accessed_files. " data file
      INSERT ls_result-config INTO TABLE rt_accessed_files. " config file
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_data_deserializer~deserialize.

* this method does not persist any changes to the database

    DATA lt_configs TYPE Lif_abapgit_data_config=>ty_config_tt.
    DATA ls_config  LIKE LINE OF lt_configs.
    DATA lr_lc_data TYPE REF TO data.
    DATA lr_db_data TYPE REF TO data.
    DATA ls_file    LIKE LINE OF it_files.
    DATA ls_result  LIKE LINE OF rt_result.

    lt_configs = ii_config->get_configs( ).

    LOOP AT lt_configs INTO ls_config.
      ASSERT ls_config-type = Lif_abapgit_data_config=>c_data_type-tabu. " todo
      ASSERT ls_config-name IS NOT INITIAL.

      lr_lc_data = Lcl_abapgit_data_utils=>build_table_itab( ls_config-name ).

      READ TABLE it_files INTO ls_file
        WITH KEY file_path
        COMPONENTS path     = Lif_abapgit_data_config=>c_default_path
                   filename = Lcl_abapgit_data_utils=>build_data_filename( ls_config ).
      IF sy-subrc = 0.
        convert_json_to_itab(
          ir_data = lr_lc_data
          is_file = ls_file ).

        lr_db_data = read_database_table(
          iv_name  = ls_config-name
          it_where = ls_config-where ).

        ls_result = preview_database_changes(
          iv_name    = ls_config-name
          ir_lc_data = lr_lc_data
          ir_db_data = lr_db_data ).

        MOVE-CORRESPONDING ls_file TO ls_result-file. " data file

        READ TABLE it_files INTO ls_file
          WITH KEY file_path
          COMPONENTS path     = Lif_abapgit_data_config=>c_default_path
                     filename = Lcl_abapgit_data_utils=>build_config_filename( ls_config ).
        ASSERT sy-subrc = 0.

        MOVE-CORRESPONDING ls_file TO ls_result-config. " config file

        INSERT ls_result INTO TABLE rt_result.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD determine_transport_request.

    DATA li_exit TYPE REF TO Lif_abapgit_exit.

    li_exit = Lcl_abapgit_exit=>get_instance( ).

    " Use transport from repo settings if maintained, or determine via user exit.
    " If transport keeps empty here, it'll requested later via popup.
    rv_transport_request = io_repo->get_local_settings( )-customizing_request.

    li_exit->determine_transport_request(
      EXPORTING
        io_repo              = io_repo
        iv_transport_type    = iv_transport_type
      CHANGING
        cv_transport_request = rv_transport_request ).

  ENDMETHOD.
  METHOD Lif_abapgit_data_deserializer~deserialize_check.

    DATA lt_configs TYPE Lif_abapgit_data_config=>ty_config_tt.

    lt_configs = ii_config->get_configs( ).

    IF lt_configs IS NOT INITIAL.
      rs_checks-required     = abap_true.
      rs_checks-type-request = Lif_abapgit_cts_api=>c_transport_type-cust_request.
      rs_checks-type-task    = Lif_abapgit_cts_api=>c_transport_type-cust_task.
      rs_checks-transport    = determine_transport_request(
                                 io_repo           = io_repo
                                 iv_transport_type = rs_checks-type ).
    ENDIF.

  ENDMETHOD.
  METHOD is_table_allowed_to_edit.

    " Is the object supported (by default or based on exit)?
    rv_allowed_to_edit = Lcl_abapgit_data_factory=>get_supporter( )->is_object_supported(
      iv_type = is_result-type
      iv_name = is_result-name ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_DATA_DESERIALIZER implementation

*>>>>>>> ZCL_ABAPGIT_DATA_SERIALIZER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_data_serializer===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_data_serializer===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_data_serializer===ccau.
*CLASS SHRITEFUH64VYIPN5I4UHL45BFNR4A DEFINITION DEFERRED.
*CLASS zcl_abapgit_data_serializer DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BFNR4A.




class LCL_ABAPGIT_DATA_SERIALIZER implementation.
*"* method's implementations
*include methods.
  METHOD convert_itab_to_json.

    DATA lo_ajson  TYPE REF TO Lcl_abapgit_ajson.
    DATA lv_string TYPE string.
    DATA lx_ajson  TYPE REF TO Lcx_abapgit_ajson_error.

    FIELD-SYMBOLS <lg_tab> TYPE ANY TABLE.

    ASSIGN ir_data->* TO <lg_tab>.

    TRY.
        lo_ajson = Lcl_abapgit_ajson=>create_empty( ).
        lo_ajson->keep_item_order( ).
        lo_ajson->set(
          iv_path = '/'
          iv_val = <lg_tab> ).

        IF iv_skip_initial = abap_true.
          lo_ajson = Lcl_abapgit_ajson=>create_from(
            ii_source_json = lo_ajson
            ii_filter = Lcl_abapgit_ajson_filter_lib=>create_empty_filter( ) ).
        ENDIF.

        lv_string = lo_ajson->stringify( 2 ).
      CATCH Lcx_abapgit_ajson_error INTO lx_ajson.
        Lcx_abapgit_exception=>raise( lx_ajson->get_text( ) ).
    ENDTRY.

    rv_data = Lcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).

  ENDMETHOD.
  METHOD read_database_table.

    DATA lv_records TYPE i.
    DATA lv_where LIKE LINE OF it_where.
    DATA lx_sql TYPE REF TO cx_sy_sql_error.

    FIELD-SYMBOLS <lg_tab> TYPE ANY TABLE.

    rr_data = Lcl_abapgit_data_utils=>build_table_itab( iv_name ).
    ASSIGN rr_data->* TO <lg_tab>.

    TRY.
        LOOP AT it_where INTO lv_where.
          SELECT * FROM (iv_name) APPENDING TABLE <lg_tab> WHERE (lv_where) ORDER BY PRIMARY KEY.
        ENDLOOP.
        IF lines( it_where ) = 0.
          SELECT * FROM (iv_name) INTO TABLE <lg_tab> ORDER BY PRIMARY KEY.
        ENDIF.
      CATCH cx_sy_sql_error INTO lx_sql.
        Lcx_abapgit_exception=>raise(
          iv_text     = lx_sql->get_text( )
          ix_previous = lx_sql ).
    ENDTRY.

    lv_records = lines( <lg_tab> ).
    IF lv_records > c_max_records.
      Lcx_abapgit_exception=>raise( |Too many records selected from table { iv_name
        } (selected { lv_records }, max { c_max_records })| ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_data_serializer~serialize.

    DATA lt_configs TYPE Lif_abapgit_data_config=>ty_config_tt.
    DATA ls_config  LIKE LINE OF lt_configs.
    DATA ls_file    LIKE LINE OF rt_files.
    DATA lr_data    TYPE REF TO data.

    ls_file-path = Lif_abapgit_data_config=>c_default_path.
    lt_configs = ii_config->get_configs( ).

    LOOP AT lt_configs INTO ls_config.
      ASSERT ls_config-type = Lif_abapgit_data_config=>c_data_type-tabu. " todo
      ASSERT ls_config-name IS NOT INITIAL.

      IF Lcl_abapgit_data_utils=>does_table_exist( ls_config-name ) = abap_true.
        lr_data = read_database_table(
          iv_name  = ls_config-name
          it_where = ls_config-where ).

        ls_file-data = convert_itab_to_json(
          ir_data         = lr_data
          iv_skip_initial = ls_config-skip_initial ).
      ELSE.
        ls_file-data = Lcl_abapgit_convert=>string_to_xstring_utf8( '[]' ).
      ENDIF.

      ls_file-filename = Lcl_abapgit_data_utils=>build_data_filename( ls_config ).
      ls_file-sha1 = Lcl_abapgit_hash=>sha1_blob( ls_file-data ).
      APPEND ls_file TO rt_files.
    ENDLOOP.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_DATA_SERIALIZER implementation

*>>>>>>> ZCL_ABAPGIT_DATA_SUPPORTER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_data_supporter====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_data_supporter====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_data_supporter====ccau.
CLASS SHRITEFUH64VYIPN5I4UHL45BFPR4A DEFINITION.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_data_supporter.
ENDCLASS.

CLASS SHRITEFUH64VYIPN5I4UHL45BFPR4A IMPLEMENTATION.
  METHOD Lif_abapgit_data_supporter~is_object_supported.

    IF iv_type = Lif_abapgit_data_config=>c_data_type-tabu AND iv_name = 'T005'.
      rv_supported = abap_true.
    ENDIF.

  ENDMETHOD.

ENDCLASS.


*CLASS zcl_abapgit_data_supporter DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BFRR4A.


class LCL_ABAPGIT_DATA_SUPPORTER implementation.
*"* method's implementations
*include methods.
  METHOD get_supported_objects.

    DATA:
      lt_tables  TYPE STANDARD TABLE OF tabname,
      lv_tabname TYPE tabname,
      ls_object  LIKE LINE OF mt_supported_objects,
      li_exit    TYPE REF TO Lif_abapgit_exit.

    " For safety reasons, by default only customer-defined customizing tables are supported
    SELECT dd02l~tabname
      FROM dd09l JOIN dd02l
        ON dd09l~tabname = dd02l~tabname
        AND dd09l~as4local = dd02l~as4local
        AND dd09l~as4vers = dd02l~as4vers
      INTO TABLE lt_tables
      WHERE dd02l~tabclass = 'TRANSP'
        AND dd09l~tabart = 'APPL2'
        AND dd09l~as4user <> 'SAP'
        AND dd09l~as4local = 'A' "Only active tables
        AND dd02l~contflag = 'C' "Only customizing tables
      ORDER BY dd02l~tabname.

    LOOP AT lt_tables INTO lv_tabname.
      ls_object-type = Lif_abapgit_data_config=>c_data_type-tabu.
      ls_object-name = lv_tabname.
      INSERT ls_object INTO TABLE mt_supported_objects.
    ENDLOOP.

    " The list of supported objects can be enhanced using an exit
    " Name patterns are allowed. For example, TABU T009*
    li_exit = Lcl_abapgit_exit=>get_instance( ).
    li_exit->change_supported_data_objects( CHANGING ct_objects = mt_supported_objects ).

  ENDMETHOD.
  METHOD Lif_abapgit_data_supporter~is_object_supported.

    FIELD-SYMBOLS <ls_object> LIKE LINE OF mt_supported_objects.

    IF mt_supported_objects IS INITIAL.
      get_supported_objects( ).
    ENDIF.

    READ TABLE mt_supported_objects TRANSPORTING NO FIELDS
      WITH TABLE KEY type = iv_type name = iv_name.
    IF sy-subrc = 0.
      rv_supported = abap_true.
    ELSE.
      " Check if object name matches pattern
      LOOP AT mt_supported_objects ASSIGNING <ls_object> WHERE type = iv_type.
        IF iv_name CP <ls_object>-name.
          rv_supported = abap_true.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_DATA_SUPPORTER implementation

*>>>>>>> ZCL_ABAPGIT_DEPENDENCIES <<<<<<<*

*"* macro definitions
*include zcl_abapgit_dependencies======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_dependencies======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_dependencies======ccau.



*CLASS zcl_abapgit_dependencies DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BFWR4A.


class LCL_ABAPGIT_DEPENDENCIES implementation.
*"* method's implementations
*include methods.
  METHOD get_ddls_dependencies.

    DATA: lt_ddls_name TYPE TABLE OF ddsymtab,
          ls_ddls_name TYPE ddsymtab.

    ls_ddls_name-name = iv_ddls_name.
    INSERT ls_ddls_name INTO TABLE lt_ddls_name.

    PERFORM ('DDLS_GET_DEP') IN PROGRAM ('RADMASDL')
                             TABLES lt_ddls_name rt_dependency.

  ENDMETHOD.
  METHOD resolve.

    DATA: lv_tabclass TYPE dd02l-tabclass.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF ct_tadir.

    " misuse field KORRNUM to fix deletion sequence
    " higher value means later deletion

    LOOP AT ct_tadir ASSIGNING <ls_tadir>.
      CASE <ls_tadir>-object.
        WHEN 'DEVC'.
          " Packages last
          <ls_tadir>-korrnum = '999000'.
        WHEN 'DOMA'.
          <ls_tadir>-korrnum = '900000'.
        WHEN 'SPRX'.
          <ls_tadir>-korrnum = '850000'.
        WHEN 'WEBI'.
          <ls_tadir>-korrnum = '840000'.
        WHEN 'PARA'.
          " PARA after DTEL
          <ls_tadir>-korrnum = '810000'.
        WHEN 'DTEL'.
          <ls_tadir>-korrnum = '800000'.
        WHEN 'SHLP'.
          " SHLP after TABL
          <ls_tadir>-korrnum = '760000'.
        WHEN 'TTYP' OR 'TABL' OR 'VIEW'.
          SELECT SINGLE tabclass FROM dd02l
            INTO lv_tabclass
            WHERE tabname = <ls_tadir>-obj_name
            AND as4local = 'A'
            AND as4vers = '0000'.
          IF sy-subrc = 0 AND lv_tabclass = 'APPEND'.
            " delete append structures before database tables
            <ls_tadir>-korrnum = '730000'.
          ELSE.
            <ls_tadir>-korrnum = '750000'.
          ENDIF.
        WHEN 'ENQU'.
          " ENQU before TABL
          <ls_tadir>-korrnum = '725000'.
        WHEN 'DDLS'.
          " DDLS after DCLS but before other DDIC
          <ls_tadir>-korrnum = '720000'.
        WHEN 'DDLX'.
          " DDLX before DDLS
          <ls_tadir>-korrnum = '719000'.
        WHEN 'AUTH'.
          " AUTH after DCLS
          <ls_tadir>-korrnum = '715000'.
        WHEN 'SUSH'.
          " SUSH after SUSC
          <ls_tadir>-korrnum = '712000'.
        WHEN 'SUSC'.
          " SUSC after SUSO
          <ls_tadir>-korrnum = '711000'.
        WHEN 'SUSO'.
          " SUSO after DCLS
          <ls_tadir>-korrnum = '710000'.
        WHEN 'DCLS'.
          " AUTH and SUSO after DCLS
          <ls_tadir>-korrnum = '705000'.
        WHEN 'IASP'.
          <ls_tadir>-korrnum = '552000'.
        WHEN 'IARP'.
          <ls_tadir>-korrnum = '551000'.
        WHEN 'IATU'.
          <ls_tadir>-korrnum = '550000'.
        WHEN 'ACID'.
          " ACID after PROG/FUGR/CLAS
          <ls_tadir>-korrnum = '300000'.
        WHEN 'FUGR'.
          <ls_tadir>-korrnum = '260000'.
        WHEN 'PROG'.
          " delete includes after main programs
          SELECT COUNT(*) FROM reposrc
            WHERE progname = <ls_tadir>-obj_name
            AND r3state = 'A'
            AND subc = 'I'.
          IF sy-subrc = 0.
            <ls_tadir>-korrnum = '250000'.
          ELSE.
            <ls_tadir>-korrnum = '240000'.
          ENDIF.
        WHEN 'INTF'.
          <ls_tadir>-korrnum = '230000'.
        WHEN 'CLAS'.
          <ls_tadir>-korrnum = '220000'.
        WHEN 'IDOC'.
          <ls_tadir>-korrnum = '200000'.
        WHEN 'IOBJ'.
          <ls_tadir>-korrnum = '195000'.
        WHEN 'ODSO'.
          <ls_tadir>-korrnum = '190000'.
        WHEN 'WDCA'.
          <ls_tadir>-korrnum = '174000'.
        WHEN 'WDYA'.
          <ls_tadir>-korrnum = '173000'.
        WHEN 'WDCC'.
          <ls_tadir>-korrnum = '172000'.
        WHEN 'WDYN'.
          <ls_tadir>-korrnum = '171000'.
        WHEN 'IEXT'.
          <ls_tadir>-korrnum = '150000'.
        WHEN 'SAPC'.
          " SAPC after SICF
          <ls_tadir>-korrnum = '140000'.
        WHEN 'PINF'.
          " PINF before exposed objects
          <ls_tadir>-korrnum = '130000'.
        WHEN OTHERS.
          <ls_tadir>-korrnum = '100000'.
      ENDCASE.
    ENDLOOP.

    resolve_ddic( CHANGING ct_tadir = ct_tadir ).
    resolve_packages( CHANGING ct_tadir = ct_tadir ).

    SORT ct_tadir BY korrnum ASCENDING.

  ENDMETHOD.
  METHOD resolve_ddic.
* this will make sure the deletion sequence of structures/tables work
* in case they have dependencies with .INCLUDE

    TYPES: BEGIN OF ty_edge,
             from TYPE ty_item,
             to   TYPE ty_item,
           END OF ty_edge.

    DATA: lt_nodes        TYPE TABLE OF ty_item,
          lt_edges        TYPE TABLE OF ty_edge,
          lt_findstrings  TYPE TABLE OF rsfind,
          lv_plus         TYPE i VALUE 1,
          lv_find_obj_cls TYPE euobj-id,
          lv_index        TYPE i,
          lv_before       TYPE i,
          lt_founds       TYPE TABLE OF rsfindlst,
          lt_scope        TYPE STANDARD TABLE OF seu_obj,
          lt_dependency   TYPE ty_dedenpencies.

    FIELD-SYMBOLS: <ls_tadir_ddls>      TYPE Lif_abapgit_definitions=>ty_tadir,
                   <ls_dependency>      TYPE ty_dependency,
                   <ls_tadir_dependent> TYPE Lif_abapgit_definitions=>ty_tadir,
                   <ls_tadir>           LIKE LINE OF ct_tadir,
                   <ls_edge>            LIKE LINE OF lt_edges,
                   <ls_found>           LIKE LINE OF lt_founds,
                   <ls_node>            LIKE LINE OF lt_nodes.

    " build nodes
    LOOP AT ct_tadir ASSIGNING <ls_tadir>
        WHERE object = 'TABL'
        OR object = 'VIEW'
        OR object = 'TTYP'.
      APPEND INITIAL LINE TO lt_nodes ASSIGNING <ls_node>.
      <ls_node>-obj_name = <ls_tadir>-obj_name.
      <ls_node>-obj_type = <ls_tadir>-object.
    ENDLOOP.

    APPEND 'TABL' TO lt_scope.
    APPEND 'VIEW' TO lt_scope.
    APPEND 'STRU' TO lt_scope.
    APPEND 'TTYP' TO lt_scope.

    " build edges
    LOOP AT lt_nodes ASSIGNING <ls_node>.

      CLEAR lt_findstrings.
      APPEND <ls_node>-obj_name TO lt_findstrings.
      lv_find_obj_cls = <ls_node>-obj_type.

      CALL FUNCTION 'RS_EU_CROSSREF'
        EXPORTING
          i_find_obj_cls           = lv_find_obj_cls
        TABLES
          i_findstrings            = lt_findstrings
          o_founds                 = lt_founds
          i_scope_object_cls       = lt_scope
        EXCEPTIONS
          not_executed             = 1
          not_found                = 2
          illegal_object           = 3
          no_cross_for_this_object = 4
          batch                    = 5
          batchjob_error           = 6
          wrong_type               = 7
          object_not_exist         = 8
          OTHERS                   = 9.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      LOOP AT lt_founds ASSIGNING <ls_found>.
        APPEND INITIAL LINE TO lt_edges ASSIGNING <ls_edge>.
        <ls_edge>-from = <ls_node>.

        <ls_edge>-to-obj_name   = <ls_found>-object.
        CASE <ls_found>-object_cls.
          WHEN 'DS'
              OR 'DT'.
            <ls_edge>-to-obj_type = 'TABL'.
          WHEN 'DV'.
            <ls_edge>-to-obj_type = 'VIEW'.
          WHEN 'DA'.
            <ls_edge>-to-obj_type = 'TTYP'.
          WHEN OTHERS.
            Lcx_abapgit_exception=>raise( 'resolve_ddic, unknown object_cls' ).
        ENDCASE.
      ENDLOOP.

    ENDLOOP.

    " build DDLS edges
    SORT ct_tadir. "binary search
    LOOP AT ct_tadir ASSIGNING <ls_tadir_ddls>
                     WHERE object = 'DDLS'.

      CLEAR: lt_dependency.

      APPEND INITIAL LINE TO lt_nodes ASSIGNING <ls_node>.
      <ls_node>-obj_name = <ls_tadir_ddls>-obj_name.
      <ls_node>-obj_type = <ls_tadir_ddls>-object.

      lt_dependency = get_ddls_dependencies( <ls_tadir_ddls>-obj_name ).

      LOOP AT lt_dependency ASSIGNING <ls_dependency>
                            WHERE deptyp = 'DDLS'
                            AND refname = <ls_tadir_ddls>-obj_name.

        READ TABLE ct_tadir ASSIGNING <ls_tadir_dependent>
                            WITH KEY pgmid    = 'R3TR'
                                     object   = 'DDLS'
                                     obj_name = <ls_dependency>-depname
                            BINARY SEARCH.
        CHECK sy-subrc = 0.

        APPEND INITIAL LINE TO lt_edges ASSIGNING <ls_edge>.
        <ls_edge>-from = <ls_node>.
        <ls_edge>-to-obj_name = <ls_dependency>-depname.
        <ls_edge>-to-obj_type = 'DDLS'.

      ENDLOOP.

    ENDLOOP.

    DO.
      lv_before = lines( lt_nodes ).
      LOOP AT lt_nodes ASSIGNING <ls_node>.
        lv_index = sy-tabix.
        READ TABLE lt_edges WITH KEY
          from-obj_name = <ls_node>-obj_name
          from-obj_type = <ls_node>-obj_type
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          LOOP AT ct_tadir ASSIGNING <ls_tadir>
              WHERE obj_name = <ls_node>-obj_name
              AND object = <ls_node>-obj_type.
            <ls_tadir>-korrnum = <ls_tadir>-korrnum + lv_plus.
            CONDENSE <ls_tadir>-korrnum.
          ENDLOOP.
          DELETE lt_edges
            WHERE to-obj_name = <ls_node>-obj_name
            AND to-obj_type = <ls_node>-obj_type.
          DELETE lt_nodes INDEX lv_index.
          EXIT. " make sure the sequence is fixed
        ENDIF.
      ENDLOOP.
      IF lv_before = lines( lt_nodes ).
        EXIT.
      ENDIF.
      lv_plus = lv_plus + 1.
    ENDDO.

  ENDMETHOD.
  METHOD resolve_packages.

    DATA: lt_subpackages TYPE Lif_abapgit_sap_package=>ty_devclass_tt.

    FIELD-SYMBOLS: <ls_tadir>            LIKE LINE OF ct_tadir,
                   <lv_subpackage>       LIKE LINE OF lt_subpackages,
                   <ls_tadir_subpackage> LIKE LINE OF ct_tadir.

    " List subpackage before corresponding superpackage

    LOOP AT ct_tadir ASSIGNING <ls_tadir>
                     WHERE object = 'DEVC'.

      lt_subpackages = Lcl_abapgit_factory=>get_sap_package( |{ <ls_tadir>-obj_name }| )->list_subpackages( ).

      LOOP AT lt_subpackages ASSIGNING <lv_subpackage>.

        READ TABLE ct_tadir ASSIGNING <ls_tadir_subpackage>
                            WITH KEY object   = 'DEVC'
                                     obj_name = <lv_subpackage>.
        IF sy-subrc = 0.
          <ls_tadir_subpackage>-korrnum = condense( |{ <ls_tadir_subpackage>-korrnum - 1 }| ).
        ENDIF.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_DEPENDENCIES implementation

*>>>>>>> ZCL_ABAPGIT_DOT_ABAPGIT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_dot_abapgit=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_dot_abapgit=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_dot_abapgit=======ccau.
*CLASS SHRITEFUH64VYIPN5I4UHL45BFZR4A DEFINITION DEFERRED.
*CLASS zcl_abapgit_dot_abapgit DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BFZR4A.



class LCL_ABAPGIT_DOT_ABAPGIT implementation.
*"* method's implementations
*include methods.
  METHOD get_name.
    rv_name = ms_data-name.
  ENDMETHOD.
  METHOD set_name.
    ms_data-name = iv_name.
  ENDMETHOD.
  METHOD add_ignore.

    DATA: lv_name TYPE string.

    FIELD-SYMBOLS: <lv_ignore> LIKE LINE OF ms_data-ignore.


    lv_name = iv_path && iv_filename.

    READ TABLE ms_data-ignore FROM lv_name TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO ms_data-ignore ASSIGNING <lv_ignore>.
    <lv_ignore> = lv_name.

  ENDMETHOD.
  METHOD build_default.

    DATA: ls_data TYPE Lif_abapgit_dot_abapgit=>ty_dot_abapgit.


    ls_data-master_language = sy-langu.
    ls_data-starting_folder = '/src/'.
    ls_data-folder_logic    = Lif_abapgit_dot_abapgit=>c_folder_logic-prefix.

    CREATE OBJECT ro_dot_abapgit
      EXPORTING
        is_data = ls_data.

  ENDMETHOD.
  METHOD constructor.
    ms_data = is_data.
  ENDMETHOD.
  METHOD deserialize.

    DATA: lv_xml  TYPE string,
          ls_data TYPE Lif_abapgit_dot_abapgit=>ty_dot_abapgit.


    lv_xml = Lcl_abapgit_convert=>xstring_to_string_utf8( iv_xstr ).

    ls_data = from_xml( lv_xml ).

    CREATE OBJECT ro_dot_abapgit
      EXPORTING
        is_data = ls_data.

  ENDMETHOD.
  METHOD from_xml.

    DATA: lv_xml TYPE string.

    lv_xml = iv_xml.

    CALL TRANSFORMATION id
      OPTIONS value_handling = 'accept_data_loss'
      SOURCE XML lv_xml
      RESULT data = rs_data.

* downward compatibility
    IF rs_data-folder_logic IS INITIAL.
      rs_data-folder_logic = Lif_abapgit_dot_abapgit=>c_folder_logic-prefix.
    ENDIF.

  ENDMETHOD.
  METHOD get_data.
    rs_data = ms_data.
  ENDMETHOD.
  METHOD get_folder_logic.
    rv_logic = ms_data-folder_logic.
  ENDMETHOD.
  METHOD get_i18n_languages.
    rt_languages = ms_data-i18n_languages.
  ENDMETHOD.
  METHOD get_main_language.
    rv_language = ms_data-master_language.
  ENDMETHOD.
  METHOD get_requirements.
    rt_requirements = ms_data-requirements.
  ENDMETHOD.
  METHOD get_signature.

    rs_signature-path     = Lif_abapgit_definitions=>c_root_dir.
    rs_signature-filename = Lif_abapgit_definitions=>c_dot_abapgit.
    rs_signature-sha1     = Lcl_abapgit_hash=>sha1_blob( serialize( ) ).

  ENDMETHOD.
  METHOD get_starting_folder.
    rv_path = ms_data-starting_folder.
  ENDMETHOD.
  METHOD get_version_constant.
    rv_version_constant = ms_data-version_constant.
  ENDMETHOD.
  METHOD is_ignored.

    DATA: lv_name     TYPE string,
          lv_starting TYPE string,
          lv_dot      TYPE string,
          lv_ignore   TYPE string.


    lv_name = iv_path && iv_filename.

    CONCATENATE ms_data-starting_folder '*' INTO lv_starting.

    " Always allow .abapgit.xml and .apack-manifest.xml
    CONCATENATE '/' Lif_abapgit_definitions=>c_dot_abapgit INTO lv_dot.
    IF lv_name = lv_dot.
      RETURN.
    ENDIF.
    CONCATENATE '/' Lif_abapgit_apack_definitions=>c_dot_apack_manifest INTO lv_dot.
    IF lv_name = lv_dot.
      RETURN.
    ENDIF.

    " Ignore all files matching pattern in ignore list
    LOOP AT ms_data-ignore INTO lv_ignore.
      IF lv_name CP lv_ignore.
        rv_ignored = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Ignore all files outside of starting folder tree
    IF ms_data-starting_folder <> '/' AND NOT lv_name CP lv_starting.
      rv_ignored = abap_true.
    ENDIF.

    IF iv_path = Lif_abapgit_data_config=>c_default_path.
      rv_ignored = abap_false.
    ENDIF.

  ENDMETHOD.
  METHOD remove_ignore.

    DATA: lv_name TYPE string.


    lv_name = iv_path && iv_filename.

    DELETE TABLE ms_data-ignore FROM lv_name.

  ENDMETHOD.
  METHOD serialize.

    DATA lv_xml TYPE string.

    lv_xml = to_xml( ms_data ).

    rv_xstr = Lcl_abapgit_convert=>string_to_xstring_utf8_bom( lv_xml ).

  ENDMETHOD.
  METHOD set_folder_logic.
    ms_data-folder_logic = iv_logic.
  ENDMETHOD.
  METHOD set_i18n_languages.
    ms_data-i18n_languages = it_languages.
  ENDMETHOD.
  METHOD set_requirements.
    ms_data-requirements = it_requirements.
  ENDMETHOD.
  METHOD set_starting_folder.
    ms_data-starting_folder = iv_path.
  ENDMETHOD.
  METHOD set_version_constant.
    ms_data-version_constant = iv_version_constant.
  ENDMETHOD.
  METHOD to_file.
    rs_file-path     = Lif_abapgit_definitions=>c_root_dir.
    rs_file-filename = Lif_abapgit_definitions=>c_dot_abapgit.
    rs_file-data     = serialize( ).
    rs_file-sha1     = Lcl_abapgit_hash=>sha1_blob( rs_file-data ).
  ENDMETHOD.
  METHOD to_xml.

    CALL TRANSFORMATION id
      OPTIONS initial_components = 'suppress'
      SOURCE data = is_data
      RESULT XML rv_xml.

    rv_xml = Lcl_abapgit_xml_pretty=>print( rv_xml ).

    REPLACE FIRST OCCURRENCE
      OF REGEX '<\?xml version="1\.0" encoding="[\w-]+"\?>'
      IN rv_xml
      WITH '<?xml version="1.0" encoding="utf-8"?>'.
    ASSERT sy-subrc = 0.

  ENDMETHOD.
  METHOD get_abap_language_version.
    rv_abap_language_version = ms_data-abap_language_version.
  ENDMETHOD.
  METHOD set_abap_language_version.
    ms_data-abap_language_version = iv_abap_language_version.
  ENDMETHOD.
  METHOD use_lxe.

    IF iv_yes <> abap_undefined.
      ms_data-use_lxe = iv_yes.
    ENDIF.

    rv_yes = ms_data-use_lxe.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_DOT_ABAPGIT implementation

*>>>>>>> ZCL_ABAPGIT_ENVIRONMENT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_environment=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_environment=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_environment=======ccau.
*CLASS SHRITEFUH64VYIPN5I4UHL45BF3R4A DEFINITION DEFERRED.
*CLASS zcl_abapgit_environment DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BF3R4A.




class LCL_ABAPGIT_ENVIRONMENT implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_environment~init_parallel_processing.

    DATA: lv_group TYPE rzlli_apcl.

    lv_group = iv_group.

    " SPBT_INITIALIZE gives error PBT_ENV_ALREADY_INITIALIZED if called
    " multiple times in same session
    CALL FUNCTION 'SPBT_INITIALIZE'
      EXPORTING
        group_name                     = lv_group
      IMPORTING
        free_pbt_wps                   = rv_free_work_processes
      EXCEPTIONS
        invalid_group_name             = 1
        internal_error                 = 2
        pbt_env_already_initialized    = 3
        currently_no_resources_avail   = 4
        no_pbt_resources_found         = 5
        cant_init_different_pbt_groups = 6
        OTHERS                         = 7.
    " If SPBT_INITIALIZE fails, check transactions RZ12, SM50, SM21, SARFC

  ENDMETHOD.
  METHOD is_system_changes_allowed.

    DATA:
      lv_systemedit         TYPE tadir-edtflag,
      lv_sys_cliinddep_edit TYPE t000-ccnocliind,
      lv_is_shadow          TYPE abap_bool,
      ls_upginfo            TYPE uvers,
      lv_is_upgrade         TYPE abap_bool.

    CALL FUNCTION 'TR_SYS_PARAMS'
      IMPORTING
        systemedit         = lv_systemedit
        sys_cliinddep_edit = lv_sys_cliinddep_edit
      EXCEPTIONS
        no_systemname      = 1
        no_systemtype      = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      " Assume system can't be changed
      RETURN.
    ENDIF.

    CALL FUNCTION 'UPG_IS_SHADOW_SYSTEM'
      IMPORTING
        ev_shadow = lv_is_shadow.

    CALL FUNCTION 'UPG_GET_ACTIVE_COMP_UPGRADE'
      EXPORTING
        iv_component = 'SAP_BASIS'
        iv_upgtype   = 'A'
        iv_buffered  = abap_false
      IMPORTING
        ev_upginfo   = ls_upginfo
      EXCEPTIONS
        OTHERS       = 4.
    IF sy-subrc = 0 AND ls_upginfo-putstatus NA 'ITU'.
      lv_is_upgrade = abap_true.
    ENDIF.

    " SAP system has status 'not modifiable' (TK 102)
    " Changes to repository objects are not permitted in this client (TK 729)
    " Shadow system
    " Running upgrade
    rv_result = boolc(
      lv_systemedit <> 'N' AND
      lv_sys_cliinddep_edit NA '23' AND
      lv_is_shadow <> abap_true AND
      lv_is_upgrade <> abap_true ).

  ENDMETHOD.
  METHOD Lif_abapgit_environment~compare_with_inactive.
    rv_result = Lif_abapgit_environment~is_sap_cloud_platform( ).
  ENDMETHOD.
  METHOD Lif_abapgit_environment~get_basis_release.

    SELECT SINGLE release extrelease FROM cvers INTO (rs_result-release, rs_result-sp)
      WHERE component = 'SAP_BASIS' ##SUBRC_OK.

  ENDMETHOD.
  METHOD Lif_abapgit_environment~get_system_language_filter.
    DATA lv_translation_detective_lang TYPE spras.
    DATA lv_pseudo_translation_language TYPE spras.
    FIELD-SYMBOLS <ls_system_language_filter> LIKE LINE OF rt_system_language_filter.

    " Translation Object Detective
    " https://help.sap.com/docs/ABAP_PLATFORM_NEW/ceb25152cb0d4adba664cebea2bf4670/88a3d3cbccf64601975acabaccdfde45.html
    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
      EXPORTING
        input            = '1Q'
      IMPORTING
        output           = lv_translation_detective_lang
      EXCEPTIONS
        unknown_language = 1
        OTHERS           = 2.
    IF sy-subrc = 1.
      " The language for Translation Object Detective was not setup
    ENDIF.
    IF NOT lv_translation_detective_lang IS INITIAL.
      APPEND INITIAL LINE TO rt_system_language_filter ASSIGNING <ls_system_language_filter>.
      <ls_system_language_filter>-sign = 'E'.
      <ls_system_language_filter>-option = 'EQ'.
      <ls_system_language_filter>-low = lv_translation_detective_lang.
    ENDIF.
    " 1943470 - Using technical language key 2Q to create pseudo-translations of ABAP developments
    " https://launchpad.support.sap.com/#/notes/1943470
    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
      EXPORTING
        input            = '2Q'
      IMPORTING
        output           = lv_pseudo_translation_language
      EXCEPTIONS
        unknown_language = 1
        OTHERS           = 2.
    IF sy-subrc = 1.
      " The language for Pseudo Translation was not setup
    ENDIF.
    IF NOT lv_pseudo_translation_language IS INITIAL.
      APPEND INITIAL LINE TO rt_system_language_filter ASSIGNING <ls_system_language_filter>.
      <ls_system_language_filter>-sign = 'E'.
      <ls_system_language_filter>-option = 'EQ'.
      <ls_system_language_filter>-low = lv_pseudo_translation_language.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_environment~is_merged.
    DATA lr_marker TYPE REF TO data ##NEEDED.

    IF mv_is_merged = abap_undefined.
      TRY.
          CREATE DATA lr_marker TYPE REF TO ('LIF_ABAPMERGE_MARKER').
          "No exception --> marker found
          mv_is_merged = abap_true.

        CATCH cx_sy_create_data_error.
          mv_is_merged = abap_false.
      ENDTRY.
    ENDIF.
    rv_result = mv_is_merged.
  ENDMETHOD.
  METHOD Lif_abapgit_environment~is_repo_object_changes_allowed.
    IF mv_modifiable = abap_undefined.
      mv_modifiable = is_system_changes_allowed( ).
    ENDIF.
    rv_result = mv_modifiable.
  ENDMETHOD.
  METHOD Lif_abapgit_environment~is_restart_required.
    " This method will be used in the context of SAP Cloud Platform:
    " Pull/Push operations are executed in background jobs.
    " In case of the respective application server needs to be restarted,
    " it is required to terminate the background job and reschedule again.
    rv_result = abap_false.
    TRY.
        CALL METHOD ('CL_APJ_SCP_TOOLS')=>('IS_RESTART_REQUIRED')
          RECEIVING
            restart_required = rv_result.
      CATCH cx_sy_dyn_call_illegal_method cx_sy_dyn_call_illegal_class.
        rv_result = abap_false.
    ENDTRY.
  ENDMETHOD.
  METHOD Lif_abapgit_environment~is_sap_cloud_platform.
    IF mv_cloud = abap_undefined.
      TRY.
          CALL METHOD ('CL_COS_UTILITIES')=>('IS_SAP_CLOUD_PLATFORM')
            RECEIVING
              rv_is_sap_cloud_platform = mv_cloud.
        CATCH cx_sy_dyn_call_error.
          mv_cloud = abap_false.
      ENDTRY.
    ENDIF.
    rv_result = mv_cloud.
  ENDMETHOD.
  METHOD Lif_abapgit_environment~is_sap_object_allowed.

    rv_allowed = cl_enh_badi_def_utility=>is_sap_system( ).
    IF rv_allowed = abap_true.
      RETURN.
    ENDIF.

    rv_allowed = Lcl_abapgit_exit=>get_instance( )->allow_sap_objects( ).

  ENDMETHOD.
  METHOD Lif_abapgit_environment~is_variant_maintenance.

    DATA:
      lt_variscreens TYPE STANDARD TABLE OF rsdynnr
                          WITH NON-UNIQUE DEFAULT KEY.

    " Memory is set in LSVARF08 / EXPORT_SCREEN_TABLES.
    IMPORT variscreens = lt_variscreens FROM MEMORY ID '%_SCRNR_%'.

    rv_is_variant_maintenance = boolc( lines( lt_variscreens ) > 0 ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ENVIRONMENT implementation

*>>>>>>> ZCL_ABAPGIT_EXIT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_exit==============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_exit==============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_exit==============ccau.
*"* use this source file for your ABAP unit test classes

*CLASS SHRITEFUH64VYIPN5I4UHL45BCIR4A DEFINITION DEFERRED.
*CLASS zcl_abapgit_exit DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BCIR4A.

" The class name SHRITEFUH64VYIPN5I4UHL45BCIR4A is hardcoded in zcl_abapgit_exit=>is_running_in_test_context



class LCL_ABAPGIT_EXIT implementation.
*"* method's implementations
*include methods.
  METHOD get_instance.

    DATA lv_class_name TYPE string.

    IF gi_global_exit IS NOT INITIAL.
      ri_exit = gi_global_exit.
      RETURN.
    ENDIF.

    lv_class_name = 'LCL_ABAPGIT_USER_EXIT'.

    IF Lcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_true.
      " Prevent accidental usage of exit handlers in the developer version
      lv_class_name = |\\PROGRAM={ sy-repid }\\CLASS={ lv_class_name }|.
    ENDIF.

    " Prevent non-mocked exit calls in unit tests
    IF is_running_in_test_context( ) = abap_false.
      TRY.
          CREATE OBJECT gi_exit TYPE (lv_class_name).
        CATCH cx_sy_create_object_error ##NO_HANDLER.
      ENDTRY.
    ENDIF.

    CREATE OBJECT gi_global_exit TYPE Lcl_abapgit_exit. " this class

    ri_exit = gi_global_exit.

  ENDMETHOD.
  METHOD is_running_in_test_context.

    IF sy-sysid = 'ABC'.
      " always run on open-abap
      rv_running_in_test_context = abap_true.
      RETURN.
    ENDIF.

    " Check if the local test class can be accessed by RTTI. If so the current process is running in a unit test.
    " Note this approach only works for the developer version. The standalone version will always report not running in
    " test context which should be fine as there are no unit tests delivered in it.
    cl_abap_typedescr=>describe_by_name(
      EXPORTING
        p_name         = |\\PROGRAM={ sy-repid }\\CLASS=SHRITEFUH64VYIPN5I4UHL45BCIR4A|
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2 ).
    rv_running_in_test_context = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_exit~adjust_display_commit_url.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->adjust_display_commit_url(
            EXPORTING
              iv_repo_url    = iv_repo_url
              iv_repo_name   = iv_repo_name
              iv_repo_key    = iv_repo_key
              iv_commit_hash = iv_commit_hash
            CHANGING
              cv_display_url = cv_display_url ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~adjust_display_filename.

    IF gi_exit IS NOT INITIAL.
      TRY.
          rv_filename = gi_exit->adjust_display_filename(
            is_repo_meta = is_repo_meta
            iv_filename  = iv_filename ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

    IF rv_filename IS INITIAL.
      rv_filename = iv_filename.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~allow_sap_objects.

    IF gi_exit IS NOT INITIAL.
      TRY.
          rv_allowed = gi_exit->allow_sap_objects( ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~change_local_host.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_local_host( CHANGING ct_hosts = ct_hosts ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~change_max_parallel_processes.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_max_parallel_processes(
            EXPORTING
              iv_package       = iv_package
            CHANGING
              cv_max_processes = cv_max_processes ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~change_proxy_authentication.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_proxy_authentication(
            EXPORTING
              iv_repo_url             = iv_repo_url
            CHANGING
              cv_proxy_authentication = cv_proxy_authentication ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~change_proxy_port.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_proxy_port(
            EXPORTING
              iv_repo_url   = iv_repo_url
            CHANGING
              cv_proxy_port = cv_proxy_port ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~change_proxy_url.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_proxy_url(
            EXPORTING
              iv_repo_url  = iv_repo_url
            CHANGING
              cv_proxy_url = cv_proxy_url ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~change_rfc_server_group.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_rfc_server_group( CHANGING cv_group = cv_group ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~change_supported_data_objects.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_supported_data_objects( CHANGING ct_objects = ct_objects ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~change_supported_object_types.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_supported_object_types( CHANGING ct_types = ct_types ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~change_tadir.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->change_tadir(
            EXPORTING
              iv_package = iv_package
              ii_log     = ii_log
            CHANGING
              ct_tadir   = ct_tadir ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~create_http_client.

    IF gi_exit IS NOT INITIAL.
      TRY.
          ri_client = gi_exit->create_http_client( iv_url ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~custom_serialize_abap_clif.

    " This exit might be called twice per object
    " 1st call: it_source = initial
    "    Can be used for serializing complete source
    "    If source is returned, there will be no second call
    " 2nd call: it_source = code as serialized by abapGit
    "    Can be used for post-processing of source
    IF gi_exit IS NOT INITIAL.
      TRY.
          rt_source = gi_exit->custom_serialize_abap_clif(
            is_class_key = is_class_key
            it_source    = it_source ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

    IF rt_source IS INITIAL.
      rt_source = it_source.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~deserialize_postprocess.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->deserialize_postprocess( is_step = is_step
                                            ii_log  = ii_log ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~determine_transport_request.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->determine_transport_request(
            EXPORTING
              io_repo              = io_repo
              iv_transport_type    = iv_transport_type
            CHANGING
              cv_transport_request = cv_transport_request ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~enhance_repo_toolbar.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->enhance_repo_toolbar(
            io_menu = io_menu
            iv_key  = iv_key
            iv_act  = iv_act ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~get_ci_tests.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->get_ci_tests(
            EXPORTING
              iv_object   = iv_object
            CHANGING
              ct_ci_repos = ct_ci_repos ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~get_ssl_id.

    IF gi_exit IS NOT INITIAL.
      TRY.
          rv_ssl_id = gi_exit->get_ssl_id( ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

    IF rv_ssl_id IS INITIAL.
      rv_ssl_id = 'ANONYM'.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~http_client.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->http_client(
            iv_url    = iv_url
            ii_client = ii_client ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~on_event.

    IF gi_exit IS NOT INITIAL.
      TRY.
          rs_handled = gi_exit->on_event( ii_event ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~pre_calculate_repo_status.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->pre_calculate_repo_status(
            EXPORTING
              is_repo_meta = is_repo_meta
            CHANGING
              ct_local     = ct_local
              ct_remote    = ct_remote ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~serialize_postprocess.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->serialize_postprocess(
            EXPORTING
              iv_package = iv_package
              ii_log     = ii_log
            CHANGING
              ct_files   = ct_files ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~validate_before_push.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->validate_before_push(
            is_comment = is_comment
            io_stage   = io_stage
            io_repo    = io_repo ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~wall_message_list.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->wall_message_list( ii_html ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_exit~wall_message_repo.

    IF gi_exit IS NOT INITIAL.
      TRY.
          gi_exit->wall_message_repo(
            is_repo_meta = is_repo_meta
            ii_html      = ii_html ).
        CATCH cx_sy_ref_is_initial cx_sy_dyn_call_illegal_method ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_EXIT implementation

*>>>>>>> ZCL_ABAPGIT_FACTORY <<<<<<<*

*"* macro definitions
*include zcl_abapgit_factory===========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_factory===========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_FACTORY implementation.
*"* method's implementations
*include methods.
  METHOD get_code_inspector.

    DATA: ls_code_inspector LIKE LINE OF gt_code_inspector.
    FIELD-SYMBOLS: <ls_code_inspector> TYPE ty_code_inspector_pack.

    READ TABLE gt_code_inspector ASSIGNING <ls_code_inspector>
      WITH TABLE KEY package = iv_package.
    IF sy-subrc <> 0.
      ls_code_inspector-package = iv_package.

      CREATE OBJECT ls_code_inspector-instance TYPE Lcl_abapgit_code_inspector
        EXPORTING
          iv_package = iv_package.

      INSERT ls_code_inspector
             INTO TABLE gt_code_inspector
             ASSIGNING <ls_code_inspector>.

    ENDIF.

    ri_code_inspector = <ls_code_inspector>-instance.

  ENDMETHOD.
  METHOD get_cts_api.
    IF gi_cts_api IS NOT BOUND.
      CREATE OBJECT gi_cts_api TYPE Lcl_abapgit_cts_api.
    ENDIF.

    ri_cts_api = gi_cts_api.
  ENDMETHOD.
  METHOD get_environment.
    IF gi_environment IS NOT BOUND.
      CREATE OBJECT gi_environment TYPE Lcl_abapgit_environment.
    ENDIF.
    ri_environment = gi_environment.
  ENDMETHOD.
  METHOD get_http_agent.

    IF gi_http_agent IS INITIAL.
      gi_http_agent = Lcl_abapgit_http_agent=>create( ).
    ENDIF.

    ri_http_agent = gi_http_agent.

  ENDMETHOD.
  METHOD get_longtexts.

    IF gi_longtext IS NOT BOUND.
      CREATE OBJECT gi_longtext TYPE Lcl_abapgit_longtexts.
    ENDIF.
    ri_longtexts = gi_longtext.

  ENDMETHOD.
  METHOD get_lxe_texts.

    IF gi_lxe_texts IS NOT BOUND.
      CREATE OBJECT gi_lxe_texts TYPE Lcl_abapgit_lxe_texts.
    ENDIF.
    ri_lxe_texts = gi_lxe_texts.

  ENDMETHOD.
  METHOD get_sap_namespace.

    IF gi_sap_namespace IS NOT BOUND.
      CREATE OBJECT gi_sap_namespace TYPE Lcl_abapgit_sap_namespace.
    ENDIF.

    ri_namespace = gi_sap_namespace.

  ENDMETHOD.
  METHOD get_sap_package.

    DATA: ls_sap_package TYPE ty_sap_package.
    FIELD-SYMBOLS: <ls_sap_package> TYPE ty_sap_package.

    READ TABLE gt_sap_package ASSIGNING <ls_sap_package>
                              WITH TABLE KEY package = iv_package.
    IF sy-subrc <> 0.

      ls_sap_package-package = iv_package.
      CREATE OBJECT ls_sap_package-instance TYPE Lcl_abapgit_sap_package
        EXPORTING
          iv_package = iv_package.

      INSERT ls_sap_package
             INTO TABLE gt_sap_package
             ASSIGNING <ls_sap_package>.

    ENDIF.

    ri_sap_package = <ls_sap_package>-instance.

  ENDMETHOD.
  METHOD get_stage_logic.

    IF gi_stage_logic IS INITIAL.
      CREATE OBJECT gi_stage_logic TYPE Lcl_abapgit_stage_logic.
    ENDIF.

    ri_logic = gi_stage_logic.

  ENDMETHOD.
  METHOD get_tadir.

    IF gi_tadir IS INITIAL.
      CREATE OBJECT gi_tadir TYPE Lcl_abapgit_tadir.
    ENDIF.

    ri_tadir = gi_tadir.

  ENDMETHOD.
  METHOD get_sap_report.

    IF gi_sap_report IS NOT BOUND.
      CREATE OBJECT gi_sap_report TYPE Lcl_abapgit_sap_report.
    ENDIF.

    ri_report = gi_sap_report.

  ENDMETHOD.
  METHOD get_function_module.

    IF gi_function_module IS INITIAL.
      CREATE OBJECT gi_function_module TYPE Lcl_abapgit_function_module.
    ENDIF.

    ri_function_module = gi_function_module.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_FACTORY implementation

*>>>>>>> ZCL_ABAPGIT_GUI_CHUNK_LIB <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_chunk_lib=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_chunk_lib=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_gui_chunk_lib=====ccau.
*"* use this source file for your ABAP unit test classes







*CLASS zcl_abapgit_gui_chunk_lib DEFINITION
*  LOCAL FRIENDS SHRITEFUH64VYIPN5I4UHL45BHXR4A
*                SHRITEFUH64VYIPN5I4UHL45BH2R4A.









class LCL_ABAPGIT_GUI_CHUNK_LIB implementation.
*"* method's implementations
*include methods.
  METHOD class_constructor.

    DATA lv_fm TYPE string.
    lv_fm = 'GET_SYSTEM_TIMEZONE'.

    TRY.
        CALL METHOD ('CL_ABAP_TSTMP')=>get_system_timezone
          RECEIVING
            system_timezone = gv_time_zone.
      CATCH cx_sy_dyn_call_illegal_method.
        CALL FUNCTION lv_fm
          IMPORTING
            timezone            = gv_time_zone
          EXCEPTIONS
            customizing_missing = 1
            OTHERS              = 2.
    ENDTRY.

  ENDMETHOD.
  METHOD get_t100_text.

    MESSAGE ID iv_msgid TYPE 'S' NUMBER iv_msgno WITH '&1' '&2' '&3' '&4' INTO rv_text.

    " Don't return any generic messages like `&1 &2 &3 &4`
    IF rv_text CO ' 0123456789&'.
      CLEAR rv_text.
    ENDIF.

  ENDMETHOD.
  METHOD normalize_program_name.

    rv_normalized_program_name = substring_before(
      val   = iv_program_name
      regex = `(=+CP)?$` ).

  ENDMETHOD.
  METHOD render_branch_name.

    DATA:
      lv_key              TYPE string,
      lv_branch           TYPE string,
      lv_selected_commit  TYPE string,
      lv_commit_short_sha TYPE string,
      lv_text             TYPE string,
      lv_icon             TYPE string,
      lv_hint             TYPE string,
      lv_class            TYPE string.

    IF iv_repo_key IS NOT INITIAL.
      lv_key = iv_repo_key.
    ELSEIF io_repo IS BOUND.
      lv_key = io_repo->get_key( ).
    ELSE.
      Lcx_abapgit_exception=>raise( 'Either iv_repo_key or io_repo must be supplied' ).
    ENDIF.

    IF iv_branch IS NOT INITIAL.
      lv_branch = iv_branch.
      lv_text = Lcl_abapgit_git_branch_list=>get_display_name( lv_branch ).
    ELSEIF io_repo IS BOUND.
      lv_selected_commit = io_repo->get_selected_commit( ).
      IF lv_selected_commit IS NOT INITIAL.
        "Convert to short commit. Example: (ae623b9...)
        lv_commit_short_sha = lv_selected_commit+0(7).
        lv_text = |({ lv_commit_short_sha }...)|.
      ELSE.
        lv_branch = io_repo->get_selected_branch( ).
        lv_text = Lcl_abapgit_git_branch_list=>get_display_name( lv_branch ).
      ENDIF.
    ELSE.
      Lcx_abapgit_exception=>raise( 'Either iv_branch or io_repo must be supplied' ).
    ENDIF.

    CASE Lcl_abapgit_git_branch_list=>get_type( lv_branch ).
      WHEN Lif_abapgit_git_definitions=>c_git_branch_type-branch.
        lv_class = 'branch branch_branch'.
        lv_icon  = 'code-branch/grey70'.
        lv_hint  = 'Current branch'.
      WHEN Lif_abapgit_git_definitions=>c_git_branch_type-annotated_tag
        OR Lif_abapgit_git_definitions=>c_git_branch_type-lightweight_tag.
        lv_class = 'branch'.
        lv_icon  = 'tag-solid/grey70'.
        lv_hint  = 'Current tag'.
      WHEN OTHERS.
        lv_class = 'branch branch_branch'.
        lv_icon  = 'code-branch/grey70'.
        lv_hint  = 'Current commit'.
    ENDCASE.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.
    ri_html->add( |<span class="{ lv_class }">| ).
    ri_html->add_icon( iv_name = lv_icon
                       iv_hint = lv_hint ).
    IF iv_interactive = abap_true.
      ri_html->add_a( iv_act = |{ Lif_abapgit_definitions=>c_action-git_branch_switch }?key={ lv_key }|
                      iv_txt = lv_text ).
    ELSE.
      ri_html->add( lv_text ).
    ENDIF.
    ri_html->add( '</span>' ).

  ENDMETHOD.
  METHOD render_error.

    DATA lv_error TYPE string.
    DATA lv_class TYPE string VALUE 'panel error center'.

    IF iv_extra_style IS NOT INITIAL.
      lv_class = lv_class && ` ` && iv_extra_style.
    ENDIF.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    IF ix_error IS BOUND.
      lv_error = ix_error->get_text( ).
    ELSE.
      lv_error = iv_error.
    ENDIF.

    ri_html->add( |<div class="{ lv_class }">| ).
    ri_html->add( |{ ri_html->icon( 'exclamation-circle/red' ) } { lv_error }| ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
  METHOD render_error_message_box.

    DATA:
      lv_error_text          TYPE string,
      lv_longtext            TYPE string,
      lt_longtext_paragraphs TYPE string_table,
      lv_program_name        TYPE sy-repid,
      lv_title               TYPE string,
      lv_text                TYPE string.
    FIELD-SYMBOLS:
      <lv_longtext_paragraph> TYPE string.


    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    lv_error_text = ix_error->get_text( ).
    lv_longtext = ix_error->if_message~get_longtext( abap_true ).

    IF lv_longtext IS NOT INITIAL.
      lv_error_text = |{ lv_error_text } <span class="emphasis">More...</span>|.

      REPLACE FIRST OCCURRENCE OF REGEX
        |({ Lcx_abapgit_exception=>c_section_text-cause }{ cl_abap_char_utilities=>newline })|
        IN lv_longtext WITH |<h3>$1</h3>|.

      REPLACE FIRST OCCURRENCE OF REGEX
        |({ Lcx_abapgit_exception=>c_section_text-system_response }{ cl_abap_char_utilities=>newline })|
        IN lv_longtext WITH |<h3>$1</h3>|.

      REPLACE FIRST OCCURRENCE OF REGEX
        |({ Lcx_abapgit_exception=>c_section_text-what_to_do }{ cl_abap_char_utilities=>newline })|
        IN lv_longtext WITH |<h3>$1</h3>|.

      REPLACE FIRST OCCURRENCE OF REGEX
        |({ Lcx_abapgit_exception=>c_section_text-sys_admin }{ cl_abap_char_utilities=>newline })|
        IN lv_longtext WITH |<h3>$1</h3>|.

      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
        IN lv_longtext
        WITH cl_abap_char_utilities=>newline.

      SPLIT lv_longtext AT cl_abap_char_utilities=>newline INTO TABLE lt_longtext_paragraphs.
      CLEAR lv_longtext.

      LOOP AT lt_longtext_paragraphs ASSIGNING <lv_longtext_paragraph>.
        CONDENSE <lv_longtext_paragraph>.

        IF <lv_longtext_paragraph> IS INITIAL.
          CONTINUE.
        ENDIF.

        lv_longtext = |{ lv_longtext }<p>{ <lv_longtext_paragraph> }</p>{ cl_abap_char_utilities=>newline }|.
      ENDLOOP.
    ENDIF.

    ri_html->add( |<div id="message" class="message-panel">| ).
    ri_html->add( |{ ri_html->icon( 'exclamation-circle/red' ) } { lv_error_text }| ).
    ri_html->add( |<div class="message-panel-bar">| ).

    ri_html->add_a(
      iv_txt   = `&#x274c;`
      iv_act   = `toggleDisplay('message')`
      iv_class = `close-btn`
      iv_typ   = Lif_abapgit_html=>c_action_type-onclick ).

    ri_html->add( |</div>| ).

    ri_html->add( |<div class="message-panel-bar message-panel-commands">| ).

    IF ix_error->if_t100_message~t100key-msgid IS NOT INITIAL.

      lv_title = get_t100_text(
        iv_msgid = ix_error->if_t100_message~t100key-msgid
        iv_msgno = ix_error->if_t100_message~t100key-msgno ).

      IF lv_title IS NOT INITIAL.
        lv_text = |Message ({ ix_error->if_t100_message~t100key-msgid }/{ ix_error->if_t100_message~t100key-msgno })|.

        ri_html->add_a(
          iv_txt   = lv_text
          iv_typ   = Lif_abapgit_html=>c_action_type-sapevent
          iv_act   = Lif_abapgit_definitions=>c_action-goto_message
          iv_title = lv_title
          iv_id    = `a_goto_message` ).
      ENDIF.
    ENDIF.

    ix_error->get_source_position( IMPORTING program_name = lv_program_name ).

    lv_title = normalize_program_name( lv_program_name ).

    ri_html->add_a(
      iv_txt   = `Goto source`
      iv_act   = Lif_abapgit_definitions=>c_action-goto_source
      iv_typ   = Lif_abapgit_html=>c_action_type-sapevent
      iv_title = lv_title
      iv_id    = `a_goto_source` ).

    ri_html->add_a(
      iv_txt = `Callstack`
      iv_act = Lif_abapgit_definitions=>c_action-show_callstack
      iv_typ = Lif_abapgit_html=>c_action_type-sapevent
      iv_id  = `a_callstack` ).

    ri_html->add( |</div>| ).
    ri_html->add( |<div class="message-panel-commands">| ).
    ri_html->add( |{ lv_longtext }| ).
    ri_html->add( |</div>| ).
    ri_html->add( |</div>| ).

  ENDMETHOD.
  METHOD render_event_as_form.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add(
      |<form id="form_{ is_event-name }" method="{ is_event-method }" action="sapevent:{ is_event-name }"></form>| ).

  ENDMETHOD.
  METHOD render_help_hint.

    " TODO potentially move to or integrate with zcl_abapgit_html_form

    DATA lt_fragments TYPE string_table.
    DATA li_html TYPE REF TO Lif_abapgit_html.
    li_html = Lcl_abapgit_html=>create( ).

    APPEND `<div class="form-field-help-tooltip">` TO lt_fragments.
    APPEND li_html->icon(
      iv_name = 'question-circle-solid'
      iv_class = 'blue' ) TO lt_fragments.
    APPEND `<div class="form-field-help-tooltip-text">` TO lt_fragments.
    APPEND iv_text_to_wrap TO lt_fragments.
    APPEND `</div>` TO lt_fragments.
    APPEND `</div>` TO lt_fragments.

    rv_html = concat_lines_of( lt_fragments ).

  ENDMETHOD.
  METHOD render_infopanel.

    DATA lv_display TYPE string.
    DATA lv_class TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    IF iv_hide = abap_true. " Initially hide
      lv_display = 'display:none'.
    ENDIF.

    lv_class = 'info-panel'.
    IF iv_scrollable = abap_false. " Initially hide
      lv_class = lv_class && ' info-panel-fixed'.
    ENDIF.

    ri_html->add( |<div id="{ iv_div_id }" class="{ lv_class }" style="{ lv_display }">| ).

    ri_html->add( |<div class="info-title">{ iv_title }|
               && '<div class="float-right">'
               && ri_html->a(
                    iv_txt   = '&#x274c;'
                    iv_typ   = Lif_abapgit_html=>c_action_type-onclick
                    iv_act   = |toggleDisplay('{ iv_div_id }')|
                    iv_class = 'close-btn' )
               && '</div></div>' ).

    IF iv_hint IS NOT INITIAL.
      ri_html->add( '<div class="info-hint">'
        && ri_html->icon( iv_name = 'exclamation-triangle'
                          iv_class = 'pad-right' )
        && iv_hint
        && '</div>' ).
    ENDIF.

    ri_html->add( |<div class="info-list">| ).
    ri_html->add( io_content ).
    ri_html->add( '</div>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
  METHOD render_item_state.

    DATA: lv_system TYPE string.

    FIELD-SYMBOLS <lv_state> TYPE char1.


    rv_html = '<span class="state-block">'.

    DO 2 TIMES.
      CASE sy-index.
        WHEN 1.
          ASSIGN iv_lstate TO <lv_state>.
          lv_system = 'Local:'.
        WHEN 2.
          ASSIGN iv_rstate TO <lv_state>.
          lv_system = 'Remote:'.
      ENDCASE.

      CASE <lv_state>.
        WHEN Lif_abapgit_definitions=>c_state-unchanged.  "None or unchanged
          IF iv_lstate = Lif_abapgit_definitions=>c_state-added OR iv_rstate = Lif_abapgit_definitions=>c_state-added.
            rv_html = rv_html && |<span class="none" title="{ lv_system } Not exists">X</span>|.
          ELSE.
            rv_html = rv_html && |<span class="none" title="{ lv_system } No changes">&nbsp;</span>|.
          ENDIF.
        WHEN Lif_abapgit_definitions=>c_state-modified.   "Changed
          rv_html = rv_html && |<span class="changed" title="{ lv_system } Modified">M</span>|.
        WHEN Lif_abapgit_definitions=>c_state-added.      "Added new
          rv_html = rv_html && |<span class="added" title="{ lv_system } Added new">A</span>|.
        WHEN Lif_abapgit_definitions=>c_state-mixed.      "Multiple changes (multifile)
          rv_html = rv_html && |<span class="mixed" title="{ lv_system } Multiple changes">&#x25A0;</span>|.
        WHEN Lif_abapgit_definitions=>c_state-deleted.    "Deleted
          rv_html = rv_html && |<span class="deleted" title="{ lv_system } Deleted">D</span>|.
      ENDCASE.
    ENDDO.

    rv_html = rv_html && '</span>'.

  ENDMETHOD.
  METHOD render_js_error_banner.
    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.
    ri_html->add( '<div id="js-error-banner" class="dummydiv error">' ).
    ri_html->add( |{ ri_html->icon( 'exclamation-triangle/red' ) }| &&
                  ' If this does not disappear soon,' &&
                  ' then there is a JS init error, please log an issue' ).
    ri_html->add( '</div>' ).
  ENDMETHOD.
  METHOD render_label_list.

    DATA lt_fragments TYPE string_table.
    DATA lv_l TYPE string.
    DATA lv_class TYPE string.
    DATA lv_style TYPE string.
    DATA ls_parsed_color TYPE Lcl_abapgit_repo_labels=>ty_color.
    DATA li_html TYPE REF TO Lif_abapgit_html.

    IF it_labels IS INITIAL.
      RETURN.
    ENDIF.

    li_html = Lcl_abapgit_html=>create( ).

    APPEND `<ul class="repo-labels">` TO lt_fragments.

    LOOP AT it_labels INTO lv_l WHERE table_line IS NOT INITIAL.
      CLEAR lv_class.
      CLEAR lv_style.
      ls_parsed_color = Lcl_abapgit_repo_labels=>parse_color( io_label_colors->get( lv_l ) ).
      IF ls_parsed_color-cls IS NOT INITIAL.
        lv_class = | class="rl-{ ls_parsed_color-cls }"|.
      ELSEIF ls_parsed_color-fg IS NOT INITIAL OR ls_parsed_color-bg IS NOT INITIAL.
        lv_style = ` style="`.
        IF ls_parsed_color-fg IS NOT INITIAL.
          lv_style = lv_style && |color:#{ ls_parsed_color-fg };|.
        ENDIF.
        IF ls_parsed_color-bg IS NOT INITIAL.
          lv_style = lv_style && |background-color:#{ ls_parsed_color-bg };|.
          IF ls_parsed_color-border IS INITIAL.
            lv_style = lv_style && |border-color:#{ ls_parsed_color-bg };|.
          ELSE.
            lv_style = lv_style && |border-color:#{ ls_parsed_color-border };|.
          ENDIF.
        ENDIF.
        lv_style = lv_style && `"`.
      ENDIF.

      IF iv_clickable_action IS NOT INITIAL.
        lv_l = li_html->a(
          iv_txt = lv_l
          iv_act = |{ iv_clickable_action }|
          iv_class = 'command'
          iv_query = lv_l ).
      ENDIF.
      lv_l = |<li{ lv_class }{ lv_style }>{ lv_l }</li>|.
      APPEND lv_l TO lt_fragments.
    ENDLOOP.

    APPEND `</ul>` TO lt_fragments.

    rv_html = concat_lines_of( lt_fragments ).

  ENDMETHOD.
  METHOD render_news.

    DATA: lv_text TYPE string,
          lv_hint TYPE string,
          lv_ul   TYPE abap_bool,
          lt_log  TYPE Lcl_abapgit_news=>ty_logs.

    FIELD-SYMBOLS: <ls_line> LIKE LINE OF lt_log.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    IF io_news IS NOT BOUND OR io_news->has_news( ) = abap_false.
      RETURN.
    ENDIF.

    lt_log = io_news->get_log( ).

    " Render news
    LOOP AT lt_log ASSIGNING <ls_line>.
      IF <ls_line>-is_header = abap_true.
        IF <ls_line>-pos_to_cur > 0.
          lv_text = <ls_line>-text && '<span class="version-marker update">update</span>'.
        ELSEIF <ls_line>-pos_to_cur = 0.
          lv_text = <ls_line>-text && '<span class="version-marker">current</span>'.
        ELSE. " < 0
          lv_text = <ls_line>-text.
        ENDIF.
        IF lv_ul = abap_true.
          ri_html->add( |</ul>| ).
        ENDIF.
        ri_html->add( |<h1>{ lv_text }</h1>| ).
        ri_html->add( |<ul>| ).
        lv_ul = abap_true.
      ELSE.
        <ls_line>-text = escape( val    = <ls_line>-text
                                 format = cl_abap_format=>e_html_text ).
        ri_html->add( |<li>{ <ls_line>-text }</li>| ).
      ENDIF.
    ENDLOOP.
    IF lv_ul = abap_true.
      ri_html->add( |</ul>| ).
    ENDIF.

    " Wrap
    IF io_news->has_important( ) = abap_true.
      lv_hint = 'Please note changes marked with "!"'.
    ENDIF.

    ri_html = render_infopanel(
      iv_div_id  = 'news'
      iv_title   = 'Announcement of the latest changes'
      iv_hint    = lv_hint
      iv_hide    = boolc( io_news->has_unseen( ) = abap_false )
      io_content = ri_html ).

  ENDMETHOD.
  METHOD render_package_name.

    DATA:
      lv_obj_name TYPE tadir-obj_name,
      lv_jump     TYPE string,
      lv_title    TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    IF iv_package IS INITIAL.
      RETURN.
    ENDIF.

    IF iv_suppress_title = abap_false.
      lv_title = Lcl_abapgit_factory=>get_sap_package( iv_package )->read_description( ).
    ENDIF.

    lv_obj_name = iv_package.
    lv_jump = Lcl_abapgit_html_action_utils=>jump_encode(
      iv_obj_type = 'DEVC'
      iv_obj_name = lv_obj_name ).

    ri_html->add( |<span class="package-box">| ).
    ri_html->add_icon( iv_name = 'box/grey70'
                       iv_hint = 'SAP package' ).
    IF iv_interactive = abap_true.
      ri_html->add_a( iv_act   = |{ Lif_abapgit_definitions=>c_action-jump }?{ lv_jump }|
                      iv_title = lv_title
                      iv_txt   = |{ iv_package }| ).
    ELSE.
      ri_html->add( iv_package ).
    ENDIF.
    ri_html->add( '</span>' ).

  ENDMETHOD.
  METHOD render_path.

    DATA:
      lv_path    TYPE string,
      lv_jump    TYPE string,
      lv_folder  TYPE string,
      lt_folders TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    IF iv_path IS INITIAL.
      RETURN.
    ENDIF.

    lv_jump = |{ Lcl_abapgit_gui_page_repo_view=>c_actions-change_dir }?PATH=|.

    ri_html->add( |<span class="path-box">| ).

    IF iv_interactive = abap_true.
      SPLIT iv_path AT '/' INTO TABLE lt_folders.

      LOOP AT lt_folders INTO lv_folder.
        IF lv_folder IS INITIAL.
          " root
          lv_path = '/'.
        ELSEIF sy-tabix < lines( lt_folders ).
          lv_path = lv_path && lv_folder && '/'.
          ri_html->add_a( iv_act = lv_jump && lv_path
                          iv_txt = lv_folder ).
        ELSE.
          " no link for current folder
          ri_html->add( | <strong>{ lv_folder }</strong> | ).
        ENDIF.
        ri_html->add( '/' ).
      ENDLOOP.
    ELSE.
      ri_html->add( iv_path ).
    ENDIF.

    ri_html->add( '</span>' ).

  ENDMETHOD.
  METHOD render_repo_palette.

    DATA lt_repo_obj_list TYPE Lif_abapgit_repo_srv=>ty_repo_list.
    DATA lt_repo_list TYPE Lif_abapgit_persistence=>ty_repos.
    DATA lv_repo_json TYPE string.
    DATA lv_size TYPE i.
    DATA ls_repo_data LIKE LINE OF lt_repo_list.

    FIELD-SYMBOLS:
      <ls_repo>     LIKE LINE OF lt_repo_list,
      <lr_repo_obj> LIKE LINE OF lt_repo_obj_list.

    lt_repo_obj_list = Lcl_abapgit_repo_srv=>get_instance( )->list( ).

    LOOP AT lt_repo_obj_list ASSIGNING <lr_repo_obj>.
      ls_repo_data = <lr_repo_obj>->ms_data.
      ls_repo_data-local_settings-display_name = <lr_repo_obj>->get_name( ).
      APPEND ls_repo_data TO lt_repo_list.
    ENDLOOP.

    lv_size = lines( lt_repo_list ).
    SORT lt_repo_list BY local_settings-display_name AS TEXT.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( 'var repoCatalog = [' ). " Maybe separate this into another method if needed in more places
    LOOP AT lt_repo_list ASSIGNING <ls_repo>.
      lv_repo_json = |\{ key: "{ <ls_repo>-key
        }", isOffline: "{ <ls_repo>-offline
        }", displayName: "{ escape( val = <ls_repo>-local_settings-display_name
                                    format = cl_abap_format=>e_html_js ) }"  \}|.
      IF sy-tabix < lv_size.
        lv_repo_json = lv_repo_json && ','.
      ENDIF.
      ri_html->add( lv_repo_json ).
    ENDLOOP.
    ri_html->add( '];' ).

    ri_html->add( |var gGoRepoPalette = new CommandPalette(createRepoCatalogEnumerator(repoCatalog, "{
      iv_action }"), \{| ).
    ri_html->add( '  toggleKey: "F2",' ).
    ri_html->add( '  hotkeyDescription: "Go to Repository ..."' ).
    ri_html->add( '});' ).

  ENDMETHOD.
  METHOD render_repo_top.

    DATA: lo_repo_online TYPE REF TO Lcl_abapgit_repo_online,
          lo_pback       TYPE REF TO Lcl_abapgit_persist_background,
          lx_error       TYPE REF TO Lcx_abapgit_exception,
          lv_hint        TYPE string,
          lv_icon        TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.
    CREATE OBJECT lo_pback.

    IF io_repo->is_offline( ) = abap_true.
      lv_icon = 'plug/darkgrey'.
      lv_hint = 'Offline Repository'.
    ELSE.
      lv_icon = 'cloud-upload-alt/blue'.
      lv_hint = 'On-line Repository'.
    ENDIF.

    ri_html->add( '<table class="w100"><tr>' ).

    ri_html->add( '<td class="repo_name">' ).

    " Repo type and name
    ri_html->add_icon( iv_name = lv_icon
                       iv_hint = lv_hint ).
    ri_html->add( |<span class="name">{ io_repo->get_name( ) }</span>| ).
    IF io_repo->is_offline( ) = abap_false.
      lo_repo_online ?= io_repo.

      ri_html->add( render_repo_url( lo_repo_online->get_url( ) ) ).
    ENDIF.

    IF iv_show_edit = abap_true.
      ri_html->add_a( iv_txt   = ri_html->icon( iv_name  = 'edit-solid'
                                                iv_class = 'pad-sides'
                                                iv_hint  = 'Change Remote' )
                      iv_act   = |{ Lif_abapgit_definitions=>c_action-repo_remote_settings }?| &&
                                 |key={ io_repo->get_key( ) }|
                      iv_class = |url| ).
    ENDIF.

    IF io_repo->is_offline( ) = abap_false.
      lo_repo_online ?= io_repo.

      ri_html->add_a( iv_txt   = ri_html->icon( iv_name  = 'copy-solid'
                                                iv_class = 'pad-sides'
                                                iv_hint  = 'Copy URL to Clipboard' )
                      iv_act   = |{ Lif_abapgit_definitions=>c_action-clipboard }| &&
                                 |?clipboard={ lo_repo_online->get_url( ) }|
                      iv_class = |url| ).
    ENDIF.

    IF io_repo->is_offline( ) = abap_false AND iv_show_commit = abap_true.
      TRY.
          render_repo_top_commit_hash( ii_html        = ri_html
                                       io_repo_online = lo_repo_online ).
        CATCH Lcx_abapgit_exception INTO lx_error.
          " In case of missing or wrong credentials, show message in status bar
          lv_hint = lx_error->get_text( ).
          IF lv_hint CS 'credentials'.
            MESSAGE lv_hint TYPE 'S' DISPLAY LIKE 'E'.
          ENDIF.
      ENDTRY.
    ENDIF.

    " News
    IF io_news IS BOUND AND io_news->has_news( ) = abap_true.
      IF io_news->has_updates( ) = abap_true.
        lv_icon = 'arrow-circle-up/warning'.
      ELSE.
        lv_icon = 'arrow-circle-up'.
      ENDIF.
      ri_html->add_a( iv_act   = |toggleDisplay('news')|
                      iv_typ   = Lif_abapgit_html=>c_action_type-onclick
                      iv_txt   = ri_html->icon( iv_name  = lv_icon
                                                iv_class = 'pad-sides'
                                                iv_hint  = 'Display Changelog' )
                      iv_class = |url| ).
    ENDIF.
    ri_html->add( '</td>' ).

    ri_html->add( '<td class="repo_attr right">' ).

    " Fav
    IF abap_true = Lcl_abapgit_persistence_user=>get_instance( )->is_favorite_repo( io_repo->get_key( ) ).
      lv_icon = 'star/blue'.
    ELSE.
      lv_icon = 'star/grey'.
    ENDIF.
    ri_html->add_a( iv_act = |{ Lif_abapgit_definitions=>c_action-repo_toggle_fav }?key={ io_repo->get_key( ) }|
                    iv_txt = ri_html->icon( iv_name  = lv_icon
                                            iv_class = 'pad-sides'
                                            iv_hint  = 'Toggle Favorite' ) ).

    " BG
    IF lo_pback->exists( io_repo->get_key( ) ) = abap_true.
      ri_html->add( '<span class="bg_marker" title="background">BG</span>' ).
    ENDIF.

    " Write protect
    IF io_repo->get_local_settings( )-write_protected = abap_true.
      ri_html->add_icon( iv_name = 'lock/grey70'
                         iv_hint = 'Locked from Pulls' ).
    ENDIF.
    IF io_repo->get_local_settings( )-flow = abap_true.
      ri_html->add_icon( iv_name = 'flow/grey70'
                         iv_hint = 'Flow' ).
    ENDIF.

    " Branch
    IF io_repo->is_offline( ) = abap_false.
      lo_repo_online ?= io_repo.
      IF iv_show_branch = abap_true.
        ri_html->add( render_branch_name( io_repo        = lo_repo_online
                                          iv_interactive = iv_interactive_branch ) ).
      ENDIF.
    ENDIF.

    " Package
    IF iv_show_package = abap_true.
      ri_html->add( render_package_name( io_repo->get_package( ) ) ).
    ENDIF.

    ri_html->add( '</td>' ).
    ri_html->add( '</tr></table>' ).

  ENDMETHOD.
  METHOD render_repo_top_commit_hash.

    DATA: lv_commit_hash       TYPE Lif_abapgit_git_definitions=>ty_sha1,
          lv_commit_short_hash TYPE Lif_abapgit_git_definitions=>ty_sha1,
          lv_display_url       TYPE Lif_abapgit_persistence=>ty_repo-url,
          lo_url               TYPE REF TO Lcl_abapgit_git_url,
          lv_icon_commit       TYPE string.

    lv_commit_hash = io_repo_online->get_current_remote( ).
    lv_commit_short_hash = lv_commit_hash(7).

    lv_icon_commit = ii_html->icon( iv_name  = 'code-commit'
                                    iv_class = 'pad-sides'
                                    iv_hint  = 'Commit' ).

    CREATE OBJECT lo_url.

    TRY.
        lv_display_url = lo_url->get_commit_display_url( io_repo_online ).

        ii_html->add_a( iv_txt   = |{ lv_icon_commit }{ lv_commit_short_hash }|
                        iv_act   = |{ Lif_abapgit_definitions=>c_action-url }?url={ lv_display_url }|
                        iv_title = 'Commit'
                        iv_class = |url| ).
      CATCH Lcx_abapgit_exception.
        ii_html->add( |<span class="url">{ lv_icon_commit }{ lv_commit_short_hash }</span>| ).
    ENDTRY.

  ENDMETHOD.
  METHOD render_repo_url.

    ri_html = Lcl_abapgit_html=>create( )->add_a(
      iv_txt   = shorten_repo_url( iv_url )
      iv_title = iv_url
      iv_act   = |{ Lif_abapgit_definitions=>c_action-url }?url={ iv_url }|
      iv_class = 'url' ).

    IF iv_render_remote_edit_for_key IS NOT INITIAL.
      ri_html->add_a(
        iv_txt   = ri_html->icon(
          iv_name  = 'edit-solid'
          iv_class = 'pad-sides'
          iv_hint  = 'Change remote' )
        iv_act   = |{ Lif_abapgit_definitions=>c_action-repo_remote_settings }?key={ iv_render_remote_edit_for_key }|
        iv_class = |remote_repo| ).
    ENDIF.

  ENDMETHOD.
  METHOD render_sci_result.

    DATA lv_icon TYPE string.

    lv_icon = ii_html->icon(
      iv_name = 'bug-solid'
      iv_hint = 'Code inspector result' ).

    CASE iv_sci_result.
      WHEN Lif_abapgit_definitions=>c_sci_result-passed.
        ii_html->add( |<span class="boxed green-filled-set">{ lv_icon }PASSED</span>| ).
      WHEN Lif_abapgit_definitions=>c_sci_result-failed.
        ii_html->add( |<span class="boxed red-filled-set">{ lv_icon }FAILED</span>| ).
      WHEN Lif_abapgit_definitions=>c_sci_result-warning.
        ii_html->add( |<span class="boxed yellow-filled-set">{ lv_icon }WARN</span>| ).
      WHEN OTHERS. " Including NO_RUN
        RETURN.
    ENDCASE.

  ENDMETHOD.
  METHOD render_text_input.

    DATA lv_attrs TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    IF iv_value IS NOT INITIAL.
      lv_attrs = | value="{ iv_value }"|.
    ENDIF.

    IF iv_max_length IS NOT INITIAL.
      lv_attrs = lv_attrs && | maxlength="{ iv_max_length }"|.
    ENDIF.

    IF iv_autofocus = abap_true.
      lv_attrs = lv_attrs && | autofocus|.
    ENDIF.

    ri_html->add( |<label for="{ iv_name }">{ iv_label }</label>| ).
    ri_html->add( |<input id="{ iv_name }" name="{ iv_name }" type="text"{ lv_attrs }>| ).

  ENDMETHOD.
  METHOD render_timestamp.

    DATA lv_date TYPE d.
    DATA lv_time TYPE t.

    CONVERT TIME STAMP iv_timestamp
      TIME ZONE gv_time_zone
      INTO DATE lv_date
      TIME lv_time.

    rv_rendered = |{ lv_date DATE = USER } { lv_time TIME = USER }|.

  ENDMETHOD.
  METHOD render_transport.

    DATA:
      lv_title TYPE string,
      lv_jump  TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    IF iv_transport IS INITIAL.
      RETURN.
    ENDIF.

    lv_title = Lcl_abapgit_factory=>get_cts_api( )->read_description( iv_transport ).

    lv_jump = |{ Lif_abapgit_definitions=>c_action-jump_transport }?transport={ iv_transport }|.

    IF iv_icon_only = abap_true.
      ri_html->add_a( iv_act   = lv_jump
                      iv_title = |Transport { iv_transport }|
                      iv_txt   = Lcl_abapgit_html=>icon( 'truck-solid/darkgrey' ) ).
    ELSE.
      ri_html->add( |<span class="transport-box">| ).

      ri_html->add_icon( iv_name = 'truck-solid/grey70'
                         iv_hint = 'Transport' ).
      IF iv_interactive = abap_true.
        ri_html->add_a( iv_act   = lv_jump
                        iv_title = lv_title
                        iv_txt   = |{ iv_transport }| ).
      ELSE.
        ri_html->add( iv_transport ).
      ENDIF.

      ri_html->add( '</span>' ).
    ENDIF.

  ENDMETHOD.
  METHOD render_user_name.

    DATA:
      lv_title TYPE string,
      lv_jump  TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    IF iv_username IS INITIAL.
      RETURN.
    ENDIF.

    IF iv_username <> Lcl_abapgit_objects_super=>c_user_unknown AND iv_suppress_title = abap_false.
      lv_title = Lcl_abapgit_user_record=>get_title( iv_username ).
    ENDIF.

    lv_jump = |{ Lif_abapgit_definitions=>c_action-jump_user }?user={ iv_username }|.

    IF iv_icon_only = abap_true.
      ri_html->add_a( iv_act   = lv_jump
                      iv_title = lv_title
                      iv_txt   = Lcl_abapgit_html=>icon( 'user-solid/darkgrey' ) ).
    ELSE.
      ri_html->add( |<span class="user-box">| ).

      ri_html->add_icon( iv_name = 'user-solid/grey70'
                         iv_hint = 'User name' ).
      IF iv_interactive = abap_true AND iv_username <> Lcl_abapgit_objects_super=>c_user_unknown.
        ri_html->add_a( iv_act   = lv_jump
                        iv_title = lv_title
                        iv_txt   = |{ iv_username }| ).
      ELSE.
        ri_html->add( iv_username ).
      ENDIF.

      ri_html->add( '</span>' ).
    ENDIF.

  ENDMETHOD.
  METHOD render_warning_banner.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.
    ri_html->add( '<div class="dummydiv warning">' ).
    ri_html->add( |{ ri_html->icon( 'exclamation-triangle/yellow' ) } { iv_text }| ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
  METHOD shorten_repo_url.
    DATA lv_new_length TYPE i.
    DATA lv_length_to_truncate_to TYPE i.

    rv_shortened = iv_full_url.

    REPLACE FIRST OCCURRENCE OF 'https://' IN rv_shortened WITH ''.
    REPLACE FIRST OCCURRENCE OF 'http://' IN rv_shortened WITH ''.
    IF rv_shortened CP '*.git'.
      lv_new_length = strlen( rv_shortened ) - 4.
      rv_shortened  = rv_shortened(lv_new_length).
    ENDIF.

    IF strlen( rv_shortened ) > iv_max_length.
      lv_length_to_truncate_to = iv_max_length - 3.
      rv_shortened = rv_shortened(lv_length_to_truncate_to) && `...`.
    ENDIF.
  ENDMETHOD.
  METHOD get_item_icon.

    CASE is_item-obj_type.
      WHEN 'PROG' OR 'CLAS' OR 'FUGR' OR 'INTF' OR 'TYPE'.
        rv_html = Lcl_abapgit_html=>icon( iv_name = 'file-code/darkgrey'
                                          iv_hint = 'Code' ).
      WHEN 'W3MI' OR 'W3HT' OR 'SFPF'.
        rv_html = Lcl_abapgit_html=>icon( iv_name = 'file-image/darkgrey'
                                          iv_hint = 'Binary' ).
      WHEN 'DEVC'.
        rv_html = Lcl_abapgit_html=>icon( iv_name = 'box/darkgrey'
                                          iv_hint = 'Package' ).
      WHEN ''.
        rv_html = space. " no icon
      WHEN OTHERS.
        rv_html = Lcl_abapgit_html=>icon( 'file-alt/darkgrey' ).
    ENDCASE.

    IF is_item-is_dir = abap_true.
      rv_html = Lcl_abapgit_html=>icon( iv_name = 'folder/darkgrey'
                                        iv_hint = 'Folder' ).
    ENDIF.

  ENDMETHOD.
  METHOD get_item_link.

    DATA lv_encode TYPE string.
    DATA li_html TYPE REF TO Lif_abapgit_html.

    CREATE OBJECT li_html TYPE Lcl_abapgit_html.

    lv_encode = Lcl_abapgit_html_action_utils=>jump_encode(
      iv_obj_type = is_item-obj_type
      iv_obj_name = is_item-obj_name ).

    rv_html = li_html->a(
      iv_txt = |{ is_item-obj_name }|
      iv_act = |{ Lif_abapgit_definitions=>c_action-jump }?{ lv_encode }| ).

  ENDMETHOD.
  METHOD render_table_footer.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<tfoot>' ).
    ri_html->add( '<tr>' ).
    ri_html->add( '<td colspan="100%">' ).

    ri_html->add( iv_message ).

    ri_html->add( '</td>' ).
    ri_html->add( '</tr>' ).
    ri_html->add( '</tfoot>' ).

  ENDMETHOD.
  METHOD render_table_header.

    DATA:
      lv_tmp       TYPE string,
      lv_disp_name TYPE string.

    FIELD-SYMBOLS <ls_col> LIKE LINE OF it_col_spec.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<thead>' ).
    ri_html->add( '<tr>' ).

    LOOP AT it_col_spec ASSIGNING <ls_col>.
      " e.g. <th class="ro-detail">Created at [{ gv_time_zone }]</th>
      lv_tmp = '<th'.
      IF <ls_col>-css_class IS NOT INITIAL.
        lv_tmp = lv_tmp && | class="{ <ls_col>-css_class }"|.
      ENDIF.
      lv_tmp = lv_tmp && '>'.

      IF <ls_col>-display_name IS NOT INITIAL.
        lv_disp_name = <ls_col>-display_name.
        IF <ls_col>-add_tz = abap_true.
          lv_disp_name = lv_disp_name && | [{ gv_time_zone }]|.
        ENDIF.
        IF <ls_col>-tech_name = iv_order_by.
          IF iv_order_descending = abap_true.
            lv_tmp = lv_tmp && ri_html->a(
              iv_txt   = lv_disp_name
              iv_act   = |{ Lif_abapgit_definitions=>c_action-change_order_by }|
              iv_title = <ls_col>-title ).
          ELSE.
            lv_tmp = lv_tmp && ri_html->a(
              iv_txt   = lv_disp_name
              iv_act   = |{ Lif_abapgit_definitions=>c_action-direction }?direction=DESCENDING|
              iv_title = <ls_col>-title ).
          ENDIF.
        ELSEIF <ls_col>-allow_order_by = abap_true.
          lv_tmp = lv_tmp && ri_html->a(
            iv_txt   = lv_disp_name
            iv_act   = |{ Lif_abapgit_definitions=>c_action-change_order_by }?orderBy={ <ls_col>-tech_name }|
            iv_title = <ls_col>-title ).
        ELSE.
          lv_tmp = lv_tmp && lv_disp_name.
        ENDIF.
      ENDIF.
      IF <ls_col>-tech_name = iv_order_by
      AND iv_order_by IS NOT INITIAL.
        IF iv_order_descending = abap_true.
          lv_tmp = lv_tmp && | &#x25BE;|. " arrow down
        ELSE.
          lv_tmp = lv_tmp && | &#x25B4;|. " arrow up
        ENDIF.
      ENDIF.

      lv_tmp = lv_tmp && '</th>'.
      ri_html->add( lv_tmp ).
    ENDLOOP.

    ri_html->add( '</tr>' ).
    ri_html->add( '</thead>' ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_CHUNK_LIB implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_ADDOFFLIN <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_addofflinccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_addofflinccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_ADDOFFLIN implementation.
*"* method's implementations
*include methods.
  METHOD choose_labels.

    DATA:
      lv_old_labels TYPE string,
      lv_new_labels TYPE string.

    lv_old_labels = mo_form_data->get( c_id-labels ).

    lv_new_labels = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_select_labels( lv_old_labels ).

    mo_form_data->set(
      iv_key = c_id-labels
      iv_val = lv_new_labels ).

  ENDMETHOD.
  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT mo_validation_log.
    CREATE OBJECT mo_form_data.
    mo_form = get_form_schema( ).
    mo_form_util = Lcl_abapgit_html_form_utils=>create( mo_form ).
  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_addofflin.

    CREATE OBJECT lo_component.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'New Offline Repository'
      ii_child_component = lo_component ).

  ENDMETHOD.
  METHOD get_form_schema.

    ro_form = Lcl_abapgit_html_form=>create(
                iv_form_id   = 'add-repo-offline-form'
                iv_help_page = 'https://docs.abapgit.org/guide-offline-install.html' ).

    ro_form->text(
      iv_name        = c_id-name
      iv_required    = abap_true
      iv_label       = 'Name'
      iv_hint        = 'Unique name for repository'
    )->text(
      iv_name        = c_id-package
      iv_side_action = c_event-choose_package
      iv_required    = abap_true
      iv_upper_case  = abap_true
      iv_label       = 'Package'
      iv_hint        = 'SAP package for repository (should be a dedicated one)'
      iv_placeholder = 'Z... / $...'
    )->radio(
      iv_name        = c_id-folder_logic
      iv_default_value = Lif_abapgit_dot_abapgit=>c_folder_logic-prefix
      iv_label       = 'Folder Logic'
      iv_hint        = 'Define how package folders are named in repository'
    )->option(
      iv_label       = 'Prefix'
      iv_value       = Lif_abapgit_dot_abapgit=>c_folder_logic-prefix
    )->option(
      iv_label       = 'Full'
      iv_value       = Lif_abapgit_dot_abapgit=>c_folder_logic-full
    )->option(
      iv_label       = 'Mixed'
      iv_value       = Lif_abapgit_dot_abapgit=>c_folder_logic-mixed
    )->text(
      iv_name        = c_id-labels
      iv_side_action = c_event-choose_labels
      iv_label       = |Labels (comma-separated, allowed chars: "{ Lcl_abapgit_repo_labels=>c_allowed_chars }")|
      iv_hint        = 'Comma-separated labels for grouping and repo organization (optional)'
    )->checkbox(
      iv_name        = c_id-ignore_subpackages
      iv_label       = 'Ignore Subpackages'
      iv_hint        = 'Synchronize root package only'
    )->checkbox(
      iv_name        = c_id-main_lang_only
      iv_label       = 'Serialize Main Language Only'
      iv_hint        = 'Ignore translations, serialize just main language' ).
