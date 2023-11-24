********************************************************************************
*
* LICENSE and NOTICE
*
* See include program ZSHRINKER_ABAPGIT_LICENSE
*
********************************************************************************
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_component,
        visibility TYPE seoexpose,
        cmpname    TYPE seocmpname,
        descript   TYPE seodescr,
        cmptype    TYPE seocmptype,
      END OF ty_component,
      BEGIN OF ty_sub_component,
        cmpname  TYPE seocmpname,
        sconame  TYPE seosconame,
        descript TYPE seodescr,
        scotype  TYPE seoscotype,
      END OF ty_sub_component,
      ty_compontents     TYPE SORTED TABLE OF ty_component WITH UNIQUE DEFAULT KEY,
      ty_sub_compontents TYPE SORTED TABLE OF ty_sub_component WITH UNIQUE DEFAULT KEY.

    CLASS-METHODS:
      get_attributes
        IMPORTING is_components    TYPE ty_compontents
        RETURNING VALUE(rs_result) TYPE Lif_abapgit_aff_oo_types_v1=>ty_component_descriptions,
      get_methods
        IMPORTING is_components     TYPE ty_compontents
                  is_sub_components TYPE ty_sub_compontents
        RETURNING VALUE(rs_result)  TYPE Lif_abapgit_aff_oo_types_v1=>ty_methods,
      get_types
        IMPORTING is_components    TYPE ty_compontents
        RETURNING VALUE(rs_result) TYPE Lif_abapgit_aff_oo_types_v1=>ty_component_descriptions,
      get_events
        IMPORTING is_components     TYPE ty_compontents
                  is_sub_components TYPE ty_sub_compontents
        RETURNING VALUE(rs_result)  TYPE Lif_abapgit_aff_oo_types_v1=>ty_events,
      set_methods
        IMPORTING iv_clif_name  TYPE seoclsname
                  iv_language   TYPE langu
                  is_properties TYPE Lif_abapgit_aff_oo_types_v1=>ty_descriptions,
      set_attributes
        IMPORTING iv_clif_name  TYPE seoclsname
                  iv_language   TYPE langu
                  is_properties TYPE Lif_abapgit_aff_oo_types_v1=>ty_descriptions,
      set_events
        IMPORTING iv_clif_name  TYPE seoclsname
                  iv_language   TYPE langu
                  is_properties TYPE Lif_abapgit_aff_oo_types_v1=>ty_descriptions,
      set_types
        IMPORTING iv_clif_name  TYPE seoclsname
                  iv_language   TYPE langu
                  is_properties TYPE Lif_abapgit_aff_oo_types_v1=>ty_descriptions .
ENDCLASS.


CLASS SHRITEFUH64VYIPO5IWUYB3KW4KSFY IMPLEMENTATION.

  METHOD get_descr_comp_subc_w_exposure.
    DATA:
      lt_components     TYPE ty_compontents,
      lt_sub_components TYPE ty_sub_compontents.


    SELECT df~exposure AS visibility component~cmpname component_text~descript component~cmptype
      INTO TABLE lt_components
      FROM seocompo AS component
      LEFT OUTER JOIN seocompotx AS component_text
      ON component~cmpname = component_text~cmpname AND component~clsname = component_text~clsname AND
         component_text~langu = iv_language
      INNER JOIN seocompodf AS df
      ON component~clsname = df~clsname AND
         component~cmpname = df~cmpname
      WHERE component~clsname = iv_clif_name AND
            df~exposure       = iv_exposure.           "#EC CI_BUFFJOIN

    SELECT sub_component~cmpname sub_component~sconame sub_component_text~descript sub_component~scotype
      INTO TABLE lt_sub_components
      FROM seosubco AS sub_component JOIN seosubcotx AS sub_component_text
      ON sub_component~clsname = sub_component_text~clsname AND
         sub_component~cmpname = sub_component_text~cmpname AND
         sub_component~sconame = sub_component_text~sconame
      INNER JOIN seocompodf AS df
      ON sub_component~clsname = df~clsname AND
         sub_component~cmpname = df~cmpname
      WHERE sub_component~clsname    = iv_clif_name
        AND df~exposure              = iv_exposure
        AND sub_component_text~langu = iv_language
        AND sub_component_text~descript <> space.      "#EC CI_BUFFJOIN



    rs_properties-attributes = get_attributes( lt_components ).
    rs_properties-methods = get_methods( is_components = lt_components
                                         is_sub_components = lt_sub_components ).
    rs_properties-events = get_events( is_components = lt_components
                                       is_sub_components = lt_sub_components ).
    rs_properties-types = get_types( lt_components ).
  ENDMETHOD.


  METHOD get_descriptions_compo_subco.
    TYPES:
      BEGIN OF ty_helper_type,
        cmpname  TYPE seocmpname,
        descript TYPE seodescr,
        cmptype  TYPE seocmptype,
      END OF ty_helper_type.
    DATA:
      lt_components     TYPE STANDARD TABLE OF ty_helper_type,
      lt_sub_components TYPE ty_sub_compontents,
      lt_components_exp TYPE ty_compontents,
      ls_component_exp  LIKE LINE OF lt_components_exp.
    FIELD-SYMBOLS:
      <ls_component> LIKE LINE OF lt_components.


    SELECT component~cmpname component_text~descript component~cmptype
      INTO TABLE lt_components
      FROM seocompo AS component
      LEFT OUTER JOIN seocompotx AS component_text
      ON component~cmpname = component_text~cmpname AND component~clsname    = component_text~clsname
                                                    AND component_text~langu = iv_language
      WHERE component~clsname = iv_clif_name
      ORDER BY component~cmpname.          "#EC CI_BUFFJOIN

    SELECT sub_component~cmpname sub_component~sconame sub_component_text~descript sub_component~scotype
      INTO TABLE lt_sub_components
      FROM seosubco AS sub_component JOIN seosubcotx AS sub_component_text
      ON sub_component~clsname      = sub_component_text~clsname
          AND sub_component~cmpname = sub_component_text~cmpname
          AND sub_component~sconame = sub_component_text~sconame
      WHERE sub_component~clsname    = iv_clif_name
        AND sub_component_text~langu = iv_language
        AND sub_component_text~descript <> space.      "#EC CI_BUFFJOIN

    LOOP AT lt_components ASSIGNING <ls_component>.
      CLEAR ls_component_exp.
      MOVE-CORRESPONDING <ls_component> TO ls_component_exp.
      INSERT ls_component_exp INTO TABLE lt_components_exp.
    ENDLOOP.

    rs_properties-attributes = get_attributes( lt_components_exp ).
    rs_properties-methods = get_methods( is_components = lt_components_exp
                                         is_sub_components = lt_sub_components ).
    rs_properties-events = get_events( is_components = lt_components_exp
                                       is_sub_components = lt_sub_components ).
    rs_properties-types = get_types( lt_components_exp ).

  ENDMETHOD.


  METHOD get_attributes.
    DATA:
      lo_component TYPE Lif_abapgit_aff_oo_types_v1=>ty_component_description.
    FIELD-SYMBOLS <lo_attribute> TYPE ty_component.

    LOOP AT is_components ASSIGNING <lo_attribute> WHERE cmptype = seoo_cmptype_attribute AND descript IS NOT INITIAL.
      lo_component-name = <lo_attribute>-cmpname.
      lo_component-description = <lo_attribute>-descript.
      INSERT lo_component INTO TABLE rs_result.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_methods.
    DATA:
      lo_method    TYPE Lif_abapgit_aff_oo_types_v1=>ty_method,
      lo_exception TYPE Lif_abapgit_aff_oo_types_v1=>ty_component_description,
      lo_parameter TYPE Lif_abapgit_aff_oo_types_v1=>ty_component_description.

    FIELD-SYMBOLS <ls_sub_component> TYPE ty_sub_component.
    FIELD-SYMBOLS <ls_component> TYPE ty_component.

    LOOP AT is_components ASSIGNING <ls_component> WHERE cmptype = seoo_cmptype_method.
      lo_method-name = <ls_component>-cmpname.
      lo_method-description = <ls_component>-descript.

      LOOP AT is_sub_components ASSIGNING <ls_sub_component> WHERE cmpname = <ls_component>-cmpname.
        CASE <ls_sub_component>-scotype.
          WHEN seos_scotype_parameter.
            lo_parameter-name = <ls_sub_component>-sconame.
            lo_parameter-description = <ls_sub_component>-descript.
            INSERT lo_parameter INTO TABLE lo_method-parameters.
          WHEN seos_scotype_exception.
            lo_exception-name = <ls_sub_component>-sconame.
            lo_exception-description = <ls_sub_component>-descript.
            INSERT lo_exception INTO TABLE lo_method-exceptions.
        ENDCASE.
      ENDLOOP.

      IF lo_method-description IS NOT INITIAL
          OR lo_method-exceptions IS NOT INITIAL
          OR lo_method-parameters IS NOT INITIAL.
        INSERT lo_method INTO TABLE rs_result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_types.
    DATA:
        lo_type TYPE Lif_abapgit_aff_oo_types_v1=>ty_component_description.
    FIELD-SYMBOLS: <ls_types> TYPE ty_component.

    LOOP AT is_components ASSIGNING <ls_types>
        WHERE cmptype = seoo_cmptype_type AND descript IS NOT INITIAL.
      lo_type-name = <ls_types>-cmpname.
      lo_type-description = <ls_types>-descript.
      INSERT lo_type INTO TABLE rs_result.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_events.
    DATA:
      lo_parameter TYPE Lif_abapgit_aff_oo_types_v1=>ty_component_description,
      lo_event     TYPE Lif_abapgit_aff_oo_types_v1=>ty_event.
    FIELD-SYMBOLS <ls_event> TYPE ty_component.
    FIELD-SYMBOLS <ls_sub_component> TYPE ty_sub_component.

    LOOP AT is_components ASSIGNING <ls_event> WHERE cmptype = seoo_cmptype_event.
      lo_event-name = <ls_event>-cmpname.
      lo_event-description = <ls_event>-descript.

      LOOP AT is_sub_components ASSIGNING <ls_sub_component> WHERE cmpname = <ls_event>-cmpname.
        lo_parameter-name = <ls_sub_component>-sconame.
        lo_parameter-description = <ls_sub_component>-descript.
        INSERT lo_parameter INTO TABLE lo_event-parameters.
      ENDLOOP.

      IF lo_event-description IS NOT INITIAL OR lo_event-parameters IS NOT INITIAL.
        INSERT lo_event INTO TABLE rs_result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_attributes.
    DATA:
      lo_attribute TYPE seocompotx.
    FIELD-SYMBOLS: <ls_attribute> TYPE Lif_abapgit_aff_oo_types_v1=>ty_component_description.

    LOOP AT is_properties-attributes ASSIGNING <ls_attribute>.
      lo_attribute-clsname  = iv_clif_name.
      lo_attribute-cmpname  = <ls_attribute>-name.
      lo_attribute-langu    = iv_language.
      lo_attribute-descript = <ls_attribute>-description.
      MODIFY seocompotx FROM lo_attribute.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_methods.
    DATA:
      lo_method           TYPE seocompotx,
      lo_method_exception TYPE seosubcotx,
      lo_method_parameter TYPE seosubcotx.
    FIELD-SYMBOLS: <ls_method>    TYPE Lif_abapgit_aff_oo_types_v1=>ty_method,
                   <ls_parameter> TYPE Lif_abapgit_aff_oo_types_v1=>ty_component_description,
                   <ls_exception> TYPE Lif_abapgit_aff_oo_types_v1=>ty_component_description.

    LOOP AT is_properties-methods ASSIGNING <ls_method>.
      lo_method-clsname  = iv_clif_name.
      lo_method-cmpname  = <ls_method>-name.
      lo_method-langu    = iv_language.
      lo_method-descript = <ls_method>-description.
      MODIFY seocompotx FROM lo_method.

      LOOP AT <ls_method>-parameters ASSIGNING <ls_parameter>.
        lo_method_parameter-clsname  = iv_clif_name.
        lo_method_parameter-cmpname  = <ls_method>-name.
        lo_method_parameter-sconame  = <ls_parameter>-name.
        lo_method_parameter-langu    = iv_language.
        lo_method_parameter-descript = <ls_parameter>-description.
        MODIFY seosubcotx FROM lo_method_parameter.
      ENDLOOP.

      LOOP AT <ls_method>-exceptions ASSIGNING <ls_exception>.
        lo_method_exception-clsname  = iv_clif_name.
        lo_method_exception-cmpname  = <ls_method>-name.
        lo_method_exception-sconame  = <ls_exception>-name.
        lo_method_exception-langu    = iv_language.
        lo_method_exception-descript = <ls_exception>-description.
        MODIFY seosubcotx FROM lo_method_exception.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_events.
    DATA:
      lo_event_parameter TYPE seosubcotx,
      lo_event           TYPE seocompotx.
    FIELD-SYMBOLS: <ls_event>     TYPE Lif_abapgit_aff_oo_types_v1=>ty_event,
                   <ls_parameter> TYPE Lif_abapgit_aff_oo_types_v1=>ty_component_description.

    LOOP AT is_properties-events ASSIGNING <ls_event>.
      lo_event-clsname  = iv_clif_name.
      lo_event-cmpname  = <ls_event>-name.
      lo_event-langu    = iv_language.
      lo_event-descript = <ls_event>-description.
      MODIFY seocompotx FROM lo_event.

      LOOP AT <ls_event>-parameters ASSIGNING <ls_parameter>.
        lo_event_parameter-clsname  = iv_clif_name.
        lo_event_parameter-cmpname  = <ls_event>-name.
        lo_event_parameter-sconame  = <ls_parameter>-name.
        lo_event_parameter-langu    = iv_language.
        lo_event_parameter-descript = <ls_parameter>-description.
        MODIFY seosubcotx FROM lo_event_parameter.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_types.
    DATA:
      lo_type TYPE seocompotx.
    FIELD-SYMBOLS: <ls_type> TYPE Lif_abapgit_aff_oo_types_v1=>ty_component_description.

    LOOP AT is_properties-types ASSIGNING <ls_type>.
      lo_type-clsname  = iv_clif_name.
      lo_type-cmpname  = <ls_type>-name.
      lo_type-langu    = iv_language.
      lo_type-descript = <ls_type>-description.
      MODIFY seocompotx FROM lo_type.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_descriptions_compo_subco.
    set_attributes( is_properties = is_properties
                    iv_clif_name = iv_clif_name
                    iv_language = iv_language ).
    set_methods( is_properties = is_properties
                 iv_clif_name = iv_clif_name
                 iv_language = iv_language ).
    set_events( is_properties = is_properties
                iv_clif_name = iv_clif_name
                iv_language = iv_language ).
    set_types( is_properties = is_properties
               iv_clif_name = iv_clif_name
               iv_language = iv_language ).
  ENDMETHOD.

ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KW4MSFY DEFINITION.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_aff_type_mapping.
  PRIVATE SECTION.
    METHODS set_abapgit_descriptions
      IMPORTING is_clsname          TYPE seoclsname
                is_intf_aff         TYPE Lif_abapgit_aff_intf_v1=>ty_main
      EXPORTING et_descriptions     TYPE Lif_abapgit_oo_object_fnc=>ty_seocompotx_tt
                et_descriptions_sub TYPE Lif_abapgit_oo_object_fnc=>ty_seosubcotx_tt.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KW4MSFY IMPLEMENTATION.

  METHOD Lif_abapgit_aff_type_mapping~to_aff.
    DATA:
      ls_data_abapgit TYPE Lcl_abapgit_object_intf=>ty_intf,
      ls_data_aff     TYPE Lif_abapgit_aff_intf_v1=>ty_main.

    ls_data_abapgit = iv_data.

    ls_data_aff-format_version = '1'.

    " get header
    ls_data_aff-header-description = ls_data_abapgit-vseointerf-descript.
    ls_data_aff-header-abap_language_version = ls_data_abapgit-vseointerf-unicode.
    ls_data_aff-header-original_language = ls_data_abapgit-vseointerf-langu.

    " get category and proxy
    ls_data_aff-category = ls_data_abapgit-vseointerf-category.
    ls_data_aff-proxy = ls_data_abapgit-vseointerf-clsproxy.

    " get descriptions
    ls_data_aff-descriptions = SHRITEFUH64VYIPO5IWUYB3KW4KSFY=>get_descriptions_compo_subco(
                              iv_language  = ls_data_aff-header-original_language
                              iv_clif_name = ls_data_abapgit-vseointerf-clsname ).

    es_data = ls_data_aff.
  ENDMETHOD.

  METHOD Lif_abapgit_aff_type_mapping~to_abapgit.
    DATA:
      ls_data_abapgit TYPE Lcl_abapgit_object_intf=>ty_intf,
      ls_data_aff     TYPE Lif_abapgit_aff_intf_v1=>ty_main,
      lv_classname    TYPE seoclsname.


    ls_data_aff = iv_data.

    lv_classname = iv_object_name.

    set_abapgit_descriptions( EXPORTING is_clsname          = lv_classname
                                        is_intf_aff         = ls_data_aff
                              IMPORTING et_descriptions     = ls_data_abapgit-description
                                        et_descriptions_sub = ls_data_abapgit-description_sub ).

    ls_data_abapgit-vseointerf-clsname = iv_object_name.
    ls_data_abapgit-vseointerf-descript = ls_data_aff-header-description.
    ls_data_abapgit-vseointerf-category = ls_data_aff-category.
    ls_data_abapgit-vseointerf-unicode  = ls_data_aff-header-abap_language_version.
    ls_data_abapgit-vseointerf-langu    = ls_data_aff-header-original_language.
    ls_data_abapgit-vseointerf-clsproxy = ls_data_aff-proxy.
    ls_data_abapgit-vseointerf-exposure = seoc_exposure_public.
    ls_data_abapgit-vseointerf-state    = seoc_state_implemented.

    es_data = ls_data_abapgit.

  ENDMETHOD.

  METHOD set_abapgit_descriptions.

    DATA ls_description       TYPE seocompotx.
    DATA ls_description_subco TYPE seosubcotx.
    FIELD-SYMBOLS <ls_description>      TYPE Lif_abapgit_aff_oo_types_v1=>ty_component_description.
    FIELD-SYMBOLS <ls_meth_description> TYPE Lif_abapgit_aff_oo_types_v1=>ty_method.
    FIELD-SYMBOLS <ls_evt_description>  TYPE Lif_abapgit_aff_oo_types_v1=>ty_event.


    LOOP AT is_intf_aff-descriptions-types ASSIGNING <ls_description>.
      ls_description-clsname  = is_clsname.
      ls_description-cmpname  = <ls_description>-name.
      ls_description-langu    = is_intf_aff-header-original_language.
      ls_description-descript = <ls_description>-description.
      APPEND ls_description TO et_descriptions.
    ENDLOOP.

    LOOP AT is_intf_aff-descriptions-attributes ASSIGNING <ls_description>.
      ls_description-clsname  = is_clsname.
      ls_description-cmpname  = <ls_description>-name.
      ls_description-langu    = is_intf_aff-header-original_language.
      ls_description-descript = <ls_description>-description.
      APPEND ls_description TO et_descriptions.
    ENDLOOP.

    LOOP AT is_intf_aff-descriptions-methods ASSIGNING <ls_meth_description>.
      ls_description-clsname  = is_clsname.
      ls_description-cmpname  = <ls_meth_description>-name.
      ls_description-langu    = is_intf_aff-header-original_language.
      ls_description-descript = <ls_meth_description>-description.
      APPEND ls_description TO et_descriptions.

      LOOP AT <ls_meth_description>-parameters ASSIGNING <ls_description>.
        ls_description_subco-clsname  = ls_description-clsname.
        ls_description_subco-cmpname  = ls_description-cmpname.
        ls_description_subco-langu    = ls_description-langu.
        ls_description_subco-sconame  = <ls_description>-name.
        ls_description_subco-descript = <ls_description>-description.
        APPEND ls_description_subco TO et_descriptions_sub.
      ENDLOOP.

      LOOP AT <ls_meth_description>-exceptions ASSIGNING <ls_description>.
        ls_description_subco-clsname  = ls_description-clsname.
        ls_description_subco-cmpname  = ls_description-cmpname.
        ls_description_subco-langu    = ls_description-langu.
        ls_description_subco-sconame  = <ls_description>-name.
        ls_description_subco-descript = <ls_description>-description.
        APPEND ls_description_subco TO et_descriptions_sub.
      ENDLOOP.
    ENDLOOP.

    LOOP AT is_intf_aff-descriptions-events ASSIGNING <ls_evt_description>.
      ls_description-clsname  = is_clsname.
      ls_description-cmpname  = <ls_evt_description>-name.
      ls_description-langu    = is_intf_aff-header-original_language.
      ls_description-descript = <ls_evt_description>-description.
      APPEND ls_description TO et_descriptions.

      LOOP AT <ls_evt_description>-parameters ASSIGNING <ls_description>.
        ls_description_subco-clsname  = ls_description-clsname.
        ls_description_subco-cmpname  = ls_description-cmpname.
        ls_description_subco-langu    = ls_description-langu.
        ls_description_subco-sconame  = <ls_description>-name.
        ls_description_subco-descript = <ls_description>-description.
        APPEND ls_description_subco TO et_descriptions_sub.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.


CLASS SHRITEFUH64VYIPO5IWUYB3KW4OSFY DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS serialize
      IMPORTING is_intf          TYPE Lcl_abapgit_object_intf=>ty_intf
      RETURNING VALUE(rv_result) TYPE xstring
      RAISING   Lcx_abapgit_exception.
    CLASS-METHODS deserialize
      IMPORTING iv_data          TYPE xstring
      RETURNING VALUE(rv_result) TYPE Lif_abapgit_aff_intf_v1=>ty_main
      RAISING   Lcx_abapgit_exception.
  PRIVATE SECTION.
    CLASS-METHODS:
      "! For serialization
      "! @parameter rt_result | Map/table that associates ABAP values to JSON values (enums)
      get_mappings
        RETURNING VALUE(rt_result) TYPE Lcl_abapgit_json_handler=>ty_enum_mappings,
      "! For serialization
      "! @parameter rt_result | Paths that will not be serialized (depending on value)
      get_paths_to_skip
        RETURNING VALUE(rt_result) TYPE Lcl_abapgit_json_handler=>ty_skip_paths.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KW4OSFY IMPLEMENTATION.

  METHOD serialize.
    DATA:
      ls_data_aff      TYPE Lif_abapgit_aff_intf_v1=>ty_main,
      lx_exception     TYPE REF TO cx_root,
      lo_aff_handler   TYPE REF TO Lcl_abapgit_json_handler,
      lo_aff_mapper    TYPE REF TO Lif_abapgit_aff_type_mapping,
      lt_enum_mappings TYPE Lcl_abapgit_json_handler=>ty_enum_mappings,
      lt_paths_to_skip TYPE Lcl_abapgit_json_handler=>ty_skip_paths.


    CREATE OBJECT lo_aff_mapper TYPE SHRITEFUH64VYIPO5IWUYB3KW4MSFY.
    lo_aff_mapper->to_aff( EXPORTING iv_data = is_intf
                           IMPORTING es_data = ls_data_aff ).

    lt_enum_mappings = get_mappings( ).
    lt_paths_to_skip = get_paths_to_skip( ).

    CREATE OBJECT lo_aff_handler.
    TRY.
        rv_result = lo_aff_handler->serialize( iv_data          = ls_data_aff
                                               iv_enum_mappings = lt_enum_mappings
                                               iv_skip_paths    = lt_paths_to_skip ).
      CATCH cx_root INTO lx_exception.
        Lcx_abapgit_exception=>raise_with_text( lx_exception ).
    ENDTRY.

  ENDMETHOD.

  METHOD get_mappings.
    DATA:
      ls_category_mapping   TYPE Lcl_abapgit_json_handler=>ty_enum_mapping,
      ls_json_abap_mapping  TYPE Lcl_abapgit_json_handler=>ty_json_abap_mapping,
      lt_json_abap_mappings TYPE Lcl_abapgit_json_handler=>ty_json_abap_mappings.

    ls_json_abap_mapping-abap = Lif_abapgit_aff_intf_v1=>co_category-general.
    ls_json_abap_mapping-json = 'standard'.
    APPEND ls_json_abap_mapping TO lt_json_abap_mappings.
    ls_json_abap_mapping-abap = Lif_abapgit_aff_intf_v1=>co_category-classic_badi.
    ls_json_abap_mapping-json = 'classicBadi'.
    APPEND ls_json_abap_mapping TO lt_json_abap_mappings.
    ls_json_abap_mapping-abap = Lif_abapgit_aff_intf_v1=>co_category-business_static_components.
    ls_json_abap_mapping-json = 'businessStaticComponents'.
    APPEND ls_json_abap_mapping TO lt_json_abap_mappings.
    ls_json_abap_mapping-abap = Lif_abapgit_aff_intf_v1=>co_category-db_procedure_proxy.
    ls_json_abap_mapping-json = 'dbProcedureProxy'.
    APPEND ls_json_abap_mapping TO lt_json_abap_mappings.
    ls_json_abap_mapping-abap = Lif_abapgit_aff_intf_v1=>co_category-web_dynpro_runtime.
    ls_json_abap_mapping-json = 'webDynproRuntime'.
    APPEND ls_json_abap_mapping TO lt_json_abap_mappings.
    ls_json_abap_mapping-abap = Lif_abapgit_aff_intf_v1=>co_category-enterprise_service.
    ls_json_abap_mapping-json = 'enterpriseService'.
    APPEND ls_json_abap_mapping TO lt_json_abap_mappings.

    ls_category_mapping-path = '/category'.
    ls_category_mapping-mappings = lt_json_abap_mappings.

    APPEND ls_category_mapping TO rt_result.
  ENDMETHOD.

  METHOD get_paths_to_skip.
    DATA:
      ls_path_to_skipp TYPE Lcl_abapgit_json_handler=>ty_path_value_pair.

    ls_path_to_skipp-path  = '/category'.
    ls_path_to_skipp-value = 'standard'.

    APPEND ls_path_to_skipp TO rt_result.
  ENDMETHOD.

  METHOD deserialize.
    DATA:
      lo_ajson                      TYPE REF TO Lcl_abapgit_json_handler,
      lx_exception                  TYPE REF TO cx_static_check,
      lt_enum_mappings              TYPE Lcl_abapgit_json_handler=>ty_enum_mappings,
      lt_default_abap_langu_version TYPE Lcl_abapgit_json_handler=>ty_path_value_pair,
      lt_values_for_initial         TYPE Lcl_abapgit_json_handler=>ty_skip_paths.

    lt_values_for_initial = get_paths_to_skip( ).

    lt_default_abap_langu_version-path  = '/header/abap_language_version'.
    lt_default_abap_langu_version-value = Lif_abapgit_dot_abapgit=>c_abap_language_version-standard.
    APPEND lt_default_abap_langu_version TO lt_values_for_initial.

    lt_enum_mappings = get_mappings( ).


    CREATE OBJECT lo_ajson.
    TRY.
        lo_ajson->deserialize(
           EXPORTING
             iv_content = iv_data
             iv_defaults = lt_values_for_initial
             iv_enum_mappings = lt_enum_mappings
           IMPORTING
             ev_data    = rv_result ).
      CATCH cx_static_check INTO lx_exception.
        Lcx_abapgit_exception=>raise_with_text( lx_exception ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

*"* test class
*include zcl_abapgit_object_intf=======ccau.
*CLASS SHRITEFUH64VYIPO5IWUYB3KW4QSFY DEFINITION DEFERRED.
*CLASS zcl_abapgit_object_intf DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KW4QSFY.









class LCL_ABAPGIT_OBJECT_INTF implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    DATA li_aff_registry TYPE REF TO Lif_abapgit_aff_registry.

    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).
    mi_object_oriented_object_fct = Lcl_abapgit_oo_factory=>make( ms_item-obj_type ).

    CREATE OBJECT li_aff_registry TYPE Lcl_abapgit_aff_registry.

    mv_aff_enabled = li_aff_registry->is_supported_object_type( 'INTF' ).

  ENDMETHOD.
  METHOD deserialize_descriptions.
    DATA:  ls_clskey TYPE seoclskey.
    ls_clskey-clsname = ms_item-obj_name.

    mi_object_oriented_object_fct->update_descriptions(
      is_key          = ls_clskey
      it_descriptions = it_description ).
  ENDMETHOD.
  METHOD deserialize_descr_sub.
    DATA:  ls_clskey TYPE seoclskey.
    ls_clskey-clsname = ms_item-obj_name.

    mi_object_oriented_object_fct->update_descriptions_sub(
      is_key          = ls_clskey
      it_descriptions = it_description ).
  ENDMETHOD.
  METHOD deserialize_docu.
    DATA: lv_object     TYPE dokhl-object,
          ls_i18n_lines TYPE Lif_abapgit_lang_definitions=>ty_i18n_line.

    lv_object = ms_item-obj_name.

    IF lines( is_docu-lines ) = 0.
      mi_object_oriented_object_fct->delete_documentation(
        iv_id          = c_longtext_id-interface
        iv_object_name = lv_object
        iv_language    = mv_language ).
      RETURN.
    ENDIF.

    mi_object_oriented_object_fct->create_documentation(
      it_lines       = is_docu-lines
      iv_id          = c_longtext_id-interface
      iv_object_name = lv_object
      iv_language    = mv_language ).

    LOOP AT is_docu-i18n_lines INTO ls_i18n_lines.
      mi_object_oriented_object_fct->create_documentation(
        it_lines         = ls_i18n_lines-lines
        iv_id            = c_longtext_id-interface
        iv_object_name   = lv_object
        iv_language      = ls_i18n_lines-language
        iv_no_masterlang = abap_true ).
    ENDLOOP.

    deserialize_longtexts(
      ii_xml           = ii_xml
      iv_longtext_name = c_longtext_name-attributes
      iv_longtext_id   = c_longtext_id-attributes ).

    deserialize_longtexts(
      ii_xml           = ii_xml
      iv_longtext_name = c_longtext_name-methods
      iv_longtext_id   = c_longtext_id-methods ).

    deserialize_longtexts(
      ii_xml           = ii_xml
      iv_longtext_name = c_longtext_name-events
      iv_longtext_id   = c_longtext_id-events ).

  ENDMETHOD.
  METHOD deserialize_pre_ddic.

    DATA ls_intf TYPE ty_intf.

    IF mv_aff_enabled = abap_true.
      ls_intf = read_json( ).
    ELSE.
      ii_xml->read( EXPORTING iv_name = 'VSEOINTERF'
                    CHANGING  cg_data = ls_intf-vseointerf ).
    ENDIF.

    set_abap_language_version( CHANGING cv_abap_language_version = ls_intf-vseointerf-unicode ).

    mi_object_oriented_object_fct->create(
      EXPORTING
        iv_check      = abap_false
        iv_package    = iv_package
      CHANGING
        cg_properties = ls_intf-vseointerf ).

  ENDMETHOD.
  METHOD deserialize_proxy.

    DATA: lv_transport    TYPE trkorr,
          li_proxy_object TYPE REF TO if_px_main,
          lv_name         TYPE prx_r3name,
          lx_proxy_fault  TYPE REF TO cx_proxy_fault.

    lv_name = ms_item-obj_name.

    lv_transport = iv_transport.

    TRY.
        li_proxy_object = cl_pxn_factory=>create(
                              application  = 'PROXY_UI'
                              display_only = abap_false
                              saveable     = abap_true
                          )->if_pxn_factory~load_by_abap_name(
                              object   = ms_item-obj_type
                              obj_name = lv_name ).

        li_proxy_object->activate(
          EXPORTING
            activate_all     = abap_true
          CHANGING
            transport_number = lv_transport ).

        li_proxy_object->dequeue( ).

      CATCH cx_proxy_fault INTO lx_proxy_fault.
        IF li_proxy_object IS BOUND.
          TRY.
              li_proxy_object->dequeue( ).
            CATCH cx_proxy_fault ##NO_HANDLER.
          ENDTRY.
        ENDIF.
        Lcx_abapgit_exception=>raise_with_text( lx_proxy_fault ).
    ENDTRY.

  ENDMETHOD.
  METHOD read_json.
    DATA lv_json_data TYPE xstring.
    DATA ls_intf_aff TYPE Lif_abapgit_aff_intf_v1=>ty_main.
    DATA lo_aff_mapper TYPE REF TO Lif_abapgit_aff_type_mapping.

    lv_json_data = Lif_abapgit_object~mo_files->read_raw( 'json' ).
    ls_intf_aff = SHRITEFUH64VYIPO5IWUYB3KW4OSFY=>deserialize( lv_json_data ).

    CREATE OBJECT lo_aff_mapper TYPE SHRITEFUH64VYIPO5IWUYB3KW4MSFY.
    lo_aff_mapper->to_abapgit( EXPORTING iv_data = ls_intf_aff
                                         iv_object_name = ms_item-obj_name
                               IMPORTING es_data = rs_intf ).
  ENDMETHOD.
  METHOD read_xml.
    ii_xml->read( EXPORTING iv_name = 'VSEOINTERF'
                  CHANGING  cg_data = rs_intf-vseointerf ).
    ii_xml->read( EXPORTING iv_name = 'DESCRIPTIONS'
                  CHANGING  cg_data = rs_intf-description ).
    ii_xml->read( EXPORTING iv_name = 'DESCRIPTIONS_SUB'
                  CHANGING  cg_data = rs_intf-description_sub ).
    ii_xml->read( EXPORTING iv_name = 'LINES'
                  CHANGING  cg_data = rs_intf-docu-lines ).
    ii_xml->read( EXPORTING iv_name = 'I18N_LINES'
                  CHANGING  cg_data = rs_intf-docu-i18n_lines ).
  ENDMETHOD.
  METHOD serialize_descr.

    DATA: lt_descriptions    TYPE Lif_abapgit_oo_object_fnc=>ty_seocompotx_tt,
          lv_language        TYPE spras,
          lt_language_filter TYPE Lif_abapgit_environment=>ty_system_language_filter.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      lv_language = mv_language.
    ENDIF.

    lt_descriptions = mi_object_oriented_object_fct->read_descriptions(
      iv_object_name = iv_clsname
      iv_language    = lv_language ).

    " Remove technical languages
    lt_language_filter = mo_i18n_params->build_language_filter( ).
    DELETE lt_descriptions WHERE NOT langu IN lt_language_filter AND langu <> mv_language.

    IF lines( lt_descriptions ) = 0.
      RETURN.
    ENDIF.

    rs_description = lt_descriptions.

  ENDMETHOD.
  METHOD serialize_descr_sub.

    DATA: lt_descriptions    TYPE Lif_abapgit_oo_object_fnc=>ty_seosubcotx_tt,
          lv_language        TYPE spras,
          lt_language_filter TYPE Lif_abapgit_environment=>ty_system_language_filter.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      lv_language = mv_language.
    ENDIF.

    lt_descriptions = mi_object_oriented_object_fct->read_descriptions_sub(
      iv_object_name = iv_clsname
      iv_language    = lv_language ).

    " Remove technical languages
    lt_language_filter = mo_i18n_params->build_language_filter( ).
    DELETE lt_descriptions WHERE NOT langu IN lt_language_filter AND langu <> mv_language.

    IF lines( lt_descriptions ) = 0.
      RETURN.
    ENDIF.

    rs_description = lt_descriptions.

  ENDMETHOD.
  METHOD serialize_docu.

    DATA: lt_lines      TYPE tlinetab,
          lv_object     TYPE dokhl-object,
          lv_langu      TYPE sy-langu,
          lt_i18n_lines TYPE Lif_abapgit_lang_definitions=>ty_i18n_lines,
          ls_i18n_lines TYPE Lif_abapgit_lang_definitions=>ty_i18n_line.

    lv_object = iv_clsname.

    lt_lines = mi_object_oriented_object_fct->read_documentation(
      iv_id          = c_longtext_id-interface
      iv_object_name = lv_object
      iv_language    = mv_language ).

    rs_docu-lines = lt_lines.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      RETURN.
    ENDIF.

    LOOP AT it_langu_additional INTO lv_langu.

      lt_lines = mi_object_oriented_object_fct->read_documentation(
        iv_id          = c_longtext_id-interface
        iv_object_name = lv_object
        iv_language    = lv_langu ).

      IF lines( lt_lines ) > 0.
        CLEAR ls_i18n_lines.
        ls_i18n_lines-language = lv_langu.
        ls_i18n_lines-lines    = lt_lines.
        INSERT ls_i18n_lines INTO TABLE lt_i18n_lines.
      ENDIF.

    ENDLOOP.

    rs_docu-i18n_lines = lt_i18n_lines.

  ENDMETHOD.
  METHOD serialize_xml.

    DATA:
      ls_intf             TYPE ty_intf,
      ls_clskey           TYPE seoclskey,
      lv_serialized_data  TYPE xstring,
      lt_langu_additional TYPE Lif_abapgit_lang_definitions=>ty_langus.

    ls_clskey-clsname = ms_item-obj_name.

    ls_intf-vseointerf = mi_object_oriented_object_fct->get_interface_properties( ls_clskey ).

    clear_abap_language_version( CHANGING cv_abap_language_version = ls_intf-vseointerf-unicode ).

    " Select all active translations of documentation
    " Skip main language - it was already serialized
    SELECT DISTINCT langu
      INTO TABLE lt_langu_additional
      FROM dokhl
      WHERE id     = c_longtext_id-interface
        AND object = ls_clskey-clsname
        AND langu  <> mv_language
      ORDER BY langu.

    ls_intf-docu = serialize_docu(
      iv_clsname          = ls_clskey-clsname
      it_langu_additional = lt_langu_additional ).

    ls_intf-description = serialize_descr( ls_clskey-clsname ).
    ls_intf-description_sub = serialize_descr_sub( ls_clskey-clsname ).

    " HERE: switch with feature flag for XML or JSON file format
    IF mv_aff_enabled = abap_true.
      lv_serialized_data = SHRITEFUH64VYIPO5IWUYB3KW4OSFY=>serialize( ls_intf ).
      Lif_abapgit_object~mo_files->add_raw( iv_ext  = 'json'
                                            iv_data = lv_serialized_data ).

    ELSE.
      io_xml->add( iv_name = 'VSEOINTERF'
                   ig_data = ls_intf-vseointerf ).
      io_xml->add( iv_name = 'DESCRIPTIONS'
                   ig_data = ls_intf-description ).
      io_xml->add( iv_name = 'DESCRIPTIONS_SUB'
                   ig_data = ls_intf-description_sub ).
      io_xml->add( iv_name = 'LINES'
                   ig_data = ls_intf-docu-lines ).
      io_xml->add( iv_name = 'I18N_LINES'
                   ig_data = ls_intf-docu-i18n_lines ).

      serialize_longtexts(
        ii_xml           = io_xml
        iv_longtext_name = c_longtext_name-attributes
        iv_longtext_id   = c_longtext_id-attributes ).

      serialize_longtexts(
        ii_xml           = io_xml
        iv_longtext_name = c_longtext_name-methods
        iv_longtext_id   = c_longtext_id-methods ).

      serialize_longtexts(
        ii_xml           = io_xml
        iv_longtext_name = c_longtext_name-events
        iv_longtext_id   = c_longtext_id-events ).

    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    TYPES: BEGIN OF ty_includes,
             programm TYPE syrepid,
           END OF ty_includes.

    TYPES: BEGIN OF ty_reposrc,
             unam  TYPE reposrc-unam,
             udat  TYPE reposrc-udat,
             utime TYPE reposrc-utime,
           END OF ty_reposrc.

    DATA: lt_reposrc  TYPE STANDARD TABLE OF ty_reposrc,
          ls_reposrc  LIKE LINE OF lt_reposrc,
          lt_includes TYPE STANDARD TABLE OF ty_includes.

    lt_includes = mi_object_oriented_object_fct->get_includes( ms_item-obj_name ).
    ASSERT lines( lt_includes ) > 0.

    SELECT unam udat utime FROM reposrc
      INTO TABLE lt_reposrc
      FOR ALL ENTRIES IN lt_includes
      WHERE progname = lt_includes-programm
      AND r3state = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ELSE.
      SORT lt_reposrc BY udat DESCENDING utime DESCENDING.
      READ TABLE lt_reposrc INDEX 1 INTO ls_reposrc.
      ASSERT sy-subrc = 0.
      rv_user = ls_reposrc-unam.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.
    DATA: ls_clskey     TYPE seoclskey,
          ls_vseointerf TYPE vseointerf.

    ls_clskey-clsname = ms_item-obj_name.
    ls_vseointerf = mi_object_oriented_object_fct->get_interface_properties( ls_clskey ).

    IF ls_vseointerf-clsproxy = abap_true.
      " Proxy interfaces are managed via SPRX
      RETURN.
    ENDIF.

    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    corr_insert( iv_package ).

    mi_object_oriented_object_fct->delete( ls_clskey ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.
    DATA: lt_source TYPE rswsourcet,
          ls_clskey TYPE seoclskey,
          ls_intf   TYPE ty_intf.

    IF iv_step = Lif_abapgit_object=>gc_step_id-abap.
      " HERE: switch with feature flag between XML and JSON file format
      IF mv_aff_enabled = abap_true.
        ls_intf = read_json( ).
      ELSE.
        ls_intf = read_xml( io_xml ).
      ENDIF.

      set_abap_language_version( CHANGING cv_abap_language_version = ls_intf-vseointerf-unicode ).

      IF ls_intf-vseointerf-clsproxy = abap_true.
        " Proxy interfaces are managed via SPRX
        deserialize_proxy( iv_transport ).

      ELSE.
        mi_object_oriented_object_fct->create(
          EXPORTING
            iv_check      = abap_true
            iv_package    = iv_package
          CHANGING
            cg_properties = ls_intf-vseointerf ).

        ls_clskey-clsname = ms_item-obj_name.
        lt_source = Lif_abapgit_object~mo_files->read_abap( ).

        mi_object_oriented_object_fct->deserialize_source(
          is_key     = ls_clskey
          iv_package = iv_package
          iv_version = ls_intf-vseointerf-unicode
          it_source  = lt_source ).

        deserialize_descriptions( ls_intf-description ).

        deserialize_descr_sub( ls_intf-description_sub ).

        deserialize_docu(
          is_docu = ls_intf-docu
          ii_xml  = io_xml ).

        mi_object_oriented_object_fct->add_to_activation_list( ms_item ).
      ENDIF.

    ELSEIF iv_step = Lif_abapgit_object=>gc_step_id-early.

      " If interface does not exist, create it
      " so DDIC that depends on it does not fail activation
      IF Lif_abapgit_object~exists( ) = abap_false.
        deserialize_pre_ddic(
          ii_xml     = io_xml
          iv_package = iv_package ).
      ELSE.
        corr_insert( iv_package ).
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: ls_class_key TYPE seoclskey,
          lv_category  TYPE seoclassdf-category.

    ls_class_key-clsname = ms_item-obj_name.

    rv_bool = mi_object_oriented_object_fct->exists( ls_class_key ).

    IF rv_bool = abap_true.
      SELECT SINGLE category FROM seoclassdf INTO lv_category
        WHERE clsname = ls_class_key-clsname
        AND ( version = '1'
        OR version = '0' ) ##WARN_OK.                   "#EC CI_GENBUFF
      IF sy-subrc = 0 AND lv_category = seoc_category_webdynpro_class.
        rv_bool = abap_false.
      ELSE.
        SELECT SINGLE obj_name FROM sproxhdr INTO ls_class_key-clsname
          WHERE object = 'INTF' AND obj_name = ls_class_key-clsname.
        IF sy-subrc = 0.
          " generated by proxy
          rv_bool = abap_false.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-early TO rt_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    DATA: lv_object TYPE eqegraarg.

    lv_object = |{ ms_item-obj_name }|.
    OVERLAY lv_object WITH '==============================P'.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESEOCLASS'
                                            iv_argument    = lv_object ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lt_source        TYPE seop_source_string,
          ls_interface_key TYPE seoclskey.

    ls_interface_key-clsname = ms_item-obj_name.

    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        version = seoc_version_active
        force   = abap_true.
    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        version = seoc_version_inactive
        force   = abap_true.

    lt_source = mi_object_oriented_object_fct->serialize_abap( ls_interface_key ).

    Lif_abapgit_object~mo_files->add_abap( lt_source ).

    serialize_xml( io_xml ).

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
endclass. "ZCL_ABAPGIT_OBJECT_INTF implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_IWMO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_iwmo=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_iwmo=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_IWMO implementation.
*"* method's implementations
*include methods.
  METHOD get_field_rules.
    ro_result = Lcl_abapgit_field_rules=>create( ).
    ro_result->add(
      iv_table     = '/IWBEP/I_MGW_OHD'
      iv_field     = 'CREATED_BY'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWBEP/I_MGW_OHD'
      iv_field     = 'CREATED_TIMESTMP'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-timestamp
    )->add(
      iv_table     = '/IWBEP/I_MGW_OHD'
      iv_field     = 'CHANGED_BY'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWBEP/I_MGW_OHD'
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

    DATA lv_created TYPE sy-uname.
    DATA lv_changed TYPE sy-uname.

    " Get entry with highest version
    SELECT created_by changed_by INTO (lv_created, lv_changed) FROM ('/IWBEP/I_MGW_OHD')
      WHERE technical_name = ms_item-obj_name
      ORDER BY PRIMARY KEY.
      rv_user = lv_changed.
      IF lv_changed IS INITIAL.
        rv_user = lv_created.
      ENDIF.
    ENDSELECT.

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

    DATA: lv_mdl_technical_name TYPE c LENGTH 32,
          lv_version            TYPE bdc_fval,
          lt_bdcdata            TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.

    lv_mdl_technical_name = ms_item-obj_name.
    lv_version = ms_item-obj_name+32(4).

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = '/IWBEP/R_DST_MODEL_BUILDER'.
    <ls_bdcdata>-dynpro   = '0100'.
    <ls_bdcdata>-dynbegin = 'X'.
    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'GS_MODEL_SCREEN_100-TECHNICAL_NAME'.
    <ls_bdcdata>-fval = lv_mdl_technical_name.
    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'GS_MODEL_SCREEN_100-VERSION'.
    <ls_bdcdata>-fval = lv_version.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = '/IWBEP/REG_MODEL'
      it_bdcdata = lt_bdcdata ).

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
endclass. "ZCL_ABAPGIT_OBJECT_IWMO implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_IWSV <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_iwsv=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_iwsv=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_IWSV implementation.
*"* method's implementations
*include methods.
  METHOD get_field_rules.
    ro_result = Lcl_abapgit_field_rules=>create( ).
    ro_result->add(
      iv_table     = '/IWBEP/I_MGW_SRH'
      iv_field     = 'CREATED_BY'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWBEP/I_MGW_SRH'
      iv_field     = 'CREATED_TIMESTMP'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-timestamp
    )->add(
      iv_table     = '/IWBEP/I_MGW_SRH'
      iv_field     = 'CHANGED_BY'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWBEP/I_MGW_SRH'
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

    DATA lv_created TYPE sy-uname.
    DATA lv_changed TYPE sy-uname.

    " Get entry with highest version
    SELECT created_by changed_by INTO (lv_created, lv_changed) FROM ('/IWBEP/I_MGW_SRH')
      WHERE technical_name = ms_item-obj_name
      ORDER BY PRIMARY KEY.
      rv_user = lv_changed.
      IF lv_changed IS INITIAL.
        rv_user = lv_created.
      ENDIF.
    ENDSELECT.

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

    DATA: lv_technical_name TYPE c LENGTH 35,
          lv_version        TYPE bdc_fval,
          lt_bdcdata        TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.

    lv_technical_name = ms_item-obj_name(36).
    lv_version = ms_item-obj_name+36(4).

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = '/IWBEP/R_DST_SERVICE_BUILDER'.
    <ls_bdcdata>-dynpro   = '0100'.
    <ls_bdcdata>-dynbegin = 'X'.
    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'GS_SCREEN_100-TECHNICAL_NAME'.
    <ls_bdcdata>-fval = lv_technical_name.
    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'GS_SCREEN_100-VERSION'.
    <ls_bdcdata>-fval = lv_version.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = '/IWBEP/REG_SERVICE'
      it_bdcdata = lt_bdcdata ).

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
endclass. "ZCL_ABAPGIT_OBJECT_IWSV implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_IWVB <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_iwvb=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_iwvb=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_IWVB implementation.
*"* method's implementations
*include methods.
  METHOD get_field_rules.
    ro_result = Lcl_abapgit_field_rules=>create( ).
    ro_result->add(
      iv_table     = '/IWBEP/I_MGW_VAH'
      iv_field     = 'CREATED_BY'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWBEP/I_MGW_VAH'
      iv_field     = 'CREATED_TIMESTMP'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-timestamp
    )->add(
      iv_table     = '/IWBEP/I_MGW_VAH'
      iv_field     = 'CHANGED_BY'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = '/IWBEP/I_MGW_VAH'
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

    DATA lv_created TYPE sy-uname.
    DATA lv_changed TYPE sy-uname.

    " Get entry with highest version
    SELECT created_by changed_by INTO (lv_created, lv_changed) FROM ('/IWBEP/I_MGW_VAH')
      WHERE technical_name = ms_item-obj_name
      ORDER BY PRIMARY KEY.
      rv_user = lv_changed.
      IF lv_changed IS INITIAL.
        rv_user = lv_created.
      ENDIF.
    ENDSELECT.

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

    SUBMIT /iwbep/r_dst_vocan_register
      WITH ip_aname = ms_item-obj_name
      WITH ip_avers = ms_item-obj_name+32(4)
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
endclass. "ZCL_ABAPGIT_OBJECT_IWVB implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_MSAG <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_msag=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_msag=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_MSAG implementation.
*"* method's implementations
*include methods.
  METHOD delete_documentation.
    DATA: lv_key_s TYPE dokhl-object.

    CLEAR lv_key_s.
    CALL FUNCTION 'DOCU_OBJECT_NAME_CONCATENATE'
      EXPORTING
        docu_id  = c_longtext_id_msag
        element  = iv_message_id
        addition = '   '
      IMPORTING
        object   = lv_key_s
      EXCEPTIONS
        OTHERS   = 0.

    CALL FUNCTION 'DOKU_DELETE_ALL'
      EXPORTING
        doku_id                        = c_longtext_id_msag
        doku_object                    = lv_key_s
        generic_use                    = 'X'
        suppress_authority             = space
        suppress_enqueue               = space
        suppress_transport             = space
      EXCEPTIONS
        header_without_text            = 1
        index_without_header           = 2
        no_authority_for_devclass_xxxx = 3
        no_docu_found                  = 4
        object_is_already_enqueued     = 5
        object_is_enqueued_by_corr     = 6
        user_break                     = 7.

  ENDMETHOD.
  METHOD delete_msgid.

    delete_documentation( iv_message_id ).

    DELETE FROM t100a WHERE arbgb = iv_message_id.
    IF sy-subrc = 0 OR sy-subrc = 4.
      CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
        EXPORTING
          object    = iv_message_id
          operation = 'DELETE'
          program   = space
          type      = 'CN'.
      DELETE FROM t100o WHERE arbgb = iv_message_id.
      DELETE FROM t100t WHERE arbgb = iv_message_id.    "#EC CI_NOFIRST
      DELETE FROM t100u WHERE arbgb = iv_message_id.
      DELETE FROM t100x WHERE arbgb = iv_message_id.
      DELETE FROM t100 WHERE arbgb = iv_message_id.
    ENDIF.


  ENDMETHOD.
  METHOD deserialize_texts.

    DATA: lv_msg_id     TYPE rglif-message_id,
          ls_t100       TYPE t100,
          lt_t100t      TYPE TABLE OF t100t,
          lt_t100_texts TYPE ty_t100_texts,
          lt_t100u      TYPE TABLE OF t100u.

    FIELD-SYMBOLS: <ls_t100_text> TYPE ty_t100_text.


    lv_msg_id = ms_item-obj_name.

    SELECT * FROM t100u INTO TABLE lt_t100u
      WHERE arbgb = lv_msg_id ORDER BY PRIMARY KEY.     "#EC CI_GENBUFF

    ii_xml->read( EXPORTING iv_name = 'T100_TEXTS'
                  CHANGING  cg_data = lt_t100_texts ).

    ii_xml->read( EXPORTING iv_name = 'T100T'
                  CHANGING  cg_data = lt_t100t ).

    mo_i18n_params->trim_saplang_keyed_table(
      EXPORTING
        iv_lang_field_name = 'SPRSL'
      CHANGING
        ct_tab = lt_t100_texts ).
    mo_i18n_params->trim_saplang_keyed_table(
      EXPORTING
        iv_lang_field_name = 'SPRSL'
      CHANGING
        ct_tab = lt_t100t ).

    MODIFY t100t FROM TABLE lt_t100t.                     "#EC CI_SUBRC

    LOOP AT lt_t100_texts ASSIGNING <ls_t100_text>.
      "check if message exists
      READ TABLE lt_t100u TRANSPORTING NO FIELDS
        WITH KEY arbgb = lv_msg_id msgnr = <ls_t100_text>-msgnr BINARY SEARCH.
      CHECK sy-subrc = 0. "if original message doesn't exist no translations added

      MOVE-CORRESPONDING <ls_t100_text> TO ls_t100.
      ls_t100-arbgb = lv_msg_id.
      MODIFY t100 FROM ls_t100.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'MSAG: Table T100 modify failed' ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD free_access_permission.
    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
        mode         = 'FREE'
        object       = iv_message_id
        object_class = 'T100'.
  ENDMETHOD.
  METHOD serialize_longtexts_msag.

    DATA: lv_doku_object_name  TYPE dokhl-object,
          lt_doku_object_names TYPE STANDARD TABLE OF dokhl-object
                          WITH NON-UNIQUE DEFAULT KEY,
          lt_dokil             TYPE Lif_abapgit_definitions=>ty_dokil_tt,
          ls_dokil             LIKE LINE OF lt_dokil,
          lt_language_filter   TYPE Lif_abapgit_environment=>ty_system_language_filter.

    FIELD-SYMBOLS: <ls_t100>  TYPE t100.

    IF lines( it_t100 ) = 0.
      RETURN.
    ENDIF.

    LOOP AT it_t100 ASSIGNING <ls_t100>.

      lv_doku_object_name = <ls_t100>-arbgb && <ls_t100>-msgnr.
      INSERT lv_doku_object_name INTO TABLE lt_doku_object_names.

    ENDLOOP.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      SELECT * FROM dokil
        INTO TABLE lt_dokil
        FOR ALL ENTRIES IN lt_doku_object_names
        WHERE id = c_longtext_id_msag
        AND object = lt_doku_object_names-table_line
        AND masterlang = abap_true
        ORDER BY PRIMARY KEY.
    ELSE.
      lt_language_filter = mo_i18n_params->build_language_filter( ).
      SELECT * FROM dokil
        INTO TABLE lt_dokil
        FOR ALL ENTRIES IN lt_doku_object_names
        WHERE id = c_longtext_id_msag
        AND object = lt_doku_object_names-table_line
        AND langu IN lt_language_filter
        ORDER BY PRIMARY KEY.
    ENDIF.

    CLEAR ls_dokil-dokstate.
    MODIFY lt_dokil FROM ls_dokil TRANSPORTING dokstate WHERE dokstate IS NOT INITIAL.

    IF lines( lt_dokil ) > 0.
      serialize_longtexts( ii_xml   = ii_xml
                           it_dokil = lt_dokil ).
    ENDIF.

  ENDMETHOD.
  METHOD serialize_texts.

    DATA: lv_msg_id          TYPE rglif-message_id,
          lt_t100_texts      TYPE ty_t100_texts,
          lt_t100t           TYPE TABLE OF t100t,
          lt_i18n_langs      TYPE TABLE OF langu,
          lt_language_filter TYPE Lif_abapgit_environment=>ty_system_language_filter.

    lv_msg_id = ms_item-obj_name.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      RETURN. " skip
    ENDIF.

    " Collect additional languages
    " Skip main lang - it has been already serialized and also technical languages
    lt_language_filter = mo_i18n_params->build_language_filter( ).

    SELECT DISTINCT sprsl AS langu INTO TABLE lt_i18n_langs
      FROM t100t
      WHERE arbgb = lv_msg_id
      AND sprsl IN lt_language_filter
      AND sprsl <> mv_language
      ORDER BY langu.                    "#EC CI_BYPASS "#EC CI_GENBUFF

    SORT lt_i18n_langs ASCENDING.

    IF lines( lt_i18n_langs ) > 0.

      SELECT * FROM t100t INTO CORRESPONDING FIELDS OF TABLE lt_t100t
        WHERE sprsl IN lt_language_filter
        AND sprsl <> mv_language
        AND arbgb = lv_msg_id
        ORDER BY PRIMARY KEY.                           "#EC CI_GENBUFF

      SELECT * FROM t100 INTO CORRESPONDING FIELDS OF TABLE lt_t100_texts
        WHERE sprsl IN lt_language_filter
        AND sprsl <> mv_language
        AND arbgb = lv_msg_id
        ORDER BY PRIMARY KEY.             "#EC CI_SUBRC "#EC CI_GENBUFF

      SORT lt_t100t BY sprsl ASCENDING.
      SORT lt_t100_texts BY sprsl msgnr ASCENDING.

      ii_xml->add( iv_name = 'I18N_LANGS'
                   ig_data = lt_i18n_langs ).

      ii_xml->add( iv_name = 'T100T'
                   ig_data = lt_t100t ).

      ii_xml->add( iv_name = 'T100_TEXTS'
                   ig_data = lt_t100_texts ).

    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE lastuser FROM t100a INTO rv_user
      WHERE arbgb = ms_item-obj_name.                   "#EC CI_GENBUFF
    IF sy-subrc <> 0 OR rv_user = ''.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.
    DATA: ls_t100a      TYPE t100a,
          lv_frozen     TYPE abap_bool,
          lv_message_id TYPE arbgb.

* parameter SUPPRESS_DIALOG doesnt exist in all versions of FM RS_DELETE_MESSAGE_ID
* replaced with a copy
    lv_message_id = ms_item-obj_name.
    IF ms_item-obj_name = space.
      Lcx_abapgit_exception=>raise( 'Error from (copy of) RS_DELETE_MESSAGE_ID' )."blank message id
    ENDIF.

    SELECT SINGLE * FROM t100a INTO ls_t100a WHERE arbgb = ms_item-obj_name.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Error from (copy of) RS_DELETE_MESSAGE_ID' )."not found
    ENDIF.

    CLEAR lv_frozen.
    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
        authority_check = 'X'
        global_lock     = 'X'
        mode            = 'MODIFY'
        object          = lv_message_id
        object_class    = 'T100'
      IMPORTING
        frozen          = lv_frozen
      EXCEPTIONS
        OTHERS          = 1.

    IF sy-subrc <> 0 OR lv_frozen <> space.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    Lcl_abapgit_factory=>get_cts_api( )->insert_transport_object(
      iv_object   = 'MSAG'
      iv_obj_name = lv_message_id
      iv_package  = iv_package
      iv_language = mv_language
      iv_mode     = Lif_abapgit_cts_api=>c_transport_mode-delete ).

    delete_msgid( lv_message_id ).

    free_access_permission( lv_message_id ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.
* fm RPY_MESSAGE_ID_INSERT almost works, but not in older versions

    DATA: ls_t100a  TYPE t100a,
          ls_t100t  TYPE t100t,
          ls_t100u  TYPE t100u,
          lt_t100   TYPE TABLE OF t100,
          lt_before TYPE TABLE OF t100u.

    FIELD-SYMBOLS: <ls_t100> LIKE LINE OF lt_t100.


    io_xml->read( EXPORTING iv_name = 'T100A'
                  CHANGING cg_data = ls_t100a ).
    io_xml->read( EXPORTING iv_name = 'T100'
                  CHANGING cg_data = lt_t100 ).

    corr_insert( iv_package ).

    SELECT * FROM t100u INTO TABLE lt_before
      WHERE arbgb = ls_t100a-arbgb ORDER BY msgnr. "#EC CI_GENBUFF "#EC CI_BYPASS

    LOOP AT lt_t100 ASSIGNING <ls_t100>.
      DELETE lt_before WHERE msgnr = <ls_t100>-msgnr.
      MODIFY t100 FROM <ls_t100>.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'MSAG: Table T100 modify failed' ).
      ENDIF.
      CLEAR ls_t100u.
      MOVE-CORRESPONDING <ls_t100> TO ls_t100u ##ENH_OK.
      ls_t100u-name    = sy-uname.
      ls_t100u-datum   = sy-datum.
      ls_t100u-selfdef = '3'.
      MODIFY t100u FROM ls_t100u.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'MSAG: Table T100U modify failed' ).
      ENDIF.
    ENDLOOP.

    ls_t100a-masterlang = mv_language.
    ls_t100a-lastuser = sy-uname.
    ls_t100a-respuser = sy-uname.
    ls_t100a-ldate = sy-datum.
    ls_t100a-ltime = sy-uzeit.
    MODIFY t100a FROM ls_t100a.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'MSAG: Table T100A modify failed' ).
    ENDIF.

    ls_t100t-sprsl = mv_language.
    ls_t100t-arbgb = ls_t100a-arbgb.
    ls_t100t-stext = ls_t100a-stext.
    MODIFY t100t FROM ls_t100t.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'MSAG: Table T100T modify failed' ).
    ENDIF.

    LOOP AT lt_before INTO ls_t100u.
      DELETE FROM t100 WHERE arbgb = ls_t100u-arbgb
        AND msgnr = ls_t100u-msgnr.                       "#EC CI_SUBRC

      DELETE FROM t100u WHERE arbgb = ls_t100u-arbgb
        AND msgnr = ls_t100u-msgnr.                       "#EC CI_SUBRC
    ENDLOOP.

    deserialize_longtexts( ii_xml         = io_xml
                           iv_longtext_id = c_longtext_id_msag ).

    IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
      deserialize_texts( io_xml ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_arbgb TYPE t100a-arbgb.


    SELECT SINGLE arbgb FROM t100a INTO lv_arbgb
      WHERE arbgb = ms_item-obj_name.                   "#EC CI_GENBUFF
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

    DATA: lv_argument TYPE seqg3-garg.

    lv_argument   = |{ ms_item-obj_name }|.
    OVERLAY lv_argument WITH '                     '.
    lv_argument = lv_argument && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = |ES_MSGSI|
                                            iv_argument    = lv_argument ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_msg_id TYPE rglif-message_id,
          ls_inf    TYPE t100a,
          lt_source TYPE ty_t100s.


    lv_msg_id = ms_item-obj_name.

    SELECT SINGLE * FROM t100a INTO ls_inf
      WHERE arbgb = lv_msg_id.                          "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    CLEAR ls_inf-respuser.

    SELECT * FROM t100 INTO TABLE lt_source
      WHERE sprsl = mv_language
      AND arbgb = lv_msg_id
      ORDER BY PRIMARY KEY.               "#EC CI_SUBRC "#EC CI_GENBUFF

    CLEAR: ls_inf-lastuser,
           ls_inf-ldate,
           ls_inf-ltime.

    io_xml->add( iv_name = 'T100A'
                 ig_data = ls_inf ).
    io_xml->add( ig_data = lt_source
                 iv_name = 'T100' ).

    serialize_longtexts_msag( it_t100 = lt_source
                              ii_xml  = io_xml ).

    IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
      serialize_texts( io_xml ).
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
endclass. "ZCL_ABAPGIT_OBJECT_MSAG implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_NSPC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_nspc=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_nspc=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_NSPC implementation.
*"* method's implementations
*include methods.
  METHOD add_to_transport.

    DATA: li_sap_package TYPE REF TO Lif_abapgit_sap_package.

    li_sap_package = Lcl_abapgit_factory=>get_sap_package( iv_package ).

    IF li_sap_package->are_changes_recorded_in_tr_req( ) = abap_true.
      corr_insert( iv_package ).
    ENDIF.

  ENDMETHOD.
  METHOD deserialize_texts.

    DATA:
      ls_trnspacett TYPE trnspacett,
      lt_i18n_langs TYPE TABLE OF langu,
      lt_nspc_texts TYPE ty_nspc_texts.

    FIELD-SYMBOLS:
      <lv_lang>      LIKE LINE OF lt_i18n_langs,
      <ls_nspc_text> LIKE LINE OF lt_nspc_texts.

    ii_xml->read( EXPORTING iv_name = 'I18N_LANGS'
                  CHANGING  cg_data = lt_i18n_langs ).

    ii_xml->read( EXPORTING iv_name = 'NSPC_TEXTS'
                  CHANGING  cg_data = lt_nspc_texts ).

    SORT lt_i18n_langs.
    SORT lt_nspc_texts BY spras. " Optimization

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.
      ls_trnspacett-namespace = iv_namespace.
      READ TABLE lt_nspc_texts ASSIGNING <ls_nspc_text> WITH KEY spras = <lv_lang>.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |NSPC_TEXTS cannot find lang { <lv_lang> } in XML| ).
      ENDIF.
      MOVE-CORRESPONDING <ls_nspc_text> TO ls_trnspacett.

      MODIFY trnspacett FROM ls_trnspacett.
      IF sy-subrc <> 0.
        INSERT trnspacett FROM ls_trnspacett.
      ENDIF.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Error upserting text for namespace| ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD serialize_texts.

    DATA:
      ls_trnspacett TYPE trnspacett,
      lt_nspc_texts TYPE ty_nspc_texts,
      lt_i18n_langs TYPE TABLE OF langu.

    FIELD-SYMBOLS:
      <lv_lang>      LIKE LINE OF lt_i18n_langs,
      <ls_nspc_text> LIKE LINE OF lt_nspc_texts.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      RETURN.
    ENDIF.

    " Collect additional languages, skip main lang - it was serialized already
    SELECT DISTINCT spras AS langu FROM trnspacett INTO TABLE lt_i18n_langs
      WHERE namespace = ms_item-obj_name AND spras <> mv_language
      ORDER BY langu.                                     "#EC CI_SUBRC

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.
      SELECT SINGLE * FROM trnspacett INTO ls_trnspacett
        WHERE namespace = ms_item-obj_name AND spras = <lv_lang>.
      IF sy-subrc = 0.
        APPEND INITIAL LINE TO lt_nspc_texts ASSIGNING <ls_nspc_text>.
        MOVE-CORRESPONDING ls_trnspacett TO <ls_nspc_text>.
      ENDIF.
    ENDLOOP.

    SORT lt_i18n_langs ASCENDING.
    SORT lt_nspc_texts BY spras ASCENDING.

    IF lines( lt_i18n_langs ) > 0.
      ii_xml->add( iv_name = 'I18N_LANGS'
                   ig_data = lt_i18n_langs ).

      ii_xml->add( iv_name = 'NSPC_TEXTS'
                   ig_data = lt_nspc_texts ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    SELECT SINGLE changeuser FROM trnspacet INTO rv_user
       WHERE namespace = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.
    RETURN. " not supported
  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA:
      ls_nspc       TYPE ty_nspc,
      ls_nspc_text  TYPE ty_nspc_text,
      lv_modifiable TYPE abap_bool,
      ls_trnspacet  TYPE trnspacet,
      ls_trnspacett TYPE trnspacett.

    io_xml->read( EXPORTING iv_name = 'NSPC'
                  CHANGING  cg_data = ls_nspc ).

    io_xml->read( EXPORTING iv_name = 'NSPC_TEXT'
                  CHANGING  cg_data = ls_nspc_text ).

    add_to_transport( iv_package ).

    SELECT SINGLE * FROM trnspacet INTO ls_trnspacet WHERE namespace = ls_nspc-namespace.
    IF sy-subrc = 0.
      " For existing namespace, check if it's modifiable (SE03)
      SELECT SINGLE editflag FROM trnspace INTO lv_modifiable WHERE namespace = ls_nspc-namespace.
      IF sy-subrc = 0 AND lv_modifiable = abap_false.
        Lcx_abapgit_exception=>raise( |Namespace is not modifiable| ).
      ENDIF.

      " keep existing role
      ls_trnspacet-replicense = ls_nspc-replicense.
      ls_trnspacet-sscrflag   = ls_nspc-sscrflag.
      ls_trnspacet-sapflag    = ls_nspc-sapflag.
      ls_trnspacet-gen_only   = ls_nspc-gen_only.
      ls_trnspacet-changeuser = sy-uname.
      ls_trnspacet-changedate = sy-datum.
      MODIFY trnspacet FROM ls_trnspacet.
    ELSE.
      MOVE-CORRESPONDING ls_nspc TO ls_trnspacet.
      ls_trnspacet-role       = 'C'. " customer repair license
      ls_trnspacet-changeuser = sy-uname.
      ls_trnspacet-changedate = sy-datum.
      INSERT trnspacet FROM ls_trnspacet.
    ENDIF.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error upserting namespace| ).
    ENDIF.

    SELECT SINGLE * FROM trnspacett INTO ls_trnspacett
      WHERE namespace = ls_nspc-namespace AND spras = mv_language.
    IF sy-subrc = 0.
      ls_trnspacett-descriptn = ls_nspc_text-descriptn.
      ls_trnspacett-owner     = ls_nspc_text-owner.
      MODIFY trnspacett FROM ls_trnspacett.
    ELSE.
      MOVE-CORRESPONDING ls_nspc_text TO ls_trnspacett.
      ls_trnspacett-namespace = ls_nspc-namespace.
      INSERT trnspacett FROM ls_trnspacett.
    ENDIF.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error upserting text for namespace| ).
    ENDIF.

    deserialize_texts( ii_xml       = io_xml
                       iv_namespace = ls_nspc-namespace ).

    " Fill trnspace and trnspacel tables
    CALL FUNCTION 'TR_ACTIVATE_NAMESPACE'
      EXPORTING
        iv_namespace         = ls_nspc-namespace
      EXCEPTIONS
        deletion_not_allowed = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error activating namespace| ).
    ENDIF.

    " Make namespace modifiable
    UPDATE trnspace SET editflag = abap_true WHERE namespace = ls_nspc-namespace.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA lv_namespace TYPE trnspace-namespace.

    lv_namespace = ms_item-obj_name.

    CALL FUNCTION 'TR_CHECK_NAMESPACE'
      EXPORTING
        iv_namespace        = lv_namespace
      EXCEPTIONS
        namespace_not_valid = 1
        OTHERS              = 2.

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
    rv_active = Lif_abapgit_object~exists( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Launch general maintenance for namespaces
    CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
      EXPORTING
        action                       = 'S'
        view_name                    = 'V_TRNSPACE'
        no_warning_for_clientindep   = 'X'
        variant_for_selection        = 'STANDARD'
      EXCEPTIONS
        client_reference             = 1
        foreign_lock                 = 2
        invalid_action               = 3
        no_clientindependent_auth    = 4
        no_database_function         = 5
        no_editor_function           = 6
        no_show_auth                 = 7
        no_tvdir_entry               = 8
        no_upd_auth                  = 9
        only_show_allowed            = 10
        system_failure               = 11
        unknown_field_in_dba_sellist = 12
        view_not_found               = 13
        OTHERS                       = 14.

    rv_exit = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA:
      ls_nspc      TYPE ty_nspc,
      ls_nspc_text TYPE ty_nspc_text.

    SELECT SINGLE * FROM trnspacet INTO CORRESPONDING FIELDS OF ls_nspc
      WHERE namespace = ms_item-obj_name.

    SELECT SINGLE * FROM trnspacett INTO CORRESPONDING FIELDS OF ls_nspc_text
      WHERE namespace = ms_item-obj_name AND spras = mv_language.

    io_xml->add( iv_name = 'NSPC'
                 ig_data = ls_nspc ).

    io_xml->add( iv_name = 'NSPC_TEXT'
                 ig_data = ls_nspc_text ).

    serialize_texts( io_xml ).

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
endclass. "ZCL_ABAPGIT_OBJECT_NSPC implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SICF <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sicf=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sicf=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SICF implementation.
*"* method's implementations
*include methods.
  METHOD change_sicf.

    DATA: lt_icfhndlist TYPE icfhndlist,
          lt_existing   TYPE TABLE OF icfhandler,
          ls_icfserdesc TYPE icfserdesc.

    FIELD-SYMBOLS: <ls_existing> LIKE LINE OF lt_existing.


    lt_icfhndlist = to_icfhndlist( it_icfhandler ).

* Do not add handlers if they already exist, it will make the below
* call to SAP standard code raise an exception
    SELECT * FROM icfhandler INTO TABLE lt_existing
      WHERE icf_name = is_icfservice-icf_name
      ORDER BY PRIMARY KEY.
    LOOP AT lt_existing ASSIGNING <ls_existing>.
      DELETE TABLE lt_icfhndlist FROM <ls_existing>-icfhandler.
    ENDLOOP.

    MOVE-CORRESPONDING is_icfservice TO ls_icfserdesc.

    cl_icf_tree=>if_icf_tree~change_node(
      EXPORTING
        icf_name                  = is_icfservice-orig_name
        icfparguid                = iv_parent
        icfdocu                   = is_icfdocu
        doculang                  = mv_language
        icfhandlst                = lt_icfhndlist
        package                   = iv_package
        application               = space
        icfserdesc                = ls_icfserdesc
        icfactive                 = abap_true
      EXCEPTIONS
        empty_icf_name            = 1
        no_new_virtual_host       = 2
        special_service_error     = 3
        parent_not_existing       = 4
        enqueue_error             = 5
        node_already_existing     = 6
        empty_docu                = 7
        doculang_not_installed    = 8
        security_info_error       = 9
        user_password_error       = 10
        password_encryption_error = 11
        invalid_url               = 12
        invalid_otr_concept       = 13
        formflg401_error          = 14
        handler_error             = 15
        transport_error           = 16
        tadir_error               = 17
        package_not_found         = 18
        wrong_application         = 19
        not_allow_application     = 20
        no_application            = 21
        invalid_icfparguid        = 22
        alt_name_invalid          = 23
        alternate_name_exist      = 24
        wrong_icf_name            = 25
        no_authority              = 26
        OTHERS                    = 27 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD find_parent.

    cl_icf_tree=>if_icf_tree~service_from_url(
      EXPORTING
        url                   = iv_url
        hostnumber            = 0
      IMPORTING
        icfnodguid            = rv_parent
      EXCEPTIONS
        wrong_application     = 1
        no_application        = 2
        not_allow_application = 3
        wrong_url             = 4
        no_authority          = 5
        OTHERS                = 6 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD insert_sicf.

    DATA: lt_icfhndlist TYPE icfhndlist,
          ls_icfserdesc TYPE icfserdesc,
          ls_icfdocu    TYPE icfdocu,
          lv_icfnodguid TYPE icfnodguid,
          lv_parent     TYPE icfparguid.


    lt_icfhndlist = to_icfhndlist( it_icfhandler ).
    lv_parent = find_parent( iv_url ).

* nice, it seems that the structure should be mistreated
    ls_icfdocu = is_icfdocu-icf_docu.

    MOVE-CORRESPONDING is_icfservice TO ls_icfserdesc.

    cl_icf_tree=>if_icf_tree~insert_node(
      EXPORTING
        icf_name                  = is_icfservice-orig_name
        icfparguid                = lv_parent
        icfdocu                   = ls_icfdocu
        doculang                  = mv_language
        icfhandlst                = lt_icfhndlist
        package                   = iv_package
        application               = space
        icfserdesc                = ls_icfserdesc
        icfactive                 = abap_true
        icfaltnme                 = is_icfservice-icfaltnme
      IMPORTING
        icfnodguid                = lv_icfnodguid
      EXCEPTIONS
        empty_icf_name            = 1
        no_new_virtual_host       = 2
        special_service_error     = 3
        parent_not_existing       = 4
        enqueue_error             = 5
        node_already_existing     = 6
        empty_docu                = 7
        doculang_not_installed    = 8
        security_info_error       = 9
        user_password_error       = 10
        password_encryption_error = 11
        invalid_url               = 12
        invalid_otr_concept       = 13
        formflg401_error          = 14
        handler_error             = 15
        transport_error           = 16
        tadir_error               = 17
        package_not_found         = 18
        wrong_application         = 19
        not_allow_application     = 20
        no_application            = 21
        invalid_icfparguid        = 22
        alt_name_invalid          = 23
        alternate_name_exist      = 24
        wrong_icf_name            = 25
        no_authority              = 26
        OTHERS                    = 27 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    " Update item with name assigned by system
    SELECT SINGLE icfparguid INTO ms_item-obj_name+15 FROM icfservice
      WHERE icfnodguid = lv_icfnodguid.

  ENDMETHOD.
  METHOD read.

    DATA: lt_serv_info TYPE icfservtbl,
          ls_serv_info LIKE LINE OF lt_serv_info,
          ls_key       TYPE ty_sicf_key.

    FIELD-SYMBOLS: <ls_icfhandler> LIKE LINE OF et_icfhandler.


    CLEAR es_icfservice.
    CLEAR es_icfdocu.
    CLEAR et_icfhandler.
    CLEAR ev_url.

    ls_key-icf_name   = ms_item-obj_name(15).
    ls_key-icfparguid = ms_item-obj_name+15.

    cl_icf_tree=>if_icf_tree~get_info_from_serv(
      EXPORTING
        icf_name          = ls_key-icf_name
        icfparguid        = ls_key-icfparguid
        icf_langu         = mv_language
      IMPORTING
        serv_info         = lt_serv_info
        icfdocu           = es_icfdocu
        url               = ev_url
      EXCEPTIONS
        wrong_name        = 1
        wrong_parguid     = 2
        incorrect_service = 3
        no_authority      = 4
        OTHERS            = 5 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    ASSERT lines( lt_serv_info ) = 1.
    READ TABLE lt_serv_info INDEX 1 INTO ls_serv_info.
    ASSERT sy-subrc = 0.

    MOVE-CORRESPONDING ls_serv_info-service TO es_icfservice.
    IF iv_clear = abap_true.
      CLEAR es_icfservice-icf_cuser.
      CLEAR es_icfservice-icf_cdate.
      CLEAR es_icfservice-icf_muser.
      CLEAR es_icfservice-icf_mdate.
    ENDIF.

    CLEAR es_icfdocu-icfparguid.

    APPEND LINES OF ls_serv_info-handlertbl TO et_icfhandler.
    LOOP AT et_icfhandler ASSIGNING <ls_icfhandler>.
      CLEAR <ls_icfhandler>-icfparguid.
    ENDLOOP.

  ENDMETHOD.
  METHOD to_icfhndlist.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF it_list.


    " Convert to sorted table
    LOOP AT it_list ASSIGNING <ls_list>.
      INSERT <ls_list>-icfhandler INTO TABLE rt_list.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: ls_icfservice TYPE icfservice.


    read( EXPORTING iv_clear = abap_false
          IMPORTING es_icfservice = ls_icfservice ).

    rv_user = ls_icfservice-icf_muser.

    IF rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA ls_icfservice TYPE icfservice.

    read( IMPORTING es_icfservice = ls_icfservice ).

    IF ls_icfservice IS INITIAL.
      " It seems that the ICF service doesn't exist anymore.
      " But that's ok, because some objects like SAPC manage
      " the lifecycle of its ICF service by itself and already
      " deleted the service.
      RETURN.
    ENDIF.

    IF ls_icfservice-icfparguid CO '0'.
      " not supported by the SAP standard API
      Lcx_abapgit_exception=>raise( 'SICF - cannot delete root node, delete node manually' ).
    ENDIF.

    " OTR long texts
    Lcl_abapgit_sots_handler=>delete_sots(
      iv_object   = ms_item-obj_type
      iv_obj_name = ms_item-obj_name ).

    " Delete Application Customizing Data the hard way, as it isn't done by the API.
    " If we wouldn't we would get errors from the API if entrys exist.
    " Transaction SICF does the same.
    DELETE FROM icfapplcust
      WHERE icf_name = ls_icfservice-icf_name
      AND icfparguid = ls_icfservice-icfparguid.

    cl_icf_tree=>if_icf_tree~delete_node(
      EXPORTING
        icfparguid                  = ls_icfservice-icfparguid
      CHANGING
        icf_name                    = ls_icfservice-icf_name
      EXCEPTIONS
        no_virtual_host_delete      = 1
        special_service_error       = 2
        enqueue_error               = 3
        node_not_existing           = 4
        node_has_childs             = 5
        node_is_aliased             = 6
        node_not_in_original_system = 7
        transport_error             = 8
        tadir_error                 = 9
        db_error                    = 10
        no_authority                = 11
        OTHERS                      = 12 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_icfservice TYPE icfservice,
          ls_read       TYPE icfservice,
          ls_icfdocu    TYPE icfdocu,
          lv_url        TYPE string,
          lv_exists     TYPE abap_bool,
          lt_icfhandler TYPE TABLE OF icfhandler.

    io_xml->read( EXPORTING iv_name = 'URL'
                  CHANGING cg_data = lv_url ).
    io_xml->read( EXPORTING iv_name = 'ICFSERVICE'
                  CHANGING cg_data = ls_icfservice ).
    io_xml->read( EXPORTING iv_name = 'ICFDOCU'
                  CHANGING cg_data = ls_icfdocu ).
    io_xml->read( EXPORTING iv_name = 'ICFHANDLER_TABLE'
                  CHANGING cg_data = lt_icfhandler ).

    lv_exists = Lif_abapgit_object~exists( ).
    IF lv_exists = abap_false.
      insert_sicf( is_icfservice = ls_icfservice
                   is_icfdocu    = ls_icfdocu
                   it_icfhandler = lt_icfhandler
                   iv_package    = iv_package
                   iv_url        = lv_url ).
    ELSE.
      read( IMPORTING es_icfservice = ls_read ).
      change_sicf( is_icfservice = ls_icfservice
                   is_icfdocu    = ls_icfdocu
                   it_icfhandler = lt_icfhandler
                   iv_package    = iv_package
                   iv_parent     = ls_read-icfparguid ).
    ENDIF.

    " OTR long texts
    deserialize_otr(
      iv_package = iv_package
      io_xml     = io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA ls_key TYPE ty_sicf_key.

    SELECT SINGLE icfaltnme FROM icfservice INTO ls_key-icf_name
      WHERE icf_name = ms_item-obj_name(15)
      AND icfparguid = ms_item-obj_name+15.
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

    DATA: lv_argument TYPE seqg3-garg.

    lv_argument = ms_item-obj_name(15).
    lv_argument+15(1) = '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESICFSER'
                                            iv_argument    = lv_argument ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: ls_bcdata TYPE bdcdata,
          lt_bcdata TYPE STANDARD TABLE OF bdcdata.

    ls_bcdata-program  = 'RSICFTREE'.
    ls_bcdata-dynpro   = '1000'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    ls_bcdata-dynpro   = space.
    ls_bcdata-dynbegin = space.
    ls_bcdata-fnam     = 'ICF_SERV'.
    ls_bcdata-fval     = ms_item-obj_name.
    APPEND ls_bcdata TO lt_bcdata.

    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=ONLI'.
    APPEND ls_bcdata TO lt_bcdata.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SICF'
      it_bdcdata = lt_bcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_icfservice TYPE icfservice,
          ls_icfdocu    TYPE icfdocu,
          lv_url        TYPE string,
          lt_icfhandler TYPE TABLE OF icfhandler.

    read( IMPORTING es_icfservice = ls_icfservice
                    es_icfdocu    = ls_icfdocu
                    et_icfhandler = lt_icfhandler
                    ev_url        = lv_url ).

    IF ls_icfservice IS INITIAL.
      RETURN.
    ENDIF.

    CLEAR ls_icfservice-icf_mandt.
    CLEAR ls_icfservice-icfnodguid.
    CLEAR ls_icfservice-icfparguid.
    CLEAR ls_icfservice-icfchildno.
    CLEAR ls_icfservice-icfaliasno.
    CLEAR ls_icfservice-icf_user.
    CLEAR ls_icfservice-icf_cclnt.
    CLEAR ls_icfservice-icf_mclnt.
    CLEAR ls_icfservice-icfaltnme_orig.
    CLEAR ls_icfservice-icfbitmap.

    io_xml->add( iv_name = 'URL'
                 ig_data = lv_url ).
    io_xml->add( iv_name = 'ICFSERVICE'
                 ig_data = ls_icfservice ).
    io_xml->add( iv_name = 'ICFDOCU'
                 ig_data = ls_icfdocu ).
    io_xml->add( iv_name = 'ICFHANDLER_TABLE'
                 ig_data = lt_icfhandler ).

    " OTR long texts
    serialize_otr( io_xml ).

  ENDMETHOD.
  METHOD deserialize_otr.

    DATA:
      lt_sots     TYPE Lcl_abapgit_sots_handler=>ty_sots_tt,
      lt_sots_use TYPE Lcl_abapgit_sots_handler=>ty_sots_use_tt.

    FIELD-SYMBOLS:
      <ls_sots_use> LIKE LINE OF lt_sots_use.

    io_xml->read( EXPORTING iv_name = 'SOTS'
                  CHANGING cg_data = lt_sots ).
    io_xml->read( EXPORTING iv_name = 'SOTS_USE'
                  CHANGING cg_data = lt_sots_use ).

    LOOP AT lt_sots_use ASSIGNING <ls_sots_use>.
      <ls_sots_use>-obj_name = ms_item-obj_name.
    ENDLOOP.

    Lcl_abapgit_sots_handler=>create_sots_from_data(
      iv_package  = iv_package
      it_sots     = lt_sots
      it_sots_use = lt_sots_use ).

  ENDMETHOD.
  METHOD serialize_otr.

    DATA:
      lt_sots     TYPE Lcl_abapgit_sots_handler=>ty_sots_tt,
      lt_sots_use TYPE Lcl_abapgit_sots_handler=>ty_sots_use_tt.

    FIELD-SYMBOLS:
      <ls_sots_use> LIKE LINE OF lt_sots_use.

    Lcl_abapgit_sots_handler=>read_sots(
      EXPORTING
        iv_object   = ms_item-obj_type
        iv_obj_name = ms_item-obj_name
        io_i18n_params = mo_i18n_params
      IMPORTING
        et_sots     = lt_sots
        et_sots_use = lt_sots_use ).

    LOOP AT lt_sots_use ASSIGNING <ls_sots_use>.
      CLEAR <ls_sots_use>-obj_name.
    ENDLOOP.

    io_xml->add( iv_name = 'SOTS'
                 ig_data = lt_sots ).
    io_xml->add( iv_name = 'SOTS_USE'
                 ig_data = lt_sots_use ).

  ENDMETHOD.
  METHOD get_hash_from_object.

    DATA:
      lv_icfnodguid TYPE icfservice-icfnodguid,
      lv_url        TYPE icfurlbuf,
      lv_ext_url    TYPE string.

    SELECT SINGLE icfnodguid FROM icfservice INTO lv_icfnodguid
      WHERE icf_name = iv_obj_name(15)
      AND icfparguid = iv_obj_name+15.

    IF sy-subrc = 0.
      CALL FUNCTION 'HTTP_GET_URL_FROM_NODGUID'
        EXPORTING
          nodguid      = lv_icfnodguid
        IMPORTING
          url          = lv_url
          extended_url = lv_ext_url
        EXCEPTIONS
          icf_inconst  = 1
          OTHERS       = 2.
      IF sy-subrc = 0.
        " It's possible that the URL contains the system id, for example for WD applications with names
        " longer than 15 characters. In that case, use the extended URL to generate the hash (#5064)
        IF lv_ext_url <> lv_url.
          rv_hash = Lcl_abapgit_hash=>sha1_raw( Lcl_abapgit_convert=>string_to_xstring_utf8( |{ lv_ext_url }| ) ).
        ELSE.
          rv_hash = Lcl_abapgit_hash=>sha1_raw( Lcl_abapgit_convert=>string_to_xstring_utf8( |{ lv_url }| ) ).
        ENDIF.
      ENDIF.
    ELSE.
      rv_hash = to_lower( iv_obj_name+15 ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.

    DATA:
      lt_tadir    TYPE Lif_abapgit_definitions=>ty_tadir_tt,
      lv_hash     TYPE ty_hash,
      lv_obj_name TYPE tadir-obj_name.

    FIELD-SYMBOLS <ls_tadir> LIKE LINE OF lt_tadir.

    lv_obj_name = to_upper( iv_filename(15) ) && '%'.
    lv_hash     = iv_filename+15(25).

    SELECT * FROM tadir INTO CORRESPONDING FIELDS OF TABLE lt_tadir
      WHERE pgmid = 'R3TR'
      AND object  = 'SICF'
      AND obj_name LIKE lv_obj_name
      ORDER BY PRIMARY KEY ##TOO_MANY_ITAB_FIELDS.      "#EC CI_GENBUFF

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      IF get_hash_from_object( <ls_tadir>-obj_name ) = lv_hash.
        cs_item-obj_name = <ls_tadir>-obj_name.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.

    DATA:
      lv_rest     TYPE string,
      lv_old_name TYPE string.

    SPLIT cv_filename AT '.' INTO lv_old_name lv_rest.
    cv_filename = |{ cv_filename(15) }{ get_hash_from_object( is_item-obj_name ) }.{ lv_rest }|.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SICF implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SOTS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sots=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sots=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SOTS implementation.
*"* method's implementations
*include methods.
  METHOD create_sots.

    " Reimplementation of SOTR_STRING_CREATE_CONCEPT because we can't supply
    " concept and it would then be generated.

    DATA: lv_subrc                 TYPE sy-subrc,
          lv_source_langu          TYPE spras,
          ls_header                TYPE btfr_head,
          lv_flag_is_string        TYPE btfr_flag VALUE abap_true,
          lt_text_tab              TYPE sotr_text_tt,
          lv_concept_default       TYPE sotr_conc,
          lt_entries               TYPE sotr_textl_tt,
          lv_concept               LIKE is_sots-header-concept,
          lv_flag_correction_entry TYPE abap_bool VALUE abap_true.

    lt_entries = is_sots-entries.

    ls_header-paket          = iv_package.
    ls_header-crea_lan       = mv_language.
    ls_header-alias_name     = is_sots-header-alias_name.
    lv_source_langu          = mv_language.
    lv_concept               = is_sots-header-concept.

    PERFORM btfr_create
      IN PROGRAM saplsotr_db_string
      USING iv_object
            lv_source_langu
            lv_flag_correction_entry
            lv_flag_is_string
      CHANGING lt_text_tab
               lt_entries
               ls_header
               lv_concept
               lv_concept_default
               lv_subrc.

    CASE lv_subrc.
      WHEN 1.
        Lcx_abapgit_exception=>raise( |No entry found| ).
      WHEN 2.
        Lcx_abapgit_exception=>raise( |OTR concept not found| ).
      WHEN 3.
        Lcx_abapgit_exception=>raise( |Enter a permitted object type| ).
      WHEN 4.
        "The concept will be created in the non-original system (not an error)
        RETURN.
      WHEN 5.
        Lcx_abapgit_exception=>raise( |Invalid alias| ).
      WHEN 6.
        Lcx_abapgit_exception=>raise( |No correction entry has been created| ).
      WHEN 7.
        Lcx_abapgit_exception=>raise( |Error in database operation| ).
      WHEN 9.
        Lcx_abapgit_exception=>raise( |Action canceled by user| ).
    ENDCASE.

  ENDMETHOD.
  METHOD get_raw_text_filename.

    DATA lv_langu TYPE string.

    " Lower case language codes can cause duplicate filenames therefore add suffix to make them unique
    " Note: Using ISO code would be better but is not compatible with existing files
    lv_langu = is_entry-langu.
    IF lv_langu = to_lower( lv_langu ).
      lv_langu = lv_langu && '-'.
    ENDIF.

    rv_filename =
        to_lower( |{ is_entry-concept }_|
               && |{ lv_langu         }_|
               && |{ is_entry-object  }_|
               && |{ is_entry-lfd_num }| ).

  ENDMETHOD.
  METHOD read_sots.

    DATA: lt_sotr_head TYPE STANDARD TABLE OF sotr_headu,
          lt_objects   TYPE sotr_objects,
          lv_object    LIKE LINE OF lt_objects,
          ls_sots      LIKE LINE OF rt_sots.

    FIELD-SYMBOLS: <ls_sotr_head> TYPE sotr_head,
                   <ls_entry>     LIKE LINE OF ls_sots-entries.


    SELECT * FROM sotr_headu
             INTO TABLE lt_sotr_head
             WHERE paket = ms_item-obj_name
             ORDER BY PRIMARY KEY.

    LOOP AT lt_sotr_head ASSIGNING <ls_sotr_head>.

      CALL FUNCTION 'SOTR_OBJECT_GET_OBJECTS'
        EXPORTING
          object_vector    = <ls_sotr_head>-objid_vec
        IMPORTING
          objects          = lt_objects
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      READ TABLE lt_objects INDEX 1 INTO lv_object.
      ASSERT sy-subrc = 0.

      " Handled by object serializer
      CHECK lv_object <> 'SICF' AND lv_object <> 'CPUB'.

      CLEAR: ls_sots.

      CALL FUNCTION 'SOTR_STRING_GET_CONCEPT'
        EXPORTING
          concept        = <ls_sotr_head>-concept
        IMPORTING
          header         = ls_sots-header
          entries        = ls_sots-entries
        EXCEPTIONS
          no_entry_found = 1
          OTHERS         = 2.

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CLEAR:
        ls_sots-header-paket,
        ls_sots-header-crea_name,
        ls_sots-header-crea_tstut,
        ls_sots-header-chan_name,
        ls_sots-header-chan_tstut.

      LOOP AT ls_sots-entries ASSIGNING <ls_entry>.
        CLEAR: <ls_entry>-version,
               <ls_entry>-crea_name,
               <ls_entry>-crea_tstut,
               <ls_entry>-chan_name,
               <ls_entry>-chan_tstut.
      ENDLOOP.

      INSERT ls_sots INTO TABLE rt_sots.

    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    SELECT SINGLE chan_name FROM sotr_headu INTO rv_user
      WHERE paket = ms_item-obj_name.                   "#EC CI_NOORDER
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lt_sots TYPE ty_sots_tt.

    FIELD-SYMBOLS: <ls_sots> TYPE ty_sots.

    lt_sots = read_sots( ).

    LOOP AT lt_sots ASSIGNING <ls_sots>.
      " Remove any usage to ensure deletion, see function module BTFR_CHECK
      DELETE FROM sotr_useu WHERE concept = <ls_sots>-header-concept.

      CALL FUNCTION 'BTFR_DELETE_SINGLE_TEXT'
        EXPORTING
          concept             = <ls_sots>-header-concept
          flag_string         = abap_true
        EXCEPTIONS
          text_not_found      = 1
          invalid_package     = 2
          text_not_changeable = 3
          text_enqueued       = 4
          no_correction       = 5
          parameter_error     = 6
          OTHERS              = 7.

      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lt_sots    TYPE ty_sots_tt,
          lt_objects TYPE sotr_objects,
          lv_object  LIKE LINE OF lt_objects.

    FIELD-SYMBOLS: <ls_sots>  TYPE ty_sots,
                   <ls_entry> LIKE LINE OF <ls_sots>-entries.

    io_xml->read(
      EXPORTING
        iv_name = 'SOTS'
      CHANGING
        cg_data = lt_sots ).

    tadir_insert( iv_package ).

    LOOP AT lt_sots ASSIGNING <ls_sots>.

      CLEAR: lt_objects.

      CALL FUNCTION 'SOTR_OBJECT_GET_OBJECTS'
        EXPORTING
          object_vector    = <ls_sots>-header-objid_vec
        IMPORTING
          objects          = lt_objects
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.

      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'error from SOTR_OBJECT_GET_OBJECTS' ).
      ENDIF.

      READ TABLE lt_objects INDEX 1 INTO lv_object.
      ASSERT sy-subrc = 0.

      LOOP AT <ls_sots>-entries ASSIGNING <ls_entry>.

        TRY.
            <ls_entry>-text = Lif_abapgit_object~mo_files->read_string(
              iv_extra = get_raw_text_filename( <ls_entry> )
              iv_ext   = 'txt' ).

          CATCH Lcx_abapgit_exception.
            " Most probably file not found -> ignore
            CONTINUE.
        ENDTRY.

      ENDLOOP.

      create_sots(
          is_sots    = <ls_sots>
          iv_package = iv_package
          iv_object  = lv_object ).

    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_object_type TYPE trobjtype,
          lv_object_name TYPE trobj_name.

    lv_object_type = ms_item-obj_type.
    lv_object_name = ms_item-obj_name.

    CALL FUNCTION 'SOTR_WBO_OBJECTS_CHECK'
      EXPORTING
        pgmid          = 'R3TR'
        object         = lv_object_type
        obj_name       = lv_object_name
      IMPORTING
        object_exist   = rv_bool
      EXCEPTIONS
        unknown_object = 1
        OTHERS         = 2.

    IF sy-subrc <> 0.
      rv_bool = abap_false.
    ENDIF.

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

    DATA: lt_sots TYPE ty_sots_tt.

    FIELD-SYMBOLS: <ls_sots>  TYPE ty_sots,
                   <ls_entry> TYPE sotr_textl.

    lt_sots = read_sots( ).

    LOOP AT lt_sots ASSIGNING <ls_sots>.

      LOOP AT <ls_sots>-entries ASSIGNING <ls_entry>.

        Lif_abapgit_object~mo_files->add_string(
          iv_extra  = get_raw_text_filename( <ls_entry> )
          iv_ext    = 'txt'
          iv_string = <ls_entry>-text ).

        CLEAR: <ls_entry>-text.

      ENDLOOP.

    ENDLOOP.

    io_xml->add( iv_name = 'SOTS'
                 ig_data = lt_sots ).

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
endclass. "ZCL_ABAPGIT_OBJECT_SOTS implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SSFO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ssfo=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ssfo=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SSFO implementation.
*"* method's implementations
*include methods.
  METHOD sort_texts.

    DATA: li_node      TYPE REF TO if_ixml_node,
          li_item      TYPE REF TO if_ixml_node,
          li_field     TYPE REF TO if_ixml_node,
          li_item_list TYPE REF TO if_ixml_node_list,
          li_iterator  TYPE REF TO if_ixml_node_iterator,
          li_items     TYPE REF TO if_ixml_node_iterator,
          lv_index     TYPE i,
          ls_item      TYPE stxfobjt,
          lt_items     TYPE STANDARD TABLE OF stxfobjt.

    FIELD-SYMBOLS <lv_field> TYPE any.

    li_iterator = ii_xml_doc->create_iterator( ).
    li_node = li_iterator->get_next( ).
    WHILE NOT li_node IS INITIAL.
      IF li_node->get_name( ) = 'T_CAPTION'.

        " Read all records for T_CAPTION
        CLEAR lt_items.
        li_item_list = li_node->get_children( ).
        li_items = li_item_list->create_iterator( ).
        DO.
          li_item = li_items->get_next( ).
          IF li_item IS INITIAL.
            EXIT.
          ENDIF.
          CLEAR ls_item.
          li_field = li_item->get_first_child( ).
          WHILE NOT li_field IS INITIAL.
            ASSIGN COMPONENT li_field->get_name( ) OF STRUCTURE ls_item TO <lv_field>.
            ASSERT sy-subrc = 0.
            <lv_field> = li_field->get_value( ).
            li_field = li_field->get_next( ).
          ENDWHILE.
          INSERT ls_item INTO TABLE lt_items.
        ENDDO.

        SORT lt_items.

        " Write all records back after sorting
        lv_index = 1.
        li_items = li_item_list->create_iterator( ).
        DO.
          li_item = li_items->get_next( ).
          IF li_item IS INITIAL.
            EXIT.
          ENDIF.
          READ TABLE lt_items INTO ls_item INDEX lv_index.
          li_field = li_item->get_first_child( ).
          WHILE NOT li_field IS INITIAL.
            ASSIGN COMPONENT li_field->get_name( ) OF STRUCTURE ls_item TO <lv_field>.
            ASSERT sy-subrc = 0.
            li_field->set_value( |{ <lv_field> }| ).
            li_field = li_field->get_next( ).
          ENDWHILE.
          lv_index = lv_index + 1.
        ENDDO.

      ENDIF.
      li_node = li_iterator->get_next( ).
    ENDWHILE.

  ENDMETHOD.
  METHOD code_item_section_handling.
    CONSTANTS: lc_node_item TYPE string VALUE 'item'.
    CONSTANTS: lc_node_text TYPE string VALUE '#text'.

    IF iv_name IN get_range_node_codes( ).
      cv_within_code_section = abap_true.
    ENDIF.

    IF cv_within_code_section = abap_true.
      IF iv_name = lc_node_item.
        TRY.
            ei_code_item_element ?= ii_node.
            RETURN.
          CATCH cx_sy_move_cast_error ##NO_HANDLER.
        ENDTRY.

      ELSEIF iv_name NOT IN get_range_node_codes( ) AND
             iv_name <> lc_node_text.
        cv_within_code_section = abap_false.
      ENDIF.
    ENDIF.

    RAISE EXCEPTION TYPE Lcx_abapgit_exception.

  ENDMETHOD.
  METHOD fix_ids.

    " makes sure ID and IDREF values are the same values for each serialization run
    " the standard code has a counter that keeps increasing values.
    "
    " It is important that IDs and IDREFs which are the same before the fix
    " are also the same after the fix.

    TYPES:
      BEGIN OF ty_id_mapping,
        old TYPE string,
        new TYPE string,
      END OF ty_id_mapping,
      ty_id_mappings TYPE HASHED TABLE OF ty_id_mapping
                          WITH UNIQUE KEY old.

    DATA: lv_name       TYPE string,
          li_idref      TYPE REF TO if_ixml_node,
          li_node       TYPE REF TO if_ixml_node,
          li_attr       TYPE REF TO if_ixml_named_node_map,
          li_iterator   TYPE REF TO if_ixml_node_iterator,
          lt_id_mapping TYPE ty_id_mappings,
          ls_id_mapping LIKE LINE OF lt_id_mapping.

    li_iterator = ii_xml_doc->create_iterator( ).
    li_node = li_iterator->get_next( ).
    WHILE NOT li_node IS INITIAL.
      lv_name = li_node->get_name( ).
      IF lv_name = 'NODE' OR lv_name = 'WINDOW'.
        li_idref = li_node->get_attributes( )->get_named_item( 'IDREF' ).
        IF li_idref IS BOUND.

          ls_id_mapping-old = li_idref->get_value( ).
          READ TABLE lt_id_mapping WITH KEY old = ls_id_mapping-old
                                   INTO ls_id_mapping.
          IF sy-subrc <> 0.
            lv_name = lines( lt_id_mapping ) + 1.
            ls_id_mapping-new = condense( lv_name ).
            INSERT ls_id_mapping INTO TABLE lt_id_mapping.
          ENDIF.

          li_idref->set_value( |{ ls_id_mapping-new }| ).
        ENDIF.
      ENDIF.
      li_node = li_iterator->get_next( ).
    ENDWHILE.

    li_iterator = ii_xml_doc->create_iterator( ).
    li_node = li_iterator->get_next( ).
    WHILE NOT li_node IS INITIAL.
      lv_name = li_node->get_name( ).
      IF lv_name = 'NODE' OR lv_name = 'WINDOW'.
        li_idref = li_node->get_attributes( )->get_named_item( 'ID' ).
        IF li_idref IS BOUND.

          ls_id_mapping-old = li_idref->get_value( ).
          READ TABLE lt_id_mapping WITH KEY old = ls_id_mapping-old
                                   INTO ls_id_mapping.
          IF sy-subrc = 0.
            li_idref->set_value( |{ ls_id_mapping-new }| ).
          ELSE.
            li_attr = li_node->get_attributes( ).
            li_attr->remove_named_item( 'ID' ).
          ENDIF.

        ENDIF.
      ENDIF.
      li_node = li_iterator->get_next( ).
    ENDWHILE.

  ENDMETHOD.
  METHOD get_range_node_codes.

    DATA: ls_range_node_code TYPE LINE OF ty_string_range.

    IF gt_range_node_codes IS INITIAL.
      ls_range_node_code-sign   = 'I'.
      ls_range_node_code-option = 'EQ'.
      ls_range_node_code-low    = 'CODE'.
      INSERT ls_range_node_code INTO TABLE gt_range_node_codes.
      ls_range_node_code-low    = 'GTYPES'.
      INSERT ls_range_node_code INTO TABLE gt_range_node_codes.
      ls_range_node_code-low    = 'GCODING'.
      INSERT ls_range_node_code INTO TABLE gt_range_node_codes.
      ls_range_node_code-low    = 'FCODING'.
      INSERT ls_range_node_code INTO TABLE gt_range_node_codes.
    ENDIF.

    rt_range_node_codes = gt_range_node_codes.

  ENDMETHOD.
  METHOD handle_attrib_leading_spaces.

    DATA li_element        TYPE REF TO if_ixml_element.
    DATA lv_leading_spaces TYPE string.
    DATA lv_coding_line    TYPE string.

    TRY.
        code_item_section_handling( EXPORTING iv_name                = iv_name
                                              ii_node                = ii_node
                                    IMPORTING ei_code_item_element   = li_element
                                    CHANGING  cv_within_code_section = cv_within_code_section ).

* for downwards compatibility, this code can be removed sometime in the future
        lv_leading_spaces = li_element->get_attribute_ns( c_attrib_abapgit_leadig_spaces ).

        lv_coding_line = li_element->get_value( ).
        IF strlen( lv_coding_line ) >= 1 AND lv_coding_line(1) <> | |.
          SHIFT lv_coding_line RIGHT BY lv_leading_spaces PLACES.
          li_element->set_value( lv_coding_line ).
        ENDIF.
      CATCH Lcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE lastuser FROM stxfadm INTO rv_user
      WHERE formname = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_formname TYPE tdsfname.

    lv_formname = ms_item-obj_name.

    CALL FUNCTION 'FB_DELETE_FORM'
      EXPORTING
        i_formname            = lv_formname
        i_with_dialog         = abap_false
        i_with_confirm_dialog = abap_false
      EXCEPTIONS
        no_form               = 1
        OTHERS                = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.
* see function module FB_UPLOAD_FORM

    DATA: li_node                TYPE REF TO if_ixml_node,
          lv_formname            TYPE tdsfname,
          lv_name                TYPE string,
          li_iterator            TYPE REF TO if_ixml_node_iterator,
          lo_sf                  TYPE REF TO cl_ssf_fb_smart_form,
          lo_res                 TYPE REF TO cl_ssf_fb_smart_form,
          lx_error               TYPE REF TO cx_ssf_fb,
          lv_text                TYPE string,
          lv_within_code_section TYPE abap_bool.

    CREATE OBJECT lo_sf.

* set "created by" and "changed by" to current user
    li_iterator = io_xml->get_raw( )->get_root_element( )->create_iterator( ).
    li_node = li_iterator->get_next( ).
    WHILE NOT li_node IS INITIAL.
      lv_name = li_node->get_name( ).
      CASE lv_name.
        WHEN 'LASTDATE'.
          li_node->set_value( sy-datum(4) && '-' && sy-datum+4(2) && '-' && sy-datum+6(2) ).
        WHEN 'LASTTIME'.
          li_node->set_value( sy-uzeit(2) && ':' && sy-uzeit+2(2) && ':' && sy-uzeit+4(2) ).
        WHEN 'FIRSTUSER' OR 'LASTUSER'.
          li_node->set_value( sy-uname && '' ).

      ENDCASE.

      handle_attrib_leading_spaces( EXPORTING iv_name                = lv_name
                                              ii_node                = li_node
                                    CHANGING  cv_within_code_section = lv_within_code_section ).

      li_node = li_iterator->get_next( ).
    ENDWHILE.

    tadir_insert( iv_package ).

    lv_formname = ms_item-obj_name.

    TRY.
        lo_sf->enqueue( suppress_corr_check = space
                        master_language     = mv_language
                        mode                = 'INSERT'
                        formname            = lv_formname ).

        lo_sf->xml_upload( EXPORTING dom      = io_xml->get_raw( )->get_root_element( )
                                     formname = lv_formname
                                     language = mv_language
                           CHANGING  sform    = lo_res ).

        lo_res->store( im_formname = lo_res->header-formname
                       im_language = mv_language
                       im_active   = abap_true ).

        lo_sf->dequeue( lv_formname ).

      CATCH cx_ssf_fb INTO lx_error.
        lv_text = lx_error->get_text( ).
        Lcx_abapgit_exception=>raise( |{ ms_item-obj_type } { ms_item-obj_name }: { lv_text } | ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_formname TYPE stxfadm-formname.

    SELECT SINGLE formname FROM stxfadm INTO lv_formname
      WHERE formname = ms_item-obj_name.
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

    DATA: lv_ssfo_formname TYPE tdsfname.
    DATA lv_inactive TYPE abap_bool.

    lv_ssfo_formname = ms_item-obj_name.

    CALL FUNCTION 'SSF_STATUS_INFO'
      EXPORTING
        i_formname = lv_ssfo_formname
      IMPORTING
        o_inactive = lv_inactive.

    rv_active = boolc( lv_inactive = abap_false ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'E_SMFORM'
                                            iv_argument    = |{ ms_item-obj_name }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: lt_bdcdata  TYPE TABLE OF bdcdata,
          lv_formtype TYPE stxfadm-formtype.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPMSSFO'.
    <ls_bdcdata>-dynpro   = '0100'.
    <ls_bdcdata>-dynbegin = abap_true.

    SELECT SINGLE formtype FROM stxfadm INTO lv_formtype
           WHERE formname = ms_item-obj_name.

    IF lv_formtype = cssf_formtype_text.

      APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
      <ls_bdcdata>-fnam = 'RB_TX'.
      <ls_bdcdata>-fval = abap_true.

      APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
      <ls_bdcdata>-fnam = 'BDC_OKCODE'.
      <ls_bdcdata>-fval = '=RB'.

      APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
      <ls_bdcdata>-program  = 'SAPMSSFO'.
      <ls_bdcdata>-dynpro   = '0100'.
      <ls_bdcdata>-dynbegin = abap_true.

      APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
      <ls_bdcdata>-fnam = 'SSFSCREEN-TNAME'.
      <ls_bdcdata>-fval = ms_item-obj_name.

    ELSE.

      APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
      <ls_bdcdata>-fnam = 'SSFSCREEN-FNAME'.
      <ls_bdcdata>-fval = ms_item-obj_name.

    ENDIF.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=DISPLAY'.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SMARTFORMS'
      it_bdcdata = lt_bdcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.
* see function module FB_DOWNLOAD_FORM

    DATA: lo_sf       TYPE REF TO cl_ssf_fb_smart_form,
          lv_name     TYPE string,
          li_node     TYPE REF TO if_ixml_node,
          li_element  TYPE REF TO if_ixml_element,
          li_iterator TYPE REF TO if_ixml_node_iterator,
          lv_formname TYPE tdsfname,
          li_ixml     TYPE REF TO if_ixml,
          li_xml_doc  TYPE REF TO if_ixml_document.

    li_ixml = cl_ixml=>create( ).
    li_xml_doc = li_ixml->create_document( ).

    CREATE OBJECT lo_sf.
    lv_formname = ms_item-obj_name. " convert type
    TRY.
        lo_sf->load( im_formname = lv_formname
                     im_language = '' ).
      CATCH cx_ssf_fb.
* the smartform is not present in system, or other error occured
        RETURN.
    ENDTRY.

    lo_sf->xml_download( EXPORTING parent   = li_xml_doc
                         CHANGING  document = li_xml_doc ).

    li_iterator = li_xml_doc->create_iterator( ).
    li_node = li_iterator->get_next( ).
    WHILE NOT li_node IS INITIAL.

      lv_name = li_node->get_name( ).
      IF lv_name = 'DEVCLASS'
          OR lv_name = 'LASTDATE'
          OR lv_name = 'LASTTIME'.
        li_node->set_value( '' ).
      ENDIF.
      IF lv_name = 'FIRSTUSER'
          OR lv_name = 'LASTUSER'.
        li_node->set_value( 'DUMMY' ).
      ENDIF.
      li_node = li_iterator->get_next( ).
    ENDWHILE.

    fix_ids( li_xml_doc ).

    sort_texts( li_xml_doc ).

    li_element = li_xml_doc->get_root_element( ).
    li_element->set_attribute(
      name      = 'sf'
      namespace = 'xmlns'
      value     = 'urn:sap-com:SmartForms:2000:internal-structure' ).
    li_element->set_attribute(
      name  = 'xmlns'
      value = 'urn:sap-com:sdixml-ifr:2000' ).

    io_xml->set_raw( li_xml_doc->get_root_element( ) ).

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
endclass. "ZCL_ABAPGIT_OBJECT_SSFO implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SUSO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_suso=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_suso=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SUSO implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_objectname = ms_item-obj_name.

  ENDMETHOD.
  METHOD delete_documentation.

    DATA:
      lv_docu_obj TYPE dokhl-object,
      lv_dummy    TYPE sy-langu.

    lv_docu_obj  = mv_objectname.

    SELECT SINGLE langu
           FROM dokil INTO lv_dummy
           WHERE id   = 'UO'                            "#EC CI_GENBUFF
           AND object = lv_docu_obj.                    "#EC CI_NOORDER

    IF sy-subrc = 0.

      CALL FUNCTION 'DOKU_DELETE_ALL'
        EXPORTING
          doku_id                        = 'UO'
          doku_object                    = lv_docu_obj
          suppress_transport             = space
        EXCEPTIONS
          header_without_text            = 1
          index_without_header           = 2
          no_authority_for_devclass_xxxx = 3
          no_docu_found                  = 4
          object_is_already_enqueued     = 5
          object_is_enqueued_by_corr     = 6
          techn_enqueue_problem          = 7
          user_break                     = 8
          OTHERS                         = 9.

      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD pre_check.

    CONSTANTS:
      lc_act_delete TYPE activ_auth VALUE '06'.

    DATA:
      lv_act_head            TYPE activ_auth,
      lo_suso                TYPE REF TO object,
      lv_failed              TYPE abap_bool,
      lv_suso_collect_in_cts TYPE i,
      ls_clskey              TYPE seoclskey.

    " Downport: CL_SUSO_GEN doesn't exist in 702
    ls_clskey-clsname = |CL_SUSO_GEN|.

    CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
      EXPORTING
        clskey        = ls_clskey
      EXCEPTIONS
        not_specified = 1
        not_existing  = 2
        is_interface  = 3
        no_text       = 4
        inconsistent  = 5
        OTHERS        = 6.

    IF sy-subrc = 0.

      " so these check are not executed in 702

      CREATE OBJECT lo_suso
        TYPE
          ('CL_SUSO_GEN').

      CALL METHOD lo_suso->('SUSO_LOAD_FROM_DB')
        EXPORTING
          id_object = mv_objectname
        RECEIVING
          ed_failed = lv_failed.

      IF lv_failed = abap_true.
        " Object & does not exist; choose an existing object
        MESSAGE s111(01) WITH mv_objectname INTO Lcx_abapgit_exception=>null.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      CALL METHOD lo_suso->('GET_SUSO_EDIT_MODE')
        EXPORTING
          id_object     = mv_objectname
          id_planed_act = lc_act_delete
        IMPORTING
          ed_mode_head  = lv_act_head.

      IF lv_act_head <> lc_act_delete.
        Lcx_abapgit_exception=>raise( |SUSO { mv_objectname }: Delete not allowed. Check where-used in SU21| ).
      ENDIF.

      CALL METHOD lo_suso->('SUSO_COLLECT_IN_CTS')
        EXPORTING
          id_object = mv_objectname
        RECEIVING
          ed_result = lv_suso_collect_in_cts.

      IF lv_suso_collect_in_cts IS NOT INITIAL.
        Lcx_abapgit_exception=>raise( |SUSO { mv_objectname }: Cannot delete| ).
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD regenerate_sap_all.

    DATA: ls_e071  TYPE e071,
          lt_e071  TYPE STANDARD TABLE OF e071,
          lt_e071k TYPE STANDARD TABLE OF e071k.

    ls_e071-pgmid = 'R3TR'.
    ls_e071-object = ms_item-obj_type.
    ls_e071-obj_name = ms_item-obj_name.
    INSERT ls_e071 INTO TABLE lt_e071.

    CALL FUNCTION 'PRGN_AFTER_IMP_SUSO_SAP_ALL'
      EXPORTING
        iv_tarclient  = '000'
        iv_is_upgrade = space
      TABLES
        tt_e071       = lt_e071
        tt_e071k      = lt_e071k.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    SELECT SINGLE modifier FROM tobjvor INTO rv_user
      WHERE objct = mv_objectname.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    " FM SUSR_DELETE_OBJECT calls the UI. Therefore we reimplement it here.
    " As the class CL_SUSO_GEN isn't present in 702, we call dynamically and
    " skip the pre checks on 702 system. That seems ok.

    pre_check( ).

    delete_documentation( ).

    DELETE FROM tobj  WHERE objct  = mv_objectname.
    DELETE FROM tobjt WHERE object = mv_objectname.
    DELETE FROM tactz WHERE brobj  = mv_objectname.

    CALL FUNCTION 'SUPV_DELETE_OBJECT_ASSIGNMENTS'
      EXPORTING
        object_name  = mv_objectname
        all_releases = abap_true.

    CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
      EXPORTING
        object    = mv_objectname
        type      = 'SUSO'
        operation = 'DELETE'.

    regenerate_sap_all( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.
* see function group SUSA

    DATA: lv_objectname TYPE trobj_name,
          ls_tobj       TYPE tobj,
          ls_tobjt      TYPE tobjt,
          ls_tobjvorflg TYPE tobjvorflg,
          lt_tactz      TYPE TABLE OF tactz,
          lt_tobjvordat TYPE TABLE OF tobjvordat,
          lt_tobjvor    TYPE TABLE OF tobjvor.


    ASSERT NOT ms_item-obj_name IS INITIAL.

    io_xml->read( EXPORTING iv_name = 'TOBJ'
                  CHANGING cg_data = ls_tobj ).
    ls_tobj-bname = sy-uname.
    io_xml->read( EXPORTING iv_name = 'TOBJT'
                  CHANGING cg_data = ls_tobjt ).
    io_xml->read( EXPORTING iv_name = 'TOBJVORFLG'
                  CHANGING cg_data = ls_tobjvorflg ).
    io_xml->read( EXPORTING iv_name = 'TACTZ'
                  CHANGING  cg_data = lt_tactz ).
    io_xml->read( EXPORTING iv_name = 'TOBJVORDAT'
                  CHANGING  cg_data = lt_tobjvordat ).
    io_xml->read( EXPORTING iv_name = 'TOBJVOR'
                  CHANGING  cg_data = lt_tobjvor ).

    tadir_insert( iv_package ).

    lv_objectname = mv_objectname.

    CALL FUNCTION 'SUSR_COMMEDITCHECK'
      EXPORTING
        objectname      = lv_objectname
        transobjecttype = 'O'.

    MODIFY tobj FROM ls_tobj.                             "#EC CI_SUBRC
    MODIFY tobjt FROM ls_tobjt.                           "#EC CI_SUBRC
    MODIFY tobjvorflg FROM ls_tobjvorflg.                 "#EC CI_SUBRC
    DELETE FROM tactz WHERE brobj = ms_item-obj_name.     "#EC CI_SUBRC
    INSERT tactz FROM TABLE lt_tactz.                     "#EC CI_SUBRC
    DELETE FROM tobjvordat WHERE objct = ms_item-obj_name. "#EC CI_SUBRC
    INSERT tobjvordat FROM TABLE lt_tobjvordat.           "#EC CI_SUBRC
    DELETE FROM tobjvor WHERE objct = ms_item-obj_name.   "#EC CI_SUBRC
    INSERT tobjvor FROM TABLE lt_tobjvor.                 "#EC CI_SUBRC

    deserialize_longtexts( ii_xml         = io_xml
                           iv_longtext_id = c_longtext_id_suso ).

    regenerate_sap_all( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_objct TYPE tobj-objct.


    SELECT SINGLE objct FROM tobj INTO lv_objct
      WHERE objct = ms_item-obj_name.

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

    CALL FUNCTION 'SUSR_SHOW_OBJECT'
      EXPORTING
        object = mv_objectname.

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_tobj       TYPE tobj,
          ls_tobjt      TYPE tobjt,
          ls_tobjvorflg TYPE tobjvorflg,
          lt_tactz      TYPE TABLE OF tactz,
          lt_tobjvordat TYPE TABLE OF tobjvordat,
          lt_tobjvor    TYPE TABLE OF tobjvor.


    SELECT SINGLE * FROM tobj INTO ls_tobj
      WHERE objct = ms_item-obj_name.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    CLEAR ls_tobj-bname.

    SELECT SINGLE * FROM tobjt INTO ls_tobjt
      WHERE object = ms_item-obj_name
      AND langu = mv_language.                          "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'TOBJT no english description'
        && ' for object (' && ms_item-obj_name && ')' ).
    ENDIF.

    SELECT SINGLE * FROM tobjvorflg INTO ls_tobjvorflg
      WHERE objct = ms_item-obj_name.                     "#EC CI_SUBRC

    SELECT * FROM tactz INTO TABLE lt_tactz
      WHERE brobj = ms_item-obj_name
      ORDER BY PRIMARY KEY.               "#EC CI_SUBRC "#EC CI_GENBUFF

    SELECT * FROM tobjvordat INTO TABLE lt_tobjvordat
      WHERE objct = ms_item-obj_name
      ORDER BY PRIMARY KEY.               "#EC CI_SUBRC "#EC CI_GENBUFF

    SELECT * FROM tobjvor INTO TABLE lt_tobjvor
      WHERE objct = ms_item-obj_name
      ORDER BY PRIMARY KEY.                               "#EC CI_SUBRC

    io_xml->add( iv_name = 'TOBJ'
                 ig_data = ls_tobj ).
    io_xml->add( iv_name = 'TOBJT'
                 ig_data = ls_tobjt ).
    io_xml->add( iv_name = 'TOBJVORFLG'
                 ig_data = ls_tobjvorflg ).
    io_xml->add( ig_data = lt_tactz
                 iv_name = 'TACTZ' ).
    io_xml->add( ig_data = lt_tobjvordat
                 iv_name = 'TOBJVORDAT' ).
    io_xml->add( ig_data = lt_tobjvor
                 iv_name = 'TOBJVOR' ).

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_suso ).

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
endclass. "ZCL_ABAPGIT_OBJECT_SUSO implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_TABL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_tabl=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_tabl=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_object_tabl=======ccau.


class LCL_ABAPGIT_OBJECT_TABL implementation.
*"* method's implementations
*include methods.
  METHOD clear_dd03p_fields.

    CONSTANTS lc_comptype_dataelement TYPE comptype VALUE 'E'.

    DATA: lv_masklen TYPE c LENGTH 4.

    FIELD-SYMBOLS: <ls_dd03p> TYPE dd03p.

* remove nested structures
    DELETE ct_dd03p WHERE depth <> '00'.
* remove fields from .INCLUDEs
    DELETE ct_dd03p WHERE adminfield <> '0'.

    LOOP AT ct_dd03p ASSIGNING <ls_dd03p> WHERE NOT rollname IS INITIAL.

      clear_dd03p_fields_common( CHANGING cs_dd03p = <ls_dd03p> ).

      lv_masklen = <ls_dd03p>-masklen.
      IF lv_masklen = '' OR NOT lv_masklen CO '0123456789'.
* make sure the field contains valid data, or the XML will dump
        CLEAR <ls_dd03p>-masklen.
      ENDIF.

      IF <ls_dd03p>-comptype = lc_comptype_dataelement.
        clear_dd03p_fields_dataelement( CHANGING cs_dd03p = <ls_dd03p> ).
      ENDIF.

      IF <ls_dd03p>-shlporigin = 'D'.
* search help from domain
        CLEAR: <ls_dd03p>-shlpfield,
               <ls_dd03p>-shlpname.
      ENDIF.

* XML output assumes correct field content
      IF <ls_dd03p>-routputlen = '      '.
        CLEAR <ls_dd03p>-routputlen.
      ENDIF.

    ENDLOOP.

    " Clear position to avoid issues with include structures that contain different number of fields
    LOOP AT ct_dd03p ASSIGNING <ls_dd03p>.
      CLEAR: <ls_dd03p>-position, <ls_dd03p>-tabname, <ls_dd03p>-ddlanguage.
    ENDLOOP.

  ENDMETHOD.
  METHOD clear_dd03p_fields_common.

    CLEAR: cs_dd03p-ddlanguage,
           cs_dd03p-dtelmaster,
           cs_dd03p-logflag,
           cs_dd03p-ddtext,
           cs_dd03p-reservedte,
           cs_dd03p-reptext,
           cs_dd03p-scrtext_s,
           cs_dd03p-scrtext_m,
           cs_dd03p-scrtext_l.

  ENDMETHOD.
  METHOD clear_dd03p_fields_dataelement.

* type specified via data element
    CLEAR: cs_dd03p-domname,
           cs_dd03p-inttype,
           cs_dd03p-intlen,
           cs_dd03p-mask,
           cs_dd03p-memoryid,
           cs_dd03p-headlen,
           cs_dd03p-scrlen1,
           cs_dd03p-scrlen2,
           cs_dd03p-scrlen3,
           cs_dd03p-datatype,
           cs_dd03p-leng,
           cs_dd03p-outputlen,
           cs_dd03p-deffdname,
           cs_dd03p-convexit,
           cs_dd03p-entitytab,
           cs_dd03p-dommaster,
           cs_dd03p-domname3l,
           cs_dd03p-decimals,
           cs_dd03p-lowercase,
           cs_dd03p-signflag.

  ENDMETHOD.
  METHOD delete_extras.

    DELETE FROM tddat WHERE tabname = iv_tabname.

  ENDMETHOD.
  METHOD delete_idoc_segment.

    DATA lv_segment_type        TYPE edilsegtyp.
    DATA lv_result              LIKE sy-subrc.

    IF is_idoc_segment( ) = abap_false.
      rv_deleted = abap_false.
      RETURN. "previous XML version or no IDoc segment
    ENDIF.

    rv_deleted = abap_true.
    lv_segment_type = ms_item-obj_name.

    CALL FUNCTION 'SEGMENT_DELETE'
      EXPORTING
        segmenttyp = lv_segment_type
      IMPORTING
        result     = lv_result
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0 OR lv_result <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.
  ENDMETHOD.
  METHOD deserialize_idoc_segment.

    DATA lv_result              LIKE sy-subrc.
    DATA lt_segment_definitions TYPE ty_segment_definitions.
    DATA lv_package             TYPE devclass.
    DATA lv_uname               TYPE sy-uname.
    DATA lv_transport           TYPE trkorr.
    DATA ls_edisdef             TYPE edisdef.
    DATA ls_segment_definition  TYPE ty_segment_definition.
    FIELD-SYMBOLS <ls_segment_definition> TYPE ty_segment_definition.

    rv_deserialized = abap_false.

    TRY.

        io_xml->read( EXPORTING iv_name = c_s_dataname-segment_definition
                      CHANGING  cg_data = lt_segment_definitions ).

      CATCH Lcx_abapgit_exception.
        RETURN. "previous XML version or no IDoc segment
    ENDTRY.

    IF lines( lt_segment_definitions ) = 0.
      RETURN. "no IDoc segment
    ENDIF.

    rv_deserialized = abap_true.

    lv_package = iv_package.
    lv_transport = iv_transport.

    LOOP AT lt_segment_definitions ASSIGNING <ls_segment_definition>.
      ls_segment_definition = <ls_segment_definition>.
      <ls_segment_definition>-segmentheader-presp = sy-uname.
      <ls_segment_definition>-segmentheader-pwork = sy-uname.

      CALL FUNCTION 'SEGMENT_READ'
        EXPORTING
          segmenttyp = <ls_segment_definition>-segmentdefinition-segtyp
        IMPORTING
          result     = lv_result
        EXCEPTIONS
          OTHERS     = 1.
      IF sy-subrc <> 0 OR lv_result <> 0.
        CALL FUNCTION 'SEGMENT_CREATE'
          IMPORTING
            segmentdefinition = <ls_segment_definition>-segmentdefinition
          TABLES
            segmentstructure  = <ls_segment_definition>-segmentstructures
          CHANGING
            segmentheader     = <ls_segment_definition>-segmentheader
            devclass          = lv_package
          EXCEPTIONS
            OTHERS            = 1.
      ELSE.

        CALL FUNCTION 'SEGMENT_MODIFY'
          CHANGING
            segmentheader = <ls_segment_definition>-segmentheader
            devclass      = lv_package
          EXCEPTIONS
            OTHERS        = 1.
        IF sy-subrc = 0.
          CALL FUNCTION 'SEGMENTDEFINITION_MODIFY'
            TABLES
              segmentstructure  = <ls_segment_definition>-segmentstructures
            CHANGING
              segmentdefinition = <ls_segment_definition>-segmentdefinition
            EXCEPTIONS
              OTHERS            = 1.
        ENDIF.
      ENDIF.

      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      IF ls_segment_definition-segmentdefinition-closed = abap_true.
        IF lv_transport IS NOT INITIAL.
          CALL FUNCTION 'SEGMENTDEFINITION_CLOSE'
            EXPORTING
              segmenttyp = ls_segment_definition-segmentdefinition-segtyp
            CHANGING
              order      = lv_transport
            EXCEPTIONS
              OTHERS     = 1.
          IF sy-subrc <> 0.
            Lcx_abapgit_exception=>raise_t100( ).
          ENDIF.
        ENDIF.

        " SEGMENTDEFINITION_CLOSE saves current release but it should be same as in repo
        SELECT SINGLE * FROM edisdef INTO ls_edisdef
          WHERE segtyp  = ls_segment_definition-segmentdefinition-segtyp
            AND version = ls_segment_definition-segmentdefinition-version.
        ls_edisdef-released = ls_segment_definition-segmentdefinition-released.
        ls_edisdef-applrel  = ls_segment_definition-segmentdefinition-applrel.
        ls_edisdef-closed   = ls_segment_definition-segmentdefinition-closed.
        UPDATE edisdef FROM ls_edisdef.
        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise( |Error updating IDOC segment {
            <ls_segment_definition>-segmentdefinition-segtyp }| ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    lv_uname = sy-uname.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_test_modus       = abap_false
        wi_tadir_pgmid      = 'R3TR'
        wi_tadir_object     = ms_item-obj_type
        wi_tadir_obj_name   = ms_item-obj_name
        wi_tadir_author     = lv_uname
        wi_tadir_devclass   = iv_package
        wi_tadir_masterlang = mv_language
        iv_set_edtflag      = abap_true
        iv_delflag          = abap_false
      EXCEPTIONS
        OTHERS              = 1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.
  ENDMETHOD.
  METHOD deserialize_indexes.

    DATA:
      lv_name      TYPE ddobjname,
      lv_subrc     TYPE sy-subrc,
      lt_dd12v     TYPE dd12vtab,
      lt_dd12v_db  TYPE dd12vtab,
      ls_dd12v     LIKE LINE OF lt_dd12v,
      lt_dd17v     TYPE dd17vtab,
      ls_dd17v     LIKE LINE OF lt_dd17v,
      lt_secondary LIKE lt_dd17v.

    io_xml->read( EXPORTING iv_name = 'DD12V'
                  CHANGING cg_data = lt_dd12v ).
    io_xml->read( EXPORTING iv_name = 'DD17V'
                  CHANGING cg_data = lt_dd17v ).

    lv_name = ms_item-obj_name.

    " Get existing indexes and drop the ones that are not included in remote
    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = lv_name
        langu         = mv_language
      TABLES
        dd12v_tab     = lt_dd12v_db
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from DDIF_TABL_GET' ).
    ENDIF.

    LOOP AT lt_dd12v_db INTO ls_dd12v.
      READ TABLE lt_dd12v TRANSPORTING NO FIELDS WITH KEY
        sqltab    = ls_dd12v-sqltab
        indexname = ls_dd12v-indexname.
      IF sy-subrc <> 0.
        CALL FUNCTION 'DD_INDX_DEL'
          EXPORTING
            sqltab    = ls_dd12v-sqltab
            indexname = ls_dd12v-indexname
            del_state = 'M'     "all states
          IMPORTING
            rc        = lv_subrc.
        IF lv_subrc <> 0.
          Lcx_abapgit_exception=>raise( |Error deleting index { ls_dd12v-sqltab }~{ ls_dd12v-indexname }| ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    " Create new or update existing indexes
    LOOP AT lt_dd12v INTO ls_dd12v.

      CLEAR lt_secondary.
      LOOP AT lt_dd17v INTO ls_dd17v
          WHERE sqltab = ls_dd12v-sqltab AND indexname = ls_dd12v-indexname.
        APPEND ls_dd17v TO lt_secondary.
      ENDLOOP.

      CALL FUNCTION 'DDIF_INDX_PUT'
        EXPORTING
          name              = ls_dd12v-sqltab
          id                = ls_dd12v-indexname
          dd12v_wa          = ls_dd12v
        TABLES
          dd17v_tab         = lt_secondary
        EXCEPTIONS
          indx_not_found    = 1
          name_inconsistent = 2
          indx_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      " Secondary indexes are automatically activated as part of R3TR TABL
      " So there's no need to add them to activation queue
    ENDLOOP.

  ENDMETHOD.
  METHOD deserialize_texts.

    DATA: lv_name       TYPE ddobjname,
          ls_dd02v_tmp  TYPE dd02v,
          lt_i18n_langs TYPE TABLE OF langu,
          lt_dd02_texts TYPE ty_dd02_texts.

    FIELD-SYMBOLS: <lv_lang>      LIKE LINE OF lt_i18n_langs,
                   <ls_dd02_text> LIKE LINE OF lt_dd02_texts.

    lv_name = ms_item-obj_name.

    ii_xml->read( EXPORTING iv_name = 'I18N_LANGS'
                  CHANGING  cg_data = lt_i18n_langs ).

    ii_xml->read( EXPORTING iv_name = 'DD02_TEXTS'
                  CHANGING  cg_data = lt_dd02_texts ).

    mo_i18n_params->trim_saplang_list( CHANGING ct_sap_langs = lt_i18n_langs ).

    SORT lt_i18n_langs.
    SORT lt_dd02_texts BY ddlanguage. " Optimization

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.

      " Table description
      ls_dd02v_tmp = is_dd02v.
      READ TABLE lt_dd02_texts ASSIGNING <ls_dd02_text> WITH KEY ddlanguage = <lv_lang>.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |DD02_TEXTS cannot find lang { <lv_lang> } in XML| ).
      ENDIF.
      MOVE-CORRESPONDING <ls_dd02_text> TO ls_dd02v_tmp.
      CALL FUNCTION 'DDIF_TABL_PUT'
        EXPORTING
          name              = lv_name
          dd02v_wa          = ls_dd02v_tmp
        EXCEPTIONS
          tabl_not_found    = 1
          name_inconsistent = 2
          tabl_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD is_db_table_category.

    " values from domain TABCLASS
    rv_is_db_table_type = boolc( iv_tabclass = 'TRANSP'
                              OR iv_tabclass = 'CLUSTER'
                              OR iv_tabclass = 'POOL' ).

  ENDMETHOD.
  METHOD is_idoc_segment.

    DATA lv_segment_type TYPE edilsegtyp.

    lv_segment_type = ms_item-obj_name.

    SELECT SINGLE segtyp
           FROM edisegment
           INTO lv_segment_type
           WHERE segtyp = lv_segment_type.
    rv_is_idoc_segment = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD read_extras.

    SELECT SINGLE * FROM tddat INTO rs_tabl_extras-tddat WHERE tabname = iv_tabname.

  ENDMETHOD.
  METHOD serialize_idoc_segment.

    DATA lv_segment_type        TYPE edilsegtyp.
    DATA lv_result              LIKE sy-subrc.
    DATA lv_devclass            TYPE devclass.
    DATA lt_segmentdefinitions  TYPE STANDARD TABLE OF edisegmdef.
    DATA ls_segment_definition  TYPE ty_segment_definition.
    DATA lt_segment_definitions TYPE ty_segment_definitions.
    FIELD-SYMBOLS: <ls_segemtndefinition> TYPE edisegmdef.

    IF is_idoc_segment( ) = abap_false.
      RETURN.
    ENDIF.

    lv_segment_type = ms_item-obj_name.
    CALL FUNCTION 'SEGMENT_READ'
      EXPORTING
        segmenttyp        = lv_segment_type
      IMPORTING
        result            = lv_result
      TABLES
        segmentdefinition = lt_segmentdefinitions
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc <> 0 OR lv_result <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    LOOP AT lt_segmentdefinitions ASSIGNING <ls_segemtndefinition>.
      CLEAR ls_segment_definition.
      CALL FUNCTION 'SEGMENTDEFINITION_READ'
        EXPORTING
          segmenttyp           = <ls_segemtndefinition>-segtyp
        IMPORTING
          result               = lv_result
          devclass             = lv_devclass
          segmentheader        = ls_segment_definition-segmentheader
          segmentdefinition    = ls_segment_definition-segmentdefinition
        TABLES
          segmentstructure     = ls_segment_definition-segmentstructures
        CHANGING
          version              = <ls_segemtndefinition>-version
        EXCEPTIONS
          no_authority         = 1
          segment_not_existing = 2
          OTHERS               = 3.
      IF sy-subrc <> 0 OR lv_result <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      Lcl_abapgit_object_idoc=>clear_idoc_segement_fields(
                                 CHANGING cg_structure = ls_segment_definition-segmentdefinition ).
      Lcl_abapgit_object_idoc=>clear_idoc_segement_fields(
                                 CHANGING cg_structure = ls_segment_definition-segmentheader ).

      APPEND ls_segment_definition TO lt_segment_definitions.
    ENDLOOP.

    io_xml->add( iv_name = c_s_dataname-segment_definition
                 ig_data = lt_segment_definitions ).

  ENDMETHOD.
  METHOD serialize_texts.

    DATA: lv_name            TYPE ddobjname,
          lv_index           TYPE i,
          ls_dd02v           TYPE dd02v,
          lt_dd02_texts      TYPE ty_dd02_texts,
          lt_i18n_langs      TYPE TABLE OF langu,
          lt_language_filter TYPE Lif_abapgit_environment=>ty_system_language_filter.

    FIELD-SYMBOLS: <lv_lang>      LIKE LINE OF lt_i18n_langs,
                   <ls_dd02_text> LIKE LINE OF lt_dd02_texts.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      RETURN.
    ENDIF.

    lv_name = ms_item-obj_name.

    " Collect additional languages, skip main lang - it was serialized already
    lt_language_filter = mo_i18n_params->build_language_filter( ).

    SELECT DISTINCT ddlanguage AS langu INTO TABLE lt_i18n_langs
      FROM dd02v
      WHERE tabname = lv_name
      AND ddlanguage IN lt_language_filter
      AND ddlanguage <> mv_language
      ORDER BY langu.                                     "#EC CI_SUBRC

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.
      lv_index = sy-tabix.
      CALL FUNCTION 'DDIF_TABL_GET'
        EXPORTING
          name          = lv_name
          langu         = <lv_lang>
        IMPORTING
          dd02v_wa      = ls_dd02v
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.
      IF sy-subrc <> 0 OR ls_dd02v-ddlanguage IS INITIAL.
        DELETE lt_i18n_langs INDEX lv_index. " Don't save this lang
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO lt_dd02_texts ASSIGNING <ls_dd02_text>.
      MOVE-CORRESPONDING ls_dd02v TO <ls_dd02_text>.

    ENDLOOP.

    SORT lt_i18n_langs ASCENDING.
    SORT lt_dd02_texts BY ddlanguage ASCENDING.

    IF lines( lt_i18n_langs ) > 0.
      ii_xml->add( iv_name = 'I18N_LANGS'
                   ig_data = lt_i18n_langs ).

      ii_xml->add( iv_name = 'DD02_TEXTS'
                   ig_data = lt_dd02_texts ).
    ENDIF.

  ENDMETHOD.
  METHOD update_extras.

    IF is_tabl_extras-tddat IS INITIAL.
      delete_extras( iv_tabname ).
    ELSE.
      MODIFY tddat FROM is_tabl_extras-tddat.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    TYPES: BEGIN OF ty_data,
             as4user TYPE dd02l-as4user,
             as4date TYPE dd02l-as4date,
             as4time TYPE dd02l-as4time,
           END OF ty_data.

    DATA: lt_data TYPE STANDARD TABLE OF ty_data WITH DEFAULT KEY,
          ls_data LIKE LINE OF lt_data.


    SELECT as4user as4date as4time
      FROM dd02l INTO TABLE lt_data
      WHERE tabname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'
      ORDER BY PRIMARY KEY.

    SELECT as4user as4date as4time
      APPENDING TABLE lt_data
      FROM dd09l
      WHERE tabname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'
      ORDER BY PRIMARY KEY.

    SELECT as4user as4date as4time
      APPENDING TABLE lt_data
      FROM dd12l
      WHERE sqltab = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'
      ORDER BY PRIMARY KEY.

    SORT lt_data BY as4date DESCENDING as4time DESCENDING.

    READ TABLE lt_data INDEX 1 INTO ls_data.
    IF sy-subrc = 0.
      rv_user = ls_data-as4user.
    ELSE.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_objname TYPE rsedd0-ddobjname,
          lv_no_ask  TYPE abap_bool,
          lv_subrc   TYPE sy-subrc,
          BEGIN OF ls_dd02l,
            tabname  TYPE dd02l-tabname,
            tabclass TYPE dd02l-tabclass,
            sqltab   TYPE dd02l-sqltab,
          END OF ls_dd02l.

    IF Lif_abapgit_object~exists( ) = abap_false.
      " Proxies e.g. delete on its own, nothing todo here then.
      RETURN.
    ENDIF.

    lv_objname = ms_item-obj_name.

    IF delete_idoc_segment( ) = abap_false.

      lv_no_ask = abap_true.
      SELECT SINGLE tabname tabclass sqltab FROM dd02l
        INTO CORRESPONDING FIELDS OF ls_dd02l
        WHERE tabname = ms_item-obj_name
        AND as4local = 'A'
        AND as4vers = '0000'.
      IF sy-subrc = 0 AND is_db_table_category( ls_dd02l-tabclass ) = abap_true.

        CALL FUNCTION 'DD_EXISTS_DATA'
          EXPORTING
            reftab          = ls_dd02l-sqltab
            tabclass        = ls_dd02l-tabclass
            tabname         = ls_dd02l-tabname
          IMPORTING
            subrc           = lv_subrc
          EXCEPTIONS
            missing_reftab  = 1
            sql_error       = 2
            buffer_overflow = 3
            unknown_error   = 4
            OTHERS          = 5.

        IF sy-subrc = 0 AND lv_subrc = 0.
          lv_no_ask = abap_false.
        ENDIF.

      ENDIF.

      delete_ddic( iv_objtype = 'T'
                   iv_no_ask  = lv_no_ask ).

      delete_longtexts( c_longtext_id_tabl ).

      delete_extras( lv_objname ).

    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_name   TYPE ddobjname,
          ls_dd02v  TYPE dd02v,
          ls_dd09l  TYPE dd09l,
          lt_dd03p  TYPE TABLE OF dd03p,
          lt_dd05m  TYPE TABLE OF dd05m,
          lt_dd08v  TYPE TABLE OF dd08v,
          lt_dd35v  TYPE TABLE OF dd35v,
          lt_dd36m  TYPE dd36mttyp,
          ls_extras TYPE ty_tabl_extras.

    FIELD-SYMBOLS: <ls_dd03p>      TYPE dd03p,
                   <ls_dd05m>      TYPE dd05m,
                   <ls_dd08v>      TYPE dd08v,
                   <ls_dd35v>      TYPE dd35v,
                   <ls_dd36m>      TYPE dd36m,
                   <lg_roworcolst> TYPE any.

    lv_name = ms_item-obj_name. " type conversion

    IF deserialize_idoc_segment( io_xml     = io_xml
                                 iv_transport = iv_transport
                                 iv_package = iv_package ) = abap_false.

      io_xml->read( EXPORTING iv_name = 'DD02V'
                    CHANGING cg_data = ls_dd02v ).
      io_xml->read( EXPORTING iv_name = 'DD09L'
                    CHANGING cg_data = ls_dd09l ).
      io_xml->read( EXPORTING iv_name  = 'DD03P_TABLE'
                    CHANGING cg_data = lt_dd03p ).
      ASSIGN COMPONENT 'ROWORCOLST' OF STRUCTURE ls_dd09l TO <lg_roworcolst>.
      IF sy-subrc = 0 AND <lg_roworcolst> IS INITIAL.
        <lg_roworcolst> = 'C'. "Reverse fix from serialize
      ENDIF.

      " Number fields sequentially and fill table name
      LOOP AT lt_dd03p ASSIGNING <ls_dd03p>.
        <ls_dd03p>-position   = sy-tabix.
        <ls_dd03p>-tabname    = lv_name.
        <ls_dd03p>-ddlanguage = mv_language.
      ENDLOOP.

      io_xml->read( EXPORTING iv_name = 'DD05M_TABLE'
                    CHANGING cg_data = lt_dd05m ).
      io_xml->read( EXPORTING iv_name = 'DD08V_TABLE'
                    CHANGING cg_data = lt_dd08v ).
      io_xml->read( EXPORTING iv_name = 'DD35V_TALE'
                    CHANGING cg_data = lt_dd35v ).
      io_xml->read( EXPORTING iv_name = 'DD36M'
                    CHANGING cg_data = lt_dd36m ).

      LOOP AT lt_dd05m ASSIGNING <ls_dd05m>.
        <ls_dd05m>-tabname = lv_name.
      ENDLOOP.
      LOOP AT lt_dd08v ASSIGNING <ls_dd08v>.
        <ls_dd08v>-tabname = lv_name.
        <ls_dd08v>-ddlanguage = mv_language.
      ENDLOOP.
      LOOP AT lt_dd35v ASSIGNING <ls_dd35v>.
        <ls_dd35v>-tabname = lv_name.
      ENDLOOP.
      LOOP AT lt_dd36m ASSIGNING <ls_dd36m>.
        <ls_dd36m>-tabname = lv_name.
      ENDLOOP.

      corr_insert( iv_package = iv_package
                   ig_object_class = 'DICT' ).

      CALL FUNCTION 'DD_TABL_EXPAND'
        EXPORTING
          dd02v_wa          = ls_dd02v
        TABLES
          dd03p_tab         = lt_dd03p
          dd05m_tab         = lt_dd05m
          dd08v_tab         = lt_dd08v
          dd35v_tab         = lt_dd35v
          dd36m_tab         = lt_dd36m
        EXCEPTIONS
          illegal_parameter = 1
          OTHERS            = 2.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      CALL FUNCTION 'DDIF_TABL_PUT'
        EXPORTING
          name              = lv_name
          dd02v_wa          = ls_dd02v
          dd09l_wa          = ls_dd09l
        TABLES
          dd03p_tab         = lt_dd03p
          dd05m_tab         = lt_dd05m
          dd08v_tab         = lt_dd08v
          dd35v_tab         = lt_dd35v
          dd36m_tab         = lt_dd36m
        EXCEPTIONS
          tabl_not_found    = 1
          name_inconsistent = 2
          tabl_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      Lcl_abapgit_objects_activation=>add_item( ms_item ).

      deserialize_indexes( io_xml ).

      IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
        deserialize_texts(
          ii_xml   = io_xml
          is_dd02v = ls_dd02v ).
      ENDIF.

      deserialize_longtexts( ii_xml         = io_xml
                             iv_longtext_id = c_longtext_id_tabl ).

      io_xml->read( EXPORTING iv_name = c_s_dataname-tabl_extras
                    CHANGING cg_data = ls_extras ).
      update_extras( iv_tabname     = lv_name
                     is_tabl_extras = ls_extras ).

    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_tabname TYPE dd02l-tabname.

    lv_tabname = ms_item-obj_name.

    " Check nametab because it's fast
    CALL FUNCTION 'DD_GET_NAMETAB_HEADER'
      EXPORTING
        tabname   = lv_tabname
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      " Check for new, inactive, or modified versions that might not be in nametab
      SELECT SINGLE tabname FROM dd02l INTO lv_tabname
        WHERE tabname = lv_tabname.                     "#EC CI_NOORDER
    ENDIF.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.

    DATA: li_local_version_output TYPE REF TO Lif_abapgit_xml_output,
          li_local_version_input  TYPE REF TO Lif_abapgit_xml_input.


    CREATE OBJECT li_local_version_output TYPE Lcl_abapgit_xml_output.

    Lif_abapgit_object~serialize( li_local_version_output ).

    CREATE OBJECT li_local_version_input
      TYPE Lcl_abapgit_xml_input
      EXPORTING
        iv_xml = li_local_version_output->render( ).

    CREATE OBJECT ri_comparator TYPE Lcl_abapgit_object_tabl_compar
      EXPORTING
        ii_local = li_local_version_input.

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
    " Covered by ZCL_ABAPGIT_OBJECT=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_name   TYPE ddobjname,
          lv_state  TYPE ddgotstate,
          ls_dd02v  TYPE dd02v,
          ls_dd09l  TYPE dd09l,
          lt_dd03p  TYPE ty_dd03p_tt,
          lt_dd05m  TYPE TABLE OF dd05m,
          lt_dd08v  TYPE TABLE OF dd08v,
          lt_dd12v  TYPE dd12vtab,
          lt_dd17v  TYPE dd17vtab,
          lt_dd35v  TYPE TABLE OF dd35v,
          lv_index  LIKE sy-index,
          lt_dd36m  TYPE dd36mttyp,
          ls_extras TYPE ty_tabl_extras.

    FIELD-SYMBOLS: <ls_dd12v>      LIKE LINE OF lt_dd12v,
                   <ls_dd05m>      LIKE LINE OF lt_dd05m,
                   <ls_dd08v>      LIKE LINE OF lt_dd08v,
                   <ls_dd35v>      LIKE LINE OF lt_dd35v,
                   <ls_dd36m>      LIKE LINE OF lt_dd36m,
                   <lg_roworcolst> TYPE any.

    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = lv_name
        langu         = mv_language
      IMPORTING
        gotstate      = lv_state
        dd02v_wa      = ls_dd02v
        dd09l_wa      = ls_dd09l
      TABLES
        dd03p_tab     = lt_dd03p
        dd05m_tab     = lt_dd05m
        dd08v_tab     = lt_dd08v
        dd12v_tab     = lt_dd12v
        dd17v_tab     = lt_dd17v
        dd35v_tab     = lt_dd35v
        dd36m_tab     = lt_dd36m
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from DDIF_TABL_GET' ).
    ENDIF.

    " Check if any active version was returned
    IF lv_state <> 'A'.
      RETURN.
    ENDIF.

    CLEAR: ls_dd02v-as4user,
           ls_dd02v-as4date,
           ls_dd02v-as4time.

* reset numeric field, so XML does not crash
    IF ls_dd02v-prozpuff = ''.
      CLEAR ls_dd02v-prozpuff.
    ENDIF.
    IF ls_dd02v-datmin = ''.
      CLEAR ls_dd02v-datmin.
    ENDIF.
    IF ls_dd02v-datmax = ''.
      CLEAR ls_dd02v-datmax.
    ENDIF.
    IF ls_dd02v-datavg = ''.
      CLEAR ls_dd02v-datavg.
    ENDIF.

    CLEAR: ls_dd09l-as4user,
           ls_dd09l-as4date,
           ls_dd09l-as4time.

    ASSIGN COMPONENT 'ROWORCOLST' OF STRUCTURE ls_dd09l TO <lg_roworcolst>.
    IF sy-subrc = 0 AND <lg_roworcolst> = 'C'.
      CLEAR <lg_roworcolst>. "To avoid diff errors. This field doesn't exists in all releases
    ENDIF.


    LOOP AT lt_dd12v ASSIGNING <ls_dd12v>.
      CLEAR: <ls_dd12v>-as4user,
             <ls_dd12v>-as4date,
             <ls_dd12v>-as4time,
             <ls_dd12v>-dbindex.
    ENDLOOP.

    clear_dd03p_fields( CHANGING ct_dd03p = lt_dd03p ).

* remove foreign keys inherited from .INCLUDEs
    DELETE lt_dd08v WHERE noinherit = 'N'.
    LOOP AT lt_dd05m ASSIGNING <ls_dd05m>.
      CLEAR <ls_dd05m>-tabname.
      CLEAR <ls_dd05m>-leng.
      lv_index = sy-tabix.
      READ TABLE lt_dd08v WITH KEY fieldname = <ls_dd05m>-fieldname TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        DELETE lt_dd05m INDEX lv_index.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_dd08v ASSIGNING <ls_dd08v>.
      CLEAR: <ls_dd08v>-tabname, <ls_dd08v>-ddlanguage.
    ENDLOOP.
    LOOP AT lt_dd35v ASSIGNING <ls_dd35v>.
      CLEAR <ls_dd35v>-tabname.
    ENDLOOP.

* remove inherited search helps
    DELETE lt_dd35v WHERE shlpinher = abap_true.
    LOOP AT lt_dd36m ASSIGNING <ls_dd36m>.
      CLEAR <ls_dd36m>-tabname.
      lv_index = sy-tabix.
      READ TABLE lt_dd35v WITH KEY fieldname = <ls_dd36m>-fieldname TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        DELETE lt_dd36m INDEX lv_index.
      ENDIF.
    ENDLOOP.

    io_xml->add( iv_name = 'DD02V'
                 ig_data = ls_dd02v ).
    IF NOT ls_dd09l IS INITIAL.
      io_xml->add( iv_name = 'DD09L'
                   ig_data = ls_dd09l ).
    ENDIF.
    io_xml->add( iv_name = 'DD03P_TABLE'
                 ig_data = lt_dd03p ).
    io_xml->add( iv_name = 'DD05M_TABLE'
                 ig_data = lt_dd05m ).
    io_xml->add( iv_name = 'DD08V_TABLE'
                 ig_data = lt_dd08v ).
    io_xml->add( iv_name = 'DD12V'
                 ig_data = lt_dd12v ).
    io_xml->add( iv_name = 'DD17V'
                 ig_data = lt_dd17v ).
    io_xml->add( iv_name = 'DD35V_TALE'
                 ig_data = lt_dd35v ).
    io_xml->add( iv_name = 'DD36M'
                 ig_data = lt_dd36m ).

    IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
      serialize_texts( io_xml ).
    ENDIF.

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_tabl ).

    serialize_idoc_segment( io_xml ).

    ls_extras = read_extras( lv_name ).
    io_xml->add( iv_name = c_s_dataname-tabl_extras
                 ig_data = ls_extras ).

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
endclass. "ZCL_ABAPGIT_OBJECT_TABL implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_TRAN <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_tran=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_tran=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_object_tran=======ccau.

*CLASS zcl_abapgit_object_tran DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KW5KSFY.


class LCL_ABAPGIT_OBJECT_TRAN implementation.
*"* method's implementations
*include methods.
  METHOD add_data.

    DATA: ls_bcdata LIKE LINE OF mt_bcdata.

    ls_bcdata-fnam = iv_fnam.
    ls_bcdata-fval = iv_fval.
    APPEND ls_bcdata TO mt_bcdata.

  ENDMETHOD.
  METHOD call_se93.

    DATA: lt_message TYPE STANDARD TABLE OF bdcmsgcoll.
    DATA lv_msg TYPE string.

    FIELD-SYMBOLS: <ls_message> TYPE bdcmsgcoll.


    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      EXPORTING
        tcode     = 'SE93'
        mode_val  = 'N'
      TABLES
        using_tab = mt_bcdata
        mess_tab  = lt_message
      EXCEPTIONS
        OTHERS    = 1.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error deserializing { ms_item-obj_type } { ms_item-obj_name }| ).
    ENDIF.

    LOOP AT lt_message ASSIGNING <ls_message> WHERE msgtyp CA 'EAX'.
      MESSAGE ID <ls_message>-msgid
        TYPE <ls_message>-msgtyp
        NUMBER <ls_message>-msgnr
        WITH <ls_message>-msgv1 <ls_message>-msgv2 <ls_message>-msgv3 <ls_message>-msgv4
        INTO lv_msg.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDLOOP.

  ENDMETHOD.
  METHOD clear_functiongroup_globals.
    TYPES ty_param_vari TYPE abap_bool.

    DATA lt_error_list TYPE STANDARD TABLE OF rsmp_check WITH DEFAULT KEY.
    FIELD-SYMBOLS <lv_param_vari> TYPE ty_param_vari.

    " only way to clear global fields in function group
    CALL FUNCTION 'RS_TRANSACTION_INCONSISTENCIES'
      EXPORTING
        transaction_code = 'ZTHISTCODENEVEREXIST'
      TABLES
        error_list       = lt_error_list
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      "Expected - fine

      " but there is no other way to clear this field
      ASSIGN ('(SAPLSEUK)PARAM_VARI') TO <lv_param_vari>.
      IF sy-subrc = 0.
        CLEAR <lv_param_vari>.
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD deserialize_oo_transaction.

    " You should remember that we don't use batch input just for fun,
    " but because FM RPY_TRANSACTION_INSERT doesn't support OO transactions.

    DATA: ls_bcdata  TYPE bdcdata.


    CLEAR mt_bcdata.

    ls_bcdata-program  = 'SAPLSEUK'.
    ls_bcdata-dynpro   = '0390'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO mt_bcdata.

    add_data( iv_fnam = 'TSTC-TCODE'
              iv_fval = is_tstc-tcode ).

    IF Lif_abapgit_object~exists( ) = abap_true.

      add_data( iv_fnam = 'BDC_OKCODE'
                iv_fval = '=CHNG' ).

    ELSE.

      add_data( iv_fnam = 'BDC_OKCODE'
                iv_fval = '=ADD' ).

    ENDIF.

    ls_bcdata-program  = 'SAPLSEUK'.
    ls_bcdata-dynpro   = '0300'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO mt_bcdata.

    add_data( iv_fnam     = 'TSTCT-TTEXT'
              iv_fval     = is_tstct-ttext ).

    add_data( iv_fnam     = 'RSSTCD-S_CLASS'
              iv_fval     = 'X' ).

    add_data( iv_fnam = 'BDC_OKCODE'
              iv_fval = '=ENTR' ).

    ls_bcdata-program  = 'SAPLSEUK'.
    ls_bcdata-dynpro   = '0360'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO mt_bcdata.

    add_data( iv_fnam     = 'RSSTCD-S_TRFRAME'
              iv_fval     = is_rsstcd-s_trframe ).

    add_data( iv_fnam     = 'RSSTCD-S_UPDTASK'
              iv_fval     = is_rsstcd-s_updtask ).

    add_data( iv_fnam = 'BDC_OKCODE'
              iv_fval = '=TR_FRAMEWORK' ).

    ls_bcdata-program  = 'SAPLSEUK'.
    ls_bcdata-dynpro   = '0360'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO mt_bcdata.

    add_data( iv_fnam     = 'RSSTCD-CLASSNAME'
              iv_fval     = is_rsstcd-classname ).

    add_data( iv_fnam     = 'RSSTCD-METHOD'
              iv_fval     = is_rsstcd-method ).

    IF is_rsstcd-s_local IS NOT INITIAL.
      add_data( iv_fnam     = 'RSSTCD-S_LOCAL'
                iv_fval     = is_rsstcd-s_local ).
    ENDIF.

    IF is_rsstcd-s_updlok IS NOT INITIAL.
      add_data( iv_fnam     = 'RSSTCD-S_UPDLOK'
                iv_fval     = is_rsstcd-s_updlok ).
    ENDIF.

    add_data( iv_fnam     = 'TSTC-PGMNA'
              iv_fval     = is_tstc-pgmna ).

    IF is_tstcc-s_webgui = '2'.

      add_data( iv_fnam     = 'G_IAC_EWT'
                iv_fval     = abap_true ).

      add_data( iv_fnam = 'BDC_OKCODE'
                iv_fval = 'MAKE_PROFI' ).

      ls_bcdata-program  = 'SAPLSEUK'.
      ls_bcdata-dynpro   = '0360'.
      ls_bcdata-dynbegin = 'X'.
      APPEND ls_bcdata TO mt_bcdata.

    ELSEIF is_tstcc-s_webgui IS NOT INITIAL.

      add_data( iv_fnam     = 'TSTCC-S_WEBGUI'
                iv_fval     = is_tstcc-s_webgui ).

    ENDIF.

    IF is_tstcc-s_pervas IS NOT INITIAL.
      add_data( iv_fnam     = 'TSTCC-S_PERVAS'
                iv_fval     = is_tstcc-s_pervas ).
    ENDIF.

    IF is_tstcc-s_service IS NOT INITIAL.
      add_data( iv_fnam     = 'TSTCC-S_SERVICE'
                iv_fval     = is_tstcc-s_service ).
    ENDIF.

    IF is_tstcc-s_platin IS NOT INITIAL.
      add_data( iv_fnam     = 'TSTCC-S_PLATIN'
                iv_fval     = is_tstcc-s_platin ).
    ENDIF.

    IF is_tstcc-s_win32 IS NOT INITIAL.
      add_data( iv_fnam     = 'TSTCC-S_WIN32'
                iv_fval     = is_tstcc-s_win32 ).
    ENDIF.

    add_data( iv_fnam = 'BDC_OKCODE'
              iv_fval = '=WB_SAVE' ).

    ls_bcdata-program  = 'SAPLSTRD'.
    ls_bcdata-dynpro   = '0100'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO mt_bcdata.

    add_data( iv_fnam     = 'KO007-L_DEVCLASS'
              iv_fval     = iv_package ).

    add_data( iv_fnam = 'BDC_OKCODE'
              iv_fval = '=ADD' ).

    ls_bcdata-program  = 'BDC_OKCODE'.
    ls_bcdata-dynpro   = '0360'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO mt_bcdata.

    add_data( iv_fnam = 'BDC_OKCODE'
              iv_fval = '=WB_BACK' ).

    ls_bcdata-program  = 'BDC_OKCODE'.
    ls_bcdata-dynpro   = '0360'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO mt_bcdata.

    add_data( iv_fnam = 'BDC_OKCODE'
              iv_fval = '=WB_BACK' ).

    call_se93( ).

  ENDMETHOD.
  METHOD deserialize_texts.

    DATA lt_tpool_i18n TYPE TABLE OF tstct.

    FIELD-SYMBOLS <ls_tpool> LIKE LINE OF lt_tpool_i18n.

    " Read XML-files data
    ii_xml->read( EXPORTING iv_name = 'I18N_TPOOL'
                  CHANGING  cg_data = lt_tpool_i18n ).

    mo_i18n_params->trim_saplang_keyed_table(
      EXPORTING
        iv_lang_field_name = 'SPRSL'
      CHANGING
        ct_tab = lt_tpool_i18n ).

    " Force t-code name (security reasons)
    LOOP AT lt_tpool_i18n ASSIGNING <ls_tpool>.
      <ls_tpool>-tcode = ms_item-obj_name.
    ENDLOOP.

    IF lines( lt_tpool_i18n ) > 0.
      MODIFY tstct FROM TABLE lt_tpool_i18n.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'Update of t-code translations failed' ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD is_variant_transaction.

    rv_variant_transaction = boolc( is_tstcp-param(1) = '@' ).

  ENDMETHOD.
  METHOD save_authorizations.

    CONSTANTS: lc_hex_chk TYPE x VALUE '04'.
    DATA: ls_transaction TYPE tstc.

    transaction_read( EXPORTING iv_transaction = iv_transaction
                      IMPORTING es_transaction = ls_transaction ).

    DELETE FROM tstca WHERE tcode = iv_transaction.

    IF ls_transaction IS NOT INITIAL.
      INSERT tstca FROM TABLE it_authorizations.
      ls_transaction-cinfo = ls_transaction-cinfo + lc_hex_chk.
      UPDATE tstc SET cinfo = ls_transaction-cinfo WHERE tcode = ls_transaction-tcode.
    ENDIF.

  ENDMETHOD.
  METHOD serialize_texts.

    DATA lt_tpool_i18n TYPE TABLE OF tstct.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      RETURN.
    ENDIF.

    " Skip main language - it was already serialized
    " Don't serialize t-code itself
    SELECT sprsl ttext
      INTO CORRESPONDING FIELDS OF TABLE lt_tpool_i18n
      FROM tstct
      WHERE sprsl <> mv_language
      AND   tcode = ms_item-obj_name
      ORDER BY sprsl ##TOO_MANY_ITAB_FIELDS.            "#EC CI_GENBUFF

    mo_i18n_params->trim_saplang_keyed_table(
      EXPORTING
        iv_lang_field_name = 'SPRSL'
      CHANGING
        ct_tab = lt_tpool_i18n ).

    IF lines( lt_tpool_i18n ) > 0.
      SORT lt_tpool_i18n BY sprsl ASCENDING.
      ii_xml->add( iv_name = 'I18N_TPOOL'
                   ig_data = lt_tpool_i18n ).
    ENDIF.

  ENDMETHOD.
  METHOD set_oo_parameters.

    DATA: ls_param LIKE LINE OF it_rsparam.

    IF cs_rsstcd-call_tcode = c_oo_tcode.
      cs_rsstcd-s_trframe = c_true.
      LOOP AT it_rsparam INTO ls_param.
        CASE ls_param-field.
          WHEN c_oo_frclass.
            cs_rsstcd-classname = ls_param-value.
          WHEN c_oo_frmethod.
            cs_rsstcd-method   = ls_param-value.
          WHEN c_oo_frupdtask.
            IF ls_param-value = c_oo_synchron.
              cs_rsstcd-s_upddir  = c_true.
              cs_rsstcd-s_updtask = c_false.
              cs_rsstcd-s_updlok  = c_false.
            ELSEIF ls_param-value = c_oo_asynchron.
              cs_rsstcd-s_upddir  = c_false.
              cs_rsstcd-s_updtask = c_true.
              cs_rsstcd-s_updlok  = c_false.
            ELSE.
              cs_rsstcd-s_upddir  = c_false.
              cs_rsstcd-s_updtask = c_false.
              cs_rsstcd-s_updlok  = c_true.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD shift_param.

    DATA: ls_param  LIKE LINE OF ct_rsparam,
          lv_fdpos  TYPE sy-fdpos,
          lv_length TYPE i.

    FIELD-SYMBOLS <lg_f> TYPE any.


    DO 254 TIMES.
      IF cs_tstcp-param = space.
        EXIT.
      ENDIF.
      CLEAR ls_param.
      IF cs_tstcp-param CA '='.
        CHECK sy-fdpos <> 0.
        ASSIGN cs_tstcp-param(sy-fdpos) TO <lg_f>.
        ls_param-field = <lg_f>.
        IF ls_param-field(1) = space.
          SHIFT ls_param-field.
        ENDIF.
        lv_fdpos = sy-fdpos + 1.
        SHIFT cs_tstcp-param BY lv_fdpos PLACES.
        IF cs_tstcp-param CA ';'.
          IF sy-fdpos <> 0.
            ASSIGN cs_tstcp-param(sy-fdpos) TO <lg_f>.
            ls_param-value = <lg_f>.
            IF ls_param-value(1) = space.
              SHIFT ls_param-value.
            ENDIF.
          ENDIF.
          lv_fdpos = sy-fdpos + 1.
          SHIFT cs_tstcp-param BY lv_fdpos PLACES.
          APPEND ls_param TO ct_rsparam.
        ELSE.
          lv_length = strlen( cs_tstcp-param ).
          CHECK lv_length > 0.
          ASSIGN cs_tstcp-param(lv_length) TO <lg_f>.
          ls_param-value = <lg_f>.
          IF ls_param-value(1) = space.
            SHIFT ls_param-value.
          ENDIF.
          lv_length = lv_length + 1.
          SHIFT cs_tstcp-param BY lv_length PLACES.
          APPEND ls_param TO ct_rsparam.
        ENDIF.
      ENDIF.
    ENDDO.

  ENDMETHOD.
  METHOD split_parameters.
* see subroutine split_parameters in include LSEUKF01

    DATA: lv_off       TYPE i,
          lv_fdpos     TYPE sy-fdpos,
          lv_param_beg TYPE i.


    CLEAR cs_rsstcd-s_vari.

    IF cs_tstcp-param(1) = '\'.             " OO-Transaktion ohne FR
      split_parameters_comp( EXPORTING ig_type = c_oo_program
                                       ig_param = cs_tstcp-param
                             CHANGING  cg_value = cs_tstc-pgmna ).
      split_parameters_comp( EXPORTING ig_type = c_oo_class
                                       ig_param = cs_tstcp-param
                             CHANGING  cg_value = cs_rsstcd-classname ).
      split_parameters_comp( EXPORTING ig_type = c_oo_method
                                       ig_param = cs_tstcp-param
                             CHANGING  cg_value = cs_rsstcd-method ).

      IF NOT cs_tstc-pgmna IS INITIAL.
        cs_rsstcd-s_local = c_true.
      ENDIF.
      RETURN.
    ELSEIF cs_tstcp-param(1) = '@'.         " Transaktionsvariante
      cs_rsstcd-s_vari = c_true.
      IF cs_tstcp-param(2) = '@@'.
        cs_rsstcd-s_ind_vari = c_true.
        lv_off = 2.
      ELSE.
        CLEAR cs_rsstcd-s_ind_vari.
        lv_off = 1.
      ENDIF.
      IF cs_tstcp-param CA ' '.
      ENDIF.
      lv_fdpos = sy-fdpos - lv_off.
      IF lv_fdpos > 0.
        cs_rsstcd-call_tcode = cs_tstcp-param+lv_off(sy-fdpos).
        lv_fdpos = lv_fdpos + 1 + lv_off.
        cs_rsstcd-variant = cs_tstcp-param+lv_fdpos.
      ENDIF.
    ELSEIF cs_tstcp-param(1) = '/'.
      cs_rsstcd-st_tcode = c_true.
      cs_rsstcd-st_prog  = space.
      IF cs_tstcp-param+1(1) = '*'.
        cs_rsstcd-st_skip_1 = c_true.
      ELSE.
        CLEAR cs_rsstcd-st_skip_1.
      ENDIF.
      IF cs_tstcp-param CA ' '.
      ENDIF.
      lv_param_beg = sy-fdpos + 1.
      lv_fdpos = sy-fdpos - 2.
      IF lv_fdpos > 0.
        cs_rsstcd-call_tcode = cs_tstcp-param+2(lv_fdpos).
      ENDIF.
      SHIFT cs_tstcp-param BY lv_param_beg PLACES.
    ELSE.
      cs_rsstcd-st_tcode = space.
      cs_rsstcd-st_prog  = c_true.
    ENDIF.

    shift_param(
      CHANGING ct_rsparam = ct_rsparam
               cs_tstcp   = cs_tstcp ).

    set_oo_parameters(
      EXPORTING it_rsparam = ct_rsparam
      CHANGING cs_rsstcd = cs_rsstcd ).

  ENDMETHOD.
  METHOD split_parameters_comp.
    DATA: lv_off TYPE i.

    IF ig_param CS ig_type.
      lv_off = sy-fdpos + strlen( ig_type ).
      cg_value = ig_param+lv_off.
      IF cg_value CA '\'.
        CLEAR cg_value+sy-fdpos.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD transaction_read.

    DATA: lt_tcodes   TYPE TABLE OF tstc,
          lt_gui_attr TYPE TABLE OF tstcc.

    CLEAR: es_transaction, es_gui_attr.

    CALL FUNCTION 'RPY_TRANSACTION_READ'
      EXPORTING
        transaction      = iv_transaction
      TABLES
        tcodes           = lt_tcodes
        gui_attributes   = lt_gui_attr
      EXCEPTIONS
        permission_error = 1
        cancelled        = 2
        not_found        = 3
        object_not_found = 4
        OTHERS           = 5.
    IF sy-subrc = 4 OR sy-subrc = 3.
      RETURN.
    ELSEIF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    READ TABLE lt_tcodes INDEX 1 INTO es_transaction.
    ASSERT sy-subrc = 0.
    READ TABLE lt_gui_attr INDEX 1 INTO es_gui_attr.
    ASSERT sy-subrc = 0.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    " Changed-by-user is not stored in transaction metadata
    " Instead, use owner of last transport or object directory

    DATA lv_transport TYPE trkorr.

    lv_transport = Lcl_abapgit_factory=>get_cts_api( )->get_transport_for_object( ms_item ).

    IF lv_transport IS NOT INITIAL.
      SELECT SINGLE as4user FROM e070 INTO rv_user WHERE trkorr = lv_transport.
    ELSE.
      SELECT SINGLE author FROM tadir INTO rv_user
        WHERE pgmid = 'R3TR' AND object = ms_item-obj_type AND obj_name = ms_item-obj_name.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_transaction TYPE tstc-tcode.


    lv_transaction = ms_item-obj_name.

    CALL FUNCTION 'RPY_TRANSACTION_DELETE'
      EXPORTING
        transaction      = lv_transaction
      EXCEPTIONS
        not_excecuted    = 1
        object_not_found = 0
        OTHERS           = 3.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    CONSTANTS: lc_hex_tra TYPE x VALUE '00',
*               lc_hex_men TYPE x VALUE '01',
               lc_hex_par TYPE x VALUE '02',
               lc_hex_rep TYPE x VALUE '80',
*               lc_hex_rpv TYPE x VALUE '10',
               lc_hex_obj TYPE x VALUE '08'.

    DATA: lv_dynpro       TYPE d020s-dnum,
          ls_tstc         TYPE tstc,
          lv_type         TYPE rglif-docutype,
          ls_tstct        TYPE tstct,
          ls_tstcc        TYPE tstcc,
          ls_tstcp        TYPE tstcp,
          lt_tstca        TYPE ty_tstca,
          lt_param_values TYPE ty_param_values,
          ls_rsstcd       TYPE rsstcd.


    IF Lif_abapgit_object~exists( ) = abap_true.
      Lif_abapgit_object~delete( iv_package   = iv_package
                                 iv_transport = iv_transport ).
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'TSTC'
                  CHANGING cg_data = ls_tstc ).
    io_xml->read( EXPORTING iv_name = 'TSTCC'
                  CHANGING cg_data = ls_tstcc ).
    io_xml->read( EXPORTING iv_name = 'TSTCT'
                  CHANGING cg_data = ls_tstct ).
    io_xml->read( EXPORTING iv_name = 'TSTCP'
                  CHANGING cg_data = ls_tstcp ).
    io_xml->read( EXPORTING iv_name = 'AUTHORIZATIONS'
                  CHANGING cg_data = lt_tstca ).

    lv_dynpro = ls_tstc-dypno.

    IF ls_tstc-cinfo O lc_hex_rep.
      lv_type = c_variant_type-report.
    ELSEIF ls_tstc-cinfo O lc_hex_obj.
      lv_type = c_variant_type-object.
    ELSEIF ls_tstc-cinfo O lc_hex_par.
      IF is_variant_transaction( ls_tstcp ) = abap_true.
        lv_type = c_variant_type-variant.
      ELSE.
        lv_type = c_variant_type-parameters.
      ENDIF.
    ELSEIF ls_tstc-cinfo O lc_hex_tra.
      lv_type = c_variant_type-dialog.
    ELSE.
      Lcx_abapgit_exception=>raise( 'Transaction, unknown CINFO' ).
    ENDIF.

    IF ls_tstcp IS NOT INITIAL.
      split_parameters( CHANGING ct_rsparam = lt_param_values
                                 cs_rsstcd  = ls_rsstcd
                                 cs_tstcp   = ls_tstcp
                                 cs_tstc    = ls_tstc ).
    ENDIF.

    CASE lv_type.
      WHEN c_variant_type-object.

        deserialize_oo_transaction( iv_package      = iv_package
                                    is_tstc         = ls_tstc
                                    is_tstcc        = ls_tstcc
                                    is_tstct        = ls_tstct
                                    is_rsstcd       = ls_rsstcd ).

      WHEN OTHERS.

        clear_functiongroup_globals( ).

        corr_insert( iv_package ).

        CALL FUNCTION 'RPY_TRANSACTION_INSERT'
          EXPORTING
            transaction             = ls_tstc-tcode
            program                 = ls_tstc-pgmna
            dynpro                  = lv_dynpro
            language                = mv_language
            development_class       = iv_package
            transaction_type        = lv_type
            shorttext               = ls_tstct-ttext
            called_transaction      = ls_rsstcd-call_tcode
            called_transaction_skip = ls_rsstcd-st_skip_1
            variant                 = ls_rsstcd-variant
            cl_independend          = ls_rsstcd-s_ind_vari
            html_enabled            = ls_tstcc-s_webgui
            java_enabled            = ls_tstcc-s_platin
            wingui_enabled          = ls_tstcc-s_win32
            suppress_corr_insert    = abap_true
          TABLES
            param_values            = lt_param_values
          EXCEPTIONS
            cancelled               = 1
            already_exist           = 2
            permission_error        = 3
            name_not_allowed        = 4
            name_conflict           = 5
            illegal_type            = 6
            object_inconsistent     = 7
            db_access_error         = 8
            OTHERS                  = 9.
        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise_t100( ).
        ENDIF.

    ENDCASE.

    IF lt_tstca IS NOT INITIAL.
      save_authorizations( iv_transaction    = ls_tstc-tcode
                           it_authorizations = lt_tstca ).
    ENDIF.

    IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
      deserialize_texts( io_xml ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_tcode TYPE tstc-tcode.


    SELECT SINGLE tcode FROM tstc INTO lv_tcode
      WHERE tcode = ms_item-obj_name.                   "#EC CI_GENBUFF
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
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'EEUDB'
                                            iv_argument    = ms_item-obj_name
                                            iv_prefix      = 'TN' ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.


    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPLSEUK'.
    <ls_bdcdata>-dynpro   = '0390'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=SHOW'.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'TSTC-TCODE'.
    <ls_bdcdata>-fval = ms_item-obj_name.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode      = 'SE93'
      it_bdcdata    = lt_bdcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_transaction TYPE tstc-tcode,
          ls_tcode       TYPE tstc,
          ls_tstct       TYPE tstct,
          ls_tstcp       TYPE tstcp,
          lt_tstca       TYPE ty_tstca,
          ls_gui_attr    TYPE tstcc.


    lv_transaction = ms_item-obj_name.

    transaction_read( EXPORTING iv_transaction = lv_transaction
                      IMPORTING es_transaction = ls_tcode
                                es_gui_attr    = ls_gui_attr ).
    IF ls_tcode IS INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM tstct INTO ls_tstct
      WHERE sprsl = mv_language
      AND tcode = lv_transaction.         "#EC CI_SUBRC "#EC CI_GENBUFF

    SELECT SINGLE * FROM tstcp INTO ls_tstcp
      WHERE tcode = lv_transaction.       "#EC CI_SUBRC "#EC CI_GENBUFF

    SELECT * FROM tstca INTO TABLE lt_tstca
      WHERE tcode = lv_transaction
      ORDER BY PRIMARY KEY.
    IF sy-subrc <> 0.
      CLEAR: lt_tstca.
    ENDIF.

    io_xml->add( iv_name = 'TSTC'
                 ig_data = ls_tcode ).
    io_xml->add( iv_name = 'TSTCC'
                 ig_data = ls_gui_attr ).
    io_xml->add( iv_name = 'TSTCT'
                 ig_data = ls_tstct ).
    IF ls_tstcp IS NOT INITIAL.
      io_xml->add( iv_name = 'TSTCP'
                   ig_data = ls_tstcp ).
    ENDIF.
    io_xml->add( iv_name = 'AUTHORIZATIONS'
                 ig_data = lt_tstca ).

    IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
      serialize_texts( io_xml ).
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
endclass. "ZCL_ABAPGIT_OBJECT_TRAN implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_TYPE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_type=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_type=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_TYPE implementation.
*"* method's implementations
*include methods.
  METHOD create.

    DATA: lv_progname  TYPE reposrc-progname,
          lv_typegroup TYPE rsedd0-typegroup.


    lv_typegroup = ms_item-obj_name.

    CALL FUNCTION 'RS_DD_TYGR_INSERT_SOURCES'
      EXPORTING
        typegroupname        = lv_typegroup
        ddtext               = iv_ddtext
        corrnum              = ''
        devclass             = iv_devclass
      TABLES
        source               = it_source
      EXCEPTIONS
        already_exists       = 1
        not_executed         = 2
        permission_failure   = 3
        object_not_specified = 4
        illegal_name         = 5
        OTHERS               = 6.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CONCATENATE c_prefix lv_typegroup INTO lv_progname.
    UPDATE progdir SET uccheck = Lif_abapgit_aff_types_v1=>co_abap_language_version_src-standard
      WHERE name = lv_progname.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error setting uccheck' ).
    ENDIF.

  ENDMETHOD.
  METHOD read.

    DATA: lv_typdname  TYPE rsedd0-typegroup,
          lt_psmodisrc TYPE TABLE OF smodisrc,
          lt_psmodilog TYPE TABLE OF smodilog,
          lt_ptrdir    TYPE TABLE OF trdir.


    SELECT SINGLE ddtext FROM ddtypet
      INTO ev_ddtext
      WHERE typegroup  = ms_item-obj_name
        AND ddlanguage = mv_language.

    lv_typdname = ms_item-obj_name.

    " Get active version, ignore errors if not found
    CALL FUNCTION 'TYPD_GET_OBJECT'
      EXPORTING
        typdname          = lv_typdname
      TABLES
        psmodisrc         = lt_psmodisrc
        psmodilog         = lt_psmodilog
        psource           = et_source
        ptrdir            = lt_ptrdir
      EXCEPTIONS
        version_not_found = 1
        reps_not_exist    = 2
        OTHERS            = 3 ##FM_SUBRC_OK.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    DATA lv_prog TYPE progname.

    CONCATENATE '%_C' ms_item-obj_name INTO lv_prog.

    SELECT SINGLE unam FROM reposrc INTO rv_user
      WHERE progname = lv_prog AND r3state = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    delete_ddic( 'G' ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_ddtext    TYPE ddtypet-ddtext,
          lt_source    TYPE abaptxt255_tab,
          lv_progname  TYPE reposrc-progname,
          lv_typegroup TYPE rsedd0-typegroup.


    lv_typegroup = ms_item-obj_name.


    io_xml->read( EXPORTING iv_name = 'DDTEXT'
                  CHANGING cg_data = lv_ddtext ).

    lt_source = Lif_abapgit_object~mo_files->read_abap( ).

    IF Lif_abapgit_object~exists( ) = abap_false.
      create( iv_ddtext   = lv_ddtext
              it_source   = lt_source
              iv_devclass = iv_package ).
    ELSE.
      CONCATENATE c_prefix lv_typegroup INTO lv_progname.

      Lcl_abapgit_factory=>get_sap_report( )->insert_report(
        iv_name    = lv_progname
        iv_package = iv_package
        iv_version = Lif_abapgit_aff_types_v1=>co_abap_language_version_src-standard
        it_source  = lt_source ).
    ENDIF.

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_progname TYPE progname,
          lv_state    TYPE r3state.

    lv_progname = |%_C{ ms_item-obj_name }|.
    SELECT SINGLE state
      FROM progdir
      INTO lv_state
      WHERE name = lv_progname.                         "#EC CI_NOORDER
    IF lv_state IS NOT INITIAL.
      rv_bool = abap_true.
    ENDIF.

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
    " Covered by ZCL_ABAPGIT_OBJECT=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_ddtext TYPE ddtypet-ddtext,
          lt_source TYPE abaptxt255_tab.


    read( IMPORTING ev_ddtext = lv_ddtext
                    et_source = lt_source ).

    IF lt_source IS INITIAL.
      RETURN.
    ENDIF.

    io_xml->add( iv_name = 'DDTEXT'
                 ig_data = lv_ddtext ).

    Lif_abapgit_object~mo_files->add_abap( lt_source ).

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
endclass. "ZCL_ABAPGIT_OBJECT_TYPE implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_UENO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ueno=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ueno=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_UENO implementation.
*"* method's implementations
*include methods.
  METHOD build_text_name.

    TYPES BEGIN OF ty_text_name.
    TYPES id TYPE c LENGTH 4.
    TYPES entity TYPE c LENGTH 26.
    TYPES modifier TYPE c LENGTH 2.
    TYPES END OF ty_text_name.

    DATA ls_text_name TYPE ty_text_name.

    ls_text_name-id = iv_id.
    ls_text_name-entity = mv_entity_id.
    ls_text_name-modifier = 'A%'.

    rv_result = ls_text_name.

  ENDMETHOD.
  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_entity_id = is_item-obj_name.

  ENDMETHOD.
  METHOD delete_docu_uen.

    DATA lt_dm02l TYPE STANDARD TABLE OF dm02l WITH DEFAULT KEY.
    DATA ls_dm02l TYPE dm02l.

    SELECT *
      FROM dm02l
      INTO TABLE lt_dm02l
      WHERE entid = mv_entity_id
      ORDER BY PRIMARY KEY.

    LOOP AT lt_dm02l INTO ls_dm02l.

      CALL FUNCTION 'SDU_DOCU_DELETE'
        EXPORTING
          key1     = ls_dm02l-entid
          key2     = ls_dm02l-as4local
          key3     = '00'
          langu    = mv_language
          obj_id   = 'UENC' "Entity Comments
        EXCEPTIONS
          ret_code = 0.

      CALL FUNCTION 'SDU_DOCU_DELETE'
        EXPORTING
          key1     = ls_dm02l-entid
          key2     = ls_dm02l-as4local
          key3     = '00'
          langu    = mv_language
          obj_id   = 'UEND' "Entity Definition
        EXCEPTIONS
          ret_code = 0.

      CALL FUNCTION 'SDU_DOCU_DELETE'
        EXPORTING
          key1     = ls_dm02l-entid
          key2     = ls_dm02l-as4local
          key3     = '00'
          langu    = mv_language
          obj_id   = 'UENE' "Entity Example
        EXCEPTIONS
          ret_code = 0.

    ENDLOOP.

  ENDMETHOD.
  METHOD delete_docu_url.

    DATA lt_dm42s TYPE STANDARD TABLE OF dm42s WITH DEFAULT KEY.
    DATA ls_dm42s LIKE LINE OF lt_dm42s.

    SELECT *
      FROM dm42s
      INTO TABLE lt_dm42s
      WHERE entidto = mv_entity_id
      ORDER BY PRIMARY KEY.

    LOOP AT lt_dm42s INTO ls_dm42s.

      CALL FUNCTION 'SDU_DOCU_DELETE'
        EXPORTING
          langu    = mv_language
          obj_id   = 'URL1'
          key1     = ls_dm42s-entidto
          key2     = ls_dm42s-as4local
          key3     = ls_dm42s-entidfrom
          key4     = ls_dm42s-ebrolnr
        EXCEPTIONS
          ret_code = 0.

      CALL FUNCTION 'SDU_DOCU_DELETE'
        EXPORTING
          langu    = mv_language
          obj_id   = 'URL2'
          key1     = ls_dm42s-entidto
          key2     = ls_dm42s-as4local
          key3     = ls_dm42s-entidfrom
          key4     = ls_dm42s-ebrolnr
        EXCEPTIONS
          ret_code = 0.

      CALL FUNCTION 'SDU_DOCU_DELETE'
        EXPORTING
          langu    = mv_language
          obj_id   = 'URLC'
          key1     = ls_dm42s-entidto
          key2     = ls_dm42s-as4local
          key3     = ls_dm42s-entidfrom
          key4     = ls_dm42s-ebrolnr
        EXCEPTIONS
          ret_code = 0.

    ENDLOOP.


  ENDMETHOD.
  METHOD delete_docu_usp.

    DATA lt_dm45l TYPE STANDARD TABLE OF dm45l WITH DEFAULT KEY.
    DATA ls_dm45l LIKE LINE OF lt_dm45l.

    SELECT *
      FROM dm45l
      INTO TABLE lt_dm45l
      WHERE entid = ms_item-obj_name
      ORDER BY PRIMARY KEY.

    LOOP AT lt_dm45l INTO ls_dm45l.

      CALL FUNCTION 'SDU_DOCU_DELETE'
        EXPORTING
          langu    = mv_language
          obj_id   = 'USPD'
          key1     = ls_dm45l-entid
          key2     = ls_dm45l-as4local
          key3     = ls_dm45l-spezid
        EXCEPTIONS
          ret_code = 0.

    ENDLOOP.


  ENDMETHOD.
  METHOD deserialize_docu_uen.

    DATA lt_docu TYPE ty_docu_lines.

    io_xml->read( EXPORTING iv_name = 'DOCU_UENC'
                 CHANGING cg_data = lt_docu ).
    deserialize_docu_xxxx( lt_docu ).


    CLEAR lt_docu.
    io_xml->read( EXPORTING iv_name = 'DOCU_UEND'
                 CHANGING cg_data = lt_docu ).
    deserialize_docu_xxxx( lt_docu ).


    CLEAR lt_docu.
    io_xml->read( EXPORTING iv_name = 'DOCU_UENE'
                 CHANGING cg_data = lt_docu ).
    deserialize_docu_xxxx( lt_docu ).

  ENDMETHOD.
  METHOD deserialize_docu_url.

    DATA lt_docu TYPE ty_docu_lines.

    io_xml->read( EXPORTING iv_name = 'DOCU_URL1'
                 CHANGING cg_data = lt_docu ).

    deserialize_docu_xxxx( lt_docu ).

    CLEAR lt_docu.
    io_xml->read( EXPORTING iv_name = 'DOCU_URL2'
                 CHANGING cg_data = lt_docu ).

    deserialize_docu_xxxx( lt_docu ).

    CLEAR lt_docu.
    io_xml->read( EXPORTING iv_name = 'DOCU_URLC'
                 CHANGING cg_data = lt_docu ).

    deserialize_docu_xxxx( lt_docu ).

  ENDMETHOD.
  METHOD deserialize_docu_usp.

    DATA lt_docu TYPE ty_docu_lines.

    io_xml->read( EXPORTING iv_name = 'DOCU_USPD'
                 CHANGING cg_data = lt_docu ).

    deserialize_docu_xxxx( lt_docu ).

  ENDMETHOD.
  METHOD deserialize_docu_xxxx.

    DATA ls_docu LIKE LINE OF it_docu.
    DATA lv_objname TYPE lxeobjname.
    DATA lv_change_flag TYPE char1.
    DATA lv_error_status  TYPE lxestatprc.

    LOOP AT it_docu INTO ls_docu.

      ls_docu-header-tdfuser = sy-uname.
      ls_docu-header-tdfdate = sy-datum.
      ls_docu-header-tdftime = sy-uzeit.
      ls_docu-header-tdfreles = sy-saprl.

      ls_docu-header-tdluser = sy-uname.
      ls_docu-header-tdldate = sy-datum.
      ls_docu-header-tdltime = sy-uzeit.
      ls_docu-header-tdlreles = sy-saprl.

      lv_objname = ls_docu-header-tdname.

      CALL FUNCTION 'LXE_OBJ_DOKU_PUT_XSTRING'
        EXPORTING
          slang       = mv_language
          tlang       = ls_docu-language
          objtype     = ls_docu-header-tdid
          objname     = lv_objname
          header      = ls_docu-header
          content     = ls_docu-content
        IMPORTING
          change_flag = lv_change_flag
          pstatus     = lv_error_status.

    ENDLOOP.


  ENDMETHOD.
  METHOD get_field_rules.

    DATA:
      lt_fields    TYPE TABLE OF string,
      lv_fields    TYPE string,
      lv_table     TYPE tabname,
      lv_field     TYPE string,
      lv_rule      TYPE string,
      lv_rule_iter TYPE string,
      lv_fill_rule TYPE Lif_abapgit_field_rules=>ty_fill_rule,
      lv_prefix    TYPE fieldname,
      lv_suffix    TYPE fieldname.

    ro_result = Lcl_abapgit_field_rules=>create( ).

    " Many tables and fields with date,time,user so we encode them
    APPEND 'DM02L,FL,DTU' TO lt_fields.
    APPEND 'DM02T,L,DTU' TO lt_fields.
    APPEND 'DM03S,FL,DTU' TO lt_fields.
    APPEND 'DM25L,FL,DTU' TO lt_fields.
    APPEND 'DM26L,FL,DTU' TO lt_fields.
    APPEND 'DM42S,FL,DTU' TO lt_fields.
    APPEND 'DM42T,L,DTU' TO lt_fields.
    APPEND 'DM43T,L,DU' TO lt_fields.
    APPEND 'DM45L,FL,DTU' TO lt_fields.
    APPEND 'DM45T,L,DTU' TO lt_fields.
    APPEND 'DM46S,FL,DTU' TO lt_fields.

    LOOP AT lt_fields INTO lv_fields.
      SPLIT lv_fields AT ',' INTO lv_table lv_field lv_rule_iter.

      DO strlen( lv_field ) TIMES.
        CASE lv_field(1).
          WHEN 'F'.
            lv_prefix = 'FST'.
          WHEN 'L'.
            lv_prefix = 'LST'.
        ENDCASE.

        lv_rule = lv_rule_iter.
        DO strlen( lv_rule ) TIMES.
          CASE lv_rule(1).
            WHEN 'D'.
              lv_suffix    = 'DATE'.
              lv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-date.
            WHEN 'T'.
              lv_suffix    = 'TIME'.
              lv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-time.
            WHEN 'U'.
              lv_suffix    = 'USER'.
              lv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user.
          ENDCASE.

          ro_result->add(
            iv_table     = lv_table
            iv_field     = lv_prefix && lv_suffix
            iv_fill_rule = lv_fill_rule ).

          SHIFT lv_rule LEFT.
        ENDDO.

        SHIFT lv_field LEFT.
      ENDDO.

    ENDLOOP.

  ENDMETHOD.
  METHOD get_generic.

    CREATE OBJECT ro_generic
      EXPORTING
        io_field_rules = get_field_rules( )
        is_item        = ms_item
        iv_language    = mv_language.

  ENDMETHOD.
  METHOD is_name_permitted.

    " It is unlikely that a serialized entity will have a name that is not permitted. However
    " there may be reservations in TRESE which could prohibit the entity name.
    " So to be safe, we check. Tx SD11 does this check.

    CALL FUNCTION 'SDU_SAA_CHECK'
      EXPORTING
        obj_name   = ms_item-obj_name
        obj_type   = ms_item-obj_type
      EXCEPTIONS
        wrong_type = 1.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD serialize_docu_uen.

    DATA lt_docu            TYPE ty_docu_lines.

    lt_docu = serialize_docu_xxxx( 'UENC' ).

    io_xml->add( iv_name = 'DOCU_UENC'
                 ig_data = lt_docu ).


    lt_docu = serialize_docu_xxxx( 'UEND' ).

    io_xml->add( iv_name = 'DOCU_UEND'
                 ig_data = lt_docu ).

    lt_docu = serialize_docu_xxxx( 'UENE' ).

    io_xml->add( iv_name = 'DOCU_UENE'
                 ig_data = lt_docu ).
  ENDMETHOD.
  METHOD serialize_docu_url.


    DATA lt_docu            TYPE ty_docu_lines.

    lt_docu = serialize_docu_xxxx( 'URL1' ).
    io_xml->add( iv_name = 'DOCU_URL1'
                 ig_data = lt_docu ).


    lt_docu = serialize_docu_xxxx( 'URL2' ).
    io_xml->add( iv_name = 'DOCU_URL2'
                 ig_data = lt_docu ).

    lt_docu = serialize_docu_xxxx( 'URLC' ).
    io_xml->add( iv_name = 'DOCU_URLC'
                 ig_data = lt_docu ).

  ENDMETHOD.
  METHOD serialize_docu_usp.

    DATA lt_docu            TYPE ty_docu_lines.

    lt_docu = serialize_docu_xxxx( 'USPD' ).

    io_xml->add( iv_name = 'DOCU_USPD'
                 ig_data = lt_docu ).


  ENDMETHOD.
  METHOD serialize_docu_xxxx.

    DATA ls_docu            TYPE ty_docu.
    DATA ls_dokvl           TYPE dokvl.
    DATA lt_dokvl           TYPE STANDARD TABLE OF dokvl.
    DATA lv_error_status    TYPE lxestatprc.
    DATA lv_objname         TYPE lxeobjname.


    ls_dokvl-object = build_text_name( iv_id ).

    SELECT id object langu
      FROM dokvl
      INTO CORRESPONDING FIELDS OF TABLE lt_dokvl
      WHERE id = c_text_object_type
      AND   object LIKE ls_dokvl-object
      ORDER BY PRIMARY KEY ##TOO_MANY_ITAB_FIELDS.

    LOOP AT lt_dokvl INTO ls_dokvl.

      ls_docu-language = ls_dokvl-langu.
      lv_objname = ls_dokvl-object.

      " You are reminded that this function gets the most recent version of the texts.
      CALL FUNCTION 'LXE_OBJ_DOKU_GET_XSTRING'
        EXPORTING
          lang    = ls_docu-language
          objtype = c_text_object_type
          objname = lv_objname
        IMPORTING
          header  = ls_docu-header
          content = ls_docu-content
          itf     = ls_docu-itf
          pstatus = lv_error_status.

      CHECK lv_error_status = 'S'. "Success

      " Administrative information is not
      CLEAR ls_docu-header-tdfuser.
      CLEAR ls_docu-header-tdfdate.
      CLEAR ls_docu-header-tdftime.
      CLEAR ls_docu-header-tdfreles.

      CLEAR ls_docu-header-tdluser.
      CLEAR ls_docu-header-tdldate.
      CLEAR ls_docu-header-tdltime.
      CLEAR ls_docu-header-tdlreles.

      APPEND ls_docu TO rt_result.

    ENDLOOP.


  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE lstuser INTO rv_user
      FROM dm02l
      WHERE entid = mv_entity_id
      AND as4local = c_active_state.

    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    " The deletion of the documentation occurs before the deletion of
    " the associated tables - otherwise we don't know what
    " documentation needs deletion
    delete_docu_uen( ).
    delete_docu_url( ).
    delete_docu_usp( ).

    " the deletion of the tables of the entity
    get_generic( )->delete( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    " Is the entity type name compliant with naming conventions?
    " Entity Type have their own conventions.
    is_name_permitted( ).

    get_generic( )->deserialize(
      iv_package = iv_package
      io_xml     = io_xml ).

    deserialize_docu_uen( io_xml ).
    deserialize_docu_url( io_xml ).
    deserialize_docu_usp( io_xml ).

    " You are reminded that entity types are not relevant for activation.

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

    rv_is_locked = exists_a_lock_entry_for(
      iv_lock_object = 'ESDUM'
      iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    " The function module listed below do not open a new window - so we revert to BDC.
    "    CALL FUNCTION 'SDU_MODEL_SHOW'
    "    CALL FUNCTION 'RS_TOOL_ACCESS'

    DATA lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPMUD00'.
    <ls_bdcdata>-dynpro   = '0100'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=SHOW'.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'RSUD3-ENTI'.
    <ls_bdcdata>-fval = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'RSUD3-OBJ_KEY'.
    <ls_bdcdata>-fval = ms_item-obj_name.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SD11'
      it_bdcdata = lt_bdcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    get_generic( )->serialize( io_xml ).

    serialize_docu_uen( io_xml ).
    serialize_docu_url( io_xml ).
    serialize_docu_usp( io_xml ).

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
endclass. "ZCL_ABAPGIT_OBJECT_UENO implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_VIEW <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_view=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_view=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_VIEW implementation.
*"* method's implementations
*include methods.
  METHOD deserialize_texts.

    DATA:
      lv_name       TYPE ddobjname,
      lt_i18n_langs TYPE TABLE OF langu,
      lt_dd25_texts TYPE ty_dd25_texts,
      ls_dd25v_tmp  TYPE dd25v.

    FIELD-SYMBOLS:
      <lv_lang>      TYPE langu,
      <ls_dd25_text> LIKE LINE OF lt_dd25_texts.

    lv_name = ms_item-obj_name.

    ii_xml->read( EXPORTING iv_name = 'I18N_LANGS'
                  CHANGING  cg_data = lt_i18n_langs ).

    ii_xml->read( EXPORTING iv_name = 'DD25_TEXTS'
                  CHANGING  cg_data = lt_dd25_texts ).

    mo_i18n_params->trim_saplang_list( CHANGING ct_sap_langs = lt_i18n_langs ).

    SORT lt_i18n_langs.
    SORT lt_dd25_texts BY ddlanguage.

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.

      " View description
      ls_dd25v_tmp = is_dd25v.
      READ TABLE lt_dd25_texts ASSIGNING <ls_dd25_text> WITH KEY ddlanguage = <lv_lang>.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |DD25_TEXTS cannot find lang { <lv_lang> } in XML| ).
      ENDIF.
      MOVE-CORRESPONDING <ls_dd25_text> TO ls_dd25v_tmp.
      CALL FUNCTION 'DDIF_VIEW_PUT'
        EXPORTING
          name              = lv_name
          dd25v_wa          = ls_dd25v_tmp
        EXCEPTIONS
          view_not_found    = 1
          name_inconsistent = 2
          view_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD read_view.

    DATA: lv_name TYPE ddobjname.

    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_VIEW_GET'
      EXPORTING
        name          = lv_name
        state         = 'A'
        langu         = iv_language
      IMPORTING
        gotstate      = ev_state
        dd25v_wa      = es_dd25v
        dd09l_wa      = es_dd09l
      TABLES
        dd26v_tab     = et_dd26v
        dd27p_tab     = et_dd27p
        dd28j_tab     = et_dd28j
        dd28v_tab     = et_dd28v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD serialize_texts.

    DATA:
      lv_index           TYPE i,
      ls_dd25v           TYPE dd25v,
      lt_dd25_texts      TYPE ty_dd25_texts,
      lt_i18n_langs      TYPE TABLE OF langu,
      lt_language_filter TYPE Lif_abapgit_environment=>ty_system_language_filter.

    FIELD-SYMBOLS:
      <lv_lang>      LIKE LINE OF lt_i18n_langs,
      <ls_dd25_text> LIKE LINE OF lt_dd25_texts.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      RETURN.
    ENDIF.

    " Collect additional languages, skip main lang - it was serialized already
    lt_language_filter = mo_i18n_params->build_language_filter( ).

    SELECT DISTINCT ddlanguage AS langu INTO TABLE lt_i18n_langs
      FROM dd25v
      WHERE viewname = ms_item-obj_name
      AND ddlanguage IN lt_language_filter
      AND ddlanguage <> mv_language
      ORDER BY langu.                                     "#EC CI_SUBRC

    LOOP AT lt_i18n_langs ASSIGNING <lv_lang>.
      lv_index = sy-tabix.
      CLEAR: ls_dd25v.

      TRY.
          read_view(
            EXPORTING
              iv_language = <lv_lang>
            IMPORTING
              es_dd25v    = ls_dd25v ).

        CATCH Lcx_abapgit_exception.
          CONTINUE.
      ENDTRY.

      IF ls_dd25v-ddlanguage IS INITIAL.
        DELETE lt_i18n_langs INDEX lv_index. " Don't save this lang
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO lt_dd25_texts ASSIGNING <ls_dd25_text>.
      MOVE-CORRESPONDING ls_dd25v TO <ls_dd25_text>.

    ENDLOOP.

    SORT lt_i18n_langs ASCENDING.
    SORT lt_dd25_texts BY ddlanguage ASCENDING.

    IF lines( lt_i18n_langs ) > 0.
      ii_xml->add( iv_name = 'I18N_LANGS'
                   ig_data = lt_i18n_langs ).

      ii_xml->add( iv_name = 'DD25_TEXTS'
                   ig_data = lt_dd25_texts ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE as4user FROM dd25l INTO rv_user
      WHERE viewname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    delete_ddic( 'V' ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_name  TYPE ddobjname,
          ls_dd25v TYPE dd25v,
          ls_dd09l TYPE dd09l,
          lt_dd26v TYPE TABLE OF dd26v,
          lt_dd27p TYPE TABLE OF dd27p,
          lt_dd28j TYPE TABLE OF dd28j,
          lt_dd28v TYPE TABLE OF dd28v.

    FIELD-SYMBOLS: <ls_dd27p> LIKE LINE OF lt_dd27p.

    io_xml->read( EXPORTING iv_name = 'DD25V'
                  CHANGING cg_data = ls_dd25v ).
    io_xml->read( EXPORTING iv_name = 'DD09L'
                  CHANGING cg_data = ls_dd09l ).
    io_xml->read( EXPORTING iv_name = 'DD26V_TABLE'
                  CHANGING cg_data = lt_dd26v ).
    io_xml->read( EXPORTING iv_name = 'DD27P_TABLE'
                  CHANGING cg_data = lt_dd27p ).
    io_xml->read( EXPORTING iv_name = 'DD28J_TABLE'
                  CHANGING cg_data = lt_dd28j ).
    io_xml->read( EXPORTING iv_name = 'DD28V_TABLE'
                  CHANGING cg_data = lt_dd28v ).

    lv_name = ms_item-obj_name. " type conversion

    LOOP AT lt_dd27p ASSIGNING <ls_dd27p>.
      <ls_dd27p>-objpos = sy-tabix.
      <ls_dd27p>-viewname = lv_name.
      " rollname seems to be mandatory in the API, but is typically not defined in the VIEW
      SELECT SINGLE rollname FROM dd03l INTO <ls_dd27p>-rollname
        WHERE tabname = <ls_dd27p>-tabname
        AND fieldname = <ls_dd27p>-fieldname.
      IF <ls_dd27p>-rollnamevi IS INITIAL.
        <ls_dd27p>-rollnamevi = <ls_dd27p>-rollname.
      ENDIF.
    ENDLOOP.

    corr_insert( iv_package = iv_package
                 ig_object_class = 'DICT' ).

    CALL FUNCTION 'DDIF_VIEW_PUT'
      EXPORTING
        name              = lv_name
        dd25v_wa          = ls_dd25v
        dd09l_wa          = ls_dd09l
      TABLES
        dd26v_tab         = lt_dd26v
        dd27p_tab         = lt_dd27p
        dd28j_tab         = lt_dd28j
        dd28v_tab         = lt_dd28v
      EXCEPTIONS
        view_not_found    = 1
        name_inconsistent = 2
        view_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
      deserialize_texts(
        ii_xml   = io_xml
        is_dd25v = ls_dd25v ).
    ENDIF.

    deserialize_longtexts( ii_xml         = io_xml
                           iv_longtext_id = c_longtext_id_view ).

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_viewname TYPE dd25l-viewname,
          lv_ddl_view TYPE abap_bool.

    SELECT SINGLE viewname FROM dd25l INTO lv_viewname
      WHERE viewname = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

    IF rv_bool = abap_true.
      TRY.
          CALL METHOD ('CL_DD_DDL_UTILITIES')=>('CHECK_FOR_DDL_VIEW')
            EXPORTING
              objname     = lv_viewname
            RECEIVING
              is_ddl_view = lv_ddl_view.

          IF lv_ddl_view = abap_true.
            rv_bool = abap_false.
          ENDIF.
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.
    ENDIF.

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
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECT=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_dd25v TYPE dd25v,
          lv_state TYPE ddgotstate,
          ls_dd09l TYPE dd09l,
          lt_dd26v TYPE ty_dd26v,
          lt_dd27p TYPE ty_dd27p,
          lt_dd28j TYPE ty_dd28j,
          lt_dd28v TYPE ty_dd28v.

    FIELD-SYMBOLS: <ls_dd27p> LIKE LINE OF lt_dd27p.

    read_view(
      EXPORTING
        iv_language = mv_language
      IMPORTING
        ev_state    = lv_state
        es_dd25v    = ls_dd25v
        es_dd09l    = ls_dd09l
        et_dd26v    = lt_dd26v
        et_dd27p    = lt_dd27p
        et_dd28j    = lt_dd28j
        et_dd28v    = lt_dd28v ).

    IF ls_dd25v IS INITIAL OR lv_state <> 'A'.
      RETURN.
    ENDIF.

    CLEAR: ls_dd25v-as4user,
           ls_dd25v-as4date,
           ls_dd25v-as4time.

    CLEAR: ls_dd09l-as4user,
           ls_dd09l-as4date,
           ls_dd09l-as4time.

    LOOP AT lt_dd27p ASSIGNING <ls_dd27p>.
      CLEAR: <ls_dd27p>-ddtext,
             <ls_dd27p>-reptext,
             <ls_dd27p>-scrtext_s,
             <ls_dd27p>-scrtext_m,
             <ls_dd27p>-scrtext_l,
             <ls_dd27p>-outputlen,
             <ls_dd27p>-decimals,
             <ls_dd27p>-lowercase,
             <ls_dd27p>-convexit,
             <ls_dd27p>-signflag,
             <ls_dd27p>-flength,
             <ls_dd27p>-domname,
             <ls_dd27p>-datatype,
             <ls_dd27p>-entitytab,
             <ls_dd27p>-inttype,
             <ls_dd27p>-intlen,
             <ls_dd27p>-headlen,
             <ls_dd27p>-scrlen1,
             <ls_dd27p>-scrlen2,
             <ls_dd27p>-scrlen3,
             <ls_dd27p>-memoryid.
      IF <ls_dd27p>-rollchange = abap_false.
        CLEAR <ls_dd27p>-rollnamevi.
      ENDIF.
      CLEAR <ls_dd27p>-ddlanguage.
      CLEAR <ls_dd27p>-rollname.
      CLEAR <ls_dd27p>-viewname.
      CLEAR <ls_dd27p>-objpos.
    ENDLOOP.

    io_xml->add( iv_name = 'DD25V'
                 ig_data = ls_dd25v ).
    io_xml->add( iv_name = 'DD09L'
                 ig_data = ls_dd09l ).
    io_xml->add( ig_data = lt_dd26v
                 iv_name = 'DD26V_TABLE' ).
    io_xml->add( ig_data = lt_dd27p
                 iv_name = 'DD27P_TABLE' ).
    io_xml->add( ig_data = lt_dd28j
                 iv_name = 'DD28J_TABLE' ).
    io_xml->add( ig_data = lt_dd28v
                 iv_name = 'DD28V_TABLE' ).

    IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
      serialize_texts( io_xml ).
    ENDIF.

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_view ).

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
endclass. "ZCL_ABAPGIT_OBJECT_VIEW implementation

*>>>>>>> ZCL_ABAPGIT_PERSIST_MIGRATE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_persist_migrate===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_persist_migrate===ccimp.
CLASS SHRITEFUH64VYIPO5IWUYB3KW5RSFY DEFINITION INHERITING FROM Lcl_abapgit_objects_program FINAL.
  PUBLIC SECTION.
    CLASS-METHODS new
      RETURNING
        VALUE(ro_instance) TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KW5RSFY.
    METHODS get_own_cua
      RETURNING
        VALUE(rs_cua) TYPE ty_cua
      RAISING
        Lcx_abapgit_exception.
    METHODS put_own_cua
      IMPORTING
        is_cua TYPE ty_cua
      RAISING
        Lcx_abapgit_exception.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KW5RSFY IMPLEMENTATION.

  METHOD new.

    DATA ls_item TYPE Lif_abapgit_definitions=>ty_item.

    SELECT SINGLE devclass object obj_name INTO (ls_item-devclass, ls_item-obj_type, ls_item-obj_name)
      FROM tadir
      WHERE pgmid  = 'R3TR'
      AND object   = 'PROG'
      AND obj_name = sy-cprog.

    CREATE OBJECT ro_instance
      EXPORTING
        iv_language = 'E'
        is_item = ls_item.

  ENDMETHOD.

  METHOD get_own_cua.

    rs_cua = serialize_cua( sy-cprog ).

  ENDMETHOD.

  METHOD put_own_cua.

    DATA li_log TYPE REF TO Lif_abapgit_log.

    deserialize_cua(
      is_cua          = is_cua
      iv_program_name = ms_item-obj_name ).

    CREATE OBJECT li_log TYPE Lcl_abapgit_log.
    Lcl_abapgit_objects_activation=>activate( li_log ).
    Lcl_abapgit_objects_activation=>clear( ).

  ENDMETHOD.

ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KW5TSFY DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS get
      RETURNING
        VALUE(rs_cua) TYPE Lcl_abapgit_objects_program=>ty_cua ##NEEDED.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KW5TSFY IMPLEMENTATION.
  METHOD get.
    DATA ls_sta LIKE LINE OF rs_cua-sta.
    DATA ls_fun LIKE LINE OF rs_cua-fun.
    DATA ls_but LIKE LINE OF rs_cua-but.
    DATA ls_pfk LIKE LINE OF rs_cua-pfk.
    DATA ls_set LIKE LINE OF rs_cua-set.
    DATA ls_doc LIKE LINE OF rs_cua-doc.
    rs_cua-adm-pfkcode = '000001'.
    CLEAR ls_sta.
    ls_sta-code = 'DECIDE_DIALOG'.
    ls_sta-modal = 'P'.
    ls_sta-pfkcode = '000001'.
    ls_sta-butcode = '0001'.
    ls_sta-int_note = 'Object list decide dialog toolbar'.
    APPEND ls_sta TO rs_cua-sta.
    CLEAR ls_fun.
    ls_fun-code = '&ILD'.
    ls_fun-textno = '001'.
    ls_fun-text_type = 'S'.
    ls_fun-text_name = 'ICON_FILTER_UNDO'.
    ls_fun-icon_id = '@GD@'.
    ls_fun-fun_text = 'Reset filter'.
    ls_fun-code = '&ILT'.
    ls_fun-textno = '001'.
    ls_fun-text_type = 'S'.
    ls_fun-text_name = 'ICON_FILTER'.
    ls_fun-icon_id = '@4G@'.
    ls_fun-fun_text = 'Set Filter'.
    ls_fun-path = 'F'.
    ls_fun-code = '&ODN'.
    ls_fun-textno = '001'.
    ls_fun-text_type = 'S'.
    ls_fun-text_name = 'ICON_SORT_DOWN'.
    ls_fun-icon_id = '@3F@'.
    ls_fun-fun_text = 'Sort in Descending Order'.
    ls_fun-path = 'O'.
    ls_fun-code = '&OUP'.
    ls_fun-textno = '001'.
    ls_fun-text_type = 'S'.
    ls_fun-text_name = 'ICON_SORT_UP'.
    ls_fun-icon_id = '@3E@'.
    ls_fun-fun_text = 'Sort in Ascending Order'.
    ls_fun-path = 'I'.
    ls_fun-code = 'CANCEL'.
    ls_fun-textno = '001'.
    ls_fun-type = 'E'.
    ls_fun-text_type = 'S'.
    ls_fun-text_name = 'ICON_CANCEL'.
    ls_fun-icon_id = '@0W@'.
    ls_fun-fun_text = 'Cancel'.
    ls_fun-icon_text = 'Cancel'.
    ls_fun-path = 'A'.
    ls_fun-code = 'OK'.
    ls_fun-textno = '001'.
    ls_fun-text_type = 'S'.
    ls_fun-text_name = 'ICON_OKAY'.
    ls_fun-icon_id = '@0V@'.
    ls_fun-fun_text = 'Continue'.
    ls_fun-icon_text = 'Continue'.
    ls_fun-code = 'SEL_ALL'.
    ls_fun-textno = '001'.
    ls_fun-text_type = 'S'.
    ls_fun-text_name = 'ICON_SELECT_ALL'.
    ls_fun-icon_id = '@4B@'.
    ls_fun-fun_text = 'Select All Visible'.
    ls_fun-code = 'SEL_CAT'.
    ls_fun-textno = '001'.
    ls_fun-text_type = 'S'.
    ls_fun-text_name = 'ICON_WD_TOOLBAR'.
    ls_fun-icon_id = '@TF@'.
    ls_fun-fun_text = 'Select category'.
    ls_fun-code = 'SEL_DEL'.
    ls_fun-textno = '001'.
    ls_fun-text_type = 'S'.
    ls_fun-text_name = 'ICON_DESELECT_ALL'.
    ls_fun-icon_id = '@4D@'.
    ls_fun-fun_text = 'Deselect All Visible'.
    ls_fun-code = 'SEL_KEY'.
    ls_fun-textno = '001'.
    ls_fun-text_type = 'S'.
    ls_fun-text_name = 'ICON_SELECT_BLOCK'.
    ls_fun-icon_id = '@4C@'.
    ls_fun-fun_text = 'Mark/Toggle Selected'.
    APPEND ls_fun TO rs_cua-fun.
    CLEAR ls_but.
    ls_but-pfk_code = '000001'.
    ls_but-code = '0001'.
    ls_but-no = '01'.
    ls_but-pfno = '00'.
    ls_but-pfk_code = '000001'.
    ls_but-code = '0001'.
    ls_but-no = '02'.
    ls_but-pfno = 'S'.
    ls_but-pfk_code = '000001'.
    ls_but-code = '0001'.
    ls_but-no = '03'.
    ls_but-pfno = '13'.
    ls_but-pfk_code = '000001'.
    ls_but-code = '0001'.
    ls_but-no = '04'.
    ls_but-pfno = '17'.
    ls_but-pfk_code = '000001'.
    ls_but-code = '0001'.
    ls_but-no = '05'.
    ls_but-pfno = '14'.
    ls_but-pfk_code = '000001'.
    ls_but-code = '0001'.
    ls_but-no = '06'.
    ls_but-pfno = '16'.
    ls_but-pfk_code = '000001'.
    ls_but-code = '0001'.
    ls_but-no = '07'.
    ls_but-pfno = 'S'.
    ls_but-pfk_code = '000001'.
    ls_but-code = '0001'.
    ls_but-no = '08'.
    ls_but-pfno = '05'.
    ls_but-pfk_code = '000001'.
    ls_but-code = '0001'.
    ls_but-no = '09'.
    ls_but-pfno = '06'.
    ls_but-pfk_code = '000001'.
    ls_but-code = '0001'.
    ls_but-no = '10'.
    ls_but-pfno = '07'.
    ls_but-pfk_code = '000001'.
    ls_but-code = '0001'.
    ls_but-no = '11'.
    ls_but-pfno = '08'.
    ls_but-pfk_code = '000001'.
    ls_but-code = '0001'.
    ls_but-no = '12'.
    ls_but-pfno = 'S'.
    ls_but-pfk_code = '000001'.
    ls_but-code = '0001'.
    ls_but-no = '13'.
    ls_but-pfno = '12'.
    APPEND ls_but TO rs_cua-but.
    CLEAR ls_pfk.
    ls_pfk-code = '000001'.
    ls_pfk-pfno = '00'.
    ls_pfk-funcode = 'OK'.
    ls_pfk-funno = '001'.
    ls_pfk-code = '000001'.
    ls_pfk-pfno = '05'.
    ls_pfk-funcode = 'SEL_ALL'.
    ls_pfk-funno = '001'.
    ls_pfk-code = '000001'.
    ls_pfk-pfno = '06'.
    ls_pfk-funcode = 'SEL_DEL'.
    ls_pfk-funno = '001'.
    ls_pfk-code = '000001'.
    ls_pfk-pfno = '07'.
    ls_pfk-funcode = 'SEL_KEY'.
    ls_pfk-funno = '001'.
    ls_pfk-code = '000001'.
    ls_pfk-pfno = '08'.
    ls_pfk-funcode = 'SEL_CAT'.
    ls_pfk-funno = '001'.
    ls_pfk-code = '000001'.
    ls_pfk-pfno = '11'.
    ls_pfk-funcode = 'OK'.
    ls_pfk-funno = '001'.
    ls_pfk-code = '000001'.
    ls_pfk-pfno = '12'.
    ls_pfk-funcode = 'CANCEL'.
    ls_pfk-funno = '001'.
    ls_pfk-code = '000001'.
    ls_pfk-pfno = '13'.
    ls_pfk-funcode = '&ILT'.
    ls_pfk-funno = '001'.
    ls_pfk-code = '000001'.
    ls_pfk-pfno = '14'.
    ls_pfk-funcode = '&OUP'.
    ls_pfk-funno = '001'.
    ls_pfk-code = '000001'.
    ls_pfk-pfno = '16'.
    ls_pfk-funcode = '&ODN'.
    ls_pfk-funno = '001'.
    ls_pfk-code = '000001'.
    ls_pfk-pfno = '17'.
    ls_pfk-funcode = '&ILD'.
    ls_pfk-funno = '001'.
    APPEND ls_pfk TO rs_cua-pfk.
    CLEAR ls_set.
    ls_set-status = 'DECIDE_DIALOG'.
    ls_set-function = '&ILD'.
    ls_set-status = 'DECIDE_DIALOG'.
    ls_set-function = '&ILT'.
    ls_set-status = 'DECIDE_DIALOG'.
    ls_set-function = '&ODN'.
    ls_set-status = 'DECIDE_DIALOG'.
    ls_set-function = '&OUP'.
    ls_set-status = 'DECIDE_DIALOG'.
    ls_set-function = 'CANCEL'.
    ls_set-status = 'DECIDE_DIALOG'.
    ls_set-function = 'OK'.
    ls_set-status = 'DECIDE_DIALOG'.
    ls_set-function = 'SEL_ALL'.
    ls_set-status = 'DECIDE_DIALOG'.
    ls_set-function = 'SEL_CAT'.
    ls_set-status = 'DECIDE_DIALOG'.
    ls_set-function = 'SEL_DEL'.
    ls_set-status = 'DECIDE_DIALOG'.
    ls_set-function = 'SEL_KEY'.
    APPEND ls_set TO rs_cua-set.
    CLEAR ls_doc.
    ls_doc-obj_type = 'P'.
    ls_doc-obj_code = '000001'.
    ls_doc-modal = 'P'.
    ls_doc-int_note = 'Object list decide dialog FK settings'.
    ls_doc-obj_type = 'B'.
    ls_doc-obj_code = '000001'.
    ls_doc-sub_code = '0001'.
    ls_doc-modal = 'P'.
    ls_doc-int_note = 'Object list decide dialog PB settings'.
    APPEND ls_doc TO rs_cua-doc.
  ENDMETHOD.
ENDCLASS.

class LCL_ABAPGIT_PERSIST_MIGRATE implementation.
*"* method's implementations
*include methods.
  METHOD gui_status_create.

    DATA ls_cua TYPE Lcl_abapgit_objects_program=>ty_cua.

    IF Lcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_false.
      RETURN. " No autocreation for full version
    ENDIF.

    IF gui_status_exists( ) = abap_true.
      RETURN.
    ENDIF.

    ls_cua = SHRITEFUH64VYIPO5IWUYB3KW5TSFY=>get( ).

    IF ls_cua IS INITIAL. " Full version or something wrong with abapmerged version
      RETURN.
    ENDIF.

    TRY.
        SHRITEFUH64VYIPO5IWUYB3KW5RSFY=>new( )->put_own_cua( ls_cua ).
      CATCH Lcx_abapgit_exception.
    ENDTRY.

  ENDMETHOD.
  METHOD gui_status_exists.

    DATA ls_own_cua TYPE Lcl_abapgit_objects_program=>ty_cua.
    DATA ls_new_cua TYPE Lcl_abapgit_objects_program=>ty_cua.
    DATA lv_x_own TYPE xstring.
    DATA lv_x_new TYPE xstring.
    DATA lv_h_own TYPE Lif_abapgit_git_definitions=>ty_sha1.
    DATA lv_h_new TYPE Lif_abapgit_git_definitions=>ty_sha1.

    TRY.
        ls_own_cua = SHRITEFUH64VYIPO5IWUYB3KW5RSFY=>new( )->get_own_cua( ).
      CATCH Lcx_abapgit_exception.
    ENDTRY.

    IF ls_own_cua IS INITIAL.
      rv_exists = abap_false.
      RETURN.
    ENDIF.

    ls_new_cua = SHRITEFUH64VYIPO5IWUYB3KW5TSFY=>get( ).
    IF ls_new_cua IS INITIAL.
      rv_exists = abap_true. " own exists and new is not - nothing to compare with
      RETURN.
    ENDIF.

    EXPORT data = ls_own_cua TO DATA BUFFER lv_x_own.
    EXPORT data = ls_new_cua TO DATA BUFFER lv_x_new.

    TRY.
        lv_h_own = Lcl_abapgit_hash=>sha1_raw( lv_x_own ).
        lv_h_new = Lcl_abapgit_hash=>sha1_raw( lv_x_new ).
      CATCH Lcx_abapgit_exception.
        rv_exists = abap_true. " own exists and some issue with calculating hash ... assume own is OK
        RETURN.
    ENDTRY.

    " New exists and differs from own - then it is really new, needs to be installed
    rv_exists = boolc( lv_h_own = lv_h_new ).

  ENDMETHOD.
  METHOD lock_create.

    DATA: lv_obj_name TYPE tadir-obj_name,
          ls_dd25v    TYPE dd25v,
          lt_dd26e    TYPE STANDARD TABLE OF dd26e WITH DEFAULT KEY,
          lt_dd27p    TYPE STANDARD TABLE OF dd27p WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_dd26e> LIKE LINE OF lt_dd26e,
                   <ls_dd27p> LIKE LINE OF lt_dd27p.


    ls_dd25v-viewname   = Lcl_abapgit_persistence_db=>c_lock.
    ls_dd25v-aggtype    = 'E'.
    ls_dd25v-roottab    = Lcl_abapgit_persistence_db=>c_tabname.
    ls_dd25v-ddlanguage = Lif_abapgit_definitions=>c_english.
    ls_dd25v-ddtext     = c_text.

    APPEND INITIAL LINE TO lt_dd26e ASSIGNING <ls_dd26e>.
    <ls_dd26e>-viewname   = Lcl_abapgit_persistence_db=>c_lock.
    <ls_dd26e>-tabname    = Lcl_abapgit_persistence_db=>c_tabname.
    <ls_dd26e>-tabpos     = '0001'.
    <ls_dd26e>-fortabname = Lcl_abapgit_persistence_db=>c_tabname.
    <ls_dd26e>-enqmode    = 'E'.

    APPEND INITIAL LINE TO lt_dd27p ASSIGNING <ls_dd27p>.
    <ls_dd27p>-viewname  = Lcl_abapgit_persistence_db=>c_lock.
    <ls_dd27p>-objpos    = '0001'.
    <ls_dd27p>-viewfield = 'TYPE'.
    <ls_dd27p>-tabname   = Lcl_abapgit_persistence_db=>c_tabname.
    <ls_dd27p>-fieldname = 'TYPE'.
    <ls_dd27p>-keyflag   = abap_true.

    APPEND INITIAL LINE TO lt_dd27p ASSIGNING <ls_dd27p>.
    <ls_dd27p>-viewname  = Lcl_abapgit_persistence_db=>c_lock.
    <ls_dd27p>-objpos    = '0002'.
    <ls_dd27p>-viewfield = 'VALUE'.
    <ls_dd27p>-tabname   = Lcl_abapgit_persistence_db=>c_tabname.
    <ls_dd27p>-fieldname = 'VALUE'.
    <ls_dd27p>-keyflag   = abap_true.

    CALL FUNCTION 'DDIF_ENQU_PUT'
      EXPORTING
        name              = Lcl_abapgit_persistence_db=>c_lock
        dd25v_wa          = ls_dd25v
      TABLES
        dd26e_tab         = lt_dd26e
        dd27p_tab         = lt_dd27p
      EXCEPTIONS
        enqu_not_found    = 1
        name_inconsistent = 2
        enqu_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    lv_obj_name = Lcl_abapgit_persistence_db=>c_lock.
    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid    = 'R3TR'
        wi_tadir_object   = 'ENQU'
        wi_tadir_obj_name = lv_obj_name
        wi_set_genflag    = abap_true
        wi_test_modus     = abap_false
        wi_tadir_devclass = '$TMP'
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'DDIF_ENQU_ACTIVATE'
      EXPORTING
        name        = Lcl_abapgit_persistence_db=>c_lock
      EXCEPTIONS
        not_found   = 1
        put_failure = 2
        OTHERS      = 3.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'migrate, error from DDIF_ENQU_ACTIVATE' ).
    ENDIF.

  ENDMETHOD.
  METHOD lock_exists.

    DATA: lv_viewname TYPE dd25l-viewname.

    SELECT SINGLE viewname FROM dd25l INTO lv_viewname
      WHERE viewname = Lcl_abapgit_persistence_db=>c_lock.
    rv_exists = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD run.

    IF table_exists( ) = abap_false.
      table_create( ).
    ENDIF.

    IF lock_exists( ) = abap_false.
      lock_create( ).
    ENDIF.

    gui_status_create( ).

  ENDMETHOD.
  METHOD table_create.

    DATA: lv_rc       LIKE sy-subrc,
          lv_obj_name TYPE tadir-obj_name,
          ls_dd02v    TYPE dd02v,
          ls_dd09l    TYPE dd09l,
          lt_dd03p    TYPE STANDARD TABLE OF dd03p WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_dd03p> LIKE LINE OF lt_dd03p.

    ls_dd02v-tabname    = Lcl_abapgit_persistence_db=>c_tabname.
    ls_dd02v-ddlanguage = Lif_abapgit_definitions=>c_english.
    ls_dd02v-tabclass   = 'TRANSP'.
    ls_dd02v-ddtext     = c_text.
    ls_dd02v-contflag   = 'L'.
    ls_dd02v-exclass    = '1'.

    ls_dd09l-tabname  = Lcl_abapgit_persistence_db=>c_tabname.
    ls_dd09l-as4local = 'A'.
    ls_dd09l-tabkat   = '1'.
    ls_dd09l-tabart   = 'APPL1'.
    ls_dd09l-bufallow = 'N'.

    APPEND INITIAL LINE TO lt_dd03p ASSIGNING <ls_dd03p>.
    <ls_dd03p>-tabname   = Lcl_abapgit_persistence_db=>c_tabname.
    <ls_dd03p>-fieldname = 'TYPE'.
    <ls_dd03p>-position  = '0001'.
    <ls_dd03p>-keyflag   = 'X'.
    <ls_dd03p>-datatype  = 'CHAR'.
    <ls_dd03p>-leng      = '000012'.

    APPEND INITIAL LINE TO lt_dd03p ASSIGNING <ls_dd03p>.
    <ls_dd03p>-tabname   = Lcl_abapgit_persistence_db=>c_tabname.
    <ls_dd03p>-fieldname = 'VALUE'.
    <ls_dd03p>-position  = '0002'.
    <ls_dd03p>-keyflag   = 'X'.
    <ls_dd03p>-datatype  = 'CHAR'.
    <ls_dd03p>-leng      = '000012'.

    APPEND INITIAL LINE TO lt_dd03p ASSIGNING <ls_dd03p>.
    <ls_dd03p>-tabname   = Lcl_abapgit_persistence_db=>c_tabname.
    <ls_dd03p>-fieldname = 'DATA_STR'.
    <ls_dd03p>-position  = '0003'.
    <ls_dd03p>-datatype  = 'STRG'.

    CALL FUNCTION 'DDIF_TABL_PUT'
      EXPORTING
        name              = Lcl_abapgit_persistence_db=>c_tabname
        dd02v_wa          = ls_dd02v
        dd09l_wa          = ls_dd09l
      TABLES
        dd03p_tab         = lt_dd03p
      EXCEPTIONS
        tabl_not_found    = 1
        name_inconsistent = 2
        tabl_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    lv_obj_name = Lcl_abapgit_persistence_db=>c_tabname.
    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid    = 'R3TR'
        wi_tadir_object   = 'TABL'
        wi_tadir_obj_name = lv_obj_name
        wi_set_genflag    = abap_true
        wi_test_modus     = abap_false
        wi_tadir_devclass = '$TMP'
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'DDIF_TABL_ACTIVATE'
      EXPORTING
        name        = Lcl_abapgit_persistence_db=>c_tabname
        auth_chk    = abap_false
      IMPORTING
        rc          = lv_rc
      EXCEPTIONS
        not_found   = 1
        put_failure = 2
        OTHERS      = 3.
    IF sy-subrc <> 0 OR lv_rc <> 0.
      Lcx_abapgit_exception=>raise( 'migrate, error from DDIF_TABL_ACTIVATE' ).
    ENDIF.

  ENDMETHOD.
  METHOD table_exists.

    DATA: lv_tabname TYPE dd02l-tabname.

    SELECT SINGLE tabname FROM dd02l INTO lv_tabname
      WHERE tabname = Lcl_abapgit_persistence_db=>c_tabname. "#EC CI_NOORDER
    rv_exists = boolc( sy-subrc = 0 ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_PERSIST_MIGRATE implementation

*>>>>>>> ZCL_ABAPGIT_POPUPS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_popups============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_popups============ccimp.
CLASS SHRITEFUH64VYIPO5IWUYB3KW5XSFY DEFINITION FINAL.
  PUBLIC SECTION.

    CONSTANTS c_default_column     TYPE abap_componentdescr-name VALUE 'DEFAULT_COLUMN'.
    CONSTANTS c_fieldname_selected TYPE abap_componentdescr-name VALUE 'SELECTED'.
    CONSTANTS c_answer_cancel      TYPE c LENGTH 1 VALUE 'A'.
    CONSTANTS c_fieldname_obj_type TYPE abap_componentdescr-name VALUE 'OBJ_TYPE'.
    CONSTANTS c_own_pfstatus       TYPE sy-pfkey VALUE 'DECIDE_DIALOG'.

    METHODS constructor
      IMPORTING
        !it_list               TYPE STANDARD TABLE
        !iv_title              TYPE lvc_title DEFAULT space
        !iv_header_text        TYPE csequence DEFAULT space
        !is_position           TYPE Lif_abapgit_popups=>ty_popup_position
        !iv_striped_pattern    TYPE abap_bool DEFAULT abap_false
        !iv_optimize_col_width TYPE abap_bool DEFAULT abap_true
        !iv_selection_mode     TYPE salv_de_constant DEFAULT if_salv_c_selection_mode=>multiple
        !iv_select_column_text TYPE csequence DEFAULT space
        !it_columns_to_display TYPE Lif_abapgit_popups=>ty_alv_column_tt
        !it_preselected_rows   TYPE Lif_abapgit_popups=>ty_rows OPTIONAL
      RAISING
        Lcx_abapgit_exception.

    METHODS display
      RAISING
        Lcx_abapgit_exception.
    METHODS get_selected
      EXPORTING
        VALUE(et_list) TYPE STANDARD TABLE.

  PRIVATE SECTION.

    DATA mr_table TYPE REF TO data.
    DATA mo_table_descr TYPE REF TO cl_abap_tabledescr.
    DATA mo_alv TYPE REF TO cl_salv_table.
    DATA mv_cancel TYPE abap_bool.
    DATA ms_position TYPE Lif_abapgit_popups=>ty_popup_position.

    " Events
    METHODS on_select_list_link_click
      FOR EVENT link_click OF cl_salv_events_table
      IMPORTING
        !row
        !column.
    METHODS on_select_list_function_click
      FOR EVENT added_function OF cl_salv_events_table
      IMPORTING
        !e_salv_function.
    METHODS on_double_click
      FOR EVENT double_click OF cl_salv_events_table
      IMPORTING
        !row
        !column.

    " Methods
    METHODS create_new_selectable_table
      IMPORTING
        it_list TYPE STANDARD TABLE.
    METHODS preselect
      IMPORTING
        it_preselected_rows TYPE Lif_abapgit_popups=>ty_rows OPTIONAL
      RAISING
        Lcx_abapgit_exception.
    METHODS create_alv
      RETURNING
        VALUE(ro_alv) TYPE REF TO cl_salv_table
      RAISING
        cx_salv_msg.
    METHODS setup_columns
      IMPORTING
        io_columns            TYPE REF TO cl_salv_columns_table
        iv_selection_mode     TYPE salv_de_constant
        iv_select_column_text TYPE csequence
        it_columns_to_display TYPE Lif_abapgit_popups=>ty_alv_column_tt
      RAISING
        cx_salv_msg.
    METHODS setup_toolbar
      IMPORTING
        !iv_selection_mode TYPE salv_de_constant
        !iv_object_list    TYPE abap_bool.
    METHODS ask_user_for_obj_category
      RETURNING
        VALUE(rv_category) TYPE string.
    METHODS mark_category
      IMPORTING
        iv_category TYPE string.
    METHODS mark_all
      IMPORTING
        iv_selected TYPE abap_bool.
    METHODS mark_visible
      IMPORTING
        iv_selected TYPE abap_bool.
    METHODS mark_selected.
    METHODS mark_indexed
      IMPORTING
        iv_selected TYPE abap_bool DEFAULT abap_true
        iv_invert   TYPE abap_bool DEFAULT abap_false
        it_scope    TYPE lvc_t_fidx.
    METHODS are_all_marked
      IMPORTING
        it_scope      TYPE lvc_t_fidx
      RETURNING
        VALUE(rv_yes) TYPE abap_bool.

ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KW5XSFY IMPLEMENTATION.

  METHOD display.

    mo_alv->display( ).

    IF mv_cancel = abap_true.
      RAISE EXCEPTION TYPE Lcx_abapgit_cancel.
    ENDIF.

  ENDMETHOD.

  METHOD get_selected.

    DATA:
      lv_condition     TYPE string,
      lr_exporting     TYPE REF TO data,
      lo_data_descr    TYPE REF TO cl_abap_datadescr,
      lo_selections    TYPE REF TO cl_salv_selections,
      lt_selected_rows TYPE salv_t_row.

    FIELD-SYMBOLS:
      <lg_exporting>    TYPE any,
      <lt_table>        TYPE STANDARD TABLE,
      <ls_line>         TYPE any,
      <lg_value>        TYPE any,
      <lv_selected>     TYPE abap_bool,
      <lv_selected_row> TYPE LINE OF salv_t_row.

    CLEAR et_list.

    " Make sure we don't accidentally return anything
    IF mv_cancel = abap_true.
      RETURN.
    ENDIF.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    lo_selections = mo_alv->get_selections( ).

    IF lo_selections->get_selection_mode( ) = if_salv_c_selection_mode=>single.

      lt_selected_rows = lo_selections->get_selected_rows( ).

      LOOP AT lt_selected_rows ASSIGNING <lv_selected_row>.

        READ TABLE <lt_table> ASSIGNING <ls_line> INDEX <lv_selected_row>.
        CHECK <ls_line> IS ASSIGNED.

        ASSIGN COMPONENT c_fieldname_selected OF STRUCTURE <ls_line> TO <lv_selected>.
        CHECK <lv_selected> IS ASSIGNED.

        <lv_selected> = abap_true.

      ENDLOOP.

    ENDIF.

    lv_condition = |{ c_fieldname_selected } = ABAP_TRUE|.

    CREATE DATA lr_exporting LIKE LINE OF et_list.
    ASSIGN lr_exporting->* TO <lg_exporting>.

    lo_data_descr = mo_table_descr->get_table_line_type( ).

    LOOP AT <lt_table> ASSIGNING <ls_line> WHERE (lv_condition).
      CLEAR <lg_exporting>.

      CASE lo_data_descr->kind.
        WHEN cl_abap_elemdescr=>kind_elem.
          ASSIGN COMPONENT c_default_column OF STRUCTURE <ls_line> TO <lg_value>.
          ASSERT <lg_value> IS ASSIGNED.
          <lg_exporting> = <lg_value>.

        WHEN OTHERS.
          MOVE-CORRESPONDING <ls_line> TO <lg_exporting>.

      ENDCASE.
      APPEND <lg_exporting> TO et_list.
    ENDLOOP.

  ENDMETHOD.

  METHOD create_alv.

    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    cl_salv_table=>factory(
      IMPORTING
        r_salv_table = ro_alv
      CHANGING
        t_table      = <lt_table> ).

  ENDMETHOD.

  METHOD constructor.

    DATA:
      lv_object_list  TYPE abap_bool,
      lo_events       TYPE REF TO cl_salv_events_table,
      lo_columns      TYPE REF TO cl_salv_columns_table,
      lo_table_header TYPE REF TO cl_salv_form_text.

    create_new_selectable_table( it_list ).
    preselect( it_preselected_rows ).

    TRY.
        mo_alv = create_alv( ).
        mo_alv->set_screen_popup(
          start_column = is_position-start_column
          end_column   = is_position-end_column
          start_line   = is_position-start_row
          end_line     = is_position-end_row ).
        ms_position = is_position.

        lo_events = mo_alv->get_event( ).

        SET HANDLER on_select_list_link_click FOR lo_events.
        SET HANDLER on_select_list_function_click FOR lo_events.
        SET HANDLER on_double_click FOR lo_events.

        IF iv_title CN ' _0'.
          mo_alv->get_display_settings( )->set_list_header( iv_title ).
        ENDIF.

        IF iv_header_text CN ' _0'.
          CREATE OBJECT lo_table_header EXPORTING text = iv_header_text.
          mo_alv->set_top_of_list( lo_table_header ).
        ENDIF.

        mo_alv->get_display_settings( )->set_striped_pattern( iv_striped_pattern ).
        mo_alv->get_selections( )->set_selection_mode( iv_selection_mode ).

        lo_columns = mo_alv->get_columns( ).
        lo_columns->set_optimize( iv_optimize_col_width ).

        TRY.
            lo_columns->get_column( |{ c_fieldname_obj_type }| ).
            lv_object_list = abap_true.
          CATCH cx_salv_not_found.
        ENDTRY.

        setup_columns(
          io_columns            = lo_columns
          iv_selection_mode     = iv_selection_mode
          iv_select_column_text = iv_select_column_text
          it_columns_to_display = it_columns_to_display ).

        setup_toolbar(
          iv_object_list    = lv_object_list
          iv_selection_mode = iv_selection_mode ).

      CATCH cx_salv_msg.
        Lcx_abapgit_exception=>raise( 'ALV error from object decision list' ).
    ENDTRY.


  ENDMETHOD.

  METHOD create_new_selectable_table.

    " create and populate a table on the fly derived from
    " it_data with a select column

    DATA:
      lr_struct        TYPE REF TO data,
      lt_components    TYPE cl_abap_structdescr=>component_table,
      lo_data_descr    TYPE REF TO cl_abap_datadescr,
      lo_elem_descr    TYPE REF TO cl_abap_elemdescr,
      lo_struct_descr  TYPE REF TO cl_abap_structdescr,
      lo_struct_descr2 TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS:
      <lt_table>     TYPE STANDARD TABLE,
      <ls_component> TYPE abap_componentdescr,
      <ls_line>      TYPE data,
      <lg_data>      TYPE any,
      <lg_value>     TYPE any.

    mo_table_descr ?= cl_abap_tabledescr=>describe_by_data( it_list ).
    lo_data_descr = mo_table_descr->get_table_line_type( ).

    CASE lo_data_descr->kind.
      WHEN cl_abap_elemdescr=>kind_elem.
        lo_elem_descr ?= mo_table_descr->get_table_line_type( ).
        INSERT INITIAL LINE INTO lt_components ASSIGNING <ls_component> INDEX 1.
        <ls_component>-name = c_default_column.
        <ls_component>-type = lo_elem_descr.

      WHEN cl_abap_elemdescr=>kind_struct.
        lo_struct_descr ?= mo_table_descr->get_table_line_type( ).
        lt_components = lo_struct_descr->get_components( ).

    ENDCASE.

    IF lt_components IS INITIAL.
      RETURN.
    ENDIF.

    INSERT INITIAL LINE INTO lt_components ASSIGNING <ls_component> INDEX 1.
    <ls_component>-name = c_fieldname_selected.
    <ls_component>-type ?= cl_abap_datadescr=>describe_by_name( 'FLAG' ).

    lo_struct_descr2 = cl_abap_structdescr=>create( lt_components ).
    mo_table_descr = cl_abap_tabledescr=>create( lo_struct_descr2 ).

    CREATE DATA mr_table TYPE HANDLE mo_table_descr.
    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    CREATE DATA lr_struct TYPE HANDLE lo_struct_descr2.
    ASSIGN lr_struct->* TO <ls_line>.
    ASSERT sy-subrc = 0.

    LOOP AT it_list ASSIGNING <lg_data>.
      CLEAR <ls_line>.
      CASE lo_data_descr->kind.
        WHEN cl_abap_elemdescr=>kind_elem.
          ASSIGN COMPONENT c_default_column OF STRUCTURE <lg_data> TO <lg_value>.
          ASSERT <lg_value> IS ASSIGNED.
          <ls_line> = <lg_value>.

        WHEN OTHERS.
          MOVE-CORRESPONDING <lg_data> TO <ls_line>.

      ENDCASE.
      INSERT <ls_line> INTO TABLE <lt_table>.
    ENDLOOP.

  ENDMETHOD.

  METHOD preselect.

    FIELD-SYMBOLS:
      <lt_table>    TYPE STANDARD TABLE,
      <lv_row>      LIKE LINE OF it_preselected_rows,
      <ls_line>     TYPE any,
      <lv_selected> TYPE data.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    LOOP AT it_preselected_rows ASSIGNING <lv_row>.

      READ TABLE <lt_table> INDEX <lv_row> ASSIGNING <ls_line>.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Preselected row { <lv_row> } doesn't exist| ).
      ENDIF.

      ASSIGN COMPONENT c_fieldname_selected OF STRUCTURE <ls_line> TO <lv_selected>.
      ASSERT sy-subrc = 0.
      <lv_selected> = abap_true.

    ENDLOOP.

  ENDMETHOD.

  METHOD on_double_click.

    DATA lo_selections TYPE REF TO cl_salv_selections.

    lo_selections = mo_alv->get_selections( ).

    IF lo_selections->get_selection_mode( ) = if_salv_c_selection_mode=>single.
      mo_alv->close_screen( ).
    ENDIF.

  ENDMETHOD.

  METHOD on_select_list_function_click.

    " Work for functions of SAPMSVIM and OWN
    CASE e_salv_function.
      WHEN 'O.K.' OR 'OK'.
        mv_cancel = abap_false.
        mo_alv->close_screen( ).

      WHEN 'ABR' OR 'CANCEL'.
        " Canceled: clear list to overwrite nothing
        mv_cancel = abap_true.
        mo_alv->close_screen( ).

      WHEN 'SALL' OR 'SEL_ALL'.
        mark_visible( abap_true ).
        mo_alv->refresh( ).

      WHEN 'DSEL' OR 'SEL_DEL'.
        mark_visible( abap_false ).
        mo_alv->refresh( ).

      WHEN 'SEL_KEY'.
        mark_selected( ).
        mo_alv->refresh( ).

      WHEN 'SEL_CAT'.
        mark_category( ask_user_for_obj_category( ) ).
        mo_alv->refresh( ).

      WHEN OTHERS.
        mv_cancel = abap_true.
        mo_alv->close_screen( ).

    ENDCASE.

  ENDMETHOD.

  METHOD mark_all.

    FIELD-SYMBOLS:
      <lt_table>    TYPE STANDARD TABLE,
      <ls_line>     TYPE any,
      <lv_selected> TYPE abap_bool.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    LOOP AT <lt_table> ASSIGNING <ls_line>.

      ASSIGN COMPONENT c_fieldname_selected OF STRUCTURE <ls_line> TO <lv_selected>.
      ASSERT sy-subrc = 0.
      <lv_selected> = iv_selected.

    ENDLOOP.

  ENDMETHOD.

  METHOD are_all_marked.

    DATA lv_index LIKE LINE OF it_scope.

    FIELD-SYMBOLS:
      <lt_table>    TYPE STANDARD TABLE,
      <ls_line>     TYPE any,
      <lv_selected> TYPE abap_bool.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    LOOP AT it_scope INTO lv_index.

      READ TABLE <lt_table> ASSIGNING <ls_line> INDEX lv_index.
      CHECK sy-subrc = 0.

      ASSIGN COMPONENT c_fieldname_selected OF STRUCTURE <ls_line> TO <lv_selected>.
      ASSERT sy-subrc = 0.

      IF <lv_selected> = abap_true.
        rv_yes = abap_true.
      ELSE.
        rv_yes = abap_false.
        RETURN.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD mark_selected.

    DATA lt_clear TYPE salv_t_row.
    DATA lt_scope TYPE lvc_t_fidx.

    lt_scope = mo_alv->get_selections( )->get_selected_rows( ).

    IF lines( lt_scope ) > 0.
      mark_indexed(
        it_scope    = lt_scope
        iv_selected = boolc( are_all_marked( lt_scope ) = abap_false ) ).
      mo_alv->get_selections( )->set_selected_rows( lt_clear ).
    ELSE.
      MESSAGE 'Select rows first to mark them' TYPE 'S'.
    ENDIF.

  ENDMETHOD.

  METHOD mark_visible.

    DATA lt_filters TYPE lvc_t_filt.
    DATA lt_scope   TYPE lvc_t_fidx.

    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    " If nothing selected, select all VISIBLE
    lt_filters = cl_salv_controller_metadata=>get_lvc_filter( mo_alv->get_filters( ) ).
    IF lines( lt_filters ) = 0.
      mark_all( iv_selected ). " No filters - just select all
      RETURN.
    ENDIF.

    CALL FUNCTION 'LVC_FILTER_APPLY'
      EXPORTING
        it_filter              = lt_filters
      IMPORTING
        et_filter_index_inside = lt_scope
      TABLES
        it_data                = <lt_table>.

    mark_indexed(
      it_scope    = lt_scope
      iv_selected = iv_selected ).

  ENDMETHOD.

  METHOD mark_indexed.

    DATA lv_index LIKE LINE OF it_scope.

    FIELD-SYMBOLS:
      <lt_table>    TYPE STANDARD TABLE,
      <ls_line>     TYPE any,
      <lv_selected> TYPE abap_bool.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    LOOP AT it_scope INTO lv_index.

      READ TABLE <lt_table> ASSIGNING <ls_line> INDEX lv_index.
      CHECK sy-subrc = 0.

      ASSIGN COMPONENT c_fieldname_selected OF STRUCTURE <ls_line> TO <lv_selected>.
      ASSERT sy-subrc = 0.

      IF iv_invert = abap_true.
        <lv_selected> = boolc( <lv_selected> = abap_false ).
      ELSE.
        <lv_selected> = iv_selected.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD ask_user_for_obj_category.

    DATA:
      lv_answer    TYPE c LENGTH 1,
      ls_position  TYPE Lif_abapgit_popups=>ty_popup_position,
      ls_selection TYPE spopli,
      lt_selection TYPE TABLE OF spopli.

    ls_selection-varoption = 'Packages'.
    APPEND ls_selection TO lt_selection.
    ls_selection-varoption = 'DDIC objects'.
    APPEND ls_selection TO lt_selection.
    ls_selection-varoption = 'Source code'.
    APPEND ls_selection TO lt_selection.
    ls_selection-varoption = 'Enhancements'.
    APPEND ls_selection TO lt_selection.

    ls_position-start_column = ms_position-start_column + 20.
    ls_position-start_row    = ms_position-start_row + 5.

    CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
      EXPORTING
        titel      = 'Selection'
        textline1  = 'Which objects should be added to the selection?'
        start_col  = ls_position-start_column
        start_row  = ls_position-start_row
        cursorline = 1
      IMPORTING
        answer     = lv_answer
      TABLES
        t_spopli   = lt_selection
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0 OR lv_answer = c_answer_cancel.
      RETURN.
    ENDIF.

    READ TABLE lt_selection INDEX lv_answer INTO ls_selection.
    IF sy-subrc = 0.
      rv_category = ls_selection-varoption.
    ENDIF.

  ENDMETHOD.

  METHOD mark_category.

    FIELD-SYMBOLS:
      <lt_table>    TYPE STANDARD TABLE,
      <lv_obj_type> TYPE tadir-object,
      <ls_line>     TYPE any,
      <lv_selected> TYPE abap_bool.

    IF iv_category IS INITIAL.
      RETURN.
    ENDIF.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    LOOP AT <lt_table> ASSIGNING <ls_line>.

      ASSIGN COMPONENT c_fieldname_obj_type OF STRUCTURE <ls_line> TO <lv_obj_type>.
      ASSERT sy-subrc = 0.

      ASSIGN COMPONENT c_fieldname_selected OF STRUCTURE <ls_line> TO <lv_selected>.
      ASSERT sy-subrc = 0.

      CASE iv_category.
        WHEN 'Packages'.
          IF <lv_obj_type> <> 'DEVC'.
            CONTINUE.
          ENDIF.
        WHEN 'DDIC objects'.
          IF Lcl_abapgit_objects_activation=>is_ddic_type( <lv_obj_type> ) = abap_false.
            CONTINUE.
          ENDIF.
        WHEN 'Source code'.
          IF 'CLAS,FUGR,INTF,PROG,TYPE' NS <lv_obj_type>.
            CONTINUE.
          ENDIF.
        WHEN 'Enhancements'.
          IF 'ENHO,ENHS,ENHC,ENSC' NS <lv_obj_type>.
            CONTINUE.
          ENDIF.
        WHEN OTHERS.
          RETURN. " Unexpected category
      ENDCASE.

      <lv_selected> = abap_true.

    ENDLOOP.

  ENDMETHOD.

  METHOD on_select_list_link_click.

    FIELD-SYMBOLS:
      <lt_table>    TYPE STANDARD TABLE,
      <ls_line>     TYPE any,
      <lv_selected> TYPE abap_bool.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    READ TABLE <lt_table> ASSIGNING <ls_line> INDEX row.
    IF sy-subrc = 0.

      ASSIGN COMPONENT c_fieldname_selected OF STRUCTURE <ls_line> TO <lv_selected>.
      ASSERT sy-subrc = 0.
      <lv_selected> = boolc( <lv_selected> = abap_false ).

    ENDIF.

    mo_alv->refresh( ).

  ENDMETHOD.

  METHOD setup_columns.

    DATA:
      lt_columns TYPE salv_t_column_ref,
      ls_column  TYPE salv_s_column_ref,
      lo_column  TYPE REF TO cl_salv_column_list.

    FIELD-SYMBOLS <ls_column_to_display> TYPE Lif_abapgit_popups=>ty_alv_column.

    lt_columns = io_columns->get( ).

    LOOP AT lt_columns INTO ls_column.

      lo_column ?= ls_column-r_column.

      IF iv_selection_mode    = if_salv_c_selection_mode=>multiple AND
         ls_column-columnname = c_fieldname_selected.
        lo_column->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ).
        lo_column->set_output_length( 20 ).
        lo_column->set_short_text( |{ iv_select_column_text }| ).
        lo_column->set_medium_text( |{ iv_select_column_text }| ).
        lo_column->set_long_text( |{ iv_select_column_text }| ).
        CONTINUE.
      ENDIF.

      READ TABLE it_columns_to_display
        ASSIGNING <ls_column_to_display>
        WITH KEY name = ls_column-columnname.

      CASE sy-subrc.
        WHEN 0.
          IF <ls_column_to_display>-text CN ' _0'.
            lo_column->set_short_text( |{ <ls_column_to_display>-text }| ).
            lo_column->set_medium_text( |{ <ls_column_to_display>-text }| ).
            lo_column->set_long_text( |{ <ls_column_to_display>-text }| ).
          ENDIF.

          IF <ls_column_to_display>-length > 0.
            lo_column->set_output_length( <ls_column_to_display>-length ).
          ENDIF.

          IF <ls_column_to_display>-show_icon = abap_true.
            lo_column->set_icon( abap_true ).
          ENDIF.

          IF <ls_column_to_display>-center = abap_true.
            lo_column->set_alignment( if_salv_c_alignment=>centered ).
          ENDIF.

        WHEN OTHERS.
          lo_column->set_technical( abap_true ). " Hide column

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD setup_toolbar.

    DATA:
      lv_report    TYPE sy-repid,
      lv_pfstatus  TYPE sy-pfkey,
      lo_functions TYPE REF TO cl_salv_functions_list,
      lt_func_list TYPE salv_t_ui_func,
      lv_fn        TYPE string,
      ls_func      LIKE LINE OF lt_func_list.

    CALL FUNCTION 'RS_CUA_STATUS_CHECK'
      EXPORTING
        program          = sy-cprog
        objectname       = c_own_pfstatus
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.

    IF sy-subrc = 0.

      mo_alv->set_screen_status(
        report   = sy-cprog
        pfstatus = c_own_pfstatus ).

    ELSE.

      lv_report  = 'SAPMSVIM'.

      IF iv_selection_mode = if_salv_c_selection_mode=>single.
        lv_pfstatus = '110'.
      ELSE.
        lv_pfstatus = '102'.
      ENDIF.

      mo_alv->set_screen_status(
        report   = lv_report
        pfstatus = lv_pfstatus ).

    ENDIF.

    lo_functions = mo_alv->get_functions( ).

    lt_func_list = lo_functions->get_functions( ).
    LOOP AT lt_func_list INTO ls_func.
      lv_fn = ls_func-r_function->get_name( ).
      IF lv_fn = 'OK' OR lv_fn = 'CANCEL'.
        ls_func-r_function->set_visible( abap_true ).
      ELSEIF iv_object_list = abap_true.
        ls_func-r_function->set_visible( abap_true ).
      ELSE.
        ls_func-r_function->set_visible( abap_false ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

class LCL_ABAPGIT_POPUPS implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_popups~popup_to_select_transport.

    CALL FUNCTION 'TR_F4_REQUESTS'
      IMPORTING
        ev_selected_request = rv_trkorr.

  ENDMETHOD.
  METHOD add_field.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF ct_fields.

    APPEND INITIAL LINE TO ct_fields ASSIGNING <ls_field>.
    <ls_field>-tabname    = iv_tabname.
    <ls_field>-fieldname  = iv_fieldname.
    <ls_field>-fieldtext  = iv_fieldtext.
    <ls_field>-value      = iv_value.
    <ls_field>-field_attr = iv_field_attr.
    <ls_field>-field_obl  = iv_obligatory.

  ENDMETHOD.
  METHOD center.

    CONSTANTS:
      lc_min_size TYPE i VALUE 10,
      lc_min_pos  TYPE i VALUE 5.

    " Magic math to approximate starting position of popup
    IF sy-scols > lc_min_size AND iv_width > 0 AND sy-scols > iv_width.
      rs_position-start_column = nmax(
        val1 = ( sy-scols - iv_width ) / 2
        val2 = lc_min_pos ).
    ELSE.
      rs_position-start_column = lc_min_pos.
    ENDIF.

    IF sy-srows > lc_min_size AND iv_height > 0 AND sy-srows > iv_height.
      rs_position-start_row = nmax(
        val1 = ( sy-srows - iv_height ) / 2 - 1
        val2 = lc_min_pos ).
    ELSE.
      rs_position-start_row = lc_min_pos.
    ENDIF.

    rs_position-end_column = rs_position-start_column + iv_width.
    rs_position-end_row = rs_position-start_row + iv_height.

  ENDMETHOD.
  METHOD commit_list_build.

    DATA:
      lv_unix_time   TYPE Lcl_abapgit_git_time=>ty_unixtime,
      lv_date        TYPE d,
      lv_date_string TYPE c LENGTH 12,
      lv_time        TYPE t,
      lv_time_string TYPE c LENGTH 10.

    FIELD-SYMBOLS:
      <ls_commit>    TYPE Lif_abapgit_git_definitions=>ty_commit,
      <ls_value_tab> TYPE ty_commit_value_tab.

    CLEAR: et_commits, et_value_tab.

    et_commits = Lcl_abapgit_git_commit=>get_by_branch( iv_branch_name  = iv_branch_name
                                                        iv_repo_url     = iv_repo_url
                                                        iv_deepen_level = 99
                                                        iv_sorted       = abap_false )-commits.

    IF et_commits IS INITIAL.
      Lcx_abapgit_exception=>raise( |No commits are available in this branch.| ).
    ENDIF.

    SORT et_commits BY time DESCENDING.

    LOOP AT et_commits ASSIGNING <ls_commit>.

      APPEND INITIAL LINE TO et_value_tab ASSIGNING <ls_value_tab>.
      <ls_value_tab>-commit  = <ls_commit>-sha1.
      <ls_value_tab>-message = <ls_commit>-message.
      lv_unix_time = <ls_commit>-time.
      Lcl_abapgit_git_time=>get_utc(
        EXPORTING
          iv_unix = lv_unix_time
        IMPORTING
          ev_time = lv_time
          ev_date = lv_date ).
      WRITE: lv_date TO lv_date_string,
             lv_time TO lv_time_string.
      <ls_value_tab>-datetime = |{ lv_date_string }, | &&
                                |{ lv_time_string }|.

    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_popups~branch_list_popup.

    DATA: lo_branches    TYPE REF TO Lcl_abapgit_git_branch_list,
          lt_branches    TYPE Lif_abapgit_git_definitions=>ty_git_branch_list_tt,
          lv_answer      TYPE c LENGTH 1,
          lv_default     TYPE i,
          lv_head_suffix TYPE string,
          lv_head_symref TYPE string,
          lv_text        TYPE string,
          lt_selection   TYPE TABLE OF spopli.

    FIELD-SYMBOLS: <ls_sel>    LIKE LINE OF lt_selection,
                   <ls_branch> LIKE LINE OF lt_branches.


    lo_branches    = Lcl_abapgit_git_transport=>branches( iv_url ).
    lt_branches    = lo_branches->get_branches_only( ).
    lv_head_suffix = | ({ Lif_abapgit_git_definitions=>c_head_name })|.
    lv_head_symref = lo_branches->get_head_symref( ).

    IF iv_hide_branch IS NOT INITIAL.
      DELETE lt_branches WHERE name = iv_hide_branch.
    ENDIF.

    IF iv_hide_head IS NOT INITIAL.
      DELETE lt_branches WHERE name    = Lif_abapgit_git_definitions=>c_head_name
                            OR is_head = abap_true.
    ENDIF.

    IF lt_branches IS INITIAL.
      IF iv_hide_head IS NOT INITIAL.
        lv_text = 'main'.
      ENDIF.
      IF iv_hide_branch IS NOT INITIAL AND iv_hide_branch <> Lif_abapgit_git_definitions=>c_git_branch-main.
        IF lv_text IS INITIAL.
          lv_text = iv_hide_branch && ' is'.
        ELSE.
          CONCATENATE lv_text 'and' iv_hide_branch 'are' INTO lv_text SEPARATED BY space.
        ENDIF.
      ELSE.
        lv_text = lv_text && ' is'.
      ENDIF.
      IF lv_text IS NOT INITIAL.
        Lcx_abapgit_exception=>raise( 'No branches available to select (' && lv_text && ' hidden)' ).
      ELSE.
        Lcx_abapgit_exception=>raise( 'No branches are available to select' ).
      ENDIF.
    ENDIF.

    LOOP AT lt_branches ASSIGNING <ls_branch>.

      CHECK <ls_branch>-name IS NOT INITIAL. " To ensure some below ifs

      IF <ls_branch>-is_head = abap_true.

        IF <ls_branch>-name = Lif_abapgit_git_definitions=>c_head_name. " HEAD
          IF <ls_branch>-name <> lv_head_symref AND lv_head_symref IS NOT INITIAL.
            " HEAD but other HEAD symref exists - ignore
            CONTINUE.
          ELSE.
            INSERT INITIAL LINE INTO lt_selection INDEX 1 ASSIGNING <ls_sel>.
            <ls_sel>-varoption = <ls_branch>-name.
          ENDIF.
        ELSE.
          INSERT INITIAL LINE INTO lt_selection INDEX 1 ASSIGNING <ls_sel>.
          <ls_sel>-varoption = <ls_branch>-display_name && lv_head_suffix.
        ENDIF.

        IF lv_default > 0. " Shift down default if set
          lv_default = lv_default + 1.
        ENDIF.
      ELSE.
        APPEND INITIAL LINE TO lt_selection ASSIGNING <ls_sel>.
        <ls_sel>-varoption = <ls_branch>-display_name.
      ENDIF.

      IF <ls_branch>-name = iv_default_branch.
        IF <ls_branch>-is_head = abap_true.
          lv_default = 1.
        ELSE.
          lv_default = sy-tabix.
        ENDIF.
      ENDIF.

    ENDLOOP.

    IF iv_show_new_option = abap_true.
      APPEND INITIAL LINE TO lt_selection ASSIGNING <ls_sel>.
      <ls_sel>-varoption = Lif_abapgit_popups=>c_new_branch_label.
    ENDIF.

    ms_position = center(
      iv_width  = 30
      iv_height = lines( lt_selection ) ).

    CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
      EXPORTING
        titel      = 'Select Branch'
        textline1  = 'Select a branch'
        start_col  = ms_position-start_column
        start_row  = ms_position-start_row
        cursorline = lv_default
      IMPORTING
        answer     = lv_answer
      TABLES
        t_spopli   = lt_selection
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Error from POPUP_TO_DECIDE_LIST' ).
    ENDIF.

    IF lv_answer = c_answer_cancel.
      RETURN.
    ENDIF.

    READ TABLE lt_selection ASSIGNING <ls_sel> WITH KEY selflag = abap_true.
    ASSERT sy-subrc = 0.

    IF iv_show_new_option = abap_true AND <ls_sel>-varoption = Lif_abapgit_popups=>c_new_branch_label.
      rs_branch-name = Lif_abapgit_popups=>c_new_branch_label.
    ELSE.
      REPLACE FIRST OCCURRENCE OF lv_head_suffix IN <ls_sel>-varoption WITH ''.
      READ TABLE lt_branches WITH KEY display_name = <ls_sel>-varoption ASSIGNING <ls_branch>.
      IF sy-subrc <> 0.
* branch name longer than 65 characters
        LOOP AT lt_branches ASSIGNING <ls_branch> WHERE display_name CS <ls_sel>-varoption.
          EXIT. " current loop
        ENDLOOP.
      ENDIF.
      ASSERT <ls_branch> IS ASSIGNED.
      rs_branch = lo_branches->find_by_name( <ls_branch>-name ).
      lv_text = |Branch switched from { Lcl_abapgit_git_branch_list=>get_display_name( iv_default_branch ) } to {
        Lcl_abapgit_git_branch_list=>get_display_name( rs_branch-name ) } |.
      MESSAGE lv_text TYPE 'S'.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_popups~choose_code_insp_check_variant.

    DATA: lt_return TYPE STANDARD TABLE OF ddshretval.

    FIELD-SYMBOLS: <ls_return> LIKE LINE OF lt_return.

    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname           = 'SCI_DYNP'
        fieldname         = 'CHKV'
      TABLES
        return_tab        = lt_return
      EXCEPTIONS
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        no_values_found   = 4
        OTHERS            = 5.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    READ TABLE lt_return ASSIGNING <ls_return>
                         WITH KEY retfield = 'SCI_DYNP-CHKV'.
    IF sy-subrc = 0.
      rv_check_variant = <ls_return>-fieldval.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_popups~commit_list_popup.

    DATA:
      lt_commits         TYPE Lif_abapgit_git_definitions=>ty_commit_tt,
      lt_value_tab       TYPE ty_commit_value_tab_tt,
      lt_selected_values TYPE ty_commit_value_tab_tt,
      lt_columns         TYPE Lif_abapgit_popups=>ty_alv_column_tt.

    FIELD-SYMBOLS:
      <ls_value_tab> TYPE ty_commit_value_tab,
      <ls_column>    TYPE Lif_abapgit_popups=>ty_alv_column.

    commit_list_build(
      EXPORTING
        iv_branch_name = iv_branch_name
        iv_repo_url    = iv_repo_url
      IMPORTING
        et_value_tab   = lt_value_tab
        et_commits     = lt_commits ).

    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name   = 'COMMIT'.
    <ls_column>-text   = 'Hash'.
    <ls_column>-length = 8.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'MESSAGE'.
    <ls_column>-text = 'Message'.
    <ls_column>-length = 60.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'DATETIME'.
    <ls_column>-text = 'Datetime'.
    <ls_column>-length = 17.

    Lif_abapgit_popups~popup_to_select_from_list(
      EXPORTING
        it_list               = lt_value_tab
        iv_title              = |Select a commit|
        iv_end_column         = 100
        iv_striped_pattern    = abap_true
        iv_optimize_col_width = abap_false
        iv_selection_mode     = if_salv_c_selection_mode=>single
        it_columns_to_display = lt_columns
      IMPORTING
        et_list               = lt_selected_values ).

    IF lt_selected_values IS INITIAL.
      RAISE EXCEPTION TYPE Lcx_abapgit_cancel.
    ENDIF.

    READ TABLE lt_selected_values ASSIGNING <ls_value_tab> INDEX 1.
    ASSERT sy-subrc = 0.

    READ TABLE lt_commits INTO rs_commit WITH KEY sha1 = <ls_value_tab>-commit.
    ASSERT sy-subrc = 0.

  ENDMETHOD.
  METHOD Lif_abapgit_popups~create_branch_popup.

    DATA: lt_fields TYPE TABLE OF sval.
    DATA: lv_name   TYPE spo_value.

    CLEAR: ev_name, ev_cancel.

    add_field( EXPORTING iv_tabname   = 'TEXTL'
                         iv_fieldname = 'LINE'
                         iv_fieldtext = 'Name'
                         iv_value     = 'new-branch-name'
               CHANGING  ct_fields    = lt_fields ).

    TRY.

        _popup_3_get_values(
          EXPORTING iv_popup_title = |Create branch from {
            Lcl_abapgit_git_branch_list=>get_display_name( iv_source_branch_name ) }|
          IMPORTING ev_value_1     = lv_name
          CHANGING  ct_fields      = lt_fields ).

        ev_name = Lcl_abapgit_git_branch_list=>complete_heads_branch_name(
              Lcl_abapgit_git_branch_list=>normalize_branch_name( lv_name ) ).

      CATCH Lcx_abapgit_cancel.
        ev_cancel = abap_true.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_popups~popup_folder_logic.

    DATA:
      lt_selection TYPE TABLE OF spopli,
      lv_answer    TYPE c LENGTH 1.

    FIELD-SYMBOLS: <ls_sel> LIKE LINE OF lt_selection.

    APPEND INITIAL LINE TO lt_selection ASSIGNING <ls_sel>.
    <ls_sel>-selflag   = abap_true.
    <ls_sel>-varoption = Lif_abapgit_dot_abapgit=>c_folder_logic-prefix.

    APPEND INITIAL LINE TO lt_selection ASSIGNING <ls_sel>.
    <ls_sel>-varoption = Lif_abapgit_dot_abapgit=>c_folder_logic-full.

    APPEND INITIAL LINE TO lt_selection ASSIGNING <ls_sel>.
    <ls_sel>-varoption = Lif_abapgit_dot_abapgit=>c_folder_logic-mixed.

    CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
      EXPORTING
        titel     = 'Folder logic'
        textline1 = 'Select folder logic'
        start_col = ms_position-start_column
        start_row = ms_position-start_row
      IMPORTING
        answer    = lv_answer
      TABLES
        t_spopli  = lt_selection
      EXCEPTIONS
        OTHERS    = 1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Error from POPUP_TO_DECIDE_LIST' ).
    ENDIF.

    IF lv_answer = c_answer_cancel.
      Lcx_abapgit_cancel=>raise( |Canceled| ).
    ENDIF.

    READ TABLE lt_selection ASSIGNING <ls_sel> WITH KEY selflag = abap_true.
    ASSERT sy-subrc = 0.

    rv_folder_logic = <ls_sel>-varoption.

  ENDMETHOD.
  METHOD Lif_abapgit_popups~popup_search_help.

    DATA lt_ret TYPE TABLE OF ddshretval.
    DATA ls_ret LIKE LINE OF lt_ret.
    DATA lv_tabname TYPE dfies-tabname.
    DATA lv_fieldname TYPE dfies-fieldname.

    SPLIT iv_tab_field AT '-' INTO lv_tabname lv_fieldname.
    lv_tabname = to_upper( lv_tabname ).
    lv_fieldname = to_upper( lv_fieldname ).

    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname    = lv_tabname
        fieldname  = lv_fieldname
      TABLES
        return_tab = lt_ret
      EXCEPTIONS
        OTHERS     = 5.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |F4IF_FIELD_VALUE_REQUEST error [{ iv_tab_field }]| ).
    ENDIF.

    IF lines( lt_ret ) > 0.
      READ TABLE lt_ret WITH KEY fieldname = lv_fieldname INTO ls_ret.
      IF sy-subrc = 0.
        rv_value = ls_ret-fieldval.
      ELSE.
        READ TABLE lt_ret INDEX 1 INTO ls_ret.
        ASSERT sy-subrc = 0.
        rv_value = ls_ret-fieldval.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_popups~popup_select_tr_requests.
    DATA ls_r_trkorr TYPE LINE OF Lif_abapgit_definitions=>ty_trrngtrkor_tt.
    DATA lr_request TYPE REF TO trwbo_request_header.
    DATA lt_request TYPE trwbo_request_headers.

    ms_position = center(
      iv_width  = 120
      iv_height = 10 ).

    CALL FUNCTION 'TRINT_SELECT_REQUESTS'
      EXPORTING
        iv_username_pattern    = iv_username_pattern
        is_selection           = is_selection
        iv_complete_projects   = abap_false
        is_popup               = ms_position
        iv_via_selscreen       = 'X'
        iv_title               = iv_title
      IMPORTING
        et_requests            = lt_request
      EXCEPTIONS
        action_aborted_by_user = 1
        OTHERS                 = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Selection canceled' ).
    ENDIF.

    IF lt_request IS INITIAL.
      Lcx_abapgit_exception=>raise( 'No Request Found' ).
    ENDIF.

    IF lines( lt_request ) > 10000.
      Lcx_abapgit_exception=>raise( 'Too many requests selected (max 10000)' ).
    ENDIF.

    LOOP AT lt_request REFERENCE INTO lr_request.
      ls_r_trkorr-sign = 'I'.
      ls_r_trkorr-option = 'EQ'.
      ls_r_trkorr-low = lr_request->trkorr.
      INSERT ls_r_trkorr INTO TABLE rt_r_trkorr.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_popups~popup_select_wb_tc_tr_and_tsk.
    DATA ls_selection  TYPE trwbo_selection.
    DATA lv_title TYPE trwbo_title.

    ls_selection-trkorrpattern = space.
    ls_selection-connect_req_task_conditions = 'X'.
    ls_selection-reqfunctions = 'KTRXS'.
    ls_selection-reqstatus = 'RNODL'.
    ls_selection-taskstatus = 'RNODL'.
    CONDENSE ls_selection-reqfunctions NO-GAPS.
    ls_selection-taskfunctions = 'QRSX'.
    CONCATENATE sy-sysid '*' INTO ls_selection-trkorrpattern.

    lv_title = 'Select Transports / Tasks'.

    rt_r_trkorr = Lif_abapgit_popups~popup_select_tr_requests(
      is_selection        = ls_selection
      iv_title            = lv_title
      iv_username_pattern = '*' ).
  ENDMETHOD.
  METHOD Lif_abapgit_popups~popup_to_confirm.

    ms_position = center(
      iv_width  = 65
      iv_height = 5 ).

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = iv_titlebar
        text_question         = iv_text_question
        text_button_1         = iv_text_button_1
        icon_button_1         = iv_icon_button_1
        text_button_2         = iv_text_button_2
        icon_button_2         = iv_icon_button_2
        default_button        = iv_default_button
        display_cancel_button = iv_display_cancel_button
        popup_type            = iv_popup_type
        start_column          = ms_position-start_column
        start_row             = ms_position-start_row
      IMPORTING
        answer                = rv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from POPUP_TO_CONFIRM' ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_popups~popup_to_create_package.
    IF Lcl_abapgit_factory=>get_function_module( )->function_exists( 'PB_POPUP_PACKAGE_CREATE' ) = abap_false.
* looks like the function module used does not exist on all
* versions since 702, so show an error
      Lcx_abapgit_exception=>raise( 'Your system does not support automatic creation of packages.' &&
        'Please, create the package manually.' ).
    ENDIF.

    CALL FUNCTION 'PB_POPUP_PACKAGE_CREATE'
      CHANGING
        p_object_data    = es_package_data
      EXCEPTIONS
        action_cancelled = 1.
    ev_create = boolc( sy-subrc = 0 ).
  ENDMETHOD.
  METHOD Lif_abapgit_popups~popup_to_create_transp_branch.
    DATA: lt_fields             TYPE TABLE OF sval,
          lv_transports_as_text TYPE string,
          lv_desc_as_text       TYPE string.
    DATA: lv_branch_name        TYPE spo_value.
    DATA: lv_commit_text        TYPE spo_value.

    CLEAR: rs_transport_branch-branch_name, rs_transport_branch-commit_text.

    lv_transports_as_text = iv_trkorr.
    lv_desc_as_text = Lcl_abapgit_factory=>get_cts_api( )->read_description( iv_trkorr ).

    add_field( EXPORTING iv_tabname   = 'TEXTL'
                         iv_fieldname = 'LINE'
                         iv_fieldtext = 'Branch name'
                         iv_value     = lv_transports_as_text
               CHANGING  ct_fields    = lt_fields ).

    add_field( EXPORTING iv_tabname   = 'ABAPTXT255'
                         iv_fieldname = 'LINE'
                         iv_fieldtext = 'Commit text'
                         iv_value     = lv_desc_as_text
               CHANGING  ct_fields    = lt_fields ).

    _popup_3_get_values( EXPORTING iv_popup_title = 'Transport to new Branch'
                         IMPORTING ev_value_1     = lv_branch_name
                                   ev_value_2     = lv_commit_text
                         CHANGING  ct_fields      = lt_fields ).

    rs_transport_branch-branch_name = lv_branch_name.
    rs_transport_branch-commit_text = lv_commit_text.

  ENDMETHOD.
  METHOD Lif_abapgit_popups~popup_to_select_from_list.

    DATA lo_popup TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KW5XSFY.

    CLEAR et_list.

    ms_position = center(
      iv_width  = iv_end_column - iv_start_column
      iv_height = iv_end_line - iv_start_line ).

    CREATE OBJECT lo_popup
      EXPORTING
        it_list               = it_list
        iv_title              = iv_title
        iv_header_text        = iv_header_text
        is_position           = ms_position
        iv_striped_pattern    = iv_striped_pattern
        iv_optimize_col_width = iv_optimize_col_width
        iv_selection_mode     = iv_selection_mode
        iv_select_column_text = iv_select_column_text
        it_columns_to_display = it_columns_to_display
        it_preselected_rows   = it_preselected_rows.

    lo_popup->display( ).
    lo_popup->get_selected( IMPORTING et_list = et_list ).

  ENDMETHOD.
  METHOD Lif_abapgit_popups~popup_to_select_labels.

    DATA:
      lt_all_labels         TYPE Lif_abapgit_repo_srv=>ty_labels,
      ls_label              LIKE LINE OF lt_all_labels,
      lt_current_labels     TYPE string_table,
      lt_selected_labels    LIKE lt_all_labels,
      lt_columns_to_display TYPE Lif_abapgit_popups=>ty_alv_column_tt,
      lt_preselected_rows   TYPE Lif_abapgit_popups=>ty_rows,
      ls_columns_to_display LIKE LINE OF lt_columns_to_display,
      lv_save_tabix         TYPE i,
      li_popup              TYPE REF TO Lif_abapgit_popups.

    FIELD-SYMBOLS: <lv_label>         TYPE Lif_abapgit_repo_srv=>ty_label,
                   <lv_current_label> TYPE LINE OF string_table.

    lt_current_labels = Lcl_abapgit_repo_labels=>split( iv_labels ).

    lt_all_labels = Lcl_abapgit_repo_srv=>get_instance( )->get_label_list( ).

    " Add labels which are not saved yet
    LOOP AT lt_current_labels ASSIGNING <lv_current_label>.

      READ TABLE lt_all_labels TRANSPORTING NO FIELDS
                               WITH KEY key_label
                               COMPONENTS label = <lv_current_label>.
      IF sy-subrc <> 0.
        ls_label-label = <lv_current_label>.
        INSERT ls_label INTO TABLE lt_all_labels.
      ENDIF.

    ENDLOOP.

    IF lines( lt_all_labels ) = 0.
      Lcx_abapgit_exception=>raise( |No labels maintained yet| ).
    ENDIF.

    SORT lt_all_labels.
    DELETE ADJACENT DUPLICATES FROM lt_all_labels.

    " Preselect current labels
    LOOP AT lt_all_labels ASSIGNING <lv_label>.

      lv_save_tabix = sy-tabix.

      READ TABLE lt_current_labels TRANSPORTING NO FIELDS
                                   WITH KEY table_line = <lv_label>-label.
      IF sy-subrc = 0.
        INSERT lv_save_tabix INTO TABLE lt_preselected_rows.
      ENDIF.

    ENDLOOP.

    ls_columns_to_display-name = 'LABEL'.
    ls_columns_to_display-text = 'Label'.
    INSERT ls_columns_to_display INTO TABLE lt_columns_to_display.

    li_popup = Lcl_abapgit_ui_factory=>get_popups( ).
    li_popup->popup_to_select_from_list(
      EXPORTING
        iv_header_text        = 'Select labels'
        iv_select_column_text = 'Add label'
        it_list               = lt_all_labels
        iv_selection_mode     = if_salv_c_selection_mode=>multiple
        it_columns_to_display = lt_columns_to_display
        it_preselected_rows   = lt_preselected_rows
        iv_start_column       = 15
        iv_end_column         = 55
      IMPORTING
        et_list               = lt_selected_labels ).

    LOOP AT lt_selected_labels ASSIGNING <lv_label>.
      IF rv_labels IS NOT INITIAL.
        rv_labels = rv_labels && ','.
      ENDIF.
      rv_labels = rv_labels && <lv_label>-label.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_popups~popup_transport_request.

    DATA: lt_e071    TYPE STANDARD TABLE OF e071,
          lt_e071k   TYPE STANDARD TABLE OF e071k,
          lv_order   TYPE trkorr,
          ls_e070use TYPE e070use.
    DATA lv_category TYPE e070-korrdev.

    " If default transport is set and its type matches, then use it as default for the popup
    ls_e070use = Lcl_abapgit_default_transport=>get_instance( )->get( ).

    IF ( ls_e070use-trfunction = is_transport_type-request OR ls_e070use-trfunction IS INITIAL )
      AND iv_use_default_transport = abap_true.
      lv_order = ls_e070use-ordernum.
    ENDIF.

    " Differentiate between customizing and WB requests
    IF is_transport_type-request = Lif_abapgit_cts_api=>c_transport_type-cust_request.
      lv_category = Lif_abapgit_cts_api=>c_transport_category-customizing.
    ELSE.
      lv_category = Lif_abapgit_cts_api=>c_transport_category-workbench.
    ENDIF.

    CALL FUNCTION 'TRINT_ORDER_CHOICE'
      EXPORTING
        wi_order_type          = is_transport_type-request
        wi_task_type           = is_transport_type-task
        wi_category            = lv_category
        wi_order               = lv_order
      IMPORTING
        we_order               = rv_transport
      TABLES
        wt_e071                = lt_e071
        wt_e071k               = lt_e071k
      EXCEPTIONS
        no_correction_selected = 1
        display_mode           = 2
        object_append_error    = 3
        recursive_call         = 4
        wrong_order_type       = 5
        OTHERS                 = 6.

    IF sy-subrc = 1.
      RAISE EXCEPTION TYPE Lcx_abapgit_cancel.
    ELSEIF sy-subrc > 1.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_popups~tag_list_popup.

    DATA: lo_branches  TYPE REF TO Lcl_abapgit_git_branch_list,
          lt_tags      TYPE Lif_abapgit_git_definitions=>ty_git_branch_list_tt,
          ls_branch    TYPE Lif_abapgit_git_definitions=>ty_git_branch,
          lv_answer    TYPE c LENGTH 1,
          lv_default   TYPE i,
          lv_tag       TYPE string,
          lt_selection TYPE TABLE OF spopli.

    FIELD-SYMBOLS: <ls_sel> LIKE LINE OF lt_selection,
                   <ls_tag> LIKE LINE OF lt_tags.


    lo_branches = Lcl_abapgit_git_transport=>branches( iv_url ).
    lt_tags     = lo_branches->get_tags_only( ).

    LOOP AT lt_tags ASSIGNING <ls_tag> WHERE name NP '*' && Lif_abapgit_git_definitions=>c_git_branch-peel.

      APPEND INITIAL LINE TO lt_selection ASSIGNING <ls_sel>.
      <ls_sel>-varoption = Lcl_abapgit_git_tag=>remove_tag_prefix( <ls_tag>-name ).

    ENDLOOP.

    IF lt_selection IS INITIAL.
      Lcx_abapgit_exception=>raise( 'No tags are available to select' ).
    ENDIF.

    ms_position = center(
      iv_width  = 30
      iv_height = lines( lt_selection ) ).

    CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
      EXPORTING
        titel      = 'Select Tag'
        textline1  = 'Select a tag'
        start_col  = ms_position-start_column
        start_row  = ms_position-start_row
        cursorline = lv_default
      IMPORTING
        answer     = lv_answer
      TABLES
        t_spopli   = lt_selection
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Error from POPUP_TO_DECIDE_LIST' ).
    ENDIF.

    IF lv_answer = c_answer_cancel.
      RETURN.
    ENDIF.

    READ TABLE lt_selection ASSIGNING <ls_sel> WITH KEY selflag = abap_true.
    ASSERT sy-subrc = 0.

    lv_tag = Lcl_abapgit_git_tag=>add_tag_prefix( <ls_sel>-varoption ).

    READ TABLE lt_tags WITH KEY name_key COMPONENTS name = lv_tag ASSIGNING <ls_tag>.
    IF sy-subrc <> 0.
      " tag name longer than 65 characters
      LOOP AT lt_tags ASSIGNING <ls_tag> WHERE name CS lv_tag.
        EXIT.
      ENDLOOP.
    ENDIF.
    ASSERT <ls_tag> IS ASSIGNED.

    ls_branch = lo_branches->find_by_name( <ls_tag>-name ).
    MOVE-CORRESPONDING ls_branch TO rs_tag.

  ENDMETHOD.
  METHOD _popup_3_get_values.

    DATA lv_answer TYPE c LENGTH 1.
    FIELD-SYMBOLS: <ls_field> TYPE sval.

    ms_position = center(
      iv_width  = 120
      iv_height = lines( ct_fields ) ).

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        no_value_check = iv_no_value_check
        popup_title    = iv_popup_title
        start_column   = ms_position-start_column
        start_row      = ms_position-start_row
      IMPORTING
        returncode     = lv_answer
      TABLES
        fields         = ct_fields
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Error from POPUP_GET_VALUES' ).
    ENDIF.

    IF lv_answer = c_answer_cancel.
      RAISE EXCEPTION TYPE Lcx_abapgit_cancel.
    ENDIF.

    IF ev_value_1 IS SUPPLIED.
      READ TABLE ct_fields INDEX 1 ASSIGNING <ls_field>.
      ASSERT sy-subrc = 0.
      ev_value_1 = <ls_field>-value.
    ENDIF.

    IF ev_value_2 IS SUPPLIED.
      READ TABLE ct_fields INDEX 2 ASSIGNING <ls_field>.
      ASSERT sy-subrc = 0.
      ev_value_2 = <ls_field>-value.
    ENDIF.

    IF ev_value_3 IS SUPPLIED.
      READ TABLE ct_fields INDEX 3 ASSIGNING <ls_field>.
      ASSERT sy-subrc = 0.
      ev_value_3 = <ls_field>-value.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_POPUPS implementation

*>>>>>>> ZCL_ABAPGIT_POPUP_PULL_REQUEST <<<<<<<*

*"* macro definitions
*include zcl_abapgit_popup_pull_requestccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_popup_pull_requestccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_POPUP_PULL_REQUEST implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    mv_repo_url = iv_url.
  ENDMETHOD.
  METHOD create.

    CREATE OBJECT ri_popup TYPE Lcl_abapgit_popup_pull_request
      EXPORTING
        iv_url = iv_url.

  ENDMETHOD.
  METHOD fetch_pull_request_list.

    rt_pulls = Lcl_abapgit_pr_enumerator=>new( mv_repo_url )->get_pulls( ).

    SORT rt_pulls DESCENDING BY number.

    IF lines( rt_pulls ) = 0.
      Lcx_abapgit_exception=>raise( 'No pull requests found' ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_render_item~render.

    FIELD-SYMBOLS <ls_pr> TYPE Lif_abapgit_pr_enum_provider=>ty_pull_request.

    ASSIGN iv_item TO <ls_pr>.
    ASSERT sy-subrc = 0.

    ri_html = Lcl_abapgit_html=>create( |<b>{ <ls_pr>-number }</b> - { <ls_pr>-title } @{ <ls_pr>-user }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_html_popup~create_picklist.

    CREATE OBJECT ro_picklist
      EXPORTING
        iv_title         = 'Choose Pull Request'
        it_list          = fetch_pull_request_list( )
        ii_item_renderer = me.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_POPUP_PULL_REQUEST implementation

*>>>>>>> ZCL_ABAPGIT_REPO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_repo==============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_repo==============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_repo==============ccau.
*"* use this source file for your ABAP unit test classes


class LCL_ABAPGIT_REPO implementation.
*"* method's implementations
*include methods.
  METHOD has_remote_source.
    rv_yes = boolc( lines( mt_remote ) > 0 ).
  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_tadir_objects.

    rt_tadir = Lcl_abapgit_factory=>get_tadir( )->read(
      iv_package            = get_package( )
      iv_ignore_subpackages = get_local_settings( )-ignore_subpackages
      iv_only_local_objects = get_local_settings( )-only_local_objects
      io_dot                = get_dot_abapgit( ) ).

  ENDMETHOD.
  METHOD bind_listener.
    mi_listener = ii_listener.
  ENDMETHOD.
  METHOD check_language.

    DATA:
      lv_main_language  TYPE spras,
      lv_error_message  TYPE string,
      lv_error_longtext TYPE string.

    " for deserialize, assumes find_remote_dot_abapgit has been called before (or language won't be defined)
    lv_main_language = get_dot_abapgit( )->get_main_language( ).

    IF lv_main_language <> sy-langu.

      lv_error_message = |Current login language |
                      && |'{ Lcl_abapgit_convert=>conversion_exit_isola_output( sy-langu ) }'|
                      && | does not match main language |
                      && |'{ Lcl_abapgit_convert=>conversion_exit_isola_output( lv_main_language ) }'.|.

      " Feature open in main language only exists if abapGit tcode is present
      IF Lcl_abapgit_services_abapgit=>get_abapgit_tcode( ) IS INITIAL.
        lv_error_message = lv_error_message && | Please logon in main language and retry.|.
        lv_error_longtext = |For the Advanced menu option 'Open in Main Language' to be available a transaction code| &&
                            | must be assigned to report { sy-cprog }.|.
      ELSE.
        lv_error_message = lv_error_message && | Select 'Advanced' > 'Open in Main Language'|.
      ENDIF.

      Lcx_abapgit_exception=>raise( iv_text     = lv_error_message
                                    iv_longtext = lv_error_longtext ).

    ENDIF.

  ENDMETHOD.
  METHOD check_write_protect.

    IF get_local_settings( )-write_protected = abap_true.
      Lcx_abapgit_exception=>raise( 'Cannot deserialize. Local code is write-protected by repo config' ).
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    ASSERT NOT is_data-key IS INITIAL.

    ms_data = is_data.
    mv_request_remote_refresh = abap_true.

  ENDMETHOD.
  METHOD create_new_log.

    CREATE OBJECT mi_log TYPE Lcl_abapgit_log.
    mi_log->set_title( iv_title ).

    ri_log = mi_log.

  ENDMETHOD.
  METHOD delete_checks.

    DATA: li_package TYPE REF TO Lif_abapgit_sap_package.

    check_write_protect( ).
    check_language( ).

    li_package = Lcl_abapgit_factory=>get_sap_package( get_package( ) ).
    rs_checks-transport-required = li_package->are_changes_recorded_in_tr_req( ).

  ENDMETHOD.
  METHOD find_remote_dot_abapgit.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF mt_remote.

    get_files_remote( ).

    READ TABLE mt_remote ASSIGNING <ls_remote>
      WITH KEY file_path
      COMPONENTS path     = Lif_abapgit_definitions=>c_root_dir
                 filename = Lif_abapgit_definitions=>c_dot_abapgit.
    IF sy-subrc = 0.
      ro_dot = Lcl_abapgit_dot_abapgit=>deserialize( <ls_remote>-data ).
      set_dot_abapgit( ro_dot ).
      COMMIT WORK AND WAIT. " to release lock
    ENDIF.

  ENDMETHOD.
  METHOD find_remote_dot_apack.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF mt_remote.

    get_files_remote( ).

    READ TABLE mt_remote ASSIGNING <ls_remote>
      WITH KEY file_path
      COMPONENTS path     = Lif_abapgit_definitions=>c_root_dir
                 filename = Lif_abapgit_apack_definitions=>c_dot_apack_manifest.
    IF sy-subrc = 0.
      ro_dot = Lcl_abapgit_apack_reader=>deserialize( iv_package_name = ms_data-package
                                                      iv_xstr         = <ls_remote>-data ).
      set_dot_apack( ro_dot ).
    ENDIF.

  ENDMETHOD.
  METHOD get_data_config.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF mt_remote.

    IF mi_data_config IS BOUND.
      ri_config = mi_data_config.
      RETURN.
    ENDIF.

    CREATE OBJECT ri_config TYPE Lcl_abapgit_data_config.
    mi_data_config = ri_config.

    " Assume remote data has been loaded already
    READ TABLE mt_remote ASSIGNING <ls_remote>
      WITH KEY file_path
      COMPONENTS path = Lif_abapgit_data_config=>c_default_path.
    IF sy-subrc = 0.
      ri_config->from_json( mt_remote ).
    ENDIF.

  ENDMETHOD.
  METHOD get_dot_apack.
    IF mo_apack_reader IS NOT BOUND.
      mo_apack_reader = Lcl_abapgit_apack_reader=>create_instance( ms_data-package ).
    ENDIF.

    ro_dot_apack = mo_apack_reader.

  ENDMETHOD.
  METHOD get_log.
    ri_log = mi_log.
  ENDMETHOD.
  METHOD get_unsupported_objects_local.

    DATA: lt_tadir           TYPE Lif_abapgit_definitions=>ty_tadir_tt,
          lt_supported_types TYPE Lcl_abapgit_objects=>ty_types_tt.

    FIELD-SYMBOLS: <ls_tadir>  LIKE LINE OF lt_tadir,
                   <ls_object> LIKE LINE OF rt_objects.

    lt_tadir = get_tadir_objects( ).

    lt_supported_types = Lcl_abapgit_objects=>supported_list( ).
    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      READ TABLE lt_supported_types WITH KEY table_line = <ls_tadir>-object TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO rt_objects ASSIGNING <ls_object>.
        MOVE-CORRESPONDING <ls_tadir> TO <ls_object>.
        <ls_object>-obj_type = <ls_tadir>-object.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD normalize_local_settings.

    cs_local_settings-labels = Lcl_abapgit_repo_labels=>normalize( cs_local_settings-labels ).

    " TODO: more validation and normalization ?

  ENDMETHOD.
  METHOD notify_listener.

    DATA ls_meta_slug TYPE Lif_abapgit_persistence=>ty_repo_xml.

    IF mi_listener IS BOUND.
      MOVE-CORRESPONDING ms_data TO ls_meta_slug.
      mi_listener->on_meta_change(
        iv_key         = ms_data-key
        is_meta        = ls_meta_slug
        is_change_mask = is_change_mask ).
    ENDIF.

  ENDMETHOD.
  METHOD refresh_local_object.

    DATA:
      ls_tadir           TYPE Lif_abapgit_definitions=>ty_tadir,
      lt_tadir           TYPE Lif_abapgit_definitions=>ty_tadir_tt,
      lt_new_local_files TYPE Lif_abapgit_definitions=>ty_files_item_tt,
      lo_serialize       TYPE REF TO Lcl_abapgit_serialize.

    lt_tadir = get_tadir_objects( ).

    DELETE mt_local WHERE item-obj_type = iv_obj_type
                      AND item-obj_name = iv_obj_name.

    READ TABLE lt_tadir INTO ls_tadir
                        WITH KEY object   = iv_obj_type
                                 obj_name = iv_obj_name.
    IF sy-subrc <> 0 OR ls_tadir-delflag = abap_true.
      " object doesn't exist anymore, nothing todo here
      RETURN.
    ENDIF.

    CLEAR lt_tadir.
    INSERT ls_tadir INTO TABLE lt_tadir.

    CREATE OBJECT lo_serialize.
    lt_new_local_files = lo_serialize->serialize(
      iv_package = ms_data-package
      it_tadir   = lt_tadir ).

    INSERT LINES OF lt_new_local_files INTO TABLE mt_local.

  ENDMETHOD.
  METHOD refresh_local_objects.

    mv_request_local_refresh = abap_true.
    get_files_local( ).

  ENDMETHOD.
  METHOD remove_ignored_files.

    DATA lo_dot TYPE REF TO Lcl_abapgit_dot_abapgit.
    DATA lv_index TYPE sy-index.

    FIELD-SYMBOLS <ls_files> LIKE LINE OF ct_files.

    lo_dot = get_dot_abapgit( ).

    " Skip ignored files
    LOOP AT ct_files ASSIGNING <ls_files>.
      lv_index = sy-tabix.
      IF lo_dot->is_ignored( iv_path     = <ls_files>-path
                             iv_filename = <ls_files>-filename ) = abap_true.
        DELETE ct_files INDEX lv_index.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD reset_remote.
    CLEAR mt_remote.
    mv_request_remote_refresh = abap_true.
  ENDMETHOD.
  METHOD set.

* TODO: refactor, maybe use zcl_abapgit_string_map ?

    DATA: ls_mask TYPE Lif_abapgit_persistence=>ty_repo_meta_mask.


    ASSERT iv_url IS SUPPLIED
      OR iv_branch_name IS SUPPLIED
      OR iv_selected_commit IS SUPPLIED
      OR iv_head_branch IS SUPPLIED
      OR iv_offline IS SUPPLIED
      OR is_dot_abapgit IS SUPPLIED
      OR is_local_settings IS SUPPLIED
      OR iv_deserialized_by IS SUPPLIED
      OR iv_deserialized_at IS SUPPLIED
      OR iv_switched_origin IS SUPPLIED.


    IF iv_url IS SUPPLIED.
      ms_data-url = iv_url.
      ls_mask-url = abap_true.
    ENDIF.

    IF iv_branch_name IS SUPPLIED.
      ms_data-branch_name = iv_branch_name.
      ls_mask-branch_name = abap_true.
    ENDIF.

    IF iv_selected_commit IS SUPPLIED.
      ms_data-selected_commit = iv_selected_commit.
      ls_mask-selected_commit = abap_true.
    ENDIF.

    IF iv_head_branch IS SUPPLIED.
      ms_data-head_branch = iv_head_branch.
      ls_mask-head_branch = abap_true.
    ENDIF.

    IF iv_offline IS SUPPLIED.
      ms_data-offline = iv_offline.
      ls_mask-offline = abap_true.
    ENDIF.

    IF is_dot_abapgit IS SUPPLIED.
      ms_data-dot_abapgit = is_dot_abapgit.
      ls_mask-dot_abapgit = abap_true.
    ENDIF.

    IF is_local_settings IS SUPPLIED.
      ms_data-local_settings = is_local_settings.
      ls_mask-local_settings = abap_true.
      normalize_local_settings( CHANGING cs_local_settings = ms_data-local_settings ).
    ENDIF.

    IF iv_deserialized_at IS SUPPLIED OR iv_deserialized_by IS SUPPLIED.
      ms_data-deserialized_at = iv_deserialized_at.
      ms_data-deserialized_by = iv_deserialized_by.
      ls_mask-deserialized_at = abap_true.
      ls_mask-deserialized_by = abap_true.
    ENDIF.

    IF iv_switched_origin IS SUPPLIED.
      ms_data-switched_origin = iv_switched_origin.
      ls_mask-switched_origin = abap_true.
    ENDIF.

    notify_listener( ls_mask ).

  ENDMETHOD.
  METHOD set_dot_apack.
    get_dot_apack( ).
    mo_apack_reader->set_manifest_descriptor( io_dot_apack->get_manifest_descriptor( ) ).
  ENDMETHOD.
  METHOD set_files_remote.

    mt_remote = it_files.
    mv_request_remote_refresh = abap_false.

  ENDMETHOD.
  METHOD set_local_settings.

    set( is_local_settings = is_settings ).

  ENDMETHOD.
  METHOD switch_repo_type.

    IF iv_offline = ms_data-offline.
      Lcx_abapgit_exception=>raise( |Cannot switch_repo_type, offline already = "{ ms_data-offline }"| ).
    ENDIF.

    IF iv_offline = abap_true. " On-line -> OFFline
      set( iv_url             = Lcl_abapgit_url=>name( ms_data-url )
           iv_branch_name     = ''
           iv_selected_commit = ''
           iv_head_branch     = ''
           iv_offline         = abap_true ).
    ELSE. " OFFline -> On-line
      set( iv_offline = abap_false ).
    ENDIF.

  ENDMETHOD.
  METHOD update_last_deserialize.

    DATA: lv_deserialized_at TYPE Lif_abapgit_persistence=>ty_repo-deserialized_at,
          lv_deserialized_by TYPE Lif_abapgit_persistence=>ty_repo-deserialized_by.

    GET TIME STAMP FIELD lv_deserialized_at.
    lv_deserialized_by = sy-uname.

    set( iv_deserialized_at = lv_deserialized_at
         iv_deserialized_by = lv_deserialized_by ).

  ENDMETHOD.
  METHOD Lif_abapgit_repo~checksums.

    CREATE OBJECT ri_checksums TYPE Lcl_abapgit_repo_checksums
      EXPORTING
        iv_repo_key = ms_data-key.

  ENDMETHOD.
  METHOD Lif_abapgit_repo~deserialize.

    DATA lt_updated_files TYPE Lif_abapgit_git_definitions=>ty_file_signatures_tt.

    find_remote_dot_abapgit( ).
    find_remote_dot_apack( ).

    check_write_protect( ).
    check_language( ).

    IF is_checks-requirements-met = Lif_abapgit_definitions=>c_no AND is_checks-requirements-decision IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Requirements not met and undecided' ).
    ENDIF.

    IF is_checks-dependencies-met = Lif_abapgit_definitions=>c_no AND is_checks-dependencies-decision IS INITIAL.
      Lcx_abapgit_exception=>raise( 'APACK dependencies not met and undecided' ).
    ENDIF.

    IF is_checks-transport-required = abap_true AND is_checks-transport-transport IS INITIAL.
      Lcx_abapgit_exception=>raise( |No transport request was supplied| ).
    ENDIF.

    deserialize_dot_abapgit( CHANGING ct_files = lt_updated_files ).

    deserialize_objects(
      EXPORTING
        is_checks = is_checks
        ii_log    = ii_log
      CHANGING
        ct_files  = lt_updated_files ).

    deserialize_data(
      EXPORTING
        is_checks = is_checks
      CHANGING
        ct_files  = lt_updated_files ).

    CLEAR mt_local. " Should be before CS update which uses NEW local

    Lif_abapgit_repo~checksums( )->update( lt_updated_files ).

    update_last_deserialize( ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.
  METHOD Lif_abapgit_repo~deserialize_checks.

    DATA: lt_requirements TYPE Lif_abapgit_dot_abapgit=>ty_requirement_tt,
          lt_dependencies TYPE Lif_abapgit_apack_definitions=>ty_dependencies.

    find_remote_dot_abapgit( ).
    find_remote_dot_apack( ).

    check_write_protect( ).
    check_language( ).
    check_abap_language_version( ).

    rs_checks = Lcl_abapgit_objects=>deserialize_checks( me ).

    lt_requirements = get_dot_abapgit( )->get_data( )-requirements.
    rs_checks-requirements-met = Lcl_abapgit_requirement_helper=>is_requirements_met( lt_requirements ).

    lt_dependencies = get_dot_apack( )->get_manifest_descriptor( )-dependencies.
    rs_checks-dependencies-met = Lcl_abapgit_apack_helper=>are_dependencies_met( lt_dependencies ).

    rs_checks-customizing = Lcl_abapgit_data_factory=>get_deserializer( )->deserialize_check(
      io_repo   = me
      ii_config = get_data_config( ) ).

  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_dot_abapgit.
    CREATE OBJECT ro_dot_abapgit
      EXPORTING
        is_data = ms_data-dot_abapgit.
  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_files_local.

    DATA lo_serialize TYPE REF TO Lcl_abapgit_serialize.

    " Serialization happened before and no refresh request
    IF lines( mt_local ) > 0 AND mv_request_local_refresh = abap_false.
      rt_files = mt_local.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_serialize
      EXPORTING
        io_dot_abapgit    = get_dot_abapgit( )
        is_local_settings = get_local_settings( ).

    rt_files = lo_serialize->files_local(
      iv_package     = get_package( )
      ii_data_config = get_data_config( )
      ii_log         = ii_log ).

    mt_local                 = rt_files.
    mv_request_local_refresh = abap_false. " Fulfill refresh

  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_files_remote.
    DATA lt_filter TYPE Lif_abapgit_definitions=>ty_tadir_tt.
    DATA lr_filter TYPE REF TO Lcl_abapgit_repo_filter.

    rt_files = mt_remote.
    IF ii_obj_filter IS NOT INITIAL.
      lt_filter = ii_obj_filter->get_filter( ).

      CREATE OBJECT lr_filter.
      lr_filter->apply_object_filter(
        EXPORTING
          it_filter   = lt_filter
          io_dot      = get_dot_abapgit( )
          iv_devclass = get_package( )
        CHANGING
          ct_files    = rt_files ).

    ENDIF.

    IF iv_ignore_files = abap_true.
      remove_ignored_files( CHANGING ct_files = rt_files ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_key.
    rv_key = ms_data-key.
  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_local_settings.

    rs_settings = ms_data-local_settings.

  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_name.

    " Local display name has priority over official name
    rv_name = ms_data-local_settings-display_name.
    IF rv_name IS INITIAL.
      rv_name = ms_data-dot_abapgit-name.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_package.
    rv_package = ms_data-package.
  ENDMETHOD.
  METHOD Lif_abapgit_repo~is_offline.
    rv_offline = ms_data-offline.
  ENDMETHOD.
  METHOD Lif_abapgit_repo~refresh.

    mv_request_local_refresh = abap_true.
    reset_remote( ).

    IF iv_drop_log = abap_true.
      CLEAR mi_log.
    ENDIF.

    IF iv_drop_cache = abap_true.
      CLEAR mt_local.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_repo~set_dot_abapgit.
    set( is_dot_abapgit = io_dot_abapgit->get_data( ) ).
  ENDMETHOD.
  METHOD deserialize_data.

    DATA:
      lt_updated_files TYPE Lif_abapgit_git_definitions=>ty_file_signatures_tt,
      lt_result        TYPE Lif_abapgit_data_deserializer=>ty_results.

    "Deserialize data
    lt_result = Lcl_abapgit_data_factory=>get_deserializer( )->deserialize(
      ii_config  = get_data_config( )
      it_files   = get_files_remote( ) ).

    "Save deserialized data to DB and add entries to transport requests
    lt_updated_files = Lcl_abapgit_data_factory=>get_deserializer( )->actualize(
      it_result = lt_result
      is_checks = is_checks ).

    INSERT LINES OF lt_updated_files INTO TABLE ct_files.

  ENDMETHOD.
  METHOD deserialize_dot_abapgit.
    INSERT get_dot_abapgit( )->get_signature( ) INTO TABLE ct_files.
  ENDMETHOD.
  METHOD deserialize_objects.

    DATA:
      lt_updated_files TYPE Lif_abapgit_git_definitions=>ty_file_signatures_tt,
      lx_error         TYPE REF TO Lcx_abapgit_exception.

    TRY.
        lt_updated_files = Lcl_abapgit_objects=>deserialize(
          io_repo   = me
          is_checks = is_checks
          ii_log    = ii_log ).
      CATCH Lcx_abapgit_exception INTO lx_error.
        " Ensure to reset default transport request task
        Lcl_abapgit_default_transport=>get_instance( )->reset( ).
        refresh( iv_drop_log = abap_false ).
        RAISE EXCEPTION lx_error.
    ENDTRY.

    INSERT LINES OF lt_updated_files INTO TABLE ct_files.

  ENDMETHOD.
  METHOD check_abap_language_version.

    DATA lo_abapgit_abap_language_vers TYPE REF TO Lcl_abapgit_abap_language_vers.
    DATA lv_text TYPE string.

    CREATE OBJECT lo_abapgit_abap_language_vers
      EXPORTING
        io_dot_abapgit = get_dot_abapgit( ).

    IF lo_abapgit_abap_language_vers->is_import_allowed( ms_data-package ) = abap_false.
      lv_text = |Repository cannot be imported. | &&
                |ABAP Language Version of linked package is not compatible with repository settings.|.
      Lcx_abapgit_exception=>raise( lv_text ).
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_files_local_filtered.

    DATA lo_serialize TYPE REF TO Lcl_abapgit_serialize.
    DATA lt_filter TYPE Lif_abapgit_definitions=>ty_tadir_tt.


    CREATE OBJECT lo_serialize
      EXPORTING
        io_dot_abapgit    = get_dot_abapgit( )
        is_local_settings = get_local_settings( ).

    lt_filter = ii_obj_filter->get_filter( ).

    rt_files = lo_serialize->files_local(
      iv_package     = get_package( )
      ii_data_config = get_data_config( )
      ii_log         = ii_log
      it_filter      = lt_filter ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_REPO implementation

*>>>>>>> ZCL_ABAPGIT_REPO_CHECKSUMS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_repo_checksums====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_repo_checksums====ccimp.
CLASS SHRITEFUH64VYIPO5IWUYB3KW54SFY DEFINITION
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_splitter TYPE string VALUE `|`.
    CONSTANTS c_root TYPE string VALUE `@`.

    CLASS-METHODS serialize
      IMPORTING
        it_checksums     TYPE Lif_abapgit_persistence=>ty_local_checksum_tt
      RETURNING
        VALUE(rv_string) TYPE string.

    CLASS-METHODS deserialize
      IMPORTING
        iv_string           TYPE string
      RETURNING
        VALUE(rt_checksums) TYPE Lif_abapgit_persistence=>ty_local_checksum_tt.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS SHRITEFUH64VYIPO5IWUYB3KW54SFY IMPLEMENTATION.


  METHOD deserialize.

    DATA lt_buf_tab TYPE string_table.
    DATA lv_buf TYPE string.
    DATA lt_checksums LIKE rt_checksums.

    FIELD-SYMBOLS <ls_cs> LIKE LINE OF lt_checksums.
    FIELD-SYMBOLS <ls_file> LIKE LINE OF <ls_cs>-files.

    SPLIT iv_string AT |\n| INTO TABLE lt_buf_tab.

    LOOP AT lt_buf_tab INTO lv_buf.
      CHECK lv_buf IS NOT INITIAL. " In fact this is a bug ... it cannot be empty, maybe raise

      IF lv_buf+0(1) = '/'.
        IF <ls_cs> IS NOT ASSIGNED.
          " Incorrect checksums structure, maybe raise, though it is not critical for execution
          RETURN.
        ENDIF.

        APPEND INITIAL LINE TO <ls_cs>-files ASSIGNING <ls_file>.
        SPLIT lv_buf AT c_splitter INTO <ls_file>-path <ls_file>-filename <ls_file>-sha1.

        IF <ls_file>-path IS INITIAL OR <ls_file>-filename IS INITIAL OR <ls_file>-sha1 IS INITIAL.
          " Incorrect checksums struture, maybe raise, though it is not critical for execution
          RETURN.
        ENDIF.
      ELSEIF lv_buf = c_root. " Root
        APPEND INITIAL LINE TO lt_checksums ASSIGNING <ls_cs>. " Empty item
      ELSE.
        APPEND INITIAL LINE TO lt_checksums ASSIGNING <ls_cs>.
        SPLIT lv_buf AT c_splitter INTO <ls_cs>-item-obj_type <ls_cs>-item-obj_name <ls_cs>-item-devclass.

        IF <ls_cs>-item-obj_type IS INITIAL OR <ls_cs>-item-obj_name IS INITIAL OR <ls_cs>-item-devclass IS INITIAL.
          " Incorrect checksums structure, maybe raise, though it is not critical for execution
          RETURN.
        ENDIF.

      ENDIF.
    ENDLOOP.

    rt_checksums = lt_checksums.

  ENDMETHOD.


  METHOD serialize.

    DATA lt_buf_tab TYPE string_table.
    DATA lv_buf TYPE string.
    DATA lt_checksums_sorted TYPE Lif_abapgit_persistence=>ty_local_checksum_by_item_tt.

    FIELD-SYMBOLS <ls_cs> LIKE LINE OF it_checksums.
    FIELD-SYMBOLS <ls_file> LIKE LINE OF <ls_cs>-files.

    lt_checksums_sorted = it_checksums.

    LOOP AT lt_checksums_sorted ASSIGNING <ls_cs>.

      IF lines( <ls_cs>-files ) = 0.
        CONTINUE.
      ENDIF.

      IF <ls_cs>-item-obj_type IS NOT INITIAL.
        CONCATENATE <ls_cs>-item-obj_type <ls_cs>-item-obj_name <ls_cs>-item-devclass
          INTO lv_buf
          SEPARATED BY c_splitter.
      ELSE.
        lv_buf = c_root.
      ENDIF.
      APPEND lv_buf TO lt_buf_tab.

      LOOP AT <ls_cs>-files ASSIGNING <ls_file>.

        CONCATENATE <ls_file>-path <ls_file>-filename <ls_file>-sha1
          INTO lv_buf
          SEPARATED BY c_splitter.
        APPEND lv_buf TO lt_buf_tab.

      ENDLOOP.

    ENDLOOP.

    rv_string = concat_lines_of(
      table = lt_buf_tab
      sep   = |\n| ).

  ENDMETHOD.
ENDCLASS.

**********************************************************************
* UPDATE CALCULATOR
**********************************************************************

CLASS SHRITEFUH64VYIPO5IWUYB3KW56SFY DEFINITION
  FINAL
  CREATE PUBLIC.
  PUBLIC SECTION.

    CLASS-METHODS calculate_updated
      IMPORTING
        it_updated_files TYPE Lif_abapgit_git_definitions=>ty_file_signatures_tt
        it_current_checksums TYPE Lif_abapgit_persistence=>ty_local_checksum_tt
        it_local_files TYPE Lif_abapgit_definitions=>ty_files_item_tt
      RETURNING
        VALUE(rt_checksums) TYPE Lif_abapgit_persistence=>ty_local_checksum_tt.

  PRIVATE SECTION.

    CLASS-METHODS process_updated_files
      CHANGING
        ct_update_index TYPE Lif_abapgit_git_definitions=>ty_file_signatures_ts
        ct_checksums    TYPE Lif_abapgit_persistence=>ty_local_checksum_by_item_tt.

    CLASS-METHODS add_new_files
      IMPORTING
        it_local        TYPE Lif_abapgit_definitions=>ty_files_item_tt
        it_update_index TYPE Lif_abapgit_git_definitions=>ty_file_signatures_ts
      CHANGING
        ct_checksums    TYPE Lif_abapgit_persistence=>ty_local_checksum_by_item_tt.

ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KW56SFY IMPLEMENTATION.

  METHOD calculate_updated.

    DATA lt_update_index TYPE Lif_abapgit_git_definitions=>ty_file_signatures_ts.
    DATA lt_checksums_sorted TYPE Lif_abapgit_persistence=>ty_local_checksum_by_item_tt.

    lt_checksums_sorted = it_current_checksums.
    lt_update_index     = it_updated_files.

    process_updated_files(
      CHANGING
        ct_update_index = lt_update_index
        ct_checksums    = lt_checksums_sorted ).

    add_new_files(
      EXPORTING
        it_update_index = lt_update_index
        it_local        = it_local_files
      CHANGING
        ct_checksums    = lt_checksums_sorted ).

    rt_checksums = lt_checksums_sorted.

  ENDMETHOD.

  METHOD process_updated_files.

    DATA lv_cs_row  TYPE i.
    DATA lv_file_row  TYPE i.

    FIELD-SYMBOLS <ls_checksum>  LIKE LINE OF ct_checksums.
    FIELD-SYMBOLS <ls_file>      LIKE LINE OF <ls_checksum>-files.
    FIELD-SYMBOLS <ls_new_state> LIKE LINE OF ct_update_index.

    " Loop through current checksum state, update sha1 for common files

    LOOP AT ct_checksums ASSIGNING <ls_checksum>.
      lv_cs_row = sy-tabix.

      LOOP AT <ls_checksum>-files ASSIGNING <ls_file>.
        lv_file_row = sy-tabix.

        READ TABLE ct_update_index ASSIGNING <ls_new_state>
          WITH KEY
            path     = <ls_file>-path
            filename = <ls_file>-filename.
        IF sy-subrc <> 0.
          CONTINUE. " Missing in updated files -> nothing to update, skip
        ENDIF.

        IF <ls_new_state>-sha1 IS INITIAL. " Empty input sha1 is a deletion marker
          DELETE <ls_checksum>-files INDEX lv_file_row.
        ELSE.
          <ls_file>-sha1 = <ls_new_state>-sha1.  " Update sha1
          CLEAR <ls_new_state>-sha1.             " Mark as processed
        ENDIF.
      ENDLOOP.

      IF lines( <ls_checksum>-files ) = 0. " Remove empty objects
        DELETE ct_checksums INDEX lv_cs_row.
      ENDIF.
    ENDLOOP.

    DELETE ct_update_index WHERE sha1 IS INITIAL. " Remove processed

  ENDMETHOD.

  METHOD add_new_files.

    DATA lt_local_sorted TYPE Lif_abapgit_definitions=>ty_files_item_by_file_tt.
    DATA ls_checksum LIKE LINE OF ct_checksums.
    FIELD-SYMBOLS <ls_checksum> LIKE LINE OF ct_checksums.
    FIELD-SYMBOLS <ls_new_file> LIKE LINE OF it_update_index.
    FIELD-SYMBOLS <ls_local>    LIKE LINE OF lt_local_sorted.

    lt_local_sorted = it_local.

    " Add new files - not deleted and not marked as processed
    LOOP AT it_update_index ASSIGNING <ls_new_file>.

      READ TABLE lt_local_sorted ASSIGNING <ls_local>
        WITH KEY
          file-path     = <ls_new_file>-path
          file-filename = <ls_new_file>-filename.
      IF sy-subrc <> 0.
        " The file should be in locals, however:
        " if the deserialization fails, the local file might not be there
        " in this case no new CS added, and the file will appear to be remote+new
        CONTINUE.
      ENDIF.

      READ TABLE ct_checksums ASSIGNING <ls_checksum>
        WITH KEY
          item-obj_type = <ls_local>-item-obj_type
          item-obj_name = <ls_local>-item-obj_name.
      IF sy-subrc <> 0.
        MOVE-CORRESPONDING <ls_local>-item TO ls_checksum-item.
        INSERT ls_checksum INTO TABLE ct_checksums ASSIGNING <ls_checksum>.
      ENDIF.

      APPEND <ls_new_file> TO <ls_checksum>-files.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

*"* test class
*include zcl_abapgit_repo_checksums====ccau.
**********************************************************************
* SERIALIZER
**********************************************************************



**********************************************************************
* CHECKSUMS
**********************************************************************


**********************************************************************
* HELPERS
**********************************************************************

CLASS SHRITEFUH64VYIPO5IWUYB3KW6DSFY DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_repo.
    INTERFACES Lif_abapgit_repo_srv.
    DATA mt_local_files TYPE Lif_abapgit_definitions=>ty_files_item_tt.
    DATA mt_remote_files TYPE Lif_abapgit_git_definitions=>ty_files_tt.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KW6DSFY IMPLEMENTATION.

  METHOD Lif_abapgit_repo_srv~get.
    IF iv_key = '1'.
      ri_repo = me.
    ENDIF.
  ENDMETHOD.

  METHOD Lif_abapgit_repo~get_files_local_filtered.
  ENDMETHOD.

  METHOD Lif_abapgit_repo~get_files_local.
    rt_files = mt_local_files.
  ENDMETHOD.

  METHOD Lif_abapgit_repo~get_files_remote.
    rt_files = mt_remote_files.
  ENDMETHOD.

  METHOD Lif_abapgit_repo~get_key.
    rv_key = '1'.
  ENDMETHOD.

  METHOD Lif_abapgit_repo~get_name.
    rv_name = 'test'.
  ENDMETHOD.
  METHOD Lif_abapgit_repo~checksums.
  ENDMETHOD.

  METHOD Lif_abapgit_repo_srv~init.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~delete.
  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_local_settings.
  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_package.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~get_repo_from_package.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~get_repo_from_url.
  ENDMETHOD.
  METHOD Lif_abapgit_repo~has_remote_source.
  ENDMETHOD.
  METHOD Lif_abapgit_repo~is_offline.
  ENDMETHOD.
  METHOD Lif_abapgit_repo~deserialize.
  ENDMETHOD.
  METHOD Lif_abapgit_repo~deserialize_checks.
  ENDMETHOD.
  METHOD Lif_abapgit_repo~set_dot_abapgit.
  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_dot_abapgit.
  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_tadir_objects.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~is_repo_installed.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~list.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~list_favorites.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~new_offline.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~new_online.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~purge.
  ENDMETHOD.
  METHOD Lif_abapgit_repo~refresh.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~validate_package.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~validate_url.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~get_label_list.
  ENDMETHOD.

ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KW6FSFY DEFINITION FINAL.
  PUBLIC SECTION.
    DATA mt_tab TYPE Lif_abapgit_definitions=>ty_files_item_tt.
    METHODS add IMPORTING iv_str TYPE string.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KW6FSFY IMPLEMENTATION.
  METHOD add.
    DATA ls_item LIKE LINE OF mt_tab.
    DATA lv_tmp TYPE string.
    lv_tmp = iv_str.
    CONDENSE lv_tmp.
    SPLIT lv_tmp AT space INTO
      ls_item-item-devclass
      ls_item-item-obj_type
      ls_item-item-obj_name
      ls_item-file-path
      ls_item-file-filename
      ls_item-file-sha1.
    IF ls_item-item-devclass = '@'. " Root items
      CLEAR: ls_item-item-devclass, ls_item-item-obj_type, ls_item-item-obj_name.
    ENDIF.
    APPEND ls_item TO mt_tab.
  ENDMETHOD.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KW6HSFY DEFINITION FINAL.
  PUBLIC SECTION.
    DATA mt_tab TYPE Lif_abapgit_git_definitions=>ty_files_tt.
    METHODS add IMPORTING iv_str TYPE string.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KW6HSFY IMPLEMENTATION.
  METHOD add.
    DATA ls_item LIKE LINE OF mt_tab.
    DATA lv_tmp TYPE string.
    lv_tmp = iv_str.
    CONDENSE lv_tmp.
    SPLIT lv_tmp AT space INTO
      ls_item-path
      ls_item-filename
      ls_item-sha1.
    APPEND ls_item TO mt_tab.
  ENDMETHOD.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KW6JSFY DEFINITION FINAL.
  PUBLIC SECTION.
    DATA mt_tab TYPE Lif_abapgit_git_definitions=>ty_file_signatures_tt.
    METHODS add IMPORTING iv_str TYPE string.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KW6JSFY IMPLEMENTATION.
  METHOD add.
    DATA ls_item LIKE LINE OF mt_tab.
    DATA lv_tmp TYPE string.
    lv_tmp = iv_str.
    CONDENSE lv_tmp.
    SPLIT lv_tmp AT space INTO
      ls_item-path
      ls_item-filename
      ls_item-sha1.
    APPEND ls_item TO mt_tab.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
* CHECKSUMS UT
**********************************************************************


**********************************************************************
* UPDATE CALCULATOR
**********************************************************************



class LCL_ABAPGIT_REPO_CHECKSUMS implementation.
*"* method's implementations
*include methods.
  METHOD add_meta.

    DATA lv_meta_str TYPE string.

    lv_meta_str = |#repo_name#{ mi_repo->get_name( ) }|.

    cv_cs_blob = lv_meta_str && |\n| && cv_cs_blob.

  ENDMETHOD.
  METHOD build_checksums_from_files.

    DATA ls_last_item TYPE Lif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS:
      <ls_checksum>    LIKE LINE OF rt_checksums,
      <ls_local>       LIKE LINE OF it_local,
      <ls_cs_file_sig> LIKE LINE OF <ls_checksum>-files.

    " This methods is run at repo creation moment or manually by user
    " In the first case it assumes that the local state is the CURRENT state
    " Thus the idea is to copy local state to checksums
    " The second case is an exception, when we acknoledge that the state is unknown
    " Thus copying the local to checksums is the "best guess"

    LOOP AT it_local ASSIGNING <ls_local>.
      IF ls_last_item <> <ls_local>-item OR sy-tabix = 1. " First or New item reached ?
        APPEND INITIAL LINE TO rt_checksums ASSIGNING <ls_checksum>.
        MOVE-CORRESPONDING <ls_local>-item TO <ls_checksum>-item.
        ls_last_item       = <ls_local>-item.
      ENDIF.

      APPEND INITIAL LINE TO <ls_checksum>-files ASSIGNING <ls_cs_file_sig>.
      MOVE-CORRESPONDING <ls_local>-file TO <ls_cs_file_sig>.

    ENDLOOP.

  ENDMETHOD.
  METHOD constructor.
    ASSERT iv_repo_key IS NOT INITIAL.
    mv_repo_key = iv_repo_key.
    mi_repo = Lcl_abapgit_repo_srv=>get_instance( )->get( mv_repo_key ).
    " Should be safe as repo_srv is supposed to be single source of repo instances
  ENDMETHOD.
  METHOD extract_meta.

    DATA lv_meta_str TYPE string.

    IF cv_cs_blob+0(1) <> '#'.
      RETURN. " No meta ? just ignore it
    ENDIF.

    SPLIT cv_cs_blob AT |\n| INTO lv_meta_str cv_cs_blob.
    " Just remove the header meta string - this is OK for now.
    " There is just repo name for the moment - needed to for DB util and potential debug

  ENDMETHOD.
  METHOD force_write.

    " for migration only for the moment

    save_checksums( it_checksums ).

  ENDMETHOD.
  METHOD remove_non_code_related_files.

    DELETE ct_local_files
      WHERE item IS INITIAL
      AND NOT ( file-path = Lif_abapgit_definitions=>c_root_dir
      AND file-filename = Lif_abapgit_definitions=>c_dot_abapgit ).

  ENDMETHOD.
  METHOD save_checksums.

    DATA lv_cs_blob TYPE string.

    lv_cs_blob = SHRITEFUH64VYIPO5IWUYB3KW54SFY=>serialize( it_checksums ).
    add_meta( CHANGING cv_cs_blob = lv_cs_blob ).
    Lcl_abapgit_persist_factory=>get_repo_cs( )->update(
      iv_key     = mv_repo_key
      iv_cs_blob = lv_cs_blob ).

  ENDMETHOD.
  METHOD Lif_abapgit_repo_checksums~get.

    DATA lv_cs_blob TYPE string.

    TRY.
        lv_cs_blob = Lcl_abapgit_persist_factory=>get_repo_cs( )->read( mv_repo_key ).
      CATCH Lcx_abapgit_exception Lcx_abapgit_not_found.
        " Ignore currently, it's not critical for execution, just return empty
        RETURN.
    ENDTRY.

    IF lv_cs_blob IS NOT INITIAL.
      extract_meta( CHANGING cv_cs_blob = lv_cs_blob ).
      rt_checksums = SHRITEFUH64VYIPO5IWUYB3KW54SFY=>deserialize( lv_cs_blob ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_checksums~get_checksums_per_file.

    DATA lt_checksums   TYPE Lif_abapgit_persistence=>ty_local_checksum_tt.
    FIELD-SYMBOLS <ls_object> LIKE LINE OF lt_checksums.

    lt_checksums = Lif_abapgit_repo_checksums~get( ).

    LOOP AT lt_checksums ASSIGNING <ls_object>.
      APPEND LINES OF <ls_object>-files TO rt_checksums.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_checksums~rebuild.

    DATA lt_local     TYPE ty_local_files_by_item_tt.
    DATA lt_checksums TYPE Lif_abapgit_persistence=>ty_local_checksum_tt.

    lt_local  = mi_repo->get_files_local( ).
    remove_non_code_related_files( CHANGING ct_local_files = lt_local ).

    lt_checksums = build_checksums_from_files( lt_local ).
    save_checksums( lt_checksums ).

  ENDMETHOD.
  METHOD Lif_abapgit_repo_checksums~update.

    DATA lt_checksums   TYPE Lif_abapgit_persistence=>ty_local_checksum_tt.
    DATA lt_local_files TYPE Lif_abapgit_definitions=>ty_files_item_tt.

    lt_checksums   = Lif_abapgit_repo_checksums~get( ).
    lt_local_files = mi_repo->get_files_local( ).

    lt_checksums = SHRITEFUH64VYIPO5IWUYB3KW56SFY=>calculate_updated(
      it_current_checksums = lt_checksums
      it_local_files       = lt_local_files
      it_updated_files     = it_updated_files ).

    save_checksums( lt_checksums ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_REPO_CHECKSUMS implementation

*>>>>>>> ZCL_ABAPGIT_REPO_CONTENT_LIST <<<<<<<*

*"* macro definitions
*include zcl_abapgit_repo_content_list=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_repo_content_list=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_REPO_CONTENT_LIST implementation.
*"* method's implementations
*include methods.
  METHOD build_repo_items.

    DATA:
      lo_state      TYPE REF TO Lcl_abapgit_item_state,
      ls_file       TYPE Lif_abapgit_definitions=>ty_repo_file,
      lt_status     TYPE Lif_abapgit_definitions=>ty_results_tt,
      ls_item       TYPE Lif_abapgit_definitions=>ty_item,
      ls_previous   LIKE ls_item,
      lv_changed_by TYPE string.

    FIELD-SYMBOLS: <ls_status>    LIKE LINE OF lt_status,
                   <ls_repo_item> LIKE LINE OF rt_repo_items.


    lt_status = Lcl_abapgit_repo_status=>calculate(
      io_repo = mo_repo
      ii_log  = mi_log ).

    LOOP AT lt_status ASSIGNING <ls_status>.
      AT NEW obj_name. "obj_type + obj_name
        APPEND INITIAL LINE TO rt_repo_items ASSIGNING <ls_repo_item>.
        <ls_repo_item>-obj_type  = <ls_status>-obj_type.
        <ls_repo_item>-obj_name  = <ls_status>-obj_name.
        <ls_repo_item>-inactive  = <ls_status>-inactive.
        <ls_repo_item>-sortkey   = c_sortkey-default. " Default sort key
        <ls_repo_item>-changes   = 0.
        <ls_repo_item>-path      = <ls_status>-path.
        <ls_repo_item>-srcsystem = <ls_status>-srcsystem.
        CREATE OBJECT lo_state.
      ENDAT.

      IF <ls_status>-filename IS NOT INITIAL.
        MOVE-CORRESPONDING <ls_status> TO ls_file.
        ls_file-is_changed = boolc( <ls_status>-match = abap_false ). " TODO refactor
        APPEND ls_file TO <ls_repo_item>-files.

        IF <ls_status>-inactive = abap_true AND <ls_repo_item>-sortkey > c_sortkey-changed.
          <ls_repo_item>-sortkey = c_sortkey-inactive.
        ENDIF.

        IF ls_file-is_changed = abap_true.
          <ls_repo_item>-sortkey = c_sortkey-changed. " Changed files
          <ls_repo_item>-changes = <ls_repo_item>-changes + 1.
          lo_state->sum_with_status_item( <ls_status> ).
        ENDIF.
      ENDIF.

      IF <ls_repo_item>-changes > 0 AND <ls_repo_item>-obj_type IS NOT INITIAL.
        MOVE-CORRESPONDING <ls_repo_item> TO ls_item.
        IF ls_previous = ls_item.
          <ls_repo_item>-changed_by = lv_changed_by.
        ELSEIF Lcl_abapgit_objects=>exists( ls_item ) = abap_true.
          <ls_repo_item>-changed_by = Lcl_abapgit_objects=>changed_by( ls_item ).
          ls_previous = ls_item.
          lv_changed_by = <ls_repo_item>-changed_by.
        ENDIF.
        CLEAR ls_item.
      ENDIF.

      AT END OF obj_name. "obj_type + obj_name
        IF <ls_repo_item>-obj_type IS INITIAL.
          <ls_repo_item>-sortkey = c_sortkey-orphan. "Virtual objects
        ENDIF.
        <ls_repo_item>-lstate = lo_state->local( ).
        <ls_repo_item>-rstate = lo_state->remote( ).
        <ls_repo_item>-packmove = lo_state->is_reassigned( ).
      ENDAT.
    ENDLOOP.

  ENDMETHOD.
  METHOD build_folders.

    DATA: lv_index    TYPE i,
          lt_subitems LIKE ct_repo_items,
          ls_subitem  LIKE LINE OF ct_repo_items,
          ls_folder   LIKE LINE OF ct_repo_items.

    DATA lo_state TYPE REF TO Lcl_abapgit_item_state.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF ct_repo_items.


    LOOP AT ct_repo_items ASSIGNING <ls_item>.
      lv_index = sy-tabix.
      CHECK <ls_item>-path <> iv_cur_dir. " files in target dir - just leave them be

      IF Lcl_abapgit_path=>is_subdir( iv_path = <ls_item>-path
                                      iv_parent = iv_cur_dir ) = abap_true.
        ls_subitem-changes = <ls_item>-changes.
        ls_subitem-path    = <ls_item>-path.
        ls_subitem-lstate  = <ls_item>-lstate.
        ls_subitem-rstate  = <ls_item>-rstate.
        APPEND ls_subitem TO lt_subitems.
      ENDIF.

      DELETE ct_repo_items INDEX lv_index.
    ENDLOOP.

    SORT lt_subitems BY path ASCENDING.

    LOOP AT lt_subitems ASSIGNING <ls_item>.
      AT NEW path.
        CLEAR ls_folder.
        ls_folder-path    = <ls_item>-path.
        ls_folder-sortkey = c_sortkey-dir. " Directory
        ls_folder-is_dir  = abap_true.
        CREATE OBJECT lo_state.
      ENDAT.

      ls_folder-changes = ls_folder-changes + <ls_item>-changes.
      lo_state->sum_with_repo_item( <ls_item> ).

      AT END OF path.
        ls_folder-lstate = lo_state->local( ).
        ls_folder-rstate = lo_state->remote( ).
        APPEND ls_folder TO ct_repo_items.
      ENDAT.
    ENDLOOP.

  ENDMETHOD.
  METHOD check_repo_size.

    CONSTANTS lc_new_repo_size TYPE i VALUE 10.

    DATA lt_remote TYPE Lif_abapgit_git_definitions=>ty_files_tt.

    lt_remote = mo_repo->get_files_remote( ).

    IF lines( lt_remote ) > lc_new_repo_size.
      " Less files means it's a new repo (with just readme and license, for example) which is ok
      READ TABLE lt_remote TRANSPORTING NO FIELDS
        WITH KEY file_path
        COMPONENTS path     = Lif_abapgit_definitions=>c_root_dir
                   filename = Lif_abapgit_definitions=>c_dot_abapgit.
      IF sy-subrc <> 0.
        mi_log->add_warning( |Cannot find .abapgit.xml - Is this an abapGit repository?| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD constructor.
    mo_repo = io_repo.
    CREATE OBJECT mi_log TYPE Lcl_abapgit_log.
  ENDMETHOD.
  METHOD determine_transports.

    DATA ls_item TYPE Lif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF ct_repo_items.

    LOOP AT ct_repo_items ASSIGNING <ls_item>.
      ls_item-obj_type = <ls_item>-obj_type.
      ls_item-obj_name = <ls_item>-obj_name.
      TRY.
          <ls_item>-transport = Lcl_abapgit_factory=>get_cts_api( )->get_transport_for_object( ls_item ).
        CATCH Lcx_abapgit_exception ##NO_HANDLER.
          " Ignore errors related to object check when trying to get transport
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.
  METHOD filter_changes.

    FIELD-SYMBOLS: <ls_item> TYPE Lif_abapgit_definitions=>ty_repo_item.

    DELETE ct_repo_items WHERE changes = 0 AND inactive = abap_false.
    LOOP AT ct_repo_items ASSIGNING <ls_item> WHERE inactive = abap_false.
      DELETE <ls_item>-files WHERE is_changed = abap_false.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_log.
    DATA li_repo_log TYPE REF TO Lif_abapgit_log.
    DATA lt_repo_msg TYPE Lif_abapgit_log=>ty_log_outs.
    DATA lr_repo_msg TYPE REF TO Lif_abapgit_log=>ty_log_out.

    ri_log = mi_log.

    "add warning and error messages from repo log
    li_repo_log = mo_repo->get_log( ).
    IF li_repo_log IS BOUND.
      lt_repo_msg = li_repo_log->get_messages( ).
      LOOP AT lt_repo_msg REFERENCE INTO lr_repo_msg WHERE type CA 'EW'.
        CASE lr_repo_msg->type.
          WHEN 'E'.
            ri_log->add_error( lr_repo_msg->text ).
          WHEN 'W'.
            ri_log->add_warning( lr_repo_msg->text ).
          WHEN OTHERS.
            CONTINUE.
        ENDCASE.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD list.

    FIELD-SYMBOLS <ls_repo_item> LIKE LINE OF rt_repo_items.

    mi_log->clear( ).

    rt_repo_items = build_repo_items( ).
    check_repo_size( ).

    IF mo_repo->has_remote_source( ) = abap_false.
      " If there's no remote source, ignore the item state
      LOOP AT rt_repo_items ASSIGNING <ls_repo_item>.
        CLEAR: <ls_repo_item>-changes, <ls_repo_item>-lstate, <ls_repo_item>-rstate.
      ENDLOOP.
    ENDIF.

    IF iv_by_folders = abap_true.
      build_folders(
        EXPORTING iv_cur_dir    = iv_path
        CHANGING  ct_repo_items = rt_repo_items ).
    ENDIF.

    IF iv_changes_only = abap_true.
      " There are never changes for offline repositories
      filter_changes( CHANGING ct_repo_items = rt_repo_items ).
    ENDIF.

    IF iv_transports = abap_true.
      determine_transports( CHANGING ct_repo_items = rt_repo_items ).
    ENDIF.

    SORT rt_repo_items BY
      sortkey ASCENDING
      path ASCENDING
      obj_name ASCENDING.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_REPO_CONTENT_LIST implementation

*>>>>>>> ZCL_ABAPGIT_REPO_LABELS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_repo_labels=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_repo_labels=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_repo_labels=======ccau.


class LCL_ABAPGIT_REPO_LABELS implementation.
*"* method's implementations
*include methods.
  METHOD class_constructor.
    gv_regex = |^[{ c_allowed_chars }]*$|. " Must start with -
  ENDMETHOD.
  METHOD normalize.

    DATA lt_labels TYPE string_table.
    DATA lt_normalized TYPE string_table.
    FIELD-SYMBOLS <lv_lab> LIKE LINE OF lt_labels.

    lt_labels = split( iv_labels ).

    LOOP AT lt_labels ASSIGNING <lv_lab>.
      FIND REGEX gv_regex IN <lv_lab>.
      IF sy-subrc = 0.
        APPEND <lv_lab> TO lt_normalized.
      ENDIF.
    ENDLOOP.

    SORT lt_normalized.
    DELETE ADJACENT DUPLICATES FROM lt_normalized.

    rv_labels = concat_lines_of(
      table = lt_normalized
      sep = `, ` ).

  ENDMETHOD.
  METHOD normalize_colors.

    DATA lt_colors TYPE ty_label_colors.
    DATA lt_normalized TYPE ty_label_colors.
    DATA lt_pairs TYPE string_table.
    DATA lv_pair TYPE string.
    FIELD-SYMBOLS <ls_c> LIKE LINE OF lt_colors.

    lt_colors = split_colors( iv_config ).

    LOOP AT lt_colors ASSIGNING <ls_c>.
      TRY.
          validate_one_label_color( <ls_c> ).
          APPEND <ls_c> TO lt_normalized.
        CATCH Lcx_abapgit_exception.
      ENDTRY.
    ENDLOOP.

    SORT lt_normalized BY label.
    DELETE ADJACENT DUPLICATES FROM lt_normalized COMPARING label.

    LOOP AT lt_normalized ASSIGNING <ls_c>.
      lv_pair = <ls_c>-label && `:` && <ls_c>-color.
      APPEND lv_pair TO lt_pairs.
    ENDLOOP.

    rv_config = concat_lines_of(
      table = lt_pairs
      sep = `, ` ).

  ENDMETHOD.
  METHOD parse_color.

    DATA lv_tmp TYPE string.

    IF iv_color IS INITIAL.
      RETURN.
    ENDIF.

    IF iv_color+0(1) = '#'.
      lv_tmp  = iv_color+1.
      SPLIT lv_tmp AT '/' INTO rs_parsed-fg rs_parsed-bg rs_parsed-border.
    ELSE.
      rs_parsed-cls = iv_color.
    ENDIF.

  ENDMETHOD.
  METHOD split.

    FIELD-SYMBOLS <lv_lab> LIKE LINE OF rt_labels.

    SPLIT iv_labels AT ',' INTO TABLE rt_labels.
    LOOP AT rt_labels ASSIGNING <lv_lab>.
      CONDENSE <lv_lab>.
    ENDLOOP.
    DELETE rt_labels WHERE table_line IS INITIAL.

  ENDMETHOD.
  METHOD split_colors.

    DATA lt_pairs TYPE string_table.
    DATA lv_clean_config LIKE iv_config.
    DATA ls_c LIKE LINE OF rt_label_colors.
    FIELD-SYMBOLS <lv_pair> LIKE LINE OF lt_pairs.

    lv_clean_config = replace(
      val = iv_config
      sub = cl_abap_char_utilities=>newline
      with = ` ` ). " text area ends with LF

    SPLIT lv_clean_config AT ',' INTO TABLE lt_pairs.
    LOOP AT lt_pairs ASSIGNING <lv_pair>.
      CONDENSE <lv_pair>.
      IF <lv_pair> IS NOT INITIAL.
        SPLIT <lv_pair> AT ':' INTO ls_c-label ls_c-color.
        CONDENSE ls_c-label.
        CONDENSE ls_c-color.
        APPEND ls_c TO rt_label_colors.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD split_colors_into_map.

    DATA lt_colors TYPE ty_label_colors.
    FIELD-SYMBOLS <ls_c> LIKE LINE OF lt_colors.

    lt_colors = split_colors( iv_config ).

    ro_map = Lcl_abapgit_string_map=>create( ).
    LOOP AT lt_colors ASSIGNING <ls_c>.
      TRY.
          ro_map->set(
            iv_key = <ls_c>-label
            iv_val = <ls_c>-color ).
        CATCH Lcx_abapgit_exception.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.
  METHOD validate.

    DATA lt_labels TYPE string_table.
    FIELD-SYMBOLS <lv_lab> LIKE LINE OF lt_labels.

    lt_labels = split( iv_labels ).

    LOOP AT lt_labels ASSIGNING <lv_lab>.
      FIND REGEX gv_regex IN <lv_lab>.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Disallowed chars in label #{ sy-tabix }| ).
      ENDIF.
      " TODO: maybe also limit length ?
    ENDLOOP.

  ENDMETHOD.
  METHOD validate_colors.

    DATA lt_colors TYPE ty_label_colors.
    FIELD-SYMBOLS <ls_c> LIKE LINE OF lt_colors.

    lt_colors = split_colors( iv_config ).

    LOOP AT lt_colors ASSIGNING <ls_c>.
      validate_one_label_color(
        is_lc    = <ls_c>
        iv_index = sy-tabix ).
    ENDLOOP.

  ENDMETHOD.
  METHOD validate_one_label_color.

    DATA ls_parsed_color TYPE ty_color.

    IF is_lc-label IS INITIAL.
      Lcx_abapgit_exception=>raise( |Label is empty in pair #{ iv_index }| ).
    ENDIF.

    IF is_lc-color IS INITIAL.
      Lcx_abapgit_exception=>raise( |Color is empty in pair #{ iv_index }| ).
    ENDIF.

    FIND REGEX gv_regex IN is_lc-label.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Disallowed chars in label in pair #{ iv_index }| ).
    ENDIF.

    ls_parsed_color = parse_color( is_lc-color ).
    IF ls_parsed_color-cls IS NOT INITIAL.
      FIND REGEX '^[-_A-Za-z]+$' IN ls_parsed_color-cls.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Disallowed chars in color in pair #{ iv_index }| ).
      ENDIF.
    ENDIF.
    IF ls_parsed_color-fg IS NOT INITIAL.
      validate_rgb_color( ls_parsed_color-fg ).
    ENDIF.
    IF ls_parsed_color-bg IS NOT INITIAL.
      validate_rgb_color( ls_parsed_color-bg ).
    ENDIF.
    IF ls_parsed_color-border IS NOT INITIAL.
      validate_rgb_color( ls_parsed_color-border ).
    ENDIF.

  ENDMETHOD.
  METHOD validate_rgb_color.

    DATA lv_len TYPE i.

    IF iv_color IS NOT INITIAL.
      FIND REGEX '^[0-9A-Fa-f]+$' IN iv_color.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Disallowed chars in color in pair #{ iv_index }| ).
      ENDIF.
      lv_len = strlen( iv_color ).
      IF NOT ( lv_len = 3 OR lv_len = 6 ).
        Lcx_abapgit_exception=>raise( |Incorrect color in pair #{ iv_index }| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_REPO_LABELS implementation

*>>>>>>> ZCL_ABAPGIT_REPO_OFFLINE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_repo_offline======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_repo_offline======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_REPO_OFFLINE implementation.
*"* method's implementations
*include methods.
  METHOD reset_remote.

    DATA lt_backup LIKE mt_remote.

    " online repo has online source to renew data from, offline does not
    " so offline repo preserves the remote
    " in case of partial pull failure the user will immediately see the new difference
    " UI will detect "pullable" content based on mt_status
    " in the uniform way both for online and offline repos
    " for more details see discussion in 2096 and 1953

    lt_backup = mt_remote.
    super->reset_remote( ).
    set_files_remote( lt_backup ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_REPO_OFFLINE implementation

*>>>>>>> ZCL_ABAPGIT_REPO_ONLINE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_repo_online=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_repo_online=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_REPO_ONLINE implementation.
*"* method's implementations
*include methods.
  METHOD fetch_remote.

    DATA: li_progress TYPE REF TO Lif_abapgit_progress,
          ls_pull     TYPE Lcl_abapgit_git_porcelain=>ty_pull_result.

    IF mv_request_remote_refresh = abap_false.
      RETURN.
    ENDIF.

    li_progress = Lcl_abapgit_progress=>get_instance( 1 ).

    li_progress->show( iv_current = 1
                       iv_text    = 'Fetch remote files' ).

    IF get_selected_commit( ) IS INITIAL.
      ls_pull = Lcl_abapgit_git_porcelain=>pull_by_branch( iv_url         = get_url( )
                                                           iv_branch_name = get_selected_branch( ) ).
    ELSE.
      ls_pull = Lcl_abapgit_git_porcelain=>pull_by_commit( iv_url         = get_url( )
                                                           iv_commit_hash = get_selected_commit( ) ).
    ENDIF.

    set_files_remote( ls_pull-files ).
    set_objects( ls_pull-objects ).
    mv_current_commit = ls_pull-commit.

  ENDMETHOD.
  METHOD get_objects.
    fetch_remote( ).
    rt_objects = mt_objects.
  ENDMETHOD.
  METHOD handle_stage_ignore.

    DATA: lv_add         TYPE abap_bool,
          lo_dot_abapgit TYPE REF TO Lcl_abapgit_dot_abapgit,
          lt_stage       TYPE Lif_abapgit_definitions=>ty_stage_tt.

    FIELD-SYMBOLS: <ls_stage> LIKE LINE OF lt_stage.


    lo_dot_abapgit = get_dot_abapgit( ).
    lt_stage = io_stage->get_all( ).
    LOOP AT lt_stage ASSIGNING <ls_stage> WHERE method = Lif_abapgit_definitions=>c_method-ignore.

      lo_dot_abapgit->add_ignore(
        iv_path     = <ls_stage>-file-path
        iv_filename = <ls_stage>-file-filename ).

      " remove it from the staging object, as the action is handled here
      io_stage->reset( iv_path     = <ls_stage>-file-path
                       iv_filename = <ls_stage>-file-filename ).

      lv_add = abap_true.

    ENDLOOP.

    IF lv_add = abap_true.
      io_stage->add(
        iv_path     = Lif_abapgit_definitions=>c_root_dir
        iv_filename = Lif_abapgit_definitions=>c_dot_abapgit
        iv_data     = lo_dot_abapgit->serialize( ) ).

      set_dot_abapgit( lo_dot_abapgit ).
    ENDIF.

  ENDMETHOD.
  METHOD raise_error_if_branch_exists.

    DATA:
      lt_branches     TYPE Lif_abapgit_git_definitions=>ty_git_branch_list_tt,
      lv_display_name TYPE string.

    lt_branches = Lcl_abapgit_git_transport=>branches( get_url( ) )->get_branches_only( ).

    READ TABLE lt_branches WITH TABLE KEY name_key
                           COMPONENTS name = iv_name
                           TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      lv_display_name = Lcl_abapgit_git_branch_list=>get_display_name( iv_name ).
      Lcx_abapgit_exception=>raise( |Branch '{ lv_display_name }' already exists| ).
    ENDIF.

  ENDMETHOD.
  METHOD set_objects.
    mt_objects = it_objects.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_online~check_for_valid_branch.

    DATA:
      lo_branch_list TYPE REF TO Lcl_abapgit_git_branch_list,
      lv_branch      TYPE string,
      lv_head        TYPE string,
      lv_msg         TYPE string.

    lv_branch = get_selected_branch( ).

    IF lv_branch IS NOT INITIAL.
      lo_branch_list = Lcl_abapgit_git_transport=>branches( get_url( ) ).

      TRY.
          lo_branch_list->find_by_name( lv_branch ).
        CATCH Lcx_abapgit_exception.
          " branch does not exist, fallback to head
          lv_head = lo_branch_list->get_head_symref( ).
          IF lo_branch_list->get_type( lv_branch ) = Lif_abapgit_git_definitions=>c_git_branch_type-branch.
            lv_msg = 'Branch'.
          ELSE.
            lv_msg = 'Tag'.
          ENDIF.
          lv_msg = |{ lv_msg } { lo_branch_list->get_display_name( lv_branch ) } does not exist.|
                && | Switched to { lo_branch_list->get_display_name( lv_head ) }|.
          MESSAGE lv_msg TYPE 'S'.
          select_branch( lv_head ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_online~create_branch.

    DATA: lv_sha1 TYPE Lif_abapgit_git_definitions=>ty_sha1.

    ASSERT iv_name CP Lif_abapgit_git_definitions=>c_git_branch-heads.

    IF iv_from IS INITIAL.
      lv_sha1 = get_current_remote( ).
    ELSE.
      lv_sha1 = iv_from.
    ENDIF.

    raise_error_if_branch_exists( iv_name ).

    Lcl_abapgit_git_porcelain=>create_branch(
      iv_url  = get_url( )
      iv_name = iv_name
      iv_from = lv_sha1 ).

    " automatically switch to new branch
    select_branch( iv_name ).

  ENDMETHOD.
  METHOD Lif_abapgit_repo_online~get_current_remote.
    fetch_remote( ).
    rv_sha1 = mv_current_commit.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_online~get_selected_branch.
    rv_name = ms_data-branch_name.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_online~get_selected_commit.
    rv_selected_commit = ms_data-selected_commit.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_online~get_switched_origin.
    rv_switched_origin = ms_data-switched_origin.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_online~get_url.
    rv_url = ms_data-url.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_online~push.

* assumption: PUSH is done on top of the currently selected branch

    DATA: ls_push   TYPE Lcl_abapgit_git_porcelain=>ty_push_result,
          lv_text   TYPE string,
          lv_parent TYPE Lif_abapgit_git_definitions=>ty_sha1.


    IF ms_data-branch_name CP Lif_abapgit_git_definitions=>c_git_branch-tags.
      lv_text = |You're working on a tag. Currently it's not |
             && |possible to push on tags. Consider creating a branch instead|.
      Lcx_abapgit_exception=>raise( lv_text ).
    ENDIF.

    IF ms_data-selected_commit IS NOT INITIAL.
      lv_text = 'You are currently checked out in a commit.'.
      lv_text = |{ lv_text } You must be on a branch to push|.
      Lcx_abapgit_exception=>raise( lv_text ).
    ENDIF.

    IF ms_data-local_settings-block_commit = abap_true
        AND Lcl_abapgit_factory=>get_code_inspector( get_package( )
          )->is_successful( ) = abap_false.
      Lcx_abapgit_exception=>raise( |A successful code inspection is required| ).
    ENDIF.

    handle_stage_ignore( io_stage ).

    IF get_selected_commit( ) IS INITIAL.
      lv_parent = get_current_remote( ).
    ELSE.
      lv_parent = get_selected_commit( ).
    ENDIF.

    ls_push = Lcl_abapgit_git_porcelain=>push(
      is_comment     = is_comment
      io_stage       = io_stage
      iv_branch_name = get_selected_branch( )
      iv_url         = get_url( )
      iv_parent      = lv_parent
      it_old_objects = get_objects( ) ).

    set_objects( ls_push-new_objects ).
    set_files_remote( ls_push-new_files ).

    mv_current_commit = ls_push-branch.

    Lif_abapgit_repo~checksums( )->update( ls_push-updated_files ).

  ENDMETHOD.
  METHOD Lif_abapgit_repo_online~select_branch.

    reset_remote( ).
    set( iv_branch_name     = iv_branch_name
         iv_selected_commit = space ).

  ENDMETHOD.
  METHOD Lif_abapgit_repo_online~select_commit.

    reset_remote( ).
    set( iv_selected_commit = iv_selected_commit ).

  ENDMETHOD.
  METHOD Lif_abapgit_repo_online~set_url.

    reset_remote( ).
    set( iv_url = iv_url ).

  ENDMETHOD.
  METHOD Lif_abapgit_repo_online~switch_origin.

    DATA lv_offs TYPE i.

    " For repo settings page
    IF iv_overwrite = abap_true.
      set( iv_switched_origin = iv_url ).
      RETURN.
    ENDIF.

    IF iv_url IS INITIAL.
      IF ms_data-switched_origin IS INITIAL.
        RETURN.
      ELSE.
        lv_offs = find(
          val = reverse( ms_data-switched_origin )
          sub = '@' ).
        IF lv_offs = -1.
          Lcx_abapgit_exception=>raise( 'Incorrect format of switched origin' ).
        ENDIF.
        lv_offs = strlen( ms_data-switched_origin ) - lv_offs - 1.
        set_url( substring(
          val = ms_data-switched_origin
          len = lv_offs ) ).
        select_branch( substring(
          val = ms_data-switched_origin
          off = lv_offs + 1 ) ).
        set( iv_switched_origin = '' ).
      ENDIF.
    ELSEIF ms_data-switched_origin IS INITIAL.
      set( iv_switched_origin = ms_data-url && '@' && ms_data-branch_name ).
      set_url( iv_url ).
      select_branch( iv_branch ).
    ELSE.
      Lcx_abapgit_exception=>raise( 'Cannot switch origin twice' ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_files_remote.
    fetch_remote( ).
    rt_files = super->get_files_remote(
      ii_obj_filter   = ii_obj_filter
      iv_ignore_files = iv_ignore_files ).
  ENDMETHOD.
  METHOD Lif_abapgit_repo~get_name.
    rv_name = super->get_name( ).
    IF rv_name IS INITIAL.
      TRY.
          rv_name = Lcl_abapgit_url=>name( ms_data-url ).
          rv_name = cl_http_utility=>unescape_url( rv_name ).
        CATCH Lcx_abapgit_exception.
          rv_name = 'New online repo'. "unlikely fallback
      ENDTRY.
    ENDIF.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_REPO_ONLINE implementation

*>>>>>>> ZCL_ABAPGIT_REPO_SRV <<<<<<<*

*"* macro definitions
*include zcl_abapgit_repo_srv==========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_repo_srv==========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_REPO_SRV implementation.
*"* method's implementations
*include methods.
  METHOD add.

    DATA li_repo LIKE LINE OF mt_list.
    DATA lo_repo TYPE REF TO Lcl_abapgit_repo.

    LOOP AT mt_list INTO li_repo.
      IF li_repo->ms_data-key = ii_repo->ms_data-key.
        IF li_repo = ii_repo.
          RETURN.
        ENDIF.
        Lcx_abapgit_exception=>raise( 'identical keys' ).
      ENDIF.
    ENDLOOP.

    lo_repo ?= ii_repo. " TODO, refactor later
    lo_repo->bind_listener( me ).
    APPEND ii_repo TO mt_list.

  ENDMETHOD.
  METHOD determine_branch_name.

    DATA lo_branch_list TYPE REF TO Lcl_abapgit_git_branch_list.

    rv_name = iv_name.
    IF rv_name IS INITIAL.
      ASSERT NOT iv_url IS INITIAL.
      lo_branch_list = Lcl_abapgit_git_transport=>branches( iv_url ).
      rv_name = lo_branch_list->get_head_symref( ).
    ELSEIF -1 = find(
        val = rv_name
        sub = Lif_abapgit_git_definitions=>c_git_branch-heads_prefix ).
      " Assume short branch name was received
      rv_name = Lif_abapgit_git_definitions=>c_git_branch-heads_prefix && rv_name.
    ENDIF.

  ENDMETHOD.
  METHOD get_instance.
    IF gi_ref IS INITIAL.
      CREATE OBJECT gi_ref TYPE Lcl_abapgit_repo_srv.
    ENDIF.
    ri_srv = gi_ref.
  ENDMETHOD.
  METHOD inject_instance.
    gi_ref = ii_srv.
  ENDMETHOD.
  METHOD instantiate_and_add.

    IF is_repo_meta-offline = abap_false.
      CREATE OBJECT ri_repo TYPE Lcl_abapgit_repo_online
        EXPORTING
          is_data = is_repo_meta.
    ELSE.
      CREATE OBJECT ri_repo TYPE Lcl_abapgit_repo_offline
        EXPORTING
          is_data = is_repo_meta.
    ENDIF.
    add( ri_repo ).

  ENDMETHOD.
  METHOD refresh_all.

    DATA: lt_list TYPE Lif_abapgit_persistence=>ty_repos.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF lt_list.

    CLEAR mt_list.

    lt_list = Lcl_abapgit_persist_factory=>get_repo( )->list( ).
    LOOP AT lt_list ASSIGNING <ls_list>.
      instantiate_and_add( <ls_list> ).
    ENDLOOP.

    mv_init = abap_true.
    mv_only_favorites = abap_false.

  ENDMETHOD.
  METHOD refresh_favorites.

    DATA: lt_list           TYPE Lif_abapgit_persistence=>ty_repos,
          lt_user_favorites TYPE Lif_abapgit_persist_user=>ty_favorites.

    DATA li_repo TYPE REF TO Lif_abapgit_repo.
    DATA lv_repo_index TYPE i.
    DATA lo_repo_db TYPE REF TO Lif_abapgit_persist_repo.

    FIELD-SYMBOLS: <ls_repo_record> LIKE LINE OF lt_list.

    lo_repo_db        = Lcl_abapgit_persist_factory=>get_repo( ).
    lt_user_favorites = Lcl_abapgit_persistence_user=>get_instance( )->get_favorites( ).
    lt_list           = lo_repo_db->list_by_keys( lt_user_favorites ).

    SORT lt_list BY package.

    LOOP AT mt_list INTO li_repo.
      lv_repo_index = sy-tabix.

      READ TABLE lt_list TRANSPORTING NO FIELDS WITH KEY package = li_repo->get_package( ).
      IF sy-subrc = 0.
        DELETE lt_list INDEX sy-tabix.
        CONTINUE. " Leave the repo be
      ELSEIF lo_repo_db->exists( li_repo->get_key( ) ) = abap_false.
        " Not a fav, and also does not exist, probably uninstalled
        DELETE mt_list INDEX lv_repo_index.
      ENDIF.

    ENDLOOP.

    " Create remaining (new) favs
    LOOP AT lt_list ASSIGNING <ls_repo_record>.
      instantiate_and_add( <ls_repo_record> ).
    ENDLOOP.

    mv_init = abap_true.
    mv_only_favorites = abap_true.

  ENDMETHOD.
  METHOD reinstantiate_repo.

    DATA li_repo      TYPE REF TO Lif_abapgit_repo.
    DATA ls_full_meta TYPE Lif_abapgit_persistence=>ty_repo.

    li_repo = Lif_abapgit_repo_srv~get( iv_key ).
    DELETE TABLE mt_list FROM li_repo.
    ASSERT sy-subrc IS INITIAL.

    MOVE-CORRESPONDING is_meta TO ls_full_meta.
    ls_full_meta-key = iv_key.

    instantiate_and_add( ls_full_meta ).

  ENDMETHOD.
  METHOD validate_sub_super_packages.

    DATA:
      ls_repo     LIKE LINE OF it_repos,
      li_package  TYPE REF TO Lif_abapgit_sap_package,
      lt_packages TYPE Lif_abapgit_sap_package=>ty_devclass_tt,
      li_repo     TYPE REF TO Lif_abapgit_repo.

    LOOP AT it_repos INTO ls_repo.
      li_repo = Lif_abapgit_repo_srv~get( ls_repo-key ).

      li_package = Lcl_abapgit_factory=>get_sap_package( ls_repo-package ).
      IF li_package->exists( ) = abap_false.
        " Skip dangling repository
        CONTINUE.
      ENDIF.

      CLEAR lt_packages.
      IF li_repo->get_local_settings( )-ignore_subpackages = abap_false.
        APPEND LINES OF li_package->list_subpackages( ) TO lt_packages.
        READ TABLE lt_packages TRANSPORTING NO FIELDS
          WITH KEY table_line = iv_package.
        IF sy-subrc = 0.
          ei_repo = li_repo.
          ev_reason = |Repository { li_repo->get_name( ) } already contains { iv_package } |.
          RETURN.
        ENDIF.
      ENDIF.

      IF iv_ign_subpkg = abap_false.
        APPEND LINES OF li_package->list_superpackages( ) TO lt_packages.
        READ TABLE lt_packages TRANSPORTING NO FIELDS
          WITH KEY table_line = iv_package.
        IF sy-subrc = 0.
          ei_repo = li_repo.
          ev_reason = |Repository { li_repo->get_name( ) } already contains subpackage of { iv_package } |.
          RETURN.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_listener~on_meta_change.

    DATA li_persistence TYPE REF TO Lif_abapgit_persist_repo.

    li_persistence = Lcl_abapgit_persist_factory=>get_repo( ).
    li_persistence->update_metadata(
      iv_key         = iv_key
      is_meta        = is_meta
      is_change_mask = is_change_mask ).


    " Recreate repo instance if type changed
    " Instances in mt_list are of *_online and *_offline type
    " If type is changed object should be recreated from the proper class
    " TODO refactor, e.g. unify repo logic in one class
    IF is_change_mask-offline = abap_true.
      reinstantiate_repo(
        iv_key  = iv_key
        is_meta = is_meta ).

    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~delete.

    Lcl_abapgit_persist_factory=>get_repo( )->delete( ii_repo->get_key( ) ).
    Lcl_abapgit_persist_factory=>get_repo_cs( )->delete( ii_repo->get_key( ) ).

    " If favorite, remove it
    IF Lcl_abapgit_persistence_user=>get_instance( )->is_favorite_repo( ii_repo->get_key( ) ) = abap_true.
      Lcl_abapgit_persistence_user=>get_instance( )->toggle_favorite( ii_repo->get_key( ) ).
    ENDIF.

    DELETE TABLE mt_list FROM ii_repo.
    ASSERT sy-subrc = 0.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~get.

    FIELD-SYMBOLS: <li_repo> LIKE LINE OF mt_list.

    ASSERT iv_key IS NOT INITIAL.

    IF mv_init = abap_false.
      refresh_all( ).
    ENDIF.

    DO 2 TIMES.
      " Repo might have been created in another session. Try again after refresh
      IF sy-index = 2.
        refresh_all( ).
      ENDIF.
      LOOP AT mt_list ASSIGNING <li_repo>.
        IF <li_repo>->ms_data-key = iv_key.
          ri_repo = <li_repo>.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDDO.

    Lcx_abapgit_exception=>raise( |Repository not found in database. Key: REPO, { iv_key }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~get_label_list.

    DATA:
      lt_repo           TYPE Lif_abapgit_repo_srv=>ty_repo_list,
      ls_local_settings TYPE Lif_abapgit_persistence=>ty_repo-local_settings,
      lt_labels         TYPE string_table,
      ls_label          LIKE LINE OF rt_labels.

    FIELD-SYMBOLS:
      <ls_repo>  TYPE REF TO Lif_abapgit_repo,
      <lv_label> TYPE LINE OF string_table.

    lt_repo = Lif_abapgit_repo_srv~list( ).

    LOOP AT lt_repo ASSIGNING <ls_repo>.

      ls_local_settings = <ls_repo>->get_local_settings( ).
      lt_labels = Lcl_abapgit_repo_labels=>split( ls_local_settings-labels ).

      LOOP AT lt_labels ASSIGNING <lv_label>.
        ls_label-label = <lv_label>.
        INSERT ls_label INTO TABLE rt_labels.
      ENDLOOP.

    ENDLOOP.

    SORT rt_labels.
    DELETE ADJACENT DUPLICATES FROM rt_labels.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~get_repo_from_package.

    DATA:
      lt_repos TYPE Lif_abapgit_persistence=>ty_repos,
      lv_name  TYPE Lif_abapgit_persistence=>ty_local_settings-display_name,
      lv_owner TYPE Lif_abapgit_persistence=>ty_local_settings-display_name.

    FIELD-SYMBOLS:
      <ls_repo> LIKE LINE OF lt_repos.

    " check if package is already in use for a different repository
    lt_repos = Lcl_abapgit_persist_factory=>get_repo( )->list( ).
    READ TABLE lt_repos WITH KEY package = iv_package ASSIGNING <ls_repo>.
    IF sy-subrc = 0.
      ei_repo = get_instance( )->get( <ls_repo>-key ).
      lv_name = ei_repo->get_name( ).
      lv_owner = <ls_repo>-created_by.
      ev_reason = |Package { iv_package } already versioned as { lv_name } by { lv_owner }|.
    ELSE.
      " check if package is include as sub-package in a different repo
      validate_sub_super_packages(
        EXPORTING
          iv_package    = iv_package
          it_repos      = lt_repos
          iv_ign_subpkg = iv_ign_subpkg
        IMPORTING
          ei_repo       = ei_repo
          ev_reason     = ev_reason ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~get_repo_from_url.

    DATA:
      lt_repos                TYPE Lif_abapgit_persistence=>ty_repos,
      lv_current_repo_address TYPE string,
      lv_check_repo_address   TYPE string,
      lv_repo_path            TYPE string,
      lv_name                 TYPE Lif_abapgit_persistence=>ty_local_settings-display_name,
      lv_owner                TYPE Lif_abapgit_persistence=>ty_local_settings-display_name.

    FIELD-SYMBOLS:
      <ls_repo> LIKE LINE OF lt_repos.

    CLEAR:
      ei_repo, ev_reason.

    lv_current_repo_address = Lcl_abapgit_url=>url_address( iv_url ).

    " check if url is already in use for a different package
    lt_repos = Lcl_abapgit_persist_factory=>get_repo( )->list( ).
    LOOP AT lt_repos ASSIGNING <ls_repo> WHERE offline = abap_false.

      lv_check_repo_address = Lcl_abapgit_url=>url_address( <ls_repo>-url ).

      IF lv_current_repo_address = lv_check_repo_address.
        ei_repo      = get_instance( )->get( <ls_repo>-key ).
        lv_repo_path = Lcl_abapgit_url=>path_name( iv_url ).
        lv_name      = ei_repo->get_name( ).
        lv_owner     = <ls_repo>-created_by.
        ev_reason    = |Repository { lv_repo_path } already versioned as { lv_name } by { lv_owner }|.
        RETURN.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~init.
    CLEAR mv_init.
  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~is_repo_installed.

    DATA: lt_repo        TYPE Lif_abapgit_repo_srv=>ty_repo_list,
          li_repo        TYPE REF TO Lif_abapgit_repo,
          lv_url         TYPE string,
          lv_package     TYPE devclass,
          lo_repo_online TYPE REF TO Lcl_abapgit_repo_online,
          lv_err         TYPE string.

    lt_repo = Lif_abapgit_repo_srv~list( ).

    LOOP AT lt_repo INTO li_repo.
      CHECK li_repo->is_offline( ) = abap_false.
      lo_repo_online ?= li_repo.

      lv_url     = lo_repo_online->get_url( ).
      lv_package = lo_repo_online->get_package( ).
      CHECK to_upper( lv_url ) = to_upper( iv_url ).

      " Validate bindings
      "TODO refactor: move this message out of this method
      IF iv_target_package IS NOT INITIAL AND iv_target_package <> lv_package.
        lv_err = |Installation to package { lv_package } detected. |
              && |Cancelling installation|.
        Lcx_abapgit_exception=>raise( lv_err ).
      ENDIF.

      rv_installed = abap_true.
      EXIT.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~list.

    DATA li_repo TYPE REF TO Lif_abapgit_repo.

    IF mv_init = abap_false OR mv_only_favorites = abap_true.
      refresh_all( ).
    ENDIF.

    LOOP AT mt_list INTO li_repo.
      IF iv_offline = abap_undefined OR li_repo->is_offline( ) = iv_offline.
        INSERT li_repo INTO TABLE rt_list.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~list_favorites.

    DATA lt_user_favorites TYPE Lif_abapgit_persist_user=>ty_favorites.
    DATA li_repo TYPE REF TO Lif_abapgit_repo.

    lt_user_favorites = Lcl_abapgit_persistence_user=>get_instance( )->get_favorites( ).
    SORT lt_user_favorites BY table_line.

    IF mv_init = abap_false OR mv_only_favorites = abap_false.
      refresh_favorites( ).
    ENDIF.

    LOOP AT mt_list INTO li_repo.
      READ TABLE lt_user_favorites
        TRANSPORTING NO FIELDS
        WITH KEY table_line = li_repo->get_key( ).
      IF sy-subrc = 0 AND ( iv_offline = abap_undefined OR li_repo->is_offline( ) = iv_offline ).
        APPEND li_repo TO rt_list.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~new_offline.

    DATA: ls_repo        TYPE Lif_abapgit_persistence=>ty_repo,
          lv_key         TYPE Lif_abapgit_persistence=>ty_repo-key,
          lo_repo        TYPE REF TO Lcl_abapgit_repo_offline,
          lo_dot_abapgit TYPE REF TO Lcl_abapgit_dot_abapgit.


    IF Lcl_abapgit_auth=>is_allowed( Lif_abapgit_auth=>c_authorization-create_repo ) = abap_false.
      Lcx_abapgit_exception=>raise( 'Not authorized' ).
    ENDIF.

    Lif_abapgit_repo_srv~validate_package(
      iv_package    = iv_package
      iv_ign_subpkg = iv_ign_subpkg ).

    IF iv_name IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Missing name for repository' ).
    ENDIF.

    " Repo Settings
    lo_dot_abapgit = Lcl_abapgit_dot_abapgit=>build_default( ).
    lo_dot_abapgit->set_folder_logic( iv_folder_logic ).
    lo_dot_abapgit->set_name( iv_name ).
    lo_dot_abapgit->set_abap_language_version( iv_abap_lang_vers ).

    lv_key = Lcl_abapgit_persist_factory=>get_repo( )->add(
      iv_package      = iv_package
      iv_offline      = abap_true
      is_dot_abapgit  = lo_dot_abapgit->get_data( ) ).

    TRY.
        ls_repo = Lcl_abapgit_persist_factory=>get_repo( )->read( lv_key ).
      CATCH Lcx_abapgit_not_found.
        Lcx_abapgit_exception=>raise( 'new_offline not found' ).
    ENDTRY.

    lo_repo ?= instantiate_and_add( ls_repo ).

    " Local Settings
    IF ls_repo-local_settings-ignore_subpackages <> iv_ign_subpkg.
      ls_repo-local_settings-ignore_subpackages = iv_ign_subpkg.
    ENDIF.
    ls_repo-local_settings-main_language_only = iv_main_lang_only.
    ls_repo-local_settings-labels = iv_labels.

    lo_repo->set_local_settings( ls_repo-local_settings ).

    ri_repo = lo_repo.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~new_online.

    DATA: ls_repo        TYPE Lif_abapgit_persistence=>ty_repo,
          lo_repo        TYPE REF TO Lcl_abapgit_repo_online,
          lv_branch_name LIKE iv_branch_name,
          lv_key         TYPE Lif_abapgit_persistence=>ty_repo-key,
          lo_dot_abapgit TYPE REF TO Lcl_abapgit_dot_abapgit,
          lv_url         TYPE string.


    ASSERT NOT iv_url IS INITIAL
      AND NOT iv_package IS INITIAL.

    lv_url = condense( iv_url ).

    IF Lcl_abapgit_auth=>is_allowed( Lif_abapgit_auth=>c_authorization-create_repo ) = abap_false.
      Lcx_abapgit_exception=>raise( 'Not authorized' ).
    ENDIF.

    Lif_abapgit_repo_srv~validate_package(
      iv_package    = iv_package
      iv_ign_subpkg = iv_ign_subpkg ).

    Lif_abapgit_repo_srv~validate_url( lv_url ).

    lv_branch_name = determine_branch_name(
      iv_name = iv_branch_name
      iv_url  = lv_url ).

    " Repo Settings
    lo_dot_abapgit = Lcl_abapgit_dot_abapgit=>build_default( ).
    lo_dot_abapgit->set_folder_logic( iv_folder_logic ).
    lo_dot_abapgit->set_name( iv_name ).
    lo_dot_abapgit->set_abap_language_version( iv_abap_lang_vers ).

    lv_key = Lcl_abapgit_persist_factory=>get_repo( )->add(
      iv_url          = lv_url
      iv_branch_name  = lv_branch_name " local !
      iv_display_name = iv_display_name
      iv_package      = iv_package
      iv_offline      = abap_false
      is_dot_abapgit  = lo_dot_abapgit->get_data( ) ).

    TRY.
        ls_repo = Lcl_abapgit_persist_factory=>get_repo( )->read( lv_key ).
      CATCH Lcx_abapgit_not_found.
        Lcx_abapgit_exception=>raise( 'new_online not found' ).
    ENDTRY.

    lo_repo ?= instantiate_and_add( ls_repo ).

    " Local Settings
    IF ls_repo-local_settings-ignore_subpackages <> iv_ign_subpkg.
      ls_repo-local_settings-ignore_subpackages = iv_ign_subpkg.
    ENDIF.
    ls_repo-local_settings-main_language_only = iv_main_lang_only.
    ls_repo-local_settings-labels = iv_labels.

    lo_repo->set_local_settings( ls_repo-local_settings ).

    lo_repo->refresh( ).
    lo_repo->find_remote_dot_abapgit( ).

    ri_repo = lo_repo.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~purge.

* uninstalls all objects, no UI or popups in this class

* todo, this should be a method on the repo instead?

    DATA: lt_tadir TYPE Lif_abapgit_definitions=>ty_tadir_tt.
    DATA: lx_error TYPE REF TO Lcx_abapgit_exception.
    DATA lo_repo TYPE REF TO Lcl_abapgit_repo.

    lo_repo ?= ii_repo. " TODO, remove later
    ri_log = lo_repo->create_new_log( 'Uninstall Log' ).

    IF ii_repo->get_local_settings( )-write_protected = abap_true.
      Lcx_abapgit_exception=>raise( 'Cannot purge. Local code is write-protected by repo config' ).
    ELSEIF Lcl_abapgit_auth=>is_allowed( Lif_abapgit_auth=>c_authorization-uninstall ) = abap_false.
      Lcx_abapgit_exception=>raise( 'Not authorized' ).
    ENDIF.

    lt_tadir = lo_repo->get_tadir_objects( ).

    TRY.
        Lcl_abapgit_objects=>delete( it_tadir  = lt_tadir
                                     is_checks = is_checks
                                     ii_log    = ri_log ).
      CATCH Lcx_abapgit_exception INTO lx_error.
        " If uninstall fails, repo needs a refresh to show which objects where deleted and which not
        ii_repo->refresh( iv_drop_log = abap_false ).
        RAISE EXCEPTION lx_error.
    ENDTRY.

    IF iv_keep_repo = abap_true.
      ii_repo->refresh( ).
      ii_repo->checksums( )->rebuild( ).
    ELSE.
      Lif_abapgit_repo_srv~delete( ii_repo ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~validate_package.

    DATA: lv_as4user TYPE usnam,
          li_repo    TYPE REF TO Lif_abapgit_repo,
          lv_reason  TYPE string.

    Lcl_abapgit_factory=>get_sap_package( iv_package )->validate_name( ).

    " Check if package owned by SAP is allowed (new packages are ok, since they are created automatically)
    lv_as4user = Lcl_abapgit_factory=>get_sap_package( iv_package )->read_responsible( ).

    IF sy-subrc = 0 AND lv_as4user = 'SAP' AND
      Lcl_abapgit_factory=>get_environment( )->is_sap_object_allowed( ) = abap_false.
      Lcx_abapgit_exception=>raise( |Package { iv_package } not allowed, responsible user = 'SAP'| ).
    ENDIF.

    " Check if package is already used in another repo
    IF iv_chk_exists = abap_true.
      Lif_abapgit_repo_srv~get_repo_from_package(
        EXPORTING
          iv_package    = iv_package
          iv_ign_subpkg = iv_ign_subpkg
        IMPORTING
          ei_repo       = li_repo
          ev_reason     = lv_reason ).

      IF li_repo IS BOUND.
        Lcx_abapgit_exception=>raise( lv_reason ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_repo_srv~validate_url.

    DATA:
      li_repo   TYPE REF TO Lif_abapgit_repo,
      lv_reason TYPE string.

    Lcl_abapgit_url=>validate( iv_url ).

    IF iv_chk_exists = abap_true.
      Lif_abapgit_repo_srv~get_repo_from_url(
        EXPORTING
          iv_url    = iv_url
        IMPORTING
          ei_repo   = li_repo
          ev_reason = lv_reason ).
      IF li_repo IS BOUND.
        Lcx_abapgit_exception=>raise( lv_reason ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_REPO_SRV implementation

*>>>>>>> ZCL_ABAPGIT_REPO_STATUS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_repo_status=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_repo_status=======ccimp.
CLASS SHRITEFUH64VYIPO5IWUYB3KW6QSFY DEFINITION FINAL.
  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        iv_root_package TYPE devclass
        io_dot          TYPE REF TO Lcl_abapgit_dot_abapgit.

    METHODS run_checks
      IMPORTING
        it_results    TYPE Lif_abapgit_definitions=>ty_results_tt
      RETURNING
        VALUE(ri_log) TYPE REF TO Lif_abapgit_log
      RAISING
        Lcx_abapgit_exception .

  PRIVATE SECTION.

    DATA mv_root_package TYPE devclass.
    DATA mo_dot          TYPE REF TO Lcl_abapgit_dot_abapgit.
    DATA mi_log          TYPE REF TO Lif_abapgit_log.

    METHODS check_package_move
      IMPORTING
        !it_results TYPE Lif_abapgit_definitions=>ty_results_tt
      RAISING
        Lcx_abapgit_exception .
    METHODS check_files_folder
      IMPORTING
        !it_results TYPE Lif_abapgit_definitions=>ty_results_tt
      RAISING
        Lcx_abapgit_exception .
    METHODS check_package_sub_package
      IMPORTING
        !it_results TYPE Lif_abapgit_definitions=>ty_results_tt
        !iv_top     TYPE devclass
      RAISING
        Lcx_abapgit_exception .
    METHODS check_package_folder
      IMPORTING
        !it_results TYPE Lif_abapgit_definitions=>ty_results_tt
        !io_dot     TYPE REF TO Lcl_abapgit_dot_abapgit
        !iv_top     TYPE devclass
      RAISING
        Lcx_abapgit_exception .
    METHODS check_multiple_files
      IMPORTING
        !it_results TYPE Lif_abapgit_definitions=>ty_results_tt
      RAISING
        Lcx_abapgit_exception .
    METHODS check_namespace
      IMPORTING
        !it_results      TYPE Lif_abapgit_definitions=>ty_results_tt
      RAISING
        Lcx_abapgit_exception .

ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KW6QSFY IMPLEMENTATION.

  METHOD constructor.
    mv_root_package = iv_root_package.
    mo_dot          = io_dot.
  ENDMETHOD.

  METHOD run_checks.

    CREATE OBJECT mi_log TYPE Lcl_abapgit_log.

    " Find all objects which were assigned to a different package
    check_package_move( it_results ).

    " Check files for one object is in the same folder
    check_files_folder( it_results ).

    " Check that sub packages are included in the package hierarchy
    check_package_sub_package(
      it_results = it_results
      iv_top     = mv_root_package ).

    " Check that objects are created in package corresponding to folder
    check_package_folder(
      it_results = it_results
      io_dot     = mo_dot
      iv_top     = mv_root_package ).

    " Check for multiple files with same filename
    check_multiple_files( it_results ).

    " Check if namespaces exist already
    check_namespace( it_results ).

    ri_log = mi_log.

  ENDMETHOD.

  METHOD check_files_folder.

    DATA:
      ls_item     TYPE Lif_abapgit_definitions=>ty_item,
      lt_res_sort LIKE it_results,
      lt_item_idx LIKE it_results.

    FIELD-SYMBOLS:
      <ls_result>     LIKE LINE OF it_results,
      <ls_result_idx> LIKE LINE OF it_results.

    " TODO optimize ?
    " sort by obj, path
    " loop, and compare to first object record

    " Collect object index
    lt_res_sort = it_results.
    SORT lt_res_sort BY obj_type ASCENDING obj_name ASCENDING.

    LOOP AT it_results ASSIGNING <ls_result> WHERE NOT obj_type IS INITIAL AND packmove = abap_false.

      IF NOT ( <ls_result>-obj_type = ls_item-obj_type
          AND <ls_result>-obj_name = ls_item-obj_name ).
        APPEND INITIAL LINE TO lt_item_idx ASSIGNING <ls_result_idx>.
        <ls_result_idx>-obj_type = <ls_result>-obj_type.
        <ls_result_idx>-obj_name = <ls_result>-obj_name.
        <ls_result_idx>-path     = <ls_result>-path.
        MOVE-CORRESPONDING <ls_result> TO ls_item.
      ENDIF.

    ENDLOOP.

    LOOP AT it_results ASSIGNING <ls_result>
      WHERE NOT obj_type IS INITIAL AND obj_type <> 'DEVC' AND packmove = abap_false.

      READ TABLE lt_item_idx ASSIGNING <ls_result_idx>
        WITH KEY obj_type = <ls_result>-obj_type obj_name = <ls_result>-obj_name
        BINARY SEARCH. " Sorted above

      IF sy-subrc <> 0 OR <ls_result>-path <> <ls_result_idx>-path. " All paths are same
        mi_log->add_warning( |Files for object { <ls_result>-obj_type } { <ls_result>-obj_name }|
         && | are not placed in the same folder| ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD check_multiple_files.

    DATA:
      lt_res_sort LIKE it_results,
      ls_file     TYPE Lif_abapgit_git_definitions=>ty_file_signature.

    FIELD-SYMBOLS <ls_result> LIKE LINE OF it_results.

    lt_res_sort = it_results.
    SORT lt_res_sort BY filename ASCENDING.

    LOOP AT lt_res_sort ASSIGNING <ls_result> WHERE obj_type <> 'DEVC' AND packmove = abap_false.
      IF <ls_result>-filename IS NOT INITIAL AND <ls_result>-filename = ls_file-filename.
        mi_log->add_warning( |Multiple files with same filename, { <ls_result>-filename }| ).
      ENDIF.

      IF <ls_result>-filename IS INITIAL.
        mi_log->add_warning( |Filename is empty for object { <ls_result>-obj_type } { <ls_result>-obj_name }| ).
      ENDIF.

      MOVE-CORRESPONDING <ls_result> TO ls_file.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_namespace.

    DATA:
      li_namespace TYPE REF TO Lif_abapgit_sap_namespace,
      lv_namespace TYPE namespace,
      lt_namespace TYPE TABLE OF namespace.

    FIELD-SYMBOLS <ls_result> LIKE LINE OF it_results.

    " Collect all namespaces based on name of xml- and json-files
    LOOP AT it_results ASSIGNING <ls_result>.
      FIND REGEX '^#([a-zA-Z0-9]+)#.*\..*\.xml$' IN <ls_result>-filename SUBMATCHES lv_namespace.
      IF sy-subrc = 0.
        lv_namespace = '/' && to_upper( lv_namespace ) && '/'.
        COLLECT lv_namespace INTO lt_namespace.
      ENDIF.
      FIND REGEX '^\(([a-zA-Z0-9]+)\).*\..*\.json$' IN <ls_result>-filename SUBMATCHES lv_namespace.
      IF sy-subrc = 0.
        lv_namespace = '/' && to_upper( lv_namespace ) && '/'.
        COLLECT lv_namespace INTO lt_namespace.
      ENDIF.
    ENDLOOP.

    li_namespace = Lcl_abapgit_factory=>get_sap_namespace( ).

    LOOP AT lt_namespace INTO lv_namespace.
      IF li_namespace->exists( lv_namespace ) = abap_false.
        mi_log->add_warning( |Namespace { lv_namespace } does not exist.|
          && | Pull it first (or create it in transaction SE03)| ).
      ELSEIF li_namespace->is_editable( lv_namespace ) = abap_false.
        mi_log->add_warning( |Namespace { lv_namespace } is not modifiable. Check it in transaction SE03| ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_package_folder.

    DATA:
      lv_path         TYPE string,
      lv_object       TYPE string,
      lo_folder_logic TYPE REF TO Lcl_abapgit_folder_logic.

    FIELD-SYMBOLS <ls_result> LIKE LINE OF it_results.

    lo_folder_logic = Lcl_abapgit_folder_logic=>get_instance( ).

    LOOP AT it_results ASSIGNING <ls_result>
      WHERE NOT package IS INITIAL AND NOT path IS INITIAL AND packmove = abap_false.

      lv_path = lo_folder_logic->package_to_path(
        iv_top     = iv_top
        io_dot     = io_dot
        iv_package = <ls_result>-package ).

      lv_object = |{ <ls_result>-obj_type } { <ls_result>-obj_name }|.

      IF lv_path IS INITIAL.
        mi_log->add_error( |{ lv_object } already exists outside of { iv_top } package hierarchy| ).
      ELSEIF lv_path <> <ls_result>-path.
        mi_log->add_warning( |Package and path do not match for object { lv_object }| ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD check_package_move.

    DATA lt_move_idx LIKE it_results.

    FIELD-SYMBOLS:
      <ls_result>      LIKE LINE OF it_results,
      <ls_result_move> LIKE LINE OF it_results.

    " TODO: optimize ?
    " delete where packmove = false, delete adj duplicates and fire messages ?
    LOOP AT it_results ASSIGNING <ls_result>
      WHERE lstate = Lif_abapgit_definitions=>c_state-added AND packmove = abap_true.

      READ TABLE lt_move_idx TRANSPORTING NO FIELDS
        WITH KEY
          obj_type = <ls_result>-obj_type
          obj_name = <ls_result>-obj_name
        BINARY SEARCH. " Sorted since it_result is sorted
      IF sy-subrc <> 0.
        mi_log->add_warning( |Changed package assignment for object|
          && | { <ls_result>-obj_type } { <ls_result>-obj_name }| ).
        APPEND INITIAL LINE TO lt_move_idx ASSIGNING <ls_result_move>.
        <ls_result_move>-obj_type = <ls_result>-obj_type.
        <ls_result_move>-obj_name = <ls_result>-obj_name.
        <ls_result_move>-path     = <ls_result>-path.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD check_package_sub_package.

    DATA lv_msg TYPE string.

    FIELD-SYMBOLS <ls_result> LIKE LINE OF it_results.

    LOOP AT it_results ASSIGNING <ls_result> USING KEY sec_key
                       WHERE package IS INITIAL AND obj_type = 'DEVC'.

      IF Lcl_abapgit_factory=>get_sap_package( |{ <ls_result>-obj_name }| )->exists( ) = abap_true.
        " If package already exist but is not included in the package hierarchy of
        " the package assigned to the repository, then a manual change of the package
        " is required i.e. setting a parent package to the repo package (or one of its
        " subpackages). We don't do this automatically since it's not clear where in the
        " hierarchy the new package should be located or whether the sub package shall be
        " removed from the repo.
        lv_msg = |Package { <ls_result>-obj_name } already exists but is not a sub-package of { iv_top }. |
              && |Check your package and folder logic, and either assign { <ls_result>-obj_name } |
              && |to the package hierarchy of { iv_top } or remove package { <ls_result>-obj_name } |
              && |from the repository.|.
        mi_log->add_warning( lv_msg ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

*"* test class
*include zcl_abapgit_repo_status=======ccau.
*CLASS SHRITEFUH64VYIPO5IWUYB3KW6SSFY DEFINITION DEFERRED.
*CLASS zcl_abapgit_repo_status DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KW6SSFY.





CLASS SHRITEFUH64VYIPO5IWUYB3KW6WSFY DEFINITION.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          it_results TYPE Lif_abapgit_definitions=>ty_results_tt,
      get_line
        IMPORTING
          iv_line        TYPE i
        RETURNING
          VALUE(rs_data) TYPE Lif_abapgit_definitions=>ty_result,
      assert_lines
        IMPORTING
          iv_lines TYPE i
          iv_msg   TYPE csequence OPTIONAL.

  PRIVATE SECTION.
    DATA: mt_results TYPE Lif_abapgit_definitions=>ty_results_tt.

ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KW6WSFY IMPLEMENTATION.

  METHOD constructor.

    mt_results = it_results.
    SORT mt_results BY path filename.

  ENDMETHOD.

  METHOD get_line.

    READ TABLE mt_results INDEX iv_line INTO rs_data.
    cl_abap_unit_assert=>assert_subrc( ).

  ENDMETHOD.

  METHOD assert_lines.

    cl_abap_unit_assert=>assert_equals(
      act = lines( mt_results )
      exp = iv_lines
      msg = iv_msg ).

  ENDMETHOD.

ENDCLASS.

*CLASS SHRITEFUH64VYIPO5IWUYB3KW6YSFY DEFINITION DEFERRED.
*CLASS zcl_abapgit_repo_status DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KW6YSFY.



*CLASS SHRITEFUH64VYIPO5IWUYB3KW62SFY DEFINITION DEFERRED.
*CLASS zcl_abapgit_repo_status DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KW62SFY.



class LCL_ABAPGIT_REPO_STATUS implementation.
*"* method's implementations
*include methods.
  METHOD build_existing.

    DATA ls_file_sig LIKE LINE OF it_state.

    " Item
    rs_result-obj_type  = is_local-item-obj_type.
    rs_result-obj_name  = is_local-item-obj_name.
    rs_result-package   = is_local-item-devclass.
    rs_result-srcsystem = is_local-item-srcsystem.
    rs_result-origlang  = is_local-item-origlang.
    rs_result-inactive  = is_local-item-inactive.

    " File
    rs_result-path     = is_local-file-path.
    rs_result-filename = is_local-file-filename.

    rs_result-match    = boolc( is_local-file-sha1 = is_remote-sha1 ).
    IF rs_result-match = abap_true.
      RETURN.
    ENDIF.

    " Match against current state
    READ TABLE it_state INTO ls_file_sig
      WITH KEY
        path     = is_local-file-path
        filename = is_local-file-filename
      BINARY SEARCH.

    IF sy-subrc = 0.
      IF ls_file_sig-sha1 <> is_local-file-sha1.
        rs_result-lstate = Lif_abapgit_definitions=>c_state-modified.
      ENDIF.
      IF ls_file_sig-sha1 <> is_remote-sha1.
        rs_result-rstate = Lif_abapgit_definitions=>c_state-modified.
      ENDIF.
    ELSE.
      " This is a strange situation. As both local and remote exist
      " the state should also be present. Maybe this is a first run of the code.
      " In this case just compare hashes directly and mark both changed
      " the user will presumably decide what to do after checking the actual diff
      rs_result-lstate = Lif_abapgit_definitions=>c_state-modified.
      rs_result-rstate = Lif_abapgit_definitions=>c_state-modified.
    ENDIF.

  ENDMETHOD.
  METHOD build_new_local.

    " Item
    rs_result-obj_type  = is_local-item-obj_type.
    rs_result-obj_name  = is_local-item-obj_name.
    rs_result-package   = is_local-item-devclass.
    rs_result-srcsystem = is_local-item-srcsystem.
    rs_result-origlang  = is_local-item-origlang.
    rs_result-inactive  = is_local-item-inactive.

    " File
    rs_result-path     = is_local-file-path.
    rs_result-filename = is_local-file-filename.

    " Match
    rs_result-match    = abap_false.
    rs_result-lstate   = Lif_abapgit_definitions=>c_state-added.

  ENDMETHOD.
  METHOD build_new_remote.

    DATA ls_item     LIKE LINE OF it_items_idx.
    DATA ls_file_sig LIKE LINE OF it_state_idx.

    " Common and default part
    rs_result-path     = is_remote-path.
    rs_result-filename = is_remote-filename.
    rs_result-match    = abap_false.
    rs_result-rstate   = Lif_abapgit_definitions=>c_state-added.

    Lcl_abapgit_filename_logic=>file_to_object(
      EXPORTING
        iv_filename = is_remote-filename
        iv_path     = is_remote-path
        iv_devclass = mv_root_package
        io_dot      = mo_dot
      IMPORTING
        es_item     = ls_item ).

    " Check if in item index + get package
    READ TABLE it_items_idx INTO ls_item
      WITH KEY
        obj_type = ls_item-obj_type
        obj_name = ls_item-obj_name.

    IF sy-subrc = 0.

      " Completely new (xml, abap) and new file in an existing object
      rs_result-obj_type  = ls_item-obj_type.
      rs_result-obj_name  = ls_item-obj_name.
      rs_result-package   = ls_item-devclass.
      rs_result-srcsystem = sy-sysid.
      rs_result-origlang  = sy-langu.

      READ TABLE it_state_idx INTO ls_file_sig
        WITH KEY
          path     = is_remote-path
          filename = is_remote-filename.

      " Existing file but from another package
      " was not added during local file proc as was not in tadir for repo package
      IF sy-subrc = 0.
        IF ls_file_sig-sha1 = is_remote-sha1.
          rs_result-match = abap_true.
          CLEAR rs_result-rstate.
        ELSE.
          rs_result-rstate = Lif_abapgit_definitions=>c_state-modified.
        ENDIF.

        " Item is in state and in cache but with no package - it was deleted
        " OR devclass is the same as repo package (see #532)
        IF ls_item-devclass IS INITIAL OR ls_item-devclass = mv_root_package.
          rs_result-match  = abap_false.
          rs_result-lstate = Lif_abapgit_definitions=>c_state-deleted.
        ENDIF.
      ENDIF.

    ELSE. " Completely unknown file, probably non-abapgit
      ASSERT 1 = 1. " No action, just follow defaults
    ENDIF.

  ENDMETHOD.
  METHOD calculate.

    DATA lt_local TYPE Lif_abapgit_definitions=>ty_files_item_tt.
    DATA lt_remote TYPE Lif_abapgit_git_definitions=>ty_files_tt.
    DATA li_exit TYPE REF TO Lif_abapgit_exit.
    DATA lo_instance TYPE REF TO Lcl_abapgit_repo_status.
    DATA lo_consistency_checks TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KW6QSFY.

    IF ii_obj_filter IS INITIAL.
      lt_local = io_repo->get_files_local( ii_log ).
    ELSE.
      lt_local = io_repo->get_files_local_filtered(
        ii_log        = ii_log
        ii_obj_filter = ii_obj_filter ).
    ENDIF.

    IF lines( lt_local ) <= 2.
      " Less equal two means that we have only the .abapgit.xml and the package in
      " our local repository. In this case we have to update our local .abapgit.xml
      " from the remote one. Otherwise we get errors when e.g. the folder starting
      " folder is different.
      io_repo->find_remote_dot_abapgit( ).
    ENDIF.

    lt_remote = io_repo->get_files_remote( ii_obj_filter = ii_obj_filter
                                           iv_ignore_files = abap_true ).

    li_exit = Lcl_abapgit_exit=>get_instance( ).
    li_exit->pre_calculate_repo_status(
      EXPORTING
        is_repo_meta = io_repo->ms_data
      CHANGING
        ct_local  = lt_local
        ct_remote = lt_remote ).

    CREATE OBJECT lo_instance
      EXPORTING
        iv_root_package = io_repo->get_package( )
        io_dot          = io_repo->get_dot_abapgit( ).

    rt_results = lo_instance->calculate_status(
      it_local     = lt_local
      it_remote    = lt_remote
      it_cur_state = io_repo->Lif_abapgit_repo~checksums( )->get_checksums_per_file( ) ).

    IF ii_log IS BOUND.
      " This method just adds messages to the log. No log, nothing to do here
      CREATE OBJECT lo_consistency_checks
        EXPORTING
          iv_root_package = io_repo->get_package( )
          io_dot          = io_repo->get_dot_abapgit( ).
      ii_log->merge_with( lo_consistency_checks->run_checks( rt_results ) ).
    ENDIF.

  ENDMETHOD.
  METHOD calculate_status.

    DATA:
      lt_remote        LIKE it_remote,
      lt_items         TYPE Lif_abapgit_definitions=>ty_items_tt,
      lt_items_by_obj  TYPE Lif_abapgit_definitions=>ty_items_ts, " Sorted by obj_type+obj_name
      lt_state_by_file TYPE Lif_abapgit_git_definitions=>ty_file_signatures_ts. " Sorted by path+filename

    lt_state_by_file = ensure_state( " Index by file
      it_cur_state = it_cur_state
      it_local     = it_local ).
    lt_remote        = it_remote.

    " Process local files and new local files
    process_local(
      EXPORTING
        it_local     = it_local
        it_state_idx = lt_state_by_file
      CHANGING
        ct_remote    = lt_remote
        ct_items     = lt_items
        ct_results   = rt_results ).

    " Remove processed remotes (with cleared SHA1)
    DELETE lt_remote WHERE sha1 IS INITIAL.

    " Complete item index for unmarked remote files
    process_items( " TODO: rename ?
      EXPORTING
        it_unprocessed_remote = lt_remote
      CHANGING
        ct_items              = lt_items ).

    " The item list was not unique by now, just collected as "mention" list
    SORT lt_items DESCENDING. " Default key - type, name, pkg, ...
    DELETE ADJACENT DUPLICATES FROM lt_items COMPARING obj_type obj_name.
    lt_items_by_obj = lt_items.

    " Process new remote files (marked above with empty SHA1)
    process_remote(
      EXPORTING
        it_local              = it_local
        it_unprocessed_remote = lt_remote
        it_state_idx          = lt_state_by_file
        it_items_idx          = lt_items_by_obj
      CHANGING
        ct_results            = rt_results ).

    SORT rt_results BY
      obj_type ASCENDING
      obj_name ASCENDING
      filename ASCENDING
      path ASCENDING.

  ENDMETHOD.
  METHOD check_local_remote_consistency.
    IF is_remote-sha1 IS INITIAL.
      IF is_local-file-filename = Lcl_abapgit_filename_logic=>c_package_file.
        Lcx_abapgit_exception=>raise(
          |Package name conflict { is_local-item-obj_type } { is_local-item-obj_name }. | &&
          |Rename package or use FULL folder logic| ).
      ELSE.
        Lcx_abapgit_exception=>raise(
          |Checksum conflict { is_local-item-obj_type } { is_local-item-obj_name }. | &&
          |Please create an issue on Github| ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD constructor.
    mv_root_package = iv_root_package.
    mo_dot          = io_dot.
  ENDMETHOD.
  METHOD ensure_state.

    FIELD-SYMBOLS <ls_state> LIKE LINE OF rt_state.
    FIELD-SYMBOLS <ls_local> LIKE LINE OF it_local.

    IF lines( it_cur_state ) = 0.
      " Empty state is usually not expected. Maybe for new repos.
      " In this case suppose the local state is unchanged
      LOOP AT it_local ASSIGNING <ls_local>.
        APPEND INITIAL LINE TO rt_state ASSIGNING <ls_state>.
        MOVE-CORRESPONDING <ls_local>-file TO <ls_state>.
      ENDLOOP.
    ELSE.
      rt_state = it_cur_state.
    ENDIF.

  ENDMETHOD.
  METHOD get_object_package.
    DATA: lv_name    TYPE devclass,
          li_package TYPE REF TO Lif_abapgit_sap_package.

    rv_devclass = Lcl_abapgit_factory=>get_tadir( )->get_object_package(
      iv_object   = iv_object
      iv_obj_name = iv_obj_name ).
    IF rv_devclass IS INITIAL AND iv_object = 'DEVC' AND iv_obj_name(1) = '$'.
      " local packages usually have no tadir entry
      lv_name = iv_obj_name.
      li_package = Lcl_abapgit_factory=>get_sap_package( lv_name ).
      IF li_package->exists( ) = abap_true.
        rv_devclass = lv_name.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD process_items.

    DATA:
      ls_item         LIKE LINE OF ct_items,
      lv_is_xml       TYPE abap_bool,
      lv_is_json      TYPE abap_bool,
      lv_sub_fetched  TYPE abap_bool,
      lt_sub_packages TYPE SORTED TABLE OF devclass WITH UNIQUE KEY table_line.

    FIELD-SYMBOLS <ls_remote> LIKE LINE OF it_unprocessed_remote.

    LOOP AT it_unprocessed_remote ASSIGNING <ls_remote>.

      Lcl_abapgit_filename_logic=>file_to_object(
        EXPORTING
          iv_filename = <ls_remote>-filename
          iv_path     = <ls_remote>-path
          io_dot      = mo_dot
          iv_devclass = mv_root_package
        IMPORTING
          es_item     = ls_item
          ev_is_xml   = lv_is_xml
          ev_is_json  = lv_is_json ).

      CHECK lv_is_xml = abap_true OR lv_is_json = abap_true. " only object definitions

      ls_item-devclass = get_object_package(
        iv_object   = ls_item-obj_type
        iv_obj_name = ls_item-obj_name ).

      IF ls_item-devclass IS NOT INITIAL AND mv_root_package <> ls_item-devclass.
        IF lv_sub_fetched = abap_false.
          lt_sub_packages = Lcl_abapgit_factory=>get_sap_package( mv_root_package )->list_subpackages( ).
          lv_sub_fetched  = abap_true.
        ENDIF.

        " Make sure the package is under the repo main package
        READ TABLE lt_sub_packages TRANSPORTING NO FIELDS
          WITH KEY table_line = ls_item-devclass.
        IF sy-subrc <> 0 AND ls_item-obj_type = 'DEVC'.
          CLEAR ls_item-devclass.
        ENDIF.
      ENDIF.

      APPEND ls_item TO ct_items.
    ENDLOOP.

  ENDMETHOD.
  METHOD process_local.

    FIELD-SYMBOLS:
      <ls_remote> LIKE LINE OF ct_remote,
      <ls_result> LIKE LINE OF ct_results,
      <ls_state>  LIKE LINE OF it_state_idx,
      <ls_local>  LIKE LINE OF it_local.

    LOOP AT it_local ASSIGNING <ls_local>.
      " Skip ignored files
      CHECK mo_dot->is_ignored(
        iv_path     = <ls_local>-file-path
        iv_filename = <ls_local>-file-filename ) = abap_false.

      IF <ls_local>-item IS NOT INITIAL
        AND Lcl_abapgit_filename_logic=>is_obj_definition_file( <ls_local>-file-filename ) = abap_true.
        " Collect for item index
        APPEND <ls_local>-item TO ct_items.
      ENDIF.

      APPEND INITIAL LINE TO ct_results ASSIGNING <ls_result>.

      " Find a match in remote
      READ TABLE ct_remote ASSIGNING <ls_remote>
        WITH KEY file_path
        COMPONENTS
          path     = <ls_local>-file-path
          filename = <ls_local>-file-filename.
      IF sy-subrc = 0.  " Both local and remote exist
        check_local_remote_consistency(
          is_local  = <ls_local>
          is_remote = <ls_remote> ).
        <ls_result> = build_existing(
          is_local  = <ls_local>
          is_remote = <ls_remote>
          it_state  = it_state_idx ).
        CLEAR <ls_remote>-sha1. " Mark as processed
      ELSE. " Only local exists
        <ls_result> = build_new_local( <ls_local> ).
        " Check if same file exists in different location
        READ TABLE ct_remote ASSIGNING <ls_remote>
          WITH KEY file
          COMPONENTS filename = <ls_local>-file-filename.
        IF sy-subrc = 0 AND <ls_local>-file-sha1 = <ls_remote>-sha1.
          " If yes, then it was probably moved
          <ls_result>-packmove = abap_true.
        ELSEIF sy-subrc = 4.
          " Check if file existed before and was deleted remotely
          READ TABLE it_state_idx ASSIGNING <ls_state>
            WITH KEY
              path     = <ls_local>-file-path
              filename = <ls_local>-file-filename.
          IF sy-subrc = 0.
            IF <ls_local>-file-sha1 = <ls_state>-sha1.
              <ls_result>-lstate = Lif_abapgit_definitions=>c_state-unchanged.
            ELSE.
              <ls_result>-lstate = Lif_abapgit_definitions=>c_state-modified.
            ENDIF.
            <ls_result>-rstate = Lif_abapgit_definitions=>c_state-deleted. " ??
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD process_remote.

    FIELD-SYMBOLS:
      <ls_remote> LIKE LINE OF it_unprocessed_remote,
      <ls_result> LIKE LINE OF ct_results,
      <ls_local>  LIKE LINE OF it_local.

    LOOP AT it_unprocessed_remote ASSIGNING <ls_remote>.

      APPEND INITIAL LINE TO ct_results ASSIGNING <ls_result>.

      <ls_result> = build_new_remote(
        is_remote   = <ls_remote>
        it_items_idx = it_items_idx
        it_state_idx = it_state_idx ).

      " Check if same file exists in different location (not for generic package files)
      READ TABLE it_local ASSIGNING <ls_local>
        WITH KEY file-filename = <ls_remote>-filename.
      IF sy-subrc = 0 AND <ls_remote>-filename <> Lcl_abapgit_filename_logic=>c_package_file.
        <ls_result>-match = abap_false.
        <ls_result>-lstate = Lif_abapgit_definitions=>c_state-deleted.
        <ls_result>-rstate = Lif_abapgit_definitions=>c_state-unchanged.
        IF <ls_local>-file-sha1 = <ls_remote>-sha1.
          <ls_result>-packmove = abap_true.
        ENDIF.
      ELSE.
        " Check if file existed before and was deleted locally
        READ TABLE it_state_idx TRANSPORTING NO FIELDS
          WITH KEY
            path     = <ls_remote>-path
            filename = <ls_remote>-filename.
        IF sy-subrc = 0.
          <ls_result>-match  = abap_false.
          <ls_result>-lstate = Lif_abapgit_definitions=>c_state-deleted.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_REPO_STATUS implementation

*>>>>>>> ZCL_ABAPGIT_SAP_PACKAGE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_sap_package=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_sap_package=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_SAP_PACKAGE implementation.
*"* method's implementations
*include methods.
  METHOD get_transport_layer.

    " Get default transport layer
    CALL FUNCTION 'TR_GET_TRANSPORT_TARGET'
      EXPORTING
        iv_use_default             = abap_true
        iv_get_layer_only          = abap_true
      IMPORTING
        ev_layer                   = rv_transport_layer
      EXCEPTIONS
        wrong_call                 = 1
        invalid_input              = 2
        cts_initialization_failure = 3
        OTHERS                     = 4.
    IF sy-subrc <> 0.
      " Return empty layer (i.e. "local workbench request" for the package)
      CLEAR rv_transport_layer.
    ENDIF.

  ENDMETHOD.
  METHOD constructor.
    mv_package = iv_package.
  ENDMETHOD.
  METHOD Lif_abapgit_sap_package~are_changes_recorded_in_tr_req.

    DATA: li_package TYPE REF TO if_package.

    cl_package_factory=>load_package(
      EXPORTING
        i_package_name             = mv_package
      IMPORTING
        e_package                  = li_package
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        no_access                  = 4
        object_locked_and_modified = 5
        OTHERS                     = 6 ).

    CASE sy-subrc.
      WHEN 0.
        rv_are_changes_rec_in_tr_req = li_package->wbo_korr_flag.
      WHEN 1.
        " For new packages, derive from package name
        rv_are_changes_rec_in_tr_req = boolc( mv_package(1) <> '$' AND mv_package(1) <> 'T' ).
      WHEN OTHERS.
        Lcx_abapgit_exception=>raise_t100( ).
    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_sap_package~create.

    DATA: lv_err     TYPE string,
          li_package TYPE REF TO if_package,
          ls_package LIKE is_package.


    ASSERT NOT is_package-devclass IS INITIAL.

    cl_package_factory=>load_package(
      EXPORTING
        i_package_name             = is_package-devclass
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        no_access                  = 4
        object_locked_and_modified = 5 ).
    IF sy-subrc = 0.
      " Package already exists. We assume this is fine. Its properties might be changed later at
      " DEVC deserialization.
      RETURN.
    ENDIF.

    ls_package = is_package.

    " Set software component to 'HOME' if none is set at this point.
    " Otherwise SOFTWARE_COMPONENT_INVALID will be raised.
    IF ls_package-dlvunit IS INITIAL.
      ls_package-dlvunit = 'HOME'.
    ENDIF.

    " For transportable packages, get default transport and layer
    IF ls_package-devclass(1) <> '$' AND ls_package-pdevclass IS INITIAL.
      ls_package-pdevclass = get_transport_layer( ).
    ENDIF.

    cl_package_factory=>create_new_package(
      EXPORTING
        i_reuse_deleted_object     = abap_true
*        i_suppress_dialog          = abap_true " does not exist in 730
      IMPORTING
        e_package                  = li_package
      CHANGING
        c_package_data             = ls_package
      EXCEPTIONS
        object_already_existing    = 1
        object_just_created        = 2
        not_authorized             = 3
        wrong_name_prefix          = 4
        undefined_name             = 5
        reserved_local_name        = 6
        invalid_package_name       = 7
        short_text_missing         = 8
        software_component_invalid = 9
        layer_invalid              = 10
        author_not_existing        = 11
        component_not_existing     = 12
        component_missing          = 13
        prefix_in_use              = 14
        unexpected_error           = 15
        intern_err                 = 16
        no_access                  = 17
*        invalid_translation_depth  = 18
*        wrong_mainpack_value       = 19
*        superpackage_invalid       = 20
*        error_in_cts_checks        = 21
        OTHERS                     = 18 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    li_package->save(
*      EXPORTING
*        i_suppress_dialog     = abap_true    " Controls whether popups can be transmitted
      EXCEPTIONS
        object_invalid        = 1
        object_not_changeable = 2
        cancelled_in_corr     = 3
        permission_failure    = 4
        unexpected_error      = 5
        intern_err            = 6
        OTHERS                = 7 ).
    IF sy-subrc <> 0.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_err.

      " Here we have to delete the package,
      " otherwise it would remain in the memory
      " and cannot created again in this session.
      li_package->delete(
        EXCEPTIONS
          object_not_empty      = 1
          object_not_changeable = 2
          object_invalid        = 3
          intern_err            = 4
          OTHERS                = 5 ).

      Lcx_abapgit_exception=>raise( lv_err ).

    ENDIF.

    li_package->set_changeable( abap_false ).

  ENDMETHOD.
  METHOD Lif_abapgit_sap_package~create_child.

    DATA: li_parent TYPE REF TO if_package,
          ls_child  TYPE scompkdtln.


    cl_package_factory=>load_package(
      EXPORTING
        i_package_name             = mv_package
      IMPORTING
        e_package                  = li_parent
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        no_access                  = 4
        object_locked_and_modified = 5 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    ls_child-devclass  = iv_child.
    ls_child-dlvunit   = li_parent->software_component.
    ls_child-component = li_parent->application_component.
    ls_child-ctext     = iv_child.
    ls_child-parentcl  = mv_package.
    ls_child-pdevclass = li_parent->transport_layer.
    ls_child-as4user   = sy-uname.

    Lif_abapgit_sap_package~create( ls_child ).

  ENDMETHOD.
  METHOD Lif_abapgit_sap_package~create_local.

    DATA: ls_package TYPE scompkdtln.


    ls_package-devclass  = mv_package.
    ls_package-ctext     = mv_package.
    ls_package-parentcl  = '$TMP'.
    ls_package-dlvunit   = 'LOCAL'.
    ls_package-as4user   = sy-uname.

    Lif_abapgit_sap_package~create( ls_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_sap_package~exists.

    cl_package_factory=>load_package(
      EXPORTING
        i_package_name             = mv_package
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        no_access                  = 4
        object_locked_and_modified = 5 ).
    rv_bool = boolc( sy-subrc <> 1 ).

  ENDMETHOD.
  METHOD Lif_abapgit_sap_package~get_transport_type.

    DATA:
      lv_pkg_name TYPE e071-obj_name,
      lv_obj_name TYPE tadir-obj_name,
      lv_role     TYPE trnrole.

    lv_pkg_name = mv_package.
    lv_obj_name = mv_package.

    CALL FUNCTION 'TR_GET_REQUEST_TYPE'
      EXPORTING
        iv_pgmid          = 'R3TR'
        iv_object         = 'DEVC'
        iv_obj_name       = lv_pkg_name
      IMPORTING
        ev_request_type   = rs_transport_type-request
        ev_task_type      = rs_transport_type-task
      EXCEPTIONS
        no_request_needed = 1
        invalid_object    = 2
        system_error      = 3
        OTHERS            = 4.

    CASE sy-subrc.
      WHEN 0 OR 1.
        RETURN.
      WHEN 2.
        " For new packages, set to workbench request
        rs_transport_type-request = 'K'.

        CALL FUNCTION 'TR_GET_NAMESPACE_AND_ROLE'
          EXPORTING
            iv_pgmid                   = 'R3TR'
            iv_object                  = 'DEVC'
            iv_objname                 = lv_obj_name
          IMPORTING
            ev_role                    = lv_role
          EXCEPTIONS
            namespace_not_existing     = 1
            invalid_object             = 2
            namespace_not_determinable = 3
            OTHERS                     = 4.
        IF sy-subrc = 0 AND lv_role = 'C'.
          " Namespace with repair license requires repair task
          rs_transport_type-task = 'R'.
        ELSE.
          " Otherweise use correction task
          rs_transport_type-task = 'S'.
        ENDIF.
      WHEN OTHERS.
        Lcx_abapgit_exception=>raise_t100( ).
    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_sap_package~list_subpackages.

    DATA: lt_list     LIKE rt_list.

    SELECT devclass FROM tdevc
      INTO TABLE lt_list
      WHERE parentcl = mv_package
      ORDER BY PRIMARY KEY.               "#EC CI_SUBRC "#EC CI_GENBUFF

    rt_list = lt_list.
    WHILE lines( lt_list ) > 0.

      SELECT devclass FROM tdevc
        INTO TABLE lt_list
        FOR ALL ENTRIES IN lt_list
        WHERE parentcl = lt_list-table_line
        ORDER BY PRIMARY KEY.             "#EC CI_SUBRC "#EC CI_GENBUFF
      APPEND LINES OF lt_list TO rt_list.

    ENDWHILE.

  ENDMETHOD.
  METHOD Lif_abapgit_sap_package~list_superpackages.

    DATA: lt_list   LIKE rt_list,
          lv_parent TYPE tdevc-parentcl.


    APPEND mv_package TO rt_list.

    lv_parent = Lif_abapgit_sap_package~read_parent( ).

    IF sy-subrc = 0 AND NOT lv_parent IS INITIAL.
      lt_list = Lcl_abapgit_factory=>get_sap_package( lv_parent )->list_superpackages( ).
      APPEND LINES OF lt_list TO rt_list.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_sap_package~read_description.

    SELECT SINGLE ctext FROM tdevct INTO rv_description
      WHERE devclass = mv_package AND spras = sy-langu ##SUBRC_OK.

  ENDMETHOD.
  METHOD Lif_abapgit_sap_package~read_parent.

    SELECT SINGLE parentcl FROM tdevc INTO rv_parentcl
      WHERE devclass = mv_package.                      "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Inconsistent package structure! Cannot find parent for { mv_package }| ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_sap_package~read_responsible.
    SELECT SINGLE as4user FROM tdevc
      INTO rv_responsible
      WHERE devclass = mv_package ##SUBRC_OK.           "#EC CI_GENBUFF
  ENDMETHOD.
  METHOD Lif_abapgit_sap_package~validate_name.

    IF mv_package IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Package name must not be empty' ).
    ENDIF.

    IF mv_package = '$TMP'.
      Lcx_abapgit_exception=>raise( 'It is not possible to use $TMP, use a different (local) package' ).
    ENDIF.

    " Check if package name is allowed
    cl_package_helper=>check_package_name(
      EXPORTING
        i_package_name       = mv_package
      EXCEPTIONS
        undefined_name       = 1
        wrong_name_prefix    = 2
        reserved_local_name  = 3
        invalid_package_name = 4 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Package name { mv_package } is not valid| ).
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SAP_PACKAGE implementation

*>>>>>>> ZCL_ABAPGIT_SERIALIZE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_serialize=========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_serialize=========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_serialize=========ccau.
*CLASS SHRITEFUH64VYIPO5IWUYCIJCM4SFY DEFINITION DEFERRED.
*CLASS zcl_abapgit_serialize DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYCIJCM4SFY.

























class LCL_ABAPGIT_SERIALIZE implementation.
*"* method's implementations
*include methods.
  METHOD is_parallelization_possible.

    rv_result = boolc( Lcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_false
                   AND Lcl_abapgit_persist_factory=>get_settings( )->read( )->get_parallel_proc_disabled( ) = abap_false
                   AND mv_group IS NOT INITIAL
                   " The function module below should always exist here as is_merged evaluated to false above.
                   " It does however not exist in the transpiled version which then causes unit tests to fail.
                   " Therefore the check needs to stay.
                   AND Lcl_abapgit_factory=>get_function_module(
                                         )->function_exists( 'Z_ABAPGIT_SERIALIZE_PARALLEL' ) = abap_true ).

  ENDMETHOD.
  METHOD add_apack.

    DATA ls_apack_file TYPE Lif_abapgit_git_definitions=>ty_file.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF ct_files.


    ls_apack_file = Lcl_abapgit_apack_helper=>to_file( iv_package ).
    IF ls_apack_file IS NOT INITIAL.
      APPEND INITIAL LINE TO ct_files ASSIGNING <ls_file>.
      <ls_file>-file = ls_apack_file.
    ENDIF.

  ENDMETHOD.
  METHOD add_data.

    DATA lt_files TYPE Lif_abapgit_git_definitions=>ty_files_tt.
    DATA ls_file LIKE LINE OF lt_files.

    FIELD-SYMBOLS <ls_return> LIKE LINE OF ct_files.

    IF ii_data_config IS INITIAL.
      RETURN.
    ENDIF.

    lt_files = ii_data_config->to_json( ).
    LOOP AT lt_files INTO ls_file.
      APPEND INITIAL LINE TO ct_files ASSIGNING <ls_return>.
      <ls_return>-file = ls_file.

      " Derive object from config filename (namespace + escaping)
      Lcl_abapgit_filename_logic=>file_to_object(
        EXPORTING
          iv_filename = <ls_return>-file-filename
          iv_path     = <ls_return>-file-path
          io_dot      = mo_dot_abapgit
        IMPORTING
          es_item     = <ls_return>-item ).

      <ls_return>-item-obj_type = Lif_abapgit_data_config=>c_data_type-tabu. " todo
    ENDLOOP.

    lt_files = Lcl_abapgit_data_factory=>get_serializer( )->serialize( ii_data_config ).
    LOOP AT lt_files INTO ls_file.
      APPEND INITIAL LINE TO ct_files ASSIGNING <ls_return>.
      <ls_return>-file = ls_file.

      " Derive object from data filename (namespace + escaping)
      Lcl_abapgit_filename_logic=>file_to_object(
        EXPORTING
          iv_filename = <ls_return>-file-filename
          iv_path     = <ls_return>-file-path
          io_dot      = mo_dot_abapgit
        IMPORTING
          es_item     = <ls_return>-item ).
    ENDLOOP.

  ENDMETHOD.
  METHOD add_dot_abapgit.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF ct_files.

    APPEND INITIAL LINE TO ct_files ASSIGNING <ls_file>.
    <ls_file>-file = mo_dot_abapgit->to_file( ).

  ENDMETHOD.
  METHOD add_objects.

    DATA: lo_filter TYPE REF TO Lcl_abapgit_repo_filter,
          lv_force  TYPE abap_bool,
          lt_found  LIKE ct_files,
          lt_tadir  TYPE Lif_abapgit_definitions=>ty_tadir_tt.

    lt_tadir = Lcl_abapgit_factory=>get_tadir( )->read(
      iv_package            = iv_package
      iv_ignore_subpackages = ms_local_settings-ignore_subpackages
      iv_only_local_objects = ms_local_settings-only_local_objects
      io_dot                = mo_dot_abapgit
      ii_log                = ii_log
      it_filter             = it_filter ).

    CREATE OBJECT lo_filter.

    lo_filter->apply( EXPORTING it_filter = it_filter
                      CHANGING  ct_tadir  = lt_tadir ).

* if there are less than 10 objects run in single thread
* this helps a lot when debugging, plus performance gain
* with low number of objects does not matter much
    lv_force = boolc( lines( lt_tadir ) < 10 ).

    lt_found = serialize(
      iv_package          = iv_package
      it_tadir            = lt_tadir
      ii_log              = ii_log
      iv_force_sequential = lv_force ).
    APPEND LINES OF lt_found TO ct_files.

  ENDMETHOD.
  METHOD add_to_return.

    FIELD-SYMBOLS: <ls_file>   LIKE LINE OF is_file_item-files,
                   <ls_return> LIKE LINE OF mt_files.


    LOOP AT is_file_item-files ASSIGNING <ls_file>.
      APPEND INITIAL LINE TO mt_files ASSIGNING <ls_return>.
      <ls_return>-file = <ls_file>.
      <ls_return>-file-path = iv_path.
      <ls_return>-item = is_file_item-item.
    ENDLOOP.

  ENDMETHOD.
  METHOD constructor.

    DATA li_exit TYPE REF TO Lif_abapgit_exit.

    mv_group = 'parallel_generators'.
    li_exit = Lcl_abapgit_exit=>get_instance( ).
    li_exit->change_rfc_server_group( CHANGING cv_group = mv_group ).

    mo_dot_abapgit = io_dot_abapgit.
    ms_local_settings = is_local_settings.

    ms_i18n_params = determine_i18n_params(
      io_dot = io_dot_abapgit
      iv_main_language_only = is_local_settings-main_language_only ).

    CREATE OBJECT mo_abap_language_version
      EXPORTING
        io_dot_abapgit = mo_dot_abapgit.

  ENDMETHOD.
  METHOD files_local.

* serializes objects, including .abapgit.xml, apack, and takes into account local settings

    add_dot_abapgit( CHANGING ct_files = rt_files ).

    add_apack(
      EXPORTING
        iv_package = iv_package
      CHANGING
        ct_files   = rt_files ).

    add_data(
      EXPORTING
        ii_data_config = ii_data_config
      CHANGING
        ct_files       = rt_files ).

    add_objects(
      EXPORTING
        iv_package = iv_package
        ii_log     = ii_log
        it_filter  = it_filter
      CHANGING
        ct_files   = rt_files ).

  ENDMETHOD.
  METHOD filter_ignored_objects.

    DATA:
      ls_ignored_count TYPE ty_unsupported_count,
      lt_ignored_count TYPE ty_unsupported_count_tt,
      lo_folder_logic  TYPE REF TO Lcl_abapgit_folder_logic,
      ls_item          TYPE Lif_abapgit_definitions=>ty_item,
      lv_path          TYPE string,
      lv_filename      TYPE string.

    FIELD-SYMBOLS:
      <ls_tadir>         LIKE LINE OF ct_tadir,
      <ls_ignored_count> TYPE ty_unsupported_count.

    " Ignore logic requires .abapGit.xml
    IF mo_dot_abapgit IS INITIAL OR iv_package IS INITIAL OR mi_log IS INITIAL.
      RETURN.
    ENDIF.

    lo_folder_logic = Lcl_abapgit_folder_logic=>get_instance( ).

    LOOP AT ct_tadir ASSIGNING <ls_tadir>.
      CLEAR: ls_ignored_count.

      ls_item-obj_type = <ls_tadir>-object.
      ls_item-obj_name = <ls_tadir>-obj_name.

      IF <ls_tadir>-devclass IS NOT INITIAL.
        lv_path = lo_folder_logic->package_to_path(
          iv_top     = iv_package
          io_dot     = mo_dot_abapgit
          iv_package = <ls_tadir>-devclass ).
      ELSE.
        lv_path = mo_dot_abapgit->get_starting_folder( ).
      ENDIF.

      lv_filename = Lcl_abapgit_filename_logic=>object_to_file(
        is_item  = ls_item
        iv_ext   = '*' ).

      IF mo_dot_abapgit->is_ignored(
        iv_path     = lv_path
        iv_filename = lv_filename ) = abap_false.
        CONTINUE.
      ENDIF.

      READ TABLE lt_ignored_count ASSIGNING <ls_ignored_count> WITH TABLE KEY obj_type = <ls_tadir>-object.
      IF sy-subrc <> 0.
        ls_ignored_count-obj_type = <ls_tadir>-object.
        ls_ignored_count-count    = 1.
        ls_ignored_count-obj_name = <ls_tadir>-obj_name.
        INSERT ls_ignored_count INTO TABLE lt_ignored_count ASSIGNING <ls_ignored_count>.
      ELSE.
        CLEAR: <ls_ignored_count>-obj_name.
        <ls_ignored_count>-count = <ls_ignored_count>-count + 1.
      ENDIF.
      " init object so we can remove these entries afterward
      CLEAR <ls_tadir>-object.
    ENDLOOP.
    IF lt_ignored_count IS INITIAL.
      RETURN.
    ENDIF.

    " remove ignored objects
    DELETE ct_tadir WHERE object IS INITIAL.

    LOOP AT lt_ignored_count ASSIGNING <ls_ignored_count>.
      IF <ls_ignored_count>-count = 1.
        mi_log->add_warning( |Object { <ls_ignored_count>-obj_type } { <ls_ignored_count>-obj_name } ignored| ).
      ELSE.
        mi_log->add_warning( |Object type { <ls_ignored_count>-obj_type } with | &&
                             |{ <ls_ignored_count>-count } objects ignored| ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD filter_unsupported_objects.

    DATA: ls_unsupported_count TYPE ty_unsupported_count,
          lt_supported_types   TYPE Lcl_abapgit_objects=>ty_types_tt,
          lt_unsupported_count TYPE ty_unsupported_count_tt.

    FIELD-SYMBOLS: <ls_tadir>             LIKE LINE OF ct_tadir,
                   <ls_unsupported_count> TYPE ty_unsupported_count.

    lt_supported_types = Lcl_abapgit_objects=>supported_list( ).
    LOOP AT ct_tadir ASSIGNING <ls_tadir>.
      CLEAR: ls_unsupported_count.
      READ TABLE lt_supported_types WITH KEY table_line = <ls_tadir>-object TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      READ TABLE lt_unsupported_count ASSIGNING <ls_unsupported_count>
                                      WITH TABLE KEY obj_type = <ls_tadir>-object.
      IF sy-subrc <> 0.
        ls_unsupported_count-obj_type = <ls_tadir>-object.
        ls_unsupported_count-count    = 1.
        ls_unsupported_count-obj_name = <ls_tadir>-obj_name.
        INSERT ls_unsupported_count INTO TABLE lt_unsupported_count ASSIGNING <ls_unsupported_count>.
      ELSE.
        CLEAR: <ls_unsupported_count>-obj_name.
        <ls_unsupported_count>-count = <ls_unsupported_count>-count + 1.
      ENDIF.
      CLEAR: <ls_tadir>-object.
    ENDLOOP.
    IF lt_unsupported_count IS INITIAL.
      RETURN.
    ENDIF.

    DELETE ct_tadir WHERE object IS INITIAL.
    IF mi_log IS BOUND.
      LOOP AT lt_unsupported_count ASSIGNING <ls_unsupported_count>.
        IF <ls_unsupported_count>-count = 1.
          mi_log->add_error( |Object type { <ls_unsupported_count>-obj_type } not supported, | &&
                             |{ <ls_unsupported_count>-obj_name } ignored| ).
        ELSE.
          mi_log->add_error( |Object type { <ls_unsupported_count>-obj_type } not supported, | &&
                             |{ <ls_unsupported_count>-count } objects ignored| ).
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD on_end_of_task.

* this method will be called from the parallel processing, thus it must be public

    DATA: lv_result    TYPE xstring,
          lv_path      TYPE string,
          lv_mess      TYPE c LENGTH 200,
          ls_file_item TYPE Lif_abapgit_objects=>ty_serialization.


    RECEIVE RESULTS FROM FUNCTION 'Z_ABAPGIT_SERIALIZE_PARALLEL'
      IMPORTING
        ev_result             = lv_result
        ev_path               = lv_path
      EXCEPTIONS
        error                 = 1
        system_failure        = 2 MESSAGE lv_mess
        communication_failure = 3 MESSAGE lv_mess
        OTHERS = 4.
    IF sy-subrc <> 0.
      IF NOT mi_log IS INITIAL.
        IF NOT lv_mess IS INITIAL.
          mi_log->add_error( lv_mess ).
        ELSE.
          mi_log->add_error( |{ sy-msgv1 }{ sy-msgv2 }{ sy-msgv3 }{ sy-msgv3 }| ).
        ENDIF.
      ENDIF.
    ELSE.
      IMPORT data = ls_file_item FROM DATA BUFFER lv_result. "#EC CI_SUBRC
      ASSERT sy-subrc = 0.
      add_to_return( is_file_item = ls_file_item
                     iv_path      = lv_path ).
    ENDIF.

    mv_free = mv_free + 1.

  ENDMETHOD.
  METHOD run_parallel.

    DATA: lv_msg  TYPE c LENGTH 100,
          lv_task TYPE c LENGTH 32,
          lv_free LIKE mv_free.
    DATA lv_abap_language_version TYPE Lif_abapgit_aff_types_v1=>ty_abap_language_version.

    ASSERT mv_free > 0.

    lv_abap_language_version = mo_abap_language_version->get_repo_abap_language_version( ).

    DO.
      lv_task = |{ iv_task }-{ sy-index }|.
      CALL FUNCTION 'Z_ABAPGIT_SERIALIZE_PARALLEL'
        STARTING NEW TASK lv_task
        DESTINATION IN GROUP mv_group
        CALLING on_end_of_task ON END OF TASK
        EXPORTING
          iv_obj_type           = is_tadir-object
          iv_obj_name           = is_tadir-obj_name
          iv_devclass           = is_tadir-devclass
          iv_path               = is_tadir-path
          iv_srcsystem          = is_tadir-srcsystem
          iv_abap_language_vers = lv_abap_language_version
          iv_language           = ms_i18n_params-main_language
          iv_main_language_only = ms_i18n_params-main_language_only
          it_translation_langs  = ms_i18n_params-translation_languages
          iv_use_lxe            = ms_i18n_params-use_lxe
        EXCEPTIONS
          system_failure        = 1 MESSAGE lv_msg
          communication_failure = 2 MESSAGE lv_msg
          resource_failure      = 3
          OTHERS                = 4.
      IF sy-subrc = 3.
        lv_free = mv_free.
        WAIT UNTIL mv_free <> lv_free UP TO 1 SECONDS.
        CONTINUE.
      ELSEIF sy-subrc <> 0.
        ASSERT lv_msg = '' AND 0 = 1.
      ENDIF.
      EXIT.
    ENDDO.

    mv_free = mv_free - 1.

  ENDMETHOD.
  METHOD run_sequential.

    DATA: lx_error     TYPE REF TO Lcx_abapgit_exception,
          ls_file_item TYPE Lif_abapgit_objects=>ty_serialization.

    ls_file_item-item-obj_type  = is_tadir-object.
    ls_file_item-item-obj_name  = is_tadir-obj_name.
    ls_file_item-item-devclass  = is_tadir-devclass.
    ls_file_item-item-srcsystem = is_tadir-srcsystem.
    ls_file_item-item-abap_language_version = mo_abap_language_version->get_repo_abap_language_version( ).

    TRY.
        ls_file_item = Lcl_abapgit_objects=>serialize(
          is_item        = ls_file_item-item
          io_i18n_params = Lcl_abapgit_i18n_params=>new( is_params = ms_i18n_params ) ).

        add_to_return( is_file_item = ls_file_item
                       iv_path      = is_tadir-path ).
      CATCH Lcx_abapgit_exception INTO lx_error.
        IF NOT mi_log IS INITIAL.
          mi_log->add_exception(
              ix_exc  = lx_error
              is_item = ls_file_item-item ).
        ENDIF.
    ENDTRY.

  ENDMETHOD.
  METHOD serialize.

* serializes only objects

    DATA: lv_max      TYPE i,
          lv_count    TYPE i,
          li_progress TYPE REF TO Lif_abapgit_progress,
          li_exit     TYPE REF TO Lif_abapgit_exit,
          lo_timer    TYPE REF TO Lcl_abapgit_timer,
          lt_tadir    TYPE Lif_abapgit_definitions=>ty_tadir_tt.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF it_tadir.


    CLEAR mt_files.

    lv_max = determine_max_processes( iv_force_sequential = iv_force_sequential
                                      iv_package          = iv_package ).
    mv_free = lv_max.
    mi_log = ii_log.

    lt_tadir = it_tadir.
    filter_unsupported_objects( CHANGING ct_tadir = lt_tadir ).

    filter_ignored_objects(
      EXPORTING
        iv_package = iv_package
      CHANGING
        ct_tadir   = lt_tadir ).

    lv_count = lines( lt_tadir ).

    li_progress = Lcl_abapgit_progress=>get_instance( lv_count ).

    lo_timer = Lcl_abapgit_timer=>create(
      iv_text  = 'Serialize:'
      iv_count = lv_count )->start( ).

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.

      li_progress->show(
        iv_current = sy-tabix
        iv_text    = |Serialize { <ls_tadir>-obj_name }, { lv_max } threads| ).

      IF lv_max = 1.
        run_sequential( <ls_tadir> ).
      ELSE.
        run_parallel(
          is_tadir = <ls_tadir>
          iv_task  = |{ sy-tabix }| ).
        WAIT UNTIL mv_free > 0 UP TO 120 SECONDS.
      ENDIF.
    ENDLOOP.

    li_progress->off( ).

    WAIT UNTIL mv_free = lv_max UP TO 120 SECONDS.
    rt_files = mt_files.
    FREE mt_files.

*   Call postprocessing
    li_exit = Lcl_abapgit_exit=>get_instance( ).

    li_exit->serialize_postprocess(
      EXPORTING
        iv_package = iv_package
        ii_log     = ii_log
      CHANGING
        ct_files   = rt_files ).

    lo_timer->end( abap_true ).

  ENDMETHOD.
  METHOD determine_i18n_params.

    " TODO: unify with ZCL_ABAPGIT_OBJECTS=>DETERMINE_I18N_PARAMS, same code

    IF io_dot IS BOUND.
      rs_i18n_params-main_language         = io_dot->get_main_language( ).
      rs_i18n_params-use_lxe               = io_dot->use_lxe( ).
      rs_i18n_params-main_language_only    = iv_main_language_only.
      rs_i18n_params-translation_languages = Lcl_abapgit_lxe_texts=>get_translation_languages(
        iv_main_language  = io_dot->get_main_language( )
        it_i18n_languages = io_dot->get_i18n_languages( ) ).
    ENDIF.

    IF rs_i18n_params-main_language IS INITIAL.
      rs_i18n_params-main_language = sy-langu.
    ENDIF.

  ENDMETHOD.
  METHOD determine_max_processes.
    DATA: li_exit TYPE REF TO Lif_abapgit_exit.

    IF iv_force_sequential = abap_true.
      rv_processes = 1.
      RETURN.
    ENDIF.

    IF gv_max_processes IS INITIAL AND is_parallelization_possible( ) = abap_true.

      gv_max_processes = Lcl_abapgit_factory=>get_environment( )->init_parallel_processing( mv_group ).

      IF gv_max_processes > 1.
        gv_max_processes = gv_max_processes - 1.
      ENDIF.

      IF gv_max_processes > 32.
        " https://en.wikipedia.org/wiki/Amdahl%27s_law
        gv_max_processes = 32.
      ENDIF.

    ENDIF.

    IF gv_max_processes IS INITIAL.
      " fallback to running sequentially.
      gv_max_processes = 1.
    ENDIF.

    rv_processes = gv_max_processes.

    ASSERT rv_processes >= 1.

    li_exit = Lcl_abapgit_exit=>get_instance( ).
    li_exit->change_max_parallel_processes(
      EXPORTING
        iv_package       = iv_package
      CHANGING
        cv_max_processes = rv_processes ).

    ASSERT rv_processes >= 1. " check exit above

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SERIALIZE implementation

*>>>>>>> ZCL_ABAPGIT_SERVICES_ABAPGIT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_services_abapgit==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_services_abapgit==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_SERVICES_ABAPGIT implementation.
*"* method's implementations
*include methods.
  METHOD check_sapgui.

    CONSTANTS:
      lc_hide_sapgui_hint TYPE string VALUE '2'.

    DATA:
      lv_answer           TYPE char1,
      ls_settings         TYPE Lif_abapgit_definitions=>ty_s_user_settings,
      li_user_persistence TYPE REF TO Lif_abapgit_persist_user.

    li_user_persistence = Lcl_abapgit_persistence_user=>get_instance( ).

    ls_settings = li_user_persistence->get_settings( ).

    IF ls_settings-hide_sapgui_hint = abap_true.
      RETURN.
    ENDIF.

    IF Lcl_abapgit_ui_factory=>get_frontend_services( )->is_sapgui_for_java( ) = abap_false.
      RETURN.
    ENDIF.

    lv_answer = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
                    iv_titlebar              = 'Not supported SAPGUI'
                    iv_text_question         = 'SAPGUI for Java is not supported! There might be some issues.'
                    iv_text_button_1         = 'Got it'
                    iv_icon_button_1         = |{ icon_okay }|
                    iv_text_button_2         = 'Hide'
                    iv_icon_button_2         = |{ icon_set_state }|
                    iv_display_cancel_button = abap_false ).

    IF lv_answer = lc_hide_sapgui_hint.
      ls_settings-hide_sapgui_hint = abap_true.
      li_user_persistence->set_settings( ls_settings ).
    ENDIF.

  ENDMETHOD.
  METHOD get_abapgit_tcode.
    CONSTANTS: lc_report_tcode_hex TYPE x VALUE '80'.
    DATA: lt_tcodes TYPE STANDARD TABLE OF tcode.

    SELECT tcode
      FROM tstc
      INTO TABLE lt_tcodes
      WHERE pgmna = sy-cprog
        AND cinfo = lc_report_tcode_hex
      ORDER BY tcode.

    IF lines( lt_tcodes ) > 0.
      READ TABLE lt_tcodes INDEX 1 INTO rv_tcode.
    ENDIF.
  ENDMETHOD.
  METHOD get_package_from_adt.

    DATA: ls_item    TYPE Lif_abapgit_definitions=>ty_item,
          lr_context TYPE REF TO data,
          lt_fields  TYPE tihttpnvp.


    FIELD-SYMBOLS: <lg_context>    TYPE any,
                   <lv_parameters> TYPE string,
                   <ls_field>      LIKE LINE OF lt_fields.

    ls_item-obj_type = 'CLAS'.
    ls_item-obj_name = 'CL_ADT_GUI_INTEGRATION_CONTEXT'.

    IF Lcl_abapgit_objects=>exists( ls_item ) = abap_false.
      " ADT is not supported in this NW release
      RETURN.
    ENDIF.

    TRY.
        CREATE DATA lr_context TYPE ('CL_ADT_GUI_INTEGRATION_CONTEXT=>TY_CONTEXT_INFO').

        ASSIGN lr_context->* TO <lg_context>.
        ASSERT sy-subrc = 0.

        CALL METHOD ('CL_ADT_GUI_INTEGRATION_CONTEXT')=>read_context
          RECEIVING
            result = <lg_context>.

        ASSIGN COMPONENT 'PARAMETERS'
               OF STRUCTURE <lg_context>
               TO <lv_parameters>.
        ASSERT sy-subrc = 0.

        lt_fields = cl_http_utility=>string_to_fields( cl_http_utility=>unescape_url( <lv_parameters> ) ).

        READ TABLE lt_fields ASSIGNING <ls_field>
                             WITH KEY name = 'p_package_name'.
        IF sy-subrc = 0.
          rv_package = <ls_field>-value.

          " We want to open the repo just once. Therefore we delete the parameters
          " and initialize the ADT context.
          CLEAR <lv_parameters>.
          CALL METHOD ('CL_ADT_GUI_INTEGRATION_CONTEXT')=>initialize_instance
            EXPORTING
              context_info = <lg_context>.

        ENDIF.

      CATCH cx_root.
        " Some problems with dynamic ADT access.
        " Let's ignore it for now and fail silently
    ENDTRY.

  ENDMETHOD.
  METHOD is_installed.

    SELECT SINGLE devclass FROM tadir INTO rv_devclass
      WHERE pgmid = 'R3TR'
      AND object = 'CLAS'
      AND obj_name = c_abapgit_class.

  ENDMETHOD.
  METHOD open_abapgit_changelog.
    open_url_in_browser( |{ c_abapgit_repo }{ c_changelog_path }| ).
  ENDMETHOD.
  METHOD open_abapgit_homepage.
    open_url_in_browser( |{ c_abapgit_homepage }/{ iv_page }| ).
  ENDMETHOD.
  METHOD open_abapgit_wikipage.
    open_url_in_browser( |{ c_abapgit_wikipage }/{ iv_page }| ).
  ENDMETHOD.
  METHOD open_dotabap_homepage.
    open_url_in_browser( c_dotabap_homepage ).
  ENDMETHOD.
  METHOD open_url_in_browser.
    DATA lx_error TYPE REF TO Lcx_abapgit_exception.

    TRY.
        Lcl_abapgit_ui_factory=>get_frontend_services( )->execute( iv_document = iv_url ).
      CATCH Lcx_abapgit_exception INTO lx_error.
        Lcx_abapgit_exception=>raise( iv_text     = 'Opening page in external browser failed.'
                                      ix_previous = lx_error ).
    ENDTRY.
  ENDMETHOD.
  METHOD prepare_gui_startup.

    DATA: lv_repo_key    TYPE Lif_abapgit_persistence=>ty_value,
          lv_package     TYPE devclass,
          lv_package_adt TYPE devclass.

    check_sapgui( ).

    IF Lcl_abapgit_persist_factory=>get_settings( )->read( )->get_show_default_repo( ) = abap_false.
      " Don't show the last seen repo at startup
      Lcl_abapgit_persistence_user=>get_instance( )->set_repo_show( || ).
    ENDIF.

    " We have three special cases for gui startup
    "   - open a specific repo by repo key
    "   - open a specific repo by package name
    "   - open a specific repo by package name provided by ADT
    " These overrule the last shown repo

    GET PARAMETER ID Lif_abapgit_definitions=>c_spagpa_param_repo_key FIELD lv_repo_key.
    GET PARAMETER ID Lif_abapgit_definitions=>c_spagpa_param_package  FIELD lv_package.
    lv_package_adt = get_package_from_adt( ).

    IF lv_repo_key IS NOT INITIAL.

      SET PARAMETER ID Lif_abapgit_definitions=>c_spagpa_param_repo_key FIELD ''.
      Lcl_abapgit_persistence_user=>get_instance( )->set_repo_show( lv_repo_key ).

    ELSEIF lv_package IS NOT INITIAL.

      SET PARAMETER ID Lif_abapgit_definitions=>c_spagpa_param_package FIELD ''.
      set_start_repo_from_package( lv_package ).

    ELSEIF lv_package_adt IS NOT INITIAL.

      set_start_repo_from_package( lv_package_adt ).

    ENDIF.

  ENDMETHOD.
  METHOD set_start_repo_from_package.

    DATA: lo_repo          TYPE REF TO Lcl_abapgit_repo,
          lt_r_package     TYPE RANGE OF devclass,
          ls_r_package     LIKE LINE OF lt_r_package,
          lt_superpackages TYPE Lif_abapgit_sap_package=>ty_devclass_tt,
          li_package       TYPE REF TO Lif_abapgit_sap_package,
          lt_repo_list     TYPE Lif_abapgit_repo_srv=>ty_repo_list.

    FIELD-SYMBOLS: <lo_repo>         TYPE LINE OF Lif_abapgit_repo_srv=>ty_repo_list,
                   <lv_superpackage> LIKE LINE OF lt_superpackages.

    li_package = Lcl_abapgit_factory=>get_sap_package( iv_package ).

    IF li_package->exists( ) = abap_false.
      RETURN.
    ENDIF.

    ls_r_package-sign   = 'I'.
    ls_r_package-option = 'EQ'.
    ls_r_package-low    = iv_package.
    INSERT ls_r_package INTO TABLE lt_r_package.

    " Also consider superpackages. E.g. when some open $abapgit_ui, abapGit repo
    " should be found via package $abapgit
    lt_superpackages = li_package->list_superpackages( ).
    LOOP AT lt_superpackages ASSIGNING <lv_superpackage>.
      ls_r_package-low = <lv_superpackage>.
      INSERT ls_r_package INTO TABLE lt_r_package.
    ENDLOOP.

    lt_repo_list = Lcl_abapgit_repo_srv=>get_instance( )->list( ).

    LOOP AT lt_repo_list ASSIGNING <lo_repo>.

      IF <lo_repo>->get_package( ) IN lt_r_package.
        lo_repo ?= <lo_repo>.
        EXIT.
      ENDIF.

    ENDLOOP.

    IF lo_repo IS BOUND.
      Lcl_abapgit_persistence_user=>get_instance( )->set_repo_show( lo_repo->get_key( ) ).
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SERVICES_ABAPGIT implementation

*>>>>>>> ZCL_ABAPGIT_SERVICES_REPO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_services_repo=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_services_repo=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_services_repo=====ccau.











class LCL_ABAPGIT_SERVICES_REPO implementation.
*"* method's implementations
*include methods.
  METHOD check_package_exists.

    IF Lcl_abapgit_factory=>get_sap_package( iv_package )->exists( ) = abap_false.
      " Check if any package is included in remote
      READ TABLE it_remote TRANSPORTING NO FIELDS
        WITH KEY file
        COMPONENTS filename = Lcl_abapgit_filename_logic=>c_package_file.
      IF sy-subrc <> 0.
        " If not, give error
        Lcx_abapgit_exception=>raise(
          iv_text     = |Package { iv_package } does not exist and there's no package included in the repository|
          iv_longtext = 'Either select an existing package, create a new one, or add a package to the repository' ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD popup_objects_overwrite.

    DATA: lt_columns  TYPE Lif_abapgit_popups=>ty_alv_column_tt,
          lt_selected LIKE ct_overwrite,
          li_popups   TYPE REF TO Lif_abapgit_popups.
    DATA lt_preselected_rows TYPE Lif_abapgit_popups=>ty_rows.

    FIELD-SYMBOLS: <ls_overwrite> LIKE LINE OF ct_overwrite,
                   <ls_column>    TYPE Lif_abapgit_popups=>ty_alv_column.


    IF lines( ct_overwrite ) = 0.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'OBJ_TYPE'.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'OBJ_NAME'.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'DEVCLASS'.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'STATE'.
    <ls_column>-text = 'State'.
    <ls_column>-length = 3.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'ICON'.
    <ls_column>-text = 'Action'.
    <ls_column>-show_icon = abap_true.
    <ls_column>-length = 5.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'TEXT'.
    <ls_column>-text = 'Description'.

    LOOP AT ct_overwrite ASSIGNING <ls_overwrite> WHERE decision = Lif_abapgit_definitions=>c_yes.
      INSERT sy-tabix INTO TABLE lt_preselected_rows.
    ENDLOOP.

    li_popups = Lcl_abapgit_ui_factory=>get_popups( ).
    li_popups->popup_to_select_from_list(
      EXPORTING
        it_list               = ct_overwrite
        iv_header_text        = |The following objects are different between local and remote repository.|
                             && | Select the objects which should be brought in line with the remote version.|
        iv_select_column_text = 'Change?'
        it_columns_to_display = lt_columns
        it_preselected_rows   = lt_preselected_rows
      IMPORTING
        et_list               = lt_selected ).

    LOOP AT ct_overwrite ASSIGNING <ls_overwrite>.
      READ TABLE lt_selected WITH TABLE KEY object_type_and_name
                             COMPONENTS obj_type = <ls_overwrite>-obj_type
                                        obj_name = <ls_overwrite>-obj_name
                             TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        <ls_overwrite>-decision = Lif_abapgit_definitions=>c_yes.
      ELSE.
        <ls_overwrite>-decision = Lif_abapgit_definitions=>c_no.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD activate_objects.

    DATA:
      lo_repo       TYPE REF TO Lcl_abapgit_repo,
      lo_browser    TYPE REF TO Lcl_abapgit_repo_content_list,
      lt_repo_items TYPE Lif_abapgit_definitions=>ty_repo_item_tt,
      lv_count      TYPE i,
      lv_message    TYPE string.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF lt_repo_items.

    lo_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    CREATE OBJECT lo_browser
      EXPORTING
        io_repo = lo_repo.

    lt_repo_items = lo_browser->list( '/' ).

    ri_log = lo_repo->create_new_log( 'Activation Log' ).

    " Add all inactive objects to activation queue
    Lcl_abapgit_objects_activation=>clear( ).

    LOOP AT lt_repo_items ASSIGNING <ls_item> WHERE inactive = abap_true.
      Lcl_abapgit_objects_activation=>add(
        iv_type = <ls_item>-obj_type
        iv_name = <ls_item>-obj_name ).
      lv_count = lv_count + 1.
    ENDLOOP.

    IF lv_count = 0.
      MESSAGE 'No inactive objects found' TYPE 'S'.
      RETURN.
    ENDIF.

    " Activate DDIC + non-DDIC
    Lcl_abapgit_objects_activation=>activate(
      iv_ddic = abap_true
      ii_log  = ri_log ).

    Lcl_abapgit_objects_activation=>activate(
      iv_ddic = abap_false
      ii_log  = ri_log ).

    IF ri_log->get_status( ) <> Lif_abapgit_log=>c_status-error.
      lv_message = |Successfully activated { lv_count } objects|.
      MESSAGE lv_message TYPE 'S'.
    ENDIF.

    lo_repo->refresh( iv_drop_log = abap_false ).

  ENDMETHOD.
  METHOD check_for_restart.

    CONSTANTS:
      lc_abapgit_prog TYPE progname VALUE `ZABAPGIT`.

    DATA lo_repo_online TYPE REF TO Lcl_abapgit_repo_online.

    IF io_repo->is_offline( ) = abap_true.
      RETURN.
    ENDIF.

    lo_repo_online ?= io_repo.

    " If abapGit was used to update itself, then restart to avoid LOAD_PROGRAM_&_MISMATCH dumps
    " because abapGit code was changed at runtime
    IF Lcl_abapgit_ui_factory=>get_frontend_services( )->gui_is_available( ) = abap_true AND
       Lcl_abapgit_url=>is_abapgit_repo( lo_repo_online->get_url( ) ) = abap_true AND
       sy-batch = abap_false AND
       sy-cprog = lc_abapgit_prog.

      IF Lcl_abapgit_persist_factory=>get_settings( )->read( )->get_show_default_repo( ) = abap_false.
        MESSAGE 'abapGit was updated and will restart itself' TYPE 'I'.
      ENDIF.

      SUBMIT (sy-cprog).

    ENDIF.

  ENDMETHOD.
  METHOD check_package.

    DATA:
      li_repo     TYPE REF TO Lif_abapgit_repo,
      li_repo_srv TYPE REF TO Lif_abapgit_repo_srv,
      lv_reason   TYPE string.

    " make sure package is not already in use for a different repository
    " 702: chaining calls with exp&imp parameters causes syntax error
    li_repo_srv = Lcl_abapgit_repo_srv=>get_instance( ).
    li_repo_srv->get_repo_from_package(
      EXPORTING
        iv_package    = is_repo_params-package
        iv_ign_subpkg = is_repo_params-ignore_subpackages
      IMPORTING
        ei_repo    = li_repo
        ev_reason  = lv_reason ).

    IF li_repo IS BOUND.
      Lcx_abapgit_exception=>raise( lv_reason ).
    ENDIF.

  ENDMETHOD.
  METHOD create_package.

    DATA ls_package_data TYPE scompkdtln.
    DATA lv_create       TYPE abap_bool.
    DATA li_popup        TYPE REF TO Lif_abapgit_popups.

    ls_package_data-devclass = condense( to_upper( iv_prefill_package ) ).

    raise_error_if_package_exists( ls_package_data-devclass ).

    li_popup = Lcl_abapgit_ui_factory=>get_popups( ).

    li_popup->popup_to_create_package(
      IMPORTING
        es_package_data = ls_package_data
        ev_create       = lv_create ).

    IF lv_create = abap_true.
      Lcl_abapgit_factory=>get_sap_package( ls_package_data-devclass )->create( ls_package_data ).
      rv_package = ls_package_data-devclass.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDMETHOD.
  METHOD delete_unnecessary_objects.

    DATA:
      ls_checks TYPE Lif_abapgit_definitions=>ty_delete_checks,
      ls_tadir  TYPE Lif_abapgit_definitions=>ty_tadir,
      lt_tadir  TYPE Lif_abapgit_definitions=>ty_tadir_tt.

    FIELD-SYMBOLS <ls_overwrite> LIKE LINE OF is_checks-overwrite.

    " get confirmed deletions
    LOOP AT is_checks-overwrite ASSIGNING <ls_overwrite>
      WHERE ( action = Lif_abapgit_objects=>c_deserialize_action-delete
      OR action = Lif_abapgit_objects=>c_deserialize_action-delete_add )
      AND decision = Lif_abapgit_definitions=>c_yes.

      ls_tadir-pgmid    = 'R3TR'.
      ls_tadir-object   = <ls_overwrite>-obj_type.
      ls_tadir-obj_name = <ls_overwrite>-obj_name.
      ls_tadir-devclass = <ls_overwrite>-devclass.
      INSERT ls_tadir INTO TABLE lt_tadir.

    ENDLOOP.

    " todo, check if object type supports deletion of parts to avoid deleting complete object

    " delete objects
    IF lines( lt_tadir ) > 0.
      ls_checks-transport = is_checks-transport.

      Lcl_abapgit_objects=>delete( it_tadir  = lt_tadir
                                   is_checks = ls_checks
                                   ii_log    = ii_log ).

      io_repo->refresh( iv_drop_log = abap_false ).
    ENDIF.

  ENDMETHOD.
  METHOD gui_deserialize.

    DATA:
      lv_msg    TYPE string,
      ls_checks TYPE Lif_abapgit_definitions=>ty_deserialize_checks,
      li_log    TYPE REF TO Lif_abapgit_log.

    " find troublesome objects
    ls_checks = io_repo->deserialize_checks( ).

    IF ls_checks-overwrite IS INITIAL.
      Lcx_abapgit_exception=>raise(
        'There is nothing to pull. The local state completely matches the remote repository.' ).
    ENDIF.

    " let the user decide what to do
    TRY.
        popup_decisions(
          EXPORTING
            io_repo   = io_repo
          CHANGING
            cs_checks = ls_checks ).

      CATCH Lcx_abapgit_cancel.
        RETURN.
    ENDTRY.

    li_log = io_repo->create_new_log( 'Pull Log' ).

    " pass decisions to delete
    delete_unnecessary_objects(
      io_repo   = io_repo
      is_checks = ls_checks
      ii_log    = li_log ).

    " pass decisions to deserialize
    io_repo->deserialize(
      is_checks = ls_checks
      ii_log    = li_log ).

    IF li_log->get_status( ) = Lif_abapgit_log=>c_status-ok.
      lv_msg = |Repository { io_repo->get_name( ) } successfully pulled for package { io_repo->get_package( ) }|.
      MESSAGE lv_msg TYPE 'S'.
    ENDIF.

    check_for_restart( io_repo ).

  ENDMETHOD.
  METHOD new_offline.

    DATA lx_error TYPE REF TO Lcx_abapgit_exception.

    check_package( is_repo_params ).

    " create new repo and add to favorites
    ro_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->new_offline(
      iv_name           = is_repo_params-name
      iv_package        = is_repo_params-package
      iv_folder_logic   = is_repo_params-folder_logic
      iv_labels         = is_repo_params-labels
      iv_ign_subpkg     = is_repo_params-ignore_subpackages
      iv_main_lang_only = is_repo_params-main_lang_only
      iv_abap_lang_vers = is_repo_params-abap_lang_vers ).

    TRY.
        check_package_exists(
          iv_package = is_repo_params-package
          it_remote  = ro_repo->get_files_remote( ) ).
      CATCH Lcx_abapgit_exception INTO lx_error.
        Lcl_abapgit_repo_srv=>get_instance( )->delete( ro_repo ).
        COMMIT WORK.
        RAISE EXCEPTION lx_error.
    ENDTRY.

    " Make sure there're no leftovers from previous repos
    ro_repo->Lif_abapgit_repo~checksums( )->rebuild( ).

    toggle_favorite( ro_repo->get_key( ) ).

    " Set default repo for user
    Lcl_abapgit_persistence_user=>get_instance( )->set_repo_show( ro_repo->get_key( ) ).

    COMMIT WORK AND WAIT.


  ENDMETHOD.
  METHOD new_online.

    DATA lx_error TYPE REF TO Lcx_abapgit_exception.

    check_package( is_repo_params ).

    ro_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->new_online(
      iv_url            = is_repo_params-url
      iv_branch_name    = is_repo_params-branch_name
      iv_name           = is_repo_params-name
      iv_package        = is_repo_params-package
      iv_display_name   = is_repo_params-display_name
      iv_folder_logic   = is_repo_params-folder_logic
      iv_labels         = is_repo_params-labels
      iv_ign_subpkg     = is_repo_params-ignore_subpackages
      iv_main_lang_only = is_repo_params-main_lang_only
      iv_abap_lang_vers = is_repo_params-abap_lang_vers ).

    TRY.
        check_package_exists(
          iv_package = is_repo_params-package
          it_remote  = ro_repo->get_files_remote( ) ).
      CATCH Lcx_abapgit_exception INTO lx_error.
        Lcl_abapgit_repo_srv=>get_instance( )->delete( ro_repo ).
        COMMIT WORK.
        RAISE EXCEPTION lx_error.
    ENDTRY.

    " Make sure there're no leftovers from previous repos
    ro_repo->Lif_abapgit_repo~checksums( )->rebuild( ).

    toggle_favorite( ro_repo->get_key( ) ).

    " Set default repo for user
    Lcl_abapgit_persistence_user=>get_instance( )->set_repo_show( ro_repo->get_key( ) ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.
  METHOD popup_decisions.

    DATA:
      lt_decision     TYPE Lif_abapgit_definitions=>ty_overwrite_tt,
      lt_requirements TYPE Lif_abapgit_dot_abapgit=>ty_requirement_tt,
      lt_dependencies TYPE Lif_abapgit_apack_definitions=>ty_dependencies.

    FIELD-SYMBOLS:
      <ls_overwrite> LIKE LINE OF cs_checks-overwrite,
      <ls_decision>  LIKE LINE OF lt_decision.

    lt_decision = cs_checks-overwrite.

    " If there's a new namespace, it has to be pulled before all other objects
    READ TABLE lt_decision ASSIGNING <ls_decision> WITH KEY obj_type = 'NSPC'.
    IF sy-subrc = 0 AND <ls_decision>-action = Lif_abapgit_objects=>c_deserialize_action-add.
      <ls_decision>-decision = Lif_abapgit_definitions=>c_yes.
    ELSE.
      " Set all new objects to YES
      LOOP AT lt_decision ASSIGNING <ls_decision> WHERE action = Lif_abapgit_objects=>c_deserialize_action-add.
        <ls_decision>-decision = Lif_abapgit_definitions=>c_yes.
      ENDLOOP.
    ENDIF.

    " Ask user what to do
    IF cs_checks-requirements-met = Lif_abapgit_definitions=>c_no.
      lt_requirements = io_repo->get_dot_abapgit( )->get_data( )-requirements.
      Lcl_abapgit_requirement_helper=>requirements_popup( lt_requirements ).
      cs_checks-requirements-decision = Lif_abapgit_definitions=>c_yes.
    ENDIF.

    IF cs_checks-dependencies-met = Lif_abapgit_definitions=>c_no.
      lt_dependencies = io_repo->get_dot_apack( )->get_manifest_descriptor( )-dependencies.
      Lcl_abapgit_apack_helper=>dependencies_popup( lt_dependencies ).
      cs_checks-dependencies-decision = Lif_abapgit_definitions=>c_yes.
    ENDIF.

    popup_objects_overwrite( CHANGING ct_overwrite = lt_decision ).
    popup_package_overwrite( CHANGING ct_overwrite = cs_checks-warning_package ).

    IF cs_checks-transport-required = abap_true AND cs_checks-transport-transport IS INITIAL.
      cs_checks-transport-transport =
        Lcl_abapgit_ui_factory=>get_popups( )->popup_transport_request( cs_checks-transport-type ).
    ENDIF.

    " Update decisions
    LOOP AT cs_checks-overwrite ASSIGNING <ls_overwrite>.
      READ TABLE lt_decision ASSIGNING <ls_decision> WITH KEY object_type_and_name COMPONENTS
        obj_type = <ls_overwrite>-obj_type
        obj_name = <ls_overwrite>-obj_name.
      ASSERT sy-subrc = 0.
      <ls_overwrite>-decision = <ls_decision>-decision.
    ENDLOOP.

  ENDMETHOD.
  METHOD popup_package_overwrite.

    DATA: lt_columns  TYPE Lif_abapgit_popups=>ty_alv_column_tt,
          lt_selected LIKE ct_overwrite,
          li_popups   TYPE REF TO Lif_abapgit_popups.

    FIELD-SYMBOLS: <ls_overwrite> LIKE LINE OF ct_overwrite,
                   <ls_column>    TYPE Lif_abapgit_popups=>ty_alv_column.

    IF lines( ct_overwrite ) = 0.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'OBJ_TYPE'.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'OBJ_NAME'.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'DEVCLASS'.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'STATE'.
    <ls_column>-text = 'State'.
    <ls_column>-length = 3.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'ICON'.
    <ls_column>-text = 'Action'.
    <ls_column>-show_icon = abap_true.
    <ls_column>-length = 5.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'TEXT'.
    <ls_column>-text = 'Description'.

    li_popups = Lcl_abapgit_ui_factory=>get_popups( ).
    li_popups->popup_to_select_from_list(
      EXPORTING
        it_list               = ct_overwrite
        iv_header_text        = |The following objects have been created in other packages.|
                             && | Select the objects which should be overwritten.|
        iv_select_column_text = |Overwrite?|
        it_columns_to_display = lt_columns
      IMPORTING
        et_list               = lt_selected ).

    LOOP AT ct_overwrite ASSIGNING <ls_overwrite>.

      READ TABLE lt_selected WITH TABLE KEY object_type_and_name
                             COMPONENTS obj_type = <ls_overwrite>-obj_type
                                        obj_name = <ls_overwrite>-obj_name
                             TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        <ls_overwrite>-decision = Lif_abapgit_definitions=>c_yes.
      ELSE.
        <ls_overwrite>-decision = Lif_abapgit_definitions=>c_no.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD purge.

    DATA: lt_tadir     TYPE Lif_abapgit_definitions=>ty_tadir_tt,
          lv_answer    TYPE c LENGTH 1,
          lo_repo      TYPE REF TO Lcl_abapgit_repo,
          lv_package   TYPE devclass,
          lv_title     TYPE c LENGTH 20,
          lv_question  TYPE c LENGTH 150,
          ls_checks    TYPE Lif_abapgit_definitions=>ty_delete_checks,
          lv_repo_name TYPE string,
          lv_message   TYPE string.


    lo_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
    lv_repo_name = lo_repo->get_name( ).

    lv_package = lo_repo->get_package( ).
    lt_tadir   = lo_repo->get_tadir_objects( ).

    IF lines( lt_tadir ) > 0.

      lv_question = |This will DELETE all objects in package { lv_package
        } including subpackages ({ lines( lt_tadir ) } objects) from the system|.

      IF iv_keep_repo = abap_true.
        lv_title = 'Remove Objects'.
        lv_question = lv_question && ', but keep the reference to the repository'.
      ELSE.
        lv_title = 'Uninstall'.
        lv_question = lv_question && ' and remove the reference to the repository'.
      ENDIF.

      lv_answer = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
        iv_titlebar              = lv_title
        iv_text_question         = lv_question
        iv_text_button_1         = 'Delete'
        iv_icon_button_1         = 'ICON_DELETE'
        iv_text_button_2         = 'Cancel'
        iv_icon_button_2         = 'ICON_CANCEL'
        iv_default_button        = '2'
        iv_popup_type            = 'ICON_MESSAGE_WARNING'
        iv_display_cancel_button = abap_false ).

      IF lv_answer = '2'.
        RAISE EXCEPTION TYPE Lcx_abapgit_cancel.
      ENDIF.

    ENDIF.

    ls_checks = lo_repo->delete_checks( ).
    IF ls_checks-transport-required = abap_true.
      ls_checks-transport-transport = Lcl_abapgit_ui_factory=>get_popups(
                                        )->popup_transport_request( ls_checks-transport-type ).
    ENDIF.

    ri_log = Lcl_abapgit_repo_srv=>get_instance( )->purge(
      ii_repo      = lo_repo
      is_checks    = ls_checks
      iv_keep_repo = iv_keep_repo ).

    COMMIT WORK.

    IF ri_log IS BOUND AND ri_log->get_status( ) = Lif_abapgit_log=>c_status-error.
      Lcl_abapgit_log_viewer=>show_log( ri_log ).
      RETURN.
    ENDIF.

    lv_message = |Repository { lv_repo_name } successfully uninstalled from Package { lv_package }. |.
    MESSAGE lv_message TYPE 'S'.

  ENDMETHOD.
  METHOD raise_error_if_package_exists.

    IF iv_devclass IS INITIAL.
      RETURN.
    ENDIF.

    IF Lcl_abapgit_factory=>get_sap_package( iv_devclass )->exists( ) = abap_true.
      Lcx_abapgit_exception=>raise( |Package { iv_devclass } already exists| ).
    ENDIF.

  ENDMETHOD.
  METHOD refresh.

    Lcl_abapgit_repo_srv=>get_instance( )->get( iv_key )->refresh( ).

  ENDMETHOD.
  METHOD refresh_local_checksums.

    DATA: lv_answer   TYPE c,
          lv_question TYPE string,
          lo_repo     TYPE REF TO Lcl_abapgit_repo.


    IF Lcl_abapgit_auth=>is_allowed( Lif_abapgit_auth=>c_authorization-update_local_checksum ) = abap_false.
      Lcx_abapgit_exception=>raise( 'Not authorized' ).
    ENDIF.

    lo_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    lv_question = 'This will rebuild and overwrite local repo checksums.'.

    IF lo_repo->is_offline( ) = abap_false.
      lv_question = lv_question
                && ' The logic: if local and remote file differs then:'
                && ' if remote branch is ahead then assume changes are remote,'
                && ' else (branches are equal) assume changes are local.'
                && ' This will lead to incorrect state for files changed on both sides.'
                && ' Please make sure you don''t have ones like that.'.
    ENDIF.

    lv_answer = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
      iv_titlebar              = 'Warning'
      iv_text_question         = lv_question
      iv_text_button_1         = 'OK'
      iv_icon_button_1         = 'ICON_DELETE'
      iv_text_button_2         = 'Cancel'
      iv_icon_button_2         = 'ICON_CANCEL'
      iv_default_button        = '2'
      iv_display_cancel_button = abap_false ).

    IF lv_answer = '2'.
      RAISE EXCEPTION TYPE Lcx_abapgit_cancel.
    ENDIF.

    lo_repo->Lif_abapgit_repo~checksums( )->rebuild( ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.
  METHOD remove.

    DATA: lv_answer    TYPE c LENGTH 1,
          li_repo      TYPE REF TO Lif_abapgit_repo,
          lv_package   TYPE devclass,
          lv_question  TYPE c LENGTH 200,
          lv_repo_name TYPE string,
          lv_message   TYPE string.


    li_repo      = Lcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
    lv_repo_name = li_repo->get_name( ).
    lv_package   = li_repo->get_package( ).
    lv_question  = |This will remove the repository reference to the package { lv_package
      }. All objects will safely remain in the system.|.

    lv_answer = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
      iv_titlebar              = 'Remove Repository'
      iv_text_question         = lv_question
      iv_text_button_1         = 'Remove'
      iv_icon_button_1         = 'ICON_WF_UNLINK'
      iv_text_button_2         = 'Cancel'
      iv_icon_button_2         = 'ICON_CANCEL'
      iv_default_button        = '2'
      iv_display_cancel_button = abap_false ).

    IF lv_answer = '2'.
      RAISE EXCEPTION TYPE Lcx_abapgit_cancel.
    ENDIF.

    Lcl_abapgit_repo_srv=>get_instance( )->delete( li_repo ).

    COMMIT WORK.

    lv_message = |Reference to repository { lv_repo_name } successfully removed from Package { lv_package }. |.
    MESSAGE lv_message TYPE 'S'.

  ENDMETHOD.
  METHOD toggle_favorite.

    Lcl_abapgit_persistence_user=>get_instance( )->toggle_favorite( iv_key ).

  ENDMETHOD.
  METHOD transport_to_branch.

    DATA:
      lo_repository          TYPE REF TO Lcl_abapgit_repo_online,
      lo_transport_to_branch TYPE REF TO Lcl_abapgit_transport_2_branch,
      lt_transport_objects   TYPE Lif_abapgit_definitions=>ty_tadir_tt,
      lv_trkorr              TYPE trkorr,
      ls_transport_to_branch TYPE Lif_abapgit_definitions=>ty_transport_to_branch.


    IF Lcl_abapgit_auth=>is_allowed( Lif_abapgit_auth=>c_authorization-transport_to_branch ) = abap_false.
      Lcx_abapgit_exception=>raise( 'Not authorized' ).
    ENDIF.

    lo_repository ?= Lcl_abapgit_repo_srv=>get_instance( )->get( iv_repository_key ).

    lv_trkorr = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_select_transport( ).
    " Also include deleted objects that are included in transport
    lt_transport_objects = Lcl_abapgit_transport=>to_tadir(
      iv_trkorr          = lv_trkorr
      iv_deleted_objects = abap_true ).
    IF lt_transport_objects IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Canceled or List of objects is empty ' ).
    ENDIF.

    ls_transport_to_branch = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_create_transp_branch( lv_trkorr ).

    CREATE OBJECT lo_transport_to_branch.
    lo_transport_to_branch->create(
      io_repository          = lo_repository
      is_transport_to_branch = ls_transport_to_branch
      it_transport_objects   = lt_transport_objects ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SERVICES_REPO implementation

*>>>>>>> ZCL_ABAPGIT_SOTR_HANDLER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_sotr_handler======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_sotr_handler======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_sotr_handler======ccau.


class LCL_ABAPGIT_SOTR_HANDLER implementation.
*"* method's implementations
*include methods.
  METHOD create_sotr.

    DATA:
      lt_sotr     TYPE ty_sotr_tt,
      lt_sotr_use TYPE ty_sotr_use_tt.

    io_xml->read( EXPORTING iv_name = 'SOTR'
                  CHANGING cg_data = lt_sotr ).
    io_xml->read( EXPORTING iv_name = 'SOTR_USE'
                  CHANGING cg_data = lt_sotr_use ).

    create_sotr_from_data(
      iv_package  = iv_package
      it_sotr     = lt_sotr
      it_sotr_use = lt_sotr_use ).

  ENDMETHOD.
  METHOD create_sotr_from_data.

    DATA:
      lt_objects TYPE sotr_objects,
      ls_paket   TYPE sotr_pack,
      lv_alias   TYPE sotr_head-alias_name,
      lv_object  LIKE LINE OF lt_objects.

    FIELD-SYMBOLS: <ls_sotr> LIKE LINE OF it_sotr.

    LOOP AT it_sotr ASSIGNING <ls_sotr>.
      CALL FUNCTION 'SOTR_OBJECT_GET_OBJECTS'
        EXPORTING
          object_vector    = <ls_sotr>-header-objid_vec
        IMPORTING
          objects          = lt_objects
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      READ TABLE lt_objects INDEX 1 INTO lv_object.
      ASSERT sy-subrc = 0.

      ls_paket-paket = iv_package.

      " Replace package in alias with new package
      lv_alias = <ls_sotr>-header-alias_name.
      IF lv_alias CS '/'.
        lv_alias = iv_package && lv_alias+sy-fdpos(*).
      ENDIF.

      CALL FUNCTION 'SOTR_CREATE_CONCEPT'
        EXPORTING
          paket                         = ls_paket
          crea_lan                      = <ls_sotr>-header-crea_lan
          alias_name                    = lv_alias
          object                        = lv_object
          entries                       = <ls_sotr>-entries
          concept_default               = <ls_sotr>-header-concept
        EXCEPTIONS
          package_missing               = 1
          crea_lan_missing              = 2
          object_missing                = 3
          paket_does_not_exist          = 4
          alias_already_exist           = 5
          object_type_not_found         = 6
          langu_missing                 = 7
          identical_context_not_allowed = 8
          text_too_long                 = 9
          error_in_update               = 10
          no_master_langu               = 11
          error_in_concept_id           = 12
          alias_not_allowed             = 13
          tadir_entry_creation_failed   = 14
          internal_error                = 15
          error_in_correction           = 16
          user_cancelled                = 17
          no_entry_found                = 18
          OTHERS                        = 19.
      IF sy-subrc <> 0 AND sy-subrc <> 5.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'SOTR_USAGE_MODIFY'
      EXPORTING
        sotr_usage = it_sotr_use.

  ENDMETHOD.
  METHOD delete_sotr.

    DATA lt_sotr_use TYPE ty_sotr_use_tt.

    FIELD-SYMBOLS <ls_sotr_use> LIKE LINE OF lt_sotr_use.

    lt_sotr_use = get_sotr_usage( iv_pgmid    = iv_pgmid
                                  iv_object   = iv_object
                                  iv_obj_name = iv_obj_name ).

    " Remove any usage to ensure deletion, see function module BTFR_CHECK
    DELETE sotr_use FROM TABLE lt_sotr_use ##SUBRC_OK.

    LOOP AT lt_sotr_use ASSIGNING <ls_sotr_use> WHERE concept IS NOT INITIAL.

      CALL FUNCTION 'SOTR_DELETE_CONCEPT'
        EXPORTING
          concept             = <ls_sotr_use>-concept
        EXCEPTIONS
          no_entry_found      = 1
          text_not_found      = 2
          invalid_package     = 3
          text_not_changeable = 4
          text_enqueued       = 5
          no_correction       = 6
          parameter_error     = 7
          OTHERS              = 8.
      IF sy-subrc > 2.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD delete_sotr_package.

    DATA lt_sotr_head TYPE STANDARD TABLE OF sotr_head WITH DEFAULT KEY.
    DATA lv_obj_name TYPE tadir-obj_name.

    FIELD-SYMBOLS <ls_sotr_head> LIKE LINE OF lt_sotr_head.

    SELECT * FROM sotr_head INTO TABLE lt_sotr_head WHERE paket = iv_package ORDER BY PRIMARY KEY.

    LOOP AT lt_sotr_head ASSIGNING <ls_sotr_head> WHERE concept IS NOT INITIAL.

      CALL FUNCTION 'SOTR_DELETE_CONCEPT'
        EXPORTING
          concept             = <ls_sotr_head>-concept
        EXCEPTIONS
          no_entry_found      = 1
          text_not_found      = 2
          invalid_package     = 3
          text_not_changeable = 4
          text_enqueued       = 5
          no_correction       = 6
          parameter_error     = 7
          OTHERS              = 8.
      IF sy-subrc > 2.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ENDLOOP.

    " Nothing left, then delete SOTR from TADIR
    SELECT * FROM sotr_head INTO TABLE lt_sotr_head WHERE paket = iv_package ORDER BY PRIMARY KEY.
    IF sy-subrc <> 0.
      SELECT SINGLE obj_name FROM tadir INTO lv_obj_name
        WHERE pgmid = 'R3TR' AND object = 'SOTR' AND obj_name = iv_package.
      IF sy-subrc = 0.
        CALL FUNCTION 'TR_TADIR_INTERFACE'
          EXPORTING
            wi_delete_tadir_entry = abap_true
            wi_test_modus         = abap_false
            wi_tadir_pgmid        = 'R3TR'
            wi_tadir_object       = 'SOTR'
            wi_tadir_obj_name     = lv_obj_name
          EXCEPTIONS
            OTHERS                = 1 ##FM_SUBRC_OK.

        IF Lcl_abapgit_factory=>get_sap_package( iv_package )->are_changes_recorded_in_tr_req( ) = abap_true.

          Lcl_abapgit_factory=>get_cts_api( )->insert_transport_object(
            iv_object   = 'SOTR'
            iv_obj_name = lv_obj_name
            iv_package  = iv_package
            iv_mode     = Lif_abapgit_cts_api=>c_transport_mode-delete ).

        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD get_sotr_4_concept.

    DATA: ls_header  TYPE ty_sotr-header,
          lv_paket   LIKE ls_header-alias_name,
          lt_entries TYPE ty_sotr-entries.

    FIELD-SYMBOLS: <ls_entry> LIKE LINE OF lt_entries.

    CALL FUNCTION 'SOTR_GET_CONCEPT'
      EXPORTING
        concept        = iv_concept
      IMPORTING
        header         = ls_header
      TABLES
        entries        = lt_entries
      EXCEPTIONS
        no_entry_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " If alias contains package, remove it
    lv_paket = ls_header-paket && '/'.
    IF ls_header-alias_name CS lv_paket.
      ls_header-alias_name = replace(
        val  = ls_header-alias_name
        sub  = lv_paket
        with = '/'
        occ  = 1 ).
    ENDIF.

    CLEAR: ls_header-paket,
           ls_header-crea_name,
           ls_header-crea_tstut,
           ls_header-chan_name,
           ls_header-chan_tstut,
           ls_header-system_id.

    LOOP AT lt_entries ASSIGNING <ls_entry>.
      CLEAR: <ls_entry>-version,
             <ls_entry>-crea_name,
             <ls_entry>-crea_tstut,
             <ls_entry>-chan_name,
             <ls_entry>-chan_tstut.
    ENDLOOP.

    rs_sotr-header  = ls_header.
    rs_sotr-entries = lt_entries.

  ENDMETHOD.
  METHOD get_sotr_usage.

    DATA: lv_obj_name TYPE trobj_name.

    lv_obj_name = iv_obj_name.

    " Objects with multiple components
    IF iv_pgmid = 'LIMU' AND ( iv_object CP 'WDY*' OR iv_object = 'WAPP' ).
      lv_obj_name+30 = '%'.
    ENDIF.

    CALL FUNCTION 'SOTR_USAGE_READ'
      EXPORTING
        pgmid          = iv_pgmid
        object         = iv_object
        obj_name       = lv_obj_name
      IMPORTING
        sotr_usage     = rt_sotr_use
      EXCEPTIONS
        no_entry_found = 1
        error_in_pgmid = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      SORT rt_sotr_use.
    ENDIF.

  ENDMETHOD.
  METHOD read_sotr.

    FIELD-SYMBOLS <ls_sotr_use> LIKE LINE OF et_sotr_use.

    DATA: lv_sotr            TYPE ty_sotr,
          lt_language_filter TYPE Lif_abapgit_environment=>ty_system_language_filter.

    " OTR short text usage: see TABLE BTFR_OBJ_IDS
    " LIMU: CPUB, WAPP, WDYC, WDYD, WDYV
    " R3TR: ENHC, ENHO, ENHS, ENSC, SCGR, SMIF, WDCA, WDCC, WEBI, WEBS

    et_sotr_use = get_sotr_usage( iv_pgmid    = iv_pgmid
                                  iv_object   = iv_object
                                  iv_obj_name = iv_obj_name ).

    LOOP AT et_sotr_use ASSIGNING <ls_sotr_use> WHERE concept IS NOT INITIAL.
      lv_sotr = get_sotr_4_concept( <ls_sotr_use>-concept ).

      IF io_xml IS BOUND AND io_i18n_params->ms_params-main_language_only = abap_true.
        DELETE lv_sotr-entries WHERE langu <> io_i18n_params->ms_params-main_language.
        CHECK lv_sotr-entries IS NOT INITIAL.
      ENDIF.
      lt_language_filter = io_i18n_params->build_language_filter( ).
      DELETE lv_sotr-entries WHERE NOT langu IN lt_language_filter
        AND langu <> io_i18n_params->ms_params-main_language.
      CHECK lv_sotr-entries IS NOT INITIAL.

      INSERT lv_sotr INTO TABLE et_sotr.
    ENDLOOP.

    IF io_xml IS BOUND.
      io_xml->add( iv_name = 'SOTR'
                   ig_data = et_sotr ).
      io_xml->add( iv_name = 'SOTR_USE'
                   ig_data = et_sotr_use ).
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SOTR_HANDLER implementation

*>>>>>>> ZCL_ABAPGIT_TRANSPORT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_transport=========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_transport=========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_TRANSPORT implementation.
*"* method's implementations
*include methods.
  METHOD add_all_objects_to_trans_req.

    DATA:
      ls_request      TYPE trwbo_request_header,
      lt_e071         TYPE tr_objects,
      lv_text         TYPE string,
      lv_answer       TYPE c LENGTH 1,
      lv_lock_objects TYPE trparflag,
      lt_log          TYPE sprot_u_tab.

    lv_answer = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
                    iv_titlebar              = `Lock objects?`
                    iv_text_question         = `Shall all objects be locked in the transport request?`
                    iv_display_cancel_button = abap_true ).

    CASE lv_answer.
      WHEN '1'.
        lv_lock_objects = abap_true.
      WHEN '2'.
        lv_lock_objects = abap_false.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    lt_e071 = collect_all_objects( iv_key ).

    " We used TR_REQUEST_CHOICE before, but it issues its error log with
    " write lists which are not compatible with abapGit.
    " There we user TRINT_REQUEST_CHOICE which returns the error log
    " and display the log ourselve.
    CALL FUNCTION 'TRINT_REQUEST_CHOICE'
      EXPORTING
        iv_request_types     = 'FTCOK'
        iv_lock_objects      = lv_lock_objects
        iv_with_error_log    = abap_false
      IMPORTING
        es_request           = ls_request
        et_log               = lt_log
      TABLES
        it_e071              = lt_e071
      EXCEPTIONS
        invalid_request      = 1
        invalid_request_type = 2
        user_not_owner       = 3
        no_objects_appended  = 4
        enqueue_error        = 5
        cancelled_by_user    = 6
        recursive_call       = 7
        OTHERS               = 8.
    IF sy-subrc = 0.
      lv_text = |Objects successfully added to { ls_request-trkorr }|.
      MESSAGE lv_text TYPE 'S'.
      RETURN.
    ENDIF.

    IF lines( lt_log ) > 0.
      show_log(
          it_log   = lt_log
          iv_title = `Error log` ).
    ELSE.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD collect_all_objects.

    DATA:
      lt_objects     TYPE scts_tadir,
      lt_objects_all LIKE lt_objects,
      ls_e071        LIKE LINE OF rt_objects,
      lo_repo        TYPE REF TO Lcl_abapgit_repo,
      lv_package     TYPE Lif_abapgit_persistence=>ty_repo-package,
      lt_packages    TYPE Lif_abapgit_sap_package=>ty_devclass_tt.

    FIELD-SYMBOLS:
      <lv_package> TYPE devclass,
      <ls_object>  TYPE tadir.

    lo_repo    ?= Lcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
    lv_package  = lo_repo->get_package( ).
    lt_packages = Lcl_abapgit_factory=>get_sap_package( lv_package )->list_subpackages( ).
    INSERT lv_package INTO TABLE lt_packages.

    LOOP AT lt_packages ASSIGNING <lv_package>.

      CLEAR: lt_objects.

      CALL FUNCTION 'TRINT_SELECT_OBJECTS'
        EXPORTING
          iv_devclass       = <lv_package>
          iv_via_selscreen  = abap_false
        IMPORTING
          et_objects_tadir  = lt_objects
        EXCEPTIONS
          cancelled_by_user = 1
          invalid_input     = 2
          OTHERS            = 3.

      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      INSERT LINES OF lt_objects INTO TABLE lt_objects_all.

    ENDLOOP.

    IF lines( lt_objects_all ) = 0.
      Lcx_abapgit_exception=>raise( |No objects found| ).
    ENDIF.

    LOOP AT lt_objects_all ASSIGNING <ls_object>.

      CLEAR: ls_e071.

      MOVE-CORRESPONDING <ls_object> TO ls_e071.
      INSERT ls_e071 INTO TABLE rt_objects.

    ENDLOOP.

  ENDMETHOD.
  METHOD find_top_package.
* assumption: all objects in transport share a common super package

    DATA: lt_obj   TYPE Lif_abapgit_sap_package=>ty_devclass_tt,
          lt_super TYPE Lif_abapgit_sap_package=>ty_devclass_tt,
          lv_super LIKE LINE OF lt_super,
          lv_index TYPE i.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF it_tadir.


    READ TABLE it_tadir INDEX 1 ASSIGNING <ls_tadir>.
    ASSERT sy-subrc = 0.
    lt_super = Lcl_abapgit_factory=>get_sap_package( <ls_tadir>-devclass )->list_superpackages( ).

    LOOP AT it_tadir ASSIGNING <ls_tadir>.
      lt_obj = Lcl_abapgit_factory=>get_sap_package( <ls_tadir>-devclass )->list_superpackages( ).

* filter out possibilities from lt_super
      LOOP AT lt_super INTO lv_super.
        lv_index = sy-tabix.
        READ TABLE lt_obj FROM lv_super TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          DELETE lt_super INDEX lv_index.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    READ TABLE lt_super INDEX lines( lt_super ) INTO rv_package.
  ENDMETHOD.
  METHOD read_requests.
    DATA lt_requests LIKE rt_requests.
    FIELD-SYMBOLS <lv_trkorr> LIKE LINE OF it_trkorr.

    LOOP AT it_trkorr ASSIGNING <lv_trkorr>.
      CALL FUNCTION 'TR_READ_REQUEST_WITH_TASKS'
        EXPORTING
          iv_trkorr     = <lv_trkorr>
        IMPORTING
          et_requests   = lt_requests
        EXCEPTIONS
          invalid_input = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      APPEND LINES OF lt_requests TO rt_requests.
    ENDLOOP.
  ENDMETHOD.
  METHOD resolve.
    DATA: lv_object    TYPE tadir-object,
          lv_obj_name  TYPE tadir-obj_name,
          ls_tadir     TYPE Lif_abapgit_definitions=>ty_tadir,
          lv_result    TYPE trpari-s_checked,
          ls_tadir_sap TYPE tadir.

    FIELD-SYMBOLS: <ls_request> LIKE LINE OF it_requests,
                   <ls_object>  LIKE LINE OF <ls_request>-objects.


    LOOP AT it_requests ASSIGNING <ls_request>.
      LOOP AT <ls_request>-objects ASSIGNING <ls_object>.
        " VARX, see https://github.com/abapGit/abapGit/issues/3107
        IF <ls_object>-pgmid = 'LIMU' AND <ls_object>-object <> 'VARX'.
          CALL FUNCTION 'TR_CHECK_TYPE'
            EXPORTING
              wi_e071   = <ls_object>
            IMPORTING
              we_tadir  = ls_tadir_sap
              pe_result = lv_result.
          IF lv_result NA 'TL' OR ls_tadir_sap IS INITIAL.
            Lcx_abapgit_exception=>raise( 'error from TR_CHECK_TYPE' ).
          ENDIF.
          lv_object   = ls_tadir_sap-object.
          lv_obj_name = ls_tadir_sap-obj_name.
        ELSE.
          lv_object   = <ls_object>-object.
          lv_obj_name = <ls_object>-obj_name.
        ENDIF.

        ls_tadir = Lcl_abapgit_factory=>get_tadir( )->read_single(
          iv_object   = lv_object
          iv_obj_name = lv_obj_name ).

        IF ls_tadir-delflag IS INITIAL OR iv_deleted_objects = abap_true.
          APPEND ls_tadir TO rt_tadir.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    SORT rt_tadir BY object ASCENDING obj_name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rt_tadir COMPARING object obj_name.
    DELETE rt_tadir WHERE table_line IS INITIAL.
  ENDMETHOD.
  METHOD show_log.

    DATA: li_log     TYPE REF TO Lif_abapgit_log,
          lv_message TYPE string.
    FIELD-SYMBOLS: <ls_log> TYPE sprot_u.

    CREATE OBJECT li_log TYPE Lcl_abapgit_log
      EXPORTING
        iv_title = iv_title.

    LOOP AT it_log ASSIGNING <ls_log>.

      MESSAGE ID <ls_log>-ag TYPE <ls_log>-severity NUMBER <ls_log>-msgnr
       WITH <ls_log>-var1 <ls_log>-var2 <ls_log>-var3 <ls_log>-var4
       INTO lv_message.

      li_log->add(
          iv_msg  = lv_message
          iv_type = <ls_log>-severity ).

    ENDLOOP.

    Lcl_abapgit_log_viewer=>show_log( li_log ).

  ENDMETHOD.
  METHOD to_tadir.
    DATA lt_requests TYPE trwbo_requests.
    DATA lt_trkorr   TYPE ty_trkorr_tt.


    IF iv_trkorr IS INITIAL.
      RETURN.
    ENDIF.

    INSERT iv_trkorr INTO TABLE lt_trkorr.

    lt_requests = read_requests( lt_trkorr ).
    rt_tadir = resolve(
      it_requests        = lt_requests
      iv_deleted_objects = iv_deleted_objects ).

  ENDMETHOD.
  METHOD zip.

    DATA: lt_requests       TYPE trwbo_requests,
          lt_tadir          TYPE Lif_abapgit_definitions=>ty_tadir_tt,
          lv_package        TYPE devclass,
          lo_dot_abapgit    TYPE REF TO Lcl_abapgit_dot_abapgit,
          ls_local_settings TYPE Lif_abapgit_persistence=>ty_repo-local_settings,
          lt_trkorr         TYPE ty_trkorr_tt,
          lv_trkorr         TYPE trkorr.


    IF is_trkorr IS SUPPLIED.
      APPEND is_trkorr-trkorr TO lt_trkorr.
    ELSE.
      lv_trkorr = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_select_transport( ).
      IF lv_trkorr IS NOT INITIAL.
        APPEND lv_trkorr TO lt_trkorr.
      ENDIF.
    ENDIF.

    IF lines( lt_trkorr ) = 0.
      RETURN.
    ENDIF.

    lt_requests = read_requests( lt_trkorr ).
    lt_tadir = resolve( lt_requests ).
    IF lines( lt_tadir ) = 0.
      Lcx_abapgit_exception=>raise( 'empty transport' ).
    ENDIF.

    lv_package = find_top_package( lt_tadir ).
    IF lv_package IS INITIAL.
      Lcx_abapgit_exception=>raise( 'error finding super package' ).
    ENDIF.

    lo_dot_abapgit = Lcl_abapgit_dot_abapgit=>build_default( ).
    IF iv_logic IS SUPPLIED AND iv_logic IS NOT INITIAL.
      lo_dot_abapgit->set_folder_logic( iv_logic ).
    ELSE.
      lo_dot_abapgit->set_folder_logic( Lcl_abapgit_ui_factory=>get_popups( )->popup_folder_logic( ) ).
    ENDIF.

    rv_xstr = Lcl_abapgit_zip=>export(
      iv_package        = lv_package
      io_dot_abapgit    = lo_dot_abapgit
      is_local_settings = ls_local_settings
      it_filter         = lt_tadir
      iv_show_log       = iv_show_log_popup ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_TRANSPORT implementation

*>>>>>>> ZCL_ABAPGIT_UI_INJECTOR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ui_injector=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ui_injector=======ccimp.
CLASS SHRITEFUH64VYIPO5IWUYCIJCOKSFY DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_gui_services.
    CLASS-METHODS create
      RETURNING
        VALUE(ro_instance) TYPE REF TO SHRITEFUH64VYIPO5IWUYCIJCOKSFY.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYCIJCOKSFY IMPLEMENTATION.
  METHOD create.
    CREATE OBJECT ro_instance.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_services~cache_asset.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_services~register_event_handler.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_services~get_current_page_name.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_services~get_hotkeys_ctl.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_services~get_html_parts.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_services~get_log.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_services~register_page_asset.
  ENDMETHOD.

ENDCLASS.

*"* test class
*include zcl_abapgit_ui_injector=======ccau.






class LCL_ABAPGIT_UI_INJECTOR implementation.
*"* method's implementations
*include methods.
  METHOD get_dummy_gui_services.

    ri_gui_services = SHRITEFUH64VYIPO5IWUYCIJCOKSFY=>create( ).

  ENDMETHOD.
  METHOD set_frontend_services.

    Lcl_abapgit_ui_factory=>gi_fe_services = ii_fe_serv.

  ENDMETHOD.
  METHOD set_gui_services.

    Lcl_abapgit_ui_factory=>gi_gui_services = ii_gui_services.

  ENDMETHOD.
  METHOD set_html_viewer.

    Lcl_abapgit_ui_factory=>gi_html_viewer = ii_html_viewer.

  ENDMETHOD.
  METHOD set_popups.

    Lcl_abapgit_ui_factory=>gi_popups = ii_popups.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_UI_INJECTOR implementation

*>>>>>>> ZCL_ABAPGIT_ZIP <<<<<<<*

*"* macro definitions
*include zcl_abapgit_zip===============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_zip===============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ZIP implementation.
*"* method's implementations
*include methods.
  METHOD encode_files.

    DATA: lo_zip      TYPE REF TO cl_abap_zip,
          lv_filename TYPE string.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF it_files.


    CREATE OBJECT lo_zip.

    LOOP AT it_files ASSIGNING <ls_file>.
      CONCATENATE <ls_file>-file-path+1 <ls_file>-file-filename INTO lv_filename.
      lo_zip->add( name    = lv_filename
                   content = <ls_file>-file-data ).
    ENDLOOP.

    rv_xstr = lo_zip->save( ).

  ENDMETHOD.
  METHOD export.

    DATA li_log       TYPE REF TO Lif_abapgit_log.
    DATA lt_zip       TYPE Lif_abapgit_definitions=>ty_files_item_tt.
    DATA lo_serialize TYPE REF TO Lcl_abapgit_serialize.

    CREATE OBJECT li_log TYPE Lcl_abapgit_log.
    li_log->set_title( 'Zip Export Log' ).

    IF Lcl_abapgit_factory=>get_sap_package( iv_package )->exists( ) = abap_false.
      Lcx_abapgit_exception=>raise( |Package { iv_package } doesn't exist| ).
    ENDIF.

    CREATE OBJECT lo_serialize
      EXPORTING
        io_dot_abapgit    = io_dot_abapgit
        is_local_settings = is_local_settings.

    lt_zip = lo_serialize->files_local(
      iv_package = iv_package
      ii_log     = li_log
      it_filter  = it_filter ).

    FREE lo_serialize.

    IF li_log->count( ) > 0 AND iv_show_log = abap_true.
      Lcl_abapgit_log_viewer=>show_log( li_log ).
    ENDIF.

    rv_xstr = encode_files( lt_zip ).

  ENDMETHOD.
  METHOD export_object.

    DATA: ls_tadir         TYPE Lif_abapgit_definitions=>ty_tadir,
          lv_folder        TYPE string,
          lv_fullpath      TYPE string,
          lv_sep           TYPE c LENGTH 1,
          ls_files_item    TYPE Lif_abapgit_objects=>ty_serialization,
          lo_frontend_serv TYPE REF TO Lif_abapgit_frontend_services.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF ls_files_item-files.

    ls_tadir = Lcl_abapgit_factory=>get_tadir( )->read_single(
        iv_object   = iv_object_type
        iv_obj_name = iv_object_name ).

    IF ls_tadir IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Object could not be found' ).
    ENDIF.

    ls_files_item-item-obj_type = ls_tadir-object.
    ls_files_item-item-obj_name = ls_tadir-obj_name.

    ls_files_item = Lcl_abapgit_objects=>serialize(
      is_item        = ls_files_item-item
      io_i18n_params = Lcl_abapgit_i18n_params=>new(
        iv_main_language_only = iv_main_language_only
        iv_main_language      = sy-langu ) ).

    IF lines( ls_files_item-files ) = 0.
      Lcx_abapgit_exception=>raise( 'Empty' ).
    ENDIF.

    lo_frontend_serv = Lcl_abapgit_ui_factory=>get_frontend_services( ).
    lo_frontend_serv->directory_browse(
      EXPORTING
        iv_initial_folder  = gv_prev
      CHANGING
        cv_selected_folder = lv_folder ).
    IF lv_folder IS INITIAL.
      RAISE EXCEPTION TYPE Lcx_abapgit_cancel.
    ENDIF.

    gv_prev = lv_folder.
    lo_frontend_serv->get_file_separator( CHANGING cv_file_separator = lv_sep ).

    LOOP AT ls_files_item-files ASSIGNING <ls_file>.
      lv_fullpath = |{ lv_folder }{ lv_sep }{ <ls_file>-filename }|.
      save_binstring_to_localfile( iv_filename  = lv_fullpath
                                   iv_binstring = <ls_file>-data ).

    ENDLOOP.

  ENDMETHOD.
  METHOD export_package.

    DATA: ls_local_settings  TYPE Lif_abapgit_persistence=>ty_repo-local_settings,
          lo_dot_abapgit     TYPE REF TO Lcl_abapgit_dot_abapgit,
          lo_frontend_serv   TYPE REF TO Lif_abapgit_frontend_services,
          lv_default         TYPE string,
          lv_package_escaped TYPE string,
          lv_path            TYPE string,
          lv_zip_xstring     TYPE xstring.

    ls_local_settings-main_language_only = iv_main_lang_only.

    lo_dot_abapgit = Lcl_abapgit_dot_abapgit=>build_default( ).
    lo_dot_abapgit->set_folder_logic( iv_folder_logic ).

    lo_frontend_serv = Lcl_abapgit_ui_factory=>get_frontend_services( ).

    lv_package_escaped = iv_package.
    REPLACE ALL OCCURRENCES OF '/' IN lv_package_escaped WITH '#'.
    lv_default = |{ lv_package_escaped }_{ sy-datlo }_{ sy-timlo }|.

    lv_zip_xstring = export(
     is_local_settings = ls_local_settings
     iv_package        = iv_package
     io_dot_abapgit    = lo_dot_abapgit ).

    lv_path = lo_frontend_serv->show_file_save_dialog(
        iv_title            = 'Package Export'
        iv_extension        = 'zip'
        iv_default_filename = lv_default ).

    lo_frontend_serv->file_download(
        iv_path = lv_path
        iv_xstr = lv_zip_xstring ).
  ENDMETHOD.
  METHOD filename.

    IF iv_str CA '/'.
      FIND REGEX '(.*/)(.*)' IN iv_str
        SUBMATCHES ev_path ev_filename.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'Malformed path' ).
      ENDIF.
      IF ev_path <> '/'.
        CONCATENATE '/' ev_path INTO ev_path.
      ENDIF.
    ELSE.
      ev_path = '/'.
      ev_filename = iv_str.
    ENDIF.
    TRANSLATE ev_filename TO LOWER CASE.

  ENDMETHOD.
  METHOD load.

    rt_files = unzip_file( iv_xstr ).

  ENDMETHOD.
  METHOD normalize_path.
* removes first folder from path if needed

    DATA: lt_split  TYPE TABLE OF string,
          lv_needed TYPE abap_bool,
          lv_length TYPE i,
          lv_split  LIKE LINE OF lt_split.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF ct_files.


    READ TABLE ct_files INDEX 1 ASSIGNING <ls_file>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SPLIT <ls_file>-path AT '/' INTO TABLE lt_split.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    READ TABLE lt_split INDEX 2 INTO lv_split.
    IF sy-subrc <> 0 OR strlen( lv_split ) = 0.
      RETURN.
    ENDIF.

    CONCATENATE '/' lv_split '/*' INTO lv_split.

    lv_needed = abap_true.
    LOOP AT ct_files ASSIGNING <ls_file>.
      IF NOT <ls_file>-path CP lv_split.
        lv_needed = abap_false.
        EXIT. " current loop
      ENDIF.
    ENDLOOP.

    IF lv_needed = abap_true.
      lv_length = strlen( lv_split ) - 2.
      LOOP AT ct_files ASSIGNING <ls_file>.
        <ls_file>-path = <ls_file>-path+lv_length.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD save_binstring_to_localfile.

    Lcl_abapgit_ui_factory=>get_frontend_services( )->file_download(
      iv_path = iv_filename
      iv_xstr = iv_binstring ).

  ENDMETHOD.
  METHOD unzip_file.

    DATA: lo_zip  TYPE REF TO cl_abap_zip,
          lv_data TYPE xstring.

    FIELD-SYMBOLS: <ls_zipfile> LIKE LINE OF lo_zip->files,
                   <ls_file>    LIKE LINE OF rt_files.


    CREATE OBJECT lo_zip.
    lo_zip->load( EXPORTING
                    zip             = iv_xstr
                  EXCEPTIONS
                    zip_parse_error = 1
                    OTHERS          = 2 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from zip' ).
    ENDIF.

    LOOP AT lo_zip->files ASSIGNING <ls_zipfile>.

      lo_zip->get(
        EXPORTING
          name                    = <ls_zipfile>-name
        IMPORTING
          content                 = lv_data
        EXCEPTIONS
          zip_index_error         = 1
          zip_decompression_error = 2
          OTHERS                  = 3 ).
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'error from zip get' ).
      ENDIF.

      APPEND INITIAL LINE TO rt_files ASSIGNING <ls_file>.

      filename(
        EXPORTING
          iv_str      = <ls_zipfile>-name
        IMPORTING
          ev_path     = <ls_file>-path
          ev_filename = <ls_file>-filename ).

      <ls_file>-data = lv_data.

      <ls_file>-sha1 = Lcl_abapgit_hash=>sha1_blob( <ls_file>-data ).

    ENDLOOP.

    DELETE rt_files WHERE filename IS INITIAL.

    normalize_path( CHANGING ct_files = rt_files ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ZIP implementation

*>>>>>>> ZCL_ABAPGIT_STAGE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_stage=============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_stage=============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_STAGE implementation.
*"* method's implementations
*include methods.
  METHOD add.

    append( iv_path     = iv_path
            iv_filename = iv_filename
            iv_method   = Lif_abapgit_definitions=>c_method-add
            is_status   = is_status
            iv_data     = iv_data ).

  ENDMETHOD.
  METHOD append.

    DATA: ls_stage LIKE LINE OF mt_stage.

    FIELD-SYMBOLS: <ls_stage> LIKE LINE OF mt_stage.


    READ TABLE mt_stage WITH KEY
      file-path     = iv_path
      file-filename = iv_filename
      ASSIGNING <ls_stage>.
    IF sy-subrc = 0.
      <ls_stage>-file-data = iv_data.
      <ls_stage>-method    = iv_method.
    ELSE.
      ls_stage-file-path     = iv_path.
      ls_stage-file-filename = iv_filename.
      ls_stage-file-data     = iv_data.
      ls_stage-method        = iv_method.
      ls_stage-status        = is_status.
      INSERT ls_stage INTO TABLE mt_stage.
    ENDIF.

  ENDMETHOD.
  METHOD constructor.
    mv_merge_source = iv_merge_source.
  ENDMETHOD.
  METHOD count.
    rv_count = lines( mt_stage ).
  ENDMETHOD.
  METHOD get_all.
    rt_stage = mt_stage.
  ENDMETHOD.
  METHOD get_merge_source.
    rv_source = mv_merge_source.
  ENDMETHOD.
  METHOD ignore.
    append( iv_path     = iv_path
            iv_filename = iv_filename
            iv_method   = Lif_abapgit_definitions=>c_method-ignore ).
  ENDMETHOD.
  METHOD method_description.

    CASE iv_method.
      WHEN Lif_abapgit_definitions=>c_method-add.
        rv_description = 'add'.
      WHEN Lif_abapgit_definitions=>c_method-rm.
        rv_description = 'remove'.
      WHEN Lif_abapgit_definitions=>c_method-ignore.
        rv_description = 'ignore'.
      WHEN OTHERS.
        Lcx_abapgit_exception=>raise( 'unknown staging method type' ).
    ENDCASE.

  ENDMETHOD.
  METHOD reset.
    DELETE mt_stage WHERE file-path = iv_path AND file-filename = iv_filename.
    ASSERT sy-subrc = 0.
  ENDMETHOD.
  METHOD rm.
    append( iv_path     = iv_path
            iv_filename = iv_filename
            is_status   = is_status
            iv_method   = Lif_abapgit_definitions=>c_method-rm ).
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_STAGE implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SMBC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_smbc=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_smbc=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SMBC implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lo_handler      TYPE REF TO object,
          lo_db_api       TYPE REF TO object,
          lr_data         TYPE REF TO data,
          lv_technical_id TYPE c LENGTH 30.

    FIELD-SYMBOLS: <ls_smbc_config>     TYPE any,
                   <lv_smbc_changed_by> TYPE any.
    TRY.
        CREATE OBJECT lo_handler TYPE ('CL_SMBC_AFF_OBJECT_HANDLER').
        CREATE OBJECT lo_db_api TYPE ('CL_MBC_BUSINESS_CONFIG_DB').
        CREATE DATA lr_data TYPE ('SMBC_CONFIG').
        ASSIGN lr_data->* TO <ls_smbc_config>.
      CATCH cx_sy_create_object_error
            cx_sy_create_data_error.
        Lcx_abapgit_exception=>raise( 'SMBC not supported' ).
    ENDTRY.
    lv_technical_id = ms_item-obj_name.
    CALL METHOD lo_db_api->('IF_MBC_BUSINESS_CONFIG_DB~READ')
      EXPORTING
        iv_technical_id = lv_technical_id
        version         = 'I'
      RECEIVING
        rs_config       = <ls_smbc_config>.
    IF <ls_smbc_config> IS INITIAL.
      CALL METHOD lo_db_api->('IF_MBC_BUSINESS_CONFIG_DB~READ')
        EXPORTING
          iv_technical_id = lv_technical_id
          version         = 'A'
        RECEIVING
          rs_config       = <ls_smbc_config>.
    ENDIF.
    ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <ls_smbc_config> TO <lv_smbc_changed_by>.
    rv_user = <lv_smbc_changed_by>.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SMBC implementation

*>>>>>>> ZCL_ABAPGIT_ITEM_STATE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_item_state========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_item_state========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_item_state========ccau.


class LCL_ABAPGIT_ITEM_STATE implementation.
*"* method's implementations
*include methods.
  METHOD is_reassigned.
    rv_is_reassigned = mv_is_reassigned.
  ENDMETHOD.
  METHOD is_unchanged.
    rv_is_unchanged = boolc( mv_is_reassigned = abap_false
      AND mv_lstate = Lif_abapgit_definitions=>c_state-unchanged
      AND mv_rstate = Lif_abapgit_definitions=>c_state-unchanged ).
  ENDMETHOD.
  METHOD local.
    rv_state = mv_lstate.
  ENDMETHOD.
  METHOD reduce.

    rv_new = iv_prev.
    IF rv_new = iv_cur OR iv_cur IS INITIAL.
      RETURN. " No change
    ELSEIF rv_new IS INITIAL.
      rv_new = iv_cur.
    ELSE.
      rv_new = Lif_abapgit_definitions=>c_state-mixed.
    ENDIF.

  ENDMETHOD.
  METHOD remote.
    rv_state = mv_rstate.
  ENDMETHOD.
  METHOD sum_with_repo_item.

    mv_lstate = reduce(
      iv_prev = mv_lstate
      iv_cur  = is_repo_item-lstate ).
    mv_rstate = reduce(
      iv_prev = mv_rstate
      iv_cur  = is_repo_item-rstate ).
    mv_is_reassigned = boolc( mv_is_reassigned = abap_true OR is_repo_item-packmove = abap_true ).

  ENDMETHOD.
  METHOD sum_with_status_item.

    mv_lstate = reduce(
      iv_prev = mv_lstate
      iv_cur  = is_status_item-lstate ).
    mv_rstate = reduce(
      iv_prev = mv_rstate
      iv_cur  = is_status_item-rstate ).
    mv_is_reassigned = boolc( mv_is_reassigned = abap_true OR is_status_item-packmove = abap_true ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ITEM_STATE implementation

*>>>>>>> ZCL_ABAPGIT_NEWS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_news==============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_news==============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_news==============ccau.
**********************************************************************
* Helper classed

CLASS SHRITEFUH64VYIPO5IWUYB3KW3QSFY DEFINITION FINAL.
  PUBLIC SECTION.
    DATA mt_buffer TYPE string_table.
    METHODS add
      IMPORTING
        iv_str TYPE string.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KW3QSFY IMPLEMENTATION.
  METHOD add.
    APPEND iv_str TO mt_buffer.
  ENDMETHOD.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KW3SSFY DEFINITION FINAL.
  PUBLIC SECTION.
    DATA mt_log_entries TYPE Lcl_abapgit_news=>ty_logs.
    METHODS add
      IMPORTING
        iv_str TYPE string.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KW3SSFY IMPLEMENTATION.
  METHOD add.
    DATA ls_log LIKE LINE OF mt_log_entries.
    DATA lv_pos_to_cur_str TYPE string.

    SPLIT iv_str AT '/' INTO
      ls_log-version
      ls_log-is_header
      ls_log-is_important
      lv_pos_to_cur_str
      ls_log-text.

    CONDENSE ls_log-version.
    CONDENSE ls_log-is_header.
    CONDENSE ls_log-is_important.
    CONDENSE ls_log-text.
    ls_log-pos_to_cur = lv_pos_to_cur_str.

    APPEND ls_log TO mt_log_entries.
  ENDMETHOD.
ENDCLASS.

**********************************************************************

*CLASS SHRITEFUH64VYIPO5IWUYB3KW3USFY DEFINITION DEFERRED.
*CLASS zcl_abapgit_news DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KW3USFY.

*----------------------------------------------------------------------*
*       CLASS SHRITEFUH64VYIPO5IWUYB3KW3USFY DEFINITION
*----------------------------------------------------------------------*
* Definition of test class for news announcement
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS SHRITEFUH64VYIPO5IWUYB3KW3USFY IMPLEMENTATION
*----------------------------------------------------------------------*
* Implementation of test class for news announcement
*----------------------------------------------------------------------*

class LCL_ABAPGIT_NEWS implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    DATA: lt_lines    TYPE string_table,
          lv_string   TYPE string,
          ls_log_line LIKE LINE OF mt_log.

    " Validate params
    mv_current_version  = Lcl_abapgit_version=>normalize( iv_current_version ).
    mv_lastseen_version = Lcl_abapgit_version=>normalize( iv_lastseen_version ).
    IF mv_current_version IS INITIAL.
      RETURN. " Internal format of program version is not correct -> abort parsing
    ENDIF.

    lv_string = Lcl_abapgit_convert=>xstring_to_string_utf8( iv_rawdata ).
    lt_lines  = Lcl_abapgit_convert=>split_string( lv_string ).
    mt_log    = parse( it_lines = lt_lines
                       iv_current_version = mv_current_version ).

    READ TABLE mt_log INTO ls_log_line INDEX 1.
    mv_latest_version = ls_log_line-version. " Empty if not found

  ENDMETHOD.
  METHOD create.

    CONSTANTS: " TODO refactor
      lc_log_path        TYPE string VALUE '/',
      lc_log_filename    TYPE string VALUE 'changelog*',
      lc_log_filename_up TYPE string VALUE 'CHANGELOG*'.

    DATA: lo_apack            TYPE REF TO Lcl_abapgit_apack_reader,
          lt_remote           TYPE Lif_abapgit_git_definitions=>ty_files_tt,
          lv_version          TYPE string,
          lv_last_seen        TYPE string,
          lv_url              TYPE string,
          lo_repo_online      TYPE REF TO Lcl_abapgit_repo_online,
          lv_version_constant TYPE Lif_abapgit_dot_abapgit=>ty_dot_abapgit-version_constant.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF lt_remote.


    IF io_repo->is_offline( ) = abap_true.
      RETURN.
    ENDIF.

    lo_repo_online ?= io_repo.
    lv_url          = lo_repo_online->get_url( ).

    lo_apack = io_repo->get_dot_apack( ).
    IF lo_apack IS BOUND.
      lv_version = lo_apack->get_manifest_descriptor( )-version.
    ENDIF.

    IF lv_version IS INITIAL.
      TRY.
          lv_version_constant = io_repo->get_dot_abapgit( )->get_version_constant( ).
          IF lv_version_constant IS NOT INITIAL.
            lv_version = Lcl_abapgit_version=>get_version_constant_value( lv_version_constant ).
          ENDIF.
        CATCH Lcx_abapgit_exception.
          CLEAR lv_version.
      ENDTRY.
    ENDIF.

    IF lv_version IS INITIAL.
      RETURN.
    ENDIF.

    lv_last_seen = Lcl_abapgit_persistence_user=>get_instance( )->get_repo_last_change_seen( lv_url ).

    TRY. " Find changelog
        lt_remote = io_repo->get_files_remote( ).
      CATCH Lcx_abapgit_exception.
        RETURN.
    ENDTRY.

    LOOP AT lt_remote ASSIGNING <ls_file> WHERE path = lc_log_path
                                            AND ( filename CP lc_log_filename OR filename CP lc_log_filename_up ).

      CREATE OBJECT ro_instance
        EXPORTING
          iv_rawdata          = <ls_file>-data
          iv_current_version  = lv_version
          iv_lastseen_version = Lcl_abapgit_version=>normalize( lv_last_seen ).

      EXIT.

    ENDLOOP.

    IF ro_instance IS BOUND AND lv_last_seen <> ro_instance->latest_version( ).
      Lcl_abapgit_persistence_user=>get_instance( )->set_repo_last_change_seen(
        iv_url     = lv_url
        iv_version = ro_instance->latest_version( ) ).
    ENDIF.

  ENDMETHOD.
  METHOD get_log.
    rt_log = mt_log.
  ENDMETHOD.
  METHOD has_important.
    READ TABLE mt_log WITH KEY is_important = abap_true TRANSPORTING NO FIELDS.
    rv_boolean = boolc( sy-subrc IS INITIAL ).
  ENDMETHOD.
  METHOD has_news.
    rv_boolean = boolc( lines( mt_log ) > 0 ).
  ENDMETHOD.
  METHOD has_unseen.
    rv_boolean = boolc( Lcl_abapgit_version=>compare(
      iv_a = mv_latest_version
      iv_b = mv_lastseen_version ) > 0 ).
  ENDMETHOD.
  METHOD has_updates.
    rv_boolean = boolc( Lcl_abapgit_version=>compare(
      iv_a = mv_latest_version
      iv_b = mv_current_version ) > 0 ).
  ENDMETHOD.
  METHOD latest_version.
    rv_version = mv_latest_version.
  ENDMETHOD.
  METHOD parse.

    DATA: lv_tail                TYPE i,
          lv_first_version_found TYPE abap_bool,
          lv_version             TYPE string,
          ls_log                 LIKE LINE OF rt_log.

    FIELD-SYMBOLS: <lv_line> LIKE LINE OF it_lines.


    LOOP AT it_lines ASSIGNING <lv_line>.
      ls_log = parse_line( iv_line = <lv_line>
                           iv_current_version = iv_current_version ).

      " Skip until first version head and Skip empty lines
      CHECK ls_log IS NOT INITIAL AND
            ( lv_first_version_found = abap_true OR ls_log-version IS NOT INITIAL ).

      IF lv_first_version_found = abap_false.
        lv_first_version_found = abap_true.
        IF Lcl_abapgit_version=>compare( iv_a = ls_log-version
                                         iv_b = iv_current_version ) <= 0.
          lv_tail = c_tail_length. " Display some last versions if no updates
        ENDIF.
      ENDIF.

      IF ls_log-is_header = abap_true.
        "Skip everything below current version or show tail news
        IF Lcl_abapgit_version=>compare( iv_a = ls_log-version
                                         iv_b = iv_current_version ) <= 0.
          IF lv_tail > 0.
            lv_tail = lv_tail - 1.
          ELSE.
            EXIT.
          ENDIF.
        ENDIF.
        lv_version = ls_log-version. " Save to fill news lines
      ELSE.
        ls_log-version = lv_version.
      ENDIF.

      APPEND ls_log TO rt_log.
    ENDLOOP.

  ENDMETHOD.
  METHOD parse_line.

    CONSTANTS: lc_header_pattern TYPE string
        VALUE '^\d{4}-\d{2}-\d{2}\s+v(\d{1,3}\.\d{1,3}\.\d{1,3})\s*$'.

    DATA: lv_version TYPE string.

    IF iv_line IS INITIAL OR iv_line CO ' -='.
      RETURN. " Skip empty and markup lines
    ENDIF.

    " Check if line is a header line
    FIND FIRST OCCURRENCE OF REGEX lc_header_pattern IN iv_line SUBMATCHES lv_version.
    IF sy-subrc IS INITIAL.
      lv_version        = Lcl_abapgit_version=>normalize( lv_version ).
      rs_log-version    = lv_version.
      rs_log-is_header  = abap_true.
      rs_log-pos_to_cur = Lcl_abapgit_version=>compare( iv_a = lv_version
                                                        iv_b = iv_current_version ).
    ELSE.
      FIND FIRST OCCURRENCE OF REGEX '^\s*!' IN iv_line.
      rs_log-is_important = boolc( sy-subrc IS INITIAL ). " Change is important
    ENDIF.

    rs_log-text = iv_line.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_NEWS implementation

*>>>>>>> ZCL_ABAPGIT_URL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_url===============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_url===============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_url===============ccau.



class LCL_ABAPGIT_URL implementation.
*"* method's implementations
*include methods.
  METHOD host.

    regex( EXPORTING iv_url = iv_url
           IMPORTING ev_host = rv_host ).

  ENDMETHOD.
  METHOD is_abapgit_repo.

    IF iv_url CS 'github.com' AND ( iv_url CP '*/abapGit' OR iv_url CP '*/abapGit.git' ).
      rv_abapgit = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD name.

    DATA: lv_path TYPE string.

    TRY.
        regex( EXPORTING iv_url = iv_url
               IMPORTING ev_name = rv_name
                         ev_path = lv_path ).

        IF rv_name IS INITIAL.
          FIND REGEX '([\w-]+)/$' IN lv_path SUBMATCHES rv_name.
          IF sy-subrc <> 0.
            Lcx_abapgit_exception=>raise( 'Malformed URL' ).
          ENDIF.
        ENDIF.

      CATCH Lcx_abapgit_exception.
        IF iv_validate = abap_true.
          Lcx_abapgit_exception=>raise( 'Malformed URL' ).
        ELSE.
          rv_name = 'URL error (fix repo with "Advanced > Change Remote")'.
        ENDIF.
    ENDTRY.

  ENDMETHOD.
  METHOD path_name.

    DATA: lv_host TYPE string ##NEEDED.

    FIND REGEX '(.*://[^/]*)(.*)' IN iv_url
      SUBMATCHES lv_host rv_path_name.

  ENDMETHOD.
  METHOD regex.

    FIND REGEX '^(https?://[^/]*)(.*/)(.*)\.git$' IN iv_url
      SUBMATCHES ev_host ev_path ev_name.
    IF sy-subrc <> 0.
      FIND REGEX '^(https?://[^/]*)(.*/)(.*)$' IN iv_url
        SUBMATCHES ev_host ev_path ev_name.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'Malformed URL' ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD url_address.

    DATA:
      lv_host TYPE string,
      lv_path TYPE string,
      lv_name TYPE string,
      lv_len  TYPE i.

    regex( EXPORTING iv_url  = iv_url
           IMPORTING ev_host = lv_host
                     ev_path = lv_path
                     ev_name = lv_name ).

    IF lv_path IS INITIAL AND lv_name IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Malformed URL' ).
    ELSEIF lv_name IS INITIAL.
      lv_len = strlen( lv_path ) - 1.
      IF lv_path+lv_len(1) = '/'.
        lv_path = lv_path(lv_len).
      ENDIF.
    ENDIF.

    rv_adress = |{ lv_host }{ lv_path }{ lv_name }|.

  ENDMETHOD.
  METHOD validate.

    name( iv_url      = iv_url
          iv_validate = abap_true ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_URL implementation

*>>>>>>> ZCL_ABAPGIT_VERSION <<<<<<<*

*"* macro definitions
*include zcl_abapgit_version===========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_version===========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_version===========ccau.
*CLASS SHRITEFUH64VYIPO5IWUYCIJCOYSFY DEFINITION DEFERRED.
*CLASS zcl_abapgit_version DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYCIJCOYSFY.



class LCL_ABAPGIT_VERSION implementation.
*"* method's implementations
*include methods.
  METHOD check_dependant_version.

    CONSTANTS: lc_message TYPE string VALUE 'Current version is older than required'.

    IF is_dependant-major > is_current-major.
      Lcx_abapgit_exception=>raise( lc_message ).
    ELSEIF is_dependant-major < is_current-major.
      RETURN.
    ENDIF.

    IF is_dependant-minor > is_current-minor.
      Lcx_abapgit_exception=>raise( lc_message ).
    ELSEIF is_dependant-minor < is_current-minor.
      RETURN.
    ENDIF.

    IF is_dependant-patch > is_current-patch.
      Lcx_abapgit_exception=>raise( lc_message ).
    ELSEIF is_dependant-patch < is_current-patch.
      RETURN.
    ENDIF.

    IF is_current-prerelase IS INITIAL.
      RETURN.
    ENDIF.

    CASE is_current-prerelase.
      WHEN 'rc'.
        IF is_dependant-prerelase = ''.
          Lcx_abapgit_exception=>raise( lc_message ).
        ENDIF.

      WHEN 'beta'.
        IF is_dependant-prerelase = '' OR is_dependant-prerelase = 'rc'.
          Lcx_abapgit_exception=>raise( lc_message ).
        ENDIF.

      WHEN 'alpha'.
        IF is_dependant-prerelase = '' OR is_dependant-prerelase = 'rc' OR is_dependant-prerelase = 'beta'.
          Lcx_abapgit_exception=>raise( lc_message ).
        ENDIF.

    ENDCASE.

    IF is_dependant-prerelase = is_current-prerelase AND is_dependant-prerelase_patch > is_current-prerelase_patch.
      Lcx_abapgit_exception=>raise( lc_message ).
    ENDIF.

  ENDMETHOD.
  METHOD compare.

    DATA: ls_version_a TYPE Lif_abapgit_definitions=>ty_version,
          ls_version_b TYPE Lif_abapgit_definitions=>ty_version.

    TRY.
        IF is_a IS NOT INITIAL.
          ls_version_a = is_a.
        ELSE.
          ls_version_a = conv_str_to_version( iv_a ).
        ENDIF.

        IF is_b IS NOT INITIAL.
          ls_version_b = is_b.
        ELSE.
          ls_version_b = conv_str_to_version( iv_b ).
        ENDIF.
      CATCH Lcx_abapgit_exception.
        rv_result = 0.
        RETURN.
    ENDTRY.

    IF ls_version_a = ls_version_b.
      rv_result = 0.
    ELSE.
      TRY.
          check_dependant_version( is_current   = ls_version_a
                                   is_dependant = ls_version_b ).
          rv_result = 1.
        CATCH Lcx_abapgit_exception.
          rv_result = -1.
          RETURN.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD conv_str_to_version.

    DATA: lt_segments TYPE STANDARD TABLE OF string,
          lt_parts    TYPE STANDARD TABLE OF string,
          lv_segment  TYPE string.

    SPLIT iv_version AT '-' INTO TABLE lt_segments.

    READ TABLE lt_segments INTO lv_segment INDEX 1. " Version
    IF sy-subrc <> 0.   " No version
      RETURN.
    ENDIF.

    SPLIT lv_segment AT '.' INTO TABLE lt_parts.

    LOOP AT lt_parts INTO lv_segment.

      TRY.
          CASE sy-tabix.
            WHEN 1.
              rs_version-major = lv_segment.
            WHEN 2.
              rs_version-minor = lv_segment.
            WHEN 3.
              rs_version-patch = lv_segment.
          ENDCASE.
        CATCH cx_sy_conversion_no_number.
          Lcx_abapgit_exception=>raise( 'Incorrect format for Semantic Version' ).
      ENDTRY.

    ENDLOOP.

    READ TABLE lt_segments INTO lv_segment INDEX 2. " Pre-release Version
    IF sy-subrc <> 0.   " No version
      RETURN.
    ENDIF.

    SPLIT lv_segment AT '.' INTO TABLE lt_parts.

    LOOP AT lt_parts INTO lv_segment.

      CASE sy-tabix.
        WHEN 1.
          rs_version-prerelase = lv_segment.
          TRANSLATE rs_version-prerelase TO LOWER CASE.
        WHEN 2.
          rs_version-prerelase_patch = lv_segment.
      ENDCASE.

    ENDLOOP.

    IF rs_version-prerelase <> 'rc' AND rs_version-prerelase <> 'beta' AND rs_version-prerelase <> 'alpha'.
      Lcx_abapgit_exception=>raise( 'Incorrect format for Semantic Version' ).
    ENDIF.

  ENDMETHOD.
  METHOD get_version_constant_value.
    DATA: lv_version_class     TYPE string,
          lv_version_component TYPE string.
    FIELD-SYMBOLS: <lv_version> TYPE string.

    IF iv_version_constant NP '*=>*'.
      Lcx_abapgit_exception=>raise( 'Version constant needs to use the format CLASS=>CONSTANT' ).
    ENDIF.

    SPLIT iv_version_constant AT '=>' INTO lv_version_class lv_version_component.
    IF sy-subrc <> 0 OR lv_version_class IS INITIAL OR lv_version_component IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Version constant cannot be parsed' ).
    ENDIF.

    ASSIGN (lv_version_class)=>(lv_version_component) TO <lv_version>.
    IF sy-subrc = 0.
      rv_version = <lv_version>.
    ELSE.
      Lcx_abapgit_exception=>raise( |Could not access version at class { lv_version_class } component | &&
                                    |{ lv_version_component }| ).
    ENDIF.
  ENDMETHOD.
  METHOD normalize.

    " Internal program version should be in format "XXX.XXX.XXX" or "vXXX.XXX.XXX"
    CONSTANTS:
      lc_version_pattern    TYPE string VALUE '^v?(\d{1,3}\.\d{1,3}\.\d{1,3})\s*$',
      lc_prerelease_pattern TYPE string VALUE '^((rc|beta|alpha)\.\d{1,3})\s*$'.

    DATA: lv_version      TYPE string,
          lv_prerelease   TYPE string,
          lv_version_n    TYPE string,
          lv_prerelease_n TYPE string.

    SPLIT iv_version AT '-' INTO lv_version lv_prerelease.

    FIND FIRST OCCURRENCE OF REGEX lc_version_pattern
      IN lv_version SUBMATCHES lv_version_n.

    IF lv_prerelease IS NOT INITIAL.

      FIND FIRST OCCURRENCE OF REGEX lc_prerelease_pattern
        IN lv_prerelease SUBMATCHES lv_prerelease_n.

    ENDIF.

    IF lv_version_n IS INITIAL.
      RETURN.
    ENDIF.

    rv_version = lv_version_n.

    IF lv_prerelease_n IS NOT INITIAL.
      CONCATENATE rv_version '-' lv_prerelease_n INTO rv_version.
    ENDIF.

  ENDMETHOD.
  METHOD version_to_numeric.

    DATA: lv_major   TYPE n LENGTH 4,
          lv_minor   TYPE n LENGTH 4,
          lv_release TYPE n LENGTH 4.

    SPLIT iv_version AT '.' INTO lv_major lv_minor lv_release.

    " Calculated value of version number, empty version will become 0 which is OK
    rv_version = lv_major * 1000000 + lv_minor * 1000 + lv_release.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_VERSION implementation

*>>>>>>> ZCL_ABAPGIT_PROGRESS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_progress==========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_progress==========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_PROGRESS implementation.
*"* method's implementations
*include methods.
  METHOD calc_pct.

    DATA: lv_f TYPE f.

    lv_f = ( iv_current / mv_total ) * 100.
    rv_pct = lv_f.

    IF rv_pct = 100.
      rv_pct = 99.
    ELSEIF rv_pct = 0.
      rv_pct = 1.
    ENDIF.

  ENDMETHOD.
  METHOD get_instance.

* max one progress indicator at a time is supported

    IF gi_progress IS INITIAL.
      CREATE OBJECT gi_progress TYPE Lcl_abapgit_progress.
    ENDIF.

    gi_progress->set_total( iv_total ).

    ri_progress = gi_progress.

  ENDMETHOD.
  METHOD set_instance.

    gi_progress = ii_progress.

  ENDMETHOD.
  METHOD Lif_abapgit_progress~off.

    " Clear the status bar
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'.

  ENDMETHOD.
  METHOD Lif_abapgit_progress~set_total.

    mv_total = iv_total.

    CLEAR mv_cv_time_next.
    CLEAR mv_cv_datum_next.

  ENDMETHOD.
  METHOD Lif_abapgit_progress~show.

    DATA: lv_pct  TYPE i,
          lv_time TYPE t.

    CONSTANTS: lc_wait_secs TYPE i VALUE 2.

    GET TIME.
    lv_time = sy-uzeit.
    IF mv_cv_time_next IS INITIAL AND mv_cv_datum_next IS INITIAL.
      mv_cv_time_next  = lv_time.
      mv_cv_datum_next = sy-datum.
    ENDIF.

    "We only do a progress indication if enough time has passed
    IF lv_time >= mv_cv_time_next
        AND sy-datum = mv_cv_datum_next
        OR sy-datum > mv_cv_datum_next.

      lv_pct = calc_pct( iv_current ).

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = lv_pct
          text       = iv_text.
      mv_cv_time_next = lv_time + lc_wait_secs.

    ENDIF.
    IF sy-datum > mv_cv_datum_next.
      mv_cv_datum_next = sy-datum.
    ENDIF.
    IF mv_cv_time_next < lv_time.
      mv_cv_datum_next = sy-datum + 1.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_PROGRESS implementation

*>>>>>>> ZCL_ABAPGIT_ZLIB_HUFFMAN <<<<<<<*

*"* macro definitions
*include zcl_abapgit_zlib_huffman======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_zlib_huffman======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_zlib_huffman======ccau.



class LCL_ABAPGIT_ZLIB_HUFFMAN implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    DATA: lv_index  TYPE i,
          lt_offset TYPE TABLE OF i,
          lv_length LIKE LINE OF it_lengths,
          lv_prev   TYPE i,
          lv_count  LIKE LINE OF mt_count.

    FIELD-SYMBOLS: <lv_offset> LIKE LINE OF lt_offset,
                   <lv_symbol> LIKE LINE OF mt_symbol,
                   <lv_i>      LIKE LINE OF it_lengths.


    DO c_maxbits TIMES.
      APPEND 0 TO mt_count.
    ENDDO.
    LOOP AT it_lengths INTO lv_index.
      IF lv_index = 0.
        CONTINUE.
      ENDIF.
      READ TABLE mt_count INDEX lv_index ASSIGNING <lv_i>.
      ASSERT sy-subrc = 0.
      <lv_i> = <lv_i> + 1.
    ENDLOOP.

************

    APPEND 0 TO lt_offset.
    DO c_maxbits - 1 TIMES.
      READ TABLE mt_count INDEX sy-index INTO lv_count.
      ASSERT sy-subrc = 0.
      lv_prev = lv_prev + lv_count.
      APPEND lv_prev TO lt_offset.
    ENDDO.

    DO lines( it_lengths ) TIMES.
      APPEND 0 TO mt_symbol.
    ENDDO.
    DO lines( it_lengths ) TIMES.
      lv_index = sy-index.
      READ TABLE it_lengths INDEX lv_index INTO lv_length.
      ASSERT sy-subrc = 0.
      IF lv_length = 0.
        CONTINUE.
      ENDIF.
      READ TABLE lt_offset INDEX lv_length ASSIGNING <lv_offset>.
      ASSERT sy-subrc = 0.
      READ TABLE mt_symbol INDEX <lv_offset> + 1 ASSIGNING <lv_symbol>.
      ASSERT sy-subrc = 0.
      <lv_symbol> = lv_index - 1.
      <lv_offset> = <lv_offset> + 1.
    ENDDO.

  ENDMETHOD.
  METHOD get_count.
    READ TABLE mt_count INDEX iv_index INTO rv_value.     "#EC CI_SUBRC
  ENDMETHOD.
  METHOD get_symbol.
    READ TABLE mt_symbol INDEX iv_index INTO rv_value.    "#EC CI_SUBRC
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ZLIB_HUFFMAN implementation

*>>>>>>> ZCL_ABAPGIT_ZLIB_STREAM <<<<<<<*

*"* macro definitions
*include zcl_abapgit_zlib_stream=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_zlib_stream=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_zlib_stream=======ccau.




class LCL_ABAPGIT_ZLIB_STREAM implementation.
*"* method's implementations
*include methods.
  METHOD clear_bits.
    CLEAR mv_bits.
  ENDMETHOD.
  METHOD constructor.

    mv_compressed = iv_data.

  ENDMETHOD.
  METHOD remaining.

    rv_length = xstrlen( mv_compressed ) + 1.

  ENDMETHOD.
  METHOD take_bits.

    DATA: lv_left  TYPE i,
          lv_index TYPE i,
          lv_x     TYPE x LENGTH 1.


    WHILE strlen( rv_bits ) < iv_length.
      IF mv_bits IS INITIAL.
        lv_x = mv_compressed(1).
        mv_bits = Lcl_abapgit_zlib_convert=>hex_to_bits( lv_x ).
        mv_compressed = mv_compressed+1.
      ENDIF.
      lv_left = iv_length - strlen( rv_bits ).
      IF lv_left >= strlen( mv_bits ).
        CONCATENATE mv_bits rv_bits INTO rv_bits.
        CLEAR mv_bits.
      ELSE.
        lv_index = strlen( mv_bits ) - lv_left.
        CONCATENATE mv_bits+lv_index(lv_left) rv_bits INTO rv_bits.
        mv_bits = mv_bits(lv_index).
      ENDIF.

    ENDWHILE.

  ENDMETHOD.
  METHOD take_bytes.

    rv_bytes = mv_compressed(iv_length).
    mv_compressed = mv_compressed+iv_length.

  ENDMETHOD.
  METHOD take_int.

    rv_int = Lcl_abapgit_zlib_convert=>bits_to_int( take_bits( iv_length ) ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ZLIB_STREAM implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_XINX <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_xinx=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_xinx=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_XINX implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    cl_wb_object_type=>get_key_components_from_id(
      EXPORTING
        p_key                   = |{ ms_item-obj_name }|
        p_external_id           = swbm_c_type_ddic_db_tabxinx
      IMPORTING
        p_key_component1        = mv_name
        p_key_component2        = mv_id
      EXCEPTIONS
        too_many_key_components = 1
        objecttype_not_existing = 2
        OTHERS                  = 3 ).

    ASSERT sy-subrc = 0.

  ENDMETHOD.
  METHOD xinx_delete_docu.

    DATA: lv_docuid  TYPE dokhl-id,
          lv_doctype TYPE dokhl-typ,
          lv_docname TYPE dokhl-object.

    lv_docname    = iv_objname.
    lv_docname+30 = iv_id.
    CALL FUNCTION 'INTERN_DD_DOCU_ID_MATCH'
      EXPORTING
        p_trobjtype  = c_objtype_extension_index
      IMPORTING
        p_docu_id    = lv_docuid
        p_doctype    = lv_doctype
      EXCEPTIONS
        illegal_type = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'DOKU_DELETE_ALL'
      EXPORTING
        doku_id            = lv_docuid
        doku_object        = lv_docname
        doku_typ           = lv_doctype
        suppress_authority = 'X'
        suppress_enqueue   = 'X'
        suppress_transport = 'X'
      EXCEPTIONS
        no_docu_found      = 1
        OTHERS             = 2.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    SELECT SINGLE as4user FROM dd12l INTO rv_user
      WHERE sqltab = mv_name AND indexname = mv_id.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    " Reimplement FM RS_DD_INDX_DELETE as it calls the UI

    DATA: ls_enqueue      TYPE ddenqs,
          lv_protname     TYPE tstrf01-file,
          lv_del_concname LIKE ls_enqueue-objname,
          lv_concname     TYPE rsdxx-objname,
          ls_transp_key   TYPE trkey,
          ls_e071         TYPE e071,
          lv_clm_corrnum  TYPE e070-trkorr.

    CONCATENATE mv_name '-' mv_id INTO lv_concname.
    ls_enqueue-objtype = c_objtype_extension_index.

    CALL FUNCTION 'INT_INDX_DEL_LOCK'
      EXPORTING
        i_trobjtype        = ls_enqueue-objtype
        i_tabname          = mv_name
        i_indexname        = mv_id
      EXCEPTIONS
        not_executed       = 1
        error_occured      = 2
        permission_failure = 3
        OTHERS             = 4.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    ls_enqueue-objname = mv_name.
    ls_enqueue-secname = mv_id.
    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object        = ls_enqueue
        object_class  = 'DICT'
        mode          = 'DELETE'
      IMPORTING
        transport_key = ls_transp_key
      EXCEPTIONS
        OTHERS        = 1.

    IF sy-subrc <> 0.
      " & was not deleted (correction entry not possible or canceled)
      MESSAGE s015(e2) WITH lv_concname INTO Lcx_abapgit_exception=>null.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'DD_LOGNPROT_NAME_GET'
      EXPORTING
        task        = 'DEL'
        obj_type    = ls_enqueue-objtype
        obj_name    = ls_enqueue-objname
        ind_name    = ls_enqueue-secname
      IMPORTING
        protname    = lv_protname
      EXCEPTIONS
        input_error = 0.

    PERFORM logdelete IN PROGRAM rddu0001 USING lv_protname.

    lv_del_concname = ls_enqueue-objname.
    lv_del_concname+16 = ls_enqueue-secname.

    CALL FUNCTION 'DD_OBJ_DEL'
      EXPORTING
        object_name = lv_del_concname
        object_type = ls_enqueue-objtype
        del_state   = 'M'
      EXCEPTIONS
        OTHERS      = 1.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'DD_DD_TO_E071'
      EXPORTING
        type          = ls_enqueue-objtype
        name          = ls_enqueue-objname
        id            = ls_enqueue-secname
      IMPORTING
        obj_name      = ls_e071-obj_name
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      " Internal error & in & (contact person in charge)
      MESSAGE i008(e2) WITH 'DD_DD_TO_E071' 'RS_DD_INDX_DELETE' INTO Lcx_abapgit_exception=>null.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    ls_e071-object = ls_enqueue-objtype.

    CALL FUNCTION 'RS_DELETE_FROM_WORKING_AREA'
      EXPORTING
        object                 = ls_e071-object
        obj_name               = ls_e071-obj_name
        immediate              = 'X'
        actualize_working_area = 'X'.

    xinx_delete_docu(
      iv_objname = mv_name
      iv_id      = mv_id ).

    CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
      EXPORTING
        object    = ls_e071-obj_name
        operation = 'DELETE'
        type      = c_objtype_extension_index.

    IF mv_id(1) CA 'YZ'.
      CALL FUNCTION 'CLM_INDX_MODIFICATION_DELETE'
        EXPORTING
          idxobj_name   = ls_enqueue-objname
          idx_type      = ls_enqueue-objtype
          idx_name      = mv_id
          transport_key = ls_transp_key
          corrnum       = lv_clm_corrnum.
    ENDIF.

    CALL FUNCTION 'RS_DD_DEQUEUE'
      EXPORTING
        objtype = ls_enqueue-objtype
        objname = ls_enqueue-objname
        secname = ls_enqueue-secname.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_extension_index TYPE ty_extension_index,
          lv_rc              TYPE sy-subrc.

    io_xml->read(
      EXPORTING
        iv_name = 'XINX'
      CHANGING
        cg_data = ls_extension_index ).

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

    CALL FUNCTION 'DDIF_INDX_PUT'
      EXPORTING
        name              = mv_name
        id                = mv_id
        dd12v_wa          = ls_extension_index-dd12v
      TABLES
        dd17v_tab         = ls_extension_index-t_dd17v
      EXCEPTIONS
        indx_not_found    = 1
        name_inconsistent = 2
        indx_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from DDIF_INDX_PUT { sy-subrc }| ).
    ENDIF.

    CALL FUNCTION 'DDIF_INDX_ACTIVATE'
      EXPORTING
        name        = mv_name
        id          = mv_id
      IMPORTING
        rc          = lv_rc
      EXCEPTIONS
        not_found   = 1
        put_failure = 2
        OTHERS      = 3.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from DDIF_INDX_ACTIVATE { sy-subrc }| ).
    ENDIF.

    IF lv_rc <> 0.
      Lcx_abapgit_exception=>raise( |Cannot activate extension index { mv_id } of table { mv_name }| ).
    ENDIF.

    deserialize_longtexts( ii_xml         = io_xml
                           iv_longtext_id = c_longtext_id_xinx ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: ls_dd12v TYPE dd12v.

    CALL FUNCTION 'DDIF_INDX_GET'
      EXPORTING
        name          = mv_name
        id            = mv_id
      IMPORTING
        dd12v_wa      = ls_dd12v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    rv_bool = boolc( ls_dd12v IS NOT INITIAL ).

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

    DATA: ls_extension_index TYPE ty_extension_index.

    CALL FUNCTION 'DDIF_INDX_GET'
      EXPORTING
        name          = mv_name
        id            = mv_id
        langu         = mv_language
      IMPORTING
        dd12v_wa      = ls_extension_index-dd12v
      TABLES
        dd17v_tab     = ls_extension_index-t_dd17v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from DDIF_INDX_GET { sy-subrc }| ).
    ENDIF.

    CLEAR: ls_extension_index-dd12v-as4user,
           ls_extension_index-dd12v-as4date,
           ls_extension_index-dd12v-as4time.

    io_xml->add( iv_name = 'XINX'
                 ig_data = ls_extension_index ).

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_xinx ).

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
endclass. "ZCL_ABAPGIT_OBJECT_XINX implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_XSLT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_xslt=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_xslt=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_XSLT implementation.
*"* method's implementations
*include methods.
  METHOD get.

    DATA: lv_name TYPE cxsltdesc.


    lv_name = ms_item-obj_name.

    cl_o2_api_xsltdesc=>load(
      EXPORTING
        p_xslt_desc        = lv_name
      IMPORTING
        p_obj              = ro_xslt
      EXCEPTIONS
        not_existing       = 1
        permission_failure = 2
        OTHERS             = 3 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from cl_o2_api_xsltdesc=>load' ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lo_xslt       TYPE REF TO cl_o2_api_xsltdesc,
          ls_attributes TYPE o2xsltattr.

    lo_xslt = get( ).
    lo_xslt->get_attributes(
      RECEIVING
        p_attributes     = ls_attributes
      EXCEPTIONS
        object_invalid   = 1
        xsltdesc_deleted = 2
        OTHERS           = 3 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    rv_user = ls_attributes-changedby.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lo_xslt TYPE REF TO cl_o2_api_xsltdesc,
          lv_name TYPE cxsltdesc.


    lv_name = ms_item-obj_name.

    cl_o2_api_xsltdesc=>load(
      EXPORTING
        p_xslt_desc        = lv_name
      IMPORTING
        p_obj              = lo_xslt
      EXCEPTIONS
        error_occured      = 1
        not_existing       = 2
        permission_failure = 3
        version_not_found  = 4
        OTHERS             = 5 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from cl_o2_api_xsltdesc=>load' ).
    ENDIF.

    lo_xslt->set_changeable( abap_true ).
    lo_xslt->delete( ).
    lo_xslt->save( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_source     TYPE string,
          lo_xslt       TYPE REF TO cl_o2_api_xsltdesc,
          lv_len        TYPE i,
          ls_attributes TYPE o2xsltattr.

    " Transformation might depend on other objects like a class
    " We attempt to activate it in late step
    IF iv_step = Lif_abapgit_object=>gc_step_id-late.
      IF Lif_abapgit_object~is_active( ) = abap_false.
        Lcl_abapgit_objects_activation=>add_item( ms_item ).
      ENDIF.
      RETURN.
    ENDIF.

    IF Lif_abapgit_object~exists( ) = abap_true.
      Lif_abapgit_object~delete( iv_package   = iv_package
                                 iv_transport = iv_transport ).
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'ATTRIBUTES'
                  CHANGING cg_data = ls_attributes ).

    ls_attributes-devclass = iv_package.

    lv_source = Lif_abapgit_object~mo_files->read_string(
      iv_extra = 'source'
      iv_ext   = 'xml' ).

* workaround: somewhere additional linefeeds are added
    lv_len = strlen( lv_source ) - 2.
    IF lv_source+lv_len(2) = cl_abap_char_utilities=>cr_lf.
      lv_source = lv_source(lv_len).
    ENDIF.

    cl_o2_api_xsltdesc=>create_new_from_string(
      EXPORTING
        p_source                = lv_source
        p_attr                  = ls_attributes
      IMPORTING
        p_obj                   = lo_xslt
      EXCEPTIONS
        action_cancelled        = 1
        error_occured           = 2
        not_authorized          = 3
        object_already_existing = 4
        undefined_name          = 5
        OTHERS                  = 6 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from XSLT new, { sy-subrc }| ).
    ENDIF.

    lo_xslt->save(
      EXCEPTIONS
        action_cancelled      = 1
        error_occured         = 2
        object_invalid        = 3
        object_not_changeable = 4
        permission_failure    = 5
        OTHERS                = 6 ).
    IF sy-subrc <> 0.
      lo_xslt->set_changeable( abap_false ). " unlock
      Lcx_abapgit_exception=>raise( |Error from XSLT save, { sy-subrc }| ).
    ENDIF.

    lo_xslt->set_changeable( abap_false ).

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_name TYPE cxsltdesc.

    lv_name = ms_item-obj_name.

    rv_bool = cl_o2_api_xsltdesc=>exists( lv_name ).
    rv_bool = boolc( rv_bool = '1' ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
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
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lo_xslt       TYPE REF TO cl_o2_api_xsltdesc,
          lv_source     TYPE string,
          ls_attributes TYPE o2xsltattr.


    lo_xslt = get( ).

    ls_attributes = lo_xslt->get_attributes( ).

    CLEAR: ls_attributes-author,
           ls_attributes-createdon,
           ls_attributes-changedby,
           ls_attributes-changedon,
           ls_attributes-devclass.

    io_xml->add( iv_name = 'ATTRIBUTES'
                 ig_data = ls_attributes ).

    lv_source = lo_xslt->get_source_string( ).

    Lif_abapgit_object~mo_files->add_string(
      iv_extra  = 'source'
      iv_ext    = 'xml'
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
endclass. "ZCL_ABAPGIT_OBJECT_XSLT implementation

*>>>>>>> ZCL_ABAPGIT_PERSISTENCE_REPO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_persistence_repo==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_persistence_repo==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_PERSISTENCE_REPO implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    DATA ls_dummy_meta_mask TYPE Lif_abapgit_persistence=>ty_repo_meta_mask.
    DATA ls_dummy_meta      TYPE Lif_abapgit_persistence=>ty_repo_xml.
    DATA lo_type_meta_mask  TYPE REF TO cl_abap_structdescr.
    DATA lo_type_meta       TYPE REF TO cl_abap_structdescr.
    FIELD-SYMBOLS <ls_comp> LIKE LINE OF lo_type_meta_mask->components.

    " Collect actual list of fields in repo meta data (used in update_meta)
    lo_type_meta_mask ?= cl_abap_structdescr=>describe_by_data( ls_dummy_meta_mask ).
    lo_type_meta      ?= cl_abap_structdescr=>describe_by_data( ls_dummy_meta ).
    LOOP AT lo_type_meta_mask->components ASSIGNING <ls_comp>.
      APPEND <ls_comp>-name TO mt_meta_fields.
    ENDLOOP.

    mo_db = Lcl_abapgit_persistence_db=>get_instance( ).

  ENDMETHOD.
  METHOD from_xml.

    DATA: lv_xml TYPE string.

    lv_xml = iv_repo_xml_string.

* fix downward compatibility
    REPLACE ALL OCCURRENCES OF '<_--28C_TYPE_REPO_--29>' IN lv_xml WITH '<REPO>'.
    REPLACE ALL OCCURRENCES OF '</_--28C_TYPE_REPO_--29>' IN lv_xml WITH '</REPO>'.

    CALL TRANSFORMATION id
      OPTIONS value_handling = 'accept_data_loss'
      SOURCE XML lv_xml
      RESULT repo = rs_repo.

* automatic migration of old fields
* todo, keep for transition period until 2022-12-31, then remove all of these
    FIND FIRST OCCURRENCE OF '</HEAD_BRANCH><WRITE_PROTECT>X</WRITE_PROTECT>' IN lv_xml.
    IF sy-subrc = 0.
      rs_repo-local_settings-write_protected = abap_true.
    ENDIF.
    FIND FIRST OCCURRENCE OF '<IGNORE_SUBPACKAGES>X</IGNORE_SUBPACKAGES></REPO>' IN lv_xml.
    IF sy-subrc = 0.
      rs_repo-local_settings-ignore_subpackages = abap_true.
    ENDIF.
    FIND FIRST OCCURRENCE OF '<SERIALIZE_MASTER_LANG_ONLY>X</SERIALIZE_MASTER_LANG_ONLY>' IN lv_xml.
    IF sy-subrc = 0.
      rs_repo-local_settings-main_language_only = abap_true.
    ENDIF.

    IF rs_repo IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Inconsistent repo metadata' ).
    ENDIF.

  ENDMETHOD.
  METHOD get_next_id.

* todo: Lock the complete persistence in order to prevent concurrent repo-creation
* however the current approach will most likely work in almost all cases

    DATA: lt_content TYPE Lif_abapgit_persistence=>ty_contents.

    FIELD-SYMBOLS: <ls_content> LIKE LINE OF lt_content.


    rv_next_repo_id = 1.

    lt_content = mo_db->list_by_type( Lcl_abapgit_persistence_db=>c_type_repo ).
    LOOP AT lt_content ASSIGNING <ls_content>.
      IF <ls_content>-value >= rv_next_repo_id.
        rv_next_repo_id = <ls_content>-value + 1.
      ENDIF.
    ENDLOOP.

    SHIFT rv_next_repo_id RIGHT DELETING TRAILING space.
    TRANSLATE rv_next_repo_id USING ' 0'.

  ENDMETHOD.
  METHOD get_repo_from_content.
    MOVE-CORRESPONDING from_xml( is_content-data_str ) TO rs_result.
    IF rs_result-local_settings-write_protected = abap_false AND
       Lcl_abapgit_factory=>get_environment( )->is_repo_object_changes_allowed( ) = abap_false.
      rs_result-local_settings-write_protected = abap_true.
    ENDIF.
    rs_result-key = is_content-value.
  ENDMETHOD.
  METHOD rewrite_repo_meta.

    DATA lv_old_blob TYPE string.
    DATA lv_new_blob TYPE string.
    DATA ls_repo_meta TYPE Lif_abapgit_persistence=>ty_repo.

    lv_old_blob = mo_db->read(
      iv_type  = Lcl_abapgit_persistence_db=>c_type_repo
      iv_value = iv_repo_key ).

    MOVE-CORRESPONDING from_xml( lv_old_blob ) TO ls_repo_meta.
    lv_new_blob = to_xml( ls_repo_meta ).

    mo_db->update(
      iv_type  = Lcl_abapgit_persistence_db=>c_type_repo
      iv_value = iv_repo_key
      iv_data  = lv_new_blob ).

  ENDMETHOD.
  METHOD to_xml.

    DATA: ls_xml TYPE Lif_abapgit_persistence=>ty_repo_xml.


    MOVE-CORRESPONDING is_repo TO ls_xml.

    CALL TRANSFORMATION id
      SOURCE repo = ls_xml
      RESULT XML rv_repo_xml_string.
  ENDMETHOD.
  METHOD Lif_abapgit_persist_repo_cs~delete.

    mo_db->delete(
      iv_type  = Lcl_abapgit_persistence_db=>c_type_repo_csum
      iv_value = iv_key ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_repo_cs~read.

    rv_cs_blob = mo_db->read(
      iv_type  = Lcl_abapgit_persistence_db=>c_type_repo_csum
      iv_value = iv_key ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_repo_cs~update.

    mo_db->modify(
      iv_type  = Lcl_abapgit_persistence_db=>c_type_repo_csum
      iv_value = iv_key
      iv_data  = iv_cs_blob ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_repo~add.

    DATA: ls_repo        TYPE Lif_abapgit_persistence=>ty_repo,
          lv_repo_as_xml TYPE string.


    ls_repo-url          = iv_url.
    ls_repo-branch_name  = iv_branch_name.
    ls_repo-package      = iv_package.
    ls_repo-offline      = iv_offline.
    ls_repo-created_by   = sy-uname.
    GET TIME STAMP FIELD ls_repo-created_at.
    ls_repo-dot_abapgit  = is_dot_abapgit.

    ls_repo-local_settings-display_name = iv_display_name.

    lv_repo_as_xml = to_xml( ls_repo ).

    rv_key = get_next_id( ).

    mo_db->add( iv_type  = Lcl_abapgit_persistence_db=>c_type_repo
                iv_value = rv_key
                iv_data  = lv_repo_as_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_repo~delete.

    DATA: lo_background TYPE REF TO Lcl_abapgit_persist_background.

    CREATE OBJECT lo_background.
    lo_background->delete( iv_key ).

    mo_db->delete( iv_type  = Lcl_abapgit_persistence_db=>c_type_repo
                   iv_value = iv_key ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_repo~exists.

    DATA lt_keys TYPE Lif_abapgit_persistence=>ty_repo_keys.
    DATA lt_content TYPE Lif_abapgit_persistence=>ty_contents.

    APPEND iv_key TO lt_keys.

    lt_content = mo_db->list_by_keys(
      it_keys = lt_keys
      iv_type = Lcl_abapgit_persistence_db=>c_type_repo ).

    rv_yes = boolc( lines( lt_content ) > 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_repo~list.

    DATA: lt_content TYPE Lif_abapgit_persistence=>ty_contents,
          ls_content LIKE LINE OF lt_content,
          ls_repo    LIKE LINE OF rt_repos.

    lt_content = mo_db->list_by_type( Lcl_abapgit_persistence_db=>c_type_repo ).

    LOOP AT lt_content INTO ls_content.
      ls_repo = get_repo_from_content( ls_content ).
      INSERT ls_repo INTO TABLE rt_repos.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_repo~list_by_keys.
    DATA: lt_content TYPE Lif_abapgit_persistence=>ty_contents,
          ls_content LIKE LINE OF lt_content,
          ls_repo    LIKE LINE OF rt_repos.

    lt_content = mo_db->list_by_keys(
      it_keys = it_keys
      iv_type = Lcl_abapgit_persistence_db=>c_type_repo ).

    LOOP AT lt_content INTO ls_content.
      ls_repo = get_repo_from_content( ls_content ).
      INSERT ls_repo INTO TABLE rt_repos.
    ENDLOOP.
  ENDMETHOD.
  METHOD Lif_abapgit_persist_repo~lock.

    mo_db->lock( iv_mode  = iv_mode
                 iv_type  = Lcl_abapgit_persistence_db=>c_type_repo
                 iv_value = iv_key ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_repo~read.

    DATA lt_repo TYPE Lif_abapgit_persistence=>ty_repos.

    lt_repo = Lif_abapgit_persist_repo~list( ).

    READ TABLE lt_repo INTO rs_repo WITH KEY key = iv_key.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE Lcx_abapgit_not_found.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_repo~update_metadata.

    DATA:
      lv_blob            TYPE Lif_abapgit_persistence=>ty_content-data_str,
      ls_persistent_meta TYPE Lif_abapgit_persistence=>ty_repo.

    FIELD-SYMBOLS <lv_field>   LIKE LINE OF mt_meta_fields.
    FIELD-SYMBOLS <lg_dst>     TYPE any.
    FIELD-SYMBOLS <lg_src>     TYPE any.
    FIELD-SYMBOLS <lv_changed> TYPE abap_bool.

    ASSERT NOT iv_key IS INITIAL.

    IF is_change_mask IS INITIAL.
      RETURN.
    ENDIF.

    " Validations
    IF is_change_mask-url = abap_true AND is_meta-url IS INITIAL.
      Lcx_abapgit_exception=>raise( 'update, url empty' ).
    ENDIF.

    ls_persistent_meta = Lcl_abapgit_repo_srv=>get_instance( )->get( iv_key )->ms_data.

    " Update
    LOOP AT mt_meta_fields ASSIGNING <lv_field>.
      ASSIGN COMPONENT <lv_field> OF STRUCTURE is_change_mask TO <lv_changed>.
      ASSERT sy-subrc = 0.
      CHECK <lv_changed> = abap_true.
      ASSIGN COMPONENT <lv_field> OF STRUCTURE ls_persistent_meta TO <lg_dst>.
      ASSERT sy-subrc = 0.
      ASSIGN COMPONENT <lv_field> OF STRUCTURE is_meta TO <lg_src>.
      ASSERT sy-subrc = 0.
      <lg_dst> = <lg_src>.
    ENDLOOP.

    lv_blob = to_xml( ls_persistent_meta ).

    mo_db->update( iv_type  = Lcl_abapgit_persistence_db=>c_type_repo
                   iv_value = iv_key
                   iv_data  = lv_blob ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_PERSISTENCE_REPO implementation

*>>>>>>> ZCL_ABAPGIT_DATA_FACTORY <<<<<<<*

*"* macro definitions
*include zcl_abapgit_data_factory======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_data_factory======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_DATA_FACTORY implementation.
*"* method's implementations
*include methods.
  METHOD get_supporter.

    IF gi_supporter IS INITIAL.
      CREATE OBJECT gi_supporter TYPE Lcl_abapgit_data_supporter.
    ENDIF.

    ri_supporter = gi_supporter.

  ENDMETHOD.
  METHOD get_config.
    CREATE OBJECT ri_config TYPE Lcl_abapgit_data_config.
  ENDMETHOD.
  METHOD get_deserializer.

    IF gi_deserializer IS INITIAL.
      CREATE OBJECT gi_deserializer TYPE Lcl_abapgit_data_deserializer.
    ENDIF.

    ri_deserializer = gi_deserializer.

  ENDMETHOD.
  METHOD get_serializer.

    IF gi_serializer IS INITIAL.
      CREATE OBJECT gi_serializer TYPE Lcl_abapgit_data_serializer.
    ENDIF.

    ri_serializer = gi_serializer.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_DATA_FACTORY implementation

*>>>>>>> ZCL_ABAPGIT_DATA_INJECTOR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_data_injector=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_data_injector=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_DATA_INJECTOR implementation.
*"* method's implementations
*include methods.
  METHOD set_supporter.
    Lcl_abapgit_data_factory=>gi_supporter = ii_supporter.
  ENDMETHOD.
  METHOD set_deserializer.
    Lcl_abapgit_data_factory=>gi_deserializer = ii_deserializer.
  ENDMETHOD.
  METHOD set_serializer.
    Lcl_abapgit_data_factory=>gi_serializer = ii_serializer.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_DATA_INJECTOR implementation

*>>>>>>> ZCL_ABAPGIT_FRONTEND_SERVICES <<<<<<<*

*"* macro definitions
*include zcl_abapgit_frontend_services=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_frontend_services=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_FRONTEND_SERVICES implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_frontend_services~clipboard_export.

    DATA lv_rc TYPE i.

    " Note: do not use a string table for 'it_data'!

    TRY.
        CALL METHOD cl_gui_frontend_services=>('CLIPBOARD_EXPORT')
          EXPORTING
            no_auth_check        = iv_no_auth_check " >= 740
          IMPORTING
            data                 = it_data
          CHANGING
            rc                   = lv_rc
          EXCEPTIONS
            cntl_error           = 1
            error_no_gui         = 2
            not_supported_by_gui = 3
            no_authority         = 4
            OTHERS               = 5.
        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise_t100( ).
        ENDIF.

      CATCH cx_sy_dyn_call_param_missing.

        cl_gui_frontend_services=>clipboard_export(
          IMPORTING
            data                 = it_data
          CHANGING
            rc                   = lv_rc
          EXCEPTIONS
            cntl_error           = 1
            error_no_gui         = 2
            not_supported_by_gui = 3
            no_authority         = 4
          OTHERS               = 5 ).
        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise_t100( ).
        ENDIF.

    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~directory_browse.

    cl_gui_frontend_services=>directory_browse(
      EXPORTING
        window_title         = iv_window_title
        initial_folder       = iv_initial_folder
      CHANGING
        selected_folder      = cv_selected_folder
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~directory_create.

    cl_gui_frontend_services=>directory_create(
      EXPORTING
        directory                = iv_directory
      CHANGING
        rc                       = cv_rc
      EXCEPTIONS
        directory_create_failed  = 1
        cntl_error               = 2
        error_no_gui             = 3
        directory_access_denied  = 4
        directory_already_exists = 5
        path_not_found           = 6
        unknown_error            = 7
        not_supported_by_gui     = 8
        wrong_parameter          = 9
        OTHERS                   = 10 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~directory_exist.

    cl_gui_frontend_services=>directory_exist(
      EXPORTING
        directory            = iv_directory
      RECEIVING
        result               = rv_exists
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~execute.

    cl_gui_frontend_services=>execute(
      EXPORTING
        document               = iv_document
        application            = iv_application
        parameter              = iv_parameter
        default_directory      = iv_default_directory
        maximized              = iv_maximized
        minimized              = iv_minimized
        synchronous            = iv_synchronous
        operation              = iv_operation
      EXCEPTIONS
        cntl_error             = 1
        error_no_gui           = 2
        bad_parameter          = 3
        file_not_found         = 4
        path_not_found         = 5
        file_extension_unknown = 6
        error_execute_failed   = 7
        synchronous_failed     = 8
        not_supported_by_gui   = 9
        OTHERS                 = 10 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~file_download.

    TYPES ty_hex TYPE x LENGTH 200.
    DATA lt_rawdata TYPE STANDARD TABLE OF ty_hex WITH DEFAULT KEY.

    Lcl_abapgit_convert=>xstring_to_bintab(
      EXPORTING iv_xstr   = iv_xstr
      IMPORTING et_bintab = lt_rawdata ).

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = xstrlen( iv_xstr )
        filename                  = iv_path
        filetype                  = 'BIN'
      CHANGING
        data_tab                  = lt_rawdata
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~file_upload.

    TYPES: ty_hex TYPE x LENGTH 255.

    DATA: lt_data   TYPE TABLE OF ty_hex WITH DEFAULT KEY,
          lv_length TYPE i.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = iv_path
        filetype                = 'BIN'
      IMPORTING
        filelength              = lv_length
      CHANGING
        data_tab                = lt_data
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CONCATENATE LINES OF lt_data INTO rv_xstr IN BYTE MODE.
    rv_xstr = rv_xstr(lv_length).

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~get_file_separator.

    cl_gui_frontend_services=>get_file_separator(
      CHANGING
        file_separator       = cv_file_separator
      EXCEPTIONS
        not_supported_by_gui = 1
        error_no_gui         = 2
        cntl_error           = 3
        OTHERS               = 4 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~get_gui_version.

    DATA:
      lt_version_table TYPE filetable,
      lv_rc            TYPE i,
      ls_version       LIKE LINE OF lt_version_table.

    cl_gui_frontend_services=>get_gui_version(
      CHANGING
        version_table            = lt_version_table
        rc                       = lv_rc
      EXCEPTIONS
        get_gui_version_failed   = 1
        cant_write_version_table = 2
        gui_no_version           = 3
        cntl_error               = 4
        error_no_gui             = 5
        not_supported_by_gui     = 6
        OTHERS                   = 7 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    READ TABLE lt_version_table INTO ls_version INDEX 1. " gui release
    ev_gui_release = ls_version-filename.
    READ TABLE lt_version_table INTO ls_version INDEX 2. " gui sp
    ev_gui_sp = ls_version-filename.
    READ TABLE lt_version_table INTO ls_version INDEX 3. " gui patch
    ev_gui_patch = ls_version-filename.

    ev_gui_version_string = |{ ev_gui_release }.{ condense( ev_gui_sp ) }.{ condense( ev_gui_patch ) }|.

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~get_system_directory.

    cl_gui_frontend_services=>get_system_directory(
      CHANGING
        system_directory     = cv_system_directory
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~gui_is_available.

    CALL FUNCTION 'GUI_IS_AVAILABLE'
      IMPORTING
        return = rv_gui_is_available.

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~is_sapgui_for_java.

    CALL FUNCTION 'GUI_HAS_JAVABEANS'
      IMPORTING
        return = rv_result.

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~is_sapgui_for_windows.

    CALL FUNCTION 'GUI_HAS_ACTIVEX'
      IMPORTING
        return = rv_result.

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~is_webgui.

    CALL FUNCTION 'GUI_IS_ITS'
      IMPORTING
        return = rv_is_webgui.

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~show_file_open_dialog.

    DATA:
      lt_file_table TYPE filetable,
      ls_file_table LIKE LINE OF lt_file_table,
      lv_filter     TYPE string,
      lv_action     TYPE i,
      lv_rc         TYPE i.

    IF iv_extension = 'zip'.
      lv_filter = 'ZIP Files (*.zip)|*.zip|' && cl_gui_frontend_services=>filetype_all.
    ENDIF.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        window_title            = iv_title
        default_filename        = iv_default_filename
        file_filter             = lv_filter
      CHANGING
        file_table              = lt_file_table
        rc                      = lv_rc
        user_action             = lv_action
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.
    IF lv_action = cl_gui_frontend_services=>action_cancel.
      Lcx_abapgit_exception=>raise( 'Cancelled' ).
    ENDIF.

    READ TABLE lt_file_table INDEX 1 INTO ls_file_table.
    ASSERT sy-subrc = 0.
    rv_path = ls_file_table-filename.

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~show_file_save_dialog.

    DATA:
      lv_action   TYPE i,
      lv_filter   TYPE string,
      lv_filename TYPE string,
      lv_path     TYPE string.

    IF iv_extension = 'zip'.
      lv_filter = 'ZIP Files (*.zip)|*.zip|' && cl_gui_frontend_services=>filetype_all.
    ENDIF.

    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        window_title         = iv_title
        default_extension    = iv_extension
        default_file_name    = iv_default_filename
        file_filter          = lv_filter
      CHANGING
        filename             = lv_filename
        path                 = lv_path
        fullpath             = rv_path
        user_action          = lv_action
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.
    IF lv_action = cl_gui_frontend_services=>action_cancel.
      Lcx_abapgit_exception=>raise( 'Cancelled' ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_frontend_services~open_ie_devtools.

    DATA: lv_system_directory TYPE string,
          lv_exe_full_path    TYPE string.

    IF Lif_abapgit_frontend_services~is_sapgui_for_windows( ) = abap_false.
      Lcx_abapgit_exception=>raise( |IE DevTools not supported on frontend OS| ).
    ENDIF.

    Lif_abapgit_frontend_services~get_system_directory( CHANGING cv_system_directory = lv_system_directory ).

    cl_gui_cfw=>flush( ).

    lv_exe_full_path = lv_system_directory && `\F12\IEChooser.exe`.

    Lif_abapgit_frontend_services~execute( iv_application = lv_exe_full_path ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_FRONTEND_SERVICES implementation

*>>>>>>> ZCL_ABAPGIT_GIT_TAG <<<<<<<*

*"* macro definitions
*include zcl_abapgit_git_tag===========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_git_tag===========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GIT_TAG implementation.
*"* method's implementations
*include methods.
  METHOD add_peel.

    rv_text = iv_text && Lif_abapgit_git_definitions=>c_git_branch-peel.

  ENDMETHOD.
  METHOD add_tag_prefix.

    rv_text = Lif_abapgit_git_definitions=>c_git_branch-tags_prefix && iv_text.

  ENDMETHOD.
  METHOD remove_peel.

    rv_text = iv_text.

    REPLACE Lif_abapgit_git_definitions=>c_git_branch-peel IN rv_text WITH ''.

  ENDMETHOD.
  METHOD remove_tag_prefix.

    rv_text = iv_text.

    REPLACE FIRST OCCURRENCE OF Lif_abapgit_git_definitions=>c_git_branch-tags_prefix
            IN rv_text
            WITH ''.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GIT_TAG implementation

*>>>>>>> ZCL_ABAPGIT_AJSON_UTILITIES <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ajson_utilities===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ajson_utilities===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_ajson_utilities===ccau.
**********************************************************************
* UTIL
**********************************************************************

CLASS SHRITEFUH64VYIPO5IWUYB3KWV3SFY DEFINITION FINAL.
  PUBLIC SECTION.

    DATA mt_nodes TYPE Lif_abapgit_ajson_types=>ty_nodes_tt READ-ONLY.

    METHODS add
      IMPORTING
        iv_str TYPE string.
    METHODS sorted
      RETURNING
        VALUE(rt_nodes) TYPE Lif_abapgit_ajson_types=>ty_nodes_ts.

ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KWV3SFY IMPLEMENTATION.
  METHOD add.

    FIELD-SYMBOLS <n> LIKE LINE OF mt_nodes.
    DATA lv_children TYPE string.
    DATA lv_index TYPE string.

    APPEND INITIAL LINE TO mt_nodes ASSIGNING <n>.

    SPLIT iv_str AT '|' INTO
      <n>-path
      <n>-name
      <n>-type
      <n>-value
      lv_index
      lv_children.
    CONDENSE <n>-path.
    CONDENSE <n>-name.
    CONDENSE <n>-type.
    CONDENSE <n>-value.
    <n>-index = lv_index.
    <n>-children = lv_children.

  ENDMETHOD.

  METHOD sorted.
    rt_nodes = mt_nodes.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
* PARSER
**********************************************************************



**********************************************************************
* JSON UTILITIES
**********************************************************************



class LCL_ABAPGIT_AJSON_UTILITIES implementation.
*"* method's implementations
*include methods.
  METHOD delete_empty_nodes.

    DATA ls_json_tree LIKE LINE OF io_json->mt_json_tree.
    DATA lv_done TYPE abap_bool.

    DO.
      lv_done = abap_true.

      IF iv_keep_empty_arrays = abap_false.
        LOOP AT io_json->mt_json_tree INTO ls_json_tree
          WHERE type = Lif_abapgit_ajson_types=>node_type-array AND children = 0.

          io_json->delete( ls_json_tree-path && ls_json_tree-name ).

        ENDLOOP.
        IF sy-subrc = 0.
          lv_done = abap_false.
        ENDIF.
      ENDIF.

      LOOP AT io_json->mt_json_tree INTO ls_json_tree
        WHERE type = Lif_abapgit_ajson_types=>node_type-object AND children = 0.

        io_json->delete( ls_json_tree-path && ls_json_tree-name ).

      ENDLOOP.
      IF sy-subrc = 0.
        lv_done = abap_false.
      ENDIF.

      IF lv_done = abap_true.
        EXIT. " nothing else to delete
      ENDIF.
    ENDDO.

  ENDMETHOD.
  METHOD diff.

    mo_json_a = normalize_input(
      iv_json = iv_json_a
      io_json = io_json_a ).

    mo_json_b = normalize_input(
      iv_json = iv_json_b
      io_json = io_json_b ).

    mo_insert = Lcl_abapgit_ajson=>create_empty( ).
    mo_delete = Lcl_abapgit_ajson=>create_empty( ).
    mo_change = Lcl_abapgit_ajson=>create_empty( ).

    diff_a_b( '/' ).
    diff_b_a( '/' ).

    eo_insert ?= mo_insert.
    eo_delete ?= mo_delete.
    eo_change ?= mo_change.

    delete_empty_nodes(
      io_json              = eo_insert
      iv_keep_empty_arrays = iv_keep_empty_arrays ).
    delete_empty_nodes(
      io_json              = eo_delete
      iv_keep_empty_arrays = iv_keep_empty_arrays ).
    delete_empty_nodes(
      io_json              = eo_change
      iv_keep_empty_arrays = iv_keep_empty_arrays ).

  ENDMETHOD.
  METHOD diff_a_b.

    DATA:
      lv_path_a TYPE string,
      lv_path_b TYPE string.

    FIELD-SYMBOLS:
      <node_a> LIKE LINE OF mo_json_a->mt_json_tree,
      <node_b> LIKE LINE OF mo_json_a->mt_json_tree.

    LOOP AT mo_json_a->mt_json_tree ASSIGNING <node_a> WHERE path = iv_path.
      lv_path_a = <node_a>-path && <node_a>-name && '/'.

      READ TABLE mo_json_b->mt_json_tree ASSIGNING <node_b>
        WITH TABLE KEY path = <node_a>-path name = <node_a>-name.
      IF sy-subrc = 0.
        lv_path_b = <node_b>-path && <node_b>-name && '/'.

        IF <node_a>-type = <node_b>-type.
          CASE <node_a>-type.
            WHEN Lif_abapgit_ajson_types=>node_type-array.
              mo_insert->touch_array( lv_path_a ).
              mo_change->touch_array( lv_path_a ).
              mo_delete->touch_array( lv_path_a ).
              diff_a_b( lv_path_a ).
            WHEN Lif_abapgit_ajson_types=>node_type-object.
              diff_a_b( lv_path_a ).
            WHEN OTHERS.
              IF <node_a>-value <> <node_b>-value.
                " save as changed value
                mo_change->set(
                  iv_path      = lv_path_b
                  iv_val       = <node_b>-value
                  iv_node_type = <node_b>-type ).
              ENDIF.
          ENDCASE.
        ELSE.
          " save changed type as delete + insert
          CASE <node_a>-type.
            WHEN Lif_abapgit_ajson_types=>node_type-array.
              mo_delete->touch_array( lv_path_a ).
              diff_a_b( lv_path_a ).
            WHEN Lif_abapgit_ajson_types=>node_type-object.
              diff_a_b( lv_path_a ).
            WHEN OTHERS.
              mo_delete->set(
                iv_path      = lv_path_a
                iv_val       = <node_a>-value
                iv_node_type = <node_a>-type ).
          ENDCASE.
          CASE <node_b>-type.
            WHEN Lif_abapgit_ajson_types=>node_type-array.
              mo_insert->touch_array( lv_path_b ).
              diff_b_a( lv_path_b ).
            WHEN Lif_abapgit_ajson_types=>node_type-object.
              diff_b_a( lv_path_b ).
            WHEN OTHERS.
              mo_insert->set(
                iv_path      = lv_path_b
                iv_val       = <node_b>-value
                iv_node_type = <node_b>-type ).
          ENDCASE.
        ENDIF.
      ELSE.
        " save as delete
        CASE <node_a>-type.
          WHEN Lif_abapgit_ajson_types=>node_type-array.
            mo_delete->touch_array( lv_path_a ).
            diff_a_b( lv_path_a ).
          WHEN Lif_abapgit_ajson_types=>node_type-object.
            diff_a_b( lv_path_a ).
          WHEN OTHERS.
            mo_delete->set(
              iv_path      = lv_path_a
              iv_val       = <node_a>-value
              iv_node_type = <node_a>-type ).
        ENDCASE.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD diff_b_a.

    DATA lv_path TYPE string.

    FIELD-SYMBOLS <node_b> LIKE LINE OF mo_json_b->mt_json_tree.

    LOOP AT mo_json_b->mt_json_tree ASSIGNING <node_b> WHERE path = iv_path.
      lv_path = <node_b>-path && <node_b>-name && '/'.

      CASE <node_b>-type.
        WHEN Lif_abapgit_ajson_types=>node_type-array.
          mo_insert->touch_array( lv_path ).
          diff_b_a(
            iv_path  = lv_path
            iv_array = abap_true ).
        WHEN Lif_abapgit_ajson_types=>node_type-object.
          diff_b_a( lv_path ).
        WHEN OTHERS.
          IF iv_array = abap_false.
            READ TABLE mo_json_a->mt_json_tree TRANSPORTING NO FIELDS
              WITH TABLE KEY path = <node_b>-path name = <node_b>-name.
            IF sy-subrc <> 0.
              " save as insert
              mo_insert->set(
                iv_path      = lv_path
                iv_val       = <node_b>-value
                iv_node_type = <node_b>-type ).
            ENDIF.
          ELSE.
            READ TABLE mo_insert->mt_json_tree TRANSPORTING NO FIELDS
              WITH KEY path = <node_b>-path value = <node_b>-value.
            IF sy-subrc <> 0.
              " save as new array value
              mo_insert->push(
                iv_path = iv_path
                iv_val  = <node_b>-value ).
            ENDIF.
          ENDIF.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.
  METHOD is_equal.

    DATA li_ins TYPE REF TO Lif_abapgit_ajson.
    DATA li_del TYPE REF TO Lif_abapgit_ajson.
    DATA li_mod TYPE REF TO Lif_abapgit_ajson.

    diff(
      EXPORTING
        iv_json_a = iv_json_a
        iv_json_b = iv_json_b
        io_json_a = ii_json_a
        io_json_b = ii_json_b
      IMPORTING
        eo_insert = li_ins
        eo_delete = li_del
        eo_change = li_mod ).

    rv_yes = boolc(
      li_ins->is_empty( ) = abap_true AND
      li_del->is_empty( ) = abap_true AND
      li_mod->is_empty( ) = abap_true ).

  ENDMETHOD.
  METHOD merge.

    mo_json_a = normalize_input(
      iv_json = iv_json_a
      io_json = io_json_a ).

    mo_json_b = normalize_input(
      iv_json = iv_json_b
      io_json = io_json_b ).

    " Start with first JSON...
    mo_insert = mo_json_a.

    " ...and add all nodes from second JSON
    diff_b_a( '/' ).

    ro_json ?= mo_insert.

    delete_empty_nodes(
      io_json              = ro_json
      iv_keep_empty_arrays = iv_keep_empty_arrays ).

  ENDMETHOD.
  METHOD new.
    CREATE OBJECT ro_instance.
  ENDMETHOD.
  METHOD normalize_input.

    IF boolc( iv_json IS INITIAL ) = boolc( io_json IS INITIAL ).
      Lcx_abapgit_ajson_error=>raise( 'Either supply JSON string or instance, but not both' ).
    ENDIF.

    IF iv_json IS NOT INITIAL.
      ro_json = Lcl_abapgit_ajson=>parse( iv_json ).
    ELSEIF io_json IS NOT INITIAL.
      ro_json = io_json.
    ELSE.
      Lcx_abapgit_ajson_error=>raise( 'Supply either JSON string or instance' ).
    ENDIF.

  ENDMETHOD.
  METHOD sort.

    DATA lo_json TYPE REF TO Lif_abapgit_ajson.

    lo_json = normalize_input(
      iv_json = iv_json
      io_json = io_json ).

    " Nodes are parsed into a sorted table, so no explicit sorting required
    rv_sorted = lo_json->stringify( 2 ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_AJSON_UTILITIES implementation

*>>>>>>> ZCL_ABAPGIT_BACKGROUND <<<<<<<*

*"* macro definitions
*include zcl_abapgit_background========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_background========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_BACKGROUND implementation.
*"* method's implementations
*include methods.
  METHOD dequeue.
    CALL FUNCTION 'DEQUEUE_EZABAPGIT'
      EXPORTING
        type = c_enq_type.
  ENDMETHOD.
  METHOD enqueue.
    CALL FUNCTION 'ENQUEUE_EZABAPGIT'
      EXPORTING
        mode_zabapgit  = 'E'
        type           = c_enq_type
        _scope         = '3'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.
  ENDMETHOD.
  METHOD list_methods.

    DATA: ls_method       LIKE LINE OF rt_methods,
          ls_key          TYPE seoclskey,
          lt_implementing TYPE seor_implementing_keys,
          ls_implementing LIKE LINE OF lt_implementing.

    FIELD-SYMBOLS: <ls_method> LIKE LINE OF rt_methods.


* in order to handle local classes in the compiled report
    ls_method-class = 'LCL_ABAPGIT_BACKGROUND_PULL'.
    INSERT ls_method INTO TABLE rt_methods.
    ls_method-class = 'LCL_ABAPGIT_BACKGROUND_PUSH_AU'.
    INSERT ls_method INTO TABLE rt_methods.
    ls_method-class = 'LCL_ABAPGIT_BACKGROUND_PUSH_FI'.
    INSERT ls_method INTO TABLE rt_methods.

    ls_key-clsname = 'LIF_ABAPGIT_BACKGROUND'.

    CALL FUNCTION 'SEO_INTERFACE_IMPLEM_GET_ALL'
      EXPORTING
        intkey       = ls_key
      IMPORTING
        impkeys      = lt_implementing
      EXCEPTIONS
        not_existing = 1
        OTHERS       = 2 ##FM_SUBRC_OK.
    LOOP AT lt_implementing INTO ls_implementing.
      ls_method-class = ls_implementing-clsname.
      INSERT ls_method INTO TABLE rt_methods.
    ENDLOOP.

    LOOP AT rt_methods ASSIGNING <ls_method>.
      CALL METHOD (<ls_method>-class)=>Lif_abapgit_background~get_description
        RECEIVING
          rv_description = <ls_method>-description.
    ENDLOOP.

  ENDMETHOD.
  METHOD run.

    DATA: lo_per        TYPE REF TO Lcl_abapgit_persist_background,
          lo_repo       TYPE REF TO Lcl_abapgit_repo_online,
          lt_list       TYPE Lcl_abapgit_persist_background=>ty_background_keys,
          li_background TYPE REF TO Lif_abapgit_background,
          li_log        TYPE REF TO Lif_abapgit_log,
          lx_error      TYPE REF TO Lcx_abapgit_exception,
          lv_repo_name  TYPE string.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF lt_list.

    TRY.
        enqueue( ).
      CATCH Lcx_abapgit_exception.
        WRITE: / 'Another intance of the program is already running'.
        RETURN.
    ENDTRY.

    CREATE OBJECT lo_per.
    lt_list = lo_per->list( ).

    WRITE: / 'Background mode'.

    LOOP AT lt_list ASSIGNING <ls_list>.
      CREATE OBJECT li_log TYPE Lcl_abapgit_log.

      TRY.
          lo_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->get( <ls_list>-key ).
          lv_repo_name = lo_repo->get_name( ).
          WRITE: / <ls_list>-method, lv_repo_name.

          Lcl_abapgit_login_manager=>set(
            iv_uri      = lo_repo->get_url( )
            iv_username = <ls_list>-username
            iv_password = <ls_list>-password ).

          CREATE OBJECT li_background TYPE (<ls_list>-method).

          li_background->run(
            io_repo     = lo_repo
            ii_log      = li_log
            it_settings = <ls_list>-settings ).

          " Clear auth buffer to allow different user/password per repository in background mode
          Lcl_abapgit_login_manager=>clear( ).

        CATCH Lcx_abapgit_exception INTO lx_error.
          li_log->add_exception( lx_error ).
      ENDTRY.

      Lcl_abapgit_log_viewer=>write_log( li_log ).
    ENDLOOP.

    IF lines( lt_list ) = 0.
      WRITE: / 'Nothing configured'.
    ENDIF.

    dequeue( ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_BACKGROUND implementation

*>>>>>>> ZCL_ABAPGIT_CONVERT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_convert===========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_convert===========ccimp.
CLASS SHRITEFUH64VYIPO5IWUYB3KWWHSFY DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS convert
      IMPORTING
        !iv_data         TYPE xsequence
        !iv_length       TYPE i OPTIONAL
      RETURNING
        VALUE(rv_string) TYPE string
      RAISING
        Lcx_abapgit_exception.
  PRIVATE SECTION.
    CLASS-DATA go_conv_new TYPE REF TO object.
    CLASS-DATA go_conv_old TYPE REF TO object.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KWWHSFY IMPLEMENTATION.
  METHOD convert.

    DATA lv_class TYPE string.
    DATA lx_error TYPE REF TO cx_root.

    IF go_conv_new IS INITIAL AND go_conv_old IS INITIAL.
      TRY.
          CALL METHOD ('CL_ABAP_CONV_CODEPAGE')=>create_in
            RECEIVING
              instance = go_conv_new.
        CATCH cx_sy_dyn_call_illegal_class.
          lv_class = 'CL_ABAP_CONV_IN_CE'.
          CALL METHOD (lv_class)=>create
            EXPORTING
              encoding = 'UTF-8'
            RECEIVING
              conv     = go_conv_old.
      ENDTRY.
    ENDIF.

    TRY.
        IF go_conv_new IS NOT INITIAL.
          CALL METHOD go_conv_new->('IF_ABAP_CONV_IN~CONVERT')
            EXPORTING
              source = iv_data
            RECEIVING
              result = rv_string.
        ELSE.
          CALL METHOD go_conv_old->('CONVERT')
            EXPORTING
              input = iv_data
              n     = iv_length
            IMPORTING
              data  = rv_string.
        ENDIF.
      CATCH cx_parameter_invalid_range
        cx_sy_codepage_converter_init
        cx_sy_conversion_codepage
        cx_parameter_invalid_type INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KWWJSFY DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS convert
      IMPORTING
        !iv_string        TYPE string
      RETURNING
        VALUE(rv_xstring) TYPE xstring
      RAISING
        Lcx_abapgit_exception.
  PRIVATE SECTION.
    CLASS-DATA go_conv_new TYPE REF TO object.
    CLASS-DATA go_conv_old TYPE REF TO object.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KWWJSFY IMPLEMENTATION.
  METHOD convert.
    DATA lx_error TYPE REF TO cx_root.
    DATA lv_class TYPE string.

    IF go_conv_new IS INITIAL AND go_conv_old IS INITIAL.
      TRY.
          CALL METHOD ('CL_ABAP_CONV_CODEPAGE')=>create_out
            RECEIVING
              instance = go_conv_new.
        CATCH cx_sy_dyn_call_illegal_class.
          lv_class = 'CL_ABAP_CONV_OUT_CE'.
          CALL METHOD (lv_class)=>create
            EXPORTING
              encoding = 'UTF-8'
            RECEIVING
              conv     = go_conv_old.
      ENDTRY.
    ENDIF.

    TRY.
        IF go_conv_new IS NOT INITIAL.
          CALL METHOD go_conv_new->('IF_ABAP_CONV_OUT~CONVERT')
            EXPORTING
              source = iv_string
            RECEIVING
              result = rv_xstring.
        ELSE.
          CALL METHOD go_conv_old->('CONVERT')
            EXPORTING
              data   = iv_string
            IMPORTING
              buffer = rv_xstring.
        ENDIF.
      CATCH cx_parameter_invalid_range
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

*"* test class
*include zcl_abapgit_convert===========ccau.
*----------------------------------------------------------------------*
*       CLASS SHRITEFUH64VYIPO5IWUYB3KWWLSFY DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS SHRITEFUH64VYIPO5IWUYB3KWWLSFY IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*

class LCL_ABAPGIT_CONVERT implementation.
*"* method's implementations
*include methods.
  METHOD base64_to_xstring.

    rv_xstr = cl_http_utility=>decode_x_base64( iv_base64 ).

  ENDMETHOD.
  METHOD bitbyte_to_int.

    DATA: lv_bitbyte TYPE string,
          lv_len     TYPE i,
          lv_offset  TYPE i.

    lv_bitbyte = iv_bits.
    SHIFT lv_bitbyte LEFT DELETING LEADING '0 '.
    lv_len     = strlen( lv_bitbyte ).
    lv_offset  = lv_len - 1.

    rv_int = 0.
    DO lv_len TIMES.

      IF sy-index = 1.
        "Intialize
        IF lv_bitbyte+lv_offset(1) = '1'.
          rv_int = 1.
        ENDIF.
      ELSEIF lv_bitbyte+lv_offset(1) = '1'.
        rv_int = rv_int + ( 2 ** ( sy-index - 1 ) ).
      ENDIF.

      lv_offset = lv_offset - 1. "Move Cursor

    ENDDO.

  ENDMETHOD.
  METHOD conversion_exit_isola_output.

    language_sap1_to_sap2(
      EXPORTING
        im_lang_sap1  = iv_spras
      RECEIVING
        re_lang_sap2  = rv_spras
      EXCEPTIONS
        no_assignment = 1
        OTHERS        = 2 ).                              "#EC CI_SUBRC

    TRANSLATE rv_spras TO UPPER CASE.

  ENDMETHOD.
  METHOD int_to_xstring4.
* returns xstring of length 4 containing the integer value iv_i

    DATA lv_x TYPE x LENGTH 4.

    lv_x = iv_i.
    rv_xstring = lv_x.

  ENDMETHOD.
  METHOD split_string.

    FIND FIRST OCCURRENCE OF cl_abap_char_utilities=>cr_lf IN iv_string.

    " Convert string into table depending on separator type CR_LF vs. LF
    IF sy-subrc = 0.
      SPLIT iv_string AT cl_abap_char_utilities=>cr_lf INTO TABLE rt_lines.
    ELSE.
      SPLIT iv_string AT cl_abap_char_utilities=>newline INTO TABLE rt_lines.
    ENDIF.

  ENDMETHOD.
  METHOD string_to_tab.

    DATA lv_length TYPE i.
    DATA lv_iterations TYPE i.
    DATA lv_offset TYPE i.

    FIELD-SYMBOLS <lg_line> TYPE any.


    CLEAR et_tab.
    ev_size = strlen( iv_str ).

    APPEND INITIAL LINE TO et_tab ASSIGNING <lg_line>.
    <lg_line> = iv_str.
    lv_length = cl_abap_typedescr=>describe_by_data( <lg_line> )->length / cl_abap_char_utilities=>charsize.
    lv_iterations = ev_size DIV lv_length.

    DO lv_iterations TIMES.
      lv_offset = sy-index * lv_length.
      APPEND INITIAL LINE TO et_tab ASSIGNING <lg_line>.
      <lg_line> = iv_str+lv_offset.
    ENDDO.

  ENDMETHOD.
  METHOD string_to_xstring.

    rv_xstr = string_to_xstring_utf8( iv_str ).

  ENDMETHOD.
  METHOD string_to_xstring_utf8.

    rv_xstring = SHRITEFUH64VYIPO5IWUYB3KWWJSFY=>convert( iv_string ).

  ENDMETHOD.
  METHOD string_to_xstring_utf8_bom.

    IF iv_string IS INITIAL.
      RETURN.
    ENDIF.

    rv_xstring = string_to_xstring_utf8( iv_string ).

    " Add UTF-8 BOM
    IF xstrlen( rv_xstring ) < 3 OR rv_xstring(3) <> cl_abap_char_utilities=>byte_order_mark_utf8.
      rv_xstring = cl_abap_char_utilities=>byte_order_mark_utf8 && rv_xstring.
    ENDIF.

  ENDMETHOD.
  METHOD xstring_to_bintab.

    DATA lv_length TYPE i.
    DATA lv_iterations TYPE i.
    DATA lv_offset TYPE i.
    DATA lv_struct TYPE abap_bool.

    FIELD-SYMBOLS <lg_line> TYPE any.


    CLEAR et_bintab.
    ev_size = xstrlen( iv_xstr ).

    APPEND INITIAL LINE TO et_bintab ASSIGNING <lg_line>.
    lv_struct = boolc(
      cl_abap_typedescr=>describe_by_data( <lg_line> )->type_kind = cl_abap_typedescr=>typekind_struct1 ).
    IF lv_struct = abap_true.
      ASSIGN COMPONENT 1 OF STRUCTURE <lg_line> TO <lg_line>.
    ENDIF.
    <lg_line> = iv_xstr.

    lv_length = cl_abap_typedescr=>describe_by_data( <lg_line> )->length.
    lv_iterations = ev_size DIV lv_length.

    DO lv_iterations TIMES.
      lv_offset = sy-index * lv_length.
      APPEND INITIAL LINE TO et_bintab ASSIGNING <lg_line>.
      IF lv_struct = abap_true.
        ASSIGN COMPONENT 1 OF STRUCTURE <lg_line> TO <lg_line>.
      ENDIF.
      <lg_line> = iv_xstr+lv_offset.
    ENDDO.

  ENDMETHOD.
  METHOD xstring_to_int.

* use the built-in type conversion
    rv_i = iv_xstring.

  ENDMETHOD.
  METHOD xstring_to_string_utf8.

    DATA lv_data   TYPE xstring.
    DATA lv_length TYPE i.

    " Remove BOM for non-Unicode systems
    lv_data = xstring_remove_bom( iv_data ).

    lv_length = iv_length.
    IF lv_length <= 0.
      lv_length = xstrlen( lv_data ).
    ENDIF.

    rv_string = SHRITEFUH64VYIPO5IWUYB3KWWHSFY=>convert(
      iv_data   = lv_data
      iv_length = lv_length ).

  ENDMETHOD.
  METHOD x_to_bitbyte.

    CLEAR rv_bitbyte.

    GET BIT 1 OF iv_x INTO rv_bitbyte+0(1).
    GET BIT 2 OF iv_x INTO rv_bitbyte+1(1).
    GET BIT 3 OF iv_x INTO rv_bitbyte+2(1).
    GET BIT 4 OF iv_x INTO rv_bitbyte+3(1).
    GET BIT 5 OF iv_x INTO rv_bitbyte+4(1).
    GET BIT 6 OF iv_x INTO rv_bitbyte+5(1).
    GET BIT 7 OF iv_x INTO rv_bitbyte+6(1).
    GET BIT 8 OF iv_x INTO rv_bitbyte+7(1).

  ENDMETHOD.
  METHOD xstring_remove_bom.

    rv_xstr = iv_xstr.

    " cl_abap_conv_in_ce does not handle BOM in non-Unicode systems, so we remove it
    IF cl_abap_char_utilities=>charsize = 1 AND xstrlen( rv_xstr ) > 3
        AND rv_xstr(3) = cl_abap_char_utilities=>byte_order_mark_utf8.

      rv_xstr = rv_xstr+3.

    ENDIF.

  ENDMETHOD.
  METHOD xstring_to_string_utf8_bom.

    DATA lv_xstring TYPE xstring.

    IF iv_xstring IS INITIAL.
      RETURN.
    ENDIF.

    lv_xstring = iv_xstring.
    " Add UTF-8 BOM
    IF xstrlen( lv_xstring ) < 3 OR lv_xstring(3) <> cl_abap_char_utilities=>byte_order_mark_utf8.
      lv_xstring = cl_abap_char_utilities=>byte_order_mark_utf8 && lv_xstring.
    ENDIF.

    rv_string = xstring_to_string_utf8( lv_xstring ).

  ENDMETHOD.
  METHOD language_sap1_to_sap2.

    DATA lv_class TYPE string.

    TRY.
        SELECT SINGLE languageisocode FROM ('I_LANGUAGE')
          INTO re_lang_sap2
          WHERE language = im_lang_sap1.
        IF sy-subrc <> 0.
          RAISE no_assignment.
        ENDIF.
      CATCH cx_sy_dynamic_osql_error.
        lv_class = 'CL_I18N_LANGUAGES'.
        CALL METHOD (lv_class)=>sap1_to_sap2
          EXPORTING
            im_lang_sap1  = im_lang_sap1
          RECEIVING
            re_lang_sap2  = re_lang_sap2
          EXCEPTIONS
            no_assignment = 1
            OTHERS        = 2.
        IF sy-subrc = 1.
          RAISE no_assignment.
        ENDIF.
    ENDTRY.
  ENDMETHOD.
  METHOD language_sap2_to_sap1.

    DATA lv_class TYPE string.

    TRY.
        SELECT SINGLE language FROM ('I_LANGUAGE')
          INTO re_lang_sap1
          WHERE languageisocode = im_lang_sap2.
        IF sy-subrc <> 0.
          RAISE no_assignment.
        ENDIF.
      CATCH cx_sy_dynamic_osql_error.
        lv_class = 'CL_I18N_LANGUAGES'.
        CALL METHOD (lv_class)=>sap2_to_sap1
          EXPORTING
            im_lang_sap2  = im_lang_sap2
          RECEIVING
            re_lang_sap1  = re_lang_sap1
          EXCEPTIONS
            no_assignment = 1
            OTHERS        = 2.
        IF sy-subrc = 1.
          RAISE no_assignment.
        ENDIF.
    ENDTRY.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_CONVERT implementation

*>>>>>>> ZCL_ABAPGIT_DATA_UTILS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_data_utils========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_data_utils========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_data_utils========ccau.


class LCL_ABAPGIT_DATA_UTILS implementation.
*"* method's implementations
*include methods.
  METHOD build_table_itab.

    DATA lo_type   TYPE REF TO cl_abap_typedescr.
    DATA lo_data   TYPE REF TO cl_abap_structdescr.
    DATA lo_table  TYPE REF TO cl_abap_tabledescr.
    DATA lt_keys   TYPE abap_table_keydescr_tab.
    DATA lt_names  TYPE ty_names.

    FIELD-SYMBOLS <lv_name>      LIKE LINE OF lt_names.
    FIELD-SYMBOLS <ls_key>       LIKE LINE OF lt_keys.
    FIELD-SYMBOLS <ls_component> LIKE LINE OF <ls_key>-components.

    cl_abap_structdescr=>describe_by_name(
      EXPORTING
        p_name         = iv_name
      RECEIVING
        p_descr_ref    = lo_type
      EXCEPTIONS
        type_not_found = 1 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Table { iv_name } not found for data serialization| ).
    ENDIF.

    TRY.
        lo_data ?= lo_type.

        " Get primary key to ensure unique entries
        IF lo_data->is_ddic_type( ) = abap_true.
          lt_names = list_key_fields( iv_name ).

          APPEND INITIAL LINE TO lt_keys ASSIGNING <ls_key>.
          <ls_key>-access_kind = cl_abap_tabledescr=>tablekind_hashed.
          <ls_key>-key_kind    = cl_abap_tabledescr=>keydefkind_user.
          <ls_key>-is_primary  = abap_true.
          <ls_key>-is_unique   = abap_true.

          LOOP AT lt_names ASSIGNING <lv_name>.
            APPEND INITIAL LINE TO <ls_key>-components ASSIGNING <ls_component>.
            <ls_component>-name = <lv_name>.
          ENDLOOP.
        ENDIF.

        IF lines( lt_names ) = 0.
          lo_table = cl_abap_tabledescr=>get( lo_data ).
        ELSE.
          lo_table = cl_abap_tabledescr=>get_with_keys(
            p_line_type = lo_data
            p_keys      = lt_keys ).
        ENDIF.

        CREATE DATA rr_data TYPE HANDLE lo_table.

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |Error creating internal table for data serialization| ).
    ENDTRY.

  ENDMETHOD.
  METHOD jump.

    " Run SE16 with authorization check
    CALL FUNCTION 'RS_TABLE_LIST_CREATE'
      EXPORTING
        table_name         = is_item-obj_name
      EXCEPTIONS
        table_is_structure = 1
        table_not_exists   = 2
        db_not_exists      = 3
        no_permission      = 4
        no_change_allowed  = 5
        table_is_gtt       = 6
        OTHERS             = 7.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Table { is_item-obj_name } cannot be displayed| ).
    ENDIF.

  ENDMETHOD.
  METHOD list_key_fields.
    DATA lo_obj        TYPE REF TO object.
    DATA lv_tabname    TYPE c LENGTH 16.
    DATA lr_ddfields   TYPE REF TO data.
    DATA lv_workaround TYPE c LENGTH 20.
    DATA lr_struct     TYPE REF TO cl_abap_structdescr.
    FIELD-SYMBOLS <lg_any> TYPE any.
    FIELD-SYMBOLS <lv_field> TYPE simple.
    FIELD-SYMBOLS <lt_ddfields> TYPE ANY TABLE.

* convert to correct type,
    lv_tabname = iv_name.

    TRY.
        CALL METHOD ('XCO_CP_ABAP_DICTIONARY')=>database_table
          EXPORTING
            iv_name           = lv_tabname
          RECEIVING
            ro_database_table = lo_obj.
        ASSIGN lo_obj->('IF_XCO_DATABASE_TABLE~FIELDS->IF_XCO_DBT_FIELDS_FACTORY~KEY') TO <lg_any>.
        IF sy-subrc  <> 0.
* fallback to RTTI, KEY field does not exist in S/4 2020
          RAISE EXCEPTION TYPE cx_sy_dyn_call_illegal_class.
        ENDIF.
        lo_obj = <lg_any>.
        CALL METHOD lo_obj->('IF_XCO_DBT_FIELDS~GET_NAMES')
          RECEIVING
            rt_names = rt_names.
      CATCH cx_sy_dyn_call_illegal_class cx_no_check.
        lv_workaround = 'DDFIELDS'.
        CREATE DATA lr_ddfields TYPE (lv_workaround).
        ASSIGN lr_ddfields->* TO <lt_ddfields>.
        ASSERT sy-subrc = 0.
        lr_struct ?= cl_abap_typedescr=>describe_by_name( lv_tabname ).
        lr_struct->get_ddic_field_list(
          RECEIVING
            p_field_list = <lt_ddfields>
          EXCEPTIONS
            not_found    = 1
            no_ddic_type = 2 ).
        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise( |Table { iv_name } not found| ).
        ENDIF.
        LOOP AT <lt_ddfields> ASSIGNING <lg_any>.
          ASSIGN COMPONENT 'KEYFLAG' OF STRUCTURE <lg_any> TO <lv_field>.
          IF sy-subrc <> 0 OR <lv_field> <> abap_true.
            CONTINUE.
          ENDIF.
          ASSIGN COMPONENT 'FIELDNAME' OF STRUCTURE <lg_any> TO <lv_field>.
          ASSERT sy-subrc = 0.
          APPEND <lv_field> TO rt_names.
        ENDLOOP.
    ENDTRY.

  ENDMETHOD.
  METHOD build_config_filename.

    rv_filename = to_lower( |{ is_config-name }.{ Lif_abapgit_data_config=>c_config }|
      && |.{ Lif_abapgit_data_config=>c_default_format }| ).

    REPLACE ALL OCCURRENCES OF '/' IN rv_filename WITH '#'.

  ENDMETHOD.
  METHOD build_data_filename.

    rv_filename = to_lower( |{ is_config-name }.{ is_config-type }|
      && |.{ Lif_abapgit_data_config=>c_default_format }| ).

    REPLACE ALL OCCURRENCES OF '/' IN rv_filename WITH '#'.

  ENDMETHOD.
  METHOD is_customizing_table.

    DATA lv_contflag       TYPE c LENGTH 1.
    DATA lo_table          TYPE REF TO object.
    DATA lo_content        TYPE REF TO object.
    DATA lo_delivery_class TYPE REF TO object.
    FIELD-SYMBOLS <ls_any> TYPE any.

    TRY.
        CALL METHOD ('XCO_CP_ABAP_DICTIONARY')=>database_table
          EXPORTING
            iv_name           = iv_name(16)
          RECEIVING
            ro_database_table = lo_table.
        CALL METHOD lo_table->('IF_XCO_DATABASE_TABLE~CONTENT')
          RECEIVING
            ro_content = lo_content.
        CALL METHOD lo_content->('IF_XCO_DBT_CONTENT~GET_DELIVERY_CLASS')
          RECEIVING
            ro_delivery_class = lo_delivery_class.
        ASSIGN lo_delivery_class->('VALUE') TO <ls_any>.
        lv_contflag = <ls_any>.
      CATCH cx_sy_dyn_call_illegal_class.
        SELECT SINGLE contflag FROM ('DD02L') INTO lv_contflag WHERE tabname = iv_name.
    ENDTRY.

    IF lv_contflag = 'C'.
      rv_customizing = abap_true.
    ELSEIF lv_contflag IS NOT INITIAL.
      rv_customizing = abap_false.
    ELSE.
      rv_customizing = abap_undefined. " table does not exist
    ENDIF.

  ENDMETHOD.
  METHOD does_table_exist.

    " This is slow but ensures that the table actually exists and is not just buffered by RTTI
    " If we just rely on RTTI, uninstalling and reinstalling a table in the same session will lead to dumps
    TRY.
        build_table_itab( iv_name ).
        rv_exists = abap_true.
      CATCH Lcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_DATA_UTILS implementation

*>>>>>>> ZCL_ABAPGIT_HASH <<<<<<<*

*"* macro definitions
*include zcl_abapgit_hash==============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_hash==============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_hash==============ccau.




class LCL_ABAPGIT_HASH implementation.
*"* method's implementations
*include methods.
  METHOD adler32.

    CONSTANTS: lc_adler TYPE i VALUE 65521,
               lc_max_b TYPE i VALUE 1800000000.

    DATA: lv_index TYPE i,
          lv_a     TYPE i VALUE 1,
          lv_b     TYPE i VALUE 0,
          lv_x     TYPE x LENGTH 2,
          lv_ca    TYPE c LENGTH 4,
          lv_cb    TYPE c LENGTH 4,
          lv_char8 TYPE c LENGTH 8.


    DO xstrlen( iv_xstring ) TIMES.
      lv_index = sy-index - 1.

      lv_a = lv_a + iv_xstring+lv_index(1).
      lv_b = lv_b + lv_a.

* delay the MOD operation until the integer might overflow
* articles describe 5552 additions are allowed, but this assumes unsigned integers
* instead of allowing a fixed number of additions before running MOD, then
* just compare value of lv_b, this is 1 operation less than comparing and adding
      IF lv_b > lc_max_b.
        lv_a = lv_a MOD lc_adler.
        lv_b = lv_b MOD lc_adler.
      ENDIF.
    ENDDO.

    lv_a = lv_a MOD lc_adler.
    lv_b = lv_b MOD lc_adler.

    lv_x = lv_a.
    lv_ca = lv_x.

    lv_x = lv_b.
    lv_cb = lv_x.

    CONCATENATE lv_cb lv_ca INTO lv_char8.

    rv_checksum = lv_char8.

  ENDMETHOD.
  METHOD sha1.

    DATA: lv_len     TYPE i,
          lv_char10  TYPE c LENGTH 10,
          lv_string  TYPE string,
          lv_xstring TYPE xstring.


    lv_len = xstrlen( iv_data ).
    lv_char10 = lv_len.
    CONDENSE lv_char10.
    CONCATENATE iv_type lv_char10 INTO lv_string SEPARATED BY space.
    lv_xstring = Lcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).

    lv_string = lv_xstring.
    CONCATENATE lv_string '00' INTO lv_string.
    lv_xstring = lv_string.

    CONCATENATE lv_xstring iv_data INTO lv_xstring IN BYTE MODE.

    rv_sha1 = sha1_raw( lv_xstring ).

  ENDMETHOD.
  METHOD sha1_blob.
    rv_sha1 = sha1( iv_type = Lif_abapgit_git_definitions=>c_type-blob
                    iv_data = iv_data ).
  ENDMETHOD.
  METHOD sha1_commit.
    rv_sha1 = sha1( iv_type = Lif_abapgit_git_definitions=>c_type-commit
                    iv_data = iv_data ).
  ENDMETHOD.
  METHOD sha1_raw.

    DATA: lv_hash  TYPE string,
          lv_key   TYPE xstring,
          lx_error TYPE REF TO cx_abap_message_digest.
    TRY.
        cl_abap_hmac=>calculate_hmac_for_raw(
      EXPORTING
        if_key        = lv_key
        if_data       = iv_data
      IMPORTING
        ef_hmacstring = lv_hash ).
      CATCH cx_abap_message_digest INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    rv_sha1 = lv_hash.
    TRANSLATE rv_sha1 TO LOWER CASE.

  ENDMETHOD.
  METHOD sha1_string.

    DATA: lv_hash  TYPE string,
          lv_key   TYPE xstring,
          lx_error TYPE REF TO cx_abap_message_digest.
    TRY.
        cl_abap_hmac=>calculate_hmac_for_char(
      EXPORTING
        if_key        = lv_key
        if_data       = iv_data
      IMPORTING
        ef_hmacstring = lv_hash ).
      CATCH cx_abap_message_digest INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    rv_sha1 = lv_hash.
    TRANSLATE rv_sha1 TO LOWER CASE.

  ENDMETHOD.
  METHOD sha1_tag.
    rv_sha1 = sha1( iv_type = Lif_abapgit_git_definitions=>c_type-tag
                    iv_data = iv_data ).
  ENDMETHOD.
  METHOD sha1_tree.
    rv_sha1 = sha1( iv_type = Lif_abapgit_git_definitions=>c_type-tree
                    iv_data = iv_data ).
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_HASH implementation

*>>>>>>> ZCL_ABAPGIT_DIFF <<<<<<<*

*"* macro definitions
*include zcl_abapgit_diff==============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_diff==============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_diff==============ccau.


class LCL_ABAPGIT_DIFF implementation.
*"* method's implementations
*include methods.
  METHOD adjust_diff.

    " ABAP kernel diff traverses files from bottom up which leads to odd display of diffs
    " SAP won't adjust this kernel service so we will do it here
    " https://github.com/abapGit/abapGit/issues/4395

    TYPES:
      BEGIN OF ty_diff_block,
        start TYPE i,
        len   TYPE i,
      END OF ty_diff_block.

    DATA:
      lv_block_begin TYPE i,
      lv_block_end   TYPE i,
      ls_diff_block  TYPE ty_diff_block,
      lt_diff_block  TYPE STANDARD TABLE OF ty_diff_block WITH DEFAULT KEY.

    FIELD-SYMBOLS:
      <ls_diff>       LIKE LINE OF mt_diff,
      <ls_diff_begin> LIKE LINE OF mt_diff,
      <ls_diff_end>   LIKE LINE OF mt_diff.

    " Determine start and length of diff blocks
    LOOP AT mt_diff ASSIGNING <ls_diff>.
      IF <ls_diff>-result = Lif_abapgit_definitions=>c_diff-insert OR
         <ls_diff>-result = Lif_abapgit_definitions=>c_diff-delete.
        IF ls_diff_block IS INITIAL.
          ls_diff_block-start = sy-tabix.
        ENDIF.
        ls_diff_block-len = ls_diff_block-len + 1.
      ELSEIF ls_diff_block-start IS NOT INITIAL.
        APPEND ls_diff_block TO lt_diff_block.
        CLEAR ls_diff_block.
      ENDIF.
    ENDLOOP.

    " For each diff block, check if beginning is same as end of block
    " If yes, move diff block down
    LOOP AT lt_diff_block INTO ls_diff_block.
      DO ls_diff_block-len TIMES.
        lv_block_begin = ls_diff_block-start + sy-index - 1.
        READ TABLE mt_diff ASSIGNING <ls_diff_begin> INDEX lv_block_begin.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        lv_block_end = ls_diff_block-start + ls_diff_block-len + sy-index - 1.
        READ TABLE mt_diff ASSIGNING <ls_diff_end> INDEX lv_block_end.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        CASE <ls_diff_begin>-result.
          WHEN Lif_abapgit_definitions=>c_diff-insert.
            IF <ls_diff_begin>-new = <ls_diff_end>-new.
              <ls_diff_begin>-old_num = <ls_diff_end>-old_num.
              <ls_diff_begin>-old     = <ls_diff_end>-old.
              <ls_diff_end>-result    = <ls_diff_begin>-result.
              CLEAR: <ls_diff_begin>-result, <ls_diff_end>-old_num, <ls_diff_end>-old.
            ELSE.
              EXIT.
            ENDIF.
          WHEN Lif_abapgit_definitions=>c_diff-delete.
            IF <ls_diff_begin>-old = <ls_diff_end>-old.
              <ls_diff_begin>-new_num = <ls_diff_end>-new_num.
              <ls_diff_begin>-new     = <ls_diff_end>-new.
              <ls_diff_end>-result    = <ls_diff_begin>-result.
              CLEAR: <ls_diff_begin>-result, <ls_diff_end>-new_num, <ls_diff_end>-new.
            ELSE.
              EXIT.
            ENDIF.
          WHEN OTHERS.
            EXIT.
        ENDCASE.
      ENDDO.
    ENDLOOP.

  ENDMETHOD.
  METHOD calculate_stats.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF mt_diff.

    LOOP AT mt_diff ASSIGNING <ls_diff>.
      CASE <ls_diff>-result.
        WHEN Lif_abapgit_definitions=>c_diff-insert.
          ms_stats-insert = ms_stats-insert + 1.
        WHEN Lif_abapgit_definitions=>c_diff-delete.
          ms_stats-delete = ms_stats-delete + 1.
        WHEN Lif_abapgit_definitions=>c_diff-update.
          ms_stats-update = ms_stats-update + 1.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.
  METHOD compute_and_render.

    DATA:
      lv_i     TYPE i,
      ls_diff  LIKE LINE OF rt_diff,
      lt_delta TYPE STANDARD TABLE OF rsedcresul WITH DEFAULT KEY.

    FIELD-SYMBOLS:
      <ls_old>   LIKE LINE OF it_old,
      <ls_new>   LIKE LINE OF it_new,
      <ls_delta> LIKE LINE OF lt_delta.

    " Note: Ignore case is for keywords, variables, types etc, but not for literals
    CALL FUNCTION 'RS_CMP_COMPUTE_DELTA'
      EXPORTING
        compare_mode            = mv_compare_mode
        ignore_case_differences = mv_ignore_case
      TABLES
        text_tab1               = it_new
        text_tab2               = it_old
        text_tab_res            = lt_delta
      EXCEPTIONS
        parameter_invalid       = 1
        difference_not_found    = 2
        OTHERS                  = 3.

    IF sy-subrc = 0.
      " Process delta
      LOOP AT lt_delta ASSIGNING <ls_delta>.
        CLEAR ls_diff.
        IF <ls_delta>-line1 > 0.
          lv_i = <ls_delta>-line1.
          ls_diff-old_num = lv_i.
          ls_diff-old     = <ls_delta>-text1.
        ENDIF.
        IF <ls_delta>-line2 > 0.
          lv_i = <ls_delta>-line2.
          ls_diff-new_num = lv_i.
          ls_diff-new     = <ls_delta>-text2.
        ENDIF.
        IF <ls_delta>-flag1 = 'D'.
          ls_diff-result = Lif_abapgit_definitions=>c_diff-delete.
        ELSEIF <ls_delta>-flag2 = 'I'.
          ls_diff-result = Lif_abapgit_definitions=>c_diff-insert.
        ELSEIF <ls_delta>-flag1 = 'M' AND <ls_delta>-flag2 = 'M'.
          ls_diff-result = Lif_abapgit_definitions=>c_diff-update.
        ELSEIF <ls_delta>-flag1 = '' AND <ls_delta>-flag2 = ''.
          ls_diff-result = Lif_abapgit_definitions=>c_diff-unchanged.
        ELSEIF <ls_delta>-flag1 = '' AND <ls_delta>-flag2 = 'E'. " ignore comment
          ls_diff-result = Lif_abapgit_definitions=>c_diff-unchanged.
        ELSEIF <ls_delta>-flag1 = 'E' AND <ls_delta>-flag2 = ''. " ignore comment
          ls_diff-result = Lif_abapgit_definitions=>c_diff-unchanged.
        ELSE.
          ASSERT 0 = 1. " unknown comparison result
        ENDIF.
        APPEND ls_diff TO rt_diff.
      ENDLOOP.
    ELSEIF sy-subrc = 2.
      " Copy input... but it might not be identical
      LOOP AT it_old ASSIGNING <ls_old>.
        CLEAR ls_diff.
        ls_diff-old_num = sy-tabix.
        ls_diff-old     = <ls_old>.
        READ TABLE it_new ASSIGNING <ls_new> INDEX sy-tabix.
        ASSERT sy-subrc = 0.
        ls_diff-new_num = sy-tabix.
        ls_diff-new     = <ls_new>.
        " SAP function ignores lines that contain only whitespace so we compare directly
        IF ( mv_compare_mode = 1 OR mv_compare_mode = 3 ) AND <ls_old> <> <ls_new> AND
           ( strlen( condense( <ls_old> ) ) = 0 OR strlen( condense( <ls_new> ) ) = 0 ).
          ls_diff-result = Lif_abapgit_definitions=>c_diff-update.
        ENDIF.
        APPEND ls_diff TO rt_diff.
      ENDLOOP.
    ELSE.
      ASSERT 0 = 1. " incorrect function call
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    DATA: lt_new TYPE rswsourcet,
          lt_old TYPE rswsourcet.

    mv_compare_mode = 1.
    IF iv_ignore_indentation = abap_true.
      mv_compare_mode = mv_compare_mode + 1.
    ENDIF.
    IF iv_ignore_comments = abap_true.
      mv_compare_mode = mv_compare_mode + 2.
    ENDIF.
    mv_ignore_case = iv_ignore_case.

    unpack( EXPORTING iv_new = iv_new
                      iv_old = iv_old
            IMPORTING et_new = lt_new
                      et_old = lt_old ).

    mt_diff = compute_and_render( it_new = lt_new
                                  it_old = lt_old ).

    adjust_diff( ).

    calculate_stats( ).
    map_beacons( ).
    shortlist( ).

  ENDMETHOD.
  METHOD create_regex_set.

    DATA: lo_regex TYPE REF TO cl_abap_regex,
          lt_regex TYPE Lif_abapgit_definitions=>ty_string_tt,
          lv_regex LIKE LINE OF lt_regex.

    APPEND '^\s*(CLASS|FORM|MODULE|REPORT|METHOD|INTERFACE|FUNCTION)\s[^=]' TO lt_regex.
    APPEND '^\s*(PUBLIC|PROTECTED|PRIVATE)\sSECTION(\s|\.)' TO lt_regex.
    APPEND '^\s*(CLASS|INTERFACE|FUNCTION|TYPE)-POOL\s' TO lt_regex.
    APPEND '^\s*(START|END)-OF-SELECTION(\s|\.)' TO lt_regex.
    APPEND '^\s*INITIALIZATION(\s|\.)' TO lt_regex.
    APPEND '^\s*(TOP-OF-PAGE|END-OF-PAGE)(\s|\.)' TO lt_regex.
    APPEND '^\s*AT\s*(SELECTION-SCREEN|LINE-SELECTION|USER-COMMAND|PF\d+)(\s|\.)' TO lt_regex.
    APPEND '^\s*(DEFINE|ENHANCEMENT)\s' TO lt_regex.

    LOOP AT lt_regex INTO lv_regex.
      CREATE OBJECT lo_regex
        EXPORTING
          pattern     = lv_regex
          ignore_case = abap_true.
      APPEND lo_regex TO rt_regex_set.
    ENDLOOP.

  ENDMETHOD.
  METHOD get.
    rt_diff = mt_diff.
  ENDMETHOD.
  METHOD get_beacons.
    rt_beacons = mt_beacons.
  ENDMETHOD.
  METHOD is_line_patched.

    FIELD-SYMBOLS: <ls_diff> TYPE Lif_abapgit_definitions=>ty_diff.

    READ TABLE mt_diff INDEX iv_index
                       ASSIGNING <ls_diff>.
    IF sy-subrc = 0.
      rv_patched = <ls_diff>-patch_flag.
    ELSE.
      Lcx_abapgit_exception=>raise( |Diff line not found { iv_index }| ).
    ENDIF.

  ENDMETHOD.
  METHOD map_beacons.

    DATA: lv_beacon_idx  TYPE i VALUE c_starting_beacon,
          lv_offs        TYPE i,
          lv_beacon_str  TYPE string,
          lv_beacon_2lev TYPE string,
          lv_submatch    TYPE string,
          lo_regex       TYPE REF TO cl_abap_regex,
          lt_regex       TYPE ty_regexset_tt.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF mt_diff.

    lt_regex = create_regex_set( ).
    LOOP AT mt_diff ASSIGNING <ls_diff>.

      CLEAR lv_offs.
      <ls_diff>-beacon = lv_beacon_idx.

      LOOP AT lt_regex INTO lo_regex.
        FIND FIRST OCCURRENCE OF REGEX lo_regex IN <ls_diff>-new SUBMATCHES lv_submatch.
        IF sy-subrc = 0. " Match
          lv_beacon_str = <ls_diff>-new.
          lv_submatch = to_upper( lv_submatch ).

          " Get rid of comments and end of line
          FIND FIRST OCCURRENCE OF '.' IN lv_beacon_str MATCH OFFSET lv_offs.
          IF sy-subrc <> 0.
            FIND FIRST OCCURRENCE OF '"' IN lv_beacon_str MATCH OFFSET lv_offs.
          ENDIF.

          IF lv_offs > 0.
            lv_beacon_str = lv_beacon_str(lv_offs).
          ENDIF.
          lv_beacon_str = condense( val = lv_beacon_str
                                    del = ` ` ).

          IF lv_submatch = 'CLASS'.
            lv_beacon_2lev = replace( val   = lv_beacon_str
                                      regex = '\s+(DEFINITION|IMPLEMENTATION)'
                                      with  = ''
                                      occ   = 0 ).
          ELSEIF lv_submatch = 'METHOD'.
            lv_beacon_str = lv_beacon_2lev && ` => ` && lv_beacon_str.
          ELSEIF lv_submatch = 'PUBLIC' OR lv_submatch = 'PROTECTED' OR lv_submatch = 'PRIVATE'.
            lv_beacon_str = lv_beacon_2lev && ` ` && lv_beacon_str.
          ENDIF.

          APPEND lv_beacon_str TO mt_beacons.
          lv_beacon_idx    = sy-tabix.
          <ls_diff>-beacon = lv_beacon_idx.
          EXIT. "Loop
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.
  METHOD set_patch_by_old_diff.

    FIELD-SYMBOLS: <ls_diff> TYPE Lif_abapgit_definitions=>ty_diff.

    LOOP AT mt_diff ASSIGNING <ls_diff>
                    USING KEY new_num
                    WHERE old     = is_diff_old-old
                      AND new     = is_diff_old-new
                      AND new_num = is_diff_old-new_num
                      AND old_num = is_diff_old-old_num.

      <ls_diff>-patch_flag = iv_patch_flag.
      EXIT.

    ENDLOOP.

  ENDMETHOD.
  METHOD set_patch_new.

    FIELD-SYMBOLS: <ls_diff> TYPE Lif_abapgit_definitions=>ty_diff.

    READ TABLE mt_diff WITH TABLE KEY new_num
                       COMPONENTS new_num = iv_line_new
                       ASSIGNING <ls_diff>.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Invalid new line number { iv_line_new }| ).
    ENDIF.

    <ls_diff>-patch_flag = iv_patch_flag.

  ENDMETHOD.
  METHOD set_patch_old.

    FIELD-SYMBOLS: <ls_diff> TYPE Lif_abapgit_definitions=>ty_diff.

    READ TABLE mt_diff WITH TABLE KEY old_num
                       COMPONENTS old_num = iv_line_old
                       ASSIGNING <ls_diff>.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Invalid old line number { iv_line_old }| ).
    ENDIF.

    <ls_diff>-patch_flag = iv_patch_flag.

  ENDMETHOD.
  METHOD shortlist.

    DATA: lv_index TYPE i.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF mt_diff.

    IF lines( mt_diff ) < 20.
      LOOP AT mt_diff ASSIGNING <ls_diff>.
        <ls_diff>-short = abap_true.
      ENDLOOP.
    ELSE.
      LOOP AT mt_diff TRANSPORTING NO FIELDS
          WHERE NOT result IS INITIAL AND short = abap_false.
        lv_index = sy-tabix.

        DO 8 TIMES. " Backward
          READ TABLE mt_diff INDEX ( lv_index - sy-index ) ASSIGNING <ls_diff>.
          IF sy-subrc <> 0 OR <ls_diff>-short = abap_true. " tab bound or prev marker
            EXIT.
          ENDIF.
          <ls_diff>-short = abap_true.
        ENDDO.

        DO 8 TIMES. " Forward
          READ TABLE mt_diff INDEX ( lv_index + sy-index - 1 ) ASSIGNING <ls_diff>.
          IF sy-subrc <> 0. " tab bound reached
            EXIT.
          ENDIF.
          CHECK <ls_diff>-short = abap_false. " skip marked
          <ls_diff>-short = abap_true.
        ENDDO.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD stats.
    rs_count = ms_stats.
  ENDMETHOD.
  METHOD unpack.

    DATA: lv_new      TYPE string,
          lv_old      TYPE string,
          lv_new_last TYPE c LENGTH 1,
          lv_old_last TYPE c LENGTH 1.

    lv_new = Lcl_abapgit_convert=>xstring_to_string_utf8( iv_new ).
    lv_old = Lcl_abapgit_convert=>xstring_to_string_utf8( iv_old ).

    " Check if one value contains a final newline but the other not
    " If yes, add a special characters that's visible in diff render
    IF lv_new IS NOT INITIAL.
      lv_new_last = substring(
        val = lv_new
        off = strlen( lv_new ) - 1 ).
    ENDIF.
    IF lv_old IS NOT INITIAL.
      lv_old_last = substring(
        val = lv_old
        off = strlen( lv_old ) - 1 ).
    ENDIF.

    IF lv_new_last = cl_abap_char_utilities=>newline AND lv_old_last <> cl_abap_char_utilities=>newline
      AND lv_old IS NOT INITIAL.
      lv_old = lv_old && cl_abap_char_utilities=>form_feed.
    ELSEIF lv_new_last <> cl_abap_char_utilities=>newline AND lv_old_last = cl_abap_char_utilities=>newline
      AND lv_new IS NOT INITIAL.
      lv_new = lv_new && cl_abap_char_utilities=>form_feed.
    ENDIF.

    SPLIT lv_new AT cl_abap_char_utilities=>newline INTO TABLE et_new.
    SPLIT lv_old AT cl_abap_char_utilities=>newline INTO TABLE et_old.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_DIFF implementation

*>>>>>>> ZCL_ABAPGIT_I18N_PARAMS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_i18n_params=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_i18n_params=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_i18n_params=======ccau.

*CLASS zcl_abapgit_i18n_params DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KW25SFY.


class LCL_ABAPGIT_I18N_PARAMS implementation.
*"* method's implementations
*include methods.
  METHOD build_language_filter.
    IF mt_language_filter IS INITIAL.
      " translation_languages are includes, system langs are excludes, so the do not interfere
      IF ms_params-translation_languages IS NOT INITIAL.
        mt_language_filter = iso_langs_to_lang_filter( ms_params-translation_languages ).
      ELSE.
        mt_language_filter = Lcl_abapgit_factory=>get_environment( )->get_system_language_filter( ).
      ENDIF.
    ENDIF.
    rt_language_filter = mt_language_filter.
  ENDMETHOD.
  METHOD constructor.
    IF is_params IS NOT INITIAL.
      ms_params = is_params.
    ELSE.
      ms_params-main_language         = iv_main_language.
      ms_params-main_language_only    = iv_main_language_only.
      ms_params-translation_languages = it_translation_langs.
      ms_params-use_lxe               = iv_use_lxe.
    ENDIF.
    ASSERT ms_params-main_language IS NOT INITIAL.
  ENDMETHOD.
  METHOD iso_langs_to_lang_filter.

    DATA lv_laiso LIKE LINE OF it_iso_filter.
    DATA lv_langu TYPE sy-langu.
    DATA ls_range LIKE LINE OF rt_language_filter.

    ls_range-sign = 'I'.
    ls_range-option = 'EQ'.

    LOOP AT it_iso_filter INTO lv_laiso.

      Lcl_abapgit_convert=>language_sap2_to_sap1(
        EXPORTING
          im_lang_sap2  = lv_laiso
        RECEIVING
          re_lang_sap1  = lv_langu
        EXCEPTIONS
          no_assignment = 1
          OTHERS        = 2 ).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      ls_range-low = lv_langu.
      APPEND ls_range TO rt_language_filter.

    ENDLOOP.

  ENDMETHOD.
  METHOD is_lxe_applicable.

    rv_yes = boolc( ms_params-main_language_only = abap_false AND
       ms_params-use_lxe = abap_true AND
       ms_params-translation_languages IS NOT INITIAL ).

  ENDMETHOD.
  METHOD new.
    CREATE OBJECT ro_instance
      EXPORTING
        iv_main_language      = iv_main_language
        iv_main_language_only = iv_main_language_only
        it_translation_langs  = it_translation_langs
        iv_use_lxe            = iv_use_lxe
        is_params             = is_params.
  ENDMETHOD.
  METHOD trim_saplang_keyed_table.

    DATA lv_laiso TYPE laiso.
    DATA lv_index TYPE i.

    FIELD-SYMBOLS <ls_i> TYPE any.
    FIELD-SYMBOLS <lv_langu> TYPE sy-langu.

    IF ms_params-translation_languages IS INITIAL OR iv_lang_field_name IS INITIAL.
      RETURN. " Nothing to filter
    ENDIF.

    LOOP AT ct_tab ASSIGNING <ls_i>.
      lv_index = sy-tabix.
      ASSIGN COMPONENT iv_lang_field_name OF STRUCTURE <ls_i> TO <lv_langu>.
      ASSERT sy-subrc = 0.

      IF iv_keep_master_lang = abap_true AND <lv_langu> = ms_params-main_language.
        CONTINUE. " Just keep it
      ENDIF.

      Lcl_abapgit_convert=>language_sap1_to_sap2(
        EXPORTING
          im_lang_sap1  = <lv_langu>
        RECEIVING
          re_lang_sap2  = lv_laiso
        EXCEPTIONS
          no_assignment = 1
          OTHERS        = 2 ).
      IF sy-subrc <> 0.
        DELETE ct_tab INDEX lv_index. " Not in the list anyway ...
        CONTINUE.
      ENDIF.

      " Not a sorted table, but presumably the list is small, so no significant performance flow
      READ TABLE ms_params-translation_languages TRANSPORTING NO FIELDS WITH KEY table_line = lv_laiso.
      IF sy-subrc <> 0.
        DELETE ct_tab INDEX lv_index.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD trim_saplang_list.

    DATA lv_langu TYPE sy-langu.
    DATA lv_laiso TYPE laiso.
    DATA lv_index TYPE i.

    IF ms_params-translation_languages IS INITIAL.
      RETURN. " Nothing to filter
    ENDIF.

    LOOP AT ct_sap_langs INTO lv_langu.
      lv_index = sy-tabix.

      Lcl_abapgit_convert=>language_sap1_to_sap2(
        EXPORTING
          im_lang_sap1  = lv_langu
        RECEIVING
          re_lang_sap2  = lv_laiso
        EXCEPTIONS
          no_assignment = 1
          OTHERS        = 2 ).
      IF sy-subrc <> 0.
        DELETE ct_sap_langs INDEX lv_index. " Not in the list anyway ...
        CONTINUE.
      ENDIF.

      " Not a sorted table, but presumably the list is small, so no significant performance flow
      READ TABLE ms_params-translation_languages TRANSPORTING NO FIELDS WITH KEY table_line = lv_laiso.
      IF sy-subrc <> 0.
        DELETE ct_sap_langs INDEX lv_index.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_I18N_PARAMS implementation

*>>>>>>> ZCL_ABAPGIT_ECATT_HELPER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ecatt_helper======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ecatt_helper======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ECATT_HELPER implementation.
*"* method's implementations
*include methods.
  METHOD build_xml_of_object.

    " downport of CL_APL_ECATT_DOWNLOAD=>BUILD_XML_OF_OBJECT

    DATA: lo_load_help_dummy TYPE REF TO cl_apl_ecatt_load_help,
          lx_ecatt           TYPE REF TO cx_ecatt_apl,
          lv_text            TYPE string,
          li_download        TYPE REF TO Lif_abapgit_ecatt_download.

    "download method will create the xml stream
    "note: it's the redefined download( ) of each object type specific download, which is called
    TRY.
        CREATE OBJECT lo_load_help_dummy
          EXPORTING
            im_maintain_function = ''.

        io_download->download( im_object_name    = iv_object_name
                               im_object_version = iv_object_version
                               im_object_type    = iv_object_type
                               im_load_help      = lo_load_help_dummy ).

      CATCH cx_ecatt_apl INTO lx_ecatt.
        lv_text = lx_ecatt->get_text( ).
        Lcx_abapgit_exception=>raise( lv_text ).
        " note, exception cx_ecatt_ui_attachment doesn't exist in 702
      CATCH cx_ecatt.
        "will never be raised from download, when called with mv_generate_xml_no_download = 'X'.
    ENDTRY.

    li_download ?= io_download.

    rv_xml_stream = li_download->get_xml_stream( ).

  ENDMETHOD.
  METHOD download_data.

    DATA:
      lo_xml TYPE REF TO cl_apl_ecatt_xml.

    TRY.
        CALL METHOD cl_apl_ecatt_xml=>('CREATE') " doesn't exist in 702
          EXPORTING
            im_type = c_xml
          RECEIVING
            re_xml  = lo_xml.

        lo_xml->set_attributes( im_dom = ii_template_over_all ).

        lo_xml->get_attributes( IMPORTING ex_xml = rv_xml_stream ).

      CATCH cx_ecatt_apl_xml.
        RETURN.
    ENDTRY.

  ENDMETHOD.
  METHOD upload_data_from_stream.

    DATA:
      lo_xml           TYPE REF TO cl_apl_ecatt_xml,
      lv_xstr          TYPE xstring,
      li_nc_xmlref_typ TYPE REF TO if_ixml_node_collection,
      li_n_xmlref_typ  TYPE REF TO if_ixml_node,
      lv_index         TYPE i VALUE 0,
      lv_count         TYPE i.

    lv_xstr = iv_xml_stream.

    CALL METHOD cl_apl_ecatt_xml=>('CREATE') " doesn't exist in 702
      EXPORTING
        im_type = c_xml
      RECEIVING
        re_xml  = lo_xml.

* whitespace stripping needs a namespace
* remove white spaces only at the time of upload
    lo_xml->stream_to_dom( im_xstream            = lv_xstr
                           im_ignore_white_space = 'X'
                           im_uri                = cl_apl_xml_const=>schema_uri ).

    lo_xml->get_attributes( IMPORTING ex_dom = ri_template_over_all ).

* MD: Workaround, because nodes starting with "XML" are not allowed
    li_nc_xmlref_typ ?= ri_template_over_all->get_elements_by_tag_name_ns( 'XMLREF_TYP' ).
    CALL METHOD li_nc_xmlref_typ->('GET_LENGTH')  " downport
      RECEIVING
        rval = lv_count.

    WHILE lv_index < lv_count.
      li_n_xmlref_typ = li_nc_xmlref_typ->get_item( lv_index ).
      li_n_xmlref_typ->set_name( 'X-MLREF_TYP' ).
      lv_index = lv_index + 1.
    ENDWHILE.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ECATT_HELPER implementation

*>>>>>>> ZCL_ABAPGIT_FILE_DESERIALIZE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_file_deserialize==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_file_deserialize==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_file_deserialize==ccau.
*CLASS SHRITEFUH64VYIPO5IWUYB3KWXHSFY DEFINITION DEFERRED.
*CLASS SHRITEFUH64VYIPO5IWUYB3KWXJSFY DEFINITION DEFERRED.

*CLASS zcl_abapgit_file_deserialize DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KWXHSFY.
*CLASS zcl_abapgit_file_deserialize DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KWXJSFY.





class LCL_ABAPGIT_FILE_DESERIALIZE implementation.
*"* method's implementations
*include methods.
  METHOD filter_files_to_deserialize.

    DATA lt_objects LIKE rt_results.
    DATA lr_object  TYPE REF TO Lif_abapgit_definitions=>ty_result.
    DATA ls_item    TYPE Lif_abapgit_definitions=>ty_item.
    DATA lv_tabix   TYPE sy-tabix.

    rt_results = it_results.

    "preparation for object logging, sort all file entries by objects
    IF ii_log IS BOUND.
      lt_objects = rt_results.
      SORT lt_objects
        BY obj_type
           obj_name.
      DELETE ADJACENT DUPLICATES FROM lt_objects COMPARING obj_type obj_name.
      DELETE lt_objects WHERE obj_type IS INITIAL AND obj_name IS INITIAL.
    ENDIF.

    "ignore objects w/o changes
    DELETE rt_results WHERE match = abap_true.     " Full match
    "log objects w/o changes
    IF sy-subrc = 0 AND ii_log IS BOUND.
      SORT rt_results BY obj_type obj_name.
      LOOP AT lt_objects REFERENCE INTO lr_object.
        lv_tabix = sy-tabix.
        READ TABLE rt_results WITH KEY obj_type = lr_object->obj_type
                                       obj_name = lr_object->obj_name
                              BINARY SEARCH TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          "all parts of the objects have not changed
          ls_item-devclass = lr_object->package.
          ls_item-obj_type = lr_object->obj_type.
          ls_item-obj_name = lr_object->obj_name.
          ii_log->add_success(
            iv_msg  = |Object { ls_item-obj_name } (type { ls_item-obj_type }) not changed; no import required|
            is_item = ls_item ).
          "ignore object for further messages
          DELETE lt_objects INDEX lv_tabix.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "ignore objects w/o object type
    DELETE rt_results WHERE obj_type IS INITIAL.
    "log objects w/o object type
    IF sy-subrc = 0 AND ii_log IS BOUND.
      " Note: Moving the CHECK condition to the LOOP WHERE clause will lead to a
      " syntax warning in higher releases and syntax error in 702
      LOOP AT lt_objects REFERENCE INTO lr_object.
        CHECK lr_object->obj_type IS INITIAL AND lr_object->obj_name IS NOT INITIAL.
        ls_item-devclass = lr_object->package.
        ls_item-obj_type = lr_object->obj_type.
        ls_item-obj_name = lr_object->obj_name.
        ii_log->add_warning(
          iv_msg  = |Object type for { ls_item-obj_name } not defined - will be ignored by abapGit|
          is_item = ls_item ).
      ENDLOOP.
      DELETE lt_objects WHERE obj_type IS INITIAL.
    ENDIF.

    "ignore objects that exists only local
    DELETE rt_results WHERE lstate = Lif_abapgit_definitions=>c_state-added AND rstate IS INITIAL.
    "ignore objects that where deleted remotely
    DELETE rt_results WHERE rstate = Lif_abapgit_definitions=>c_state-deleted.
    "log objects that exists only local or where deleted remotely
    IF sy-subrc = 0 AND ii_log IS BOUND.
      SORT rt_results BY obj_type obj_name.
      LOOP AT lt_objects REFERENCE INTO lr_object.
        lv_tabix = sy-tabix.
        READ TABLE rt_results WITH KEY obj_type = lr_object->obj_type
                                       obj_name = lr_object->obj_name
                              BINARY SEARCH TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          "all parts exists only local
          "no log message; ignore object for further messages
          DELETE lt_objects INDEX lv_tabix.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "ignore table content
    DELETE rt_results WHERE path = Lif_abapgit_data_config=>c_default_path.

    SORT rt_results
      BY obj_type ASCENDING
         obj_name ASCENDING
         rstate   DESCENDING  " ensures that non-empty rstate is kept
         lstate   DESCENDING. " ensures that non-empty lstate is kept
    DELETE ADJACENT DUPLICATES FROM rt_results COMPARING obj_type obj_name.

  ENDMETHOD.
  METHOD get_results.

    DATA lt_results TYPE Lif_abapgit_definitions=>ty_results_tt.

    lt_results = filter_files_to_deserialize(
      it_results = Lcl_abapgit_repo_status=>calculate( io_repo )
      ii_log     = ii_log ).

    rt_results = prioritize_deser(
      ii_log     = ii_log
      it_results = lt_results ).

  ENDMETHOD.
  METHOD map_results_to_items.

    DATA ls_item LIKE LINE OF rt_items.
    FIELD-SYMBOLS: <ls_result> TYPE Lif_abapgit_definitions=>ty_result.

    LOOP AT it_results ASSIGNING <ls_result>.
      ls_item-devclass = <ls_result>-package.
      ls_item-obj_type = <ls_result>-obj_type.
      ls_item-obj_name = <ls_result>-obj_name.
      INSERT ls_item INTO TABLE rt_items.
    ENDLOOP.

  ENDMETHOD.
  METHOD prioritize_deser.

    DATA lt_items    TYPE Lif_abapgit_definitions=>ty_items_tt.
    DATA ls_item     LIKE LINE OF lt_items.
    DATA lt_requires TYPE Lif_abapgit_definitions=>ty_items_tt.
    DATA ls_require  LIKE LINE OF lt_requires.
    DATA ls_result   LIKE LINE OF it_results.
    DATA lo_graph    TYPE REF TO Lcl_abapgit_item_graph.

    lt_items = map_results_to_items( it_results ).

    CREATE OBJECT lo_graph EXPORTING it_items = lt_items.

    LOOP AT lt_items INTO ls_item.
      CLEAR lt_requires.

* TODO: BEGIN extract to object handler method in ZIF_ABAPGIT_OBJECT:
*    METHODS get_deserialize_order
*      IMPORTING
*        it_items TYPE ty_items_tt
*      RETURNING
*        VALUE(rt_requries) TYPE ty_items_tt

      CASE ls_item-obj_type.
        WHEN 'SPRX'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'WEBI'.
        WHEN 'CLAS'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'SPRX'
            AND obj_type <> 'INTF'
            AND obj_type <> 'XSLT'.
        WHEN 'PROG'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'XSLT'.
        WHEN 'INTF'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'SPRX'
            AND obj_type <> 'XSLT'.
        WHEN 'TABL'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'SPRX'.
        WHEN 'IARP'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'IASP'.
        WHEN 'IATU' OR 'IAXU' OR 'IAMU'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'IASP'
            AND obj_type <> 'PROG'
            AND obj_type <> 'IARP'.
        WHEN 'DCLS'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'DDLS'.
        WHEN 'ODSO'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'IOBJ'.
        WHEN 'SCP1'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'TOBJ'.
        WHEN 'CHAR'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'OTGR'.
        WHEN 'PINF'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'CLAS'
            AND obj_type <> 'INTF'
            AND obj_type <> 'TABL'
            AND obj_type <> 'DOMA'
            AND obj_type <> 'DTEL'.
        WHEN 'DEVC'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'PINF'.
        WHEN 'ENHC'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'ENHO'.
        WHEN 'ENHO'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'ENSC' AND obj_type <> 'ENHS'.
        WHEN 'ENSC'.
          lt_requires = lt_items.
          DELETE lt_requires WHERE obj_type <> 'ENHS'.
      ENDCASE.
* TODO: END extract to object handler method

      LOOP AT lt_requires INTO ls_require.
        lo_graph->add_edge(
          is_from = ls_require
          is_to   = ls_item ).
      ENDLOOP.
    ENDLOOP.

    WHILE lo_graph->has_vertices( ) = abap_true.
      ls_item = lo_graph->get_next( ii_log ).
      READ TABLE it_results INTO ls_result WITH KEY sec_key COMPONENTS
        obj_name = ls_item-obj_name
        obj_type = ls_item-obj_type.
      ASSERT sy-subrc = 0.
      APPEND ls_result TO rt_results.
    ENDWHILE.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_FILE_DESERIALIZE implementation

*>>>>>>> ZCL_ABAPGIT_OBJECTS_BRIDGE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_objects_bridge====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_objects_bridge====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECTS_BRIDGE implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    DATA ls_objtype_map LIKE LINE OF gt_objtype_map.

    super->constructor( is_item = is_item
                        iv_language = Lif_abapgit_definitions=>c_english ).

    initialize( ).

*    determine the responsible plugin
    READ TABLE gt_objtype_map INTO ls_objtype_map
      WITH TABLE KEY obj_typ = is_item-obj_type.
    IF sy-subrc = 0.
      CREATE OBJECT mo_plugin TYPE (ls_objtype_map-plugin_class).

      CALL METHOD mo_plugin->('SET_ITEM')
        EXPORTING
          iv_obj_type = is_item-obj_type
          iv_obj_name = is_item-obj_name.
    ELSE.
      RAISE EXCEPTION TYPE cx_sy_create_object_error
        EXPORTING
          classname = 'LCL_OBJECTS_BRIDGE'.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    rv_user = c_user_unknown. " todo
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.
    DATA lx_plugin TYPE REF TO cx_static_check.

    TRY.
        CALL METHOD mo_plugin->('LIF_ABAPGITP_PLUGIN~DELETE').
      CATCH cx_static_check INTO lx_plugin.
        Lcx_abapgit_exception=>raise( lx_plugin->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lx_plugin        TYPE REF TO cx_static_check.

    TRY.
        CALL METHOD mo_plugin->('WRAP_DESERIALIZE')
          EXPORTING
            iv_package = iv_package
            io_xml     = io_xml.
      CATCH cx_static_check INTO lx_plugin.
        Lcx_abapgit_exception=>raise( lx_plugin->get_text( ) ).
    ENDTRY.
  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    CALL METHOD mo_plugin->('LIF_ABAPGITP_PLUGIN~EXISTS')
      RECEIVING
        rv_bool = rv_bool.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.

    DATA ls_meta TYPE ty_metadata.

    CALL METHOD mo_plugin->('LIF_ABAPGITP_PLUGIN~GET_METADATA')
      RECEIVING
        rs_metadata = ls_meta.

    IF ls_meta-late_deser = abap_true.
      APPEND Lif_abapgit_object=>gc_step_id-late TO rt_steps.
    ELSEIF ls_meta-ddic = abap_true.
      APPEND Lif_abapgit_object=>gc_step_id-ddic TO rt_steps.
    ELSE.
      APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.

    DATA ls_meta TYPE ty_metadata.

    CALL METHOD mo_plugin->('LIF_ABAPGITP_PLUGIN~GET_METADATA')
      RECEIVING
        rs_metadata = ls_meta.

    MOVE-CORRESPONDING ls_meta TO rs_metadata.

  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = abap_true.
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    CALL METHOD mo_plugin->('LIF_ABAPGITP_PLUGIN~JUMP').
    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    CALL METHOD mo_plugin->('WRAP_SERIALIZE')
      EXPORTING
        io_xml = io_xml.

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
  METHOD initialize.

    DATA lt_plugin_class    TYPE STANDARD TABLE OF seoclsname WITH DEFAULT KEY.
    DATA lv_plugin_class    LIKE LINE OF lt_plugin_class.
    DATA lo_plugin          TYPE REF TO object.
    DATA lt_plugin_obj_type TYPE STANDARD TABLE OF tadir-object WITH DEFAULT KEY.
    DATA ls_objtype_map     LIKE LINE OF gt_objtype_map.

    IF gv_init = abap_true.
      RETURN.
    ENDIF.
    gv_init = abap_true.

    SELECT clsname
      FROM seometarel
      INTO TABLE lt_plugin_class
      WHERE refclsname LIKE 'LCL_ABAPGITP_OBJECT%'
      AND version = '1'
      ORDER BY clsname.                                   "#EC CI_SUBRC

    CLEAR gt_objtype_map.
    LOOP AT lt_plugin_class INTO lv_plugin_class
        WHERE table_line <> 'LCL_ABAPGITP_OBJECT_BY_SOBJ'.
* have the generic plugin only as fallback
      TRY.
          CREATE OBJECT lo_plugin TYPE (lv_plugin_class).
        CATCH cx_sy_create_object_error.
          CONTINUE. ">>>>>>>>>>>>>>
      ENDTRY.

      CALL METHOD lo_plugin->('GET_SUPPORTED_OBJ_TYPES')
        IMPORTING
          rt_obj_type = lt_plugin_obj_type.

      ls_objtype_map-plugin_class = lv_plugin_class.
      LOOP AT lt_plugin_obj_type INTO ls_objtype_map-obj_typ.
        INSERT ls_objtype_map INTO TABLE gt_objtype_map.
        IF sy-subrc <> 0.
* No exception in class-contructor possible.
* Anyway, a shortdump is more appropriate in this case
          ASSERT 'There must not be' =
            |multiple abapGit-Plugins for the same object type {
            ls_objtype_map-obj_typ }|.
        ENDIF.
      ENDLOOP.
    ENDLOOP. "at plugins

* and the same for the generic plugin if exists
* have the generic plugin only as fallback
    LOOP AT lt_plugin_class INTO lv_plugin_class
        WHERE table_line = 'LCL_ABAPGITP_OBJECT_BY_SOBJ'.
      CREATE OBJECT lo_plugin TYPE (lv_plugin_class).

      CALL METHOD lo_plugin->('GET_SUPPORTED_OBJ_TYPES')
        RECEIVING
          rt_obj_type = lt_plugin_obj_type.

      ls_objtype_map-plugin_class = lv_plugin_class.
      LOOP AT lt_plugin_obj_type INTO ls_objtype_map-obj_typ.
        INSERT ls_objtype_map INTO TABLE gt_objtype_map. "knowingly ignore the subrc
      ENDLOOP.
    ENDLOOP. "at plugins

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECTS_BRIDGE implementation

*>>>>>>> ZCL_ABAPGIT_GIT_PACK <<<<<<<*

*"* macro definitions
*include zcl_abapgit_git_pack==========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_git_pack==========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS SHRITEFUH64VYIPO5IWUYB3KWX7SFY IMPLEMENTATION.

  METHOD constructor.
    mv_data = iv_data.
  ENDMETHOD.

  METHOD eat_byte.
    rv_x = mv_data(1).
    mv_data = mv_data+1.
  ENDMETHOD.

  METHOD get.
    rv_data = mv_data.
  ENDMETHOD.

  METHOD eat_bytes.
    rv_x = mv_data(iv_length).
    mv_data = mv_data+iv_length.
  ENDMETHOD.

ENDCLASS.

*"* test class
*include zcl_abapgit_git_pack==========ccau.





*CLASS SHRITEFUH64VYIPO5IWUYB3KWYFSFY DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_pack DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KWYFSFY.



*CLASS SHRITEFUH64VYIPO5IWUYB3KWYHSFY DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_pack DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KWYHSFY.









*CLASS SHRITEFUH64VYIPO5IWUYB3KWYNSFY DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_pack DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KWYNSFY.



class LCL_ABAPGIT_GIT_PACK implementation.
*"* method's implementations
*include methods.
  METHOD decode.

    DATA: lv_x              TYPE x,
          lv_data           TYPE xstring,
          lv_type           TYPE c LENGTH 6,
          lv_zlib           TYPE x LENGTH 2,
          lv_objects        TYPE i,
          lv_len            TYPE i,
          lv_sha1           TYPE Lif_abapgit_git_definitions=>ty_sha1,
          lv_ref_delta      TYPE Lif_abapgit_git_definitions=>ty_sha1,
          lv_compressed_len TYPE i,
          lv_compressed     TYPE xstring,
          lv_decompressed   TYPE xstring,
          lv_decompress_len TYPE i,
          lv_xstring        TYPE xstring,
          lv_expected       TYPE i,
          ls_object         LIKE LINE OF rt_objects,
          lv_uindex         TYPE sy-index.


    lv_data = iv_data.

* header
    IF NOT xstrlen( lv_data ) > 4 OR lv_data(4) <> c_pack_start.
      Lcx_abapgit_exception=>raise( |Unexpected pack header| ).
    ENDIF.
    lv_data = lv_data+4.

* version
    IF lv_data(4) <> c_version.
      Lcx_abapgit_exception=>raise( |Version not supported| ).
    ENDIF.
    lv_data = lv_data+4.

* number of objects
    lv_xstring = lv_data(4).
    lv_objects = Lcl_abapgit_convert=>xstring_to_int( lv_xstring ).
    lv_data = lv_data+4.


    DO lv_objects TIMES.

      lv_uindex = sy-index.

      lv_x = lv_data(1).
      lv_type = get_type( lv_x ).

      get_length( IMPORTING ev_length = lv_expected
                  CHANGING cv_data = lv_data ).

      IF lv_type = Lif_abapgit_git_definitions=>c_type-ref_d.
        lv_ref_delta = lv_data(20).
        lv_data = lv_data+20.
      ENDIF.

* strip header, '789C', CMF + FLG
      lv_zlib = lv_data(2).
      IF lv_zlib <> c_zlib AND lv_zlib <> c_zlib_hmm.
        Lcx_abapgit_exception=>raise( |Unexpected zlib header| ).
      ENDIF.
      lv_data = lv_data+2.

*******************************

      IF lv_zlib = c_zlib.
        cl_abap_gzip=>decompress_binary(
          EXPORTING
            gzip_in     = lv_data
          IMPORTING
            raw_out     = lv_decompressed
            raw_out_len = lv_decompress_len ).

        IF lv_expected <> lv_decompress_len.
          Lcx_abapgit_exception=>raise( |Decompression falied| ).
        ENDIF.

        cl_abap_gzip=>compress_binary(
          EXPORTING
            raw_in         = lv_decompressed
          IMPORTING
            gzip_out       = lv_compressed
            gzip_out_len   = lv_compressed_len ).

        IF xstrlen( lv_data ) <= lv_compressed_len OR
          lv_compressed(lv_compressed_len) <> lv_data(lv_compressed_len).
          "Lets try with zlib before error in out for good
          "This fixes issues with TFS 2017 and visualstudio.com Git repos
          zlib_decompress( CHANGING cv_data = lv_data
                                    cv_decompressed = lv_decompressed ).
        ELSE.
          lv_data = lv_data+lv_compressed_len.
        ENDIF.

      ELSEIF lv_zlib = c_zlib_hmm.
* cl_abap_gzip compression works for header '789C', but does not work for
* '7801', call custom implementation of DEFLATE algorithm.
* The custom implementation could handle both, but most likely the kernel
* implementation runs faster than the custom ABAP.
        zlib_decompress( CHANGING cv_data = lv_data
                                  cv_decompressed = lv_decompressed ).
      ENDIF.

      CLEAR ls_object.
      ls_object-adler32 = lv_data(4).
      lv_data = lv_data+4. " skip adler checksum

      IF lv_type = Lif_abapgit_git_definitions=>c_type-ref_d.
        ls_object-sha1 = lv_ref_delta.
        TRANSLATE ls_object-sha1 TO LOWER CASE.
      ELSE.
        ls_object-sha1 = Lcl_abapgit_hash=>sha1(
          iv_type = lv_type
          iv_data = lv_decompressed ).
      ENDIF.
      ls_object-type = lv_type.
      ls_object-data = lv_decompressed.
      ls_object-index = lv_uindex.
      APPEND ls_object TO rt_objects.
    ENDDO.

* check SHA1 at end of pack
    lv_len = xstrlen( iv_data ) - 20.
    lv_xstring = iv_data(lv_len).
    lv_sha1 = Lcl_abapgit_hash=>sha1_raw( lv_xstring ).
    IF to_upper( lv_sha1 ) <> lv_data.
      Lcx_abapgit_exception=>raise( |SHA1 at end of pack doesnt match| ).
    ENDIF.

    decode_deltas( CHANGING ct_objects = rt_objects ).

  ENDMETHOD.
  METHOD decode_commit.

    DATA: lv_string        TYPE string,
          lv_word          TYPE string,
          lv_offset        TYPE i,
          lv_length        TYPE i,
          lv_length_gpgsig TYPE i,
          lv_trash         TYPE string ##NEEDED,
          lt_string        TYPE TABLE OF string.

    FIELD-SYMBOLS: <lv_string> LIKE LINE OF lt_string.


    lv_string = Lcl_abapgit_convert=>xstring_to_string_utf8( iv_data ).

    SPLIT lv_string AT cl_abap_char_utilities=>newline INTO TABLE lt_string.

    LOOP AT lt_string ASSIGNING <lv_string>.
      lv_length = strlen( <lv_string> ) + 1.
      lv_string = lv_string+lv_length.

      SPLIT <lv_string> AT space INTO lv_word lv_trash.
      CASE lv_word.
        WHEN 'tree'.
          rs_commit-tree = <lv_string>+5.
        WHEN 'parent'.
          IF rs_commit-parent IS INITIAL.
            rs_commit-parent = <lv_string>+7.
          ELSE.
            rs_commit-parent2 = <lv_string>+7.
          ENDIF.
        WHEN 'author'.
          rs_commit-author = <lv_string>+7.
        WHEN 'committer'.
          rs_commit-committer = <lv_string>+10.
          EXIT. " current loop
        WHEN OTHERS.
          ASSERT 1 = 0.
      ENDCASE.

    ENDLOOP.

    lv_length = strlen( lv_string ).
    IF lv_length >= 6 AND lv_string+0(6) = 'gpgsig'.
      FIND REGEX |-----END PGP SIGNATURE-----[[:space:]]+|
        IN lv_string
        MATCH OFFSET lv_offset
        MATCH LENGTH lv_length.
      lv_length = lv_length - 1.
      lv_length_gpgsig = lv_offset + lv_length - 7.
      lv_length = lv_offset + lv_length.
      rs_commit-gpgsig = lv_string+7(lv_length_gpgsig).
      lv_string = lv_string+lv_length.
    ENDIF.

    rs_commit-body = lv_string+1.

    IF rs_commit-author IS INITIAL
        OR rs_commit-committer IS INITIAL
        OR rs_commit-tree IS INITIAL.
      Lcx_abapgit_exception=>raise( |multiple parents? not supported| ).
    ENDIF.

  ENDMETHOD.
  METHOD decode_deltas.

    DATA: ls_object   LIKE LINE OF ct_objects,
          li_progress TYPE REF TO Lif_abapgit_progress,
          lt_deltas   LIKE ct_objects.


    LOOP AT ct_objects INTO ls_object
        USING KEY type
        WHERE type = Lif_abapgit_git_definitions=>c_type-ref_d.
      INSERT ls_object INTO TABLE lt_deltas.
    ENDLOOP.

    DELETE ct_objects
      USING KEY type
      WHERE type = Lif_abapgit_git_definitions=>c_type-ref_d.

    "Restore correct Delta Order
    SORT lt_deltas BY index.

    li_progress = Lcl_abapgit_progress=>get_instance( lines( lt_deltas ) ).

    LOOP AT lt_deltas INTO ls_object.
      li_progress->show( iv_current = sy-tabix
                         iv_text    = 'Decode deltas' ).

      delta( EXPORTING is_object = ls_object
             CHANGING ct_objects = ct_objects ).
    ENDLOOP.

  ENDMETHOD.
  METHOD decode_tag.

    DATA: lv_string TYPE string,
          lv_word   TYPE string,
          lv_trash  TYPE string ##NEEDED,
          lt_string TYPE TABLE OF string.

    FIELD-SYMBOLS: <lv_string> LIKE LINE OF lt_string.


    lv_string = Lcl_abapgit_convert=>xstring_to_string_utf8( iv_data ).

    SPLIT lv_string AT cl_abap_char_utilities=>newline INTO TABLE lt_string.

    LOOP AT lt_string ASSIGNING <lv_string>.

      SPLIT <lv_string> AT space INTO lv_word lv_trash.

      CASE lv_word.
        WHEN 'object'.
          rs_tag-object = lv_trash.
        WHEN 'type'.
          rs_tag-type = lv_trash.
        WHEN 'tag'.
          rs_tag-tag = lv_trash.
        WHEN 'tagger'.

          FIND FIRST OCCURRENCE OF REGEX `(.*)<(.*)>`
                     IN lv_trash
                     SUBMATCHES rs_tag-tagger_name
                                rs_tag-tagger_email.

          rs_tag-tagger_name = condense( rs_tag-tagger_name ).

        WHEN ''.
          " ignore blank lines
          CONTINUE.
        WHEN OTHERS.

          " these are the non empty line which don't start with a key word
          " the first one is the message, the rest are cumulated to the body

          IF rs_tag-message IS INITIAL.
            rs_tag-message = <lv_string>.
          ELSE.

            IF rs_tag-body IS NOT INITIAL.
              rs_tag-body = rs_tag-body && cl_abap_char_utilities=>newline.
            ENDIF.

            rs_tag-body = rs_tag-body && <lv_string>.

          ENDIF.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.
  METHOD decode_tree.

    CONSTANTS: lc_sha_length TYPE i VALUE 20,
               lc_null       TYPE x VALUE '00'.

    DATA: lv_xstring TYPE xstring,
          lv_chmod   TYPE Lif_abapgit_git_definitions=>ty_chmod,
          lv_name    TYPE string,
          lv_string  TYPE string,
          lv_len     TYPE i,
          lv_offset  TYPE i,
          lv_cursor  TYPE i,
          lv_match   TYPE i,
          ls_node    TYPE ty_node.


    DO.
      FIND FIRST OCCURRENCE OF lc_null IN SECTION OFFSET lv_cursor OF iv_data
        IN BYTE MODE MATCH OFFSET lv_match.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      lv_len = lv_match - lv_cursor.
      lv_xstring = iv_data+lv_cursor(lv_len).

      lv_string = Lcl_abapgit_convert=>xstring_to_string_utf8( lv_xstring ).
      SPLIT lv_string AT space INTO lv_chmod lv_name.

      CLEAR ls_node.
      ls_node-chmod = lv_chmod.
      IF ls_node-chmod <> Lif_abapgit_git_definitions=>c_chmod-dir
          AND ls_node-chmod <> Lif_abapgit_git_definitions=>c_chmod-file
          AND ls_node-chmod <> Lif_abapgit_git_definitions=>c_chmod-executable
          AND ls_node-chmod <> Lif_abapgit_git_definitions=>c_chmod-submodule.
        Lcx_abapgit_exception=>raise( |Unknown chmod| ).
      ENDIF.

      lv_offset = lv_match + 1.
      ls_node-name = lv_name.
      ls_node-sha1 = iv_data+lv_offset(lc_sha_length).
      TRANSLATE ls_node-sha1 TO LOWER CASE.
      APPEND ls_node TO rt_nodes.

      lv_cursor = lv_match + 1 + lc_sha_length.
    ENDDO.

  ENDMETHOD.
  METHOD delta.

    CONSTANTS: lc_1   TYPE x VALUE '01',
               lc_2   TYPE x VALUE '02',
               lc_4   TYPE x VALUE '04',
               lc_8   TYPE x VALUE '08',
               lc_16  TYPE x VALUE '10',
               lc_32  TYPE x VALUE '20',
               lc_64  TYPE x VALUE '40',
               lc_128 TYPE x VALUE '80'.

    DATA: lv_base   TYPE xstring,
          lv_result TYPE xstring,
          lv_offset TYPE i,
          lo_stream TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KWX7SFY,
          lv_sha1   TYPE Lif_abapgit_git_definitions=>ty_sha1,
          ls_object LIKE LINE OF ct_objects,
          lv_len    TYPE i,
          lv_tmp    TYPE xstring,
          lv_org    TYPE x.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF ct_objects.


    CREATE OBJECT lo_stream
      EXPORTING
        iv_data = is_object-data.

* find base
    READ TABLE ct_objects ASSIGNING <ls_object>
      WITH KEY sha COMPONENTS sha1 = is_object-sha1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Base not found, { is_object-sha1 }| ).
    ELSEIF <ls_object>-type = Lif_abapgit_git_definitions=>c_type-ref_d.
* sanity check
      Lcx_abapgit_exception=>raise( |Delta, base eq delta| ).
    ENDIF.

    lv_base = <ls_object>-data.

* skip the 2 headers
    delta_header( lo_stream ).
    delta_header( lo_stream ).

    WHILE xstrlen( lo_stream->get( ) ) > 0.

      lv_org = lo_stream->eat_byte( ).

      IF lv_org BIT-AND lc_128 = lc_128. " MSB = 1

        lv_offset = 0.
        IF lv_org BIT-AND lc_1 = lc_1.
          lv_offset = lo_stream->eat_byte( ).
        ENDIF.
        IF lv_org BIT-AND lc_2 = lc_2.
          lv_offset = lv_offset + lo_stream->eat_byte( ) * 256.
        ENDIF.
        IF lv_org BIT-AND lc_4 = lc_4.
          lv_offset = lv_offset + lo_stream->eat_byte( ) * 65536.
        ENDIF.
        IF lv_org BIT-AND lc_8 = lc_8.
          lv_offset = lv_offset + lo_stream->eat_byte( ) * 16777216. " hmm, overflow?
        ENDIF.

        lv_len = 0.
        IF lv_org BIT-AND lc_16 = lc_16.
          lv_len = lo_stream->eat_byte( ).
        ENDIF.
        IF lv_org BIT-AND lc_32 = lc_32.
          lv_len = lv_len + lo_stream->eat_byte( ) * 256.
        ENDIF.
        IF lv_org BIT-AND lc_64 = lc_64.
          lv_len = lv_len + lo_stream->eat_byte( ) * 65536.
        ENDIF.

        IF lv_len = 0.
          lv_len = 65536.
        ENDIF.

        CONCATENATE lv_result lv_base+lv_offset(lv_len)
          INTO lv_result IN BYTE MODE.
      ELSE. " lv_bitbyte(1) = '0'
* insert from delta
        lv_len = lv_org. " convert to int
        lv_tmp = lo_stream->eat_bytes( lv_len ).
        CONCATENATE lv_result lv_tmp INTO lv_result IN BYTE MODE.
      ENDIF.

    ENDWHILE.

    lv_sha1 = Lcl_abapgit_hash=>sha1( iv_type = <ls_object>-type
                                      iv_data = lv_result ).

    CLEAR ls_object.
    ls_object-sha1 = lv_sha1.
    ls_object-type = <ls_object>-type.
    ls_object-data = lv_result.
    ls_object-index = <ls_object>-index. "Retain sort index
    APPEND ls_object TO ct_objects.

  ENDMETHOD.
  METHOD delta_header.

    DATA: lv_bitbyte TYPE Lif_abapgit_git_definitions=>ty_bitbyte,
          lv_bits    TYPE string,
          lv_x       TYPE x.


    lv_bits = ''.
    DO.
      lv_x = io_stream->eat_byte( ).
      lv_bitbyte = Lcl_abapgit_convert=>x_to_bitbyte( lv_x ).
      CONCATENATE lv_bitbyte+1 lv_bits INTO lv_bits.
      IF lv_bitbyte(1) = '0'.
        EXIT. " current loop
      ENDIF.
    ENDDO.
    rv_header = Lcl_abapgit_convert=>bitbyte_to_int( lv_bits ).

  ENDMETHOD.
  METHOD encode.

    DATA: lv_sha1          TYPE x LENGTH 20,
          lv_adler32       TYPE Lif_abapgit_git_definitions=>ty_adler32,
          lv_compressed    TYPE xstring,
          lv_xstring       TYPE xstring,
          li_progress      TYPE REF TO Lif_abapgit_progress,
          lv_objects_total TYPE i.

    FIELD-SYMBOLS: <ls_object>  LIKE LINE OF it_objects.


    rv_data = c_pack_start.

    CONCATENATE rv_data c_version INTO rv_data IN BYTE MODE.

    lv_xstring = Lcl_abapgit_convert=>int_to_xstring4( lines( it_objects ) ).
    CONCATENATE rv_data lv_xstring INTO rv_data IN BYTE MODE.

    lv_objects_total = lines( it_objects ).

    li_progress = Lcl_abapgit_progress=>get_instance( lv_objects_total ).

    LOOP AT it_objects ASSIGNING <ls_object>.
      li_progress->show(
        iv_current = sy-tabix
        iv_text    = |Encoding objects ( { sy-tabix } of { lv_objects_total } )| ).

      lv_xstring = type_and_length(
        iv_type   = <ls_object>-type
        iv_length = xstrlen( <ls_object>-data ) ).
      CONCATENATE rv_data lv_xstring INTO rv_data IN BYTE MODE.

      cl_abap_gzip=>compress_binary(
        EXPORTING
          raw_in   = <ls_object>-data
        IMPORTING
          gzip_out = lv_compressed ).

      CONCATENATE rv_data c_zlib lv_compressed INTO rv_data IN BYTE MODE.

      IF NOT <ls_object>-adler32 IS INITIAL.
        lv_adler32 = <ls_object>-adler32.
      ELSE.
        lv_adler32 = Lcl_abapgit_hash=>adler32( <ls_object>-data ).
      ENDIF.
      CONCATENATE rv_data lv_adler32 INTO rv_data IN BYTE MODE.

    ENDLOOP.

    lv_sha1 = to_upper( Lcl_abapgit_hash=>sha1_raw( rv_data ) ).
    CONCATENATE rv_data lv_sha1 INTO rv_data IN BYTE MODE.

  ENDMETHOD.
  METHOD encode_commit.

    DATA: lv_string       TYPE string,
          lv_tmp          TYPE string,
          lv_tree_lower   TYPE string,
          lv_parent_lower TYPE string.


    lv_tree_lower = is_commit-tree.
    TRANSLATE lv_tree_lower TO LOWER CASE.

    lv_string = ''.

    CONCATENATE 'tree' lv_tree_lower INTO lv_tmp SEPARATED BY space.
    CONCATENATE lv_string lv_tmp cl_abap_char_utilities=>newline INTO lv_string.

    IF NOT is_commit-parent IS INITIAL.
      lv_parent_lower = is_commit-parent.
      TRANSLATE lv_parent_lower TO LOWER CASE.

      CONCATENATE 'parent' lv_parent_lower
        INTO lv_tmp SEPARATED BY space.
      CONCATENATE lv_string lv_tmp cl_abap_char_utilities=>newline INTO lv_string.
    ENDIF.

    IF NOT is_commit-parent2 IS INITIAL.
      lv_parent_lower = is_commit-parent2.
      TRANSLATE lv_parent_lower TO LOWER CASE.

      CONCATENATE 'parent' lv_parent_lower
        INTO lv_tmp SEPARATED BY space.
      CONCATENATE lv_string lv_tmp cl_abap_char_utilities=>newline INTO lv_string.
    ENDIF.

    CONCATENATE 'author' is_commit-author
      INTO lv_tmp SEPARATED BY space.
    CONCATENATE lv_string lv_tmp cl_abap_char_utilities=>newline INTO lv_string.

    CONCATENATE 'committer' is_commit-committer
      INTO lv_tmp SEPARATED BY space.
    CONCATENATE lv_string lv_tmp cl_abap_char_utilities=>newline INTO lv_string.

    IF NOT is_commit-gpgsig IS INITIAL.
      CONCATENATE 'gpgsig' is_commit-gpgsig
        INTO lv_tmp SEPARATED BY space.
      CONCATENATE lv_string lv_tmp INTO lv_string.
    ENDIF.

    CONCATENATE lv_string cl_abap_char_utilities=>newline is_commit-body INTO lv_string.

    rv_data = Lcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).

  ENDMETHOD.
  METHOD encode_tag.

    DATA: lv_string TYPE string,
          lv_time   TYPE Lcl_abapgit_git_time=>ty_unixtime.

    lv_time = Lcl_abapgit_git_time=>get_unix( ).

    lv_string = |object { is_tag-object }{ cl_abap_char_utilities=>newline }|
             && |type { is_tag-type }{ cl_abap_char_utilities=>newline }|
             && |tag { Lcl_abapgit_git_tag=>remove_tag_prefix( is_tag-tag ) }{ cl_abap_char_utilities=>newline }|
             && |tagger { is_tag-tagger_name } <{ is_tag-tagger_email }> { lv_time }|
             && |{ cl_abap_char_utilities=>newline }|
             && |{ cl_abap_char_utilities=>newline }|
             && |{ is_tag-message }|.

    rv_data = Lcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).

  ENDMETHOD.
  METHOD encode_tree.

    CONSTANTS: lc_null TYPE x VALUE '00'.

    DATA: lv_string  TYPE string,
          lt_nodes   LIKE it_nodes,
          lv_hex20   TYPE x LENGTH 20,
          lv_xstring TYPE xstring.

    FIELD-SYMBOLS: <ls_node> LIKE LINE OF it_nodes.


    lt_nodes = sort_tree( it_nodes ).

    LOOP AT lt_nodes ASSIGNING <ls_node>.
      ASSERT NOT <ls_node>-chmod IS INITIAL.
      ASSERT NOT <ls_node>-name IS INITIAL.
      ASSERT NOT <ls_node>-sha1 IS INITIAL.

      CONCATENATE <ls_node>-chmod <ls_node>-name INTO lv_string SEPARATED BY space.
      lv_xstring = Lcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).

      lv_hex20 = to_upper( <ls_node>-sha1 ).
      CONCATENATE rv_data lv_xstring lc_null lv_hex20 INTO rv_data IN BYTE MODE.
    ENDLOOP.

  ENDMETHOD.
  METHOD get_length.

* https://github.com/git/git/blob/master/Documentation/technical/pack-format.txt

* n-byte sizeN (as long as MSB is set, each 7-bit)
*    size0..sizeN form 4+7+7+..+7 bit integer, size0
*    is the least significant part, and sizeN is the
*    most significant part.

    DATA: lv_x           TYPE x,
          lv_length_bits TYPE string,
          lv_bitbyte     TYPE Lif_abapgit_git_definitions=>ty_bitbyte.


    lv_x = cv_data(1).
    lv_bitbyte = Lcl_abapgit_convert=>x_to_bitbyte( lv_x ).

    cv_data = cv_data+1.
    lv_length_bits = lv_bitbyte+4.

    WHILE lv_bitbyte(1) <> '0'.
      lv_x = cv_data(1).
      lv_bitbyte = Lcl_abapgit_convert=>x_to_bitbyte( lv_x ).
      cv_data = cv_data+1.
      CONCATENATE lv_bitbyte+1 lv_length_bits INTO lv_length_bits.
    ENDWHILE.

    ev_length = Lcl_abapgit_convert=>bitbyte_to_int( lv_length_bits ).

  ENDMETHOD.
  METHOD get_type.

    CONSTANTS: lc_mask TYPE x VALUE 112.
    DATA: lv_xtype TYPE x.

    lv_xtype = iv_x BIT-AND lc_mask.

    CASE lv_xtype.
      WHEN 16.
        rv_type = Lif_abapgit_git_definitions=>c_type-commit.
      WHEN 32.
        rv_type = Lif_abapgit_git_definitions=>c_type-tree.
      WHEN 48.
        rv_type = Lif_abapgit_git_definitions=>c_type-blob.
      WHEN 64.
        rv_type = Lif_abapgit_git_definitions=>c_type-tag.
      WHEN 112.
        rv_type = Lif_abapgit_git_definitions=>c_type-ref_d.
      WHEN OTHERS.
        Lcx_abapgit_exception=>raise( |Todo, unknown git pack type| ).
    ENDCASE.

  ENDMETHOD.
  METHOD sort_tree.

    TYPES: BEGIN OF ty_sort,
             sort TYPE string,
             node TYPE ty_node,
           END OF ty_sort.

    DATA: lt_sort TYPE STANDARD TABLE OF ty_sort WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_sort> LIKE LINE OF lt_sort,
                   <ls_node> LIKE LINE OF it_nodes.


    LOOP AT it_nodes ASSIGNING <ls_node>.
      APPEND INITIAL LINE TO lt_sort ASSIGNING <ls_sort>.
      IF <ls_node>-chmod = Lif_abapgit_git_definitions=>c_chmod-dir.
        CONCATENATE <ls_node>-name '/' INTO <ls_sort>-sort.
      ELSE.
        <ls_sort>-sort = <ls_node>-name.
      ENDIF.
      <ls_sort>-node = <ls_node>.
    ENDLOOP.

* following has to be done, or unpack will fail on server side
    SORT lt_sort BY sort ASCENDING.

    LOOP AT lt_sort ASSIGNING <ls_sort>.
      APPEND <ls_sort>-node TO rt_nodes.
    ENDLOOP.

  ENDMETHOD.
  METHOD type_and_length.

* see http://stefan.saasen.me/articles/git-clone-in-haskell-from-the-bottom-up/#pack_file_objects

    DATA: lv_type   TYPE i,
          lv_length TYPE i,
          lv_hex    TYPE x LENGTH 1.


    CASE iv_type.
      WHEN Lif_abapgit_git_definitions=>c_type-commit.
        lv_type = 16.
      WHEN Lif_abapgit_git_definitions=>c_type-tree.
        lv_type = 32.
      WHEN Lif_abapgit_git_definitions=>c_type-blob.
        lv_type = 48.
      WHEN Lif_abapgit_git_definitions=>c_type-tag.
        lv_type = 64.
      WHEN Lif_abapgit_git_definitions=>c_type-ref_d.
        lv_type = 112.
      WHEN OTHERS.
        Lcx_abapgit_exception=>raise( |Unexpected object type while encoding pack| ).
    ENDCASE.

    lv_length = iv_length.

* first byte
    IF lv_length > 15.
      lv_hex = 128.
    ENDIF.
    lv_hex = lv_hex + lv_type + lv_length MOD 16.
    rv_xstring = lv_hex.
    lv_length = lv_length DIV 16.

* subsequent bytes
    WHILE lv_length >= 128.
      lv_hex = 128 + lv_length MOD 128.
      CONCATENATE rv_xstring lv_hex INTO rv_xstring IN BYTE MODE.
      lv_length = lv_length DIV 128.
    ENDWHILE.

* last byte
    IF lv_length > 0.
      lv_hex = lv_length.
      CONCATENATE rv_xstring lv_hex INTO rv_xstring IN BYTE MODE.
    ENDIF.

  ENDMETHOD.
  METHOD zlib_decompress.

    DATA: ls_data           TYPE Lcl_abapgit_zlib=>ty_decompress,
          lv_compressed_len TYPE i,
          lv_adler32        TYPE Lif_abapgit_git_definitions=>ty_adler32.


    ls_data = Lcl_abapgit_zlib=>decompress( cv_data ).
    lv_compressed_len = ls_data-compressed_len.
    cv_decompressed = ls_data-raw.

    IF lv_compressed_len IS INITIAL.
      Lcx_abapgit_exception=>raise( |Decompression falied :o/| ).
    ENDIF.

    cv_data = cv_data+lv_compressed_len.

    lv_adler32 = Lcl_abapgit_hash=>adler32( cv_decompressed ).
    IF cv_data(4) <> lv_adler32.
      cv_data = cv_data+1.
    ENDIF.
    IF cv_data(4) <> lv_adler32.
      cv_data = cv_data+1.
    ENDIF.
    IF cv_data(4) <> lv_adler32.
      Lcx_abapgit_exception=>raise( |Wrong Adler checksum| ).
    ENDIF.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GIT_PACK implementation

*>>>>>>> ZCL_ABAPGIT_GIT_PORCELAIN <<<<<<<*

*"* macro definitions
*include zcl_abapgit_git_porcelain=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_git_porcelain=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_git_porcelain=====ccau.
*CLASS SHRITEFUH64VYIPO5IWUYB3KWYPSFY DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_porcelain DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KWYPSFY.



class LCL_ABAPGIT_GIT_PORCELAIN implementation.
*"* method's implementations
*include methods.
  METHOD build_trees.

    DATA: lt_nodes   TYPE Lcl_abapgit_git_pack=>ty_nodes_tt,
          ls_tree    LIKE LINE OF rt_trees,
          lv_len     TYPE i,
          lt_folders TYPE ty_folders_tt.

    FIELD-SYMBOLS: <ls_folder> LIKE LINE OF lt_folders,
                   <ls_node>   LIKE LINE OF lt_nodes,
                   <ls_sub>    LIKE LINE OF lt_folders,
                   <ls_exp>    LIKE LINE OF it_expanded.


    lt_folders = find_folders( it_expanded ).

* start with the deepest folders
    SORT lt_folders BY count DESCENDING.

    LOOP AT lt_folders ASSIGNING <ls_folder>.
      CLEAR lt_nodes.

* files
      LOOP AT it_expanded ASSIGNING <ls_exp> USING KEY path_name WHERE path = <ls_folder>-path.
        APPEND INITIAL LINE TO lt_nodes ASSIGNING <ls_node>.
        <ls_node>-chmod = <ls_exp>-chmod.
        <ls_node>-name  = <ls_exp>-name.
        <ls_node>-sha1  = <ls_exp>-sha1.
      ENDLOOP.

* folders
      LOOP AT lt_folders ASSIGNING <ls_sub> WHERE count = <ls_folder>-count + 1.
        lv_len = strlen( <ls_folder>-path ).
        IF strlen( <ls_sub>-path ) > lv_len AND <ls_sub>-path(lv_len) = <ls_folder>-path.
          APPEND INITIAL LINE TO lt_nodes ASSIGNING <ls_node>.
          <ls_node>-chmod = Lif_abapgit_git_definitions=>c_chmod-dir.

* extract folder name, this can probably be done easier using regular expressions
          <ls_node>-name = <ls_sub>-path+lv_len.
          lv_len = strlen( <ls_node>-name ) - 1.
          <ls_node>-name = <ls_node>-name(lv_len).

          <ls_node>-sha1 = <ls_sub>-sha1.
        ENDIF.
      ENDLOOP.

      CLEAR ls_tree.
      ls_tree-path = <ls_folder>-path.
      ls_tree-data = Lcl_abapgit_git_pack=>encode_tree( lt_nodes ).
      ls_tree-sha1 = Lcl_abapgit_hash=>sha1_tree( ls_tree-data ).
      APPEND ls_tree TO rt_trees.

      <ls_folder>-sha1 = ls_tree-sha1.
    ENDLOOP.

  ENDMETHOD.
  METHOD create_annotated_tag.

    DATA: lv_tag          TYPE xstring,
          lt_objects      TYPE Lif_abapgit_definitions=>ty_objects_tt,
          lv_pack         TYPE xstring,
          ls_object       LIKE LINE OF lt_objects,
          ls_tag          TYPE Lcl_abapgit_git_pack=>ty_tag,
          lv_new_tag_sha1 TYPE Lif_abapgit_git_definitions=>ty_sha1.

* new tag
    ls_tag-object       = is_tag-sha1.
    ls_tag-type         = Lif_abapgit_git_definitions=>c_type-commit.
    ls_tag-tag          = is_tag-name.
    ls_tag-tagger_name  = is_tag-tagger_name.
    ls_tag-tagger_email = is_tag-tagger_email.
    ls_tag-message      = is_tag-message
                      && |{ cl_abap_char_utilities=>newline }|
                      && |{ cl_abap_char_utilities=>newline }|
                      && is_tag-body.

    lv_tag = Lcl_abapgit_git_pack=>encode_tag( ls_tag ).

    lv_new_tag_sha1 = Lcl_abapgit_hash=>sha1_tag( lv_tag ).

    ls_object-sha1  = lv_new_tag_sha1.
    ls_object-type  = Lif_abapgit_git_definitions=>c_type-tag.
    ls_object-data  = lv_tag.
    ls_object-index = 1.
    APPEND ls_object TO lt_objects.

    lv_pack = Lcl_abapgit_git_pack=>encode( lt_objects ).

    Lcl_abapgit_git_transport=>receive_pack(
      iv_url         = iv_url
      iv_old         = c_zero
      iv_new         = lv_new_tag_sha1
      iv_branch_name = is_tag-name
      iv_pack        = lv_pack ).

  ENDMETHOD.
  METHOD create_branch.

    IF iv_name CS ` `.
      Lcx_abapgit_exception=>raise( 'Branch name cannot contain blank spaces' ).
    ENDIF.

    Lcl_abapgit_git_transport=>receive_pack(
      iv_url         = iv_url
      iv_old         = c_zero
      iv_new         = iv_from
      iv_branch_name = iv_name
      iv_pack        = empty_packfile( ) ).

  ENDMETHOD.
  METHOD create_lightweight_tag.

    Lcl_abapgit_git_transport=>receive_pack(
      iv_url         = iv_url
      iv_old         = c_zero
      iv_new         = is_tag-sha1
      iv_branch_name = is_tag-name
      iv_pack        = empty_packfile( ) ).

  ENDMETHOD.
  METHOD create_tag.

    IF is_tag-name CS ` `.
      Lcx_abapgit_exception=>raise( 'Tag name cannot contain blank spaces' ).
    ENDIF.

    CASE is_tag-type.
      WHEN Lif_abapgit_git_definitions=>c_git_branch_type-annotated_tag.

        create_annotated_tag(
          is_tag = is_tag
          iv_url = iv_url ).

      WHEN Lif_abapgit_git_definitions=>c_git_branch_type-lightweight_tag.

        create_lightweight_tag(
          is_tag = is_tag
          iv_url = iv_url ).

      WHEN OTHERS.

        Lcx_abapgit_exception=>raise( |Invalid tag type: { is_tag-type }| ).

    ENDCASE.

  ENDMETHOD.
  METHOD delete_annotated_tag.

    DATA:
      lo_branches TYPE REF TO Lcl_abapgit_git_branch_list,
      lv_tag      TYPE string,
      ls_tag      TYPE Lif_abapgit_git_definitions=>ty_git_branch,
      lt_tags     TYPE Lif_abapgit_git_definitions=>ty_git_branch_list_tt.

    " For annotated tags, find the correct commit
    lo_branches = Lcl_abapgit_git_transport=>branches( iv_url ).
    lt_tags     = lo_branches->get_tags_only( ).
    lv_tag      = Lcl_abapgit_git_tag=>remove_peel( is_tag-name ).

    READ TABLE lt_tags INTO ls_tag WITH KEY name_key COMPONENTS name = lv_tag.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Annotated tag { lv_tag } not found| ).
    ENDIF.

    Lcl_abapgit_git_transport=>receive_pack(
      iv_url         = iv_url
      iv_old         = ls_tag-sha1
      iv_new         = c_zero
      iv_branch_name = ls_tag-name ).

  ENDMETHOD.
  METHOD delete_branch.

    Lcl_abapgit_git_transport=>receive_pack(
      iv_url         = iv_url
      iv_old         = is_branch-sha1
      iv_new         = c_zero
      iv_branch_name = is_branch-name ).

  ENDMETHOD.
  METHOD delete_lightweight_tag.

    Lcl_abapgit_git_transport=>receive_pack(
      iv_url         = iv_url
      iv_old         = is_tag-sha1
      iv_new         = c_zero
      iv_branch_name = is_tag-name ).

  ENDMETHOD.
  METHOD delete_tag.

    IF is_tag-name CS Lif_abapgit_git_definitions=>c_git_branch-peel.

      delete_annotated_tag(
        is_tag = is_tag
        iv_url = iv_url ).

    ELSE.

      delete_lightweight_tag(
        is_tag = is_tag
        iv_url = iv_url ).

    ENDIF.

  ENDMETHOD.
  METHOD empty_packfile.

    " For avoiding "client MUST send an empty packfile" error
    " https://github.com/git/git/blob/master/Documentation/gitprotocol-pack.txt#L595-L599

    DATA lt_objects TYPE Lif_abapgit_definitions=>ty_objects_tt.

    rv_pack = Lcl_abapgit_git_pack=>encode( lt_objects ).

  ENDMETHOD.
  METHOD find_folders.

    DATA: lt_paths TYPE TABLE OF string,
          lv_split TYPE string,
          lv_path  TYPE string.

    FIELD-SYMBOLS: <ls_folder> LIKE LINE OF rt_folders,
                   <ls_new>    LIKE LINE OF rt_folders,
                   <ls_exp>    LIKE LINE OF it_expanded.


    LOOP AT it_expanded ASSIGNING <ls_exp>.
      READ TABLE rt_folders WITH KEY path = <ls_exp>-path TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO rt_folders ASSIGNING <ls_folder>.
        <ls_folder>-path = <ls_exp>-path.
      ENDIF.
    ENDLOOP.

* add empty folders
    LOOP AT rt_folders ASSIGNING <ls_folder>.
      SPLIT <ls_folder>-path AT '/' INTO TABLE lt_paths.

      CLEAR lv_path.
      LOOP AT lt_paths INTO lv_split.
        CONCATENATE lv_path lv_split '/' INTO lv_path.
        READ TABLE rt_folders WITH KEY path = lv_path TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO rt_folders ASSIGNING <ls_new>.
          <ls_new>-path = lv_path.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    LOOP AT rt_folders ASSIGNING <ls_folder>.
      FIND ALL OCCURRENCES OF '/' IN <ls_folder>-path MATCH COUNT <ls_folder>-count.
    ENDLOOP.

  ENDMETHOD.
  METHOD full_tree.

    DATA: ls_object LIKE LINE OF it_objects,
          ls_commit TYPE Lcl_abapgit_git_pack=>ty_commit.

    READ TABLE it_objects INTO ls_object
      WITH KEY type COMPONENTS
        type = Lif_abapgit_git_definitions=>c_type-commit
        sha1 = iv_parent.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'commit not found' ).
    ENDIF.
    ls_commit = Lcl_abapgit_git_pack=>decode_commit( ls_object-data ).

    rt_expanded = walk_tree( it_objects = it_objects
                             iv_tree    = ls_commit-tree
                             iv_base    = '/' ).

  ENDMETHOD.
  METHOD pull.

    DATA: ls_object TYPE Lif_abapgit_definitions=>ty_object,
          ls_commit TYPE Lcl_abapgit_git_pack=>ty_commit.

    READ TABLE it_objects INTO ls_object
      WITH KEY type COMPONENTS
        type = Lif_abapgit_git_definitions=>c_type-commit
        sha1 = iv_commit.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Commit/Branch not found.' ).
    ENDIF.

    ls_commit = Lcl_abapgit_git_pack=>decode_commit( ls_object-data ).

    walk( EXPORTING it_objects = it_objects
                    iv_sha1    = ls_commit-tree
                    iv_path    = '/'
          CHANGING  ct_files   = rt_files ).

  ENDMETHOD.
  METHOD pull_by_branch.

    Lcl_abapgit_git_transport=>upload_pack_by_branch(
      EXPORTING
        iv_url          = iv_url
        iv_branch_name  = iv_branch_name
        iv_deepen_level = iv_deepen_level
      IMPORTING
        et_objects      = rs_result-objects
        ev_branch       = rs_result-commit ).

    rs_result-files = pull( iv_commit  = rs_result-commit
                            it_objects = rs_result-objects ).

  ENDMETHOD.
  METHOD pull_by_commit.

    Lcl_abapgit_git_transport=>upload_pack_by_commit(
      EXPORTING
        iv_url          = iv_url
        iv_hash         = iv_commit_hash
        iv_deepen_level = iv_deepen_level
      IMPORTING
        et_objects      = rs_result-objects
        ev_commit       = rs_result-commit ).

    rs_result-files = pull( iv_commit  = rs_result-commit
                            it_objects = rs_result-objects ).

  ENDMETHOD.
  METHOD push.

    DATA: lt_expanded TYPE Lif_abapgit_git_definitions=>ty_expanded_tt,
          lt_blobs    TYPE Lif_abapgit_git_definitions=>ty_files_tt,
          lv_sha1     TYPE Lif_abapgit_git_definitions=>ty_sha1,
          lv_new_tree TYPE Lif_abapgit_git_definitions=>ty_sha1,
          lt_trees    TYPE ty_trees_tt,
          lt_stage    TYPE Lif_abapgit_definitions=>ty_stage_tt.

    FIELD-SYMBOLS: <ls_stage>   LIKE LINE OF lt_stage,
                   <ls_updated> LIKE LINE OF rs_result-updated_files,
                   <ls_exp>     LIKE LINE OF lt_expanded.

    lt_expanded = full_tree( it_objects = it_old_objects
                             iv_parent  = iv_parent ).

    lt_stage = io_stage->get_all( ).
    LOOP AT lt_stage ASSIGNING <ls_stage>.

      " Save file ref to updated files table
      APPEND INITIAL LINE TO rs_result-updated_files ASSIGNING <ls_updated>.
      MOVE-CORRESPONDING <ls_stage>-file TO <ls_updated>.

      CASE <ls_stage>-method.
        WHEN Lif_abapgit_definitions=>c_method-add.

          APPEND <ls_stage>-file TO lt_blobs.

          READ TABLE lt_expanded ASSIGNING <ls_exp> WITH TABLE KEY path_name COMPONENTS
            name = <ls_stage>-file-filename
            path = <ls_stage>-file-path.
          IF sy-subrc <> 0. " new files
            APPEND INITIAL LINE TO lt_expanded ASSIGNING <ls_exp>.
            <ls_exp>-name  = <ls_stage>-file-filename.
            <ls_exp>-path  = <ls_stage>-file-path.
            <ls_exp>-chmod = Lif_abapgit_git_definitions=>c_chmod-file.
          ENDIF.

          lv_sha1 = Lcl_abapgit_hash=>sha1_blob( <ls_stage>-file-data ).
          IF <ls_exp>-sha1 <> lv_sha1.
            <ls_exp>-sha1 = lv_sha1.
          ENDIF.

          <ls_updated>-sha1 = lv_sha1.   "New sha1

        WHEN Lif_abapgit_definitions=>c_method-rm.
          READ TABLE lt_expanded ASSIGNING <ls_exp> WITH TABLE KEY path_name COMPONENTS
            name = <ls_stage>-file-filename
            path = <ls_stage>-file-path.
          ASSERT sy-subrc = 0.

          CLEAR <ls_exp>-sha1.           " Mark as deleted
          CLEAR <ls_updated>-sha1.       " Mark as deleted

        WHEN OTHERS.
          Lcx_abapgit_exception=>raise( 'stage method not supported, todo' ).
      ENDCASE.
    ENDLOOP.

    DELETE lt_expanded WHERE sha1 IS INITIAL.

    lt_trees = build_trees( lt_expanded ).

    receive_pack_push(
      EXPORTING
        is_comment     = is_comment
        it_trees       = lt_trees
        iv_branch_name = iv_branch_name
        iv_url         = iv_url
        iv_parent      = iv_parent
        iv_parent2     = io_stage->get_merge_source( )
        it_blobs       = lt_blobs
      IMPORTING
        ev_new_commit  = rs_result-branch
        et_new_objects = rs_result-new_objects
        ev_new_tree    = lv_new_tree ).

    APPEND LINES OF it_old_objects TO rs_result-new_objects.

    walk( EXPORTING it_objects = rs_result-new_objects
                    iv_sha1    = lv_new_tree
                    iv_path    = '/'
          CHANGING  ct_files   = rs_result-new_files ).

  ENDMETHOD.
  METHOD receive_pack_push.

    DATA: lv_time   TYPE Lcl_abapgit_git_time=>ty_unixtime,
          lv_commit TYPE xstring,
          lv_pack   TYPE xstring,
          ls_object LIKE LINE OF et_new_objects,
          ls_commit TYPE Lcl_abapgit_git_pack=>ty_commit,
          lv_uindex TYPE sy-index.

    FIELD-SYMBOLS: <ls_tree> LIKE LINE OF it_trees,
                   <ls_blob> LIKE LINE OF it_blobs.


    lv_time = Lcl_abapgit_git_time=>get_unix( ).

    READ TABLE it_trees ASSIGNING <ls_tree> WITH KEY path = '/'.
    ASSERT sy-subrc = 0.

* new commit
    ls_commit-committer = |{ is_comment-committer-name
      } <{ is_comment-committer-email }> { lv_time }|.
    IF is_comment-author-name IS NOT INITIAL.
      ls_commit-author = |{ is_comment-author-name
        } <{ is_comment-author-email }> { lv_time }|.
    ELSE.
      ls_commit-author = ls_commit-committer.
    ENDIF.

    ls_commit-tree      = <ls_tree>-sha1.
    ls_commit-parent    = iv_parent.
    ls_commit-parent2   = iv_parent2.
    ls_commit-body      = is_comment-comment.
    lv_commit = Lcl_abapgit_git_pack=>encode_commit( ls_commit ).

    ls_object-sha1 = Lcl_abapgit_hash=>sha1_commit( lv_commit ).
    ls_object-type = Lif_abapgit_git_definitions=>c_type-commit.
    ls_object-data = lv_commit.
    APPEND ls_object TO et_new_objects.

    LOOP AT it_trees ASSIGNING <ls_tree>.
      CLEAR ls_object.
      ls_object-sha1 = <ls_tree>-sha1.

      READ TABLE et_new_objects
        WITH KEY type COMPONENTS
          type = Lif_abapgit_git_definitions=>c_type-tree
          sha1 = ls_object-sha1
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
* two identical trees added at the same time, only add one to the pack
        CONTINUE.
      ENDIF.

      ls_object-type = Lif_abapgit_git_definitions=>c_type-tree.
      ls_object-data = <ls_tree>-data.
      lv_uindex = lv_uindex + 1.
      ls_object-index = lv_uindex.
      APPEND ls_object TO et_new_objects.
    ENDLOOP.

    LOOP AT it_blobs ASSIGNING <ls_blob>.
      CLEAR ls_object.
      ls_object-sha1 = Lcl_abapgit_hash=>sha1_blob( <ls_blob>-data ).

      READ TABLE et_new_objects
        WITH KEY type COMPONENTS
          type = Lif_abapgit_git_definitions=>c_type-blob
          sha1 = ls_object-sha1
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
* two identical files added at the same time, only add one blob to the pack
        CONTINUE.
      ENDIF.

      ls_object-type = Lif_abapgit_git_definitions=>c_type-blob.
* note <ls_blob>-data can be empty, #1857 allow empty files - some more checks needed?
      ls_object-data = <ls_blob>-data.
      lv_uindex = lv_uindex + 1.
      ls_object-index = lv_uindex.
      APPEND ls_object TO et_new_objects.
    ENDLOOP.

    lv_pack = Lcl_abapgit_git_pack=>encode( et_new_objects ).

    ev_new_commit = Lcl_abapgit_hash=>sha1_commit( lv_commit ).

    Lcl_abapgit_git_transport=>receive_pack(
      iv_url         = iv_url
      iv_old         = iv_parent
      iv_new         = ev_new_commit
      iv_branch_name = iv_branch_name
      iv_pack        = lv_pack ).

    ev_new_tree = ls_commit-tree.

  ENDMETHOD.
  METHOD walk.

    DATA: lv_path  TYPE string,
          ls_file  LIKE LINE OF ct_files,
          lt_nodes TYPE Lcl_abapgit_git_pack=>ty_nodes_tt.

    FIELD-SYMBOLS: <ls_tree> LIKE LINE OF it_objects,
                   <ls_blob> LIKE LINE OF it_objects,
                   <ls_node> LIKE LINE OF lt_nodes.


    READ TABLE it_objects ASSIGNING <ls_tree>
      WITH KEY type COMPONENTS
        type = Lif_abapgit_git_definitions=>c_type-tree
        sha1 = iv_sha1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Walk, tree not found' ).
    ENDIF.

    lt_nodes = Lcl_abapgit_git_pack=>decode_tree( <ls_tree>-data ).

    LOOP AT lt_nodes ASSIGNING <ls_node>.
      IF <ls_node>-chmod = Lif_abapgit_git_definitions=>c_chmod-file.
        READ TABLE it_objects ASSIGNING <ls_blob>
          WITH KEY type COMPONENTS
            type = Lif_abapgit_git_definitions=>c_type-blob
            sha1 = <ls_node>-sha1.
        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise( 'Walk, blob not found' ).
        ENDIF.

        CLEAR ls_file.
        ls_file-path     = iv_path.
        ls_file-filename = <ls_node>-name.
        ls_file-data     = <ls_blob>-data.
        ls_file-sha1     = <ls_blob>-sha1.
        APPEND ls_file TO ct_files.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_nodes ASSIGNING <ls_node> WHERE chmod = Lif_abapgit_git_definitions=>c_chmod-dir.
      CONCATENATE iv_path <ls_node>-name '/' INTO lv_path.

      walk( EXPORTING it_objects = it_objects
                      iv_sha1    = <ls_node>-sha1
                      iv_path    = lv_path
            CHANGING  ct_files   = ct_files ).
    ENDLOOP.

  ENDMETHOD.
  METHOD walk_tree.

    DATA: ls_object   LIKE LINE OF it_objects,
          lt_expanded LIKE rt_expanded,
          lt_nodes    TYPE Lcl_abapgit_git_pack=>ty_nodes_tt.

    FIELD-SYMBOLS: <ls_exp>  LIKE LINE OF rt_expanded,
                   <ls_node> LIKE LINE OF lt_nodes.


    READ TABLE it_objects INTO ls_object
      WITH KEY type COMPONENTS
        type = Lif_abapgit_git_definitions=>c_type-tree
        sha1 = iv_tree.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'tree not found' ).
    ENDIF.
    lt_nodes = Lcl_abapgit_git_pack=>decode_tree( ls_object-data ).

    LOOP AT lt_nodes ASSIGNING <ls_node>.
      CASE <ls_node>-chmod.
        WHEN Lif_abapgit_git_definitions=>c_chmod-file
            OR Lif_abapgit_git_definitions=>c_chmod-executable
            OR Lif_abapgit_git_definitions=>c_chmod-submodule.
          APPEND INITIAL LINE TO rt_expanded ASSIGNING <ls_exp>.
          <ls_exp>-path  = iv_base.
          <ls_exp>-name  = <ls_node>-name.
          <ls_exp>-sha1  = <ls_node>-sha1.
          <ls_exp>-chmod = <ls_node>-chmod.
        WHEN Lif_abapgit_git_definitions=>c_chmod-dir.
          lt_expanded = walk_tree(
            it_objects = it_objects
            iv_tree    = <ls_node>-sha1
            iv_base    = iv_base && <ls_node>-name && '/' ).
          APPEND LINES OF lt_expanded TO rt_expanded.
        WHEN OTHERS.
          Lcx_abapgit_exception=>raise( 'walk_tree: unknown chmod' ).
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GIT_PORCELAIN implementation

*>>>>>>> ZCL_ABAPGIT_GUI <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui===============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui===============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI implementation.
*"* method's implementations
*include methods.
  METHOD back.

    DATA lv_index TYPE i.
    DATA ls_stack LIKE LINE OF mt_stack.

    " If viewer is showing Internet page, then use browser navigation
    IF mi_html_viewer->get_url( ) CP 'http*'.
      mi_html_viewer->back( ).
      RETURN.
    ENDIF.

    lv_index = lines( mt_stack ).

    IF lv_index = 0.
      rv_exit = abap_true.
      RETURN.
    ENDIF.

    IF iv_graceful = abap_true AND back_graceful( ) = abap_true.
      RETURN.
    ENDIF.

    DO lv_index TIMES.
      READ TABLE mt_stack INDEX lv_index INTO ls_stack.
      ASSERT sy-subrc = 0.

      DELETE mt_stack INDEX lv_index.
      ASSERT sy-subrc = 0.

      lv_index = lv_index - 1.

      IF iv_to_bookmark = abap_false OR ls_stack-bookmark = abap_true.
        EXIT.
      ENDIF.
    ENDDO.

    mi_cur_page = ls_stack-page. " last page always stays
    render( ).

  ENDMETHOD.
  METHOD cache_html.

    rv_url = Lif_abapgit_gui_services~cache_asset(
      iv_text    = iv_text
      iv_type    = 'text'
      iv_subtype = 'html' ).

  ENDMETHOD.
  METHOD call_page.

    DATA: ls_stack TYPE ty_page_stack.

    IF iv_replacing = abap_false AND NOT mi_cur_page IS INITIAL.
      ls_stack-page     = mi_cur_page.
      ls_stack-bookmark = iv_with_bookmark.
      APPEND ls_stack TO mt_stack.
    ENDIF.

    mi_cur_page = ii_page.
    render( ).

  ENDMETHOD.
  METHOD constructor.

    IF io_component IS BOUND.
      IF Lcl_abapgit_gui_utils=>is_renderable( io_component ) = abap_true.
        mi_cur_page ?= io_component. " direct page
      ELSE.
        IF Lcl_abapgit_gui_utils=>is_event_handler( io_component ) = abap_false.
          Lcx_abapgit_exception=>raise( 'Component must be renderable or be an event handler' ).
        ENDIF.
        mi_router ?= io_component.
      ENDIF.
    ENDIF.

    CREATE OBJECT mo_html_parts.

    mv_rollback_on_error = iv_rollback_on_error.
    mi_asset_man      = ii_asset_man.
    mi_hotkey_ctl     = ii_hotkey_ctl.
    mi_html_processor = ii_html_processor. " Maybe improve to middlewares stack ??
    startup( ).

  ENDMETHOD.
  METHOD free.

    SET HANDLER on_event FOR mi_html_viewer ACTIVATION space.
    mi_html_viewer->close_document( ).
    mi_html_viewer->free( ).
    FREE mi_html_viewer.

  ENDMETHOD.
  METHOD go_home.

    DATA ls_stack LIKE LINE OF mt_stack.

    IF mi_router IS BOUND.
      CLEAR: mt_stack, mt_event_handlers.
      APPEND mi_router TO mt_event_handlers.

      on_event( action = |{ iv_action }| ).
    ELSE.
      IF lines( mt_stack ) > 0.
        READ TABLE mt_stack INTO ls_stack INDEX 1.
        mi_cur_page = ls_stack-page.
      ENDIF.
      render( ).
    ENDIF.

  ENDMETHOD.
  METHOD handle_action.

    DATA:
      lx_exception TYPE REF TO Lcx_abapgit_exception,
      li_handler   TYPE REF TO Lif_abapgit_gui_event_handler,
      li_event     TYPE REF TO Lif_abapgit_gui_event,
      ls_handled   TYPE Lif_abapgit_gui_event_handler=>ty_handling_result.

    CREATE OBJECT li_event TYPE Lcl_abapgit_gui_event
      EXPORTING
        ii_gui_services = me
        iv_action       = iv_action
        iv_getdata      = iv_getdata
        it_postdata     = it_postdata.

    TRY.
        ls_handled = Lcl_abapgit_exit=>get_instance( )->on_event( li_event ).

        IF ls_handled-state = c_event_state-not_handled.
          LOOP AT mt_event_handlers INTO li_handler.
            ls_handled = li_handler->on_event( li_event ).
            IF ls_handled-state IS NOT INITIAL AND ls_handled-state <> c_event_state-not_handled. " is handled
              EXIT.
            ENDIF.
          ENDLOOP.
        ENDIF.

        IF is_page_modal( mi_cur_page ) = abap_true AND NOT (
          ls_handled-state = c_event_state-re_render OR
          ls_handled-state = c_event_state-go_back OR
          ls_handled-state = c_event_state-no_more_act ).
          " Restrict new page switching from modals
          ls_handled-state = c_event_state-no_more_act.
        ENDIF.

        CASE ls_handled-state.
          WHEN c_event_state-re_render.
            render( ).
          WHEN c_event_state-new_page.
            call_page( ls_handled-page ).
          WHEN c_event_state-new_page_w_bookmark.
            call_page(
              ii_page = ls_handled-page
              iv_with_bookmark = abap_true ).
          WHEN c_event_state-new_page_replacing.
            call_page(
              ii_page = ls_handled-page
              iv_replacing = abap_true ).
          WHEN c_event_state-go_back.
            back( ).
          WHEN c_event_state-go_back_to_bookmark.
            back( iv_to_bookmark = abap_true ).
          WHEN c_event_state-no_more_act.
            " Do nothing, handling completed
          WHEN OTHERS.
            Lcx_abapgit_exception=>raise( |Unknown action: { iv_action }| ).
        ENDCASE.

      CATCH Lcx_abapgit_cancel ##NO_HANDLER.
        " Do nothing = c_event_state-no_more_act
      CATCH Lcx_abapgit_exception INTO lx_exception.
        handle_error( lx_exception ).
    ENDTRY.

  ENDMETHOD.
  METHOD handle_error.

    DATA: li_gui_error_handler TYPE REF TO Lif_abapgit_gui_error_handler,
          lx_exception         TYPE REF TO cx_root.

    IF mv_rollback_on_error = abap_true.
      ROLLBACK WORK.
    ENDIF.

    TRY.
        li_gui_error_handler ?= mi_cur_page.

        IF li_gui_error_handler IS BOUND AND li_gui_error_handler->handle_error( ix_exception ) = abap_true.
          " We rerender the current page to display the error box
          render( ).
        ELSEIF ix_exception->mi_log IS BOUND.
          mi_common_log = ix_exception->mi_log.
          IF mi_common_log->get_log_level( ) >= Lif_abapgit_log=>c_log_level-warning.
            Lcl_abapgit_log_viewer=>show_log( mi_common_log ).
          ENDIF.
        ELSE.
          MESSAGE ix_exception TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

      CATCH Lcx_abapgit_exception cx_sy_move_cast_error INTO lx_exception.
        " In case of fire we just fallback to plain old message
        MESSAGE lx_exception TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.
  METHOD on_event.

    handle_action(
      iv_action   = action
      iv_getdata  = getdata
      it_postdata = postdata ).

  ENDMETHOD.
  METHOD render.

    DATA: lv_url  TYPE string,
          lv_html TYPE string,
          li_html TYPE REF TO Lif_abapgit_html.

    IF mi_cur_page IS NOT BOUND.
      Lcx_abapgit_exception=>raise( 'GUI error: no current page' ).
    ENDIF.

    CLEAR mt_event_handlers.
    mo_html_parts->clear( ).

    IF mi_router IS BOUND AND is_page_modal( mi_cur_page ) = abap_false.
      " No global commands in modals
      APPEND mi_router TO mt_event_handlers.
    ENDIF.

    IF mi_hotkey_ctl IS BOUND.
      mi_hotkey_ctl->reset( ).
    ENDIF.

    li_html = mi_cur_page->render( ).
    lv_html = li_html->render( abap_true ).

    IF mi_html_processor IS BOUND.
      lv_html = mi_html_processor->process(
        iv_html         = lv_html
        ii_gui_services = me ).
    ENDIF.

    lv_url = cache_html( lv_html ).
    mi_html_viewer->show_url( lv_url ).

  ENDMETHOD.
  METHOD set_focus.
    mi_html_viewer->set_focus( ).
  ENDMETHOD.
  METHOD startup.

    DATA: lt_events TYPE cntl_simple_events,
          ls_event  LIKE LINE OF lt_events,
          lt_assets TYPE Lif_abapgit_gui_asset_manager=>ty_web_assets.

    FIELD-SYMBOLS <ls_asset> LIKE LINE OF lt_assets.


    mi_html_viewer = Lcl_abapgit_ui_factory=>get_html_viewer( ).

    IF mi_asset_man IS BOUND.
      lt_assets = mi_asset_man->get_all_assets( ).
      LOOP AT lt_assets ASSIGNING <ls_asset> WHERE is_cacheable = abap_true.
        Lif_abapgit_gui_services~cache_asset(
          iv_xdata   = <ls_asset>-content
          iv_url     = <ls_asset>-url
          iv_type    = <ls_asset>-type
          iv_subtype = <ls_asset>-subtype ).
      ENDLOOP.
    ENDIF.

    ls_event-eventid    = mi_html_viewer->c_id_sapevent.
    ls_event-appl_event = abap_true.
    APPEND ls_event TO lt_events.

    mi_html_viewer->set_registered_events( lt_events ).
    SET HANDLER on_event FOR mi_html_viewer.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_services~cache_asset.

    TYPES ty_hex TYPE x LENGTH 200.
    TYPES ty_char TYPE c LENGTH 200.

    DATA: lt_xdata TYPE STANDARD TABLE OF ty_hex WITH DEFAULT KEY,
          lv_size  TYPE i,
          lt_html  TYPE STANDARD TABLE OF ty_char WITH DEFAULT KEY.

    ASSERT iv_text IS SUPPLIED OR iv_xdata IS SUPPLIED.

    IF iv_text IS SUPPLIED. " String input
      Lcl_abapgit_convert=>string_to_tab(
         EXPORTING
           iv_str  = iv_text
         IMPORTING
           ev_size = lv_size
           et_tab  = lt_html ).

      mi_html_viewer->load_data(
        EXPORTING
          iv_type         = iv_type
          iv_subtype      = iv_subtype
          iv_size         = lv_size
          iv_url          = iv_url
        IMPORTING
          ev_assigned_url = rv_url
        CHANGING
          ct_data_table   = lt_html ).
    ELSE. " Raw input
      Lcl_abapgit_convert=>xstring_to_bintab(
        EXPORTING
          iv_xstr   = iv_xdata
        IMPORTING
          ev_size   = lv_size
          et_bintab = lt_xdata ).

      mi_html_viewer->load_data(
        EXPORTING
          iv_type         = iv_type
          iv_subtype      = iv_subtype
          iv_size         = lv_size
          iv_url          = iv_url
        IMPORTING
          ev_assigned_url = rv_url
        CHANGING
          ct_data_table   = lt_xdata ).
    ENDIF.

    ASSERT sy-subrc = 0. " Image data error

  ENDMETHOD.
  METHOD Lif_abapgit_gui_services~get_current_page_name.

    DATA li_page_hoc TYPE REF TO Lcl_abapgit_gui_page_hoc.

    IF mi_cur_page IS BOUND.
      rv_page_name = cl_abap_classdescr=>describe_by_object_ref( mi_cur_page )->get_relative_name( ).

      " For HOC components return name of child component instead
      IF rv_page_name = 'LCL_ABAPGIT_GUI_PAGE_HOC'.
        li_page_hoc ?= mi_cur_page.
        IF li_page_hoc->get_child( ) IS BOUND.
          rv_page_name = cl_abap_classdescr=>describe_by_object_ref(
                           li_page_hoc->get_child( ) )->get_relative_name( ).
        ENDIF.
      ENDIF.
    ENDIF." ELSE - return is empty => initial page

  ENDMETHOD.
  METHOD Lif_abapgit_gui_services~get_hotkeys_ctl.
    ri_hotkey_ctl = mi_hotkey_ctl.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_services~get_html_parts.
    ro_parts = mo_html_parts.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_services~get_log.

    IF iv_create_new = abap_true OR mi_common_log IS NOT BOUND.
      CREATE OBJECT mi_common_log TYPE Lcl_abapgit_log.
    ENDIF.

    ri_log = mi_common_log.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_services~register_event_handler.
    ASSERT ii_event_handler IS BOUND.
    INSERT ii_event_handler INTO mt_event_handlers INDEX 1.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_services~register_page_asset.

    " Maybe forbid registering cachable existing assets, maybe this is the right place (see also asset_man commments)

    mi_asset_man->register_asset(
      iv_url = iv_url
      iv_type = iv_type
      iv_mime_name = iv_mime_name
      iv_inline = iv_inline
      " This registering will happen after initialization so all cachable already cached
      iv_cachable = abap_false ).

  ENDMETHOD.
  METHOD back_graceful.

    DATA li_handler TYPE REF TO Lif_abapgit_gui_event_handler.
    DATA ls_handled TYPE Lif_abapgit_gui_event_handler=>ty_handling_result.

    " This code can be potentially improved
    " Why send go_back to the topmost handler only ? It makes sense to notify the whole stack
    " But than how to handle re-render ? render if at least one handler asks for it ?
    " Probably that's the way but needs a relevant example. Postponed arch decision.
    READ TABLE mt_event_handlers INTO li_handler INDEX 1.
    IF sy-subrc = 0.
      ls_handled = li_handler->on_event( Lcl_abapgit_gui_event=>new(
        iv_action       = Lif_abapgit_definitions=>c_action-go_back
        ii_gui_services = me ) ).
      IF ls_handled-state = c_event_state-re_render. " soft exit, probably popup
        render( ).
        rv_handled = abap_true.
      ELSEIF ls_handled-state = c_event_state-no_more_act. " soft exit, probably GUI popup
        rv_handled = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD is_page_modal.

    DATA li_modal TYPE REF TO Lif_abapgit_gui_modal.

    TRY.
        IF ii_page IS BOUND.
          li_modal ?= ii_page.
          rv_yes = li_modal->is_modal( ).
        ENDIF.
      CATCH cx_sy_move_cast_error.
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_AQBG <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_aqbg=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_aqbg=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_AQBG implementation.
*"* method's implementations
*include methods.
  METHOD get_field_rules.

    ro_result = Lcl_abapgit_field_rules=>create( ).

    ro_result->add(
      iv_table     = 'AQGDBBG'
      iv_field     = 'BGCNAM'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = 'AQGDBBG'
      iv_field     = 'BGUNAM'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user ).

    ro_result->add(
      iv_table     = 'AQGDBBG'
      iv_field     = 'BGCDAT'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-date
    )->add(
      iv_table     = 'AQGDBBG'
      iv_field     = 'BGUDAT'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-date ).

    ro_result->add(
      iv_table     = 'AQGDBBG'
      iv_field     = 'DEVC'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-package ).

  ENDMETHOD.
  METHOD get_generic.
    " transaction SQ03
    CREATE OBJECT ro_generic
      EXPORTING
        is_item        = ms_item
        io_field_rules = get_field_rules( )
        iv_language    = mv_language.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    SELECT SINGLE bgunam FROM aqgdbbg INTO rv_user WHERE num = ms_item-obj_name.
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

    DATA lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS <ls_bdcdata> LIKE LINE OF lt_bdcdata.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPMS38S'.
    <ls_bdcdata>-dynpro   = '0050'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'RS38S-BGNUM'.
    <ls_bdcdata>-fval = ms_item-obj_name.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode      = 'SQ03'
      it_bdcdata    = lt_bdcdata ).

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
endclass. "ZCL_ABAPGIT_OBJECT_AQBG implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_AQQU <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_aqqu=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_aqqu=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_AQQU implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD get_field_rules.

    ro_result = Lcl_abapgit_field_rules=>create( ).

* add rules here if needed

  ENDMETHOD.
  METHOD get_generic.
    " transaction SQ01
    CREATE OBJECT ro_generic
      EXPORTING
        is_item        = ms_item
        io_field_rules = get_field_rules( )
        iv_language    = mv_language.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    rv_user = c_user_unknown.
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

    DATA lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS <ls_bdcdata> LIKE LINE OF lt_bdcdata.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPMS38R'.
    <ls_bdcdata>-dynpro   = '0050'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'RS38R-QNUM'.
    <ls_bdcdata>-fval = ms_item-obj_name.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode      = 'SQ01'
      it_bdcdata    = lt_bdcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.
    get_generic( )->serialize( io_xml ).
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_AQQU implementation

