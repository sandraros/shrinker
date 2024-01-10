CLASS zcl_shrinker_copy_objects DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

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
        VALUE(result) TYPE REF TO zcl_shrinker_copy_objects.

    METHODS run
      IMPORTING
        source_package    TYPE devclass
        target_package    TYPE devclass
        user_exit         TYPE REF TO zif_shrinker_user_exit_abapgit
        objects           TYPE ty_object_copies
        test_mode         TYPE abap_bool DEFAULT abap_false
        transport_request TYPE trkorr OPTIONAL
      RETURNING
        VALUE(result)     TYPE REF TO zcl_shrinker_connect_abapgit
      RAISING
        zcx_shrinker.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA customizer TYPE REF TO zif_shrinker_abap_code_adapter.

ENDCLASS.


CLASS zcl_shrinker_copy_objects IMPLEMENTATION.
  METHOD create.
    result = NEW zcl_shrinker_copy_objects( ).
    result->customizer = customizer.
  ENDMETHOD.

  METHOD run.
    TYPES:
      BEGIN OF ty_map_object_file,
        "! AUTH - Authorization fields
        "! SUSO - Authorization objects
        "! TABL - Tables and Structures
        "! CLAS - Classes
        "! INTF - Interfaces
        object     TYPE trobjtype,
        file_names TYPE string_table,
      END OF ty_map_object_file.
    TYPES ty_map_object_files TYPE STANDARD TABLE OF ty_map_object_file WITH EMPTY KEY.

    DATA xstring TYPE xstring.

    DATA(map_object_files) = VALUE ty_map_object_files(
                                       ( object = 'AUTH' file_names = VALUE #( ( `<obj_name>.auth.xml` ) ) )
                                       ( object = 'SUSO' file_names = VALUE #( ( `<obj_name>.suso.xml` ) ) )
                                       ( object = 'TABL' file_names = VALUE #( ( `<obj_name>.tabl.xml` ) ) )
                                       ( object = 'DOMA' file_names = VALUE #( ( `<obj_name>.doma.xml` ) ) )
                                       ( object = 'DTEL' file_names = VALUE #( ( `<obj_name>.dtel.xml` ) ) )
                                       ( object     = 'CLAS'
                                         file_names = VALUE #( ( `<obj_name>.clas.abap` )
                                                               ( `<obj_name>.clas.locals_def.abap` )   " zif_abapgit_oo_object_fnc=>c_parts-locals_def
                                                               ( `<obj_name>.clas.locals_imp.abap` )   " zif_abapgit_oo_object_fnc=>c_parts-locals_imp
                                                               ( `<obj_name>.clas.testclasses.abap` )  " zif_abapgit_oo_object_fnc=>c_parts-testclasses
                                                               ( `<obj_name>.clas.macros.abap` )       " zif_abapgit_oo_object_fnc=>c_parts-macros
                                                               ( `<obj_name>.clas.xml` ) ) )
                                       ( object     = 'INTF'
                                         file_names = VALUE #( ( `<obj_name>.intf.abap` )
                                                               ( `<obj_name>.intf.xml` ) ) ) ).

    DATA(soar_source) = zcl_shrinker_connect_abapgit=>create( package = source_package ).

    soar_source->serialize( ).

    DATA(zip_soar_source) = soar_source->get_zip( ).

    DATA(soar_target) = zcl_shrinker_connect_abapgit=>create( package   = target_package
                                                              user_exit = user_exit ).

    soar_target->serialize( ).

    DATA(zip_soar_target) = soar_source->get_zip( ).

    LOOP AT objects REFERENCE INTO DATA(object).

      SELECT SINGLE devclass
          FROM tadir
          WHERE     pgmid    = 'R3TR'
                AND object   = @object->object
                AND obj_name = @object->source_obj_name
          INTO @DATA(source_object_package).

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_shrinker
          EXPORTING
            text = |Object { object->object } { object->source_obj_name } not found in TADIR|.
      ENDIF.

      DATA(map_object_file) = REF #( map_object_files[ object = object->object ] OPTIONAL ).
      DATA(file_names) = map_object_file->file_names.

      DATA(source_package_path) = soar_source->get_package_path( source_object_package ).
      " ZIP file requires to remove the leading slash character.
      SHIFT source_package_path LEFT DELETING LEADING '/'.

      DATA(target_package_path) = soar_target->get_package_path( target_package ).
      " ZIP file requires to remove the leading slash character.
      SHIFT target_package_path LEFT DELETING LEADING '/'.

      DATA(count_files_for_object) = 0.

      LOOP AT file_names REFERENCE INTO DATA(file_name).

        DATA(source_file_name) = source_package_path && replace( val  = file_name->*
                                                                 sub  = '<obj_name>'
                                                                 with = to_lower( object->source_obj_name ) ).
        DATA(target_file_name) = target_package_path && replace( val  = file_name->*
                                                                 sub  = '<obj_name>'
                                                                 with = to_lower( object->target_obj_name ) ).

        zip_soar_source->get( EXPORTING  name                    = source_file_name
                              IMPORTING  content                 = DATA(content_xstring)
                              EXCEPTIONS zip_index_error         = 1
                                         zip_decompression_error = 2
                                         OTHERS                  = 3 ).

        IF sy-subrc = 0.

          count_files_for_object = count_files_for_object + 1.

          DATA(content_string) = cl_abap_codepage=>convert_from( content_xstring ).

          LOOP AT objects REFERENCE INTO DATA(object_2).
            REPLACE ALL OCCURRENCES
                    OF REGEX `\<` && object_2->source_obj_name && `\>` " match only whole words
                    IN content_string
                    WITH object_2->target_obj_name
                    IGNORING CASE ##REGEX_POSIX.
          ENDLOOP.

          content_xstring = cl_abap_codepage=>convert_to( content_string ).

          soar_target->zip_replace( file_path = target_file_name
                                    content   = content_xstring ).

        ENDIF.

      ENDLOOP. " Continue with next possible file for objet

      IF count_files_for_object = 0.
        RAISE EXCEPTION TYPE zcx_shrinker
          EXPORTING
            text  = 'No file found for object &1 &2 in source Git repository'(003)
            msgv1 = object->object
            msgv2 = object->source_obj_name.
      ENDIF.

    ENDLOOP. " Continue with next object

    "==============================================================================
    " Recreate ABAP objects from the ZIP file = Deserialize objects via abapGit
    "==============================================================================

    IF test_mode = abap_false.

      TRY.

          soar_target->deserialize( transport_request = transport_request ).

        CATCH cx_root INTO DATA(error) ##NO_HANDLER.
          RAISE EXCEPTION TYPE zcx_shrinker
            EXPORTING
              text     = 'Error during copy of objects'
              previous = error.
      ENDTRY.

    ENDIF.

    result = soar_target.
  ENDMETHOD.
ENDCLASS.
