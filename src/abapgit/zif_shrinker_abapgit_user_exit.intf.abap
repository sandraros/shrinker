INTERFACE zif_shrinker_abapgit_user_exit
  PUBLIC .

  METHODS is_to_be_deserialized
    IMPORTING
      object        TYPE tadir-object
      obj_name      TYPE tadir-obj_name
    RETURNING
      VALUE(result) TYPE abap_bool.

ENDINTERFACE.
