INTERFACE zif_ags_backend_tree
  PUBLIC .
  INTERFACES if_badi_interface .

  METHODS after_save
    IMPORTING
      !io_tree   TYPE REF TO zcl_ags_obj_tree
      !is_object TYPE zags_objects .
ENDINTERFACE.
