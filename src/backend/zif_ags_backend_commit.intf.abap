INTERFACE zif_ags_backend_commit
  PUBLIC .
  INTERFACES if_badi_interface .

  METHODS after_save
    IMPORTING
      !io_commit TYPE REF TO zcl_ags_obj_commit
      !is_object TYPE zags_objects .
ENDINTERFACE.
