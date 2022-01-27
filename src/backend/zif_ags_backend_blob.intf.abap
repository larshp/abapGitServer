INTERFACE zif_ags_backend_blob
  PUBLIC .
  INTERFACES if_badi_interface .

  METHODS after_save
    IMPORTING
      !io_blob   TYPE REF TO zcl_ags_obj_blob
      !is_object TYPE zags_objects .
ENDINTERFACE.
