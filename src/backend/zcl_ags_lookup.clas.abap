CLASS zcl_ags_lookup DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS read_object
      IMPORTING
        !iv_sha1       TYPE zags_sha1
      RETURNING
        VALUE(rs_data) TYPE zags_objects
      RAISING
        zcx_ags_error.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AGS_LOOKUP IMPLEMENTATION.


  METHOD read_object.

    rs_data = zcl_ags_db=>get_objects( )->single( iv_sha1 ).

  ENDMETHOD.
ENDCLASS.
