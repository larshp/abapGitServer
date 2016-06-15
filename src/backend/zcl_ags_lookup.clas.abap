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

    SELECT SINGLE * FROM zags_objects
      INTO rs_data
      WHERE sha1 = iv_sha1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ags_error
        EXPORTING
          textid = zcx_ags_error=>m005
          sha1   = iv_sha1.
    ENDIF.

  ENDMETHOD.
ENDCLASS.