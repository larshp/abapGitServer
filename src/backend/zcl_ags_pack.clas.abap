CLASS zcl_ags_pack DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS encode
      IMPORTING
        !io_commit     TYPE REF TO zcl_ags_obj_commit
      RETURNING
        VALUE(rv_pack) TYPE xstring.
    CLASS-METHODS decode
      IMPORTING
        !iv_pack TYPE xstring.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AGS_PACK IMPLEMENTATION.


  METHOD decode.

* todo

  ENDMETHOD.


  METHOD encode.

* todo

  ENDMETHOD.
ENDCLASS.