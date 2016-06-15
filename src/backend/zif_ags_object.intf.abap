INTERFACE zif_ags_object PUBLIC.

  CONSTANTS c_newline TYPE abap_char1 VALUE cl_abap_char_utilities=>newline ##NO_TEXT.

  METHODS type
    RETURNING
      VALUE(rv_type) TYPE zags_type.
  METHODS serialize
    RETURNING
      VALUE(rv_data) TYPE xstring
    RAISING
      zcx_ags_error.
  METHODS deserialize
    IMPORTING
      !iv_data TYPE xstring
    RAISING
      zcx_ags_error.
  METHODS sha1
    RETURNING
      VALUE(rv_sha1) TYPE zags_sha1
    RAISING
      zcx_ags_error.
  METHODS save
    RAISING
      zcx_ags_error.
ENDINTERFACE.