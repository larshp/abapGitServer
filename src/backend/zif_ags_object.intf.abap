INTERFACE zif_ags_object PUBLIC.

  METHODS deserialize
    IMPORTING
      !iv_data    TYPE xstring
      !iv_adler32 TYPE zags_adler32 OPTIONAL
    RAISING
      zcx_ags_error .
  METHODS get_adler32
    RETURNING
      VALUE(rv_adler32) TYPE zags_adler32
    RAISING
      zcx_ags_error .
  METHODS get_sha1
    RETURNING
      VALUE(rv_sha1) TYPE zags_sha1
    RAISING
      zcx_ags_error .
  METHODS get_type
    RETURNING
      VALUE(rv_type) TYPE zags_type .
  METHODS save
    RAISING
      zcx_ags_error .
  METHODS serialize
    RETURNING
      VALUE(rv_data) TYPE xstring
    RAISING
      zcx_ags_error .

ENDINTERFACE.
