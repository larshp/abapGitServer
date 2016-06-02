class ZCL_AGS_OBJ_FILE definition
  public
  create public .

public section.

  interfaces ZIF_AGS_OBJECT .

  aliases C_NEWLINE
    for ZIF_AGS_OBJECT~C_NEWLINE .
  aliases DESERIALIZE
    for ZIF_AGS_OBJECT~DESERIALIZE .
  aliases SAVE
    for ZIF_AGS_OBJECT~SAVE .
  aliases SERIALIZE
    for ZIF_AGS_OBJECT~SERIALIZE .
  aliases SHA1
    for ZIF_AGS_OBJECT~SHA1 .

  methods GET_DATA
    returning
      value(RV_DATA) type XSTRING .
  methods SET_DATA
    importing
      !IV_DATA type XSTRING .
protected section.
private section.

  data MV_DATA type XSTRING .
ENDCLASS.



CLASS ZCL_AGS_OBJ_FILE IMPLEMENTATION.


METHOD get_data.

  rv_data = mv_data.

ENDMETHOD.


METHOD set_data.

  mv_data = iv_data.

ENDMETHOD.


METHOD zif_ags_object~deserialize.

  mv_data = iv_data.

ENDMETHOD.


METHOD zif_ags_object~save.

  DATA: ls_object TYPE zags_objects.

  ls_object-sha1 = sha1( ).
  ls_object-type = 'blob'.
  ls_object-data = serialize( ).

  MODIFY zags_objects FROM ls_object.

ENDMETHOD.


METHOD zif_ags_object~serialize.

  rv_data = mv_data.

ENDMETHOD.


METHOD zif_ags_object~sha1.

  rv_sha1 = zcl_ags_util=>sha1(
      iv_type = 'blob'
      iv_data = serialize( ) ) ##NO_TEXT.

ENDMETHOD.
ENDCLASS.