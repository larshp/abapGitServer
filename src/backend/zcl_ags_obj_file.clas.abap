class ZCL_AGS_OBJ_FILE definition
  public
  create public .

public section.

  interfaces ZIF_AGS_OBJECT .

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


METHOD zif_ags_object~serialize.

  rv_data = mv_data.

ENDMETHOD.


METHOD zif_ags_object~sha1.

  BREAK-POINT.

ENDMETHOD.
ENDCLASS.