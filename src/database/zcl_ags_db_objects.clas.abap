class ZCL_AGS_DB_OBJECTS definition
  public
  create private

  global friends ZCL_AGS_DB .

public section.

  methods SINGLE
    importing
      !IV_SHA1 type ZAGS_OBJECTS-SHA1
    returning
      value(RS_DATA) type ZAGS_OBJECTS
    raising
      ZCX_AGS_ERROR .
  methods MODIFY
    importing
      !IS_DATA type ZAGS_OBJECTS .
protected section.
private section.

  data MT_OBJECTS type ZAGS_OBJECTS_TT .
  data MV_FAKE type ABAP_BOOL .

  methods SET_FAKE .
ENDCLASS.



CLASS ZCL_AGS_DB_OBJECTS IMPLEMENTATION.


  METHOD modify.

    IF mv_fake = abap_true.
      DELETE mt_objects WHERE sha1 = is_data-sha1.
      INSERT is_data INTO TABLE mt_objects.
    ELSE.
      MODIFY zags_objects FROM is_data.                   "#EC CI_SUBRC
    ENDIF.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD set_fake.

    mv_fake = abap_true.

  ENDMETHOD.


  METHOD single.

    IF mv_fake = abap_true.
      READ TABLE mt_objects INTO rs_data
        WITH KEY sha1 = iv_sha1.                          "#EC CI_SUBRC
    ELSE.
      SELECT SINGLE * FROM zags_objects
        INTO rs_data
        WHERE sha1 = iv_sha1.
    ENDIF.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ags_error
        EXPORTING
          textid = zcx_ags_error=>m005
          sha1   = iv_sha1.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
