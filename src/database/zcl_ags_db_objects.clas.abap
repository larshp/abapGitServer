class ZCL_AGS_DB_OBJECTS definition
  public
  create private

  global friends ZCL_AGS_DB .

public section.

  methods LIST
    returning
      value(RT_OBJECTS) type ZAGS_OBJECTS_TT .
  methods MODIFY
    importing
      value(IS_DATA) type ZAGS_OBJECTS .
  methods SINGLE
    importing
      !IV_REPO type ZAGS_OBJECTS-REPO
      !IV_SHA1 type ZAGS_OBJECTS-SHA1
    returning
      value(RS_DATA) type ZAGS_OBJECTS
    raising
      ZCX_AGS_ERROR .
  PROTECTED SECTION.
private section.

  data MT_OBJECTS type ZAGS_OBJECTS_TT .
  data MV_FAKE type ABAP_BOOL .

  methods SET_FAKE .
ENDCLASS.



CLASS ZCL_AGS_DB_OBJECTS IMPLEMENTATION.


  METHOD list.
* used in migration program, see https://github.com/larshp/abapGitServer/issues/41

    IF mv_fake = abap_true.
      rt_objects = mt_objects.
    ELSE.
      SELECT * FROM zags_objects
        INTO CORRESPONDING FIELDS OF TABLE rt_objects.    "#EC CI_SUBRC
    ENDIF.

  ENDMETHOD.


  METHOD modify.

    ASSERT NOT is_data-repo IS INITIAL.
    ASSERT NOT is_data-sha1 IS INITIAL.

    IF mv_fake = abap_true.
      DELETE mt_objects WHERE sha1 = is_data-sha1 AND repo = is_data-repo.
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

    ASSERT NOT iv_repo IS INITIAL.

    IF mv_fake = abap_true.
      READ TABLE mt_objects INTO rs_data
        WITH KEY repo = iv_repo
        sha1 = iv_sha1.                                   "#EC CI_SUBRC
    ELSE.
      SELECT SINGLE * FROM zags_objects
        INTO rs_data
        WHERE repo = iv_repo
        AND sha1 = iv_sha1.
    ENDIF.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ags_error
        EXPORTING
          textid = zcx_ags_error=>m005
          sha1   = iv_sha1.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
