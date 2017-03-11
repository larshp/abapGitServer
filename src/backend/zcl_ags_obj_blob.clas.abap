class ZCL_AGS_OBJ_BLOB definition
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
  methods CONSTRUCTOR
    importing
      !IV_REPO type ZAGS_OBJECTS-REPO
      !IV_SHA1 type ZAGS_OBJECTS-SHA1 optional
    raising
      ZCX_AGS_ERROR .
  class-methods GET_INSTANCE
    importing
      !IV_REPO type ZAGS_OBJECTS-REPO
      !IV_SHA1 type ZAGS_OBJECTS-SHA1
    returning
      value(RO_BLOB) type ref to ZCL_AGS_OBJ_BLOB
    raising
      ZCX_AGS_ERROR .
  PROTECTED SECTION.
private section.

  data MV_DATA type XSTRING .
  data MV_NEW type ABAP_BOOL .
  data MV_REPO type ZAGS_OBJECTS-REPO .
ENDCLASS.



CLASS ZCL_AGS_OBJ_BLOB IMPLEMENTATION.


  METHOD constructor.

    ASSERT NOT iv_repo IS INITIAL.

    IF iv_sha1 IS INITIAL.
      mv_new = abap_true.
      mv_repo = iv_repo.
    ELSE.
      mv_new = abap_false.
      deserialize( zcl_ags_db=>get_objects( )->single( iv_repo = iv_repo iv_sha1 = iv_sha1 )-data_raw ).
    ENDIF.

  ENDMETHOD.


  METHOD get_data.

    rv_data = mv_data.

  ENDMETHOD.


  METHOD get_instance.

    CREATE OBJECT ro_blob
      EXPORTING
        iv_repo = iv_repo
        iv_sha1 = iv_sha1.

  ENDMETHOD.


  METHOD set_data.

    mv_data = iv_data.

  ENDMETHOD.


  METHOD zif_ags_object~deserialize.

    mv_data = iv_data.

  ENDMETHOD.


  METHOD zif_ags_object~save.

    DATA: ls_object TYPE zags_objects.


    ASSERT mv_new = abap_true.

    ls_object-repo = mv_repo.
    ls_object-sha1 = sha1( ).
    ls_object-type = zif_ags_constants=>c_type-blob.
    ls_object-data_raw = serialize( ).

    zcl_ags_db=>get_objects( )->modify( ls_object ).

  ENDMETHOD.


  METHOD zif_ags_object~serialize.

    rv_data = mv_data.

  ENDMETHOD.


  METHOD zif_ags_object~sha1.

    rv_sha1 = zcl_ags_util=>sha1(
        iv_type = zif_ags_constants=>c_type-blob
        iv_data = serialize( ) ) ##NO_TEXT.

  ENDMETHOD.


  METHOD zif_ags_object~type.
    rv_type = zif_ags_constants=>c_type-blob.
  ENDMETHOD.
ENDCLASS.
