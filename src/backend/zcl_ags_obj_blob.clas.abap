class ZCL_AGS_OBJ_BLOB definition
  public
  create private .

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
    for ZIF_AGS_OBJECT~GET_SHA1 .
  aliases TYPE
    for ZIF_AGS_OBJECT~GET_TYPE .

  types:
    ty_list TYPE STANDARD TABLE OF REF TO zcl_ags_obj_blob WITH DEFAULT KEY .

  methods GET_DATA
    returning
      value(RV_DATA) type XSTRING .
  methods SET_DATA
    importing
      !IV_DATA type XSTRING .
  class-methods NEW
    importing
      !IV_REPO type ZAGS_OBJECTS-REPO
    returning
      value(RO_BLOB) type ref to ZCL_AGS_OBJ_BLOB .
  class-methods LOAD
    importing
      !IV_REPO type ZAGS_OBJECTS-REPO
      !IV_SHA1 type ZAGS_OBJECTS-SHA1
    returning
      value(RO_BLOB) type ref to ZCL_AGS_OBJ_BLOB
    raising
      ZCX_AGS_ERROR .
  class-methods LOAD_MASS
    importing
      !IV_REPO type ZAGS_OBJECTS-REPO
      !IT_SHA1 type ZAGS_SHA1_TT
    returning
      value(RT_LIST) type TY_LIST
    raising
      ZCX_AGS_ERROR .
  PROTECTED SECTION.
private section.

  data MV_DATA type XSTRING .
  data MV_NEW type ABAP_BOOL .
  data MV_REPO type ZAGS_OBJECTS-REPO .
ENDCLASS.



CLASS ZCL_AGS_OBJ_BLOB IMPLEMENTATION.


  METHOD get_data.

    rv_data = mv_data.

  ENDMETHOD.


  METHOD load.

    CREATE OBJECT ro_blob.
    ro_blob->mv_new = abap_false.
    ro_blob->deserialize( zcl_ags_db=>get_objects( )->single( iv_repo = iv_repo iv_sha1 = iv_sha1 )-data_raw ).

  ENDMETHOD.


  METHOD load_mass.

    DATA: lt_objects TYPE zags_objects_tt,
          lo_blob    TYPE REF TO zcl_ags_obj_blob.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF lt_objects.


    ASSERT NOT iv_repo IS INITIAL.

    IF lines( it_sha1 ) = 0.
      RETURN.
    ENDIF.

    lt_objects = zcl_ags_db=>get_objects( )->mass(
      iv_repo = iv_repo
      it_sha1 = it_sha1 ).

    LOOP AT lt_objects ASSIGNING <ls_object>.
      ASSERT <ls_object>-type = zif_ags_constants=>c_type-blob.

      lo_blob = zcl_ags_obj_blob=>new( iv_repo ).
      lo_blob->mv_new = abap_false.
      lo_blob->deserialize( <ls_object>-data_raw ).
      APPEND lo_blob TO rt_list.

    ENDLOOP.

  ENDMETHOD.


  METHOD new.

    ASSERT NOT iv_repo IS INITIAL.

    CREATE OBJECT ro_blob.
    ro_blob->mv_new = abap_true.
    ro_blob->mv_repo = iv_repo.

  ENDMETHOD.


  METHOD set_data.

    mv_data = iv_data.

  ENDMETHOD.


  METHOD zif_ags_object~deserialize.

    mv_data = iv_data.

  ENDMETHOD.


  METHOD zif_ags_object~get_sha1.

    rv_sha1 = zcl_ags_util=>sha1(
        iv_type = zif_ags_constants=>c_type-blob
        iv_data = serialize( ) ) ##NO_TEXT.

  ENDMETHOD.


  METHOD zif_ags_object~get_type.
    rv_type = zif_ags_constants=>c_type-blob.
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
ENDCLASS.
