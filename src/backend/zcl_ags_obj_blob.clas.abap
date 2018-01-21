CLASS zcl_ags_obj_blob DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_ags_object .

    ALIASES deserialize
      FOR zif_ags_object~deserialize .
    ALIASES get_adler32
      FOR zif_ags_object~get_adler32 .
    ALIASES get_sha1
      FOR zif_ags_object~get_sha1 .
    ALIASES get_type
      FOR zif_ags_object~get_type .
    ALIASES save
      FOR zif_ags_object~save .
    ALIASES serialize
      FOR zif_ags_object~serialize .

    TYPES:
      ty_list TYPE STANDARD TABLE OF REF TO zcl_ags_obj_blob WITH DEFAULT KEY .

    METHODS get_data
      RETURNING
        VALUE(rv_data) TYPE xstring .
    METHODS set_data
      IMPORTING
        !iv_data TYPE xstring .
    CLASS-METHODS new
      IMPORTING
        !iv_repo       TYPE zags_objects-repo
      RETURNING
        VALUE(ro_blob) TYPE REF TO zcl_ags_obj_blob .
    CLASS-METHODS load
      IMPORTING
        !iv_repo       TYPE zags_objects-repo
        !iv_sha1       TYPE zags_objects-sha1
      RETURNING
        VALUE(ro_blob) TYPE REF TO zcl_ags_obj_blob
      RAISING
        zcx_ags_error .
    CLASS-METHODS load_mass
      IMPORTING
        !iv_repo       TYPE zags_objects-repo
        !it_sha1       TYPE zags_sha1_tt
      RETURNING
        VALUE(rt_list) TYPE ty_list
      RAISING
        zcx_ags_error .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_data TYPE xstring .
    DATA mv_new TYPE abap_bool .
    DATA mv_repo TYPE zags_objects-repo .
    DATA mv_sha1 TYPE zags_sha1 .
    DATA mv_adler32 TYPE zags_adler32 .
ENDCLASS.



CLASS ZCL_AGS_OBJ_BLOB IMPLEMENTATION.


  METHOD get_data.

    rv_data = mv_data.

  ENDMETHOD.


  METHOD load.

    DATA: ls_data TYPE zags_objects.

    CREATE OBJECT ro_blob.
    ro_blob->mv_new = abap_false.
    ro_blob->mv_sha1 = iv_sha1.

    ls_data = zcl_ags_db=>get_objects( )->single(
      iv_repo = iv_repo
      iv_sha1 = iv_sha1 ).

    ro_blob->deserialize(
      iv_data    = ls_data-data_raw
      iv_adler32 = ls_data-adler32 ).

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
      lo_blob->mv_sha1 = <ls_object>-sha1.
      lo_blob->deserialize(
        iv_data    = <ls_object>-data_raw
        iv_adler32 = <ls_object>-adler32 ).
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

    ASSERT mv_new = abap_true.

    mv_data = iv_data.

  ENDMETHOD.


  METHOD zif_ags_object~deserialize.

    mv_data = iv_data.
    mv_adler32 = iv_adler32.

  ENDMETHOD.


  METHOD zif_ags_object~get_adler32.

    IF mv_adler32 IS INITIAL.
      rv_adler32 = zcl_abapgit_hash=>adler32( serialize( ) ).
    ELSE.
      ASSERT NOT mv_adler32 IS INITIAL.
      rv_adler32 = mv_adler32.
    ENDIF.

  ENDMETHOD.


  METHOD zif_ags_object~get_sha1.

    IF mv_new = abap_true.
      rv_sha1 = zcl_ags_util=>sha1(
        iv_type = zif_ags_constants=>c_type-blob
        iv_data = serialize( ) ).
    ELSE.
      ASSERT NOT mv_sha1 IS INITIAL.
      rv_sha1 = mv_sha1.
    ENDIF.

  ENDMETHOD.


  METHOD zif_ags_object~get_type.
    rv_type = zif_ags_constants=>c_type-blob.
  ENDMETHOD.


  METHOD zif_ags_object~save.

    DATA: ls_object TYPE zags_objects.


    ASSERT mv_new = abap_true.

    ls_object-repo = mv_repo.
    ls_object-sha1 = get_sha1( ).
    ls_object-type = get_type( ).
    ls_object-data_raw = serialize( ).
    ls_object-adler32 = get_adler32( ).

    zcl_ags_db=>get_objects( )->modify( ls_object ).

  ENDMETHOD.


  METHOD zif_ags_object~serialize.

    rv_data = mv_data.

  ENDMETHOD.
ENDCLASS.
