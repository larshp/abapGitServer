CLASS zcl_ags_obj_blob DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_ags_object .

    ALIASES c_newline
      FOR zif_ags_object~c_newline .
    ALIASES deserialize
      FOR zif_ags_object~deserialize .
    ALIASES save
      FOR zif_ags_object~save .
    ALIASES serialize
      FOR zif_ags_object~serialize .
    ALIASES sha1
      FOR zif_ags_object~sha1 .

    TYPES:
      ty_list TYPE STANDARD TABLE OF REF TO zcl_ags_obj_blob WITH DEFAULT KEY .

    METHODS get_data
      RETURNING
        VALUE(rv_data) TYPE xstring .
    METHODS set_data
      IMPORTING
        !iv_data TYPE xstring .
    METHODS constructor
      IMPORTING
        !iv_repo TYPE zags_objects-repo
        !iv_sha1 TYPE zags_objects-sha1 OPTIONAL
      RAISING
        zcx_ags_error .
    CLASS-METHODS get_instance
      IMPORTING
        !iv_repo       TYPE zags_objects-repo
        !iv_sha1       TYPE zags_objects-sha1
      RETURNING
        VALUE(ro_blob) TYPE REF TO zcl_ags_obj_blob
      RAISING
        zcx_ags_error .
    CLASS-METHODS constructor_mass
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


  METHOD constructor_mass.

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

      CREATE OBJECT lo_blob
        EXPORTING
          iv_repo = iv_repo.
* fix internal object state
      lo_blob->mv_new = abap_false.
      lo_blob->deserialize( <ls_object>-data_raw ).
      APPEND lo_blob TO rt_list.

    ENDLOOP.

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
