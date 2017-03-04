CLASS zcl_ags_obj_blob DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_ags_object.

    ALIASES c_newline
      FOR zif_ags_object~c_newline.
    ALIASES deserialize
      FOR zif_ags_object~deserialize.
    ALIASES save
      FOR zif_ags_object~save.
    ALIASES serialize
      FOR zif_ags_object~serialize.
    ALIASES sha1
      FOR zif_ags_object~sha1.

    METHODS get_data
      RETURNING
        VALUE(rv_data) TYPE xstring.
    METHODS set_data
      IMPORTING
        !iv_data TYPE xstring.
    METHODS constructor
      IMPORTING
        !iv_sha1 TYPE zags_sha1 OPTIONAL
      RAISING
        zcx_ags_error.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_data TYPE xstring.
    DATA mv_new TYPE abap_bool.
ENDCLASS.



CLASS ZCL_AGS_OBJ_BLOB IMPLEMENTATION.


  METHOD constructor.

    IF iv_sha1 IS INITIAL.
      mv_new = abap_true.
    ELSE.
      mv_new = abap_false.
      deserialize( zcl_ags_lookup=>read_object( iv_sha1 )-data_raw ).
    ENDIF.

  ENDMETHOD.


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

    ASSERT mv_new = abap_true.

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
