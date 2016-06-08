CLASS zcl_ags_obj_file DEFINITION
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
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_data TYPE xstring.
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