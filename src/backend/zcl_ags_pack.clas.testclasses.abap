
*----------------------------------------------------------------------*
*       CLASS ltcl_encode DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_encode DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    METHODS: test1 FOR TESTING
      RAISING zcx_ags_error.

ENDCLASS.       "ltcl_Encode

*----------------------------------------------------------------------*
*       CLASS ltcl_encode IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_encode IMPLEMENTATION.

  METHOD test1.

    CONSTANTS: lc_data TYPE xstring VALUE '12345'.

    DATA: lo_blob    TYPE REF TO zcl_ags_obj_blob,
          lv_raw     TYPE xstring,
          lt_objects TYPE zcl_ags_pack=>ty_objects_tt,
          lt_result  TYPE zcl_ags_pack=>ty_objects_tt.


    CREATE OBJECT lo_blob
      EXPORTING
        iv_repo = 'test'.
    lo_blob->set_data( lc_data ).

    lt_objects = zcl_ags_pack=>explode( iv_repo = 'test' ii_object = lo_blob ).

    lv_raw = zcl_ags_pack=>encode( lt_objects ).
    lt_result = zcl_ags_pack=>decode( lv_raw ).

    cl_abap_unit_assert=>assert_not_initial( lv_raw ).

    cl_abap_unit_assert=>assert_not_initial( lt_objects ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result
      exp = lt_objects ).

  ENDMETHOD.                    "test1

ENDCLASS.                    "ltcl_encode IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltcl_explode DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_explode DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      test1 FOR TESTING
        RAISING zcx_ags_error.

ENDCLASS.       "ltcl_Encode

*----------------------------------------------------------------------*
*       CLASS ltcl_explode IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_explode IMPLEMENTATION.

  METHOD test1.

    DATA: lt_result TYPE zcl_ags_pack=>ty_objects_tt,
          lo_blob   TYPE REF TO zcl_ags_obj_blob.


    CREATE OBJECT lo_blob
      EXPORTING
        iv_repo = 'test'.

    lt_result = zcl_ags_pack=>explode( iv_repo = 'test' ii_object = lo_blob ).

    cl_abap_unit_assert=>assert_not_initial( lt_result ).

  ENDMETHOD.                    "test1

ENDCLASS.                    "ltcl_explode IMPLEMENTATION
