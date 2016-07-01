
*----------------------------------------------------------------------*
*       CLASS ltcl_get_time DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_get_time DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    METHODS: test FOR TESTING.
ENDCLASS.       "ltcl_Get_Time


*----------------------------------------------------------------------*
*       CLASS ltcl_get_time IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_get_time IMPLEMENTATION.

  METHOD test.

    DATA: lv_time TYPE string.


    lv_time = zcl_ags_util=>get_time( ).

    cl_abap_unit_assert=>assert_not_initial( lv_time ).

  ENDMETHOD.                    "test

ENDCLASS.                    "ltcl_get_time IMPLEMENTATION