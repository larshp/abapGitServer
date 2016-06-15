
CLASS ltcl_raw DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    METHODS: test1 FOR TESTING.

ENDCLASS.       "ltcl_Encode

CLASS ltcl_raw IMPLEMENTATION.

  METHOD test1.

    DATA: lt_objects TYPE zcl_ags_pack=>ty_objects_tt.


    DATA(lv_raw) = zcl_ags_pack=>encode( lt_objects ).
    DATA(lt_result) = zcl_ags_pack=>decode( lv_raw ).

    cl_abap_unit_assert=>assert_not_initial( lv_raw ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result
      exp = lt_objects ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_explode DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    METHODS: test1 FOR TESTING
      RAISING zcx_ags_error.

ENDCLASS.       "ltcl_Encode

CLASS ltcl_explode IMPLEMENTATION.

  METHOD test1.

    DATA: lo_commit TYPE REF TO zcl_ags_obj_commit.


    CREATE OBJECT lo_commit.

    DATA(lt_result) = zcl_ags_pack=>explode( lo_commit ).

    cl_abap_unit_assert=>assert_not_initial( lt_result ).

  ENDMETHOD.

ENDCLASS.