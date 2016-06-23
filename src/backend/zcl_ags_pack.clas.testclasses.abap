
CLASS ltcl_encode DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    METHODS: test1 FOR TESTING
      RAISING zcx_ags_error.

ENDCLASS.       "ltcl_Encode

CLASS ltcl_encode IMPLEMENTATION.

  METHOD test1.

    CONSTANTS: lc_data TYPE xstring VALUE '12345'.


    DATA(lo_blob) = NEW zcl_ags_obj_blob( ).
    lo_blob->set_data( lc_data ).

    DATA(lt_objects) = zcl_ags_pack=>explode( lo_blob ).

    DATA(lv_raw) = zcl_ags_pack=>encode( lt_objects ).
    DATA(lt_result) = zcl_ags_pack=>decode( lv_raw ).

    cl_abap_unit_assert=>assert_not_initial( lv_raw ).

    cl_abap_unit_assert=>assert_not_initial( lt_objects ).

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
    METHODS:
      test1 FOR TESTING
        RAISING zcx_ags_error.

ENDCLASS.       "ltcl_Encode

CLASS ltcl_explode IMPLEMENTATION.

  METHOD test1.

    DATA: lo_blob TYPE REF TO zcl_ags_obj_blob.


    CREATE OBJECT lo_blob.

    DATA(lt_result) = zcl_ags_pack=>explode( lo_blob ).

    cl_abap_unit_assert=>assert_not_initial( lt_result ).

  ENDMETHOD.

ENDCLASS.