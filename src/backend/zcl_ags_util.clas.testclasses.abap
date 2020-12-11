

CLASS ltcl_get_time DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    METHODS: get_time FOR TESTING.
ENDCLASS.

CLASS ltcl_get_time IMPLEMENTATION.

  METHOD get_time.

    DATA: lv_time TYPE string.

    lv_time = zcl_ags_util=>get_time( ).

    cl_abap_unit_assert=>assert_not_initial( lv_time ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_sha1 DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    METHODS:
      get_sha1_for_hello FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_sha1 IMPLEMENTATION.

  METHOD get_sha1_for_hello.

    DATA: lv_sha1 TYPE zags_sha1.

    lv_sha1 = zcl_ags_util=>sha1(
      iv_type = zif_ags_constants=>c_type-blob
      iv_data = zcl_ags_util=>string_to_xstring_utf8( 'hello' ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_sha1
      exp = 'b6fc4c620b67d95f953a5c1c1230aaab5db5a1b0' ).

  ENDMETHOD.

ENDCLASS.
