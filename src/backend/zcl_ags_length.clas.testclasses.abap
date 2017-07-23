
*----------------------------------------------------------------------*
*       CLASS ltcl_length DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_length DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    METHODS: encode_length FOR TESTING.
    METHODS: decode_length FOR TESTING.
ENDCLASS.       "ltcl_Encode_Length


*----------------------------------------------------------------------*
*       CLASS ltcl_length IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_length IMPLEMENTATION.

  METHOD encode_length.

    DATA: lv_encoded TYPE zags_hex4.


    lv_encoded = zcl_ags_length=>encode( 100 ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_encoded
      exp = '30303634' ).

  ENDMETHOD.                    "encode_length

  METHOD decode_length.

    DATA: lv_length TYPE i.


    lv_length = zcl_ags_length=>decode( '0091' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_length
      exp = 145 ).

  ENDMETHOD.                    "decode_length

ENDCLASS.                    "ltcl_length IMPLEMENTATION
