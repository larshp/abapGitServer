
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
    METHODS:
      encode_length1 FOR TESTING,
      encode_length2 FOR TESTING,
      decode_length1 FOR TESTING,
      decode_length2 FOR TESTING.

ENDCLASS.       "ltcl_Encode_Length

*----------------------------------------------------------------------*
*       CLASS ltcl_length IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_length IMPLEMENTATION.

  METHOD encode_length1.

    DATA: lv_encoded TYPE zags_hex4.


    lv_encoded = zcl_ags_length=>encode( 100 ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_encoded
      exp = '30303634' ).

  ENDMETHOD.                    "encode_length

  METHOD encode_length2.

    DATA: lv_encoded TYPE xstring.


    lv_encoded = zcl_ags_length=>encode( 159 ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_ags_util=>xstring_to_string_utf8( lv_encoded )
      exp = '009f' ).

  ENDMETHOD.

  METHOD decode_length1.

    DATA: lv_length TYPE i.


    lv_length = zcl_ags_length=>decode( '0091' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_length
      exp = 145 ).

  ENDMETHOD.                    "decode_length

  METHOD decode_length2.

    DATA: lv_length TYPE i.


    lv_length = zcl_ags_length=>decode( '009f' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_length
      exp = 159 ).

  ENDMETHOD.

ENDCLASS.                    "ltcl_length IMPLEMENTATION
