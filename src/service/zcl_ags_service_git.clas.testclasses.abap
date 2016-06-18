
CLASS ltcl_encode_length DEFINITION DEFERRED.
CLASS zcl_ags_service_git DEFINITION LOCAL FRIENDS ltcl_encode_length.

CLASS ltcl_encode_length DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    METHODS: encode_length FOR TESTING.
ENDCLASS.       "ltcl_Encode_Length


CLASS ltcl_encode_length IMPLEMENTATION.

  METHOD encode_length.

    DATA(lo_git) = NEW zcl_ags_service_git( ).

    DATA(lv_encoded) = lo_git->encode_length( 100 ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_encoded
      exp = '30303634' ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_decode_length DEFINITION DEFERRED.
CLASS zcl_ags_service_git DEFINITION LOCAL FRIENDS ltcl_decode_length.

CLASS ltcl_decode_length DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    METHODS: decode_length FOR TESTING.

ENDCLASS.       "ltcl_Encode_Length

CLASS ltcl_decode_length IMPLEMENTATION.

  METHOD decode_length.

    DATA(lo_git) = NEW zcl_ags_service_git( ).

    DATA(lv_length) = lo_git->decode_length( '0091' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_length
      exp = 145 ).

  ENDMETHOD.

ENDCLASS.