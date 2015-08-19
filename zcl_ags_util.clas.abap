class ZCL_AGS_UTIL definition
  public
  create public .

public section.

  class-methods UUID
    returning
      value(RV_UUID) type SYSUUID_C22 .
  class-methods XSTRING_TO_STRING_UTF8
    importing
      !IV_DATA type XSTRING
    returning
      value(RV_STRING) type STRING .
  class-methods STRING_TO_XSTRING_UTF8
    importing
      !IV_STRING type STRING
    returning
      value(RV_XSTRING) type XSTRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_AGS_UTIL IMPLEMENTATION.


METHOD string_to_xstring_utf8.

  DATA: lo_obj TYPE REF TO cl_abap_conv_out_ce.


  TRY.
      lo_obj = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).

      lo_obj->convert( EXPORTING data = iv_string
                       IMPORTING buffer = rv_xstring ).

    CATCH cx_parameter_invalid_range
          cx_sy_codepage_converter_init
          cx_sy_conversion_codepage
          cx_parameter_invalid_type.
      ASSERT 1 = 2.
  ENDTRY.

ENDMETHOD.


METHOD uuid.

  TRY.
      rv_uuid = cl_system_uuid=>if_system_uuid_static~create_uuid_c22( ).
    CATCH cx_uuid_error.
      ASSERT 1 = 2.
  ENDTRY.

ENDMETHOD.


METHOD xstring_to_string_utf8.

  DATA: lv_len TYPE i,
        lo_obj TYPE REF TO cl_abap_conv_in_ce.


  TRY.
      lo_obj = cl_abap_conv_in_ce=>create(
          input    = iv_data
          encoding = 'UTF-8' ).
      lv_len = xstrlen( iv_data ).

      lo_obj->read( EXPORTING n    = lv_len
                    IMPORTING data = rv_string ).

    CATCH cx_parameter_invalid_range
          cx_sy_codepage_converter_init
          cx_sy_conversion_codepage
          cx_parameter_invalid_type.
      ASSERT 1 = 2.
  ENDTRY.

ENDMETHOD.
ENDCLASS.