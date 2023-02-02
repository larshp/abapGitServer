CLASS zcl_ags_util DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS sha1_raw
      IMPORTING
        !iv_data       TYPE xstring
      RETURNING
        VALUE(rv_sha1) TYPE zags_sha1
      RAISING
        zcx_ags_error.
    CLASS-METHODS sha1
      IMPORTING
        !iv_type       TYPE zags_type
        !iv_data       TYPE xstring
      RETURNING
        VALUE(rv_sha1) TYPE zags_sha1
      RAISING
        zcx_ags_error.
    CLASS-METHODS uuid
      RETURNING
        VALUE(rv_uuid) TYPE sysuuid_c22.
    CLASS-METHODS xstring_to_string_utf8
      IMPORTING
        !iv_data         TYPE xstring
      RETURNING
        VALUE(rv_string) TYPE string.
    CLASS-METHODS string_to_xstring_utf8
      IMPORTING
        !iv_string        TYPE string
      RETURNING
        VALUE(rv_xstring) TYPE xstring.
    CLASS-METHODS get_time
      RETURNING
        VALUE(rv_time) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AGS_UTIL IMPLEMENTATION.


  METHOD get_time.

    TRY.

        rv_time = zcl_abapgit_git_time=>get_unix( ).
      CATCH zcx_abapgit_exception.
        ASSERT 0 = 1.
    ENDTRY.

  ENDMETHOD.


  METHOD sha1.

    DATA: lv_len     TYPE i,
          lv_char10  TYPE c LENGTH 10,
          lv_string  TYPE string,
          lv_xstring TYPE xstring.


    lv_len = xstrlen( iv_data ).
    lv_char10 = lv_len.
    CONDENSE lv_char10.
    CONCATENATE iv_type lv_char10 INTO lv_string SEPARATED BY space.
    lv_xstring = string_to_xstring_utf8( lv_string ).

    lv_string = lv_xstring.
    CONCATENATE lv_string '00' INTO lv_string.
    lv_xstring = lv_string.

    CONCATENATE lv_xstring iv_data INTO lv_xstring IN BYTE MODE.

    rv_sha1 = sha1_raw( lv_xstring ).

  ENDMETHOD.


  METHOD sha1_raw.

    DATA: lv_hash TYPE hash160.


    CALL FUNCTION 'CALCULATE_HASH_FOR_RAW'
      EXPORTING
        data           = iv_data
      IMPORTING
        hash           = lv_hash
      EXCEPTIONS
        unknown_alg    = 1
        param_error    = 2
        internal_error = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ags_error
        EXPORTING
          textid = zcx_ags_error=>m004.
    ENDIF.

    rv_sha1 = lv_hash.

    TRANSLATE rv_sha1 TO LOWER CASE.

  ENDMETHOD.


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
        ASSERT 0 = 1.
    ENDTRY.

  ENDMETHOD.


  METHOD xstring_to_string_utf8.

    DATA: lv_len TYPE i,
          lo_obj TYPE REF TO cl_abap_conv_in_ce.


    TRY.
        lo_obj = cl_abap_conv_in_ce=>create(
            input    = iv_data
*            ignore_cerr = abap_true
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
