*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_length DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      encode
        IMPORTING iv_length     TYPE i
        RETURNING VALUE(rv_hex) TYPE zags_hex4,
      decode
        IMPORTING iv_data          TYPE string
        RETURNING VALUE(rv_length) TYPE i.

ENDCLASS.

CLASS lcl_length IMPLEMENTATION.

  METHOD encode.

    DATA: lv_char TYPE string,
          lv_x    TYPE x LENGTH 2.

    lv_x = iv_length.

    lv_char = lv_x.

    DATA(lv_xstring) = zcl_ags_util=>string_to_xstring_utf8( lv_char ).

    rv_hex = lv_xstring.

  ENDMETHOD.

  METHOD decode.

    DATA: lv_x TYPE x LENGTH 1.


    ASSERT strlen( iv_data ) >= 4.

* todo, extend implementation
    ASSERT iv_data(2) = '00'.

    lv_x = iv_data+2.

    rv_length = lv_x.

  ENDMETHOD.

ENDCLASS.