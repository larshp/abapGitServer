CLASS zcl_ags_xstream DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS append
      IMPORTING
        !iv_value TYPE xsequence .
    METHODS append_band01
      IMPORTING
        !iv_value TYPE xsequence .
    METHODS append_band02
      IMPORTING
        !iv_value TYPE xsequence .
    METHODS append_length
      IMPORTING
        !iv_value TYPE xsequence .
    METHODS clear .
    METHODS get
      RETURNING
        VALUE(rv_xstr) TYPE xstring .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_xstr TYPE xstring .
ENDCLASS.



CLASS ZCL_AGS_XSTREAM IMPLEMENTATION.


  METHOD append.

    CONCATENATE mv_xstr iv_value INTO mv_xstr IN BYTE MODE.

  ENDMETHOD.


  METHOD append_band01.

    CONSTANTS: lc_band01 TYPE x VALUE '01'.

    DATA: lv_length TYPE xstring.

* make sure to include the length encoding itself and band identifier in the length
    lv_length = zcl_ags_length=>encode( xstrlen( iv_value ) + 5 ).
    append( lv_length ).
    append( lc_band01 ).
    append( iv_value ).

  ENDMETHOD.


  METHOD append_band02.

    CONSTANTS: lc_band02 TYPE x VALUE '02'.

    DATA: lv_length TYPE xstring.

* make sure to include the length encoding itself and band identifier in the length
    lv_length = zcl_ags_length=>encode( xstrlen( iv_value ) + 5 ).
    append( lv_length ).
    append( lc_band02 ).
    append( iv_value ).

  ENDMETHOD.


  METHOD append_length.

    DATA: lv_length TYPE xstring.

    lv_length = zcl_ags_length=>encode( xstrlen( iv_value ) + 4 ).
    append( lv_length ).
    append( iv_value ).

  ENDMETHOD.


  METHOD clear.

    CLEAR mv_xstr.

  ENDMETHOD.


  METHOD get.
    rv_xstr = mv_xstr.
  ENDMETHOD.
ENDCLASS.
