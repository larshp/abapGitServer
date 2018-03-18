
CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA: mo_stream TYPE REF TO zcl_ags_xstream.

    METHODS:
      setup,
      append_and_clear FOR TESTING,
      band01 FOR TESTING,
      band02 FOR TESTING,
      length FOR TESTING.

ENDCLASS.       "ltcl_Test


CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_stream.
  ENDMETHOD.

  METHOD append_and_clear.

    CONSTANTS: lc_value TYPE xstring VALUE '00FF'.

    mo_stream->append( lc_value ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stream->get( )
      exp = lc_value ).

    mo_stream->clear( ).

    cl_abap_unit_assert=>assert_initial( mo_stream->get( ) ).

  ENDMETHOD.

  METHOD band01.

    mo_stream->append_band01( '0011223344' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stream->get( )
      exp = '30303061010011223344' ).

  ENDMETHOD.

  METHOD band02.

    mo_stream->append_band02( '0011223344' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stream->get( )
      exp = '30303061020011223344' ).

  ENDMETHOD.

  METHOD length.

    mo_stream->append_length( '0011223344' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stream->get( )
      exp = '303030390011223344' ).

  ENDMETHOD.

ENDCLASS.
