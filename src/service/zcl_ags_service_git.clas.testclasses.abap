
CLASS ltcl_test DEFINITION DEFERRED.
CLASS zcl_ags_service_git DEFINITION LOCAL FRIENDS ltcl_test.

CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS: unpack_ok FOR TESTING.

ENDCLASS.       "ltcl_Test


CLASS ltcl_test IMPLEMENTATION.

  METHOD unpack_ok.

    DATA: lv_result TYPE xstring,
          lo_git    TYPE REF TO zcl_ags_service_git.


    CREATE OBJECT lo_git.

    lv_result = lo_git->unpack_ok( 'refs/heads/master' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = '303031330130303065756E7061636B206F6B0A3030316501303031396' &&
        'F6B20726566732F68656164732F6D61737465720A30303039013030303030303030' ).

  ENDMETHOD.

ENDCLASS.
