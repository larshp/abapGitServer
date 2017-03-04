
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      setup,
      test FOR TESTING RAISING zcx_ags_error.

ENDCLASS.       "ltcl_Test

CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    zcl_ags_db=>set_fake( ).
  ENDMETHOD.

  METHOD test.

    CONSTANTS: lc_name TYPE zags_repos-name VALUE 'UNIT_TEST_NAME'.

    DATA: lt_list TYPE zags_repos_tt,
          lo_repo TYPE REF TO zcl_ags_repo.


    lo_repo = zcl_ags_repo=>create(
      iv_name        = lc_name
      iv_description = 'DESCRIPTION' ).

    lt_list = zcl_ags_repo=>list( ).
    READ TABLE lt_list WITH KEY name = lc_name TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_subrc( ).

    lo_repo->delete( ).

    lt_list = zcl_ags_repo=>list( ).
    READ TABLE lt_list WITH KEY name = lc_name TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_subrc( exp = 4 ).

  ENDMETHOD.

ENDCLASS.
