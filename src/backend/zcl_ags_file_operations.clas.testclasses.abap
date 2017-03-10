
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA: mo_branch TYPE REF TO zcl_ags_branch.

    METHODS:
      setup RAISING zcx_ags_error,
      add FOR TESTING RAISING zcx_ags_error.

ENDCLASS.       "ltcl_Test

CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    zcl_ags_db=>set_fake( ).

    mo_branch = zcl_ags_repo=>create(
      iv_name        = 'UNITTEST'
      iv_description = 'FOOBAR'
      )->get_default_branch( ).
  ENDMETHOD.

  METHOD add.

    CONSTANTS: lc_filename TYPE string VALUE 'NEW.TXT'.

    DATA: lt_before TYPE zcl_ags_cache=>ty_files_simple_tt,
          lt_after  TYPE zcl_ags_cache=>ty_files_simple_tt.


    lt_before = mo_branch->get_cache( )->list_files_simple( ).

    mo_branch->get_files( )->add(
      iv_filename       = lc_filename
      iv_path           = '/'
      iv_file_contents  = 'WELLO'
      iv_commit_message = 'BLAH' ).

    lt_after = mo_branch->get_cache( )->list_files_simple( ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_before ) + 1
      exp = lines( lt_after ) ).

    READ TABLE lt_after WITH KEY filename = lc_filename TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_subrc( ).

  ENDMETHOD.

ENDCLASS.
