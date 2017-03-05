
CLASS ltcl_list_commits DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      setup RAISING zcx_ags_error,
      test01 FOR TESTING RAISING zcx_ags_error.

ENDCLASS.

CLASS ltcl_list_commits IMPLEMENTATION.

  METHOD setup.
    zcl_ags_db=>set_fake( ).
  ENDMETHOD.

  METHOD test01.

    DATA: lo_branch  TYPE REF TO zcl_ags_branch,
          lt_commits TYPE zcl_ags_obj_commit=>ty_pretty_tt.


    lo_branch = zcl_ags_repo=>create(
      iv_name        = 'UNITTEST'
      iv_description = 'FOOBAR' )->get_default_branch( ).

    lt_commits = lo_branch->get_cache( )->list_commits( ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_commits )
      exp = 1 ).

    lo_branch->get_files( )->add(
      iv_filename       = 'BLAH.TXT'
      iv_path           = '/'
      iv_file_contents  = 'WELLO'
      iv_commit_message = 'BLAH' ).

    lt_commits = lo_branch->get_cache( )->list_commits( ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_commits )
      exp = 2 ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_list_files_by_path DEFINITION
    FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    CONSTANTS: c_root TYPE string VALUE '/'.

    DATA: mo_branch TYPE REF TO zcl_ags_branch.

    METHODS:
      setup RAISING zcx_ags_error,
      check IMPORTING iv_lines TYPE i RAISING zcx_ags_error,
      check_filled IMPORTING it_files TYPE zcl_ags_cache=>ty_files_tt,
      test01 FOR TESTING RAISING zcx_ags_error.

ENDCLASS.       "ltcl_List_Files_By_Path

CLASS ltcl_list_files_by_path IMPLEMENTATION.

  METHOD setup.
    zcl_ags_db=>set_fake( ).

    mo_branch = zcl_ags_repo=>create(
      iv_name        = 'UNITTEST'
      iv_description = 'FOOBAR' )->get_default_branch( ).
  ENDMETHOD.

  METHOD check.

    DATA: lt_files1 TYPE zcl_ags_cache=>ty_files_tt,
          lt_files2 TYPE zcl_ags_cache=>ty_files_tt.

* call method 2 times, first to save to cache, 2nd time to read from cache

    lt_files1 = mo_branch->get_cache( )->list_files_by_path( c_root ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files1 )
      exp = iv_lines ).
    check_filled( lt_files1 ).

    lt_files2 = mo_branch->get_cache( )->list_files_by_path( c_root ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files2 )
      exp = iv_lines ).
    check_filled( lt_files2 ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_files1
      exp = lt_files2 ).

  ENDMETHOD.

  METHOD check_filled.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF it_files.

    LOOP AT it_files ASSIGNING <ls_file>.
      cl_abap_unit_assert=>assert_not_initial( <ls_file>-filename ).
      cl_abap_unit_assert=>assert_not_initial( <ls_file>-path ).
      cl_abap_unit_assert=>assert_not_initial( <ls_file>-blob_sha1 ).
      cl_abap_unit_assert=>assert_not_initial( <ls_file>-last_commit_sha1 ).
      cl_abap_unit_assert=>assert_not_initial( <ls_file>-time ).
      cl_abap_unit_assert=>assert_not_initial( <ls_file>-comment ).
    ENDLOOP.

  ENDMETHOD.

  METHOD test01.

    check( 1 ).

    mo_branch->get_files( )->add(
      iv_filename       = 'NEW.TXT'
      iv_path           = c_root
      iv_file_contents  = 'WELLO'
      iv_commit_message = 'BLAH' ).

    check( 2 ).

  ENDMETHOD.

ENDCLASS.
