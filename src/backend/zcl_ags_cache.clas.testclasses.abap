
CLASS ltcl_list_commits DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA: mo_branch  TYPE REF TO zcl_ags_branch.

    METHODS:
      setup RAISING zcx_ags_error,
      test01 FOR TESTING RAISING zcx_ags_error,
      test02 FOR TESTING RAISING zcx_ags_error.

ENDCLASS.

CLASS ltcl_list_commits IMPLEMENTATION.

  METHOD setup.
    zcl_ags_db=>set_fake( ).

    mo_branch = zcl_ags_repo=>create(
      iv_name        = 'UNITTEST'
      iv_description = 'FOOBAR' )->get_default_branch( ).
  ENDMETHOD.

  METHOD test01.

    DATA: lt_commits TYPE zcl_ags_obj_commit=>ty_pretty_tt.


    lt_commits = mo_branch->get_cache( )->list_commits( ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_commits )
      exp = 1 ).

    mo_branch->get_files( )->add(
      iv_filename       = 'BLAH.TXT'
      iv_path           = '/'
      iv_file_contents  = 'WELLO'
      iv_commit_message = 'BLAH' ).

    lt_commits = mo_branch->get_cache( )->list_commits( ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_commits )
      exp = 2 ).

  ENDMETHOD.

  METHOD test02.

    CONSTANTS: lc_count TYPE i VALUE 900.

    DATA: lt_commits TYPE zcl_ags_obj_commit=>ty_pretty_tt,
          lv_index   TYPE i.

    FIELD-SYMBOLS: <ls_commit> LIKE LINE OF lt_commits.


    DO lc_count TIMES.
      lv_index = sy-index.
      mo_branch->get_files( )->modify(
        iv_filename       = 'README.md'
        iv_path           = '/'
        iv_file_contents  = |COMMIT{ lv_index }|
        iv_commit_message = |COMMIT{ lv_index }| ).
    ENDDO.

    lt_commits = mo_branch->get_cache( )->list_commits( ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_commits )
      exp = lc_count + 1 ).

* make sure the order is correct
    lv_index = lc_count.
    LOOP AT lt_commits ASSIGNING <ls_commit>.
      IF sy-tabix = lc_count + 1.
        CONTINUE.
      ENDIF.
      cl_abap_unit_assert=>assert_equals(
        act = <ls_commit>-text
        exp = |COMMIT{ lv_index }| ).
      lv_index = lv_index - 1.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_list_files_by_path DEFINITION
    FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    CONSTANTS: c_root TYPE string VALUE '/'.

    DATA: mo_branch TYPE REF TO zcl_ags_branch.

    METHODS:
      setup RAISING zcx_ags_error,
      check
        IMPORTING iv_lines        TYPE i
        RETURNING VALUE(rt_files) TYPE zcl_ags_cache=>ty_files_tt
        RAISING   zcx_ags_error,
      check_filled IMPORTING it_files TYPE zcl_ags_cache=>ty_files_tt,
      test01 FOR TESTING RAISING zcx_ags_error,
      test02 FOR TESTING RAISING zcx_ags_error,
      test03 FOR TESTING RAISING zcx_ags_error,
      test04 FOR TESTING RAISING zcx_ags_error.

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

    rt_files = lt_files1.

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

  METHOD test02.
* test folder last commit

    CONSTANTS: lc_path   TYPE string VALUE '/FOO/',
               lc_first  TYPE string VALUE 'FIRST',
               lc_second TYPE string VALUE 'SECOND'.

    DATA: lt_files TYPE zcl_ags_cache=>ty_files_tt.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF lt_files.


* trigger build of initial cache
    mo_branch->get_cache( )->list_files_by_path( c_root ).

    mo_branch->get_files( )->add(
      iv_filename       = 'ASDF.TXT'
      iv_path           = '/BAR/'
      iv_file_contents  = 'WELLO'
      iv_commit_message = lc_first ).

    mo_branch->get_files( )->add(
      iv_filename       = 'NEW.TXT'
      iv_path           = lc_path
      iv_file_contents  = 'WELLO'
      iv_commit_message = lc_first ).

    lt_files = mo_branch->get_cache( )->list_files_by_path( c_root ).
* README.md and folder FOO + BAR at root
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 3 ).

    READ TABLE lt_files WITH KEY filename = 'FOO' ASSIGNING <ls_file>.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals(
      act = <ls_file>-chmod
      exp = zcl_ags_obj_tree=>c_chmod-dir ).
    cl_abap_unit_assert=>assert_equals(
      act = <ls_file>-comment
      exp = lc_first ).

    mo_branch->get_files( )->add(
      iv_filename       = 'NEW2.TXT'
      iv_path           = lc_path
      iv_file_contents  = 'HELLO'
      iv_commit_message = lc_second ).

    lt_files = mo_branch->get_cache( )->list_files_by_path( c_root ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 3 ).

    READ TABLE lt_files WITH KEY filename = 'BAR' ASSIGNING <ls_file>.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals(
      act = <ls_file>-chmod
      exp = zcl_ags_obj_tree=>c_chmod-dir ).
    cl_abap_unit_assert=>assert_equals(
      act = <ls_file>-comment
      exp = lc_first ).

    READ TABLE lt_files WITH KEY filename = 'FOO' ASSIGNING <ls_file>.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals(
      act = <ls_file>-chmod
      exp = zcl_ags_obj_tree=>c_chmod-dir ).
    cl_abap_unit_assert=>assert_equals(
      act = <ls_file>-comment
      exp = lc_second ).

  ENDMETHOD.

  METHOD test03.

    CONSTANTS: lc_latest TYPE string VALUE 'LATEST'.

    DATA: lt_files TYPE zcl_ags_cache=>ty_files_tt.


    mo_branch->get_files( )->add(
      iv_filename       = 'NEW.TXT'
      iv_path           = c_root
      iv_file_contents  = 'WELLO'
      iv_commit_message = 'BLAH' ).

    mo_branch->get_files( )->modify(
      iv_filename       = 'NEW.TXT'
      iv_path           = c_root
      iv_file_contents  = '234'
      iv_commit_message = lc_latest ).

    lt_files = check( 2 ).

    READ TABLE lt_files WITH KEY comment = lc_latest TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_subrc( ).

  ENDMETHOD.

  METHOD test04.

    mo_branch->get_files( )->add(
      iv_filename       = 'NEW.TXT'
      iv_path           = c_root
      iv_file_contents  = 'WELLO'
      iv_commit_message = 'BLAH' ).

    check( 2 ).

    mo_branch->get_files( )->delete(
      iv_filename       = 'NEW.TXT'
      iv_path           = c_root
      iv_commit_message = 'MOO' ).

    check( 1 ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_list_files_simple DEFINITION
    FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA: mo_branch TYPE REF TO zcl_ags_branch.

    METHODS:
      setup RAISING zcx_ags_error,
      test01 FOR TESTING RAISING zcx_ags_error.

ENDCLASS.       "ltcl_List_Files_Simple

CLASS ltcl_list_files_simple IMPLEMENTATION.

  METHOD setup.
    zcl_ags_db=>set_fake( ).

    mo_branch = zcl_ags_repo=>create(
      iv_name        = 'UNITTEST'
      iv_description = 'FOOBAR' )->get_default_branch( ).
  ENDMETHOD.

  METHOD test01.

    CONSTANTS: lc_path TYPE string VALUE '/foo/bar/'.

    DATA: lt_files TYPE zcl_ags_cache=>ty_files_simple_tt.


    mo_branch->get_files( )->add(
      iv_filename       = 'NEW.TXT'
      iv_path           = lc_path
      iv_file_contents  = 'WELLO'
      iv_commit_message = 'BLAH' ).

    lt_files = mo_branch->get_cache( )->list_files_simple( ).

    READ TABLE lt_files WITH KEY
      filename = 'NEW.TXT'
      path = lc_path
      chmod = zcl_ags_obj_tree=>c_chmod-file
      TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_subrc( ).

    READ TABLE lt_files WITH KEY
      filename = 'foo'
      path = '/'
      chmod = zcl_ags_obj_tree=>c_chmod-dir
      TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_subrc( ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_bubble_dir DEFINITION DEFERRED.
CLASS zcl_ags_cache DEFINITION LOCAL FRIENDS ltcl_bubble_dir.

CLASS ltcl_bubble_dir DEFINITION
    FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      single_step FOR TESTING,
      multi FOR TESTING.

ENDCLASS.       "ltcl_Bubble_Dir

CLASS ltcl_bubble_dir IMPLEMENTATION.

  METHOD single_step.

    CONSTANTS: lc_last TYPE zags_sha1 VALUE 'LAST'.

    DATA: lo_cache TYPE REF TO zcl_ags_cache,
          ls_file  TYPE zags_tree_cache_data,
          lt_files TYPE zags_tree_cache_data_tt.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF lt_files.


    ls_file-filename = 'FOO'.
    ls_file-path     = '/'.
    ls_file-chmod    = zcl_ags_obj_tree=>c_chmod-dir.
    INSERT ls_file INTO TABLE lt_files.
    CLEAR ls_file.

    ls_file-filename = 'BAR'.
    ls_file-path     = '/'.
    ls_file-chmod    = zcl_ags_obj_tree=>c_chmod-dir.
    INSERT ls_file INTO TABLE lt_files.
    CLEAR ls_file.

    CREATE OBJECT lo_cache
      EXPORTING
        iv_repo   = ''
        iv_commit = ''.

    ls_file-path = '/FOO/'.

    lo_cache->bubble_dir(
      EXPORTING iv_commit = lc_last
                is_file = ls_file
      CHANGING ct_files = lt_files ).

    READ TABLE lt_files WITH KEY filename = 'FOO' ASSIGNING <ls_file>.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals(
      act = <ls_file>-last_commit_sha1
      exp = lc_last ).

    READ TABLE lt_files WITH KEY filename = 'BAR' ASSIGNING <ls_file>.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals(
      act = <ls_file>-last_commit_sha1
      exp = '' ).

  ENDMETHOD.

  METHOD multi.

    CONSTANTS: lc_last TYPE zags_sha1 VALUE 'MULTI'.

    DATA: lo_cache TYPE REF TO zcl_ags_cache,
          ls_file  TYPE zags_tree_cache_data,
          lt_files TYPE zags_tree_cache_data_tt.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF lt_files.


    ls_file-filename = 'FOO'.
    ls_file-path     = '/'.
    ls_file-chmod    = zcl_ags_obj_tree=>c_chmod-dir.
    INSERT ls_file INTO TABLE lt_files.
    CLEAR ls_file.

    ls_file-filename = 'BAR'.
    ls_file-path     = '/FOO/'.
    ls_file-chmod    = zcl_ags_obj_tree=>c_chmod-dir.
    INSERT ls_file INTO TABLE lt_files.
    CLEAR ls_file.

    CREATE OBJECT lo_cache
      EXPORTING
        iv_repo   = ''
        iv_commit = ''.

    ls_file-path = '/FOO/BAR/'.

    lo_cache->bubble_dir(
      EXPORTING iv_commit = lc_last
                is_file = ls_file
      CHANGING ct_files = lt_files ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 2 ).

    LOOP AT lt_files ASSIGNING <ls_file>.
      cl_abap_unit_assert=>assert_equals(
        act = <ls_file>-last_commit_sha1
        exp = lc_last ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
