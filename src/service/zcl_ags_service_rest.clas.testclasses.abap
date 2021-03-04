CLASS ltcl_rest DEFINITION DEFERRED.
CLASS zcl_ags_service_rest DEFINITION LOCAL FRIENDS ltcl_rest.

CLASS ltcl_rest DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    CONSTANTS:
      c_name        TYPE zags_repos-name VALUE 'UNIT_TEST',
      c_description TYPE zags_repos-description VALUE 'DESCRIPTION, FOOBAR'.

    DATA: mo_rest TYPE REF TO zcl_ags_service_rest,
          mo_repo TYPE REF TO zcl_ags_repo.

    METHODS:
      setup RAISING zcx_ags_error,
      list_files FOR TESTING RAISING zcx_ags_error,
      list_branches FOR TESTING RAISING zcx_ags_error.

ENDCLASS.       "ltcl_List_Files

CLASS ltcl_rest IMPLEMENTATION.

  METHOD setup.

    zcl_ags_db=>set_fake( ).

    CREATE OBJECT mo_rest.

    mo_repo = zcl_ags_repo=>create(
      iv_name        = c_name
      iv_description = c_description ).

  ENDMETHOD.

  METHOD list_files.

    DATA: lt_files TYPE zcl_ags_cache=>ty_files_tt.


    lt_files = mo_rest->list_files(
      iv_repo   = c_name
      iv_branch = mo_repo->get_data( )-head
      iv_path   = '' ).

    cl_abap_unit_assert=>assert_not_initial( lt_files ).

  ENDMETHOD.

  METHOD list_branches.

    DATA: lt_branches TYPE zcl_ags_service_rest=>ty_branches_tt.


    lt_branches = mo_rest->list_branches( c_name ).

    cl_abap_unit_assert=>assert_not_initial( lt_branches ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_merge_request DEFINITION FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS setup
      RAISING
        cx_static_check.

    " File added
    METHODS create_merge_request_af FOR TESTING
      RAISING
        cx_static_check.

    " File modified
    METHODS create_merge_request_mf FOR TESTING
      RAISING
        cx_static_check.

    " File deleted
    METHODS create_merge_request_df FOR TESTING
      RAISING
        cx_static_check.

ENDCLASS.

CLASS ltcl_merge_request IMPLEMENTATION.

  METHOD setup.

    CALL FUNCTION 'ZIMPORT_BUNDLE_FROM_TDC'
      EXPORTING
        tdc = 'ZAGS_REPO_SNAPSHOT'
        tdc_version = 1
        variant = 'ECATTDEFAULT'
        do_commit = abap_false
        content_handler = 'REPLACE_CONTENT_ALL_TABLES'.

  ENDMETHOD.

  METHOD create_merge_request_af.

    DATA(ls_act_merge_request) = NEW zcl_ags_service_rest( )->create_merge_request(
      iv_data = VALUE #( repo = 'ags_ci' targetbranch = 'refs/heads/master'
        sourcebranch = 'refs/heads/b1' ) ).

    DATA(ls_exp_merge_request) = VALUE zcl_ags_service_rest=>ty_merge_request(
      anchestor = '6013dd73f098a043e9d82fd84b80a7caf19f5fa8'
      source = '6a1552600ef051b03473e206b200855434728ef4'
      target = '6013dd73f098a043e9d82fd84b80a7caf19f5fa8'
      repo = '051MciMJ7jk0v6lyvLF0sW' id = 1
      target_branch = '051MciMJ7jk0v6lyvLFWsW'
      source_branch = '051MciMJ7jkQzS7GMQrWsW'
      source_branch_name = 'b1'
      target_branch_name = 'master'
      changed_files = VALUE #(
        ( filename = 'MANIFEST.md'
          path = '/'
          new_blob = 'ff39d3ea3f50f7d711f970a8cb2293638777a2f2' ) ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_act_merge_request
      exp = ls_exp_merge_request ).
    SELECT COUNT(*) FROM zags_merge_req
      WHERE repo = ls_exp_merge_request-repo AND id = ls_exp_merge_request-id
      AND target_branch = ls_exp_merge_request-target_branch
      AND source_branch = ls_exp_merge_request-source_branch.
    cl_abap_unit_assert=>assert_subrc( act = 0
      msg = 'Database entry for table zags_merge_req missing' ).

  ENDMETHOD.

  METHOD create_merge_request_mf.

    DATA(ls_act_merge_request) = NEW zcl_ags_service_rest( )->create_merge_request(
      iv_data = VALUE #( repo = 'ags_ci' targetbranch = 'refs/heads/master'
        sourcebranch = 'refs/heads/b2' ) ).

    DATA(ls_exp_merge_request) = VALUE zcl_ags_service_rest=>ty_merge_request(
      anchestor = '6013dd73f098a043e9d82fd84b80a7caf19f5fa8'
      source = 'd26ed200dfbdf3e79e92b2f9032c3ffcb5644a76'
      target = '6013dd73f098a043e9d82fd84b80a7caf19f5fa8'
      repo = '051MciMJ7jk0v6lyvLF0sW' id = 1
      target_branch = '051MciMJ7jk0v6lyvLFWsW'
      source_branch = '051MciMJ7jkVbzqd2dz0sW'
      source_branch_name = 'b2'
      target_branch_name = 'master'
      changed_files = VALUE #(
        ( filename = 'README.md'
          path = '/'
          old_blob = 'bfc169314c32d7e9279cf018e4a1e290f561ac5e'
          new_blob = '8473c15d9db2aad97e5d39b4b8ed25e52b3dcf62' ) ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_act_merge_request
      exp = ls_exp_merge_request ).
    SELECT COUNT(*) FROM zags_merge_req
      WHERE repo = ls_exp_merge_request-repo AND id = ls_exp_merge_request-id
      AND target_branch = ls_exp_merge_request-target_branch
      AND source_branch = ls_exp_merge_request-source_branch.
    cl_abap_unit_assert=>assert_subrc( act = 0
      msg = 'Database entry for table zags_merge_req missing' ).

  ENDMETHOD.

  METHOD create_merge_request_df.

    DATA(ls_act_merge_request) = NEW zcl_ags_service_rest( )->create_merge_request(
      iv_data = VALUE #( repo = 'ags_ci' targetbranch = 'refs/heads/master'
        sourcebranch = 'refs/heads/b3' ) ).

    DATA(ls_exp_merge_request) = VALUE zcl_ags_service_rest=>ty_merge_request(
      anchestor = '6013dd73f098a043e9d82fd84b80a7caf19f5fa8'
      source = 'faee7a29cceda981cf7dfde82687f004154a046f'
      target = '6013dd73f098a043e9d82fd84b80a7caf19f5fa8'
      repo = '051MciMJ7jk0v6lyvLF0sW' id = 1
      target_branch = '051MciMJ7jk0v6lyvLFWsW'
      source_branch = '051MciMJ7jkVc3yk9a{0sW'
      source_branch_name = 'b3'
      target_branch_name = 'master'
      changed_files = VALUE #(
        ( filename = 'README.md'
          path = '/'
          old_blob = 'bfc169314c32d7e9279cf018e4a1e290f561ac5e'
          new_blob = '' ) ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_act_merge_request
      exp = ls_exp_merge_request ).
    SELECT COUNT(*) FROM zags_merge_req
      WHERE repo = ls_exp_merge_request-repo AND id = ls_exp_merge_request-id
      AND target_branch = ls_exp_merge_request-target_branch
      AND source_branch = ls_exp_merge_request-source_branch.
    cl_abap_unit_assert=>assert_subrc( act = 0
      msg = 'Database entry for table zags_merge_req missing' ).

  ENDMETHOD.

ENDCLASS.
