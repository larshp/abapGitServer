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

    METHODS create_merge_request FOR TESTING
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

  METHOD create_merge_request.

    DATA(ls_act_merge_request) = NEW zcl_ags_service_rest( )->create_merge_request(
      iv_data = VALUE #( repo = 'ags_ci' targetbranch = 'refs/heads/master'
        sourcebranch = 'refs/heads/b1' )
    ).

    DATA(ls_exp_merge_request) = VALUE zcl_ags_service_rest=>ty_merge_request(
      anchestor = '6013dd73f098a043e9d82fd84b80a7caf19f5fa8'
      source = 'a222703e81ca9a3b0097f094cd0538ba6f6eaa51'
      target = '6013dd73f098a043e9d82fd84b80a7caf19f5fa8'
      repo = '051MciMJ7jk0v6lyvLF0sW' id = 1
      target_branch = '051MciMJ7jk0v6lyvLFWsW'
      source_branch = '051MciMJ7jkQzS7GMQrWsW'
    ).
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
