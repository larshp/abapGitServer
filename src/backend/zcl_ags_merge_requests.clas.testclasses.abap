CLASS ltcl_merge_requests DEFINITION FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.

    METHODS setup
      RAISING cx_static_check.

    METHODS fast_forward_merge FOR TESTING
      RAISING zcx_ags_error.

    METHODS normal_merge FOR TESTING
      RAISING zcx_ags_error.

    METHODS normal_commit FOR TESTING
      RAISING zcx_ags_error.

    METHODS branch_merged FOR TESTING
      RAISING zcx_ags_error.

    METHODS branch_not_merged FOR TESTING
      RAISING zcx_ags_error.

ENDCLASS.

CLASS ltcl_merge_requests IMPLEMENTATION.

  METHOD setup.

    CALL FUNCTION 'ZIMPORT_BUNDLE_FROM_TDC'
      EXPORTING
        tdc = 'ZAGS_REPO_SNAPSHOT'
        tdc_version = 1
        variant = 'MERGE'
        do_commit = abap_false
        content_handler = 'REPLACE_CONTENT_ALL_TABLES'.

  ENDMETHOD.

  METHOD fast_forward_merge.

    DATA(lt_act_merge_requests) = zcl_ags_merge_requests=>find_merged_merge_requests(
      iv_repo = '051MciMJ7jkcyI0Qk}G0sW' iv_target_branch = '051MciMJ7jkcyI0Qk}GWsW'
      iv_sha1_commit_old = '915fb79a4ca60160a1bb83d2587c50224f388655'
      iv_sha1_commit_new = '1a51f4108bbb2befa991f5c4b65b0413fd8744c8' ).

    cl_abap_unit_assert=>assert_equals( exp = 1
      act = lines( lt_act_merge_requests ) ).

  ENDMETHOD.

  METHOD normal_merge.

    DATA(lt_act_merge_requests) = zcl_ags_merge_requests=>find_merged_merge_requests(
      iv_repo = '051MciMJ7jkcybs}nEaWsW' iv_target_branch = '051MciMJ7jkcybs}nEb0sW'
      iv_sha1_commit_old = '2291f2d6320fe84a650b87c47b5c91b70911c021'
      iv_sha1_commit_new = '466d9a59e9ffb337d388cfc83825e98fd2746cde' ).

    cl_abap_unit_assert=>assert_equals( exp = 1
      act = lines( lt_act_merge_requests ) ).

  ENDMETHOD.

  METHOD normal_commit.

    DATA(lt_act_merge_requests) = zcl_ags_merge_requests=>find_merged_merge_requests(
      iv_repo = '051MciMJ7jkcykFf16X0sW' iv_target_branch = '051MciMJ7jkcykFf16XWsW'
      iv_sha1_commit_old = 'cd20cbd60ce772edb54f7620eeb7552d2c0e04b0'
      iv_sha1_commit_new = '7b8030cc56e41da96ee99c13b4c97f5223168397' ).

    cl_abap_unit_assert=>assert_initial( act = lt_act_merge_requests ).

  ENDMETHOD.

  METHOD branch_merged.

    cl_abap_unit_assert=>assert_true( act = zcl_ags_merge_requests=>is_branch_merged(
      iv_repo_name = 'ags_ci2' iv_target_branch = 'refs/heads/master'
      iv_source_branch = 'refs/heads/f2' ) ).

  ENDMETHOD.

  METHOD branch_not_merged.

    cl_abap_unit_assert=>assert_false( act = zcl_ags_merge_requests=>is_branch_merged(
      iv_repo_name = 'ags_ci4' iv_target_branch = 'refs/heads/master'
      iv_source_branch = 'refs/heads/f1' ) ).

  ENDMETHOD.

ENDCLASS.
