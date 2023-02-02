FUNCTION zags_handle_merge_requests.
*"----------------------------------------------------------------------
*"*"Verbuchungsfunktionsbaustein:
*"
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(IV_REPO) TYPE  ZAGS_REPO
*"     VALUE(IV_TARGET_BRANCH) TYPE  ZAGS_BRANCH
*"     VALUE(IV_SHA1_COMMIT_NEW) TYPE  ZAGS_SHA1
*"     VALUE(IV_SHA1_COMMIT_OLD) TYPE  ZAGS_SHA1
*"----------------------------------------------------------------------

  DATA: lt_merge_requests TYPE zags_merge_req_htt.
  FIELD-SYMBOLS: <ls_mr> TYPE zags_merge_req.
  TRY.
      lt_merge_requests = zcl_ags_merge_requests=>find_merged_merge_requests(
        iv_repo = iv_repo iv_target_branch = iv_target_branch
        iv_sha1_commit_new = iv_sha1_commit_new iv_sha1_commit_old = iv_sha1_commit_old ).
      LOOP AT lt_merge_requests ASSIGNING <ls_mr>.
        CALL FUNCTION 'ENQUEUE_EZAGS_MERGE_REQ'
          EXPORTING
            repo         = iv_repo
            id           = <ls_mr>-id
            _wait        = abap_true
          EXCEPTIONS
            foreign_lock = 2.
        IF sy-subrc <> 0.
          DELETE TABLE lt_merge_requests FROM <ls_mr>.
          CONTINUE.
        ENDIF.
        <ls_mr>-merged = abap_true.
      ENDLOOP.
      UPDATE zags_merge_req FROM TABLE lt_merge_requests.
    CATCH zcx_ags_error.
      ASSERT 1 = 0.
  ENDTRY.

ENDFUNCTION.
