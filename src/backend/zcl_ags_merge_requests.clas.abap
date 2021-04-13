CLASS zcl_ags_merge_requests DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS find_merged_merge_requests
      IMPORTING
        !iv_repo                 TYPE zags_repo
        !iv_target_branch        TYPE zags_branch
        !iv_sha1_commit_new      TYPE zags_sha1
        !iv_sha1_commit_old      TYPE zags_sha1
      RETURNING
        VALUE(rt_merge_requests) TYPE zags_merge_req_htt
      RAISING
        zcx_ags_error .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS find_source_branch
      IMPORTING
                iv_repo               TYPE zags_repo
                iv_sha1_commit_latest TYPE zags_sha1
                iv_sha1_commit_old    TYPE zags_sha1
                iv_target_branch      TYPE zags_branch
      RETURNING VALUE(rv_branch)      TYPE zags_branch
      RAISING   zcx_ags_error.

ENDCLASS.



CLASS ZCL_AGS_MERGE_REQUESTS IMPLEMENTATION.


  METHOD find_merged_merge_requests.

    DATA(lo_ags_cache) = NEW zcl_ags_cache( iv_repo = iv_repo iv_commit = iv_sha1_commit_new ).
    DATA(lt_commits) = lo_ags_cache->list_commits( ).

    LOOP AT lt_commits REFERENCE INTO DATA(lr_commit).
      IF lr_commit->*-sha1 = iv_sha1_commit_old.
        EXIT.
      ENDIF.
      DATA(lv_source_branch) = find_source_branch( iv_repo = iv_repo
        iv_sha1_commit_latest =
        COND zags_sha1( WHEN lr_commit->*-parent2 IS NOT INITIAL THEN lr_commit->*-parent2 ELSE lr_commit->*-sha1 )
        iv_sha1_commit_old = iv_sha1_commit_old
        iv_target_branch = iv_target_branch ).
      IF lv_source_branch IS NOT INITIAL.

        SELECT * FROM zags_merge_req INTO @DATA(ls_merge_request)
          WHERE repo = @iv_repo AND target_branch = @iv_target_branch
          AND source_branch = @lv_source_branch.
          INSERT ls_merge_request INTO TABLE rt_merge_requests.
        ENDSELECT.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD find_source_branch.

    DATA(lo_ags_cache) = NEW zcl_ags_cache( iv_repo = iv_repo iv_commit = iv_sha1_commit_latest ).
    DATA(lt_commits) = lo_ags_cache->list_commits( ).

    LOOP AT lt_commits REFERENCE INTO DATA(lr_commit).
      SELECT branch UP TO 1 ROWS FROM zags_branches INTO rv_branch
        WHERE repo = iv_repo AND sha1 = lr_commit->*-sha1
        AND branch <> iv_target_branch.
      ENDSELECT.
      IF sy-subrc = 0.
        RETURN.
      ENDIF.
      IF lr_commit->*-sha1 = iv_sha1_commit_old.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
