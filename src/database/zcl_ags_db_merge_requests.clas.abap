CLASS zcl_ags_db_merge_requests DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
                iv_repo_name     TYPE zags_repo_name
                iv_target_branch TYPE zags_branch_name
                iv_source_branch TYPE zags_branch_name
                iv_title         TYPE zags_merge_request_title
      RETURNING VALUE(rv_req)    TYPE zags_merge_req
      RAISING
                zcx_ags_error.
    CLASS-METHODS list_open
      IMPORTING
                iv_repo_name       TYPE zags_repo_name
      RETURNING VALUE(rt_requests) TYPE zags_merge_req_tt
      RAISING
                zcx_ags_error.
    CLASS-METHODS merge
      IMPORTING
        iv_repo_name TYPE zags_repo_name
        iv_id        TYPE zags_merge_request_id
      RAISING
        zcx_ags_error.
    CLASS-METHODS delete
      IMPORTING
        iv_repo_name TYPE zags_repo_name
        iv_id        TYPE zags_merge_request_id
      RAISING
        zcx_ags_error.
    CLASS-METHODS single
      IMPORTING
                iv_repo_name      TYPE zags_repo_name
                iv_id             TYPE zags_merge_request_id
      RETURNING VALUE(rs_request) TYPE zags_merge_req_s
      RAISING
                zcx_ags_error.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AGS_DB_MERGE_REQUESTS IMPLEMENTATION.


  METHOD create.

    DATA(ls_repo) = zcl_ags_db=>get_repos( )->single( iv_repo_name ).
    SELECT id FROM zags_merge_req
        UP TO 1 ROWS
        INTO @DATA(lv_id)
        WHERE repo = @ls_repo-repo
        ORDER BY id DESCENDING.
    ENDSELECT.
    lv_id = lv_id + 1.

    DATA(ls_target_branch) = zcl_ags_db=>get_branches( )->single(
      iv_repo = ls_repo-repo iv_name = iv_target_branch ).
    DATA(ls_source_branch) = zcl_ags_db=>get_branches( )->single(
      iv_repo = ls_repo-repo iv_name = iv_source_branch ).

    rv_req = VALUE #(
      repo = ls_repo-repo id = lv_id target_branch = ls_target_branch-branch
      source_branch = ls_source_branch-branch title = iv_title
      created_by = sy-uname ).
    INSERT zags_merge_req FROM rv_req.

  ENDMETHOD.


  METHOD delete.

    DATA(ls_repo) = zcl_ags_db=>get_repos( )->single( iv_repo_name ).
    DELETE FROM zags_merge_req WHERE repo = ls_repo-repo
      AND id = iv_id.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ags_error
        EXPORTING
          textid = zcx_ags_error=>m013
          repo_name = iv_repo_name
          id = iv_id.
    ENDIF.

  ENDMETHOD.


  METHOD list_open.

    DATA(ls_repo) = zcl_ags_db=>get_repos( )->single( iv_repo_name ).
    SELECT * FROM zags_merge_req INTO TABLE rt_requests
      WHERE repo = ls_repo-repo AND merged = abap_false.

  ENDMETHOD.


  METHOD merge.

    DATA(ls_repo) = zcl_ags_db=>get_repos( )->single( iv_repo_name ).


  ENDMETHOD.


  METHOD single.

    DATA(ls_repo) = zcl_ags_db=>get_repos( )->single( iv_repo_name ).
    ##TOO_MANY_ITAB_FIELDS
    SELECT SINGLE * FROM zags_merge_req INTO rs_request
      WHERE repo = ls_repo-repo AND id = iv_id.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ags_error
        EXPORTING
          textid    = zcx_ags_error=>m013
          repo_name = iv_repo_name
          id        = iv_id.
    ENDIF.

    SELECT SINGLE name FROM zags_branches INTO rs_request-target_branch_name
      WHERE repo = ls_repo-repo AND branch = rs_request-target_branch.
    SELECT SINGLE name FROM zags_branches INTO rs_request-source_branch_name
      WHERE repo = ls_repo-repo AND branch = rs_request-source_branch.

  ENDMETHOD.
ENDCLASS.
