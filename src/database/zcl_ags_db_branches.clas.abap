CLASS zcl_ags_db_branches DEFINITION
  PUBLIC
  CREATE PRIVATE

  GLOBAL FRIENDS zcl_ags_db .

  PUBLIC SECTION.

    METHODS update_sha1
      IMPORTING
        !iv_repo   TYPE zags_branches-repo
        !iv_branch TYPE zags_branches-branch
        !iv_sha1   TYPE zags_branches-sha1 .
    METHODS single
      IMPORTING
        !iv_repo       TYPE zags_branches-repo
        !iv_name       TYPE zags_branches-name
      RETURNING
        VALUE(rs_data) TYPE zags_branches
      RAISING
        zcx_ags_error .
    METHODS list
      IMPORTING
        !iv_repo       TYPE zags_branches-repo
      RETURNING
        VALUE(rt_list) TYPE zags_branches_tt .
    METHODS delete
      IMPORTING
        !iv_repo TYPE zags_branches-repo
        !iv_name TYPE zags_branches-name .
    METHODS insert
      IMPORTING
        !is_branch TYPE zags_branches .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_branches TYPE zags_branches_tt .
    DATA mv_fake TYPE abap_bool .

    METHODS set_fake ##RELAX.
ENDCLASS.



CLASS ZCL_AGS_DB_BRANCHES IMPLEMENTATION.


  METHOD delete.

    IF mv_fake = abap_true.
      DELETE mt_branches
        WHERE repo = iv_repo AND name = iv_name.
    ELSE.
      DELETE FROM zags_branches
        WHERE repo = iv_repo AND name = iv_name.          "#EC CI_SUBRC
    ENDIF.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD insert.

    IF mv_fake = abap_true.
      INSERT is_branch INTO TABLE mt_branches.
    ELSE.
      INSERT zags_branches FROM is_branch.                "#EC CI_SUBRC
    ENDIF.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD list.

    IF mv_fake = abap_true.
      rt_list = mt_branches.
      DELETE rt_list WHERE repo <> iv_repo.             "#EC CI_SORTSEQ
    ELSE.
      SELECT * FROM zags_branches
        INTO TABLE rt_list
        WHERE repo = iv_repo
        ORDER BY name ASCENDING.          "#EC CI_NOWHERE "#EC CI_SUBRC
    ENDIF.

  ENDMETHOD.


  METHOD set_fake.

    mv_fake = abap_true.

  ENDMETHOD.


  METHOD single.

    IF mv_fake = abap_true.
      READ TABLE mt_branches INTO rs_data
        WITH KEY name = iv_name repo = iv_repo.           "#EC CI_SUBRC
    ELSE.
      SELECT SINGLE * FROM zags_branches INTO rs_data
        WHERE name = iv_name
        AND repo = iv_repo ##WARN_OK.
    ENDIF.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ags_error
        EXPORTING
          textid = zcx_ags_error=>m002.
    ENDIF.

  ENDMETHOD.


  METHOD update_sha1.

    FIELD-SYMBOLS: <ls_branch> LIKE LINE OF mt_branches.


    IF mv_fake = abap_true.
      READ TABLE mt_branches ASSIGNING <ls_branch>
        WITH KEY repo = iv_repo branch = iv_branch.
      ASSERT sy-subrc = 0.
      <ls_branch>-sha1 = iv_sha1.
    ELSE.
      UPDATE zags_branches
        SET sha1 = iv_sha1
        WHERE repo = iv_repo
        AND branch = iv_branch.                           "#EC CI_SUBRC
      ASSERT sy-subrc = 0.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
