class ZCL_AGS_DB_BRANCHES definition
  public
  create private

  global friends ZCL_AGS_DB .

public section.

  methods UPDATE_SHA1
    importing
      !IV_REPO type ZAGS_BRANCHES-REPO
      !IV_BRANCH type ZAGS_BRANCHES-BRANCH
      !IV_SHA1 type ZAGS_BRANCHES-SHA1 .
  methods SINGLE
    importing
      !IV_REPO type ZAGS_BRANCHES-REPO
      !IV_NAME type ZAGS_BRANCHES-NAME
    returning
      value(RS_DATA) type ZAGS_BRANCHES
    raising
      ZCX_AGS_ERROR .
  methods LIST
    importing
      !IV_REPO type ZAGS_BRANCHES-REPO
    returning
      value(RT_LIST) type ZAGS_BRANCHES_TT .
  methods DELETE
    importing
      !IV_REPO type ZAGS_BRANCHES-REPO
      !IV_NAME type ZAGS_BRANCHES-NAME .
  methods INSERT
    importing
      !IS_BRANCH type ZAGS_BRANCHES .
protected section.
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
      DELETE rt_list WHERE repo <> iv_repo.
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
