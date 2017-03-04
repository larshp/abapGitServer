class ZCL_AGS_DB_REPOS definition
  public
  create private

  global friends ZCL_AGS_DB .

public section.

  methods SINGLE
    importing
      !IV_NAME type ZAGS_REPOS-NAME
    returning
      value(RS_REPO) type ZAGS_REPOS
    raising
      ZCX_AGS_ERROR .
  methods LIST
    returning
      value(RT_LIST) type ZAGS_REPOS_TT .
  methods DELETE
    importing
      !IV_NAME type ZAGS_REPOS-NAME .
  methods UPDATE_DESCRIPTION
    importing
      !IV_REPO type ZAGS_REPOS-REPO
      !IV_DESCRIPTION type ZAGS_REPOS-DESCRIPTION .
  methods INSERT
    importing
      !IS_REPO type ZAGS_REPOS .
protected section.
private section.

  data MT_REPOS type ZAGS_REPOS_TT .
  data MV_FAKE type ABAP_BOOL .

  methods SET_FAKE .
ENDCLASS.



CLASS ZCL_AGS_DB_REPOS IMPLEMENTATION.


  METHOD delete.

    IF mv_fake = abap_true.
      DELETE mt_repos WHERE name = iv_name.
    ELSE.
      DELETE FROM zags_repos WHERE name = iv_name.        "#EC CI_SUBRC
    ENDIF.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD insert.

    IF mv_fake = abap_true.
      INSERT is_repo INTO TABLE mt_repos.
    ELSE.
      INSERT zags_repos FROM is_repo.                     "#EC CI_SUBRC
    ENDIF.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD list.

    IF mv_fake = abap_true.
      rt_list = mt_repos.
    ELSE.
      SELECT * FROM zags_repos
        INTO TABLE rt_list
        ORDER BY name ASCENDING.          "#EC CI_NOWHERE "#EC CI_SUBRC
    ENDIF.

  ENDMETHOD.


  METHOD set_fake.

    mv_fake = abap_true.

  ENDMETHOD.


  METHOD single.

    IF mv_fake = abap_true.
      READ TABLE mt_repos INTO rs_repo
        WITH KEY name = iv_name.                          "#EC CI_SUBRC
    ELSE.
      SELECT SINGLE * FROM zags_repos
        INTO rs_repo
        WHERE name = iv_name.
    ENDIF.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ags_error
        EXPORTING
          textid = zcx_ags_error=>m002.
    ENDIF.

  ENDMETHOD.


  METHOD update_description.

    FIELD-SYMBOLS: <ls_repo> LIKE LINE OF mt_repos.


    IF mv_fake = abap_true.
      READ TABLE mt_repos WITH KEY repo = iv_repo ASSIGNING <ls_repo>.
      ASSERT sy-subrc = 0.
      <ls_repo>-description = iv_description.
    ELSE.
      UPDATE zags_repos
        SET description = iv_description
        WHERE repo = iv_repo.                             "#EC CI_SUBRC
      ASSERT sy-subrc = 0.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
