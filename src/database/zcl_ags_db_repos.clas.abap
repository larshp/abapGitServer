CLASS zcl_ags_db_repos DEFINITION
  PUBLIC
  CREATE PRIVATE

  GLOBAL FRIENDS zcl_ags_db .

  PUBLIC SECTION.

    METHODS single
      IMPORTING
        !iv_name       TYPE zags_repos-name
      RETURNING
        VALUE(rs_repo) TYPE zags_repos
      RAISING
        zcx_ags_error .
    METHODS list
      RETURNING
        VALUE(rt_list) TYPE zags_repos_tt .
    METHODS delete
      IMPORTING
        !iv_name TYPE zags_repos-name .
    METHODS update_description
      IMPORTING
        !iv_repo        TYPE zags_repos-repo
        !iv_description TYPE zags_repos-description .
    METHODS insert
      IMPORTING
        !is_repo TYPE zags_repos .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_repos TYPE zags_repos_tt .
    DATA mv_fake TYPE abap_bool .

    METHODS set_fake .
ENDCLASS.



CLASS ZCL_AGS_DB_REPOS IMPLEMENTATION.


  METHOD delete.

    IF mv_fake = abap_true.
      DELETE mt_repos WHERE name = iv_name.             "#EC CI_SORTSEQ
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
        INTO TABLE rt_list.               "#EC CI_NOWHERE "#EC CI_SUBRC
    ENDIF.

  ENDMETHOD.


  METHOD set_fake.

    mv_fake = abap_true.

  ENDMETHOD.


  METHOD single.

    IF mv_fake = abap_true.
      READ TABLE mt_repos INTO rs_repo
        WITH KEY name = iv_name.          "#EC CI_SUBRC "#EC CI_SORTSEQ
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
