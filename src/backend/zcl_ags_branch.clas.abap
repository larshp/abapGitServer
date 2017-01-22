CLASS zcl_ags_branch DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS delete.
    METHODS get_data
      RETURNING
        VALUE(rs_data) TYPE zags_branches.
    CLASS-METHODS create
      IMPORTING
        !io_repo   TYPE REF TO zcl_ags_repo
        !iv_name   TYPE zags_branches-name
        !iv_commit TYPE zags_sha1
      RAISING
        zcx_ags_error.
    METHODS constructor
      IMPORTING
        !io_repo TYPE REF TO zcl_ags_repo
        !iv_name TYPE zags_branches-name
      RAISING
        zcx_ags_error.
    METHODS update_sha1
      IMPORTING
        !iv_sha1 TYPE zags_sha1.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA ms_data TYPE zags_branches.
ENDCLASS.



CLASS ZCL_AGS_BRANCH IMPLEMENTATION.


  METHOD constructor.

    DATA: lv_repo TYPE zags_repos-repo.


    lv_repo = io_repo->get_data( )-repo.

    SELECT SINGLE * FROM zags_branches INTO ms_data
      WHERE name = iv_name
      AND repo = lv_repo.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ags_error
        EXPORTING
          textid = zcx_ags_error=>m002.
    ENDIF.

  ENDMETHOD.


  METHOD create.

    DATA: ls_branch TYPE zags_branches.

* todo, validate that iv_commit exists?

    ls_branch-repo   = io_repo->get_data( )-repo.
    ls_branch-branch = zcl_ags_util=>uuid( ).
    ls_branch-name   = iv_name.
    ls_branch-sha1   = iv_commit.

    INSERT zags_branches FROM ls_branch.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD delete.

    DELETE FROM zags_branches
      WHERE repo = ms_data-repo
      AND name = ms_data-name.
    ASSERT sy-subrc = 0.

* unreferenced objects stay in the database

  ENDMETHOD.


  METHOD get_data.

    rs_data = ms_data.

  ENDMETHOD.


  METHOD update_sha1.

    ASSERT NOT iv_sha1 IS INITIAL.

    UPDATE zags_branches SET sha1 = iv_sha1
      WHERE repo = ms_data-repo
      AND branch = ms_data-branch.
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
