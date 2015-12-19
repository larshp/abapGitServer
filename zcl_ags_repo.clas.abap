CLASS zcl_ags_repo DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: ty_repos_tt TYPE STANDARD TABLE OF zags_repos WITH DEFAULT KEY,
           ty_branches_tt type standard table of zags_branches with default key.

    METHODS get
      RETURNING
        VALUE(rs_data) TYPE zags_repos .
    CLASS-METHODS list
      RETURNING
        VALUE(rt_list) TYPE ty_repos_tt .
    METHODS list_branches
      RETURNING
        VALUE(rt_list) TYPE ty_branches_tt .
    CLASS-METHODS create
      IMPORTING
        !iv_name       TYPE zags_repos-name
      RETURNING
        VALUE(ro_repo) TYPE REF TO zcl_ags_repo
      RAISING
        zcx_ags_error .
    METHODS constructor
      IMPORTING
        !iv_name TYPE zags_repos-name
      RAISING
        zcx_ags_error .
protected section.
private section.

  data MS_DATA type ZAGS_REPOS .
ENDCLASS.



CLASS ZCL_AGS_REPO IMPLEMENTATION.


METHOD constructor.

  SELECT SINGLE * FROM zags_repos INTO ms_data
    WHERE name = iv_name.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_ags_error
      EXPORTING
        textid = zcx_ags_error=>m002.
  ENDIF.

ENDMETHOD.


METHOD create.

  DATA: lt_list TYPE ty_repos_tt,
        ls_repo TYPE zags_repos.


  lt_list = list( ).
  READ TABLE lt_list WITH KEY name = iv_name TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    RAISE EXCEPTION TYPE zcx_ags_error
      EXPORTING
        textid = zcx_ags_error=>m001.
  ENDIF.

  ls_repo-repo = zcl_ags_util=>uuid( ).
  ls_repo-name = iv_name.
  ls_repo-head = 'refs/heads/master'.

  INSERT zags_repos FROM ls_repo.
  ASSERT sy-subrc = 0.

  ro_repo = NEW zcl_ags_repo( iv_name ).
  zcl_ags_branch=>create( io_repo = ro_repo
                          iv_name = ls_repo-head ).

ENDMETHOD.


METHOD get.

  rs_data = ms_data.

ENDMETHOD.


METHOD list.

  SELECT * FROM zags_repos
    INTO TABLE rt_list.

ENDMETHOD.


METHOD list_branches.

  SELECT * FROM zags_branches INTO TABLE rt_list
    WHERE repo = ms_data-repo.

ENDMETHOD.
ENDCLASS.