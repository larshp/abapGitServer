class ZCL_AGS_REPO definition
  public
  create public .

public section.

  methods GET
    returning
      value(RS_DATA) type ZAGS_REPOS .
  class-methods LIST
    returning
      value(RT_LIST) type ZAGS_REPOS_TT .
  methods LIST_BRANCHES
    returning
      value(RT_LIST) type ZAGS_BRANCHES_TT .
  class-methods CREATE
    importing
      !IV_NAME type ZAGS_REPOS-NAME
    returning
      value(RO_REPO) type ref to ZCL_AGS_REPO
    raising
      ZCX_AGS_ERROR .
  methods CONSTRUCTOR
    importing
      !IV_NAME type ZAGS_REPOS-NAME
    raising
      ZCX_AGS_ERROR .
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

  DATA: lt_list TYPE zags_repos_tt,
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