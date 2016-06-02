class ZCL_AGS_REPO definition
  public
  create public .

public section.

  types:
    ty_repos_tt TYPE STANDARD TABLE OF zags_repos WITH DEFAULT KEY .
  types:
    ty_branches_tt type standard table of ref to zcl_ags_branch with default key .

  methods GET_DATA
    returning
      value(RS_DATA) type ZAGS_REPOS .
  class-methods LIST
    returning
      value(RT_LIST) type TY_REPOS_TT .
  methods LIST_BRANCHES
    returning
      value(RT_LIST) type TY_BRANCHES_TT
    raising
      ZCX_AGS_ERROR .
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
  methods DELETE
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


METHOD delete.

  DATA(lt_branches) = list_branches( ).
  LOOP AT lt_branches ASSIGNING FIELD-SYMBOL(<lo_branch>).
    <lo_branch>->delete( ).
  ENDLOOP.

  DELETE FROM zags_repos WHERE name = ms_data-name.
  ASSERT sy-subrc = 0.

ENDMETHOD.


METHOD GET_DATA.

  rs_data = ms_data.

ENDMETHOD.


METHOD list.

  SELECT * FROM zags_repos
    INTO TABLE rt_list.

ENDMETHOD.


METHOD list_branches.

  DATA: lt_list   TYPE TABLE OF zags_branches,
        lo_branch TYPE REF TO zcl_ags_branch.


  SELECT * FROM zags_branches INTO TABLE lt_list
    WHERE repo = ms_data-repo.

  LOOP AT lt_list ASSIGNING FIELD-SYMBOL(<ls_list>).
    CREATE OBJECT lo_branch
      EXPORTING
        io_repo = me
        iv_name = <ls_list>-name.

    APPEND lo_branch TO rt_list.
  ENDLOOP.

ENDMETHOD.
ENDCLASS.