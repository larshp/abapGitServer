CLASS zcl_ags_repo DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      ty_repos_tt TYPE STANDARD TABLE OF zags_repos WITH DEFAULT KEY.
    TYPES:
      ty_branches_tt TYPE STANDARD TABLE OF REF TO zcl_ags_branch WITH DEFAULT KEY.

    CLASS-METHODS create
      IMPORTING
        !iv_name       TYPE zags_repos-name
      RETURNING
        VALUE(ro_repo) TYPE REF TO zcl_ags_repo
      RAISING
        zcx_ags_error.
    CLASS-METHODS list
      RETURNING
        VALUE(rt_list) TYPE ty_repos_tt.
    METHODS constructor
      IMPORTING
        !iv_name TYPE zags_repos-name
      RAISING
        zcx_ags_error.
    METHODS delete
      RAISING
        zcx_ags_error.
    METHODS get_data
      RETURNING
        VALUE(rs_data) TYPE zags_repos.
    METHODS list_branches
      RETURNING
        VALUE(rt_list) TYPE ty_branches_tt
      RAISING
        zcx_ags_error.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA ms_data TYPE zags_repos.
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
    ls_repo-head = 'refs/heads/master' ##NO_TEXT.

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


  METHOD get_data.

    rs_data = ms_data.

  ENDMETHOD.


  METHOD list.

    SELECT * FROM zags_repos
      INTO TABLE rt_list.                               "#EC CI_NOWHERE

  ENDMETHOD.


  METHOD list_branches.

    DATA: lt_list   TYPE TABLE OF zags_branches-name,
          lo_branch TYPE REF TO zcl_ags_branch.


    SELECT name FROM zags_branches INTO TABLE lt_list
      WHERE repo = ms_data-repo.

    LOOP AT lt_list ASSIGNING FIELD-SYMBOL(<lv_list>).
      CREATE OBJECT lo_branch
        EXPORTING
          io_repo = me
          iv_name = <lv_list>.

      APPEND lo_branch TO rt_list.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.