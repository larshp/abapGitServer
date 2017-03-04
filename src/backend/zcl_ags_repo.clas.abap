class ZCL_AGS_REPO definition
  public
  create public .

public section.

  types:
    ty_branches_tt TYPE STANDARD TABLE OF REF TO zcl_ags_branch WITH DEFAULT KEY .

  class-methods CREATE
    importing
      !IV_NAME type ZAGS_REPOS-NAME
      !IV_DESCRIPTION type ZAGS_REPOS-DESCRIPTION
    returning
      value(RO_REPO) type ref to ZCL_AGS_REPO
    raising
      ZCX_AGS_ERROR .
  class-methods GET_INSTANCE
    importing
      !IV_NAME type ZAGS_REPOS-NAME
    returning
      value(RO_REPO) type ref to ZCL_AGS_REPO
    raising
      ZCX_AGS_ERROR .
  class-methods LIST
    returning
      value(RT_LIST) type ZAGS_REPOS_TT .
  methods CONSTRUCTOR
    importing
      !IV_NAME type ZAGS_REPOS-NAME
    raising
      ZCX_AGS_ERROR .
  methods CREATE_BRANCH
    importing
      !IV_NAME type ZAGS_BRANCHES-NAME
      !IV_COMMIT type ZAGS_SHA1
    returning
      value(RO_BRANCH) type ref to ZCL_AGS_BRANCH
    raising
      ZCX_AGS_ERROR .
  methods DELETE
    raising
      ZCX_AGS_ERROR .
  methods GET_BRANCH
    importing
      !IV_BRANCH_NAME type ZAGS_BRANCH_NAME
    returning
      value(RO_BRANCH) type ref to ZCL_AGS_BRANCH
    raising
      ZCX_AGS_ERROR .
  methods GET_DATA
    returning
      value(RS_DATA) type ZAGS_REPOS .
  methods GET_DEFAULT_BRANCH
    returning
      value(RO_BRANCH) type ref to ZCL_AGS_BRANCH
    raising
      ZCX_AGS_ERROR .
  methods LIST_BRANCHES
    returning
      value(RT_LIST) type TY_BRANCHES_TT
    raising
      ZCX_AGS_ERROR .
  methods SET_DESCRIPTION
    importing
      !IV_DESCRIPTION type ZAGS_REPOS-DESCRIPTION
    raising
      ZCX_AGS_ERROR .
  PROTECTED SECTION.
private section.

  data MS_DATA type ZAGS_REPOS .

  class-methods INITIAL_COMMIT
    importing
      !IV_NAME type CLIKE
    returning
      value(RV_COMMIT) type ZAGS_SHA1
    raising
      ZCX_AGS_ERROR .
ENDCLASS.



CLASS ZCL_AGS_REPO IMPLEMENTATION.


  METHOD constructor.

    ms_data = zcl_ags_db=>get_repos( )->single( iv_name ).

  ENDMETHOD.


  METHOD create.

    DATA: ls_repo TYPE zags_repos.


    ASSERT NOT iv_name CA '/\'.
    ASSERT NOT iv_name IS INITIAL.


    TRY.
        zcl_ags_db=>get_repos( )->single( iv_name ).
        RAISE EXCEPTION TYPE zcx_ags_error
          EXPORTING
            textid = zcx_ags_error=>m001.
      CATCH zcx_ags_error ##NO_HANDLER.
    ENDTRY.

    ls_repo-repo        = zcl_ags_util=>uuid( ).
    ls_repo-name        = iv_name.
    ls_repo-description = iv_description.
    ls_repo-head        = 'master' ##no_text.

    zcl_ags_db=>get_repos( )->insert( ls_repo ).

    ro_repo = get_instance( iv_name ).

    ro_repo->create_branch(
      iv_name   = ls_repo-head
      iv_commit = initial_commit( iv_name ) ).

  ENDMETHOD.


  METHOD create_branch.

    DATA: ls_branch TYPE zags_branches.

* validate that iv_commit exists?
    zcl_ags_obj_commit=>get_instance( iv_commit ).

    ls_branch-repo   = ms_data-repo.
    ls_branch-branch = zcl_ags_util=>uuid( ).
    ls_branch-name   = iv_name.
    ls_branch-sha1   = iv_commit.

    zcl_ags_db=>get_branches( )->insert( ls_branch ).

    ro_branch = get_branch( iv_name ).

  ENDMETHOD.


  METHOD delete.

    DATA: lt_branches TYPE ty_branches_tt.

    FIELD-SYMBOLS: <lo_branch> LIKE LINE OF lt_branches.


    lt_branches = list_branches( ).
    LOOP AT lt_branches ASSIGNING <lo_branch>.
      <lo_branch>->delete( ).
    ENDLOOP.

    zcl_ags_db=>get_repos( )->delete( ms_data-name ).

  ENDMETHOD.


  METHOD get_branch.

    DATA: lt_branches TYPE ty_branches_tt.


    lt_branches = list_branches( ).
    LOOP AT lt_branches INTO ro_branch.
      IF ro_branch->get_data( )-name = iv_branch_name.
        RETURN.
      ENDIF.
    ENDLOOP.

    ASSERT 0 = 1.

  ENDMETHOD.


  METHOD get_data.

    rs_data = ms_data.

  ENDMETHOD.


  METHOD get_default_branch.

    ro_branch = get_branch( ms_data-head ).

  ENDMETHOD.


  METHOD get_instance.

    CREATE OBJECT ro_repo
      EXPORTING
        iv_name = iv_name.

  ENDMETHOD.


  METHOD initial_commit.

    DATA: lo_blob   TYPE REF TO zcl_ags_obj_blob,
          lo_commit TYPE REF TO zcl_ags_obj_commit,
          lo_tree   TYPE REF TO zcl_ags_obj_tree,
          lv_str    TYPE string,
          lv_user   TYPE string.


    CREATE OBJECT lo_blob.
    lv_str = iv_name.
    lo_blob->set_data( zcl_ags_util=>string_to_xstring_utf8( lv_str ) ).
    lo_blob->save( ).

    CREATE OBJECT lo_tree.
    lo_tree->add_file( iv_chmod = zcl_ags_obj_tree=>c_chmod-file
                       iv_name  = 'README.md'
                       iv_sha1  = lo_blob->sha1( ) ) ##no_text.
    lo_tree->save( ).

    lv_user = |initial <foo@bar.com> { zcl_ags_util=>get_time( ) }|.

    CREATE OBJECT lo_commit.
    lo_commit->set_tree( lo_tree->sha1( ) ).
    lo_commit->set_author( lv_user ).
    lo_commit->set_committer( lv_user ).
    lo_commit->set_body( 'initial' ) ##no_text.
    lo_commit->save( ).

    rv_commit = lo_commit->sha1( ).

  ENDMETHOD.


  METHOD list.

    rt_list = zcl_ags_db=>get_repos( )->list( ).

  ENDMETHOD.


  METHOD list_branches.

    DATA: lt_list   TYPE zags_branches_tt,
          lo_branch TYPE REF TO zcl_ags_branch.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF lt_list.


    lt_list = zcl_ags_db=>get_branches( )->list( ms_data-repo ).

    LOOP AT lt_list ASSIGNING <ls_list>.
      CREATE OBJECT lo_branch
        EXPORTING
          io_repo = me
          iv_name = <ls_list>-name.

      APPEND lo_branch TO rt_list.
    ENDLOOP.

  ENDMETHOD.


  METHOD set_description.

    zcl_ags_db=>get_repos( )->update_description(
      iv_repo        = ms_data-repo
      iv_description = iv_description ).

    ms_data-description = iv_description.

  ENDMETHOD.
ENDCLASS.
