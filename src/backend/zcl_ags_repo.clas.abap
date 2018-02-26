CLASS zcl_ags_repo DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_branches_tt TYPE STANDARD TABLE OF REF TO zcl_ags_branch WITH DEFAULT KEY .

    CLASS-METHODS create
      IMPORTING
        !iv_name        TYPE zags_repos-name
        !iv_description TYPE zags_repos-description
      RETURNING
        VALUE(ro_repo)  TYPE REF TO zcl_ags_repo
      RAISING
        zcx_ags_error .
    CLASS-METHODS get_instance
      IMPORTING
        !iv_name       TYPE zags_repos-name
      RETURNING
        VALUE(ro_repo) TYPE REF TO zcl_ags_repo
      RAISING
        zcx_ags_error .
    CLASS-METHODS list
      RETURNING
        VALUE(rt_list) TYPE zags_repos_tt .
    METHODS constructor
      IMPORTING
        !iv_name TYPE zags_repos-name
      RAISING
        zcx_ags_error .
    METHODS create_branch
      IMPORTING
        !iv_name         TYPE zags_branches-name
        !iv_commit       TYPE zags_sha1
      RETURNING
        VALUE(ro_branch) TYPE REF TO zcl_ags_branch
      RAISING
        zcx_ags_error .
    METHODS delete
      RAISING
        zcx_ags_error .
    METHODS get_branch
      IMPORTING
        !iv_branch_name  TYPE zags_branch_name
      RETURNING
        VALUE(ro_branch) TYPE REF TO zcl_ags_branch
      RAISING
        zcx_ags_error .
    METHODS get_data
      RETURNING
        VALUE(rs_data) TYPE zags_repos .
    METHODS get_default_branch
      RETURNING
        VALUE(ro_branch) TYPE REF TO zcl_ags_branch
      RAISING
        zcx_ags_error .
    METHODS list_branches
      RETURNING
        VALUE(rt_list) TYPE ty_branches_tt
      RAISING
        zcx_ags_error .
    METHODS set_description
      IMPORTING
        !iv_description TYPE zags_repos-description
      RAISING
        zcx_ags_error .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA ms_data TYPE zags_repos .

    CLASS-METHODS initial_commit
      IMPORTING
        !iv_repo         TYPE zags_repos-repo
        !iv_name         TYPE clike
      RETURNING
        VALUE(rv_commit) TYPE zags_sha1
      RAISING
        zcx_ags_error .
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
    ls_repo-head        = '/refs/heads/master' ##no_text.

    zcl_ags_db=>get_repos( )->insert( ls_repo ).

    ro_repo = get_instance( iv_name ).

    ro_repo->create_branch(
      iv_name   = ls_repo-head
      iv_commit = initial_commit(
        iv_repo = ls_repo-repo
        iv_name = iv_name ) ).

  ENDMETHOD.


  METHOD create_branch.

    DATA: ls_branch TYPE zags_branches.

* validate that iv_commit exists?
    zcl_ags_obj_commit=>load(
      iv_repo = ms_data-repo
      iv_sha1 = iv_commit ).

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
      IF ro_branch->get_data( )-name = iv_branch_name
          OR ro_branch->get_data( )-name = 'refs/heads/' && iv_branch_name.
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


    lo_blob = zcl_ags_obj_blob=>new( iv_repo ).
    lv_str = iv_name.
    lo_blob->set_data( zcl_ags_util=>string_to_xstring_utf8( lv_str ) ).
    lo_blob->save( ).

    lo_tree = zcl_ags_obj_tree=>new( iv_repo ).
    lo_tree->add_file( iv_chmod = zcl_ags_obj_tree=>c_chmod-file
                       iv_name  = 'README.md'
                       iv_sha1  = lo_blob->get_sha1( ) ) ##no_text.
    lo_tree->save( ).

    lv_user = |initial <foo@bar.com> { zcl_ags_util=>get_time( ) }|.

    lo_commit = zcl_ags_obj_commit=>new( iv_repo ).
    lo_commit->set_tree( lo_tree->get_sha1( ) ).
    lo_commit->set_author( lv_user ).
    lo_commit->set_committer( lv_user ).
    lo_commit->set_body( 'initial' ) ##no_text.
    lo_commit->save( ).

    rv_commit = lo_commit->get_sha1( ).

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
