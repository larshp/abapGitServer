class ZCL_AGS_BRANCH definition
  public
  create public .

public section.

  methods DELETE .
  methods GET_DATA
    returning
      value(RS_DATA) type ZAGS_BRANCHES .
  class-methods CREATE
    importing
      !IO_REPO type ref to ZCL_AGS_REPO
      !IV_NAME type ZAGS_BRANCHES-NAME .
  methods CONSTRUCTOR
    importing
      !IO_REPO type ref to ZCL_AGS_REPO
      !IV_NAME type ZAGS_BRANCHES-NAME
    raising
      ZCX_AGS_ERROR .
protected section.
private section.

  data MS_DATA type ZAGS_BRANCHES .
ENDCLASS.



CLASS ZCL_AGS_BRANCH IMPLEMENTATION.


METHOD constructor.

  DATA(lv_repo) = io_repo->get_data( )-repo.

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


  ls_branch-repo   = io_repo->get_data( )-repo.
  ls_branch-branch = zcl_ags_util=>uuid( ).
  ls_branch-name   = iv_name.


  DATA(lo_file) = NEW zcl_ags_obj_file( ).
  lo_file->set_data( zcl_ags_util=>string_to_xstring_utf8( 'test' ) ).

  DATA(lo_tree) = NEW zcl_ags_obj_tree( ).
  lo_tree->add_file( iv_chmod = zcl_ags_obj_tree=>c_chmod-file
                     iv_name  = 'test.txt'
                     io_file  = lo_file ).

  DATA(lo_commit) = NEW zcl_ags_obj_commit( ).
  lo_commit->set_tree( lo_tree->sha1( ) ).
  lo_commit->set_author( 'author' ).
  lo_commit->set_body( 'body' ).
  lo_commit->set_committer( 'committer' ).

  ls_branch-sha1 = lo_commit->sha1( ).

  INSERT zags_branches FROM ls_branch.
  ASSERT sy-subrc = 0.

ENDMETHOD.


METHOD delete.

  DELETE FROM zags_branches
    WHERE repo = ms_data-repo
    AND name = ms_data-name.
  ASSERT sy-subrc = 0.

* todo, delete unreferenced objects?

ENDMETHOD.


METHOD get_data.

  rs_data = ms_data.

ENDMETHOD.
ENDCLASS.