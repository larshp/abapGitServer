class ZCL_AGS_BRANCH definition
  public
  create public .

public section.

  class-methods CREATE
    importing
      !IO_REPO type ref to ZCL_AGS_REPO
      !IV_NAME type ZAGS_BRANCHES-NAME .
  methods CONSTRUCTOR
    importing
      !IO_REPO type ref to ZCL_AGS_REPO
      !IV_NAME type ZAGS_BRANCHES-NAME .
protected section.
private section.
ENDCLASS.



CLASS ZCL_AGS_BRANCH IMPLEMENTATION.


METHOD constructor.

  BREAK-POINT.

ENDMETHOD.


METHOD create.

  DATA: ls_branch TYPE zags_branches.


  ls_branch-repo = io_repo->get( )-repo.
  ls_branch-branch = zcl_ags_util=>uuid( ).
  ls_branch-name = iv_name.


  DATA(lo_file) = NEW zcl_ags_obj_file( ).
  lo_file->set_data( zcl_ags_util=>string_to_xstring_utf8( 'test' ) ).

  DATA(lo_tree) = NEW zcl_ags_obj_tree( ).
  lo_tree->add_file( iv_chmod = zcl_ags_obj_tree=>c_chmod-file
                     iv_name  = 'test.txt'
                     io_file  = lo_file ).

  DATA(lo_commit) = NEW zcl_ags_obj_commit( ).
*  lo_commit->set_tree( lo_tree ).

* ls_branch-sha1 =

  INSERT zags_branches FROM ls_branch.
  ASSERT sy-subrc = 0.

ENDMETHOD.
ENDCLASS.