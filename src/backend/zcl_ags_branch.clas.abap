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
        !io_repo TYPE REF TO zcl_ags_repo
        !iv_name TYPE zags_branches-name
      RAISING
        zcx_ags_error.
    METHODS constructor
      IMPORTING
        !io_repo TYPE REF TO zcl_ags_repo
        !iv_name TYPE zags_branches-name
      RAISING
        zcx_ags_error.

  PRIVATE SECTION.
    DATA ms_data TYPE zags_branches.
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


    DATA(lo_blob) = NEW zcl_ags_obj_blob( ).
    lo_blob->set_data( zcl_ags_util=>string_to_xstring_utf8( 'test' ) ) ##NO_TEXT.
    lo_blob->save( ).

    DATA(lo_tree) = NEW zcl_ags_obj_tree( ).
    lo_tree->add_file( iv_chmod = zcl_ags_obj_tree=>c_chmod-file
                       iv_name  = 'test.txt'
                       iv_sha1  = lo_blob->sha1( ) ) ##NO_TEXT.
    lo_tree->save( ).

    DATA(lo_commit) = NEW zcl_ags_obj_commit( ).
    lo_commit->set_tree( lo_tree->sha1( ) ).
    lo_commit->set_author( 'author' ) ##NO_TEXT.
    lo_commit->set_body( 'body' ) ##NO_TEXT.
    lo_commit->set_committer( 'committer' ) ##NO_TEXT.
    lo_commit->save( ).

    ls_branch-repo   = io_repo->get_data( )-repo.
    ls_branch-branch = zcl_ags_util=>uuid( ).
    ls_branch-name   = iv_name.
    ls_branch-sha1   = lo_commit->sha1( ).

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