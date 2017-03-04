class ZCL_AGS_BRANCH definition
  public
  create public .

public section.

  class-methods CREATE
    importing
      !IO_REPO type ref to ZCL_AGS_REPO
      !IV_NAME type ZAGS_BRANCHES-NAME
      !IV_COMMIT type ZAGS_SHA1
    raising
      ZCX_AGS_ERROR .
  methods CONSTRUCTOR
    importing
      !IO_REPO type ref to ZCL_AGS_REPO
      !IV_NAME type ZAGS_BRANCHES-NAME
    raising
      ZCX_AGS_ERROR .
  methods DELETE .
  methods GET_CACHE
    returning
      value(RO_CACHE) type ref to ZCL_AGS_CACHE .
  methods GET_DATA
    returning
      value(RS_DATA) type ZAGS_BRANCHES .
  methods UPDATE_SHA1
    importing
      !IV_SHA1 type ZAGS_SHA1 .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA ms_data TYPE zags_branches.
ENDCLASS.



CLASS ZCL_AGS_BRANCH IMPLEMENTATION.


  METHOD constructor.

    DATA: lv_repo TYPE zags_repos-repo.


    lv_repo = io_repo->get_data( )-repo.

    ms_data = zcl_ags_db=>get_branches( )->single(
      iv_repo = lv_repo
      iv_name = iv_name ).

  ENDMETHOD.


  METHOD create.

    DATA: ls_branch TYPE zags_branches.

* todo, validate that iv_commit exists?

    ls_branch-repo   = io_repo->get_data( )-repo.
    ls_branch-branch = zcl_ags_util=>uuid( ).
    ls_branch-name   = iv_name.
    ls_branch-sha1   = iv_commit.

    zcl_ags_db=>get_branches( )->insert( ls_branch ).

  ENDMETHOD.


  METHOD delete.

    zcl_ags_db=>get_branches( )->delete(
      iv_repo = ms_data-repo
      iv_name = ms_data-name ).

* todo, unreferenced objects stay in the database
* see https://github.com/larshp/abapGitServer/issues/41

  ENDMETHOD.


  METHOD get_cache.

    CREATE OBJECT ro_cache
      EXPORTING
        iv_repo   = ms_data-repo
        iv_commit = ms_data-sha1.

  ENDMETHOD.


  METHOD get_data.

    rs_data = ms_data.

  ENDMETHOD.


  METHOD update_sha1.

    ASSERT NOT iv_sha1 IS INITIAL.

    zcl_ags_db=>get_branches( )->update_sha1(
      iv_sha1   = iv_sha1
      iv_repo   = ms_data-repo
      iv_branch = ms_data-branch ).
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
