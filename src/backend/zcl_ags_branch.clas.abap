class ZCL_AGS_BRANCH definition
  public
  create public .

public section.

  types:
    BEGIN OF ty_push,
        old    TYPE zags_sha1,
        new    TYPE zags_sha1,
        name   TYPE zags_branch_name,
        length TYPE i,
      END OF ty_push .

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
  methods PUSH
    importing
      !IV_NEW type ZAGS_SHA1
      !IV_OLD type ZAGS_SHA1
      !IT_OBJECTS type ZCL_AGS_PACK=>TY_OBJECTS_TT
    raising
      ZCX_AGS_ERROR .
  methods UPDATE_SHA1
    importing
      !IV_SHA1 type ZAGS_SHA1 .
protected section.
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


  METHOD push.

    ASSERT NOT iv_new IS INITIAL.
    ASSERT NOT iv_old IS INITIAL.
    ASSERT NOT it_objects IS INITIAL.

    READ TABLE it_objects WITH KEY sha1 = iv_new TRANSPORTING NO FIELDS.
* new commit should exist in objects
    ASSERT sy-subrc = 0.

    ASSERT get_data( )-sha1 = iv_old.

* todo, add object validations?
    zcl_ags_pack=>save( it_objects ).

    update_sha1( iv_new ).

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
