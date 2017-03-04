class ZCL_AGS_CACHE definition
  public
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IV_REPO type ZAGS_REPO
      !IV_COMMIT type ZAGS_SHA1 .
  methods LIST_COMMITS
    returning
      value(RT_COMMITS) type ZCL_AGS_OBJ_COMMIT=>TY_PRETTY_TT .
  methods LIST_COMMITS_BY_FILE .
  methods LIST_COMMITS_BY_USER .
  methods LIST_FILES_BY_PATH
    importing
      !IV_REPO type ZAGS_REPO_NAME
      !IO_COMMIT type ref to ZCL_AGS_OBJ_COMMIT
      !IV_PATH type STRING
    returning
      value(RT_FILES) type ZAGS_TREE_CACHE_DATA_TT
    raising
      ZCX_AGS_ERROR .
protected section.
private section.

  data MV_REPO type ZAGS_REPO .
  data MV_COMMIT type ZAGS_SHA1 .
ENDCLASS.



CLASS ZCL_AGS_CACHE IMPLEMENTATION.


  METHOD constructor.
    mv_repo   = iv_repo.
    mv_commit = iv_commit.
  ENDMETHOD.


  METHOD list_commits.

    DATA: lt_visit  TYPE STANDARD TABLE OF zags_sha1,
          ls_data   TYPE zcl_ags_obj_commit=>ty_pretty,
          lo_branch TYPE REF TO zcl_ags_branch,
          lv_commit LIKE LINE OF lt_visit.


    APPEND mv_commit TO lt_visit.

    LOOP AT lt_visit INTO lv_commit.
      ls_data = zcl_ags_obj_commit=>get_instance( lv_commit )->get_pretty( ).

      APPEND ls_data TO rt_commits.

      IF NOT ls_data-parent IS INITIAL.
        READ TABLE lt_visit FROM ls_data-parent TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND ls_data-parent TO lt_visit.
        ENDIF.
      ENDIF.
      IF NOT ls_data-parent2 IS INITIAL.
        READ TABLE lt_visit FROM ls_data-parent2 TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND ls_data-parent2 TO lt_visit.
        ENDIF.
      ENDIF.

    ENDLOOP.

    SORT rt_commits BY author-time DESCENDING.

  ENDMETHOD.


  METHOD list_commits_by_file.

* todo, return list of commits
* inputs: filename/+path
* https://github.com/larshp/abapGitServer/issues/23
    RETURN.

  ENDMETHOD.


  METHOD LIST_COMMITS_BY_USER.

* todo, return list of commits
* input: user
* https://github.com/larshp/abapGitServer/issues/20
    RETURN.

  ENDMETHOD.


  METHOD LIST_FILES_BY_PATH.

    DATA: lv_commit_sha1 TYPE zags_sha1,
          lv_repo        TYPE zags_tree_cache-repo.


    lv_repo = zcl_ags_repo=>get_instance( iv_repo )->get_data( )-repo.
    lv_commit_sha1 = io_commit->zif_ags_object~sha1( ).

* todo
*    SELECT * FROM zags_tree_cache
*      INTO CORRESPONDING FIELDS OF TABLE rt_files
*      WHERE repo = lv_repo
*      AND commit_sha1 = lv_commit_sha1
*      AND path = iv_path.
*    IF sy-subrc = 0.
*      RETURN.
*    ENDIF.

  ENDMETHOD.
ENDCLASS.
