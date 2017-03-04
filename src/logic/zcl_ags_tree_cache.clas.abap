class ZCL_AGS_TREE_CACHE definition
  public
  create public .

public section.

  class-methods BUILD .
  class-methods GET
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
ENDCLASS.



CLASS ZCL_AGS_TREE_CACHE IMPLEMENTATION.


  METHOD build.

* todo
    RETURN.

  ENDMETHOD.


  METHOD get.

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
