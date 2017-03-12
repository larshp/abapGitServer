CLASS zcl_ags_db_tree_cache DEFINITION
  PUBLIC
  CREATE PRIVATE

  GLOBAL FRIENDS zcl_ags_db .

  PUBLIC SECTION.

    METHODS delete_all .
    METHODS insert
      IMPORTING
        !it_data TYPE zags_tree_cache_tt .
    METHODS select
      IMPORTING
        !iv_repo        TYPE zags_tree_cache-repo
        !iv_commit_sha1 TYPE zags_tree_cache-commit_sha1
        !iv_max         TYPE i DEFAULT 0
      RETURNING
        VALUE(rt_data)  TYPE zags_tree_cache_tt
      RAISING
        zcx_ags_error .
    METHODS select_by_path
      IMPORTING
        !iv_repo        TYPE zags_tree_cache-repo
        !iv_commit_sha1 TYPE zags_tree_cache-commit_sha1
        !iv_path        TYPE zags_tree_cache-path
        !iv_max         TYPE i DEFAULT 0
      RETURNING
        VALUE(rt_data)  TYPE zags_tree_cache_tt
      RAISING
        zcx_ags_error .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_cache TYPE zags_tree_cache_tt .
    DATA mv_fake TYPE abap_bool .

    METHODS set_fake .
ENDCLASS.



CLASS ZCL_AGS_DB_TREE_CACHE IMPLEMENTATION.


  METHOD delete_all.

    IF mv_fake = abap_true.
      CLEAR mt_cache.
    ELSE.
      DELETE FROM zags_tree_cache.                        "#EC CI_SUBRC
    ENDIF.

  ENDMETHOD.


  METHOD insert.

    FIELD-SYMBOLS: <ls_data> LIKE LINE OF it_data.

    IF mv_fake = abap_true.
      LOOP AT it_data ASSIGNING <ls_data>.
        INSERT <ls_data> INTO TABLE mt_cache.
        ASSERT sy-subrc = 0.
      ENDLOOP.
    ELSE.
      INSERT zags_tree_cache FROM TABLE it_data.          "#EC CI_SUBRC
    ENDIF.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD select.

    FIELD-SYMBOLS: <ls_cache> LIKE LINE OF mt_cache.


    ASSERT NOT iv_repo IS INITIAL.
    ASSERT NOT iv_commit_sha1 IS INITIAL.

    IF mv_fake = abap_true.
      LOOP AT mt_cache ASSIGNING <ls_cache>
          WHERE repo = iv_repo
          AND commit_sha1 = iv_commit_sha1.
        APPEND <ls_cache> TO rt_data.
        IF iv_max <> 0 AND lines( rt_data ) = iv_max.
          RETURN.
        ENDIF.
      ENDLOOP.
    ELSE.
      SELECT * FROM zags_tree_cache
        INTO TABLE rt_data
        UP TO iv_max ROWS
        WHERE repo = iv_repo
        AND commit_sha1 = iv_commit_sha1.                 "#EC CI_SUBRC
    ENDIF.

  ENDMETHOD.


  METHOD select_by_path.

    FIELD-SYMBOLS: <ls_cache> LIKE LINE OF mt_cache.


    ASSERT NOT iv_repo IS INITIAL.
    ASSERT NOT iv_commit_sha1 IS INITIAL.

    IF mv_fake = abap_true.
      LOOP AT mt_cache ASSIGNING <ls_cache>
          WHERE repo = iv_repo
          AND commit_sha1 = iv_commit_sha1
          AND path = iv_path.
        APPEND <ls_cache> TO rt_data.
        IF iv_max <> 0 AND lines( rt_data ) = iv_max.
          RETURN.
        ENDIF.
      ENDLOOP.
    ELSE.
      SELECT * FROM zags_tree_cache
        INTO TABLE rt_data
        UP TO iv_max ROWS
        WHERE repo = iv_repo
        AND path = iv_path
        AND commit_sha1 = iv_commit_sha1.                 "#EC CI_SUBRC
    ENDIF.

  ENDMETHOD.


  METHOD set_fake.

    mv_fake = abap_true.

  ENDMETHOD.
ENDCLASS.
