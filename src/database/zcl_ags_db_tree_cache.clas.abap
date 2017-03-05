CLASS zcl_ags_db_tree_cache DEFINITION
  PUBLIC
  CREATE PRIVATE

  GLOBAL FRIENDS zcl_ags_db .

  PUBLIC SECTION.

    METHODS select
      IMPORTING
        !iv_repo        TYPE zags_tree_cache-repo
        !iv_commit_sha1 TYPE zags_tree_cache-commit_sha1
      RETURNING
        VALUE(rt_data)  TYPE zags_tree_cache_tt
      RAISING
        zcx_ags_error .
    METHODS insert
      IMPORTING
        !it_data TYPE zags_tree_cache_tt .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_cache TYPE zags_tree_cache_tt .
    DATA mv_fake TYPE abap_bool .

    METHODS set_fake .
ENDCLASS.



CLASS ZCL_AGS_DB_TREE_CACHE IMPLEMENTATION.


  METHOD insert.

    FIELD-SYMBOLS: <ls_data> LIKE LINE OF it_data.

    IF mv_fake = abap_true.
      LOOP AT it_data ASSIGNING <ls_data>.
        INSERT <ls_data> INTO TABLE mt_cache.
      ENDLOOP.
    ELSE.
      INSERT zags_tree_cache FROM TABLE it_data.          "#EC CI_SUBRC
    ENDIF.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD select.

    FIELD-SYMBOLS: <ls_cache> LIKE LINE OF mt_cache.

    IF mv_fake = abap_true.
      LOOP AT mt_cache ASSIGNING <ls_cache>
          WHERE repo = iv_repo AND commit_sha1 = iv_commit_sha1.
        APPEND <ls_cache> TO rt_data.
      ENDLOOP.
    ELSE.
      SELECT * FROM zags_tree_cache
        INTO TABLE rt_data
        WHERE repo = iv_repo
        AND commit_sha1 = iv_commit_sha1.
    ENDIF.

  ENDMETHOD.


  METHOD set_fake.

    mv_fake = abap_true.

  ENDMETHOD.
ENDCLASS.
