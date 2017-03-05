class ZCL_AGS_DB_TREE_CACHE definition
  public
  create private

  global friends ZCL_AGS_DB .

public section.

  methods DELETE_ALL .
  methods INSERT
    importing
      !IT_DATA type ZAGS_TREE_CACHE_TT .
  methods SELECT
    importing
      !IV_REPO type ZAGS_TREE_CACHE-REPO
      !IV_COMMIT_SHA1 type ZAGS_TREE_CACHE-COMMIT_SHA1
      !IV_MAX type I default 0
    returning
      value(RT_DATA) type ZAGS_TREE_CACHE_TT
    raising
      ZCX_AGS_ERROR .
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
          WHERE repo = iv_repo AND commit_sha1 = iv_commit_sha1.
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


  METHOD set_fake.

    mv_fake = abap_true.

  ENDMETHOD.
ENDCLASS.
