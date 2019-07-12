CLASS zcl_ags_db_objects DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_ags_db .

  PUBLIC SECTION.

    METHODS mass
      IMPORTING
        !iv_repo       TYPE zags_objects-repo
        !it_sha1       TYPE zags_sha1_tt
      RETURNING
        VALUE(rt_list) TYPE zags_objects_tt .
    METHODS list
      RETURNING
        VALUE(rt_objects) TYPE zags_objects_tt .
    METHODS list_by_type
      IMPORTING
        !iv_repo          TYPE zags_objects-repo
        !iv_type          TYPE zags_objects-type
      RETURNING
        VALUE(rt_objects) TYPE zags_objects_tt .
    METHODS modify
      IMPORTING
        VALUE(is_data) TYPE zags_objects .
    METHODS single
      IMPORTING
        !iv_repo       TYPE zags_objects-repo
        !iv_sha1       TYPE zags_objects-sha1
      RETURNING
        VALUE(rs_data) TYPE zags_objects
      RAISING
        zcx_ags_error .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_objects TYPE zags_objects_tt .
    DATA mv_fake TYPE abap_bool .

    METHODS set_fake .
ENDCLASS.



CLASS ZCL_AGS_DB_OBJECTS IMPLEMENTATION.


  METHOD list.
* used in migration program, see https://github.com/larshp/abapGitServer/issues/41

    IF mv_fake = abap_true.
      rt_objects = mt_objects.
    ELSE.
      SELECT * FROM zags_objects
        INTO CORRESPONDING FIELDS OF TABLE rt_objects. "#EC CI_SUBRC "#EC CI_NOWHERE
    ENDIF.

  ENDMETHOD.


  METHOD list_by_type.

    ASSERT NOT iv_repo IS INITIAL.

    IF mv_fake = abap_true.
      rt_objects = mt_objects.
      DELETE rt_objects
        WHERE repo <> iv_repo
        OR type <> iv_type.                             "#EC CI_SORTSEQ
    ELSE.
      SELECT * FROM zags_objects
        INTO CORRESPONDING FIELDS OF TABLE rt_objects
        WHERE repo = iv_repo
        AND type = iv_type.                               "#EC CI_SUBRC
    ENDIF.

  ENDMETHOD.


  METHOD mass.

    DATA: ls_data LIKE LINE OF rt_list,
          lv_sha1 LIKE LINE OF it_sha1.

    ASSERT NOT iv_repo IS INITIAL.
    ASSERT lines( it_sha1 ) > 0.


    IF mv_fake = abap_true.
      LOOP AT it_sha1 INTO lv_sha1.
        READ TABLE mt_objects INTO ls_data
          WITH KEY repo = iv_repo
          sha1 = lv_sha1.
        IF sy-subrc = 0.
          APPEND ls_data TO rt_list.
        ENDIF.
      ENDLOOP.
    ELSE.
      SELECT * FROM zags_objects
        INTO TABLE rt_list
        FOR ALL ENTRIES IN it_sha1
        WHERE repo = iv_repo
        AND sha1 = it_sha1-table_line.
    ENDIF.

  ENDMETHOD.


  METHOD modify.

    ASSERT NOT is_data-repo IS INITIAL.
    ASSERT NOT is_data-sha1 IS INITIAL.

    IF mv_fake = abap_true.
      DELETE mt_objects WHERE sha1 = is_data-sha1 AND repo = is_data-repo.
      INSERT is_data INTO TABLE mt_objects.
    ELSE.
      MODIFY zags_objects FROM is_data.                   "#EC CI_SUBRC
    ENDIF.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD set_fake.

    mv_fake = abap_true.

  ENDMETHOD.


  METHOD single.

    ASSERT NOT iv_repo IS INITIAL.

    IF mv_fake = abap_true.
      READ TABLE mt_objects INTO rs_data
        WITH KEY repo = iv_repo
        sha1 = iv_sha1.                                   "#EC CI_SUBRC
    ELSE.
      SELECT SINGLE * FROM zags_objects
        INTO rs_data
        WHERE repo = iv_repo
        AND sha1 = iv_sha1.
    ENDIF.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ags_error
        EXPORTING
          textid = zcx_ags_error=>m005
          sha1   = iv_sha1.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
