class ZCL_AGS_OBJ_COMMIT definition
  public
  create private .

public section.

  interfaces ZIF_AGS_OBJECT .

  aliases C_NEWLINE
    for ZIF_AGS_OBJECT~C_NEWLINE .
  aliases DESERIALIZE
    for ZIF_AGS_OBJECT~DESERIALIZE .
  aliases SAVE
    for ZIF_AGS_OBJECT~SAVE .
  aliases SERIALIZE
    for ZIF_AGS_OBJECT~SERIALIZE .
  aliases SHA1
    for ZIF_AGS_OBJECT~GET_SHA1 .
  aliases TYPE
    for ZIF_AGS_OBJECT~GET_TYPE .

  types:
    BEGIN OF ty_userfield,
        name  TYPE string,
        email TYPE string,
        time  TYPE zags_unix_time,
      END OF ty_userfield .
  types:
    BEGIN OF ty_pretty,
        sha1      TYPE zags_sha1,
        tree      TYPE zags_sha1,
        parent    TYPE zags_sha1,
        parent2   TYPE zags_sha1,
        author    TYPE ty_userfield,
        committer TYPE ty_userfield,
        text      TYPE string,
        body      TYPE string,
      END OF ty_pretty .
  types:
    ty_pretty_tt TYPE STANDARD TABLE OF ty_pretty WITH DEFAULT KEY .
  types:
    BEGIN OF ty_commit,
        tree      TYPE zags_sha1,
        parent    TYPE zags_sha1,
        parent2   TYPE zags_sha1,
        author    TYPE string,
        committer TYPE string,
        body      TYPE string,
      END OF ty_commit .
  types:
    ty_commits_tt TYPE STANDARD TABLE OF ty_commit WITH DEFAULT KEY .

  class-methods LOAD
    importing
      !IV_REPO type ZAGS_OBJECTS-REPO
      !IV_SHA1 type ZAGS_OBJECTS-SHA1
    returning
      value(RO_COMMIT) type ref to ZCL_AGS_OBJ_COMMIT
    raising
      ZCX_AGS_ERROR .
  class-methods NEW
    importing
      !IV_REPO type ZAGS_OBJECTS-REPO
    returning
      value(RO_COMMIT) type ref to ZCL_AGS_OBJ_COMMIT
    raising
      ZCX_AGS_ERROR .
  methods GET
    returning
      value(RS_DATA) type TY_COMMIT .
  methods GET_PRETTY
    returning
      value(RS_DATA) type TY_PRETTY
    raising
      ZCX_AGS_ERROR .
  methods SET_AUTHOR
    importing
      !IV_AUTHOR type TY_COMMIT-AUTHOR
    raising
      ZCX_AGS_ERROR .
  methods SET_BODY
    importing
      !IV_BODY type TY_COMMIT-BODY .
  methods SET_COMMITTER
    importing
      !IV_COMMITTER type TY_COMMIT-COMMITTER
    raising
      ZCX_AGS_ERROR .
  methods SET_PARENT
    importing
      !IV_PARENT type TY_COMMIT-PARENT .
  methods SET_PARENT2
    importing
      !IV_PARENT type TY_COMMIT-PARENT .
  methods SET_TREE
    importing
      !IV_TREE type ZAGS_SHA1 .
  PROTECTED SECTION.
private section.

  data MS_DATA type TY_COMMIT .
  data MV_NEW type ABAP_BOOL .
  data MV_SHA1 type ZAGS_SHA1 .
  data MV_REPO type ZAGS_OBJECTS-REPO .

  methods PARSE_USERFIELD
    importing
      !IV_FIELD type STRING
    returning
      value(RS_USERFIELD) type TY_USERFIELD
    raising
      ZCX_AGS_ERROR .
ENDCLASS.



CLASS ZCL_AGS_OBJ_COMMIT IMPLEMENTATION.


  METHOD get.

    rs_data = ms_data.

  ENDMETHOD.


  METHOD get_pretty.

    DATA: ls_data TYPE ty_commit,
          lt_body TYPE TABLE OF string,
          lv_body LIKE LINE OF lt_body.


    ls_data = get( ).

    IF mv_sha1 IS INITIAL.
      ASSERT mv_new = abap_true.
      rs_data-sha1    = sha1( ).
    ELSE.
      rs_data-sha1    = mv_sha1.
    ENDIF.

    rs_data-tree      = ls_data-tree.
    rs_data-parent    = ls_data-parent.
    rs_data-parent2   = ls_data-parent2.
    rs_data-author    = parse_userfield( ls_data-author ).
    rs_data-committer = parse_userfield( ls_data-committer ).

    SPLIT ls_data-body AT cl_abap_char_utilities=>newline INTO TABLE lt_body.

    READ TABLE lt_body INDEX 1 INTO lv_body.              "#EC CI_SUBRC
    rs_data-text = lv_body.

    DELETE lt_body INDEX 1.
    CONCATENATE LINES OF lt_body
      INTO rs_data-body
      SEPARATED BY cl_abap_char_utilities=>newline.

  ENDMETHOD.


  METHOD load.

    CREATE OBJECT ro_commit.
    ro_commit->mv_new = abap_false.
    ro_commit->mv_sha1 = iv_sha1.
    ro_commit->deserialize( zcl_ags_db=>get_objects( )->single( iv_repo = iv_repo iv_sha1 = iv_sha1 )-data_raw ).

  ENDMETHOD.


  METHOD new.

    ASSERT NOT iv_repo IS INITIAL.

    CREATE OBJECT ro_commit.
    ro_commit->mv_new = abap_true.
    ro_commit->mv_repo = iv_repo.

  ENDMETHOD.


  METHOD parse_userfield.

    DATA: lv_time TYPE string.

    FIND REGEX '^([\w\*\.\(\) ]+) <(.*)> (\d{10}) .\d{4}$' IN iv_field
      SUBMATCHES
      rs_userfield-name
      rs_userfield-email
      lv_time ##NO_TEXT.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ags_error
        EXPORTING
          textid = zcx_ags_error=>m012.
    ENDIF.

    rs_userfield-time = lv_time.

  ENDMETHOD.


  METHOD set_author.

    ASSERT mv_new = abap_true.

    parse_userfield( iv_author ).

    ms_data-author = iv_author.

  ENDMETHOD.


  METHOD set_body.

    ASSERT mv_new = abap_true.

    ms_data-body = iv_body.

  ENDMETHOD.


  METHOD set_committer.

    ASSERT mv_new = abap_true.

    parse_userfield( iv_committer ).

    ms_data-committer = iv_committer.

  ENDMETHOD.


  METHOD set_parent.

    ASSERT mv_new = abap_true.

    ms_data-parent = iv_parent.

  ENDMETHOD.


  METHOD set_parent2.

    ASSERT mv_new = abap_true.

    ms_data-parent2 = iv_parent.

  ENDMETHOD.


  METHOD set_tree.

    ASSERT mv_new = abap_true.

    ms_data-tree = iv_tree.

  ENDMETHOD.


  METHOD zif_ags_object~deserialize.

    ms_data = zcl_abapgit_git_pack=>decode_commit( iv_data ).

  ENDMETHOD.


  METHOD ZIF_AGS_OBJECT~GET_SHA1.

    rv_sha1 = zcl_ags_util=>sha1(
        iv_type = zif_ags_constants=>c_type-commit
        iv_data = serialize( ) ) ##NO_TEXT.

  ENDMETHOD.


  METHOD ZIF_AGS_OBJECT~GET_TYPE.
    rv_type = zif_ags_constants=>c_type-commit.
  ENDMETHOD.


  METHOD zif_ags_object~save.

    DATA: ls_object TYPE zags_objects.

    ASSERT mv_new = abap_true.

    ls_object-repo = mv_repo.
    ls_object-sha1 = sha1( ).
    ls_object-type = zif_ags_constants=>c_type-commit.
    ls_object-data_raw = serialize( ).

    zcl_ags_db=>get_objects( )->modify( ls_object ).

  ENDMETHOD.


  METHOD zif_ags_object~serialize.

    ASSERT NOT ms_data-tree IS INITIAL.
    ASSERT NOT ms_data-author IS INITIAL.
    ASSERT NOT ms_data-committer IS INITIAL.

    rv_data = zcl_abapgit_git_pack=>encode_commit( ms_data ).

  ENDMETHOD.
ENDCLASS.
