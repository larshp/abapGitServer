CLASS zcl_ags_obj_commit DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_ags_object .

    ALIASES deserialize
      FOR zif_ags_object~deserialize .
    ALIASES get_adler32
      FOR zif_ags_object~get_adler32 .
    ALIASES get_sha1
      FOR zif_ags_object~get_sha1 .
    ALIASES get_type
      FOR zif_ags_object~get_type .
    ALIASES save
      FOR zif_ags_object~save .
    ALIASES serialize
      FOR zif_ags_object~serialize .

    TYPES:
      BEGIN OF ty_userfield,
        name  TYPE string,
        email TYPE string,
        time  TYPE zags_unix_time,
      END OF ty_userfield .
    TYPES:
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
    TYPES:
      ty_pretty_tt TYPE STANDARD TABLE OF ty_pretty WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_commit,
        tree      TYPE zags_sha1,
        parent    TYPE zags_sha1,
        parent2   TYPE zags_sha1,
        author    TYPE string,
        committer TYPE string,
        body      TYPE string,
      END OF ty_commit .
    TYPES:
      ty_commits_tt TYPE STANDARD TABLE OF ty_commit WITH DEFAULT KEY .

    CLASS-METHODS load
      IMPORTING
        !iv_repo         TYPE zags_objects-repo
        !iv_sha1         TYPE zags_objects-sha1
      RETURNING
        VALUE(ro_commit) TYPE REF TO zcl_ags_obj_commit
      RAISING
        zcx_ags_error .
    CLASS-METHODS new
      IMPORTING
        !iv_repo         TYPE zags_objects-repo
      RETURNING
        VALUE(ro_commit) TYPE REF TO zcl_ags_obj_commit
      RAISING
        zcx_ags_error .
    METHODS get
      RETURNING
        VALUE(rs_data) TYPE ty_commit .
    METHODS get_pretty
      RETURNING
        VALUE(rs_data) TYPE ty_pretty
      RAISING
        zcx_ags_error .
    METHODS set_author
      IMPORTING
        !iv_author TYPE ty_commit-author
      RAISING
        zcx_ags_error .
    METHODS set_body
      IMPORTING
        !iv_body TYPE ty_commit-body .
    METHODS set_committer
      IMPORTING
        !iv_committer TYPE ty_commit-committer
      RAISING
        zcx_ags_error .
    METHODS set_parent
      IMPORTING
        !iv_parent TYPE ty_commit-parent .
    METHODS set_parent2
      IMPORTING
        !iv_parent TYPE ty_commit-parent .
    METHODS set_tree
      IMPORTING
        !iv_tree TYPE zags_sha1 .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA ms_data TYPE ty_commit .
    DATA mv_new TYPE abap_bool .
    DATA mv_sha1 TYPE zags_sha1 .
    DATA mv_repo TYPE zags_objects-repo .
    DATA mv_adler32 TYPE zags_adler32 .

    METHODS parse_userfield
      IMPORTING
        !iv_field           TYPE string
      RETURNING
        VALUE(rs_userfield) TYPE ty_userfield
      RAISING
        zcx_ags_error .
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

    rs_data-sha1      = get_sha1( ).
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

    DATA: ls_data TYPE zags_objects.

    CREATE OBJECT ro_commit.
    ro_commit->mv_new = abap_false.
    ro_commit->mv_sha1 = iv_sha1.

    ls_data = zcl_ags_db=>get_objects( )->single(
      iv_repo = iv_repo
      iv_sha1 = iv_sha1 ).

    ro_commit->deserialize(
      iv_data    = ls_data-data_raw
      iv_adler32 = ls_data-adler32 ).

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

    TRY.
        ms_data = zcl_abapgit_git_pack=>decode_commit( iv_data ).
      CATCH zcx_abapgit_exception.
        ASSERT 0 = 1.
    ENDTRY.

    mv_adler32 = iv_adler32.

  ENDMETHOD.


  METHOD zif_ags_object~get_adler32.

    IF mv_adler32 IS INITIAL.
      rv_adler32 = zcl_abapgit_hash=>adler32( serialize( ) ).
    ELSE.
      ASSERT NOT mv_adler32 IS INITIAL.
      rv_adler32 = mv_adler32.
    ENDIF.

  ENDMETHOD.


  METHOD zif_ags_object~get_sha1.

    IF mv_new = abap_true.
      rv_sha1 = zcl_ags_util=>sha1(
        iv_type = zif_ags_constants=>c_type-commit
        iv_data = serialize( ) ).
    ELSE.
      ASSERT NOT mv_sha1 IS INITIAL.
      rv_sha1 = mv_sha1.
    ENDIF.

  ENDMETHOD.


  METHOD zif_ags_object~get_type.
    rv_type = zif_ags_constants=>c_type-commit.
  ENDMETHOD.


  METHOD zif_ags_object~save.

    DATA: ls_object TYPE zags_objects.


    ASSERT mv_new = abap_true.

    ls_object-repo = mv_repo.
    ls_object-sha1 = get_sha1( ).
    ls_object-type = get_type( ).
    ls_object-data_raw = serialize( ).
    ls_object-adler32 = get_adler32( ).

    zcl_ags_db=>get_objects( )->modify( ls_object ).

  ENDMETHOD.


  METHOD zif_ags_object~serialize.

    ASSERT NOT ms_data-tree IS INITIAL.
    ASSERT NOT ms_data-author IS INITIAL.
    ASSERT NOT ms_data-committer IS INITIAL.

    rv_data = zcl_abapgit_git_pack=>encode_commit( ms_data ).

  ENDMETHOD.
ENDCLASS.
