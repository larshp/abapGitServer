class ZCL_AGS_OBJ_COMMIT definition
  public
  create public .

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
    for ZIF_AGS_OBJECT~SHA1 .

  types:
    BEGIN OF ty_commit,
        tree      TYPE zags_sha1,
        parent    TYPE zags_sha1,
        author    TYPE string,
        committer TYPE string,
        body      TYPE string,
      END OF ty_commit .
  types:
    ty_commits_tt TYPE STANDARD TABLE OF ty_commit WITH DEFAULT KEY .

  methods GET
    returning
      value(RS_DATA) type TY_COMMIT .
  methods SET_TREE
    importing
      !IV_TREE type ZAGS_SHA1 .
  methods SET_PARENT
    importing
      !IV_PARENT type TY_COMMIT-PARENT .
  methods SET_AUTHOR
    importing
      !IV_AUTHOR type TY_COMMIT-AUTHOR
    raising
      ZCX_AGS_ERROR .
  methods SET_COMMITTER
    importing
      !IV_COMMITTER type TY_COMMIT-COMMITTER
    raising
      ZCX_AGS_ERROR .
  methods SET_BODY
    importing
      !IV_BODY type TY_COMMIT-BODY .
  methods CONSTRUCTOR
    importing
      !IV_SHA1 type ZAGS_SHA1 optional
    raising
      ZCX_AGS_ERROR .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA ms_data TYPE ty_commit .
    DATA mv_new TYPE abap_bool .

    METHODS validate_userfield
      IMPORTING
        !iv_data TYPE string
      RAISING
        zcx_ags_error .
ENDCLASS.



CLASS ZCL_AGS_OBJ_COMMIT IMPLEMENTATION.


  METHOD constructor.

    IF iv_sha1 IS INITIAL.
      mv_new = abap_true.
    ELSE.
      mv_new = abap_false.
      deserialize( zcl_ags_lookup=>read_object( iv_sha1 )-data ).
    ENDIF.

  ENDMETHOD.


  METHOD get.

    rs_data = ms_data.

  ENDMETHOD.


  METHOD set_author.

    validate_userfield( iv_author ).

    ms_data-author = iv_author.

  ENDMETHOD.


  METHOD set_body.

    ms_data-body = iv_body.

  ENDMETHOD.


  METHOD set_committer.

    validate_userfield( iv_committer ).

    ms_data-committer = iv_committer.

  ENDMETHOD.


  METHOD set_parent.

    ms_data-parent = iv_parent.

  ENDMETHOD.


  METHOD set_tree.

    ms_data-tree = iv_tree.

  ENDMETHOD.


  METHOD validate_userfield.

    FIND REGEX '^\w+ <.*> \d{10} .\d{4}$' IN iv_data.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ags_error
        EXPORTING
          textid = zcx_ags_error=>m012.
    ENDIF.

  ENDMETHOD.


  METHOD zif_ags_object~deserialize.

    DATA: lv_string TYPE string,
          lv_char40 TYPE c LENGTH 40,
          lv_mode   TYPE string,
          lv_len    TYPE i,
          lt_string TYPE TABLE OF string.


    lv_string = zcl_ags_util=>xstring_to_string_utf8( iv_data ).

    SPLIT lv_string AT c_newline INTO TABLE lt_string.

    lv_mode = 'tree'.                                       "#EC NOTEXT
    LOOP AT lt_string ASSIGNING FIELD-SYMBOL(<lv_string>).
      lv_len = strlen( lv_mode ).

      IF NOT lv_mode IS INITIAL AND <lv_string>(lv_len) = lv_mode.
        CASE lv_mode.
          WHEN 'tree'.
            lv_char40 = <lv_string>+5.
            ms_data-tree = lv_char40.
            lv_mode = 'parent'.                             "#EC NOTEXT
          WHEN 'parent'.
            lv_char40 = <lv_string>+7.
            ms_data-parent = lv_char40.
            lv_mode = 'author'.                             "#EC NOTEXT
          WHEN 'author'.
            ms_data-author = <lv_string>+7.
            lv_mode = 'committer'.                          "#EC NOTEXT
          WHEN 'committer'.
            ms_data-committer = <lv_string>+10.
            CLEAR lv_mode.
        ENDCASE.
      ELSEIF lv_mode = 'parent' AND <lv_string>(6) = 'author'. "#EC NOTEXT
* first commit doesnt have parent
        ms_data-author = <lv_string>+7.
        lv_mode = 'committer'.                              "#EC NOTEXT
      ELSE.
* body
        CONCATENATE ms_data-body <lv_string> INTO ms_data-body
          SEPARATED BY c_newline.
      ENDIF.
    ENDLOOP.

* strip first newline
    IF strlen( ms_data-body ) >= 2.
      ms_data-body = ms_data-body+2.
    ENDIF.

    IF ms_data-author IS INITIAL
        OR ms_data-committer IS INITIAL
        OR ms_data-tree IS INITIAL.
* multiple parents? not supported
      ASSERT 1 = 2.
    ENDIF.

  ENDMETHOD.


  METHOD zif_ags_object~save.

    DATA: ls_object TYPE zags_objects.

    ASSERT mv_new = abap_true.

    ls_object-sha1 = sha1( ).
    ls_object-type = zif_ags_constants=>c_type-commit.
    ls_object-data = serialize( ).

    MODIFY zags_objects FROM ls_object.

  ENDMETHOD.


  METHOD zif_ags_object~serialize.

    DATA: lv_string       TYPE string,
          lv_tmp          TYPE string,
          lv_tree_lower   TYPE string,
          lv_parent_lower TYPE string.



    lv_tree_lower = ms_data-tree.
    TRANSLATE lv_tree_lower TO LOWER CASE.

    lv_parent_lower = ms_data-parent.
    TRANSLATE lv_parent_lower TO LOWER CASE.

    lv_string = ''.

    CONCATENATE 'tree' lv_tree_lower INTO lv_tmp SEPARATED BY space. "#EC NOTEXT
    CONCATENATE lv_string lv_tmp c_newline INTO lv_string.

    IF NOT ms_data-parent IS INITIAL.
      CONCATENATE 'parent' lv_parent_lower
        INTO lv_tmp SEPARATED BY space.                     "#EC NOTEXT
      CONCATENATE lv_string lv_tmp c_newline INTO lv_string.
    ENDIF.

    CONCATENATE 'author' ms_data-author
      INTO lv_tmp SEPARATED BY space.                       "#EC NOTEXT
    CONCATENATE lv_string lv_tmp c_newline INTO lv_string.

    CONCATENATE 'committer' ms_data-committer
      INTO lv_tmp SEPARATED BY space.                       "#EC NOTEXT
    CONCATENATE lv_string lv_tmp c_newline INTO lv_string.

    CONCATENATE lv_string c_newline ms_data-body INTO lv_string.

    rv_data = zcl_ags_util=>string_to_xstring_utf8( lv_string ).

  ENDMETHOD.


  METHOD zif_ags_object~sha1.

    rv_sha1 = zcl_ags_util=>sha1(
        iv_type = zif_ags_constants=>c_type-commit
        iv_data = serialize( ) ) ##NO_TEXT.

  ENDMETHOD.


  METHOD zif_ags_object~type.
    rv_type = zif_ags_constants=>c_type-commit.
  ENDMETHOD.
ENDCLASS.