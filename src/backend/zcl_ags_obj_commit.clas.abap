class ZCL_AGS_OBJ_COMMIT definition
  public
  create public .

public section.

  interfaces ZIF_AGS_OBJECT .

  aliases C_NEWLINE
    for ZIF_AGS_OBJECT~C_NEWLINE .
  aliases DESERIALIZE
    for ZIF_AGS_OBJECT~DESERIALIZE .
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

  methods GET_TREE
    returning
      value(RV_TREE) type TY_COMMIT-TREE .
  methods GET_PARENT
    returning
      value(RV_PARENT) type TY_COMMIT-PARENT .
  methods GET_AUTHOR
    returning
      value(RV_AUTHOR) type TY_COMMIT-AUTHOR .
  methods GET_COMMITTER
    returning
      value(RV_COMMITTER) type TY_COMMIT-COMMITTER .
  methods GET_BODY
    returning
      value(RV_BODY) type TY_COMMIT-BODY .
  methods SET_TREE
    importing
      !IV_TREE type ZAGS_SHA1 .
  methods SET_PARENT
    importing
      !IV_PARENT type TY_COMMIT-PARENT .
  methods SET_AUTHOR
    importing
      !IV_AUTHOR type TY_COMMIT-AUTHOR .
  methods SET_COMMITTER
    importing
      !IV_COMMITTER type TY_COMMIT-COMMITTER .
  methods SET_BODY
    importing
      !IV_BODY type TY_COMMIT-BODY .
protected section.
private section.

  data MS_DATA type TY_COMMIT .
ENDCLASS.



CLASS ZCL_AGS_OBJ_COMMIT IMPLEMENTATION.


METHOD get_author.

  rv_author = ms_data-author.

ENDMETHOD.


METHOD get_body.

  rv_body = ms_data-body.

ENDMETHOD.


METHOD get_committer.

  rv_committer = ms_data-committer.

ENDMETHOD.


METHOD get_parent.

  rv_parent = ms_data-parent.

ENDMETHOD.


METHOD get_tree.

  rv_tree = ms_data-tree.

ENDMETHOD.


METHOD set_author.

  ms_data-author = iv_author.

ENDMETHOD.


METHOD set_body.

  ms_data-body = iv_body.

ENDMETHOD.


METHOD set_committer.

  ms_data-committer = iv_committer.

ENDMETHOD.


METHOD set_parent.

  ms_data-parent = iv_parent.

ENDMETHOD.


METHOD set_tree.

  ms_data-tree = iv_tree.

ENDMETHOD.


METHOD zif_ags_object~deserialize.

  DATA: lv_string TYPE string,
        lv_char40 TYPE c LENGTH 40,
        lv_mode   TYPE string,
        lv_len    TYPE i,
        lt_string TYPE TABLE OF string.

  FIELD-SYMBOLS: <lv_string> LIKE LINE OF lt_string.


  lv_string = zcl_ags_util=>xstring_to_string_utf8( iv_data ).

  SPLIT lv_string AT c_newline INTO TABLE lt_string.

  lv_mode = 'tree'.                                         "#EC NOTEXT
  LOOP AT lt_string ASSIGNING <lv_string>.
    lv_len = strlen( lv_mode ).

    IF NOT lv_mode IS INITIAL AND <lv_string>(lv_len) = lv_mode.
      CASE lv_mode.
        WHEN 'tree'.
          lv_char40 = <lv_string>+5.
          TRANSLATE lv_char40 TO UPPER CASE.
          ms_data-tree = lv_char40.
          lv_mode = 'parent'.                               "#EC NOTEXT
        WHEN 'parent'.
          lv_char40 = <lv_string>+7.
          TRANSLATE lv_char40 TO UPPER CASE.
          ms_data-parent = lv_char40.
          lv_mode = 'author'.                               "#EC NOTEXT
        WHEN 'author'.
          ms_data-author = <lv_string>+7.
          lv_mode = 'committer'.                            "#EC NOTEXT
        WHEN 'committer'.
          ms_data-committer = <lv_string>+10.
          CLEAR lv_mode.
      ENDCASE.
    ELSEIF lv_mode = 'parent' AND <lv_string>(6) = 'author'. "#EC NOTEXT
* first commit doesnt have parent
      ms_data-author = <lv_string>+7.
      lv_mode = 'committer'.                                "#EC NOTEXT
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
      INTO lv_tmp SEPARATED BY space.                       "#EC NOTEXT
    CONCATENATE lv_string lv_tmp c_newline INTO lv_string.
  ENDIF.

  CONCATENATE 'author' ms_data-author
    INTO lv_tmp SEPARATED BY space.                         "#EC NOTEXT
  CONCATENATE lv_string lv_tmp c_newline INTO lv_string.

  CONCATENATE 'committer' ms_data-committer
    INTO lv_tmp SEPARATED BY space.                         "#EC NOTEXT
  CONCATENATE lv_string lv_tmp c_newline INTO lv_string.

  CONCATENATE lv_string c_newline ms_data-body INTO lv_string.

  rv_data = zcl_ags_util=>string_to_xstring_utf8( lv_string ).

ENDMETHOD.


METHOD zif_ags_object~sha1.

  zcl_ags_util=>sha1(
      iv_type = 'commit'
      iv_data = serialize( ) ).

ENDMETHOD.
ENDCLASS.