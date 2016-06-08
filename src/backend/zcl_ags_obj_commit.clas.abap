CLASS zcl_ags_obj_commit DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_ags_object.

    ALIASES c_newline
      FOR zif_ags_object~c_newline.
    ALIASES deserialize
      FOR zif_ags_object~deserialize.
    ALIASES save
      FOR zif_ags_object~save.
    ALIASES serialize
      FOR zif_ags_object~serialize.
    ALIASES sha1
      FOR zif_ags_object~sha1.

    TYPES:
      BEGIN OF ty_commit,
        tree      TYPE zags_sha1,
        parent    TYPE zags_sha1,
        author    TYPE string,
        committer TYPE string,
        body      TYPE string,
      END OF ty_commit.

    METHODS get_tree
      RETURNING
        VALUE(rv_tree) TYPE ty_commit-tree.
    METHODS get_parent
      RETURNING
        VALUE(rv_parent) TYPE ty_commit-parent.
    METHODS get_author
      RETURNING
        VALUE(rv_author) TYPE ty_commit-author.
    METHODS get_committer
      RETURNING
        VALUE(rv_committer) TYPE ty_commit-committer.
    METHODS get_body
      RETURNING
        VALUE(rv_body) TYPE ty_commit-body.
    METHODS set_tree
      IMPORTING
        !iv_tree TYPE zags_sha1.
    METHODS set_parent
      IMPORTING
        !iv_parent TYPE ty_commit-parent.
    METHODS set_author
      IMPORTING
        !iv_author TYPE ty_commit-author.
    METHODS set_committer
      IMPORTING
        !iv_committer TYPE ty_commit-committer.
    METHODS set_body
      IMPORTING
        !iv_body TYPE ty_commit-body.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA ms_data TYPE ty_commit.
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

    lv_mode = 'tree'.                                       "#EC NOTEXT
    LOOP AT lt_string ASSIGNING <lv_string>.
      lv_len = strlen( lv_mode ).

      IF NOT lv_mode IS INITIAL AND <lv_string>(lv_len) = lv_mode.
        CASE lv_mode.
          WHEN 'tree'.
            lv_char40 = <lv_string>+5.
            TRANSLATE lv_char40 TO UPPER CASE.
            ms_data-tree = lv_char40.
            lv_mode = 'parent'.                             "#EC NOTEXT
          WHEN 'parent'.
            lv_char40 = <lv_string>+7.
            TRANSLATE lv_char40 TO UPPER CASE.
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

    ls_object-sha1 = sha1( ).
    ls_object-type = 'commit' ##NO_TEXT.
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
        iv_type = 'commit'
        iv_data = serialize( ) ) ##NO_TEXT.

  ENDMETHOD.
ENDCLASS.