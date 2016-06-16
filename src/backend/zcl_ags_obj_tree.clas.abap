CLASS zcl_ags_obj_tree DEFINITION
  PUBLIC
  FINAL
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
      ty_chmod TYPE c LENGTH 6.
    TYPES:
      BEGIN OF ty_tree,
        chmod TYPE string,
        name  TYPE string,
        sha1  TYPE zags_sha1,
      END OF ty_tree.
    TYPES:
      ty_tree_tt TYPE STANDARD TABLE OF ty_tree WITH DEFAULT KEY.

    CONSTANTS:
      BEGIN OF c_chmod,
        file       TYPE ty_chmod VALUE '100644',
        executable TYPE ty_chmod VALUE '100755',
        dir        TYPE ty_chmod VALUE '40000',
      END OF c_chmod.

    METHODS add_file
      IMPORTING
        !iv_chmod TYPE ty_chmod
        !iv_name  TYPE ty_tree-name
        !iv_sha1  TYPE ty_tree-sha1.
    METHODS get_files
      RETURNING
        VALUE(rt_files) TYPE ty_tree_tt.
    METHODS constructor
      IMPORTING
        !iv_sha1 TYPE zags_sha1 OPTIONAL
      RAISING
        zcx_ags_error.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_data TYPE ty_tree_tt.
    DATA mv_new TYPE abap_bool.
ENDCLASS.



CLASS ZCL_AGS_OBJ_TREE IMPLEMENTATION.


  METHOD add_file.

    APPEND INITIAL LINE TO mt_data ASSIGNING FIELD-SYMBOL(<ls_data>).
    <ls_data>-chmod = iv_chmod.
    <ls_data>-name = iv_name.
    <ls_data>-sha1 = iv_sha1.

  ENDMETHOD.


  METHOD constructor.

    IF iv_sha1 IS INITIAL.
      mv_new = abap_true.
    ELSE.
      mv_new = abap_false.
      deserialize( zcl_ags_lookup=>read_object( iv_sha1 )-data ).
    ENDIF.

  ENDMETHOD.


  METHOD get_files.

    rt_files = mt_data.

  ENDMETHOD.


  METHOD zif_ags_object~deserialize.

    CONSTANTS: lc_sha_length TYPE i VALUE 20,
               lc_null       TYPE x VALUE '00'.

    DATA: lv_xstring TYPE xstring,
          lv_chmod   TYPE ty_chmod,
          lv_name    TYPE string,
          lv_string  TYPE string,
          lv_len     TYPE i,
          lv_offset  TYPE i,
          lv_cursor  TYPE i,
          ls_node    LIKE LINE OF mt_data,
          lv_start   TYPE i.


    DO.
      IF lv_cursor >= xstrlen( iv_data ).
        EXIT. " current loop
      ENDIF.

      IF iv_data+lv_cursor(1) = lc_null.
        lv_len = lv_cursor - lv_start.
        lv_xstring = iv_data+lv_start(lv_len).

        lv_string = zcl_ags_util=>xstring_to_string_utf8( lv_xstring ).
        SPLIT lv_string AT space INTO lv_chmod lv_name.

        lv_offset = lv_cursor + 1.

        CLEAR ls_node.
        ls_node-chmod = lv_chmod.
        IF ls_node-chmod <> c_chmod-dir
            AND ls_node-chmod <> c_chmod-file
            AND ls_node-chmod <> c_chmod-executable.
          RAISE EXCEPTION TYPE zcx_ags_error
            EXPORTING
              textid = zcx_ags_error=>m007.
        ENDIF.

        ls_node-name = lv_name.
        ls_node-sha1 = iv_data+lv_offset(lc_sha_length).
        TRANSLATE ls_node-sha1 TO LOWER CASE.
        APPEND ls_node TO mt_data.

        lv_start = lv_cursor + 1 + lc_sha_length.
        lv_cursor = lv_start.
      ELSE.
        lv_cursor = lv_cursor + 1.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD zif_ags_object~save.

    DATA: ls_object TYPE zags_objects.

    ASSERT mv_new = abap_true.

    ls_object-sha1 = sha1( ).
    ls_object-type = zif_ags_constants=>c_type-tree.
    ls_object-data = serialize( ).

    MODIFY zags_objects FROM ls_object.

  ENDMETHOD.


  METHOD zif_ags_object~serialize.

    CONSTANTS: lc_null TYPE x VALUE '00'.

    DATA: lv_string  TYPE string,
          lv_hex20   TYPE x LENGTH 20,
          lv_xstring TYPE xstring.

* todo, sort tree

    LOOP AT mt_data ASSIGNING FIELD-SYMBOL(<ls_data>).
      ASSERT NOT <ls_data>-chmod IS INITIAL.
      ASSERT NOT <ls_data>-name IS INITIAL.

      CONCATENATE <ls_data>-chmod <ls_data>-name INTO lv_string SEPARATED BY space.
      lv_xstring = zcl_ags_util=>string_to_xstring_utf8( lv_string ).

      lv_hex20 = to_upper( <ls_data>-sha1 ).
      CONCATENATE rv_data lv_xstring lc_null lv_hex20 INTO rv_data IN BYTE MODE.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_ags_object~sha1.

    rv_sha1 = zcl_ags_util=>sha1(
        iv_type = zif_ags_constants=>c_type-tree
        iv_data = serialize( ) ) ##NO_TEXT.

  ENDMETHOD.


  METHOD zif_ags_object~type.
    rv_type = zif_ags_constants=>c_type-tree.
  ENDMETHOD.
ENDCLASS.