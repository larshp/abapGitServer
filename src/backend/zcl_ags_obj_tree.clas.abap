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
        chmod TYPE ty_chmod,
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

    FIELD-SYMBOLS: <ls_data> LIKE LINE OF mt_data.


    APPEND INITIAL LINE TO mt_data ASSIGNING <ls_data>.
    <ls_data>-chmod = iv_chmod.
    <ls_data>-name = iv_name.
    <ls_data>-sha1 = iv_sha1.

  ENDMETHOD.


  METHOD constructor.

    IF iv_sha1 IS INITIAL.
      mv_new = abap_true.
    ELSE.
      mv_new = abap_false.
      deserialize( zcl_ags_lookup=>read_object( iv_sha1 )-data_raw ).
    ENDIF.

  ENDMETHOD.


  METHOD get_files.

    rt_files = mt_data.

  ENDMETHOD.


  METHOD zif_ags_object~deserialize.

    CALL METHOD ('\PROGRAM=ZABAPGIT\CLASS=LCL_GIT_PACK')=>decode_tree
      EXPORTING
        iv_data  = iv_data
      RECEIVING
        rt_nodes = mt_data.

  ENDMETHOD.


  METHOD zif_ags_object~save.

    DATA: ls_object TYPE zags_objects.

    ASSERT mv_new = abap_true.

    ls_object-sha1 = sha1( ).
    ls_object-type = zif_ags_constants=>c_type-tree.
    ls_object-data_raw = serialize( ).

    MODIFY zags_objects FROM ls_object.

  ENDMETHOD.


  METHOD zif_ags_object~serialize.

    CALL METHOD ('\PROGRAM=ZABAPGIT\CLASS=LCL_GIT_PACK')=>encode_tree
      EXPORTING
        it_nodes = mt_data
      RECEIVING
        rv_data  = rv_data.

  ENDMETHOD.


  METHOD zif_ags_object~sha1.

    rv_sha1 = zcl_ags_util=>sha1(
      iv_type = zif_ags_constants=>c_type-tree
      iv_data = serialize( ) ).

  ENDMETHOD.


  METHOD zif_ags_object~type.
    rv_type = zif_ags_constants=>c_type-tree.
  ENDMETHOD.
ENDCLASS.
