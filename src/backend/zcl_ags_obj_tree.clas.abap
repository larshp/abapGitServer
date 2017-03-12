CLASS zcl_ags_obj_tree DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_ags_object .

    ALIASES c_newline
      FOR zif_ags_object~c_newline .
    ALIASES deserialize
      FOR zif_ags_object~deserialize .
    ALIASES save
      FOR zif_ags_object~save .
    ALIASES serialize
      FOR zif_ags_object~serialize .
    ALIASES sha1
      FOR zif_ags_object~sha1 .

    TYPES:
      BEGIN OF ty_tree,
        chmod TYPE zags_chmod,
        name  TYPE string,
        sha1  TYPE zags_sha1,
      END OF ty_tree .
    TYPES:
      ty_tree_tt TYPE STANDARD TABLE OF ty_tree WITH DEFAULT KEY .

    CONSTANTS:
      BEGIN OF c_chmod,
        file       TYPE zags_chmod VALUE '100644',
        executable TYPE zags_chmod VALUE '100755',
        dir        TYPE zags_chmod VALUE '40000',
      END OF c_chmod .

    CLASS-METHODS get_instance
      IMPORTING
        !iv_repo       TYPE zags_objects-repo
        !iv_sha1       TYPE zags_objects-sha1
      RETURNING
        VALUE(ro_tree) TYPE REF TO zcl_ags_obj_tree
      RAISING
        zcx_ags_error .
    METHODS add_file
      IMPORTING
        !iv_chmod TYPE zags_chmod
        !iv_name  TYPE ty_tree-name
        !iv_sha1  TYPE ty_tree-sha1 .
    METHODS constructor
      IMPORTING
        !iv_repo TYPE zags_objects-repo
        !iv_sha1 TYPE zags_objects-sha1 OPTIONAL
      RAISING
        zcx_ags_error .
    METHODS get_files
      RETURNING
        VALUE(rt_files) TYPE ty_tree_tt .
    METHODS set_files
      IMPORTING
        VALUE(it_files) TYPE ty_tree_tt .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_data TYPE ty_tree_tt .
    DATA mv_new TYPE abap_bool .
    DATA mv_repo TYPE zags_objects-repo .
ENDCLASS.



CLASS ZCL_AGS_OBJ_TREE IMPLEMENTATION.


  METHOD add_file.
* todo, rename method to "ADD"? as it can also be used to add folders/directories

    FIELD-SYMBOLS: <ls_data> LIKE LINE OF mt_data.

    ASSERT mv_new = abap_true.

    READ TABLE mt_data WITH KEY
      chmod = iv_chmod
      name = iv_name
      sha1 = iv_sha1
      TRANSPORTING NO FIELDS.
    ASSERT sy-subrc <> 0.

    APPEND INITIAL LINE TO mt_data ASSIGNING <ls_data>.
    <ls_data>-chmod = iv_chmod.
    <ls_data>-name = iv_name.
    <ls_data>-sha1 = iv_sha1.

  ENDMETHOD.


  METHOD constructor.

    ASSERT NOT iv_repo IS INITIAL.

    IF iv_sha1 IS INITIAL.
      mv_new = abap_true.
      mv_repo = iv_repo.
    ELSE.
      mv_new = abap_false.
      deserialize( zcl_ags_db=>get_objects( )->single(
        iv_repo = iv_repo
        iv_sha1 = iv_sha1 )-data_raw ).
    ENDIF.

  ENDMETHOD.


  METHOD get_files.

    rt_files = mt_data.

  ENDMETHOD.


  METHOD get_instance.

    CREATE OBJECT ro_tree
      EXPORTING
        iv_repo = iv_repo
        iv_sha1 = iv_sha1.

  ENDMETHOD.


  METHOD set_files.

    ASSERT mv_new = abap_true.
    ASSERT lines( mt_data ) = 0.

    mt_data = it_files.

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

    ls_object-repo = mv_repo.
    ls_object-sha1 = sha1( ).
    ls_object-type = zif_ags_constants=>c_type-tree.
    ls_object-data_raw = serialize( ).

    zcl_ags_db=>get_objects( )->modify( ls_object ).

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
