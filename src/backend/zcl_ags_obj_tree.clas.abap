CLASS zcl_ags_obj_tree DEFINITION
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

    CLASS-METHODS load
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
    CLASS-METHODS new
      IMPORTING
        !iv_repo       TYPE zags_objects-repo
      RETURNING
        VALUE(ro_tree) TYPE REF TO zcl_ags_obj_tree
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
    DATA mv_sha1 TYPE zags_sha1 .
    DATA mv_adler32 TYPE zags_adler32 .
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


  METHOD get_files.

    rt_files = mt_data.

  ENDMETHOD.


  METHOD load.

    DATA: ls_data TYPE zags_objects.

    CREATE OBJECT ro_tree.
    ro_tree->mv_new = abap_false.
    ro_tree->mv_sha1 = iv_sha1.

    ls_data = zcl_ags_db=>get_objects( )->single(
      iv_repo = iv_repo
      iv_sha1 = iv_sha1 ).

    ro_tree->deserialize(
      iv_data    = ls_data-data_raw
      iv_adler32 = ls_data-adler32 ).

  ENDMETHOD.


  METHOD new.

    ASSERT NOT iv_repo IS INITIAL.

    CREATE OBJECT ro_tree.
    ro_tree->mv_new = abap_true.
    ro_tree->mv_repo = iv_repo.

  ENDMETHOD.


  METHOD set_files.

    ASSERT mv_new = abap_true.
    ASSERT lines( mt_data ) = 0. " do not overwrite existing data

    mt_data = it_files.

  ENDMETHOD.


  METHOD zif_ags_object~deserialize.

    mt_data = zcl_abapgit_git_pack=>decode_tree( iv_data ).
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
        iv_type = zif_ags_constants=>c_type-tree
        iv_data = serialize( ) ).
    ELSE.
      ASSERT NOT mv_sha1 IS INITIAL.
      rv_sha1 = mv_sha1.
    ENDIF.

  ENDMETHOD.


  METHOD zif_ags_object~get_type.
    rv_type = zif_ags_constants=>c_type-tree.
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

    rv_data = zcl_abapgit_git_pack=>encode_tree( mt_data ).

  ENDMETHOD.
ENDCLASS.
