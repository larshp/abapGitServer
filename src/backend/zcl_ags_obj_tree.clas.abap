class ZCL_AGS_OBJ_TREE definition
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
    BEGIN OF ty_tree,
        chmod TYPE zags_chmod,
        name  TYPE string,
        sha1  TYPE zags_sha1,
      END OF ty_tree .
  types:
    ty_tree_tt TYPE STANDARD TABLE OF ty_tree WITH DEFAULT KEY .

  constants:
    BEGIN OF c_chmod,
        file       TYPE zags_chmod VALUE '100644',
        executable TYPE zags_chmod VALUE '100755',
        dir        TYPE zags_chmod VALUE '40000',
      END OF c_chmod .

  class-methods LOAD
    importing
      !IV_REPO type ZAGS_OBJECTS-REPO
      !IV_SHA1 type ZAGS_OBJECTS-SHA1
    returning
      value(RO_TREE) type ref to ZCL_AGS_OBJ_TREE
    raising
      ZCX_AGS_ERROR .
  methods ADD_FILE
    importing
      !IV_CHMOD type ZAGS_CHMOD
      !IV_NAME type TY_TREE-NAME
      !IV_SHA1 type TY_TREE-SHA1 .
  class-methods NEW
    importing
      !IV_REPO type ZAGS_OBJECTS-REPO
    returning
      value(RO_TREE) type ref to ZCL_AGS_OBJ_TREE
    raising
      ZCX_AGS_ERROR .
  methods GET_FILES
    returning
      value(RT_FILES) type TY_TREE_TT .
  methods SET_FILES
    importing
      value(IT_FILES) type TY_TREE_TT .
  PROTECTED SECTION.
private section.

  data MT_DATA type TY_TREE_TT .
  data MV_NEW type ABAP_BOOL .
  data MV_REPO type ZAGS_OBJECTS-REPO .
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

    CREATE OBJECT ro_tree.

    ro_tree->mv_new = abap_false.

    ro_tree->deserialize( zcl_ags_db=>get_objects( )->single(
      iv_repo = iv_repo
      iv_sha1 = iv_sha1 )-data_raw ).

  ENDMETHOD.


  METHOD new.

    ASSERT NOT iv_repo IS INITIAL.

    CREATE OBJECT ro_tree.
    ro_tree->mv_new = abap_true.
    ro_tree->mv_repo = iv_repo.

  ENDMETHOD.


  METHOD set_files.

    ASSERT mv_new = abap_true.
    ASSERT lines( mt_data ) = 0.

    mt_data = it_files.

  ENDMETHOD.


  METHOD zif_ags_object~deserialize.

    mt_data = zcl_abapgit_git_pack=>decode_tree( iv_data ).

  ENDMETHOD.


  METHOD ZIF_AGS_OBJECT~GET_SHA1.

    rv_sha1 = zcl_ags_util=>sha1(
      iv_type = zif_ags_constants=>c_type-tree
      iv_data = serialize( ) ).

  ENDMETHOD.


  METHOD ZIF_AGS_OBJECT~GET_TYPE.
    rv_type = zif_ags_constants=>c_type-tree.
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

    rv_data = zcl_abapgit_git_pack=>encode_tree( mt_data ).

  ENDMETHOD.
ENDCLASS.
