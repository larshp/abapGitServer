class ZCL_AGS_OBJ_TREE definition
  public
  final
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
    ty_chmod TYPE c LENGTH 6 .
  types:
    BEGIN OF ty_tree,
        chmod TYPE ty_chmod,
        name  TYPE string,
        sha1  TYPE zags_sha1,
      END OF ty_tree .
  types:
    ty_tree_tt TYPE STANDARD TABLE OF ty_tree WITH DEFAULT KEY .

  constants:
    BEGIN OF c_chmod,
        file       TYPE ty_chmod VALUE '100644',
        executable TYPE ty_chmod VALUE '100755',
        dir        TYPE ty_chmod VALUE '40000',
      END OF c_chmod .

  class-methods GET_INSTANCE
    importing
      !IV_REPO type ZAGS_OBJECTS-REPO
      !IV_SHA1 type ZAGS_OBJECTS-SHA1
    returning
      value(RO_TREE) type ref to ZCL_AGS_OBJ_TREE
    raising
      ZCX_AGS_ERROR .
  methods ADD_FILE
    importing
      !IV_CHMOD type TY_CHMOD
      !IV_NAME type TY_TREE-NAME
      !IV_SHA1 type TY_TREE-SHA1 .
  methods CONSTRUCTOR
    importing
      !IV_REPO type ZAGS_OBJECTS-REPO
      !IV_SHA1 type ZAGS_OBJECTS-SHA1 optional
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
