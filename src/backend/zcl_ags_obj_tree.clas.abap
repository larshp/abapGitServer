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
  aliases SERIALIZE
    for ZIF_AGS_OBJECT~SERIALIZE .
  aliases SHA1
    for ZIF_AGS_OBJECT~SHA1 .

  types:
    BEGIN OF ty_tree,
             chmod TYPE string,
             name  TYPE string,
             file  TYPE REF TO zcl_ags_obj_file,
           END OF ty_tree .
  types:
    ty_tree_tt TYPE STANDARD TABLE OF ty_tree WITH DEFAULT KEY .

  constants:
    BEGIN OF c_chmod,
        file TYPE c LENGTH 6 VALUE '100644',
        dir  TYPE c LENGTH 5 VALUE '40000',
      END OF c_chmod .

  methods ADD_FILE
    importing
      !IV_CHMOD type CLIKE
      !IV_NAME type TY_TREE-NAME
      !IO_FILE type TY_TREE-FILE .
  methods LIST_FILES
    returning
      value(RT_FILES) type TY_TREE_TT .
protected section.
private section.

  data MT_DATA type ty_tree_tt .
ENDCLASS.



CLASS ZCL_AGS_OBJ_TREE IMPLEMENTATION.


METHOD add_file.

  APPEND INITIAL LINE TO mt_data ASSIGNING FIELD-SYMBOL(<ls_data>).
  <ls_data>-chmod = iv_chmod.
  <ls_data>-name = iv_name.
  <ls_data>-file = io_file.

ENDMETHOD.


METHOD list_files.

  rt_files = mt_data.

ENDMETHOD.


METHOD zif_ags_object~deserialize.

  BREAK-POINT.

ENDMETHOD.


METHOD zif_ags_object~serialize.

  BREAK-POINT.

ENDMETHOD.


METHOD zif_ags_object~sha1.

  zcl_ags_util=>sha1(
      iv_type = 'tree'
      iv_data = serialize( ) ).

ENDMETHOD.
ENDCLASS.