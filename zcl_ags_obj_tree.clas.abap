class ZCL_AGS_OBJ_TREE definition
  public
  final
  create public .

public section.

  interfaces ZIF_AGS_OBJECT .

  constants:
    BEGIN OF c_chmod,
                 file TYPE c LENGTH 6 VALUE '100644',
                 dir  TYPE c LENGTH 5 VALUE '40000',
    END OF c_chmod .

  methods ADD_FILE
    importing
      !IV_CHMOD type CLIKE
      !IV_NAME type ZAGS_OBJ_TREE-NAME
      !IO_FILE type ZAGS_OBJ_TREE-FILE .
  methods LIST_FILES
    returning
      value(RT_FILES) type ZAGS_OBJ_TREE_TT .
protected section.
private section.

  data MT_DATA type ZAGS_OBJ_TREE_TT .
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

  BREAK-POINT.

ENDMETHOD.
ENDCLASS.