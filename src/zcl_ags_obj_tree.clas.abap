CLASS zcl_ags_obj_tree DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_ags_object .

    CONSTANTS:
      BEGIN OF c_chmod,
        file TYPE c LENGTH 6 VALUE '100644',
        dir  TYPE c LENGTH 5 VALUE '40000',
      END OF c_chmod .

    TYPES: BEGIN OF ty_tree,
             chmod TYPE string,
             name  TYPE string,
             file  TYPE REF TO zcl_ags_obj_file,
           END OF ty_tree.

    TYPES: ty_tree_tt TYPE STANDARD TABLE OF ty_tree WITH DEFAULT KEY.

    METHODS add_file
      IMPORTING
        !iv_chmod TYPE clike
        !iv_name  TYPE ty_tree-name
        !io_file  TYPE ty_tree-file .
    METHODS list_files
      RETURNING
        VALUE(rt_files) TYPE ty_tree_tt .
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

  BREAK-POINT.

ENDMETHOD.
ENDCLASS.