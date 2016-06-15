class ZCL_AGS_PACK definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_object,
        sha1 TYPE zags_sha1,
        type TYPE zags_type,
        data TYPE xstring,
      END OF ty_object .
  types:
    ty_objects_tt TYPE STANDARD TABLE OF ty_object WITH DEFAULT KEY .

  class-methods ENCODE
    importing
      !IT_OBJECTS type TY_OBJECTS_TT
    returning
      value(RV_PACK) type XSTRING .
  class-methods DECODE
    importing
      !IV_PACK type XSTRING
    returning
      value(RT_PACK) type TY_OBJECTS_TT .
  class-methods EXPLODE
    importing
      !II_OBJECT type ref to ZIF_AGS_OBJECT
    returning
      value(RT_OBJECTS) type TY_OBJECTS_TT
    raising
      ZCX_AGS_ERROR .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AGS_PACK IMPLEMENTATION.


  METHOD decode.
  ENDMETHOD.


  METHOD encode.
  ENDMETHOD.


  METHOD explode.

    DATA: lo_tree TYPE REF TO zcl_ags_obj_tree.


    APPEND INITIAL LINE TO rt_objects ASSIGNING FIELD-SYMBOL(<ls_obj>).
    <ls_obj>-sha1 = ii_object->sha1( ).
    <ls_obj>-type = ii_object->type( ).
    <ls_obj>-data = ii_object->serialize( ).

* traverse sub objects
    CASE TYPE OF ii_object.
      WHEN TYPE zcl_ags_obj_commit INTO DATA(lo_commit).
        lo_tree = NEW zcl_ags_obj_tree( lo_commit->get_tree( ) ).
        APPEND LINES OF explode( lo_tree ) TO rt_objects.
      WHEN TYPE zcl_ags_obj_tree INTO lo_tree.
        LOOP AT lo_tree->list_files( ) ASSIGNING FIELD-SYMBOL(<ls_file>).
          CASE <ls_file>-chmod.
            WHEN zcl_ags_obj_tree=>c_chmod-dir.
              DATA(lo_sub) = NEW zcl_ags_obj_tree( <ls_file>-sha1 ).
              APPEND LINES OF explode( lo_sub ) TO rt_objects.
            WHEN OTHERS.
              DATA(lo_blob) = NEW zcl_ags_obj_blob( <ls_file>-sha1 ).
              APPEND LINES OF explode( lo_blob ) TO rt_objects.
          ENDCASE.
        ENDLOOP.
    ENDCASE.

* todo, how to handle duplicates?
* eg, if the same file exists in 2 subdirectories?

  ENDMETHOD.
ENDCLASS.