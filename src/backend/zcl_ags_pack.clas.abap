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
  types:
    ty_adler32 TYPE x LENGTH 4 .

  class-methods TO_OBJECT
    importing
      !II_OBJECT type ref to ZIF_AGS_OBJECT
    returning
      value(RS_OBJECT) type TY_OBJECT
    raising
      ZCX_AGS_ERROR .
  class-methods ENCODE
    importing
      !IT_OBJECTS type TY_OBJECTS_TT
    returning
      value(RV_DATA) type XSTRING .
  class-methods DECODE
    importing
      !IV_DATA type XSTRING
    returning
      value(RT_OBJECTS) type TY_OBJECTS_TT .
  class-methods SAVE
    importing
      !IT_OBJECTS type TY_OBJECTS_TT
    raising
      ZCX_AGS_ERROR .
  class-methods EXPLODE
    importing
      !II_OBJECT type ref to ZIF_AGS_OBJECT
      !IV_DEEPEN type I default 0
    returning
      value(RT_OBJECTS) type TY_OBJECTS_TT
    raising
      ZCX_AGS_ERROR .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AGS_PACK IMPLEMENTATION.


  METHOD decode.

    CALL METHOD ('\PROGRAM=ZABAPGIT\CLASS=LCL_GIT_PACK')=>decode
      EXPORTING
        iv_data    = iv_data
      RECEIVING
        rt_objects = rt_objects.

  ENDMETHOD.


  METHOD encode.

    CALL METHOD ('\PROGRAM=ZABAPGIT\CLASS=LCL_GIT_PACK')=>encode
      EXPORTING
        it_objects = it_objects
      RECEIVING
        rv_data    = rv_data.

  ENDMETHOD.


  METHOD explode.

    TYPES: BEGIN OF ty_visit,
             object TYPE REF TO zif_ags_object,
             deepen TYPE i,
           END OF ty_visit.

    DEFINE _visit_commit.
      CREATE OBJECT lo_parent EXPORTING iv_sha1 = &1.
      APPEND INITIAL LINE TO lt_visit ASSIGNING <ls_new>.
      <ls_new>-object = lo_parent.
      <ls_new>-deepen = <ls_visit>-deepen - 1.
    END-OF-DEFINITION.

    DEFINE _visit_tree.
      CREATE OBJECT lo_sub EXPORTING iv_sha1 = &1.
      APPEND INITIAL LINE TO lt_visit ASSIGNING <ls_new>.
      <ls_new>-object = lo_sub.
    END-OF-DEFINITION.

    DEFINE _visit_blob.
      CREATE OBJECT lo_blob EXPORTING iv_sha1 = &1.
      APPEND INITIAL LINE TO lt_visit ASSIGNING <ls_new>.
      <ls_new>-object = lo_blob.
    END-OF-DEFINITION.

    DATA: lo_tree   TYPE REF TO zcl_ags_obj_tree,
          lt_files  TYPE zcl_ags_obj_tree=>ty_tree_tt,
          lo_sub    TYPE REF TO zcl_ags_obj_tree,
          lo_blob   TYPE REF TO zcl_ags_obj_blob,
          lo_parent TYPE REF TO zcl_ags_obj_commit,
          ls_commit TYPE zcl_ags_obj_commit=>ty_commit,
          lt_visit  TYPE STANDARD TABLE OF ty_visit WITH DEFAULT KEY,
          lo_commit TYPE REF TO zcl_ags_obj_commit.

    FIELD-SYMBOLS: <ls_visit> LIKE LINE OF lt_visit,
                   <ls_new>   LIKE LINE OF lt_visit,
                   <ls_file>  LIKE LINE OF lt_files.


    APPEND INITIAL LINE TO lt_visit ASSIGNING <ls_visit>.
    <ls_visit>-object = ii_object.
    <ls_visit>-deepen = iv_deepen.

    LOOP AT lt_visit ASSIGNING <ls_visit>.
      APPEND to_object( <ls_visit>-object ) TO rt_objects.

      IF <ls_visit>-object->type( ) = zif_ags_constants=>c_type-commit.
        lo_commit ?= <ls_visit>-object.
        ls_commit = lo_commit->get( ).

        _visit_tree ls_commit-tree.

        IF <ls_visit>-deepen <> 1 AND NOT ls_commit-parent IS INITIAL.
          _visit_commit ls_commit-parent.

          IF NOT ls_commit-parent2 IS INITIAL.
            _visit_commit ls_commit-parent2.
          ENDIF.
        ENDIF.
      ELSEIF <ls_visit>-object->type( ) = zif_ags_constants=>c_type-tree.
        lo_tree ?= <ls_visit>-object.
        lt_files = lo_tree->get_files( ).
        LOOP AT lt_files ASSIGNING <ls_file>.
          IF <ls_file>-chmod = zcl_ags_obj_tree=>c_chmod-dir.
            _visit_tree <ls_file>-sha1.
          ELSE.
            _visit_blob <ls_file>-sha1.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD save.

    DATA: li_object TYPE REF TO zif_ags_object,
          lo_blob   TYPE REF TO zcl_ags_obj_blob,
          lo_tree   TYPE REF TO zcl_ags_obj_tree,
          lo_commit TYPE REF TO zcl_ags_obj_commit.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF it_objects.


    LOOP AT it_objects ASSIGNING <ls_object>.
      CASE <ls_object>-type.
        WHEN zif_ags_constants=>c_type-blob.
          CREATE OBJECT lo_blob.
          li_object = lo_blob.
        WHEN zif_ags_constants=>c_type-tree.
          CREATE OBJECT lo_tree.
          li_object = lo_tree.
        WHEN zif_ags_constants=>c_type-commit.
          CREATE OBJECT lo_commit.
          li_object = lo_commit.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_ags_error
            EXPORTING
              textid = zcx_ags_error=>m009.
      ENDCASE.

      li_object->deserialize( <ls_object>-data ).

      ASSERT li_object->sha1( ) = <ls_object>-sha1.

      li_object->save( ).

    ENDLOOP.

  ENDMETHOD.


  METHOD to_object.

    rs_object-sha1 = ii_object->sha1( ).
    rs_object-type = ii_object->type( ).
    rs_object-data = ii_object->serialize( ).

  ENDMETHOD.
ENDCLASS.
