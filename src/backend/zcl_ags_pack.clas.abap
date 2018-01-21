CLASS zcl_ags_pack DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS to_object
      IMPORTING
        !ii_object       TYPE REF TO zif_ags_object
      RETURNING
        VALUE(rs_object) TYPE zif_abapgit_definitions=>ty_object
      RAISING
        zcx_ags_error .
    CLASS-METHODS encode
      IMPORTING
        !it_objects    TYPE zif_abapgit_definitions=>ty_objects_tt
      RETURNING
        VALUE(rv_data) TYPE xstring .
    CLASS-METHODS decode
      IMPORTING
        !iv_data          TYPE xstring
      RETURNING
        VALUE(rt_objects) TYPE zif_abapgit_definitions=>ty_objects_tt .
    CLASS-METHODS save
      IMPORTING
        !iv_repo    TYPE zags_repos-repo
        !it_objects TYPE zif_abapgit_definitions=>ty_objects_tt
      RAISING
        zcx_ags_error .
    CLASS-METHODS explode
      IMPORTING
        !iv_repo          TYPE zags_repos-repo
        !ii_object        TYPE REF TO zif_ags_object
        !iv_deepen        TYPE i DEFAULT 0
      RETURNING
        VALUE(rt_objects) TYPE zif_abapgit_definitions=>ty_objects_tt
      RAISING
        zcx_ags_error .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AGS_PACK IMPLEMENTATION.


  METHOD decode.

    rt_objects = zcl_abapgit_git_pack=>decode( iv_data ).

  ENDMETHOD.


  METHOD encode.

    rv_data = zcl_abapgit_git_pack=>encode( it_objects ).

  ENDMETHOD.


  METHOD explode.

    TYPES: BEGIN OF ty_visit,
             object TYPE REF TO zif_ags_object,
             deepen TYPE i,
           END OF ty_visit.

    DEFINE _visit_commit.
      lo_parent = zcl_ags_obj_commit=>load( iv_repo = iv_repo iv_sha1 = &1 ).
      APPEND INITIAL LINE TO lt_visit ASSIGNING <ls_new>.
      <ls_new>-object = lo_parent.
      <ls_new>-deepen = <ls_visit>-deepen - 1.
    END-OF-DEFINITION.

    DEFINE _visit_tree.
      lo_sub = zcl_ags_obj_tree=>load( iv_repo = iv_repo iv_sha1 = &1 ).
      APPEND INITIAL LINE TO lt_visit ASSIGNING <ls_new>.
      <ls_new>-object = lo_sub.
    END-OF-DEFINITION.

    DEFINE _visit_blob.
      APPEND &1 TO lt_blobs_sha1.
    END-OF-DEFINITION.

    DATA: lo_tree       TYPE REF TO zcl_ags_obj_tree,
          lt_files      TYPE zcl_ags_obj_tree=>ty_tree_tt,
          lo_sub        TYPE REF TO zcl_ags_obj_tree,
          lt_blobs_sha1 TYPE zags_sha1_tt,
          li_object     TYPE REF TO zif_ags_object,
          lt_blobs      TYPE zcl_ags_obj_blob=>ty_list,
          lo_parent     TYPE REF TO zcl_ags_obj_commit,
          ls_commit     TYPE zcl_ags_obj_commit=>ty_commit,
          lt_visit      TYPE STANDARD TABLE OF ty_visit WITH DEFAULT KEY,
          lo_commit     TYPE REF TO zcl_ags_obj_commit.

    FIELD-SYMBOLS: <ls_visit> LIKE LINE OF lt_visit,
                   <ls_new>   LIKE LINE OF lt_visit,
                   <ls_file>  LIKE LINE OF lt_files.


    ASSERT NOT iv_repo IS INITIAL.

    APPEND INITIAL LINE TO lt_visit ASSIGNING <ls_visit>.
    <ls_visit>-object = ii_object.
    <ls_visit>-deepen = iv_deepen.

    LOOP AT lt_visit ASSIGNING <ls_visit>.
      APPEND to_object( <ls_visit>-object ) TO rt_objects.

      IF <ls_visit>-object->get_type( ) = zif_ags_constants=>c_type-commit.
        lo_commit ?= <ls_visit>-object.
        ls_commit = lo_commit->get( ).

        _visit_tree ls_commit-tree.

        IF <ls_visit>-deepen <> 1 AND NOT ls_commit-parent IS INITIAL.
          _visit_commit ls_commit-parent.

          IF NOT ls_commit-parent2 IS INITIAL.
            _visit_commit ls_commit-parent2.
          ENDIF.
        ENDIF.
      ELSEIF <ls_visit>-object->get_type( ) = zif_ags_constants=>c_type-tree.
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

    lt_blobs = zcl_ags_obj_blob=>load_mass(
      iv_repo = iv_repo
      it_sha1 = lt_blobs_sha1 ).
    LOOP AT lt_blobs INTO li_object.
      APPEND to_object( li_object ) TO rt_objects.
    ENDLOOP.

  ENDMETHOD.


  METHOD save.

    DATA: li_object TYPE REF TO zif_ags_object.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF it_objects.


    ASSERT NOT iv_repo IS INITIAL.

    LOOP AT it_objects ASSIGNING <ls_object>.
      CASE <ls_object>-type.
        WHEN zif_ags_constants=>c_type-blob.
          li_object = zcl_ags_obj_blob=>new( iv_repo ).
        WHEN zif_ags_constants=>c_type-tree.
          li_object = zcl_ags_obj_tree=>new( iv_repo ).
        WHEN zif_ags_constants=>c_type-commit.
          li_object = zcl_ags_obj_commit=>new( iv_repo ).
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_ags_error
            EXPORTING
              textid = zcx_ags_error=>m009.
      ENDCASE.

      li_object->deserialize( iv_data    = <ls_object>-data
                              iv_adler32 = <ls_object>-adler32 ).

      ASSERT li_object->get_sha1( ) = <ls_object>-sha1.

      li_object->save( ).

    ENDLOOP.

  ENDMETHOD.


  METHOD to_object.

    rs_object-sha1 = ii_object->get_sha1( ).
    rs_object-type = ii_object->get_type( ).
    rs_object-data = ii_object->serialize( ).
    rs_object-adler32 = ii_object->get_adler32( ).

  ENDMETHOD.
ENDCLASS.
