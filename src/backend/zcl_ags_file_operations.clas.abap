CLASS zcl_ags_file_operations DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS add
      IMPORTING
        !iv_filename       TYPE string
        !iv_path           TYPE string
        !iv_file_contents  TYPE string
        !iv_commit_message TYPE string
      RAISING
        zcx_ags_error .
    METHODS constructor
      IMPORTING
        !io_branch TYPE REF TO zcl_ags_branch .
    METHODS delete
      IMPORTING
        !iv_filename       TYPE string
        !iv_path           TYPE string
        !iv_commit_message TYPE string
      RAISING
        zcx_ags_error .
    METHODS modify
      IMPORTING
        !iv_filename       TYPE string
        !iv_path           TYPE string
        !iv_file_contents  TYPE string
        !iv_commit_message TYPE string
      RAISING
        zcx_ags_error .
  PROTECTED SECTION.

    DATA mo_branch TYPE REF TO zcl_ags_branch .
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_tree,
        tree TYPE REF TO zcl_ags_obj_tree,
        path TYPE string,
      END OF ty_tree .
    TYPES:
      ty_trees_tt TYPE STANDARD TABLE OF ty_tree WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_folder,
        path  TYPE string,
        depth TYPE i,
        sha1  TYPE zags_sha1,
      END OF ty_folder .
    TYPES:
      ty_folders_tt TYPE STANDARD TABLE OF ty_folder WITH DEFAULT KEY .

    CLASS-METHODS extract_folder_name
      IMPORTING
        !iv_path       TYPE string
      RETURNING
        VALUE(rv_name) TYPE string .
    METHODS find_folders
      IMPORTING
        !it_files         TYPE zcl_ags_cache=>ty_files_simple_tt
      RETURNING
        VALUE(rt_folders) TYPE ty_folders_tt .
    METHODS build_trees
      IMPORTING
        !it_files       TYPE zcl_ags_cache=>ty_files_simple_tt
      RETURNING
        VALUE(rt_trees) TYPE ty_trees_tt
      RAISING
        zcx_ags_error .
    METHODS set_author_and_committer
      IMPORTING
        !io_commit TYPE REF TO zcl_ags_obj_commit
      RAISING
        zcx_ags_error .
ENDCLASS.



CLASS ZCL_AGS_FILE_OPERATIONS IMPLEMENTATION.


  METHOD add.

    DATA: lo_blob    TYPE REF TO zcl_ags_obj_blob,
          lt_files   TYPE zcl_ags_cache=>ty_files_simple_tt,
          lo_commit  TYPE REF TO zcl_ags_obj_commit,
          lt_trees   TYPE ty_trees_tt,
          lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF lt_files,
                   <ls_tree> LIKE LINE OF lt_trees.


    lo_blob = zcl_ags_obj_blob=>new( mo_branch->get_data( )-repo ).
    lo_blob->set_data( zcl_ags_util=>string_to_xstring_utf8( iv_file_contents ) ).

    lt_files = mo_branch->get_cache( )->list_files_simple( ).

    APPEND INITIAL LINE TO lt_files ASSIGNING <ls_file>.
    <ls_file>-filename  = iv_filename.
    <ls_file>-path      = iv_path.
    <ls_file>-blob_sha1 = lo_blob->get_sha1( ).
    <ls_file>-chmod     = zcl_ags_obj_tree=>c_chmod-file.

    lt_trees = build_trees( lt_files ).
    READ TABLE lt_trees ASSIGNING <ls_tree> WITH KEY path = '/'.
    ASSERT sy-subrc = 0.

    lo_commit = zcl_ags_obj_commit=>new( mo_branch->get_data( )-repo ).
    lo_commit->set_tree( <ls_tree>-tree->get_sha1( ) ).
    set_author_and_committer( lo_commit ).
    lo_commit->set_body( iv_commit_message ).
    lo_commit->set_parent( mo_branch->get_data( )-sha1 ).

    APPEND zcl_ags_pack=>to_object( lo_blob ) TO lt_objects.
    APPEND zcl_ags_pack=>to_object( lo_commit ) TO lt_objects.
    LOOP AT lt_trees ASSIGNING <ls_tree>.
      APPEND zcl_ags_pack=>to_object( <ls_tree>-tree ) TO lt_objects.
    ENDLOOP.

    mo_branch->push(
      iv_new     = lo_commit->get_sha1( )
      iv_old     = mo_branch->get_data( )-sha1
      it_objects = lt_objects ).

  ENDMETHOD.


  METHOD build_trees.

    DATA: lt_folders TYPE ty_folders_tt,
          lv_name    TYPE string,
          lv_sub     TYPE string,
          lo_tree    TYPE REF TO zcl_ags_obj_tree.

    FIELD-SYMBOLS: <ls_file>   LIKE LINE OF it_files,
                   <ls_sub>    LIKE LINE OF lt_folders,
                   <ls_tree>   LIKE LINE OF rt_trees,
                   <ls_folder> LIKE LINE OF lt_folders.


    lt_folders = find_folders( it_files ).

* start with the deepest folders
    SORT lt_folders BY depth DESCENDING.

    LOOP AT lt_folders ASSIGNING <ls_folder>.
      lo_tree = zcl_ags_obj_tree=>new( mo_branch->get_data( )-repo ).

* files
      LOOP AT it_files ASSIGNING <ls_file>
          WHERE path = <ls_folder>-path
          AND chmod <> zcl_ags_obj_tree=>c_chmod-dir.
        lo_tree->add_file(
          iv_chmod = <ls_file>-chmod
          iv_name  = <ls_file>-filename
          iv_sha1  = <ls_file>-blob_sha1 ).
      ENDLOOP.

* folders
      lv_sub = <ls_folder>-path && '+*'.
      LOOP AT lt_folders ASSIGNING <ls_sub>
          WHERE depth = <ls_folder>-depth + 1
          AND path CP lv_sub.

        lv_name = extract_folder_name( <ls_sub>-path ).

        lo_tree->add_file(
          iv_chmod = zcl_ags_obj_tree=>c_chmod-dir
          iv_name  = lv_name
          iv_sha1  = <ls_sub>-sha1 ).
      ENDLOOP.

      APPEND INITIAL LINE TO rt_trees ASSIGNING <ls_tree>.
      <ls_tree>-tree = lo_tree.
      <ls_tree>-path = <ls_folder>-path.

      <ls_folder>-sha1 = lo_tree->get_sha1( ).
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.
    mo_branch = io_branch.
  ENDMETHOD.


  METHOD delete.

    DATA: lo_tree    TYPE REF TO zcl_ags_obj_tree,
          lt_old     TYPE zcl_ags_obj_tree=>ty_tree_tt,
          lo_commit  TYPE REF TO zcl_ags_obj_commit,
          lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt.

    FIELD-SYMBOLS: <ls_old> LIKE LINE OF lt_old.


* todo, additional handling required to handle paths
* use method BUILD_TREES
    ASSERT iv_path = '/'.

    lt_old = zcl_ags_obj_tree=>load(
      iv_repo = mo_branch->get_data( )-repo
      iv_sha1 = zcl_ags_obj_commit=>load(
      iv_repo = mo_branch->get_data( )-repo
      iv_sha1 = mo_branch->get_data( )-sha1
      )->get( )-tree )->get_files( ).

    READ TABLE lt_old ASSIGNING <ls_old> WITH KEY
      chmod = zcl_ags_obj_tree=>c_chmod-file
      name  = iv_filename.
    ASSERT sy-subrc = 0.
    DELETE lt_old INDEX sy-tabix.
    ASSERT sy-subrc = 0.

    lo_tree = zcl_ags_obj_tree=>new( mo_branch->get_data( )-repo ).
    lo_tree->set_files( lt_old ).

    lo_commit = zcl_ags_obj_commit=>new( mo_branch->get_data( )-repo ).
    lo_commit->set_tree( lo_tree->get_sha1( ) ).
    set_author_and_committer( lo_commit ).
    lo_commit->set_body( iv_commit_message ).
    lo_commit->set_parent( mo_branch->get_data( )-sha1 ).

    APPEND zcl_ags_pack=>to_object( lo_tree ) TO lt_objects.
    APPEND zcl_ags_pack=>to_object( lo_commit ) TO lt_objects.

    mo_branch->push(
      iv_new     = lo_commit->get_sha1( )
      iv_old     = mo_branch->get_data( )-sha1
      it_objects = lt_objects ).

  ENDMETHOD.


  METHOD extract_folder_name.

    DATA: lt_table TYPE STANDARD TABLE OF string WITH DEFAULT KEY.


    SPLIT iv_path AT '/' INTO TABLE lt_table.
    READ TABLE lt_table INDEX lines( lt_table ) INTO rv_name.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD find_folders.

    DATA: lt_paths TYPE TABLE OF string,
          lv_split TYPE string,
          lv_path  TYPE string.

    FIELD-SYMBOLS: <ls_folder> LIKE LINE OF rt_folders,
                   <ls_new>    LIKE LINE OF rt_folders,
                   <ls_file>   LIKE LINE OF it_files.


    LOOP AT it_files ASSIGNING <ls_file>.
      READ TABLE rt_folders WITH KEY path = <ls_file>-path TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO rt_folders ASSIGNING <ls_folder>.
        <ls_folder>-path = <ls_file>-path.
      ENDIF.
    ENDLOOP.

* add empty folders
    LOOP AT rt_folders ASSIGNING <ls_folder>.
      SPLIT <ls_folder>-path AT '/' INTO TABLE lt_paths.

      CLEAR lv_path.
      LOOP AT lt_paths INTO lv_split.
        CONCATENATE lv_path lv_split '/' INTO lv_path.
        READ TABLE rt_folders WITH KEY path = lv_path TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO rt_folders ASSIGNING <ls_new>.
          <ls_new>-path = lv_path.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    LOOP AT rt_folders ASSIGNING <ls_folder>.
      FIND ALL OCCURRENCES OF '/' IN <ls_folder>-path MATCH COUNT <ls_folder>-depth.
    ENDLOOP.

  ENDMETHOD.


  METHOD modify.

    DATA: lo_blob    TYPE REF TO zcl_ags_obj_blob,
          lo_tree    TYPE REF TO zcl_ags_obj_tree,
          lt_old     TYPE zcl_ags_obj_tree=>ty_tree_tt,
          lo_commit  TYPE REF TO zcl_ags_obj_commit,
          lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt.

    FIELD-SYMBOLS: <ls_old> LIKE LINE OF lt_old.


* todo, additional handling required to handle paths
* use method BUILD_TREES
    ASSERT iv_path = '/'.

    lt_old = zcl_ags_obj_tree=>load(
      iv_repo = mo_branch->get_data( )-repo
      iv_sha1 = zcl_ags_obj_commit=>load(
      iv_repo = mo_branch->get_data( )-repo
      iv_sha1 = mo_branch->get_data( )-sha1
      )->get( )-tree )->get_files( ).

    lo_blob = zcl_ags_obj_blob=>new( mo_branch->get_data( )-repo ).
    lo_blob->set_data( zcl_ags_util=>string_to_xstring_utf8( iv_file_contents ) ).

    READ TABLE lt_old ASSIGNING <ls_old> WITH KEY
      chmod = zcl_ags_obj_tree=>c_chmod-file
      name  = iv_filename.
    ASSERT sy-subrc = 0.
    <ls_old>-sha1 = lo_blob->get_sha1( ).

    lo_tree = zcl_ags_obj_tree=>new( mo_branch->get_data( )-repo ).
    lo_tree->set_files( lt_old ).

    lo_commit = zcl_ags_obj_commit=>new( mo_branch->get_data( )-repo ).
    lo_commit->set_tree( lo_tree->get_sha1( ) ).
    set_author_and_committer( lo_commit ).
    lo_commit->set_body( iv_commit_message ).
    lo_commit->set_parent( mo_branch->get_data( )-sha1 ).

    APPEND zcl_ags_pack=>to_object( lo_blob ) TO lt_objects.
    APPEND zcl_ags_pack=>to_object( lo_tree ) TO lt_objects.
    APPEND zcl_ags_pack=>to_object( lo_commit ) TO lt_objects.

    mo_branch->push(
      iv_new     = lo_commit->get_sha1( )
      iv_old     = mo_branch->get_data( )-sha1
      it_objects = lt_objects ).

  ENDMETHOD.


  METHOD set_author_and_committer.

    DATA: lv_user TYPE string.

    lv_user = |{ sy-uname } <{ sy-uname }@localhost> { zcl_ags_util=>get_time( ) }|.

    io_commit->set_author( lv_user ).
    io_commit->set_committer( lv_user ).

  ENDMETHOD.
ENDCLASS.
