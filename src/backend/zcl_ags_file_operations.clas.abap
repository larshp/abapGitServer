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

    METHODS set_author_and_committer
      IMPORTING
        !io_commit TYPE REF TO zcl_ags_obj_commit
      RAISING
        zcx_ags_error .
ENDCLASS.



CLASS ZCL_AGS_FILE_OPERATIONS IMPLEMENTATION.


  METHOD add.

    DATA: lo_blob    TYPE REF TO zcl_ags_obj_blob,
          lo_tree    TYPE REF TO zcl_ags_obj_tree,
          lt_old     TYPE zcl_ags_obj_tree=>ty_tree_tt,
          lo_commit  TYPE REF TO zcl_ags_obj_commit,
          lt_objects TYPE zcl_ags_pack=>ty_objects_tt.


* todo, additional handling required to handle paths
    ASSERT iv_path = '/'.

    lt_old = zcl_ags_obj_tree=>get_instance(
      iv_repo = mo_branch->get_data( )-repo
      iv_sha1 = zcl_ags_obj_commit=>get_instance(
      iv_repo = mo_branch->get_data( )-repo
      iv_sha1 = mo_branch->get_data( )-sha1
      )->get( )-tree )->get_files( ).

    CREATE OBJECT lo_blob
      EXPORTING
        iv_repo = mo_branch->get_data( )-repo.
    lo_blob->set_data( zcl_ags_util=>string_to_xstring_utf8( iv_file_contents ) ).

    CREATE OBJECT lo_tree
      EXPORTING
        iv_repo = mo_branch->get_data( )-repo.
    lo_tree->set_files( lt_old ).
    lo_tree->add_file(
      iv_chmod = zcl_ags_obj_tree=>c_chmod-file
      iv_name  = iv_filename
      iv_sha1  = lo_blob->sha1( ) ).

    CREATE OBJECT lo_commit
      EXPORTING
        iv_repo = mo_branch->get_data( )-repo.
    lo_commit->set_tree( lo_tree->sha1( ) ).
    set_author_and_committer( lo_commit ).
    lo_commit->set_body( iv_commit_message ).
    lo_commit->set_parent( mo_branch->get_data( )-sha1 ).

    APPEND zcl_ags_pack=>to_object( lo_blob ) TO lt_objects.
    APPEND zcl_ags_pack=>to_object( lo_tree ) TO lt_objects.
    APPEND zcl_ags_pack=>to_object( lo_commit ) TO lt_objects.

    mo_branch->push(
      iv_new     = lo_commit->sha1( )
      iv_old     = mo_branch->get_data( )-sha1
      it_objects = lt_objects ).

  ENDMETHOD.


  METHOD constructor.
    mo_branch = io_branch.
  ENDMETHOD.


  METHOD delete.

    DATA: lo_tree    TYPE REF TO zcl_ags_obj_tree,
          lt_old     TYPE zcl_ags_obj_tree=>ty_tree_tt,
          lo_commit  TYPE REF TO zcl_ags_obj_commit,
          lt_objects TYPE zcl_ags_pack=>ty_objects_tt.

    FIELD-SYMBOLS: <ls_old> LIKE LINE OF lt_old.


* todo, additional handling required to handle paths
    ASSERT iv_path = '/'.

    lt_old = zcl_ags_obj_tree=>get_instance(
      iv_repo = mo_branch->get_data( )-repo
      iv_sha1 = zcl_ags_obj_commit=>get_instance(
      iv_repo = mo_branch->get_data( )-repo
      iv_sha1 = mo_branch->get_data( )-sha1
      )->get( )-tree )->get_files( ).

    READ TABLE lt_old ASSIGNING <ls_old> WITH KEY
      chmod = zcl_ags_obj_tree=>c_chmod-file
      name  = iv_filename.
    ASSERT sy-subrc = 0.
    DELETE lt_old INDEX sy-tabix.
    ASSERT sy-subrc = 0.

    CREATE OBJECT lo_tree
      EXPORTING
        iv_repo = mo_branch->get_data( )-repo.
    lo_tree->set_files( lt_old ).

    CREATE OBJECT lo_commit
      EXPORTING
        iv_repo = mo_branch->get_data( )-repo.
    lo_commit->set_tree( lo_tree->sha1( ) ).
    set_author_and_committer( lo_commit ).
    lo_commit->set_body( iv_commit_message ).
    lo_commit->set_parent( mo_branch->get_data( )-sha1 ).

    APPEND zcl_ags_pack=>to_object( lo_tree ) TO lt_objects.
    APPEND zcl_ags_pack=>to_object( lo_commit ) TO lt_objects.

    mo_branch->push(
      iv_new     = lo_commit->sha1( )
      iv_old     = mo_branch->get_data( )-sha1
      it_objects = lt_objects ).

  ENDMETHOD.


  METHOD modify.

    DATA: lo_blob    TYPE REF TO zcl_ags_obj_blob,
          lo_tree    TYPE REF TO zcl_ags_obj_tree,
          lt_old     TYPE zcl_ags_obj_tree=>ty_tree_tt,
          lo_commit  TYPE REF TO zcl_ags_obj_commit,
          lt_objects TYPE zcl_ags_pack=>ty_objects_tt.

    FIELD-SYMBOLS: <ls_old> LIKE LINE OF lt_old.


* todo, additional handling required to handle paths
    ASSERT iv_path = '/'.

    lt_old = zcl_ags_obj_tree=>get_instance(
      iv_repo = mo_branch->get_data( )-repo
      iv_sha1 = zcl_ags_obj_commit=>get_instance(
      iv_repo = mo_branch->get_data( )-repo
      iv_sha1 = mo_branch->get_data( )-sha1
      )->get( )-tree )->get_files( ).

    CREATE OBJECT lo_blob
      EXPORTING
        iv_repo = mo_branch->get_data( )-repo.
    lo_blob->set_data( zcl_ags_util=>string_to_xstring_utf8( iv_file_contents ) ).

    READ TABLE lt_old ASSIGNING <ls_old> WITH KEY
      chmod = zcl_ags_obj_tree=>c_chmod-file
      name  = iv_filename.
    ASSERT sy-subrc = 0.
    <ls_old>-sha1 = lo_blob->sha1( ).

    CREATE OBJECT lo_tree
      EXPORTING
        iv_repo = mo_branch->get_data( )-repo.
    lo_tree->set_files( lt_old ).

    CREATE OBJECT lo_commit
      EXPORTING
        iv_repo = mo_branch->get_data( )-repo.
    lo_commit->set_tree( lo_tree->sha1( ) ).
    set_author_and_committer( lo_commit ).
    lo_commit->set_body( iv_commit_message ).
    lo_commit->set_parent( mo_branch->get_data( )-sha1 ).

    APPEND zcl_ags_pack=>to_object( lo_blob ) TO lt_objects.
    APPEND zcl_ags_pack=>to_object( lo_tree ) TO lt_objects.
    APPEND zcl_ags_pack=>to_object( lo_commit ) TO lt_objects.

    mo_branch->push(
      iv_new     = lo_commit->sha1( )
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
