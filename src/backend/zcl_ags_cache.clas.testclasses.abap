
CLASS ltcl_list_commits DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      setup,
      commit_new_file
        IMPORTING io_repo TYPE REF TO zcl_ags_repo
        RAISING   zcx_ags_error,
      list_commits FOR TESTING RAISING zcx_ags_error.

ENDCLASS.

CLASS ltcl_list_commits IMPLEMENTATION.

  METHOD setup.
    zcl_ags_db=>set_fake( ).
  ENDMETHOD.

  METHOD commit_new_file.

    DATA: lo_blob    TYPE REF TO zcl_ags_obj_blob,
          lo_tree    TYPE REF TO zcl_ags_obj_tree,
          lo_commit  TYPE REF TO zcl_ags_obj_commit,
          lv_user    TYPE string,
          lo_branch  TYPE REF TO zcl_ags_branch,
          lt_objects TYPE zcl_ags_pack=>ty_objects_tt.


    lo_branch = io_repo->get_default_branch( ).

    CREATE OBJECT lo_blob.
    lo_blob->set_data( zcl_ags_util=>string_to_xstring_utf8( 'hello' ) ).

    CREATE OBJECT lo_tree.
    lo_tree->add_file(
      iv_chmod = zcl_ags_obj_tree=>c_chmod-file
      iv_name  = 'NEW.TXT'
      iv_sha1  = lo_blob->sha1( ) ).

    lv_user = |initial <foo@bar.com> { zcl_ags_util=>get_time( ) }|.

    CREATE OBJECT lo_commit.
    lo_commit->set_tree( lo_tree->sha1( ) ).
    lo_commit->set_author( lv_user ).
    lo_commit->set_committer( lv_user ).
    lo_commit->set_body( 'FOOBAR' ).
    lo_commit->set_parent( lo_branch->get_data( )-sha1 ).

    APPEND zcl_ags_pack=>to_object( lo_blob ) TO lt_objects.
    APPEND zcl_ags_pack=>to_object( lo_tree ) TO lt_objects.
    APPEND zcl_ags_pack=>to_object( lo_commit ) TO lt_objects.

    lo_branch->push(
      iv_new     = lo_commit->sha1( )
      iv_old     = lo_branch->get_data( )-sha1
      it_objects = lt_objects ).

  ENDMETHOD.

  METHOD list_commits.

    DATA: lo_repo    TYPE REF TO zcl_ags_repo,
          lt_commits TYPE zcl_ags_obj_commit=>ty_pretty_tt.


    lo_repo = zcl_ags_repo=>create(
      iv_name        = 'UNITTEST'
      iv_description = 'FOOBAR' ).

    lt_commits = lo_repo->get_default_branch( )->get_cache( )->list_commits( ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_commits )
      exp = 1 ).

    commit_new_file( lo_repo ).

    lt_commits = lo_repo->get_default_branch( )->get_cache( )->list_commits( ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_commits )
      exp = 2 ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_list_files_by_path DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS: setup,
      check_filled IMPORTING it_files TYPE zcl_ags_cache=>ty_files_tt,
      push IMPORTING io_repo TYPE REF TO zcl_ags_repo RAISING zcx_ags_error,
      list_files_by_path FOR TESTING RAISING zcx_ags_error.

ENDCLASS.       "ltcl_List_Files_By_Path

CLASS ltcl_list_files_by_path IMPLEMENTATION.

  METHOD setup.
    zcl_ags_db=>set_fake( ).
  ENDMETHOD.

  METHOD check_filled.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF it_files.

    LOOP AT it_files ASSIGNING <ls_file>.
      cl_abap_unit_assert=>assert_not_initial( <ls_file>-filename ).
      cl_abap_unit_assert=>assert_not_initial( <ls_file>-path ).
      cl_abap_unit_assert=>assert_not_initial( <ls_file>-blob_sha1 ).
      cl_abap_unit_assert=>assert_not_initial( <ls_file>-last_commit_sha1 ).
      cl_abap_unit_assert=>assert_not_initial( <ls_file>-time ).
      cl_abap_unit_assert=>assert_not_initial( <ls_file>-comment ).
    ENDLOOP.

  ENDMETHOD.

  METHOD push.

    DATA: lo_blob    TYPE REF TO zcl_ags_obj_blob,
          lo_tree1   TYPE REF TO zcl_ags_obj_tree,
          lo_tree2   TYPE REF TO zcl_ags_obj_tree,
          lo_commit  TYPE REF TO zcl_ags_obj_commit,
          lv_user    TYPE string,
          lo_branch  TYPE REF TO zcl_ags_branch,
          lt_objects TYPE zcl_ags_pack=>ty_objects_tt.


    lo_branch = io_repo->get_default_branch( ).

    CREATE OBJECT lo_blob.
    lo_blob->set_data( zcl_ags_util=>string_to_xstring_utf8( 'hello' ) ).

    CREATE OBJECT lo_tree1.
    lo_tree1->add_file(
      iv_chmod = zcl_ags_obj_tree=>c_chmod-file
      iv_name  = 'NEW.TXT'
      iv_sha1  = lo_blob->sha1( ) ).

    CREATE OBJECT lo_tree2.
    lo_tree2->add_file(
      iv_chmod = zcl_ags_obj_tree=>c_chmod-file
      iv_name  = 'NEW.TXT'
      iv_sha1  = lo_blob->sha1( ) ).
    lo_tree2->add_file(
      iv_chmod = zcl_ags_obj_tree=>c_chmod-dir
      iv_name  = 'folder'
      iv_sha1  = lo_tree1->sha1( ) ).

    lv_user = |initial <foo@bar.com> { zcl_ags_util=>get_time( ) }|.

    CREATE OBJECT lo_commit.
    lo_commit->set_tree( lo_tree2->sha1( ) ).
    lo_commit->set_author( lv_user ).
    lo_commit->set_committer( lv_user ).
    lo_commit->set_body( 'FOOBAR' ).
    lo_commit->set_parent( lo_branch->get_data( )-sha1 ).

    APPEND zcl_ags_pack=>to_object( lo_blob ) TO lt_objects.
    APPEND zcl_ags_pack=>to_object( lo_tree1 ) TO lt_objects.
    APPEND zcl_ags_pack=>to_object( lo_tree2 ) TO lt_objects.
    APPEND zcl_ags_pack=>to_object( lo_commit ) TO lt_objects.

    lo_branch->push(
      iv_new     = lo_commit->sha1( )
      iv_old     = lo_branch->get_data( )-sha1
      it_objects = lt_objects ).

  ENDMETHOD.

  METHOD list_files_by_path.

    DATA: lt_files  TYPE zcl_ags_cache=>ty_files_tt,
          lt_files2 TYPE zcl_ags_cache=>ty_files_tt,
          lo_repo   TYPE REF TO zcl_ags_repo.


    lo_repo = zcl_ags_repo=>create(
      iv_name        = 'UNITTEST'
      iv_description = 'FOOBAR' ).

    lt_files = lo_repo->get_default_branch( )->get_cache( )->list_files_by_path( '/' ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 1 ).
    check_filled( lt_files ).

    push( lo_repo ).

    lt_files = lo_repo->get_default_branch( )->get_cache( )->list_files_by_path( '/' ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 2 ).
    check_filled( lt_files ).

* identical query, test cache returns the correct result
    lt_files2 = lo_repo->get_default_branch( )->get_cache( )->list_files_by_path( '/' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_files
      exp = lt_files2 ).

  ENDMETHOD.

ENDCLASS.
