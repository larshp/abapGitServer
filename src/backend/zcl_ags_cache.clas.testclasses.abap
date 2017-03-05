
CLASS ltcl_list_commits DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      setup,
      commit_new_file
        IMPORTING io_repo TYPE REF TO zcl_ags_repo
        RAISING   zcx_ags_error,
      create_repo
        RETURNING VALUE(ro_repo) TYPE REF TO zcl_ags_repo
        RAISING   zcx_ags_error,
      list_commits FOR TESTING RAISING zcx_ags_error.

ENDCLASS.

CLASS ltcl_list_commits IMPLEMENTATION.

  METHOD setup.
    zcl_ags_db=>set_fake( ).
  ENDMETHOD.

  METHOD create_repo.
    ro_repo = zcl_ags_repo=>create(
      iv_name        = 'UNITTEST'
      iv_description = 'FOOBAR' ).
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


    lo_repo = create_repo( ).

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
