REPORT zabapgitserver.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

PARAMETERS: p_rname TYPE zags_repos-name DEFAULT 'foobar' ##NO_TEXT,
            p_bname TYPE zags_branches-name DEFAULT 'master' ##NO_TEXT,
            p_sha1  TYPE zags_sha1.

PARAMETERS: p_crepo  TYPE c RADIOBUTTON GROUP g1,
            p_listb  TYPE c RADIOBUTTON GROUP g1,
            p_listbf TYPE c RADIOBUTTON GROUP g1,
            p_delete TYPE c RADIOBUTTON GROUP g1,
            p_blob   TYPE c RADIOBUTTON GROUP g1,
            p_commit TYPE c RADIOBUTTON GROUP g1,
            p_tree   TYPE c RADIOBUTTON GROUP g1,
            p_devery TYPE c RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b1.


CLASS lcl_app DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: run.

  PRIVATE SECTION.
    CLASS-METHODS:
      display_blob
        RAISING zcx_ags_error,
      display_tree
        RAISING zcx_ags_error,
      display_commit
        RAISING zcx_ags_error,
      delete_repository
        RAISING zcx_ags_error,
      create_repository
        RAISING zcx_ags_error,
      list_branches
        RAISING zcx_ags_error,
      delete_everything
        RAISING zcx_ags_error,
      list_branch_files
        RAISING zcx_ags_error.

ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD run.

    TRY.
        CASE abap_true.
          WHEN p_crepo.
            create_repository( ).
          WHEN p_listb.
            list_branches( ).
          WHEN p_delete.
            delete_repository( ).
          WHEN p_blob.
            display_blob( ).
          WHEN p_commit.
            display_commit( ).
          WHEN p_tree.
            display_tree( ).
          WHEN p_listbf.
            list_branch_files( ).
          WHEN p_devery.
            delete_everything( ).
          WHEN OTHERS.
            ASSERT 1 = 2.
        ENDCASE.
        COMMIT WORK.
        MESSAGE s003(zabapgitserver).
      CATCH zcx_ags_error INTO DATA(lx_error).
        ROLLBACK WORK.                                 "#EC CI_ROLLBACK
        MESSAGE lx_error TYPE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD list_branch_files.

    DATA(lo_repo) = NEW zcl_ags_repo( p_rname ).
    DATA(lo_branch) = lo_repo->get_branch( p_bname ).
    DATA(lo_commit) = NEW zcl_ags_obj_commit( lo_branch->get_data( )-sha1 ).
    DATA(lo_tree) = NEW zcl_ags_obj_tree( lo_commit->get( )-tree ).

    LOOP AT lo_tree->get_files( ) ASSIGNING FIELD-SYMBOL(<ls_file>).
      WRITE: / <ls_file>-name.
    ENDLOOP.

  ENDMETHOD.

  METHOD delete_everything.
* todo, remove this method when the code gets more stable
    DELETE FROM zags_branches.                          "#EC CI_NOWHERE
    DELETE FROM zags_objects.                           "#EC CI_NOWHERE
    DELETE FROM zags_repos.                             "#EC CI_NOWHERE
    WRITE: / 'Done'(007).
  ENDMETHOD.

  METHOD display_blob.
    DATA(lo_blob) = NEW zcl_ags_obj_blob( p_sha1 ).

    DATA(lv_string) = zcl_ags_util=>xstring_to_string_utf8( lo_blob->get_data( ) ).

    WHILE strlen( lv_string ) > 200.
      WRITE: / lv_string(200).
      lv_string = lv_string+200.
    ENDWHILE.
    WRITE: / lv_string.

  ENDMETHOD.

  METHOD display_tree.
    DATA(lo_tree) = NEW zcl_ags_obj_tree( p_sha1 ).

    LOOP AT lo_tree->get_files( ) ASSIGNING FIELD-SYMBOL(<ls_file>).
      WRITE: / <ls_file>-sha1, <ls_file>-chmod, <ls_file>-name.
    ENDLOOP.

  ENDMETHOD.

  METHOD display_commit.
    DATA(lo_commit) = NEW zcl_ags_obj_commit( p_sha1 ).

    WRITE: / 'tree:'(002), lo_commit->get( )-tree.
    WRITE: / 'parent:'(003), lo_commit->get( )-parent.
    WRITE: / 'author:'(004), lo_commit->get( )-author.
    WRITE: / 'committer:'(005), lo_commit->get( )-committer.
    WRITE: / 'body:'(006), lo_commit->get( )-body.

  ENDMETHOD.

  METHOD delete_repository.

    DATA: lo_repo TYPE REF TO zcl_ags_repo.


    CREATE OBJECT lo_repo
      EXPORTING
        iv_name = p_rname.

    lo_repo->delete( ).

  ENDMETHOD.

  METHOD create_repository.

    zcl_ags_repo=>create( p_rname ).

  ENDMETHOD.

  METHOD list_branches.

    DATA: lo_repo     TYPE REF TO zcl_ags_repo,
          lt_branches TYPE zcl_ags_repo=>ty_branches_tt.


    CREATE OBJECT lo_repo
      EXPORTING
        iv_name = p_rname.

    lt_branches = lo_repo->list_branches( ).

    LOOP AT lt_branches ASSIGNING FIELD-SYMBOL(<lo_branch>).
      WRITE: / <lo_branch>->get_data( )-name, <lo_branch>->get_data( )-sha1.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  lcl_app=>run( ).