REPORT zabapgitserver.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_crepo  TYPE c RADIOBUTTON GROUP g1,
            p_listb  TYPE c RADIOBUTTON GROUP g1,
            p_delete TYPE c RADIOBUTTON GROUP g1,
            p_rname  TYPE zags_repos-name DEFAULT 'foobar',
            p_cbra   TYPE c RADIOBUTTON GROUP g1,
            p_bname  TYPE zags_repos-name DEFAULT 'refs/heads/master'.
SELECTION-SCREEN END OF BLOCK b1.

CLASS lcl_app DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: run.

  PRIVATE SECTION.
    CLASS-METHODS:
      delete_repository
        RAISING zcx_ags_error,
      create_repository
        RAISING zcx_ags_error,
      list_branches
        RAISING zcx_ags_error,
      create_branch
        RAISING zcx_ags_error.

ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD run.

    DATA: lx_error TYPE REF TO zcx_ags_error.


    TRY.
        CASE abap_true.
          WHEN p_crepo.
            create_repository( ).
          WHEN p_cbra.
            create_branch( ).
          WHEN p_listb.
            list_branches( ).
          WHEN p_delete.
            delete_repository( ).
          WHEN OTHERS.
            ASSERT 1 = 2.
        ENDCASE.
        COMMIT WORK.
        MESSAGE s001(zabapgitserver).
      CATCH zcx_ags_error INTO lx_error.
        ROLLBACK WORK.
        MESSAGE lx_error TYPE 'E'.
    ENDTRY.

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

  METHOD create_branch.

    BREAK-POINT.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  lcl_app=>run( ).