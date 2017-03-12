REPORT zags_cache_rebuild.

START-OF-SELECTION.
  PERFORM run.

FORM run RAISING zcx_ags_error.

  PERFORM delete.
  PERFORM build.

  COMMIT WORK.

  WRITE: / 'Done'(001).

ENDFORM.

FORM delete.

  zcl_ags_db=>get_tree_cache( )->delete_all( ).

ENDFORM.

FORM build RAISING zcx_ags_error.

  DATA: lt_repos    TYPE zags_repos_tt,
        lt_branches TYPE zcl_ags_repo=>ty_branches_tt,
        lo_branch   LIKE LINE OF lt_branches.

  FIELD-SYMBOLS: <ls_repo> LIKE LINE OF lt_repos.


  lt_repos = zcl_ags_repo=>list( ).
  LOOP AT lt_repos ASSIGNING <ls_repo>.
    lt_branches = zcl_ags_repo=>get_instance( <ls_repo>-name )->list_branches( ).
    LOOP AT lt_branches INTO lo_branch.
      lo_branch->get_cache( )->build( abap_true ).
    ENDLOOP.
  ENDLOOP.

ENDFORM.
